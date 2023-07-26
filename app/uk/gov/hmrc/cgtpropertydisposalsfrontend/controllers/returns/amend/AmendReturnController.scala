/*
 * Copyright 2023 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.amend

import cats.instances.string._
import cats.syntax.eq._
import com.google.inject.Inject
import play.api.data.Form
import play.api.data.Forms.{mapping, of}
import play.api.mvc._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.exemptionandlosses.routes.{ExemptionAndLossesController => exemptionsAndLossesRoutes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.initialgainorloss.routes.{InitialGainOrLossController => initialGainorLossRoutes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.yeartodatelliability.routes.{YearToDateLiabilityController => ytdRoutes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.StartingToAmendToFillingOutReturnBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.acquisitiondetails.RebasingEligibilityUtil
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.amend.AmendReturnController._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, StartingToAmendReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.UUIDGenerator
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.AmendReturnData
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.audit.CancelAmendReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{BooleanFormatter, TaxYear}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.AuditService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views.html.returns.{amend => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

class AmendReturnController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionStore,
  val errorHandler: ErrorHandler,
  uuidGenerator: UUIDGenerator,
  auditService: AuditService,
  cc: MessagesControllerComponents,
  rebasingEligibilityUtil: RebasingEligibilityUtil,
  confirmCancelPage: pages.confirm_cancel,
  checkYourAnswersPage: pages.check_your_answers,
  unmetDependencyPage: pages.unmet_dependency
)(implicit viewConfig: ViewConfig, ec: ExecutionContext)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionUpdates
    with Logging
    with StartingToAmendToFillingOutReturnBehaviour {

  def confirmCancel(back: String): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      confirmCancelBackLinkMappings.get(back) match {
        case None =>
          logger.warn(s"Could not get back link location for '$back'")
          errorHandler.errorResult()

        case Some(backLink) =>
          withStartingToAmendOrFillingOutReturn(request) { _ =>
            Ok(
              confirmCancelPage(
                confirmCancelForm,
                backLink,
                routes.AmendReturnController.confirmCancelSubmit(back)
              )
            )
          }
      }
    }

  def confirmCancelSubmit(back: String): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      confirmCancelBackLinkMappings.get(back) match {
        case None =>
          logger.warn(s"Could not get back link location for '$back'")
          errorHandler.errorResult()

        case Some(backLink) =>
          withStartingToAmendOrFillingOutReturn(request) { journeyState =>
            confirmCancelForm
              .bindFromRequest()
              .fold(
                formWithErrors =>
                  BadRequest(
                    confirmCancelPage(formWithErrors, backLink, routes.AmendReturnController.confirmCancelSubmit(back))
                  ),
                { cancel =>
                  val redirectTo =
                    if (cancel) {
                      val auditEvent = CancelAmendReturn(
                        journeyState
                          .fold(_.subscribedDetails.cgtReference.value, _._1.subscribedDetails.cgtReference.value),
                        journeyState
                          .fold(_.originalReturn.summary.submissionId, _._2.originalReturn.summary.submissionId),
                        journeyState.fold(_.agentReferenceNumber.map(_.value), _._1.agentReferenceNumber.map(_.value))
                      )
                      auditService.sendEvent("CancelAmendReturn", auditEvent, "cancel-amend-return")
                      controllers.returns.routes.ViewReturnController.displayReturn()
                    } else backLink

                  Redirect(redirectTo)
                }
              )
          }
      }
    }

  def checkYourAnswers(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withStartingToAmendReturn(request) { journey =>
        val originalTaxYear               = journey.originalReturn.summary.taxYear
        val currentTaxYear                = TaxYear.thisTaxYearStartDate().getYear.toString
        val futureDatesEnabled            = viewConfig.futureDatesEnabled
        val isSubmissionInPreviousTaxYear = originalTaxYear =!= currentTaxYear

        (futureDatesEnabled, isSubmissionInPreviousTaxYear) match {
          case (false, true) | (true, _) =>
            Redirect(
              controllers.returns.triage.routes.CommonTriageQuestionsController.amendsHaveYouAlreadySentSelfAssessment()
            )
          case _                         =>
            Ok(
              checkYourAnswersPage(
                journey.originalReturn.completeReturn,
                rebasingEligibilityUtil,
                journey.subscribedDetails,
                journey.originalReturn.completeReturn.representativeType(),
                journey.originalReturn.completeReturn.isIndirectDisposal,
                Some(journey.originalReturn.returnType.isFurtherOrAmendReturn),
                controllers.returns.routes.ViewReturnController.displayReturn()
              )
            )
        }
      }
    }

  def unmetDependency(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withStartingToAmendReturn(request) { startingToAmend =>
        startingToAmend.unmetDependencyFieldUrl match {
          case None =>
            Redirect(routes.AmendReturnController.checkYourAnswers())

          case Some(unmetDependencyFieldUrl) =>
            unmetDependencyKey(unmetDependencyFieldUrl).fold {
              logger.warn(s"Could not understand unmet dependency field url '$unmetDependencyFieldUrl'")
              errorHandler.errorResult()
            }(key => Ok(unmetDependencyPage(routes.AmendReturnController.checkYourAnswers(), key)))

        }
      }
    }

  def unmetDependencySubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withStartingToAmendReturn(request) { startingToAmend =>
        startingToAmend.unmetDependencyFieldUrl match {
          case None =>
            Redirect(routes.AmendReturnController.checkYourAnswers())

          case Some(_) =>
            convertFromStartingAmendToFillingOutReturn(
              startingToAmend,
              sessionStore,
              errorHandler,
              uuidGenerator,
              Some(controllers.returns.routes.TaskListController.taskList().url)
            )
        }
      }
    }

  private def unmetDependencyKey(unmetDependencyFieldUrl: String): Option[String] = {
    def is(s: String): Boolean = unmetDependencyFieldUrl === s

    val key =
      if (is(exemptionsAndLossesRoutes.inYearLosses().url)) Some("inYearLosses")
      else if (is(exemptionsAndLossesRoutes.previousYearsLosses().url)) Some("previousYearLosses")
      else if (is(exemptionsAndLossesRoutes.annualExemptAmount().url)) Some("annualExemptAmount")
      else if (is(ytdRoutes.estimatedIncome().url)) Some("income")
      else if (is(ytdRoutes.personalAllowance().url)) Some("personalAllowance")
      else if (is(ytdRoutes.taxableGainOrLoss().url)) Some("taxableGainOrLoss")
      else if (is(ytdRoutes.yearToDateLiability().url)) Some("yearToDateLiability")
      else if (is(ytdRoutes.taxDue().url)) Some("taxOwed")
      else if (is(ytdRoutes.nonCalculatedEnterTaxDue().url)) Some("taxOwed")
      else if (is(ytdRoutes.repayment().url)) Some("repayment")
      else if (is(ytdRoutes.hasEstimatedDetails().url)) Some("hasEstimated")
      else if (is(initialGainorLossRoutes.enterInitialGainOrLoss().url)) Some("initialGainOrLoss")
      else None

    key.map(k =>
      if (k === "initialGainOrLoss" || k === "annualExemptAmount" || k === "income" || k === "personalAllowance")
        s"unmetDependency.x1"
      else
        s"unmetDependency.x2"
    )
  }

  private def withStartingToAmendReturn(
    request: RequestWithSessionData[_]
  )(f: StartingToAmendReturn => Future[Result]): Future[Result] =
    request.sessionData.flatMap(_.journeyStatus) match {
      case Some(s: StartingToAmendReturn) => f(s)
      case _                              => Redirect(controllers.routes.StartController.start())
    }

  private def withStartingToAmendOrFillingOutReturn(
    request: RequestWithSessionData[_]
  )(f: Either[StartingToAmendReturn, (FillingOutReturn, AmendReturnData)] => Future[Result]): Future[Result] =
    request.sessionData.flatMap(_.journeyStatus) match {
      case Some(s: StartingToAmendReturn)                                   => f(Left(s))
      case Some(r @ FillingOutReturn(_, _, _, _, _, Some(amendReturnData))) => f(Right(r -> amendReturnData))
      case _                                                                => Redirect(controllers.routes.StartController.start())
    }

}

object AmendReturnController {

  object ConfirmCancelBackLocations {
    val checkAnswers: String           = "checkAnswers"
    val unmetDependency: String        = "unmetDependency"
    val taskList: String               = "taskList"
    val checkAnswersAcceptSend: String = "checkAnswersAmendSend"
  }

  val confirmCancelBackLinkMappings: Map[String, Call] =
    Map(
      ConfirmCancelBackLocations.checkAnswers           -> routes.AmendReturnController.checkYourAnswers(),
      ConfirmCancelBackLocations.unmetDependency        -> routes.AmendReturnController.unmetDependency(),
      ConfirmCancelBackLocations.taskList               -> controllers.returns.routes.TaskListController.taskList(),
      ConfirmCancelBackLocations.checkAnswersAcceptSend -> controllers.returns.routes.CheckAllAnswersAndSubmitController
        .checkAllAnswers()
    )

  val confirmCancelForm: Form[Boolean] =
    Form(
      mapping(
        "confirmCancelAmendReturn" -> of(BooleanFormatter.formatter)
      )(identity)(Some(_))
    )

}
