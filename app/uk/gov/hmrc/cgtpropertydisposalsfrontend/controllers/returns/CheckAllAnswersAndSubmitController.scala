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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns

import cats.data.EitherT
import cats.instances.future._
import com.google.inject.{Inject, Singleton}
import play.api.i18n.{Lang, Messages}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.accounts.homepage
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.CheckAllAnswersAndSubmitController.SubmitReturnResult
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.CheckAllAnswersAndSubmitController.SubmitReturnResult.{SubmitReturnError, SubmitReturnSuccess}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.acquisitiondetails.RebasingEligibilityUtil
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{SessionUpdates, routes => baseRoutes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, JustSubmittedReturn, SubmitReturnFailed, Subscribed}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.CompleteReturn.{CompleteMultipleDisposalsReturn, CompleteMultipleIndirectDisposalReturn, CompleteSingleDisposalReturn, CompleteSingleIndirectDisposalReturn, CompleteSingleMixedUseDisposalReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.RepresenteeAnswers.CompleteRepresenteeAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.RepresenteeReferenceId.NoReferenceId
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{B64Html, Error, SessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.onboarding.SubscriptionService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.{FurtherReturnCalculationEligibility, FurtherReturnCalculationEligibilityUtil, PaymentsService, ReturnsService}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging.LoggerOps
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views.html.{returns => pages}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import java.util.Base64
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class CheckAllAnswersAndSubmitController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionStore,
  val errorHandler: ErrorHandler,
  returnsService: ReturnsService,
  paymentsService: PaymentsService,
  furtherReturnCalculationEligibilityUtil: FurtherReturnCalculationEligibilityUtil,
  cc: MessagesControllerComponents,
  checkAllAnswersPage: pages.check_all_answers,
  confirmationOfSubmissionPage: pages.confirmation_of_submission,
  rebasingEligibilityUtil: RebasingEligibilityUtil,
  submitReturnFailedPage: pages.submit_return_error,
  subscriptionService: SubscriptionService
)(implicit viewConfig: ViewConfig, ec: ExecutionContext)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionUpdates
    with Logging {

  private val explicitEnglishMessage: Messages = messagesApi.preferred(Seq(Lang("en")))

  private def withFurtherReturnCalculationEligibilityCheck(fillingOutReturn: FillingOutReturn)(
    f: Option[FurtherReturnCalculationEligibility] => Future[Result]
  )(implicit r: RequestWithSessionData[_]): Future[Result] = {
    val furtherReturnCalculationEligibilityCheck =
      if (fillingOutReturn.isFurtherOrAmendReturn.contains(true)) {
        furtherReturnCalculationEligibilityUtil
          .isEligibleForFurtherReturnOrAmendCalculation(fillingOutReturn)
          .map(Some(_))
      } else {
        EitherT.pure(None)
      }

    furtherReturnCalculationEligibilityCheck.foldF(
      { e =>
        logger.warn("Could not check eligibility for further return calculation", e)
        errorHandler.errorResult()
      },
      f
    )
  }

  def checkAllAnswers(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withCompleteDraftReturn(request) { (_, fillingOutReturn, completeReturn) =>
        withFurtherReturnCalculationEligibilityCheck(fillingOutReturn) { furtherOrAmendReturnCalculationEligibility =>
          Ok(
            checkAllAnswersPage(
              completeReturn,
              rebasingEligibilityUtil,
              fillingOutReturn,
              showSubmissionDetails = false,
              fillingOutReturn.amendReturnData.exists(_.preserveEstimatesAnswer),
              furtherOrAmendReturnCalculationEligibility
            )
          )
        }
      }
    }

  def checkAllAnswersSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withCompleteDraftReturn(request) { (_, fillingOutReturn, completeReturn) =>
        withGeneratedCGTReference(completeReturn) { updatedCompleteReturn =>
          withFurtherReturnCalculationEligibilityCheck(fillingOutReturn) { furtherOrAmendReturnCalculationEligibility =>
            val cyaPageB64Html =
              B64Html(
                new String(
                  Base64.getEncoder.encode(
                    checkAllAnswersPage(
                      updatedCompleteReturn,
                      rebasingEligibilityUtil,
                      fillingOutReturn,
                      showSubmissionDetails = true,
                      fillingOutReturn.amendReturnData.exists(_.preserveEstimatesAnswer),
                      furtherOrAmendReturnCalculationEligibility
                    )(request, explicitEnglishMessage)
                      .toString()
                      .getBytes
                  )
                )
              )

            val result =
              for {
                response        <- EitherT.liftF(
                                     submitReturn(
                                       updatedCompleteReturn,
                                       fillingOutReturn,
                                       cyaPageB64Html,
                                       request.authenticatedRequest.request.messages.lang
                                     )
                                   )
                newJourneyStatus = response match {
                                     case _: SubmitReturnError =>
                                       SubmitReturnFailed(
                                         fillingOutReturn.subscribedDetails,
                                         fillingOutReturn.ggCredId,
                                         fillingOutReturn.agentReferenceNumber
                                       )
                                     case SubmitReturnSuccess(
                                           submitReturnResponse
                                         ) =>
                                       JustSubmittedReturn(
                                         fillingOutReturn.subscribedDetails,
                                         fillingOutReturn.ggCredId,
                                         fillingOutReturn.agentReferenceNumber,
                                         updatedCompleteReturn,
                                         submitReturnResponse,
                                         fillingOutReturn.amendReturnData
                                       )
                                   }
                _               <- EitherT(
                                     updateSession(sessionStore, request)(
                                       _.copy(journeyStatus = Some(newJourneyStatus))
                                     )
                                   )
              } yield response

            result.fold(
              { e =>
                logger.warn("Error while trying to update session", e)
                errorHandler.errorResult()
              },
              {
                case SubmitReturnError(e) =>
                  logger.warn(s"Could not submit return", e)
                  Redirect(
                    routes.CheckAllAnswersAndSubmitController.submissionError()
                  )

                case SubmitReturnSuccess(r) =>
                  logger.info(s"Successfully submitted return with submission id ${r.formBundleId}")
                  Redirect(
                    routes.CheckAllAnswersAndSubmitController
                      .confirmationOfSubmission()
                  )
              }
            )
          }
        }
      }
    }

  private def submitReturn(
    completeReturn: CompleteReturn,
    fillingOutReturn: FillingOutReturn,
    cyaPageB64Html: B64Html,
    language: Lang
  )(implicit
    hc: HeaderCarrier
  ): Future[SubmitReturnResult] =
    returnsService
      .submitReturn(
        SubmitReturnRequest(
          completeReturn,
          fillingOutReturn.draftReturn.id,
          fillingOutReturn.subscribedDetails,
          fillingOutReturn.agentReferenceNumber,
          fillingOutReturn.isFurtherOrAmendReturn.contains(true),
          cyaPageB64Html,
          fillingOutReturn.amendReturnData
        ),
        language
      )
      .bimap(
        SubmitReturnError,
        SubmitReturnSuccess
      )
      .merge

  def submissionError(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withSubmitReturnFailesOrSubscribed(request)(_ => Ok(submitReturnFailedPage()))
    }

  def submissionErrorSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withSubmitReturnFailesOrSubscribed(request)(_ => Redirect(homepage.routes.HomePageController.homepage()))
    }

  def confirmationOfSubmission(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withJustSubmittedReturn(request)(j => Ok(confirmationOfSubmissionPage(j)))
    }

  def payReturn(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withJustSubmittedReturn(request) { j =>
        j.submissionResponse.charge.fold[Future[Result]] {
          logger.warn("Could not find charge in pay return call, redirecting to homepage")
          Redirect(homepage.routes.HomePageController.homepage())
        } { charge =>
          paymentsService
            .startPaymentJourney(
              j.subscribedDetails.cgtReference,
              Some(charge.chargeReference),
              charge.amount,
              Some(charge.dueDate),
              homepage.routes.HomePageController.homepage(),
              routes.CheckAllAnswersAndSubmitController
                .confirmationOfSubmission()
            )
            .fold(
              { e =>
                logger.warn("Could not start payments journey", e)
                errorHandler.errorResult()
              },
              { paymentsJourney =>
                logger.info(
                  s"Payment journey started with journey id ${paymentsJourney.journeyId}. Redirecting to ${paymentsJourney.nextUrl}"
                )
                Redirect(paymentsJourney.nextUrl)
              }
            )
        }
      }

    }

  private def withJustSubmittedReturn(
    request: RequestWithSessionData[_]
  )(f: JustSubmittedReturn => Future[Result]): Future[Result] =
    request.sessionData.flatMap(_.journeyStatus) match {
      case Some(j: JustSubmittedReturn) => f(j)
      case _                            => Redirect(baseRoutes.StartController.start())
    }

  private def withSubmitReturnFailesOrSubscribed(
    request: RequestWithSessionData[_]
  )(
    f: Either[SubmitReturnFailed, Subscribed] => Future[Result]
  ): Future[Result] =
    request.sessionData.flatMap(_.journeyStatus) match {
      case Some(s: SubmitReturnFailed) => f(Left(s))
      case Some(s: Subscribed)         => f(Right(s))
      case _                           => Redirect(baseRoutes.StartController.start())
    }

  private def withGeneratedCGTReference(
    completeReturn: CompleteReturn
  )(f: CompleteReturn => Future[Result])(implicit request: RequestWithSessionData[_]) =
    completeReturn.representeeAnswers match {
      case Some(a @ CompleteRepresenteeAnswers(_, NoReferenceId, _, _, _)) =>
        subscriptionService
          .registerWithoutIdAndSubscribe(a, request.authenticatedRequest.request.messages.lang)
          .fold(
            _ => {
              logger.warn("Error registering user without id")
              Future(errorHandler.errorResult())
            },
            cgt => {
              val updatedCompleteReturn: CompleteReturn = completeReturn match {
                case a: CompleteMultipleDisposalsReturn        =>
                  a.copy(representeeAnswers = a.representeeAnswers.map(e => e.copy(id = cgt)))
                case a: CompleteSingleDisposalReturn           =>
                  a.copy(representeeAnswers = a.representeeAnswers.map(e => e.copy(id = cgt)))
                case a: CompleteSingleIndirectDisposalReturn   =>
                  a.copy(representeeAnswers = a.representeeAnswers.map(e => e.copy(id = cgt)))
                case a: CompleteMultipleIndirectDisposalReturn =>
                  a.copy(representeeAnswers = a.representeeAnswers.map(e => e.copy(id = cgt)))
                case a: CompleteSingleMixedUseDisposalReturn   =>
                  a.copy(representeeAnswers = a.representeeAnswers.map(e => e.copy(id = cgt)))
              }
              f(updatedCompleteReturn)
            }
          )
          .flatMap(e => e)
      case _                                                               => f(completeReturn)
    }

  private def withCompleteDraftReturn(
    request: RequestWithSessionData[_]
  )(
    f: (SessionData, FillingOutReturn, CompleteReturn) => Future[Result]
  ): Future[Result] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some(
            (
              s,
              r @ FillingOutReturn(
                _,
                _,
                _,
                draftReturn: DraftSingleDisposalReturn,
                _,
                _
              )
            )
          ) =>
        CompleteSingleDisposalReturn
          .fromDraftReturn(draftReturn)
          .fold[Future[Result]](
            Redirect(routes.TaskListController.taskList())
          )(f(s, r, _))

      case Some(
            (
              s,
              r @ FillingOutReturn(
                _,
                _,
                _,
                draftReturn: DraftMultipleDisposalsReturn,
                _,
                _
              )
            )
          ) =>
        CompleteMultipleDisposalsReturn
          .fromDraftReturn(draftReturn)
          .fold[Future[Result]](
            Redirect(routes.TaskListController.taskList())
          )(f(s, r, _))

      case Some(
            (
              s,
              r @ FillingOutReturn(
                _,
                _,
                _,
                draftReturn: DraftSingleIndirectDisposalReturn,
                _,
                _
              )
            )
          ) =>
        CompleteSingleIndirectDisposalReturn
          .fromDraftReturn(draftReturn)
          .fold[Future[Result]](
            Redirect(routes.TaskListController.taskList())
          )(f(s, r, _))

      case Some(
            (
              s,
              r @ FillingOutReturn(
                _,
                _,
                _,
                draftReturn: DraftMultipleIndirectDisposalsReturn,
                _,
                _
              )
            )
          ) =>
        CompleteMultipleIndirectDisposalReturn
          .fromDraftReturn(draftReturn)
          .fold[Future[Result]](
            Redirect(routes.TaskListController.taskList())
          )(f(s, r, _))

      case Some(
            (
              s,
              r @ FillingOutReturn(
                _,
                _,
                _,
                draftReturn: DraftSingleMixedUseDisposalReturn,
                _,
                _
              )
            )
          ) =>
        CompleteSingleMixedUseDisposalReturn
          .fromDraftReturn(draftReturn)
          .fold[Future[Result]](
            Redirect(routes.TaskListController.taskList())
          )(f(s, r, _))

      case _ =>
        Redirect(baseRoutes.StartController.start())
    }

}

object CheckAllAnswersAndSubmitController {

  sealed trait SubmitReturnResult

  object SubmitReturnResult {

    final case class SubmitReturnError(error: Error) extends SubmitReturnResult

    final case class SubmitReturnSuccess(response: SubmitReturnResponse) extends SubmitReturnResult

  }

}
