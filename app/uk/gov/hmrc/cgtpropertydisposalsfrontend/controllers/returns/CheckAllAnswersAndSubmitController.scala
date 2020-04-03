/*
 * Copyright 2020 HM Revenue & Customs
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
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.accounts.homepage
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.CheckAllAnswersAndSubmitController.SubmitReturnResult
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.CheckAllAnswersAndSubmitController.SubmitReturnResult.{SubmitReturnError, SubmitReturnSuccess}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.acquisitiondetails.RebasingEligibilityUtil
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{SessionUpdates, routes => baseRoutes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, JustSubmittedReturn, SubmitReturnFailed, Subscribed}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.CompleteReturn.{CompleteMultipleDisposalsReturn, CompleteSingleDisposalReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{DraftMultipleDisposalsReturn, DraftSingleDisposalReturn, _}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, SessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.{PaymentsService, ReturnsService}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging.LoggerOps
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views.html.{returns => pages}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class CheckAllAnswersAndSubmitController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionStore,
  val errorHandler: ErrorHandler,
  returnsService: ReturnsService,
  paymentsService: PaymentsService,
  cc: MessagesControllerComponents,
  checkAllAnswersPage: pages.check_all_answers,
  confirmationOfSubmissionPage: pages.confirmation_of_submission,
  rebasingEligibilityUtil: RebasingEligibilityUtil,
  submitReturnFailedPage: pages.submit_return_error
)(implicit viewConfig: ViewConfig, ec: ExecutionContext)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionUpdates
    with Logging {

  def checkAllAnswers(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withCompleteDraftReturn(request) { (_, fillingOutReturn, completeReturn) =>
      Ok(
        checkAllAnswersPage(
          completeReturn,
          rebasingEligibilityUtil,
          fillingOutReturn.subscribedDetails.isATrust
        )
      )
    }
  }

  def checkAllAnswersSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withCompleteDraftReturn(request) { (_, fillingOutReturn, completeReturn) =>
      val result =
        for {
          response <- EitherT.liftF(submitReturn(completeReturn, fillingOutReturn))
          newJourneyStatus = response match {
            case _: SubmitReturnError =>
              SubmitReturnFailed(
                fillingOutReturn.subscribedDetails,
                fillingOutReturn.ggCredId,
                fillingOutReturn.agentReferenceNumber
              )
            case SubmitReturnSuccess(submitReturnResponse) =>
              JustSubmittedReturn(
                fillingOutReturn.subscribedDetails,
                fillingOutReturn.ggCredId,
                fillingOutReturn.agentReferenceNumber,
                completeReturn,
                submitReturnResponse
              )
          }
          _ <- EitherT(
                updateSession(sessionStore, request)(
                  _.copy(journeyStatus = Some(newJourneyStatus))
                )
              )
        } yield response

      result.fold(
        { e =>
          logger.warn("Error while trying to update session", e)
          errorHandler.errorResult()
        }, {
          case SubmitReturnError(e) =>
            logger.warn(s"Could not submit return}", e)
            Redirect(routes.CheckAllAnswersAndSubmitController.submissionError())

          case SubmitReturnSuccess(r) =>
            logger.info(s"Successfully submitted return with submission id ${r.formBundleId}")
            Redirect(routes.CheckAllAnswersAndSubmitController.confirmationOfSubmission())
        }
      )

    }
  }

  private def submitReturn(completeReturn: CompleteReturn, fillingOutReturn: FillingOutReturn)(
    implicit hc: HeaderCarrier
  ): Future[SubmitReturnResult] =
    returnsService
      .submitReturn(
        SubmitReturnRequest(
          completeReturn,
          fillingOutReturn.draftReturn.id,
          fillingOutReturn.subscribedDetails,
          fillingOutReturn.agentReferenceNumber
        )
      )
      .bimap(
        SubmitReturnError,
        SubmitReturnSuccess
      )
      .merge

  def submissionError(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withSubmitReturnFailesOrSubscribed(request)(_ => Ok(submitReturnFailedPage()))
  }

  def submissionErrorSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withSubmitReturnFailesOrSubscribed(request)(_ => Redirect(homepage.routes.HomePageController.homepage()))
  }

  def confirmationOfSubmission(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withJustSubmittedReturn(request)(j => Ok(confirmationOfSubmissionPage(j)))
  }

  def payReturn(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
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
            homepage.routes.HomePageController.homepage(),
            routes.CheckAllAnswersAndSubmitController.confirmationOfSubmission()
          )
          .fold(
            { e =>
              logger.warn("Could not start payments journey", e)
              errorHandler.errorResult()
            }, { paymentsJourney =>
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
  )(f: Either[SubmitReturnFailed, Subscribed] => Future[Result]): Future[Result] =
    request.sessionData.flatMap(_.journeyStatus) match {
      case Some(s: SubmitReturnFailed) => f(Left(s))
      case Some(s: Subscribed)         => f(Right(s))
      case _                           => Redirect(baseRoutes.StartController.start())
    }

  private def withCompleteDraftReturn(
    request: RequestWithSessionData[_]
  )(f: (SessionData, FillingOutReturn, CompleteReturn) => Future[Result]): Future[Result] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some((s, r @ FillingOutReturn(_, _, _, draftReturn: DraftSingleDisposalReturn))) =>
        CompleteSingleDisposalReturn
          .fromDraftReturn(draftReturn)
          .fold[Future[Result]](
            Redirect(routes.TaskListController.taskList())
          )(f(s, r, _))

      case Some((s, r @ FillingOutReturn(_, _, _, draftReturn: DraftMultipleDisposalsReturn))) =>
        CompleteMultipleDisposalsReturn
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
