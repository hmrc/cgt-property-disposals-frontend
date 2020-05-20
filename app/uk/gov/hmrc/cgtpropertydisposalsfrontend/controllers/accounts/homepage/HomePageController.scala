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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.accounts.homepage

import java.util.UUID

import cats.data.EitherT
import cats.instances.future._
import cats.instances.string._
import cats.instances.uuid._
import cats.syntax.eq._
import com.google.inject.Inject
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.triage
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{SessionUpdates, returns}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, JustSubmittedReturn, StartingNewDraftReturn, SubmitReturnFailed, Subscribed, ViewingReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.CgtReference
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.IncompleteSingleDisposalTriageAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{DraftReturn, ReturnSummary}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.{PaymentsService, ReturnsService}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging.LoggerOps
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.{controllers, views}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

class HomePageController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  errorHandler: ErrorHandler,
  sessionStore: SessionStore,
  returnsService: ReturnsService,
  paymentsService: PaymentsService,
  cc: MessagesControllerComponents,
  manageYourDetailsPage: views.html.account.manage_your_details,
  homePage: views.html.account.home,
  detailUpdatedPage: views.html.account.details_updated,
  signedOutPage: views.html.account.signed_out,
  subsequentReturnExitPage: views.html.returns.subsequent_return_exit
)(implicit viewConfig: ViewConfig, ec: ExecutionContext)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionUpdates
    with Logging {

  // homepage for after private beta: includes functionality to do with returns
  def homepage(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request: RequestWithSessionData[AnyContent] =>
      withSubscribedUser((_, subscribed) => Ok(homePage(subscribed)))(
        withUplift = true
      )
    }

  def startNewReturn(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withSubscribedUser { (_, subscribed) =>
        val exitForSubsequentReturnFlag =
          subscribed.sentReturns.nonEmpty || subscribed.draftReturns.nonEmpty

        exitForSubsequentReturnFlag match {
          case true =>
            Redirect(routes.HomePageController.exitForSubsequentReturn())

          case _    =>
            val redirectTo = subscribed.subscribedDetails
              .userType()
              .fold(
                _ =>
                  triage.routes.CommonTriageQuestionsController
                    .howManyProperties(),
                _ =>
                  triage.routes.CommonTriageQuestionsController
                    .whoIsIndividualRepresenting()
              )

            updateSession(sessionStore, request)(
              _.copy(
                journeyStatus = Some(
                  StartingNewDraftReturn(
                    subscribed.subscribedDetails,
                    subscribed.ggCredId,
                    subscribed.agentReferenceNumber,
                    Right(IncompleteSingleDisposalTriageAnswers.empty),
                    None
                  )
                )
              )
            ).map {
              case Left(e)  =>
                logger.warn("Could not update session", e)
                errorHandler.errorResult()

              case Right(_) =>
                Redirect(redirectTo)
            }
        }
      }(withUplift = false)
    }

  def resumeDraftReturn(id: UUID): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withSubscribedUser { (_, subscribed) =>
        subscribed.draftReturns
          .find(_.id === id)
          .fold[Future[Result]] {
            logger.warn(
              s"For cgt reference ${subscribed.subscribedDetails.cgtReference.value} " +
                s"could not find draft return with id $id"
            )
            errorHandler.errorResult()
          } { draftReturn =>
            updateSession(sessionStore, request)(
              _.copy(
                journeyStatus = Some(
                  FillingOutReturn(
                    subscribed.subscribedDetails,
                    subscribed.ggCredId,
                    subscribed.agentReferenceNumber,
                    draftReturn
                  )
                )
              )
            ).map {
              case Left(e)  =>
                logger.warn("Could not update session", e)
                errorHandler.errorResult()

              case Right(_) =>
                Redirect(returns.routes.TaskListController.taskList())
            }
          }
      }(withUplift = false)
    }

  def viewSentReturn(submissionId: String): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withSubscribedUser {
        case (_, subscribed) =>
          subscribed.sentReturns
            .find(_.submissionId === submissionId)
            .fold[Future[Result]] {
              logger.warn(
                s"Could not find return with submission id $submissionId for cgt reference ${subscribed.subscribedDetails.cgtReference.value}"
              )
              NotFound
            } { returnSummary =>
              val result = for {
                sentReturn <- returnsService
                                .displayReturn(
                                  subscribed.subscribedDetails.cgtReference,
                                  returnSummary.submissionId
                                )
                _          <- EitherT(
                       updateSession(sessionStore, request)(
                         _.copy(
                           journeyStatus = Some(
                             ViewingReturn(
                               subscribed.subscribedDetails,
                               subscribed.ggCredId,
                               subscribed.agentReferenceNumber,
                               sentReturn,
                               returnSummary
                             )
                           )
                         )
                       )
                     )
              } yield ()

              result.fold(
                { e =>
                  logger.warn("Could not get sent return", e)
                  errorHandler.errorResult()
                },
                _ => Redirect(returns.routes.ViewReturnController.displayReturn())
              )
            }
      }(withUplift = false)
    }

  def payTotalAmountLeftToPay(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withSubscribedUser {
        case (_, subscribed) =>
          paymentsService
            .startPaymentJourney(
              subscribed.subscribedDetails.cgtReference,
              None,
              subscribed.totalLeftToPay(),
              routes.HomePageController.homepage(),
              routes.HomePageController.homepage()
            )
            .fold(
              { e =>
                logger.warn(
                  "Could not start payments journey to pay total amount outstanding",
                  e
                )
                errorHandler.errorResult()
              },
              { journey =>
                logger.info(
                  s"Payment journey started with journeyId ${journey.journeyId} to pay total outstanding amount on account"
                )
                Redirect(journey.nextUrl)
              }
            )
      }(withUplift = false)

    }

  def exitForSubsequentReturn(): Action[AnyContent] =
    authenticatedActionWithSessionData { implicit request =>
      Ok(subsequentReturnExitPage(routes.HomePageController.homepage()))
    }

  private def withSubscribedUser(
    f: (SessionData, Subscribed) => Future[Result]
  )(withUplift: Boolean)(implicit
    hc: HeaderCarrier,
    request: RequestWithSessionData[_]
  ): Future[Result] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some((s: SessionData, r: StartingNewDraftReturn)) if withUplift =>
        upliftToSubscribedAndThen(r, r.subscribedDetails.cgtReference) {
          case (r, draftReturns, sentReturns) =>
            Subscribed(
              r.subscribedDetails,
              r.ggCredId,
              r.agentReferenceNumber,
              draftReturns,
              sentReturns
            )
        }(f(s, _))

      case Some((s: SessionData, r: FillingOutReturn)) if withUplift       =>
        upliftToSubscribedAndThen(r, r.subscribedDetails.cgtReference) {
          case (r, draftReturns, sentReturns) =>
            Subscribed(
              r.subscribedDetails,
              r.ggCredId,
              r.agentReferenceNumber,
              draftReturns,
              sentReturns
            )
        }(f(s, _))

      case Some((s: SessionData, r: JustSubmittedReturn)) if withUplift    =>
        upliftToSubscribedAndThen(r, r.subscribedDetails.cgtReference) {
          case (r, draftReturns, sentReturns) =>
            Subscribed(
              r.subscribedDetails,
              r.ggCredId,
              r.agentReferenceNumber,
              draftReturns,
              sentReturns
            )
        }(f(s, _))

      case Some((s: SessionData, r: SubmitReturnFailed)) if withUplift     =>
        upliftToSubscribedAndThen(r, r.subscribedDetails.cgtReference) {
          case (r, draftReturns, sentReturns) =>
            Subscribed(
              r.subscribedDetails,
              r.ggCredId,
              r.agentReferenceNumber,
              draftReturns,
              sentReturns
            )
        }(f(s, _))

      case Some((s: SessionData, r: ViewingReturn)) if withUplift          =>
        upliftToSubscribedAndThen(r, r.subscribedDetails.cgtReference) {
          case (r, draftReturns, sentReturns) =>
            Subscribed(
              r.subscribedDetails,
              r.ggCredId,
              r.agentReferenceNumber,
              draftReturns,
              sentReturns
            )
        }(f(s, _))

      case Some((s: SessionData, r: Subscribed))                           =>
        f(s, r)

      case _                                                               =>
        Redirect(controllers.routes.StartController.start().url)
    }

  private def upliftToSubscribedAndThen[J](
    journey: J,
    cgtReference: CgtReference
  )(
    uplift: (J, List[DraftReturn], List[ReturnSummary]) => Subscribed
  )(
    f: Subscribed => Future[Result]
  )()(implicit
    hc: HeaderCarrier,
    request: RequestWithSessionData[_]
  ): Future[Result] = {
    val result = for {
      sentReturns  <- returnsService.listReturns(cgtReference)
      draftReturns <- returnsService.getDraftReturns(cgtReference, sentReturns)
      subscribed    = uplift(journey, draftReturns, sentReturns)
      _            <- EitherT(
             updateSession(sessionStore, request)(
               _.copy(journeyStatus = Some(subscribed))
             )
           )
    } yield subscribed

    result
      .biSemiflatMap(
        { e =>
          logger.warn("Could not update session", e)
          errorHandler.errorResult()
        },
        f
      )
      .merge
  }

}
