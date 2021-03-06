/*
 * Copyright 2021 HM Revenue & Customs
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
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, JustSubmittedReturn, PreviousReturnData, StartingNewDraftReturn, SubmitReturnFailed, Subscribed, ViewingReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.TimeUtils.localDateOrder
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.AmountInPence
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.CgtReference
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.IncompleteSingleDisposalTriageAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.YearToDateLiabilityAnswers.NonCalculatedYTDAnswers.CompleteNonCalculatedYTDAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{CompleteReturn, DraftReturn, ReturnSummary}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.{PaymentsService, ReturnsService}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging.LoggerOps
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.{controllers, views}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

class HomePageController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  errorHandler: ErrorHandler,
  sessionStore: SessionStore,
  returnsService: ReturnsService,
  paymentsService: PaymentsService,
  cc: MessagesControllerComponents,
  homePage: views.html.account.home,
  multipleDraftExitPage: views.html.returns.multiple_draft_return_exit
)(implicit viewConfig: ViewConfig, ec: ExecutionContext)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionUpdates
    with Logging {

  def homepage(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request: RequestWithSessionData[AnyContent] =>
      withSubscribedUser((_, subscribed) => Ok(homePage(subscribed)))(
        withUplift = true
      )
    }

  def startNewReturn(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withSubscribedUser { (_, subscribed) =>
        val redirectToExitPage = subscribed.draftReturns.nonEmpty

        if (redirectToExitPage)
          Redirect(routes.HomePageController.exitForMultipleDraftReturn())
        else {
          val redirectTo = subscribed.subscribedDetails
            .userType()
            .fold(
              _ =>
                if (subscribed.sentReturns.isEmpty)
                  triage.routes.CommonTriageQuestionsController.howManyProperties()
                else
                  triage.routes.CommonTriageQuestionsController.howManyPropertiesFurtherReturn(),
              _ =>
                triage.routes.CommonTriageQuestionsController
                  .whoIsIndividualRepresenting()
            )

          val result = for {
            previousYtdLiability <-
              getPreviousYearToDateLiability(subscribed.sentReturns, subscribed.subscribedDetails.cgtReference)
            _                    <- EitherT(
                                      updateSession(sessionStore, request)(
                                        _.copy(
                                          journeyStatus = Some(
                                            StartingNewDraftReturn(
                                              subscribed.subscribedDetails,
                                              subscribed.ggCredId,
                                              subscribed.agentReferenceNumber,
                                              Right(IncompleteSingleDisposalTriageAnswers.empty),
                                              None,
                                              Some(
                                                PreviousReturnData(
                                                  subscribed.sentReturns,
                                                  previousYtdLiability,
                                                  None,
                                                  None
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
          } yield ()

          result.fold(
            { e =>
              logger.warn("Could not update session", e)
              errorHandler.errorResult()
            },
            _ => Redirect(redirectTo)
          )
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
            val result = for {
              previousYtdLiability <-
                getPreviousYearToDateLiability(subscribed.sentReturns, subscribed.subscribedDetails.cgtReference)
              _                    <- EitherT(
                                        updateSession(sessionStore, request)(
                                          _.copy(
                                            journeyStatus = Some(
                                              FillingOutReturn(
                                                subscribed.subscribedDetails,
                                                subscribed.ggCredId,
                                                subscribed.agentReferenceNumber,
                                                draftReturn,
                                                Some(
                                                  PreviousReturnData(
                                                    subscribed.sentReturns,
                                                    previousYtdLiability,
                                                    None,
                                                    None
                                                  )
                                                ),
                                                None
                                              )
                                            )
                                          )
                                        )
                                      )
            } yield ()
            result.fold(
              { e =>
                logger.warn("Could not update session", e)
                errorHandler.errorResult()
              },
              _ => Redirect(returns.routes.TaskListController.taskList())
            )

          }
      }(withUplift = false)
    }

  def viewSentReturn(submissionId: String): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withSubscribedUser { case (_, subscribed) =>
        subscribed.sentReturns
          .find(_.submissionId === submissionId)
          .fold[Future[Result]] {
            logger.warn(
              s"Could not find return with submission id $submissionId for cgt reference ${subscribed.subscribedDetails.cgtReference.value}"
            )
            NotFound
          } { returnSummary =>
            val result = for {
              sentReturn           <- returnsService
                                        .displayReturn(
                                          subscribed.subscribedDetails.cgtReference,
                                          returnSummary.submissionId
                                        )
              previousYtdLiability <- getPreviousYearToDateLiability(
                                        subscribed.sentReturns,
                                        subscribed.subscribedDetails.cgtReference,
                                        Some(
                                          CompleteReturnWithSummary(
                                            sentReturn.completeReturn,
                                            returnSummary,
                                            sentReturn.returnType
                                          )
                                        )
                                      )
              _                    <- EitherT(
                                        updateSession(sessionStore, request)(
                                          _.copy(
                                            journeyStatus = Some(
                                              ViewingReturn(
                                                subscribed.subscribedDetails,
                                                subscribed.ggCredId,
                                                subscribed.agentReferenceNumber,
                                                sentReturn.completeReturn,
                                                sentReturn.returnType,
                                                returnSummary,
                                                Some(PreviousReturnData(subscribed.sentReturns, previousYtdLiability, None, None))
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
      }(withUplift = true)
    }

  def payTotalAmountLeftToPay(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withSubscribedUser { case (_, subscribed) =>
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

  def exitForMultipleDraftReturn(): Action[AnyContent] =
    authenticatedActionWithSessionData { implicit request =>
      Ok(multipleDraftExitPage(routes.HomePageController.homepage()))
    }

  private def getPreviousYearToDateLiability(
    previousSentReturns: List[ReturnSummary],
    cgtReference: CgtReference,
    returnData: Option[CompleteReturnWithSummary] = None
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, Option[AmountInPence]] = {
    def fromNonCalculatedYtdAnswers(a: CompleteNonCalculatedYTDAnswers): AmountInPence =
      a.yearToDateLiability.getOrElse(a.taxDue)

    def fromCompleteReturn(c: CompleteReturn): AmountInPence =
      c.fold(
        multiple => fromNonCalculatedYtdAnswers(multiple.yearToDateLiabilityAnswers),
        single => single.yearToDateLiabilityAnswers.fold(fromNonCalculatedYtdAnswers, _.taxDue),
        singleIndirect => fromNonCalculatedYtdAnswers(singleIndirect.yearToDateLiabilityAnswers),
        multipleIndirect => fromNonCalculatedYtdAnswers(multipleIndirect.yearToDateLiabilityAnswers),
        singleMixedUse => fromNonCalculatedYtdAnswers(singleMixedUse.yearToDateLiabilityAnswers)
      )

    val previousSentReturnsWithDates = previousSentReturns.map(r => r -> r.lastUpdatedDate.getOrElse(r.submissionDate))
    val latestReturnWithData         =
      previousSentReturnsWithDates.sortBy(_._2)(localDateOrder.toOrdering).lastOption

    latestReturnWithData match {
      case None                             => EitherT.pure(None)
      case Some((latestReturn, latestDate)) =>
        val moreThanOneReturnOnLatestDate =
          previousSentReturns
            .count(r => r.lastUpdatedDate.contains(latestDate) || localDateOrder.eqv(r.submissionDate, latestDate)) > 1

        if (moreThanOneReturnOnLatestDate)
          EitherT.pure(None)
        else
          returnData match {
            case Some(CompleteReturnWithSummary(completeReturn, summary, _))
                if summary.submissionId === latestReturn.submissionId =>
              EitherT.pure(Some(fromCompleteReturn(completeReturn)))

            case _ =>
              returnsService.displayReturn(cgtReference, latestReturn.submissionId).map { displayReturn =>
                Some(fromCompleteReturn(displayReturn.completeReturn))
              }
          }

    }

  }

  private def withSubscribedUser(
    f: (SessionData, Subscribed) => Future[Result]
  )(withUplift: Boolean)(implicit
    hc: HeaderCarrier,
    request: RequestWithSessionData[_]
  ): Future[Result] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some((s: SessionData, r: StartingNewDraftReturn)) if withUplift =>
        upliftToSubscribedAndThen(r, r.subscribedDetails.cgtReference) { case (r, draftReturns, sentReturns) =>
          Subscribed(
            r.subscribedDetails,
            r.ggCredId,
            r.agentReferenceNumber,
            draftReturns,
            sentReturns
          )
        }(f(s, _))

      case Some((s: SessionData, r: FillingOutReturn)) if withUplift =>
        upliftToSubscribedAndThen(r, r.subscribedDetails.cgtReference) { case (r, draftReturns, sentReturns) =>
          Subscribed(
            r.subscribedDetails,
            r.ggCredId,
            r.agentReferenceNumber,
            draftReturns,
            sentReturns
          )
        }(f(s, _))

      case Some((s: SessionData, r: JustSubmittedReturn)) if withUplift =>
        upliftToSubscribedAndThen(r, r.subscribedDetails.cgtReference) { case (r, draftReturns, sentReturns) =>
          Subscribed(
            r.subscribedDetails,
            r.ggCredId,
            r.agentReferenceNumber,
            draftReturns,
            sentReturns
          )
        }(f(s, _))

      case Some((s: SessionData, r: SubmitReturnFailed)) if withUplift =>
        upliftToSubscribedAndThen(r, r.subscribedDetails.cgtReference) { case (r, draftReturns, sentReturns) =>
          Subscribed(
            r.subscribedDetails,
            r.ggCredId,
            r.agentReferenceNumber,
            draftReturns,
            sentReturns
          )
        }(f(s, _))

      case Some((s: SessionData, r: ViewingReturn)) if withUplift =>
        upliftToSubscribedAndThen(r, r.subscribedDetails.cgtReference) { case (r, draftReturns, sentReturns) =>
          Subscribed(
            r.subscribedDetails,
            r.ggCredId,
            r.agentReferenceNumber,
            draftReturns,
            sentReturns
          )
        }(f(s, _))

      case Some((s: SessionData, r: Subscribed)) =>
        f(s, r)

      case _ =>
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
