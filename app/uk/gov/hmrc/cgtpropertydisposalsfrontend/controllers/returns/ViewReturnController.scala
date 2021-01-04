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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns

import cats.instances.future._
import cats.instances.string._
import cats.syntax.eq._
import com.google.inject.{Inject, Singleton}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.accounts.homepage.{routes => homeRoutes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.acquisitiondetails.RebasingEligibilityUtil
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.amend.{routes => amendRoutes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{SessionUpdates, routes => baseRoutes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.CompleteReturnWithSummary
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{StartingToAmendReturn, ViewingReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.PaymentsService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class ViewReturnController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  errorHandler: ErrorHandler,
  paymentsService: PaymentsService,
  sessionStore: SessionStore,
  cc: MessagesControllerComponents,
  viewReturnPage: views.html.returns.view_return,
  rebasingEligibilityUtil: RebasingEligibilityUtil
)(implicit viewConfig: ViewConfig, ec: ExecutionContext)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionUpdates
    with Logging {

  def displayReturn(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withViewingReturn() {
        case ViewingReturn(
              subscribedDetails,
              _,
              _,
              sentReturn,
              returnType,
              returnSummary,
              _
            ) =>
          Ok(
            viewReturnPage(
              sentReturn,
              returnSummary,
              rebasingEligibilityUtil,
              subscribedDetails,
              sentReturn.representativeType,
              sentReturn.isIndirectDisposal,
              returnType
            )
          )
      }
    }

  def startAmendingReturn(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withViewingReturn() { viewingReturn =>
        val newJourneyStatus = StartingToAmendReturn(
          viewingReturn.subscribedDetails,
          viewingReturn.ggCredId,
          viewingReturn.agentReferenceNumber,
          CompleteReturnWithSummary(
            viewingReturn.completeReturn,
            viewingReturn.returnSummary,
            viewingReturn.returnType
          ),
          viewingReturn.returnType.isFirstReturn,
          viewingReturn.previousSentReturns,
          None
        )

        updateSession(sessionStore, request)(_.copy(journeyStatus = Some(newJourneyStatus))).map {
          case Left(e)  =>
            logger.warn("Could not start amending a return", e)
            errorHandler.errorResult()
          case Right(_) =>
            Redirect(amendRoutes.AmendReturnController.checkYourAnswers())
        }
      }
    }

  def payCharge(chargeReference: String): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withViewingReturn() { case ViewingReturn(subscribedDetails, _, _, _, _, returnSummary, _) =>
        val cgtReference = subscribedDetails.cgtReference
        val details      =
          s"(chargeReference, cgtReference, submissionId) = ($chargeReference, $cgtReference, ${returnSummary.submissionId})"

        returnSummary.charges
          .find(_.chargeReference === chargeReference)
          .fold[Future[Result]] {
            logger.warn(
              s"Could not find charge with charge reference '$chargeReference' for $details"
            )
            NotFound
          } { charge =>
            paymentsService
              .startPaymentJourney(
                cgtReference,
                Some(charge.chargeReference),
                charge.amount,
                homeRoutes.HomePageController.homepage(),
                routes.ViewReturnController.displayReturn()
              )
              .fold(
                { e =>
                  logger
                    .warn(s"Could not start payments journey for $details", e)
                  errorHandler.errorResult()
                },
                { paymentsJourney =>
                  logger.info(
                    s"Started payments journey with journey id ${paymentsJourney.journeyId} for $details}"
                  )
                  Redirect(paymentsJourney.nextUrl)
                }
              )
          }

      }
    }

  def withViewingReturn()(
    f: ViewingReturn => Future[Result]
  )(implicit request: RequestWithSessionData[_]): Future[Result] =
    request.sessionData.flatMap(_.journeyStatus) match {
      case Some(v: ViewingReturn) => f(v)

      case Some(s: StartingToAmendReturn) =>
        val journeyStatus = ViewingReturn(
          s.subscribedDetails,
          s.ggCredId,
          s.agentReferenceNumber,
          s.originalReturn.completeReturn,
          s.originalReturn.returnType,
          s.originalReturn.summary,
          s.previousSentReturns
        )

        updateSession(sessionStore, request)(_.copy(journeyStatus = Some(journeyStatus))).flatMap {
          case Left(e) =>
            logger.warn("Could not update session", e)
            errorHandler.errorResult()

          case Right(_) =>
            f(journeyStatus)
        }

      case _ => Redirect(baseRoutes.StartController.start())
    }

}
