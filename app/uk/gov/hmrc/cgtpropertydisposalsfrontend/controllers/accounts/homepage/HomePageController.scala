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

import com.google.inject.Inject
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.triage
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, StartingNewDraftReturn, Subscribed}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.IndividualTriageAnswers.IncompleteIndividualTriageAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{SessionData, UserType}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging.LoggerOps
import uk.gov.hmrc.cgtpropertydisposalsfrontend.{controllers, views}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

class HomePageController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  errorHandler: ErrorHandler,
  sessionStore: SessionStore,
  cc: MessagesControllerComponents,
  manageYourDetailsPage: views.html.account.manage_your_details,
  homePage: views.html.account.home,
  privateBetaHomePage: views.html.account.home_private_beta,
  detailUpdatedPage: views.html.account.details_updated,
  signedOutPage: views.html.account.signed_out
)(implicit viewConfig: ViewConfig, ec: ExecutionContext)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionUpdates
    with Logging {

  // homepage for after private beta: includes functionality to do with returns
  def homepage(): Action[AnyContent] = authenticatedActionWithSessionData.async {
    implicit request: RequestWithSessionData[AnyContent] =>
      withSubscribedUser { (_, subscribed) =>
        Ok(homePage(subscribed.subscribedDetails))
      }(withUplift = true)
  }

  def startNewReturn(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withSubscribedUser { (_, subscribed) =>
      request.userType match {
        case Some(UserType.Individual) =>
          updateSession(sessionStore, request)(
            _.copy(
              journeyStatus = Some(
                StartingNewDraftReturn(
                  subscribed.subscribedDetails,
                  subscribed.ggCredId,
                  subscribed.agentReferenceNumber,
                  IncompleteIndividualTriageAnswers.empty
                )
              )
            )
          ).map {
            case Left(e) =>
              logger.warn("Could not update session", e)
              errorHandler.errorResult()

            case Right(_) =>
              Redirect(triage.routes.CanTheyUseOurServiceController.whoIsIndividualRepresenting())
          }

        case other =>
          logger.warn(s"Start a new return for user type: $other is not supported")
          errorHandler.errorResult()
      }
    }(withUplift = false)
  }

  private def withSubscribedUser(
    f: (SessionData, Subscribed) => Future[Result]
  )(withUplift: Boolean)(implicit hc: HeaderCarrier, request: RequestWithSessionData[_]): Future[Result] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some((s: SessionData, r: StartingNewDraftReturn)) if withUplift =>
        // TODO: get draft returns
        upliftToSubscribedAndThen(r)(r =>
          Subscribed(r.subscribedDetails, r.ggCredId, r.agentReferenceNumber, List.empty)
        )(f(s, _))

      case Some((s: SessionData, r: FillingOutReturn)) if withUplift =>
        upliftToSubscribedAndThen(r)(r =>
          Subscribed(r.subscribedDetails, r.ggCredId, r.agentReferenceNumber, List.empty)
        )(f(s, _))

      case Some((s: SessionData, r: Subscribed)) =>
        f(s, r)

      case _ =>
        Redirect(controllers.routes.StartController.start().url)
    }

  private def upliftToSubscribedAndThen[J](journey: J)(uplift: J => Subscribed)(
    f: Subscribed => Future[Result]
  )()(implicit hc: HeaderCarrier, request: RequestWithSessionData[_]): Future[Result] = {
    val subscribed = uplift(journey)
    updateSession(sessionStore, request)(_.copy(journeyStatus = Some(subscribed))).flatMap {
      case Left(e) =>
        logger.warn("Could not update session", e)
        errorHandler.errorResult()

      case Right(_) =>
        f(subscribed)
    }
  }

}
