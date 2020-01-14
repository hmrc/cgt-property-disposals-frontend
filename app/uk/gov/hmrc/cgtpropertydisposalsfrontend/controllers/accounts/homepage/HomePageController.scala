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
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.Subscribed
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{SessionData, UserType}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

class HomePageController @Inject()(
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
      withSubscribedUser(request) { (_, subscribed) =>
        Future.successful(Ok(homePage(subscribed.subscribedDetails)))
      }
  }

  // homepage for private beta: does not include any functionality to do with returns. This will
  // eventually be removed when we move out of private beta
  def privateBetaHomepage(): Action[AnyContent] = authenticatedActionWithSessionData.async {
    implicit request: RequestWithSessionData[AnyContent] =>
      withSubscribedUser(request) { (_, subscribed) =>
        Future.successful(Ok(privateBetaHomePage(subscribed.subscribedDetails)))
      }
  }

  def startNewReturn(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withSubscribedUser(request) { (_, _) =>
    request.userType match  {
      case Some(UserType.Individual) =>
        Future.successful(Redirect(controllers.returns.triage.routes.CanTheyUseOurServiceController.whoIsIndividualRepresenting()))

      case other =>
        logger.warn(s"Start a new return for user type: $other is not supported")
        errorHandler.errorResult()
    }
    }
  }

  private def withSubscribedUser(request: RequestWithSessionData[_])(
    f: (SessionData, Subscribed) => Future[Result]
  ): Future[Result] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some((s: SessionData, r: Subscribed)) =>
        f(s, r)
      case _ =>
        Future.successful(SeeOther(uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.routes.StartController.start().url))
    }

}
