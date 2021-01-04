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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.accounts

import com.google.inject.{Inject, Singleton}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.ViewConfig
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.Subscribed
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.SubscriptionDetail
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.SessionData
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.Future

@Singleton
class AccountController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  cc: MessagesControllerComponents,
  manageYourDetailsPage: views.html.account.manage_your_details,
  detailUpdatedPage: views.html.account.details_updated,
  signedOutPage: views.html.account.signed_out
)(implicit viewConfig: ViewConfig)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionUpdates
    with Logging {

  def manageYourDetails(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request: RequestWithSessionData[AnyContent] =>
      withSubscribedUser(request)((_, subscribed) => Ok(manageYourDetailsPage(subscribed.subscribedDetails)))
    }

  def signedOut(): Action[AnyContent] =
    Action(implicit request => Ok(signedOutPage()))

  def contactNameUpdated(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withSubscribedUser(request) { case _ =>
        Ok(detailUpdatedPage(SubscriptionDetail.ContactName))
      }
    }

  def contactEmailUpdated(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withSubscribedUser(request) { case _ =>
        Ok(detailUpdatedPage(SubscriptionDetail.Email))
      }
    }

  def contactAddressUpdated(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withSubscribedUser(request) { case _ =>
        Ok(detailUpdatedPage(SubscriptionDetail.Address))
      }
    }

  private def withSubscribedUser(request: RequestWithSessionData[_])(
    f: (SessionData, Subscribed) => Future[Result]
  ): Future[Result] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some((s: SessionData, r: Subscribed)) =>
        f(s, r)
      case _                                     =>
        Future.successful(
          SeeOther(
            uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.routes.StartController
              .start()
              .url
          )
        )
    }
}
