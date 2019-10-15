/*
 * Copyright 2019 HM Revenue & Customs
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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers

import com.google.inject.{Inject, Singleton}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.ViewConfig
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedActionWithRetrievedData, RequestWithSessionDataAndRetrievedData, SessionDataActionWithRetrievedData, WithAuthRetrievalsAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.Subscribed
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.SubscriptionStatus.TryingToGetIndividualsFootprint
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.Future

@Singleton
class HomeController @Inject()(
  val authenticatedActionWithRetrievedData: AuthenticatedActionWithRetrievedData,
  val sessionDataActionWithRetrievedData: SessionDataActionWithRetrievedData,
  cc: MessagesControllerComponents,
  startPage: views.html.subscribed.start
)(implicit viewConfig: ViewConfig)
    extends FrontendController(cc)
    with WithAuthRetrievalsAndSessionDataAction
    with SessionUpdates
    with Logging {

  def start(): Action[AnyContent] = authenticatedActionWithRetrievedDataAndSessionData.async {
    implicit request: RequestWithSessionDataAndRetrievedData[AnyContent] =>
      withSubscribedUser(request) { status =>
        Ok(startPage())
      }
  }

  //TODO: how to handle the left case?
  private def withSubscribedUser(request: RequestWithSessionDataAndRetrievedData[_])(
    f: Either[TryingToGetIndividualsFootprint, Subscribed] => Future[Result]
  ): Future[Result] =
    request.sessionData.flatMap(_.journeyStatus) match {
      case Some(r: Subscribed) =>
        f(Right(r))
      case _ =>
        Future.successful(SeeOther(routes.StartController.start().url))
    }
}
