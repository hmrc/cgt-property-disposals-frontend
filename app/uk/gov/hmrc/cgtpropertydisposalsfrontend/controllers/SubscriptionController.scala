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

import cats.data.EitherT
import cats.instances.future._
import com.google.inject.{Inject, Singleton}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, SessionDataAction, SubscriptionDetailsAction, WithActions, WithSubscriptionDetailsActions}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.SubscriptionService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.ExecutionContext
import scala.util.Random

@Singleton
class SubscriptionController @Inject()(
  subscriptionService: SubscriptionService,
  sessionStore: SessionStore,
  errorHandler: ErrorHandler,
  cc: MessagesControllerComponents,
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val subscriptionDetailsAction: SubscriptionDetailsAction,
  checkYourDetailsPage: views.html.subscription.check_your_details,
  subscribedPage: views.html.subscription.subscribed
)(implicit viewConfig: ViewConfig, ec: ExecutionContext)
    extends FrontendController(cc)
    with WithSubscriptionDetailsActions
    with WithActions
    with Logging
    with SessionUpdates {

  def checkYourDetails(): Action[AnyContent] =
    authenticatedActionWithSubscriptionDetails { implicit request =>
      Ok(checkYourDetailsPage(request.subscriptionDetails))
    }

  def checkYourDetailsSubmit(): Action[AnyContent] =
    authenticatedActionWithSubscriptionDetails.async { implicit request =>
      val result = for {
        subscriptionResponse <- subscriptionService.subscribe(request.subscriptionDetails)
        _                    <- EitherT(updateSession(sessionStore, request)(_.copy(subscriptionResponse = Some(subscriptionResponse))))
      } yield subscriptionResponse

      result.fold(
        { e =>
          logger.warn("Could not subscribe", e)
          errorHandler.errorResult()
        }, { subscriptionResponse =>
          logger.info(s"Successfully subscribed with cgt id ${subscriptionResponse.cgtReferenceNumber}")
          SeeOther(routes.SubscriptionController.subscribed().url)
        }
      )
    }

  def subscribed(): Action[AnyContent] = authenticatedActionWithSessionData { implicit request =>
    (request.sessionData.flatMap(_.subscriptionDetails), request.sessionData.flatMap(_.subscriptionResponse)) match {
      case (None, _) => SeeOther(routes.StartController.start().url)
      case (Some(_), None) =>
        SeeOther(routes.SubscriptionController.checkYourDetails().url)
      case (Some(_), Some(subscriptionResponse)) =>
        Ok(subscribedPage(subscriptionResponse.cgtReferenceNumber))
    }

  }

}
