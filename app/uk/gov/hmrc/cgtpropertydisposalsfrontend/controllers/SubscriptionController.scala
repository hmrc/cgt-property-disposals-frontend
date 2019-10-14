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
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.SubscriptionStatus._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.SubscriptionService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.ExecutionContext

@Singleton
class SubscriptionController @Inject()(
  subscriptionService: SubscriptionService,
  sessionStore: SessionStore,
  errorHandler: ErrorHandler,
  cc: MessagesControllerComponents,
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val subscriptionDetailsAction: SubscriptionReadyAction,
  checkYourDetailsPage: views.html.subscription.check_your_details,
  subscribedPage: views.html.subscription.subscribed
)(implicit viewConfig: ViewConfig, ec: ExecutionContext)
    extends FrontendController(cc)
    with WithSubscriptionDetailsActions
    with WithAuthAndSessionDataAction
    with Logging
    with SessionUpdates {

  def checkYourDetails(): Action[AnyContent] =
    authenticatedActionWithSubscriptionReady { implicit request =>
      Ok(checkYourDetailsPage(request.subscriptionReady.subscriptionDetails))
    }

  def checkYourDetailsSubmit(): Action[AnyContent] =
    authenticatedActionWithSubscriptionReady.async { implicit request =>
      val details = request.subscriptionReady.subscriptionDetails
      val result = for {
        subscriptionResponse <- subscriptionService.subscribe(details)
        _ <- EitherT(
              updateSession(sessionStore, request)(
                _.copy(journeyStatus = Some(SubscriptionComplete(details, subscriptionResponse)))
              )
            )
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
    request.sessionData.flatMap(_.journeyStatus) match {
      case Some(SubscriptionComplete(_, complete)) => Ok(subscribedPage(complete.cgtReferenceNumber))
      case _                                       => Redirect(routes.StartController.start())
    }
  }

}
