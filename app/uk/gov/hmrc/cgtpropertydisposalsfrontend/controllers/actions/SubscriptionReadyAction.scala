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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions

import cats.syntax.either._
import com.google.inject.{Inject, Singleton}
import play.api.i18n.MessagesApi
import play.api.mvc.Results.Redirect
import play.api.mvc._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.ErrorHandler
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.SessionData
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.SubscriptionStatus
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.SubscriptionStatus.SubscriptionReady
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging._
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.HeaderCarrierConverter

import scala.concurrent.{ExecutionContext, Future}

final case class RequestWithSubscriptionReady[A](
  subscriptionReady: SubscriptionReady,
  sessionData: SessionData,
  authenticatedRequest: AuthenticatedRequest[A]
) extends WrappedRequest[A](authenticatedRequest)
    with PreferredMessagesProvider {
  override def messagesApi: MessagesApi =
    authenticatedRequest.request.messagesApi
}

@Singleton
class SubscriptionReadyAction @Inject()(sessionStore: SessionStore, errorHandler: ErrorHandler)(
  implicit ec: ExecutionContext
) extends ActionRefiner[AuthenticatedRequest, RequestWithSubscriptionReady]
    with Logging {

  override protected def executionContext: ExecutionContext = ec

  override protected def refine[A](
    request: AuthenticatedRequest[A]
  ): Future[Either[Result, RequestWithSubscriptionReady[A]]] = {
    implicit val hc: HeaderCarrier = HeaderCarrierConverter
      .fromHeadersAndSession(request.headers, Some(request.session))

    sessionStore
      .get()
      .map(
        _.leftMap { e =>
          logger.warn("Could not get session data", e)
          errorHandler.errorResult(None)(request)
        }.flatMap { maybeSessionData =>
          (maybeSessionData, maybeSessionData.flatMap(_.journeyStatus)) match {
            case (Some(sessionData), Some(ready: SubscriptionStatus.SubscriptionReady)) =>
              Right(RequestWithSubscriptionReady(ready, sessionData, request))

            case (_, _) =>
              Left(Redirect(controllers.routes.StartController.start()))
          }
        }
      )
  }

}
