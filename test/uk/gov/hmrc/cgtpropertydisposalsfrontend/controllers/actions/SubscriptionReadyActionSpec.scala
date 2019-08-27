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

import play.api.i18n.MessagesApi
import play.api.mvc.Results.Ok
import play.api.mvc.{MessagesRequest, Result}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.ErrorHandler
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{ControllerSpec, SessionSupport, routes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.SubscriptionStatus.{SubscriptionComplete, SubscriptionReady}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class SubscriptionReadyActionSpec extends ControllerSpec with SessionSupport {

  lazy val action =
    new SubscriptionReadyAction(mockSessionStore, instanceOf[ErrorHandler])

  "SubscriptionDetailsAction" must {

    val subscriptionDetails = sample[SubscriptionDetails]

    def performAction(sessionData: SessionData, requestUrl: String = "/"): Future[Result] = {
      val messagesRequest = new MessagesRequest(FakeRequest("GET", requestUrl), instanceOf[MessagesApi])
      val authenticatedRequest = AuthenticatedRequest(messagesRequest)

      action.invokeBlock(
        authenticatedRequest, { r: RequestWithSubscriptionReady[_] =>
          r.sessionData       shouldBe sessionData
          r.subscriptionReady shouldBe SubscriptionReady(subscriptionDetails)
          r.messagesApi shouldBe messagesRequest.messagesApi
          Future.successful(Ok)
        }
      )
    }

    "return an error if there is an error getting session data" in {
      mockGetSession(Future.successful(Left(Error(new Exception("Oh no!")))))

      checkIsTechnicalErrorPage(performAction(sample[SessionData]))
    }

    "redirect to the start journey endpoint" when {

      "there is no session data in store" in {
        mockGetSession(Future.successful(Right(None)))

        checkIsRedirect(performAction(sample[SessionData]), routes.StartController.start())
      }

      "there is no subscription details in the session data" in {
        mockGetSession(Future.successful(Right(Some(SessionData.empty))))

        checkIsRedirect(performAction(sample[SessionData]), routes.StartController.start())
      }

    }

    "redirect to the subscribed page" when {

      "there are subscription details and a subscription response in session and the request is not " +
        "for the subscribed page" in {
        val sessionData = SessionData.empty.copy(
          subscriptionStatus = Some(SubscriptionComplete(subscriptionDetails, SubscriptionResponse("number")))
        )

        mockGetSession(Future.successful(Right(Some(sessionData))))

        checkIsRedirect(performAction(sessionData), routes.SubscriptionController.subscribed())
      }

    }

    "perform the action with the subscription details" when {

      "the subscription details exist and no subscription response exists" in {
        val sessionData = SessionData.empty.copy(
          subscriptionStatus = Some(SubscriptionReady(subscriptionDetails))
        )

        mockGetSession(Future.successful(Right(Some(sessionData))))

        status(performAction(sessionData)) shouldBe OK
      }

    }
  }

}
