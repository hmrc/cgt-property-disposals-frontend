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

import org.scalacheck.ScalacheckShapeless._
import play.api.i18n.MessagesApi
import play.api.mvc.Results.Ok
import play.api.mvc.{MessagesRequest, Result}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.ErrorHandler
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.{routes => onboardingRoutes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{ControllerSpec, SessionSupport}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.GGCredId
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.JourneyStatus.SubscriptionStatus.SubscriptionReady
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.SubscriptionDetails

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class SubscriptionReadyActionSpec extends ControllerSpec with SessionSupport {

  lazy val action =
    new SubscriptionReadyAction(mockSessionStore, instanceOf[ErrorHandler])

  "SubscriptionDetailsAction" must {

    val subscriptionDetails = sample[SubscriptionDetails]

    val ggCredId = sample[GGCredId]

    def performAction(sessionData: SessionData, requestUrl: String = "/"): Future[Result] = {
      val messagesRequest      = new MessagesRequest(FakeRequest("GET", requestUrl), instanceOf[MessagesApi])
      val authenticatedRequest = AuthenticatedRequest(messagesRequest)

      action.invokeBlock(
        authenticatedRequest, { r: RequestWithSubscriptionReady[_] =>
          r.sessionData       shouldBe sessionData
          r.subscriptionReady shouldBe SubscriptionReady(subscriptionDetails, ggCredId)
          r.messagesApi       shouldBe messagesRequest.messagesApi
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

        checkIsRedirect(performAction(sample[SessionData]), onboardingRoutes.StartController.start())
      }

      "there is no subscription details in the session data" in {
        mockGetSession(Future.successful(Right(Some(SessionData.empty))))

        checkIsRedirect(performAction(sample[SessionData]), onboardingRoutes.StartController.start())
      }

    }

    "perform the action with the subscription details" when {

      "the session data indicates that subscription is ready" in {
        val sessionData = SessionData.empty.copy(
          journeyStatus = Some(SubscriptionReady(subscriptionDetails, ggCredId))
        )

        mockGetSession(Future.successful(Right(Some(sessionData))))

        status(performAction(sessionData)) shouldBe OK
      }

    }
  }

}
