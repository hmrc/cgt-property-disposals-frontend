/*
 * Copyright 2023 HM Revenue & Customs
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
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.SubscriptionStatus._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.IdGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.OnboardingDetailsGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.SessionDataGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.GGCredId
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.SubscriptionDetails

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class SubscriptionReadyActionSpec extends ControllerSpec with SessionSupport {

  lazy val action =
    new SubscriptionReadyAction(mockSessionStore, instanceOf[ErrorHandler])

  implicit lazy val messagesApi: MessagesApi = instanceOf[MessagesApi]

  "SubscriptionDetailsAction" must {

    val subscriptionDetails = sample[SubscriptionDetails]

    val ggCredId = sample[GGCredId]

    def performAction(
      sessionData: SessionData,
      requestUrl: String = "/"
    ): Future[Result] = {
      val messagesRequest      =
        new MessagesRequest(FakeRequest("GET", requestUrl), messagesApi)
      val authenticatedRequest = AuthenticatedRequest(messagesRequest)

      action.invokeBlock(
        authenticatedRequest,
        { (r: RequestWithSubscriptionReady[?]) =>
          r.sessionData       shouldBe sessionData
          r.subscriptionReady shouldBe SubscriptionReady(
            subscriptionDetails,
            ggCredId
          )
          r.messagesApi       shouldBe messagesRequest.messagesApi
          Future.successful(Ok)
        }
      )
    }

    "return an error if there is an error getting session data" in {
      mockGetSession(Left(Error(new Exception("Oh no!"))))

      checkIsTechnicalErrorPage(performAction(sample[SessionData]))
    }

    "redirect to the start journey endpoint" when {

      "there is no session data in store" in {
        mockGetSession(Right(None))

        checkIsRedirect(
          performAction(sample[SessionData]),
          routes.StartController.start()
        )
      }

      "there is no subscription details in the session data" in {
        mockGetSession(SessionData.empty)

        checkIsRedirect(
          performAction(sample[SessionData]),
          routes.StartController.start()
        )
      }

    }

    "perform the action with the subscription details" when {

      "the session data indicates that subscription is ready" in {
        val sessionData = SessionData.empty.copy(
          journeyStatus = Some(SubscriptionReady(subscriptionDetails, ggCredId))
        )

        mockGetSession(sessionData)

        status(performAction(sessionData)) shouldBe OK
      }

    }
  }

}
