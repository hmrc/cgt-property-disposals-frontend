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

import play.api.i18n.MessagesApi
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.CSRFTokenHelper._
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{NINO, SessionData, SubscriptionDetails, sample}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore

import scala.concurrent.Future

class SubscriptionControllerSpec extends ControllerSpec with AuthSupport with SessionSupport {

  override val overrideBindings =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore)
    )

  lazy val controller = instanceOf[SubscriptionController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  val requestWithCSRFToken = FakeRequest().withCSRFToken

  def requestWithFormData(data: (String, String)*) = FakeRequest().withFormUrlEncodedBody(data: _*).withCSRFToken

  val nino = NINO("AB123456C")

  val subscriptionDetails = sample[SubscriptionDetails]

  "The SubscriptionController" when {

    "handling requests to check subscription details" must {

        def performAction(): Future[Result] =
          controller.checkYourDetails()(requestWithCSRFToken)

      "show the check you details page" when {

        "there are subscription details in session" in {
          inSequence {
            mockAuthWithCl200AndRetrievedNino(nino.value)
            mockGetSession(Future.successful(Right(Some(SessionData.empty.copy(subscriptionDetails = Some(subscriptionDetails))))))
          }

          val result = performAction()
          status(result) shouldBe OK
          contentAsString(result) should include(message("subscription.title"))
        }

      }

      "redirect to start a journey" when {

        "there are no subscription details in session" in {
          inSequence {
            mockAuthWithCl200AndRetrievedNino(nino.value)
            mockGetSession(Future.successful(Right(Some(SessionData.empty))))
          }

          checkIsRedirect(performAction(), routes.StartController.start())
        }

      }

    }

    "handling submitted confirmation of subscription details" must {

      "redirect to the start endpoint if there is no subscription details in session" in {
        inSequence {
          mockAuthWithCl200AndRetrievedNino(nino.value)
          mockGetSession(Future.successful(Right(Some(SessionData.empty))))
        }

        val result = controller.checkYourDetailsSubmit()(requestWithCSRFToken)
        checkIsRedirect(result, routes.StartController.start())
      }

      "handle the case when the user confirms their details" in {
        inSequence {
          mockAuthWithCl200AndRetrievedNino(nino.value)
          mockGetSession(Future.successful(Right(Some(SessionData.empty.copy(subscriptionDetails = Some(subscriptionDetails))))))
        }

        val result = controller.checkYourDetailsSubmit()(requestWithCSRFToken)
        checkIsRedirect(result, routes.SubscriptionController.subscribed())
      }

    }

  }

}

