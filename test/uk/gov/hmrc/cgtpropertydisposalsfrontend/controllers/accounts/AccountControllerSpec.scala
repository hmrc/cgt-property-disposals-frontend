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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.accounts

import uk.gov.hmrc.cgtpropertydisposalsfrontend.SampledScalaCheck
import play.api.i18n.MessagesApi
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.Subscribed
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.SessionData
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.JourneyStatusGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore

import scala.concurrent.Future
class AccountControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with SampledScalaCheck
    with RedirectToStartBehaviour {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore)
    )

  private val controller = instanceOf[AccountController]

  implicit val messagesApi: MessagesApi = controller.messagesApi

  private val subscribed = sample[Subscribed]

  def redirectToStartBehaviour(performAction: () => Future[Result]): Unit =
    redirectToStartWhenInvalidJourney(
      performAction,
      {
        case _: Subscribed => true
        case _             => false
      }
    )

  "The Account Controller" when {

    "handling requests signed out" must {

      def performAction(): Future[Result] =
        controller.signedOut()(FakeRequest())

      "display the signed out page" in {

        val result = performAction()
        status(result)        shouldBe OK
        contentAsString(result) should include(
          messageFromMessageKey("signed-out.title")
        )
      }

    }

    "handling requests for manage your details" must {

      def performAction(): Future[Result] =
        controller.manageYourDetails()(FakeRequest())
      behave like redirectToStartBehaviour(() => performAction())

      "display the manage your details page" in {
        val sessionData =
          SessionData.empty.copy(journeyStatus = Some(subscribed))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionData)
        }

        val result = performAction()
        status(result)        shouldBe OK
        contentAsString(result) should include(
          messageFromMessageKey("account.manageYourDetails.p")
        )
      }

    }

    "handling requests for the address changed page" must {
      def performAction(): Future[Result] =
        controller.contactAddressUpdated()(FakeRequest())

      behave like redirectToStartBehaviour(() => performAction())

      "display the page" in {
        val sessionData =
          SessionData.empty.copy(journeyStatus = Some(subscribed))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionData)
        }

        val result = performAction()
        status(result)        shouldBe OK
        contentAsString(result) should include(
          messageFromMessageKey("account.manageYourDetails.Address.changed")
        )
      }

    }

    "handling requests for the email changed page" must {
      def performAction(): Future[Result] =
        controller.contactEmailUpdated()(FakeRequest())

      behave like redirectToStartBehaviour(() => performAction())

      "display the page" in {
        val sessionData =
          SessionData.empty.copy(journeyStatus = Some(subscribed))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionData)
        }

        val result = performAction()
        status(result)        shouldBe OK
        contentAsString(result) should include(
          messageFromMessageKey("account.manageYourDetails.Email.changed")
        )
      }

    }

    "handling requests for the name changed page" must {
      def performAction(): Future[Result] =
        controller.contactNameUpdated()(FakeRequest())

      behave like redirectToStartBehaviour(() => performAction())

      "display the page" in {
        val sessionData =
          SessionData.empty.copy(journeyStatus = Some(subscribed))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionData)
        }

        val result = performAction()
        status(result)        shouldBe OK
        contentAsString(result) should include(
          messageFromMessageKey("account.manageYourDetails.ContactName.changed")
        )
      }

    }

  }
}
