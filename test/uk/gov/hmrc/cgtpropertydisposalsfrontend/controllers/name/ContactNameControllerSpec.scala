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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.name

import org.scalacheck.ScalacheckShapeless._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.i18n.MessagesApi
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.CSRFTokenHelper._
import play.api.test.FakeRequest
import play.api.test.Helpers._
import shapeless.{Lens, lens}
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, ControllerSpec, RedirectToStartBehaviour, SessionSupport}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.SubscriptionStatus.SubscriptionReady
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.ContactName
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, SessionData, sample}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore

import scala.concurrent.Future

class ContactNameControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with RedirectToStartBehaviour
    with ScalaCheckDrivenPropertyChecks {

  override val overrideBindings: List[GuiceableModule] =
    List(
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore)
    )

  lazy val controller = instanceOf[ContactNameController]

  lazy implicit val messagesApi: MessagesApi = controller.messagesApi

  def redirectToStartBehaviour(performAction: () => Future[Result]): Unit =
    redirectToStartWhenInvalidJourney(
      performAction,
      {
        case _: SubscriptionReady => true
        case _ => false
      }
    )

  "ContactNameController" when {

    "handling requests to display the change contact name page" must {

      def performAction(): Future[Result] = controller.enterContactName()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      "display the page" when {

        "the user is ready for subscription" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              Future.successful(Right(Some(SessionData.empty.copy(journeyStatus = Some(sample[SubscriptionReady])))))
            )
          }

          val result = performAction()
          status(result)          shouldBe OK
          contentAsString(result) should include(message ("contactName.title"))
        }

      }

    }

    "handling submitted contact names" must {
      def performAction(formData: Seq[(String,String)]): Future[Result] =
        controller.enterContactNameSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*).withCSRFToken)

      val subscriptionReadyContactNameLens: Lens[SubscriptionReady,ContactName] =
        lens[SubscriptionReady].subscriptionDetails.contactName

      val previousSubscriptionReady =
        subscriptionReadyContactNameLens.set(sample[SubscriptionReady])(ContactName("Nob"))

      val previousSessionData =
        SessionData.empty.copy(journeyStatus = Some(previousSubscriptionReady))

      val validContactName = ContactName("Bob")

      val updatedSessionData = SessionData.empty.copy(
        journeyStatus = Some(
          subscriptionReadyContactNameLens.set(previousSubscriptionReady)(validContactName)
        ))

      behave like redirectToStartBehaviour(() => performAction(Seq.empty))

      "show a form error" when {

        def testFormError(formData: (String,String)*)(expectedErrorMessageKey: String): Unit =
          withClue(s"For form data [$formData]: ") {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(Future.successful(Right(Some(previousSessionData))))
            }

            val result = performAction(formData)
            val content = contentAsString(result)

            status(result) shouldBe BAD_REQUEST
            content should include(message("contactName.title"))
            content should include(message(expectedErrorMessageKey))
          }

        "the contact name is too long" in {
          testFormError("contactName" -> List.fill(106)('a').mkString(""))("contactName.error.pattern")
        }

        "the contact name contains an invalid character" in {
          testFormError("contactName" -> "Bo+b")("contactName.error.pattern")
        }

        "there is no contact name submitted" in {
          testFormError()("contactName.error.required")
        }

        "the contact name submitted is empty" in {
          testFormError("contactName" -> "")("contactName.error.required")
        }

      }

      "show an error page" when {

        "a valid contact name is submitted but the session cannot be updated" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(previousSessionData))))
            mockStoreSession(updatedSessionData)(Future.successful(Left(Error(""))))
          }

          checkIsTechnicalErrorPage(performAction(Seq("contactName" -> validContactName.value)))
        }

      }

      "redirect to the subscription check your details page" when {

        "a valid contact name is submitted and the session is updated" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(previousSessionData))))
            mockStoreSession(updatedSessionData)(Future.successful(Right(())))
          }

          checkIsRedirect(
            performAction(Seq("contactName" -> validContactName.value)),
            controllers.routes.SubscriptionController.checkYourDetails()
          )
        }

        "a valid contact name is submitted and it is the same as the one that is already in the session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(previousSessionData))))
          }

          checkIsRedirect(
            performAction(Seq("contactName" -> previousSubscriptionReady.subscriptionDetails.contactName.value)),
            controllers.routes.SubscriptionController.checkYourDetails()
          )
        }
      }

    }

  }

}
