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

import cats.Eq
import cats.syntax.eq._
import org.scalacheck.ScalacheckShapeless._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.i18n.MessagesApi
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.CSRFTokenHelper._
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.SubscriptionStatus.IndividualWithInsufficientConfidenceLevel
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{HasSAUTR, Name, SessionData, SubscriptionStatus, sample}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore

import scala.concurrent.Future

class RegistrationControllerSpec
  extends ControllerSpec with AuthSupport with SessionSupport with ScalaCheckDrivenPropertyChecks {

  override val overrideBindings =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore)
    )

  lazy val controller = instanceOf[RegistrationController]

  implicit val messagesApi: MessagesApi = controller.messagesApi

  implicit val subscriptionStatusEq: Eq[SubscriptionStatus] = Eq.fromUniversalEquals

  val name = sample[Name]

  "RegistrationController" when {

    def redirectToStartBehaviour(performAction: () => Future[Result]): Unit = {

      "redirect to the start endpoint" when {

        "the session data indicates that user should not see page" in {
          def isValidStatus(subscriptionStatus: SubscriptionStatus): Boolean = subscriptionStatus match {
            case IndividualWithInsufficientConfidenceLevel(Some(false), Some(HasSAUTR(None)), _, _) => true
            case _ => false
          }

          forAll { subscriptionStatus: SubscriptionStatus =>
            whenever(!isValidStatus(subscriptionStatus)) {
              val sessionData = SessionData.empty.copy(subscriptionStatus = Some(subscriptionStatus))

              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(Future.successful(Right(Some(sessionData))))
              }

              checkIsRedirect(performAction(), routes.StartController.start())
            }
          }

        }

      }
    }

    "handling requests to show the registration start page" must {

      def performAction(): Future[Result] =
        controller.startRegistration()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      "show the page" when {

        "the session data indicates that the user has no digital footprint and " +
          "they have indicated that they have no NINO or SA UTR" in {
          val sessionData =
            SessionData.empty.copy(subscriptionStatus = Some(
              IndividualWithInsufficientConfidenceLevel(Some(false), Some(HasSAUTR(None)), name, None))
              )

          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionData))))
          }

          val result = performAction()
          status(result) shouldBe OK
          contentAsString(result) should include(message("registration.start.title"))
        }

      }

    }

    "handling requests to show the select entity type page" must {

      def performAction(): Future[Result] =
        controller.selectEntityType()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      "show the page" when {
        "the session data indicates that the user has no digital footprint and " +
          "the user has opted to start registration" in {
          val sessionData =
            SessionData.empty.copy(subscriptionStatus = Some(
              IndividualWithInsufficientConfidenceLevel(Some(false), Some(HasSAUTR(None)), name, None))
            )

          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionData))))
          }

          val result = performAction()
          status(result) shouldBe OK
          contentAsString(result) should include(message("entityType.title"))
        }
      }
    }

    "handling requests to submit the selected entity type" must {

      def performAction(formData: (String, String)*): Future[Result] =
        controller.selectEntityTypeSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*).withCSRFToken)

      val sessionData =
        SessionData.empty.copy(subscriptionStatus = Some(
          IndividualWithInsufficientConfidenceLevel(Some(false), Some(HasSAUTR(None)), name, None))
        )

      behave like redirectToStartBehaviour(() => performAction())

      "show the page with errors" when {
        "the request submits no selection" in {
          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionData))))
          }

          val result = performAction()
          status(result) shouldBe BAD_REQUEST
          contentAsString(result) should include(message("entityType.error.required"))
        }

        "the request submits an invalid value" in {
          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionData))))
          }
          val result = performAction("entityType" -> "2")
          status(result) shouldBe BAD_REQUEST
          contentAsString(result) should include(message("entityType.invalid"))
        }
      }

      "redirect to the wrong gg account page" when {
        "the request selects trust" in {
          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionData))))
          }
          val result = performAction("entityType" -> "1")
          checkIsRedirect(result, routes.RegistrationController.wrongGGAccountForTrusts())
        }
      }

      "continue the registration journey" when {
        "the request selects individual" in {
          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionData))))
          }
          val result = performAction("entityType" -> "0")
          checkIsRedirect(result, routes.RegistrationController.enterName())
        }
      }

    }

    "handling requests to view the enter name page" must {
      def performAction(): Future[Result] =
        controller.enterName()(FakeRequest())

      val sessionData =
        SessionData.empty.copy(subscriptionStatus = Some(
          IndividualWithInsufficientConfidenceLevel(Some(false), Some(HasSAUTR(None)), name, None))
        )

      behave like redirectToStartBehaviour(performAction)

      "show the page" when {
        "the endpoint is requested" in {
          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionData))))
          }
          val result = performAction()
          status(result) shouldBe OK
          contentAsString(result) should include(message("enterName.title"))
        }
      }
    }

    "handling requests to submit the enter name page" must {

      def performAction(formData: (String, String)*): Future[Result] =
        controller.enterNameSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*).withCSRFToken)

      val sessionData =
        SessionData.empty.copy(subscriptionStatus = Some(
          IndividualWithInsufficientConfidenceLevel(Some(false), Some(HasSAUTR(None)), name, None))
        )

      behave like redirectToStartBehaviour(() => performAction())

      "be successful" when {
        "the request submits valid values" in {
          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionData))))
          }
          val result = performAction("firstName" -> "Bob", "lastName" -> "Smith")
          status(result) shouldBe OK
        }
        "request submits valid values with leading and trailing spaces" in {
          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionData))))
          }
          val result = performAction("firstName" -> " Bob ", "lastName" -> " Smith ")
          status(result) shouldBe OK
        }
      }

      "show the page with errors" when {
        "the request submits no selection" in {
          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionData))))
          }
          val result = performAction()
          status(result) shouldBe BAD_REQUEST
          val resultAsString = contentAsString(result)
          resultAsString should include(message("firstName.error.required"))
          resultAsString should include(message("lastName.error.required"))
        }
        "the request submits a first name that is too long" in {
          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionData))))
          }
          val result = performAction("firstName" -> "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", "lastName" -> "Smith")
          status(result) shouldBe BAD_REQUEST
          contentAsString(result) should include(message("firstName.error.tooLong"))
        }
        "the request submits a first name with illegal characters" in {
          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionData))))
          }
          val result = performAction("firstName" -> "999", "lastName" -> "Smith")
          status(result) shouldBe BAD_REQUEST
          contentAsString(result) should include(message("firstName.error.pattern"))
        }
        "the request submits a last name that is too long" in {
          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionData))))
          }
          val result = performAction("firstName" -> "Bob", "lastName" -> "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
          status(result) shouldBe BAD_REQUEST
          contentAsString(result) should include(message("lastName.error.tooLong"))
        }
        "the request submits a last name with illegal characters" in {
          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionData))))
          }
          val result = performAction("firstName" -> "Bob", "lastName" -> "i99")
          status(result) shouldBe BAD_REQUEST
          contentAsString(result) should include(message("lastName.error.pattern"))
        }
      }
    }

    "handling requests to view the wrong gg account page" must {

      def performAction(): Future[Result] =
        controller.wrongGGAccountForTrusts()(FakeRequest())

      val sessionData =
        SessionData.empty.copy(subscriptionStatus = Some(
          IndividualWithInsufficientConfidenceLevel(Some(false), Some(HasSAUTR(None)), name, None))
        )

      behave like redirectToStartBehaviour(performAction)

      "show the page" when {
        "the endpoint is requested" in {
          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionData))))
          }
          val result = performAction()
          status(result) shouldBe OK
          contentAsString(result) should include(message("wrongAccountForTrusts.title"))
        }
      }
    }

  }

}
