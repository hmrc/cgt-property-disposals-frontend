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

import java.util.UUID

import cats.data.EitherT
import java.time._
import play.api.i18n.MessagesApi
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.CSRFTokenHelper._
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.SubscriptionStatus.{SubscriptionComplete, SubscriptionReady}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.SubscriptionService
import uk.gov.hmrc.http.HeaderCarrier
import org.joda.time.{LocalDate => JodaLocalDate}
import scala.concurrent.Future

class SubscriptionControllerSpec extends ControllerSpec with AuthSupport with SessionSupport {

  val mockSubscriptionService = mock[SubscriptionService]

  override val overrideBindings =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore),
      bind[SubscriptionService].toInstance(mockSubscriptionService)
    )

  lazy val controller = instanceOf[SubscriptionController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  val requestWithCSRFToken = FakeRequest().withCSRFToken

  val nino        = NINO("AB123456C")
  val name        = Name("forename", "surname")
  val dateOfBirth = new JodaLocalDate(2000, 4, 10)

  val subscriptionDetails = sample[SubscriptionDetails]

  val sessionWithSubscriptionDetails =
    SessionData.empty.copy(subscriptionStatus = Some(SubscriptionReady(subscriptionDetails)))

  def mockSubscribe(expectedSubscriptionDetails: SubscriptionDetails)(response: Either[Error, SubscriptionResponse]) =
    (mockSubscriptionService
      .subscribe(_: SubscriptionDetails)(_: HeaderCarrier))
      .expects(expectedSubscriptionDetails, *)
      .returning(EitherT(Future.successful(response)))

  "The SubscriptionController" when {

    "handling requests to check subscription details" must {

      def performAction(): Future[Result] =
        controller.checkYourDetails()(requestWithCSRFToken)

      "show the check you details page" when {

        "there are subscription details in session" in {
          inSequence {
            mockAuthWithCl200AndWithAllRetrievals(nino.value, name, dateOfBirth)
            mockGetSession(Future.successful(Right(Some(sessionWithSubscriptionDetails))))
          }

          val result = performAction()
          status(result)          shouldBe OK
          contentAsString(result) should include(message("subscription.title"))
        }

      }

      "redirect to start a journey" when {

        "there are no subscription details in session" in {
          inSequence {
            mockAuthWithCl200AndWithAllRetrievals(nino.value, name, dateOfBirth)
            mockGetSession(Future.successful(Right(Some(SessionData.empty))))
          }

          checkIsRedirect(performAction(), routes.StartController.start())
        }

      }

    }

    "handling submitted confirmation of subscription details" must {

      def performAction(): Future[Result] =
        controller.checkYourDetailsSubmit()(requestWithCSRFToken)

      val subscriptionResponse = SubscriptionResponse("number")

      val sessionWithSubscriptionComplete =
        SessionData.empty.copy(
          subscriptionStatus = Some(SubscriptionComplete(subscriptionDetails, subscriptionResponse)))

      "redirect to the start endpoint if there is no subscription details in session" in {
        inSequence {
          mockAuthWithCl200AndWithAllRetrievals(nino.value, name, dateOfBirth)
          mockGetSession(Future.successful(Right(Some(SessionData.empty))))
        }

        checkIsRedirect(performAction(), routes.StartController.start())
      }

      "return an error" when {

        "there is an error during subscription" in {
          inSequence {
            mockAuthWithCl200AndWithAllRetrievals(nino.value, name, dateOfBirth)
            mockGetSession(Future.successful(Right(Some(sessionWithSubscriptionDetails))))
            mockSubscribe(subscriptionDetails)(Left(Error(new Exception(""))))
          }

          checkIsTechnicalErrorPage(performAction())
        }

        "there is an error updating the session" in {
          inSequence {
            mockAuthWithCl200AndWithAllRetrievals(nino.value, name, dateOfBirth)
            mockGetSession(Future.successful(Right(Some(sessionWithSubscriptionDetails))))
            mockSubscribe(subscriptionDetails)(Right(subscriptionResponse))
            mockStoreSession(sessionWithSubscriptionComplete)(Future.successful(Left(Error(""))))
          }

          checkIsTechnicalErrorPage(performAction())
        }

      }

      "redirect to the subscribed confirmation page" when {

        "subscription is successful and the session is updated successfully" in {
          inSequence {
            mockAuthWithCl200AndWithAllRetrievals(nino.value, name, dateOfBirth)
            mockGetSession(Future.successful(Right(Some(sessionWithSubscriptionDetails))))
            mockSubscribe(subscriptionDetails)(Right(subscriptionResponse))
            mockStoreSession(sessionWithSubscriptionComplete)(Future.successful(Right(())))
          }

          checkIsRedirect(performAction(), routes.SubscriptionController.subscribed())
        }

      }

    }

    "handling requests to display the subscribed page" must {

      def performAction(): Future[Result] =
        controller.subscribed()(FakeRequest())

      "redirect to the start endpoint" when {

        "there is no session data" in {
          inSequence {
            mockAuthWithCl200AndWithAllRetrievals(nino.value, name, dateOfBirth)
            mockGetSession(Future.successful(Right(None)))
          }

          checkIsRedirect(performAction(), routes.StartController.start())
        }

        "there are no subscription details in session" in {
          inSequence {
            mockAuthWithCl200AndWithAllRetrievals(nino.value, name, dateOfBirth)
            mockGetSession(Future.successful(Right(Some(SessionData.empty))))
          }

          checkIsRedirect(performAction(), routes.StartController.start())
        }

      }

      "redirect to the check your details page" when {

        "there is not subscription response in session but there are subscription details" in {
          inSequence {
            mockAuthWithCl200AndWithAllRetrievals(nino.value, name, dateOfBirth)
            mockGetSession(Future.successful(Right(Some(sessionWithSubscriptionDetails))))
          }

          checkIsRedirect(performAction(), routes.SubscriptionController.checkYourDetails())
        }
      }

      "display the subscription confirmation page" when {

        "there is a subscription response and subscription details in session" in {
          val cgtReferenceNumber = UUID.randomUUID().toString
          val session = SessionData.empty.copy(
            subscriptionStatus =
              Some(SubscriptionComplete(subscriptionDetails, SubscriptionResponse(cgtReferenceNumber)))
          )

          inSequence {
            mockAuthWithCl200AndWithAllRetrievals(nino.value, name, dateOfBirth)
            mockGetSession(Future.successful(Right(Some(session))))
          }

          val result = performAction()
          status(result)          shouldBe OK
          contentAsString(result) should include(cgtReferenceNumber)
          contentAsString(result) should include(message("subscribed.title"))

        }
      }

    }

  }

}
