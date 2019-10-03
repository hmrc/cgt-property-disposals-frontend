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
import org.scalacheck.ScalacheckShapeless._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.i18n.MessagesApi
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.CSRFTokenHelper._
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.SubscriptionStatus
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.SubscriptionStatus._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.GGCredId
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.IndividualName
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.SubscriptionService
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.Future

class SubscriptionControllerSpec extends ControllerSpec with AuthSupport with SessionSupport with ScalaCheckDrivenPropertyChecks with RedirectToStartBehaviour {

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

  val subscriptionDetails = sample[SubscriptionDetails]

  val sessionWithSubscriptionDetails =
    SessionData.empty.copy(journeyStatus = Some(SubscriptionReady(subscriptionDetails)))

  def mockSubscribe(expectedSubscriptionDetails: SubscriptionDetails)(response: Either[Error, SubscriptionResponse]) =
    (mockSubscriptionService
      .subscribe(_: SubscriptionDetails)(_: HeaderCarrier))
      .expects(expectedSubscriptionDetails, *)
      .returning(EitherT(Future.successful(response)))

  val name = sample[IndividualName]

  def redirectToStart(performAction: () => Future[Result]) =
    redirectToStartWhenInvalidJourney(
      performAction,
      {
        case _: SubscriptionReady => true
        case _ => false
      }
    )

  "The SubscriptionController" when {

    "handling requests to check subscription details" must {

      def performAction(): Future[Result] =
        controller.checkYourDetails()(requestWithCSRFToken)

      behave like redirectToStart(performAction)

      "show the check you details page" when {

        "there are subscription details in session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionWithSubscriptionDetails))))
          }

          val result = performAction()
          status(result)          shouldBe OK
          contentAsString(result) should include(message("subscription.title"))
        }

      }

    }

    "handling submitted confirmation of subscription details" must {

      def performAction(): Future[Result] =
        controller.checkYourDetailsSubmit()(requestWithCSRFToken)

      val subscriptionResponse = SubscriptionResponse("number")

      val sessionWithSubscriptionComplete =
        SessionData.empty.copy(
          journeyStatus = Some(SubscriptionComplete(subscriptionDetails, subscriptionResponse)))

      behave like redirectToStart(performAction)

      "return an error" when {

        "there is an error during subscription" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionWithSubscriptionDetails))))
            mockSubscribe(subscriptionDetails)(Left(Error(new Exception(""))))
          }

          checkIsTechnicalErrorPage(performAction())
        }

        "there is an error updating the session" in {
          inSequence {
            mockAuthWithNoRetrievals()
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
            mockAuthWithNoRetrievals()
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

      redirectToStartWhenInvalidJourney(
        performAction,
        {
          case _: SubscriptionComplete => true
          case _ => false
        }
      )

      "display the subscription confirmation page" when {

        "there is a subscription response and subscription details in session" in {
          val cgtReferenceNumber = UUID.randomUUID().toString
          val session = SessionData.empty.copy(
            journeyStatus =
              Some(SubscriptionComplete(subscriptionDetails, SubscriptionResponse(cgtReferenceNumber)))
          )

          inSequence {
            mockAuthWithNoRetrievals()
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
