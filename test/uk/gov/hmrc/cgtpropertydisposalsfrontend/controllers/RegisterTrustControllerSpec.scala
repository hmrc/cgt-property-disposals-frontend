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
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.SubscriptionStatus.{IndividualInsufficientConfidenceLevel, OrganisationUnregisteredTrust, SubscriptionComplete, SubscriptionMissingData, SubscriptionReady}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{BusinessPartnerRecord, Name, SessionData, SubscriptionDetails, SubscriptionResponse, sample}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore

import scala.concurrent.Future


class RegisterTrustControllerSpec extends ControllerSpec with AuthSupport with SessionSupport {

  override val overrideBindings =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore)
    )

  lazy val controller = instanceOf[RegisterTrustController]

  lazy implicit val messagesApi: MessagesApi = controller.messagesApi

  val bpr = sample[BusinessPartnerRecord]
  val name = sample[Name]
  val subscriptionDetails = sample[SubscriptionDetails]
  val subscriptionResponse = sample[SubscriptionResponse]

  "RegisterTrustController" when {

    "handling requests to display the register your trust page" must {

      def performAction(): Future[Result] = controller.registerYourTrust()(FakeRequest())

      "redirect to the start endpoint" when {

        "there is no session data" in {
          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(None)))
          }

          checkIsRedirect(performAction(), routes.StartController.start())
        }

        "there is no subscription status in session" in {
          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(SessionData.empty))))
          }

          checkIsRedirect(performAction(), routes.StartController.start())
        }

        "the session data indicates that an individual is missing data for subscription" in {
          val sessionData = SessionData.empty.copy(subscriptionStatus =
            Some(SubscriptionMissingData(bpr, name)))

          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionData))))
          }

          checkIsRedirect(performAction(), routes.StartController.start())
        }

      }

      "redirect to the check your details page" when {

        "the session data indicates the user is ready to subscribe" in {
          val sessionData = SessionData.empty.copy(subscriptionStatus =
            Some(SubscriptionReady(subscriptionDetails)))

          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionData))))
          }

          checkIsRedirect(performAction(), routes.SubscriptionController.checkYourDetails())
        }

      }

      "redirect to the subscription confirmation page" when {

        "the session data indicates the user has already subscribed" in {
          val sessionData = SessionData.empty.copy(subscriptionStatus =
            Some(SubscriptionComplete(subscriptionDetails, subscriptionResponse)))

          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionData))))
          }

          checkIsRedirect(performAction(), routes.SubscriptionController.subscribed())
        }
      }

      "redirect to the ask for NINO page" when {

        "the session data indicates the user is an individual who has insufficient confidence level" in {
          val sessionData = SessionData.empty.copy(subscriptionStatus =
            Some(IndividualInsufficientConfidenceLevel))

          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionData))))
          }

          checkIsRedirect(performAction(), routes.InsufficientConfidenceLevelController.doYouHaveNINO())
        }
      }

      "show the register your trust page" when {

        "the session data indicates the user is an organisation which is not associated with a registered trust" in {
          val sessionData = SessionData.empty.copy(subscriptionStatus = Some(OrganisationUnregisteredTrust))

          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionData))))
          }

          val result = performAction()
          status(result) shouldBe OK
          contentAsString(result) should include(message("registerTrust.title"))
        }

      }

    }

  }

}
