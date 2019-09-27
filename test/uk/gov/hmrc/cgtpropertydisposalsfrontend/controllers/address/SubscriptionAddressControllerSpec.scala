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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.address

import org.scalacheck.ScalacheckShapeless._
import play.api.i18n.MessagesApi
import play.api.mvc.Result
import play.api.test.CSRFTokenHelper._
import play.api.test.FakeRequest
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.SubscriptionStatus._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{RegistrationStatus, SubscriptionStatus}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.bpr.BusinessPartnerRecord
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.GGCredId
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{SessionData, SubscriptionDetails, sample}

import scala.concurrent.Future

class SubscriptionAddressControllerSpec extends AddressControllerSpec[SubscriptionReady] {

  val subscriptionDetails: SubscriptionDetails =
    sample[SubscriptionDetails].copy(address = address(1))

  val validJourneyStatus = SubscriptionReady(subscriptionDetails)

  lazy val controller = instanceOf[SubscriptionAddressController]

  lazy implicit val messagesApi: MessagesApi = controller.messagesApi

  override def updateAddress(journey: SubscriptionReady, address: Address): SubscriptionReady =
  journey.copy(subscriptionDetails = journey.subscriptionDetails.copy(address = address))

  def subscriptionDetailsBehavior(performAction: () => Future[Result]): Unit = {
    "redirect to the start endpoint" when {

      "there is no session data" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(Future.successful(Right(None)))
        }

        val result = performAction()
        checkIsRedirect(result, controllers.routes.StartController.start())
      }

      "there is no subscription details in session" in {
        val bpr = sample[BusinessPartnerRecord]
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            Future.successful(
              Right(Some(SessionData.empty.copy(journeyStatus = Some(SubscriptionMissingData(bpr)))))))
        }

        val result = performAction()
        checkIsRedirect(result, controllers.routes.StartController.start())
      }

    }

    "redirect to the do you have a nino page" when {

      "the session data indicates the user does not have sufficient confidence level" in {
        val session = SessionData.empty.copy(journeyStatus = Some(
          SubscriptionStatus.IndividualWithInsufficientConfidenceLevel(None, None, None, sample[GGCredId])
        ))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(Future.successful(Right(Some(session))))
        }

        val result = performAction()
        checkIsRedirect(result, controllers.routes.InsufficientConfidenceLevelController.doYouHaveNINO())
      }

    }

    "redirect to the register your trust page" when {

      "the session data indicates the user is an organisation without a registered trust associated with it" in {
        val session = SessionData.empty.copy(journeyStatus = Some(SubscriptionStatus.UnregisteredTrust))
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(Future.successful(Right(Some(session))))
        }

        val result = performAction()
        checkIsRedirect(result, controllers.routes.RegisterTrustController.registerYourTrust())
      }

    }

    "redirect to the registration start endpoint" when {

      "the session data indicates the user has started a registration journey" in {
        List(
          RegistrationStatus.IndividualWantsToRegisterTrust,
          sample[RegistrationStatus.IndividualSupplyingInformation]
        ).foreach { registrationStatus =>
          withClue(s"For registration status $registrationStatus: ") {
            val session = SessionData.empty.copy(journeyStatus = Some(registrationStatus))

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(Future.successful(Right(Some(session))))
            }

            checkIsRedirect(performAction(), controllers.routes.RegistrationController.startRegistration())
          }

        }

      }

    }

  }

  "AddressController" when {

    "handling requests to display the is UK page" must {
      def performAction(): Future[Result] = controller.isUk()(FakeRequest())

      behave like subscriptionDetailsBehavior(performAction)

      behave like displayIsUkBehaviour(performAction)
    }

    "handling requests to submit the is UK page" must {
      def performAction(formData: Seq[(String, String)]): Future[Result] =
        controller.isUkSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*).withCSRFToken)

      behave like subscriptionDetailsBehavior(() => performAction(Seq.empty))

      behave like submitIsUkBehaviour(
        performAction,
        routes.SubscriptionAddressController.enterPostcode(),
        routes.SubscriptionAddressController.enterNonUkAddress()
      )

    }

    "handling requests to display the enter UK address page" must {

      def performAction(): Future[Result] = controller.enterUkAddress()(FakeRequest())

      behave like subscriptionDetailsBehavior(performAction)

      behave like displayEnterUkAddressPage(performAction)

    }

    "handling submitted addresses from enter UK address page" must {

      def performAction(formData: Seq[(String, String)]): Future[Result] =
        controller.enterUkAddressSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*).withCSRFToken)

      behave like subscriptionDetailsBehavior(() => performAction(Seq.empty))

      behave like submitEnterUkAddress(
        performAction,
        controllers.routes.SubscriptionController.checkYourDetails()
      )

    }

    "handling requests to display the enter non UK address page" must {

      def performAction(): Future[Result] = controller.enterNonUkAddress()(FakeRequest())

      behave like subscriptionDetailsBehavior(performAction)

      behave like displayEnterNonUkPage(performAction)

    }

    "handling requests to submit the enter non UK address page" must {
      def performAction(formData: (String, String)*): Future[Result] =
        controller.enterNonUkAddressSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*).withCSRFToken)

      behave like subscriptionDetailsBehavior(() => performAction())

      behave like submitEnterNonUkAddress(performAction, controllers.routes.SubscriptionController.checkYourDetails())
    }

    "handling requests to display the enter postcode page" must {

      def performAction(): Future[Result] = controller.enterPostcode()(FakeRequest())

      behave like subscriptionDetailsBehavior(performAction)

      behave like enterPostcodePage(performAction)

    }

    "handling submitted postcodes and filters" must {

      def performAction(formData: Seq[(String, String)]): Future[Result] =
        controller.enterPostcodeSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*).withCSRFToken)

      behave like subscriptionDetailsBehavior(() => performAction(Seq.empty))

      behave like submitEnterPostcode(performAction, routes.SubscriptionAddressController.selectAddress())

    }

    "handling requests to display the select address page" must {

      def performAction(): Future[Result] =
        controller.selectAddress()(FakeRequest())

      behave like subscriptionDetailsBehavior(performAction)

      behave like displaySelectAddress(performAction, controllers.routes.SubscriptionController.checkYourDetails())

    }

    "handling submitted selected addresses" must {

      def performAction(formData: Seq[(String, String)]): Future[Result] =
        controller.selectAddressSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*).withCSRFToken)

      behave like subscriptionDetailsBehavior(() => performAction(Seq.empty))

      behave like submitSelectAddress(
        performAction,
        controllers.routes.SubscriptionController.checkYourDetails(),
        controllers.routes.SubscriptionController.checkYourDetails()
      )

      "not update the session" when {

        "the user selects an address which is already in their subscription details" in {
          val session = sessionWithValidJourneyStatus.copy(addressLookupResult = Some(addressLookupResult))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(session))))
          }

          val result = performAction(Seq("address-select" -> "0"))
          checkIsRedirect(result, controllers.routes.SubscriptionController.checkYourDetails())
        }

      }

    }
  }

}
