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
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.RegistrationStatus.IndividualSupplyingInformation
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.SubscriptionStatus._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{RegistrationStatus, SubscriptionStatus}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.bpr.BusinessPartnerRecord
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.GGCredId
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Name, SessionData, sample}

import scala.concurrent.Future

class RegistrationAddressControllerSpec extends AddressControllerSpec[IndividualSupplyingInformation] {

  val validJourneyStatus = IndividualSupplyingInformation(Some(sample[Name]), None)

  lazy val controller = instanceOf[RegistrationAddressController]

  lazy implicit val messagesApi: MessagesApi = controller.messagesApi

  override def updateAddress(
    journey: IndividualSupplyingInformation,
    address: Address
  ): IndividualSupplyingInformation =
    journey.copy(address = Some(address))

  def commonBehaviour(performAction: () => Future[Result]): Unit = {
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
            Future.successful(Right(Some(SessionData.empty.copy(journeyStatus = Some(SubscriptionMissingData(bpr))))))
          )
        }

        val result = performAction()
        checkIsRedirect(result, controllers.routes.StartController.start())
      }

    }

    "redirect to the do you have a nino page" when {

      "the session data indicates the user does not have sufficient confidence level" in {
        val session = SessionData.empty.copy(
          journeyStatus = Some(
            SubscriptionStatus.IndividualWithInsufficientConfidenceLevel(None, None, None, sample[GGCredId])
          )
        )

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

      "the session data indicates the user has started a registration journey but should not " +
        "be entering an address yet" in {
        List(
          RegistrationStatus.IndividualWantsToRegisterTrust,
          RegistrationStatus.IndividualSupplyingInformation(None, None)
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

      behave like commonBehaviour(performAction)

      behave like displayIsUkBehaviour(performAction)
    }

    "handling requests to submit the is UK page" must {
      def performAction(formData: Seq[(String, String)]): Future[Result] =
        controller.isUkSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*).withCSRFToken)

      behave like commonBehaviour(() => performAction(Seq.empty))

      behave like submitIsUkBehaviour(
        performAction,
        routes.RegistrationAddressController.enterPostcode(),
        routes.RegistrationAddressController.enterNonUkAddress()
      )

    }

    "handling requests to display the enter UK address page" must {

      def performAction(): Future[Result] = controller.enterUkAddress()(FakeRequest())

      behave like commonBehaviour(performAction)

      behave like displayEnterUkAddressPage(performAction)

    }

    "handling submitted addresses from enter UK address page" must {

      def performAction(formData: Seq[(String, String)]): Future[Result] =
        controller.enterUkAddressSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*).withCSRFToken)

      behave like commonBehaviour(() => performAction(Seq.empty))

      behave like submitEnterUkAddress(
        performAction,
        controllers.routes.RegistrationController.checkYourAnswers()
      )

    }

    "handling requests to display the enter non UK address page" must {

      def performAction(): Future[Result] = controller.enterNonUkAddress()(FakeRequest())

      behave like commonBehaviour(performAction)

      behave like displayEnterNonUkPage(performAction)

    }

    "handling requests to submit the enter non UK address page" must {
      def performAction(formData: (String, String)*): Future[Result] =
        controller.enterNonUkAddressSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*).withCSRFToken)

      behave like commonBehaviour(() => performAction())

      behave like submitEnterNonUkAddress(
        performAction,
        controllers.routes.RegistrationController.checkYourAnswers()
      )
    }

    "handling requests to display the enter postcode page" must {

      def performAction(): Future[Result] = controller.enterPostcode()(FakeRequest())

      behave like commonBehaviour(performAction)

      behave like enterPostcodePage(performAction)

    }

    "handling submitted postcodes and filters" must {

      def performAction(formData: Seq[(String, String)]): Future[Result] =
        controller.enterPostcodeSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*).withCSRFToken)

      behave like commonBehaviour(() => performAction(Seq.empty))

      behave like submitEnterPostcode(
        performAction,
        routes.RegistrationAddressController.selectAddress()
      )

    }

    "handling requests to display the select address page" must {

      def performAction(): Future[Result] =
        controller.selectAddress()(FakeRequest())

      behave like commonBehaviour(performAction)

      behave like displaySelectAddress(performAction, controllers.routes.RegistrationController.enterName())

    }

    "handling submitted selected addresses" must {

      def performAction(formData: Seq[(String, String)]): Future[Result] =
        controller.selectAddressSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*).withCSRFToken)

      behave like commonBehaviour(() => performAction(Seq.empty))

      behave like submitSelectAddress(
        performAction,
        controllers.routes.RegistrationController.enterName(),
        controllers.routes.RegistrationController.checkYourAnswers()
      )

    }
  }

}
