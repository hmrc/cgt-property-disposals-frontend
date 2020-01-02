/*
 * Copyright 2020 HM Revenue & Customs
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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.address

import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.i18n.MessagesApi
import play.api.mvc.Result
import play.api.test.CSRFTokenHelper._
import play.api.test.FakeRequest
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.AddressControllerSpec
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Error
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.SubscriptionStatus._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.GGCredId
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.SubscriptionDetails

import scala.concurrent.Future

class SubscriptionAddressControllerSpec
    extends AddressControllerSpec[SubscriptionReady]
    with ScalaCheckDrivenPropertyChecks
    with RedirectToStartBehaviour {

  val subscriptionDetails: SubscriptionDetails =
    sample[SubscriptionDetails].copy(address = address(1))

  val validJourneyStatus = SubscriptionReady(subscriptionDetails, sample[GGCredId])

  lazy val controller = instanceOf[SubscriptionAddressController]

  lazy implicit val messagesApi: MessagesApi = controller.messagesApi

  override def updateAddress(journey: SubscriptionReady, address: Address): SubscriptionReady =
    journey.copy(subscriptionDetails = journey.subscriptionDetails.copy(address = address))

  override val mockUpdateAddress: Option[(SubscriptionReady, Address, Either[Error, Unit]) => Unit] = None

  def redirectToStartBehaviour(performAction: () => Future[Result]): Unit =
    redirectToStartWhenInvalidJourney(
      performAction, {
        case _: SubscriptionReady => true
        case _                    => false
      }
    )

  "AddressController" when {

    "handling requests to display the is UK page" must {
      def performAction(): Future[Result] = controller.isUk()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      behave like displayIsUkBehaviour(performAction)
    }

    "handling requests to submit the is UK page" must {
      def performAction(formData: Seq[(String, String)]): Future[Result] =
        controller.isUkSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*).withCSRFToken)

      behave like redirectToStartBehaviour(() => performAction(Seq.empty))

      behave like submitIsUkBehaviour(
        performAction,
        routes.SubscriptionAddressController.enterPostcode(),
        routes.SubscriptionAddressController.enterNonUkAddress()
      )

    }

    "handling requests to display the enter UK address page" must {

      def performAction(): Future[Result] = controller.enterUkAddress()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      behave like displayEnterUkAddressPage(performAction)

    }

    "handling submitted addresses from enter UK address page" must {

      def performAction(formData: Seq[(String, String)]): Future[Result] =
        controller.enterUkAddressSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*).withCSRFToken)

      behave like redirectToStartBehaviour(() => performAction(Seq.empty))

      behave like submitEnterUkAddress(
        performAction,
        controllers.onboarding.routes.SubscriptionController.checkYourDetails()
      )

    }

    "handling requests to display the enter non UK address page" must {

      def performAction(): Future[Result] = controller.enterNonUkAddress()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      behave like displayEnterNonUkPage(performAction)

    }

    "handling requests to submit the enter non UK address page" must {
      def performAction(formData: (String, String)*): Future[Result] =
        controller.enterNonUkAddressSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*).withCSRFToken)

      behave like redirectToStartBehaviour(() => performAction())

      behave like submitEnterNonUkAddress(
        performAction,
        controllers.onboarding.routes.SubscriptionController.checkYourDetails()
      )
    }

    "handling requests to display the enter postcode page" must {

      def performAction(): Future[Result] = controller.enterPostcode()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      behave like enterPostcodePage(performAction)

    }

    "handling submitted postcodes and filters" must {

      def performAction(formData: Seq[(String, String)]): Future[Result] =
        controller.enterPostcodeSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*).withCSRFToken)

      behave like redirectToStartBehaviour(() => performAction(Seq.empty))

      behave like submitEnterPostcode(performAction, routes.SubscriptionAddressController.selectAddress())

    }

    "handling requests to display the select address page" must {

      def performAction(): Future[Result] =
        controller.selectAddress()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      behave like displaySelectAddress(
        performAction,
        controllers.onboarding.routes.SubscriptionController.checkYourDetails()
      )

    }

    "handling submitted selected addresses" must {

      def performAction(formData: Seq[(String, String)]): Future[Result] =
        controller.selectAddressSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*).withCSRFToken)

      behave like redirectToStartBehaviour(() => performAction(Seq.empty))

      behave like submitSelectAddress(
        performAction,
        controllers.onboarding.routes.SubscriptionController.checkYourDetails(),
        controllers.onboarding.routes.SubscriptionController.checkYourDetails()
      )

      "not update the session" when {

        "the user selects an address which is already in their subscription details" in {
          val session = sessionWithValidJourneyStatus.copy(addressLookupResult = Some(addressLookupResult))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(session))))
          }

          val result = performAction(Seq("address-select" -> "0"))
          checkIsRedirect(result, controllers.onboarding.routes.SubscriptionController.checkYourDetails())
        }

      }

    }
  }

}
