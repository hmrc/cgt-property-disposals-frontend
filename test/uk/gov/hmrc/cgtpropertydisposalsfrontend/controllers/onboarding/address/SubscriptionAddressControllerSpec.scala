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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.address

import uk.gov.hmrc.cgtpropertydisposalsfrontend.SampledScalaCheck
import play.api.i18n.MessagesApi
import play.api.mvc.Result
import play.api.test.CSRFTokenHelper._
import play.api.test.FakeRequest
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.AddressControllerSpec
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.SubscriptionStatus._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.{Address, AddressSource}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.IdGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.OnboardingDetailsGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.GGCredId
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.SubscriptionDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, UserType}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views.address.AddressJourneyType.Onboarding.SubscriptionReadyAddressJourney

import scala.concurrent.Future

class SubscriptionAddressControllerSpec
    extends AddressControllerSpec[SubscriptionReadyAddressJourney]
    with SampledScalaCheck
    with RedirectToStartBehaviour {

  private val subscriptionDetails =
    sample[SubscriptionDetails].copy(
      address = ukAddress(1),
      addressSource = AddressSource.BusinessPartnerRecord
    )

  protected override val validJourneyStatus: SubscriptionReadyAddressJourney = SubscriptionReadyAddressJourney(
    SubscriptionReady(subscriptionDetails, sample[GGCredId])
  )

  protected override val controller: SubscriptionAddressController = instanceOf[SubscriptionAddressController]

  lazy implicit val messagesApi: MessagesApi = controller.messagesApi

  override def updateAddress(
    journey: SubscriptionReadyAddressJourney,
    address: Address
  ): SubscriptionReady =
    journey.journey.copy(subscriptionDetails =
      journey.journey.subscriptionDetails
        .copy(address = address, addressSource = AddressSource.ManuallyEntered)
    )

  override val mockUpdateAddress: Option[(SubscriptionReadyAddressJourney, Address, Either[Error, Unit]) => Unit] = None

  def redirectToStartBehaviour(performAction: () => Future[Result]): Unit =
    redirectToStartWhenInvalidJourney(
      () => performAction(),
      {
        case _: SubscriptionReady => true
        case _                    => false
      }
    )

  "AddressController" when {

    "handling requests to display the is UK page" must {
      def performAction(): Future[Result] = controller.isUk()(FakeRequest())

      behave like redirectToStartBehaviour(() => performAction())

      behave like displayIsUkBehaviour(() => performAction())
    }

    "handling requests to submit the is UK page" must {
      def performAction(formData: Seq[(String, String)]): Future[Result] =
        controller.isUkSubmit()(
          FakeRequest().withFormUrlEncodedBody(formData*).withCSRFToken.withMethod("POST")
        )

      behave like redirectToStartBehaviour(() => performAction(Seq.empty))

      behave like submitIsUkBehaviour(
        performAction,
        routes.SubscriptionAddressController.enterPostcode(),
        routes.SubscriptionAddressController.enterNonUkAddress(),
        None
      )

    }

    "handling requests to display the enter UK address page" must {

      def performAction(): Future[Result] =
        controller.enterUkAddress()(FakeRequest())

      behave like redirectToStartBehaviour(() => performAction())

      behave like displayEnterUkAddressPage(UserType.Individual, None, () => performAction())
      behave like displayEnterUkAddressPage(UserType.Agent, None, () => performAction())
      behave like displayEnterUkAddressPage(UserType.Organisation, None, () => performAction())
    }

    "handling submitted addresses from enter UK address page" must {

      def performAction(formData: Seq[(String, String)]): Future[Result] =
        controller.enterUkAddressSubmit()(
          FakeRequest().withFormUrlEncodedBody(formData*).withCSRFToken.withMethod("POST")
        )

      behave like redirectToStartBehaviour(() => performAction(Seq.empty))

      behave like submitEnterUkAddress(
        performAction,
        controllers.onboarding.routes.SubscriptionController.checkYourDetails()
      )

    }

    "handling requests to display the enter non UK address page" must {

      def performAction(): Future[Result] =
        controller.enterNonUkAddress()(FakeRequest())

      behave like redirectToStartBehaviour(() => performAction())

      behave like displayEnterNonUkPage(() => performAction())

    }

    "handling requests to submit the enter non UK address page" must {
      def performAction(formData: (String, String)*): Future[Result] =
        controller.enterNonUkAddressSubmit()(
          FakeRequest().withFormUrlEncodedBody(formData*).withCSRFToken.withMethod("POST")
        )

      behave like redirectToStartBehaviour(() => performAction())

      behave like submitEnterNonUkAddress(
        performAction,
        controllers.onboarding.routes.SubscriptionController.checkYourDetails()
      )
    }

    "handling requests to display the enter postcode page" must {

      def performAction(): Future[Result] =
        controller.enterPostcode()(FakeRequest())

      behave like redirectToStartBehaviour(() => performAction())
      behave like enterPostcodePage(UserType.Individual, None, () => performAction())
      behave like enterPostcodePage(UserType.Agent, None, () => performAction())
      behave like enterPostcodePage(UserType.Organisation, None, () => performAction())
    }

    "handling submitted postcodes and filters" must {

      def performAction(formData: Seq[(String, String)]): Future[Result] =
        controller.enterPostcodeSubmit()(
          FakeRequest().withFormUrlEncodedBody(formData*).withCSRFToken.withMethod("POST")
        )

      behave like redirectToStartBehaviour(() => performAction(Seq.empty))

      behave like submitEnterPostcode(performAction, routes.SubscriptionAddressController.selectAddress())

    }

    "handling requests to display the select address page" must {

      def performAction(): Future[Result] =
        controller.selectAddress()(FakeRequest())

      behave like redirectToStartBehaviour(() => performAction())

      behave like displaySelectAddress(
        UserType.Individual,
        None,
        () => performAction(),
        controllers.onboarding.routes.SubscriptionController.checkYourDetails()
      )

      behave like displaySelectAddress(
        UserType.Agent,
        None,
        () => performAction(),
        controllers.onboarding.routes.SubscriptionController.checkYourDetails()
      )

      behave like displaySelectAddress(
        UserType.Organisation,
        None,
        () => performAction(),
        controllers.onboarding.routes.SubscriptionController.checkYourDetails()
      )
    }

    "handling submitted selected addresses" must {

      def performAction(formData: Seq[(String, String)]): Future[Result] =
        controller.selectAddressSubmit()(
          FakeRequest().withFormUrlEncodedBody(formData*).withCSRFToken.withMethod("POST")
        )

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
            mockGetSession(session)
          }

          val result = performAction(Seq("address-select" -> "0"))
          checkIsRedirect(
            result,
            controllers.onboarding.routes.SubscriptionController
              .checkYourDetails()
          )
        }

      }

    }
  }

}
