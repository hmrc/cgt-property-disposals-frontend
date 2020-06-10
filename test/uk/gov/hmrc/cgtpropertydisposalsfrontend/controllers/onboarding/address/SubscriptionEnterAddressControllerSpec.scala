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
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.SubscriptionStatus._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.GGCredId
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.bpr.BusinessPartnerRecord
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, UserType}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views.address.AddressJourneyType.Onboarding.SubscriptionEnterAddressJourney

import scala.concurrent.Future

class SubscriptionEnterAddressControllerSpec
    extends AddressControllerSpec[SubscriptionEnterAddressJourney]
    with ScalaCheckDrivenPropertyChecks
    with RedirectToStartBehaviour {

  val subscriptionMissingData = SubscriptionMissingData(
    sample[BusinessPartnerRecord].copy(address = None),
    None,
    None,
    sample[GGCredId],
    None
  )

  val validJourneyStatus = SubscriptionEnterAddressJourney(subscriptionMissingData)

  lazy val controller = instanceOf[SubscriptionEnterAddressController]

  lazy implicit val messagesApi: MessagesApi = controller.messagesApi

  override def updateAddress(
    journey: SubscriptionEnterAddressJourney,
    address: Address
  ): SubscriptionMissingData =
    journey.journey.copy(manuallyEnteredAddress = Some(address))

  override val mockUpdateAddress: Option[
    (SubscriptionEnterAddressJourney, Address, Either[Error, Unit]) => Unit
  ] = None

  def redirectToStartBehaviour(performAction: () => Future[Result]): Unit =
    redirectToStartWhenInvalidJourney(
      performAction,
      {
        case _: SubscriptionMissingData => true
        case _                          => false
      }
    )

  "SubscriptionEnterAddressController" when {

    "handling requests to display the is UK page" must {
      def performAction(): Future[Result] = controller.isUk()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      behave like displayIsUkBehaviour(performAction)
    }

    "handling requests to submit the is UK page" must {
      def performAction(formData: Seq[(String, String)]): Future[Result] =
        controller.isUkSubmit()(
          FakeRequest().withFormUrlEncodedBody(formData: _*).withCSRFToken
        )

      behave like redirectToStartBehaviour(() => performAction(Seq.empty))

      behave like submitIsUkBehaviour(
        performAction,
        routes.SubscriptionEnterAddressController.enterPostcode(),
        routes.SubscriptionEnterAddressController.enterNonUkAddress(),
        None
      )

    }

    "handling requests to display the enter UK address page" must {

      def performAction(): Future[Result] =
        controller.enterUkAddress()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      behave like displayEnterUkAddressPage(
        UserType.Individual,
        None,
        performAction
      )
      behave like displayEnterUkAddressPage(UserType.Agent, None, performAction)
      behave like displayEnterUkAddressPage(
        UserType.Organisation,
        None,
        performAction
      )
    }

    "handling submitted addresses from enter UK address page" must {

      def performAction(formData: Seq[(String, String)]): Future[Result] =
        controller.enterUkAddressSubmit()(
          FakeRequest().withFormUrlEncodedBody(formData: _*).withCSRFToken
        )

      behave like redirectToStartBehaviour(() => performAction(Seq.empty))

      behave like submitEnterUkAddress(
        performAction,
        controllers.routes.StartController.start()
      )

    }

    "handling requests to display the enter non UK address page" must {

      def performAction(): Future[Result] =
        controller.enterNonUkAddress()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      behave like displayEnterNonUkPage(performAction)

    }

    "handling requests to submit the enter non UK address page" must {
      def performAction(formData: (String, String)*): Future[Result] =
        controller.enterNonUkAddressSubmit()(
          FakeRequest().withFormUrlEncodedBody(formData: _*).withCSRFToken
        )

      behave like redirectToStartBehaviour(() => performAction())

      behave like submitEnterNonUkAddress(
        performAction,
        controllers.routes.StartController.start()
      )
    }

    "handling requests to display the enter postcode page" must {

      def performAction(): Future[Result] =
        controller.enterPostcode()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)
      behave like enterPostcodePage(UserType.Individual, None, performAction)
      behave like enterPostcodePage(UserType.Agent, None, performAction)
      behave like enterPostcodePage(UserType.Organisation, None, performAction)
    }

    "handling submitted postcodes and filters" must {

      def performAction(formData: Seq[(String, String)]): Future[Result] =
        controller.enterPostcodeSubmit()(
          FakeRequest().withFormUrlEncodedBody(formData: _*).withCSRFToken
        )

      behave like redirectToStartBehaviour(() => performAction(Seq.empty))

      behave like submitEnterPostcode(
        performAction,
        routes.SubscriptionEnterAddressController.selectAddress()
      )

    }

    "handling requests to display the select address page" must {

      def performAction(): Future[Result] =
        controller.selectAddress()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      behave like displaySelectAddress(
        UserType.Individual,
        None,
        performAction,
        controllers.routes.StartController.weNeedMoreDetails()
      )

      behave like displaySelectAddress(
        UserType.Agent,
        None,
        performAction,
        controllers.routes.StartController.weNeedMoreDetails()
      )

      behave like displaySelectAddress(
        UserType.Organisation,
        None,
        performAction,
        controllers.routes.StartController.weNeedMoreDetails()
      )
    }

    "handling submitted selected addresses" must {

      def performAction(formData: Seq[(String, String)]): Future[Result] =
        controller.selectAddressSubmit()(
          FakeRequest().withFormUrlEncodedBody(formData: _*).withCSRFToken
        )

      behave like redirectToStartBehaviour(() => performAction(Seq.empty))

      behave like submitSelectAddress(
        performAction,
        controllers.routes.StartController.weNeedMoreDetails(),
        controllers.routes.StartController.start()
      )

    }
  }

}
