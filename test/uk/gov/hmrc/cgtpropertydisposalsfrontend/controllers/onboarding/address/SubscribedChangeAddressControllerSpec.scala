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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.address

import cats.data.EitherT
import cats.instances.future._
import org.scalacheck.ScalacheckShapeless._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.i18n.MessagesApi
import play.api.mvc.Result
import play.api.test.CSRFTokenHelper._
import play.api.test.FakeRequest
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.accounts
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.accounts.SubscribedChangeAddressController
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.Subscribed
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.GGCredId
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.{SubscribedDetails, SubscribedUpdateDetails}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, sample}
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class SubscribedChangeAddressControllerSpec
    extends AddressControllerSpec[Subscribed]
    with ScalaCheckDrivenPropertyChecks
    with RedirectToStartBehaviour {

  val subscribedDetails: SubscribedDetails =
    sample[SubscribedDetails].copy(address = address(1))

  val validJourneyStatus = Subscribed(subscribedDetails, sample[GGCredId])

  lazy val controller = instanceOf[SubscribedChangeAddressController]

  lazy implicit val messagesApi: MessagesApi = controller.messagesApi

  override def updateAddress(journey: Subscribed, address: Address): Subscribed =
    journey.copy(subscribedDetails = journey.subscribedDetails.copy(address = address))

  override val mockUpdateAddress: Option[(Subscribed, Address, Either[Error, Unit]) => Unit] =
    Some {
      case (newDetails: Subscribed, a: Address, r: Either[Error, Unit]) =>
        mockUpdateSubscribedDetails(
          SubscribedUpdateDetails(newDetails.subscribedDetails.copy(address = a), newDetails.subscribedDetails)
        )(r)
    }

  def mockUpdateSubscribedDetails(
    subscribedAndVerifierDetails: SubscribedUpdateDetails
  )(result: Either[Error, Unit]) =
    (mockSubscriptionService
      .updateSubscribedDetails(_: SubscribedUpdateDetails)(_: HeaderCarrier))
      .expects(subscribedAndVerifierDetails, *)
      .returning(EitherT.fromEither[Future](result))

  override val updateSubscriptionDetailChangedFlag: Boolean = true

  def redirectToStartBehaviour(performAction: () => Future[Result]): Unit =
    redirectToStartWhenInvalidJourney(
      performAction, {
        case _: Subscribed => true
        case _             => false
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
        accounts.routes.SubscribedChangeAddressController.enterPostcode(),
        accounts.routes.SubscribedChangeAddressController.enterNonUkAddress()
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
        controllers.routes.HomeController.manageYourDetails()
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

      behave like submitEnterNonUkAddress(performAction, controllers.routes.HomeController.manageYourDetails())
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

      behave like submitEnterPostcode(performAction, accounts.routes.SubscribedChangeAddressController.selectAddress())

    }

    "handling requests to display the select address page" must {

      def performAction(): Future[Result] =
        controller.selectAddress()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      behave like displaySelectAddress(performAction, controllers.routes.HomeController.manageYourDetails())

    }

    "handling submitted selected addresses" must {

      def performAction(formData: Seq[(String, String)]): Future[Result] =
        controller.selectAddressSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*).withCSRFToken)

      behave like redirectToStartBehaviour(() => performAction(Seq.empty))

      behave like submitSelectAddress(
        performAction,
        controllers.routes.HomeController.manageYourDetails(),
        controllers.routes.HomeController.manageYourDetails()
      )

      "not update the session" when {

        "the user selects an address which is already in their subscribed details" in {
          val session = sessionWithValidJourneyStatus.copy(addressLookupResult = Some(addressLookupResult))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(session))))
          }

          val result = performAction(Seq("address-select" -> "0"))
          checkIsRedirect(result, controllers.routes.HomeController.manageYourDetails())
        }

      }

    }
  }

}
