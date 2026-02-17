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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.accounts

import cats.data.EitherT
import cats.instances.future._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.SampledScalaCheck
import play.api.i18n.MessagesApi
import play.api.mvc.Result
import play.api.test.CSRFTokenHelper._
import play.api.test.FakeRequest
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AddressControllerSpec, accounts}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.Subscribed
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.IdGen.ggCredIdGen
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.SubscribedDetailsGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.GGCredId
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.{SubscribedDetails, SubscribedUpdateDetails}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, UserType}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views.address.AddressJourneyType.ManagingSubscription.SubscribedAddressJourney
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class SubscribedChangeAddressControllerSpec
    extends AddressControllerSpec[SubscribedAddressJourney]
    with SampledScalaCheck
    with RedirectToStartBehaviour {

  implicit val hc: HeaderCarrier = HeaderCarrier()

  val subscribedDetails: SubscribedDetails =
    sample[SubscribedDetails].copy(address = ukAddress(1))

  protected override val validJourneyStatus: SubscribedAddressJourney = SubscribedAddressJourney(
    Subscribed(
      subscribedDetails,
      sample[GGCredId],
      None,
      List.empty,
      List.empty
    )
  )

  protected override val controller: SubscribedChangeAddressController =
    instanceOf[SubscribedChangeAddressController]

  lazy implicit val messagesApi: MessagesApi = controller.messagesApi

  override def updateAddress(
    journey: SubscribedAddressJourney,
    address: Address
  ): Subscribed =
    journey.journey.copy(subscribedDetails = journey.journey.subscribedDetails.copy(address = address))

  override val mockUpdateAddress: Option[(SubscribedAddressJourney, Address, Either[Error, Unit]) => Unit] =
    Some {
      case (
            newDetails: SubscribedAddressJourney,
            a: Address,
            r: Either[Error, Unit]
          ) =>
        mockUpdateSubscribedDetails(
          SubscribedUpdateDetails(
            newDetails.journey.subscribedDetails.copy(address = a),
            newDetails.journey.subscribedDetails
          )
        )(r)
    }

  private def mockUpdateSubscribedDetails(
    subscribedAndVerifierDetails: SubscribedUpdateDetails
  )(result: Either[Error, Unit]) =
    (mockSubscriptionService
      .updateSubscribedDetails(_: SubscribedUpdateDetails)(using _: HeaderCarrier))
      .expects(subscribedAndVerifierDetails, *)
      .returning(EitherT.fromEither[Future](result))

  def redirectToStartBehaviour(performAction: () => Future[Result]): Unit =
    redirectToStartWhenInvalidJourney(
      () => performAction(),
      {
        case _: Subscribed => true
        case _             => false
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
        accounts.routes.SubscribedChangeAddressController.enterPostcode(),
        accounts.routes.SubscribedChangeAddressController.enterNonUkAddress(),
        Some(accounts.routes.SubscribedChangeAddressController.showExitPage())
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
        controllers.accounts.routes.AccountController.contactAddressUpdated()
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
        controllers.accounts.routes.AccountController.contactAddressUpdated()
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

      behave like submitEnterPostcode(performAction, accounts.routes.SubscribedChangeAddressController.selectAddress())

    }

    "handling requests to display the select address page" must {

      def performAction(): Future[Result] =
        controller.selectAddress()(FakeRequest())

      behave like redirectToStartBehaviour(() => performAction())

      behave like displaySelectAddress(
        UserType.Individual,
        None,
        () => performAction(),
        controllers.accounts.routes.AccountController.manageYourDetails()
      )

      behave like displaySelectAddress(
        UserType.Agent,
        None,
        () => performAction(),
        controllers.accounts.routes.AccountController.manageYourDetails()
      )

      behave like displaySelectAddress(
        UserType.Organisation,
        None,
        () => performAction(),
        controllers.accounts.routes.AccountController.manageYourDetails()
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
        controllers.accounts.routes.AccountController.manageYourDetails(),
        controllers.accounts.routes.AccountController.contactAddressUpdated()
      )

      "not update the session" when {

        "the user selects an address which is already in their subscribed details" in {
          val session = sessionWithValidJourneyStatus.copy(addressLookupResult = Some(addressLookupResult))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          val result = performAction(Seq("address-select" -> "0"))
          checkIsRedirect(
            result,
            controllers.accounts.routes.AccountController
              .contactAddressUpdated()
          )
        }

      }

    }
  }

}
