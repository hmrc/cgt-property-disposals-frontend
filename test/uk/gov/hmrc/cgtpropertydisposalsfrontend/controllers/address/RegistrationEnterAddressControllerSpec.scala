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
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.i18n.MessagesApi
import play.api.mvc.Result
import play.api.test.CSRFTokenHelper._
import play.api.test.FakeRequest
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.RegistrationStatus.IndividualSupplyingInformation
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.IndividualName
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Email, Error, sample}

import scala.concurrent.Future

class RegistrationEnterAddressControllerSpec
    extends AddressControllerSpec[IndividualSupplyingInformation]
    with ScalaCheckDrivenPropertyChecks
    with RedirectToStartBehaviour {

  val validJourneyStatus = IndividualSupplyingInformation(Some(sample[IndividualName]), None, Some(sample[Email]))

  lazy val controller = instanceOf[RegistrationEnterAddressController]

  lazy implicit val messagesApi: MessagesApi = controller.messagesApi

  override def updateAddress(
    journey: IndividualSupplyingInformation,
    address: Address
  ): IndividualSupplyingInformation =
    journey.copy(address = Some(address))

  override val mockUpdateAddress: Option[(IndividualSupplyingInformation, Address, Either[Error, Unit]) => Unit] = None

  def redirectToStartBehaviour(performAction: () => Future[Result]): Unit =
    redirectToStartWhenInvalidJourney(
      performAction, {
        case _: IndividualSupplyingInformation => true
        case _                                 => false
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
        routes.RegistrationEnterAddressController.enterPostcode(),
        routes.RegistrationEnterAddressController.enterNonUkAddress()
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
        controllers.routes.RegistrationController.checkYourAnswers()
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
        controllers.routes.RegistrationController.checkYourAnswers()
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

      behave like submitEnterPostcode(
        performAction,
        routes.RegistrationEnterAddressController.selectAddress()
      )

    }

    "handling requests to display the select address page" must {

      def performAction(): Future[Result] =
        controller.selectAddress()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      behave like displaySelectAddress(
        performAction,
        controllers.name.routes.RegistrationEnterIndividualNameController.enterIndividualName()
      )

    }

    "handling submitted selected addresses" must {

      def performAction(formData: Seq[(String, String)]): Future[Result] =
        controller.selectAddressSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*).withCSRFToken)

      behave like redirectToStartBehaviour(() => performAction(Seq.empty))

      behave like submitSelectAddress(
        performAction,
        controllers.name.routes.RegistrationEnterIndividualNameController.enterIndividualName(),
        controllers.routes.RegistrationController.checkYourAnswers()
      )

    }
  }

}
