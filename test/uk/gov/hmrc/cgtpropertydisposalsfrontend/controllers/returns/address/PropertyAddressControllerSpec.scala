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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.address

import cats.data.EitherT
import cats.instances.future._
import org.jsoup.nodes.Document
import org.scalatest.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.i18n.{Lang, MessagesApi}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.CSRFTokenHelper._
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.AddressControllerSpec
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.address.PropertyAddressControllerSpec.validatePropertyAddressPage
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.address.{routes => returnsAddressRoutes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.disposaldetails.DisposalDetailsControllerSpec.{expectedTitles, validateDisposalDetailsCheckYourAnswersPage}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, Subscribed}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address.{NonUkAddress, UkAddress}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.AmountInPence
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.MoneyUtils.formatAmountOfMoneyWithPoundSign
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.GGCredId
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.SubscribedDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.DisposalDetailsAnswers.CompleteDisposalDetailsAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.DraftReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.YearToDateLiabilityAnswers.CompleteYearToDateLiabilityAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, SessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class PropertyAddressControllerSpec
    extends AddressControllerSpec[FillingOutReturn]
    with ScalaCheckDrivenPropertyChecks
    with RedirectToStartBehaviour {

  val draftReturn: DraftReturn = sample[DraftReturn].copy(propertyAddress = Some(ukAddress(1)))

  val validJourneyStatus = FillingOutReturn(sample[SubscribedDetails], sample[GGCredId], None, draftReturn)

  val mockReturnsService = mock[ReturnsService]

  override def overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](bind[ReturnsService].toInstance(mockReturnsService)) ::: super.overrideBindings

  lazy val controller = instanceOf[PropertyAddressController]

  lazy implicit val messagesApi: MessagesApi = controller.messagesApi

  override def updateAddress(journey: FillingOutReturn, address: Address): FillingOutReturn = address match {
    case a: UkAddress    => journey.copy(draftReturn = journey.draftReturn.copy(propertyAddress = Some(a)))
    case _: NonUkAddress => journey
  }

  override val mockUpdateAddress: Option[(FillingOutReturn, Address, Either[Error, Unit]) => Unit] =
    Some {
      case (newDetails: FillingOutReturn, a: UkAddress, r: Either[Error, Unit]) =>
        mockStoreDraftReturn(newDetails.draftReturn.copy(propertyAddress = Some(a)))(r)

      case (_, _: NonUkAddress, _) =>
        sys.error("Non UK addresses not handled in this spec")
    }

  def mockStoreDraftReturn(
    draftReturn: DraftReturn
  )(result: Either[Error, Unit]) =
    (mockReturnsService
      .storeDraftReturn(_: DraftReturn)(_: HeaderCarrier))
      .expects(draftReturn, *)
      .returning(EitherT.fromEither[Future](result))

  def redirectToStartBehaviour(performAction: () => Future[Result]): Unit =
    redirectToStartWhenInvalidJourney(
      performAction, {
        case _: FillingOutReturn => true
        case _                   => false
      }
    )

  "AddressController" when {

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
        returnsAddressRoutes.PropertyAddressController.checkYourAnswers()
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

      behave like submitEnterPostcode(performAction, returnsAddressRoutes.PropertyAddressController.selectAddress())

    }

    "handling requests to display the select address page" must {

      def performAction(): Future[Result] =
        controller.selectAddress()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      behave like displaySelectAddress(
        performAction,
        controllers.returns.address.routes.PropertyAddressController.enterPostcode()
      )

    }

    "handling submitted selected addresses" must {

      def performAction(formData: Seq[(String, String)]): Future[Result] =
        controller.selectAddressSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*).withCSRFToken)

      behave like redirectToStartBehaviour(() => performAction(Seq.empty))

      behave like submitSelectAddress(
        performAction,
        controllers.returns.address.routes.PropertyAddressController.enterPostcode(),
        returnsAddressRoutes.PropertyAddressController.checkYourAnswers()
      )

      "not update the session" when {

        "the user selects an address which is already in their subscribed details" in {
          val session = sessionWithValidJourneyStatus.copy(addressLookupResult = Some(addressLookupResult))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          val result = performAction(Seq("address-select" -> "0"))
          checkIsRedirect(result, returnsAddressRoutes.PropertyAddressController.checkYourAnswers())
        }

      }

    }

    "handling requests to display the check your answers page" must {

      def performAction(): Future[Result] =
        controller.checkYourAnswers()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      def testIsCheckYourAnswers(
        result: Future[Result],
        ukAddressDetails: UkAddress,
        expectedTitleKey: String
      ): Unit =
        checkPageIsDisplayed(
          result,
          messageFromMessageKey(expectedTitleKey), { doc =>
            validatePropertyAddressPage(ukAddressDetails, doc)
          }
        )

      "redirect to the task list if there is no property address in session" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            SessionData.empty.copy(journeyStatus = Some(
              sample[FillingOutReturn].copy(
                draftReturn = draftReturn.copy(propertyAddress = None)
              )
            )
            )
          )
        }

        checkIsRedirect(performAction(), controllers.returns.routes.TaskListController.taskList())
      }

      "display the page if there is an address in session" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionWithValidJourneyStatus)
        }

        testIsCheckYourAnswers(
          performAction(),
          draftReturn.propertyAddress.fold(sys.error("Error"))(_.asInstanceOf[UkAddress]),
          "returns.property-address.cya.title"
        )
      }

    }

    "handling check your answers submits" must {

      def performAction(): Future[Result] =
        controller.checkYourAnswersSubmit()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      "redirect to the task list" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionWithValidJourneyStatus)
        }

        checkIsRedirect(performAction(), controllers.returns.routes.TaskListController.taskList())
      }

    }

  }
}

object PropertyAddressControllerSpec extends Matchers {
  def validatePropertyAddressPage(
    ukAddress: UkAddress,
    doc: Document
  )(implicit messages: MessagesApi, lang: Lang): Unit =
    doc.select("#property-address-answer").text() shouldBe
      List(Some(ukAddress.line1), ukAddress.line2, ukAddress.town, ukAddress.county, Some(ukAddress.postcode.value))
        .collect { case Some(s) => s }
        .mkString(" ")
}
