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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns

import java.time.LocalDate

import cats.data.EitherT
import cats.instances.future._
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.i18n.{Messages, MessagesApi, MessagesImpl}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.{Call, Request, Result}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.CheckAllAnswersAndSubmitControllerSpec.validateAllCheckYourAnswersSections
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.ViewingReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.LocalDateUtils.govShortDisplayFormat
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.ChargeType.{PenaltyInterest, UkResidentReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.MoneyUtils.formatAmountOfMoneyWithPoundSign
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.PaymentMethod.DirectDebit
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.CgtReference
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ReturnSummary
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, SessionData, UserType}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.PaymentsService
import uk.gov.hmrc.http.HeaderCarrier

import scala.collection.JavaConverters._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class ViewReturnControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with ScalaCheckDrivenPropertyChecks
    with RedirectToStartBehaviour {

  val mockPaymentsService = mock[PaymentsService]

  override val overrideBindings =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore),
      bind[PaymentsService].toInstance(mockPaymentsService)
    )

  lazy val controller = instanceOf[ViewReturnController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  implicit val messages: Messages = MessagesImpl(lang, messagesApi)

  def mockStartPaymentJourney(
    cgtReference: CgtReference,
    chargeReference: Option[String],
    amount: AmountInPence,
    returnUrl: Call,
    backUrl: Call
  )(response: Either[Error, PaymentsJourney]) =
    (mockPaymentsService
      .startPaymentJourney(_: CgtReference, _: Option[String], _: AmountInPence, _: Call, _: Call)(
        _: HeaderCarrier,
        _: Request[_]
      ))
      .expects(cgtReference, chargeReference, amount, returnUrl, backUrl, *, *)
      .returning(EitherT.fromEither[Future](response))

  "ViewReturnController" when {

    "handling requests to display a return" must {

      def performAction(): Future[Result] =
        controller.displayReturn()(FakeRequest())

      val ukResidentMainReturnChargeAmount: AmountInPence = AmountInPence(10000)
      val ukResidentReturnSentDate: LocalDate             = LocalDate.now()
      val ukResidentMainReturnChargeDueDate: LocalDate    = LocalDate.now().plusMonths(1)

      val penaltyInterestChargeAmount: AmountInPence    = AmountInPence(10000)
      val penaltyInterestChargeAmountDueDate: LocalDate = LocalDate.now().plusMonths(2)

      val penaltyInterestCharge = sample[Charge].copy(
        chargeType = PenaltyInterest,
        amount     = penaltyInterestChargeAmount,
        dueDate    = penaltyInterestChargeAmountDueDate,
        payments   = List.empty
      )

      val fullPaymentForUkResidentMainReturnChargeDueDate: LocalDate = LocalDate.now().plusMonths(2)

      val fullPaymentForUkResidentReturnCharge = sample[Payment].copy(
        amount       = ukResidentMainReturnChargeAmount,
        method       = DirectDebit,
        clearingDate = fullPaymentForUkResidentMainReturnChargeDueDate
      )

      val ukResidentReturnChargeFullPayment = sample[Charge].copy(
        chargeType = UkResidentReturn,
        amount     = ukResidentMainReturnChargeAmount,
        dueDate    = ukResidentMainReturnChargeDueDate,
        payments   = List(fullPaymentForUkResidentReturnCharge)
      )

      val chargesWithChargeRaiseAndFullPayment = List(ukResidentReturnChargeFullPayment, penaltyInterestCharge)

      val sentReturn = sample[ReturnSummary].copy(
        charges                = chargesWithChargeRaiseAndFullPayment,
        mainReturnChargeAmount = ukResidentMainReturnChargeAmount,
        submissionDate         = ukResidentReturnSentDate
      )

      def validatePaymentsSection(document: Document, viewingReturn: ViewingReturn): Unit = {
        val paymentDetails = document
          .select(s"#returnPaymentDetails-${viewingReturn.returnSummary.submissionId} > tr > td")
          .eachText()
          .asScala

        paymentDetails.headOption.fold(sys.error("Error"))(_.toString) should startWith("Tax payment")
        paymentDetails(1)                                              shouldBe govShortDisplayFormat(ukResidentMainReturnChargeDueDate)
        paymentDetails(2)                                              shouldBe formatAmountOfMoneyWithPoundSign(ukResidentMainReturnChargeAmount.inPounds())
        paymentDetails(3) shouldBe formatAmountOfMoneyWithPoundSign(
          ukResidentMainReturnChargeAmount.inPounds() - fullPaymentForUkResidentReturnCharge.amount.inPounds()
        )
        paymentDetails(4) shouldBe "Paid"

        paymentDetails(5) shouldBe s"${formatAmountOfMoneyWithPoundSign(
          fullPaymentForUkResidentReturnCharge.amount.inPounds()
        )} direct debit payment received on ${govShortDisplayFormat(fullPaymentForUkResidentMainReturnChargeDueDate)}"

        paymentDetails(6)  should startWith("Interest on penalties paid late")
        paymentDetails(7)  shouldBe govShortDisplayFormat(penaltyInterestChargeAmountDueDate)
        paymentDetails(8)  shouldBe formatAmountOfMoneyWithPoundSign(penaltyInterestChargeAmount.inPounds())
        paymentDetails(9)  shouldBe formatAmountOfMoneyWithPoundSign(penaltyInterestChargeAmount.inPounds())
        paymentDetails(10) shouldBe "Pay now"
      }

      behave like redirectToStartWhenInvalidJourney(
        performAction, {
          case _: ViewingReturn => true
          case _                => false
        }
      )

      "display the page" in {
        forAll { sampleViewingReturn: ViewingReturn =>
          val viewingReturn = sampleViewingReturn.copy(returnSummary = sentReturn)
          val userType      = if (sampleViewingReturn.agentReferenceNumber.isDefined) Some(UserType.Agent) else None
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData.empty.copy(journeyStatus = Some(viewingReturn), userType = userType))
          }

          val result   = performAction()
          val document = Jsoup.parse(contentAsString(result))

          document
            .select("#content > article > div.govuk-box-highlight.govuk-box-highlight--status > h1")
            .text() shouldBe messageFromMessageKey(
            "viewReturn.title"
          )
          document.select("#heading-reference").text() shouldBe viewingReturn.returnSummary.submissionId
          document.select("#heading-tax-owed").text() shouldBe MoneyUtils.formatAmountOfMoneyWithPoundSign(
            viewingReturn.returnSummary.mainReturnChargeAmount.withFloorZero.inPounds()
          )

          validatePaymentsSection(document, viewingReturn)
          validateAllCheckYourAnswersSections(document, viewingReturn.completeReturn, userType)
        }
      }

    }

    "handling requests to pay a charge" must {
      def performAction(chargeReference: String): Future[Result] =
        controller.payCharge(chargeReference)(FakeRequest())

      val charge = sample[Charge].copy(chargeReference = "reference")

      val viewingReturn = sample[ViewingReturn].copy(
        returnSummary = sample[ReturnSummary].copy(charges = List(charge))
      )

      behave like redirectToStartWhenInvalidJourney(
        () => performAction(""), {
          case _: ViewingReturn => true
          case _                => false
        }
      )

      "return a not found response" when {

        "no charge can be found for the given charge reference" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData.empty.copy(journeyStatus = Some(viewingReturn)))
          }

          val result = performAction(s"${charge.chargeReference}123")
          status(result) shouldBe NOT_FOUND
        }

      }

      "return an error page" when {

        "a charge can be found but there is an error starting the payments journey" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData.empty.copy(journeyStatus = Some(viewingReturn)))
            mockStartPaymentJourney(
              viewingReturn.subscribedDetails.cgtReference,
              Some(charge.chargeReference),
              charge.amount,
              controllers.accounts.homepage.routes.HomePageController.homepage(),
              controllers.returns.routes.ViewReturnController.displayReturn()
            )(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction(charge.chargeReference))
        }

      }

      "redirect to the payments url" when {

        "a charge can be found amd a payments journey is successfully started" in {
          val paymentsJourney = sample[PaymentsJourney]

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData.empty.copy(journeyStatus = Some(viewingReturn)))
            mockStartPaymentJourney(
              viewingReturn.subscribedDetails.cgtReference,
              Some(charge.chargeReference),
              charge.amount,
              controllers.accounts.homepage.routes.HomePageController.homepage(),
              controllers.returns.routes.ViewReturnController.displayReturn()
            )(Right(paymentsJourney))
          }

          checkIsRedirect(performAction(charge.chargeReference), paymentsJourney.nextUrl)
        }

      }

    }

  }

}
