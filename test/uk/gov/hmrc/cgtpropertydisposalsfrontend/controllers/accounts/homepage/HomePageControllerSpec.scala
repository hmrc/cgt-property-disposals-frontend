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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.accounts.homepage

import java.time.LocalDate
import java.util.UUID

import cats.data.EitherT
import cats.instances.future._
import cats.syntax.order._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.i18n.{Messages, MessagesApi, MessagesImpl}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.{Call, Request, Result}
import play.api.test.FakeRequest
import play.api.test.Helpers.{contentAsString, _}
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, JustSubmittedReturn, StartingNewDraftReturn, SubmitReturnFailed, Subscribed, ViewingReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.TimeUtils.govShortDisplayFormat
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address.UkAddress
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.ChargeType.{PenaltyInterest, UkResidentReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.MoneyUtils.formatAmountOfMoneyWithPoundSign
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.{AgentReferenceNumber, CgtReference}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.{IndividualName, TrustName}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.SubscribedDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.CompleteReturn.CompleteSingleDisposalReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.{CompleteSingleDisposalTriageAnswers, IncompleteSingleDisposalTriageAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, SessionData, UserType}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.{PaymentsService, ReturnsService}
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class HomePageControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with ScalaCheckDrivenPropertyChecks
    with RedirectToStartBehaviour {

  val mockReturnsService = mock[ReturnsService]

  val mockPaymentsService = mock[PaymentsService]

  override val overrideBindings =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore),
      bind[ReturnsService].toInstance(mockReturnsService),
      bind[PaymentsService].toInstance(mockPaymentsService)
    )

  def mockGetDraftReturns(
    cgtReference: CgtReference,
    sentReturns: List[ReturnSummary]
  )(
    response: Either[Error, List[DraftReturn]]
  ) =
    (mockReturnsService
      .getDraftReturns(_: CgtReference, _: List[ReturnSummary])(
        _: HeaderCarrier
      ))
      .expects(cgtReference, sentReturns, *)
      .returning(EitherT.fromEither[Future](response))

  def mockGetReturnsList(cgtReference: CgtReference)(
    response: Either[Error, List[ReturnSummary]]
  ) =
    (mockReturnsService
      .listReturns(_: CgtReference)(_: HeaderCarrier))
      .expects(cgtReference, *)
      .returning(EitherT.fromEither[Future](response))

  def mockDisplayReturn(cgtReference: CgtReference, submissionId: String)(
    response: Either[Error, CompleteSingleDisposalReturn]
  ) =
    (mockReturnsService
      .displayReturn(_: CgtReference, _: String)(_: HeaderCarrier))
      .expects(cgtReference, submissionId, *)
      .returning(EitherT.fromEither[Future](response))

  def mockStartPaymentJourney(
    cgtReference: CgtReference,
    chargeReference: Option[String],
    amount: AmountInPence,
    returnUrl: Call,
    backUrl: Call
  )(response: Either[Error, PaymentsJourney]) =
    (mockPaymentsService
      .startPaymentJourney(
        _: CgtReference,
        _: Option[String],
        _: AmountInPence,
        _: Call,
        _: Call
      )(
        _: HeaderCarrier,
        _: Request[_]
      ))
      .expects(cgtReference, chargeReference, amount, returnUrl, backUrl, *, *)
      .returning(EitherT.fromEither[Future](response))

  lazy val controller = instanceOf[HomePageController]

  implicit val messagesApi: MessagesApi = controller.messagesApi

  implicit val messages: Messages = MessagesImpl(lang, messagesApi)

  def sessionDataWithSubscribed(subscribed: Subscribed) =
    SessionData.empty.copy(journeyStatus = Some(subscribed))

  "The HomePage Controller" when {

    "handling requests for account home" must {

      def performAction(): Future[Result] = controller.homepage()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(
        performAction,
        {
          case _: Subscribed | _: StartingNewDraftReturn | _: FillingOutReturn | _: JustSubmittedReturn |
              _: ViewingReturn | _: SubmitReturnFailed =>
            true
          case _ => false
        }
      )

      val ukResidentMainReturnChargeAmount: AmountInPence = AmountInPence(10000)
      val ukResidentReturnSentDate: LocalDate             = LocalDate.now()
      val ukResidentMainReturnChargeDueDate: LocalDate    =
        LocalDate.now().plusMonths(1)

      val ukResidentReturnChargeNoPayments = sample[Charge].copy(
        chargeType = UkResidentReturn,
        amount = ukResidentMainReturnChargeAmount,
        dueDate = ukResidentMainReturnChargeDueDate,
        payments = List.empty
      )

      val penaltyInterestChargeAmount: AmountInPence    = AmountInPence(10000)
      val penaltyInterestChargeAmountDueDate: LocalDate =
        LocalDate.now().plusMonths(1)

      val penaltyInterestCharge = sample[Charge].copy(
        chargeType = PenaltyInterest,
        amount = penaltyInterestChargeAmount,
        dueDate = penaltyInterestChargeAmountDueDate,
        payments = List.empty
      )

      val chargesWithoutChargeRaiseAndNoPayment =
        List(ukResidentReturnChargeNoPayments)
      val chargesWithChargeRaiseAndNoPayment    =
        List(ukResidentReturnChargeNoPayments, penaltyInterestCharge)

      val partialPaymentForUkResidentMainReturnChargeAmount: AmountInPence  =
        AmountInPence(1000)
      val partialPaymentForUkResidentMainReturnChargePaymentDate: LocalDate =
        LocalDate.now().plusMonths(2)

      val partialPaymentForUkResidentReturnCharge = sample[Payment].copy(
        amount = partialPaymentForUkResidentMainReturnChargeAmount,
        clearingDate = partialPaymentForUkResidentMainReturnChargePaymentDate
      )

      val ukResidentReturnChargePartialPayment = sample[Charge].copy(
        chargeType = UkResidentReturn,
        amount = ukResidentMainReturnChargeAmount,
        dueDate = ukResidentMainReturnChargeDueDate,
        payments = List(partialPaymentForUkResidentReturnCharge)
      )

      val chargesWithChargeRaiseAndPartialPayment                    =
        List(ukResidentReturnChargePartialPayment, penaltyInterestCharge)
      val fullPaymentForUkResidentMainReturnChargeDueDate: LocalDate =
        LocalDate.now().plusMonths(2)

      val fullPaymentForUkResidentReturnCharge = sample[Payment].copy(
        amount = ukResidentMainReturnChargeAmount,
        clearingDate = fullPaymentForUkResidentMainReturnChargeDueDate
      )

      val ukResidentReturnChargeFullPayment = sample[Charge].copy(
        chargeType = UkResidentReturn,
        amount = ukResidentMainReturnChargeAmount,
        dueDate = ukResidentMainReturnChargeDueDate,
        payments = List(fullPaymentForUkResidentReturnCharge)
      )

      val chargesWithChargeRaiseAndFullPayment =
        List(ukResidentReturnChargeFullPayment, penaltyInterestCharge)

      def extractAmount(s: String): String = s.substring(s.indexOf('£'))

      // the callToActionButton selector is designed to select the only
      // button on the page for us to check which message it has
      val callToActionButton    = "#content a.button"
      val resumeDraftMessage    = "drafts.list.resume"
      val viewAndPayMessage     = "returns.list.viewAndPay"
      val startNewReturnMessage = "account.home.button.start-a-new-return"

      "display the resume draft link as a button when there is a draft and no sent returns" in {
        val sampleDraftReturn = sample[DraftSingleDisposalReturn]
        val subscribed        =
          sample[Subscribed].copy(
            draftReturns = List(sampleDraftReturn),
            sentReturns = List.empty[ReturnSummary]
          )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            SessionData.empty.copy(
              userType = Some(UserType.Individual),
              journeyStatus = Some(subscribed)
            )
          )
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("account.home.title"),
          doc =>
            doc
              .select(callToActionButton)
              .text shouldBe
              messageFromMessageKey(
                resumeDraftMessage
              )
        )
      }

      "display the resume draft link as a button when there is a draft and no outstanding amounts to pay in sent returns" in {
        val triageAnswers                       = sample[CompleteSingleDisposalTriageAnswers]
          .copy(completionDate = CompletionDate(ukResidentMainReturnChargeDueDate))
        val sampleDraftReturn                   = sample[DraftSingleDisposalReturn].copy(
          triageAnswers = triageAnswers
        )
        val fullPaymentForPenaltyInterestCharge = sample[Payment].copy(
          amount = penaltyInterestChargeAmount,
          clearingDate = penaltyInterestChargeAmountDueDate
        )

        val penaltyInterestCharge = sample[Charge].copy(
          chargeType = PenaltyInterest,
          amount = penaltyInterestChargeAmount,
          dueDate = penaltyInterestChargeAmountDueDate,
          payments = List(fullPaymentForPenaltyInterestCharge)
        )

        val fullPaymentForUkResidentReturnCharge = sample[Payment].copy(
          amount = ukResidentMainReturnChargeAmount,
          clearingDate = ukResidentMainReturnChargeDueDate
        )

        val ukResidentReturnCharge = sample[Charge].copy(
          chargeType = UkResidentReturn,
          amount = ukResidentMainReturnChargeAmount,
          dueDate = ukResidentMainReturnChargeDueDate,
          payments = List(fullPaymentForUkResidentReturnCharge)
        )
        val charges                = List(ukResidentReturnCharge, penaltyInterestCharge)
        val sampleSentReturn       = sample[ReturnSummary].copy(
          charges = charges,
          mainReturnChargeAmount = ukResidentMainReturnChargeAmount,
          submissionDate = ukResidentReturnSentDate
        )
        val subscribed             =
          sample[Subscribed].copy(
            draftReturns = List(sampleDraftReturn),
            sentReturns = List(sampleSentReturn)
          )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            SessionData.empty.copy(
              userType = Some(UserType.Individual),
              journeyStatus = Some(subscribed)
            )
          )
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("account.home.title"),
          doc =>
            doc
              .select(callToActionButton)
              .text shouldBe
              messageFromMessageKey(
                resumeDraftMessage
              )
        )
      }

      "display the resume draft link as a button when there is a draft with no completion date and no outstanding amounts to pay in sent returns" in {
        val triageAnswers                       = sample[IncompleteSingleDisposalTriageAnswers]
          .copy(completionDate = None)
        val sampleDraftReturn                   = sample[DraftSingleDisposalReturn].copy(
          triageAnswers = triageAnswers
        )
        val fullPaymentForPenaltyInterestCharge = sample[Payment].copy(
          amount = penaltyInterestChargeAmount,
          clearingDate = penaltyInterestChargeAmountDueDate
        )

        val penaltyInterestCharge = sample[Charge].copy(
          chargeType = PenaltyInterest,
          amount = penaltyInterestChargeAmount,
          dueDate = penaltyInterestChargeAmountDueDate,
          payments = List(fullPaymentForPenaltyInterestCharge)
        )

        val fullPaymentForUkResidentReturnCharge = sample[Payment].copy(
          amount = ukResidentMainReturnChargeAmount,
          clearingDate = ukResidentMainReturnChargeDueDate
        )

        val ukResidentReturnCharge = sample[Charge].copy(
          chargeType = UkResidentReturn,
          amount = ukResidentMainReturnChargeAmount,
          dueDate = ukResidentMainReturnChargeDueDate,
          payments = List(fullPaymentForUkResidentReturnCharge)
        )
        val charges                = List(ukResidentReturnCharge, penaltyInterestCharge)
        val sampleSentReturn       = sample[ReturnSummary].copy(
          charges = charges,
          mainReturnChargeAmount = ukResidentMainReturnChargeAmount,
          submissionDate = ukResidentReturnSentDate
        )
        val subscribed             =
          sample[Subscribed].copy(
            draftReturns = List(sampleDraftReturn),
            sentReturns = List(sampleSentReturn)
          )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            SessionData.empty.copy(
              userType = Some(UserType.Individual),
              journeyStatus = Some(subscribed)
            )
          )
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("account.home.title"),
          doc =>
            doc
              .select(callToActionButton)
              .text shouldBe
              messageFromMessageKey(
                resumeDraftMessage
              )
        )
      }

      "display the resume draft link as a button when there is a draft with an expected due date before any sent return due dates" in {
        val propertyAddress   = sample[UkAddress]
        val triageAnswers     = sample[CompleteSingleDisposalTriageAnswers]
          .copy(completionDate = CompletionDate(ukResidentMainReturnChargeDueDate.minusDays(31)))
        val sampleDraftReturn = sample[DraftSingleDisposalReturn].copy(
          triageAnswers = triageAnswers,
          lastUpdatedDate = LocalDate.now(),
          propertyAddress = Some(propertyAddress)
        )
        val sampleSentReturn  = sample[ReturnSummary].copy(
          charges = chargesWithChargeRaiseAndNoPayment,
          mainReturnChargeAmount = ukResidentMainReturnChargeAmount,
          submissionDate = ukResidentReturnSentDate
        )
        val subscribed        =
          sample[Subscribed].copy(
            draftReturns = List(sampleDraftReturn),
            sentReturns = List(sampleSentReturn)
          )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            SessionData.empty.copy(
              userType = Some(UserType.Individual),
              journeyStatus = Some(subscribed)
            )
          )
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("account.home.title"),
          doc =>
            doc
              .select(callToActionButton)
              .text shouldBe
              messageFromMessageKey(
                resumeDraftMessage
              )
        )
      }

      "display the view and pay link as a button when there is a sent return due date before an expected draft return due date" in {
        val propertyAddress   = sample[UkAddress]
        val triageAnswers     = sample[CompleteSingleDisposalTriageAnswers]
          .copy(completionDate = CompletionDate(ukResidentMainReturnChargeDueDate.minusDays(10)))
        val sampleDraftReturn = sample[DraftSingleDisposalReturn].copy(
          triageAnswers = triageAnswers,
          lastUpdatedDate = LocalDate.now(),
          propertyAddress = Some(propertyAddress)
        )
        val sampleSentReturn  = sample[ReturnSummary].copy(
          charges = chargesWithChargeRaiseAndNoPayment,
          mainReturnChargeAmount = ukResidentMainReturnChargeAmount,
          submissionDate = ukResidentReturnSentDate
        )
        val subscribed        =
          sample[Subscribed].copy(
            draftReturns = List(sampleDraftReturn),
            sentReturns = List(sampleSentReturn)
          )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            SessionData.empty.copy(
              userType = Some(UserType.Individual),
              journeyStatus = Some(subscribed)
            )
          )
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("account.home.title"),
          doc =>
            doc
              .select(callToActionButton)
              .text shouldBe
              messageFromMessageKey(
                viewAndPayMessage
              )
        )
      }

      "display the view and pay link as a button when there is a sent return due date and no draft return" in {
        val sampleSentReturn = sample[ReturnSummary].copy(
          charges = chargesWithChargeRaiseAndNoPayment,
          mainReturnChargeAmount = ukResidentMainReturnChargeAmount,
          submissionDate = ukResidentReturnSentDate
        )
        val subscribed       =
          sample[Subscribed].copy(
            draftReturns = List.empty[DraftReturn],
            sentReturns = List(sampleSentReturn)
          )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            SessionData.empty.copy(
              userType = Some(UserType.Individual),
              journeyStatus = Some(subscribed)
            )
          )
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("account.home.title"),
          doc =>
            doc
              .select(callToActionButton)
              .text shouldBe
              messageFromMessageKey(
                viewAndPayMessage
              )
        )
      }

      "display the start new return link as a button when there is a sent return with no payment due and no draft return" in {
        val fullPaymentForPenaltyInterestCharge = sample[Payment].copy(
          amount = penaltyInterestChargeAmount,
          clearingDate = penaltyInterestChargeAmountDueDate
        )

        val penaltyInterestCharge = sample[Charge].copy(
          chargeType = PenaltyInterest,
          amount = penaltyInterestChargeAmount,
          dueDate = penaltyInterestChargeAmountDueDate,
          payments = List(fullPaymentForPenaltyInterestCharge)
        )

        val fullPaymentForUkResidentReturnCharge = sample[Payment].copy(
          amount = ukResidentMainReturnChargeAmount,
          clearingDate = ukResidentMainReturnChargeDueDate
        )

        val ukResidentReturnCharge = sample[Charge].copy(
          chargeType = UkResidentReturn,
          amount = ukResidentMainReturnChargeAmount,
          dueDate = ukResidentMainReturnChargeDueDate,
          payments = List(fullPaymentForUkResidentReturnCharge)
        )
        val charges                = List(ukResidentReturnCharge, penaltyInterestCharge)
        val sampleSentReturn       = sample[ReturnSummary].copy(
          charges = charges,
          mainReturnChargeAmount = ukResidentMainReturnChargeAmount,
          submissionDate = ukResidentReturnSentDate
        )
        val subscribed             =
          sample[Subscribed].copy(
            draftReturns = List.empty[DraftReturn],
            sentReturns = List(sampleSentReturn)
          )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            SessionData.empty.copy(
              userType = Some(UserType.Individual),
              journeyStatus = Some(subscribed)
            )
          )
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("account.home.title"),
          doc =>
            doc
              .select(callToActionButton)
              .text shouldBe
              messageFromMessageKey(
                startNewReturnMessage
              )
        )
      }

      "display draft returns on the home page when there is no property address" in {
        val triageAnswers     = sample[CompleteSingleDisposalTriageAnswers]
          .copy(completionDate = CompletionDate(LocalDate.now().minusMonths(1)))
        val sampleDraftReturn = sample[DraftSingleDisposalReturn]
          .copy(
            triageAnswers = triageAnswers,
            lastUpdatedDate = LocalDate.now(),
            propertyAddress = None
          )
        val subscribed        =
          sample[Subscribed].copy(draftReturns = List(sampleDraftReturn))

        val completionDate: LocalDate = sampleDraftReturn.triageAnswers match {
          case a: CompleteSingleDisposalTriageAnswers => a.completionDate.value
          case _                                      => sys.error("Error")
        }

        val expectedDraftReturnSendAndPayBy = completionDate.plusDays(30)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            SessionData.empty.copy(
              userType = Some(UserType.Individual),
              journeyStatus = Some(subscribed)
            )
          )
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("account.home.title"),
          { doc =>
            doc
              .select(s"#draftReturnLastUpdatedDate-${sampleDraftReturn.id}")
              .text() shouldBe
              messages(
                "drafts.list.lastUpdated",
                govShortDisplayFormat(sampleDraftReturn.lastUpdatedDate)
              )
            doc
              .select(s"#draftReturnsendAndPayBy-${sampleDraftReturn.id} > h4")
              .text() shouldBe
              messages(
                "drafts.list.sendAndPayBy"
              ) + " " + govShortDisplayFormat(expectedDraftReturnSendAndPayBy)
            doc
              .select(s"#draftReturn-${sampleDraftReturn.id} > h3")
              .text() shouldBe messages(
              "drafts.list.completionDate"
            ) + " " + govShortDisplayFormat(
              completionDate
            )
          }
        )
      }

      "display draft returns on the home page when there is property address" in {
        val propertyAddress   = sample[UkAddress]
        val triageAnswers     = sample[CompleteSingleDisposalTriageAnswers]
          .copy(completionDate = CompletionDate(LocalDate.now().minusMonths(1)))
        val sampleDraftReturn = sample[DraftSingleDisposalReturn].copy(
          triageAnswers = triageAnswers,
          lastUpdatedDate = LocalDate.now(),
          propertyAddress = Some(propertyAddress)
        )
        val subscribed        =
          sample[Subscribed].copy(draftReturns = List(sampleDraftReturn))

        val completionDate: LocalDate = sampleDraftReturn.triageAnswers match {
          case a: CompleteSingleDisposalTriageAnswers => a.completionDate.value
          case _                                      => sys.error("Error")
        }

        val expectedDraftReturnSendAndPayBy = completionDate.plusDays(30)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            SessionData.empty.copy(
              userType = Some(UserType.Individual),
              journeyStatus = Some(subscribed)
            )
          )
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("account.home.title"),
          { doc =>
            doc
              .select(s"#draftReturnLastUpdatedDate-${sampleDraftReturn.id}")
              .text() shouldBe
              messages(
                "drafts.list.lastUpdated",
                govShortDisplayFormat(sampleDraftReturn.lastUpdatedDate)
              )
            doc
              .select(s"#draftReturnsendAndPayBy-${sampleDraftReturn.id} > h4")
              .text() shouldBe
              messages(
                "drafts.list.sendAndPayBy"
              ) + " " + govShortDisplayFormat(expectedDraftReturnSendAndPayBy)
            doc
              .select(s"#draftReturn-${sampleDraftReturn.id} > h3")
              .text() shouldBe messages(
              "drafts.list.disposalDetails"
            ) + " " + propertyAddress.line1 + " " + propertyAddress.getAddressLines.drop(1).mkString(", ")
          }
        )
      }

      "display draft returns on the home page when there is no completion date" in {
        val propertyAddress   = sample[UkAddress]
        val triageAnswers     = sample[IncompleteSingleDisposalTriageAnswers]
          .copy(completionDate = None)
        val sampleDraftReturn = sample[DraftSingleDisposalReturn].copy(
          triageAnswers = triageAnswers,
          lastUpdatedDate = LocalDate.now(),
          propertyAddress = Some(propertyAddress)
        )
        val subscribed        =
          sample[Subscribed].copy(draftReturns = List(sampleDraftReturn))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            SessionData.empty.copy(
              userType = Some(UserType.Individual),
              journeyStatus = Some(subscribed)
            )
          )
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("account.home.title"),
          { doc =>
            doc
              .select(s"#resumeDraftReturn-${sampleDraftReturn.id}")
              .text() shouldBe "Complete return"
            doc
              .select(s"#draftReturnLastUpdatedDate-${sampleDraftReturn.id}")
              .text() shouldBe
              messages(
                "drafts.list.lastUpdated",
                govShortDisplayFormat(sampleDraftReturn.lastUpdatedDate)
              )
            doc
              .select(s"#draftReturnsendAndPayBy-${sampleDraftReturn.id} > h4")
              .text() shouldBe empty
            doc
              .select(s"#draftReturn-${sampleDraftReturn.id} > h4")
              .text() shouldBe empty
          }
        )
      }

      "display sent returns on the home page when there is no charge raise and no payments have been made" in {

        val sentReturn = sample[ReturnSummary].copy(
          charges = chargesWithoutChargeRaiseAndNoPayment,
          mainReturnChargeAmount = ukResidentMainReturnChargeAmount,
          submissionDate = ukResidentReturnSentDate
        )
        val subscribed = sample[Subscribed].copy(sentReturns = List(sentReturn))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            SessionData.empty.copy(
              userType = Some(UserType.Individual),
              journeyStatus = Some(subscribed)
            )
          )
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("account.home.title"),
          { doc =>
            extractAmount(
              doc.select(s"#leftToPay-${sentReturn.submissionId}").text
            )       shouldBe formatAmountOfMoneyWithPoundSign(
              ukResidentMainReturnChargeAmount.inPounds()
            )
            doc
              .select(s"#paymentDue-${sentReturn.submissionId}")
              .text shouldBe govShortDisplayFormat(
              ukResidentMainReturnChargeDueDate
            )
            extractAmount(
              doc.select(s"#taxOwed-${sentReturn.submissionId}").text
            )       shouldBe formatAmountOfMoneyWithPoundSign(
              ukResidentMainReturnChargeAmount.inPounds()
            )
            doc
              .select(s"#viewSentReturn-${sentReturn.submissionId}")
              .text shouldBe "View and pay"
            doc
              .select(s"#sentDate-${sentReturn.submissionId}")
              .text shouldBe messages(
              "returns.list.sentDate",
              govShortDisplayFormat(ukResidentReturnSentDate)
            )
          }
        )
      }

      "display sent returns on the home page when there is a charge raise and no payments have been made" in {

        val sentReturn = sample[ReturnSummary].copy(
          charges = chargesWithChargeRaiseAndNoPayment,
          mainReturnChargeAmount = ukResidentMainReturnChargeAmount,
          submissionDate = ukResidentReturnSentDate
        )
        val subscribed = sample[Subscribed].copy(sentReturns = List(sentReturn))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            SessionData.empty.copy(
              userType = Some(UserType.Individual),
              journeyStatus = Some(subscribed)
            )
          )
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("account.home.title"),
          { doc =>
            extractAmount(
              doc.select(s"#leftToPay-${sentReturn.submissionId}").text
            )       shouldBe formatAmountOfMoneyWithPoundSign(
              ukResidentMainReturnChargeAmount
                .inPounds() + penaltyInterestChargeAmount.inPounds()
            )
            doc
              .select(s"#paymentDue-${sentReturn.submissionId}")
              .text shouldBe govShortDisplayFormat(
              ukResidentMainReturnChargeDueDate
            )
            extractAmount(
              doc.select(s"#taxOwed-${sentReturn.submissionId}").text
            )       shouldBe formatAmountOfMoneyWithPoundSign(
              ukResidentMainReturnChargeAmount.inPounds()
            )
            doc
              .select(s"#viewSentReturn-${sentReturn.submissionId}")
              .text shouldBe "View and pay"
            doc
              .select(s"#sentDate-${sentReturn.submissionId}")
              .text shouldBe messages(
              "returns.list.sentDate",
              govShortDisplayFormat(ukResidentReturnSentDate)
            )
          }
        )
      }

      "display sent returns on the home page when there is a charge raise and partial payment have been made for return" in {

        val sentReturn = sample[ReturnSummary].copy(
          charges = chargesWithChargeRaiseAndPartialPayment,
          mainReturnChargeAmount = ukResidentMainReturnChargeAmount,
          submissionDate = ukResidentReturnSentDate
        )
        val subscribed = sample[Subscribed].copy(sentReturns = List(sentReturn))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            SessionData.empty.copy(
              userType = Some(UserType.Individual),
              journeyStatus = Some(subscribed)
            )
          )
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("account.home.title"),
          { doc =>
            extractAmount(
              doc.select(s"#leftToPay-${sentReturn.submissionId}").text
            )       shouldBe formatAmountOfMoneyWithPoundSign(
              ukResidentMainReturnChargeAmount
                .inPounds() + penaltyInterestChargeAmount
                .inPounds() - partialPaymentForUkResidentMainReturnChargeAmount
                .inPounds()
            )
            doc
              .select(s"#paymentDue-${sentReturn.submissionId}")
              .text shouldBe govShortDisplayFormat(
              ukResidentMainReturnChargeDueDate
            )
            extractAmount(
              doc.select(s"#taxOwed-${sentReturn.submissionId}").text
            )       shouldBe formatAmountOfMoneyWithPoundSign(
              ukResidentMainReturnChargeAmount.inPounds()
            )
            doc
              .select(s"#viewSentReturn-${sentReturn.submissionId}")
              .text shouldBe "View and pay"
            doc
              .select(s"#sentDate-${sentReturn.submissionId}")
              .text shouldBe messages(
              "returns.list.sentDate",
              govShortDisplayFormat(ukResidentReturnSentDate)
            )
          }
        )
      }

      "display sent returns on the home page when there is a charge raise and full payment have been made for return" in {

        val sentReturn = sample[ReturnSummary].copy(
          charges = chargesWithChargeRaiseAndFullPayment,
          mainReturnChargeAmount = ukResidentMainReturnChargeAmount,
          submissionDate = ukResidentReturnSentDate
        )
        val subscribed = sample[Subscribed].copy(sentReturns = List(sentReturn))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            SessionData.empty.copy(
              userType = Some(UserType.Individual),
              journeyStatus = Some(subscribed)
            )
          )
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("account.home.title"),
          { doc =>
            extractAmount(
              doc.select(s"#leftToPay-${sentReturn.submissionId}").text
            )       shouldBe formatAmountOfMoneyWithPoundSign(
              ukResidentMainReturnChargeAmount
                .inPounds() + penaltyInterestChargeAmount
                .inPounds() - fullPaymentForUkResidentReturnCharge.amount
                .inPounds()
            )
            doc
              .select(s"#paymentDue-${sentReturn.submissionId}")
              .text shouldBe govShortDisplayFormat(
              penaltyInterestChargeAmountDueDate
            )
            extractAmount(
              doc.select(s"#taxOwed-${sentReturn.submissionId}").text
            )       shouldBe formatAmountOfMoneyWithPoundSign(
              ukResidentMainReturnChargeAmount.inPounds()
            )
            doc
              .select(s"#viewSentReturn-${sentReturn.submissionId}")
              .text shouldBe "View and pay"
            doc
              .select(s"#sentDate-${sentReturn.submissionId}")
              .text shouldBe messages(
              "returns.list.sentDate",
              govShortDisplayFormat(ukResidentReturnSentDate)
            )
          }
        )
      }

      "display sent returns on the home page when there is a charge raise and full payments have been made for return and charge raise" in {

        val fullPaymentForPenaltyInterestCharge = sample[Payment].copy(
          amount = penaltyInterestChargeAmount,
          clearingDate = penaltyInterestChargeAmountDueDate
        )

        val penaltyInterestCharge = sample[Charge].copy(
          chargeType = PenaltyInterest,
          amount = penaltyInterestChargeAmount,
          dueDate = penaltyInterestChargeAmountDueDate,
          payments = List(fullPaymentForPenaltyInterestCharge)
        )

        val fullPaymentForUkResidentReturnCharge = sample[Payment].copy(
          amount = ukResidentMainReturnChargeAmount,
          clearingDate = ukResidentMainReturnChargeDueDate
        )

        val ukResidentReturnCharge = sample[Charge].copy(
          chargeType = UkResidentReturn,
          amount = ukResidentMainReturnChargeAmount,
          dueDate = ukResidentMainReturnChargeDueDate,
          payments = List(fullPaymentForUkResidentReturnCharge)
        )
        val charges                = List(ukResidentReturnCharge, penaltyInterestCharge)
        val sentReturn             = sample[ReturnSummary].copy(
          charges = charges,
          mainReturnChargeAmount = ukResidentMainReturnChargeAmount,
          submissionDate = ukResidentReturnSentDate
        )
        val subscribed             = sample[Subscribed].copy(sentReturns = List(sentReturn))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            SessionData.empty.copy(
              userType = Some(UserType.Individual),
              journeyStatus = Some(subscribed)
            )
          )
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("account.home.title"),
          { doc =>
            extractAmount(
              doc.select(s"#leftToPay-${sentReturn.submissionId}").text
            )       shouldBe formatAmountOfMoneyWithPoundSign(
              ukResidentMainReturnChargeAmount
                .inPounds() + penaltyInterestChargeAmount
                .inPounds() - fullPaymentForUkResidentReturnCharge.amount
                .inPounds() -
                fullPaymentForPenaltyInterestCharge.amount.inPounds()
            )
            doc
              .select(s"#paymentDue-${sentReturn.submissionId}")
              .text shouldBe ""
            extractAmount(
              doc.select(s"#taxOwed-${sentReturn.submissionId}").text
            )       shouldBe formatAmountOfMoneyWithPoundSign(
              ukResidentMainReturnChargeAmount.inPounds()
            )
            doc
              .select(s"#viewSentReturn-${sentReturn.submissionId}")
              .text shouldBe "View return"
            doc
              .select(s"#sentDate-${sentReturn.submissionId}")
              .text shouldBe messages(
              "returns.list.sentDate",
              govShortDisplayFormat(ukResidentReturnSentDate)
            )
          }
        )
      }

      "display the home page" in {
        forAll { (userType: Option[UserType], subscribed: Subscribed) =>
          whenever(!userType.contains(UserType.Agent)) {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                SessionData.empty.copy(
                  userType = userType,
                  journeyStatus = Some(subscribed.copy(agentReferenceNumber = None))
                )
              )
            }
            checkPageIsDisplayed(
              performAction(),
              messageFromMessageKey(
                "account.home.title"
              ),
              { doc =>
                if (subscribed.sentReturns.isEmpty && subscribed.draftReturns.isEmpty)
                  doc
                    .select("#content > article > div > div > div > a.button")
                    .text should include(
                    messageFromMessageKey(
                      "account.home.button.start-a-new-return"
                    )
                  )
                else if (subscribed.totalLeftToPay().isZero && subscribed.draftReturns.isEmpty)
                  doc
                    .select("#content a.button")
                    .text should include(
                    messageFromMessageKey(
                      "account.home.button.start-a-new-return"
                    )
                  )
                else
                  doc
                    .select(
                      "#content > article > div:nth-child(1) > div > p > a"
                    )
                    .text should include(
                    messageFromMessageKey(
                      "account.home.button.start-a-new-return"
                    )
                  )

                doc
                  .select("#content > article > div:nth-child(1) > div > span")
                  .text shouldNot include(
                  messageFromMessageKey("account.agent.prefix")
                )
                doc.select("h1 > p").text should include(
                  messageFromMessageKey(
                    "account.home.subtitle",
                    subscribed.subscribedDetails.cgtReference.value
                  )
                )
                if (subscribed.sentReturns.nonEmpty) {
                  doc.select("h2").text should include(
                    messageFromMessageKey(
                      "account.totalLeftToPay"
                    )
                  )
                  if (subscribed.totalLeftToPay() > AmountInPence.zero)
                    doc
                      .select(
                        "#content > article > div.grid-row.account-home-heading > div.column-one-third.account-due > a"
                      )
                      .attr(
                        "href"
                      )               shouldBe controllers.accounts.homepage.routes.HomePageController
                      .payTotalAmountLeftToPay()
                      .url
                  else
                    doc.body().text() shouldNot include(
                      controllers.accounts.homepage.routes.HomePageController
                        .payTotalAmountLeftToPay()
                        .url
                    )
                } else
                  doc.body().text shouldNot include(
                    messageFromMessageKey(
                      "account.totalLeftToPay"
                    )
                  )
              }
            )
          }
        }
      }

      "display the home page for agents" in {
        val subscribed  = sample[Subscribed]
          .copy(agentReferenceNumber = Some(sample[AgentReferenceNumber]))
        val sessionData =
          SessionData.empty.copy(journeyStatus = Some(subscribed))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionData)
        }
        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey(
            "account.home.title"
          ),
          { doc =>
            doc
              .select("#content > article > div:nth-child(1) > div > span")
              .text() should include(
              messageFromMessageKey("account.agent.prefix")
            )
            doc
              .select("#content > article > div:nth-child(1) > div > span")
              .text() should include(
              subscribed.subscribedDetails.makeAccountName()
            )
          },
          OK
        )
      }

      {
        val subscribed = sample[Subscribed]

        val startingNewDraftReturn = StartingNewDraftReturn(
          subscribed.subscribedDetails,
          subscribed.ggCredId,
          subscribed.agentReferenceNumber,
          Right(sample[IncompleteSingleDisposalTriageAnswers]),
          None,
          Some(subscribed.sentReturns)
        )

        val fillingOurReturn    = FillingOutReturn(
          subscribed.subscribedDetails,
          subscribed.ggCredId,
          subscribed.agentReferenceNumber,
          sample[DraftSingleDisposalReturn],
          Some(subscribed.sentReturns)
        )
        val justSubmittedReturn = JustSubmittedReturn(
          subscribed.subscribedDetails,
          subscribed.ggCredId,
          subscribed.agentReferenceNumber,
          sample[CompleteSingleDisposalReturn],
          sample[SubmitReturnResponse]
        )

        val viewingReturn = ViewingReturn(
          subscribed.subscribedDetails,
          subscribed.ggCredId,
          subscribed.agentReferenceNumber,
          sample[CompleteSingleDisposalReturn],
          sample[ReturnSummary]
        )

        val submitReturnFailed = SubmitReturnFailed(
          subscribed.subscribedDetails,
          subscribed.ggCredId,
          subscribed.agentReferenceNumber
        )

        List(
          startingNewDraftReturn,
          fillingOurReturn,
          justSubmittedReturn,
          viewingReturn,
          submitReturnFailed
        ).foreach { journeyStatus =>
          s"convert a ${journeyStatus.getClass.getSimpleName} to Subscribed journey status" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                SessionData.empty.copy(
                  journeyStatus = Some(journeyStatus),
                  userType = Some(UserType.Individual)
                )
              )
              mockGetReturnsList(subscribed.subscribedDetails.cgtReference)(
                Right(subscribed.sentReturns)
              )
              mockGetDraftReturns(
                subscribed.subscribedDetails.cgtReference,
                subscribed.sentReturns
              )(
                Right(subscribed.draftReturns)
              )
              mockStoreSession(
                SessionData.empty.copy(
                  journeyStatus = Some(subscribed),
                  userType = Some(UserType.Individual)
                )
              )(Right(()))
            }

            val result = performAction()
            status(result)        shouldBe OK
            contentAsString(result) should include(
              messageFromMessageKey("account.home.title")
            )
          }

          "show an error page" when {

            s"the conversion from ${journeyStatus.getClass.getSimpleName} is successful but " +
              "there is an error updating the session" in {
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  SessionData.empty.copy(
                    journeyStatus = Some(journeyStatus),
                    userType = Some(UserType.Individual)
                  )
                )
                mockGetReturnsList(subscribed.subscribedDetails.cgtReference)(
                  Right(subscribed.sentReturns)
                )
                mockGetDraftReturns(
                  subscribed.subscribedDetails.cgtReference,
                  subscribed.sentReturns
                )(
                  Right(subscribed.draftReturns)
                )
                mockStoreSession(
                  SessionData.empty.copy(
                    journeyStatus = Some(subscribed),
                    userType = Some(UserType.Individual)
                  )
                )(Left(Error("")))
              }

              checkIsTechnicalErrorPage(performAction())
            }

            s"the conversion from ${journeyStatus.getClass.getSimpleName} is successful but " +
              "there is an error getting the draft returns" in {
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  SessionData.empty.copy(
                    journeyStatus = Some(journeyStatus),
                    userType = Some(UserType.Individual)
                  )
                )
                mockGetReturnsList(subscribed.subscribedDetails.cgtReference)(
                  Right(subscribed.sentReturns)
                )
                mockGetDraftReturns(
                  subscribed.subscribedDetails.cgtReference,
                  subscribed.sentReturns
                )(
                  Left(Error(""))
                )
              }

              checkIsTechnicalErrorPage(performAction())
            }

            s"the conversion from ${journeyStatus.getClass.getSimpleName} is successful but " +
              "there is an error getting the list of returns" in {
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  SessionData.empty.copy(
                    journeyStatus = Some(journeyStatus),
                    userType = Some(UserType.Individual)
                  )
                )
                mockGetReturnsList(subscribed.subscribedDetails.cgtReference)(
                  Left(Error(""))
                )
              }

              checkIsTechnicalErrorPage(performAction())
            }

          }
        }
      }

    }

    "handling requests to start a new return" must {

      def performAction(): Future[Result] =
        controller.startNewReturn()(FakeRequest())

      redirectToStartWhenInvalidJourney(
        performAction,
        {
          case _: Subscribed => true
          case _             => false
        }
      )

      "show an error page" when {

        "there is an error updating the session" in {
          val subscribed = sample[Subscribed].copy(
            draftReturns = List.empty,
            sentReturns = List.empty
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionDataWithSubscribed(subscribed))
            mockStoreSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  StartingNewDraftReturn(
                    subscribed.subscribedDetails,
                    subscribed.ggCredId,
                    subscribed.agentReferenceNumber,
                    Right(IncompleteSingleDisposalTriageAnswers.empty),
                    None,
                    Some(subscribed.sentReturns)
                  )
                )
              )
            )(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction())
        }
      }

      "redirect to the who is the individual reporting for page" when {

        "the subscribed user type is individual" in {
          val subscribed = sample[Subscribed]
            .copy(
              subscribedDetails = sample[SubscribedDetails]
                .copy(name = Right(sample[IndividualName])),
              draftReturns = List.empty,
              sentReturns = List.empty
            )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionDataWithSubscribed(subscribed))
            mockStoreSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  StartingNewDraftReturn(
                    subscribed.subscribedDetails,
                    subscribed.ggCredId,
                    subscribed.agentReferenceNumber,
                    Right(IncompleteSingleDisposalTriageAnswers.empty),
                    None,
                    Some(subscribed.sentReturns)
                  )
                )
              )
            )(Right(()))
          }

          checkIsRedirect(
            performAction(),
            controllers.returns.triage.routes.CommonTriageQuestionsController
              .whoIsIndividualRepresenting()
          )
        }

      }

      "redirect to the number of disposals page" when {

        "the subscribed user type is trust" in {
          val subscribed = sample[Subscribed].copy(
            subscribedDetails = sample[SubscribedDetails].copy(name = Left(sample[TrustName])),
            draftReturns = List.empty,
            sentReturns = List.empty
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionDataWithSubscribed(subscribed))
            mockStoreSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  StartingNewDraftReturn(
                    subscribed.subscribedDetails,
                    subscribed.ggCredId,
                    subscribed.agentReferenceNumber,
                    Right(IncompleteSingleDisposalTriageAnswers.empty),
                    None,
                    Some(subscribed.sentReturns)
                  )
                )
              )
            )(Right(()))
          }

          checkIsRedirect(
            performAction(),
            controllers.returns.triage.routes.CommonTriageQuestionsController
              .howManyProperties()
          )
        }

      }

      "redirect to the subsequent return exit page" when {

        "the session has a draft return" in {
          val subscribed = sample[Subscribed].copy(
            subscribedDetails = sample[SubscribedDetails].copy(name = Left(sample[TrustName])),
            sentReturns = List.empty,
            draftReturns = List(sample[DraftSingleDisposalReturn])
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionDataWithSubscribed(subscribed))
          }

          checkIsRedirect(
            performAction(),
            routes.HomePageController.exitForSubsequentReturn()
          )
        }

        "there is submitted and draft returns" in {
          val subscribed =
            sample[Subscribed].copy(
              subscribedDetails = sample[SubscribedDetails].copy(name = Left(sample[TrustName])),
              sentReturns = List(sample[ReturnSummary]),
              draftReturns = List(sample[DraftSingleDisposalReturn])
            )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionDataWithSubscribed(subscribed))
          }

          checkIsRedirect(
            performAction(),
            routes.HomePageController.exitForSubsequentReturn()
          )
        }

      }

    }

    "handling requests to subsequent return exit page" must {

      def performAction(): Future[Result] =
        controller.exitForSubsequentReturn()(FakeRequest())

      val expectedPageTitleMessageKey = "subsequentReturnExit.title"

      "display the page" when {

        def test(sessionData: SessionData, expectedBackLink: Call): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionData)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(expectedPageTitleMessageKey),
            doc => doc.select("#back").attr("href") shouldBe expectedBackLink.url
          )
        }

        "there is a draft or subsequent return" in {
          test(
            SessionData.empty.copy(
              journeyStatus = Some(sample[Subscribed])
            ),
            routes.HomePageController.homepage()
          )
        }

        "the user is on a starting new draft return journey" in {
          test(
            SessionData.empty.copy(
              journeyStatus = Some(sample[StartingNewDraftReturn])
            ),
            controllers.returns.representee.routes.RepresenteeController.isFirstReturn()
          )
        }

        "the user is on a filling out return journey" in {
          test(
            SessionData.empty.copy(
              journeyStatus = Some(sample[FillingOutReturn])
            ),
            controllers.returns.representee.routes.RepresenteeController.isFirstReturn()
          )
        }

      }

    }

    "handling requests to resume a draft return" must {

      def performAction(id: UUID): Future[Result] =
        controller.resumeDraftReturn(id)(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(
        () => performAction(UUID.randomUUID()),
        {
          case _: Subscribed => true
          case _             => false
        }
      )

      val draftReturn = sample[DraftSingleDisposalReturn]

      val subscribed = sample[Subscribed].copy(draftReturns = List(draftReturn))

      val sessionWithSubscribed =
        SessionData.empty.copy(journeyStatus = Some(subscribed))

      val fillingOutReturn = FillingOutReturn(
        subscribed.subscribedDetails,
        subscribed.ggCredId,
        subscribed.agentReferenceNumber,
        draftReturn,
        Some(subscribed.sentReturns)
      )

      "show an error page" when {

        "no draft return can be found with the given id" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionWithSubscribed)
          }

          checkIsTechnicalErrorPage(performAction(UUID.randomUUID()))
        }

        "there is an error updating the session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionWithSubscribed)
            mockStoreSession(
              SessionData.empty.copy(journeyStatus = Some(fillingOutReturn))
            )(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction(draftReturn.id))
        }
      }

      "redirect to the task list page" when {

        "a draft return can be found with the given id and the session is successfully updated" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionWithSubscribed)
            mockStoreSession(
              SessionData.empty.copy(journeyStatus = Some(fillingOutReturn))
            )(Right(()))
          }

          checkIsRedirect(
            performAction(draftReturn.id),
            controllers.returns.routes.TaskListController.taskList()
          )
        }

      }

    }

    "handling requests to view a return" must {

      def performAction(submissionId: String): Future[Result] =
        controller.viewSentReturn(submissionId)(FakeRequest())

      val returnSummary = sample[ReturnSummary]
      val subscribed    =
        sample[Subscribed].copy(sentReturns = List(returnSummary))
      val sessionData   = SessionData.empty.copy(journeyStatus = Some(subscribed))

      redirectToStartWhenInvalidJourney(
        () => performAction(""),
        {
          case _: Subscribed => true
          case _             => false
        }
      )

      "return a bad request" when {

        "the user does not have a return with the given submission id" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionData)
          }

          status(
            performAction(returnSummary.submissionId + "abc")
          ) shouldBe NOT_FOUND
        }

      }

      "show an error page" when {

        "there is an error getting the return" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionData)
            mockDisplayReturn(
              subscribed.subscribedDetails.cgtReference,
              returnSummary.submissionId
            )(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction(returnSummary.submissionId))
        }

        "there is an error updating the session" in {
          val completeReturn = sample[CompleteSingleDisposalReturn]

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionData)
            mockDisplayReturn(
              subscribed.subscribedDetails.cgtReference,
              returnSummary.submissionId
            )(
              Right(completeReturn)
            )
            mockStoreSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  ViewingReturn(
                    subscribed.subscribedDetails,
                    subscribed.ggCredId,
                    subscribed.agentReferenceNumber,
                    completeReturn,
                    returnSummary
                  )
                )
              )
            )(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction(returnSummary.submissionId))
        }

      }

      "redirect to the view return screen" when {

        "the return is successfully retrieved and the session is updated" in {
          val completeReturn = sample[CompleteSingleDisposalReturn]

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionData)
            mockDisplayReturn(
              subscribed.subscribedDetails.cgtReference,
              returnSummary.submissionId
            )(
              Right(completeReturn)
            )
            mockStoreSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  ViewingReturn(
                    subscribed.subscribedDetails,
                    subscribed.ggCredId,
                    subscribed.agentReferenceNumber,
                    completeReturn,
                    returnSummary
                  )
                )
              )
            )(Right(()))
          }

          checkIsRedirect(
            performAction(returnSummary.submissionId),
            controllers.returns.routes.ViewReturnController.displayReturn()
          )
        }

      }

    }

    "handling requests to pay a return" must {

      def performAction(): Future[Result] =
        controller.payTotalAmountLeftToPay()(FakeRequest())

      redirectToStartWhenInvalidJourney(
        performAction,
        {
          case _: Subscribed => true
          case _             => false
        }
      )

      "show an error page" when {

        "there is an error starting a payments journey" in {
          val subscribed = sample[Subscribed]

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(journeyStatus = Some(subscribed))
            )
            mockStartPaymentJourney(
              subscribed.subscribedDetails.cgtReference,
              None,
              subscribed.totalLeftToPay(),
              routes.HomePageController.homepage(),
              routes.HomePageController.homepage()
            )(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction())
        }

      }

      "redirect to the payment journey" when {

        "the payment journey is successfully started" in {
          val subscribed      = sample[Subscribed]
          val paymentsJourney = sample[PaymentsJourney]

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(journeyStatus = Some(subscribed))
            )
            mockStartPaymentJourney(
              subscribed.subscribedDetails.cgtReference,
              None,
              subscribed.totalLeftToPay(),
              routes.HomePageController.homepage(),
              routes.HomePageController.homepage()
            )(Right(paymentsJourney))
          }

          checkIsRedirect(performAction(), paymentsJourney.nextUrl)
        }

      }

    }

  }

}
