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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns

import cats.data.EitherT
import cats.instances.future.*
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import org.scalacheck.Gen
import uk.gov.hmrc.cgtpropertydisposalsfrontend.SampledScalaCheck
import play.api.i18n.{Messages, MessagesApi, MessagesImpl}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.{Call, Request, Result}
import play.api.test.FakeRequest
import play.api.test.Helpers.*
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.CheckAllAnswersAndSubmitControllerSpec.*
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.acquisitiondetails.RebasingEligibilityUtil
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{StartingToAmendReturn, ViewingReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.TimeUtils.govShortDisplayFormat
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address.UkAddress
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.{Country, Postcode}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.*
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.ChargeType.{DeltaCharge, PenaltyInterest, UkResidentReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.MoneyUtils.formatAmountOfMoneyWithPoundSign
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.PaymentMethod.DirectDebit
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.AddressGen.*
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.CompleteReturnGen.{*, given}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.ExampleCompanyDetailsAnswersGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.IdGen.*
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.JourneyStatusGen.*
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.MoneyGen.*
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.NameGen.*
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.ReliefDetailsGen.completeReliefDetailsAnswersGen
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.ReturnGen.*
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.SubscribedDetailsGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.{AgentReferenceNumber, CgtReference}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.{IndividualName, TrustName}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.SubscribedDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.CompleteReturn.{CompleteMultipleDisposalsReturn, CompleteMultipleIndirectDisposalReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ExampleCompanyDetailsAnswers.CompleteExampleCompanyDetailsAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ReliefDetailsAnswers.CompleteReliefDetailsAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{AssetType, ReturnSummary, ReturnType}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, SessionData, TimeUtils, UserType}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.PaymentsService
import uk.gov.hmrc.http.HeaderCarrier

import java.time.LocalDate
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.jdk.CollectionConverters.*

class ViewReturnControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with SampledScalaCheck
    with RedirectToStartBehaviour {

  private val mockPaymentsService = mock[PaymentsService]

  protected override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore),
      bind[PaymentsService].toInstance(mockPaymentsService)
    )

  private lazy val controller = instanceOf[ViewReturnController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  implicit val messages: Messages = MessagesImpl(lang, messagesApi)

  val rebasingUtil: RebasingEligibilityUtil = new RebasingEligibilityUtil()

  private def mockStartPaymentJourney(
    cgtReference: CgtReference,
    chargeReference: Option[String],
    amount: AmountInPence,
    dueDate: Option[LocalDate],
    returnUrl: Call,
    backUrl: Call
  )(response: Either[Error, PaymentsJourney]) =
    (
      mockPaymentsService
        .startPaymentJourney(
          _: CgtReference,
          _: Option[String],
          _: AmountInPence,
          _: Option[LocalDate],
          _: Call,
          _: Call
        )(using
          _: HeaderCarrier,
          _: Request[?]
        )
      )
      .expects(cgtReference, chargeReference, amount, dueDate, returnUrl, backUrl, *, *)
      .returning(EitherT.fromEither[Future](response))

  def setNameForUserType(
    userType: UserType
  ): Either[TrustName, IndividualName] =
    userType match {
      case UserType.Organisation => Left(sample[TrustName])
      case _                     => Right(sample[IndividualName])
    }

  def setAgentReferenceNumber(
    userType: UserType
  ): Option[AgentReferenceNumber] =
    userType match {
      case UserType.Agent => Some(sample[AgentReferenceNumber])
      case _              => None
    }

  private def deriveUserKey(isAgent: Boolean, isATrust: Boolean) =
    if (isAgent) ".agent" else if (isATrust) ".trust" else ""

  private def nameLabelUserKey(isAgent: Boolean, isATrust: Boolean) =
    if (isATrust) ".trust" else if (isAgent) ".agent" else ""

  val acceptedUserTypeGen: Gen[UserType] =
    Gen.oneOf(UserType.Agent, UserType.Organisation, UserType.Individual)

  "ViewReturnController" when {

    "handling requests to display a return" must {

      def performAction(): Future[Result] =
        controller.displayReturn()(FakeRequest())

      val ukResidentMainReturnChargeAmount: AmountInPence  = AmountInPence(10000)
      val ukResidentDeltaReturnChargeAmount: AmountInPence = AmountInPence(1000)
      val ukResidentReturnSentDate: LocalDate              = LocalDate.now()
      val ukResidentMainReturnChargeDueDate: LocalDate     =
        LocalDate.now().plusMonths(1)
      val ukResidentDeltaChargeDueDate: LocalDate          = LocalDate.of(2022, 1, 31)
      val penaltyInterestChargeAmount: AmountInPence       = AmountInPence(10000)
      val penaltyInterestChargeAmountDueDate: LocalDate    =
        LocalDate.now().plusMonths(2)

      val penaltyInterestCharge = sample[Charge].copy(
        chargeType = PenaltyInterest,
        amount = penaltyInterestChargeAmount,
        dueDate = penaltyInterestChargeAmountDueDate,
        payments = List.empty
      )

      val fullPaymentForUkResidentMainReturnChargeDueDate: LocalDate =
        LocalDate.now().plusMonths(2)

      val fullPaymentForUkResidentReturnCharge = sample[Payment].copy(
        amount = ukResidentMainReturnChargeAmount,
        method = Some(DirectDebit),
        clearingDate = fullPaymentForUkResidentMainReturnChargeDueDate
      )

      val ukResidentReturnChargeFullPayment = sample[Charge].copy(
        chargeType = UkResidentReturn,
        amount = ukResidentMainReturnChargeAmount,
        dueDate = ukResidentMainReturnChargeDueDate,
        payments = List(fullPaymentForUkResidentReturnCharge)
      )

      val ukResidentReturnDeltaChargeNoPayment = sample[Charge].copy(
        chargeType = DeltaCharge,
        amount = ukResidentDeltaReturnChargeAmount,
        dueDate = ukResidentDeltaChargeDueDate,
        payments = List.empty
      )

      val chargesWithChargeRaiseAndFullPayment =
        List(ukResidentReturnChargeFullPayment, penaltyInterestCharge)

      val chargesWithDeltaChargeAndPartialPayment =
        List(ukResidentReturnChargeFullPayment, ukResidentReturnDeltaChargeNoPayment)

      val mainChargeAmountWithDelta = ukResidentMainReturnChargeAmount ++ ukResidentDeltaReturnChargeAmount

      val sentReturn = sample[ReturnSummary].copy(
        charges = chargesWithChargeRaiseAndFullPayment,
        lastUpdatedDate = None,
        mainReturnChargeAmount = ukResidentMainReturnChargeAmount,
        submissionDate = ukResidentReturnSentDate,
        isRecentlyAmended = false,
        expired = false
      )

      val sentAmendedReturn = sample[ReturnSummary].copy(
        charges = chargesWithDeltaChargeAndPartialPayment,
        lastUpdatedDate = Some(LocalDate.now().plusMonths(2)),
        mainReturnChargeAmount = mainChargeAmountWithDelta,
        submissionDate = ukResidentReturnSentDate,
        isRecentlyAmended = false,
        expired = false
      )

      def validatePaymentsSection(
        document: Document,
        viewingReturn: ViewingReturn
      ): Unit = {
        val paymentDetails = document
          .select(
            s"#returnPaymentDetails-${viewingReturn.returnSummary.submissionId} > tr > td"
          )
          .eachText()
          .asScala

        paymentDetails.headOption.getOrElse(sys.error("Error")) should startWith(
          messageFromMessageKey("viewReturn.chargeType.UkResidentReturn")
        )
        paymentDetails(1)                                     shouldBe govShortDisplayFormat(
          ukResidentMainReturnChargeDueDate
        )
        paymentDetails(2)                                     shouldBe formatAmountOfMoneyWithPoundSign(
          ukResidentMainReturnChargeAmount.inPounds()
        )
        paymentDetails(3)                                     shouldBe formatAmountOfMoneyWithPoundSign(
          ukResidentMainReturnChargeAmount
            .inPounds() - fullPaymentForUkResidentReturnCharge.amount.inPounds()
        )
        paymentDetails(4)                                     shouldBe messageFromMessageKey("viewReturn.charge.status.paid")

        paymentDetails(5) shouldBe s"${formatAmountOfMoneyWithPoundSign(
            fullPaymentForUkResidentReturnCharge.amount.inPounds()
          )} direct debit payment received on ${govShortDisplayFormat(fullPaymentForUkResidentMainReturnChargeDueDate)}"

        paymentDetails(6)    should startWith(messageFromMessageKey("viewReturn.chargeType.PenaltyInterest"))
        paymentDetails(7)  shouldBe govShortDisplayFormat(
          penaltyInterestChargeAmountDueDate
        )
        paymentDetails(8)  shouldBe formatAmountOfMoneyWithPoundSign(
          penaltyInterestChargeAmount.inPounds()
        )
        paymentDetails(9)  shouldBe formatAmountOfMoneyWithPoundSign(
          penaltyInterestChargeAmount.inPounds()
        )
        paymentDetails(10) shouldBe "Pay now"
      }

      def validatePaymentsSectionWithDelta(
        document: Document,
        viewingReturn: ViewingReturn
      ): Unit = {
        val paymentDetails = document
          .select(
            s"#returnPaymentDetails-${viewingReturn.returnSummary.submissionId} > tr > td"
          )
          .eachText()
          .asScala

        paymentDetails.headOption.getOrElse(sys.error("Error")) should startWith(
          messageFromMessageKey("viewReturn.chargeType.UkResidentReturn")
        )
        paymentDetails(1)                                     shouldBe govShortDisplayFormat(
          ukResidentMainReturnChargeDueDate
        )
        paymentDetails(2)                                     shouldBe formatAmountOfMoneyWithPoundSign(
          ukResidentMainReturnChargeAmount.inPounds()
        )
        paymentDetails(3)                                     shouldBe formatAmountOfMoneyWithPoundSign(
          ukResidentMainReturnChargeAmount
            .inPounds() - fullPaymentForUkResidentReturnCharge.amount.inPounds()
        )
        paymentDetails(4)                                     shouldBe messageFromMessageKey("viewReturn.charge.status.paid")

        paymentDetails(5) shouldBe s"${formatAmountOfMoneyWithPoundSign(
            fullPaymentForUkResidentReturnCharge.amount.inPounds()
          )} ${messageFromMessageKey("viewReturn.charge.paymentMethod.DirectDebit")} ${messageFromMessageKey("generic.on")} ${govShortDisplayFormat(fullPaymentForUkResidentMainReturnChargeDueDate)}"

        paymentDetails(6)    should startWith(messageFromMessageKey("viewReturn.chargeType.DeltaCharge"))
        paymentDetails(7)  shouldBe govShortDisplayFormat(
          ukResidentDeltaChargeDueDate
        )
        paymentDetails(8)  shouldBe formatAmountOfMoneyWithPoundSign(
          ukResidentDeltaReturnChargeAmount.inPounds()
        )
        paymentDetails(9)  shouldBe formatAmountOfMoneyWithPoundSign(
          ukResidentDeltaReturnChargeAmount.inPounds()
        )
        paymentDetails(10) shouldBe "Pay now"
      }

      behave like journeyStatusBehaviour(() => performAction())

      val address = sample[UkAddress].copy(
        line1 = "123 fake street",
        line2 = None,
        town = None,
        county = None,
        postcode = Postcode("abc123")
      )

      "display the page for a single disposal journey" in {

        forAll(acceptedUserTypeGen, completeSingleDisposalReturnGen) { (userType, c) =>
          val subscribedDetails = sample[SubscribedDetails].copy(
            name = setNameForUserType(userType)
          )

          val completeSingleDisposalReturn = c.copy(
            triageAnswers = c.triageAnswers.copy(individualUserType = None),
            propertyAddress = address,
            representeeAnswers = None,
            gainOrLossAfterReliefs = None,
            reliefDetails = sample[CompleteReliefDetailsAnswers].copy(
              privateResidentsRelief = AmountInPence(0L),
              lettingsRelief = AmountInPence(0L)
            )
          )

          val sampleViewingReturn = sample[ViewingReturn]
            .copy(
              completeReturn = completeSingleDisposalReturn,
              agentReferenceNumber = setAgentReferenceNumber(userType),
              subscribedDetails = subscribedDetails,
              returnType = ReturnType.FirstReturn
            )

          val viewingReturn =
            sampleViewingReturn.copy(returnSummary = sentReturn)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(viewingReturn),
                userType = Some(userType)
              )
            )
          }

          val result       = performAction()
          val document     = Jsoup.parse(contentAsString(result))
          val nameLabelKey = nameLabelUserKey(userType === UserType.Agent, subscribedDetails.isATrust)

          document
            .select("#account-name-table-question")
            .text()                                         shouldBe messageFromMessageKey(s"viewReturn$nameLabelKey.nameLabel")
          document
            .select("#account-name-table-answer")
            .text()                                         shouldBe subscribedDetails.name
            .fold(_.value, e => e.makeSingleName)
          document
            .select("#date-sent-table-question")
            .text()                                         shouldBe messageFromMessageKey("viewReturn.sentToHmrc")
          document.select("#date-sent-table-answer").text() shouldBe TimeUtils
            .govDisplayFormat(
              sentReturn.submissionDate
            )
          document
            .select("#property-address-table-question")
            .text()                                         shouldBe messageFromMessageKey("viewReturn.propertyAddress")
          document
            .select("#property-address-table-answer")
            .text()                                         shouldBe "123 fake street, abc123"
          document
            .select("#return-reference-table-question")
            .text()                                         shouldBe messageFromMessageKey("viewReturn.returnReference")
          document
            .select("#return-reference-table-answer")
            .text()                                         shouldBe sentReturn.submissionId

          document
            .select(
              ".govuk-box-highlight--status > h1"
            )
            .text() shouldBe messageFromMessageKey(
            "viewReturn.title"
          )

          val isAgent  = userType === UserType.Agent
          val isATrust = subscribedDetails.isATrust

          val time    = extractDueDate(viewingReturn)
          val userKey = deriveUserKey(isAgent, isATrust)

          document.select("#warning").text() shouldBe "! " + messageFromMessageKey(
            "generic.warning"
          ) + " " + messageFromMessageKey(s"viewReturn$userKey.warning", time)

          document
            .select("#heading-reference")
            .text() shouldBe viewingReturn.returnSummary.mainReturnChargeReference.getOrElse(
            viewingReturn.returnSummary.submissionId
          )

          document.select("#heading-tax-owed").text() shouldBe MoneyUtils
            .formatAmountOfMoneyWithPoundSign(
              viewingReturn.returnSummary.mainReturnChargeAmount.withFloorZero
                .inPounds()
            )

          document.select("#amend-link-1").attr("href") shouldBe routes.ViewReturnController.startAmendingReturn().url

          document.select("#amend-link-2").attr("href") shouldBe routes.ViewReturnController.startAmendingReturn().url

          validatePaymentsSection(document, viewingReturn)

          validateSingleDisposalCheckAllYourAnswersSections(
            document,
            completeSingleDisposalReturn,
            Some(userType),
            rebasingUtil.isUk(completeSingleDisposalReturn),
            rebasingUtil.isEligibleForRebase(completeSingleDisposalReturn),
            viewingReturn.subscribedDetails.isATrust,
            completeSingleDisposalReturn.triageAnswers.assetType,
            isFurtherOrAmendReturn = false,
            !completeSingleDisposalReturn.yearToDateLiabilityAnswers.fold(_.hasEstimatedDetails, _.hasEstimatedDetails),
            showAnnualExemptAmount = true
          )

        }

      }

      "display the page for an amendment with delta charge" in {

        forAll(acceptedUserTypeGen, completeSingleDisposalReturnGen) { (userType, c) =>
          val subscribedDetails = sample[SubscribedDetails].copy(
            name = setNameForUserType(userType)
          )

          val completeSingleDisposalReturn = c.copy(
            triageAnswers = c.triageAnswers.copy(individualUserType = None),
            propertyAddress = address,
            representeeAnswers = None,
            gainOrLossAfterReliefs = None,
            reliefDetails = sample[CompleteReliefDetailsAnswers].copy(
              privateResidentsRelief = AmountInPence(0L),
              lettingsRelief = AmountInPence(0L)
            )
          )

          val sampleViewingReturn = sample[ViewingReturn]
            .copy(
              completeReturn = completeSingleDisposalReturn,
              agentReferenceNumber = setAgentReferenceNumber(userType),
              subscribedDetails = subscribedDetails,
              returnType = ReturnType.AmendedReturn
            )

          val viewingReturn =
            sampleViewingReturn.copy(returnSummary = sentAmendedReturn)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(viewingReturn),
                userType = Some(userType)
              )
            )
          }

          val result       = performAction()
          val document     = Jsoup.parse(contentAsString(result))
          val nameLabelKey = nameLabelUserKey(userType === UserType.Agent, subscribedDetails.isATrust)

          document
            .select("#account-name-table-question")
            .text() shouldBe messageFromMessageKey(s"viewReturn$nameLabelKey.nameLabel")
          document
            .select("#account-name-table-answer")
            .text() shouldBe subscribedDetails.name
            .fold(_.value, e => e.makeSingleName)

          document
            .select("#date-sent-table-question")
            .text()                                         shouldBe messageFromMessageKey("viewReturn.sentToHmrc")
          document.select("#date-sent-table-answer").text() shouldBe TimeUtils
            .govDisplayFormat(
              sentAmendedReturn.submissionDate
            )
          document
            .select("#property-address-table-question")
            .text()                                         shouldBe messageFromMessageKey("viewReturn.propertyAddress")
          document
            .select("#property-address-table-answer")
            .text()                                         shouldBe "123 fake street, abc123"
          document
            .select("#return-reference-table-question")
            .text()                                         shouldBe messageFromMessageKey("viewReturn.returnReference")
          document
            .select("#return-reference-table-answer")
            .text()                                         shouldBe sentAmendedReturn.submissionId

          document
            .select(
              ".govuk-box-highlight--status > h1"
            )
            .text() shouldBe messageFromMessageKey(
            "viewReturn.title"
          )

          document
            .select(
              ".govuk-panel__body > p"
            )
            .text() should include(
            messageFromMessageKey(
              "viewReturn.amend.heading.taxOwed"
            )
          )

          val isAgent  = userType === UserType.Agent
          val isATrust = subscribedDetails.isATrust

          val time    = extractDueDate(viewingReturn)
          val userKey = deriveUserKey(isAgent, isATrust)

          document.select("#warning").text() shouldBe "! " + messageFromMessageKey(
            "generic.warning"
          ) + " " + messageFromMessageKey(s"viewReturn$userKey.warning", time)

          document
            .select("#heading-reference")
            .text() shouldBe viewingReturn.returnSummary.mainReturnChargeReference.getOrElse(
            viewingReturn.returnSummary.submissionId
          )

          document.select("#heading-tax-owed").text() shouldBe MoneyUtils
            .formatAmountOfMoneyWithPoundSign(
              viewingReturn.returnSummary.mainReturnChargeAmount.withFloorZero
                .inPounds()
            )

          document.select("#amend-link-1").attr("href") shouldBe routes.ViewReturnController.startAmendingReturn().url

          document.select("#amend-link-2").attr("href") shouldBe routes.ViewReturnController.startAmendingReturn().url

          validatePaymentsSectionWithDelta(document, viewingReturn)

          validateSingleDisposalCheckAllYourAnswersSections(
            document,
            completeSingleDisposalReturn,
            Some(userType),
            rebasingUtil.isUk(completeSingleDisposalReturn),
            rebasingUtil.isEligibleForRebase(completeSingleDisposalReturn),
            viewingReturn.subscribedDetails.isATrust,
            completeSingleDisposalReturn.triageAnswers.assetType,
            isFurtherOrAmendReturn = true,
            !completeSingleDisposalReturn.yearToDateLiabilityAnswers.fold(_.hasEstimatedDetails, _.hasEstimatedDetails),
            showAnnualExemptAmount =
              completeSingleDisposalReturn.exemptionsAndLossesDetails.annualExemptAmount.isPositive
          )

        }

      }

      "display the page for a multiple disposals journey" in {

        forAll(acceptedUserTypeGen, completeMultipleDisposalsReturnGen) { (userType, c) =>
          val subscribedDetails = sample[SubscribedDetails].copy(
            name = setNameForUserType(userType)
          )

          val completeMultipleDisposalsReturn = c.copy(
            triageAnswers = c.triageAnswers.copy(
              countryOfResidence = Country("NZ"),
              assetTypes = List(AssetType.Residential),
              individualUserType = None
            ),
            representeeAnswers = None
          )

          val sampleViewingReturn = sample[ViewingReturn]
            .copy(
              completeReturn = completeMultipleDisposalsReturn,
              agentReferenceNumber = setAgentReferenceNumber(userType),
              subscribedDetails = subscribedDetails,
              returnType = ReturnType.FirstReturn
            )

          val viewingReturn =
            sampleViewingReturn.copy(returnSummary = sentReturn)

          val sessionData = SessionData.empty.copy(
            journeyStatus = Some(viewingReturn),
            userType = Some(userType)
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionData)
          }

          val result   = performAction()
          val document = Jsoup.parse(contentAsString(result))

          val isAgent      = userType === UserType.Agent
          val isATrust     = subscribedDetails.isATrust
          val userKey      = deriveUserKey(isAgent, isATrust)
          val time         = extractDueDate(viewingReturn)
          val nameLabelKey = nameLabelUserKey(userType === UserType.Agent, subscribedDetails.isATrust)

          document
            .select("#account-name-table-question")
            .text() shouldBe messageFromMessageKey(s"viewReturn$nameLabelKey.nameLabel")
          document
            .select("#account-name-table-answer")
            .text() shouldBe subscribedDetails.name
            .fold(_.value, e => e.makeSingleName)

          document.select("#warning").text()                shouldBe "! " + messageFromMessageKey(
            "generic.warning"
          ) + " " + messageFromMessageKey(s"viewReturn$userKey.warning", time)
          document
            .select("#date-sent-table-question")
            .text()                                         shouldBe messageFromMessageKey("viewReturn.sentToHmrc")
          document.select("#date-sent-table-answer").text() shouldBe TimeUtils
            .govDisplayFormat(
              sentReturn.submissionDate
            )
          val address = generateAddressLineForMultipleDisposals(
            completeMultipleDisposalsReturn
          )
          document
            .select("#property-address-table-question")
            .text() shouldBe messageFromMessageKey("viewReturn.propertyAddress")
          document
            .select("#property-address-table-answer")
            .text() shouldBe address
          document
            .select("#return-reference-table-question")
            .text() shouldBe messageFromMessageKey("viewReturn.returnReference")
          document
            .select("#return-reference-table-answer")
            .text() shouldBe sentReturn.submissionId

          document
            .select(
              ".govuk-box-highlight--status > h1"
            )
            .text() shouldBe messageFromMessageKey(
            "viewReturn.title"
          )

          document
            .select("#heading-reference")
            .text() shouldBe viewingReturn.returnSummary.mainReturnChargeReference.getOrElse(
            viewingReturn.returnSummary.submissionId
          )

          document.select("#heading-tax-owed").text() shouldBe MoneyUtils
            .formatAmountOfMoneyWithPoundSign(
              viewingReturn.returnSummary.mainReturnChargeAmount.withFloorZero
                .inPounds()
            )

          document.select("#amend-link-1").attr("href") shouldBe routes.ViewReturnController.startAmendingReturn().url

          document.select("#amend-link-2").attr("href") shouldBe routes.ViewReturnController.startAmendingReturn().url

          validatePaymentsSection(document, viewingReturn)

          validateMultipleDisposalsCheckAllYourAnswersSections(
            document,
            completeMultipleDisposalsReturn,
            Some(userType),
            subscribedDetails.isATrust,
            isFurtherOrAmendReturn = false,
            !completeMultipleDisposalsReturn.yearToDateLiabilityAnswers.hasEstimatedDetails,
            showAnnualExemptAmount = true
          )
        }

      }

      "display the page for a multiple indirect disposals journey" in {

        forAll(acceptedUserTypeGen, completeMultipleIndirectDisposalReturnGen) { (userType, c) =>
          val subscribedDetails = sample[SubscribedDetails].copy(
            name = setNameForUserType(userType)
          )

          val completeMultipleIndirectDisposalsReturn: CompleteMultipleIndirectDisposalReturn = c.copy(
            triageAnswers = c.triageAnswers.copy(
              individualUserType = None
            ),
            exampleCompanyDetailsAnswers = sample[CompleteExampleCompanyDetailsAnswers].copy(
              address = sample[UkAddress]
            ),
            representeeAnswers = None
          )

          val sampleViewingReturn = sample[ViewingReturn]
            .copy(
              completeReturn = completeMultipleIndirectDisposalsReturn,
              agentReferenceNumber = setAgentReferenceNumber(userType),
              subscribedDetails = subscribedDetails,
              returnType = ReturnType.FirstReturn
            )

          val viewingReturn =
            sampleViewingReturn.copy(returnSummary = sentReturn)

          val sessionData = SessionData.empty.copy(
            journeyStatus = Some(viewingReturn),
            userType = Some(userType)
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionData)
          }

          val result       = performAction()
          val document     = Jsoup.parse(contentAsString(result))
          val nameLabelKey = nameLabelUserKey(userType === UserType.Agent, subscribedDetails.isATrust)

          document
            .select("#account-name-table-question")
            .text()                                         shouldBe messageFromMessageKey(s"viewReturn$nameLabelKey.nameLabel")
          document
            .select("#account-name-table-answer")
            .text()                                         shouldBe subscribedDetails.name
            .fold(_.value, e => e.makeSingleName)
          document
            .select("#date-sent-table-question")
            .text()                                         shouldBe "Return sent to HMRC"
          document.select("#date-sent-table-answer").text() shouldBe TimeUtils
            .govDisplayFormat(
              sentReturn.submissionDate
            )
          val address = generateAddressLineForMultipleIndirectDisposals(
            completeMultipleIndirectDisposalsReturn
          )
          document
            .select("#property-address-table-question")
            .text() shouldBe "Company address"
          document
            .select("#property-address-table-answer")
            .text() shouldBe address
          document
            .select("#return-reference-table-question")
            .text() shouldBe "Return reference number"
          document
            .select("#return-reference-table-answer")
            .text() shouldBe sentReturn.submissionId

          document
            .select(
              ".govuk-box-highlight--status > h1"
            )
            .text() shouldBe messageFromMessageKey(
            "viewReturn.title"
          )

          document
            .select("#heading-reference")
            .text() shouldBe viewingReturn.returnSummary.mainReturnChargeReference.getOrElse(
            viewingReturn.returnSummary.submissionId
          )

          document.select("#heading-tax-owed").text() shouldBe MoneyUtils
            .formatAmountOfMoneyWithPoundSign(
              viewingReturn.returnSummary.mainReturnChargeAmount.withFloorZero
                .inPounds()
            )

          document.select("#amend-link-1").attr("href") shouldBe routes.ViewReturnController.startAmendingReturn().url

          document.select("#amend-link-2").attr("href") shouldBe routes.ViewReturnController.startAmendingReturn().url

          validatePaymentsSection(document, viewingReturn)

          validateMultipleIndirectDisposalsCheckAllYourAnswersSections(
            document,
            completeMultipleIndirectDisposalsReturn,
            Some(userType),
            subscribedDetails.isATrust,
            isFurtherOrAmendReturn = false,
            !completeMultipleIndirectDisposalsReturn.yearToDateLiabilityAnswers.hasEstimatedDetails,
            showAnnualExemptAmount = true
          )
        }

      }

      "Don't show amend links if submission has expired" in {

        forAll(acceptedUserTypeGen, completeMultipleIndirectDisposalReturnGen) { (userType, c) =>
          val subscribedDetails = sample[SubscribedDetails].copy(
            name = setNameForUserType(userType)
          )

          val completeMultipleIndirectDisposalsReturn: CompleteMultipleIndirectDisposalReturn = c.copy(
            triageAnswers = c.triageAnswers.copy(
              individualUserType = None
            ),
            exampleCompanyDetailsAnswers = sample[CompleteExampleCompanyDetailsAnswers].copy(
              address = sample[UkAddress]
            ),
            representeeAnswers = None
          )

          val sampleViewingReturn = sample[ViewingReturn]
            .copy(
              completeReturn = completeMultipleIndirectDisposalsReturn,
              agentReferenceNumber = setAgentReferenceNumber(userType),
              subscribedDetails = subscribedDetails,
              returnType = ReturnType.FirstReturn
            )

          val viewingReturn = sampleViewingReturn.copy(returnSummary = sentReturn.copy(expired = true))

          val sessionData = SessionData.empty.copy(
            journeyStatus = Some(viewingReturn),
            userType = Some(userType)
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionData)
          }

          val result   = performAction()
          val document = Jsoup.parse(contentAsString(result))

          document.select("#amend-link-1").isEmpty shouldBe true
          document.select("#amend-link-2").isEmpty shouldBe true
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

      behave like journeyStatusBehaviour(() => performAction(""))

      "return a not found response" when {

        "no charge can be found for the given charge reference" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(journeyStatus = Some(viewingReturn))
            )
          }

          val result = performAction(s"${charge.chargeReference}123")
          status(result) shouldBe NOT_FOUND
        }

      }

      "return an error page" when {

        "a charge can be found but there is an error starting the payments journey" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(journeyStatus = Some(viewingReturn))
            )
            mockStartPaymentJourney(
              viewingReturn.subscribedDetails.cgtReference,
              Some(charge.chargeReference),
              charge.amount,
              Some(charge.dueDate),
              controllers.accounts.homepage.routes.HomePageController
                .homepage(),
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
            mockGetSession(
              SessionData.empty.copy(journeyStatus = Some(viewingReturn))
            )
            mockStartPaymentJourney(
              viewingReturn.subscribedDetails.cgtReference,
              Some(charge.chargeReference),
              charge.amount,
              Some(charge.dueDate),
              controllers.accounts.homepage.routes.HomePageController
                .homepage(),
              controllers.returns.routes.ViewReturnController.displayReturn()
            )(Right(paymentsJourney))
          }

          checkIsRedirect(
            performAction(charge.chargeReference),
            paymentsJourney.nextUrl
          )
        }

      }

    }

  }

  def journeyStatusBehaviour(performAction: () => Future[Result]): Unit = {

    behave like redirectToStartWhenInvalidJourney(
      performAction,
      {
        case _: ViewingReturn | _: StartingToAmendReturn => true
        case _                                           => false
      }
    )

    "show an error page" when {

      "there is a problem updating the session when converting from StartingToAmendReturn" in {
        val journey       = sample[StartingToAmendReturn]
        val viewingReturn =
          ViewingReturn(
            journey.subscribedDetails,
            journey.ggCredId,
            journey.agentReferenceNumber,
            journey.originalReturn.completeReturn,
            journey.originalReturn.returnType,
            journey.originalReturn.summary,
            journey.previousSentReturns
          )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData.empty.copy(journeyStatus = Some(journey)))
          mockStoreSession(SessionData.empty.copy(journeyStatus = Some(viewingReturn)))(Left(Error("")))
        }

        checkIsTechnicalErrorPage(performAction())
      }

    }

  }

  private def generateAddressLineForMultipleDisposals(
    completeMultipleDisposalsReturn: CompleteMultipleDisposalsReturn
  ): String =
    completeMultipleDisposalsReturn.examplePropertyDetailsAnswers.address.getAddressLines
      .mkString(", ")

  private def generateAddressLineForMultipleIndirectDisposals(
    completeMultipleIndirectDisposalsReturn: CompleteMultipleIndirectDisposalReturn
  ): String =
    completeMultipleIndirectDisposalsReturn.exampleCompanyDetailsAnswers.address.getAddressLines
      .mkString(", ")

  private def extractDueDate(
    viewingReturn: ViewingReturn
  ) =
    viewingReturn.returnSummary.charges
      .filter(c => c.chargeType === ChargeType.UkResidentReturn || c.chargeType === ChargeType.NonUkResidentReturn)
      .map(e => TimeUtils.govDisplayFormat(e.dueDate))
      .headOption
      .getOrElse(sys.error("Error"))
}
