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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.models

import java.time.{Instant, LocalDate, LocalDateTime, ZoneId}

import cats.syntax.order._
import org.scalacheck.ScalacheckShapeless._
import org.scalacheck.{Arbitrary, Gen}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.AmountInPence._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.RegistrationStatus.{IndividualMissingEmail, IndividualSupplyingInformation, RegistrationReady}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.SubscriptionStatus.SubscriptionReady
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, JustSubmittedReturn, StartingNewDraftReturn, Subscribed, ViewingReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address.{NonUkAddress, UkAddress}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.{Address, Country, Postcode}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.agents.UnsuccessfulVerifierAttempts
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.{AmountInPence, Charge, FinancialTransaction, Payment, PaymentsJourney}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.{ContactName, IndividualName, TrustName}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.SubscriptionResponse.SubscriptionSuccessful
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.bpr.UnsuccessfulNameMatchAttempts.NameMatchDetails.{IndividualNameMatchDetails, TrustNameMatchDetails}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.bpr.{BusinessPartnerRecord, BusinessPartnerRecordRequest, UnsuccessfulNameMatchAttempts}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.email.{Email, EmailSource}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.{RegistrationDetails, SubscribedDetails, SubscribedUpdateDetails, SubscriptionDetails}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.AcquisitionDetailsAnswers.{CompleteAcquisitionDetailsAnswers, IncompleteAcquisitionDetailsAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.CalculatedTaxDue.GainCalculatedTaxDue
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.DisposalDetailsAnswers.{CompleteDisposalDetailsAnswers, IncompleteDisposalDetailsAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ExemptionAndLossesAnswers.{CompleteExemptionAndLossesAnswers, IncompleteExemptionAndLossesAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.MultipleDisposalsTriageAnswers.CompleteMultipleDisposalsAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.{CompleteSingleDisposalTriageAnswers, IncompleteSingleDisposalTriageAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.OtherReliefsOption.OtherReliefs
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ReliefDetailsAnswers.{CompleteReliefDetailsAnswers, IncompleteReliefDetailsAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.YearToDateLiabilityAnswers.{CompleteYearToDateLiabilityAnswers, IncompleteYearToDateLiabilityAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{CalculateCgtTaxDueRequest, _}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.UpscanService.UpscanNotifyResponse
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.UpscanService.UpscanServiceResponse.{UpscanNotifyEvent, UpscanResponse}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.FinancialDataServiceImpl.FinancialDataResponse
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsServiceImpl.{ListReturnsResponse, ReturnSummaryWithoutPaymentInfo}

object Generators
    extends GenUtils
    with SessionDataGen
    with BusinessPartnerRecordGen
    with IdGen
    with NameGen
    with JourneyStatusGen
    with AddressGen
    with NameMatchGen
    with OnboardingDetailsGen
    with EmailGen
    with VerifierMatchGen
    with UserTypeGen
    with UpscanGen
    with TriageQuestionsGen
    with ReturnGen
    with DisposalMethodGen
    with DisposalDetailsGen
    with MoneyGen
    with AcquisitionDetailsGen
    with ReliefDetailsAnswersGen
    with TaxYearGen
    with ExemptionAndLossesAnswersGen
    with YearToDateLiabilityAnswersGen
    with FinancialDataGen {

  implicit val booleanGen: Gen[Boolean] = Gen.oneOf(true, false)

  implicit val stringGen: Gen[String] = stringArb.arbitrary

  implicit def listGen[A](g: Gen[A]): Gen[List[A]] = Gen.listOf(g)

  def sample[A](implicit gen: Gen[A]): A =
    gen.sample.getOrElse(sys.error(s"Could not generate instance with $gen"))

  implicit def arb[A](implicit g: Gen[A]): Arbitrary[A] = Arbitrary(g)

}

sealed trait GenUtils {

  def gen[A](implicit arb: Arbitrary[A]): Gen[A] = arb.arbitrary

  // define our own Arbitrary instance for String to generate more legible strings
  implicit val stringArb: Arbitrary[String] = Arbitrary(Gen.alphaNumStr)

  implicit val longArb: Arbitrary[Long] = Arbitrary(Gen.choose(-5e13.toLong, 5e13.toLong))

  implicit val bigDecimalGen: Arbitrary[BigDecimal] = Arbitrary(Gen.choose(0L, 1e9.toLong).map(BigDecimal(_)))

  implicit val localDateArb: Arbitrary[LocalDate] = Arbitrary(
    Gen.chooseNum(0, Int.MaxValue).map(LocalDate.ofEpochDay(_))
  )

  implicit val localDateTimeArb: Arbitrary[LocalDateTime] =
    Arbitrary(
      Gen
        .chooseNum(0L, 10000L)
        .map(l => LocalDateTime.ofInstant(Instant.ofEpochMilli(l), ZoneId.systemDefault()))
    )

}

trait SessionDataGen { this: GenUtils =>

  implicit val sessionDataGen: Gen[SessionData] = gen[SessionData]

}

trait BusinessPartnerRecordGen { this: GenUtils =>

  implicit val bprArb: Gen[BusinessPartnerRecord] = gen[BusinessPartnerRecord]

  implicit val bprRequestArb: Gen[BusinessPartnerRecordRequest] = gen[BusinessPartnerRecordRequest]

}

trait IdGen { this: GenUtils =>

  implicit val cgtReferenceArb: Gen[CgtReference] = gen[CgtReference]

  implicit val ggCredIdGen: Gen[GGCredId] = gen[GGCredId]

  implicit val sautrGen: Gen[SAUTR] = gen[SAUTR]

  implicit val sapNumberGen: Gen[SapNumber] = gen[SapNumber]

  implicit val arnGen: Gen[AgentReferenceNumber] = gen[AgentReferenceNumber]

}

trait NameGen { this: GenUtils =>

  implicit val contactNameGen: Gen[ContactName] = gen[ContactName]

  implicit val individualNameGen: Gen[IndividualName] = gen[IndividualName]

  implicit val trustNameGen: Gen[TrustName] = gen[TrustName]

}

trait JourneyStatusGen extends JourneyStatusLowerPriorityGen { this: GenUtils =>

  implicit val journeyStatusGen: Gen[JourneyStatus] = gen[JourneyStatus]

  implicit val subscriptionReadyGen: Gen[SubscriptionReady] = gen[SubscriptionReady]

}

trait JourneyStatusLowerPriorityGen { this: GenUtils =>

  implicit val subscriptionSuccessfulGen: Gen[SubscriptionSuccessful] = gen[SubscriptionSuccessful]

  implicit val individualSupplyingInformationGen: Gen[IndividualSupplyingInformation] =
    gen[IndividualSupplyingInformation]

  implicit val subscribedGen: Gen[Subscribed] = gen[Subscribed]

  implicit val individualMissingEmailGen: Gen[IndividualMissingEmail] = gen[IndividualMissingEmail]

  implicit val registrationReadyGen: Gen[RegistrationReady] = gen[RegistrationReady]

  implicit val startingNewDraftReturnGen: Gen[StartingNewDraftReturn] = gen[StartingNewDraftReturn]

  implicit val fillingOutReturnGen: Gen[FillingOutReturn] = gen[FillingOutReturn]

  implicit val justSubmittedReturnGen: Gen[JustSubmittedReturn] = gen[JustSubmittedReturn]

  implicit val viewingReturnGen: Gen[ViewingReturn] = gen[ViewingReturn]

}

trait AddressGen extends AddressLowerPriorityGen { this: GenUtils =>

  implicit val addressGen: Gen[Address] = gen[Address]

  implicit val nonUkAddressGen: Gen[NonUkAddress] = gen[NonUkAddress]

  implicit val postcodeGen: Gen[Postcode] = gen[Postcode]

  implicit val countryGen: Gen[Country] = {
    val countries = Country.countryCodeToCountryName.map { case (code, name) => Country(code, Some(name)) }.toList
    Gen.oneOf(countries)
  }

}

trait AddressLowerPriorityGen { this: GenUtils =>

  implicit val ukAddressGen: Gen[UkAddress] = gen[UkAddress]

}

trait NameMatchGen { this: GenUtils =>

  implicit val trustNameMatchDetailsGen: Gen[TrustNameMatchDetails] = gen[TrustNameMatchDetails]

  implicit val individualNameMatchDetailsGen: Gen[IndividualNameMatchDetails] = gen[IndividualNameMatchDetails]

  implicit val individualUnsuccessfulNameMatchAttemptsGen
    : Gen[UnsuccessfulNameMatchAttempts[IndividualNameMatchDetails]] =
    gen[UnsuccessfulNameMatchAttempts[IndividualNameMatchDetails]]

}

trait OnboardingDetailsGen { this: GenUtils =>

  implicit val registrationDetailsArb: Gen[RegistrationDetails] = gen[RegistrationDetails]

  implicit val subscriptionDetailsArb: Gen[SubscriptionDetails] = gen[SubscriptionDetails]

  implicit val subscribedDetailsGen: Gen[SubscribedDetails] = gen[SubscribedDetails]

  implicit val subscribedUpdateDetailsGen: Gen[SubscribedUpdateDetails] = gen[SubscribedUpdateDetails]

}

trait EmailGen { this: GenUtils =>

  implicit val emailGen: Gen[Email] = gen[Email]

  implicit val emailSourceGen: Gen[EmailSource] = gen[EmailSource]
}

trait UpscanGen { this: GenUtils =>

  implicit val upscanGen: Gen[UpscanNotifyEvent] = gen[UpscanNotifyEvent]

  implicit val upscanNotifyResponse: Gen[UpscanNotifyResponse] = gen[UpscanNotifyResponse]

  implicit val upscanResponse: Gen[UpscanResponse] = gen[UpscanResponse]

}

trait VerifierMatchGen { this: GenUtils =>

  implicit val unsuccessfulVerifierMatchAttemptsGen: Gen[UnsuccessfulVerifierAttempts] =
    gen[UnsuccessfulVerifierAttempts]

}

trait UserTypeGen { this: GenUtils =>

  implicit val userTypeGen: Gen[UserType] = gen[UserType]

}

trait TriageQuestionsGen { this: GenUtils =>

  implicit val individualTriageAnswersGen: Gen[SingleDisposalTriageAnswers] = gen[SingleDisposalTriageAnswers]

  implicit val incompleteSingleDisposalTriageAnswersGen: Gen[IncompleteSingleDisposalTriageAnswers] =
    gen[IncompleteSingleDisposalTriageAnswers]

  implicit val completeSingleDisposalTriageAnswersGen: Gen[CompleteSingleDisposalTriageAnswers] =
    gen[CompleteSingleDisposalTriageAnswers]

  implicit val completeMultipleDisposalsTriageAnswersGen: Gen[CompleteMultipleDisposalsAnswers] =
    gen[CompleteMultipleDisposalsAnswers]

  implicit val individualUserTypeGen: Gen[IndividualUserType] = gen[IndividualUserType]

  implicit val numberOfPropertiesGen: Gen[NumberOfProperties] = gen[NumberOfProperties]

  implicit val disposalDateGen: Gen[DisposalDate] = gen[DisposalDate]

  implicit val completionDateGen: Gen[CompletionDate] = gen[CompletionDate]

  implicit val assetTypeGen: Gen[AssetType] = gen[AssetType]

}

trait ReturnGen { this: GenUtils =>

  implicit val draftReturnGen: Gen[DraftReturn] = gen[DraftReturn]

  implicit val completeReturnGen: Gen[CompleteReturn] = gen[CompleteReturn]

  implicit val submitReturnRequestGen: Gen[SubmitReturnRequest] = gen[SubmitReturnRequest]

  implicit val submitReturnResponseGen: Gen[SubmitReturnResponse] = gen[SubmitReturnResponse]

  implicit val listReturnsResponseGen: Gen[ListReturnsResponse] = gen[ListReturnsResponse]

  implicit val returnSummaryGen: Gen[ReturnSummary] = gen[ReturnSummary]

  implicit val returnSummaryWithoutPaymentInfoGen: Gen[ReturnSummaryWithoutPaymentInfo] =
    gen[ReturnSummaryWithoutPaymentInfo]

  implicit val calculateCgtTaxDueRequestGen: Gen[CalculateCgtTaxDueRequest] = gen[CalculateCgtTaxDueRequest]

}

trait DisposalMethodGen { this: GenUtils =>

  implicit val disposalMethod: Gen[DisposalMethod] =
    gen[DisposalMethod]

}

trait DisposalDetailsGen { this: GenUtils =>

  implicit val completeDisposalDetailsAnswersGen: Gen[CompleteDisposalDetailsAnswers] =
    gen[CompleteDisposalDetailsAnswers]

  implicit val incompleteDisposalDetailsAnswersGen: Gen[IncompleteDisposalDetailsAnswers] =
    gen[IncompleteDisposalDetailsAnswers]

  implicit val shareOfPropertyGen: Gen[ShareOfProperty] =
    gen[ShareOfProperty].map {
      case a: ShareOfProperty.Other if a.percentageValue > 100 => ShareOfProperty.Full
      case other: ShareOfProperty                              => other
    }
}

trait AcquisitionDetailsGen { this: GenUtils =>

  implicit val completeAcquisitionDetailsAnswersGen: Gen[CompleteAcquisitionDetailsAnswers] =
    gen[CompleteAcquisitionDetailsAnswers]

  implicit val incompleteAcquisitionDetailsAnswersGen: Gen[IncompleteAcquisitionDetailsAnswers] =
    gen[IncompleteAcquisitionDetailsAnswers]

  implicit val acquisitionMethodGen: Gen[AcquisitionMethod] = gen[AcquisitionMethod]

  implicit val acquisitionDateGen: Gen[AcquisitionDate] = gen[AcquisitionDate]

}

trait ReliefDetailsGen { this: GenUtils =>

  implicit val completeReliefDetailsAnswersGen: Gen[CompleteReliefDetailsAnswers] =
    gen[CompleteReliefDetailsAnswers]

  implicit val incompleteReliefDetailsAnswersGen: Gen[IncompleteReliefDetailsAnswers] =
    gen[IncompleteReliefDetailsAnswers]

}

trait MoneyGen { this: GenUtils =>

  implicit val amountInPenceGen: Gen[AmountInPence] = gen[AmountInPence]

  implicit val chargeGen: Gen[Charge] = gen[Charge]

  implicit val paymentGen: Gen[Payment] = gen[Payment]

  implicit val paymentsJourneyGen: Gen[PaymentsJourney] = gen[PaymentsJourney]

}

trait TaxYearGen { this: GenUtils =>

  implicit val taxYearGen: Gen[TaxYear] = gen[TaxYear]

}

trait ReliefDetailsAnswersGen extends LowerPriorityReliefDetailsAnswersGen { this: GenUtils =>

  implicit val reliefDetailsAnswersGen: Gen[ReliefDetailsAnswers] =
    gen[ReliefDetailsAnswers]

  implicit val completeReliefDetailsAnswersGen: Gen[CompleteReliefDetailsAnswers] =
    gen[CompleteReliefDetailsAnswers]

  implicit val otherReliefsGen: Gen[OtherReliefs] = gen[OtherReliefs]
}

trait LowerPriorityReliefDetailsAnswersGen { this: GenUtils =>

  implicit val incompleteReliefDetailsAnswersGen: Gen[IncompleteReliefDetailsAnswers] =
    gen[IncompleteReliefDetailsAnswers]

}

trait ExemptionAndLossesAnswersGen { this: GenUtils =>

  implicit val completeExemptionAndLossesAnswersGen: Gen[CompleteExemptionAndLossesAnswers] =
    gen[CompleteExemptionAndLossesAnswers]

  implicit val incompleteExemptionAndLossesAnswersGen: Gen[IncompleteExemptionAndLossesAnswers] =
    gen[IncompleteExemptionAndLossesAnswers]
}

trait YearToDateLiabilityAnswersGen { this: GenUtils =>

  implicit val completeYTDLiabilityAnswersGen: Gen[CompleteYearToDateLiabilityAnswers] =
    gen[CompleteYearToDateLiabilityAnswers].map {
      case a: CompleteYearToDateLiabilityAnswers
          if a.estimatedIncome > AmountInPence.zero && a.personalAllowance.isEmpty =>
        a.copy(personalAllowance = Some(AmountInPence.zero))
      case other => other
    }

  implicit val incompleteYTDLiabilityAnswersGen: Gen[IncompleteYearToDateLiabilityAnswers] =
    gen[IncompleteYearToDateLiabilityAnswers].map {
      case a: IncompleteYearToDateLiabilityAnswers
          if a.estimatedIncome.exists(_ > AmountInPence.zero) && a.personalAllowance.isEmpty =>
        a.copy(personalAllowance = Some(AmountInPence.zero))
      case other => other
    }

  implicit val calculatedTaxDueGen: Gen[CalculatedTaxDue] = gen[CalculatedTaxDue]

  implicit val gainCalculatedTaxDueGen: Gen[GainCalculatedTaxDue] = gen[GainCalculatedTaxDue]

}

trait FinancialDataGen { this: GenUtils =>

  implicit val financialTransactionGen: Gen[FinancialTransaction] = gen[FinancialTransaction]

  implicit val financialDataResponseGen: Gen[FinancialDataResponse] = gen[FinancialDataResponse]

}
