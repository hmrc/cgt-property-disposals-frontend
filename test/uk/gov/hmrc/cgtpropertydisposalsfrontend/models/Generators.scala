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
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.RegistrationStatus.{IndividualMissingEmail, IndividualSupplyingInformation, RegistrationReady}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.SubscriptionStatus.SubscriptionReady
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, JustSubmittedReturn, StartingNewDraftReturn, SubmitReturnFailed, Subscribed, ViewingReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address.{NonUkAddress, UkAddress}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.{Address, Country, Postcode}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.agents.UnsuccessfulVerifierAttempts
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.AmountInPence._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.{ContactName, IndividualName, TrustName}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.SubscriptionResponse.SubscriptionSuccessful
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.UnsuccessfulNameMatchAttempts.NameMatchDetails.{IndividualRepresenteeNameMatchDetails, IndividualSautrNameMatchDetails, TrustNameMatchDetails}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.bpr.{BusinessPartnerRecord, BusinessPartnerRecordRequest}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.email.{Email, EmailSource}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.{RegistrationDetails, SubscribedDetails, SubscribedUpdateDetails, SubscriptionDetails}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.AcquisitionDetailsAnswers.{CompleteAcquisitionDetailsAnswers, IncompleteAcquisitionDetailsAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.CalculatedTaxDue.GainCalculatedTaxDue
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.CompleteReturn.{CompleteMultipleDisposalsReturn, CompleteMultipleIndirectDisposalReturn, CompleteSingleDisposalReturn, CompleteSingleIndirectDisposalReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.DisposalDetailsAnswers.{CompleteDisposalDetailsAnswers, IncompleteDisposalDetailsAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ExampleCompanyDetailsAnswers.{CompleteExampleCompanyDetailsAnswers, IncompleteExampleCompanyDetailsAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ExamplePropertyDetailsAnswers.{CompleteExamplePropertyDetailsAnswers, IncompleteExamplePropertyDetailsAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ExemptionAndLossesAnswers.{CompleteExemptionAndLossesAnswers, IncompleteExemptionAndLossesAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.MixedUsePropertyDetailsAnswers.{CompleteMixedUsePropertyDetailsAnswers, IncompleteMixedUsePropertyDetailsAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.MultipleDisposalsTriageAnswers.{CompleteMultipleDisposalsTriageAnswers, IncompleteMultipleDisposalsTriageAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.OtherReliefsOption.{NoOtherReliefs, OtherReliefs}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ReliefDetailsAnswers.{CompleteReliefDetailsAnswers, IncompleteReliefDetailsAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.RepresenteeAnswers.{CompleteRepresenteeAnswers, IncompleteRepresenteeAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.RepresenteeReferenceId.{RepresenteeCgtReference, RepresenteeNino, RepresenteeSautr}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.{CompleteSingleDisposalTriageAnswers, IncompleteSingleDisposalTriageAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SubmitReturnResponse.ReturnCharge
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SupportingEvidenceAnswers.{CompleteSupportingEvidenceAnswers, IncompleteSupportingEvidenceAnswers, SupportingEvidence}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.YearToDateLiabilityAnswers.CalculatedYTDAnswers._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.YearToDateLiabilityAnswers.NonCalculatedYTDAnswers.{CompleteNonCalculatedYTDAnswers, IncompleteNonCalculatedYTDAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{CalculateCgtTaxDueRequest, _}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.upscan.UpscanCallBack.{UpscanFailure, UpscanSuccess}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.upscan.{UploadReference, UploadRequest, UpscanUpload}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsServiceImpl.ListReturnsResponse

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
    with TriageQuestionsGen
    with ReturnGen
    with FileUploadGen
    with DisposalDetailsGen
    with DisposalMethodGen
    with MoneyGen
    with AcquisitionDetailsGen
    with ReliefDetailsAnswersGen
    with TaxYearGen
    with ExemptionAndLossesAnswersGen
    with YearToDateLiabilityAnswersGen
    with ExamplePropertyDetailsAnswersGen
    with ExampleCompanyDetailsAnswersGen
    with RepresenteeAnswersGen
    with SingleMixedUseDetailsAnswersGen {

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
  implicit val stringArb: Arbitrary[String] = Arbitrary(
    Gen.nonEmptyListOf(Gen.alphaUpperChar).map(_.mkString(""))
  )

  implicit val longArb: Arbitrary[Long] = Arbitrary(
    Gen.choose(-5e13.toLong, 5e13.toLong)
  )

  implicit val bigDecimalGen: Arbitrary[BigDecimal] = Arbitrary(
    Gen.choose(0L, 1e9.toLong).map(BigDecimal(_))
  )

  implicit val localDateArb: Arbitrary[LocalDate] = Arbitrary(
    Gen.chooseNum(0L, 10000L).map(LocalDate.ofEpochDay(_))
  )

  implicit val localDateTimeArb: Arbitrary[LocalDateTime] =
    Arbitrary(
      Gen
        .chooseNum(0L, 10000L)
        .map(l =>
          LocalDateTime
            .ofInstant(Instant.ofEpochMilli(l), ZoneId.systemDefault())
        )
    )

}

trait SessionDataGen { this: GenUtils =>

  implicit val sessionDataGen: Gen[SessionData] = gen[SessionData]

}

trait BusinessPartnerRecordGen { this: GenUtils =>

  implicit val bprArb: Gen[BusinessPartnerRecord] = gen[BusinessPartnerRecord]

  implicit val bprRequestArb: Gen[BusinessPartnerRecordRequest] =
    gen[BusinessPartnerRecordRequest]

}

trait IdGen { this: GenUtils =>

  implicit val cgtReferenceArb: Gen[CgtReference] = gen[CgtReference]

  implicit val ggCredIdGen: Gen[GGCredId] = gen[GGCredId]

  implicit val sautrGen: Gen[SAUTR] = gen[SAUTR]

  implicit val sapNumberGen: Gen[SapNumber] = gen[SapNumber]

  implicit val arnGen: Gen[AgentReferenceNumber] = gen[AgentReferenceNumber]

  implicit val draftReturnIdGen: Gen[DraftReturnId] = gen[DraftReturnId]

  implicit val uploadReferenceGen: Gen[UploadReference] = gen[UploadReference]

}

trait NameGen { this: GenUtils =>

  implicit val contactNameGen: Gen[ContactName] = gen[ContactName]

  implicit val individualNameGen: Gen[IndividualName] = gen[IndividualName]

  implicit val trustNameGen: Gen[TrustName] = gen[TrustName]

}

trait JourneyStatusGen extends JourneyStatusLowerPriorityGen { this: GenUtils =>

  implicit val journeyStatusGen: Gen[JourneyStatus] = gen[JourneyStatus]

  implicit val subscriptionReadyGen: Gen[SubscriptionReady] =
    gen[SubscriptionReady]

}

trait JourneyStatusLowerPriorityGen { this: GenUtils =>

  implicit val subscriptionSuccessfulGen: Gen[SubscriptionSuccessful] =
    gen[SubscriptionSuccessful]

  implicit val individualSupplyingInformationGen: Gen[IndividualSupplyingInformation] =
    gen[IndividualSupplyingInformation]

  implicit val subscribedGen: Gen[Subscribed] = gen[Subscribed]

  implicit val individualMissingEmailGen: Gen[IndividualMissingEmail] =
    gen[IndividualMissingEmail]

  implicit val registrationReadyGen: Gen[RegistrationReady] =
    gen[RegistrationReady]

  implicit val startingNewDraftReturnGen: Gen[StartingNewDraftReturn] =
    gen[StartingNewDraftReturn]

  implicit val fillingOutReturnGen: Gen[FillingOutReturn] =
    gen[FillingOutReturn]

  implicit val justSubmittedReturnGen: Gen[JustSubmittedReturn] =
    gen[JustSubmittedReturn]

  implicit val viewingReturnGen: Gen[ViewingReturn] = gen[ViewingReturn]

  implicit val submitReturnFailedGen: Gen[SubmitReturnFailed] =
    gen[SubmitReturnFailed]

}

trait AddressGen extends AddressLowerPriorityGen { this: GenUtils =>

  implicit val addressGen: Gen[Address] = gen[Address]

  implicit val nonUkAddressGen: Gen[NonUkAddress] = gen[NonUkAddress]

  implicit val postcodeGen: Gen[Postcode] = gen[Postcode]

  implicit val countryGen: Gen[Country] = {
    val countries = Country.countryCodeToCountryName.map {
      case (code, name) => Country(code, Some(name))
    }.toList
    Gen.oneOf(countries)
  }

}

trait AddressLowerPriorityGen { this: GenUtils =>

  implicit val ukAddressGen: Gen[UkAddress] = gen[UkAddress]

}

trait NameMatchGen { this: GenUtils =>

  implicit val trustNameMatchDetailsGen: Gen[TrustNameMatchDetails] =
    gen[TrustNameMatchDetails]

  implicit val individualSautrNameMatchDetailsGen: Gen[IndividualSautrNameMatchDetails] =
    gen[IndividualSautrNameMatchDetails]

  implicit val individualUnsuccessfulNameMatchAttemptsGen
    : Gen[UnsuccessfulNameMatchAttempts[IndividualSautrNameMatchDetails]] =
    gen[UnsuccessfulNameMatchAttempts[IndividualSautrNameMatchDetails]]

  implicit val individualRepresenteeNameMatchDetailsGen: Gen[IndividualRepresenteeNameMatchDetails] =
    gen[IndividualRepresenteeNameMatchDetails]

}

trait OnboardingDetailsGen { this: GenUtils =>

  implicit val registrationDetailsArb: Gen[RegistrationDetails] =
    gen[RegistrationDetails]

  implicit val subscriptionDetailsArb: Gen[SubscriptionDetails] =
    gen[SubscriptionDetails]

  implicit val subscribedDetailsGen: Gen[SubscribedDetails] =
    gen[SubscribedDetails]

  implicit val subscribedUpdateDetailsGen: Gen[SubscribedUpdateDetails] =
    gen[SubscribedUpdateDetails]

}

trait EmailGen { this: GenUtils =>

  implicit val emailGen: Gen[Email] = gen[Email]

  implicit val emailSourceGen: Gen[EmailSource] = gen[EmailSource]
}

trait VerifierMatchGen { this: GenUtils =>

  implicit val unsuccessfulVerifierMatchAttemptsGen: Gen[UnsuccessfulVerifierAttempts] =
    gen[UnsuccessfulVerifierAttempts]

}

trait UserTypeGen { this: GenUtils =>

  implicit val userTypeGen: Gen[UserType] = gen[UserType]

}

trait TriageQuestionsGen extends LowerPriorityTriageQuestionsGen {
  this: GenUtils =>

  implicit val individualTriageAnswersGen: Gen[SingleDisposalTriageAnswers] =
    gen[SingleDisposalTriageAnswers]

  implicit val completeSingleDisposalTriageAnswersGen: Gen[CompleteSingleDisposalTriageAnswers] =
    gen[CompleteSingleDisposalTriageAnswers]

  implicit val completeMultipleDisposalsTriageAnswersGen: Gen[CompleteMultipleDisposalsTriageAnswers] =
    gen[CompleteMultipleDisposalsTriageAnswers]

  implicit val incompleteMultipleDisposalsTriageAnswersGen: Gen[IncompleteMultipleDisposalsTriageAnswers] =
    gen[IncompleteMultipleDisposalsTriageAnswers]

  implicit val individualUserTypeGen: Gen[IndividualUserType] =
    gen[IndividualUserType]

  implicit val numberOfPropertiesGen: Gen[NumberOfProperties] =
    gen[NumberOfProperties]

  implicit val disposalDateGen: Gen[DisposalDate] = gen[DisposalDate]

  implicit val completionDateGen: Gen[CompletionDate] = gen[CompletionDate]

  implicit val assetTypeGen: Gen[AssetType] = gen[AssetType]

}

trait LowerPriorityTriageQuestionsGen { this: GenUtils =>

  implicit val incompleteSingleDisposalTriageAnswersGen: Gen[IncompleteSingleDisposalTriageAnswers] =
    gen[IncompleteSingleDisposalTriageAnswers]

}

trait ReturnGen extends LowerPriorityReturnGen { this: GenUtils =>

  implicit val draftReturnGen: Gen[DraftReturn] = gen[DraftReturn]

  implicit val singleDisposalDraftReturnGen: Gen[DraftSingleDisposalReturn] =
    gen[DraftSingleDisposalReturn]

  implicit val completeSingleDisposalReturnGen: Gen[CompleteSingleDisposalReturn] = gen[CompleteSingleDisposalReturn]

  implicit val submitReturnRequestGen: Gen[SubmitReturnRequest] =
    gen[SubmitReturnRequest]

  implicit val submitReturnResponseGen: Gen[SubmitReturnResponse] =
    gen[SubmitReturnResponse]

  implicit val listReturnsResponseGen: Gen[ListReturnsResponse] =
    gen[ListReturnsResponse]

  implicit val returnSummaryGen: Gen[ReturnSummary] = gen[ReturnSummary]

  implicit val calculateCgtTaxDueRequestGen: Gen[CalculateCgtTaxDueRequest] =
    gen[CalculateCgtTaxDueRequest]

}

trait LowerPriorityReturnGen { this: GenUtils =>

  implicit val multipleDisposalDraftReturnGen: Gen[DraftMultipleDisposalsReturn] = gen[DraftMultipleDisposalsReturn]

  implicit val multipleIndirectDisposalDraftReturnGen: Gen[DraftMultipleIndirectDisposalsReturn] =
    gen[DraftMultipleIndirectDisposalsReturn]

  implicit val completeMultipleDisposalsReturnGen: Gen[CompleteMultipleDisposalsReturn] =
    gen[CompleteMultipleDisposalsReturn]

  implicit val singleIndirectDisposalDraftReturnGen: Gen[DraftSingleIndirectDisposalReturn] =
    gen[DraftSingleIndirectDisposalReturn]

  implicit val completeSingleIndirectDisposalReturnGen: Gen[CompleteSingleIndirectDisposalReturn] =
    gen[CompleteSingleIndirectDisposalReturn]

  implicit val singleMixedUseDraftReturnGen: Gen[DraftSingleMixedUseDisposalReturn] =
    gen[DraftSingleMixedUseDisposalReturn]

  implicit val completeMultipleIndirectDisposalReturnGen: Gen[CompleteMultipleIndirectDisposalReturn] =
    gen[CompleteMultipleIndirectDisposalReturn]

}

trait FileUploadGen { this: GenUtils =>
  implicit val completeUploadSupportingEvidenceAnswersGen: Gen[CompleteSupportingEvidenceAnswers] =
    gen[CompleteSupportingEvidenceAnswers]

  implicit val incompleteUploadSupportingEvidenceAnswersGen: Gen[IncompleteSupportingEvidenceAnswers] =
    gen[IncompleteSupportingEvidenceAnswers]

  implicit val supportingEvidenceGen: Gen[SupportingEvidence] =
    gen[SupportingEvidence]

  implicit val uploadRequestGen: Gen[UploadRequest] = gen[UploadRequest]

  implicit val upscanUploadGen: Gen[UpscanUpload] = gen[UpscanUpload]

  implicit val upscanSuccessGen: Gen[UpscanSuccess] = gen[UpscanSuccess]

  implicit val upscanFailureGen: Gen[UpscanFailure] = gen[UpscanFailure]

}

trait DisposalMethodGen { this: GenUtils =>

  implicit val disposalMethodGen: Gen[DisposalMethod] =
    gen[DisposalMethod]

}

trait DisposalDetailsGen { this: GenUtils =>

  implicit val completeDisposalDetailsAnswersGen: Gen[CompleteDisposalDetailsAnswers] =
    gen[CompleteDisposalDetailsAnswers]

  implicit val incompleteDisposalDetailsAnswersGen: Gen[IncompleteDisposalDetailsAnswers] =
    gen[IncompleteDisposalDetailsAnswers]

  implicit val shareOfPropertyGen: Gen[ShareOfProperty] =
    gen[ShareOfProperty].map {
      case a: ShareOfProperty.Other if a.percentageValue > 100 =>
        ShareOfProperty.Full
      case other: ShareOfProperty                              => other
    }
}

trait AcquisitionDetailsGen { this: GenUtils =>

  implicit val completeAcquisitionDetailsAnswersGen: Gen[CompleteAcquisitionDetailsAnswers] =
    gen[CompleteAcquisitionDetailsAnswers]

  implicit val incompleteAcquisitionDetailsAnswersGen: Gen[IncompleteAcquisitionDetailsAnswers] =
    gen[IncompleteAcquisitionDetailsAnswers]

  implicit val acquisitionMethodGen: Gen[AcquisitionMethod] =
    gen[AcquisitionMethod]

  implicit val acquisitionDateGen: Gen[AcquisitionDate] = gen[AcquisitionDate]

}

trait ReliefDetailsGen { this: GenUtils =>

  implicit val completeReliefDetailsAnswersGen: Gen[CompleteReliefDetailsAnswers] =
    gen[CompleteReliefDetailsAnswers].map {
      case a: CompleteReliefDetailsAnswers if a.otherReliefs.isEmpty =>
        a.copy(otherReliefs = Some(NoOtherReliefs))
      case other                                                     => other
    }

  implicit val incompleteReliefDetailsAnswersGen: Gen[IncompleteReliefDetailsAnswers] =
    gen[IncompleteReliefDetailsAnswers].map {
      case a: IncompleteReliefDetailsAnswers if a.otherReliefs.isEmpty =>
        a.copy(otherReliefs = Some(NoOtherReliefs))
      case other                                                       => other
    }

}

trait MoneyGen { this: GenUtils =>

  implicit val amountInPenceGen: Gen[AmountInPence] = gen[AmountInPence]

  implicit val chargeGen: Gen[Charge] = gen[Charge]

  implicit val returnCharge: Gen[ReturnCharge] = gen[ReturnCharge]

  implicit val paymentGen: Gen[Payment] = gen[Payment]

  implicit val paymentsJourneyGen: Gen[PaymentsJourney] = gen[PaymentsJourney]

  implicit val amountInPenceWithSourceGen: Gen[AmountInPenceWithSource] =
    gen[AmountInPenceWithSource]

}

trait TaxYearGen { this: GenUtils =>

  implicit val taxYearGen: Gen[TaxYear] = gen[TaxYear]

}

trait ReliefDetailsAnswersGen extends LowerPriorityReliefDetailsAnswersGen {
  this: GenUtils =>

  override implicit val longArb: Arbitrary[Long] = Arbitrary(
    Gen.choose(0.toLong, 5e13.toLong)
  )

  implicit val reliefDetailsAnswersGen: Gen[ReliefDetailsAnswers] =
    gen[ReliefDetailsAnswers]

  implicit val completeReliefDetailsAnswersGen: Gen[CompleteReliefDetailsAnswers] =
    gen[CompleteReliefDetailsAnswers].map {
      case a: CompleteReliefDetailsAnswers if a.otherReliefs.isEmpty =>
        a.copy(otherReliefs = Some(NoOtherReliefs))
      case other                                                     => other
    }

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

trait YearToDateLiabilityAnswersGen extends LowerPriorityYearToDateLiabilityAnswersGen { this: GenUtils =>

  implicit val ytdLiabilityAnswersGen: Gen[YearToDateLiabilityAnswers] =
    gen[YearToDateLiabilityAnswers]

  implicit val completeCalculatedYTDLiabilityAnswersGen: Gen[CompleteCalculatedYTDAnswers] =
    gen[CompleteCalculatedYTDAnswers].map {
      case a: CompleteCalculatedYTDAnswers if a.estimatedIncome > AmountInPence.zero && a.personalAllowance.isEmpty =>
        a.copy(personalAllowance = Some(AmountInPence.zero))
      case other                                                                                                    => other
    }

  implicit val calculatedTaxDueGen: Gen[CalculatedTaxDue] =
    gen[CalculatedTaxDue]

  implicit val gainCalculatedTaxDueGen: Gen[GainCalculatedTaxDue] =
    gen[GainCalculatedTaxDue]

  implicit val mandatoryEvidenceGen: Gen[MandatoryEvidence] =
    gen[MandatoryEvidence]

}

trait LowerPriorityYearToDateLiabilityAnswersGen extends EvenLowerPriorityYearToDateLiabilityAnswersGen {
  this: GenUtils =>

  implicit val incompleteCalculatedYTDLiabilityAnswersGen: Gen[IncompleteCalculatedYTDAnswers] =
    gen[IncompleteCalculatedYTDAnswers].map {
      case a: IncompleteCalculatedYTDAnswers
          if a.estimatedIncome.exists(
            _ > AmountInPence.zero
          ) && a.personalAllowance.isEmpty =>
        a.copy(personalAllowance = Some(AmountInPence.zero))
      case other => other
    }

  implicit val completeNonCalculatedYTDLiabilityAnswersGen: Gen[CompleteNonCalculatedYTDAnswers] =
    gen[CompleteNonCalculatedYTDAnswers]

}

trait EvenLowerPriorityYearToDateLiabilityAnswersGen { this: GenUtils =>

  implicit val incompleteNonCalculatedYTDLiabilityAnswersGen: Gen[IncompleteNonCalculatedYTDAnswers] =
    gen[IncompleteNonCalculatedYTDAnswers]

}

trait ExamplePropertyDetailsAnswersGen { this: GenUtils =>

  implicit val incompleteExamplePropertyDetailsAnswersGen: Gen[IncompleteExamplePropertyDetailsAnswers] =
    gen[IncompleteExamplePropertyDetailsAnswers]

  implicit val completeExamplePropertyDetailsAnswersGen: Gen[CompleteExamplePropertyDetailsAnswers] =
    gen[CompleteExamplePropertyDetailsAnswers]

}

trait ExampleCompanyDetailsAnswersGen { this: GenUtils =>

  implicit val incompleteExampleCompanyDetailsAnswersGen: Gen[IncompleteExampleCompanyDetailsAnswers] =
    gen[IncompleteExampleCompanyDetailsAnswers]

  implicit val completeExampleCompanyDetailsAnswersGen: Gen[CompleteExampleCompanyDetailsAnswers] =
    gen[CompleteExampleCompanyDetailsAnswers]

}

trait SingleMixedUseDetailsAnswersGen { this: GenUtils =>

  implicit val incompleteMixedUsePropertyDetailsAnswers: Gen[IncompleteMixedUsePropertyDetailsAnswers] =
    gen[IncompleteMixedUsePropertyDetailsAnswers]

  implicit val completeMixedUsePropertyDetailsAnswers: Gen[CompleteMixedUsePropertyDetailsAnswers] =
    gen[CompleteMixedUsePropertyDetailsAnswers]
}

trait RepresenteeAnswersGen extends LowerPriorityRepresenteeAnswersGen {
  this: GenUtils =>

  implicit val representeeAnswersGen: Gen[RepresenteeAnswers] =
    gen[RepresenteeAnswers]

  implicit val incompleteRepresenteeAnswersGen: Gen[IncompleteRepresenteeAnswers] = gen[IncompleteRepresenteeAnswers]

  implicit val representeeReferenceIdGen: Gen[RepresenteeReferenceId] =
    gen[RepresenteeReferenceId]

  implicit val representeeCgtReferenceGen: Gen[RepresenteeCgtReference] =
    gen[RepresenteeCgtReference]

  implicit val representeeContactDetailsGen: Gen[RepresenteeContactDetails] =
    gen[RepresenteeContactDetails]
}

trait LowerPriorityRepresenteeAnswersGen { this: GenUtils =>

  implicit val completeRepresenteeAnswersGen: Gen[CompleteRepresenteeAnswers] =
    gen[CompleteRepresenteeAnswers]

  implicit val representeeSautrGen: Gen[RepresenteeSautr] =
    gen[RepresenteeSautr]

  implicit val representeeNinoGen: Gen[RepresenteeNino] = gen[RepresenteeNino]

  implicit val dateOfDeathGen: Gen[DateOfDeath] = gen[DateOfDeath]
}
