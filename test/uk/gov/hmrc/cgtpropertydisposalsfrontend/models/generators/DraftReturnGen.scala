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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators

import org.scalacheck.ScalacheckShapeless._
import org.scalacheck.{Arbitrary, Gen}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns._

object DraftReturnGen extends HigherPriorityDraftReturnGen with GenUtils

trait HigherPriorityDraftReturnGen extends LowerPriorityDraftReturnGen {
  implicit val singleDisposalDraftReturnGen: Gen[DraftSingleDisposalReturn] = singleDisposalDraftReturnGen2

  implicit val draftReturnGen: Gen[DraftReturn] = Gen.oneOf(
    singleDisposalDraftReturnGen,
    singleIndirectDisposalDraftReturnGen,
    singleMixedUseDraftReturnGen,
    multipleDisposalDraftReturnGen,
    multipleIndirectDisposalDraftReturnGen
  )
}

trait LowerPriorityDraftReturnGen extends GenUtils {

  val triageAnswersGen: Gen[SingleDisposalTriageAnswers]             = gen[SingleDisposalTriageAnswers]
  private val supportingEvidenceGen: Gen[SupportingEvidenceAnswers]  = gen[SupportingEvidenceAnswers]
  private val exemptionsAndLossesAnswersGen                          = Gen.oneOf(
    ExemptionsAndLossesAnswersGen.completeExemptionAndLossesAnswersGen,
    ExemptionsAndLossesAnswersGen.incompleteExemptionAndLossesAnswersGen
  )
  private val disposalDetailsAnswersGen: Gen[DisposalDetailsAnswers] =
    Gen.oneOf(
      DisposalDetailsGen.completeDisposalDetailsAnswersGen,
      DisposalDetailsGen.incompleteDisposalDetailsAnswersGen
    )
  private val acquisitionDetailsAnswersGen                           = gen[AcquisitionDetailsAnswers]

  val singleDisposalDraftReturnGen2: Gen[DraftSingleDisposalReturn] =
    for {
      id                         <- Gen.uuid
      triageAnswers              <- triageAnswersGen
      propertyAddress            <- Gen.option(AddressGen.ukAddressGen)
      disposalDetailsAnswers     <- Gen.option(disposalDetailsAnswersGen)
      acquisitionDetailsAnswers  <- Gen.option(acquisitionDetailsAnswersGen)
      reliefDetailsAnswers       <- Gen.option(gen[ReliefDetailsAnswers])
      exemptionAndLossesAnswers  <- Gen.option(exemptionsAndLossesAnswersGen)
      yearToDateLiabilityAnswers <- Gen.option(YearToDateLiabilityAnswersGen.ytdLiabilityAnswersGen)
      initialGainOrLoss          <- Gen.option(MoneyGen.amountInPenceGen)
      supportingEvidenceAnswers  <- Gen.option(supportingEvidenceGen)
      representeeAnswers         <- Gen.option(RepresenteeAnswersGen.representeeAnswersGen)
      gainOrLossAfterReliefs     <- Gen.option(MoneyGen.amountInPenceGen)
      lastUpdatedDate            <- Arbitrary.arbitrary[LocalDate]
    } yield DraftSingleDisposalReturn(
      id,
      triageAnswers,
      propertyAddress,
      disposalDetailsAnswers,
      acquisitionDetailsAnswers,
      reliefDetailsAnswers,
      exemptionAndLossesAnswers,
      yearToDateLiabilityAnswers,
      initialGainOrLoss,
      supportingEvidenceAnswers,
      representeeAnswers,
      gainOrLossAfterReliefs,
      lastUpdatedDate
    )

  val multipleTriageGen = gen[MultipleDisposalsTriageAnswers]

  implicit val multipleDisposalDraftReturnGen: Gen[DraftMultipleDisposalsReturn] = {
    for {
      id                            <- Gen.uuid
      triageAnswers                 <- multipleTriageGen
      examplePropertyDetailsAnswers <- Gen.option(gen[ExamplePropertyDetailsAnswers])
      exemptionAndLossesAnswers     <- Gen.option(exemptionsAndLossesAnswersGen)
      yearToDateLiabilityAnswers    <- Gen.option(YearToDateLiabilityAnswersGen.ytdLiabilityAnswersGen)
      supportingEvidenceAnswers     <- Gen.option(supportingEvidenceGen)
      representeeAnswers            <- Gen.option(RepresenteeAnswersGen.representeeAnswersGen)
      gainOrLossAfterReliefs        <- Gen.option(MoneyGen.amountInPenceGen)
      lastUpdatedDate               <- Arbitrary.arbitrary[LocalDate]
    } yield DraftMultipleDisposalsReturn(
      id,
      triageAnswers,
      examplePropertyDetailsAnswers,
      exemptionAndLossesAnswers,
      yearToDateLiabilityAnswers,
      supportingEvidenceAnswers,
      representeeAnswers,
      gainOrLossAfterReliefs,
      lastUpdatedDate
    )
  }

  implicit val multipleIndirectDisposalDraftReturnGen: Gen[DraftMultipleIndirectDisposalsReturn] = {
    for {
      id                           <- Gen.uuid
      triageAnswers                <- multipleTriageGen
      exampleCompanyDetailsAnswers <- Gen.option(gen[ExampleCompanyDetailsAnswers])
      exemptionAndLossesAnswers    <- Gen.option(exemptionsAndLossesAnswersGen)
      yearToDateLiabilityAnswers   <- Gen.option(YearToDateLiabilityAnswersGen.ytdLiabilityAnswersGen)
      supportingEvidenceAnswers    <- Gen.option(supportingEvidenceGen)
      representeeAnswers           <- Gen.option(RepresenteeAnswersGen.representeeAnswersGen)
      gainOrLossAfterReliefs       <- Gen.option(MoneyGen.amountInPenceGen)
      lastUpdatedDate              <- Arbitrary.arbitrary[LocalDate]
    } yield DraftMultipleIndirectDisposalsReturn(
      id,
      triageAnswers,
      exampleCompanyDetailsAnswers,
      exemptionAndLossesAnswers,
      yearToDateLiabilityAnswers,
      supportingEvidenceAnswers,
      representeeAnswers,
      gainOrLossAfterReliefs,
      lastUpdatedDate
    )
  }

  implicit val singleIndirectDisposalDraftReturnGen: Gen[DraftSingleIndirectDisposalReturn] =
    for {
      id                         <- Gen.uuid
      triageAnswers              <- triageAnswersGen
      companyAddress             <- Gen.option(AddressGen.addressGen)
      disposalDetailsAnswers     <- Gen.option(disposalDetailsAnswersGen)
      acquisitionDetailsAnswers  <- Gen.option(acquisitionDetailsAnswersGen)
      exemptionAndLossesAnswers  <- Gen.option(exemptionsAndLossesAnswersGen)
      yearToDateLiabilityAnswers <- Gen.option(YearToDateLiabilityAnswersGen.ytdLiabilityAnswersGen)
      supportingEvidenceAnswers  <- Gen.option(supportingEvidenceGen)
      representeeAnswers         <- Gen.option(RepresenteeAnswersGen.representeeAnswersGen)
      gainOrLossAfterReliefs     <- Gen.option(MoneyGen.amountInPenceGen)
      lastUpdatedDate            <- Arbitrary.arbitrary[LocalDate]
    } yield DraftSingleIndirectDisposalReturn(
      id,
      triageAnswers,
      companyAddress,
      disposalDetailsAnswers,
      acquisitionDetailsAnswers,
      exemptionAndLossesAnswers,
      yearToDateLiabilityAnswers,
      supportingEvidenceAnswers,
      representeeAnswers,
      gainOrLossAfterReliefs,
      lastUpdatedDate
    )

  implicit val singleMixedUseDraftReturnGen: Gen[DraftSingleMixedUseDisposalReturn] =
    for {
      id                             <- Gen.uuid
      triageAnswers                  <- triageAnswersGen
      mixedUsePropertyDetailsAnswers <- Gen.option(gen[MixedUsePropertyDetailsAnswers])
      exemptionAndLossesAnswers      <- Gen.option(exemptionsAndLossesAnswersGen)
      yearToDateLiabilityAnswers     <- Gen.option(YearToDateLiabilityAnswersGen.ytdLiabilityAnswersGen)
      supportingEvidenceAnswers      <- Gen.option(supportingEvidenceGen)
      representeeAnswers             <- Gen.option(RepresenteeAnswersGen.representeeAnswersGen)
      gainOrLossAfterReliefs         <- Gen.option(MoneyGen.amountInPenceGen)
      lastUpdatedDate                <- Arbitrary.arbitrary[LocalDate]
    } yield DraftSingleMixedUseDisposalReturn(
      id,
      triageAnswers,
      mixedUsePropertyDetailsAnswers,
      exemptionAndLossesAnswers,
      yearToDateLiabilityAnswers,
      supportingEvidenceAnswers,
      representeeAnswers,
      gainOrLossAfterReliefs,
      lastUpdatedDate
    )

}
