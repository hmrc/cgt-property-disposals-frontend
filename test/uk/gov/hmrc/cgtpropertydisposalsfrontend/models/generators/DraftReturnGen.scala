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

import org.scalacheck.{Arbitrary, Gen}
import io.github.martinhh.derived.scalacheck.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.*
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.FileUploadGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.ReliefDetailsGen.given

import java.time.LocalDate

object DraftReturnGen extends HigherPriorityDraftReturnGen with GenUtils

trait HigherPriorityDraftReturnGen extends LowerPriorityDraftReturnGen {
  given singleDisposalDraftReturnGen: Gen[DraftSingleDisposalReturn] = singleDisposalDraftReturnGen2

  given draftReturnGen: Gen[DraftReturn] = Gen.oneOf(
    singleDisposalDraftReturnGen,
    singleIndirectDisposalDraftReturnGen,
    singleMixedUseDraftReturnGen,
    multipleDisposalDraftReturnGen,
    multipleIndirectDisposalDraftReturnGen
  )
}

trait LowerPriorityDraftReturnGen extends GenUtils {

  given supportingEvidenceGen: Gen[SupportingEvidenceAnswers] = gen[SupportingEvidenceAnswers]

  private val exemptionsAndLossesAnswersGen                          = Gen.oneOf(
    ExemptionsAndLossesAnswersGen.completeExemptionAndLossesAnswersGen,
    ExemptionsAndLossesAnswersGen.incompleteExemptionAndLossesAnswersGen
  )
  private val disposalDetailsAnswersGen: Gen[DisposalDetailsAnswers] =
    Gen.oneOf(
      DisposalDetailsGen.completeDisposalDetailsAnswersGen,
      DisposalDetailsGen.incompleteDisposalDetailsAnswersGen
    )

  given acquisitionDetailsAnswersGen: Gen[AcquisitionDetailsAnswers] = gen[AcquisitionDetailsAnswers]

  given singleDisposalDraftReturnGen2: Gen[DraftSingleDisposalReturn] =
    for {
      id                         <- Gen.uuid
      triageAnswers              <- TriageQuestionsGen.singleDisposalTraiageAnswersGen
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

  given multipleDisposalDraftReturnGen: Gen[DraftMultipleDisposalsReturn] =
    for {
      id                            <- Gen.uuid
      triageAnswers                 <- TriageQuestionsGen.multipleDisposalsTriageAnswersGen
      examplePropertyDetailsAnswers <- Gen.option(
                                         Gen.oneOf(
                                           ExamplePropertyDetailsAnswersGen.completeExamplePropertyDetailsAnswersGen,
                                           ExamplePropertyDetailsAnswersGen.incompleteExamplePropertyDetailsAnswersGen
                                         )
                                       )
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

  given multipleIndirectDisposalDraftReturnGen: Gen[DraftMultipleIndirectDisposalsReturn] =
    for {
      id                           <- Gen.uuid
      triageAnswers                <- TriageQuestionsGen.multipleDisposalsTriageAnswersGen
      exampleCompanyDetailsAnswers <- Gen.option(
                                        Gen.oneOf(
                                          ExampleCompanyDetailsAnswersGen.completeExampleCompanyDetailsAnswersGen,
                                          ExampleCompanyDetailsAnswersGen.incompleteExampleCompanyDetailsAnswersGen
                                        )
                                      )
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

  given singleIndirectDisposalDraftReturnGen: Gen[DraftSingleIndirectDisposalReturn] =
    for {
      id                         <- Gen.uuid
      triageAnswers              <- TriageQuestionsGen.singleDisposalTraiageAnswersGen
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

  given singleMixedUseDraftReturnGen: Gen[DraftSingleMixedUseDisposalReturn] =
    for {
      id                             <- Gen.uuid
      triageAnswers                  <- TriageQuestionsGen.singleDisposalTraiageAnswersGen
      mixedUsePropertyDetailsAnswers <- Gen.option(
                                          Gen.oneOf(
                                            SingleMixedUseDetailsAnswersGen.given_Gen_CompleteMixedUsePropertyDetailsAnswers,
                                            SingleMixedUseDetailsAnswersGen.given_Gen_IncompleteMixedUsePropertyDetailsAnswers
                                          )
                                        )
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
