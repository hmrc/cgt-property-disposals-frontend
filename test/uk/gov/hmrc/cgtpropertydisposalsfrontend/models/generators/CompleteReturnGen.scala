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

import org.scalacheck.Gen
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.CompleteReturn.{CompleteMultipleDisposalsReturn, CompleteMultipleIndirectDisposalReturn, CompleteSingleDisposalReturn, CompleteSingleIndirectDisposalReturn, CompleteSingleMixedUseDisposalReturn}

object CompleteReturnGen extends LowerPriorityCompleteReturnGen {

  given completeSingleDisposalReturnGen: Gen[CompleteSingleDisposalReturn] =
    for {
      triageAnswers                    <- TriageQuestionsGen.completeSingleDisposalTriageAnswersGen
      propertyAddress                  <- AddressGen.ukAddressGen
      disposalDetails                  <- disposalDetails
      acquisitionDetails               <- acquisitionDetails
      reliefDetails                    <- ReliefDetailsGen.completeReliefDetailsAnswersGen
      exemptionsAndLossesDetails       <- ExemptionsAndLossesAnswersGen.completeExemptionAndLossesAnswersGen
      chosenYearToDateLiabilityAnswers <-
        Gen.either(yearToDateLiabilityAnswers, YearToDateLiabilityAnswersGen.completeCalculatedYTDLiabilityAnswersGen)
      supportingDocumentAnswers        <- supportingDocumentAnswers
      initialGainOrLoss                <- Gen.option(MoneyGen.amountInPenceGen)
      representeeAnswers               <- Gen.option(RepresenteeAnswersGen.completeRepresenteeAnswersGen)
      gainOrLossAfterReliefs           <- Gen.option(MoneyGen.amountInPenceGen)
      hasAttachments                   <- Generators.booleanGen
    } yield CompleteSingleDisposalReturn(
      triageAnswers,
      propertyAddress,
      disposalDetails,
      acquisitionDetails,
      reliefDetails,
      exemptionsAndLossesDetails,
      chosenYearToDateLiabilityAnswers,
      supportingDocumentAnswers,
      initialGainOrLoss,
      representeeAnswers,
      gainOrLossAfterReliefs,
      hasAttachments
    )

}

trait LowerPriorityCompleteReturnGen extends Common {

  given completeMultipleDisposalsReturnGen: Gen[CompleteMultipleDisposalsReturn] = for {
    triageAnswers                 <- TriageQuestionsGen.completeMultipleDisposalsTriageAnswersGen
    examplePropertyDetailsAnswers <- ExamplePropertyDetailsAnswersGen.completeExamplePropertyDetailsAnswersGen
    exemptionAndLossesAnswers     <- exemptionAndLossesAnswers
    yearToDateLiabilityAnswers    <- yearToDateLiabilityAnswers
    supportingDocumentAnswers     <- supportingDocumentAnswers
    representeeAnswers            <- Gen.option(representeeAnswers)
    gainOrLossAfterReliefs        <- Gen.option(MoneyGen.amountInPenceGen)
    hasAttachments                <- Generators.booleanGen
  } yield CompleteMultipleDisposalsReturn(
    triageAnswers,
    examplePropertyDetailsAnswers,
    exemptionAndLossesAnswers,
    yearToDateLiabilityAnswers,
    supportingDocumentAnswers,
    representeeAnswers,
    gainOrLossAfterReliefs,
    hasAttachments
  )

  given completeSingleIndirectDisposalReturnGen: Gen[CompleteSingleIndirectDisposalReturn] =
    for {
      triageAnswers              <- TriageQuestionsGen.completeSingleDisposalTriageAnswersGen
      companyAddress             <- AddressGen.addressGen
      disposalDetails            <- disposalDetails
      acquisitionDetails         <- acquisitionDetails
      exemptionsAndLossesDetails <- exemptionAndLossesAnswers
      yearToDateLiabilityAnswers <- yearToDateLiabilityAnswers
      supportingDocumentAnswers  <- supportingDocumentAnswers
      representeeAnswers         <- Gen.option(representeeAnswers)
      gainOrLossAfterReliefs     <- Gen.option(MoneyGen.amountInPenceGen)
      hasAttachments             <- Generators.booleanGen
    } yield CompleteSingleIndirectDisposalReturn(
      triageAnswers,
      companyAddress,
      disposalDetails,
      acquisitionDetails,
      exemptionsAndLossesDetails,
      yearToDateLiabilityAnswers,
      supportingDocumentAnswers,
      representeeAnswers,
      gainOrLossAfterReliefs,
      hasAttachments
    )

  given completeMultipleIndirectDisposalReturnGen: Gen[CompleteMultipleIndirectDisposalReturn] = for {
    triageAnswers                <- TriageQuestionsGen.completeMultipleDisposalsTriageAnswersGen
    exampleCompanyDetailsAnswers <- ExampleCompanyDetailsAnswersGen.completeExampleCompanyDetailsAnswersGen
    exemptionsAndLossesDetails   <- exemptionAndLossesAnswers
    yearToDateLiabilityAnswers   <- yearToDateLiabilityAnswers
    supportingDocumentAnswers    <- supportingDocumentAnswers
    representeeAnswers           <- Gen.option(representeeAnswers)
    gainOrLossAfterReliefs       <- Gen.option(MoneyGen.amountInPenceGen)
    hasAttachments               <- Generators.booleanGen
  } yield CompleteMultipleIndirectDisposalReturn(
    triageAnswers,
    exampleCompanyDetailsAnswers,
    exemptionsAndLossesDetails,
    yearToDateLiabilityAnswers,
    supportingDocumentAnswers,
    representeeAnswers,
    gainOrLossAfterReliefs,
    hasAttachments
  )

  given completeSingleMixedUseDisposalReturnGen: Gen[CompleteSingleMixedUseDisposalReturn] =
    for {
      triageAnswers              <- TriageQuestionsGen.completeSingleDisposalTriageAnswersGen
      propertyDetailsAnswers     <- SingleMixedUseDetailsAnswersGen.given_Gen_CompleteMixedUsePropertyDetailsAnswers
      exemptionsAndLossesDetails <- exemptionAndLossesAnswers
      yearToDateLiabilityAnswers <- yearToDateLiabilityAnswers
      supportingDocumentAnswers  <- supportingDocumentAnswers
      representeeAnswers         <- Gen.option(representeeAnswers)
      gainOrLossAfterReliefs     <- Gen.option(MoneyGen.amountInPenceGen)
      hasAttachments             <- Generators.booleanGen
    } yield CompleteSingleMixedUseDisposalReturn(
      triageAnswers,
      propertyDetailsAnswers,
      exemptionsAndLossesDetails,
      yearToDateLiabilityAnswers,
      supportingDocumentAnswers,
      representeeAnswers,
      gainOrLossAfterReliefs,
      hasAttachments
    )

}
