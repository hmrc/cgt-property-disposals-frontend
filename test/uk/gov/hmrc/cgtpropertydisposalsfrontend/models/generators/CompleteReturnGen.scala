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
import org.scalacheck.ScalacheckShapeless._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.AcquisitionDetailsAnswers.CompleteAcquisitionDetailsAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.CompleteReturn.{CompleteMultipleDisposalsReturn, CompleteMultipleIndirectDisposalReturn, CompleteSingleDisposalReturn, CompleteSingleIndirectDisposalReturn, CompleteSingleMixedUseDisposalReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.DisposalDetailsAnswers.CompleteDisposalDetailsAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ExampleCompanyDetailsAnswers.CompleteExampleCompanyDetailsAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ExamplePropertyDetailsAnswers.CompleteExamplePropertyDetailsAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ExemptionAndLossesAnswers.CompleteExemptionAndLossesAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.MixedUsePropertyDetailsAnswers.CompleteMixedUsePropertyDetailsAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.MultipleDisposalsTriageAnswers.CompleteMultipleDisposalsTriageAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.RepresenteeAnswers.CompleteRepresenteeAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.CompleteSingleDisposalTriageAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SupportingEvidenceAnswers.CompleteSupportingEvidenceAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.YearToDateLiabilityAnswers.NonCalculatedYTDAnswers.CompleteNonCalculatedYTDAnswers

object CompleteReturnGen extends LowerPriorityCompleteReturnGen with GenUtils {

  implicit val completeSingleDisposalReturnGen: Gen[CompleteSingleDisposalReturn] = gen[CompleteSingleDisposalReturn]

}

trait LowerPriorityCompleteReturnGen { this: GenUtils =>

  private val completeMultipleDisposalsTriageAnswers = gen[CompleteMultipleDisposalsTriageAnswers]
  private val completeSingleDisposalTriageAnswers    = gen[CompleteSingleDisposalTriageAnswers]
  private val examplePropertyDetailsAnswers          = gen[CompleteExamplePropertyDetailsAnswers]
  private val exemptionAndLossesAnswers              = gen[CompleteExemptionAndLossesAnswers]
  private val yearToDateLiabilityAnswers             = gen[CompleteNonCalculatedYTDAnswers]
  private val supportingDocumentAnswers              = gen[CompleteSupportingEvidenceAnswers]
  private val representeeAnswers                     = gen[CompleteRepresenteeAnswers]
  private val disposalDetails                        = gen[CompleteDisposalDetailsAnswers]
  private val acquisitionDetails                     = gen[CompleteAcquisitionDetailsAnswers]

  implicit val completeMultipleDisposalsReturnGen: Gen[CompleteMultipleDisposalsReturn] = for {
    triageAnswers                 <- completeMultipleDisposalsTriageAnswers
    examplePropertyDetailsAnswers <- examplePropertyDetailsAnswers
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

  implicit val completeSingleIndirectDisposalReturnGen: Gen[CompleteSingleIndirectDisposalReturn] = {
    for {
      triageAnswers              <- completeSingleDisposalTriageAnswers
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
  }

  implicit val completeMultipleIndirectDisposalReturnGen: Gen[CompleteMultipleIndirectDisposalReturn] = for {
    triageAnswers                <- completeMultipleDisposalsTriageAnswers
    exampleCompanyDetailsAnswers <- gen[CompleteExampleCompanyDetailsAnswers]
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

  implicit val completeSingleMixedUseDisposalReturnGen: Gen[CompleteSingleMixedUseDisposalReturn] = {
    for {
      triageAnswers              <- completeSingleDisposalTriageAnswers
      propertyDetailsAnswers     <- gen[CompleteMixedUsePropertyDetailsAnswers]
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

}
