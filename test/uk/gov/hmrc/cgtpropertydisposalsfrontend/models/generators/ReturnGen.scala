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
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.CompleteReturnWithSummary
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.PreviousReturnData
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.*

import java.time.LocalDate

object ReturnGen extends GenUtils {

  implicit val completeReturnGen: Gen[CompleteReturn] =
    Gen.oneOf(
      CompleteReturnGen.completeSingleDisposalReturnGen,
      CompleteReturnGen.completeSingleIndirectDisposalReturnGen,
      CompleteReturnGen.completeSingleMixedUseDisposalReturnGen,
      CompleteReturnGen.completeMultipleDisposalsReturnGen,
      CompleteReturnGen.completeMultipleIndirectDisposalReturnGen
    )

  given completeReturnArb: Arbitrary[CompleteReturn] = Arbitrary(completeReturnGen)

  implicit val returnSummaryGen: Gen[ReturnSummary] =
    for {
      submissionId              <- Generators.stringGen
      submissionDate            <- Arbitrary.arbitrary[LocalDate]
      completionDate            <- Arbitrary.arbitrary[LocalDate]
      lastUpdatedDate           <- Gen.option(Arbitrary.arbitrary[LocalDate])
      taxYear                   <- Generators.stringGen
      mainReturnChargeAmount    <- MoneyGen.amountInPenceGen
      mainReturnChargeReference <- Gen.option(Generators.stringGen)
      propertyAddress           <- AddressGen.addressGen
      charges                   <- Generators.listOfMax(3, MoneyGen.chargeGen)
      isRecentlyAmended         <- Generators.booleanGen
      expired                   <- Generators.booleanGen
    } yield ReturnSummary(
      submissionId,
      submissionDate,
      completionDate,
      lastUpdatedDate,
      taxYear,
      mainReturnChargeAmount,
      mainReturnChargeReference,
      propertyAddress,
      charges,
      isRecentlyAmended,
      expired
    )

  private val furtherReturnCalculationData = for {
    address                <- AddressGen.ukAddressGen
    gainOrLossAfterReliefs <- MoneyGen.amountInPenceGen
  } yield FurtherReturnCalculationData(address, gainOrLossAfterReliefs)

  implicit val previousReturnDataGen: Gen[PreviousReturnData] =
    for {
      summaries                                     <- Generators.listOfMax(3, returnSummaryGen)
      previousYearToDate                            <- Gen.option(MoneyGen.amountInPenceGen)
      previousReturnsImplyEligibilityForCalculation <- Gen.option(Generators.booleanGen)
      calculationData                               <- Gen.option(Generators.listOfMax(2, furtherReturnCalculationData))
    } yield PreviousReturnData(
      summaries,
      previousYearToDate,
      previousReturnsImplyEligibilityForCalculation,
      calculationData
    )

  implicit val returnTypeGen: Gen[ReturnType] =
    Gen.oneOf(ReturnType.FirstReturn, ReturnType.FurtherReturn, ReturnType.AmendedReturn)

  implicit val completeReturnWithSummaryGen: Gen[CompleteReturnWithSummary] = for {
    completeReturn <- completeReturnGen
    summary        <- returnSummaryGen
    returnType     <- returnTypeGen
  } yield CompleteReturnWithSummary(completeReturn, summary, returnType)

  implicit val amendReturnDataGen: Gen[AmendReturnData] = for {
    originalReturn                      <- completeReturnWithSummaryGen
    shouldDisplayGainOrLossAfterReliefs <- Generators.booleanGen
  } yield AmendReturnData(originalReturn, shouldDisplayGainOrLossAfterReliefs)

  given Arbitrary[AmendReturnData] = Arbitrary(amendReturnDataGen)
}
