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

import cats.syntax.order._
import org.scalacheck.Gen
import org.scalacheck.ScalacheckShapeless._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.AmountInPence
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.AmountInPence._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.CalculatedTaxDue.GainCalculatedTaxDue
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.YearToDateLiabilityAnswers.CalculatedYTDAnswers.{CompleteCalculatedYTDAnswers, IncompleteCalculatedYTDAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.YearToDateLiabilityAnswers.NonCalculatedYTDAnswers.{CompleteNonCalculatedYTDAnswers, IncompleteNonCalculatedYTDAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{CalculatedTaxDue, MandatoryEvidence, YearToDateLiabilityAnswers}

object YearToDateLiabilityAnswersGen extends HigherPriorityYearToDateLiabilityAnswersGen with GenUtils

trait HigherPriorityYearToDateLiabilityAnswersGen extends LowerPriorityYearToDateLiabilityAnswersGen { this: GenUtils =>
  private val completeCalculatedYTDLiabilityAnswersRaw: Gen[CompleteCalculatedYTDAnswers] = for {
    estimatedIncome     <- MoneyGen.amountInPenceGen
    personalAllowance   <- Gen.option(MoneyGen.amountInPenceGen)
    hasEstimatedDetails <- Generators.booleanGen
    calculatedTaxDue    <- calculatedTaxDueGen
    taxDue              <- MoneyGen.amountInPenceGen
    mandatoryEvidence   <- Gen.option(mandatoryEvidenceGen)
  } yield CompleteCalculatedYTDAnswers(
    estimatedIncome,
    personalAllowance,
    hasEstimatedDetails,
    calculatedTaxDue,
    taxDue,
    mandatoryEvidence
  )

  implicit val completeCalculatedYTDLiabilityAnswersGen: Gen[CompleteCalculatedYTDAnswers] =
    completeCalculatedYTDLiabilityAnswersRaw.map {
      case a: CompleteCalculatedYTDAnswers if a.estimatedIncome > AmountInPence.zero && a.personalAllowance.isEmpty =>
        a.copy(personalAllowance = Some(AmountInPence.zero))
      case other                                                                                                    => other
    }

  implicit val calculatedTaxDue: Gen[CalculatedTaxDue] = calculatedTaxDueGen

  implicit val gainCalculatedTaxDueGen: Gen[GainCalculatedTaxDue] =
    gen[GainCalculatedTaxDue]

  implicit val ytdLiabilityAnswersGen: Gen[YearToDateLiabilityAnswers] =
    Gen.oneOf(
      completeCalculatedYTDLiabilityAnswersGen,
      incompleteCalculatedYTDLiabilityAnswersGen,
      incompleteNonCalculatedYTDLiabilityAnswersGen
    )

}

trait LowerPriorityYearToDateLiabilityAnswersGen extends EvenLowerPriorityYearToDateLiabilityAnswersGen {
  this: GenUtils =>

  val calculatedTaxDueGen: Gen[CalculatedTaxDue] =
    gen[CalculatedTaxDue]

  private val incompleteCalculatedYTDLiabilityAnswersRaw = for {
    estimatedIncome     <- Gen.option(MoneyGen.amountInPenceGen)
    personalAllowance   <- Gen.option(MoneyGen.amountInPenceGen)
    hasEstimatedDetails <- Gen.option(Generators.booleanGen)
    calculatedTaxDue    <- Gen.option(calculatedTaxDueGen)
    taxDue              <- Gen.option(MoneyGen.amountInPenceGen)
    mandatoryEvidence   <- Gen.option(mandatoryEvidenceGen)
    expiredEvidence     <- Gen.option(mandatoryEvidenceGen)
    pendingUpscanUpload <- Gen.option(FileUploadGen.upscanUploadGen)
  } yield IncompleteCalculatedYTDAnswers(
    estimatedIncome,
    personalAllowance,
    hasEstimatedDetails,
    calculatedTaxDue,
    taxDue,
    mandatoryEvidence,
    expiredEvidence,
    pendingUpscanUpload
  )

  implicit val incompleteCalculatedYTDLiabilityAnswersGen: Gen[IncompleteCalculatedYTDAnswers] =
    incompleteCalculatedYTDLiabilityAnswersRaw.map {
      case a: IncompleteCalculatedYTDAnswers
          if a.estimatedIncome.exists(
            _ > AmountInPence.zero
          ) && a.personalAllowance.isEmpty =>
        a.copy(personalAllowance = Some(AmountInPence.zero))
      case other => other
    }

  implicit val completeNonCalculatedYTDLiabilityAnswersGen: Gen[CompleteNonCalculatedYTDAnswers] = {
    for {
      taxableGainOrLoss              <- MoneyGen.amountInPenceGen
      hasEstimatedDetails            <- Generators.booleanGen
      taxDue                         <- MoneyGen.amountInPenceGen
      mandatoryEvidence              <- Gen.option(mandatoryEvidenceGen)
      yearToDateLiability            <- Gen.option(MoneyGen.amountInPenceGen)
      checkForRepayment              <- Gen.option(Generators.booleanGen)
      estimatedIncome                <- Gen.option(MoneyGen.amountInPenceGen)
      personalAllowance              <- Gen.option(MoneyGen.amountInPenceGen)
      taxableGainOrLossCalculation   <- Gen.option(FurtherReturnCalculationGen.taxableGainOrLossCalculationGen)
      yearToDateLiabilityCalculation <- Gen.option(FurtherReturnCalculationGen.yearToDateLiabilityCalculationGen)
    } yield CompleteNonCalculatedYTDAnswers(
      taxableGainOrLoss,
      hasEstimatedDetails,
      taxDue,
      mandatoryEvidence,
      yearToDateLiability,
      checkForRepayment,
      estimatedIncome,
      personalAllowance,
      taxableGainOrLossCalculation,
      yearToDateLiabilityCalculation
    )
  }

}

trait EvenLowerPriorityYearToDateLiabilityAnswersGen { this: GenUtils =>

  implicit val mandatoryEvidenceGen: Gen[MandatoryEvidence] =
    gen[MandatoryEvidence]

  implicit val incompleteNonCalculatedYTDLiabilityAnswersGen: Gen[IncompleteNonCalculatedYTDAnswers] = {
    for {
      taxableGainOrLoss              <- Gen.option(MoneyGen.amountInPenceGen)
      hasEstimatedDetails            <- Gen.option(Generators.booleanGen)
      taxDue                         <- Gen.option(MoneyGen.amountInPenceGen)
      mandatoryEvidence              <- Gen.option(mandatoryEvidenceGen)
      expiredEvidence                <- Gen.option(mandatoryEvidenceGen)
      pendingUpscanUpload            <- Gen.option(FileUploadGen.upscanUploadGen)
      yearToDateLiability            <- Gen.option(MoneyGen.amountInPenceGen)
      checkForRepayment              <- Gen.option(Generators.booleanGen)
      estimatedIncome                <- Gen.option(MoneyGen.amountInPenceGen)
      personalAllowance              <- Gen.option(MoneyGen.amountInPenceGen)
      taxableGainOrLossCalculation   <- Gen.option(FurtherReturnCalculationGen.taxableGainOrLossCalculationGen)
      yearToDateLiabilityCalculation <- Gen.option(FurtherReturnCalculationGen.yearToDateLiabilityCalculationGen)
    } yield IncompleteNonCalculatedYTDAnswers(
      taxableGainOrLoss,
      hasEstimatedDetails,
      taxDue,
      mandatoryEvidence,
      expiredEvidence,
      pendingUpscanUpload,
      yearToDateLiability,
      checkForRepayment,
      estimatedIncome,
      personalAllowance,
      taxableGainOrLossCalculation,
      yearToDateLiabilityCalculation
    )
  }

}
