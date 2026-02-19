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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import uk.gov.hmrc.cgtpropertydisposalsfrontend.SampledScalaCheck
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.AmountInPence
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.Generators.{arb, sample}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.MoneyGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.YearToDateLiabilityAnswersGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.YearToDateLiabilityAnswers.CalculatedYTDAnswers.{CompleteCalculatedYTDAnswers, IncompleteCalculatedYTDAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.YearToDateLiabilityAnswers.NonCalculatedYTDAnswers.{CompleteNonCalculatedYTDAnswers, IncompleteNonCalculatedYTDAnswers}

class YearToDateLiabilityAnswersSpec extends AnyWordSpec with Matchers with SampledScalaCheck {

  "IncompleteNonCalculatedYTDAnswers" must {

    "have a method which converts incomplete answers to complete answers" in {
      forAll { (completeAnswers: CompleteNonCalculatedYTDAnswers) =>
        IncompleteNonCalculatedYTDAnswers.fromCompleteAnswers(
          completeAnswers
        ) shouldBe IncompleteNonCalculatedYTDAnswers(
          Some(completeAnswers.taxableGainOrLoss),
          Some(completeAnswers.hasEstimatedDetails),
          Some(completeAnswers.taxDue),
          completeAnswers.mandatoryEvidence,
          None,
          None,
          completeAnswers.yearToDateLiability,
          completeAnswers.checkForRepayment,
          completeAnswers.estimatedIncome,
          completeAnswers.personalAllowance,
          completeAnswers.taxableGainOrLossCalculation,
          completeAnswers.yearToDateLiabilityCalculation
        )
      }

    }
  }

  "NonCalculatedYTDAnswers" must {

    val completeAnswers   = sample[CompleteNonCalculatedYTDAnswers]
    val incompleteAnswers =
      IncompleteNonCalculatedYTDAnswers(
        Some(completeAnswers.taxableGainOrLoss),
        Some(completeAnswers.hasEstimatedDetails),
        Some(completeAnswers.taxDue),
        completeAnswers.mandatoryEvidence,
        None,
        None,
        completeAnswers.yearToDateLiability,
        completeAnswers.checkForRepayment,
        completeAnswers.estimatedIncome,
        completeAnswers.personalAllowance,
        completeAnswers.taxableGainOrLossCalculation,
        completeAnswers.yearToDateLiabilityCalculation
      )

    "have a method which unsets fields" when {

      "given incomplete answers" in {
        incompleteAnswers.unset(_.taxableGainOrLoss)   shouldBe incompleteAnswers.copy(taxableGainOrLoss = None)
        incompleteAnswers.unset(_.hasEstimatedDetails) shouldBe incompleteAnswers.copy(hasEstimatedDetails = None)
        incompleteAnswers.unset(_.taxDue)              shouldBe incompleteAnswers.copy(taxDue = None)
        incompleteAnswers.unset(_.mandatoryEvidence)   shouldBe incompleteAnswers.copy(mandatoryEvidence = None)
        incompleteAnswers.unset(_.yearToDateLiability) shouldBe incompleteAnswers.copy(yearToDateLiability = None)
      }

      "given complete answers" in {
        completeAnswers.unset(_.taxableGainOrLoss)   shouldBe incompleteAnswers.copy(taxableGainOrLoss = None)
        completeAnswers.unset(_.hasEstimatedDetails) shouldBe incompleteAnswers.copy(hasEstimatedDetails = None)
        completeAnswers.unset(_.taxDue)              shouldBe incompleteAnswers.copy(taxDue = None)
        completeAnswers.unset(_.mandatoryEvidence)   shouldBe incompleteAnswers.copy(mandatoryEvidence = None)
        completeAnswers.unset(_.yearToDateLiability) shouldBe incompleteAnswers.copy(yearToDateLiability = None)

      }

    }

    "has a method which unset everything except for income details" in {
      completeAnswers.unsetAllButIncomeDetails()   shouldBe None
      incompleteAnswers.unsetAllButIncomeDetails() shouldBe None
    }

  }

  "IncompleteCalculatedYTDAnswers" must {

    "have a method which converts incomplete answers to complete answers" in {
      forAll { (completeAnswers: CompleteCalculatedYTDAnswers) =>
        IncompleteCalculatedYTDAnswers.fromCompleteAnswers(
          completeAnswers
        ) shouldBe IncompleteCalculatedYTDAnswers(
          Some(completeAnswers.estimatedIncome),
          completeAnswers.personalAllowance,
          Some(completeAnswers.hasEstimatedDetails),
          Some(completeAnswers.calculatedTaxDue),
          Some(completeAnswers.taxDue),
          completeAnswers.mandatoryEvidence,
          None,
          None
        )
      }

    }
  }

  "CalculatedYTDAnswers" must {

    val personalAllowance = sample[AmountInPence]
    val mandatoryEvidence = sample[MandatoryEvidence]

    val completeAnswers = sample[CompleteCalculatedYTDAnswers].copy(
      personalAllowance = Some(personalAllowance),
      mandatoryEvidence = Some(mandatoryEvidence)
    )

    val incompleteAnswers =
      IncompleteCalculatedYTDAnswers(
        Some(completeAnswers.estimatedIncome),
        Some(personalAllowance),
        Some(completeAnswers.hasEstimatedDetails),
        Some(completeAnswers.calculatedTaxDue),
        Some(completeAnswers.taxDue),
        Some(mandatoryEvidence),
        None,
        None
      )

    "have a method which unsets fields" when {

      "given incomplete answers" in {
        incompleteAnswers.unset(_.estimatedIncome)     shouldBe incompleteAnswers.copy(estimatedIncome = None)
        incompleteAnswers.unset(_.personalAllowance)   shouldBe incompleteAnswers.copy(personalAllowance = None)
        incompleteAnswers.unset(_.hasEstimatedDetails) shouldBe incompleteAnswers.copy(hasEstimatedDetails = None)
        incompleteAnswers.unset(_.calculatedTaxDue)    shouldBe incompleteAnswers.copy(calculatedTaxDue = None)
        incompleteAnswers.unset(_.taxDue)              shouldBe incompleteAnswers.copy(taxDue = None)
        incompleteAnswers.unset(_.mandatoryEvidence)   shouldBe incompleteAnswers.copy(mandatoryEvidence = None)
      }

      "given complete answers" in {
        completeAnswers.unset(_.estimatedIncome)     shouldBe incompleteAnswers.copy(estimatedIncome = None)
        completeAnswers.unset(_.personalAllowance)   shouldBe incompleteAnswers.copy(personalAllowance = None)
        completeAnswers.unset(_.hasEstimatedDetails) shouldBe incompleteAnswers.copy(hasEstimatedDetails = None)
        completeAnswers.unset(_.calculatedTaxDue)    shouldBe incompleteAnswers.copy(calculatedTaxDue = None)
        completeAnswers.unset(_.taxDue)              shouldBe incompleteAnswers.copy(taxDue = None)
        completeAnswers.unset(_.mandatoryEvidence)   shouldBe incompleteAnswers.copy(mandatoryEvidence = None)
      }

    }

    "has a method which unset everything except for income details" in {
      val expectedResult =
        IncompleteCalculatedYTDAnswers(
          Some(completeAnswers.estimatedIncome),
          Some(personalAllowance),
          None,
          None,
          None,
          None,
          None,
          None
        )

      completeAnswers.unsetAllButIncomeDetails()   shouldBe Some(expectedResult)
      incompleteAnswers.unsetAllButIncomeDetails() shouldBe Some(expectedResult)
    }

  }

}
