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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators

import cats.syntax.order._
import org.scalacheck.Gen
import org.scalacheck.ScalacheckShapeless._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.AmountInPence
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.AmountInPence._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.CalculatedTaxDue.GainCalculatedTaxDue
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{CalculatedTaxDue, MandatoryEvidence, YearToDateLiabilityAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.YearToDateLiabilityAnswers.CalculatedYTDAnswers.{CompleteCalculatedYTDAnswers, IncompleteCalculatedYTDAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.YearToDateLiabilityAnswers.NonCalculatedYTDAnswers.{CompleteNonCalculatedYTDAnswers, IncompleteNonCalculatedYTDAnswers}

object YearToDateLiabilityAnswersGen extends HigherPriorityYearToDateLiabilityAnswersGen with GenUtils

trait HigherPriorityYearToDateLiabilityAnswersGen extends LowerPriorityYearToDateLiabilityAnswersGen { this: GenUtils =>
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
