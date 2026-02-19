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
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.ExemptionsAndLossesAnswersGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.Generators.{arb, sample}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ExemptionAndLossesAnswers.{CompleteExemptionAndLossesAnswers, IncompleteExemptionAndLossesAnswers}

class ExemptionAndLossesAnswersSpec extends AnyWordSpec with Matchers with SampledScalaCheck {

  "IncompleteExemptionAndLossesAnswers" must {

    "have a method which converts incomplete answers to complete answers" in {
      forAll { (completeAnswers: CompleteExemptionAndLossesAnswers) =>
        IncompleteExemptionAndLossesAnswers.fromCompleteAnswers(
          completeAnswers
        ) shouldBe IncompleteExemptionAndLossesAnswers(
          Some(completeAnswers.inYearLosses),
          Some(completeAnswers.previousYearsLosses),
          Some(completeAnswers.annualExemptAmount)
        )
      }

    }

    "ExemptionAndLossesAnswers" must {

      "have a method which unsets fields" when {

        val completeAnswers   = sample[CompleteExemptionAndLossesAnswers]
        val incompleteAnswers =
          IncompleteExemptionAndLossesAnswers(
            Some(completeAnswers.inYearLosses),
            Some(completeAnswers.previousYearsLosses),
            Some(completeAnswers.annualExemptAmount)
          )
        "given incomplete answers" in {
          incompleteAnswers.unset(_.inYearLosses) shouldBe incompleteAnswers
            .copy(inYearLosses = None)
          incompleteAnswers.unset(
            _.previousYearsLosses
          )                                       shouldBe incompleteAnswers.copy(previousYearsLosses = None)
          incompleteAnswers.unset(
            _.annualExemptAmount
          )                                       shouldBe incompleteAnswers.copy(annualExemptAmount = None)
        }

        "given complete answers" in {
          completeAnswers.unset(_.inYearLosses)       shouldBe incompleteAnswers
            .copy(inYearLosses = None)
          completeAnswers.unset(
            _.previousYearsLosses
          )                                           shouldBe incompleteAnswers.copy(previousYearsLosses = None)
          completeAnswers.unset(_.annualExemptAmount) shouldBe incompleteAnswers
            .copy(annualExemptAmount = None)
        }

      }

    }

  }
}
