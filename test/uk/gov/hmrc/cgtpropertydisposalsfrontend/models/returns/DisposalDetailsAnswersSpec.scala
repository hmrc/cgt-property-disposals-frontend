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
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.DisposalDetailsGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.Generators.{arb, sample}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.DisposalDetailsAnswers.{CompleteDisposalDetailsAnswers, IncompleteDisposalDetailsAnswers}

class DisposalDetailsAnswersSpec extends AnyWordSpec with Matchers with SampledScalaCheck {

  "IncompleteDisposalDetailsAnswers" must {

    "have a method which converts incomplete answers to complete answers" in {
      forAll { (completeAnswers: CompleteDisposalDetailsAnswers) =>
        IncompleteDisposalDetailsAnswers
          .fromCompleteAnswers(
            completeAnswers
          ) shouldBe IncompleteDisposalDetailsAnswers(
          Some(completeAnswers.shareOfProperty),
          Some(completeAnswers.disposalPrice),
          Some(completeAnswers.disposalFees)
        )

      }

    }

    "DisposalDetailsAnswers" must {

      "have a method which unsets fields" when {

        val completeAnswers   = sample[CompleteDisposalDetailsAnswers]
        val incompleteAnswers =
          IncompleteDisposalDetailsAnswers(
            Some(completeAnswers.shareOfProperty),
            Some(completeAnswers.disposalPrice),
            Some(completeAnswers.disposalFees)
          )

        "given incomplete answers" in {
          incompleteAnswers.unset(_.shareOfProperty) shouldBe incompleteAnswers
            .copy(shareOfProperty = None)
          incompleteAnswers.unset(_.disposalPrice)   shouldBe incompleteAnswers
            .copy(disposalPrice = None)
          incompleteAnswers.unset(_.disposalFees)    shouldBe incompleteAnswers
            .copy(disposalFees = None)
        }

        "given complete answers" in {
          completeAnswers.unset(_.shareOfProperty) shouldBe incompleteAnswers
            .copy(shareOfProperty = None)
          completeAnswers.unset(_.disposalPrice)   shouldBe incompleteAnswers
            .copy(disposalPrice = None)
          completeAnswers.unset(_.disposalFees)    shouldBe incompleteAnswers
            .copy(disposalFees = None)
        }

      }

    }

  }

}
