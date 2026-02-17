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
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.ExamplePropertyDetailsAnswersGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.Generators.{arb, sample}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ExamplePropertyDetailsAnswers.{CompleteExamplePropertyDetailsAnswers, IncompleteExamplePropertyDetailsAnswers}

class ExamplePropertyDetailsAnswersSpec extends AnyWordSpec with Matchers with SampledScalaCheck {

  "IncompleteExamplePropertyDetailsAnswers" must {

    "have a method which converts incomplete answers to complete answers" in {
      forAll { (completeAnswers: CompleteExamplePropertyDetailsAnswers) =>
        IncompleteExamplePropertyDetailsAnswers.fromCompleteAnswers(
          completeAnswers
        ) shouldBe IncompleteExamplePropertyDetailsAnswers(
          Some(completeAnswers.address),
          Some(completeAnswers.disposalDate),
          Some(completeAnswers.disposalPrice),
          Some(completeAnswers.acquisitionPrice)
        )

      }

    }

    "ExamplePropertyDetailsAnswers" must {

      "have a method which unsets fields" when {

        val completeAnswers   = sample[CompleteExamplePropertyDetailsAnswers]
        val incompleteAnswers =
          IncompleteExamplePropertyDetailsAnswers(
            Some(completeAnswers.address),
            Some(completeAnswers.disposalDate),
            Some(completeAnswers.disposalPrice),
            Some(completeAnswers.acquisitionPrice)
          )

        "given incomplete answers" in {
          incompleteAnswers.unset(_.address)          shouldBe incompleteAnswers
            .copy(address = None)
          incompleteAnswers.unset(_.disposalDate)     shouldBe incompleteAnswers
            .copy(disposalDate = None)
          incompleteAnswers.unset(_.disposalPrice)    shouldBe incompleteAnswers
            .copy(disposalPrice = None)
          incompleteAnswers.unset(_.acquisitionPrice) shouldBe incompleteAnswers
            .copy(acquisitionPrice = None)

        }

        "given complete answers" in {
          completeAnswers.unset(_.address)          shouldBe incompleteAnswers
            .copy(address = None)
          completeAnswers.unset(_.disposalDate)     shouldBe incompleteAnswers
            .copy(disposalDate = None)
          completeAnswers.unset(_.disposalPrice)    shouldBe incompleteAnswers
            .copy(disposalPrice = None)
          completeAnswers.unset(_.acquisitionPrice) shouldBe incompleteAnswers
            .copy(acquisitionPrice = None)
        }

      }

    }

  }
}
