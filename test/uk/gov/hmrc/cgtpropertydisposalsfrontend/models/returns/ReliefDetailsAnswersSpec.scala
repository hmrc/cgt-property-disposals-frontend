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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns

import org.scalatest.{Matchers, WordSpec}
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ReliefDetailsAnswers.{CompleteReliefDetailsAnswers, IncompleteReliefDetailsAnswers}

class ReliefDetailsAnswersSpec extends WordSpec with Matchers with ScalaCheckDrivenPropertyChecks {

  "IncompleteReliefDetailsAnswers" must {

    "have a method which converts incomplete answers to complete answers" in {
      forAll { completeAnswers: CompleteReliefDetailsAnswers =>
        IncompleteReliefDetailsAnswers.fromCompleteAnswers(
          completeAnswers
        ) shouldBe IncompleteReliefDetailsAnswers(
          Some(completeAnswers.privateResidentsRelief),
          Some(completeAnswers.lettingsRelief),
          completeAnswers.otherReliefs
        )

      }

    }

    "ReliefDetailsAnswers" must {

      val otherReliefs      = sample[OtherReliefsOption.OtherReliefs]
      val completeAnswers   = sample[CompleteReliefDetailsAnswers]
        .copy(otherReliefs = Some(otherReliefs))
      val incompleteAnswers =
        IncompleteReliefDetailsAnswers(
          Some(completeAnswers.privateResidentsRelief),
          Some(completeAnswers.lettingsRelief),
          Some(otherReliefs)
        )

      "have a method which unsets fields" when {

        "given incomplete answers" in {
          incompleteAnswers.unset(
            _.privateResidentsRelief
          )                                         shouldBe incompleteAnswers.copy(privateResidentsRelief = None)
          incompleteAnswers.unset(_.lettingsRelief) shouldBe incompleteAnswers
            .copy(lettingsRelief = None)
          incompleteAnswers.unset(_.otherReliefs)   shouldBe incompleteAnswers
            .copy(otherReliefs = None)
        }

        "given complete answers" in {
          completeAnswers.unset(
            _.privateResidentsRelief
          )                                       shouldBe incompleteAnswers.copy(privateResidentsRelief = None)
          completeAnswers.unset(_.lettingsRelief) shouldBe incompleteAnswers
            .copy(lettingsRelief = None)
          completeAnswers.unset(_.otherReliefs)   shouldBe incompleteAnswers
            .copy(otherReliefs = None)
        }

      }

      "have a method which unsets values of private residence relief and letting relief" in {
        val expectedResult =
          IncompleteReliefDetailsAnswers(
            None,
            None,
            Some(otherReliefs)
          )

        incompleteAnswers.unsetPrrAndLettingRelief() shouldBe expectedResult
        completeAnswers.unsetPrrAndLettingRelief()   shouldBe expectedResult
      }

    }

  }
}
