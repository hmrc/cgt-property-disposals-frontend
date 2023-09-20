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
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.AmountInPence

class CalculatedGlarBreakdownSpec extends AnyWordSpec with Matchers {
  "CalculatedGlarBreakdown" when {
    "provided 0/false in all fields" should {
      "return with 0/false values" in {
        val breakdown = CalculatedGlarBreakdown(
          acquisitionPrice = AmountInPence(0),
          acquisitionCosts = AmountInPence(0),
          disposalPrice = AmountInPence(0),
          disposalFees = AmountInPence(0),
          privateResidentReliefs = AmountInPence(0),
          lettingRelief = AmountInPence(0),
          improvementCosts = AmountInPence(0),
          shouldUseRebase = false,
          rebasedAcquisitionPrice = None
        )
        breakdown.propertyDisposalAmountLessCosts    shouldBe AmountInPence(0)
        breakdown.propertyAcquisitionAmountPlusCosts shouldBe AmountInPence(0)
        breakdown.totalReliefs                       shouldBe AmountInPence(0)
        breakdown.initialGainOrLoss                  shouldBe AmountInPence(0)
        breakdown.acquisitionOrRebased               shouldBe AmountInPence(0)
        breakdown.gainOrLossAfterReliefs             shouldBe AmountInPence(0)
      }

    }
    "Given a real life example" should {
      val breakdown = CalculatedGlarBreakdown(
        acquisitionPrice = AmountInPence(300_000_00),
        acquisitionCosts = AmountInPence(15_000_00),
        disposalPrice = AmountInPence(400_000_00),
        disposalFees = AmountInPence(16_000_00),
        privateResidentReliefs = AmountInPence(5_000_00),
        lettingRelief = AmountInPence(1_000_00),
        improvementCosts = AmountInPence(10_000_00),
        shouldUseRebase = false,
        rebasedAcquisitionPrice = None
      )
      "Return an accurate propertyDisposalAmountLessCosts value" in {
        breakdown.propertyDisposalAmountLessCosts shouldBe AmountInPence(38400000)
      }
      "Return an accurate propertyAcquisitionAmountPlusCosts value" in {
        breakdown.propertyAcquisitionAmountPlusCosts shouldBe AmountInPence(32500000)
      }
      "Return an accurate totalReliefs value" in {
        breakdown.totalReliefs shouldBe AmountInPence(600000)
      }
      "Return an accurate initialGainOrLoss value" in {
        breakdown.initialGainOrLoss shouldBe AmountInPence(5900000)
      }
      "Return an accurate acquisitionOrRebased value" in {
        breakdown.acquisitionOrRebased shouldBe AmountInPence(30000000)
      }
      "Return an accurate gainOrLossAfterReliefs value" in {
        breakdown.gainOrLossAfterReliefs shouldBe AmountInPence(5300000)
      }

    }
    "Given a loosing disposal" should {
      val breakdown = CalculatedGlarBreakdown(
        acquisitionPrice = AmountInPence(300_000_00),
        acquisitionCosts = AmountInPence(15_000_00),
        disposalPrice = AmountInPence(250_000_00),
        disposalFees = AmountInPence(16_000_00),
        privateResidentReliefs = AmountInPence(5_000_00),
        lettingRelief = AmountInPence(1_000_00),
        improvementCosts = AmountInPence(10_000_00),
        shouldUseRebase = true,
        rebasedAcquisitionPrice = Some(AmountInPence(350_000_00))
      )
      "Return an accurate propertyDisposalAmountLessCosts value" in {
        breakdown.propertyDisposalAmountLessCosts shouldBe AmountInPence(234_000_00)
      }
      "Return an accurate propertyAcquisitionAmountPlusCosts value" in {
        breakdown.propertyAcquisitionAmountPlusCosts shouldBe AmountInPence(375_000_00)
      }
      "Return an accurate totalReliefs value" in {
        breakdown.totalReliefs shouldBe AmountInPence(6_000_00)
      }
      "Return an accurate initialGainOrLoss value" in {
        breakdown.initialGainOrLoss shouldBe AmountInPence(-141_000_00)
      }
      "Return an accurate acquisitionOrRebased value" in {
        breakdown.acquisitionOrRebased shouldBe AmountInPence(350_000_00)
      }
      "Return an accurate gainOrLossAfterReliefs value" in {
        breakdown.gainOrLossAfterReliefs shouldBe AmountInPence(-135_000_00)
      }
    }
    "using a rebase value" should {
      val breakdown = CalculatedGlarBreakdown(
        acquisitionPrice = AmountInPence(300_000_00),
        acquisitionCosts = AmountInPence(15_000_00),
        disposalPrice = AmountInPence(400_000_00),
        disposalFees = AmountInPence(16_000_00),
        privateResidentReliefs = AmountInPence(5_000_00),
        lettingRelief = AmountInPence(1_000_00),
        improvementCosts = AmountInPence(10_000_00),
        shouldUseRebase = true,
        rebasedAcquisitionPrice = Some(AmountInPence(350_000_00))
      )
      "Return an accurate propertyDisposalAmountLessCosts value" in {
        breakdown.propertyDisposalAmountLessCosts shouldBe AmountInPence(384_000_00)
      }
      "Return an accurate propertyAcquisitionAmountPlusCosts value" in {
        breakdown.propertyAcquisitionAmountPlusCosts shouldBe AmountInPence(375_000_00)
      }
      "Return an accurate totalReliefs value" in {
        breakdown.totalReliefs shouldBe AmountInPence(6_000_00)
      }
      "Return an accurate initialGainOrLoss value" in {
        breakdown.initialGainOrLoss shouldBe AmountInPence(9_000_00)
      }
      "Return an accurate acquisitionOrRebased value" in {
        breakdown.acquisitionOrRebased shouldBe AmountInPence(350_000_00)
      }
      "Return an accurate gainOrLossAfterReliefs value" in {
        breakdown.gainOrLossAfterReliefs shouldBe AmountInPence(3_000_00)
      }
    }
    "Given a rebase value while not using rebase" should {
      val breakdown = CalculatedGlarBreakdown(
        acquisitionPrice = AmountInPence(300_000_00),
        acquisitionCosts = AmountInPence(15_000_00),
        disposalPrice = AmountInPence(400_000_00),
        disposalFees = AmountInPence(16_000_00),
        privateResidentReliefs = AmountInPence(5_000_00),
        lettingRelief = AmountInPence(1_000_00),
        improvementCosts = AmountInPence(10_000_00),
        shouldUseRebase = false,
        rebasedAcquisitionPrice = Some(AmountInPence(350_000_00))
      )
      "Return an accurate propertyDisposalAmountLessCosts value" in {
        breakdown.propertyDisposalAmountLessCosts shouldBe AmountInPence(384_000_00)
      }
      "Return an accurate propertyAcquisitionAmountPlusCosts value" in {
        breakdown.propertyAcquisitionAmountPlusCosts shouldBe AmountInPence(325_000_00)
      }
      "Return an accurate totalReliefs value" in {
        breakdown.totalReliefs shouldBe AmountInPence(6_000_00)
      }
      "Return an accurate initialGainOrLoss value" in {
        breakdown.initialGainOrLoss shouldBe AmountInPence(59_000_00)
      }
      "Return an accurate acquisitionOrRebased value" in {
        breakdown.acquisitionOrRebased shouldBe AmountInPence(300_000_00)
      }
      "Return an accurate gainOrLossAfterReliefs value" in {
        breakdown.gainOrLossAfterReliefs shouldBe AmountInPence(53_000_00)
      }
    }
    "shouldUseRebase is true but no rebasedAcquisitionPrice" should {
      val breakdown = CalculatedGlarBreakdown(
        acquisitionPrice = AmountInPence(300_000_00),
        acquisitionCosts = AmountInPence(15_000_00),
        disposalPrice = AmountInPence(400_000_00),
        disposalFees = AmountInPence(16_000_00),
        privateResidentReliefs = AmountInPence(5_000_00),
        lettingRelief = AmountInPence(1_000_00),
        improvementCosts = AmountInPence(10_000_00),
        shouldUseRebase = true,
        rebasedAcquisitionPrice = None
      )
      "Throw an accurate exception" in {
        assertThrows[Exception](breakdown.acquisitionOrRebased)
      }
    }
  }
}
