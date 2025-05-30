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

import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.AmountInPence

final case class CalculatedGlarBreakdown(
  acquisitionPrice: AmountInPence,
  acquisitionCosts: AmountInPence,
  disposalPrice: AmountInPence,
  disposalFees: AmountInPence,
  privateResidentReliefs: AmountInPence,
  lettingRelief: AmountInPence,
  improvementCosts: AmountInPence,
  shouldUseRebase: Boolean,
  rebasedAcquisitionPrice: Option[AmountInPence]
)

object CalculatedGlarBreakdown {

  implicit class CalculatedGlarBreakdownOps(private val c: CalculatedGlarBreakdown) extends AnyVal {
    def propertyDisposalAmountLessCosts: AmountInPence    = c.disposalPrice -- c.disposalFees
    def propertyAcquisitionAmountPlusCosts: AmountInPence =
      acquisitionOrRebased ++ c.improvementCosts ++ c.acquisitionCosts
    def totalReliefs: AmountInPence                       = c.privateResidentReliefs ++ c.lettingRelief
    def initialGainOrLoss: AmountInPence                  = propertyDisposalAmountLessCosts -- propertyAcquisitionAmountPlusCosts

    def acquisitionOrRebased: AmountInPence   = {
      val initialGainOrLossRebase = (c.shouldUseRebase, c.rebasedAcquisitionPrice) match {
        case (true, Some(rebasedAcquisitionPrice)) => rebasedAcquisitionPrice
        case (false, _)                            => c.acquisitionPrice
        case (true, None)                          => throw new Exception("Unreachable")
      }
      initialGainOrLossRebase
    }
    def gainOrLossAfterReliefs: AmountInPence =
      if (initialGainOrLoss.isPositive) {
        (initialGainOrLoss -- totalReliefs).withFloorZero
      } else if (initialGainOrLoss.isNegative) {
        (initialGainOrLoss ++ totalReliefs).withCeilingZero
      } else {
        AmountInPence.zero
      }

  }

}
