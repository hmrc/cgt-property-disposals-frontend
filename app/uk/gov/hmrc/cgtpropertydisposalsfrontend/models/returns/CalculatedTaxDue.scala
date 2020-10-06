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

import julienrf.json.derived
import play.api.libs.json.{Json, OFormat}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.PreviousReturnData
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.AmountInPence
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.AssetType.{NonResidential, Residential}

final case class AmountInPenceWithSource(
  amount: AmountInPence,
  source: Source
)

object AmountInPenceWithSource {
  implicit val format: OFormat[AmountInPenceWithSource] = Json.format
}

sealed trait Source
object Source {
  case object UserSupplied extends Source
  case object Calculated extends Source
  implicit val format: OFormat[Source] = derived.oformat()
}

sealed trait CalculatedTaxDue extends Product with Serializable {
  val disposalAmountLessCosts: AmountInPence
  val acquisitionAmountPlusCosts: AmountInPence
  val initialGainOrLoss: AmountInPenceWithSource
  val totalReliefs: AmountInPence
  val gainOrLossAfterReliefs: AmountInPence
  val totalLosses: AmountInPence
  val gainOrLossAfterLosses: AmountInPence
  val taxableGainOrNetLoss: AmountInPence
  val amountOfTaxDue: AmountInPence
}

final case class CalculatedGlarBreakdown(
  acquisitionPrice: AmountInPence,
  acquisitionCosts: AmountInPence,
  disposalPrice: AmountInPence,
  disposalFees: AmountInPence,
  privateResidentReliefs: AmountInPence,
  lettingRelief: AmountInPence,
  improvementCosts: AmountInPence,
  assetType: Either[NonResidential.type, Residential.type],
  previousReturnData: PreviousReturnData
) {

  val gainOrLossAfterReliefs: AmountInPence = {
    val propertyDisposalAmountLessCosts = disposalPrice ++ disposalFees

    val propertyAcquisitionAmountPlusCosts =
      acquisitionPrice ++ improvementCosts ++ acquisitionCosts

    val totalReliefs = privateResidentReliefs ++ lettingRelief

    val initialGainOrLoss = propertyDisposalAmountLessCosts -- propertyAcquisitionAmountPlusCosts

    initialGainOrLoss -- totalReliefs
  }

  val isGain: Boolean = gainOrLossAfterReliefs.isPositive

}

object CalculatedTaxDue {

  final case class NonGainCalculatedTaxDue(
    disposalAmountLessCosts: AmountInPence,
    acquisitionAmountPlusCosts: AmountInPence,
    initialGainOrLoss: AmountInPenceWithSource,
    totalReliefs: AmountInPence,
    gainOrLossAfterReliefs: AmountInPence,
    totalLosses: AmountInPence,
    gainOrLossAfterLosses: AmountInPence,
    taxableGainOrNetLoss: AmountInPence,
    amountOfTaxDue: AmountInPence
  ) extends CalculatedTaxDue

  final case class GainCalculatedTaxDue(
    disposalAmountLessCosts: AmountInPence,
    acquisitionAmountPlusCosts: AmountInPence,
    initialGainOrLoss: AmountInPenceWithSource,
    totalReliefs: AmountInPence,
    gainOrLossAfterReliefs: AmountInPence,
    totalLosses: AmountInPence,
    gainOrLossAfterLosses: AmountInPence,
    taxableGainOrNetLoss: AmountInPence,
    taxableIncome: AmountInPence,
    taxDueAtLowerRate: TaxableAmountOfMoney,
    taxDueAtHigherRate: TaxableAmountOfMoney,
    amountOfTaxDue: AmountInPence
  ) extends CalculatedTaxDue

  @SuppressWarnings(Array("org,wartremover.warts.PublicInference"))
  implicit val format: OFormat[CalculatedTaxDue] = derived.oformat()

}
