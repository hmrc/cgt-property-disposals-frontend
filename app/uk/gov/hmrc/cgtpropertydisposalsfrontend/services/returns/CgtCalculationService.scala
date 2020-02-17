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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns

import cats.syntax.order._
import com.google.inject.{ImplementedBy, Singleton}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.AmountInPence
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.AmountInPence._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.AcquisitionDetailsAnswers.CompleteAcquisitionDetailsAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.CalculatedTaxDue.{GainCalculatedTaxDue, NonGainCalculatedTaxDue}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.DisposalDetailsAnswers.CompleteDisposalDetailsAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ExemptionAndLossesAnswers.CompleteExemptionAndLossesAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.IndividualTriageAnswers.CompleteIndividualTriageAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ReliefDetailsAnswers.CompleteReliefDetailsAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{AssetType, CalculatedTaxDue, TaxableAmountOfMoney}

@ImplementedBy(classOf[CgtCalculationServiceImpl])
trait CgtCalculationService {

  def calculateTaxDue(
    triageAnswers: CompleteIndividualTriageAnswers,
    disposalDetails: CompleteDisposalDetailsAnswers,
    acquisitionDetails: CompleteAcquisitionDetailsAnswers,
    reliefDetails: CompleteReliefDetailsAnswers,
    exemptionAndLosses: CompleteExemptionAndLossesAnswers,
    estimatedIncome: AmountInPence,
    personalAllowance: AmountInPence
  ): CalculatedTaxDue

}

@Singleton
class CgtCalculationServiceImpl extends CgtCalculationService {

  def calculateTaxDue(
    triageAnswers: CompleteIndividualTriageAnswers,
    disposalDetails: CompleteDisposalDetailsAnswers,
    acquisitionDetails: CompleteAcquisitionDetailsAnswers,
    reliefDetails: CompleteReliefDetailsAnswers,
    exemptionAndLosses: CompleteExemptionAndLossesAnswers,
    estimatedIncome: AmountInPence,
    personalAllowance: AmountInPence
  ): CalculatedTaxDue = {
    val disposalAmountLessCosts: AmountInPence =
      disposalDetails.disposalPrice -- disposalDetails.disposalFees

    val acquisitionAmountPlusCosts: AmountInPence =
      acquisitionDetails.rebasedAcquisitionPrice.getOrElse(acquisitionDetails.acquisitionPrice) ++
        acquisitionDetails.improvementCosts ++
        acquisitionDetails.acquisitionFees

    val initialGainOrLoss: AmountInPence =
      disposalAmountLessCosts -- acquisitionAmountPlusCosts

    val totalReliefs: AmountInPence = {
      val otherReliefs =
        reliefDetails.otherReliefs.map(_.fold(_.amount, () => AmountInPence.zero)).getOrElse(AmountInPence.zero)
      reliefDetails.privateResidentsRelief ++ reliefDetails.lettingsRelief ++ otherReliefs
    }

    val gainOrLossAfterReliefs: AmountInPence =
      if (initialGainOrLoss > AmountInPence.zero)
        (initialGainOrLoss -- totalReliefs).withFloorZero
      else if (initialGainOrLoss < AmountInPence.zero)
        (initialGainOrLoss ++ totalReliefs).withCeilingZero
      else
        AmountInPence.zero

    val totalLosses: AmountInPence = {
      val previousYearsLosses =
        if (gainOrLossAfterReliefs < AmountInPence.zero ||
            (gainOrLossAfterReliefs -- exemptionAndLosses.inYearLosses) < AmountInPence.zero)
          AmountInPence.zero
        else
          exemptionAndLosses.previousYearsLosses

      exemptionAndLosses.inYearLosses ++ previousYearsLosses
    }

    val gainOrLossAfterLosses: AmountInPence =
      if (gainOrLossAfterReliefs >= AmountInPence.zero)
        (gainOrLossAfterReliefs -- totalLosses).withFloorZero
      else
        (gainOrLossAfterReliefs ++ totalLosses).withCeilingZero

    val taxableGain: AmountInPence =
      if (gainOrLossAfterLosses > AmountInPence.zero)
        (gainOrLossAfterLosses -- exemptionAndLosses.annualExemptAmount).withFloorZero
      else
        gainOrLossAfterReliefs

    if (taxableGain <= AmountInPence.zero)
      NonGainCalculatedTaxDue(
        disposalAmountLessCosts,
        acquisitionAmountPlusCosts,
        initialGainOrLoss,
        totalReliefs,
        gainOrLossAfterReliefs,
        totalLosses,
        gainOrLossAfterLosses,
        taxableGain,
        AmountInPence.zero,
        AmountInPence.zero
      )
    else {
      val taxYear       = triageAnswers.disposalDate.taxYear
      val taxableIncome = (estimatedIncome -- personalAllowance).withFloorZero
      val (lowerBandRate, higherTaxRate) =
        triageAnswers.assetType match {
          case AssetType.Residential => taxYear.cgtRateLowerBandResidential    -> taxYear.cgtRateHigherBandResidential
          case _                     => taxYear.cgtRateLowerBandNonResidential -> taxYear.cgtRateHigherBandNonResidential
        }
      val lowerBandTax = TaxableAmountOfMoney(
        lowerBandRate,
        AmountInPence.zero.max(
          taxableGain.min(
            (taxYear.incomeTaxHigherRateThreshold -- taxableIncome).withFloorZero
          )
        )
      )
      val higherBandTax = TaxableAmountOfMoney(
        higherTaxRate,
        (taxableGain -- lowerBandTax.taxableAmount).withFloorZero
      )

      val taxDue = lowerBandTax.taxDue() ++ higherBandTax.taxDue()

      GainCalculatedTaxDue(
        disposalAmountLessCosts,
        acquisitionAmountPlusCosts,
        initialGainOrLoss,
        totalReliefs,
        gainOrLossAfterReliefs,
        totalLosses,
        gainOrLossAfterLosses,
        taxableGain,
        taxableIncome,
        lowerBandTax,
        higherBandTax,
        taxDue,
        taxDue
      )
    }
  }

}
