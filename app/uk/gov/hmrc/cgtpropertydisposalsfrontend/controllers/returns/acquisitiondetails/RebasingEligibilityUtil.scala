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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.acquisitiondetails

import java.time.LocalDate

import cats.syntax.eq._
import com.google.inject.Singleton
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.RebasingCutoffDates
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.CompleteReturn.CompleteSingleDisposalReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{AcquisitionDate, AssetType}

@Singleton
class RebasingEligibilityUtil {

  def isUk(completeReturn: CompleteSingleDisposalReturn): Boolean =
    extractIsUk(completeReturn)

  def isEligibleForRebase(
    completeReturn: CompleteSingleDisposalReturn
  ): Boolean =
    isEligibleForRebase(
      isUk(completeReturn),
      extractAssetType(completeReturn),
      completeReturn.acquisitionDetails.acquisitionDate.value
    )

  def isEligibleForAcquisitionPrice(
    completeReturn: CompleteSingleDisposalReturn
  ): Boolean =
    isEligibleForAcquisitionPrice(
      isUk(completeReturn),
      completeReturn.acquisitionDetails.acquisitionDate.value
    )

  def isEligibleForAcquisitionPrice(
    wasAUkResident: Boolean,
    purchaseDate: LocalDate
  ): Boolean =
    !wasAUkResident || purchaseDate.isAfter(RebasingCutoffDates.ukResidents)

  def isEligibleForRebase(
    wasAUkResident: Boolean,
    assetType: AssetType,
    purchaseDate: LocalDate
  ): Boolean =
    (wasAUkResident, assetType, purchaseDate) match {
      case (true, _, date)                           => date.isBefore(RebasingCutoffDates.ukResidents)
      case (false, AssetType.Residential, date)      =>
        date.isBefore(RebasingCutoffDates.nonUkResidentsResidentialProperty)
      case (false, AssetType.NonResidential, date)   =>
        date.isBefore(RebasingCutoffDates.nonUkResidentsNonResidentialProperty)
      case (false, AssetType.IndirectDisposal, date) =>
        date.isBefore(RebasingCutoffDates.nonUkResidentsNonResidentialProperty)
      case _                                         => false
    }

  def rebasingCutOffDate(
    acquisitionDate: AcquisitionDate,
    assetType: AssetType,
    wasUkResident: Boolean
  ): Option[LocalDate] = {
    val cutoffDate =
      if (wasUkResident)
        RebasingCutoffDates.ukResidents
      else if (assetType === AssetType.Residential)
        RebasingCutoffDates.nonUkResidentsResidentialProperty
      else RebasingCutoffDates.nonUkResidentsNonResidentialProperty

    Some(cutoffDate).filter(acquisitionDate.value.isBefore)
  }

  def invalidForRebasing(
    acquisitionDate: AcquisitionDate,
    assetType: AssetType,
    wasUkResident: Boolean
  ): Option[LocalDate] = {
    val cutoffDate =
      if (wasUkResident)
        RebasingCutoffDates.ukResidents
      else if (assetType === AssetType.Residential)
        RebasingCutoffDates.nonUkResidentsResidentialProperty
      else RebasingCutoffDates.nonUkResidentsNonResidentialProperty

    Some(cutoffDate).filter(acquisitionDate.value.isBefore)
  }

  def getRebasingCutOffDate(
    completeReturn: CompleteSingleDisposalReturn
  ): LocalDate =
    getRebasingCutOffDate(
      extractAssetType(completeReturn),
      isUk(completeReturn)
    )

  def getRebasingCutOffDate(
    assetType: AssetType,
    wasUkResident: Boolean
  ): LocalDate =
    if (wasUkResident)
      RebasingCutoffDates.ukResidents
    else if (assetType === AssetType.Residential)
      RebasingCutoffDates.nonUkResidentsResidentialProperty
    else RebasingCutoffDates.nonUkResidentsNonResidentialProperty

  private def extractIsUk(
    completeReturn: CompleteSingleDisposalReturn
  ): Boolean =
    completeReturn.triageAnswers.countryOfResidence.isUk()

  private def extractAssetType(
    completeReturn: CompleteSingleDisposalReturn
  ): AssetType =
    completeReturn.triageAnswers.assetType

}
