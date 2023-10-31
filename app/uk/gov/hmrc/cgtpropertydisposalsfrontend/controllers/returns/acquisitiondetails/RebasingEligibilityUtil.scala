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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.acquisitiondetails

import com.google.inject.Singleton
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.RebasingCutoffDates
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.CompleteReturn.CompleteSingleDisposalReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.IndividualUserType.PersonalRepresentativeInPeriodOfAdmin
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{AcquisitionDate, AssetType, RepresentativeType}

import java.time.LocalDate

@Singleton
class RebasingEligibilityUtil {

  def isUk(completeReturn: CompleteSingleDisposalReturn): Boolean =
    extractIsUk(completeReturn)

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
    !wasAUkResident || purchaseDate.isAfter(RebasingCutoffDates.allResidents)

  def isEligibleForRebase(
    completeReturn: CompleteSingleDisposalReturn
  ): Boolean =
    isEligibleForRebase(
      isUk(completeReturn),
      completeReturn.triageAnswers.assetType,
      completeReturn.acquisitionDetails.acquisitionDate,
      completeReturn.triageAnswers.representativeType()
    )

  def isEligibleForRebase(
    wasAUkResident: Boolean,
    assetType: AssetType,
    acquisitionDate: AcquisitionDate,
    representativeType: Option[RepresentativeType]
  ): Boolean =
    rebasingCutOffDateIfEligibleForRebase(acquisitionDate, assetType, wasAUkResident, representativeType).isDefined

  def rebasingCutOffDateIfEligibleForRebase(
    acquisitionDate: AcquisitionDate,
    assetType: AssetType,
    wasUkResident: Boolean,
    representativeType: Option[RepresentativeType]
  ): Option[LocalDate] =
    if (representativeType.contains(PersonalRepresentativeInPeriodOfAdmin)) {
      None
    } else {
      Some(getRebasingCutOffDate(assetType, wasUkResident)).filter(acquisitionDate.value.minusDays(1L).isBefore)
    }

  def getRebasingCutOffDate(
    completeReturn: CompleteSingleDisposalReturn
  ): LocalDate =
    getRebasingCutOffDate(
      completeReturn.triageAnswers.assetType,
      isUk(completeReturn)
    )

  def getRebasingCutOffDate(
    assetType: AssetType,
    wasUkResident: Boolean
  ): LocalDate =
    if (!wasUkResident && assetType == AssetType.NonResidential) {
      RebasingCutoffDates.nonUkResidentsNonResidentialProperty
    } else if (!wasUkResident && assetType == AssetType.Residential) {
      RebasingCutoffDates.nonUkResidentsResidentialProperty
    } else {
      RebasingCutoffDates.allResidents
    }

  private def extractIsUk(
    completeReturn: CompleteSingleDisposalReturn
  ): Boolean =
    completeReturn.triageAnswers.countryOfResidence.isUk

}
