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
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{AcquisitionDate, AssetType}

private final case class RebaseQuery(isUkResident: Boolean, assetType: AssetType, purchaseDate: LocalDate)

@Singleton
class RebasingEligabilityUtil {

  def isEligableForPrice(wasAUkResident: Boolean, assetType: AssetType, purchaseDate: LocalDate): Boolean = {
    val rebaseQuery = RebaseQuery(wasAUkResident, assetType, purchaseDate)
    rebaseQuery match {
      case RebaseQuery(true, AssetType.Residential, date) => date.isAfter(RebasingCutoffDates.ukResidents)
      case _                                              => true
    }
  }

  def isEligableForRebase(wasAUkResident: Boolean, assetType: AssetType, purchaseDate: LocalDate): Boolean = {
    val rebaseQuery = RebaseQuery(wasAUkResident, assetType, purchaseDate)
    rebaseQuery match {
      case RebaseQuery(false, AssetType.Residential, date) =>
        date.isBefore(RebasingCutoffDates.nonUkResidentsResidentialProperty)
      case RebaseQuery(false, AssetType.NonResidential, date) =>
        date.isBefore(RebasingCutoffDates.nonUkResidentsNonResidentialProperty)
      case RebaseQuery(true, AssetType.Residential, date) => date.isBefore(RebasingCutoffDates.ukResidents)
    }
  }

  def isUkAndEligableForRebase(
    wasUkResident: Boolean,
    acquisitionDate: AcquisitionDate,
    assetType: AssetType
  ): Boolean =
    wasUkResident && rebasingCutOffDate(acquisitionDate, assetType, wasUkResident).isDefined

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

}
