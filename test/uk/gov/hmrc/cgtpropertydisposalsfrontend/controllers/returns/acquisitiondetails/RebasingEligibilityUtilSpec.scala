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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.acquisitiondetails;

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.RebasingCutoffDates
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.AcquisitionDetailsGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.CompleteReturnGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.TriageQuestionsGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.AcquisitionDate
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.AcquisitionDetailsAnswers.CompleteAcquisitionDetailsAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.AssetType.{NonResidential, Residential}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.CompleteReturn.CompleteSingleDisposalReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.CompleteSingleDisposalTriageAnswers

class RebasingEligibilityUtilSpec extends AnyWordSpec with Matchers {

  object uk {
    val beforeCutoff = sample[CompleteAcquisitionDetailsAnswers]
      .copy(acquisitionDate = AcquisitionDate(RebasingCutoffDates.ukResidents.minusDays(1)))
    val afterCutoff  = sample[CompleteAcquisitionDetailsAnswers]
      .copy(acquisitionDate = AcquisitionDate(RebasingCutoffDates.ukResidents.plusDays(1)))
    val triage       = sample[CompleteSingleDisposalTriageAnswers]
      .copy(countryOfResidence = Country.uk, individualUserType = None)
  }

  object nonUkNonResidential {
    val beforeCutoff = sample[CompleteAcquisitionDetailsAnswers]
      .copy(acquisitionDate =
        AcquisitionDate(
          RebasingCutoffDates.nonUkResidentsNonResidentialProperty.minusDays(1)
        )
      )
    val afterCutoff  = sample[CompleteAcquisitionDetailsAnswers]
      .copy(acquisitionDate =
        AcquisitionDate(
          RebasingCutoffDates.nonUkResidentsNonResidentialProperty.plusDays(1)
        )
      )
    val triage       = sample[CompleteSingleDisposalTriageAnswers]
      .copy(
        countryOfResidence = Country("US"),
        assetType = NonResidential,
        individualUserType = None
      )
  }

  object nonUkResidential {
    val beforeCutoff = sample[CompleteAcquisitionDetailsAnswers]
      .copy(acquisitionDate =
        AcquisitionDate(
          RebasingCutoffDates.nonUkResidentsResidentialProperty.minusDays(1)
        )
      )
    val afterCutoff  = sample[CompleteAcquisitionDetailsAnswers]
      .copy(acquisitionDate =
        AcquisitionDate(
          RebasingCutoffDates.nonUkResidentsResidentialProperty.plusDays(1)
        )
      )
    val triage       = sample[CompleteSingleDisposalTriageAnswers]
      .copy(
        countryOfResidence = Country("US"),
        assetType = Residential,
        individualUserType = None
      )
  }

  val underTest = new RebasingEligibilityUtil();

  "RebasingEligibilityUtil" must {

    "when checking is UK" when {

      "is uk" in {
        val completedReturn =
          sample[CompleteSingleDisposalReturn].copy(
            triageAnswers = uk.triage,
            acquisitionDetails = uk.beforeCutoff
          )
        underTest.isUk(completedReturn) shouldBe true
      }

      "is not uk" in {
        val completedReturn =
          sample[CompleteSingleDisposalReturn]
            .copy(
              triageAnswers = nonUkNonResidential.triage,
              acquisitionDetails = nonUkNonResidential.afterCutoff
            )
        underTest.isUk(completedReturn) shouldBe false
      }

    }

    "when checking for eligibility for rebasing" when {

      "complete return uk and eligible" in {
        val completedReturn =
          sample[CompleteSingleDisposalReturn].copy(
            triageAnswers = uk.triage,
            acquisitionDetails = uk.beforeCutoff
          )
        underTest.isEligibleForRebase(completedReturn) shouldBe true
      }

      "complete return uk and not eligible" in {
        val completedReturn =
          sample[CompleteSingleDisposalReturn].copy(
            triageAnswers = uk.triage,
            acquisitionDetails = uk.afterCutoff
          )
        underTest.isEligibleForRebase(completedReturn) shouldBe false
      }

      "complete return non uk, residential and eligible" in {
        val completedReturn =
          sample[CompleteSingleDisposalReturn].copy(
            triageAnswers = nonUkResidential.triage,
            acquisitionDetails = nonUkResidential.beforeCutoff
          )
        underTest.isEligibleForRebase(completedReturn) shouldBe true
      }

      "complete return non uk, residential and not eligible" in {
        val completedReturn =
          sample[CompleteSingleDisposalReturn].copy(
            triageAnswers = nonUkResidential.triage,
            acquisitionDetails = nonUkResidential.afterCutoff
          )
        underTest.isEligibleForRebase(completedReturn) shouldBe false
      }

      "complete return non uk, non residential and eligible" in {
        val completedReturn =
          sample[CompleteSingleDisposalReturn].copy(
            triageAnswers = nonUkNonResidential.triage,
            acquisitionDetails = nonUkNonResidential.beforeCutoff
          )
        underTest.isEligibleForRebase(completedReturn) shouldBe true
      }

      "complete return non uk, non residential and not eligible" in {
        val completedReturn =
          sample[CompleteSingleDisposalReturn].copy(
            triageAnswers = nonUkNonResidential.triage,
            acquisitionDetails = nonUkNonResidential.afterCutoff
          )
        underTest.isEligibleForRebase(completedReturn) shouldBe false
      }
    }

    "when checking for eligibility for acquisition price" when {

      "complete return uk and eligible" in {
        val completedReturn =
          sample[CompleteSingleDisposalReturn].copy(
            triageAnswers = uk.triage,
            acquisitionDetails = uk.afterCutoff
          )
        underTest.isEligibleForAcquisitionPrice(completedReturn) shouldBe true
      }

      "complete return uk and not eligible" in {
        val completedReturn =
          sample[CompleteSingleDisposalReturn].copy(
            triageAnswers = uk.triage,
            acquisitionDetails = uk.beforeCutoff
          )
        underTest.isEligibleForAcquisitionPrice(completedReturn) shouldBe false
      }

      "complete return non uk, residential and eligible - before" in {
        val completedReturn =
          sample[CompleteSingleDisposalReturn].copy(
            triageAnswers = nonUkResidential.triage,
            acquisitionDetails = nonUkResidential.beforeCutoff
          )
        underTest.isEligibleForAcquisitionPrice(completedReturn) shouldBe true
      }

      "complete return non uk, residential and eligible - after" in {
        val completedReturn =
          sample[CompleteSingleDisposalReturn].copy(
            triageAnswers = nonUkResidential.triage,
            acquisitionDetails = nonUkResidential.afterCutoff
          )
        underTest.isEligibleForAcquisitionPrice(completedReturn) shouldBe true
      }

      "complete return non uk, non residential and eligible - before" in {
        val completedReturn =
          sample[CompleteSingleDisposalReturn].copy(
            triageAnswers = nonUkNonResidential.triage,
            acquisitionDetails = nonUkNonResidential.beforeCutoff
          )
        underTest.isEligibleForAcquisitionPrice(completedReturn) shouldBe true
      }

      "complete return non uk, non residential and eligible - after" in {
        val completedReturn =
          sample[CompleteSingleDisposalReturn].copy(
            triageAnswers = nonUkNonResidential.triage,
            acquisitionDetails = nonUkNonResidential.afterCutoff
          )
        underTest.isEligibleForAcquisitionPrice(completedReturn) shouldBe true
      }
    }
  }
}
