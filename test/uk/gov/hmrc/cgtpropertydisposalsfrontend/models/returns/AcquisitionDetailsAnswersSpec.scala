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
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.AmountInPence
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.AcquisitionDetailsGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.Generators.{arb, sample}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.MoneyGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.TriageQuestionsGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.AcquisitionDetailsAnswers.{CompleteAcquisitionDetailsAnswers, IncompleteAcquisitionDetailsAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.AssetType.{IndirectDisposal, Residential}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.IndividualUserType.{PersonalRepresentativeInPeriodOfAdmin, Self}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.CompleteSingleDisposalTriageAnswers

class AcquisitionDetailsAnswersSpec extends AnyWordSpec with Matchers with SampledScalaCheck {

  "IncompleteAcquisitionDetailsAnswers" must {

    "have a method which converts incomplete answers to complete answers" in {
      forAll { (completeAnswers: CompleteAcquisitionDetailsAnswers) =>
        IncompleteAcquisitionDetailsAnswers.fromCompleteAnswers(
          completeAnswers
        ) shouldBe IncompleteAcquisitionDetailsAnswers(
          Some(completeAnswers.acquisitionMethod),
          Some(completeAnswers.acquisitionDate),
          Some(completeAnswers.acquisitionPrice),
          completeAnswers.rebasedAcquisitionPrice,
          Some(completeAnswers.improvementCosts),
          Some(completeAnswers.acquisitionFees),
          Some(completeAnswers.shouldUseRebase)
        )
      }

    }

    "AcquisitionDetailsAnswers" must {

      "have a method which unsets fields" when {

        val completeAnswers   =
          sample[CompleteAcquisitionDetailsAnswers]
            .copy(rebasedAcquisitionPrice = Some(sample[AmountInPence]))
        val incompleteAnswers =
          IncompleteAcquisitionDetailsAnswers(
            Some(completeAnswers.acquisitionMethod),
            Some(completeAnswers.acquisitionDate),
            Some(completeAnswers.acquisitionPrice),
            completeAnswers.rebasedAcquisitionPrice,
            Some(completeAnswers.improvementCosts),
            Some(completeAnswers.acquisitionFees),
            Some(completeAnswers.shouldUseRebase)
          )

        "given incomplete answers" in {
          incompleteAnswers.unset(
            _.acquisitionMethod
          )                                           shouldBe incompleteAnswers.copy(acquisitionMethod = None)
          incompleteAnswers.unset(_.acquisitionDate)  shouldBe incompleteAnswers
            .copy(acquisitionDate = None)
          incompleteAnswers.unset(_.acquisitionPrice) shouldBe incompleteAnswers
            .copy(acquisitionPrice = None)
          incompleteAnswers.unset(
            _.rebasedAcquisitionPrice
          )                                           shouldBe incompleteAnswers.copy(rebasedAcquisitionPrice = None)
          incompleteAnswers.unset(_.improvementCosts) shouldBe incompleteAnswers
            .copy(improvementCosts = None)
          incompleteAnswers.unset(_.acquisitionFees)  shouldBe incompleteAnswers
            .copy(acquisitionFees = None)
          incompleteAnswers.unset(_.shouldUseRebase)  shouldBe incompleteAnswers
            .copy(shouldUseRebase = None)
        }

        "given complete answers" in {
          completeAnswers.unset(_.acquisitionMethod) shouldBe incompleteAnswers
            .copy(acquisitionMethod = None)
          completeAnswers.unset(_.acquisitionDate)   shouldBe incompleteAnswers
            .copy(acquisitionDate = None)
          completeAnswers.unset(_.acquisitionPrice)  shouldBe incompleteAnswers
            .copy(acquisitionPrice = None)
          completeAnswers.unset(
            _.rebasedAcquisitionPrice
          )                                          shouldBe incompleteAnswers.copy(rebasedAcquisitionPrice = None)
          completeAnswers.unset(_.improvementCosts)  shouldBe incompleteAnswers
            .copy(improvementCosts = None)
          completeAnswers.unset(_.acquisitionFees)   shouldBe incompleteAnswers
            .copy(acquisitionFees = None)
          completeAnswers.unset(_.shouldUseRebase)   shouldBe incompleteAnswers
            .copy(shouldUseRebase = None)
        }

      }

      "have a method which unsets all relevant fields but the acquisition method" when {

        val completeAnswers =
          sample[CompleteAcquisitionDetailsAnswers]
            .copy(rebasedAcquisitionPrice = Some(sample[AmountInPence]))

        "the user is in period of admin and the asset type is indirect disposal" in {
          completeAnswers.unsetAllButAcquisitionMethod(
            sample[CompleteSingleDisposalTriageAnswers].copy(
              individualUserType = Some(PersonalRepresentativeInPeriodOfAdmin),
              assetType = IndirectDisposal
            )
          ) shouldBe IncompleteAcquisitionDetailsAnswers(
            Some(completeAnswers.acquisitionMethod),
            Some(completeAnswers.acquisitionDate),
            None,
            None,
            Some(completeAnswers.improvementCosts),
            None,
            None
          )

        }

        "the user is in period of admin and the asset type is not indirect disposal" in {
          completeAnswers.unsetAllButAcquisitionMethod(
            sample[CompleteSingleDisposalTriageAnswers].copy(
              individualUserType = Some(PersonalRepresentativeInPeriodOfAdmin),
              assetType = Residential
            )
          ) shouldBe IncompleteAcquisitionDetailsAnswers(
            Some(completeAnswers.acquisitionMethod),
            Some(completeAnswers.acquisitionDate),
            None,
            None,
            None,
            None,
            None
          )
        }

        "the user is not in period of admin and the asset type is indirect disposal" in {
          completeAnswers.unsetAllButAcquisitionMethod(
            sample[CompleteSingleDisposalTriageAnswers].copy(
              individualUserType = Some(Self),
              assetType = IndirectDisposal
            )
          ) shouldBe IncompleteAcquisitionDetailsAnswers(
            Some(completeAnswers.acquisitionMethod),
            None,
            None,
            None,
            Some(completeAnswers.improvementCosts),
            None,
            None
          )
        }

        "the user is not in period of admin and the asset type is not indirect disposal" in {
          completeAnswers.unsetAllButAcquisitionMethod(
            sample[CompleteSingleDisposalTriageAnswers].copy(
              individualUserType = Some(Self),
              assetType = Residential
            )
          ) shouldBe IncompleteAcquisitionDetailsAnswers(
            Some(completeAnswers.acquisitionMethod),
            None,
            None,
            None,
            None,
            None,
            None
          )
        }

      }

    }

  }

}
