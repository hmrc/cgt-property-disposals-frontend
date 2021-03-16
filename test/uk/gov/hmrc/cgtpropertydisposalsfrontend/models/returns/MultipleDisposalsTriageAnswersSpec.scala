/*
 * Copyright 2021 HM Revenue & Customs
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
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.Generators.{arb, sample}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.AddressGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.TriageQuestionsGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Country
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.MultipleDisposalsTriageAnswers.{CompleteMultipleDisposalsTriageAnswers, IncompleteMultipleDisposalsTriageAnswers}

class MultipleDisposalsTriageAnswersSpec extends WordSpec with Matchers with ScalaCheckDrivenPropertyChecks {

  "IncompleteMultipleDisposalsTriageAnswers" must {

    "have a method which converts incomplete answers to complete answers" when {

      "the country of residence is uk" in {
        forAll { c: CompleteMultipleDisposalsTriageAnswers =>
          val completeAnswers = c.copy(
            countryOfResidence = Country.uk,
            assetTypes = List(AssetType.Residential)
          )

          IncompleteMultipleDisposalsTriageAnswers.fromCompleteAnswers(
            completeAnswers
          ) shouldBe IncompleteMultipleDisposalsTriageAnswers(
            c.individualUserType,
            Some(c.numberOfProperties),
            wasAUKResident = Some(true),
            None,
            Some(true),
            Some(List(AssetType.Residential)),
            Some(c.taxYearExchanged),
            Some(c.taxYear),
            Some(c.completionDate)
          )
        }
      }

      "the country of residence is not uk" in {
        forAll { c: CompleteMultipleDisposalsTriageAnswers =>
          val completeAnswers = c.copy(countryOfResidence = sample[Country])

          IncompleteMultipleDisposalsTriageAnswers.fromCompleteAnswers(
            completeAnswers
          ) shouldBe IncompleteMultipleDisposalsTriageAnswers(
            c.individualUserType,
            Some(c.numberOfProperties),
            wasAUKResident = Some(false),
            Some(completeAnswers.countryOfResidence),
            None,
            Some(completeAnswers.assetTypes),
            Some(c.taxYearExchanged),
            Some(c.taxYear),
            Some(c.completionDate)
          )
        }

      }
    }

    "MultipleDisposalsTriageAnswers" must {

      "have a method which unsets fields" when {

        val ukCompleteAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
          .copy(
            countryOfResidence = Country.uk,
            assetTypes = List(AssetType.Residential),
            individualUserType = Some(IndividualUserType.Self)
          )

        val nonUkCompleteAnswers =
          sample[CompleteMultipleDisposalsTriageAnswers]
            .copy(
              countryOfResidence = sample[Country],
              assetTypes = List(AssetType.Residential),
              individualUserType = Some(IndividualUserType.Self)
            )

        val ukIncompleteAnswers =
          IncompleteMultipleDisposalsTriageAnswers(
            Some(IndividualUserType.Self),
            Some(ukCompleteAnswers.numberOfProperties),
            wasAUKResident = Some(true),
            None,
            Some(true),
            Some(List(AssetType.Residential)),
            Some(ukCompleteAnswers.taxYearExchanged),
            Some(ukCompleteAnswers.taxYear),
            Some(ukCompleteAnswers.completionDate)
          )

        val nonUkIncompleteAnswers =
          IncompleteMultipleDisposalsTriageAnswers(
            Some(IndividualUserType.Self),
            Some(nonUkCompleteAnswers.numberOfProperties),
            wasAUKResident = Some(false),
            Some(nonUkCompleteAnswers.countryOfResidence),
            None,
            Some(nonUkCompleteAnswers.assetTypes),
            Some(nonUkCompleteAnswers.taxYearExchanged),
            Some(nonUkCompleteAnswers.taxYear),
            Some(nonUkCompleteAnswers.completionDate)
          )

        "given incomplete answers" in {
          nonUkIncompleteAnswers.unset(
            _.individualUserType
          ) shouldBe nonUkIncompleteAnswers.copy(individualUserType = None)
          nonUkIncompleteAnswers.unset(
            _.numberOfProperties
          ) shouldBe nonUkIncompleteAnswers.copy(numberOfProperties = None)
          nonUkIncompleteAnswers.unset(
            _.countryOfResidence
          ) shouldBe nonUkIncompleteAnswers.copy(countryOfResidence = None)
          ukIncompleteAnswers.unset(
            _.wereAllPropertiesResidential
          ) shouldBe ukIncompleteAnswers
            .copy(wereAllPropertiesResidential = None)
          nonUkIncompleteAnswers.unset(
            _.assetTypes
          ) shouldBe nonUkIncompleteAnswers.copy(assetTypes = None)
          nonUkIncompleteAnswers.unset(
            _.taxYearExchanged
          ) shouldBe nonUkIncompleteAnswers
            .copy(taxYearExchanged = None)
          nonUkIncompleteAnswers.unset(
            _.taxYear
          ) shouldBe nonUkIncompleteAnswers.copy(taxYear = None)
          nonUkIncompleteAnswers.unset(
            _.completionDate
          ) shouldBe nonUkIncompleteAnswers.copy(completionDate = None)
        }

        "given complete answers" in {
          nonUkCompleteAnswers.unset(
            _.individualUserType
          )                                     shouldBe nonUkIncompleteAnswers.copy(individualUserType = None)
          nonUkCompleteAnswers.unset(
            _.numberOfProperties
          )                                     shouldBe nonUkIncompleteAnswers.copy(numberOfProperties = None)
          nonUkCompleteAnswers.unset(
            _.countryOfResidence
          )                                     shouldBe nonUkIncompleteAnswers.copy(countryOfResidence = None)
          ukCompleteAnswers.unset(
            _.wereAllPropertiesResidential
          )                                     shouldBe ukIncompleteAnswers
            .copy(wereAllPropertiesResidential = None)
          nonUkCompleteAnswers.unset(
            _.assetTypes
          )                                     shouldBe nonUkIncompleteAnswers.copy(assetTypes = None)
          nonUkCompleteAnswers.unset(
            _.taxYearExchanged
          )                                     shouldBe nonUkIncompleteAnswers
            .copy(taxYearExchanged = None)
          nonUkCompleteAnswers.unset(_.taxYear) shouldBe nonUkIncompleteAnswers
            .copy(taxYear = None)
          nonUkCompleteAnswers.unset(
            _.completionDate
          )                                     shouldBe nonUkIncompleteAnswers.copy(completionDate = None)
        }

      }

    }

  }

}
