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
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Country
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.AddressGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.Generators.{arb, sample}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.TriageQuestionsGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.{CompleteSingleDisposalTriageAnswers, IncompleteSingleDisposalTriageAnswers}

import java.time.LocalDate

class SingleDisposalTriageAnswersSpec extends AnyWordSpec with Matchers with SampledScalaCheck {

  "IncompleteSingleDisposalTriageAnswers" must {

    "have a method which converts incomplete answers to complete answers for uk resident" in {
      forAll { (c: CompleteSingleDisposalTriageAnswers) =>
        val completeAnswers = c.copy(countryOfResidence = Country.uk)

        IncompleteSingleDisposalTriageAnswers.fromCompleteAnswers(
          completeAnswers
        ) shouldBe IncompleteSingleDisposalTriageAnswers(
          completeAnswers.individualUserType,
          hasConfirmedSingleDisposal = true,
          Some(completeAnswers.disposalMethod),
          wasAUKResident = Some(true),
          None,
          Some(completeAnswers.assetType),
          Some(completeAnswers.disposalDate),
          completeAnswers.alreadySentSelfAssessment,
          Some(completeAnswers.completionDate),
          None
        )
      }
    }

    "have a method which converts incomplete answers to complete answers for non-uk residents" in {
      forAll { (c: CompleteSingleDisposalTriageAnswers) =>
        val completeAnswers = c.copy(countryOfResidence = sample[Country])

        IncompleteSingleDisposalTriageAnswers.fromCompleteAnswers(
          completeAnswers
        ) shouldBe IncompleteSingleDisposalTriageAnswers(
          completeAnswers.individualUserType,
          hasConfirmedSingleDisposal = true,
          Some(completeAnswers.disposalMethod),
          wasAUKResident = Some(false),
          Some(completeAnswers.countryOfResidence),
          Some(completeAnswers.assetType),
          Some(completeAnswers.disposalDate),
          completeAnswers.alreadySentSelfAssessment,
          Some(completeAnswers.completionDate),
          None
        )
      }
    }

  }

  "SingleDisposalTriageAnswers" must {

    "have a method which unsets fields" when {

      val completeAnswers = sample[CompleteSingleDisposalTriageAnswers]
        .copy(
          individualUserType = Some(IndividualUserType.Self),
          countryOfResidence = sample[Country]
        )

      val incompleteAnswers = IncompleteSingleDisposalTriageAnswers(
        Some(IndividualUserType.Self),
        hasConfirmedSingleDisposal = true,
        Some(completeAnswers.disposalMethod),
        wasAUKResident = Some(false),
        Some(completeAnswers.countryOfResidence),
        Some(completeAnswers.assetType),
        Some(completeAnswers.disposalDate),
        completeAnswers.alreadySentSelfAssessment,
        Some(completeAnswers.completionDate),
        None
      )

      "given incomplete answers" in {
        incompleteAnswers.unset(_.individualUserType) shouldBe incompleteAnswers
          .copy(individualUserType = None)
        incompleteAnswers.unset(_.disposalMethod)     shouldBe incompleteAnswers
          .copy(disposalMethod = None)
        incompleteAnswers.unset(_.wasAUKResident)     shouldBe incompleteAnswers
          .copy(wasAUKResident = None)
        incompleteAnswers.unset(_.countryOfResidence) shouldBe incompleteAnswers
          .copy(countryOfResidence = None)
        incompleteAnswers.unset(_.assetType)          shouldBe incompleteAnswers
          .copy(assetType = None)
        incompleteAnswers.unset(_.disposalDate)       shouldBe incompleteAnswers
          .copy(disposalDate = None)
        incompleteAnswers.unset(_.completionDate)     shouldBe incompleteAnswers
          .copy(completionDate = None)
        incompleteAnswers
          .copy(tooEarlyDisposalDate = Some(LocalDate.MAX))
          .unset(_.tooEarlyDisposalDate)              shouldBe incompleteAnswers
          .copy(tooEarlyDisposalDate = None)
      }

      "given complete answers" in {
        completeAnswers.unset(_.individualUserType) shouldBe incompleteAnswers
          .copy(individualUserType = None)
        completeAnswers.unset(_.disposalMethod)     shouldBe incompleteAnswers
          .copy(disposalMethod = None)
        completeAnswers.unset(_.wasAUKResident)     shouldBe incompleteAnswers
          .copy(wasAUKResident = None)
        completeAnswers.unset(_.countryOfResidence) shouldBe incompleteAnswers
          .copy(countryOfResidence = None)
        completeAnswers.unset(_.assetType)          shouldBe incompleteAnswers
          .copy(assetType = None)
        completeAnswers.unset(_.disposalDate)       shouldBe incompleteAnswers
          .copy(disposalDate = None)
        completeAnswers.unset(_.completionDate)     shouldBe incompleteAnswers
          .copy(completionDate = None)
      }

    }

  }
}
