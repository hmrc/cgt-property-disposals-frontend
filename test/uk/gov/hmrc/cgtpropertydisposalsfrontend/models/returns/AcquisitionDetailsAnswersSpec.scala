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

import org.scalatest.{Matchers, WordSpec}
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.AmountInPence
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.AcquisitionDetailsAnswers.{CompleteAcquisitionDetailsAnswers, IncompleteAcquisitionDetailsAnswers}

class AcquisitionDetailsAnswersSpec extends WordSpec with Matchers with ScalaCheckDrivenPropertyChecks {

  "IncompleteAcquisitionDetailsAnswers" must {

    "have a method which converts incomplete answers to complete answers" in {
      forAll { completeAnswers: CompleteAcquisitionDetailsAnswers =>
        IncompleteAcquisitionDetailsAnswers.fromCompleteAnswers(completeAnswers) shouldBe IncompleteAcquisitionDetailsAnswers(
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

        val completeAnswers =
          sample[CompleteAcquisitionDetailsAnswers].copy(rebasedAcquisitionPrice = Some(sample[AmountInPence]))
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
          incompleteAnswers.unset(_.acquisitionMethod) shouldBe incompleteAnswers.copy(acquisitionMethod = None)
          incompleteAnswers.unset(_.acquisitionDate)   shouldBe incompleteAnswers.copy(acquisitionDate   = None)
          incompleteAnswers.unset(_.acquisitionPrice)  shouldBe incompleteAnswers.copy(acquisitionPrice  = None)
          incompleteAnswers.unset(_.rebasedAcquisitionPrice) shouldBe incompleteAnswers.copy(rebasedAcquisitionPrice =
            None
          )
          incompleteAnswers.unset(_.improvementCosts) shouldBe incompleteAnswers.copy(improvementCosts = None)
          incompleteAnswers.unset(_.acquisitionFees)  shouldBe incompleteAnswers.copy(acquisitionFees  = None)
          incompleteAnswers.unset(_.shouldUseRebase)  shouldBe incompleteAnswers.copy(shouldUseRebase  = None)
        }

        "given complete answers" in {
          completeAnswers.unset(_.acquisitionMethod) shouldBe incompleteAnswers.copy(acquisitionMethod = None)
          completeAnswers.unset(_.acquisitionDate)   shouldBe incompleteAnswers.copy(acquisitionDate   = None)
          completeAnswers.unset(_.acquisitionPrice)  shouldBe incompleteAnswers.copy(acquisitionPrice  = None)
          completeAnswers.unset(_.rebasedAcquisitionPrice) shouldBe incompleteAnswers.copy(rebasedAcquisitionPrice =
            None
          )
          completeAnswers.unset(_.improvementCosts) shouldBe incompleteAnswers.copy(improvementCosts = None)
          completeAnswers.unset(_.acquisitionFees)  shouldBe incompleteAnswers.copy(acquisitionFees  = None)
          completeAnswers.unset(_.shouldUseRebase)  shouldBe incompleteAnswers.copy(shouldUseRebase  = None)
        }

      }

    }

  }

}
