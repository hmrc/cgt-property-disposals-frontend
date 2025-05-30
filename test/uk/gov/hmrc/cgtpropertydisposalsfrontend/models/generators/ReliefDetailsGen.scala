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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators

import org.scalacheck.{Arbitrary, Gen}
import io.github.martinhh.derived.scalacheck.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.AmountInPence
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.MoneyGen.amountInPenceGen
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.OtherReliefsOption.{NoOtherReliefs, OtherReliefs}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ReliefDetailsAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ReliefDetailsAnswers.{CompleteReliefDetailsAnswers, IncompleteReliefDetailsAnswers}

object ReliefDetailsGen extends HigherPriorityReliefDetailGen with GenUtils {
  override implicit val longArb: Arbitrary[Long] = Arbitrary(
    Gen.choose(0.toLong, 5e13.toLong)
  )
  implicit val m: Arbitrary[AmountInPence]       = Arbitrary(amountInPenceGen)
}

trait HigherPriorityReliefDetailGen extends LowerPriorityReliefDetailGen { this: GenUtils =>

  given reliefDetailsAnswersGen: Gen[ReliefDetailsAnswers] = gen[ReliefDetailsAnswers]

  given completeReliefDetailsAnswersGen: Gen[CompleteReliefDetailsAnswers] =
    gen[CompleteReliefDetailsAnswers].map {
      case a: CompleteReliefDetailsAnswers if a.otherReliefs.isEmpty =>
        a.copy(otherReliefs = Some(NoOtherReliefs))
      case other                                                     => other
    }

  given otherReliefsGen: Gen[OtherReliefs] = gen[OtherReliefs]
}

trait LowerPriorityReliefDetailGen { this: GenUtils =>

  given incompleteReliefDetailsAnswersGen: Gen[IncompleteReliefDetailsAnswers] =
    gen[IncompleteReliefDetailsAnswers].map {
      case a: IncompleteReliefDetailsAnswers if a.otherReliefs.isEmpty =>
        a.copy(otherReliefs = Some(NoOtherReliefs))
      case other                                                       => other
    }
}
