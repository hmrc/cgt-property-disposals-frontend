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

import org.scalacheck.Gen
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ExemptionAndLossesAnswers.{CompleteExemptionAndLossesAnswers, IncompleteExemptionAndLossesAnswers}

object ExemptionsAndLossesAnswersGen extends GenUtils {

  implicit val completeExemptionAndLossesAnswersGen: Gen[CompleteExemptionAndLossesAnswers] =
    for {
      inYearLosses        <- MoneyGen.amountInPenceGen
      previousYearsLosses <- MoneyGen.amountInPenceGen
      annualExemptAmount  <- MoneyGen.amountInPenceGen
    } yield CompleteExemptionAndLossesAnswers(inYearLosses, previousYearsLosses, annualExemptAmount)

  implicit val incompleteExemptionAndLossesAnswersGen: Gen[IncompleteExemptionAndLossesAnswers] =
    for {
      inYearLosses        <- Gen.option(MoneyGen.amountInPenceGen)
      previousYearsLosses <- Gen.option(MoneyGen.amountInPenceGen)
      annualExemptAmount  <- Gen.option(MoneyGen.amountInPenceGen)
    } yield IncompleteExemptionAndLossesAnswers(inYearLosses, previousYearsLosses, annualExemptAmount)

}
