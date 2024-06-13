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
import org.scalacheck.ScalacheckShapeless._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.CompleteReturnWithSummary
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.PreviousReturnData
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{AmendReturnData, CompleteReturn, ReturnSummary, ReturnType}


object ReturnGen extends GenUtils {

  implicit val completeReturnGen: Gen[CompleteReturn] =
    Gen.oneOf(
      CompleteReturnGen.completeSingleDisposalReturnGen,
      CompleteReturnGen.completeSingleIndirectDisposalReturnGen,
      CompleteReturnGen.completeSingleMixedUseDisposalReturnGen,
      CompleteReturnGen.completeMultipleDisposalsReturnGen,
      CompleteReturnGen.completeMultipleIndirectDisposalReturnGen
    )

  implicit val returnSummaryGen: Gen[ReturnSummary] = gen[ReturnSummary]

  implicit val previousReturnDataGen: Gen[PreviousReturnData] = gen[PreviousReturnData]

  implicit val returnTypeGen: Gen[ReturnType] = gen[ReturnType]

  implicit val completeReturnWithSummaryGen: Gen[CompleteReturnWithSummary] = for {
    completeReturn <- completeReturnGen
    summary        <- returnSummaryGen
    returnType     <- returnTypeGen
  } yield CompleteReturnWithSummary(completeReturn, summary, returnType)

  implicit val amendReturnDataGen: Gen[AmendReturnData] = for {
    originalReturn                      <- completeReturnWithSummaryGen
    shouldDisplayGainOrLossAfterReliefs <- Generators.booleanGen
  } yield AmendReturnData(originalReturn, shouldDisplayGainOrLossAfterReliefs)
}
