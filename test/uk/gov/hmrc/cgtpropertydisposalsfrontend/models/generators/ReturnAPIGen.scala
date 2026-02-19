/*
 * Copyright 2025 HM Revenue & Customs
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

import io.github.martinhh.derived.scalacheck.given
import org.scalacheck.{Arbitrary, Gen}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.B64Html
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{CalculateCgtTaxDueRequest, SubmitReturnRequest, SubmitReturnResponse}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsServiceImpl.ListReturnsResponse

object ReturnAPIGen extends Common {

  given submitReturnRequestGen: Gen[SubmitReturnRequest] = for {
    completeReturn          <- ReturnGen.completeReturnGen
    id                      <- Gen.uuid
    subscribedDetails       <- SubscribedDetailsGen.subscribedDetailsGen
    agentReferenceNumber    <- Gen.option(IdGen.arnGen)
    isFurtherReturn         <- Generators.booleanGen
    checkYourAnswerPageHtml <- Generators.stringGen.map(B64Html(_))
    amendReturnData         <- Gen.option(ReturnGen.amendReturnDataGen)
  } yield SubmitReturnRequest(
    completeReturn,
    id,
    subscribedDetails,
    agentReferenceNumber,
    isFurtherReturn,
    checkYourAnswerPageHtml,
    amendReturnData
  )

  given submitReturnResponseGen: Gen[SubmitReturnResponse] =
    gen[SubmitReturnResponse]

  given listReturnsResponseGen: Gen[ListReturnsResponse] = for {
    returns <- Generators.listOfMax(3, ReturnGen.returnSummaryGen)
  } yield ListReturnsResponse(returns)

  given calculateCgtTaxDueRequestGen: Gen[CalculateCgtTaxDueRequest] =
    for {
      triageAnswers      <- TriageQuestionsGen.completeSingleDisposalTriageAnswersGen
      address            <- AddressGen.ukAddressGen
      disposalDetails    <- disposalDetails
      acquisitionDetails <- acquisitionDetails
      reliefDetails      <- ReliefDetailsGen.completeReliefDetailsAnswersGen
      exemptionAndLosses <- exemptionAndLossesAnswers
      estimatedIncome    <- MoneyGen.amountInPenceGen
      personalAllowance  <- MoneyGen.amountInPenceGen
      initialGainOrLoss  <- Gen.option(MoneyGen.amountInPenceGen)
      isATrust           <- Generators.booleanGen
    } yield CalculateCgtTaxDueRequest(
      triageAnswers,
      address,
      disposalDetails,
      acquisitionDetails,
      reliefDetails,
      exemptionAndLosses,
      estimatedIncome,
      personalAllowance,
      initialGainOrLoss,
      isATrust
    )
}
