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

import play.api.libs.json.{Json, OFormat}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.AmountInPence
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.AcquisitionDetailsAnswers.CompleteAcquisitionDetailsAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.DisposalDetailsAnswers.CompleteDisposalDetailsAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ExemptionAndLossesAnswers.CompleteExemptionAndLossesAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ReliefDetailsAnswers.CompleteReliefDetailsAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.CompleteSingleDisposalTriageAnswers

final case class CalculateCgtTaxDueRequest(
  triageAnswers: CompleteSingleDisposalTriageAnswers,
  disposalDetails: CompleteDisposalDetailsAnswers,
  acquisitionDetails: CompleteAcquisitionDetailsAnswers,
  reliefDetails: CompleteReliefDetailsAnswers,
  exemptionAndLosses: CompleteExemptionAndLossesAnswers,
  estimatedIncome: AmountInPence,
  personalAllowance: AmountInPence,
  initialGainOrLoss: Option[AmountInPence],
  isATrust: Boolean
)

object CalculateCgtTaxDueRequest {

  implicit val format: OFormat[CalculateCgtTaxDueRequest] = {
    implicit val triageFormat: OFormat[CompleteSingleDisposalTriageAnswers]           = Json.format
    implicit val disposalDetailsFormat: OFormat[CompleteDisposalDetailsAnswers]       = Json.format
    implicit val acquisitionDetailsFormat: OFormat[CompleteAcquisitionDetailsAnswers] = Json.format
    implicit val reliefDetailsFormat: OFormat[CompleteReliefDetailsAnswers]           = Json.format
    implicit val exemptionAndLossesFormat: OFormat[CompleteExemptionAndLossesAnswers] = Json.format
    Json.format
  }

}
