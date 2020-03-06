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

import java.time.LocalDate

import play.api.libs.json.{Json, OFormat}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.{AmountInPence}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SubmitReturnResponse.ReturnCharge

final case class SubmitReturnResponse(
  formBundleId: String,
  charge: Option[ReturnCharge]
)

object SubmitReturnResponse {

  final case class ReturnCharge(
    chargeReference: String,
    amount: AmountInPence,
    dueDate: LocalDate
  )

  implicit val returnChargeFormat: OFormat[ReturnCharge] = Json.format

  implicit val format: OFormat[SubmitReturnResponse] = Json.format

}
