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
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.{AmountInPence, Charge}

final case class ReturnSummary(
  submissionId: String,
  submissionDate: LocalDate,
  completionDate: LocalDate,
  lastUpdatedDate: Option[LocalDate],
  taxYear: String,
  mainReturnChargeAmount: AmountInPence,
  propertyAddress: Address,
  charges: List[Charge],
  isRecentlyAmended: Boolean
)

object ReturnSummary {

  implicit class ReturnSummaryOps(private val r: ReturnSummary) extends AnyVal {

    def totalCharges(): AmountInPence =
      AmountInPence(r.charges.map(_.amount.withFloorZero.value).sum)

    def totalPaid(): AmountInPence =
      AmountInPence(r.charges.map(_.totalPaid().withFloorZero.value).sum)

    def totalOutstanding(): AmountInPence = totalCharges() -- totalPaid()

  }

  implicit val format: OFormat[ReturnSummary] = Json.format

}
