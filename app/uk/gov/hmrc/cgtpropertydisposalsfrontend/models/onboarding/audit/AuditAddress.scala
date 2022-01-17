/*
 * Copyright 2022 HM Revenue & Customs
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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.audit

import play.api.libs.json.{Json, OFormat}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.{Address, Country}

final case class AuditAddress(
  line1: String,
  line2: Option[String],
  line3: Option[String],
  line4: Option[String],
  postcode: Option[String],
  country: Country
)

object AuditAddress {

  implicit val formatAddress: OFormat[AuditAddress] = Json.format[AuditAddress]

  def fromAddress(address: Address): AuditAddress =
    address match {
      case Address.UkAddress(line1, line2, town, county, postcode) =>
        AuditAddress(
          line1,
          line2,
          town,
          county,
          Some(postcode.value),
          Country("GB")
        )
      case Address
            .NonUkAddress(line1, line2, line3, line4, postcode, country) =>
        AuditAddress(
          line1,
          line2,
          line3,
          line4,
          postcode,
          country
        )
    }

}
