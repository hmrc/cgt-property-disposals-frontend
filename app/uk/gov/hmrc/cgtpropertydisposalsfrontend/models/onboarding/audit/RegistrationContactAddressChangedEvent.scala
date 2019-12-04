/*
 * Copyright 2019 HM Revenue & Customs
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
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Country
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.audit.RegistrationContactAddressChangedEvent.Address

final case class RegistrationContactAddressChangedEvent(
  oldContactAddress: Address,
  newContactAddress: Address,
  source: String
)

object RegistrationContactAddressChangedEvent {

  final case class Address(
    line1: String,
    line2: Option[String],
    line3: Option[String],
    line4: Option[String],
    postcode: Option[String],
    country: Country
  )

  implicit val formatAddress: OFormat[Address] = Json.format[Address]
  implicit val format: OFormat[RegistrationContactAddressChangedEvent] =
    Json.format[RegistrationContactAddressChangedEvent]
}
