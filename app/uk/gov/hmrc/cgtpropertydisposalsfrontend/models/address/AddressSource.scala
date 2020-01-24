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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address

import cats.Eq
import julienrf.json.derived
import play.api.libs.json.OFormat

sealed trait AddressSource extends Product with Serializable

object AddressSource {

  final case object BusinessPartnerRecord extends AddressSource

  final case object ManuallyEntered extends AddressSource

  implicit val eq: Eq[AddressSource] = Eq.fromUniversalEquals[AddressSource]

  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  implicit val format: OFormat[AddressSource] = derived.oformat()

}
