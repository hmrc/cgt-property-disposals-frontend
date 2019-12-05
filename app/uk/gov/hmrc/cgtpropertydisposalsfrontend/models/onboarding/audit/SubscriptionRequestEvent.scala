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

final case class SubscriptionRequestEvent(
  prePopulatedUserData: PrePopulatedUserData,
  manuallyEnteredData: ManuallyEnteredData
)

final case class PrePopulatedUserData(
  regime: String,
  sapNumber: String,
  individualDetails: Option[IndividualAuditDetails],
  trustDetails: Option[TrustAuditDetails],
  emailAddress: Option[EmailAuditDetails]
)

object PrePopulatedUserData {
  implicit val formatPrepop: OFormat[PrePopulatedUserData] = Json.format[PrePopulatedUserData]
}

final case class ManuallyEnteredData(
  contactName: String,
  emailAddress: Option[String],
  address: Address
)

object ManuallyEnteredData {
  implicit val formatManual: OFormat[ManuallyEnteredData] = Json.format[ManuallyEnteredData]
}

object SubscriptionRequestEvent {
  implicit val format: OFormat[SubscriptionRequestEvent] = Json.format[SubscriptionRequestEvent]

}
