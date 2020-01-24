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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.audit

import cats.syntax.eq._
import play.api.libs.json.{Json, OFormat}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.AddressSource
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.ContactNameSource
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.SubscriptionDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.email.EmailSource

final case class SubscriptionRequestEvent(
  prePopulatedUserData: PrePopulatedUserData,
  manuallyEnteredData: ManuallyEnteredData
)

final case class PrePopulatedUserData(
  regime: String,
  sapNumber: String,
  individualDetails: Option[IndividualAuditDetails],
  trustDetails: Option[TrustAuditDetails],
  emailAddress: Option[EmailAuditDetails],
  address: Option[AuditAddress],
  contactName: Option[String]
)

object PrePopulatedUserData {
  implicit val formatPrepop: OFormat[PrePopulatedUserData] = Json.format[PrePopulatedUserData]
}

final case class ManuallyEnteredData(
  contactName: Option[String],
  emailAddress: Option[String],
  address: Option[AuditAddress]
)

object ManuallyEnteredData {
  implicit val formatManual: OFormat[ManuallyEnteredData] = Json.format[ManuallyEnteredData]
}

object SubscriptionRequestEvent {

  def fromSubscriptionDetails(subscriptionDetails: SubscriptionDetails): SubscriptionRequestEvent = {
    val prepopulatedEmailSource =
      if (subscriptionDetails.emailSource === EmailSource.BusinessPartnerRecord)
        Some("ETMP business partner record")
      else if (subscriptionDetails.emailSource === EmailSource.GovernmentGateway)
        Some("government-gateway")
      else
        None

    val auditAddress = AuditAddress.fromAddress(subscriptionDetails.address)

    val prePopulatedUserData = {
      PrePopulatedUserData(
        "CGT",
        subscriptionDetails.sapNumber.value,
        subscriptionDetails.name.toOption.map(i => IndividualAuditDetails(i.firstName, i.lastName)),
        subscriptionDetails.name.swap.toOption.map(t => TrustAuditDetails(t.value)),
        prepopulatedEmailSource.map(source => EmailAuditDetails(subscriptionDetails.emailAddress.value, source)),
        if (subscriptionDetails.addressSource =!= AddressSource.ManuallyEntered) Some(auditAddress) else None,
        if (subscriptionDetails.contactNameSource =!= ContactNameSource.ManuallyEntered)
          Some(subscriptionDetails.contactName.value)
        else None
      )
    }

    val manuallyEnteredData =
      ManuallyEnteredData(
        if (subscriptionDetails.contactNameSource === ContactNameSource.ManuallyEntered)
          Some(subscriptionDetails.contactName.value)
        else None,
        if (prepopulatedEmailSource.isDefined)
          None
        else
          Some(subscriptionDetails.emailAddress.value),
        if (subscriptionDetails.addressSource === AddressSource.ManuallyEntered) Some(auditAddress) else None
      )

    SubscriptionRequestEvent(prePopulatedUserData, manuallyEnteredData)
  }

  implicit val format: OFormat[SubscriptionRequestEvent] = Json.format[SubscriptionRequestEvent]

}
