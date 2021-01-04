/*
 * Copyright 2021 HM Revenue & Customs
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
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.RegistrationDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.email.EmailSource

final case class RegistrationRequestEvent(
  prePopulatedUserData: RegistrationPrePopulatedUserData,
  manuallyEnteredData: RegistrationManuallyEnteredData
)

final case class RegistrationPrePopulatedUserData(
  regime: String,
  emailAddress: Option[EmailAuditDetails]
)

object RegistrationPrePopulatedUserData {
  implicit val format: OFormat[RegistrationPrePopulatedUserData] =
    Json.format[RegistrationPrePopulatedUserData]
}

final case class RegistrationManuallyEnteredData(
  contactName: String,
  emailAddress: Option[String],
  address: AuditAddress
)

object RegistrationManuallyEnteredData {
  implicit val formatManual: OFormat[RegistrationManuallyEnteredData] =
    Json.format[RegistrationManuallyEnteredData]
}

object RegistrationRequestEvent {

  def fromRegistrationDetails(
    registrationDetails: RegistrationDetails
  ): RegistrationRequestEvent = {
    val prepopulatedEmailSource =
      if (registrationDetails.emailSource === EmailSource.BusinessPartnerRecord)
        Some("ETMP business partner record")
      else if (registrationDetails.emailSource === EmailSource.GovernmentGateway)
        Some("government-gateway")
      else
        None

    val prePopulatedUserData =
      RegistrationPrePopulatedUserData(
        "CGT",
        prepopulatedEmailSource.map(source => EmailAuditDetails(registrationDetails.emailAddress.value, source))
      )

    val manuallyEnteredData =
      RegistrationManuallyEnteredData(
        s"${registrationDetails.name.firstName} ${registrationDetails.name.lastName}",
        if (prepopulatedEmailSource.isEmpty)
          Some(registrationDetails.emailAddress.value)
        else None,
        AuditAddress.fromAddress(registrationDetails.address)
      )

    RegistrationRequestEvent(prePopulatedUserData, manuallyEnteredData)
  }

  implicit val format: OFormat[RegistrationRequestEvent] =
    Json.format[RegistrationRequestEvent]

}
