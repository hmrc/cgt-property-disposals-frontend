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

import play.api.libs.json.{Json, OFormat, OWrites, Reads}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.bpr.UnsuccessfulNameMatchAttempts.NameMatchDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.bpr.UnsuccessfulNameMatchAttempts.NameMatchDetails.IndividualNameMatchDetails

sealed trait BusinessPartnerRecordNameMatchDetails extends Product with Serializable

object BusinessPartnerRecordNameMatchDetails {

  final case class IndividualNameWithSaUtrAuditDetails(firstName: String, lastName: String, sautr: String)
      extends BusinessPartnerRecordNameMatchDetails

  final case class TrustNameWithTrnAuditDetails(trustName: String, trn: String)
      extends BusinessPartnerRecordNameMatchDetails

  def fromNameMatchDetails(nameMatchDetails: NameMatchDetails): BusinessPartnerRecordNameMatchDetails =
    nameMatchDetails match {
      case IndividualNameMatchDetails(name, sautr) =>
        IndividualNameWithSaUtrAuditDetails(name.firstName, name.lastName, sautr.value)
      case NameMatchDetails.TrustNameMatchDetails(name, trn) => TrustNameWithTrnAuditDetails(name.value, trn.value)
    }

  implicit val format: OFormat[BusinessPartnerRecordNameMatchDetails] = {
    val individualFormat: OFormat[IndividualNameWithSaUtrAuditDetails] =
      Json.format[IndividualNameWithSaUtrAuditDetails]

    val trustFormat: OFormat[TrustNameWithTrnAuditDetails] =
      Json.format[TrustNameWithTrnAuditDetails]

    OFormat(
      Reads(json => individualFormat.reads(json).orElse(trustFormat.reads(json))),
      OWrites[BusinessPartnerRecordNameMatchDetails] {
        case i: IndividualNameWithSaUtrAuditDetails => individualFormat.writes(i)
        case t: TrustNameWithTrnAuditDetails        => trustFormat.writes(t)
      }
    )
  }
}
