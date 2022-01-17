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

import play.api.libs.json.{Json, OWrites}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.UnsuccessfulNameMatchAttempts.NameMatchDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.UnsuccessfulNameMatchAttempts.NameMatchDetails._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.RepresenteeReferenceId.{RepresenteeNino, RepresenteeSautr}

sealed trait BusinessPartnerRecordNameMatchAuditDetails extends Product with Serializable

object BusinessPartnerRecordNameMatchAuditDetails {

  final case class IndividualNameWithSaUtrAuditDetails(
    firstName: String,
    lastName: String,
    sautr: String
  ) extends BusinessPartnerRecordNameMatchAuditDetails

  final case class IndividualNameWithNinoAuditDetails(
    firstName: String,
    lastName: String,
    nino: String
  ) extends BusinessPartnerRecordNameMatchAuditDetails

  final case class TrustNameWithTrnAuditDetails(trustName: String, trn: String)
      extends BusinessPartnerRecordNameMatchAuditDetails

  def fromNameMatchDetails(
    nameMatchDetails: NameMatchDetails
  ): Option[BusinessPartnerRecordNameMatchAuditDetails] =
    nameMatchDetails match {
      case IndividualSautrNameMatchDetails(name, sautr) =>
        Some(
          IndividualNameWithSaUtrAuditDetails(
            name.firstName,
            name.lastName,
            sautr.value
          )
        )

      case TrustNameMatchDetails(name, trn) =>
        Some(TrustNameWithTrnAuditDetails(name.value, trn.value))

      case IndividualRepresenteeNameMatchDetails(name, id) =>
        id match {
          case RepresenteeNino(nino)   =>
            Some(
              IndividualNameWithNinoAuditDetails(
                name.firstName,
                name.lastName,
                nino.value
              )
            )
          case RepresenteeSautr(sautr) =>
            Some(
              IndividualNameWithSaUtrAuditDetails(
                name.firstName,
                name.lastName,
                sautr.value
              )
            )
          case _                       => None
        }

    }

  implicit val writes: OWrites[BusinessPartnerRecordNameMatchAuditDetails] = {
    val individualSautrWrites: OWrites[IndividualNameWithSaUtrAuditDetails] =
      Json.writes[IndividualNameWithSaUtrAuditDetails]

    val individualNinoWrites: OWrites[IndividualNameWithNinoAuditDetails] =
      Json.writes[IndividualNameWithNinoAuditDetails]

    val trustWrites: OWrites[TrustNameWithTrnAuditDetails] =
      Json.writes[TrustNameWithTrnAuditDetails]

    OWrites[BusinessPartnerRecordNameMatchAuditDetails] {
      case i: IndividualNameWithSaUtrAuditDetails =>
        individualSautrWrites.writes(i)
      case i: IndividualNameWithNinoAuditDetails  =>
        individualNinoWrites.writes(i)
      case t: TrustNameWithTrnAuditDetails        => trustWrites.writes(t)
    }
  }
}
