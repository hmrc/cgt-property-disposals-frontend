/*
 * Copyright 2023 HM Revenue & Customs
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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.models

import cats.Eq
import play.api.libs.json.{Json, OFormat, Reads, Writes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.UnsuccessfulNameMatchAttempts.NameMatchDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.{SAUTR, TRN}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.{IndividualName, TrustName}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.RepresenteeReferenceId

final case class UnsuccessfulNameMatchAttempts[+A <: NameMatchDetails](
  unsuccessfulAttempts: Int,
  maximumAttempts: Int,
  lastDetailsTried: A
)

object UnsuccessfulNameMatchAttempts {

  sealed trait NameMatchDetails extends Product with Serializable

  object NameMatchDetails {

    final case class IndividualSautrNameMatchDetails(
      name: IndividualName,
      sautr: SAUTR
    ) extends NameMatchDetails

    final case class TrustNameMatchDetails(name: TrustName, trn: TRN) extends NameMatchDetails

    final case class IndividualRepresenteeNameMatchDetails(
      name: IndividualName,
      id: RepresenteeReferenceId
    ) extends NameMatchDetails

    implicit val eq: Eq[NameMatchDetails] = Eq.fromUniversalEquals

    implicit val individualSaUtrNameMatchDetailsFormat: OFormat[IndividualSautrNameMatchDetails] =
      Json.format[IndividualSautrNameMatchDetails]

    implicit val trustNameMatchDetailsFormat: OFormat[TrustNameMatchDetails] =
      Json.format[TrustNameMatchDetails]

    implicit val individualRepresenteeNameMatchDetailsFormat: OFormat[IndividualRepresenteeNameMatchDetails] =
      Json.format[IndividualRepresenteeNameMatchDetails]

    implicit val nameMatchDetailsFormat: OFormat[NameMatchDetails] = Json.format[NameMatchDetails]
  }

  implicit def unsuccessfulNameMatchAttemptsFormat[A <: NameMatchDetails : OFormat]
    : OFormat[UnsuccessfulNameMatchAttempts[A]] = Json.format[UnsuccessfulNameMatchAttempts[A]]

  implicit def unsuccessfulNameMatchAttemptsReads[A <: NameMatchDetails : Reads]
    : Reads[UnsuccessfulNameMatchAttempts[A]] =
    Json.reads[UnsuccessfulNameMatchAttempts[A]]

  implicit def unsuccessfulNameMatchAttemptsWrites[A <: NameMatchDetails : Writes]
    : Writes[UnsuccessfulNameMatchAttempts[A]] =
    Json.writes[UnsuccessfulNameMatchAttempts[A]]

}
