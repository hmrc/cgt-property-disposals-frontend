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

import play.api.libs.json.{Format, JsError, JsObject, JsResult, JsValue, Json, OFormat, Reads, Writes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.email.Email
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.*

sealed trait RetrievedUserType extends Product with Serializable

object RetrievedUserType {

  final case class Individual(
    id: Either[SAUTR, NINO],
    email: Option[Email],
    ggCredId: GGCredId
  ) extends RetrievedUserType

  final case class Trust(sautr: SAUTR, email: Option[Email], ggCredId: GGCredId) extends RetrievedUserType

  final case class OrganisationUnregisteredTrust(
    email: Option[Email],
    ggCredId: GGCredId
  ) extends RetrievedUserType

  final case class IndividualWithInsufficientConfidenceLevel(
    nino: Option[NINO],
    sautr: Option[SAUTR],
    email: Option[Email],
    ggCredId: GGCredId
  ) extends RetrievedUserType

  final case class Subscribed(
    cgtReference: CgtReference,
    ggCredId: GGCredId
  ) extends RetrievedUserType

  final case class Agent(
    GGCredId: GGCredId,
    agentReferenceNumber: Option[AgentReferenceNumber]
  ) extends RetrievedUserType

  final case class NonGovernmentGatewayRetrievedUser(authProvider: String) extends RetrievedUserType

  implicit val trustFormat: OFormat[Trust]                                                                         = Json.format[Trust]
  implicit val organisationUnregisteredTrustFormat: OFormat[OrganisationUnregisteredTrust]                         =
    Json.format[OrganisationUnregisteredTrust]
  implicit val individualWithInsufficientConfidenceLevelFormat: OFormat[IndividualWithInsufficientConfidenceLevel] =
    Json.format[IndividualWithInsufficientConfidenceLevel]
  implicit val agentFormat: OFormat[Agent]                                                                         = Json.format[Agent]
  implicit val subscribedFormat: OFormat[Subscribed]                                                               = Json.format[Subscribed]
  implicit val sautrFormat: Format[SAUTR]                                                                          = SAUTR.format
  implicit val ninoFormat: Format[NINO]                                                                            = NINO.format
  implicit val ggCredIdFormat: Format[GGCredId]                                                                    = GGCredId.format
  implicit val cgtReferenceFormat: Format[CgtReference]                                                            = CgtReference.format
  implicit val agentReferenceNumberFormat: Format[AgentReferenceNumber]                                            = AgentReferenceNumber.format
  implicit val emailFormat: Format[Email]                                                                          = Email.format
  implicit val nonGovernmentGatewayRetrievedUserFormat: OFormat[NonGovernmentGatewayRetrievedUser]                 =
    Json.format[NonGovernmentGatewayRetrievedUser]
  implicit val individualFormat: OFormat[Individual]                                                               = {
    implicit val _: Format[Either[SAUTR, NINO]] =
      Format(
        Reads[Either[SAUTR, NINO]] { json =>
          (json \ "left").validate[SAUTR].map(Left(_)) orElse
            (json \ "right").validate[NINO].map(Right(_))
        },
        Writes {
          case Left(sautr) => Json.obj("left" -> Json.toJson(sautr))
          case Right(nino) => Json.obj("right" -> Json.toJson(nino))
        }
      )

    Json.format[Individual]
  }

  implicit val format: OFormat[RetrievedUserType] = new OFormat[RetrievedUserType] {
    override def reads(json: JsValue): JsResult[RetrievedUserType] = json match {
      case JsObject(fields) if fields.size == 1 =>
        fields.head match {
          case ("Individual", value)                                => value.validate[Individual]
          case ("Trust", value)                                     => value.validate[Trust]
          case ("OrganisationUnregisteredTrust", value)             => value.validate[OrganisationUnregisteredTrust]
          case ("IndividualWithInsufficientConfidenceLevel", value) =>
            value.validate[IndividualWithInsufficientConfidenceLevel]
          case ("Subscribed", value)                                => value.validate[Subscribed]
          case ("Agent", value)                                     => value.validate[Agent]
          case ("NonGovernmentGatewayRetrievedUser", value)         => value.validate[NonGovernmentGatewayRetrievedUser]
          case (other, _)                                           => JsError(s"Unrecognized RetrievedUserType: $other")
        }
      case _                                    => JsError("Expected RetrievedUserType wrapper object with a single entry")
    }

    override def writes(o: RetrievedUserType): JsObject = o match {
      case i: Individual                                => Json.obj("Individual" -> Json.toJson(i))
      case t: Trust                                     => Json.obj("Trust" -> Json.toJson(t))
      case o: OrganisationUnregisteredTrust             => Json.obj("OrganisationUnregisteredTrust" -> Json.toJson(o))
      case i: IndividualWithInsufficientConfidenceLevel =>
        Json.obj("IndividualWithInsufficientConfidenceLevel" -> Json.toJson(i))
      case s: Subscribed                                => Json.obj("Subscribed" -> Json.toJson(s))
      case a: Agent                                     => Json.obj("Agent" -> Json.toJson(a))
      case n: NonGovernmentGatewayRetrievedUser         => Json.obj("NonGovernmentGatewayRetrievedUser" -> Json.toJson(n))
    }
  }
}
