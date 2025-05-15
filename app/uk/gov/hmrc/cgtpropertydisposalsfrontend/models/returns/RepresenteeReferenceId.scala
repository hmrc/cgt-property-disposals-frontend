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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns

import play.api.libs.json.*
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.{CgtReference, NINO, SAUTR}

sealed trait RepresenteeReferenceId extends Product with Serializable

object RepresenteeReferenceId {

  final case class RepresenteeNino(value: NINO) extends RepresenteeReferenceId

  final case class RepresenteeSautr(value: SAUTR) extends RepresenteeReferenceId

  final case class RepresenteeCgtReference(value: CgtReference) extends RepresenteeReferenceId

  case object NoReferenceId extends RepresenteeReferenceId

  implicit val noIdFormat: OFormat[NoReferenceId.type] = OFormat[NoReferenceId.type](
    Reads[NoReferenceId.type] {
      case JsObject(_) => JsSuccess(NoReferenceId)
      case _           => JsError("Invalid reference id")
    },
    OWrites[NoReferenceId.type] { _ =>
      Json.obj()
    }
  )

  implicit val ninoFormat: OFormat[RepresenteeNino]           = Json.format[RepresenteeNino]
  implicit val saUtrFormat: OFormat[RepresenteeSautr]         = Json.format[RepresenteeSautr]
  implicit val cgtRefFormat: OFormat[RepresenteeCgtReference] = Json.format[RepresenteeCgtReference]

  implicit val format: OFormat[RepresenteeReferenceId] = new OFormat[RepresenteeReferenceId] {
    override def reads(json: JsValue): JsResult[RepresenteeReferenceId] = json match {
      case JsObject(fields) if fields.size == 1 =>
        fields.head match {
          case ("RepresenteeNino", value)         => value.validate[RepresenteeNino]
          case ("RepresenteeSautr", value)        => value.validate[RepresenteeSautr]
          case ("RepresenteeCgtReference", value) => value.validate[RepresenteeCgtReference]
          case ("NoReferenceId", value)           => value.validate[NoReferenceId.type]
          case (other, _)                         => JsError(s"Unrecognized RepresenteeReferenceId: $other")
        }
      case _                                    =>
        JsError("Expected a RepresenteeReferenceId wrapper object with a single entry")
    }

    override def writes(o: RepresenteeReferenceId): JsObject = o match {
      case n: RepresenteeNino         => Json.obj("RepresenteeNino" -> Json.toJson(n))
      case s: RepresenteeSautr        => Json.obj("RepresenteeSautr" -> Json.toJson(s))
      case c: RepresenteeCgtReference => Json.obj("RepresenteeCgtReference" -> Json.toJson(c))
      case NoReferenceId              => Json.obj("NoReferenceId" -> Json.toJson(NoReferenceId))
    }
  }

}
