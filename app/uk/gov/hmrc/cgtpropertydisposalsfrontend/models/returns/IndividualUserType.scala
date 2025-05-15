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

import cats.Eq
import play.api.libs.json.*

sealed trait IndividualUserType extends Product with Serializable

sealed trait RepresentativeType extends IndividualUserType

object IndividualUserType {

  case object Self extends IndividualUserType

  /** When helping someone do the return */
  case object Capacitor extends RepresentativeType

  /** When disposing for someone before they died */
  case object PersonalRepresentative extends RepresentativeType

  /** When disposing for someone after they died */
  case object PersonalRepresentativeInPeriodOfAdmin extends RepresentativeType

  implicit val individualUserTypeEq: Eq[IndividualUserType] = Eq.fromUniversalEquals

  implicit val representativeTypeEq: Eq[RepresentativeType] = Eq.fromUniversalEquals

  implicit val representativeTypeFormat: Format[RepresentativeType] = new Format[RepresentativeType] {
    override def reads(json: JsValue): JsResult[RepresentativeType] = json match {
      case JsObject(fields) if fields.size == 1 =>
        fields.head._1 match {
          case "Capacitor"                             => JsSuccess(Capacitor)
          case "PersonalRepresentative"                => JsSuccess(PersonalRepresentative)
          case "PersonalRepresentativeInPeriodOfAdmin" => JsSuccess(PersonalRepresentativeInPeriodOfAdmin)
          case other                                   => JsError(s"Invalid representative type: $other")
        }
      case _                                    => JsError("Expected JSON object with one RepresentativeType key")
    }

    override def writes(r: RepresentativeType): JsValue = r match {
      case Capacitor                             => Json.obj("Capacitor" -> Json.obj())
      case PersonalRepresentative                => Json.obj("PersonalRepresentative" -> Json.obj())
      case PersonalRepresentativeInPeriodOfAdmin => Json.obj("PersonalRepresentativeInPeriodOfAdmin" -> Json.obj())
    }
  }

  implicit val individualUserTypeFormat: Format[IndividualUserType] = new Format[IndividualUserType] {
    override def reads(json: JsValue): JsResult[IndividualUserType] = json match {
      case JsObject(fields) if fields.size == 1 =>
        fields.head._1 match {
          case "Self"                                  => JsSuccess(Self)
          case "Capacitor"                             => JsSuccess(Capacitor)
          case "PersonalRepresentative"                => JsSuccess(PersonalRepresentative)
          case "PersonalRepresentativeInPeriodOfAdmin" => JsSuccess(PersonalRepresentativeInPeriodOfAdmin)
          case other                                   => JsError(s"Invalid individual user type: $other")
        }
      case _                                    => JsError("Expected JSON object with one IndividualUserType key")
    }

    override def writes(o: IndividualUserType): JsValue = o match {
      case Self                                  => Json.obj("Self" -> Json.obj())
      case Capacitor                             => Json.obj("Capacitor" -> Json.obj())
      case PersonalRepresentative                => Json.obj("PersonalRepresentative" -> Json.obj())
      case PersonalRepresentativeInPeriodOfAdmin => Json.obj("PersonalRepresentativeInPeriodOfAdmin" -> Json.obj())
    }
  }
}
