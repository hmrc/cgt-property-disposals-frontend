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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name

import cats.Eq
import play.api.libs.json.*

sealed trait ContactNameSource extends Product with Serializable

object ContactNameSource {

  case object DerivedFromBusinessPartnerRecord extends ContactNameSource

  case object ManuallyEntered extends ContactNameSource

  implicit val eq: Eq[ContactNameSource] = Eq.fromUniversalEquals[ContactNameSource]

  implicit val format: Format[ContactNameSource] = new Format[ContactNameSource] {
    override def reads(json: JsValue): JsResult[ContactNameSource] = json match {
      case JsObject(fields) if fields.size == 1 =>
        fields.head._1 match {
          case "ManuallyEntered"                  => JsSuccess(ManuallyEntered)
          case "DerivedFromBusinessPartnerRecord" => JsSuccess(DerivedFromBusinessPartnerRecord)
          case other                              => JsError(s"Unknown contact name source: $other")
        }
      case _                                    => JsError("Expected wrapper object for ContactNameSource")
    }

    override def writes(o: ContactNameSource): JsObject = o match {
      case ManuallyEntered                  => Json.obj("ManuallyEntered" -> Json.obj())
      case DerivedFromBusinessPartnerRecord => Json.obj("DerivedFromBusinessPartnerRecord" -> Json.obj())
    }
  }

}
