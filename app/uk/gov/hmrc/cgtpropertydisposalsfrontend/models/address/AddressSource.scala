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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address

import cats.Eq
import play.api.libs.json.*

sealed trait AddressSource extends Product with Serializable

object AddressSource {

  case object BusinessPartnerRecord extends AddressSource

  case object ManuallyEntered extends AddressSource

  implicit val eq: Eq[AddressSource] = Eq.fromUniversalEquals[AddressSource]

  implicit val format: Format[AddressSource] = new Format[AddressSource] {
    override def reads(json: JsValue): JsResult[AddressSource] = json match {
      case JsObject(fields) if fields.size == 1 =>
        fields.head._1 match {
          case "ManuallyEntered"       => JsSuccess(ManuallyEntered)
          case "BusinessPartnerRecord" => JsSuccess(BusinessPartnerRecord)
          case other                   => JsError(s"Unknown AddressSource: $other")
        }
      case _                                    => JsError("Expected wrapper object for AddressSource")
    }

    override def writes(o: AddressSource): JsObject = o match {
      case ManuallyEntered       => Json.obj("ManuallyEntered" -> Json.obj())
      case BusinessPartnerRecord => Json.obj("BusinessPartnerRecord" -> Json.obj())
    }
  }
}
