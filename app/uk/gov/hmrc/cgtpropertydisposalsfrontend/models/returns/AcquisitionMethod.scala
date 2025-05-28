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

sealed trait AcquisitionMethod extends Product with Serializable

object AcquisitionMethod {

  case object Bought extends AcquisitionMethod

  case object Inherited extends AcquisitionMethod

  case object Gifted extends AcquisitionMethod

  final case class Other(value: String) extends AcquisitionMethod

  implicit val format: Format[AcquisitionMethod] = new Format[AcquisitionMethod] {
    override def reads(json: JsValue): JsResult[AcquisitionMethod] = json match {
      case JsObject(fields) if fields.size == 1 =>
        fields.head match {
          case ("Bought", _)    => JsSuccess(Bought)
          case ("Inherited", _) => JsSuccess(Inherited)
          case ("Gifted", _)    => JsSuccess(Gifted)
          case ("Other", value) => (value \ "value").validate[String].map(Other(_))
          case (other, _)       => JsError(s"Unrecognized acquisition method: $other")
        }
      case _                                    => JsError("Expected JSON object with one AcquisitionMethod key")
    }

    override def writes(o: AcquisitionMethod): JsValue = o match {
      case Bought       => Json.obj("Bought" -> Json.obj())
      case Inherited    => Json.obj("Inherited" -> Json.obj())
      case Gifted       => Json.obj("Gifted" -> Json.obj())
      case Other(value) => Json.obj("Other" -> Json.obj("value" -> value))
    }
  }

}
