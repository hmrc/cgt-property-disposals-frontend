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

sealed trait DisposalMethod extends Product with Serializable

object DisposalMethod {

  case object Sold extends DisposalMethod

  case object Gifted extends DisposalMethod

  case object Other extends DisposalMethod

  implicit val eq: Eq[DisposalMethod] = Eq.fromUniversalEquals

  implicit val format: Format[DisposalMethod] = new Format[DisposalMethod] {
    override def reads(json: JsValue): JsResult[DisposalMethod] = json match {
      case JsObject(fields) if fields.size == 1 =>
        fields.head._1 match {
          case "Sold"   => JsSuccess(Sold)
          case "Gifted" => JsSuccess(Gifted)
          case "Other"  => JsSuccess(Other)
          case other    => JsError(s"Invalid disposal method: $other")
        }
      case _                                    => JsError("Expected JSON object with one DisposalMethod key")
    }

    override def writes(o: DisposalMethod): JsValue = o match {
      case Sold   => Json.obj("Sold" -> Json.obj())
      case Gifted => Json.obj("Gifted" -> Json.obj())
      case Other  => Json.obj("Other" -> Json.obj())
    }
  }

}
