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

import play.api.libs.json.*

sealed trait JourneyType

case object OnBoarding extends JourneyType

case object Returns extends JourneyType

case object Amend extends JourneyType

object JourneyType {
  implicit val format: Format[JourneyType] = new Format[JourneyType] {
    override def reads(json: JsValue): JsResult[JourneyType] = json match {
      case JsObject(fields) if fields.size == 1 =>
        fields.head._1 match {
          case "OnBoarding" => JsSuccess(OnBoarding)
          case "Returns"    => JsSuccess(Returns)
          case "Amend"      => JsSuccess(Amend)
          case other        => JsError(s"Unknown JourneyType: $other")
        }
      case _                                    => JsError("Expected wrapper object for JourneyType")
    }

    override def writes(o: JourneyType): JsObject = o match {
      case OnBoarding => Json.obj("OnBoarding" -> Json.obj())
      case Returns    => Json.obj("Returns" -> Json.obj())
      case Amend      => Json.obj("Amend" -> Json.obj())
    }
  }
}
