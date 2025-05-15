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

sealed trait ReturnType

object ReturnType {

  case object FirstReturn extends ReturnType

  case object FurtherReturn extends ReturnType

  case object AmendedReturn extends ReturnType

  implicit class ReturnTypeOps(private val r: ReturnType) extends AnyVal {
    def isFurtherOrAmendReturn: Boolean = r match {
      case FirstReturn => false
      case _           => true
    }

    def isAmendReturn: Boolean = r match {
      case AmendedReturn => true
      case _             => false
    }

    def isFirstReturn: Boolean = !isFurtherOrAmendReturn

  }

  implicit val format: Format[ReturnType] = new Format[ReturnType] {
    override def reads(json: JsValue): JsResult[ReturnType] = json match {
      case JsObject(fields) if fields.size == 1 =>
        fields.head._1 match {
          case "FirstReturn"   => JsSuccess(FirstReturn)
          case "FurtherReturn" => JsSuccess(FurtherReturn)
          case "AmendedReturn" => JsSuccess(AmendedReturn)
          case other           => JsError(s"Invalid return type: $other")
        }
      case _                                    => JsError("Expected JSON object with one ReturnType key")
    }

    override def writes(o: ReturnType): JsValue = o match {
      case FirstReturn   => Json.obj("FirstReturn" -> Json.obj())
      case FurtherReturn => Json.obj("FurtherReturn" -> Json.obj())
      case AmendedReturn => Json.obj("AmendedReturn" -> Json.obj())
    }
  }

}
