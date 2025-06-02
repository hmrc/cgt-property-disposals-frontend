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
import play.api.libs.json.*

sealed trait UserType extends Product with Serializable

object UserType {

  case object Individual extends UserType
  case object Organisation extends UserType
  case object NonGovernmentGatewayUser extends UserType
  case object Agent extends UserType

  implicit val eq: Eq[UserType] = Eq.fromUniversalEquals

  implicit val format: Format[UserType] = new Format[UserType] {
    override def reads(json: JsValue): JsResult[UserType] = json match {
      case JsObject(fields) if fields.size == 1 =>
        fields.head._1 match {
          case "Individual"               => JsSuccess(Individual)
          case "Organisation"             => JsSuccess(Organisation)
          case "NonGovernmentGatewayUser" => JsSuccess(NonGovernmentGatewayUser)
          case "Agent"                    => JsSuccess(Agent)
          case other                      => JsError(s"Unknown UserType: $other")
        }
      case _                                    => JsError("Expected wrapper object for UserType")
    }

    override def writes(o: UserType): JsObject = o match {
      case Individual               => Json.obj("Individual" -> Json.obj())
      case Organisation             => Json.obj("Organisation" -> Json.obj())
      case NonGovernmentGatewayUser => Json.obj("NonGovernmentGatewayUser" -> Json.obj())
      case Agent                    => Json.obj("Agent" -> Json.obj())
    }
  }

}
