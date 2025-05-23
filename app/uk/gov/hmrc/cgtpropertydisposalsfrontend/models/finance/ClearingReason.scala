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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance

import cats.Eq
import play.api.libs.json.*

sealed trait ClearingReason

object ClearingReason {
  case object IncomingPayment extends ClearingReason
  case object OutgoingPayment extends ClearingReason
  case object WriteOff extends ClearingReason
  case object Reversal extends ClearingReason
  case object MassWriteOff extends ClearingReason
  case object AutomaticClearing extends ClearingReason
  case object SomeOtherClearingReason extends ClearingReason

  implicit val eq: Eq[ClearingReason]         = Eq.fromUniversalEquals
  implicit val format: Format[ClearingReason] = new Format[ClearingReason] {
    override def reads(json: JsValue): JsResult[ClearingReason] = json match {
      case JsObject(fields) if fields.size == 1 =>
        fields.head._1 match {
          case "IncomingPayment"         => JsSuccess(IncomingPayment)
          case "OutgoingPayment"         => JsSuccess(OutgoingPayment)
          case "WriteOff"                => JsSuccess(WriteOff)
          case "Reversal"                => JsSuccess(Reversal)
          case "MassWriteOff"            => JsSuccess(MassWriteOff)
          case "AutomaticClearing"       => JsSuccess(AutomaticClearing)
          case "SomeOtherClearingReason" => JsSuccess(SomeOtherClearingReason)
          case other                     => JsError(s"Invalid clearing reason: $other")
        }
      case _                                    => JsError("Expected JSON object with one ClearingReason key")
    }

    override def writes(reason: ClearingReason): JsValue = reason match {
      case IncomingPayment         => Json.obj("IncomingPayment" -> Json.obj())
      case OutgoingPayment         => Json.obj("OutgoingPayment" -> Json.obj())
      case WriteOff                => Json.obj("WriteOff" -> Json.obj())
      case Reversal                => Json.obj("Reversal" -> Json.obj())
      case MassWriteOff            => Json.obj("MassWriteOff" -> Json.obj())
      case AutomaticClearing       => Json.obj("AutomaticClearing" -> Json.obj())
      case SomeOtherClearingReason => Json.obj("SomeOtherClearingReason" -> Json.obj())
    }
  }
}
