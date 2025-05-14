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

sealed trait ChargeType extends Product with Serializable

object ChargeType {

  case object UkResidentReturn extends ChargeType
  case object NonUkResidentReturn extends ChargeType
  case object DeltaCharge extends ChargeType

  case object Interest extends ChargeType

  case object LateFilingPenalty extends ChargeType

  case object SixMonthLateFilingPenalty extends ChargeType

  case object TwelveMonthLateFilingPenalty extends ChargeType

  case object LatePaymentPenalty extends ChargeType

  case object SixMonthLatePaymentPenalty extends ChargeType

  case object TwelveMonthLatePaymentPenalty extends ChargeType

  case object PenaltyInterest extends ChargeType

  implicit val eq: Eq[ChargeType] = Eq.fromUniversalEquals

  implicit val format: Format[ChargeType] = new Format[ChargeType] {
    override def reads(json: JsValue): JsResult[ChargeType] = json match {
      case JsObject(fields) if fields.size == 1 =>
        fields.head match {
          case ("UkResidentReturn", _)              => JsSuccess(UkResidentReturn)
          case ("NonUkResidentReturn", _)           => JsSuccess(NonUkResidentReturn)
          case ("DeltaCharge", _)                   => JsSuccess(DeltaCharge)
          case ("Interest", _)                      => JsSuccess(Interest)
          case ("LateFilingPenalty", _)             => JsSuccess(LateFilingPenalty)
          case ("SixMonthLateFilingPenalty", _)     => JsSuccess(SixMonthLateFilingPenalty)
          case ("TwelveMonthLateFilingPenalty", _)  => JsSuccess(TwelveMonthLateFilingPenalty)
          case ("LatePaymentPenalty", _)            => JsSuccess(LatePaymentPenalty)
          case ("SixMonthLatePaymentPenalty", _)    => JsSuccess(SixMonthLatePaymentPenalty)
          case ("TwelveMonthLatePaymentPenalty", _) => JsSuccess(TwelveMonthLatePaymentPenalty)
          case ("PenaltyInterest", _)               => JsSuccess(PenaltyInterest)
          case (other, _)                           => JsError(s"Unrecognized ChargeType: $other")
        }
      case _                                    => JsError("Expected JSON object with a single ChargeType field")
    }

    override def writes(o: ChargeType): JsValue = o match {
      case UkResidentReturn              => Json.obj("UkResidentReturn" -> Json.obj())
      case NonUkResidentReturn           => Json.obj("NonUkResidentReturn" -> Json.obj())
      case DeltaCharge                   => Json.obj("DeltaCharge" -> Json.obj())
      case Interest                      => Json.obj("Interest" -> Json.obj())
      case LateFilingPenalty             => Json.obj("LateFilingPenalty" -> Json.obj())
      case SixMonthLateFilingPenalty     => Json.obj("SixMonthLateFilingPenalty" -> Json.obj())
      case TwelveMonthLateFilingPenalty  => Json.obj("TwelveMonthLateFilingPenalty" -> Json.obj())
      case LatePaymentPenalty            => Json.obj("LatePaymentPenalty" -> Json.obj())
      case SixMonthLatePaymentPenalty    => Json.obj("SixMonthLatePaymentPenalty" -> Json.obj())
      case TwelveMonthLatePaymentPenalty => Json.obj("TwelveMonthLatePaymentPenalty" -> Json.obj())
      case PenaltyInterest               => Json.obj("PenaltyInterest" -> Json.obj())
    }
  }

}
