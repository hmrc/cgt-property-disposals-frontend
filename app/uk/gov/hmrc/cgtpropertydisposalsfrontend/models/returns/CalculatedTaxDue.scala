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
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.AmountInPence

final case class AmountInPenceWithSource(
  amount: AmountInPence,
  source: Source
)

object AmountInPenceWithSource {
  implicit val format: OFormat[AmountInPenceWithSource] = Json.format
}

sealed trait Source
object Source {
  case object UserSupplied extends Source
  case object Calculated extends Source

  implicit val format: Format[Source] = new Format[Source] {
    override def reads(json: JsValue): JsResult[Source] = json match {
      case JsObject(fields) if fields.size == 1 =>
        fields.head._1 match {
          case "UserSupplied" => JsSuccess(UserSupplied)
          case "Calculated"   => JsSuccess(Calculated)
          case other          => JsError(s"Invalid Source type: $other")
        }
      case _                                    => JsError("Expected Source wrapper object with one key")
    }

    override def writes(o: Source): JsValue = o match {
      case UserSupplied => Json.obj("UserSupplied" -> Json.obj())
      case Calculated   => Json.obj("Calculated" -> Json.obj())
    }
  }
}

sealed trait CalculatedTaxDue extends Product with Serializable {
  val disposalAmountLessCosts: AmountInPence
  val acquisitionAmountPlusCosts: AmountInPence
  val initialGainOrLoss: AmountInPenceWithSource
  val gainOrLossAfterInYearLosses: AmountInPence
  val totalReliefs: AmountInPence
  val gainOrLossAfterReliefs: AmountInPence
  val yearPosition: AmountInPence
  val taxableGainOrNetLoss: AmountInPence
  val amountOfTaxDue: AmountInPence
}

object CalculatedTaxDue {

  final case class NonGainCalculatedTaxDue(
    disposalAmountLessCosts: AmountInPence,
    acquisitionAmountPlusCosts: AmountInPence,
    initialGainOrLoss: AmountInPenceWithSource,
    gainOrLossAfterInYearLosses: AmountInPence,
    totalReliefs: AmountInPence,
    gainOrLossAfterReliefs: AmountInPence,
    yearPosition: AmountInPence,
    taxableGainOrNetLoss: AmountInPence,
    amountOfTaxDue: AmountInPence
  ) extends CalculatedTaxDue

  final case class GainCalculatedTaxDue(
    disposalAmountLessCosts: AmountInPence,
    acquisitionAmountPlusCosts: AmountInPence,
    initialGainOrLoss: AmountInPenceWithSource,
    gainOrLossAfterInYearLosses: AmountInPence,
    totalReliefs: AmountInPence,
    gainOrLossAfterReliefs: AmountInPence,
    yearPosition: AmountInPence,
    taxableGainOrNetLoss: AmountInPence,
    taxableIncome: AmountInPence,
    taxDueAtLowerRate: TaxableAmountOfMoney,
    taxDueAtHigherRate: TaxableAmountOfMoney,
    amountOfTaxDue: AmountInPence
  ) extends CalculatedTaxDue

  implicit val gainFormat: OFormat[GainCalculatedTaxDue]       = Json.format[GainCalculatedTaxDue]
  implicit val nonGainFormat: OFormat[NonGainCalculatedTaxDue] = Json.format[NonGainCalculatedTaxDue]

  implicit val format: OFormat[CalculatedTaxDue] = new OFormat[CalculatedTaxDue] {
    override def reads(json: JsValue): JsResult[CalculatedTaxDue] = json match {
      case JsObject(fields) if fields.size == 1 =>
        fields.head match {
          case ("GainCalculatedTaxDue", value)    => value.validate[GainCalculatedTaxDue]
          case ("NonGainCalculatedTaxDue", value) => value.validate[NonGainCalculatedTaxDue]
          case (other, _)                         => JsError(s"Unknown CalculatedTaxDue subtype: $other")
        }
      case _                                    => JsError("Expected wrapper object with one CalculatedTaxDue key")
    }

    override def writes(o: CalculatedTaxDue): JsObject = o match {
      case g: GainCalculatedTaxDue     => Json.obj("GainCalculatedTaxDue" -> Json.toJson(g))
      case ng: NonGainCalculatedTaxDue => Json.obj("NonGainCalculatedTaxDue" -> Json.toJson(ng))
    }
  }

}
