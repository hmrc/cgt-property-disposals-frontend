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

sealed trait OtherReliefsOption extends Product with Serializable

object OtherReliefsOption {

  final case class OtherReliefs(name: String, amount: AmountInPence) extends OtherReliefsOption

  case object NoOtherReliefs extends OtherReliefsOption

  implicit class OtherReliefsOptionOps(private val o: OtherReliefsOption) extends AnyVal {
    def fold[A](
      ifOtherReliefs: OtherReliefs => A,
      ifNoOtherReliefs: () => A
    ): A =
      o match {
        case NoOtherReliefs      => ifNoOtherReliefs()
        case value: OtherReliefs => ifOtherReliefs(value)
      }
  }

  implicit val noOtherReliefsFormat: OFormat[NoOtherReliefs.type] = OFormat[NoOtherReliefs.type](
    Reads[NoOtherReliefs.type] {
      case JsObject(_) => JsSuccess(NoOtherReliefs)
      case _           => JsError("Invalid other reliefs option")
    },
    OWrites[NoOtherReliefs.type] { _ =>
      Json.obj()
    }
  )

  implicit val otherReliefsFormat: OFormat[OtherReliefs] = Json.format[OtherReliefs]
  implicit val format: OFormat[OtherReliefsOption]       = new OFormat[OtherReliefsOption] {
    override def reads(json: JsValue): JsResult[OtherReliefsOption] = json match {
      case JsObject(fields) if fields.size == 1 =>
        fields.head match {
          case ("NoOtherReliefs", _) => JsSuccess(NoOtherReliefs)
          case ("OtherReliefs", v)   => v.validate[OtherReliefs]
          case (other, _)            => JsError(s"Unrecognized OtherReliefsOption type: $other")
        }
      case _                                    => JsError("Expected OtherReliefsOption wrapper object with a single entry")
    }

    override def writes(o: OtherReliefsOption): JsObject = o match {
      case NoOtherReliefs  => Json.obj("NoOtherReliefs" -> Json.obj())
      case o: OtherReliefs => Json.obj("OtherReliefs" -> Json.toJson(o))
    }
  }

}
