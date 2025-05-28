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

import monocle.Lens
import monocle.macros.GenLens
import play.api.libs.json.*
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.AmountInPence

sealed trait DisposalDetailsAnswers extends Product with Serializable

@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
object DisposalDetailsAnswers {

  final case class IncompleteDisposalDetailsAnswers(
    shareOfProperty: Option[ShareOfProperty],
    disposalPrice: Option[AmountInPence],
    disposalFees: Option[AmountInPence]
  ) extends DisposalDetailsAnswers

  object IncompleteDisposalDetailsAnswers {

    val disposalFees    = GenLens[IncompleteDisposalDetailsAnswers](_.disposalFees)
    val disposalPrice   = GenLens[IncompleteDisposalDetailsAnswers](_.disposalPrice)
    val shareOfProperty = GenLens[IncompleteDisposalDetailsAnswers](_.shareOfProperty)

    val empty: IncompleteDisposalDetailsAnswers =
      IncompleteDisposalDetailsAnswers(None, None, None)

    def fromCompleteAnswers(
      c: CompleteDisposalDetailsAnswers
    ): IncompleteDisposalDetailsAnswers =
      IncompleteDisposalDetailsAnswers(
        Some(c.shareOfProperty),
        Some(c.disposalPrice),
        Some(c.disposalFees)
      )
  }

  final case class CompleteDisposalDetailsAnswers(
    shareOfProperty: ShareOfProperty,
    disposalPrice: AmountInPence,
    disposalFees: AmountInPence
  ) extends DisposalDetailsAnswers

  implicit class DisposalDetailsAnswersOps(
    private val i: DisposalDetailsAnswers
  ) extends AnyVal {

    def fold[A](
      ifIncomplete: IncompleteDisposalDetailsAnswers => A,
      ifComplete: CompleteDisposalDetailsAnswers => A
    ): A =
      i match {
        case incomplete: IncompleteDisposalDetailsAnswers =>
          ifIncomplete(incomplete)
        case complete: CompleteDisposalDetailsAnswers     => ifComplete(complete)
      }

    def unset[A](
      fieldLens: IncompleteDisposalDetailsAnswers.type => Lens[IncompleteDisposalDetailsAnswers, Option[A]]
    ): IncompleteDisposalDetailsAnswers =
      fieldLens(IncompleteDisposalDetailsAnswers).replace(None)(
        fold(identity, IncompleteDisposalDetailsAnswers.fromCompleteAnswers)
      )

  }

  implicit val completeFormat: OFormat[CompleteDisposalDetailsAnswers]     = Json.format[CompleteDisposalDetailsAnswers]
  implicit val inCompleteFormat: OFormat[IncompleteDisposalDetailsAnswers] =
    Json.format[IncompleteDisposalDetailsAnswers]

  implicit val format: OFormat[DisposalDetailsAnswers] = new OFormat[DisposalDetailsAnswers] {
    override def reads(json: JsValue): JsResult[DisposalDetailsAnswers] = json match {
      case JsObject(fields) if fields.size == 1 =>
        fields.head match {
          case ("IncompleteDisposalDetailsAnswers", value) =>
            value.validate[IncompleteDisposalDetailsAnswers]
          case ("CompleteDisposalDetailsAnswers", value)   =>
            value.validate[CompleteDisposalDetailsAnswers]
          case (other, _)                                  =>
            JsError(s"Unrecognized DisposalDetailsAnswers type: $other")
        }
      case _                                    =>
        JsError("Expected DisposalDetailsAnswers wrapper object with a single entry")
    }

    override def writes(a: DisposalDetailsAnswers): JsObject = a match {
      case i: IncompleteDisposalDetailsAnswers =>
        Json.obj("IncompleteDisposalDetailsAnswers" -> Json.toJson(i))
      case c: CompleteDisposalDetailsAnswers   =>
        Json.obj("CompleteDisposalDetailsAnswers" -> Json.toJson(c))
    }
  }

}
