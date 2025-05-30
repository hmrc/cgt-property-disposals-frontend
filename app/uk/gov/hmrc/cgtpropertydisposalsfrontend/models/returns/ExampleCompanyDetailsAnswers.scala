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
import play.api.libs.json.{Json, OFormat}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.AmountInPence

sealed trait ExampleCompanyDetailsAnswers extends Product with Serializable

@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
object ExampleCompanyDetailsAnswers {

  final case class IncompleteExampleCompanyDetailsAnswers(
    address: Option[Address],
    disposalPrice: Option[AmountInPence],
    acquisitionPrice: Option[AmountInPence]
  ) extends ExampleCompanyDetailsAnswers

  object IncompleteExampleCompanyDetailsAnswers {
    val address          = GenLens[IncompleteExampleCompanyDetailsAnswers](_.address)
    val disposalPrice    = GenLens[IncompleteExampleCompanyDetailsAnswers](_.disposalPrice)
    val acquisitionPrice = GenLens[IncompleteExampleCompanyDetailsAnswers](_.acquisitionPrice)

    val empty: IncompleteExampleCompanyDetailsAnswers =
      IncompleteExampleCompanyDetailsAnswers(None, None, None)

    def fromCompleteAnswers(
      c: CompleteExampleCompanyDetailsAnswers
    ): IncompleteExampleCompanyDetailsAnswers =
      IncompleteExampleCompanyDetailsAnswers(
        Some(c.address),
        Some(c.disposalPrice),
        Some(c.acquisitionPrice)
      )

  }

  final case class CompleteExampleCompanyDetailsAnswers(
    address: Address,
    disposalPrice: AmountInPence,
    acquisitionPrice: AmountInPence
  ) extends ExampleCompanyDetailsAnswers

  implicit class ExampleCompanyDetailsAnswersOps(
    private val m: ExampleCompanyDetailsAnswers
  ) extends AnyVal {

    def fold[A](
      whenIncomplete: IncompleteExampleCompanyDetailsAnswers => A,
      whenComplete: CompleteExampleCompanyDetailsAnswers => A
    ): A =
      m match {
        case i: IncompleteExampleCompanyDetailsAnswers => whenIncomplete(i)
        case c: CompleteExampleCompanyDetailsAnswers   => whenComplete(c)
      }

    def unset[A](
      fieldLens: IncompleteExampleCompanyDetailsAnswers.type => Lens[IncompleteExampleCompanyDetailsAnswers, Option[A]]
    ): IncompleteExampleCompanyDetailsAnswers =
      fieldLens(IncompleteExampleCompanyDetailsAnswers).replace(None)(
        fold(
          identity,
          IncompleteExampleCompanyDetailsAnswers.fromCompleteAnswers
        )
      )

  }

  implicit val completeExampleCompanyDetailsAnswersFormat: OFormat[CompleteExampleCompanyDetailsAnswers]     =
    Json.format[CompleteExampleCompanyDetailsAnswers]
  implicit val incompleteExampleCompanyDetailsAnswersFormat: OFormat[IncompleteExampleCompanyDetailsAnswers] =
    Json.format[IncompleteExampleCompanyDetailsAnswers]

  implicit val format: OFormat[ExampleCompanyDetailsAnswers] = new OFormat[ExampleCompanyDetailsAnswers] {
    import play.api.libs.json._
    override def reads(json: JsValue): JsResult[ExampleCompanyDetailsAnswers] = json match {
      case JsObject(fields) if fields.size == 1 =>
        fields.head match {
          case ("IncompleteExampleCompanyDetailsAnswers", value) =>
            value.validate[IncompleteExampleCompanyDetailsAnswers]
          case ("CompleteExampleCompanyDetailsAnswers", value)   => value.validate[CompleteExampleCompanyDetailsAnswers]
          case (other, _)                                        => JsError(s"Unrecognized ExampleCompanyDetailsAnswers type: $other")
        }
      case _                                    => JsError("Expected ExampleCompanyDetailsAnswers wrapper object with a single entry")
    }

    override def writes(o: ExampleCompanyDetailsAnswers): JsObject = o match {
      case i: IncompleteExampleCompanyDetailsAnswers =>
        Json.obj("IncompleteExampleCompanyDetailsAnswers" -> Json.toJson(i))
      case c: CompleteExampleCompanyDetailsAnswers   => Json.obj("CompleteExampleCompanyDetailsAnswers" -> Json.toJson(c))
    }
  }

}
