/*
 * Copyright 2020 HM Revenue & Customs
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

import julienrf.json.derived
import monocle.Lens
import monocle.macros.Lenses
import play.api.libs.json.{Json, OFormat}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address.UkAddress
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.AmountInPence

sealed trait ExamplePropertyDetailsAnswers extends Product with Serializable

@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
object ExamplePropertyDetailsAnswers {

  @Lenses
  final case class IncompleteExamplePropertyDetailsAnswers(
    address: Option[UkAddress],
    disposalDate: Option[DisposalDate],
    disposalPrice: Option[AmountInPence],
    acquisitionPrice: Option[AmountInPence]
  ) extends ExamplePropertyDetailsAnswers

  object IncompleteExamplePropertyDetailsAnswers {
    val empty: IncompleteExamplePropertyDetailsAnswers =
      IncompleteExamplePropertyDetailsAnswers(None, None, None, None)

    def fromCompleteAnswers(c: CompleteExamplePropertyDetailsAnswers): IncompleteExamplePropertyDetailsAnswers =
      IncompleteExamplePropertyDetailsAnswers(
        Some(c.address),
        Some(c.disposalDate),
        Some(c.disposalPrice),
        Some(c.acquisitionPrice)
      )

  }

  final case class CompleteExamplePropertyDetailsAnswers(
    address: UkAddress,
    disposalDate: DisposalDate,
    disposalPrice: AmountInPence,
    acquisitionPrice: AmountInPence
  ) extends ExamplePropertyDetailsAnswers

  implicit class ExamplePropertyDetailsAnswersOps(
    private val m: ExamplePropertyDetailsAnswers
  ) extends AnyVal {

    def fold[A](
      whenIncomplete: IncompleteExamplePropertyDetailsAnswers => A,
      whenComplete: CompleteExamplePropertyDetailsAnswers => A
    ): A = m match {
      case i: IncompleteExamplePropertyDetailsAnswers => whenIncomplete(i)
      case c: CompleteExamplePropertyDetailsAnswers   => whenComplete(c)
    }

    def unset[A](
      fieldLens: IncompleteExamplePropertyDetailsAnswers.type => Lens[IncompleteExamplePropertyDetailsAnswers, Option[
        A
      ]]
    ): IncompleteExamplePropertyDetailsAnswers =
      fieldLens(IncompleteExamplePropertyDetailsAnswers).set(None)(
        fold(identity, IncompleteExamplePropertyDetailsAnswers.fromCompleteAnswers)
      )

  }

  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  implicit val format: OFormat[ExamplePropertyDetailsAnswers] = {
    implicit val ukAddressFormat: OFormat[UkAddress] = Json.format
    derived.oformat()
  }

}
