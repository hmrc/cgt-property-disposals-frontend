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

import com.github.ghik.silencer.silent
import julienrf.json.derived
import monocle.Lens
import monocle.macros.Lenses
import play.api.libs.json.{Json, OFormat}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address.UkAddress
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.AmountInPence

sealed trait MixedUsePropertyDetailsAnswers extends Product with Serializable

@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
object MixedUsePropertyDetailsAnswers {

  @Lenses
  final case class IncompleteMixedUsePropertyDetailsAnswers(
    address: Option[UkAddress],
    disposalPrice: Option[AmountInPence],
    acquisitionPrice: Option[AmountInPence]
  ) extends MixedUsePropertyDetailsAnswers

  object IncompleteMixedUsePropertyDetailsAnswers {
    val empty: IncompleteMixedUsePropertyDetailsAnswers =
      IncompleteMixedUsePropertyDetailsAnswers(None, None, None)

    def fromCompleteAnswers(
      c: CompleteMixedUsePropertyDetailsAnswers
    ): IncompleteMixedUsePropertyDetailsAnswers =
      IncompleteMixedUsePropertyDetailsAnswers(
        Some(c.address),
        Some(c.disposalPrice),
        Some(c.acquisitionPrice)
      )

  }

  final case class CompleteMixedUsePropertyDetailsAnswers(
    address: UkAddress,
    disposalPrice: AmountInPence,
    acquisitionPrice: AmountInPence
  ) extends MixedUsePropertyDetailsAnswers

  implicit class ExamplePropertyDetailsAnswersOps(
    private val m: MixedUsePropertyDetailsAnswers
  ) extends AnyVal {

    def fold[A](
      whenIncomplete: IncompleteMixedUsePropertyDetailsAnswers => A,
      whenComplete: CompleteMixedUsePropertyDetailsAnswers => A
    ): A =
      m match {
        case i: IncompleteMixedUsePropertyDetailsAnswers => whenIncomplete(i)
        case c: CompleteMixedUsePropertyDetailsAnswers   => whenComplete(c)
      }

    def unset[A](
      fieldLens: IncompleteMixedUsePropertyDetailsAnswers.type => Lens[
        IncompleteMixedUsePropertyDetailsAnswers,
        Option[
          A
        ]
      ]
    ): IncompleteMixedUsePropertyDetailsAnswers =
      fieldLens(IncompleteMixedUsePropertyDetailsAnswers).set(None)(
        fold(
          identity,
          IncompleteMixedUsePropertyDetailsAnswers.fromCompleteAnswers
        )
      )

  }

  @silent
  implicit val format: OFormat[MixedUsePropertyDetailsAnswers] = {
    implicit val ukAddressFormat: OFormat[UkAddress] = Json.format
    derived.oformat()
  }

}
