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

import cats.kernel.Eq
import julienrf.json.derived
import monocle.Lens
import monocle.macros.Lenses
import play.api.libs.json.OFormat
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.AmountInPence

sealed trait ExemptionAndLossesAnswers extends Product with Serializable

@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
object ExemptionAndLossesAnswers {

  @Lenses
  final case class IncompleteExemptionAndLossesAnswers(
    inYearLosses: Option[AmountInPence],
    previousYearsLosses: Option[AmountInPence],
    annualExemptAmount: Option[AmountInPence]
  ) extends ExemptionAndLossesAnswers

  object IncompleteExemptionAndLossesAnswers {

    val empty: IncompleteExemptionAndLossesAnswers =
      IncompleteExemptionAndLossesAnswers(None, None, None)

    def fromCompleteAnswers(
      c: CompleteExemptionAndLossesAnswers
    ): IncompleteExemptionAndLossesAnswers =
      IncompleteExemptionAndLossesAnswers(
        Some(c.inYearLosses),
        Some(c.previousYearsLosses),
        Some(c.annualExemptAmount)
      )

  }

  final case class CompleteExemptionAndLossesAnswers(
    inYearLosses: AmountInPence,
    previousYearsLosses: AmountInPence,
    annualExemptAmount: AmountInPence
  ) extends ExemptionAndLossesAnswers

  implicit class ExemptionAndLossesAnswersOps(
    private val a: ExemptionAndLossesAnswers
  ) extends AnyVal {

    def fold[A](
      ifIncomplete: IncompleteExemptionAndLossesAnswers => A,
      ifComplete: CompleteExemptionAndLossesAnswers => A
    ): A =
      a match {
        case i: IncompleteExemptionAndLossesAnswers => ifIncomplete(i)
        case c: CompleteExemptionAndLossesAnswers   => ifComplete(c)
      }

    def unset[A](
      fieldLens: IncompleteExemptionAndLossesAnswers.type => Lens[IncompleteExemptionAndLossesAnswers, Option[A]]
    ): IncompleteExemptionAndLossesAnswers =
      fieldLens(IncompleteExemptionAndLossesAnswers).set(None)(
        fold(identity, IncompleteExemptionAndLossesAnswers.fromCompleteAnswers)
      )

    def isEmpty: Boolean = a match {
      case IncompleteExemptionAndLossesAnswers(None, None, None) => true
      case _                                                     => false
    }

  }

  implicit val eq: Eq[ExemptionAndLossesAnswers] = Eq.fromUniversalEquals

  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  implicit val format: OFormat[ExemptionAndLossesAnswers] = derived.oformat()

}
