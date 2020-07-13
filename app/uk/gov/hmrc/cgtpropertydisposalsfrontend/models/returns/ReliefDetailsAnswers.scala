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

import cats.Eq
import julienrf.json.derived
import monocle.Lens
import monocle.macros.Lenses
import play.api.libs.json.OFormat
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.AmountInPence

sealed trait ReliefDetailsAnswers extends Product with Serializable

@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
object ReliefDetailsAnswers {

  @Lenses
  final case class IncompleteReliefDetailsAnswers(
    privateResidentsRelief: Option[AmountInPence],
    lettingsRelief: Option[AmountInPence],
    otherReliefs: Option[OtherReliefsOption]
  ) extends ReliefDetailsAnswers

  object IncompleteReliefDetailsAnswers {
    val empty: IncompleteReliefDetailsAnswers =
      IncompleteReliefDetailsAnswers(None, None, None)

    def fromCompleteAnswers(
      c: CompleteReliefDetailsAnswers
    ): IncompleteReliefDetailsAnswers =
      IncompleteReliefDetailsAnswers(
        Some(c.privateResidentsRelief),
        Some(c.lettingsRelief),
        c.otherReliefs
      )
  }

  final case class CompleteReliefDetailsAnswers(
    privateResidentsRelief: AmountInPence,
    lettingsRelief: AmountInPence,
    otherReliefs: Option[OtherReliefsOption]
  ) extends ReliefDetailsAnswers

  implicit class ReliefDetailsAnswersOps(private val a: ReliefDetailsAnswers) extends AnyVal {

    def fold[A](
      ifIncomplete: IncompleteReliefDetailsAnswers => A,
      ifComplete: CompleteReliefDetailsAnswers => A
    ): A =
      a match {
        case i: IncompleteReliefDetailsAnswers => ifIncomplete(i)
        case c: CompleteReliefDetailsAnswers   => ifComplete(c)
      }

    def unset[A](
      fieldLens: IncompleteReliefDetailsAnswers.type => Lens[
        IncompleteReliefDetailsAnswers,
        Option[A]
      ]
    ): IncompleteReliefDetailsAnswers =
      fieldLens(IncompleteReliefDetailsAnswers).set(None)(
        fold(identity, IncompleteReliefDetailsAnswers.fromCompleteAnswers)
      )

    def unsetPrrAndLettingRelief(isPeriodOfAdmin: Boolean): IncompleteReliefDetailsAnswers =
      if (isPeriodOfAdmin) unset(_.privateResidentsRelief).copy(lettingsRelief = Some(AmountInPence.zero))
      else unset(_.privateResidentsRelief).unset(_.lettingsRelief)

  }

  implicit val eq: Eq[ReliefDetailsAnswers] = Eq.fromUniversalEquals

  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  implicit val format: OFormat[ReliefDetailsAnswers] = derived.oformat()
}
