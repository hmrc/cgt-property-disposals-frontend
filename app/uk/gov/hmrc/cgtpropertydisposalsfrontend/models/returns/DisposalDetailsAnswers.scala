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

import julienrf.json.derived
import monocle.Lens
import monocle.macros.Lenses
import play.api.libs.json.OFormat
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.AmountInPence

sealed trait DisposalDetailsAnswers extends Product with Serializable

@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
object DisposalDetailsAnswers {

  @Lenses
  final case class IncompleteDisposalDetailsAnswers(
    shareOfProperty: Option[ShareOfProperty],
    disposalPrice: Option[AmountInPence],
    disposalFees: Option[AmountInPence]
  ) extends DisposalDetailsAnswers

  object IncompleteDisposalDetailsAnswers {
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
      fieldLens(IncompleteDisposalDetailsAnswers).set(None)(
        fold(identity, IncompleteDisposalDetailsAnswers.fromCompleteAnswers)
      )

  }
  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  implicit val format: OFormat[DisposalDetailsAnswers] =
    derived.oformat[DisposalDetailsAnswers]()

}
