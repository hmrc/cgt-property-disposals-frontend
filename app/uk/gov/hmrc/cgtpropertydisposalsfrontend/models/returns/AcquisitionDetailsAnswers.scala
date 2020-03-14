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
import play.api.libs.json.OFormat
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.AmountInPence

sealed trait AcquisitionDetailsAnswers extends Product with Serializable

@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
object AcquisitionDetailsAnswers {

  @Lenses
  final case class IncompleteAcquisitionDetailsAnswers(
    acquisitionMethod: Option[AcquisitionMethod],
    acquisitionDate: Option[AcquisitionDate],
    acquisitionPrice: Option[AmountInPence],
    rebasedAcquisitionPrice: Option[AmountInPence],
    improvementCosts: Option[AmountInPence],
    acquisitionFees: Option[AmountInPence]
  ) extends AcquisitionDetailsAnswers

  object IncompleteAcquisitionDetailsAnswers {

    val empty: IncompleteAcquisitionDetailsAnswers =
      IncompleteAcquisitionDetailsAnswers(None, None, None, None, None, None)

    def fromCompleteAnswers(c: CompleteAcquisitionDetailsAnswers): IncompleteAcquisitionDetailsAnswers =
      IncompleteAcquisitionDetailsAnswers(
        Some(c.acquisitionMethod),
        Some(c.acquisitionDate),
        Some(c.acquisitionPrice),
        c.rebasedAcquisitionPrice,
        Some(c.improvementCosts),
        Some(c.acquisitionFees)
      )

  }

  final case class CompleteAcquisitionDetailsAnswers(
    acquisitionMethod: AcquisitionMethod,
    acquisitionDate: AcquisitionDate,
    acquisitionPrice: AmountInPence,
    rebasedAcquisitionPrice: Option[AmountInPence],
    improvementCosts: AmountInPence,
    acquisitionFees: AmountInPence
  ) extends AcquisitionDetailsAnswers

  implicit class AcquisitionDetailsAnswersOps(private val a: AcquisitionDetailsAnswers) extends AnyVal {

    def fold[A](
      ifIncomplete: IncompleteAcquisitionDetailsAnswers => A,
      ifComplete: CompleteAcquisitionDetailsAnswers => A
    ): A = a match {
      case i: IncompleteAcquisitionDetailsAnswers => ifIncomplete(i)
      case c: CompleteAcquisitionDetailsAnswers   => ifComplete(c)
    }

    def unset[A](
      fieldLens: Lens[IncompleteAcquisitionDetailsAnswers, Option[A]]
    ): IncompleteAcquisitionDetailsAnswers =
      fieldLens.set(None)(
        fold(identity, IncompleteAcquisitionDetailsAnswers.fromCompleteAnswers)
      )

  }

  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  implicit val format: OFormat[AcquisitionDetailsAnswers] = derived.oformat()

}
