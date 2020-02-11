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
import play.api.libs.json.OFormat
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.AmountInPence

sealed trait ReliefDetailsAnswers extends Product with Serializable

object ReliefDetailsAnswers {

  final case class IncompleteReliefDetailsAnswers(
    privateResidentsRelief: Option[AmountInPence],
    lettingsRelief: Option[AmountInPence],
    otherReliefs: Option[AmountInPence]
  ) extends ReliefDetailsAnswers

  object IncompleteReliefDetailsAnswers {
    val empty: IncompleteReliefDetailsAnswers =
      IncompleteReliefDetailsAnswers(None, None, None)
  }

  final case class CompleteReliefDetailsAnswers(
    privateResidentsRelief: AmountInPence,
    lettingsRelief: AmountInPence,
    otherReliefs: AmountInPence
  ) extends ReliefDetailsAnswers

  object CompleteReliefDetailsAnswers {}

  implicit class ReliefDetailsAnswersOps(private val a: ReliefDetailsAnswers) extends AnyVal {

    def fold[A](
      ifIncomplete: IncompleteReliefDetailsAnswers => A,
      ifComplete: CompleteReliefDetailsAnswers => A
    ): A = a match {
      case i: IncompleteReliefDetailsAnswers => ifIncomplete(i)
      case c: CompleteReliefDetailsAnswers   => ifComplete(c)
    }

  }

  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  implicit val format: OFormat[ReliefDetailsAnswers] = derived.oformat()
}
