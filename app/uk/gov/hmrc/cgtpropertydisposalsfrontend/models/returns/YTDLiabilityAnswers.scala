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

sealed trait YTDLiabilityAnswers extends Product with Serializable

object YTDLiabilityAnswers {

  final case class IncompleteYTDLiabilityAnswers(
    estimatedIncome: Option[AmountInPence]
  ) extends YTDLiabilityAnswers

  object IncompleteYTDLiabilityAnswers {
    val empty: IncompleteYTDLiabilityAnswers =
      IncompleteYTDLiabilityAnswers(None)
  }

  final case class CompleteYTDLiabilityAnswers(
    estimatedIncome: AmountInPence
  ) extends YTDLiabilityAnswers

  implicit class YTDLiabilityAnswersOps(private val a: YTDLiabilityAnswers) extends AnyVal {

    def fold[A](
      ifIncomplete: IncompleteYTDLiabilityAnswers => A,
      ifComplete: CompleteYTDLiabilityAnswers => A
    ): A = a match {
      case i: IncompleteYTDLiabilityAnswers => ifIncomplete(i)
      case c: CompleteYTDLiabilityAnswers   => ifComplete(c)
    }

  }

  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  implicit val format: OFormat[YTDLiabilityAnswers] = derived.oformat()

}
