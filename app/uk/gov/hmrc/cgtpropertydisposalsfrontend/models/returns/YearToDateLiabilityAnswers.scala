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
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.AmountInPence

sealed trait YearToDateLiabilityAnswers extends Product with Serializable

object YearToDateLiabilityAnswers {

  final case class IncompleteYearToDateLiabilityAnswers(
    estimatedIncome: Option[AmountInPence],
    personalAllowance: Option[AmountInPence],
    hasEstimatedDetailsWithCalculatedTaxDue: Option[HasEstimatedDetailsWithCalculatedTaxDue],
    taxDue: Option[AmountInPence],
    mandatoryEvidence: Option[String]
  ) extends YearToDateLiabilityAnswers

  object IncompleteYearToDateLiabilityAnswers {
    val empty: IncompleteYearToDateLiabilityAnswers =
      IncompleteYearToDateLiabilityAnswers(None, None, None, None, None)
  }

  final case class CompleteYearToDateLiabilityAnswers(
    estimatedIncome: AmountInPence,
    personalAllowance: Option[AmountInPence],
    hasEstimatedDetailsWithCalculatedTaxDue: HasEstimatedDetailsWithCalculatedTaxDue,
    taxDue: AmountInPence,
    mandatoryEvidence: Option[String]
  ) extends YearToDateLiabilityAnswers

  implicit class YTDLiabilityAnswersOps(private val a: YearToDateLiabilityAnswers) extends AnyVal {

    def fold[A](
      ifIncomplete: IncompleteYearToDateLiabilityAnswers => A,
      ifComplete: CompleteYearToDateLiabilityAnswers => A
    ): A = a match {
      case i: IncompleteYearToDateLiabilityAnswers => ifIncomplete(i)
      case c: CompleteYearToDateLiabilityAnswers   => ifComplete(c)
    }

  }

  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  implicit val format: OFormat[YearToDateLiabilityAnswers] = derived.oformat()

}
