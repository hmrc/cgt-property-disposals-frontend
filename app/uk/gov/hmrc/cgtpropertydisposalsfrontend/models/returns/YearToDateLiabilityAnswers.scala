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

  sealed trait CalculatedYearToDateLiabilityAnswers extends YearToDateLiabilityAnswers

  sealed trait NonCalculatedYearToDateLiabilityAnswers extends YearToDateLiabilityAnswers

  object NonCalculatedYearToDateLiabilityAnswers {

    final case class IncompleteNonCalculatedYearToDateLiabilityAnswers(
      taxableGainOrLoss: Option[AmountInPence],
      hasEstimatedDetails: Option[Boolean],
      taxDue: Option[AmountInPence]
    ) extends NonCalculatedYearToDateLiabilityAnswers

    object IncompleteNonCalculatedYearToDateLiabilityAnswers {
      val empty: IncompleteNonCalculatedYearToDateLiabilityAnswers =
        IncompleteNonCalculatedYearToDateLiabilityAnswers(None, None, None)
    }

    final case class CompleteNonCalculatedYearToDateLiabilityAnswers(
      taxableGainOrLoss: AmountInPence,
      hasEstimatedDetails: Boolean,
      taxDue: AmountInPence
    ) extends NonCalculatedYearToDateLiabilityAnswers

    implicit class NonCalculatedYTDLiabilityAnswersOps(private val a: NonCalculatedYearToDateLiabilityAnswers)
        extends AnyVal {

      def fold[A](
        ifIncomplete: IncompleteNonCalculatedYearToDateLiabilityAnswers => A,
        ifComplete: CompleteNonCalculatedYearToDateLiabilityAnswers => A
      ): A = a match {
        case i: IncompleteNonCalculatedYearToDateLiabilityAnswers => ifIncomplete(i)
        case c: CompleteNonCalculatedYearToDateLiabilityAnswers   => ifComplete(c)
      }
    }

  }

  object CalculatedYearToDateLiabilityAnswers {
    final case class IncompleteCalculatedYearToDateLiabilityAnswers(
      estimatedIncome: Option[AmountInPence],
      personalAllowance: Option[AmountInPence],
      hasEstimatedDetails: Option[Boolean],
      calculatedTaxDue: Option[CalculatedTaxDue],
      taxDue: Option[AmountInPence],
      mandatoryEvidence: Option[String]
    ) extends CalculatedYearToDateLiabilityAnswers

    object IncompleteCalculatedYearToDateLiabilityAnswers {
      val empty: IncompleteCalculatedYearToDateLiabilityAnswers =
        IncompleteCalculatedYearToDateLiabilityAnswers(None, None, None, None, None, None)
    }

    final case class CompleteCalculatedYearToDateLiabilityAnswers(
      estimatedIncome: AmountInPence,
      personalAllowance: Option[AmountInPence],
      hasEstimatedDetails: Boolean,
      calculatedTaxDue: CalculatedTaxDue,
      taxDue: AmountInPence,
      mandatoryEvidence: Option[String]
    ) extends CalculatedYearToDateLiabilityAnswers

    implicit class CalculatedYTDLiabilityAnswersOps(private val a: CalculatedYearToDateLiabilityAnswers)
        extends AnyVal {

      def fold[A](
        ifIncomplete: IncompleteCalculatedYearToDateLiabilityAnswers => A,
        ifComplete: CompleteCalculatedYearToDateLiabilityAnswers => A
      ): A = a match {
        case i: IncompleteCalculatedYearToDateLiabilityAnswers => ifIncomplete(i)
        case c: CompleteCalculatedYearToDateLiabilityAnswers   => ifComplete(c)
      }
    }

  }

  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  implicit val format: OFormat[YearToDateLiabilityAnswers] = derived.oformat()

}
