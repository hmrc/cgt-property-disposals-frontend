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

  sealed trait CalculatedYTDAnswers extends YearToDateLiabilityAnswers

  sealed trait NonCalculatedYTDAnswers extends YearToDateLiabilityAnswers

  object NonCalculatedYTDAnswers {

    final case class IncompleteNonCalculatedYTDAnswers(
      taxableGainOrLoss: Option[AmountInPence],
      hasEstimatedDetails: Option[Boolean],
      taxDue: Option[AmountInPence]
    ) extends NonCalculatedYTDAnswers

    object IncompleteNonCalculatedYTDAnswers {
      val empty: IncompleteNonCalculatedYTDAnswers =
        IncompleteNonCalculatedYTDAnswers(None, None, None)
    }

    final case class CompleteNonCalculatedYTDAnswers(
      taxableGainOrLoss: AmountInPence,
      hasEstimatedDetails: Boolean,
      taxDue: AmountInPence
    ) extends NonCalculatedYTDAnswers

    implicit class NonCalculatedYTDLiabilityAnswersOps(private val a: NonCalculatedYTDAnswers) extends AnyVal {

      def fold[A](
        ifIncomplete: IncompleteNonCalculatedYTDAnswers => A,
        ifComplete: CompleteNonCalculatedYTDAnswers => A
      ): A = a match {
        case i: IncompleteNonCalculatedYTDAnswers => ifIncomplete(i)
        case c: CompleteNonCalculatedYTDAnswers   => ifComplete(c)
      }
    }

  }

  object CalculatedYTDAnswers {
    final case class IncompleteCalculatedYTDAnswers(
      estimatedIncome: Option[AmountInPence],
      personalAllowance: Option[AmountInPence],
      hasEstimatedDetails: Option[Boolean],
      calculatedTaxDue: Option[CalculatedTaxDue],
      taxDue: Option[AmountInPence],
      mandatoryEvidence: Option[String]
    ) extends CalculatedYTDAnswers

    object IncompleteCalculatedYTDAnswers {
      val empty: IncompleteCalculatedYTDAnswers =
        IncompleteCalculatedYTDAnswers(None, None, None, None, None, None)
    }

    final case class CompleteCalculatedYTDAnswers(
      estimatedIncome: AmountInPence,
      personalAllowance: Option[AmountInPence],
      hasEstimatedDetails: Boolean,
      calculatedTaxDue: CalculatedTaxDue,
      taxDue: AmountInPence,
      mandatoryEvidence: Option[String]
    ) extends CalculatedYTDAnswers

    implicit class CalculatedYTDLiabilityAnswersOps(private val a: CalculatedYTDAnswers) extends AnyVal {

      def fold[A](
        ifIncomplete: IncompleteCalculatedYTDAnswers => A,
        ifComplete: CompleteCalculatedYTDAnswers => A
      ): A = a match {
        case i: IncompleteCalculatedYTDAnswers => ifIncomplete(i)
        case c: CompleteCalculatedYTDAnswers   => ifComplete(c)
      }
    }

  }

  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  implicit val format: OFormat[YearToDateLiabilityAnswers] = derived.oformat()

}
