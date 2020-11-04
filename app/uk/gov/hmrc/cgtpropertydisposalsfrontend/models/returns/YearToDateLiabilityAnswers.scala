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
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.upscan.UpscanUpload

sealed trait YearToDateLiabilityAnswers extends Product with Serializable

@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
object YearToDateLiabilityAnswers {

  sealed trait CalculatedYTDAnswers extends YearToDateLiabilityAnswers

  sealed trait NonCalculatedYTDAnswers extends YearToDateLiabilityAnswers

  object NonCalculatedYTDAnswers {

    @Lenses
    final case class IncompleteNonCalculatedYTDAnswers(
      taxableGainOrLoss: Option[AmountInPence],
      hasEstimatedDetails: Option[Boolean],
      taxDue: Option[AmountInPence],
      mandatoryEvidence: Option[MandatoryEvidence],
      expiredEvidence: Option[MandatoryEvidence],
      pendingUpscanUpload: Option[UpscanUpload],
      yearToDateLiability: Option[AmountInPence],
      checkForRepayment: Option[Boolean],
      estimatedIncome: Option[AmountInPence],
      personalAllowance: Option[AmountInPence]
    ) extends NonCalculatedYTDAnswers

    object IncompleteNonCalculatedYTDAnswers {
      val empty: IncompleteNonCalculatedYTDAnswers =
        IncompleteNonCalculatedYTDAnswers(None, None, None, None, None, None, None, None, None, None)

      def fromCompleteAnswers(
        c: CompleteNonCalculatedYTDAnswers
      ): IncompleteNonCalculatedYTDAnswers =
        IncompleteNonCalculatedYTDAnswers(
          Some(c.taxableGainOrLoss),
          Some(c.hasEstimatedDetails),
          Some(c.taxDue),
          c.mandatoryEvidence,
          None,
          None,
          c.yearToDateLiability,
          c.checkForRepayment,
          c.estimatedIncome,
          c.personalAllowance
        )
    }

    final case class CompleteNonCalculatedYTDAnswers(
      taxableGainOrLoss: AmountInPence,
      hasEstimatedDetails: Boolean,
      taxDue: AmountInPence,
      mandatoryEvidence: Option[MandatoryEvidence],
      yearToDateLiability: Option[AmountInPence],
      checkForRepayment: Option[Boolean],
      estimatedIncome: Option[AmountInPence],
      personalAllowance: Option[AmountInPence]
    ) extends NonCalculatedYTDAnswers

    implicit class NonCalculatedYTDLiabilityAnswersOps(
      private val a: NonCalculatedYTDAnswers
    ) extends AnyVal {

      def fold[A](
        ifIncomplete: IncompleteNonCalculatedYTDAnswers => A,
        ifComplete: CompleteNonCalculatedYTDAnswers => A
      ): A =
        a match {
          case i: IncompleteNonCalculatedYTDAnswers => ifIncomplete(i)
          case c: CompleteNonCalculatedYTDAnswers   => ifComplete(c)
        }

      def unset[A](
        fieldLens: IncompleteNonCalculatedYTDAnswers.type => Lens[
          IncompleteNonCalculatedYTDAnswers,
          Option[A]
        ]
      ): IncompleteNonCalculatedYTDAnswers =
        fieldLens(IncompleteNonCalculatedYTDAnswers).set(None)(
          fold(identity, IncompleteNonCalculatedYTDAnswers.fromCompleteAnswers)
        )
    }

  }

  object CalculatedYTDAnswers {

    @Lenses
    final case class IncompleteCalculatedYTDAnswers(
      estimatedIncome: Option[AmountInPence],
      personalAllowance: Option[AmountInPence],
      hasEstimatedDetails: Option[Boolean],
      calculatedTaxDue: Option[CalculatedTaxDue],
      taxDue: Option[AmountInPence],
      mandatoryEvidence: Option[MandatoryEvidence],
      expiredEvidence: Option[MandatoryEvidence],
      pendingUpscanUpload: Option[UpscanUpload]
    ) extends CalculatedYTDAnswers

    object IncompleteCalculatedYTDAnswers {
      val empty: IncompleteCalculatedYTDAnswers =
        IncompleteCalculatedYTDAnswers(
          None,
          None,
          None,
          None,
          None,
          None,
          None,
          None
        )

      def fromCompleteAnswers(
        c: CompleteCalculatedYTDAnswers
      ): IncompleteCalculatedYTDAnswers =
        IncompleteCalculatedYTDAnswers(
          Some(c.estimatedIncome),
          c.personalAllowance,
          Some(c.hasEstimatedDetails),
          Some(c.calculatedTaxDue),
          Some(c.taxDue),
          c.mandatoryEvidence,
          None,
          None
        )

    }

    final case class CompleteCalculatedYTDAnswers(
      estimatedIncome: AmountInPence,
      personalAllowance: Option[AmountInPence],
      hasEstimatedDetails: Boolean,
      calculatedTaxDue: CalculatedTaxDue,
      taxDue: AmountInPence,
      mandatoryEvidence: Option[MandatoryEvidence]
    ) extends CalculatedYTDAnswers

    implicit class CalculatedYTDLiabilityAnswersOps(
      private val a: CalculatedYTDAnswers
    ) extends AnyVal {

      def fold[A](
        ifIncomplete: IncompleteCalculatedYTDAnswers => A,
        ifComplete: CompleteCalculatedYTDAnswers => A
      ): A =
        a match {
          case i: IncompleteCalculatedYTDAnswers => ifIncomplete(i)
          case c: CompleteCalculatedYTDAnswers   => ifComplete(c)
        }

      def unset[A](
        fieldLens: IncompleteCalculatedYTDAnswers.type => Lens[
          IncompleteCalculatedYTDAnswers,
          Option[A]
        ]
      ): IncompleteCalculatedYTDAnswers =
        fieldLens(IncompleteCalculatedYTDAnswers).set(None)(
          fold(identity, IncompleteCalculatedYTDAnswers.fromCompleteAnswers)
        )

    }

  }

  implicit class YearToDateLiabilityAnswersOps(
    private val y: YearToDateLiabilityAnswers
  ) extends AnyVal {

    def unsetAllButIncomeDetails(): Option[YearToDateLiabilityAnswers] =
      y match {
        case c: CalculatedYTDAnswers    =>
          Some(
            c.unset(_.hasEstimatedDetails)
              .unset(_.calculatedTaxDue)
              .unset(_.taxDue)
              .unset(_.mandatoryEvidence)
              .unset(_.expiredEvidence)
              .unset(_.pendingUpscanUpload)
          )
        case _: NonCalculatedYTDAnswers => None
      }

  }

  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  implicit val format: OFormat[YearToDateLiabilityAnswers] = derived.oformat()

}
