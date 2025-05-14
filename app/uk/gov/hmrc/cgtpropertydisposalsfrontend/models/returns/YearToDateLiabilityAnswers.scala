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

import monocle.Lens
import monocle.macros.GenLens
import play.api.libs.json._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.AmountInPence
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.upscan.UpscanUpload

sealed trait YearToDateLiabilityAnswers extends Product with Serializable

@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
object YearToDateLiabilityAnswers {

  sealed trait CalculatedYTDAnswers extends YearToDateLiabilityAnswers

  sealed trait NonCalculatedYTDAnswers extends YearToDateLiabilityAnswers

  import CalculatedYTDAnswers.*
  import NonCalculatedYTDAnswers.*

  object NonCalculatedYTDAnswers {

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
      personalAllowance: Option[AmountInPence],
      taxableGainOrLossCalculation: Option[TaxableGainOrLossCalculation],
      yearToDateLiabilityCalculation: Option[YearToDateLiabilityCalculation]
    ) extends NonCalculatedYTDAnswers

    object IncompleteNonCalculatedYTDAnswers {
      val taxDue                         = GenLens[IncompleteNonCalculatedYTDAnswers](_.taxDue)
      val expiredEvidence                = GenLens[IncompleteNonCalculatedYTDAnswers](_.expiredEvidence)
      val checkForRepayment              = GenLens[IncompleteNonCalculatedYTDAnswers](_.checkForRepayment)
      val mandatoryEvidence              = GenLens[IncompleteNonCalculatedYTDAnswers](_.mandatoryEvidence)
      val taxableGainOrLoss              = GenLens[IncompleteNonCalculatedYTDAnswers](_.taxableGainOrLoss)
      val yearToDateLiability            = GenLens[IncompleteNonCalculatedYTDAnswers](_.yearToDateLiability)
      val pendingUpscanUpload            = GenLens[IncompleteNonCalculatedYTDAnswers](_.pendingUpscanUpload)
      val hasEstimatedDetails            = GenLens[IncompleteNonCalculatedYTDAnswers](_.hasEstimatedDetails)
      val yearToDateLiabilityCalculation = GenLens[IncompleteNonCalculatedYTDAnswers](_.yearToDateLiabilityCalculation)

      val empty: IncompleteNonCalculatedYTDAnswers =
        IncompleteNonCalculatedYTDAnswers(None, None, None, None, None, None, None, None, None, None, None, None)

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
          c.personalAllowance,
          c.taxableGainOrLossCalculation,
          c.yearToDateLiabilityCalculation
        )

      implicit val format: OFormat[IncompleteNonCalculatedYTDAnswers] = Json.format[IncompleteNonCalculatedYTDAnswers]
    }

    final case class CompleteNonCalculatedYTDAnswers(
      taxableGainOrLoss: AmountInPence,
      hasEstimatedDetails: Boolean,
      taxDue: AmountInPence,
      mandatoryEvidence: Option[MandatoryEvidence],
      yearToDateLiability: Option[AmountInPence],
      checkForRepayment: Option[Boolean],
      estimatedIncome: Option[AmountInPence],
      personalAllowance: Option[AmountInPence],
      taxableGainOrLossCalculation: Option[TaxableGainOrLossCalculation],
      yearToDateLiabilityCalculation: Option[YearToDateLiabilityCalculation]
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
        fieldLens: IncompleteNonCalculatedYTDAnswers.type => Lens[IncompleteNonCalculatedYTDAnswers, Option[A]]
      ): IncompleteNonCalculatedYTDAnswers =
        fieldLens(IncompleteNonCalculatedYTDAnswers).replace(None)(
          fold(identity, IncompleteNonCalculatedYTDAnswers.fromCompleteAnswers)
        )
    }

  }

  object CalculatedYTDAnswers {

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
      val taxDue              = GenLens[IncompleteCalculatedYTDAnswers](_.taxDue)
      val calculatedTaxDue    = GenLens[IncompleteCalculatedYTDAnswers](_.calculatedTaxDue)
      val estimatedIncome     = GenLens[IncompleteCalculatedYTDAnswers](_.estimatedIncome)
      val expiredEvidence     = GenLens[IncompleteCalculatedYTDAnswers](_.expiredEvidence)
      val mandatoryEvidence   = GenLens[IncompleteCalculatedYTDAnswers](_.mandatoryEvidence)
      val personalAllowance   = GenLens[IncompleteCalculatedYTDAnswers](_.personalAllowance)
      val hasEstimatedDetails = GenLens[IncompleteCalculatedYTDAnswers](_.hasEstimatedDetails)
      val pendingUpscanUpload = GenLens[IncompleteCalculatedYTDAnswers](_.pendingUpscanUpload)

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
        fieldLens: IncompleteCalculatedYTDAnswers.type => Lens[IncompleteCalculatedYTDAnswers, Option[A]]
      ): IncompleteCalculatedYTDAnswers =
        fieldLens(IncompleteCalculatedYTDAnswers).replace(None)(
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

  implicit val completeCalculatedFormat: OFormat[CompleteCalculatedYTDAnswers]     =
    Json.format[CompleteCalculatedYTDAnswers]
  implicit val inCompleteCalculatedFormat: OFormat[IncompleteCalculatedYTDAnswers] =
    Json.format[IncompleteCalculatedYTDAnswers]
  implicit val calculatedFormat: OFormat[CalculatedYTDAnswers]                     = Json.format[CalculatedYTDAnswers]

  implicit val completeNonCalculatedFormat: OFormat[CompleteNonCalculatedYTDAnswers]     =
    Json.format[CompleteNonCalculatedYTDAnswers]
  implicit val inCompleteNonCalculatedFormat: OFormat[IncompleteNonCalculatedYTDAnswers] =
    Json.format[IncompleteNonCalculatedYTDAnswers]
  implicit val nonCalculatedFormat: OFormat[NonCalculatedYTDAnswers]                     = Json.format[NonCalculatedYTDAnswers]

  implicit val format: OFormat[YearToDateLiabilityAnswers] = new OFormat[YearToDateLiabilityAnswers] {
    override def reads(json: JsValue): JsResult[YearToDateLiabilityAnswers] = json match {
      case JsObject(fields) if fields.size == 1 =>
        fields.head match {
          case ("CompleteCalculatedYTDAnswers", value)      => value.validate[CompleteCalculatedYTDAnswers]
          case ("IncompleteCalculatedYTDAnswers", value)    => value.validate[IncompleteCalculatedYTDAnswers]
          case ("CompleteNonCalculatedYTDAnswers", value)   => value.validate[CompleteNonCalculatedYTDAnswers]
          case ("IncompleteNonCalculatedYTDAnswers", value) => value.validate[IncompleteNonCalculatedYTDAnswers]
          case (other, _)                                   => JsError(s"Unknown YearToDateLiabilityAnswers subtype: $other")
        }
      case _                                    =>
        JsError("Expected wrapper object with one YearToDateLiabilityAnswers subtype key")
    }

    override def writes(o: YearToDateLiabilityAnswers): JsObject = o match {
      case c: CompleteCalculatedYTDAnswers      => Json.obj("CompleteCalculatedYTDAnswers" -> Json.toJson(c))
      case i: IncompleteCalculatedYTDAnswers    => Json.obj("IncompleteCalculatedYTDAnswers" -> Json.toJson(i))
      case c: CompleteNonCalculatedYTDAnswers   => Json.obj("CompleteNonCalculatedYTDAnswers" -> Json.toJson(c))
      case i: IncompleteNonCalculatedYTDAnswers => Json.obj("IncompleteNonCalculatedYTDAnswers" -> Json.toJson(i))
    }
  }

}
