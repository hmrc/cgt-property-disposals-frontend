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
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.AssetType.IndirectDisposal
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.IndividualUserType.PersonalRepresentativeInPeriodOfAdmin

sealed trait AcquisitionDetailsAnswers extends Product with Serializable

@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
object AcquisitionDetailsAnswers {

  final case class IncompleteAcquisitionDetailsAnswers(
    acquisitionMethod: Option[AcquisitionMethod],
    acquisitionDate: Option[AcquisitionDate],
    acquisitionPrice: Option[AmountInPence],
    rebasedAcquisitionPrice: Option[AmountInPence],
    improvementCosts: Option[AmountInPence],
    acquisitionFees: Option[AmountInPence],
    shouldUseRebase: Option[Boolean]
  ) extends AcquisitionDetailsAnswers

  object IncompleteAcquisitionDetailsAnswers {

    val acquisitionFees         = GenLens[IncompleteAcquisitionDetailsAnswers](_.acquisitionFees)
    val acquisitionMethod       = GenLens[IncompleteAcquisitionDetailsAnswers](_.acquisitionMethod)
    val acquisitionPrice        = GenLens[IncompleteAcquisitionDetailsAnswers](_.acquisitionPrice)
    val acquisitionDate         = GenLens[IncompleteAcquisitionDetailsAnswers](_.acquisitionDate)
    val shouldUseRebase         = GenLens[IncompleteAcquisitionDetailsAnswers](_.shouldUseRebase)
    val improvementCosts        = GenLens[IncompleteAcquisitionDetailsAnswers](_.improvementCosts)
    val rebasedAcquisitionPrice = GenLens[IncompleteAcquisitionDetailsAnswers](_.rebasedAcquisitionPrice)

    val empty: IncompleteAcquisitionDetailsAnswers =
      IncompleteAcquisitionDetailsAnswers(
        None,
        None,
        None,
        None,
        None,
        None,
        None
      )

    def fromCompleteAnswers(
      c: CompleteAcquisitionDetailsAnswers
    ): IncompleteAcquisitionDetailsAnswers =
      IncompleteAcquisitionDetailsAnswers(
        Some(c.acquisitionMethod),
        Some(c.acquisitionDate),
        Some(c.acquisitionPrice),
        c.rebasedAcquisitionPrice,
        Some(c.improvementCosts),
        Some(c.acquisitionFees),
        Some(c.shouldUseRebase)
      )

  }

  final case class CompleteAcquisitionDetailsAnswers(
    acquisitionMethod: AcquisitionMethod,
    acquisitionDate: AcquisitionDate,
    acquisitionPrice: AmountInPence,
    rebasedAcquisitionPrice: Option[AmountInPence],
    improvementCosts: AmountInPence,
    acquisitionFees: AmountInPence,
    shouldUseRebase: Boolean
  ) extends AcquisitionDetailsAnswers

  implicit class AcquisitionDetailsAnswersOps(
    private val a: AcquisitionDetailsAnswers
  ) extends AnyVal {

    def fold[A](
      ifIncomplete: IncompleteAcquisitionDetailsAnswers => A,
      ifComplete: CompleteAcquisitionDetailsAnswers => A
    ): A =
      a match {
        case i: IncompleteAcquisitionDetailsAnswers => ifIncomplete(i)
        case c: CompleteAcquisitionDetailsAnswers   => ifComplete(c)
      }

    def unset[A](
      fieldLens: IncompleteAcquisitionDetailsAnswers.type => Lens[IncompleteAcquisitionDetailsAnswers, Option[A]]
    ): IncompleteAcquisitionDetailsAnswers =
      fieldLens(IncompleteAcquisitionDetailsAnswers).replace(None)(
        fold(identity, IncompleteAcquisitionDetailsAnswers.fromCompleteAnswers)
      )

    def unsetAllButAcquisitionMethod(
      triageAnswers: SingleDisposalTriageAnswers
    ): IncompleteAcquisitionDetailsAnswers = {
      val isPeriodOfAdmin    = triageAnswers
        .fold(_.individualUserType, _.individualUserType)
        .contains(PersonalRepresentativeInPeriodOfAdmin)
      val isIndirectDisposal = triageAnswers
        .fold(_.assetType, e => Some(e.assetType))
        .contains(IndirectDisposal)
      val newAnswers         = a
        .unset(_.acquisitionPrice)
        .unset(_.rebasedAcquisitionPrice)
        .unset(_.shouldUseRebase)
        .unset(_.acquisitionFees)

      if (isPeriodOfAdmin && isIndirectDisposal) {
        newAnswers
      } else if (isPeriodOfAdmin && !isIndirectDisposal) {
        newAnswers.unset(_.improvementCosts)
      } else if (!isPeriodOfAdmin && isIndirectDisposal) {
        newAnswers.unset(_.acquisitionDate)
      } else {
        newAnswers.unset(_.acquisitionDate).unset(_.improvementCosts)
      }
    }

  }

  implicit val completeFormat: OFormat[CompleteAcquisitionDetailsAnswers]     =
    Json.format[CompleteAcquisitionDetailsAnswers]
  implicit val inCompleteFormat: OFormat[IncompleteAcquisitionDetailsAnswers] =
    Json.format[IncompleteAcquisitionDetailsAnswers]

  implicit val format: OFormat[AcquisitionDetailsAnswers] = new OFormat[AcquisitionDetailsAnswers] {
    override def reads(json: JsValue): JsResult[AcquisitionDetailsAnswers] = json match {
      case JsObject(fields) if fields.size == 1 =>
        fields.head match {
          case ("IncompleteAcquisitionDetailsAnswers", value) =>
            value.validate[IncompleteAcquisitionDetailsAnswers]
          case ("CompleteAcquisitionDetailsAnswers", value)   =>
            value.validate[CompleteAcquisitionDetailsAnswers]
          case (other, _)                                     =>
            JsError(s"Unrecognized AcquisitionDetailsAnswers type: $other")
        }
      case _                                    =>
        JsError("Expected AcquisitionDetailsAnswers wrapper object with a single entry")
    }

    override def writes(a: AcquisitionDetailsAnswers): JsObject = a match {
      case i: IncompleteAcquisitionDetailsAnswers =>
        Json.obj("IncompleteAcquisitionDetailsAnswers" -> Json.toJson(i))
      case c: CompleteAcquisitionDetailsAnswers   =>
        Json.obj("CompleteAcquisitionDetailsAnswers" -> Json.toJson(c))
    }
  }

}
