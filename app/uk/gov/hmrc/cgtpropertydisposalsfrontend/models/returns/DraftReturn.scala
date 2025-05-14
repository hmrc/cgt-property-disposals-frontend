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

import cats.Eq
import play.api.libs.json._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.TimeUtils
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address.UkAddress
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.AmountInPence

import java.time.LocalDate
import java.util.UUID

sealed trait DraftReturn extends Product with Serializable {
  val id: UUID
  val lastUpdatedDate: LocalDate
  val gainOrLossAfterReliefs: Option[AmountInPence]
  val yearToDateLiabilityAnswers: Option[YearToDateLiabilityAnswers]
  val representeeAnswers: Option[RepresenteeAnswers]
  val exemptionAndLossesAnswers: Option[ExemptionAndLossesAnswers]
}

final case class DraftSingleDisposalReturn(
  id: UUID,
  triageAnswers: SingleDisposalTriageAnswers,
  propertyAddress: Option[UkAddress],
  disposalDetailsAnswers: Option[DisposalDetailsAnswers],
  acquisitionDetailsAnswers: Option[AcquisitionDetailsAnswers],
  reliefDetailsAnswers: Option[ReliefDetailsAnswers],
  exemptionAndLossesAnswers: Option[ExemptionAndLossesAnswers],
  yearToDateLiabilityAnswers: Option[YearToDateLiabilityAnswers],
  initialGainOrLoss: Option[AmountInPence],
  supportingEvidenceAnswers: Option[SupportingEvidenceAnswers],
  representeeAnswers: Option[RepresenteeAnswers],
  gainOrLossAfterReliefs: Option[AmountInPence],
  lastUpdatedDate: LocalDate
) extends DraftReturn

object DraftSingleDisposalReturn {

  implicit val eq: Eq[DraftSingleDisposalReturn] =
    Eq.fromUniversalEquals[DraftSingleDisposalReturn]

  def newDraftReturn(
    id: UUID,
    triageAnswers: SingleDisposalTriageAnswers,
    representeeAnswers: Option[RepresenteeAnswers]
  ): DraftSingleDisposalReturn =
    DraftSingleDisposalReturn(
      id,
      triageAnswers,
      None,
      None,
      None,
      None,
      None,
      None,
      None,
      None,
      representeeAnswers,
      None,
      TimeUtils.today()
    )

}

final case class DraftMultipleDisposalsReturn(
  id: UUID,
  triageAnswers: MultipleDisposalsTriageAnswers,
  examplePropertyDetailsAnswers: Option[ExamplePropertyDetailsAnswers],
  exemptionAndLossesAnswers: Option[ExemptionAndLossesAnswers],
  yearToDateLiabilityAnswers: Option[YearToDateLiabilityAnswers],
  supportingEvidenceAnswers: Option[SupportingEvidenceAnswers],
  representeeAnswers: Option[RepresenteeAnswers],
  gainOrLossAfterReliefs: Option[AmountInPence],
  lastUpdatedDate: LocalDate
) extends DraftReturn

object DraftMultipleDisposalsReturn {

  def newDraftReturn(
    id: UUID,
    triageAnswers: MultipleDisposalsTriageAnswers,
    representeeAnswers: Option[RepresenteeAnswers]
  ): DraftMultipleDisposalsReturn =
    DraftMultipleDisposalsReturn(
      id,
      triageAnswers,
      None,
      None,
      None,
      None,
      representeeAnswers,
      None,
      TimeUtils.today()
    )

}

final case class DraftSingleIndirectDisposalReturn(
  id: UUID,
  triageAnswers: SingleDisposalTriageAnswers,
  companyAddress: Option[Address],
  disposalDetailsAnswers: Option[DisposalDetailsAnswers],
  acquisitionDetailsAnswers: Option[AcquisitionDetailsAnswers],
  exemptionAndLossesAnswers: Option[ExemptionAndLossesAnswers],
  yearToDateLiabilityAnswers: Option[YearToDateLiabilityAnswers],
  supportingEvidenceAnswers: Option[SupportingEvidenceAnswers],
  representeeAnswers: Option[RepresenteeAnswers],
  gainOrLossAfterReliefs: Option[AmountInPence],
  lastUpdatedDate: LocalDate
) extends DraftReturn

object DraftSingleIndirectDisposalReturn {

  def newDraftReturn(
    id: UUID,
    triageAnswers: SingleDisposalTriageAnswers,
    representeeAnswers: Option[RepresenteeAnswers]
  ): DraftSingleIndirectDisposalReturn =
    DraftSingleIndirectDisposalReturn(
      id,
      triageAnswers,
      None,
      None,
      None,
      None,
      None,
      None,
      representeeAnswers,
      None,
      TimeUtils.today()
    )

}

final case class DraftMultipleIndirectDisposalsReturn(
  id: UUID,
  triageAnswers: MultipleDisposalsTriageAnswers,
  exampleCompanyDetailsAnswers: Option[ExampleCompanyDetailsAnswers],
  exemptionAndLossesAnswers: Option[ExemptionAndLossesAnswers],
  yearToDateLiabilityAnswers: Option[YearToDateLiabilityAnswers],
  supportingEvidenceAnswers: Option[SupportingEvidenceAnswers],
  representeeAnswers: Option[RepresenteeAnswers],
  gainOrLossAfterReliefs: Option[AmountInPence],
  lastUpdatedDate: LocalDate
) extends DraftReturn

object DraftMultipleIndirectDisposalsReturn {

  def newDraftReturn(
    id: UUID,
    triageAnswers: MultipleDisposalsTriageAnswers,
    representeeAnswers: Option[RepresenteeAnswers]
  ): DraftMultipleIndirectDisposalsReturn =
    DraftMultipleIndirectDisposalsReturn(
      id,
      triageAnswers,
      None,
      None,
      None,
      None,
      representeeAnswers,
      None,
      TimeUtils.today()
    )

}

final case class DraftSingleMixedUseDisposalReturn(
  id: UUID,
  triageAnswers: SingleDisposalTriageAnswers,
  mixedUsePropertyDetailsAnswers: Option[MixedUsePropertyDetailsAnswers],
  exemptionAndLossesAnswers: Option[ExemptionAndLossesAnswers],
  yearToDateLiabilityAnswers: Option[YearToDateLiabilityAnswers],
  supportingEvidenceAnswers: Option[SupportingEvidenceAnswers],
  representeeAnswers: Option[RepresenteeAnswers],
  gainOrLossAfterReliefs: Option[AmountInPence],
  lastUpdatedDate: LocalDate
) extends DraftReturn

object DraftSingleMixedUseDisposalReturn {

  def newDraftReturn(
    id: UUID,
    triageAnswers: SingleDisposalTriageAnswers,
    representeeAnswers: Option[RepresenteeAnswers]
  ): DraftSingleMixedUseDisposalReturn =
    DraftSingleMixedUseDisposalReturn(
      id,
      triageAnswers,
      None,
      None,
      None,
      None,
      representeeAnswers,
      None,
      TimeUtils.today()
    )

}

object DraftReturn {

  implicit class DraftReturnOps(private val d: DraftReturn) extends AnyVal {
    def fold[A](
      whenMultiple: DraftMultipleDisposalsReturn => A,
      whenSingle: DraftSingleDisposalReturn => A,
      whenSingleIndirect: DraftSingleIndirectDisposalReturn => A,
      whenMultipleIndirect: DraftMultipleIndirectDisposalsReturn => A,
      whenSingleMixedUse: DraftSingleMixedUseDisposalReturn => A
    ): A =
      d match {
        case m: DraftMultipleDisposalsReturn         => whenMultiple(m)
        case s: DraftSingleDisposalReturn            => whenSingle(s)
        case s: DraftSingleIndirectDisposalReturn    => whenSingleIndirect(s)
        case m: DraftMultipleIndirectDisposalsReturn => whenMultipleIndirect(m)
        case s: DraftSingleMixedUseDisposalReturn    => whenSingleMixedUse(s)
      }

    def isMultipleIndirectDisposal: Boolean =
      fold(_ => false, _ => false, _ => false, _ => true, _ => false)

    def triageAnswers(): Either[MultipleDisposalsTriageAnswers, SingleDisposalTriageAnswers] =
      fold(
        multiple => Left(multiple.triageAnswers),
        single => Right(single.triageAnswers),
        singleIndirect => Right(singleIndirect.triageAnswers),
        multipleIndirect => Left(multipleIndirect.triageAnswers),
        singleMixedUse => Right(singleMixedUse.triageAnswers)
      )

    def representativeType(): Option[RepresentativeType] =
      triageAnswers().fold(_.representativeType(), _.representativeType())

  }

  implicit val eq: Eq[DraftReturn] = Eq.fromUniversalEquals

  implicit val ukAddressFormat: OFormat[UkAddress] = Json.format[UkAddress]

  implicit val singleReturnFormat: OFormat[DraftSingleDisposalReturn]                      = Json.format[DraftSingleDisposalReturn]
  implicit val singleIndirectReturnFormat: OFormat[DraftSingleIndirectDisposalReturn]      =
    Json.format[DraftSingleIndirectDisposalReturn]
  implicit val singleMixedReturnFormat: OFormat[DraftSingleMixedUseDisposalReturn]         =
    Json.format[DraftSingleMixedUseDisposalReturn]
  implicit val multipleReturnFormat: OFormat[DraftMultipleDisposalsReturn]                 = Json.format[DraftMultipleDisposalsReturn]
  implicit val multipleIndirectReturnFormat: OFormat[DraftMultipleIndirectDisposalsReturn] =
    Json.format[DraftMultipleIndirectDisposalsReturn]

  implicit val format: OFormat[DraftReturn] = new OFormat[DraftReturn] {
    override def reads(json: JsValue): JsResult[DraftReturn] = json match {
      case JsObject(fields) if fields.size == 1 =>
        fields.head match {
          case ("DraftSingleDisposalReturn", value)            =>
            value.validate[DraftSingleDisposalReturn]
          case ("DraftMultipleDisposalsReturn", value)         =>
            value.validate[DraftMultipleDisposalsReturn]
          case ("DraftSingleIndirectDisposalReturn", value)    =>
            value.validate[DraftSingleIndirectDisposalReturn]
          case ("DraftMultipleIndirectDisposalsReturn", value) =>
            value.validate[DraftMultipleIndirectDisposalsReturn]
          case ("DraftSingleMixedUseDisposalReturn", value)    =>
            value.validate[DraftSingleMixedUseDisposalReturn]
          case (other, _)                                      =>
            JsError(s"Unrecognized DraftReturn type: $other")
        }
      case _                                    =>
        JsError("Expected a DraftReturn wrapper object with a single entry")
    }

    override def writes(d: DraftReturn): JsObject = d match {
      case s: DraftSingleDisposalReturn            =>
        Json.obj("DraftSingleDisposalReturn" -> Json.toJson(s))
      case m: DraftMultipleDisposalsReturn         =>
        Json.obj("DraftMultipleDisposalsReturn" -> Json.toJson(m))
      case s: DraftSingleIndirectDisposalReturn    =>
        Json.obj("DraftSingleIndirectDisposalReturn" -> Json.toJson(s))
      case m: DraftMultipleIndirectDisposalsReturn =>
        Json.obj("DraftMultipleIndirectDisposalsReturn" -> Json.toJson(m))
      case s: DraftSingleMixedUseDisposalReturn    =>
        Json.obj("DraftSingleMixedUseDisposalReturn" -> Json.toJson(s))
    }
  }

}
