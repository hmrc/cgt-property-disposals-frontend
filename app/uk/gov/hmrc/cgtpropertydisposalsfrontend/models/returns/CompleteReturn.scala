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

import cats.syntax.eq._
import com.github.ghik.silencer.silent
import julienrf.json.derived
import play.api.libs.json.{Json, OFormat}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.EitherUtils.eitherFormat
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address.UkAddress
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.AmountInPence
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.AcquisitionDetailsAnswers.CompleteAcquisitionDetailsAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.DisposalDetailsAnswers.CompleteDisposalDetailsAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ExampleCompanyDetailsAnswers.CompleteExampleCompanyDetailsAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ExamplePropertyDetailsAnswers.CompleteExamplePropertyDetailsAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ExemptionAndLossesAnswers.CompleteExemptionAndLossesAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.IndividualUserType.{Capacitor, PersonalRepresentative, PersonalRepresentativeInPeriodOfAdmin}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.MixedUsePropertyDetailsAnswers.CompleteMixedUsePropertyDetailsAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.MultipleDisposalsTriageAnswers.CompleteMultipleDisposalsTriageAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ReliefDetailsAnswers.CompleteReliefDetailsAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.RepresenteeAnswers.CompleteRepresenteeAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.CompleteSingleDisposalTriageAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SupportingEvidenceAnswers.CompleteSupportingEvidenceAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.YearToDateLiabilityAnswers.CalculatedYTDAnswers.CompleteCalculatedYTDAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.YearToDateLiabilityAnswers.NonCalculatedYTDAnswers.CompleteNonCalculatedYTDAnswers

sealed trait CompleteReturn

object CompleteReturn {

  final case class CompleteMultipleDisposalsReturn(
    triageAnswers: CompleteMultipleDisposalsTriageAnswers,
    examplePropertyDetailsAnswers: CompleteExamplePropertyDetailsAnswers,
    exemptionAndLossesAnswers: CompleteExemptionAndLossesAnswers,
    yearToDateLiabilityAnswers: CompleteNonCalculatedYTDAnswers,
    supportingDocumentAnswers: CompleteSupportingEvidenceAnswers,
    representeeAnswers: Option[CompleteRepresenteeAnswers],
    hasAttachments: Boolean
  ) extends CompleteReturn

  object CompleteMultipleDisposalsReturn {

    def fromDraftReturn(
      draftReturn: DraftMultipleDisposalsReturn
    ): Option[CompleteMultipleDisposalsReturn] =
      draftReturn match {
        case DraftMultipleDisposalsReturn(
              _,
              t: CompleteMultipleDisposalsTriageAnswers,
              Some(p: CompleteExamplePropertyDetailsAnswers),
              Some(e: CompleteExemptionAndLossesAnswers),
              Some(y: CompleteNonCalculatedYTDAnswers),
              Some(u: CompleteSupportingEvidenceAnswers),
              representeeAnswers,
              _
            ) =>
          validRepresenteeAnswers(t.individualUserType, representeeAnswers).map(maybeCompleteRepresenteeAnswers =>
            CompleteMultipleDisposalsReturn(
              t,
              p,
              e,
              y,
              u,
              maybeCompleteRepresenteeAnswers,
              hasAttachments = true
            )
          )

        case _ =>
          None

      }

  }

  final case class CompleteSingleDisposalReturn(
    triageAnswers: CompleteSingleDisposalTriageAnswers,
    propertyAddress: UkAddress,
    disposalDetails: CompleteDisposalDetailsAnswers,
    acquisitionDetails: CompleteAcquisitionDetailsAnswers,
    reliefDetails: CompleteReliefDetailsAnswers,
    exemptionsAndLossesDetails: CompleteExemptionAndLossesAnswers,
    yearToDateLiabilityAnswers: Either[
      CompleteNonCalculatedYTDAnswers,
      CompleteCalculatedYTDAnswers
    ],
    supportingDocumentAnswers: CompleteSupportingEvidenceAnswers,
    initialGainOrLoss: Option[AmountInPence],
    representeeAnswers: Option[CompleteRepresenteeAnswers],
    hasAttachments: Boolean
  ) extends CompleteReturn

  object CompleteSingleDisposalReturn {

    def fromDraftReturn(
      draftReturn: DraftSingleDisposalReturn
    ): Option[CompleteSingleDisposalReturn] =
      draftReturn match {
        case DraftSingleDisposalReturn(
              _,
              t: CompleteSingleDisposalTriageAnswers,
              Some(p: UkAddress),
              Some(d: CompleteDisposalDetailsAnswers),
              Some(a: CompleteAcquisitionDetailsAnswers),
              Some(r: CompleteReliefDetailsAnswers),
              Some(e: CompleteExemptionAndLossesAnswers),
              Some(y: CompleteCalculatedYTDAnswers),
              i,
              Some(u: CompleteSupportingEvidenceAnswers),
              representeeAnswers,
              _
            ) =>
          val hasAttachments =
            u.evidences.nonEmpty || y.mandatoryEvidence.isDefined
          validRepresenteeAnswers(t.individualUserType, representeeAnswers).map(maybeCompleteRepresenteeAnswers =>
            CompleteSingleDisposalReturn(
              t,
              p,
              d,
              a,
              r,
              e,
              Right(y),
              u,
              i,
              maybeCompleteRepresenteeAnswers,
              hasAttachments
            )
          )

        case DraftSingleDisposalReturn(
              _,
              t: CompleteSingleDisposalTriageAnswers,
              Some(p: UkAddress),
              Some(d: CompleteDisposalDetailsAnswers),
              Some(a: CompleteAcquisitionDetailsAnswers),
              Some(r: CompleteReliefDetailsAnswers),
              Some(e: CompleteExemptionAndLossesAnswers),
              Some(y: CompleteNonCalculatedYTDAnswers),
              i,
              Some(u: CompleteSupportingEvidenceAnswers),
              representeeAnswers,
              _
            ) =>
          validRepresenteeAnswers(t.individualUserType, representeeAnswers).map(maybeCompleteRepresenteeAnswers =>
            CompleteSingleDisposalReturn(
              t,
              p,
              d,
              a,
              r,
              e,
              Left(y),
              u,
              i,
              maybeCompleteRepresenteeAnswers,
              hasAttachments = true
            )
          )

        case _ =>
          None
      }

    implicit class CompleteSingleDisposalReturnOps(
      private val c: CompleteSingleDisposalReturn
    ) extends AnyVal {

      def hasNonResidentialAssetType(): Boolean =
        c.triageAnswers.assetType === AssetType.NonResidential

    }

  }

  final case class CompleteSingleIndirectDisposalReturn(
    triageAnswers: CompleteSingleDisposalTriageAnswers,
    companyAddress: Address,
    disposalDetails: CompleteDisposalDetailsAnswers,
    acquisitionDetails: CompleteAcquisitionDetailsAnswers,
    exemptionsAndLossesDetails: CompleteExemptionAndLossesAnswers,
    yearToDateLiabilityAnswers: CompleteNonCalculatedYTDAnswers,
    supportingDocumentAnswers: CompleteSupportingEvidenceAnswers,
    representeeAnswers: Option[CompleteRepresenteeAnswers],
    hasAttachments: Boolean
  ) extends CompleteReturn

  object CompleteSingleIndirectDisposalReturn {

    def fromDraftReturn(
      draftReturn: DraftSingleIndirectDisposalReturn
    ): Option[CompleteSingleIndirectDisposalReturn] =
      draftReturn match {
        case DraftSingleIndirectDisposalReturn(
              _,
              t: CompleteSingleDisposalTriageAnswers,
              Some(c: Address),
              Some(d: CompleteDisposalDetailsAnswers),
              Some(a: CompleteAcquisitionDetailsAnswers),
              Some(e: CompleteExemptionAndLossesAnswers),
              Some(y: CompleteNonCalculatedYTDAnswers),
              Some(u: CompleteSupportingEvidenceAnswers),
              representeeAnswers,
              _
            ) =>
          validRepresenteeAnswers(t.individualUserType, representeeAnswers).map(maybeCompleteRepresenteeAnswers =>
            CompleteSingleIndirectDisposalReturn(
              t,
              c,
              d,
              a,
              e,
              y,
              u,
              maybeCompleteRepresenteeAnswers,
              hasAttachments = true
            )
          )

        case _ =>
          None
      }

  }

  final case class CompleteMultipleIndirectDisposalReturn(
    triageAnswers: CompleteMultipleDisposalsTriageAnswers,
    exampleCompanyDetailsAnswers: CompleteExampleCompanyDetailsAnswers,
    exemptionsAndLossesDetails: CompleteExemptionAndLossesAnswers,
    yearToDateLiabilityAnswers: CompleteNonCalculatedYTDAnswers,
    supportingDocumentAnswers: CompleteSupportingEvidenceAnswers,
    representeeAnswers: Option[CompleteRepresenteeAnswers],
    hasAttachments: Boolean
  ) extends CompleteReturn

  object CompleteMultipleIndirectDisposalReturn {

    def fromDraftReturn(
      draftReturn: DraftMultipleIndirectDisposalsReturn
    ): Option[CompleteMultipleIndirectDisposalReturn] =
      draftReturn match {
        case DraftMultipleIndirectDisposalsReturn(
              _,
              t: CompleteMultipleDisposalsTriageAnswers,
              Some(c: CompleteExampleCompanyDetailsAnswers),
              Some(e: CompleteExemptionAndLossesAnswers),
              Some(y: CompleteNonCalculatedYTDAnswers),
              Some(u: CompleteSupportingEvidenceAnswers),
              representeeAnswers,
              _
            ) =>
          validRepresenteeAnswers(t.individualUserType, representeeAnswers).map(maybeCompleteRepresenteeAnswers =>
            CompleteMultipleIndirectDisposalReturn(
              t,
              c,
              e,
              y,
              u,
              maybeCompleteRepresenteeAnswers,
              hasAttachments = true
            )
          )

        case _ =>
          None
      }

  }

  final case class CompleteSingleMixedUseDisposalReturn(
    triageAnswers: CompleteSingleDisposalTriageAnswers,
    propertyDetailsAnswers: CompleteMixedUsePropertyDetailsAnswers,
    exemptionsAndLossesDetails: CompleteExemptionAndLossesAnswers,
    yearToDateLiabilityAnswers: CompleteNonCalculatedYTDAnswers,
    supportingDocumentAnswers: CompleteSupportingEvidenceAnswers,
    representeeAnswers: Option[CompleteRepresenteeAnswers],
    hasAttachments: Boolean
  ) extends CompleteReturn

  object CompleteSingleMixedUseDisposalReturn {

    def fromDraftReturn(
      draftReturn: DraftSingleMixedUseDisposalReturn
    ): Option[CompleteSingleMixedUseDisposalReturn] =
      draftReturn match {
        case DraftSingleMixedUseDisposalReturn(
              _,
              t: CompleteSingleDisposalTriageAnswers,
              Some(c: CompleteMixedUsePropertyDetailsAnswers),
              Some(e: CompleteExemptionAndLossesAnswers),
              Some(y: CompleteNonCalculatedYTDAnswers),
              Some(u: CompleteSupportingEvidenceAnswers),
              representeeAnswers,
              _
            ) =>
          validRepresenteeAnswers(t.individualUserType, representeeAnswers).map(maybeCompleteRepresenteeAnswers =>
            CompleteSingleMixedUseDisposalReturn(
              t,
              c,
              e,
              y,
              u,
              maybeCompleteRepresenteeAnswers,
              hasAttachments = true
            )
          )

        case _ =>
          None
      }

  }

  implicit class CompleteReturnOps(private val c: CompleteReturn) extends AnyVal {
    def fold[A](
      whenMultiple: CompleteMultipleDisposalsReturn => A,
      whenSingle: CompleteSingleDisposalReturn => A,
      whenSingleIndirect: CompleteSingleIndirectDisposalReturn => A,
      whenMultipleIndirect: CompleteMultipleIndirectDisposalReturn => A,
      whenSingleMixedUse: CompleteSingleMixedUseDisposalReturn => A
    ): A =
      c match {
        case m: CompleteMultipleDisposalsReturn        => whenMultiple(m)
        case s: CompleteSingleDisposalReturn           => whenSingle(s)
        case s: CompleteSingleIndirectDisposalReturn   => whenSingleIndirect(s)
        case m: CompleteMultipleIndirectDisposalReturn => whenMultipleIndirect(m)
        case s: CompleteSingleMixedUseDisposalReturn   => whenSingleMixedUse(s)
      }

    def isIndirectDisposal(): Boolean =
      c.fold[Boolean](
        m => m.triageAnswers.isIndirectDisposal(),
        _ => false,
        _ => true,
        _ => true,
        _ => false
      )

    def representativeType(): Option[RepresentativeType] =
      c.fold(
        _.triageAnswers.representativeType(),
        _.triageAnswers.representativeType(),
        _.triageAnswers.representativeType(),
        _.triageAnswers.representativeType(),
        _.triageAnswers.representativeType()
      )

  }

  private def validRepresenteeAnswers(
    individualUserType: Option[IndividualUserType],
    representeeAnswers: Option[RepresenteeAnswers]
  ): Option[Option[CompleteRepresenteeAnswers]] =
    (individualUserType, representeeAnswers) match {
      case (
            Some(_: RepresentativeType),
            Some(r: CompleteRepresenteeAnswers)
          ) =>
        Some(Some(r))

      case (Some(_: RepresentativeType), _) =>
        None

      case _                                                                                                 =>
        Some(None)
    }

  @silent
  implicit val format: OFormat[CompleteReturn] = {
    implicit val singleDisposalTriageFormat: OFormat[CompleteSingleDisposalTriageAnswers]              = Json.format
    implicit val multipleDisposalsTriageFormat: OFormat[CompleteMultipleDisposalsTriageAnswers]        = Json.format
    implicit val ukAddressFormat: OFormat[UkAddress]                                                   = Json.format
    implicit val examplePropertyDetailsFormat: OFormat[CompleteExamplePropertyDetailsAnswers]          = Json.format
    implicit val disposalDetailsFormat: OFormat[CompleteDisposalDetailsAnswers]                        = Json.format
    implicit val acquisitionDetailsFormat: OFormat[CompleteAcquisitionDetailsAnswers]                  = Json.format
    implicit val reliefDetailsFormat: OFormat[CompleteReliefDetailsAnswers]                            =
      Json.format
    implicit val exemptionAndLossesFormat: OFormat[CompleteExemptionAndLossesAnswers]                  = Json.format
    implicit val nonCalculatedYearToDateLiabilityFormat: OFormat[CompleteNonCalculatedYTDAnswers]      = Json.format
    implicit val calculatedYearToDateLiabilityFormat: OFormat[CompleteCalculatedYTDAnswers]            = Json.format
    implicit val supportingDocumentsFormat: OFormat[CompleteSupportingEvidenceAnswers]                 = Json.format
    implicit val representeeAnswersFormat: OFormat[CompleteRepresenteeAnswers]                         =
      Json.format
    implicit val exampleCompanyDetailsAnswersFormat: OFormat[CompleteExampleCompanyDetailsAnswers]     = Json.format
    implicit val mixedUsePropertyDetailsAnswersFormat: OFormat[CompleteMixedUsePropertyDetailsAnswers] = Json.format
    derived.oformat()
  }

}
