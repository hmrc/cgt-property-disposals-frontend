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

import cats.instances.list.*
import cats.syntax.eq.*
import monocle.Lens
import monocle.macros.GenLens
import play.api.libs.json._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.TaxYear
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Country
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.AssetType.IndirectDisposal
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.IndividualUserType.PersonalRepresentativeInPeriodOfAdmin

sealed trait MultipleDisposalsTriageAnswers

@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
object MultipleDisposalsTriageAnswers {

  final case class IncompleteMultipleDisposalsTriageAnswers(
    individualUserType: Option[IndividualUserType],
    numberOfProperties: Option[Int],
    wasAUKResident: Option[Boolean],
    countryOfResidence: Option[Country],
    wereAllPropertiesResidential: Option[Boolean],
    assetTypes: Option[List[AssetType]],
    taxYearExchanged: Option[TaxYearExchanged],
    taxYear: Option[TaxYear],
    alreadySentSelfAssessment: Option[Boolean],
    completionDate: Option[CompletionDate]
  ) extends MultipleDisposalsTriageAnswers

  object IncompleteMultipleDisposalsTriageAnswers {
    val taxYear                      = GenLens[IncompleteMultipleDisposalsTriageAnswers](_.taxYear)
    val assetTypes                   = GenLens[IncompleteMultipleDisposalsTriageAnswers](_.assetTypes)
    val wasAUKResident               = GenLens[IncompleteMultipleDisposalsTriageAnswers](_.wasAUKResident)
    val completionDate               = GenLens[IncompleteMultipleDisposalsTriageAnswers](_.completionDate)
    val countryOfResidence           = GenLens[IncompleteMultipleDisposalsTriageAnswers](_.countryOfResidence)
    val numberOfProperties           = GenLens[IncompleteMultipleDisposalsTriageAnswers](_.numberOfProperties)
    val individualUserType           = GenLens[IncompleteMultipleDisposalsTriageAnswers](_.individualUserType)
    val taxYearExchanged             = GenLens[IncompleteMultipleDisposalsTriageAnswers](_.taxYearExchanged)
    val alreadySentSelfAssessment    = GenLens[IncompleteMultipleDisposalsTriageAnswers](_.alreadySentSelfAssessment)
    val wereAllPropertiesResidential =
      GenLens[IncompleteMultipleDisposalsTriageAnswers](_.wereAllPropertiesResidential)

    val empty: IncompleteMultipleDisposalsTriageAnswers =
      IncompleteMultipleDisposalsTriageAnswers(
        None,
        None,
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
      c: CompleteMultipleDisposalsTriageAnswers
    ): IncompleteMultipleDisposalsTriageAnswers =
      IncompleteMultipleDisposalsTriageAnswers(
        c.individualUserType,
        Some(c.numberOfProperties),
        Some(c.countryOfResidence.isUk),
        if (c.countryOfResidence.isUk) None else Some(c.countryOfResidence),
        if (c.countryOfResidence.isUk) Some(c.assetTypes === List(AssetType.Residential)) else None,
        Some(c.assetTypes),
        c.taxYearExchanged,
        Some(c.taxYear),
        c.alreadySentSelfAssessment,
        Some(c.completionDate)
      )
  }

  final case class CompleteMultipleDisposalsTriageAnswers(
    individualUserType: Option[IndividualUserType],
    numberOfProperties: Int,
    countryOfResidence: Country,
    assetTypes: List[AssetType],
    taxYearExchanged: Option[TaxYearExchanged],
    taxYear: TaxYear,
    alreadySentSelfAssessment: Option[Boolean],
    completionDate: CompletionDate
  ) extends MultipleDisposalsTriageAnswers

  implicit class MultipleDisposalsTriageAnswersOps(
    private val m: MultipleDisposalsTriageAnswers
  ) extends AnyVal {
    def fold[A](
      ifIncomplete: IncompleteMultipleDisposalsTriageAnswers => A,
      ifComplete: CompleteMultipleDisposalsTriageAnswers => A
    ): A =
      m match {
        case i: IncompleteMultipleDisposalsTriageAnswers => ifIncomplete(i)
        case c: CompleteMultipleDisposalsTriageAnswers   => ifComplete(c)
      }

    def unset[A](
      fieldLens: IncompleteMultipleDisposalsTriageAnswers.type => Lens[
        IncompleteMultipleDisposalsTriageAnswers,
        Option[A]
      ]
    ): IncompleteMultipleDisposalsTriageAnswers =
      fieldLens(IncompleteMultipleDisposalsTriageAnswers).replace(None)(
        fold(
          identity,
          IncompleteMultipleDisposalsTriageAnswers.fromCompleteAnswers
        )
      )

    def representativeType(): Option[RepresentativeType] =
      m.fold[Option[IndividualUserType]](
        _.individualUserType,
        _.individualUserType
      ) match {
        case Some(r: RepresentativeType) => Some(r)
        case _                           => None
      }

    def isIndirectDisposal: Boolean =
      m.fold[Option[List[AssetType]]](
        _.assetTypes,
        c => Some(c.assetTypes)
      ).exists {
        case List(IndirectDisposal) => true
        case _                      => false
      }

    def isPeriodOfAdmin: Boolean = representativeType().contains(PersonalRepresentativeInPeriodOfAdmin)

  }

  implicit val completeMultipleDisposalsTriageAnswersFormat: OFormat[CompleteMultipleDisposalsTriageAnswers]     =
    Json.format[CompleteMultipleDisposalsTriageAnswers]
  implicit val incompleteMultipleDisposalsTriageAnswersFormat: OFormat[IncompleteMultipleDisposalsTriageAnswers] =
    Json.format[IncompleteMultipleDisposalsTriageAnswers]

  implicit val format: OFormat[MultipleDisposalsTriageAnswers] = new OFormat[MultipleDisposalsTriageAnswers] {
    override def reads(json: JsValue): JsResult[MultipleDisposalsTriageAnswers] = json match {
      case JsObject(fields) if fields.size == 1 =>
        fields.head match {
          case ("IncompleteMultipleDisposalsTriageAnswers", value) =>
            value.validate[IncompleteMultipleDisposalsTriageAnswers]
          case ("CompleteMultipleDisposalsTriageAnswers", value)   =>
            value.validate[CompleteMultipleDisposalsTriageAnswers]
          case (other, _)                                          =>
            JsError(s"Unrecognized MultipleDisposalsTriageAnswers type: $other")
        }
      case _                                    =>
        JsError("Expected wrapper object with one MultipleDisposalsTriageAnswers entry")
    }

    override def writes(o: MultipleDisposalsTriageAnswers): JsObject = o match {
      case i: IncompleteMultipleDisposalsTriageAnswers =>
        Json.obj("IncompleteMultipleDisposalsTriageAnswers" -> Json.toJson(i))
      case c: CompleteMultipleDisposalsTriageAnswers   =>
        Json.obj("CompleteMultipleDisposalsTriageAnswers" -> Json.toJson(c))
    }
  }
}
