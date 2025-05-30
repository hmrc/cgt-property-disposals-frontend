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
import monocle.Lens
import monocle.macros.GenLens
import play.api.libs.json.{Json, OFormat}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Country
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.IndividualUserType.PersonalRepresentativeInPeriodOfAdmin

import java.time.LocalDate

sealed trait SingleDisposalTriageAnswers extends Product with Serializable

@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
object SingleDisposalTriageAnswers {

  final case class IncompleteSingleDisposalTriageAnswers(
    individualUserType: Option[IndividualUserType],
    hasConfirmedSingleDisposal: Boolean,
    disposalMethod: Option[DisposalMethod],
    wasAUKResident: Option[Boolean],
    countryOfResidence: Option[Country],
    assetType: Option[AssetType],
    disposalDate: Option[DisposalDate],
    alreadySentSelfAssessment: Option[Boolean],
    completionDate: Option[CompletionDate],
    tooEarlyDisposalDate: Option[LocalDate]
  ) extends SingleDisposalTriageAnswers

  object IncompleteSingleDisposalTriageAnswers {
    val assetType                 = GenLens[IncompleteSingleDisposalTriageAnswers](_.assetType)
    val disposalDate              = GenLens[IncompleteSingleDisposalTriageAnswers](_.disposalDate)
    val completionDate            = GenLens[IncompleteSingleDisposalTriageAnswers](_.completionDate)
    val wasAUKResident            = GenLens[IncompleteSingleDisposalTriageAnswers](_.wasAUKResident)
    val disposalMethod            = GenLens[IncompleteSingleDisposalTriageAnswers](_.disposalMethod)
    val countryOfResidence        = GenLens[IncompleteSingleDisposalTriageAnswers](_.countryOfResidence)
    val individualUserType        = GenLens[IncompleteSingleDisposalTriageAnswers](_.individualUserType)
    val tooEarlyDisposalDate      = GenLens[IncompleteSingleDisposalTriageAnswers](_.tooEarlyDisposalDate)
    val alreadySentSelfAssessment = GenLens[IncompleteSingleDisposalTriageAnswers](_.alreadySentSelfAssessment)

    val empty: IncompleteSingleDisposalTriageAnswers =
      IncompleteSingleDisposalTriageAnswers(
        None,
        hasConfirmedSingleDisposal = false,
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
      c: CompleteSingleDisposalTriageAnswers
    ): IncompleteSingleDisposalTriageAnswers =
      IncompleteSingleDisposalTriageAnswers(
        c.individualUserType,
        hasConfirmedSingleDisposal = true,
        Some(c.disposalMethod),
        Some(c.countryOfResidence.isUk),
        if (c.countryOfResidence.isUk) None else Some(c.countryOfResidence),
        Some(c.assetType),
        Some(c.disposalDate),
        c.alreadySentSelfAssessment,
        Some(c.completionDate),
        None
      )

    implicit val format: OFormat[IncompleteSingleDisposalTriageAnswers] =
      Json.format
  }

  final case class CompleteSingleDisposalTriageAnswers(
    individualUserType: Option[IndividualUserType],
    disposalMethod: DisposalMethod,
    countryOfResidence: Country,
    assetType: AssetType,
    disposalDate: DisposalDate,
    alreadySentSelfAssessment: Option[Boolean],
    completionDate: CompletionDate
  ) extends SingleDisposalTriageAnswers

  implicit class IndividualTriageQuestionOps(
    private val s: SingleDisposalTriageAnswers
  ) extends AnyVal {

    def fold[A](
      ifIncomplete: IncompleteSingleDisposalTriageAnswers => A,
      ifComplete: CompleteSingleDisposalTriageAnswers => A
    ): A =
      s match {
        case incomplete: IncompleteSingleDisposalTriageAnswers =>
          ifIncomplete(incomplete)
        case complete: CompleteSingleDisposalTriageAnswers     =>
          ifComplete(complete)
      }

    def unset[A](
      fieldLens: IncompleteSingleDisposalTriageAnswers.type => Lens[IncompleteSingleDisposalTriageAnswers, Option[A]]
    ): IncompleteSingleDisposalTriageAnswers =
      fieldLens(IncompleteSingleDisposalTriageAnswers).replace(None)(
        fold(
          identity,
          IncompleteSingleDisposalTriageAnswers.fromCompleteAnswers
        )
      )

    def representativeType(): Option[RepresentativeType] =
      s.fold[Option[IndividualUserType]](
        _.individualUserType,
        _.individualUserType
      ) match {
        case Some(r: RepresentativeType) => Some(r)
        case _                           => None
      }

    def isPeriodOfAdmin: Boolean = representativeType().contains(PersonalRepresentativeInPeriodOfAdmin)

    def isIndirectDisposal: Boolean =
      s.fold(
        _.assetType,
        c => Some(c.assetType)
      ).exists {
        case AssetType.IndirectDisposal => true
        case _                          => false
      }

  }

  implicit val eq: Eq[IncompleteSingleDisposalTriageAnswers] =
    Eq.fromUniversalEquals

  implicit val completeFormat: OFormat[CompleteSingleDisposalTriageAnswers]     =
    Json.format[CompleteSingleDisposalTriageAnswers]
  implicit val inCompleteFormat: OFormat[IncompleteSingleDisposalTriageAnswers] =
    Json.format[IncompleteSingleDisposalTriageAnswers]

  implicit val format: OFormat[SingleDisposalTriageAnswers] = new OFormat[SingleDisposalTriageAnswers] {
    import play.api.libs.json._
    override def reads(json: JsValue): JsResult[SingleDisposalTriageAnswers] = json match {
      case JsObject(fields) if fields.size == 1 =>
        fields.head match {
          case ("IncompleteSingleDisposalTriageAnswers", value) =>
            value.validate[IncompleteSingleDisposalTriageAnswers]
          case ("CompleteSingleDisposalTriageAnswers", value)   =>
            value.validate[CompleteSingleDisposalTriageAnswers]
          case (other, _)                                       =>
            JsError(s"Unrecognized SingleDisposalTriageAnswers type: $other")
        }
      case _                                    =>
        JsError("Expected SingleDisposalTriageAnswers wrapper object with a single entry")
    }

    override def writes(o: SingleDisposalTriageAnswers): JsObject = o match {
      case i: IncompleteSingleDisposalTriageAnswers =>
        Json.obj("IncompleteSingleDisposalTriageAnswers" -> Json.toJson(i))
      case c: CompleteSingleDisposalTriageAnswers   =>
        Json.obj("CompleteSingleDisposalTriageAnswers" -> Json.toJson(c))
    }
  }

}
