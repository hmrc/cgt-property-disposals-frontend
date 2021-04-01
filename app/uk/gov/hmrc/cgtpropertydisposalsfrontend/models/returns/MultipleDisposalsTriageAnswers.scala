/*
 * Copyright 2021 HM Revenue & Customs
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
import cats.instances.list._
import julienrf.json.derived
import monocle.Lens
import monocle.macros.Lenses
import play.api.libs.json.OFormat
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.TaxYear
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Country
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.AssetType.IndirectDisposal
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.IndividualUserType.PersonalRepresentativeInPeriodOfAdmin

sealed trait MultipleDisposalsTriageAnswers

@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
object MultipleDisposalsTriageAnswers {

  @Lenses
  final case class IncompleteMultipleDisposalsTriageAnswers(
    individualUserType: Option[IndividualUserType],
    numberOfProperties: Option[Int],
    wasAUKResident: Option[Boolean],
    countryOfResidence: Option[Country],
    wereAllPropertiesResidential: Option[Boolean],
    assetTypes: Option[List[AssetType]],
    taxYearExchanged: Option[TaxYearExchanged],
    taxYear: Option[TaxYear],
    completionDate: Option[CompletionDate]
  ) extends MultipleDisposalsTriageAnswers

  object IncompleteMultipleDisposalsTriageAnswers {
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
        None
      )

    def fromCompleteAnswers(
      c: CompleteMultipleDisposalsTriageAnswers
    ): IncompleteMultipleDisposalsTriageAnswers =
      IncompleteMultipleDisposalsTriageAnswers(
        c.individualUserType,
        Some(c.numberOfProperties),
        Some(c.countryOfResidence.isUk()),
        if (c.countryOfResidence.isUk()) None else Some(c.countryOfResidence),
        if (c.countryOfResidence.isUk())
          Some(c.assetTypes === List(AssetType.Residential))
        else None,
        Some(c.assetTypes),
        c.taxYearExchanged,
        Some(c.taxYear),
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
        Option[
          A
        ]
      ]
    ): IncompleteMultipleDisposalsTriageAnswers =
      fieldLens(IncompleteMultipleDisposalsTriageAnswers).set(None)(
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

    def isIndirectDisposal(): Boolean =
      m.fold[Option[List[AssetType]]](
        _.assetTypes,
        c => Some(c.assetTypes)
      ).exists {
        case List(IndirectDisposal) => true
        case _                      => false
      }

    def isPeriodOfAdmin(): Boolean = representativeType().contains(PersonalRepresentativeInPeriodOfAdmin)

  }

  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  implicit val format: OFormat[MultipleDisposalsTriageAnswers] =
    derived.oformat()
}
