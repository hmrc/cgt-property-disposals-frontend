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

import cats.instances.list._
import cats.syntax.eq._
import julienrf.json.derived
import monocle.Lens
import monocle.macros.Lenses
import play.api.libs.json.OFormat
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.TaxYear
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Country

sealed trait MultipleDisposalsTriageAnswers

@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
object MultipleDisposalsTriageAnswers {

  @Lenses
  final case class IncompleteMultipleDisposalsAnswers(
    individualUserType: Option[IndividualUserType],
    numberOfProperties: Option[Int],
    wasAUKResident: Option[Boolean],
    countryOfResidence: Option[Country],
    wereAllPropertiesResidential: Option[Boolean],
    assetTypes: Option[List[AssetType]],
    taxYearAfter6April2020: Option[Boolean],
    taxYear: Option[TaxYear],
    completionDate: Option[CompletionDate]
  ) extends MultipleDisposalsTriageAnswers

  object IncompleteMultipleDisposalsAnswers {
    val empty: IncompleteMultipleDisposalsAnswers =
      IncompleteMultipleDisposalsAnswers(None, None, None, None, None, None, None, None, None)

    def fromCompleteAnswers(c: CompleteMultipleDisposalsAnswers): IncompleteMultipleDisposalsAnswers =
      IncompleteMultipleDisposalsAnswers(
        c.individualUserType,
        Some(c.numberOfProperties),
        Some(c.countryOfResidence.isUk()),
        if (c.countryOfResidence.isUk()) None else Some(c.countryOfResidence),
        if (c.countryOfResidence.isUk()) Some(c.assetTypes === List(AssetType.Residential)) else None,
        Some(c.assetTypes),
        Some(true),
        Some(c.taxYear),
        Some(c.completionDate)
      )

  }

  final case class CompleteMultipleDisposalsAnswers(
    individualUserType: Option[IndividualUserType],
    numberOfProperties: Int,
    countryOfResidence: Country,
    assetTypes: List[AssetType],
    taxYear: TaxYear,
    completionDate: CompletionDate
  ) extends MultipleDisposalsTriageAnswers

  implicit class MultipleDisposalsTriageAnswersOps(private val m: MultipleDisposalsTriageAnswers) extends AnyVal {
    def fold[A](
      ifIncomplete: IncompleteMultipleDisposalsAnswers => A,
      ifComplete: CompleteMultipleDisposalsAnswers => A
    ): A = m match {
      case i: IncompleteMultipleDisposalsAnswers => ifIncomplete(i)
      case c: CompleteMultipleDisposalsAnswers   => ifComplete(c)
    }

    def unset[A](
      fieldLens: Lens[IncompleteMultipleDisposalsAnswers, Option[A]]
    ): IncompleteMultipleDisposalsAnswers =
      fieldLens.set(None)(
        fold(identity, IncompleteMultipleDisposalsAnswers.fromCompleteAnswers)
      )
  }

  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  implicit val format: OFormat[MultipleDisposalsTriageAnswers] = derived.oformat()
}
