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

import java.time.LocalDate

import cats.Eq
import julienrf.json.derived
import monocle.Lens
import monocle.macros.Lenses
import play.api.libs.json.{Json, OFormat}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Country

sealed trait SingleDisposalTriageAnswers extends Product with Serializable

@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
object SingleDisposalTriageAnswers {

  @Lenses
  final case class IncompleteSingleDisposalTriageAnswers(
    individualUserType: Option[IndividualUserType],
    hasConfirmedSingleDisposal: Boolean,
    disposalMethod: Option[DisposalMethod],
    wasAUKResident: Option[Boolean],
    countryOfResidence: Option[Country],
    assetType: Option[AssetType],
    disposalDate: Option[DisposalDate],
    completionDate: Option[CompletionDate],
    tooEarlyDisposalDate: Option[LocalDate]
  ) extends SingleDisposalTriageAnswers

  object IncompleteSingleDisposalTriageAnswers {
    val empty: IncompleteSingleDisposalTriageAnswers =
      IncompleteSingleDisposalTriageAnswers(None, false, None, None, None, None, None, None, None)

    def fromCompleteAnswers(c: CompleteSingleDisposalTriageAnswers): IncompleteSingleDisposalTriageAnswers =
      IncompleteSingleDisposalTriageAnswers(
        c.individualUserType,
        true,
        Some(c.disposalMethod),
        Some(c.countryOfResidence.isUk()),
        if (c.countryOfResidence.isUk()) None else Some(c.countryOfResidence),
        Some(c.assetType),
        Some(c.disposalDate),
        Some(c.completionDate),
        None
      )

    implicit val format: OFormat[IncompleteSingleDisposalTriageAnswers] = Json.format
  }

  final case class CompleteSingleDisposalTriageAnswers(
    individualUserType: Option[IndividualUserType],
    disposalMethod: DisposalMethod,
    countryOfResidence: Country,
    assetType: AssetType,
    disposalDate: DisposalDate,
    completionDate: CompletionDate
  ) extends SingleDisposalTriageAnswers

  implicit class IndividualTriageQuestionOps(private val i: SingleDisposalTriageAnswers) extends AnyVal {

    def fold[A](
      ifIncomplete: IncompleteSingleDisposalTriageAnswers => A,
      ifComplete: CompleteSingleDisposalTriageAnswers => A
    ): A = i match {
      case incomplete: IncompleteSingleDisposalTriageAnswers => ifIncomplete(incomplete)
      case complete: CompleteSingleDisposalTriageAnswers     => ifComplete(complete)
    }

    def unset[A](
      fieldLens: IncompleteSingleDisposalTriageAnswers.type => Lens[IncompleteSingleDisposalTriageAnswers, Option[A]]
    ): IncompleteSingleDisposalTriageAnswers =
      fieldLens(IncompleteSingleDisposalTriageAnswers).set(None)(
        fold(identity, IncompleteSingleDisposalTriageAnswers.fromCompleteAnswers)
      )

  }

  implicit val eq: Eq[IncompleteSingleDisposalTriageAnswers] = Eq.fromUniversalEquals

  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  implicit val format: OFormat[SingleDisposalTriageAnswers] = derived.oformat()

}
