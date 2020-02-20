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

import cats.Eq
import julienrf.json.derived
import play.api.libs.json.{Json, OFormat}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Country

sealed trait TriageAnswers extends Product with Serializable

object TriageAnswers {

  final case class IncompleteTriageAnswers(
    individualUserType: Option[IndividualUserType],
    numberOfProperties: Option[NumberOfProperties],
    disposalMethod: Option[DisposalMethod],
    wasAUKResident: Option[Boolean],
    countryOfResidence: Option[Country],
    assetType: Option[AssetType],
    disposalDate: Option[DisposalDate],
    completionDate: Option[CompletionDate]
  ) extends TriageAnswers

  object IncompleteTriageAnswers {
    val empty: IncompleteTriageAnswers =
      IncompleteTriageAnswers(None, None, None, None, None, None, None, None)

    implicit val format: OFormat[IncompleteTriageAnswers] = Json.format
  }
  final case class CompleteTriageAnswers(
    individualUserType: IndividualUserType,
    numberOfProperties: NumberOfProperties,
    disposalMethod: DisposalMethod,
    countryOfResidence: Country,
    assetType: AssetType,
    disposalDate: DisposalDate,
    completionDate: CompletionDate
  ) extends TriageAnswers

  object CompleteTriageAnswers {

    implicit val format: OFormat[CompleteTriageAnswers] = Json.format
  }

  implicit class IndividualTriageQuestionOps(val i: TriageAnswers) extends AnyVal {

    def fold[A](
      ifIncomplete: IncompleteTriageAnswers => A,
      ifComplete: CompleteTriageAnswers => A
    ): A = i match {
      case incomplete: IncompleteTriageAnswers => ifIncomplete(incomplete)
      case complete: CompleteTriageAnswers     => ifComplete(complete)
    }

  }

  implicit val eq: Eq[IncompleteTriageAnswers] = Eq.fromUniversalEquals

  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  implicit val format: OFormat[TriageAnswers] = derived.oformat()

}
