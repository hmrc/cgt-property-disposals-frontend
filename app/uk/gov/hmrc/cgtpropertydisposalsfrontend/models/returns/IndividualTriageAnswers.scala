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

import julienrf.json.derived
import play.api.libs.json.{Json, OFormat}

sealed trait IndividualTriageAnswers extends Product with Serializable

object IndividualTriageAnswers {

  final case class IncompleteIndividualTriageAnswers(
    individualUserType: Option[IndividualUserType],
    numberOfProperties: Option[NumberOfProperties],
    disposalMethod: Option[DisposalMethod],
    wasAUKResident: Option[Boolean],
    wasResidentialProperty: Option[Boolean],
    disposalDate: Option[DisposalDate],
    completionDate: Option[CompletionDate]
  ) extends IndividualTriageAnswers

  object IncompleteIndividualTriageAnswers {
    val empty: IncompleteIndividualTriageAnswers =
      IncompleteIndividualTriageAnswers(None, None, None, None, None, None, None)

    implicit val format: OFormat[IncompleteIndividualTriageAnswers] = Json.format
  }
  final case class CompleteIndividualTriageAnswers(
    individualUserType: IndividualUserType,
    numberOfProperties: NumberOfProperties,
    disposalMethod: DisposalMethod,
    wasAUKResident: Boolean,
    wasResidentialProperty: Boolean,
    disposalDate: DisposalDate,
    completionDate: CompletionDate
  ) extends IndividualTriageAnswers

  object CompleteIndividualTriageAnswers {

    implicit val format: OFormat[CompleteIndividualTriageAnswers] = Json.format
  }

  implicit class IndividualTriageQuestionOps(val i: IndividualTriageAnswers) extends AnyVal {

    def fold[A](
      ifIncomplete: IncompleteIndividualTriageAnswers => A,
      ifComplete: CompleteIndividualTriageAnswers => A
    ): A = i match {
      case incomplete: IncompleteIndividualTriageAnswers => ifIncomplete(incomplete)
      case complete: CompleteIndividualTriageAnswers     => ifComplete(complete)
    }

  }

  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  implicit val format: OFormat[IndividualTriageAnswers] = derived.oformat()

}
