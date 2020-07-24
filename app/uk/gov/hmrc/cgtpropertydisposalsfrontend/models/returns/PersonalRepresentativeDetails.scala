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

import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.StartingNewDraftReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.IndividualUserType.{PersonalRepresentative, PersonalRepresentativeInPeriodOfAdmin}

final case class PersonalRepresentativeDetails(
  personalRepresentativeType: Either[PersonalRepresentativeInPeriodOfAdmin.type, PersonalRepresentative.type],
  dateOfDeath: DateOfDeath
)

object PersonalRepresentativeDetails {

  def fromDraftReturn(
    draftReturn: DraftReturn
  ): Either[String, Option[PersonalRepresentativeDetails]] =
    fromAnswers(draftReturn.triageAnswers(), draftReturn.representeeAnswers)

  def fromStartingNewDraftReturn(
    startingNewDraftReturn: StartingNewDraftReturn
  ): Either[String, Option[PersonalRepresentativeDetails]] =
    fromAnswers(startingNewDraftReturn.newReturnTriageAnswers, startingNewDraftReturn.representeeAnswers)

  private def fromAnswers(
    triageAnswers: Either[MultipleDisposalsTriageAnswers, SingleDisposalTriageAnswers],
    representeeAnswers: Option[RepresenteeAnswers]
  ): Either[String, Option[PersonalRepresentativeDetails]] = {
    val individualUserType = triageAnswers.fold(
      _.fold(_.individualUserType, _.individualUserType),
      _.fold(_.individualUserType, _.individualUserType)
    )

    lazy val dateOfDeath = representeeAnswers.flatMap(_.fold(_.dateOfDeath, _.dateOfDeath))

    individualUserType match {
      case Some(PersonalRepresentative)                =>
        dateOfDeath.fold[Either[String, Option[PersonalRepresentativeDetails]]](
          Left("Could not find date of death for personal rep")
        )(date => Right(Some(PersonalRepresentativeDetails(Right(PersonalRepresentative), date))))

      case Some(PersonalRepresentativeInPeriodOfAdmin) =>
        dateOfDeath.fold[Either[String, Option[PersonalRepresentativeDetails]]](
          Left("Could not find date of death for personal rep in period of admin")
        )(date => Right(Some(PersonalRepresentativeDetails(Left(PersonalRepresentativeInPeriodOfAdmin), date))))

      case _                                           =>
        Right(None)
    }
  }

}
