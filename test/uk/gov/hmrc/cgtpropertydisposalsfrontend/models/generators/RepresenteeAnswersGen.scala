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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators

import org.scalacheck.Gen
import org.scalacheck.ScalacheckShapeless._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.RepresenteeAnswers.{CompleteRepresenteeAnswers, IncompleteRepresenteeAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.RepresenteeReferenceId.{RepresenteeCgtReference, RepresenteeNino, RepresenteeSautr}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{DateOfDeath, RepresenteeAnswers, RepresenteeContactDetails, RepresenteeReferenceId}

object RepresenteeAnswersGen extends HigherPriorityRepresenteeAnswersGen with GenUtils

trait HigherPriorityRepresenteeAnswersGen extends LowerPriorityRepresenteeAnswersGen { this: GenUtils =>

  implicit val representeeAnswersGen: Gen[RepresenteeAnswers] =
    gen[RepresenteeAnswers]

  implicit val incompleteRepresenteeAnswersGen: Gen[IncompleteRepresenteeAnswers] = gen[IncompleteRepresenteeAnswers]

  implicit val representeeReferenceIdGen: Gen[RepresenteeReferenceId] =
    gen[RepresenteeReferenceId]

  implicit val representeeCgtReferenceGen: Gen[RepresenteeCgtReference] =
    gen[RepresenteeCgtReference]

  implicit val representeeContactDetailsGen: Gen[RepresenteeContactDetails] =
    gen[RepresenteeContactDetails]

}

trait LowerPriorityRepresenteeAnswersGen { this: GenUtils =>

  implicit val completeRepresenteeAnswersGen: Gen[CompleteRepresenteeAnswers] =
    gen[CompleteRepresenteeAnswers]

  implicit val representeeSautrGen: Gen[RepresenteeSautr] =
    gen[RepresenteeSautr]

  implicit val representeeNinoGen: Gen[RepresenteeNino] = gen[RepresenteeNino]

  implicit val dateOfDeathGen: Gen[DateOfDeath] = gen[DateOfDeath]
}
