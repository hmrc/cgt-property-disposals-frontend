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
import org.scalacheck.Arbitrary
import io.github.martinhh.derived.scalacheck.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.NameGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.EmailGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.AddressGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.RepresenteeAnswers.{CompleteRepresenteeAnswers, IncompleteRepresenteeAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.RepresenteeReferenceId.{RepresenteeCgtReference, RepresenteeNino, RepresenteeSautr}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{DateOfDeath, RepresenteeAnswers, RepresenteeContactDetails, RepresenteeReferenceId}

object RepresenteeAnswersGen extends HigherPriorityRepresenteeAnswersGen with GenUtils

trait HigherPriorityRepresenteeAnswersGen extends LowerPriorityRepresenteeAnswersGen { this: GenUtils =>

  given representeeAnswersGen: Gen[RepresenteeAnswers] = gen[RepresenteeAnswers]

  given incompleteRepresenteeAnswersGen: Gen[IncompleteRepresenteeAnswers] = gen[IncompleteRepresenteeAnswers]

  given representeeReferenceIdGen: Gen[RepresenteeReferenceId] = gen[RepresenteeReferenceId]

  given representeeCgtReferenceGen: Gen[RepresenteeCgtReference] = gen[RepresenteeCgtReference]

  given representeeContactDetailsGen: Gen[RepresenteeContactDetails] = gen[RepresenteeContactDetails]

}

trait LowerPriorityRepresenteeAnswersGen { this: GenUtils =>

  given completeRepresenteeAnswersGen: Gen[CompleteRepresenteeAnswers] = gen[CompleteRepresenteeAnswers]

  given representeeSautrGen: Gen[RepresenteeSautr] = gen[RepresenteeSautr]

  given representeeNinoGen: Gen[RepresenteeNino] = gen[RepresenteeNino]

  given dateOfDeathGen: Gen[DateOfDeath] = gen[DateOfDeath]
}
