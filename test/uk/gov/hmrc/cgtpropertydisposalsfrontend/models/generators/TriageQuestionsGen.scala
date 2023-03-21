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
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.MultipleDisposalsTriageAnswers.{CompleteMultipleDisposalsTriageAnswers, IncompleteMultipleDisposalsTriageAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.{CompleteSingleDisposalTriageAnswers, IncompleteSingleDisposalTriageAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns._

object TriageQuestionsGen extends HigherPriorityTriageQuestionsGen with GenUtils

trait HigherPriorityTriageQuestionsGen extends LowerPriorityTriageQuestionsGen { this: GenUtils =>
  implicit val individualTriageAnswersGen: Gen[SingleDisposalTriageAnswers] =
    gen[SingleDisposalTriageAnswers]

  implicit val completeSingleDisposalTriageAnswersGen: Gen[CompleteSingleDisposalTriageAnswers] =
    gen[CompleteSingleDisposalTriageAnswers]

  implicit val completeMultipleDisposalsTriageAnswersGen: Gen[CompleteMultipleDisposalsTriageAnswers] =
    gen[CompleteMultipleDisposalsTriageAnswers]

  implicit val incompleteMultipleDisposalsTriageAnswersGen: Gen[IncompleteMultipleDisposalsTriageAnswers] =
    gen[IncompleteMultipleDisposalsTriageAnswers]

  implicit val individualUserTypeGen: Gen[IndividualUserType] =
    gen[IndividualUserType]

  implicit val numberOfPropertiesGen: Gen[NumberOfProperties] =
    gen[NumberOfProperties]

  implicit val disposalDateGen: Gen[DisposalDate] = gen[DisposalDate]

  implicit val completionDateGen: Gen[CompletionDate] = gen[CompletionDate]

  implicit val assetTypeGen: Gen[AssetType] = gen[AssetType]

}

trait LowerPriorityTriageQuestionsGen { this: GenUtils =>

  implicit val incompleteSingleDisposalTriageAnswersGen: Gen[IncompleteSingleDisposalTriageAnswers] =
    gen[IncompleteSingleDisposalTriageAnswers]

}
