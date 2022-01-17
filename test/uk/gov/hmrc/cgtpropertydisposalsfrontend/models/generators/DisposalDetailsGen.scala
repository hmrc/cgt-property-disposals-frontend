/*
 * Copyright 2022 HM Revenue & Customs
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
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.DisposalDetailsAnswers.{CompleteDisposalDetailsAnswers, IncompleteDisposalDetailsAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ShareOfProperty

object DisposalDetailsGen extends GenUtils {

  implicit val completeDisposalDetailsAnswersGen: Gen[CompleteDisposalDetailsAnswers] =
    gen[CompleteDisposalDetailsAnswers]

  implicit val incompleteDisposalDetailsAnswersGen: Gen[IncompleteDisposalDetailsAnswers] =
    gen[IncompleteDisposalDetailsAnswers]

  implicit val shareOfPropertyGen: Gen[ShareOfProperty] =
    gen[ShareOfProperty].map {
      case a: ShareOfProperty.Other if a.percentageValue > 100 =>
        ShareOfProperty.Full
      case other: ShareOfProperty                              => other
    }

}
