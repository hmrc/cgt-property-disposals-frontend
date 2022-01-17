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
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{DraftMultipleDisposalsReturn, DraftMultipleIndirectDisposalsReturn, DraftReturn, DraftSingleDisposalReturn, DraftSingleIndirectDisposalReturn, DraftSingleMixedUseDisposalReturn}

object DraftReturnGen extends HigherPriorityDraftReturnGen with GenUtils

trait HigherPriorityDraftReturnGen extends LowerPriorityDraftReturnGen { this: GenUtils =>
  implicit val draftReturnGen: Gen[DraftReturn] = gen[DraftReturn]

  implicit val singleDisposalDraftReturnGen: Gen[DraftSingleDisposalReturn] =
    gen[DraftSingleDisposalReturn]

}

trait LowerPriorityDraftReturnGen { this: GenUtils =>

  implicit val multipleDisposalDraftReturnGen: Gen[DraftMultipleDisposalsReturn] = gen[DraftMultipleDisposalsReturn]

  implicit val multipleIndirectDisposalDraftReturnGen: Gen[DraftMultipleIndirectDisposalsReturn] =
    gen[DraftMultipleIndirectDisposalsReturn]

  implicit val singleIndirectDisposalDraftReturnGen: Gen[DraftSingleIndirectDisposalReturn] =
    gen[DraftSingleIndirectDisposalReturn]

  implicit val singleMixedUseDraftReturnGen: Gen[DraftSingleMixedUseDisposalReturn] =
    gen[DraftSingleMixedUseDisposalReturn]

}
