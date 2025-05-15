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

import org.scalacheck.{Arbitrary, Gen}
import io.github.martinhh.derived.scalacheck.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.IdGen.uploadReferenceGen
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SupportingEvidenceAnswers.{CompleteSupportingEvidenceAnswers, IncompleteSupportingEvidenceAnswers, SupportingEvidence}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.upscan.UpscanCallBack.{UpscanFailure, UpscanSuccess}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.upscan.{UploadReference, UploadRequest, UpscanCallBack, UpscanUpload}

object FileUploadGen extends GenUtils {

  given completeUploadSupportingEvidenceAnswersGen: Gen[CompleteSupportingEvidenceAnswers] =
    gen[CompleteSupportingEvidenceAnswers]

  given incompleteUploadSupportingEvidenceAnswersGen: Gen[IncompleteSupportingEvidenceAnswers] =
    gen[IncompleteSupportingEvidenceAnswers]

  given uploadReferenceArb: Arbitrary[UploadReference] = Arbitrary(uploadReferenceGen)
  given supportingEvidenceGen: Gen[SupportingEvidence] = gen[SupportingEvidence]

  given uploadRequestGen: Gen[UploadRequest] = gen[UploadRequest]

  given upscanUploadGen: Gen[UpscanUpload] = gen[UpscanUpload]

  given upscanSuccessGen: Gen[UpscanSuccess] = gen[UpscanSuccess]

  given upscanFailureGen: Gen[UpscanFailure] = gen[UpscanFailure]

  given Gen[UpscanCallBack] = Gen.oneOf(upscanSuccessGen, upscanFailureGen)
}
