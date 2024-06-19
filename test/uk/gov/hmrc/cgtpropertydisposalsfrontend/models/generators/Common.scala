/*
 * Copyright 2024 HM Revenue & Customs
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
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.AcquisitionDetailsAnswers.CompleteAcquisitionDetailsAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.DisposalDetailsAnswers.CompleteDisposalDetailsAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ExemptionAndLossesAnswers.CompleteExemptionAndLossesAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.RepresenteeAnswers.CompleteRepresenteeAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SupportingEvidenceAnswers.CompleteSupportingEvidenceAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.YearToDateLiabilityAnswers.NonCalculatedYTDAnswers.CompleteNonCalculatedYTDAnswers

trait Common extends GenUtils {
  val exemptionAndLossesAnswers: Gen[CompleteExemptionAndLossesAnswers] =
    ExemptionsAndLossesAnswersGen.completeExemptionAndLossesAnswersGen
  val yearToDateLiabilityAnswers: Gen[CompleteNonCalculatedYTDAnswers]  =
    YearToDateLiabilityAnswersGen.completeNonCalculatedYTDLiabilityAnswersGen
  val supportingDocumentAnswers: Gen[CompleteSupportingEvidenceAnswers] =
    FileUploadGen.completeUploadSupportingEvidenceAnswersGen
  val representeeAnswers: Gen[CompleteRepresenteeAnswers]               = RepresenteeAnswersGen.completeRepresenteeAnswersGen
  val disposalDetails: Gen[CompleteDisposalDetailsAnswers]              = DisposalDetailsGen.completeDisposalDetailsAnswersGen
  val acquisitionDetails: Gen[CompleteAcquisitionDetailsAnswers]        =
    AcquisitionDetailsGen.completeAcquisitionDetailsAnswersGen
}
