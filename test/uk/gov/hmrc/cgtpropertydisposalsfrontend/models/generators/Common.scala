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
import org.scalacheck.ScalacheckShapeless._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.AcquisitionDetailsAnswers.CompleteAcquisitionDetailsAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.DisposalDetailsAnswers.CompleteDisposalDetailsAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ExemptionAndLossesAnswers.CompleteExemptionAndLossesAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.RepresenteeAnswers.CompleteRepresenteeAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SupportingEvidenceAnswers.CompleteSupportingEvidenceAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.YearToDateLiabilityAnswers.NonCalculatedYTDAnswers.CompleteNonCalculatedYTDAnswers

trait Common extends GenUtils {
  val exemptionAndLossesAnswers: Gen[CompleteExemptionAndLossesAnswers] = gen[CompleteExemptionAndLossesAnswers]
  val yearToDateLiabilityAnswers: Gen[CompleteNonCalculatedYTDAnswers]  = gen[CompleteNonCalculatedYTDAnswers]
  val supportingDocumentAnswers: Gen[CompleteSupportingEvidenceAnswers] = gen[CompleteSupportingEvidenceAnswers]
  val representeeAnswers: Gen[CompleteRepresenteeAnswers]               = gen[CompleteRepresenteeAnswers]
  val disposalDetails: Gen[CompleteDisposalDetailsAnswers]              = gen[CompleteDisposalDetailsAnswers]
  val acquisitionDetails: Gen[CompleteAcquisitionDetailsAnswers]        = gen[CompleteAcquisitionDetailsAnswers]
}
