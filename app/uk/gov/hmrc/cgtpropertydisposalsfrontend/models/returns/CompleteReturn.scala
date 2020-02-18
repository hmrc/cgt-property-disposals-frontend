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

import java.util.UUID

import play.api.libs.json.{Json, OFormat}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address.UkAddress
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.CgtReference
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.AcquisitionDetailsAnswers.CompleteAcquisitionDetailsAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.DisposalDetailsAnswers.CompleteDisposalDetailsAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ExemptionAndLossesAnswers.CompleteExemptionAndLossesAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.IndividualTriageAnswers.CompleteIndividualTriageAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ReliefDetailsAnswers.CompleteReliefDetailsAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.YearToDateLiabilityAnswers.CompleteYearToDateLiabilityAnswers

final case class CompleteReturn(
  id: UUID,
  cgtReference: CgtReference,
  triageAnswers: CompleteIndividualTriageAnswers,
  propertyAddress: UkAddress,
  disposalDetails: CompleteDisposalDetailsAnswers,
  acquisitionDetails: CompleteAcquisitionDetailsAnswers,
  reliefDetails: CompleteReliefDetailsAnswers,
  exemptionsAndLossesDetails: CompleteExemptionAndLossesAnswers,
  yearToDateLiabilityAnswers: CompleteYearToDateLiabilityAnswers
)

object CompleteReturn {

  def fromDraftReturn(draftReturn: DraftReturn): Option[CompleteReturn] = draftReturn match {
    case DraftReturn(
        id,
        cgtReference,
        t: CompleteIndividualTriageAnswers,
        Some(p: UkAddress),
        Some(d: CompleteDisposalDetailsAnswers),
        Some(a: CompleteAcquisitionDetailsAnswers),
        Some(r: CompleteReliefDetailsAnswers),
        Some(e: CompleteExemptionAndLossesAnswers),
        Some(y: CompleteYearToDateLiabilityAnswers)
        ) =>
      Some(CompleteReturn(id, cgtReference, t, p, d, a, r, e, y))

    case _ =>
      None
  }

  implicit val format: OFormat[CompleteReturn] = {
    implicit val triageFormat: OFormat[CompleteIndividualTriageAnswers]                 = Json.format
    implicit val ukAddressFormat: OFormat[UkAddress]                                    = Json.format
    implicit val disposalDetailsFormat: OFormat[CompleteDisposalDetailsAnswers]         = Json.format
    implicit val acquisitionDetailsFormat: OFormat[CompleteAcquisitionDetailsAnswers]   = Json.format
    implicit val reliefDetailsFormat: OFormat[CompleteReliefDetailsAnswers]             = Json.format
    implicit val exemptionAndLossesFormat: OFormat[CompleteExemptionAndLossesAnswers]   = Json.format
    implicit val yearToDateLiabilityFormat: OFormat[CompleteYearToDateLiabilityAnswers] = Json.format
    Json.format
  }

}
