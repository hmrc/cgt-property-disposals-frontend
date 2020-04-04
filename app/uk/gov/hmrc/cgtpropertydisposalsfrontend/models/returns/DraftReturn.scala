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

import java.time.LocalDate
import java.util.UUID

import cats.Eq
import julienrf.json.derived
import play.api.libs.json.{Json, OFormat}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.TimeUtils
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address.UkAddress
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.AmountInPence

sealed trait DraftReturn extends Product with Serializable {
  val id: UUID
  val lastUpdatedDate: LocalDate
}

final case class DraftSingleDisposalReturn(
  id: UUID,
  triageAnswers: SingleDisposalTriageAnswers,
  propertyAddress: Option[UkAddress],
  disposalDetailsAnswers: Option[DisposalDetailsAnswers],
  acquisitionDetailsAnswers: Option[AcquisitionDetailsAnswers],
  reliefDetailsAnswers: Option[ReliefDetailsAnswers],
  exemptionAndLossesAnswers: Option[ExemptionAndLossesAnswers],
  yearToDateLiabilityAnswers: Option[YearToDateLiabilityAnswers],
  initialGainOrLoss: Option[AmountInPence],
  uploadSupportingDocuments: Option[UploadSupportingEvidenceAnswers],
  lastUpdatedDate: LocalDate
) extends DraftReturn

object DraftSingleDisposalReturn {

  implicit val eq: Eq[DraftSingleDisposalReturn] = Eq.fromUniversalEquals[DraftSingleDisposalReturn]

  def newDraftReturn(id: UUID, triageAnswers: SingleDisposalTriageAnswers): DraftSingleDisposalReturn =
    DraftSingleDisposalReturn(id, triageAnswers, None, None, None, None, None, None, None, None, TimeUtils.today())

}

final case class DraftMultipleDisposalsReturn(
  id: UUID,
  triageAnswers: MultipleDisposalsTriageAnswers,
  examplePropertyDetailsAnswers: Option[ExamplePropertyDetailsAnswers],
  exemptionAndLossesAnswers: Option[ExemptionAndLossesAnswers],
  yearToDateLiabilityAnswers: Option[YearToDateLiabilityAnswers],
  uploadSupportingDocuments: Option[UploadSupportingEvidenceAnswers],
  lastUpdatedDate: LocalDate
) extends DraftReturn

object DraftMultipleDisposalsReturn {

  def newDraftReturn(id: UUID, triageAnswers: MultipleDisposalsTriageAnswers): DraftMultipleDisposalsReturn =
    DraftMultipleDisposalsReturn(id, triageAnswers, None, None, None, None, TimeUtils.today())

}

object DraftReturn {

  implicit class DraftReturnOps(private val d: DraftReturn) extends AnyVal {
    def fold[A](whenMultiple: DraftMultipleDisposalsReturn => A, whenSingle: DraftSingleDisposalReturn => A): A =
      d match {
        case m: DraftMultipleDisposalsReturn => whenMultiple(m)
        case s: DraftSingleDisposalReturn    => whenSingle(s)
      }

  }

  implicit val eq: Eq[DraftReturn] = Eq.fromUniversalEquals

  implicit val ukAddressFormat: OFormat[UkAddress] = Json.format[UkAddress]

  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  implicit val format: OFormat[DraftReturn] = derived.oformat()

}
