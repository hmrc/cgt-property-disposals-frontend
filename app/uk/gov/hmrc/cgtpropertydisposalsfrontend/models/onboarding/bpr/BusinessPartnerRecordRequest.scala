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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.bpr

import julienrf.json.derived
import play.api.libs.json.OFormat
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.{NINO, SAUTR, TRN}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.{IndividualName, TrustName}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.EitherUtils.eitherFormat

sealed trait BusinessPartnerRecordRequest extends Product with Serializable

object BusinessPartnerRecordRequest {

  final case class IndividualBusinessPartnerRecordRequest(
    id: Either[SAUTR, NINO],
    nameMatch: Option[IndividualName],
    ggCredId: String,
    createNewEnrolmentIfMissing: Boolean
  ) extends BusinessPartnerRecordRequest

  final case class TrustBusinessPartnerRecordRequest(
    id: Either[TRN, SAUTR],
    nameMatch: Option[TrustName],
    ggCredId: String,
    createNewEnrolmentIfMissing: Boolean
  ) extends BusinessPartnerRecordRequest

  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  implicit val format: OFormat[BusinessPartnerRecordRequest] = derived.oformat()

}
