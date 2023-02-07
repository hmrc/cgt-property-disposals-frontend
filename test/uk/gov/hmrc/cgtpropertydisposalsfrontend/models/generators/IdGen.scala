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
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.{AgentReferenceNumber, CgtReference, DraftReturnId, GGCredId, NINO, SAUTR, SapNumber}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.upscan.UploadReference

object IdGen extends GenUtils {

  implicit val cgtReferenceArb: Gen[CgtReference] = gen[CgtReference]

  implicit val ggCredIdGen: Gen[GGCredId] = gen[GGCredId]

  implicit val ninoGen: Gen[NINO] = gen[NINO]

  implicit val sautrGen: Gen[SAUTR] = gen[SAUTR]

  implicit val sapNumberGen: Gen[SapNumber] = gen[SapNumber]

  implicit val arnGen: Gen[AgentReferenceNumber] = gen[AgentReferenceNumber]

  implicit val draftReturnIdGen: Gen[DraftReturnId] = gen[DraftReturnId]

  implicit val uploadReferenceGen: Gen[UploadReference] = gen[UploadReference]

}
