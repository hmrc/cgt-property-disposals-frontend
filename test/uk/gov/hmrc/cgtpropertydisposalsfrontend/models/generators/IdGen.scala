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
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.upscan.UploadReference

object IdGen extends GenUtils {

  implicit val cgtReferenceArb: Gen[CgtReference] = Generators.stringGen.map(CgtReference(_))

  implicit val ggCredIdGen: Gen[GGCredId] = Generators.stringGen.map(GGCredId(_))

  implicit val ninoGen: Gen[NINO] = Generators.stringGen.map(NINO(_))

  implicit val sautrGen: Gen[SAUTR] = Generators.stringGen.map(SAUTR(_))

  implicit val sapNumberGen: Gen[SapNumber] = Generators.stringGen.map(SapNumber(_))

  implicit val arnGen: Gen[AgentReferenceNumber] = Generators.stringGen.map(AgentReferenceNumber(_))

  implicit val draftReturnIdGen: Gen[DraftReturnId] = Generators.stringGen.map(DraftReturnId(_))

  implicit val uploadReferenceGen: Gen[UploadReference] = Generators.stringGen.map(UploadReference(_))

}
