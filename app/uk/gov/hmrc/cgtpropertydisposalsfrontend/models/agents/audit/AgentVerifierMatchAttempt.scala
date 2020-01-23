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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.models.agents.audit

import play.api.libs.json.{JsObject, JsString, Json, Writes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.{Country, Postcode}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.{AgentReferenceNumber, CgtReference}

final case class AgentVerifierMatchAttempt(
  agentReferenceNumber: AgentReferenceNumber,
  cgtReference: CgtReference,
  attemptsMade: Int,
  maxAttempts: Int,
  attemptedVerifier: Either[Country, Postcode],
  success: Boolean
)

object AgentVerifierMatchAttempt {

  implicit val verifierWrites: Writes[Either[Country, Postcode]] = Writes(
    _.fold(
      country => JsObject(Map("country"   -> Json.toJson(country))),
      postcode => JsObject(Map("postcode" -> JsString(postcode.value)))
    )
  )

  implicit val writes: Writes[AgentVerifierMatchAttempt] = Json.writes[AgentVerifierMatchAttempt]

}
