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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.services.agents

import com.google.inject.{ImplementedBy, Inject, Singleton}
import play.api.libs.json.{Json, Writes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.agents.routes
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.{Country, Postcode}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.agents.audit.{AgentAccessAttempt, AgentVerifierMatchAttempt}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.{AgentReferenceNumber, CgtReference}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.audit.AuditExtensions._
import uk.gov.hmrc.play.audit.http.connector.AuditConnector
import uk.gov.hmrc.play.audit.model.ExtendedDataEvent

import scala.concurrent.ExecutionContext
@ImplementedBy(classOf[AgentAccessAuditServiceImpl])
trait AgentAccessAuditService {

  def sendAgentAccessAttemptEvent(
    agentReferenceNumber: AgentReferenceNumber,
    cgtReference: CgtReference,
    success: Boolean
  )(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext
  ): Unit

  def sendAgentVerifierMatchAttemptEvent(
    agentReferenceNumber: AgentReferenceNumber,
    cgtReference: CgtReference,
    attemptsMade: Int,
    maxAttempts: Int,
    attemptedVerifier: Either[Country, Postcode],
    success: Boolean
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Unit

}

@Singleton
class AgentAccessAuditServiceImpl @Inject()(auditConnector: AuditConnector)
    extends AgentAccessAuditService
    with Logging {

  private def sendEvent[A](auditType: String, detail: A, tags: Map[String, String])(
    implicit ec: ExecutionContext,
    writes: Writes[A]
  ): Unit = {
    val extendedDataEvent = ExtendedDataEvent(
      auditSource = "cgt-property-disposals",
      auditType   = auditType,
      detail      = Json.toJson(detail),
      tags        = tags
    )
    auditConnector.sendExtendedEvent(extendedDataEvent)
  }

  def sendAgentAccessAttemptEvent(
    agentReferenceNumber: AgentReferenceNumber,
    cgtReference: CgtReference,
    success: Boolean
  )(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext
  ): Unit =
    sendEvent(
      "agentAccessAttempt",
      AgentAccessAttempt(agentReferenceNumber, cgtReference, success),
      hc.toAuditTags("agent-access-attempt", routes.AgentAccessController.enterClientsCgtRef().url)
    )

  def sendAgentVerifierMatchAttemptEvent(
    agentReferenceNumber: AgentReferenceNumber,
    cgtReference: CgtReference,
    attemptsMade: Int,
    maxAttempts: Int,
    attemptedVerifier: Either[Country, Postcode],
    success: Boolean
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Unit =
    sendEvent(
      "agentVerifierMatchAttemptEvent",
      AgentVerifierMatchAttempt(
        agentReferenceNumber: AgentReferenceNumber,
        cgtReference,
        attemptsMade,
        maxAttempts,
        attemptedVerifier,
        success
      ),
      hc.toAuditTags(
        "agent-verifier-match-attempt",
        attemptedVerifier
          .fold(
            _ => routes.AgentAccessController.enterClientsCountry(),
            _ => routes.AgentAccessController.enterClientsPostcode()
          )
          .url
      )
    )

}
