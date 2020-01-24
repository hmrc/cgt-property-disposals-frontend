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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.models

import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.email.Email
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.EitherUtils._
import julienrf.json.derived
import play.api.libs.json.OFormat

sealed trait RetrievedUserType extends Product with Serializable

object RetrievedUserType {

  final case class Individual(
    id: Either[SAUTR, NINO],
    email: Option[Email],
    ggCredId: GGCredId
  ) extends RetrievedUserType

  object Individual{
    implicit val format: OFormat[Individual] = derived.oformat()
  }

  final case class Trust(sautr: SAUTR, email: Option[Email], ggCredId: GGCredId) extends RetrievedUserType

  object Trust{
    implicit val format: OFormat[Trust] = derived.oformat()
  }

  final case class OrganisationUnregisteredTrust(email: Option[Email], ggCredId: GGCredId) extends RetrievedUserType

  object OrganisationUnregisteredTrust{
    implicit val format: OFormat[OrganisationUnregisteredTrust] = derived.oformat()
  }

  final case class IndividualWithInsufficientConfidenceLevel(
    nino: Option[NINO],
    sautr: Option[SAUTR],
    email: Option[Email],
    ggCredId: GGCredId
  ) extends RetrievedUserType

  object IndividualWithInsufficientConfidenceLevel {
    implicit val format: OFormat[IndividualWithInsufficientConfidenceLevel] = derived.oformat()
  }

  final case class Subscribed(
    cgtReference: CgtReference,
    ggCredId: GGCredId
  ) extends RetrievedUserType

  object Subscribed{
    implicit val format: OFormat[Subscribed] = derived.oformat()
  }

  final case class Agent(
    GGCredId: GGCredId,
    agentReferenceNumber: AgentReferenceNumber
  ) extends RetrievedUserType

  object Agent{
    implicit val format: OFormat[Agent] = derived.oformat()
  }

  final case class NonGovernmentGatewayRetrievedUser(authProvider: String) extends RetrievedUserType

  object NonGovernmentGatewayRetrievedUser {
    implicit val format: OFormat[NonGovernmentGatewayRetrievedUser] = derived.oformat()
  }

}
