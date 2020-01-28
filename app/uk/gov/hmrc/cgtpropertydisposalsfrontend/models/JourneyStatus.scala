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

import cats.Eq
import julienrf.json.derived
import play.api.libs.json.{Json, OFormat}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.{AgentReferenceNumber, CgtReference, GGCredId}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.IndividualName
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.bpr.BusinessPartnerRecord
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.email.{Email, EmailSource}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.{RegistrationDetails, SubscribedDetails, SubscriptionDetails}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{DraftReturn, IndividualTriageAnswers}

sealed trait JourneyStatus extends Product with Serializable

object JourneyStatus {

  sealed trait SubscriptionStatus extends JourneyStatus with Product with Serializable

  object SubscriptionStatus {

    // user with affinity organisation is trying to subscribe without having a trust enrolment
    final case class DeterminingIfOrganisationIsTrust(
      ggCredId: GGCredId,
      ggEmail: Option[Email],
      isReportingForTrust: Option[Boolean],
      hasTrn: Option[Boolean]
    ) extends SubscriptionStatus

    // we cannot automatically find an identifier for an individual to get their business partner record
    final case class TryingToGetIndividualsFootprint(
      hasNino: Option[Boolean],
      hasSautr: Option[Boolean],
      ggEmail: Option[Email],
      ggCredId: GGCredId
    ) extends SubscriptionStatus

    // entity is missing data in order to continue on with subscription
    final case class SubscriptionMissingData(
      businessPartnerRecord: BusinessPartnerRecord,
      manuallyEnteredEmail: Option[Email],
      ggCredId: GGCredId,
      ggEmail: Option[Email]
    ) extends SubscriptionStatus

    // subscription details have been gathered and are ready to be used to subscribe
    final case class SubscriptionReady(subscriptionDetails: SubscriptionDetails, ggCredId: GGCredId)
        extends SubscriptionStatus
  }

  // subscription has been submitted to ETMP
  final case class Subscribed(
    subscribedDetails: SubscribedDetails,
    ggCredId: GGCredId,
    agentReferenceNumber: Option[AgentReferenceNumber],
    individualTriageAnswers: Option[IndividualTriageAnswers],
    draftReturn: Option[DraftReturn]
  ) extends JourneyStatus

  final case class AlreadySubscribedWithDifferentGGAccount(ggCredId: GGCredId, cgtReference: Option[CgtReference])
      extends JourneyStatus

  sealed trait RegistrationStatus extends JourneyStatus with Product with Serializable {
    val emailSource: Option[EmailSource]
    val ggCredId: GGCredId
  }

  object RegistrationStatus {

    // user with individual account has said they want to register a trust
    final case class IndividualWantsToRegisterTrust(ggCredId: GGCredId) extends RegistrationStatus {
      val emailSource: Option[EmailSource] = None
    }

    // user is supplying information for subscription
    final case class IndividualSupplyingInformation(
      name: Option[IndividualName],
      address: Option[Address],
      email: Option[Email],
      emailSource: Option[EmailSource],
      ggCredId: GGCredId
    ) extends RegistrationStatus

    // we are capturing an email for a user who doesn't have one we can retrieve
    final case class IndividualMissingEmail(name: IndividualName, address: Address, ggCredId: GGCredId)
        extends RegistrationStatus {
      val emailSource: Option[EmailSource] = None
    }

    // we have all the details necessary for registration
    final case class RegistrationReady(registrationDetails: RegistrationDetails, ggCredId: GGCredId)
        extends RegistrationStatus {
      val emailSource: Option[EmailSource] = Some(registrationDetails.emailSource)
    }

  }

  object AgentStatus {

    final case class AgentSupplyingClientDetails(
      agentReferenceNumber: AgentReferenceNumber,
      agentGGCredId: GGCredId,
      verifierMatchingDetails: Option[VerifierMatchingDetails]
    ) extends JourneyStatus

    final case class VerifierMatchingDetails(
      clientDetails: SubscribedDetails,
      correctVerifierSupplied: Boolean
    )

    implicit val verifierMatchingDetailsFormat: OFormat[VerifierMatchingDetails] = Json.format

  }

  final case object NonGovernmentGatewayJourney extends JourneyStatus

  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  implicit val format: OFormat[JourneyStatus] = derived.oformat()

  implicit val eq: Eq[JourneyStatus] = Eq.fromUniversalEquals

  implicit def eqJ[J <: JourneyStatus]: Eq[J] = Eq.instance(eq.eqv(_, _))

}
