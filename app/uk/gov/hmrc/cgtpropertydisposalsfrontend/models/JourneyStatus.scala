/*
 * Copyright 2019 HM Revenue & Customs
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

import julienrf.json.derived
import play.api.libs.json.OFormat
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.bpr.BusinessPartnerRecord
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.GGCredId
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.IndividualName

sealed trait JourneyStatus extends Product with Serializable

object JourneyStatus {

  sealed trait SubscriptionStatus extends JourneyStatus with Product with Serializable

  object SubscriptionStatus {

    // user with affinity organisation is trying to subscribe without having a trust enrolment
    final case object UnregisteredTrust extends SubscriptionStatus

    // we cannot automatically find an identifier for an individual to get their business partner record
    final case class TryingToGetIndividualsFootprint(
      hasNino: Option[Boolean],
      hasSautr: Option[Boolean],
      email: Option[Email],
      ggCredId: GGCredId
    ) extends SubscriptionStatus

    // entity is missing data in order to continue on with subscription
    final case class SubscriptionMissingData(businessPartnerRecord: BusinessPartnerRecord) extends SubscriptionStatus

    // subscription details have been gathered and are ready to be used to subscribe
    final case class SubscriptionReady(subscriptionDetails: SubscriptionDetails) extends SubscriptionStatus
  }

  // subscription has been submitted to ETMP
  final case class Subscribed(subscribedDetails: SubscribedDetails) extends JourneyStatus

  sealed trait RegistrationStatus extends JourneyStatus with Product with Serializable

  object RegistrationStatus {

    // user with individual account has said they want to register a trust
    final case object IndividualWantsToRegisterTrust extends RegistrationStatus

    // user is supplying information for subscription
    final case class IndividualSupplyingInformation(
      name: Option[IndividualName],
      address: Option[Address],
      email: Option[Email]
    ) extends RegistrationStatus

    // we are capturing an email for a user who doesn't have one we can retrieve
    final case class IndividualMissingEmail(name: IndividualName, address: Address) extends RegistrationStatus

    // we have all the details necessary for registration
    final case class RegistrationReady(registrationDetails: RegistrationDetails) extends RegistrationStatus

  }
  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  implicit val format: OFormat[JourneyStatus] = derived.oformat[JourneyStatus]

}
