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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.services.onboarding

import cats.syntax.either._
import cats.syntax.eq._
import com.google.inject.{ImplementedBy, Inject, Singleton}
import play.api.libs.json.{Json, Writes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.routes
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.{Address, AddressSource, Country}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.{AgentReferenceNumber, GGCredId}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.{ContactNameSource, IndividualName}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.audit.BusinessPartnerRecordNameMatchDetails.{IndividualNameWithSaUtrAuditDetails, TrustNameWithTrnAuditDetails}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.audit.{Address => AuditAddress, _}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.email.EmailSource
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.{RegistrationDetails, SubscriptionDetails}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.audit.AuditExtensions._
import uk.gov.hmrc.play.audit.http.connector.AuditConnector
import uk.gov.hmrc.play.audit.model.ExtendedDataEvent

import scala.concurrent.ExecutionContext

@ImplementedBy(classOf[OnboardingAuditServiceImpl])
trait OnboardingAuditService {
  def sendHandOffToIvEvent(ggCredId: GGCredId, redirectUrl: String)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext
  ): Unit

  def sendBusinessPartnerRecordNameMatchAttemptEvent(
    attemptsMade: Int,
    maxAttemptsMade: Int,
    nameMatchDetails: BusinessPartnerRecordNameMatchDetails
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Unit

  def sendSubscriptionChangeEmailAddressAttemptedEvent(
    oldEmailAddress: String,
    newEmailAddress: String,
    path: String
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Unit

  def sendSubscriptionChangeEmailAddressVerifiedEvent(
    oldEmailAddress: String,
    newEmailAddress: String,
    path: String
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Unit

  def sendSubscriptionContactNameChangedEvent(
    oldContactName: String,
    newContactName: String,
    path: String
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Unit

  def sendSubscriptionContactAddressChangedEvent(
    oldContactAddress: Address,
    newContactAddress: Address,
    isManualAddress: Boolean,
    path: String
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Unit

  def sendSubscriptionRequestEvent(
    subscriptionDetails: SubscriptionDetails,
    path: String
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Unit

  def sendRegistrationContactNameChangedEvent(
    oldContactName: IndividualName,
    newContactName: IndividualName,
    path: String
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Unit

  def sendRegistrationContactAddressChangedEvent(
    oldContactAddress: Address,
    newContactAddress: Address,
    isManuallyEnteredAddress: Boolean,
    path: String
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Unit

  def sendRegistrationChangeEmailAddressAttemptedEvent(
    oldEmailAddress: String,
    newEmailAddress: String,
    path: String
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Unit

  def sendRegistrationChangeEmailVerifiedEvent(
    oldEmailAddress: String,
    newEmailAddress: String,
    path: String
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Unit

  def sendSubscribedChangeContactNameEvent(
    oldContactName: String,
    newContactName: String,
    cgtReference: String,
    agentReferenceNumber: Option[AgentReferenceNumber],
    path: String
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Unit

  def sendSubscribedChangeEmailAddressAttemptedEvent(
    oldEmailAddress: String,
    newEmailAddress: String,
    cgtReference: String,
    agentReferenceNumber: Option[AgentReferenceNumber],
    path: String
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Unit

  def sendSubscribedChangeEmailAddressVerifiedEvent(
    oldEmailAddress: String,
    newEmailAddress: String,
    cgtReference: String,
    agentReferenceNumber: Option[AgentReferenceNumber],
    path: String
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Unit

  def sendSubscribedContactAddressChangedEvent(
    oldContactAddress: Address,
    newContactAddress: Address,
    isManuallyEntered: Boolean,
    cgtReference: String,
    agentReferenceNumber: Option[AgentReferenceNumber],
    path: String
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Unit

  def sendRegistrationSetupEmailAttemptedEvent(
    emailAddress: String,
    path: String
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Unit

  def sendRegistrationSetupEmailVerifiedEvent(
    emailAddress: String,
    path: String
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Unit

  def sendRegistrationRequestEvent(
    registrationDetails: RegistrationDetails,
    path: String
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Unit

  def sendAccessWithWrongGGAccountEvent(
    ggCredId: GGCredId,
    path: String
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Unit

}

@Singleton
class OnboardingAuditServiceImpl @Inject()(auditConnector: AuditConnector) extends OnboardingAuditService with Logging {

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

  private def toAuditAddress(address: Address): AuditAddress = address match {
    case Address.UkAddress(line1, line2, town, county, postcode) =>
      AuditAddress(
        line1,
        line2,
        town,
        county,
        Some(postcode.value),
        Country("GB", Some("United Kingdom"))
      )
    case Address.NonUkAddress(line1, line2, line3, line4, postcode, country) =>
      AuditAddress(
        line1,
        line2,
        line3,
        line4,
        postcode,
        Country(country.code, country.name)
      )
  }

  override def sendHandOffToIvEvent(
    ggCredId: GGCredId,
    redirectUrl: String
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Unit = {
    val detail = HandOffTIvEvent(ggCredId.value, redirectUrl)

    sendEvent(
      "handOffToIv",
      detail,
      hc.toAuditTags("handoff-to-iv", redirectUrl)
    )
  }

  override def sendBusinessPartnerRecordNameMatchAttemptEvent(
    attemptsMade: Int,
    maxAttempts: Int,
    nameMatchDetails: BusinessPartnerRecordNameMatchDetails
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Unit = {

    val path = nameMatchDetails match {
      case IndividualNameWithSaUtrAuditDetails(firstName, lastName, sautr) =>
        routes.InsufficientConfidenceLevelController.enterSautrAndNameSubmit().url
      case TrustNameWithTrnAuditDetails(trustName, trn) =>
        routes.DeterminingIfOrganisationIsTrustController.enterTrnSubmit().url
    }

    val detail = BusinessPartnerRecordNameMatchAttemptEvent(
      attemptsMade,
      maxAttempts,
      nameMatchDetails
    )

    sendEvent(
      "businessPartnerRecordNameMatchAttempt",
      detail,
      hc.toAuditTags("business-partner-record-name-match-attempt", path)
    )

  }

  override def sendSubscriptionChangeEmailAddressAttemptedEvent(
    oldEmailAddress: String,
    newEmailAddress: String,
    path: String
  )(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext
  ): Unit = {

    val detail = SubscriptionChangeEmailAddressAttemptedEvent(
      oldEmailAddress,
      newEmailAddress
    )

    sendEvent(
      "subscriptionChangeEmailAddressAttempted",
      detail,
      hc.toAuditTags("subscription-change-email-address-attempted", path)
    )
  }

  override def sendSubscriptionChangeEmailAddressVerifiedEvent(
    oldEmailAddress: String,
    newEmailAddress: String,
    path: String
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Unit = {
    val detail = SubscriptionChangeEmailAddressAttemptedEvent(
      oldEmailAddress,
      newEmailAddress
    )

    sendEvent(
      "subscriptionChangeEmailAddressVerified",
      detail,
      hc.toAuditTags("subscription-change-email-address-verified", path)
    )
  }

  override def sendSubscriptionContactNameChangedEvent(
    oldContactName: String,
    newContactName: String,
    path: String
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Unit = {
    val detail = SubscriptionContactNameChangedEvent(
      oldContactName,
      newContactName
    )

    sendEvent(
      "subscriptionContactNameChanged",
      detail,
      hc.toAuditTags("subscription-contact-name-changed", path)
    )

  }

  override def sendSubscriptionContactAddressChangedEvent(
    oldContactAddress: Address,
    newContactAddress: Address,
    isManuallyEnteredAddress: Boolean,
    path: String
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Unit = {

    val source = if (isManuallyEnteredAddress) "manual-entry" else "postcode-lookup"

    val detail = SubscriptionContactAddressChangedEvent(
      toAuditAddress(oldContactAddress),
      toAuditAddress(newContactAddress),
      source
    )

    sendEvent(
      "subscriptionContactAddressChanged",
      detail,
      hc.toAuditTags("subscription-contact-address-changed", path)
    )
  }

  override def sendSubscriptionRequestEvent(
    subscriptionDetails: SubscriptionDetails,
    path: String
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Unit = {
    val prepopulatedEmailSource =
      if (subscriptionDetails.emailSource === EmailSource.BusinessPartnerRecord)
        Some("ETMP business partner record")
      else if (subscriptionDetails.emailSource === EmailSource.GovernmentGateway)
        Some("government-gateway")
      else
        None

    val auditAddress = toAuditAddress(subscriptionDetails.address)

    val prePopulatedUserData = {
      PrePopulatedUserData(
        "CGT",
        subscriptionDetails.sapNumber.value,
        subscriptionDetails.name.toOption.map(i => IndividualAuditDetails(i.firstName, i.lastName)),
        subscriptionDetails.name.swap.toOption.map(t => TrustAuditDetails(t.value)),
        prepopulatedEmailSource.map(source => EmailAuditDetails(subscriptionDetails.emailAddress.value, source)),
        if (subscriptionDetails.addressSource =!= AddressSource.ManuallyEntered) Some(auditAddress) else None,
        if (subscriptionDetails.contactNameSource =!= ContactNameSource.ManuallyEntered)
          Some(subscriptionDetails.contactName.value)
        else None
      )
    }

    val manuallyEnteredData =
      ManuallyEnteredData(
        if (subscriptionDetails.contactNameSource === ContactNameSource.ManuallyEntered)
          Some(subscriptionDetails.contactName.value)
        else None,
        if (prepopulatedEmailSource.isDefined)
          None
        else
          Some(subscriptionDetails.emailAddress.value),
        if (subscriptionDetails.addressSource === AddressSource.ManuallyEntered) Some(auditAddress) else None
      )

    val event = SubscriptionRequestEvent(prePopulatedUserData, manuallyEnteredData)

    sendEvent(
      "subscriptionRequest",
      event,
      hc.toAuditTags("subscription-request", path)
    )

  }

  override def sendRegistrationContactNameChangedEvent(
    oldContactName: IndividualName,
    newContactName: IndividualName,
    path: String
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Unit = {
    val detail = RegistrationContactNameChangedEvent(
      s"${oldContactName.firstName} ${oldContactName.lastName}",
      s"${newContactName.firstName} ${newContactName.lastName}"
    )

    sendEvent(
      "registrationContactNameChanged",
      detail,
      hc.toAuditTags("registration-contact-name-changed", path)
    )
  }

  override def sendRegistrationContactAddressChangedEvent(
    oldContactAddress: Address,
    newContactAddress: Address,
    isManuallyEnteredAddress: Boolean,
    path: String
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Unit = {

    val source = if (isManuallyEnteredAddress) "manual-entry" else "postcode-lookup"

    val detail: RegistrationContactAddressChangedEvent = RegistrationContactAddressChangedEvent(
      toAuditAddress(oldContactAddress),
      toAuditAddress(newContactAddress),
      source
    )

    sendEvent(
      "registrationContactAddressChanged",
      detail,
      hc.toAuditTags("registration-contact-address-changed", path)
    )
  }

  override def sendRegistrationChangeEmailAddressAttemptedEvent(
    oldEmailAddress: String,
    newEmailAddress: String,
    path: String
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Unit = {

    val detail = RegistrationChangeEmailAttemptedEvent(
      oldEmailAddress,
      newEmailAddress
    )

    sendEvent(
      "registrationChangeEmailAddressAttempted",
      detail,
      hc.toAuditTags("registration-change-email-address-attempted", path)
    )
  }

  override def sendRegistrationChangeEmailVerifiedEvent(
    oldEmailAddress: String,
    newEmailAddress: String,
    path: String
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Unit = {
    val detail = RegistrationChangeEmailAddressVerifiedEvent(
      oldEmailAddress,
      newEmailAddress
    )

    sendEvent(
      "registrationChangeEmailAddressVerified",
      detail,
      hc.toAuditTags("registration-change-email-address-verified", path)
    )
  }

  override def sendRegistrationSetupEmailAttemptedEvent(
    emailAddress: String,
    path: String
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Unit = {
    val detail = RegistrationSetupEmailAttemptedEvent(
      emailAddress
    )

    sendEvent(
      "registrationSetupEmailAddressAttempted",
      detail,
      hc.toAuditTags("registration-setup-email-address-attempted", path)
    )
  }

  override def sendRegistrationSetupEmailVerifiedEvent(
    emailAddress: String,
    path: String
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Unit = {
    val detail = RegistrationSetupEmailVerifiedEvent(
      emailAddress
    )

    sendEvent(
      "registrationSetupEmailAddressVerified",
      detail,
      hc.toAuditTags("registration-setup-email-address-verified", path)
    )
  }

  override def sendRegistrationRequestEvent(
    registrationDetails: RegistrationDetails,
    path: String
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Unit = {
    val prepopulatedEmailSource =
      if (registrationDetails.emailSource === EmailSource.BusinessPartnerRecord)
        Some("ETMP business partner record")
      else if (registrationDetails.emailSource === EmailSource.GovernmentGateway)
        Some("government-gateway")
      else
        None

    val prePopulatedUserData =
      RegistrationPrePopulatedUserData(
        "CGT",
        prepopulatedEmailSource.map(source => EmailAuditDetails(registrationDetails.emailAddress.value, source))
      )

    val manuallyEnteredData =
      RegistrationManuallyEnteredData(
        s"${registrationDetails.name.firstName} ${registrationDetails.name.lastName}",
        if (prepopulatedEmailSource.isEmpty) Some(registrationDetails.emailAddress.value) else None,
        toAuditAddress(registrationDetails.address)
      )

    val event = RegistrationRequestEvent(prePopulatedUserData, manuallyEnteredData)

    sendEvent(
      "registrationRequest",
      event,
      hc.toAuditTags("registration-request", path)
    )
  }

  override def sendSubscribedChangeContactNameEvent(
    oldContactName: String,
    newContactName: String,
    cgtReference: String,
    agentReferenceNumber: Option[AgentReferenceNumber],
    path: String
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Unit = {
    val detail = SubscribedContactNameChangedEvent(
      oldContactName,
      newContactName,
      cgtReference,
      agentReferenceNumber.isDefined,
      agentReferenceNumber.map(_.value)
    )

    sendEvent(
      "contactNameChanged",
      detail,
      hc.toAuditTags("contact-name-changed", path)
    )

  }

  override def sendSubscribedChangeEmailAddressAttemptedEvent(
    oldEmailAddress: String,
    newEmailAddress: String,
    cgtReference: String,
    agentReferenceNumber: Option[AgentReferenceNumber],
    path: String
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Unit = {
    val detail = SubscribedChangeEmailAddressAttemptedEvent(
      oldEmailAddress,
      newEmailAddress,
      cgtReference,
      agentReferenceNumber.isDefined,
      agentReferenceNumber.map(_.value)
    )

    sendEvent(
      "changeEmailAddressAttempted",
      detail,
      hc.toAuditTags("change-email-address-attempted", path)
    )

  }

  override def sendSubscribedChangeEmailAddressVerifiedEvent(
    oldEmailAddress: String,
    newEmailAddress: String,
    cgtReference: String,
    agentReferenceNumber: Option[AgentReferenceNumber],
    path: String
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Unit = {
    val detail = SubscribedChangeEmailAddressVerifiedEvent(
      oldEmailAddress,
      newEmailAddress,
      cgtReference,
      agentReferenceNumber.isDefined,
      agentReferenceNumber.map(_.value)
    )

    sendEvent(
      "changeEmailAddressVerified",
      detail,
      hc.toAuditTags("change-email-address-verified", path)
    )

  }

  override def sendSubscribedContactAddressChangedEvent(
    oldContactAddress: Address,
    newContactAddress: Address,
    isManuallyEnteredAddress: Boolean,
    cgtReference: String,
    agentReferenceNumber: Option[AgentReferenceNumber],
    path: String
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Unit = {

    val source = if (isManuallyEnteredAddress) "manual-entry" else "postcode-lookup"

    val detail = SubscribedContactAddressChangedEvent(
      toAuditAddress(oldContactAddress),
      toAuditAddress(newContactAddress),
      source,
      cgtReference,
      agentReferenceNumber.isDefined,
      agentReferenceNumber.map(_.value)
    )
    sendEvent(
      "contactAddressChanged",
      detail,
      hc.toAuditTags("contact-address-changed", path)
    )
  }

  override def sendAccessWithWrongGGAccountEvent(
    ggCredId: GGCredId,
    path: String
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Unit = {
    val detail = WrongGGAccountEvent(None, ggCredId.value)

    sendEvent(
      "accessWithWrongGGAccount",
      detail,
      hc.toAuditTags("access-with-wrong-gg-account", path)
    )
  }
}
