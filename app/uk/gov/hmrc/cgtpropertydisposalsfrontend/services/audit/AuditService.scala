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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.services.audit

import com.google.inject.{ImplementedBy, Inject, Singleton}
import play.api.libs.json.{JsValue, Json, Writes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.audit._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.bpr.UnsuccessfulNameMatchAttempts
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.bpr.UnsuccessfulNameMatchAttempts.NameMatchDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.GGCredId
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.IndividualName
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{RegistrationDetails, SubscriptionDetails}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.audit.AuditExtensions._
import uk.gov.hmrc.play.audit.http.connector.AuditConnector
import uk.gov.hmrc.play.audit.model.ExtendedDataEvent

import scala.concurrent.ExecutionContext

@ImplementedBy(classOf[AuditServiceImpl])
trait AuditService {
  def sendHandOffToIvEvent(ggCredId: GGCredId, redirectUrl: String)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext
  ): Unit

  def sendBusinessPartnerRecordNameMatchAttemptEvent[A <: NameMatchDetails: Writes](
    attemptsMade: Option[UnsuccessfulNameMatchAttempts[A]],
    nameMatchDetails: NameMatchDetails,
    path: String
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
    isGGAuthEmail: Option[Boolean],
    path: String
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Unit

  def sendSubscriptionConfirmationEmailSentEvent(
    emailAddress: String,
    cgtReference: String,
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
    path: String
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Unit

  def sendSubscribedChangeEmailAddressAttemptedEvent(
    oldEmailAddress: String,
    newEmailAddress: String,
    cgtReference: String,
    path: String
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Unit

  def sendSubscribedChangeEmailAddressVerifiedEvent(
    oldEmailAddress: String,
    newEmailAddress: String,
    cgtReference: String,
    path: String
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Unit

  def sendSubscribedContactAddressChangedEvent(
    oldContactAddress: Address,
    newContactAddress: Address,
    isManuallyEntered: Boolean,
    cgtReference: String,
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

}

@Singleton
class AuditServiceImpl @Inject()(auditConnector: AuditConnector) extends AuditService with Logging {

  private def sendEvent(auditSource: String, auditType: String, detail: JsValue, tags: Map[String, String])(
    implicit ec: ExecutionContext
  ): Unit = {
    val extendedDataEvent = ExtendedDataEvent(
      auditSource = auditSource,
      auditType   = auditType,
      detail      = detail,
      tags        = tags
    )
    auditConnector.sendExtendedEvent(extendedDataEvent)
  }

  override def sendHandOffToIvEvent(
    ggCredId: GGCredId,
    redirectUrl: String
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Unit = {
    val detail = HandOffTIvEvent(ggCredId.value, redirectUrl)

    sendEvent(
      "cgt-property-disposals",
      "handOffToIv",
      Json.toJson(detail),
      hc.toAuditTags("handoff-to-iv", redirectUrl)
    )
  }

  override def sendBusinessPartnerRecordNameMatchAttemptEvent[A <: NameMatchDetails: Writes](
    attemptsMade: Option[UnsuccessfulNameMatchAttempts[A]],
    nameMatchDetails: NameMatchDetails,
    path: String
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Unit = {
    val detailOfAttempts = attemptsMade match {
      case Some(attempts) =>
        nameMatchDetails match {
          case NameMatchDetails.IndividualNameMatchDetails(name, sautr) =>
            Some(
              BusinessPartnerRecordNameMatchAttemptEvent(
                attempts.unsuccessfulAttempts,
                attempts.maximumAttempts,
                None,
                Some(IndividualNameWithSaUtrAuditDetails(name.firstName, name.lastName, sautr.value))
              )
            )
          case NameMatchDetails.TrustNameMatchDetails(name, trn) =>
            Some(
              BusinessPartnerRecordNameMatchAttemptEvent(
                attempts.unsuccessfulAttempts,
                attempts.maximumAttempts,
                Some(TrustNameWithTrnAuditDetails(name.value, trn.value)),
                None
              )
            )
        }
      case None => None
    }

    detailOfAttempts match {
      case Some(details) =>
        sendEvent(
          "cgt-property-disposals",
          "businessPartnerRecordNameMatchAttempt",
          Json.toJson(details),
          hc.toAuditTags("business-partner-record-name-match-attempt", path)
        )
      case None =>
        logger.warn("Could not send audit event for BusinessPartnerRecordNameMatchAttemptEvent")
    }

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
      "cgt-property-disposals",
      "subscriptionChangeEmailAddressAttempted",
      Json.toJson(detail),
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
      "cgt-property-disposals",
      "subscriptionChangeEmailAddressVerified",
      Json.toJson(detail),
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
      "cgt-property-disposals",
      "subscriptionContactNameChanged",
      Json.toJson(detail),
      hc.toAuditTags("subscription-contact-name-changed", path)
    )

  }

  override def sendSubscriptionContactAddressChangedEvent(
    oldContactAddress: Address,
    newContactAddress: Address,
    isManuallyEnteredAddress: Boolean,
    path: String
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Unit = {

    val source = if (isManuallyEnteredAddress) "manual" else "postcode-lookup"

    val detail = SubscriptionContactAddressChangedEvent(
      oldContactAddress,
      newContactAddress,
      source
    )

    sendEvent(
      "cgt-property-disposals",
      "subscriptionContactAddressChanged",
      Json.toJson(detail),
      hc.toAuditTags("subscription-contact-address-changed", path)
    )

  }

  override def sendSubscriptionRequestEvent(
    subscriptionDetails: SubscriptionDetails,
    isGGAuthEmail: Option[Boolean],
    path: String
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Unit =
    isGGAuthEmail match {
      case Some(ggEmail) => {
        val origin = if (ggEmail) "government gateway" else "ETMP business partner record"

        val prepop = subscriptionDetails.name match {
          case Left(trust) =>
            PrePopulatedUserData(
              "CGT",
              subscriptionDetails.sapNumber,
              None,
              Some(TrustAuditDetails(trust.value)),
              EmailAuditDetails(subscriptionDetails.emailAddress.value, origin)
            )
          case Right(individual) =>
            PrePopulatedUserData(
              "CGT",
              subscriptionDetails.sapNumber,
              Some(IndividualAuditDetails(individual.firstName, individual.lastName)),
              None,
              EmailAuditDetails(subscriptionDetails.emailAddress.value, origin)
            )
        }

        val manual = ManuallyEnteredData(
          subscriptionDetails.contactName.value,
          subscriptionDetails.emailAddress.value,
          subscriptionDetails.address
        )

        val detail = SubscriptionRequestEvent(
          prepop,
          manual
        )

        sendEvent(
          "cgt-property-disposals",
          "subscriptionRequest",
          Json.toJson(detail),
          hc.toAuditTags("subscription-request", path)
        )

      }
      case None => logger.warn("Could not send subscription request event")
    }

  override def sendSubscriptionConfirmationEmailSentEvent(
    emailAddress: String,
    cgtReference: String,
    path: String
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Unit = {

    val detail = SubscriptionConfirmationEmailSentEvent(
      emailAddress,
      cgtReference
    )

    sendEvent(
      "cgt-property-disposals",
      "subscriptionConfirmationEmailSent",
      Json.toJson(detail),
      hc.toAuditTags("subscription-confirmation-email-sent", path)
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
      "cgt-property-disposals",
      "registrationContactNameChanged",
      Json.toJson(detail),
      hc.toAuditTags("registration-contact-name-changed", path)
    )
  }

  override def sendRegistrationContactAddressChangedEvent(
    oldContactAddress: Address,
    newContactAddress: Address,
    isManuallyEnteredAddress: Boolean,
    path: String
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Unit = {

    val source = if (isManuallyEnteredAddress) "manual" else "postcode-lookup"

    val details: RegistrationContactAddressChangedEvent = RegistrationContactAddressChangedEvent(
      oldContactAddress,
      newContactAddress,
      source
    )

    sendEvent(
      "cgt-property-disposals",
      "registrationContactAddressChanged",
      Json.toJson(details),
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
      "cgt-property-disposals",
      "registrationChangeEmailAddressAttempted",
      Json.toJson(detail),
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
      "cgt-property-disposals",
      "registrationChangeEmailAddressVerified",
      Json.toJson(detail),
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
      "cgt-property-disposals",
      "registrationSetupEmailAddressAttempted",
      Json.toJson(detail),
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
      "cgt-property-disposals",
      "registrationSetupEmailAddressVerified",
      Json.toJson(detail),
      hc.toAuditTags("registration-setup-email-address-verified", path)
    )
  }

  override def sendRegistrationRequestEvent(
    registrationDetails: RegistrationDetails,
    path: String
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Unit = {

    val detail = RegistrationRequestEvent(
      RegistrationPrePopulatedUserData(
        "CGT",
        EmailAuditDetails(
          registrationDetails.emailAddress.value,
          "government gateway"
        )
      ),
      RegistrationManuallyEnteredData(
        s"${registrationDetails.name.firstName} ${registrationDetails.name.lastName}",
        registrationDetails.emailAddress.value,
        registrationDetails.address
      )
    )

    sendEvent(
      "cgt-property-disposals",
      "registrationRequest",
      Json.toJson(detail),
      hc.toAuditTags("registration-request", path)
    )
  }

  override def sendSubscribedChangeContactNameEvent(
    oldContactName: String,
    newContactName: String,
    cgtReference: String,
    path: String
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Unit = {
    val detail = SubscribedContactNameChangedEvent(
      oldContactName,
      newContactName,
      cgtReference
    )

    sendEvent(
      "cgt-property-disposals",
      "contactNameChanged",
      Json.toJson(detail),
      hc.toAuditTags("contact-name-changed", path)
    )

  }

  override def sendSubscribedChangeEmailAddressAttemptedEvent(
    oldEmailAddress: String,
    newEmailAddress: String,
    cgtReference: String,
    path: String
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Unit = {
    val detail = SubscribedChangeEmailAddressAttemptedEvent(
      oldEmailAddress,
      newEmailAddress,
      cgtReference
    )

    sendEvent(
      "cgt-property-disposals",
      "changeEmailAddressAttempted",
      Json.toJson(detail),
      hc.toAuditTags("change-email-address-attempted", path)
    )

  }

  override def sendSubscribedChangeEmailAddressVerifiedEvent(
    oldEmailAddress: String,
    newEmailAddress: String,
    cgtReference: String,
    path: String
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Unit = {
    val detail = SubscribedChangeEmailAddressVerifiedEvent(
      oldEmailAddress,
      newEmailAddress,
      cgtReference
    )

    sendEvent(
      "cgt-property-disposals",
      "changeEmailAddressVerified",
      Json.toJson(detail),
      hc.toAuditTags("change-email-address-verified", path)
    )

  }

  override def sendSubscribedContactAddressChangedEvent(
    oldContactAddress: Address,
    newContactAddress: Address,
    source: Boolean,
    cgtReference: String,
    path: String
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Unit = {
    val detail = SubscribedContactAddressChangedEvent(
      oldContactAddress,
      newContactAddress,
      source,
      cgtReference
    )

    sendEvent(
      "cgt-property-disposals",
      "contactAddressChanged",
      Json.toJson(detail),
      hc.toAuditTags("contact-address-changed", path)
    )
  }

}
