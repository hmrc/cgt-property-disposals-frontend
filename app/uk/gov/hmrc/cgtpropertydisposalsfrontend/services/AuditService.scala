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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.services

import com.google.inject.{ImplementedBy, Inject, Singleton}
import play.api.libs.json.{Json, Writes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.SubscriptionDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.audit._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.bpr.UnsuccessfulNameMatchAttempts
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.bpr.UnsuccessfulNameMatchAttempts.NameMatchDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.GGCredId
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.audit.AuditExtensions._
import uk.gov.hmrc.play.audit.http.connector.{AuditConnector, AuditResult}
import uk.gov.hmrc.play.audit.model.ExtendedDataEvent

import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[AuditServiceImpl])
trait AuditService {
  def sendHandOffToIvEvent(ggCredId: GGCredId, redirectUrl: String)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext
  ): Future[AuditResult]

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

  def sendSubscriptionContactAddressChanged(
    oldContactAddress: Address,
    newContactAddress: Address,
    isManualAddress: Boolean,
    path: String
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Unit

  def sendSubscriptionRequestEvent(
    subscriptionDetails: SubscriptionDetails,
    isGGAuthEmail: Boolean,
    path: String
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Unit

  def sendSubscriptionConfirmationEmailSentEvent(
    emailAddress: String,
    cgtReference: String,
    path: String
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Unit

}

@Singleton
class AuditServiceImpl @Inject()(auditConnector: AuditConnector) extends AuditService with Logging {

  override def sendHandOffToIvEvent(
    ggCredId: GGCredId,
    redirectUrl: String
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[AuditResult] = {
    val detail = HandOffTIvEvent(ggCredId.value, redirectUrl)

    val extendedDataEvent = ExtendedDataEvent(
      auditSource = "cgt-property-disposals",
      auditType   = "handOffToIv",
      detail      = Json.toJson(detail),
      tags        = hc.toAuditTags("handoff-to-iv", redirectUrl)
    )
    auditConnector.sendExtendedEvent(extendedDataEvent)
  }

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  override def sendBusinessPartnerRecordNameMatchAttemptEvent[A <: NameMatchDetails: Writes](
    attemptsMade: Option[UnsuccessfulNameMatchAttempts[A]],
    nameMatchDetails: NameMatchDetails,
    path: String
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Unit = {
    val details = attemptsMade match {
      case Some(attempts) =>
        nameMatchDetails match {
          case NameMatchDetails.IndividualNameMatchDetails(name, sautr) =>
            Some(
              BusinessPartnerRecordNameMatchAttemptEvent(
                attempts.unsuccessfulAttempts,
                attempts.maximumAttempts,
                None,
                Some(IndividualNameEvent(name.firstName, name.lastName, sautr.value))
              )
            )
          case NameMatchDetails.TrustNameMatchDetails(name, trn) =>
            Some(
              BusinessPartnerRecordNameMatchAttemptEvent(
                attempts.unsuccessfulAttempts,
                attempts.maximumAttempts,
                Some(TrustNameEvent(name.value, trn.value)),
                None
              )
            )
        }
      case None =>
        None
    }

    details match {
      case Some(d) => {
        val extendedDataEvent = ExtendedDataEvent(
          auditSource = "cgt-property-disposals",
          auditType   = "businessPartnerRecordNameMatchAttempt",
          detail      = Json.toJson(d),
          tags        = hc.toAuditTags("business-partner-record-name-match-attempt", path)
        )
        auditConnector.sendExtendedEvent(extendedDataEvent)
      }
      case None => {
        logger.warn("Unable to send audit event for BusinessPartnerRecordNameMatchAttemptEvent")
      }
    }

  }

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
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

    val extendedDataEvent = ExtendedDataEvent(
      auditSource = "cgt-property-disposals",
      auditType   = "subscriptionChangeEmailAddressAttempted",
      detail      = Json.toJson(detail),
      tags        = hc.toAuditTags("subscription-change-email-address-attempted", path)
    )

    auditConnector.sendExtendedEvent(extendedDataEvent)
  }

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  override def sendSubscriptionChangeEmailAddressVerifiedEvent(
    oldEmailAddress: String,
    newEmailAddress: String,
    path: String
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Unit = {
    val detail = SubscriptionChangeEmailAddressAttemptedEvent(
      oldEmailAddress,
      newEmailAddress
    )

    val extendedDataEvent = ExtendedDataEvent(
      auditSource = "cgt-property-disposals",
      auditType   = "subscriptionChangeEmailAddressVerified",
      detail      = Json.toJson(detail),
      tags        = hc.toAuditTags("subscription-change-email-address-verified", path)
    )

    auditConnector.sendExtendedEvent(extendedDataEvent)
  }

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  def sendSubscriptionContactAddressChanged(
    oldContactAddress: Address,
    newContactAddress: Address,
    source: String,
    path: String
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Unit = {

    val detail = SubscriptionContactAddressChangedEvent(
      oldContactAddress,
      newContactAddress,
      source
    )

    val extendedDataEvent = ExtendedDataEvent(
      auditSource = "cgt-property-disposals",
      auditType   = "subscriptionContactAddressChanged",
      detail      = Json.toJson(detail),
      tags        = hc.toAuditTags("subscription-contact-address-changed", path)
    )

    auditConnector.sendExtendedEvent(extendedDataEvent)

  }
  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  override def sendSubscriptionRequestEvent(
    subscriptionDetails: SubscriptionDetails,
    isGGAuthEmail: Boolean,
    path: String
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Unit = {

    val origin = if (isGGAuthEmail) "government gateway" else "ETMP business partner record"

    val prepop = subscriptionDetails.name match {
      case Left(trust) =>
        PrePopulatedUserData(
          "CGT",
          subscriptionDetails.sapNumber,
          None,
          Some(Trust(trust.value)),
          EmailEvent(subscriptionDetails.emailAddress.value, origin)
        )
      case Right(individual) =>
        PrePopulatedUserData(
          "CGT",
          subscriptionDetails.sapNumber,
          Some(Individual(individual.firstName, individual.lastName)),
          None,
          EmailEvent(subscriptionDetails.emailAddress.value, origin)
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

    val extendedDataEvent = ExtendedDataEvent(
      auditSource = "cgt-property-disposals",
      auditType   = "subscriptionRequest",
      detail      = Json.toJson(detail),
      tags        = hc.toAuditTags("subscription-request", path)
    )

    auditConnector.sendExtendedEvent(extendedDataEvent)

  }

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  override def sendSubscriptionConfirmationEmailSentEvent(
    emailAddress: String,
    cgtReference: String,
    path: String
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Unit = {

    val detail = SubscriptionConfirmationEmailSentEvent(
      emailAddress,
      cgtReference
    )

    val extendedDataEvent = ExtendedDataEvent(
      auditSource = "cgt-property-disposals",
      auditType   = "subscriptionConfirmationEmailSent",
      detail      = Json.toJson(detail),
      tags        = hc.toAuditTags("subscription-confirmation-email-sent", path)
    )

    auditConnector.sendExtendedEvent(extendedDataEvent)
  }

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  override def sendSubscriptionContactNameChangedEvent(
    oldContactName: String,
    newContactName: String,
    path: String
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Unit = {

    val detail = SubscriptionContactNameChangedEvent(
      oldContactName,
      newContactName
    )

    val extendedDataEvent = ExtendedDataEvent(
      auditSource = "cgt-property-disposals",
      auditType   = "subscriptionContactNameChanged",
      detail      = Json.toJson(detail),
      tags        = hc.toAuditTags("subscription-contact-name-changed", path)
    )

    auditConnector.sendExtendedEvent(extendedDataEvent)

  }

}
