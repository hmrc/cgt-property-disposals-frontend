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

import cats.syntax.eq._
import com.google.inject.{ImplementedBy, Inject, Singleton}
import play.api.libs.json.{Json, Writes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.routes
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.audit._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.bpr.UnsuccessfulNameMatchAttempts.NameMatchDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.GGCredId
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.IndividualName
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Email, RegistrationDetails, SubscriptionDetails}
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

  def sendBusinessPartnerRecordNameMatchAttemptEvent(
    attemptsMade: Int,
    maxAttemptsMade: Int,
    nameMatchDetails: NameMatchDetails
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
    ggEmail: Option[Email],
    bprEmail: Option[Email],
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
    ggEmail: Option[Email],
    path: String
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Unit

}

@Singleton
class AuditServiceImpl @Inject()(auditConnector: AuditConnector) extends AuditService with Logging {

  private def sendEvent[A](auditType: String, detail: A, tags: Map[String, String])(
    implicit ec: ExecutionContext,
    writes: Writes[A]
  ): Unit = {
    val extendedDataEvent = ExtendedDataEvent(
      auditSource = "cgy-property-disposals",
      auditType   = auditType,
      detail      = Json.toJson(detail),
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
      "handOffToIv",
      detail,
      hc.toAuditTags("handoff-to-iv", redirectUrl)
    )
  }

  override def sendBusinessPartnerRecordNameMatchAttemptEvent(
    attemptsMade: Int,
    maxAttempts: Int,
    nameMatchDetails: NameMatchDetails
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Unit = {

    val path = nameMatchDetails match {
      case NameMatchDetails.IndividualNameMatchDetails(name, sautr) =>
        routes.InsufficientConfidenceLevelController.enterSautrAndNameSubmit().url
      case NameMatchDetails.TrustNameMatchDetails(name, trn) =>
        routes.DeterminingIfOrganisationIsTrustController.enterTrnSubmit().url
    }

    val detail = nameMatchDetails match {
      case NameMatchDetails.IndividualNameMatchDetails(name, sautr) =>
        Some(
          BusinessPartnerRecordNameMatchAttemptEvent(
            attemptsMade,
            maxAttempts,
            None,
            Some(IndividualNameWithSaUtrAuditDetails(name.firstName, name.lastName, sautr.value))
          )
        )
      case NameMatchDetails.TrustNameMatchDetails(name, trn) =>
        Some(
          BusinessPartnerRecordNameMatchAttemptEvent(
            attemptsMade,
            maxAttempts,
            Some(TrustNameWithTrnAuditDetails(name.value, trn.value)),
            None
          )
        )
    }

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

    val source = if (isManuallyEnteredAddress) "manual" else "postcode-lookup"

    val detail = SubscriptionContactAddressChangedEvent(
      oldContactAddress,
      newContactAddress,
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
    ggEmail: Option[Email],
    bprEmail: Option[Email],
    path: String
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Unit = {

    def isPrepop(ggEmail: Option[Email], bprEmail: Option[Email], email: Email): (Boolean, String) =
      bprEmail match {
        case Some(be) =>
          if (be === email) (true, "ETMP business partner record")
          else {
            ggEmail match {
              case Some(ge) => if (ge === email) (true, "government-gateway") else (false, "")
              case None     => (false, "")
            }
          }
        case None =>
          ggEmail match {
            case Some(ge) => if (ge === email) (true, "government-gateway") else (false, "")
            case None     => (false, "")
          }
      }

    val (prepop, origin) = isPrepop(ggEmail, bprEmail, subscriptionDetails.emailAddress)

    val detail: Either[PrePopulatedUserData, ManuallyEnteredData] = if (prepop) {
      subscriptionDetails.name match {
        case Left(trust) =>
          Left(
            PrePopulatedUserData(
              "CGT",
              subscriptionDetails.sapNumber,
              None,
              Some(TrustAuditDetails(trust.value)),
              EmailAuditDetails(subscriptionDetails.emailAddress.value, origin)
            )
          )

        case Right(individual) =>
          Left(
            PrePopulatedUserData(
              "CGT",
              subscriptionDetails.sapNumber,
              Some(IndividualAuditDetails(individual.firstName, individual.lastName)),
              None,
              EmailAuditDetails(subscriptionDetails.emailAddress.value, origin)
            )
          )
      }
    } else {
      Right(
        ManuallyEnteredData(
          subscriptionDetails.contactName.value,
          subscriptionDetails.emailAddress.value,
          subscriptionDetails.address
        )
      )
    }

    val result = detail match {
      case Left(prePopulatedUserData) => SubscriptionRequestEvent(Some(prePopulatedUserData), None)
      case Right(manuallyEnteredData) => SubscriptionRequestEvent(None, Some(manuallyEnteredData))
    }

    sendEvent(
      "subscriptionRequest",
      result,
      hc.toAuditTags("subscription-request", path)
    )

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
      "subscriptionConfirmationEmailSent",
      detail,
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

    val source = if (isManuallyEnteredAddress) "manual" else "postcode-lookup"

    val detail: RegistrationContactAddressChangedEvent = RegistrationContactAddressChangedEvent(
      oldContactAddress,
      newContactAddress,
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
    ggEmail: Option[Email],
    path: String
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Unit = {

    def isPrepop(ggEmail: Option[Email], email: Email): (Boolean, String) =
      ggEmail match {
        case Some(ge) =>
          if (ge === email) (true, "government-gateway")
          else {
            (false, "")
          }
        case None =>
          (false, "")
      }

    val (prepop, origin) = isPrepop(ggEmail, registrationDetails.emailAddress)

    val detail: Either[RegistrationPrePopulatedUserData, RegistrationManuallyEnteredData] = if (prepop) {
      Left(
        RegistrationPrePopulatedUserData(
          "CGT",
          EmailAuditDetails(
            registrationDetails.emailAddress.value,
            origin
          )
        )
      )
    } else {
      Right(
        RegistrationManuallyEnteredData(
          s"${registrationDetails.name.firstName} ${registrationDetails.name.lastName}",
          registrationDetails.emailAddress.value,
          registrationDetails.address
        )
      )
    }

    val result = detail match {
      case Left(prePopulatedUserData) => RegistrationRequestEvent(Some(prePopulatedUserData), None)
      case Right(manuallyEnteredData) => RegistrationRequestEvent(None, Some(manuallyEnteredData))
    }

    sendEvent(
      "registrationRequest",
      result,
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
      "contactNameChanged",
      detail,
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
      "changeEmailAddressAttempted",
      detail,
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
      "changeEmailAddressVerified",
      detail,
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
      "contactAddressChanged",
      detail,
      hc.toAuditTags("contact-address-changed", path)
    )
  }

}
