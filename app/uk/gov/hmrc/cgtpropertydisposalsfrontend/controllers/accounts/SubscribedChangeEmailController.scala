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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.accounts

import cats.data.EitherT
import cats.instances.future._
import cats.syntax.eq._
import com.google.inject.{Inject, Singleton}
import play.api.mvc.{Call, MessagesControllerComponents, Request, Result}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{EmailController, SessionUpdates}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.Subscribed
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.email.EmailJourneyType.ManagingSubscription.ChangingAccountEmail
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.UUIDGenerator
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.ContactName
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.SubscribedUpdateDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.audit.{SubscribedChangeEmailAddressAttemptedEvent, SubscribedChangeEmailAddressVerifiedEvent}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.email.Email
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, JourneyStatus, SessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.onboarding.SubscriptionService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.{AuditService, EmailVerificationService}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging
import uk.gov.hmrc.cgtpropertydisposalsfrontend.{controllers, views}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import java.util.UUID
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class SubscribedChangeEmailController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionStore,
  val emailVerificationService: EmailVerificationService,
  val subscriptionService: SubscriptionService,
  val auditService: AuditService,
  val uuidGenerator: UUIDGenerator,
  val errorHandler: ErrorHandler,
  cc: MessagesControllerComponents,
  val enterEmailPage: views.html.onboarding.email.enter_email,
  val checkYourInboxPage: views.html.onboarding.email.check_your_inbox,
  val emailVerifiedPage: views.html.onboarding.email.email_verified
)(implicit val viewConfig: ViewConfig, val ec: ExecutionContext)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with Logging
    with SessionUpdates
    with EmailController[ChangingAccountEmail] {

  override def validJourney(
    request: RequestWithSessionData[_]
  ): Either[Result, (SessionData, ChangingAccountEmail)] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some((sessionData, s: Subscribed)) =>
        Right(sessionData -> ChangingAccountEmail(s))
      case _                                  => Left(Redirect(controllers.routes.StartController.start()))
    }

  override def validVerificationCompleteJourney(
    request: RequestWithSessionData[_]
  ): Either[Result, (SessionData, ChangingAccountEmail)] =
    validJourney(request)

  override def updateEmail(
    changingAccountEmail: ChangingAccountEmail,
    email: Email
  )(implicit
    hc: HeaderCarrier,
    request: Request[_]
  ): EitherT[Future, Error, JourneyStatus] = {
    val journey                 = changingAccountEmail.journey
    val journeyWithUpdatedEmail =
      journey.subscribedDetails.copy(emailAddress = email)
    if (journey.subscribedDetails === journeyWithUpdatedEmail) {
      EitherT.pure[Future, Error](journey)
    } else {
      subscriptionService
        .updateSubscribedDetails(
          SubscribedUpdateDetails(
            journeyWithUpdatedEmail,
            journey.subscribedDetails
          )
        )
        .map(_ => journey.copy(journeyWithUpdatedEmail))
    }
  }

  override def auditEmailVerifiedEvent(
    changingAccountEmail: ChangingAccountEmail,
    email: Email
  )(implicit hc: HeaderCarrier, request: Request[_]): Unit = {
    val journey = changingAccountEmail.journey
    auditService.sendEvent(
      "changeEmailAddressVerified",
      SubscribedChangeEmailAddressVerifiedEvent(
        journey.subscribedDetails.emailAddress.value,
        email.value,
        journey.subscribedDetails.cgtReference.value,
        journey.agentReferenceNumber.isDefined,
        journey.agentReferenceNumber.map(_.value)
      ),
      "change-email-address-verified"
    )
  }

  override def auditEmailChangeAttempt(
    changingAccountEmail: ChangingAccountEmail,
    email: Email
  )(implicit hc: HeaderCarrier, request: Request[_]): Unit = {
    val journey = changingAccountEmail.journey
    auditService.sendEvent(
      "changeEmailAddressAttempted",
      SubscribedChangeEmailAddressAttemptedEvent(
        journey.subscribedDetails.emailAddress.value,
        email.value,
        journey.subscribedDetails.cgtReference.value,
        journey.agentReferenceNumber.isDefined,
        journey.agentReferenceNumber.map(_.value)
      ),
      "change-email-address-attempted"
    )
  }

  override def name(changingAccountEmail: ChangingAccountEmail): ContactName =
    changingAccountEmail.journey.subscribedDetails.contactName

  override lazy protected val backLinkCall: Option[Call]      = Some(
    controllers.accounts.routes.AccountController.manageYourDetails()
  )
  override lazy protected val enterEmailCall: Call            =
    routes.SubscribedChangeEmailController.enterEmail()
  override lazy protected val enterEmailSubmitCall: Call      =
    routes.SubscribedChangeEmailController.enterEmailSubmit()
  override lazy protected val checkYourInboxCall: Call        =
    routes.SubscribedChangeEmailController.checkYourInbox()
  override lazy protected val verifyEmailCall: UUID => Call   =
    routes.SubscribedChangeEmailController.verifyEmail
  override lazy protected val emailVerifiedCall: Call         =
    routes.SubscribedChangeEmailController.emailVerified()
  override lazy protected val emailVerifiedContinueCall: Call =
    controllers.accounts.routes.AccountController.contactEmailUpdated()

}
