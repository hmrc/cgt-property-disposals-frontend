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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.email

import cats.data.EitherT
import cats.instances.future._
import com.google.inject.{Inject, Singleton}
import play.api.mvc.{Call, MessagesControllerComponents, Request, Result}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{EmailController, SessionUpdates}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.RegistrationStatus.RegistrationReady
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.email.EmailJourneyType.Onboarding.ChangingRegistrationEmail
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.UUIDGenerator
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.ContactName
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.audit.{RegistrationChangeEmailAddressVerifiedEvent, RegistrationChangeEmailAttemptedEvent}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.email.{Email, EmailSource}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.{AuditService, EmailVerificationService}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging
import uk.gov.hmrc.cgtpropertydisposalsfrontend.{controllers, views}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import java.util.UUID
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class RegistrationChangeEmailController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionStore,
  val emailVerificationService: EmailVerificationService,
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
    with EmailController[ChangingRegistrationEmail] {

  override def validJourney(
    request: RequestWithSessionData[_]
  ): Either[Result, (SessionData, ChangingRegistrationEmail)] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some((sessionData, r: RegistrationReady)) =>
        Right(sessionData -> ChangingRegistrationEmail(r))
      case _                                         => Left(Redirect(controllers.routes.StartController.start()))
    }

  override def validVerificationCompleteJourney(
    request: RequestWithSessionData[_]
  ): Either[Result, (SessionData, ChangingRegistrationEmail)] =
    validJourney(request)

  override def updateEmail(
    changingRegistrationEmail: ChangingRegistrationEmail,
    email: Email
  )(implicit
    hc: HeaderCarrier,
    request: Request[_]
  ): EitherT[Future, Error, JourneyStatus] =
    EitherT.rightT[Future, Error](
      changingRegistrationEmail.journey.copy(
        registrationDetails = changingRegistrationEmail.journey.registrationDetails.copy(
          emailAddress = email,
          emailSource = EmailSource.ManuallyEntered
        )
      )
    )

  override def auditEmailVerifiedEvent(
    changingRegistrationEmail: ChangingRegistrationEmail,
    email: Email
  )(implicit hc: HeaderCarrier, request: Request[_]): Unit =
    auditService.sendEvent(
      "registrationChangeEmailAddressVerified",
      RegistrationChangeEmailAddressVerifiedEvent(
        changingRegistrationEmail.journey.registrationDetails.emailAddress.value,
        email.value
      ),
      "registration-change-email-address-verified"
    )

  override def auditEmailChangeAttempt(
    changingRegistrationEmail: ChangingRegistrationEmail,
    email: Email
  )(implicit hc: HeaderCarrier, request: Request[_]): Unit =
    auditService.sendEvent(
      "registrationChangeEmailAddressAttempted",
      RegistrationChangeEmailAttemptedEvent(
        changingRegistrationEmail.journey.registrationDetails.emailAddress.value,
        email.value
      ),
      "registration-change-email-address-attempted"
    )

  override def name(
    changingRegistrationEmail: ChangingRegistrationEmail
  ): ContactName =
    ContactName(
      changingRegistrationEmail.journey.registrationDetails.name.makeSingleName
    )

  override lazy protected val backLinkCall: Option[Call] = Some(
    controllers.onboarding.routes.RegistrationController.checkYourAnswers()
  )

  override lazy protected val enterEmailCall: Call          =
    routes.RegistrationChangeEmailController.enterEmail()
  override lazy protected val enterEmailSubmitCall: Call    =
    routes.RegistrationChangeEmailController.enterEmailSubmit()
  override lazy protected val checkYourInboxCall: Call      =
    routes.RegistrationChangeEmailController.checkYourInbox()
  override lazy protected val verifyEmailCall: UUID => Call =
    routes.RegistrationChangeEmailController.verifyEmail
  override lazy protected val emailVerifiedCall: Call       =
    routes.RegistrationChangeEmailController.emailVerified()

  override lazy protected val emailVerifiedContinueCall: Call =
    controllers.onboarding.routes.RegistrationController.checkYourAnswers()
}
