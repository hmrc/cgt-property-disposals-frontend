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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.email

import java.util.UUID

import cats.data.EitherT
import cats.instances.future._
import com.google.inject.{Inject, Singleton}
import play.api.mvc.{Call, MessagesControllerComponents, Result}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{EmailController, SessionUpdates}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.UUIDGenerator
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.ContactName
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.RegistrationStatus.{IndividualMissingEmail, RegistrationReady}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.RegistrationDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.email.{Email, EmailSource}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.EmailVerificationService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.onboarding.OnboardingAuditService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging
import uk.gov.hmrc.cgtpropertydisposalsfrontend.{controllers, views}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class RegistrationEnterEmailController @Inject()(
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionStore,
  val emailVerificationService: EmailVerificationService,
  val auditService: OnboardingAuditService,
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
    with EmailController[IndividualMissingEmail, RegistrationReady] {

  override val isAmendJourney: Boolean      = false
  override val isSubscribedJourney: Boolean = false

  override def validJourney(request: RequestWithSessionData[_]): Either[Result, (SessionData, IndividualMissingEmail)] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some((sessionData, i: IndividualMissingEmail)) => Right(sessionData -> i)
      case _                                              => Left(Redirect(controllers.routes.StartController.start()))
    }

  override def validVerificationCompleteJourney(
    request: RequestWithSessionData[_]
  ): Either[Result, (SessionData, RegistrationReady)] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some((sessionData, r: RegistrationReady)) => Right(sessionData -> r)
      case _                                         => Left(Redirect(controllers.routes.StartController.start()))
    }

  override def updateEmail(journey: IndividualMissingEmail, email: Email)(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, RegistrationReady] =
    EitherT.rightT[Future, Error](
      RegistrationReady(
        RegistrationDetails(journey.name, email, journey.address, EmailSource.ManuallyEntered),
        journey.ggCredId
      )
    )

  override def auditEmailVerifiedEvent(journey: IndividualMissingEmail, email: Email)(
    implicit hc: HeaderCarrier
  ): Unit =
    auditService
      .sendRegistrationSetupEmailVerifiedEvent(email.value, routes.RegistrationEnterEmailController.emailVerified().url)

  override def auditEmailChangeAttempt(journey: IndividualMissingEmail, email: Email)(
    implicit hc: HeaderCarrier
  ): Unit =
    auditService
      .sendRegistrationSetupEmailAttemptedEvent(
        email.value,
        routes.RegistrationEnterEmailController.enterEmailSubmit().url
      )

  override def name(journeyStatus: IndividualMissingEmail): ContactName =
    ContactName(journeyStatus.name.makeSingleName())

  override lazy protected val backLinkCall: Option[Call]    = None
  override lazy protected val enterEmailCall: Call          = routes.RegistrationEnterEmailController.enterEmail()
  override lazy protected val enterEmailSubmitCall: Call    = routes.RegistrationEnterEmailController.enterEmailSubmit()
  override lazy protected val checkYourInboxCall: Call      = routes.RegistrationEnterEmailController.checkYourInbox()
  override lazy protected val verifyEmailCall: UUID => Call = routes.RegistrationEnterEmailController.verifyEmail
  override lazy protected val emailVerifiedCall: Call       = routes.RegistrationEnterEmailController.emailVerified()

  override lazy protected val emailVerifiedContinueCall: Call =
    controllers.onboarding.routes.RegistrationController.checkYourAnswers()
}
