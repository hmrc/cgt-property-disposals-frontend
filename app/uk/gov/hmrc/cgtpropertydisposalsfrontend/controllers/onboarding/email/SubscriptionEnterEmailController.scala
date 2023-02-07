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

import java.util.UUID
import cats.data.EitherT
import cats.instances.future._
import com.google.inject.{Inject, Singleton}
import play.api.mvc.{Call, MessagesControllerComponents, Request, Result}
import shapeless.{Lens, lens}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{EmailController, SessionUpdates}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.SubscriptionStatus.SubscriptionMissingData
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.email.EmailJourneyType.Onboarding.EnteringSubscriptionEmail
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.UUIDGenerator
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.ContactName
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.email.Email
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, JourneyStatus, SessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.{AuditService, EmailVerificationService}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging
import uk.gov.hmrc.cgtpropertydisposalsfrontend.{controllers, views}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class SubscriptionEnterEmailController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionStore,
  val emailVerificationService: EmailVerificationService,
  val uuidGenerator: UUIDGenerator,
  val errorHandler: ErrorHandler,
  val auditService: AuditService,
  cc: MessagesControllerComponents,
  val enterEmailPage: views.html.onboarding.email.enter_email,
  val checkYourInboxPage: views.html.onboarding.email.check_your_inbox,
  val emailVerifiedPage: views.html.onboarding.email.email_verified
)(implicit val viewConfig: ViewConfig, val ec: ExecutionContext)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with Logging
    with SessionUpdates
    with EmailController[EnteringSubscriptionEmail] {

  override def validJourney(
    request: RequestWithSessionData[_]
  ): Either[Result, (SessionData, EnteringSubscriptionEmail)] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some((sessionData, s: SubscriptionMissingData)) =>
        Right(sessionData -> EnteringSubscriptionEmail(s))
      case _                                               => Left(Redirect(controllers.routes.StartController.start()))
    }

  override def validVerificationCompleteJourney(
    request: RequestWithSessionData[_]
  ): Either[Result, (SessionData, EnteringSubscriptionEmail)] =
    validJourney(request)

  val subscriptionMissingDataEmailLens: Lens[SubscriptionMissingData, Option[Email]] =
    lens[SubscriptionMissingData].businessPartnerRecord.emailAddress

  override def updateEmail(
    enteringSubscriptionEmail: EnteringSubscriptionEmail,
    email: Email
  )(implicit
    hc: HeaderCarrier,
    request: Request[_]
  ): EitherT[Future, Error, JourneyStatus] =
    EitherT.rightT[Future, Error](
      subscriptionMissingDataEmailLens
        .set(enteringSubscriptionEmail.journey)(Some(email))
    )

  override def auditEmailVerifiedEvent(
    enteringSubscriptionEmail: EnteringSubscriptionEmail,
    email: Email
  )(implicit
    hc: HeaderCarrier,
    request: Request[_]
  ): Unit = ()

  override def auditEmailChangeAttempt(
    enteringSubscriptionEmail: EnteringSubscriptionEmail,
    email: Email
  )(implicit
    hc: HeaderCarrier,
    request: Request[_]
  ): Unit = ()

  override def name(
    enteringSubscriptionEmail: EnteringSubscriptionEmail
  ): ContactName =
    ContactName(
      enteringSubscriptionEmail.journey.businessPartnerRecord.name
        .fold(_.value, n => n.makeSingleName())
    )

  override lazy protected val backLinkCall: Option[Call]      = None
  override lazy protected val enterEmailCall: Call            =
    routes.SubscriptionEnterEmailController.enterEmail()
  override lazy protected val enterEmailSubmitCall: Call      =
    routes.SubscriptionEnterEmailController.enterEmailSubmit()
  override lazy protected val checkYourInboxCall: Call        =
    routes.SubscriptionEnterEmailController.checkYourInbox()
  override lazy protected val verifyEmailCall: UUID => Call   =
    routes.SubscriptionEnterEmailController.verifyEmail
  override lazy protected val emailVerifiedCall: Call         =
    routes.SubscriptionEnterEmailController.emailVerified()
  override lazy protected val emailVerifiedContinueCall: Call =
    controllers.routes.StartController.start()

}
