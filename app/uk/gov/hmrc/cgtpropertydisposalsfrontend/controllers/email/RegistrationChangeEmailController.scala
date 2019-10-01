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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.email

import java.util.UUID

import com.google.inject.{Inject, Singleton}
import play.api.mvc.{Call, MessagesControllerComponents, Result}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.RegistrationStatus.RegistrationReady
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.UUIDGenerator
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.EmailVerificationService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging
import uk.gov.hmrc.cgtpropertydisposalsfrontend.{controllers, views}
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.ExecutionContext

@Singleton
class RegistrationChangeEmailController @Inject()(
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionStore,
  val emailVerificationService: EmailVerificationService,
  val uuidGenerator: UUIDGenerator,
  val errorHandler: ErrorHandler,
  cc: MessagesControllerComponents,
  val enterEmailPage: views.html.email.enter_email,
  val checkYourInboxPage: views.html.email.check_your_inbox,
  val emailVerifiedPage: views.html.email.email_verified
)(implicit val viewConfig: ViewConfig, val ec: ExecutionContext)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with Logging
    with SessionUpdates
    with EmailController[RegistrationReady,RegistrationReady] {

  override val isAmendJourney: Boolean = false

  override def validJourney(
    request: RequestWithSessionData[_]
  ): Either[Result, (SessionData, RegistrationReady)] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some((sessionData, r: RegistrationReady)) => Right(sessionData -> r)
      case _                                         => Left(Redirect(controllers.routes.StartController.start()))
    }

  override def validVerificationCompleteJourney(request: RequestWithSessionData[_]): Either[Result, (SessionData, RegistrationReady)] =
    validJourney(request)

  override def updateEmail(journey: RegistrationReady, email: Email): RegistrationReady =
    RegistrationReady(journey.name, journey.address, email)

  override def name(journeyStatus: RegistrationReady): Either[TrustName, Name] =
    Right(journeyStatus.name)
  override lazy protected val backLinkCall: Option[Call] = Some(
    controllers.routes.RegistrationController.checkYourAnswers()
  )
  override lazy protected val enterEmailCall: Call          = routes.RegistrationChangeEmailController.enterEmail()
  override lazy protected val enterEmailSubmitCall: Call    = routes.RegistrationChangeEmailController.enterEmailSubmit()
  override lazy protected val checkYourInboxCall: Call      = routes.RegistrationChangeEmailController.checkYourInbox()
  override lazy protected val verifyEmailCall: UUID => Call = routes.RegistrationChangeEmailController.verifyEmail
  override lazy protected val emailVerifiedCall: Call       = routes.RegistrationChangeEmailController.emailVerified()

  override lazy protected val emailVerifiedContinueCall: Call =
    controllers.routes.RegistrationController.checkYourAnswers()
}
