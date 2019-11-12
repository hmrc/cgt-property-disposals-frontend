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

import cats.data.EitherT
import cats.instances.future._
import com.google.inject.{Inject, Singleton}
import play.api.mvc.{Call, MessagesControllerComponents, Result}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.Subscribed
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.UUIDGenerator
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.ContactName
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Email, Error, SessionData, SubscriptionDetail}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.{EmailVerificationService, SubscriptionService}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging
import uk.gov.hmrc.cgtpropertydisposalsfrontend.{controllers, views}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class SubscribedChangeEmailController @Inject()(
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionStore,
  val emailVerificationService: EmailVerificationService,
  val subscriptionService: SubscriptionService,
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
    with EmailController[Subscribed, Subscribed] {

  override val isAmendJourney: Boolean = true

  override def validJourney(request: RequestWithSessionData[_]): Either[Result, (SessionData, Subscribed)] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some((sessionData, s: Subscribed)) => Right(sessionData -> s)
      case _                                  => Left(Redirect(controllers.routes.StartController.start()))
    }

  override def validVerificationCompleteJourney(
    request: RequestWithSessionData[_]
  ): Either[Result, (SessionData, Subscribed)] =
    validJourney(request)

  override def updateEmail(journey: Subscribed, email: Email)(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, Subscribed] = {
    val journeyWithUpdatedEmail = journey.subscribedDetails.copy(emailAddress = email)
    subscriptionService
      .updateSubscribedDetails(journeyWithUpdatedEmail)
      .map(_ => journey.copy(journeyWithUpdatedEmail))
  }

  override def name(journeyStatus: Subscribed): ContactName =
    journeyStatus.subscribedDetails.contactName

  override val updateSubscriptionDetailChangedFlag: Boolean = true

  override lazy protected val backLinkCall: Option[Call] = Some(
    controllers.routes.HomeController.homepage()
  )
  override lazy protected val enterEmailCall: Call            = routes.SubscribedChangeEmailController.enterEmail()
  override lazy protected val enterEmailSubmitCall: Call      = routes.SubscribedChangeEmailController.enterEmailSubmit()
  override lazy protected val checkYourInboxCall: Call        = routes.SubscribedChangeEmailController.checkYourInbox()
  override lazy protected val verifyEmailCall: UUID => Call   = routes.SubscribedChangeEmailController.verifyEmail
  override lazy protected val emailVerifiedCall: Call         = routes.SubscribedChangeEmailController.emailVerified()
  override lazy protected val emailVerifiedContinueCall: Call = controllers.routes.HomeController.homepage()

}
