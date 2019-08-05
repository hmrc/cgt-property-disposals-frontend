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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers

import java.util.UUID

import cats.data.EitherT
import cats.instances.future._
import cats.instances.uuid._
import cats.syntax.eq._
import com.google.inject.{Inject, Singleton}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithActions}
import uk.gov.hmrc.play.bootstrap.controller.FrontendController
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Email, EmailToBeVerified, Error, SessionData, UUIDGenerator}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.EmailVerificationService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.EmailVerificationService.EmailVerificationResponse.{EmailAlreadyVerified, EmailVerificationRequested}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.toFuture
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class EmailController @Inject() (
    val authenticatedAction: AuthenticatedAction,
    val sessionDataAction: SessionDataAction,
    sessionStore: SessionStore,
    emailVerificationService: EmailVerificationService,
    uuidGenerator: UUIDGenerator,
    errorHandler: ErrorHandler,
    cc: MessagesControllerComponents,
    enterEmail: views.html.subscription.enter_email,
    checkYourInboxPage: views.html.subscription.check_your_inbox,
    emailVerifiedPage: views.html.subscription.email_verified
)(implicit viewConfig: ViewConfig, ec: ExecutionContext) extends FrontendController(cc) with WithActions with Logging {

  def enterEmail(): Action[AnyContent] = authenticatedActionWithSessionData { implicit request =>
    (request.sessionData.flatMap(_.businessPartnerRecord), request.sessionData.flatMap(_.emailToBeVerified)) match {
      case (None, _) =>
        SeeOther(routes.SubscriptionController.checkYourDetails().url)

      case (Some(_), emailToBeVerified) =>
        val form = emailToBeVerified.fold(Email.form)(e => Email.form.fill(e.email))
        Ok(enterEmail(form))
    }
  }

  def enterEmailSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    request.sessionData.flatMap(_.businessPartnerRecord) match {
      case None =>
        SeeOther(routes.SubscriptionController.checkYourDetails().url)

      case Some(_) =>
        Email.form.bindFromRequest().fold(
          formWithErrors => BadRequest(enterEmail(formWithErrors)),
          { email =>
            val emailToBeVerified = EmailToBeVerified(email, uuidGenerator.nextId(), verified = false)

            val result = for {
              _ <- EitherT(updateSession(sessionStore)(_.copy(emailToBeVerified = Some(emailToBeVerified))))
              result <- EitherT(emailVerificationService.verifyEmail(email, emailToBeVerified.id))
            } yield result

            result.value.map {
              case Left(e) =>
                logger.warn("Could not verify email", e)
                errorHandler.errorResult()

              case Right(EmailAlreadyVerified) =>
                SeeOther(routes.EmailController.verifyEmail(emailToBeVerified.id).url)

              case Right(EmailVerificationRequested) =>
                SeeOther(routes.EmailController.checkYourInbox().url)
            }
          }
        )
    }
  }

  def checkYourInbox(): Action[AnyContent] = authenticatedActionWithSessionData { implicit request =>
    (request.sessionData.flatMap(_.businessPartnerRecord), request.sessionData.flatMap(_.emailToBeVerified)) match {
      case (None, _) | (_, None) =>
        SeeOther(routes.SubscriptionController.checkYourDetails().url)

      case (Some(_), Some(_)) =>
        Ok(checkYourInboxPage())
    }

  }

  def verifyEmail(p: UUID): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    (request.sessionData.flatMap(_.businessPartnerRecord), request.sessionData.flatMap(_.emailToBeVerified)) match {
      case (None, _) | (_, None) =>
        SeeOther(routes.SubscriptionController.checkYourDetails().url)

      case (Some(bpr), Some(emailToBeVerified)) =>
        if (emailToBeVerified.id =!= p) {
          logger.warn(s"Received verify email request where id sent ($p) did not match the id in session (${emailToBeVerified.id})")
          errorHandler.errorResult()
        } else {
          if (emailToBeVerified.verified) {
            SeeOther(routes.EmailController.emailVerified().url)
          } else {
            updateSession(sessionStore)(
              _.copy(
                businessPartnerRecord = Some(bpr.copy(emailAddress = Some(emailToBeVerified.email.value))),
                emailToBeVerified     = Some(emailToBeVerified.copy(verified = true))
              )
            )
              .map(
                _.fold(
                  { e =>
                    logger.warn("Could not store email verified result", e)
                    errorHandler.errorResult()
                  }, { _ =>
                    SeeOther(routes.EmailController.emailVerified().url)
                  }
                )
              )
          }
        }
    }
  }

  def emailVerified(): Action[AnyContent] = authenticatedActionWithSessionData { implicit request =>
    (request.sessionData.flatMap(_.businessPartnerRecord), request.sessionData.flatMap(_.emailToBeVerified)) match {
      case (None, _) | (_, None) =>
        SeeOther(routes.SubscriptionController.checkYourDetails().url)

      case (Some(_), Some(emailToBeVerified)) =>
        if (emailToBeVerified.verified) {
          Ok(emailVerifiedPage())
        } else {
          logger.warn("Email verified endpoint called but email was not verified")
          errorHandler.errorResult()
        }

    }
  }

  protected def updateSession(sessionStore: SessionStore)(update: SessionData => SessionData)(implicit request: RequestWithSessionData[_], hc: HeaderCarrier): Future[Either[Error, Unit]] =
    sessionStore.store(update(request.sessionData.getOrElse(SessionData.empty)))

}
