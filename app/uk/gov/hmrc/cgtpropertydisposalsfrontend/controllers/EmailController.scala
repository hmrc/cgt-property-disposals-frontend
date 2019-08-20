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
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, SubscriptionDetailsAction, WithSubscriptionDetailsActions}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Email, EmailToBeVerified, UUIDGenerator}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.EmailVerificationService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.EmailVerificationService.EmailVerificationResponse.{EmailAlreadyVerified, EmailVerificationRequested}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class EmailController @Inject()(
  val authenticatedAction: AuthenticatedAction,
  val subscriptionDetailsAction: SubscriptionDetailsAction,
  sessionStore: SessionStore,
  emailVerificationService: EmailVerificationService,
  uuidGenerator: UUIDGenerator,
  errorHandler: ErrorHandler,
  cc: MessagesControllerComponents,
  enterEmail: views.html.subscription.enter_email,
  checkYourInboxPage: views.html.subscription.check_your_inbox,
  emailVerifiedPage: views.html.subscription.email_verified
)(implicit viewConfig: ViewConfig, ec: ExecutionContext)
    extends FrontendController(cc)
    with WithSubscriptionDetailsActions
    with Logging
    with SessionUpdates {

  def enterEmail(): Action[AnyContent] = authenticatedActionWithSubscriptionDetails { implicit request =>
    val form = request.sessionData.emailToBeVerified.fold(Email.form)(e => Email.form.fill(e.email))
    Ok(enterEmail(form))
  }

  def enterEmailSubmit(): Action[AnyContent] = authenticatedActionWithSubscriptionDetails.async { implicit request =>
    Email.form
      .bindFromRequest()
      .fold(
        formWithErrors => BadRequest(enterEmail(formWithErrors)), { email =>
          val emailToBeVerified = request.sessionData.emailToBeVerified match {
            case Some(e) if e.email === email => e
            case _                            => EmailToBeVerified(email, uuidGenerator.nextId(), verified = false)
          }

          val result = for {
            _ <- EitherT(updateSession(sessionStore, request)(_.copy(emailToBeVerified = Some(emailToBeVerified))))
            result <- emailVerificationService
                       .verifyEmail(email, emailToBeVerified.id, request.subscriptionDetails.forename)
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

  def checkYourInbox(): Action[AnyContent] = authenticatedActionWithSubscriptionDetails { implicit request =>
    request.sessionData.emailToBeVerified
      .fold(SeeOther(routes.SubscriptionController.checkYourDetails().url))(
        emailToBeVerified => Ok(checkYourInboxPage(emailToBeVerified.email))
      )
  }

  def verifyEmail(p: UUID): Action[AnyContent] = authenticatedActionWithSubscriptionDetails.async { implicit request =>
    request.sessionData.emailToBeVerified
      .fold[Future[Result]](SeeOther(routes.SubscriptionController.checkYourDetails().url)) { emailToBeVerified =>
        if (emailToBeVerified.id =!= p) {
          logger.warn(
            s"Received verify email request where id sent ($p) did not match the id in session (${emailToBeVerified.id})"
          )
          errorHandler.errorResult()
        } else {
          if (emailToBeVerified.verified) {
            SeeOther(routes.EmailController.emailVerified().url)
          } else {
            updateSession(sessionStore, request)(
              _.copy(
                subscriptionDetails =
                  Some(request.subscriptionDetails.copy(emailAddress = emailToBeVerified.email.value)),
                emailToBeVerified = Some(emailToBeVerified.copy(verified = true))
              )
            ).map(_.fold({ e =>
              logger.warn("Could not store email verified result", e)
              errorHandler.errorResult()
            }, { _ =>
              SeeOther(routes.EmailController.emailVerified().url)
            }))
          }
        }
      }
  }

  def emailVerified(): Action[AnyContent] = authenticatedActionWithSubscriptionDetails { implicit request =>
    request.sessionData.emailToBeVerified.fold(SeeOther(routes.SubscriptionController.checkYourDetails().url)) {
      emailToBeVerified =>
        if (emailToBeVerified.verified) {
          Ok(emailVerifiedPage(emailToBeVerified.email))
        } else {
          logger.warn("Email verified endpoint called but email was not verified")
          errorHandler.errorResult()
        }
    }
  }

}
