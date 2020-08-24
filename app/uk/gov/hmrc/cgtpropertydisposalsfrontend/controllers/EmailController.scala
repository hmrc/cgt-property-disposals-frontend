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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers

import java.util.UUID

import cats.data.EitherT
import cats.instances.future._
import cats.instances.uuid._
import cats.syntax.eq._
import play.api.data.Form
import play.api.data.Forms.{mapping, of}
import play.api.mvc._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.EmailController.SubmitEmailDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{RequestWithSessionData, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.email.EmailJourneyType
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.http.AcceptLanguage
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.UUIDGenerator
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.ContactName
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.email.{Email, EmailToBeVerified}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.EmailVerificationService.EmailVerificationResponse.{EmailAlreadyVerified, EmailVerificationRequested}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.{AuditService, EmailVerificationService}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

trait EmailController[JourneyType <: EmailJourneyType] {
  this: FrontendController with WithAuthAndSessionDataAction with Logging with SessionUpdates =>

  implicit val viewConfig: ViewConfig
  implicit val ec: ExecutionContext

  val uuidGenerator: UUIDGenerator
  val sessionStore: SessionStore
  val emailVerificationService: EmailVerificationService
  val auditService: AuditService
  val errorHandler: ErrorHandler

  val enterEmailPage: views.html.onboarding.email.enter_email
  val checkYourInboxPage: views.html.onboarding.email.check_your_inbox
  val emailVerifiedPage: views.html.onboarding.email.email_verified

  def updateEmail(journey: JourneyType, email: Email)(implicit
    hc: HeaderCarrier,
    request: Request[_]
  ): EitherT[Future, Error, JourneyStatus]

  def validJourney(
    request: RequestWithSessionData[_]
  ): Either[Result, (SessionData, JourneyType)]

  def validVerificationCompleteJourney(
    request: RequestWithSessionData[_]
  ): Either[Result, (SessionData, JourneyType)]

  def auditEmailVerifiedEvent(journey: JourneyType, email: Email)(implicit
    hc: HeaderCarrier,
    request: Request[_]
  ): Unit

  def auditEmailChangeAttempt(journey: JourneyType, email: Email)(implicit
    hc: HeaderCarrier,
    request: Request[_]
  ): Unit

  def name(journeyStatus: JourneyType): ContactName

  private def withValidJourney(request: RequestWithSessionData[_])(
    f: (SessionData, JourneyType) => Future[Result]
  ): Future[Result] =
    validJourney(request).fold[Future[Result]](toFuture, f.tupled)

  protected val backLinkCall: Option[Call]
  protected val enterEmailCall: Call
  protected val enterEmailSubmitCall: Call
  protected val checkYourInboxCall: Call
  protected val verifyEmailCall: UUID => Call
  protected val emailVerifiedCall: Call
  protected val emailVerifiedContinueCall: Call

  def enterEmail(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withValidJourney(request) {
        case (sessionData, journey) =>
          val form = sessionData.emailToBeVerified.fold(
            EmailController.submitEmailForm
          )(e =>
            EmailController.submitEmailForm.fill(
              SubmitEmailDetails(e.email, e.hasResentVerificationEmail)
            )
          )
          Ok(enterEmailPage(form, journey, backLinkCall, enterEmailSubmitCall))
      }
    }

  def enterEmailSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withValidJourney(request) {
        case (sessionData, journey) =>
          EmailController.submitEmailForm
            .bindFromRequest()
            .fold(
              formWithErrors =>
                BadRequest(
                  enterEmailPage(
                    formWithErrors,
                    journey,
                    backLinkCall,
                    enterEmailSubmitCall
                  )
                ),
              {
                case SubmitEmailDetails(email, resendVerificationEmail) =>
                  val emailToBeVerified = sessionData.emailToBeVerified match {
                    case Some(e) if e.email === email =>
                      e.copy(hasResentVerificationEmail = resendVerificationEmail)
                    case _                            =>
                      EmailToBeVerified(
                        email,
                        uuidGenerator.nextId(),
                        verified = false,
                        hasResentVerificationEmail = resendVerificationEmail
                      )
                  }

                  val result = for {
                    language <- EitherT.fromOption[Future](
                                  AcceptLanguage.fromPlayLang(request.messages.lang),
                                  Error("could not find Play application language")
                                )
                    _        <- EitherT(
                                  updateSession(sessionStore, request)(
                                    _.copy(emailToBeVerified = Some(emailToBeVerified))
                                  )
                                )
                    result   <- emailVerificationService
                                  .verifyEmail(
                                    email,
                                    name(journey),
                                    verifyEmailCall(emailToBeVerified.id),
                                    language
                                  )
                    _        <- EitherT.pure[Future, Error](
                                  auditEmailChangeAttempt(
                                    journey,
                                    emailToBeVerified.email
                                  )
                                )
                  } yield result

                  result.fold(
                    { e =>
                      logger.warn("Could not verify email", e)
                      errorHandler.errorResult()
                    },
                    {
                      case EmailAlreadyVerified       =>
                        Redirect(verifyEmailCall(emailToBeVerified.id))

                      case EmailVerificationRequested =>
                        Redirect(checkYourInboxCall)
                    }
                  )
              }
            )
      }
    }

  def checkYourInbox(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withValidJourney(request) {
        case (sessionData, journey) =>
          sessionData.emailToBeVerified.fold(
            Redirect(enterEmailCall)
          )(emailToBeVerified =>
            Ok(
              checkYourInboxPage(
                emailToBeVerified.email,
                enterEmailCall,
                enterEmailCall,
                enterEmailSubmitCall,
                emailToBeVerified.hasResentVerificationEmail,
                journey
              )
            )
          )
      }
    }

  def verifyEmail(p: UUID): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withValidJourney(request) {
        case (sessionData, journey) =>
          sessionData.emailToBeVerified.fold[Future[Result]](
            Redirect(enterEmailCall)
          ) { emailToBeVerified =>
            if (emailToBeVerified.id =!= p) {
              logger.warn(
                s"Received verify email request where id sent ($p) did not match the id in session (${emailToBeVerified.id})"
              )
              errorHandler.errorResult()
            } else if (emailToBeVerified.verified)
              Redirect(emailVerifiedCall)
            else {
              auditEmailVerifiedEvent(journey, emailToBeVerified.email)

              val result = for {
                updatedJourney <- updateEmail(journey, emailToBeVerified.email)
                _              <- EitherT[Future, Error, Unit](
                                    updateSession(sessionStore, request) { s =>
                                      s.copy(
                                        journeyStatus = Some(updatedJourney),
                                        emailToBeVerified = Some(emailToBeVerified.copy(verified = true))
                                      )
                                    }
                                  )
              } yield ()

              result.fold(
                error => {
                  logger.warn(s"Could not update email: $error")
                  errorHandler.errorResult()
                },
                _ => Redirect(emailVerifiedCall)
              )
            }
          }
      }
    }

  def emailVerified(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      validVerificationCompleteJourney(request)
        .fold[Future[Result]](
          toFuture,
          {
            case (sessionData, journey) =>
              sessionData.emailToBeVerified.fold(
                Redirect(enterEmailCall)
              ) { emailToBeVerified =>
                if (emailToBeVerified.verified)
                  Ok(
                    emailVerifiedPage(
                      emailToBeVerified.email,
                      emailVerifiedContinueCall,
                      journey
                    )
                  )
                else {

                  logger.warn(
                    "Email verified endpoint called but email was not verified"
                  )
                  errorHandler.errorResult()
                }
              }
          }
        )

    }

}

object EmailController {

  final case class SubmitEmailDetails(
    email: Email,
    resendVerificationEmail: Boolean
  )

  val submitEmailForm: Form[SubmitEmailDetails] =
    Form(
      mapping(
        "email"                   -> Email.mapping,
        "resendVerificationEmail" -> of(BooleanFormatter.formatter)
      )(SubmitEmailDetails.apply)(SubmitEmailDetails.unapply)
    )

}
