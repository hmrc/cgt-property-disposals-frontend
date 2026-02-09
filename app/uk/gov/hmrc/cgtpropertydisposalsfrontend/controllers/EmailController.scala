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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers

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
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.Subscribed
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.email.{Email, EmailJourneyType, EmailToBeVerified}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.http.AcceptLanguage
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.UUIDGenerator
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.ContactName
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.EmailVerificationService.EmailVerificationResponse.{EmailAlreadyVerified, EmailVerificationRequested}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.{AuditService, EmailVerificationService}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, given}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import java.util.UUID
import scala.concurrent.{ExecutionContext, Future}

trait EmailController[T <: EmailJourneyType] {
  this: FrontendController & WithAuthAndSessionDataAction & Logging & SessionUpdates =>

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

  def updateEmail(journey: T, email: Email)(implicit
    hc: HeaderCarrier,
    request: Request[?]
  ): EitherT[Future, Error, JourneyStatus]

  def validJourney(
    request: RequestWithSessionData[?]
  ): Either[Result, (SessionData, T)]

  def validVerificationCompleteJourney(
    request: RequestWithSessionData[?]
  ): Either[Result, (SessionData, T)]

  def auditEmailVerifiedEvent(journey: T, email: Email)(implicit
    hc: HeaderCarrier,
    request: Request[?]
  ): Unit

  def auditEmailChangeAttempt(journey: T, email: Email)(implicit
    hc: HeaderCarrier,
    request: Request[?]
  ): Unit

  def name(journeyStatus: T): ContactName

  private def withValidJourney(request: RequestWithSessionData[?])(
    f: (SessionData, T) => Future[Result]
  ): Future[Result] =
    validJourney(request).fold[Future[Result]](given_Conversion_Result_Future, f.tupled)

  protected lazy val backLinkCall: Option[Call]
  protected lazy val enterEmailCall: Call
  protected lazy val enterEmailSubmitCall: Call
  protected lazy val checkYourInboxCall: Call
  protected lazy val verifyEmailCall: UUID => Call
  protected lazy val emailVerifiedCall: Call
  protected lazy val emailVerifiedContinueCall: Call

  def enterEmail(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withValidJourney(request) { case (sessionData, journey) =>
        val form = sessionData.emailToBeVerified.fold(
          request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
            case Some((_, subscribed: Subscribed)) =>
              EmailController.submitEmailForm.fill(
                SubmitEmailDetails(subscribed.subscribedDetails.emailAddress.get, resendVerificationEmail = false)
              )
            case _                                 => EmailController.submitEmailForm
          }
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
      withValidJourney(request) { case (sessionData, journey) =>
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
            { case SubmitEmailDetails(email, resendVerificationEmail) =>
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
                              updateSession(sessionStore, request.toSession)(
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
                  case EmailAlreadyVerified =>
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
      withValidJourney(request) { case (sessionData, journey) =>
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
      withValidJourney(request) { case (sessionData, journey) =>
        sessionData.emailToBeVerified.fold[Future[Result]](
          Redirect(enterEmailCall)
        ) { emailToBeVerified =>
          if (emailToBeVerified.id =!= p) {
            logger.warn(
              s"Received verify email request where id sent ($p) did not match the id in session (${emailToBeVerified.id})"
            )
            errorHandler.errorResult()
          } else if (emailToBeVerified.verified) {
            Redirect(emailVerifiedCall)
          } else {
            auditEmailVerifiedEvent(journey, emailToBeVerified.email)

            val result = for {
              updatedJourney <- updateEmail(journey, emailToBeVerified.email)
              _              <- EitherT[Future, Error, Unit](
                                  updateSession(sessionStore, request.toSession) { s =>
                                    s.copy(
                                      journeyStatus = Some(updatedJourney),
                                      Some(emailToBeVerified.copy(verified = true))
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
          given_Conversion_Result_Future,
          { case (sessionData, journey) =>
            sessionData.emailToBeVerified.fold(
              Redirect(enterEmailCall)
            ) { emailToBeVerified =>
              if (emailToBeVerified.verified) {
                Ok(
                  emailVerifiedPage(
                    emailToBeVerified.email,
                    emailVerifiedContinueCall,
                    journey
                  )
                )
              } else {
                logger.warn("Email verified endpoint called but email was not verified")
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

  private val submitEmailForm =
    Form(
      mapping(
        "email"                   -> Email.mapping,
        "resendVerificationEmail" -> of(using BooleanFormatter.formatter)
      )(SubmitEmailDetails.apply)(o => Some(o.email, o.resendVerificationEmail))
    )

}
