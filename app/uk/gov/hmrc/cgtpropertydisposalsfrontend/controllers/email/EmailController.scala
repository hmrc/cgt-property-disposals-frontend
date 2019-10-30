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
import cats.instances.uuid._
import cats.syntax.eq._
import play.api.mvc.{Action, AnyContent, Call, Result}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{RequestWithSessionData, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.UUIDGenerator
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.ContactName
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.EmailVerificationService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.EmailVerificationService.EmailVerificationResponse.{EmailAlreadyVerified, EmailVerificationRequested}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

trait EmailController[Journey <: JourneyStatus, VerificationCompleteJourney <: JourneyStatus] {
  this: FrontendController with WithAuthAndSessionDataAction with Logging with SessionUpdates =>

  implicit val viewConfig: ViewConfig
  implicit val ec: ExecutionContext

  val uuidGenerator: UUIDGenerator
  val sessionStore: SessionStore
  val emailVerificationService: EmailVerificationService
  val errorHandler: ErrorHandler
  val isAmendJourney: Boolean

  val enterEmailPage: views.html.email.enter_email
  val checkYourInboxPage: views.html.email.check_your_inbox
  val emailVerifiedPage: views.html.email.email_verified

  def updateEmail(journey: Journey, email: Email)(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, VerificationCompleteJourney]

  def validJourney(request: RequestWithSessionData[_]): Either[Result, (SessionData, Journey)]

  def validVerificationCompleteJourney(
    request: RequestWithSessionData[_]
  ): Either[Result, (SessionData, VerificationCompleteJourney)]

  def name(journeyStatus: Journey): ContactName

  private def withValidJourney(request: RequestWithSessionData[_])(
    f: (SessionData, Journey) => Future[Result]
  ): Future[Result] =
    validJourney(request).fold[Future[Result]](toFuture, f.tupled)

  protected val backLinkCall: Option[Call]
  protected val enterEmailCall: Call
  protected val enterEmailSubmitCall: Call
  protected val checkYourInboxCall: Call
  protected val verifyEmailCall: UUID => Call
  protected val emailVerifiedCall: Call
  protected val emailVerifiedContinueCall: Call

  def enterEmail(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withValidJourney(request) {
      case (sessionData, _) =>
        val form = sessionData.emailToBeVerified.fold(Email.form)(e => Email.form.fill(e.email))
        Ok(enterEmailPage(form, isAmendJourney, backLinkCall, enterEmailSubmitCall))
    }
  }

  def enterEmailSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withValidJourney(request) {
        case (sessionData, journey) =>
          Email.form
            .bindFromRequest()
            .fold(
              formWithErrors =>
                BadRequest(
                  enterEmailPage(formWithErrors, isAmendJourney, backLinkCall, enterEmailSubmitCall)
                ), { email =>
                val emailToBeVerified = sessionData.emailToBeVerified match {
                  case Some(e) if e.email === email => e
                  case _                            => EmailToBeVerified(email, uuidGenerator.nextId(), verified = false)
                }

                val result = for {
                  _ <- EitherT(
                        updateSession(sessionStore, request)(_.copy(emailToBeVerified = Some(emailToBeVerified)))
                      )
                  result <- emailVerificationService
                             .verifyEmail(email, name(journey), verifyEmailCall(emailToBeVerified.id))
                } yield result

                result.fold(
                  { e =>
                    logger.warn("Could not verify email", e)
                    errorHandler.errorResult()
                  }, {
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
      withValidJourney(request) {
        case (sessionData, _) =>
          sessionData.emailToBeVerified.fold(
            Redirect(enterEmailCall)
          )(
            emailToBeVerified =>
              Ok(
                checkYourInboxPage(emailToBeVerified.email, enterEmailCall, enterEmailCall, enterEmailSubmitCall)
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
            } else {
              if (emailToBeVerified.verified) {
                Redirect(emailVerifiedCall)
              } else {
                val email = Email(emailToBeVerified.email.value)
                val result: EitherT[Future, Result, Unit] = for {
                  journey <- updateEmail(journey, email).leftMap(e => errorHandler.errorResult())
                  _ <- EitherT.liftF(updateSession(sessionStore, request) { s =>
                        s.copy(
                          journeyStatus     = Some(journey),
                          emailToBeVerified = Some(emailToBeVerified.copy(verified = true))
                        )
                      })
                } yield ()

                result.fold(
                  _ => errorHandler.errorResult(),
                  _ => Redirect(emailVerifiedCall)
                )
              }
            }
          }
      }
    }

  def emailVerified(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      validVerificationCompleteJourney(request)
        .fold[Future[Result]](
          toFuture, {
            case (sessionData, _) =>
              sessionData.emailToBeVerified.fold(
                Redirect(enterEmailCall)
              ) { emailToBeVerified =>
                if (emailToBeVerified.verified) {
                  Ok(emailVerifiedPage(emailToBeVerified.email, emailVerifiedContinueCall))
                } else {
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
