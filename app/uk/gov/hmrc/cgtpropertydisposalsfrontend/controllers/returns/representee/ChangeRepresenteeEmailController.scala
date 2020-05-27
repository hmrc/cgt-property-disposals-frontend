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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.representee

import java.util.UUID

import cats.data.EitherT
import cats.instances.future._
import cats.syntax.either._
import cats.syntax.eq._
import com.google.inject.Inject
import play.api.mvc.{Call, MessagesControllerComponents, Request, Result}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{EmailController, SessionUpdates}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, StartingNewDraftReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.email.EmailJourneyType.Returns.ChangingRepresenteeEmail
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.UUIDGenerator
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.ContactName
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.email.Email
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.RepresenteeAnswers.IncompleteRepresenteeAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{RepresenteeAnswers, RepresenteeContactDetails}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, JourneyStatus, SessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.{AuditService, EmailVerificationService}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging
import uk.gov.hmrc.cgtpropertydisposalsfrontend.{controllers, views}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

class ChangeRepresenteeEmailController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionStore,
  val emailVerificationService: EmailVerificationService,
  returnsService: ReturnsService,
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
    with EmailController[ChangingRepresenteeEmail] {

  private def extractAnswersAndContactDetails(
    answers: Option[RepresenteeAnswers]
  ): Option[(RepresenteeAnswers, RepresenteeContactDetails)] =
    answers.flatMap(r =>
      r.fold(
        incomplete => incomplete.contactDetails.map(r -> _),
        complete => Some(complete -> complete.contactDetails)
      )
    )

  def validJourney(
    request: RequestWithSessionData[_]
  ): Either[Result, (SessionData, ChangingRepresenteeEmail)] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some((sessionData, s: StartingNewDraftReturn)) =>
        Either.fromOption(
          extractAnswersAndContactDetails(s.representeeAnswers)
            .map {
              case (answers, contactDetails) =>
                sessionData -> ChangingRepresenteeEmail(
                  Left(s),
                  answers,
                  contactDetails
                )
            },
          Redirect(controllers.routes.StartController.start())
        )

      case Some((sessionData, f: FillingOutReturn))       =>
        Either.fromOption(
          extractAnswersAndContactDetails(
            f.draftReturn.fold(
              _.representeeAnswers,
              _.representeeAnswers,
              _.representeeAnswers,
              _.representeeAnswers
            )
          ).map {
            case (answers, contactDetails) =>
              sessionData -> ChangingRepresenteeEmail(
                Right(f),
                answers,
                contactDetails
              )
          },
          Redirect(controllers.routes.StartController.start())
        )

      case _                                              => Left(Redirect(controllers.routes.StartController.start()))
    }

  override def validVerificationCompleteJourney(
    request: RequestWithSessionData[_]
  ): Either[Result, (SessionData, ChangingRepresenteeEmail)] =
    validJourney(request)

  override def updateEmail(
    changingRepresenteeEmail: ChangingRepresenteeEmail,
    email: Email
  )(implicit
    hc: HeaderCarrier,
    request: Request[_]
  ): EitherT[Future, Error, JourneyStatus] =
    if (changingRepresenteeEmail.contactDetails.emailAddress === email)
      EitherT.pure[Future, Error](changingRepresenteeEmail.journey.merge)
    else {
      val newContactDetails =
        changingRepresenteeEmail.contactDetails.copy(emailAddress = email)
      val newAnswers        = changingRepresenteeEmail.answers.fold(
        _.copy(
          contactDetails = Some(newContactDetails),
          hasConfirmedContactDetails = false
        ),
        complete =>
          IncompleteRepresenteeAnswers(
            Some(complete.name),
            Some(complete.id),
            complete.dateOfDeath,
            Some(newContactDetails),
            hasConfirmedPerson = true,
            hasConfirmedContactDetails = false
          )
      )

      val newJourney = changingRepresenteeEmail.journey.bimap(
        _.copy(representeeAnswers = Some(newAnswers)),
        fillingOutReturn =>
          fillingOutReturn.copy(draftReturn =
            fillingOutReturn.draftReturn.fold(
              _.copy(representeeAnswers = Some(newAnswers)),
              _.copy(representeeAnswers = Some(newAnswers)),
              _.copy(representeeAnswers = Some(newAnswers)),
              _.copy(representeeAnswers = Some(newAnswers))
            )
          )
      )

      newJourney.fold(
        EitherT.pure[Future, Error](_),
        fillingOutReturn =>
          returnsService
            .storeDraftReturn(
              fillingOutReturn.draftReturn,
              fillingOutReturn.subscribedDetails.cgtReference,
              fillingOutReturn.agentReferenceNumber
            )
            .map(_ => fillingOutReturn)
      )

    }

  override def auditEmailVerifiedEvent(
    changingRepresenteeEmail: ChangingRepresenteeEmail,
    email: Email
  )(implicit hc: HeaderCarrier, request: Request[_]): Unit = ()

  override def auditEmailChangeAttempt(
    ChangingRepresenteeEmail: ChangingRepresenteeEmail,
    email: Email
  )(implicit hc: HeaderCarrier, request: Request[_]): Unit = ()

  override def name(
    changingRepresenteeEmail: ChangingRepresenteeEmail
  ): ContactName =
    changingRepresenteeEmail.journey
      .fold(_.subscribedDetails.contactName, _.subscribedDetails.contactName)

  override lazy protected val backLinkCall: Option[Call]      = Some(
    routes.RepresenteeController.checkYourAnswers()
  )
  override lazy protected val enterEmailCall: Call            =
    routes.ChangeRepresenteeEmailController.enterEmail()
  override lazy protected val enterEmailSubmitCall: Call      =
    routes.ChangeRepresenteeEmailController.enterEmailSubmit()
  override lazy protected val checkYourInboxCall: Call        =
    routes.ChangeRepresenteeEmailController.checkYourInbox()
  override lazy protected val verifyEmailCall: UUID => Call   =
    routes.ChangeRepresenteeEmailController.verifyEmail
  override lazy protected val emailVerifiedCall: Call         =
    routes.ChangeRepresenteeEmailController.emailVerified()
  override lazy protected val emailVerifiedContinueCall: Call =
    routes.RepresenteeController.checkYourAnswers()

}
