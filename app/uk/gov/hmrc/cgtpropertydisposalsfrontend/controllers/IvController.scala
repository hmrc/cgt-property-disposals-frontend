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

import cats.instances.future._
import cats.syntax.eq._
import com.google.inject.{Inject, Singleton}
import play.api.Configuration
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.SessionData
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.iv.IvErrorResponse
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.IvService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.ExecutionContext

@Singleton
class IvController @Inject()(
  sessionStore: SessionStore,
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val errorHandler: ErrorHandler,
  val config: Configuration,
  ivService: IvService,
  cc: MessagesControllerComponents,
  failedIvPage: views.html.iv.failed_iv,
  failedMatchingPage: views.html.iv.failed_matching,
  insufficientEvidencePage: views.html.iv.insufficient_evidence,
  lockedOutPage: views.html.iv.locked_out,
  preconditionFailedPage: views.html.iv.precondition_failed,
  technicalIssuesPage: views.html.iv.technical_iv_issues,
  timeoutPage: views.html.iv.time_out,
  userAbortedPage: views.html.iv.user_aborted
)(implicit ec: ExecutionContext, viewConfig: ViewConfig)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with Logging
    with SessionUpdates
    with IvBehaviour {

  def ivSuccessCallback(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    if (request.sessionData.forall(_ === SessionData.empty)) {
      SeeOther(routes.StartController.start().url)
    } else {
      updateSession(sessionStore, request)(_ => SessionData.empty).map {
        case Left(e) =>
          logger.warn("Could not clear session after IV success", e)
          errorHandler.errorResult()

        case Right(_) =>
          SeeOther(routes.StartController.start().url)
      }
    }
  }

  def ivFailureCallback(journeyId: UUID): Action[AnyContent] = authenticatedActionWithSessionData.async {
    implicit request =>
      ivService
        .getFailedJourneyStatus(journeyId)
        .fold(
          { e =>
            logger.warn("Could not check IV journey error status", e)
            Redirect(routes.IvController.getTechnicalIssue())
          }, {
            //The journey has not been completed yet.
            //This result can only occur when a service asks for the result too early (before receiving the redirect from IV)
            case IvErrorResponse.Incomplete => Redirect(routes.IvController.getTechnicalIssue())

            case IvErrorResponse.FailedMatching => Redirect(routes.IvController.getFailedMatching())

            case IvErrorResponse.FailedIV => Redirect(routes.IvController.getFailedIV())

            case IvErrorResponse.InsufficientEvidence => Redirect(routes.IvController.getInsufficientEvidence())

            case IvErrorResponse.LockedOut => Redirect(routes.IvController.getLockedOut())

            case IvErrorResponse.UserAborted => Redirect(routes.IvController.getUserAborted())

            case IvErrorResponse.Timeout => Redirect(routes.IvController.getTimedOut())

            case IvErrorResponse.TechnicalIssue => Redirect(routes.IvController.getTechnicalIssue())

            case IvErrorResponse.PreconditionFailed => Redirect(routes.IvController.getPreconditionFailed())

            case IvErrorResponse.Unknown(value) =>
              logger.warn(s"Received unknown error response status from IV: $value")
              Redirect(routes.IvController.getTechnicalIssue())
          }
        )
  }

  def getFailedMatching: Action[AnyContent] = authenticatedActionWithSessionData { implicit r ⇒
    Ok(failedMatchingPage(redirectToIvUrl))
  }

  def getFailedIV: Action[AnyContent] = authenticatedActionWithSessionData { implicit r ⇒
    Ok(failedIvPage(redirectToIvUrl))
  }

  def getInsufficientEvidence: Action[AnyContent] = authenticatedActionWithSessionData { implicit r ⇒
    Ok(insufficientEvidencePage())
  }

  def getLockedOut: Action[AnyContent] = authenticatedActionWithSessionData { implicit r ⇒
    Ok(lockedOutPage())
  }

  def getUserAborted: Action[AnyContent] = authenticatedActionWithSessionData { implicit r ⇒
    Ok(userAbortedPage(redirectToIvUrl))
  }

  def getTimedOut: Action[AnyContent] = authenticatedActionWithSessionData { implicit r ⇒
    Ok(timeoutPage(redirectToIvUrl))
  }

  def getTechnicalIssue: Action[AnyContent] = authenticatedActionWithSessionData { implicit r ⇒
    Ok(technicalIssuesPage(redirectToIvUrl))
  }
  def getPreconditionFailed: Action[AnyContent] = authenticatedActionWithSessionData { implicit r ⇒
    Ok(preconditionFailedPage())
  }

}
