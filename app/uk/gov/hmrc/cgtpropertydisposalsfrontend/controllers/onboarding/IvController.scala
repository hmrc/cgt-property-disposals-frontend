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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding

import cats.instances.future._
import cats.syntax.eq._
import com.google.inject.{Inject, Singleton}
import play.api.Configuration
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.routes
import uk.gov.hmrc.cgtpropertydisposalsfrontend.metrics.Metrics
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.SessionData
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.iv.IvErrorStatus
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.onboarding.IvService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.{controllers, views}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import java.util.UUID
import scala.concurrent.ExecutionContext

@Singleton
class IvController @Inject() (
  sessionStore: SessionStore,
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val errorHandler: ErrorHandler,
  val config: Configuration,
  ivService: IvService,
  metrics: Metrics,
  cc: MessagesControllerComponents,
  failedIvPage: views.html.onboarding.iv.failed_iv,
  failedMatchingPage: views.html.onboarding.iv.failed_matching,
  insufficientEvidencePage: views.html.onboarding.iv.insufficient_evidence,
  lockedOutPage: views.html.onboarding.iv.locked_out,
  preconditionFailedPage: views.html.onboarding.iv.precondition_failed,
  technicalIssuesPage: views.html.onboarding.iv.technical_iv_issues,
  timeoutPage: views.html.onboarding.iv.time_out,
  userAbortedPage: views.html.onboarding.iv.user_aborted
)(implicit ec: ExecutionContext, viewConfig: ViewConfig)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with Logging
    with SessionUpdates
    with IvBehaviour {

  def ivSuccessCallback(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      if (request.sessionData.forall(_ === SessionData.empty)) {
        SeeOther(controllers.routes.StartController.start().url)
      } else {
        updateSession(sessionStore, request)(_ => SessionData.empty).map {
          case Left(e) =>
            logger.warn("Could not clear session after IV success", e)
            errorHandler.errorResult()

          case Right(_) =>
            SeeOther(controllers.routes.StartController.start().url)
        }
      }
    }

  def retry(): Action[AnyContent] =
    authenticatedActionWithSessionData.async(_ => redirectToIv)

  def ivFailureCallback(journeyId: UUID): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      ivService
        .getFailedJourneyStatus(journeyId)
        .fold(
          { e =>
            logger.warn("Could not check IV journey error status", e)
            Redirect(routes.IvController.getTechnicalIssue())
          },
          {
            case IvErrorStatus.Incomplete           =>
              metrics.ivIncompleteCounter.inc()
              Redirect(routes.IvController.getTechnicalIssue())
            case IvErrorStatus.FailedMatching       =>
              metrics.ivFailedMatchingCounter.inc()
              Redirect(routes.IvController.getFailedMatching())
            case IvErrorStatus.FailedIV             =>
              metrics.ivFailedIVCounter.inc()
              Redirect(routes.IvController.getFailedIV())
            case IvErrorStatus.InsufficientEvidence =>
              metrics.ivInsufficientEvidenceCounter.inc()
              Redirect(routes.IvController.getInsufficientEvidence())
            case IvErrorStatus.LockedOut            =>
              metrics.ivLockedOutCounter.inc()
              Redirect(routes.IvController.getLockedOut())
            case IvErrorStatus.UserAborted          =>
              metrics.ivUserAbortedCounter.inc()
              Redirect(routes.IvController.getUserAborted())
            case IvErrorStatus.Timeout              =>
              metrics.ivTimeoutCounter.inc()
              Redirect(routes.IvController.getTimedOut())
            case IvErrorStatus.TechnicalIssue       =>
              metrics.ivTechnicalIssueCounter.inc()
              Redirect(routes.IvController.getTechnicalIssue())
            case IvErrorStatus.PreconditionFailed   =>
              metrics.ivPreconditionFailedCounter.inc()
              Redirect(routes.IvController.getPreconditionFailed())
            case IvErrorStatus.Unknown(value)       =>
              metrics.ivUnknownErrorCounter.inc()
              logger
                .warn(s"Received unknown error response status from IV: $value")
              Redirect(routes.IvController.getTechnicalIssue())
          }
        )
    }

  def getFailedMatching: Action[AnyContent] =
    authenticatedActionWithSessionData { implicit r =>
      Ok(failedMatchingPage())
    }

  def getFailedIV: Action[AnyContent] =
    authenticatedActionWithSessionData(implicit r => Ok(failedIvPage()))

  def getInsufficientEvidence: Action[AnyContent] =
    authenticatedActionWithSessionData { implicit r =>
      Ok(insufficientEvidencePage())
    }

  def getLockedOut: Action[AnyContent] =
    authenticatedActionWithSessionData(implicit r => Ok(lockedOutPage()))

  def getUserAborted: Action[AnyContent] =
    authenticatedActionWithSessionData(implicit r => Ok(userAbortedPage()))

  def getTimedOut: Action[AnyContent] =
    authenticatedActionWithSessionData(implicit r => Ok(timeoutPage()))

  def getTechnicalIssue: Action[AnyContent]     =
    authenticatedActionWithSessionData { implicit r =>
      Ok(technicalIssuesPage())
    }
  def getPreconditionFailed: Action[AnyContent] =
    authenticatedActionWithSessionData { implicit r =>
      Ok(preconditionFailedPage())
    }

}
