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

import cats.syntax.eq._

import com.google.inject.{Inject, Singleton}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.ErrorHandler
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.SessionData
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging._
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.ExecutionContext

@Singleton
class IvController @Inject()(
                              sessionStore: SessionStore,
                              val authenticatedAction: AuthenticatedAction,
                              val sessionDataAction: SessionDataAction,
                              errorHandler: ErrorHandler,
                              cc: MessagesControllerComponents
                            )(implicit ec: ExecutionContext) extends FrontendController(cc)
  with WithAuthAndSessionDataAction
  with Logging
  with SessionUpdates {

  def ivSuccess(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
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

  def ivFailure(journeyId: UUID): Action[AnyContent] = Action { implicit request =>
    logger.warn(s"IV journey failed: ${journeyId.toString}")
    InternalServerError(errorHandler.internalServerErrorTemplate)
  }

}
