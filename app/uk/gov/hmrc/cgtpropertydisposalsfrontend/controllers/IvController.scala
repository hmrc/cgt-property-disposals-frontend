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

import com.google.inject.{Inject, Singleton}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.ErrorHandler
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, SessionDataAction, WithActions}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

@Singleton
class IvController @Inject() (
    val authenticatedAction: AuthenticatedAction,
    val sessionDataAction: SessionDataAction,
    errorHandler: ErrorHandler,
    cc: MessagesControllerComponents
) extends FrontendController(cc) with WithActions with Logging {

  def ivSuccess(): Action[AnyContent] = authenticatedActionWithSessionData { implicit request =>
    request.sessionData.flatMap(_.ivContinueUrl) match {
      case Some(ivContinueUrl) =>
        SeeOther(ivContinueUrl)

      case None =>
        logger.warn("Could not get ivContinueUrl from session")
        InternalServerError(errorHandler.internalServerErrorTemplate)
    }
  }

  def ivFailure(journeyId: UUID): Action[AnyContent] = Action { implicit request =>
    logger.warn(s"IV journey failed: ${journeyId.toString}")
    InternalServerError(errorHandler.internalServerErrorTemplate)
  }

}
