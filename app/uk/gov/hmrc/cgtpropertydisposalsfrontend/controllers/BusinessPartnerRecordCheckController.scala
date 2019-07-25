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

import com.google.inject.{Inject, Singleton}
import play.api.data.Form
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithActions}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.DateOfBirth.dobForm
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.NINO.ninoForm
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class BusinessPartnerRecordCheckController @Inject() (
    sessionStore: SessionStore,
    errorHandler: ErrorHandler,
    cc: MessagesControllerComponents,
    val authenticatedAction: AuthenticatedAction,
    val sessionDataAction: SessionDataAction,
    getNinoPage: views.html.bprcheck.nino,
    getDobPage: views.html.bprcheck.date_of_birth
)(implicit viewConfig: ViewConfig, ec: ExecutionContext) extends FrontendController(cc) with WithActions with Logging {

  def getNino(): Action[AnyContent] = authenticatedActionWithSessionData { implicit request =>
    val sessionNino = request.sessionData.flatMap(_.nino)
    val form = sessionNino.fold(ninoForm)(ninoForm.fill)
    Ok(getNinoPage(form))
  }

  def getNinoSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    ninoForm.bindFromRequest().fold(
      formWithErrors => BadRequest(getNinoPage(formWithErrors)),
      nino => storeAndThen(_.copy(nino = Some(nino)))(SeeOther(routes.BusinessPartnerRecordCheckController.getDateOfBirth().url))
    )
  }

  def getDateOfBirth(): Action[AnyContent] = authenticatedActionWithSessionData { implicit request =>
    val form: Option[Form[DateOfBirth]] = request.sessionData.flatMap(_.nino) -> request.sessionData.flatMap(_.dob) match {
      case (None, _)      => None
      case (_, Some(dob)) => Some(dobForm.fill(dob))
      case (_, None)      => Some(dobForm)
    }

    form.fold(SeeOther(routes.BusinessPartnerRecordCheckController.getNino().url)) {
      f => Ok(getDobPage(f))
    }
  }

  def getDateOfBirthSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    request.sessionData.flatMap(_.nino).fold[Future[Result]](
      SeeOther(routes.BusinessPartnerRecordCheckController.getNino().url)
    ) { _ =>
        dobForm.bindFromRequest().fold(
          formWithErrors => BadRequest(getDobPage(formWithErrors)),
          dateOfBirth => storeAndThen(_.copy(dob = Some(dateOfBirth)))(Ok(s"Got $dateOfBirth"))
        )
      }
  }

  def storeAndThen(updateSession: SessionData => SessionData)(f: => Result)(implicit request: RequestWithSessionData[_]): Future[Result] =
    sessionStore.store(updateSession(request.sessionData.getOrElse(SessionData.empty))).map {
      case Left(e) =>
        logger.warn("Could not store data in mongo", e)
        InternalServerError(errorHandler.internalServerErrorTemplate)

      case Right(_) =>
        f
    }

}

