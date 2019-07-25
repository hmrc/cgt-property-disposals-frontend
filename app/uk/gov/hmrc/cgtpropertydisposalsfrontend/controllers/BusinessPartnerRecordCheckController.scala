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
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.ViewConfig
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.AuthenticatedAction
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.DateOfBirth.dobForm
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.NINO.ninoForm
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

@Singleton
class BusinessPartnerRecordCheckController @Inject() (
    authenticatedAction: AuthenticatedAction,
    cc: MessagesControllerComponents,
    getNinoPage: views.html.bprcheck.nino,
    getDobPage: views.html.bprcheck.date_of_birth
)(implicit viewConfig: ViewConfig) extends FrontendController(cc) {

  def getNino(): Action[AnyContent] = authenticatedAction.andThen(Action) { implicit request =>
    Ok(getNinoPage(ninoForm))
  }

  def getNinoSubmit(): Action[AnyContent] = authenticatedAction.andThen(Action) { implicit request =>
    ninoForm.bindFromRequest().fold(
      formWithErrors => BadRequest(getNinoPage(formWithErrors)),
      _ => SeeOther(routes.BusinessPartnerRecordCheckController.getDateOfBirth().url)
    )
  }

  def getDateOfBirth(): Action[AnyContent] = authenticatedAction.andThen(Action) { implicit request =>
    Ok(getDobPage(dobForm))
  }

  def getDateOfBirthSubmit(): Action[AnyContent] = authenticatedAction.andThen(Action) { implicit request =>
    dobForm.bindFromRequest().fold(
      formWithErrors => BadRequest(getDobPage(formWithErrors)),
      d => Ok(s"Got $d")
    )
  }

}

