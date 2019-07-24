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
import play.api.data._
import play.api.data.Forms._
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.ViewConfig
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.AuthenticatedAction
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.NINO
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.util.matching.Regex

@Singleton
class BusinessPartnerRecordCheckController @Inject() (
    authenticatedAction: AuthenticatedAction,
    cc: MessagesControllerComponents,
    getNinoPage: views.html.bprcheck.nino
)(implicit viewConfig: ViewConfig) extends FrontendController(cc) {

  import BusinessPartnerRecordCheckController._

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
    Ok("Give us your date of birth")
  }

}

object BusinessPartnerRecordCheckController {

  val ninoForm: Form[NINO] = {
    val ninoRegex: Regex = """^((?!(BG|GB|KN|NK|NT|TN|ZZ)|(D|F|I|Q|U|V)[A-Z]|[A-|Z](D|F|I|O|Q|U|V))[A-Z]{2})[0-9]{6}[A-D]$""".r

    Form(
      mapping(
        "nino" -> text.verifying("invalid", ninoRegex.pattern.matcher(_).matches())
      )(NINO.apply)(NINO.unapply)
    )
  }

}
