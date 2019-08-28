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

import com.google.inject.Inject
import play.api.data.Form
import play.api.data.Forms.{boolean, mapping}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.ViewConfig
import uk.gov.hmrc.play.bootstrap.controller.FrontendController
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views

class InsufficientConfidenceLevelController @Inject()(
                                                       doYouHaveANinoPage: views.html.do_you_have_a_nino,
                                                       cc: MessagesControllerComponents)
                                                     (
                                                       implicit viewConfig: ViewConfig
                                                     )
  extends FrontendController(cc){

  def doYouHaveNINO(): Action[AnyContent] = Action { implicit request =>
    Ok(doYouHaveANinoPage(InsufficientConfidenceLevelController.haveANinoForm))
  }

  def doYouHaveNINOSubmit(): Action[AnyContent] = Action { implicit request =>
    InsufficientConfidenceLevelController.haveANinoForm.bindFromRequest().fold(
      e => BadRequest(doYouHaveANinoPage(e)),
      boolean => Ok(s"Got answer $boolean")
    )

  }



}


object InsufficientConfidenceLevelController {

  val haveANinoForm: Form[Boolean] =
    Form(
      mapping(
        "hasNino" -> boolean
      )(identity)(Some(_))
    )


}