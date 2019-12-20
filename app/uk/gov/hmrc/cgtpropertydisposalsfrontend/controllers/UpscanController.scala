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
import play.api.libs.json.JsValue
import play.api.mvc.{Action, MessagesControllerComponents}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.UpscanService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.ExecutionContext

@Singleton
class UpscanController @Inject()(upscanService: UpscanService, cc: MessagesControllerComponents)(
  implicit ec: ExecutionContext
) extends FrontendController(cc)
    with Logging {

  def callBack(): Action[JsValue] = Action(parse.json) { implicit request =>
    NoContent
  }

  def successCallBack(): Action[JsValue] = Action(parse.json) { implicit request =>
    NoContent
  }

  def errorCallBack(): Action[JsValue] = Action(parse.json) { implicit request =>
    NoContent
  }

}
