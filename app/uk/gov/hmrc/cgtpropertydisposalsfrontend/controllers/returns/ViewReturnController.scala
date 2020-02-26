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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns

import cats.instances.future._
import com.google.inject.{Inject, Singleton}
import play.api.data.Form
import play.api.data.Forms.{mapping, nonEmptyText}
import play.api.libs.json.{JsValue, Json}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.ViewReturnController._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{SessionUpdates, routes => baseRoutes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.ViewingReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

@Singleton
class ViewReturnController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  errorHandler: ErrorHandler,
  sessionStore: SessionStore,
  returnsService: ReturnsService,
  cc: MessagesControllerComponents,
  viewReturnPage: views.html.returns.view_return
)(implicit viewConfig: ViewConfig, ec: ExecutionContext)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionUpdates
    with Logging {

  def displayReturn(): Action[AnyContent] = authenticatedActionWithSessionData { implicit request =>
    request.sessionData.flatMap(_.journeyStatus) match {
      case Some(ViewingReturn(_, _, _, sentReturn)) =>
        Ok(viewReturnPage(sentReturn, amendReturnForm))

      case _ =>
        Redirect(baseRoutes.StartController.start())
    }
  }

  def amendReturn(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    request.sessionData.flatMap(_.journeyStatus) match {
      case Some(v @ ViewingReturn(_, _, _, sentReturn)) =>
        amendReturnForm
          .bindFromRequest()
          .fold[Future[Result]](
            formWithErrors => BadRequest(viewReturnPage(sentReturn, formWithErrors)), { json =>
              returnsService
                .amendReturn(v.subscribedDetails.cgtReference, json)
                .fold({ e =>
                  logger.warn("Could not amend return", e)
                  errorHandler.errorResult()
                }, response => Ok(response.toString))
            }
          )

      case _ =>
        Redirect(baseRoutes.StartController.start())
    }

  }

}

object ViewReturnController {

  implicit val amendReturnForm: Form[JsValue] = Form(
    mapping(
      "amend" -> nonEmptyText.verifying("invalid JSON", s => Try(Json.parse(s)).isSuccess)
    )(Json.parse)(j => Some(j.toString))
  )

}
