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

import cats.syntax.either._
import com.google.inject.Inject
import play.api.Configuration
import play.api.data.{Form, FormError}
import play.api.data.Forms.{boolean, mapping, of}
import play.api.data.format.Formatter
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.SubscriptionStatus.IndividualInsufficientConfidenceLevel
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging
import uk.gov.hmrc.play.bootstrap.controller.FrontendController
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views

import scala.concurrent.{ExecutionContext, Future}

class InsufficientConfidenceLevelController @Inject()(
                                                       val authenticatedAction: AuthenticatedAction,
                                                       val sessionDataAction: SessionDataAction,
                                                       val sessionStore: SessionStore,
                                                       val errorHandler: ErrorHandler,
                                                       val config: Configuration,
                                                       doYouHaveANinoPage: views.html.do_you_have_a_nino,
                                                       cc: MessagesControllerComponents)
                                                     (
                                                       implicit viewConfig: ViewConfig,
                                                       ec: ExecutionContext
                                                     )
  extends FrontendController(cc)
    with IvBehaviour
    with Logging
    with WithAuthAndSessionDataAction
    with DefaultRedirects {
  import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.toFuture

  private def withInsufficientConfidenceLevelUser(
    f: => Future[Result])(implicit request: RequestWithSessionData[_]): Future[Result] =
    request.sessionData.flatMap(_.subscriptionStatus) match {
      case Some(IndividualInsufficientConfidenceLevel) => f
      case other                             => defaultRedirect(other)
    }


  def doYouHaveNINO(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withInsufficientConfidenceLevelUser{
      Ok(doYouHaveANinoPage(InsufficientConfidenceLevelController.haveANinoForm))
    }
  }

  def doYouHaveNINOSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withInsufficientConfidenceLevelUser {
      InsufficientConfidenceLevelController.haveANinoForm.bindFromRequest().fold(
        e => BadRequest(doYouHaveANinoPage(e)),
        hasNino =>
          if (hasNino) {
            redirectToIv
          } else {
            Ok(s"Got answer $hasNino")
          }
      )
    }
  }

}


object InsufficientConfidenceLevelController {

  //don't want to use out-of-box boolean formatter - that one defaults null values to false
  val booleanFormatter: Formatter[Boolean] = new Formatter[Boolean] {

    override val format = Some(("format.boolean", Nil))

    def bind(key: String, data: Map[String, String]): Either[Seq[FormError], Boolean] = {
      Either.fromOption(data.get(key), Seq(FormError(key, "error.required")))
        .flatMap{
          case "true"  => Right(true)
          case "false" => Right(false)
          case _       => Left(Seq(FormError(key, "error.boolean", Nil)))
        }
    }

    def unbind(key: String, value: Boolean): Map[String, String] = Map(key -> value.toString)
  }


  val haveANinoForm: Form[Boolean] =
    Form(
      mapping(
        "hasNino" -> of(booleanFormatter)
      )(identity)(Some(_))
    )


}