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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.config

import play.api.i18n.{Messages, MessagesApi}
import play.api.mvc.Results.InternalServerError
import play.api.mvc.{Request, RequestHeader, Result}
import play.twirl.api.Html
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.RequestWithSessionData
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.UserType
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views
import uk.gov.hmrc.play.bootstrap.frontend.http.FrontendErrorHandler

import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class ErrorHandler @Inject() (
  val messagesApi: MessagesApi,
  error_template: views.html.error_template
)(implicit
  val appConfig: ViewConfig,
  val ec: ExecutionContext
) extends FrontendErrorHandler {
  override def standardErrorTemplate(
    pageTitle: String,
    heading: String,
    message: String
  )(implicit
    request: RequestHeader
  ): Future[Html] =
    Future.successful(error_template(None, pageTitle, heading, message))

  def errorResult[R <: Request[?]](
    userType: Option[UserType]
  )(implicit request: R): Result =
    InternalServerError(
      error_template(
        userType,
        Messages("global.error.InternalServerError500.title"),
        Messages("global.error.InternalServerError500.heading"),
        Messages("global.error.InternalServerError500.message")
      )
    )

  def errorResult()(implicit request: RequestWithSessionData[?]): Result =
    errorResult(request.userType)
}
