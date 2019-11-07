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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.name

import play.api.mvc.{Action, AnyContent, Call, Result}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{RequestWithSessionData, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.IndividualName
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{JourneyStatus, SessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

trait IndividualNameController[J <: JourneyStatus] {
  this: FrontendController with WithAuthAndSessionDataAction with SessionUpdates with Logging =>

  implicit val viewConfig: ViewConfig
  implicit val ec: ExecutionContext

  val enterNamePage: views.html.name.enter_name

  val sessionStore: SessionStore
  val errorHandler: ErrorHandler

  def validJourney(request: RequestWithSessionData[_]): Either[Result, (SessionData, J)]

  def updateName(journey: J, name: IndividualName): JourneyStatus

  def name(journey: J): Option[IndividualName]

  protected val backLinkCall: Call
  protected val enterNameSubmitCall: Call
  protected val continueCall: Call

  private def withValidJourney(request: RequestWithSessionData[_])(
    f: (SessionData, J) => Future[Result]
  ): Future[Result] =
    validJourney(request).fold[Future[Result]](toFuture, f.tupled)

  def enterIndividualName(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withValidJourney(request) {
      case (_, journey) =>
        val form = {
          name(journey).fold(IndividualName.form)(IndividualName.form.fill)
        }

        Ok(enterNamePage(form, backLinkCall, enterNameSubmitCall))
    }
  }

  def enterIndividualNameSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withValidJourney(request) {
      case (_, journey) =>
        IndividualName.form
          .bindFromRequest()
          .fold(
            e => BadRequest(enterNamePage(e, backLinkCall, enterNameSubmitCall)),
            name =>
              updateSession(sessionStore, request)(
                _.copy(journeyStatus = Some(updateName(journey, name)))
              ).map {
                case Left(e) =>
                  logger.warn("Could not update registration status with name", e)
                  errorHandler.errorResult()

                case Right(_) =>
                  Redirect(continueCall)
              }
          )
    }
  }

}
