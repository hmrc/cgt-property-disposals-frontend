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

import play.api.mvc._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{RequestWithSessionData, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.ContactName
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{JourneyStatus, SessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

trait ContactNameController[J <: JourneyStatus] {
  this: FrontendController with WithAuthAndSessionDataAction with SessionUpdates with Logging =>

  implicit val viewConfig: ViewConfig
  implicit val ec: ExecutionContext

  val enterContactNamePage: views.html.contactname.contact_name

  val sessionStore: SessionStore
  val errorHandler: ErrorHandler
  val isAmendJourney: Boolean

  def validJourney(request: RequestWithSessionData[_]): Either[Result, (SessionData, J)]

  def updateContactName(journey: J, contactName: ContactName): JourneyStatus

  def contactName(journey: J): Option[ContactName]

  protected val backLinkCall: Call
  protected val enterContactNameSubmitCall: Call
  protected val continueCall: Call

  private def withValidJourney(request: RequestWithSessionData[_])(
    f: (SessionData, J) => Future[Result]
  ): Future[Result] =
    validJourney(request).fold[Future[Result]](toFuture, f.tupled)

  def enterContactName(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withValidJourney(request) {
      case (_, journey) =>
        val form = contactName(journey).fold(ContactName.form)(ContactName.form.fill)
        Ok(enterContactNamePage(form, isAmendJourney, backLinkCall, enterContactNameSubmitCall))
    }
  }

  //TODO: make the call to the subscription update service
  def enterContactNameSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withValidJourney(request) {
      case (_, journey) =>
        ContactName.form
          .bindFromRequest()
          .fold(
            e => BadRequest(enterContactNamePage(e, isAmendJourney, backLinkCall, enterContactNameSubmitCall)),
            name =>
              updateSession(sessionStore, request)(
                _.copy(journeyStatus = Some(updateContactName(journey, name)))
              ).map {
                case Left(e) =>
                  logger.warn("Could not submit contact name", e)
                  errorHandler.errorResult()

                case Right(_) =>
                  Redirect(continueCall)
              }
          )
    }
  }
}
