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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.triage

import cats.instances.int._
import cats.syntax.eq._
import com.google.inject.Inject
import play.api.Configuration
import play.api.data.Form
import play.api.data.Forms.{mapping, number}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.Subscribed
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.SessionData
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.IndividualUserType.{Capacitor, PersonalRepresentative, Self}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{IndividualTriageAnswers, IndividualUserType}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

class CanTheyUseOurServiceController @Inject()(
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionStore,
  val errorHandler: ErrorHandler,
  cc: MessagesControllerComponents,
  val config: Configuration,
  whoAreYouReportingForPage: views.html.returns.triage.who_are_you_reporting_for
)(implicit viewConfig: ViewConfig, ec: ExecutionContext)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with Logging
    with SessionUpdates {

  import CanTheyUseOurServiceController._

  def whoIsIndividualRepresenting(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withSubscribedUser(request) {
      case (_, subscribed) =>
        val form = subscribed.individualTriageAnswers
          .flatMap(_.individualUserType)
          .fold(whoAreYouReportingForForm)(whoAreYouReportingForForm.fill)

        Ok(whoAreYouReportingForPage(form))
    }
  }

  def whoIsIndividualRepresentingSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async {
    implicit request =>
      withSubscribedUser(request) {
        case (_, subscribed) =>
          whoAreYouReportingForForm
            .bindFromRequest()
            .fold(
              formWithErrors => BadRequest(whoAreYouReportingForPage(formWithErrors)), { userType =>
                val newSubscribed = subscribed.copy(
                  individualTriageAnswers = Some(
                    subscribed.individualTriageAnswers
                      .map(_.copy(individualUserType = Some(userType)))
                      .getOrElse(IndividualTriageAnswers(Some(userType)))
                  )
                )

                updateSession(sessionStore, request)(_.copy(journeyStatus = Some(newSubscribed)))
                  .map {
                    case Left(e) =>
                      logger.warn("Could not update session", e)
                      errorHandler.errorResult()

                    case Right(_) =>
                      Ok(s"Got user type $userType")
                  }

              }
            )
      }
  }

  private def withSubscribedUser(request: RequestWithSessionData[_])(
    f: (SessionData, Subscribed) => Future[Result]
  ): Future[Result] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some((s: SessionData, r: Subscribed)) =>
        f(s, r)
      case _ =>
        Redirect(uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.routes.StartController.start())
    }

}

object CanTheyUseOurServiceController {

  val whoAreYouReportingForForm: Form[IndividualUserType] = Form(
    mapping(
      "individualUserType" -> number
        .verifying("error.invalid", a => a === 0 || a === 1 || a === 2)
        .transform[IndividualUserType](
          value => if (value === 0) Self else if (value === 1) Capacitor else PersonalRepresentative, {
            case Self                   => 0
            case Capacitor              => 1
            case PersonalRepresentative => 2
          }
        )
    )(identity)(Some(_))
  )

}
