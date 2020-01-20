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

import java.time.{Clock, LocalDate, ZoneId}

import cats.instances.int._
import cats.syntax.eq._
import com.google.inject.Inject
import configs.syntax._
import play.api.Configuration
import play.api.data.Form
import play.api.data.Forms.{mapping, number, of}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.Subscribed
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.LocalDateUtils.configs
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.IndividualUserType.{Capacitor, PersonalRepresentative, Self}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.NumberOfProperties.{MoreThanOne, One}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{DisposalDate, IndividualTriageAnswers, IndividualUserType, NumberOfProperties}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{LocalDateUtils, SessionData}
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
  whoAreYouReportingForPage: views.html.returns.triage.who_are_you_reporting_for,
  howManyPropertiesPage: views.html.returns.triage.how_many_properties,
  disposalDatePage: views.html.returns.triage.disposal_date
)(implicit viewConfig: ViewConfig, ec: ExecutionContext)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with Logging
    with SessionUpdates {

  import CanTheyUseOurServiceController._

  val earliestDisposalDateInclusive: LocalDate =
    config.underlying.get[LocalDate]("returns.earliest-disposal-date-inclusive").value

  val clock: Clock = Clock.systemUTC()

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
                      .getOrElse(IndividualTriageAnswers.empty.copy(individualUserType = Some(userType)))
                  )
                )

                updateSession(sessionStore, request)(_.copy(journeyStatus = Some(newSubscribed)))
                  .map {
                    case Left(e) =>
                      logger.warn("Could not update session", e)
                      errorHandler.errorResult()

                    case Right(_) =>
                      userType match {
                        case IndividualUserType.Self =>
                          Redirect(routes.CanTheyUseOurServiceController.howManyProperties())

                        case other => Ok(s"$other not handled yet")
                      }

                  }

              }
            )
      }
  }

  def howManyProperties(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withIndividualTriageAnswers(request) {
      case (_, subscribed, individualTriageAnswers) =>
        if (individualTriageAnswers.individualUserType.isEmpty) {
          Redirect(routes.CanTheyUseOurServiceController.whoIsIndividualRepresenting())
        } else {
          val form = subscribed.individualTriageAnswers
            .flatMap(_.numberOfProperties)
            .fold(numberOfPropertiesForm)(numberOfPropertiesForm.fill)

          Ok(howManyPropertiesPage(form))
        }

    }
  }

  def howManyPropertiesSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withIndividualTriageAnswers(request) {
      case (_, subscribed, individualTriageAnswers) =>
        if (individualTriageAnswers.individualUserType.isEmpty) {
          Redirect(routes.CanTheyUseOurServiceController.whoIsIndividualRepresenting())
        } else {
          numberOfPropertiesForm
            .bindFromRequest()
            .fold(
              formWithErrors => BadRequest(howManyPropertiesPage(formWithErrors)), { numberOfProperties =>
                val newSubscribed = subscribed.copy(
                  individualTriageAnswers =
                    Some(individualTriageAnswers.copy(numberOfProperties = Some(numberOfProperties)))
                )

                updateSession(sessionStore, request)(_.copy(journeyStatus = Some(newSubscribed)))
                  .map {
                    case Left(e) =>
                      logger.warn("Could not update session", e)
                      errorHandler.errorResult()

                    case Right(_) =>
                      numberOfProperties match {
                        case NumberOfProperties.One =>
                        Redirect(routes.CanTheyUseOurServiceController.whenWasDisposalDate())

                        case NumberOfProperties.MoreThanOne =>
                          Ok("multiple disposals not handled yet")
                      }
                  }

              }
            )
        }
    }
  }

  def whenWasDisposalDate(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withIndividualTriageAnswers(request) {
      case (_, subscribed, individualTriageAnswers) =>
        if (individualTriageAnswers.numberOfProperties.isEmpty) {
          Redirect(routes.CanTheyUseOurServiceController.howManyProperties())
        } else {
          val today = LocalDate.now(clock)
          val form = subscribed.individualTriageAnswers
            .flatMap(_.disposalDate)
            .fold(disposalDateForm(today))(disposalDateForm(today).fill)


          Ok(disposalDatePage(form))
        }
    }
  }

  def whenWasDisposalDateSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withIndividualTriageAnswers(request) {
      case (_, subscribed, individualTriageAnswers) =>
        if (individualTriageAnswers.numberOfProperties.isEmpty) {
          Redirect(routes.CanTheyUseOurServiceController.howManyProperties())
        } else {
          disposalDateForm(LocalDate.now(clock))
            .bindFromRequest()
            .fold(
              formWithErrors => BadRequest(disposalDatePage(formWithErrors)), { d =>
                val newSubscribed = subscribed.copy(
                  individualTriageAnswers = Some(individualTriageAnswers.copy(disposalDate = Some(d)))
                )

                updateSession(sessionStore, request)(_.copy(journeyStatus = Some(newSubscribed))).map({
                  case Left(e) =>
                    logger.warn("Could not update session", e)
                    errorHandler.errorResult()

                  case Right(_) =>
                    if (d.value.isBefore(earliestDisposalDateInclusive)) {
                      Ok(s"disposal date was strictly before $earliestDisposalDateInclusive")
                    } else {
                      Ok(s"disposal date was on or after $earliestDisposalDateInclusive")
                    }

                })
              }
            )
        }
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

  private def withIndividualTriageAnswers(request: RequestWithSessionData[_])(
    f: (SessionData, Subscribed, IndividualTriageAnswers) => Future[Result]
  ): Future[Result] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some((s: SessionData, r @ Subscribed(_, _, _, Some(i)))) =>
        f(s, r, i)
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

  val numberOfPropertiesForm: Form[NumberOfProperties] = Form(
    mapping(
      "numberOfProperties" -> number
        .verifying("error.invalid", a => a === 0 || a === 1)
        .transform[NumberOfProperties](
          value => if (value === 0) One else MoreThanOne, {
            case One         => 0
            case MoreThanOne => 1
          }
        )
    )(identity)(Some(_))
  )

  def disposalDateForm(maximumDateInclusive: LocalDate): Form[DisposalDate] = Form(
    mapping(
      "date" -> of(
        LocalDateUtils.dateFormatter(
          maximumDateInclusive,
          "disposalDate-day",
          "disposalDate-month",
          "disposalDate-year",
          "disposalDate"
        )
      )
    )(DisposalDate(_))(d => Some(d.value))
  )

}
