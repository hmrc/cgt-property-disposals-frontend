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

import java.time.{Clock, LocalDate}

import cats.Eq
import cats.instances.int._
import cats.syntax.either._
import cats.syntax.eq._
import com.google.inject.Inject
import configs.syntax._
import play.api.Configuration
import play.api.data.Forms.{mapping, of}
import play.api.data.format.Formatter
import play.api.data.{Form, FormError}
import play.api.http.Writeable
import play.api.mvc._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.Subscribed
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.LocalDateUtils.configs
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.DisposalMethod.{Gifted, Sold}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.IndividualUserType.{Capacitor, PersonalRepresentative, Self}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.NumberOfProperties.{MoreThanOne, One}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{BooleanFormatter, LocalDateUtils, SessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views.html.returns.{triage => triagePages}
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

class CanTheyUseOurServiceController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionStore,
  val errorHandler: ErrorHandler,
  cc: MessagesControllerComponents,
  val config: Configuration,
  whoAreYouReportingForPage: triagePages.who_are_you_reporting_for,
  howManyPropertiesPage: triagePages.how_many_properties,
  disposalMethodPage: triagePages.how_did_you_dispose,
  wereYouAUKResidentPage: triagePages.were_you_a_uk_resident,
  didYouDisposeOfResidentialPropertyPage: triagePages.did_you_dispose_of_residential_property,
  disposalDatePage: triagePages.disposal_date,
  completionDatePage: triagePages.completion_date
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
    displayIndividualTriagePage(
      _.individualUserType,
      routes.CanTheyUseOurServiceController.whoIsIndividualRepresenting()
    )(_ => numberOfPropertiesForm)(_.numberOfProperties, howManyPropertiesPage(_))
  }

  def howManyPropertiesSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    handleIndividualTriagePageSubmit(
      _.individualUserType,
      routes.CanTheyUseOurServiceController.whoIsIndividualRepresenting()
    )(_ => numberOfPropertiesForm)(
      howManyPropertiesPage(_),
      updateState = { case (numberOfProperties, i) => i.copy(numberOfProperties = Some(numberOfProperties)) },
      nextResult = {
        case NumberOfProperties.One =>
          Redirect(routes.CanTheyUseOurServiceController.howDidYouDisposeOfProperty())

        case NumberOfProperties.MoreThanOne =>
          Ok("multiple disposals not handled yet")
      }
    )
  }

  def howDidYouDisposeOfProperty(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    displayIndividualTriagePage(_.numberOfProperties, routes.CanTheyUseOurServiceController.howManyProperties())(_ =>
      disposalMethodForm
    )(_.disposalMethod, disposalMethodPage(_))
  }

  def howDidYouDisposeOfPropertySubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async {
    implicit request =>
      handleIndividualTriagePageSubmit(_.numberOfProperties, routes.CanTheyUseOurServiceController.howManyProperties())(
        _ => disposalMethodForm
      )(
        disposalMethodPage(_),
        updateState = { case (disposalMethod, i) => i.copy(disposalMethod = Some(disposalMethod)) },
        nextResult  = _ => Redirect(routes.CanTheyUseOurServiceController.wereYouAUKResident())
      )
  }

  def wereYouAUKResident(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    displayIndividualTriagePage(
      _.disposalMethod,
      routes.CanTheyUseOurServiceController.howDidYouDisposeOfProperty()
    )(_ => wasAUkResidentForm)(_.wasAUKResident, wereYouAUKResidentPage(_))
  }

  def wereYouAUKResidentSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    handleIndividualTriagePageSubmit(
      _.disposalMethod,
      routes.CanTheyUseOurServiceController.howDidYouDisposeOfProperty()
    )(_ => wasAUkResidentForm)(
      wereYouAUKResidentPage(_),
      updateState = { case (wasAUKResident, i) => i.copy(wasAUKResident = Some(wasAUKResident)) },
      nextResult = { wasAUKResident =>
        if (wasAUKResident) {
          Redirect(routes.CanTheyUseOurServiceController.didYouDisposeOfAResidentialProperty())
        } else {
          Ok("non residents not handled yet")
        }
      }
    )
  }

  def didYouDisposeOfAResidentialProperty(): Action[AnyContent] = authenticatedActionWithSessionData.async {
    implicit request =>
      displayIndividualTriagePage(_.wasAUKResident, routes.CanTheyUseOurServiceController.wereYouAUKResident())(_ =>
        wasResidentialPropertyForm
      )(_.wasResidentialProperty, didYouDisposeOfResidentialPropertyPage(_))
  }

  def didYouDisposeOfAResidentialPropertySubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async {
    implicit request =>
      handleIndividualTriagePageSubmit(_.wasAUKResident, routes.CanTheyUseOurServiceController.wereYouAUKResident())(
        _ => wasResidentialPropertyForm
      )(
        didYouDisposeOfResidentialPropertyPage(_),
        updateState = {
          case (wasResidentialProperty, i) => i.copy(wasResidentialProperty = Some(wasResidentialProperty))
        },
        nextResult = { wasResidentialProperty =>
          if (wasResidentialProperty) {
            Redirect(routes.CanTheyUseOurServiceController.whenWasDisposalDate())
          } else {
            Ok("individuals can only report on residential properties")
          }
        }
      )
  }

  def whenWasDisposalDate(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    val today = LocalDate.now(clock)
    displayIndividualTriagePage(
      _.wasResidentialProperty,
      routes.CanTheyUseOurServiceController.didYouDisposeOfAResidentialProperty()
    )(_ => disposalDateForm(LocalDate.now(clock)))(_.disposalDate, disposalDatePage(_))
  }

  def whenWasDisposalDateSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    handleIndividualTriagePageSubmit(
      _.wasResidentialProperty,
      routes.CanTheyUseOurServiceController.didYouDisposeOfAResidentialProperty()
    )(_ => disposalDateForm(LocalDate.now(clock)))(
      disposalDatePage(_),
      updateState = { case (d, i) => i.copy(disposalDate = Some(d)) },
      nextResult = { d =>
        if (d.value.isBefore(earliestDisposalDateInclusive)) {
          Ok(s"disposal date was strictly before $earliestDisposalDateInclusive")
        } else {
          Redirect(routes.CanTheyUseOurServiceController.whenWasCompletionDate())
        }
      }
    )
  }

  def whenWasCompletionDate(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    displayIndividualTriagePage(
      _.disposalDate,
      routes.CanTheyUseOurServiceController.whenWasDisposalDate()
    )(disposalDate => completionDateForm(disposalDate))(_.completionDate, completionDatePage(_))
  }

  def whenWasCompletionDateSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    handleIndividualTriagePageSubmit(
      _.disposalDate,
      routes.CanTheyUseOurServiceController.whenWasDisposalDate()
    )(disposalDate => completionDateForm(disposalDate))(
      completionDatePage(_),
      updateState = { case (d, i) => i.copy(completionDate = Some(d)) },
      nextResult = { _ =>
        Ok("Got completion date")
      }
    )
  }

  private def handleIndividualTriagePageSubmit[R, A, Page: Writeable](
    requiredField: IndividualTriageAnswers => Option[R],
    redirectToIfNotValidJourney: => Call
  )(
    form: R => Form[A]
  )(
    page: Form[A] => Page,
    updateState: (A, IndividualTriageAnswers) => IndividualTriageAnswers,
    nextResult: A => Result
  )(
    implicit request: RequestWithSessionData[_]
  ): Future[Result] =
    withIndividualTriageAnswers(request) {
      case (_, subscribed, individualTriageAnswers) =>
        requiredField(individualTriageAnswers) match {
          case None => Redirect(redirectToIfNotValidJourney)
          case Some(r) =>
            form(r)
              .bindFromRequest()
              .fold(
                formWithErrors => BadRequest(page(formWithErrors)), { value =>
                  val newSubscribed = subscribed.copy(
                    individualTriageAnswers = Some(updateState(value, individualTriageAnswers))
                  )

                  updateSession(sessionStore, request)(_.copy(journeyStatus = Some(newSubscribed))).map({
                    case Left(e) =>
                      logger.warn("Could not update session", e)
                      errorHandler.errorResult()

                    case Right(_) =>
                      nextResult(value)
                  })
                }
              )

        }
    }

  private def displayIndividualTriagePage[R, A, Page: Writeable](
    requiredField: IndividualTriageAnswers => Option[R],
    redirectToIfNotValidJourney: => Call
  )(form: R => Form[A])(extractField: IndividualTriageAnswers => Option[A], page: Form[A] => Page)(
    implicit request: RequestWithSessionData[_]
  ): Future[Result] =
    withIndividualTriageAnswers(request) {
      case (_, _, individualTriageAnswers) =>
        requiredField(individualTriageAnswers) match {
          case None => Redirect(redirectToIfNotValidJourney)
          case Some(r) =>
            val f = extractField(individualTriageAnswers)
              .fold(form(r))(form(r).fill)

            Ok(page(f))

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

  def radioFormFormatter[A: Eq](id: String, orderedOptions: List[A]): Formatter[A] = new Formatter[A] {
    val optionsZippedWithIndex = orderedOptions.zipWithIndex

    override def bind(key: String, data: Map[String, String]): Either[Seq[FormError], A] = {
      lazy val invalidError = FormError(key, "error.invalid")
      data
        .get(key)
        .map(_.trim())
        .filter(_.nonEmpty)
        .fold[Either[Seq[FormError], A]](Left(Seq(FormError(key, "error.required")))) { stringValue =>
          Either
            .fromTry(Try(stringValue.toInt))
            .leftMap(_ => Seq(invalidError))
            .flatMap(i => Either.fromOption(optionsZippedWithIndex.find(_._2 === i), Seq(invalidError)).map(_._1))
        }
    }

    override def unbind(key: String, value: A): Map[String, String] =
      optionsZippedWithIndex
        .find(_._1 === value)
        .fold(Map.empty[String, String]) { case (_, i) => Map(key -> i.toString) }
  }

  val whoAreYouReportingForForm: Form[IndividualUserType] = Form(
    mapping(
      "individualUserType" -> of(
        radioFormFormatter("individualUserType", List(Self, Capacitor, PersonalRepresentative))
      )
    )(identity)(Some(_))
  )

  val numberOfPropertiesForm: Form[NumberOfProperties] = Form(
    mapping(
      "numberOfProperties" -> of(radioFormFormatter("numberOfProperties", List(One, MoreThanOne)))
    )(identity)(Some(_))
  )

  val disposalMethodForm: Form[DisposalMethod] = Form(
    mapping(
      "disposalMethod" -> of(radioFormFormatter("disposalMethod", List(Sold, Gifted)))
    )(identity)(Some(_))
  )

  val wasAUkResidentForm: Form[Boolean] = Form(
    mapping(
      "wereYouAUKResident" -> of(BooleanFormatter.formatter)
    )(identity)(Some(_))
  )

  val wasResidentialPropertyForm: Form[Boolean] = Form(
    mapping(
      "didYouDisposeOfResidentialProperty" -> of(BooleanFormatter.formatter)
    )(identity)(Some(_))
  )

  def disposalDateForm(maximumDateInclusive: LocalDate): Form[DisposalDate] = Form(
    mapping(
      "" -> of(
        LocalDateUtils.dateFormatter(
          Some(maximumDateInclusive),
          None,
          "disposalDate-day",
          "disposalDate-month",
          "disposalDate-year",
          "disposalDate"
        )
      )
    )(DisposalDate(_))(d => Some(d.value))
  )

  def completionDateForm(disposalDate: DisposalDate): Form[CompletionDate] = Form(
    mapping(
      "" -> of(
        LocalDateUtils.dateFormatter(
          None,
          Some(disposalDate.value),
          "completionDate-day",
          "completionDate-month",
          "completionDate-year",
          "completionDate"
        )
      )
    )(CompletionDate(_))(d => Some(d.value))
  )
}
