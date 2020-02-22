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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.testonly

import java.time.LocalDate
import java.time.format.DateTimeFormatter

import cats.Eq
import cats.data.OptionT
import cats.instances.either._
import cats.instances.string._
import cats.syntax.either._
import cats.syntax.order._
import com.google.inject.Inject
import configs.syntax._
import play.api.Configuration
import play.api.data.Forms.{mapping, of}
import play.api.data.format.Formatter
import play.api.data.{Form, FormError}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.ViewConfig
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.StartingNewDraftReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.LocalDateUtils.order
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Country
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.TriageAnswers.IncompleteTriageAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{SessionData, TaxYear}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.testonly.JourneyStatusController._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

class JourneyStatusController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  config: Configuration,
  sessionStore: SessionStore,
  cc: MessagesControllerComponents,
  setReturnStatePage: views.html.testonly.set_return_state,
  sign_in_page: views.html.sign_in
)(implicit viewConfig: ViewConfig, ec: ExecutionContext)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionUpdates
    with Logging {

  val taxYears: List[TaxYear] =
    config.underlying.get[List[TaxYear]]("tax-years").value

  def setReturnState(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withStartingNewReturn(request) {
      case (_, _) =>
        Ok(setReturnStatePage((returnStateForm(taxYears))))
    }
  }

  def setReturnStateSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withStartingNewReturn(request) {
      case (_, newReturn) =>
        returnStateForm(taxYears)
          .bindFromRequest()
          .fold(
            formWithErrors => BadRequest(setReturnStatePage(formWithErrors)), { state =>
              updateSession(sessionStore, request)(
                _.copy(journeyStatus = Some(newReturn.copy(newReturnTriageAnswers = state)))
              ).map {
                case Left(e) =>
                  logger.warn("Could not update session", e)
                  InternalServerError(s"Could not update session: $e")
                case Right(_) =>
                  Ok("session updated")
              }
            }
          )
    }
  }

  private def withStartingNewReturn(request: RequestWithSessionData[_])(
    f: (SessionData, StartingNewDraftReturn) => Future[Result]
  ): Future[Result] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some((s: SessionData, r: StartingNewDraftReturn)) =>
        f(s, r)
      case _ =>
        BadRequest("User has not subscribed and logged in")
    }

}

object JourneyStatusController {

  val individualUserTypeFormatter: Formatter[Option[IndividualUserType]] = optionFormatter[IndividualUserType](
    Map(
      IndividualUserType.Self                   -> "self",
      IndividualUserType.Capacitor              -> "capacitor",
      IndividualUserType.PersonalRepresentative -> "personal-representative"
    )
  )

  val numberOfPropertiesFormatter: Formatter[Option[NumberOfProperties]] = optionFormatter[NumberOfProperties](
    Map(
      NumberOfProperties.One         -> "one",
      NumberOfProperties.MoreThanOne -> "moreThanOne"
    )
  )

  val disposalMethodFormatter: Formatter[Option[DisposalMethod]] = optionFormatter[DisposalMethod](
    Map(
      DisposalMethod.Sold   -> "sold",
      DisposalMethod.Gifted -> "gifted"
    )
  )

  val optionalBooleanFormatter: Formatter[Option[Boolean]] = new Formatter[Option[Boolean]] {
    override def bind(key: String, data: Map[String, String]): Either[Seq[FormError], Option[Boolean]] =
      data
        .get(key)
        .filter(_.nonEmpty)
        .fold[Either[Seq[FormError], Option[Boolean]]](
          Right(None)
        )(s =>
          Either
            .fromTry(Try(s.toBoolean))
            .bimap(
              _ => Seq(FormError(key, "error.invalid")),
              Some(_)
            )
        )

    override def unbind(key: String, value: Option[Boolean]): Map[String, String] =
      value.fold(Map.empty[String, String])(b => Map(key -> b.toString))

  }

  val optionalDateFormatter: Formatter[Option[LocalDate]] = new Formatter[Option[LocalDate]] {
    override def bind(key: String, data: Map[String, String]): Either[Seq[FormError], Option[LocalDate]] =
      data
        .get(key)
        .filter(_.nonEmpty)
        .fold[Either[Seq[FormError], Option[LocalDate]]](
          Right(None)
        )(s =>
          Either
            .fromTry(Try(LocalDate.parse(s, DateTimeFormatter.ISO_DATE)))
            .bimap(
              _ => Seq(FormError(key, "error.invalid")),
              Some(_)
            )
        )

    override def unbind(key: String, value: Option[LocalDate]): Map[String, String] =
      value.fold(Map.empty[String, String])(d => Map(key -> d.format(DateTimeFormatter.ISO_DATE)))
  }

  def disposalDateFormatter(taxYears: List[TaxYear]): Formatter[Option[DisposalDate]] =
    new Formatter[Option[DisposalDate]] {
      override def bind(key: String, data: Map[String, String]): Either[Seq[FormError], Option[DisposalDate]] =
        OptionT[Either[Seq[FormError], ?], LocalDate](
          optionalDateFormatter.bind(key, data)
        ).semiflatMap { d =>
          Either
            .fromOption(
              taxYears.find(t => d < t.endDateExclusive && d >= (t.startDateInclusive)),
              Seq(FormError("disposal-date", "could not find tax year"))
            )
            .map(DisposalDate(d, _))
        }.value

      override def unbind(key: String, value: Option[DisposalDate]): Map[String, String] =
        optionalDateFormatter.unbind(key, value.map(_.value))
    }

  def returnStateForm(taxYears: List[TaxYear]): Form[IncompleteTriageAnswers] =
    Form(
      mapping(
        "individual-user-type"             -> of(individualUserTypeFormatter),
        "number-of-properties"             -> of(numberOfPropertiesFormatter),
        "disposal-method"                  -> of(disposalMethodFormatter),
        "was-a-uk-resident"                -> of(optionalBooleanFormatter),
        "disposed-of-residential-property" -> of(optionalBooleanFormatter),
        "disposal-date"                    -> of(disposalDateFormatter(taxYears)),
        "completion-date"                  -> of(optionalDateFormatter)
      ) {
        case (
            individualUserType,
            numberOfProperties,
            disposalMethod,
            wasAUKResident,
            disposedOfResidentialProperty,
            disposalDate,
            completionDate
            ) =>
          IncompleteTriageAnswers(
            individualUserType,
            numberOfProperties,
            disposalMethod,
            wasAUKResident,
            wasAUKResident.map(if (_) Country.uk else Country("HK", Some("Hong Kong"))),
            disposedOfResidentialProperty.map(if (_) AssetType.Residential else AssetType.NonResidential),
            disposalDate,
            completionDate.map(CompletionDate(_))
          )
      } { i =>
        Some(
          (
            i.individualUserType,
            i.numberOfProperties,
            i.disposalMethod,
            i.wasAUKResident,
            i.assetType.map(_ === AssetType.Residential),
            i.disposalDate,
            i.completionDate.map(_.value)
          )
        )
      }
    )

  implicit class MapOps[A, B](private val m: Map[A, B]) extends AnyVal {

    def getKey(b: B)(implicit eq: Eq[B]): Option[A] = m.find(_._2 === b).map(_._1)

  }

  // empty strings will map to `None`, strings not recognised by `stringToA` will result in a form
  // error
  def optionFormatter[A](aToString: Map[A, String]): Formatter[Option[A]] = new Formatter[Option[A]] {
    override def bind(key: String, data: Map[String, String]): Either[Seq[FormError], Option[A]] =
      data
        .get(key)
        .filter(_.nonEmpty)
        .fold[Either[Seq[FormError], Option[A]]](
          Right(None)
        )(s => Either.fromOption(aToString.getKey(s), Seq(FormError(key, "error.invalid"))).map(Some(_)))

    override def unbind(key: String, value: Option[A]): Map[String, String] =
      value.map(aToString).fold(Map.empty[String, String])(s => Map(key -> s))
  }

}
