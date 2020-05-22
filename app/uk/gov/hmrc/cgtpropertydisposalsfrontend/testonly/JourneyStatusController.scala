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
import cats.data.EitherT
import cats.instances.future._
import cats.instances.option._
import cats.instances.string._
import cats.syntax.either._
import cats.syntax.order._
import cats.syntax.traverse._
import com.google.inject.Inject
import play.api.data.Forms.{mapping, of}
import play.api.data.format.Formatter
import play.api.data.{Form, FormError}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.ViewConfig
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.StartingNewDraftReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Country
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.IncompleteSingleDisposalTriageAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, SessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.TaxYearService
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
  taxYearService: TaxYearService,
  sessionStore: SessionStore,
  cc: MessagesControllerComponents,
  setReturnStatePage: views.html.testonly.set_return_state
)(implicit viewConfig: ViewConfig, ec: ExecutionContext)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionUpdates
    with Logging {

  def setReturnState(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withStartingNewReturn(request) {
        case (_, _) =>
          Ok(setReturnStatePage(returnStateForm))
      }
    }

  def setReturnStateSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withStartingNewReturn(request) {
        case (_, newReturn) =>
          returnStateForm
            .bindFromRequest()
            .fold(
              formWithErrors => BadRequest(setReturnStatePage(formWithErrors)),
              { answers =>
                val getDisposalDate =
                  answers.disposalDate
                    .map(date =>
                      taxYearService
                        .taxYear(date)
                        .subflatMap(
                          _.fold[Either[Error, DisposalDate]](
                            Left(Error(s"Could not find tax year for $date"))
                          )(t => Right(DisposalDate(date, t)))
                        )
                    )
                    .sequence[EitherT[Future, Error, ?], DisposalDate]

                val result = for {
                  disposalDate <- getDisposalDate
                  triageAnswers = toSingleDisposalTriageAnswers(answers, disposalDate)
                  _            <- EitherT(
                         updateSession(sessionStore, request)(
                           _.copy(journeyStatus =
                             Some(
                               newReturn.copy(newReturnTriageAnswers = Right(triageAnswers))
                             )
                           )
                         )
                       )
                } yield ()

                result.fold(
                  { e =>
                    logger.warn("Could not update session", e)
                    InternalServerError(s"Could not update session: $e")
                  },
                  _ => Ok("session updated")
                )
              }
            )
      }
    }

  private def toSingleDisposalTriageAnswers(
    answers: Answers,
    disposalDate: Option[DisposalDate]
  ): IncompleteSingleDisposalTriageAnswers =
    IncompleteSingleDisposalTriageAnswers(
      answers.individualUserType,
      answers.numberOfProperties.contains(NumberOfProperties.One),
      answers.disposalMethod,
      answers.wasAUkResident,
      answers.wasAUkResident
        .map(if (_) Country.uk else Country("HK", Some("Hong Kong"))),
      answers.disposedOfResidentialProperty
        .map(if (_) AssetType.Residential else AssetType.NonResidential),
      disposalDate,
      answers.completionDate.map(CompletionDate(_)),
      None
    )

  private def withStartingNewReturn(request: RequestWithSessionData[_])(
    f: (SessionData, StartingNewDraftReturn) => Future[Result]
  ): Future[Result] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some((s: SessionData, r: StartingNewDraftReturn)) =>
        f(s, r)
      case _                                                 =>
        BadRequest("User has not subscribed and logged in")
    }

}

object JourneyStatusController {

  final case class Answers(
    individualUserType: Option[IndividualUserType],
    numberOfProperties: Option[NumberOfProperties],
    disposalMethod: Option[DisposalMethod],
    wasAUkResident: Option[Boolean],
    disposedOfResidentialProperty: Option[Boolean],
    disposalDate: Option[LocalDate],
    completionDate: Option[LocalDate]
  )

  val individualUserTypeFormatter: Formatter[Option[IndividualUserType]] =
    optionFormatter[IndividualUserType](
      Map(
        IndividualUserType.Self                   -> "self",
        IndividualUserType.Capacitor              -> "capacitor",
        IndividualUserType.PersonalRepresentative -> "personal-representative"
      )
    )

  val numberOfPropertiesFormatter: Formatter[Option[NumberOfProperties]] =
    optionFormatter[NumberOfProperties](
      Map(
        NumberOfProperties.One         -> "one",
        NumberOfProperties.MoreThanOne -> "moreThanOne"
      )
    )

  val disposalMethodFormatter: Formatter[Option[DisposalMethod]] =
    optionFormatter[DisposalMethod](
      Map(
        DisposalMethod.Sold   -> "sold",
        DisposalMethod.Gifted -> "gifted"
      )
    )

  val optionalBooleanFormatter: Formatter[Option[Boolean]] =
    new Formatter[Option[Boolean]] {
      override def bind(
        key: String,
        data: Map[String, String]
      ): Either[Seq[FormError], Option[Boolean]] =
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

      override def unbind(
        key: String,
        value: Option[Boolean]
      ): Map[String, String] =
        value.fold(Map.empty[String, String])(b => Map(key -> b.toString))

    }

  val optionalDateFormatter: Formatter[Option[LocalDate]] =
    new Formatter[Option[LocalDate]] {
      override def bind(
        key: String,
        data: Map[String, String]
      ): Either[Seq[FormError], Option[LocalDate]] =
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

      override def unbind(
        key: String,
        value: Option[LocalDate]
      ): Map[String, String] =
        value.fold(Map.empty[String, String])(d => Map(key -> d.format(DateTimeFormatter.ISO_DATE)))
    }

  val returnStateForm: Form[Answers] =
    Form(
      mapping(
        "individual-user-type"             -> of(individualUserTypeFormatter),
        "number-of-properties"             -> of(numberOfPropertiesFormatter),
        "disposal-method"                  -> of(disposalMethodFormatter),
        "was-a-uk-resident"                -> of(optionalBooleanFormatter),
        "disposed-of-residential-property" -> of(optionalBooleanFormatter),
        "disposal-date"                    -> of(optionalDateFormatter),
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
          Answers(
            individualUserType,
            numberOfProperties,
            disposalMethod,
            wasAUKResident,
            disposedOfResidentialProperty,
            disposalDate,
            completionDate
          )
      } { a =>
        Some(
          (
            a.individualUserType,
            a.numberOfProperties,
            a.disposalMethod,
            a.wasAUkResident,
            a.disposedOfResidentialProperty,
            a.disposalDate,
            a.completionDate
          )
        )
      }
    )

  implicit class MapOps[A, B](private val m: Map[A, B]) extends AnyVal {

    def getKey(b: B)(implicit eq: Eq[B]): Option[A] =
      m.find(_._2 === b).map(_._1)

  }

  // empty strings will map to `None`, strings not recognised by `stringToA` will result in a form
  // error
  def optionFormatter[A](aToString: Map[A, String]): Formatter[Option[A]] =
    new Formatter[Option[A]] {
      override def bind(
        key: String,
        data: Map[String, String]
      ): Either[Seq[FormError], Option[A]] =
        data
          .get(key)
          .filter(_.nonEmpty)
          .fold[Either[Seq[FormError], Option[A]]](
            Right(None)
          )(s =>
            Either
              .fromOption(
                aToString.getKey(s),
                Seq(FormError(key, "error.invalid"))
              )
              .map(Some(_))
          )

      override def unbind(key: String, value: Option[A]): Map[String, String] =
        value.map(aToString).fold(Map.empty[String, String])(s => Map(key -> s))
    }

}
