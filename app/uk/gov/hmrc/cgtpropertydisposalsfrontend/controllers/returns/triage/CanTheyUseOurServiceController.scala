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

import java.time.LocalDate

import cats.Eq
import cats.data.EitherT
import cats.instances.future._
import cats.instances.int._
import cats.syntax.either._
import cats.syntax.order._
import com.google.inject.{Inject, Singleton}
import configs.syntax._
import play.api.Configuration
import play.api.data.Forms.{mapping, of}
import play.api.data.format.Formatter
import play.api.data.{Form, FormError}
import play.api.http.Writeable
import play.api.mvc._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.accounts.homepage.{routes => homeRoutes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.{routes => returnsRoutes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, StartingNewDraftReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.LocalDateUtils.{configs, order}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.UUIDGenerator
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.DisposalMethod.{Gifted, Sold}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.IndividualTriageAnswers.{CompleteIndividualTriageAnswers, IncompleteIndividualTriageAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.IndividualUserType.{Capacitor, PersonalRepresentative, Self}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.NumberOfProperties.{MoreThanOne, One}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{BooleanFormatter, LocalDateUtils, SessionData, TaxYear}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views.html.returns.{triage => triagePages}
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

@Singleton
class CanTheyUseOurServiceController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionStore,
  val errorHandler: ErrorHandler,
  returnsService: ReturnsService,
  uuidGenerator: UUIDGenerator,
  cc: MessagesControllerComponents,
  val config: Configuration,
  whoAreYouReportingForPage: triagePages.who_are_you_reporting_for,
  howManyPropertiesPage: triagePages.how_many_properties,
  disposalMethodPage: triagePages.how_did_you_dispose,
  wereYouAUKResidentPage: triagePages.were_you_a_uk_resident,
  didYouDisposeOfResidentialPropertyPage: triagePages.did_you_dispose_of_residential_property,
  disposalDatePage: triagePages.disposal_date,
  completionDatePage: triagePages.completion_date,
  checkYourAnswersPage: triagePages.check_your_answers
)(implicit viewConfig: ViewConfig, ec: ExecutionContext)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with Logging
    with SessionUpdates {

  import CanTheyUseOurServiceController._

  val earliestDisposalDateInclusive: LocalDate =
    config.underlying.get[LocalDate]("returns.earliest-disposal-date-inclusive").value

  val taxYears: List[TaxYear] =
    config.underlying.get[List[TaxYear]]("tax-years").value

  def whoIsIndividualRepresenting(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    displayIndividualTriagePage(
      _ => Some(()),
      homeRoutes.HomePageController.homepage()
    )(_ => whoAreYouReportingForForm)(
      _.fold(_.individualUserType, c => Some(c.individualUserType)), {
        case (_, form, isDraftReturn) =>
          whoAreYouReportingForPage(
            form,
            None,
            isDraftReturn
          )
      }
    )
  }

  def whoIsIndividualRepresentingSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async {
    implicit request =>
      handleIndividualTriagePageSubmit(
        _ => Some(()),
        homeRoutes.HomePageController.homepage()
      )(_ => whoAreYouReportingForForm)(
        {
          case (_, form, isDraftReturn) =>
            whoAreYouReportingForPage(
              form,
              None,
              isDraftReturn
            )
        },
        updateState = {
          case (userType, i) =>
            i.fold(_.copy(individualUserType = Some(userType)), _.copy(individualUserType = userType))
        },
        nextResult = {
          case (IndividualUserType.Self, newAnswers) =>
            newAnswers.fold(
              _ => Redirect(routes.CanTheyUseOurServiceController.howManyProperties()),
              _ => Redirect(routes.CanTheyUseOurServiceController.checkYourAnswers())
            )

          case (other, _) => Ok(s"$other not handled yet")
        }
      )
  }

  def howManyProperties(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    displayIndividualTriagePage(
      _.fold(_.individualUserType, c => Some(c.individualUserType)),
      routes.CanTheyUseOurServiceController.whoIsIndividualRepresenting()
    )(_ => numberOfPropertiesForm)(
      _.fold(_.numberOfProperties, c => Some(c.numberOfProperties)), {
        case (currentState, form, isDraftReturn) =>
          howManyPropertiesPage(
            form,
            backLink(currentState, routes.CanTheyUseOurServiceController.whoIsIndividualRepresenting()),
            isDraftReturn
          )
      }
    )
  }

  def howManyPropertiesSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    handleIndividualTriagePageSubmit(
      _.fold(_.individualUserType, c => Some(c.individualUserType)),
      routes.CanTheyUseOurServiceController.whoIsIndividualRepresenting()
    )(_ => numberOfPropertiesForm)(
      {
        case (currentState, form, isDraftReturn) =>
          howManyPropertiesPage(
            form,
            backLink(currentState, routes.CanTheyUseOurServiceController.whoIsIndividualRepresenting()),
            isDraftReturn
          )
      },
      updateState = {
        case (numberOfProperties, i) =>
          i.fold(_.copy(numberOfProperties = Some(numberOfProperties)), _.copy(numberOfProperties = numberOfProperties))
      },
      nextResult = {
        case (NumberOfProperties.One, updatedState) =>
          updatedState.fold(
            _ => Redirect(routes.CanTheyUseOurServiceController.howDidYouDisposeOfProperty()),
            _ => Redirect(routes.CanTheyUseOurServiceController.checkYourAnswers())
          )
        case (NumberOfProperties.MoreThanOne, _) =>
          Ok("multiple disposals not handled yet")
      }
    )
  }

  def howDidYouDisposeOfProperty(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    displayIndividualTriagePage(
      _.fold(_.numberOfProperties, c => Some(c.numberOfProperties)),
      routes.CanTheyUseOurServiceController.howManyProperties()
    )(_ => disposalMethodForm)(
      extractField = _.fold(_.disposalMethod, c => Some(c.disposalMethod)),
      page = {
        case (currentState, form, isDraftReturn) =>
          disposalMethodPage(
            form,
            backLink(currentState, routes.CanTheyUseOurServiceController.howManyProperties()),
            isDraftReturn
          )
      }
    )
  }

  def howDidYouDisposeOfPropertySubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async {
    implicit request =>
      handleIndividualTriagePageSubmit(
        _.fold(_.numberOfProperties, c => Some(c.numberOfProperties)),
        routes.CanTheyUseOurServiceController.howManyProperties()
      )(_ => disposalMethodForm)(
        page = {
          case (currentState, form, isDraftReturn) =>
            disposalMethodPage(
              form,
              backLink(currentState, routes.CanTheyUseOurServiceController.howManyProperties()),
              isDraftReturn
            )
        },
        updateState = {
          case (disposalMethod, i) =>
            i.fold(_.copy(disposalMethod = Some(disposalMethod)), _.copy(disposalMethod = disposalMethod))
        },
        nextResult = {
          case (_, updatedState) =>
            updatedState.fold(
              _ => Redirect(routes.CanTheyUseOurServiceController.wereYouAUKResident()),
              _ => Redirect(routes.CanTheyUseOurServiceController.checkYourAnswers())
            )
        }
      )
  }

  def wereYouAUKResident(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    displayIndividualTriagePage(
      _.fold(_.disposalMethod, c => Some(c.disposalMethod)),
      routes.CanTheyUseOurServiceController.howDidYouDisposeOfProperty()
    )(_ => wasAUkResidentForm)(
      extractField = _.fold(_.wasAUKResident, c => Some(c.wasAUKResident)),
      page = {
        case (currentState, form, isDraftReturn) =>
          wereYouAUKResidentPage(
            form,
            backLink(currentState, routes.CanTheyUseOurServiceController.howDidYouDisposeOfProperty()),
            isDraftReturn
          )
      }
    )
  }

  def wereYouAUKResidentSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    handleIndividualTriagePageSubmit(
      _.fold(_.disposalMethod, c => Some(c.disposalMethod)),
      routes.CanTheyUseOurServiceController.howDidYouDisposeOfProperty()
    )(_ => wasAUkResidentForm)(
      page = {
        case (currentState, form, isDraftReturn) =>
          wereYouAUKResidentPage(
            form,
            backLink(currentState, routes.CanTheyUseOurServiceController.howDidYouDisposeOfProperty()),
            isDraftReturn
          )
      },
      updateState = {
        case (wasAUKResident, i) =>
          i.fold(_.copy(wasAUKResident = Some(wasAUKResident)), _.copy(wasAUKResident = wasAUKResident))
      },
      nextResult = {
        case (wasAUKResident, updatedState) =>
          if (wasAUKResident) {
            updatedState.fold(
              _ => Redirect(routes.CanTheyUseOurServiceController.didYouDisposeOfAResidentialProperty()),
              _ => Redirect(routes.CanTheyUseOurServiceController.checkYourAnswers())
            )
          } else {
            Ok("non residents not handled yet")
          }
      }
    )
  }

  def didYouDisposeOfAResidentialProperty(): Action[AnyContent] = authenticatedActionWithSessionData.async {
    implicit request =>
      displayIndividualTriagePage(
        _.fold(_.wasAUKResident, c => Some(c.wasAUKResident)),
        routes.CanTheyUseOurServiceController.wereYouAUKResident()
      )(_ => wasResidentialPropertyForm)(
        extractField =
          _.fold(_.assetType.map(_ === AssetType.Residential), c => Some(c.assetType === AssetType.Residential)),
        page = {
          case (currentState, form, isDraftReturn) =>
            didYouDisposeOfResidentialPropertyPage(
              form,
              backLink(currentState, routes.CanTheyUseOurServiceController.wereYouAUKResident()),
              isDraftReturn
            )
        }
      )
  }

  def didYouDisposeOfAResidentialPropertySubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async {
    implicit request =>
      handleIndividualTriagePageSubmit(
        _.fold(_.wasAUKResident, c => Some(c.wasAUKResident)),
        routes.CanTheyUseOurServiceController.wereYouAUKResident()
      )(_ => wasResidentialPropertyForm)(
        page = {
          case (currentState, form, isDraftReturn) =>
            didYouDisposeOfResidentialPropertyPage(
              form,
              backLink(currentState, routes.CanTheyUseOurServiceController.wereYouAUKResident()),
              isDraftReturn
            )
        },
        updateState = {
          case (wasResidentialProperty, i) =>
            val assetType = if (wasResidentialProperty) AssetType.Residential else AssetType.NonResidential
            i.fold(
              _.copy(assetType = Some(assetType)),
              _.copy(assetType = assetType)
            )
        },
        nextResult = {
          case (wasResidentialProperty, updatedState) =>
            if (wasResidentialProperty) {
              updatedState.fold(
                _ => Redirect(routes.CanTheyUseOurServiceController.whenWasDisposalDate()),
                _ => Redirect(routes.CanTheyUseOurServiceController.checkYourAnswers())
              )
            } else {
              Ok("individuals can only report on residential properties")
            }
        }
      )
  }

  def whenWasDisposalDate(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    displayIndividualTriagePage(
      _.fold(_.assetType, c => Some(c.assetType)),
      routes.CanTheyUseOurServiceController.didYouDisposeOfAResidentialProperty()
    )(_ => disposalDateForm(LocalDateUtils.today(), taxYears))(
      extractField = _.fold(_.disposalDate, c => Some(c.disposalDate)),
      page = {
        case (currentState, form, isDraftReturn) =>
          disposalDatePage(
            form,
            backLink(currentState, routes.CanTheyUseOurServiceController.didYouDisposeOfAResidentialProperty()),
            isDraftReturn
          )
      }
    )
  }

  def whenWasDisposalDateSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    handleIndividualTriagePageSubmit(
      _.fold(_.assetType, c => Some(c.assetType)),
      routes.CanTheyUseOurServiceController.didYouDisposeOfAResidentialProperty()
    )(_ => disposalDateForm(LocalDateUtils.today(), taxYears))(
      page = {
        case (currentState, form, isDraftReturn) =>
          disposalDatePage(
            form,
            backLink(currentState, routes.CanTheyUseOurServiceController.didYouDisposeOfAResidentialProperty()),
            isDraftReturn
          )
      },
      updateState = {
        case (d, i) =>
          i.fold(
            _.copy(disposalDate = Some(d)),
            c =>
              IncompleteIndividualTriageAnswers(
                Some(c.individualUserType),
                Some(c.numberOfProperties),
                Some(c.disposalMethod),
                Some(c.wasAUKResident),
                Some(c.assetType),
                Some(d),
                None
              )
          )
      },
      nextResult = { (d, updatedState) =>
        if (d.value.isBefore(earliestDisposalDateInclusive)) {
          Ok(s"disposal date was strictly before $earliestDisposalDateInclusive")
        } else {
          updatedState.fold(
            _ => Redirect(routes.CanTheyUseOurServiceController.whenWasCompletionDate()),
            _ => Redirect(routes.CanTheyUseOurServiceController.checkYourAnswers())
          )

        }
      }
    )
  }

  def whenWasCompletionDate(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    displayIndividualTriagePage(
      _.fold(_.disposalDate, c => Some(c.disposalDate)),
      routes.CanTheyUseOurServiceController.whenWasDisposalDate()
    )(disposalDate => completionDateForm(disposalDate, LocalDateUtils.today()))(
      extractField = _.fold(_.completionDate, c => Some(c.completionDate)),
      page = {
        case (currentState, form, isDraftReturn) =>
          completionDatePage(
            form,
            backLink(currentState, routes.CanTheyUseOurServiceController.whenWasDisposalDate()),
            isDraftReturn
          )
      }
    )
  }

  def whenWasCompletionDateSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    handleIndividualTriagePageSubmit(
      _.fold(_.disposalDate, c => Some(c.disposalDate)),
      routes.CanTheyUseOurServiceController.whenWasDisposalDate()
    )(disposalDate => completionDateForm(disposalDate, LocalDateUtils.today()))(
      page = {
        case (currentState, form, isDraftReturn) =>
          completionDatePage(
            form,
            backLink(currentState, routes.CanTheyUseOurServiceController.whenWasDisposalDate()),
            isDraftReturn
          )
      },
      updateState = { case (d, i) => i.fold(_.copy(completionDate = Some(d)), _.copy(completionDate = d)) },
      nextResult = {
        case (_, _) =>
          Redirect(routes.CanTheyUseOurServiceController.checkYourAnswers())
      }
    )
  }

  def checkYourAnswers(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withIndividualTriageAnswers(request) {
      case (_, state) =>
        lazy val displayReturnToSummaryLink = state.fold(_ => false, _ => true)

        individualTriageAnswersFomState(state) match {
          case c: CompleteIndividualTriageAnswers =>
            Ok(checkYourAnswersPage(c, displayReturnToSummaryLink))

          case IncompleteIndividualTriageAnswers(None, _, _, _, _, _, _) =>
            Redirect(routes.CanTheyUseOurServiceController.whoIsIndividualRepresenting())

          case IncompleteIndividualTriageAnswers(_, None, _, _, _, _, _) =>
            Redirect(routes.CanTheyUseOurServiceController.howManyProperties())

          case IncompleteIndividualTriageAnswers(_, _, None, _, _, _, _) =>
            Redirect(routes.CanTheyUseOurServiceController.howDidYouDisposeOfProperty())

          case IncompleteIndividualTriageAnswers(_, _, _, None, _, _, _) =>
            Redirect(routes.CanTheyUseOurServiceController.wereYouAUKResident())

          case IncompleteIndividualTriageAnswers(_, _, _, _, None, _, _) =>
            Redirect(routes.CanTheyUseOurServiceController.didYouDisposeOfAResidentialProperty())

          case IncompleteIndividualTriageAnswers(_, _, _, _, _, None, _) =>
            Redirect(routes.CanTheyUseOurServiceController.whenWasDisposalDate())
          case IncompleteIndividualTriageAnswers(_, _, _, _, _, _, None) =>
            Redirect(routes.CanTheyUseOurServiceController.whenWasCompletionDate())

          case IncompleteIndividualTriageAnswers(Some(t), Some(n), Some(m), Some(u), Some(r), Some(d), Some(c)) =>
            val completeIndividualTriageAnswers = CompleteIndividualTriageAnswers(t, n, m, u, r, d, c)

            val updatedJourney = state.fold(
              _.copy(newReturnTriageAnswers = completeIndividualTriageAnswers),
              r => r.copy(draftReturn = r.draftReturn.copy(triageAnswers = completeIndividualTriageAnswers))
            )

            updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney))).map {
              case Left(e) =>
                logger.warn("Could not update session", e)
                errorHandler.errorResult()

              case Right(_) =>
                Ok(checkYourAnswersPage(completeIndividualTriageAnswers, displayReturnToSummaryLink))
            }
        }
    }
  }

  def checkYourAnswersSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withIndividualTriageAnswers(request) {
      case (_, state) =>
        individualTriageAnswersFomState(state) match {
          case _: IncompleteIndividualTriageAnswers =>
            Redirect(routes.CanTheyUseOurServiceController.checkYourAnswers())

          case complete: CompleteIndividualTriageAnswers =>
            lazy val continueToTaskList = Redirect(returnsRoutes.TaskListController.taskList())

            def toFillingOurNewReturn(startingNewDraftReturn: StartingNewDraftReturn): Future[Result] = {
              val newDraftReturn =
                DraftReturn(
                  uuidGenerator.nextId(),
                  startingNewDraftReturn.subscribedDetails.cgtReference,
                  complete,
                  None,
                  None,
                  None,
                  None,
                  None,
                  None
                )
              val result = for {
                _ <- returnsService.storeDraftReturn(newDraftReturn)
                _ <- EitherT(
                      updateSession(sessionStore, request)(
                        _.copy(journeyStatus = Some(
                          FillingOutReturn(
                            startingNewDraftReturn.subscribedDetails,
                            startingNewDraftReturn.ggCredId,
                            startingNewDraftReturn.agentReferenceNumber,
                            newDraftReturn
                          )
                        )
                        )
                      )
                    )
              } yield ()

              result.fold(
                e => {
                  logger.warn("Could not store draft return", e)
                  errorHandler.errorResult()
                },
                _ => continueToTaskList
              )
            }

            state.fold[Future[Result]](
              toFillingOurNewReturn,
              _ => continueToTaskList
            )
        }
    }
  }

  private def handleIndividualTriagePageSubmit[R, A, Page: Writeable](
    requiredField: IndividualTriageAnswers => Option[R],
    redirectToIfNotValidJourney: => Call
  )(
    form: R => Form[A]
  )(
    page: (IndividualTriageAnswers, Form[A], Boolean) => Page,
    updateState: (A, IndividualTriageAnswers) => IndividualTriageAnswers,
    nextResult: (A, IndividualTriageAnswers) => Result
  )(
    implicit request: RequestWithSessionData[_]
  ): Future[Result] =
    withIndividualTriageAnswers(request) {
      case (_, state) =>
        val individualTriageAnswers = individualTriageAnswersFomState(state)
        requiredField(individualTriageAnswers) match {
          case None => Redirect(redirectToIfNotValidJourney)
          case Some(r) =>
            form(r)
              .bindFromRequest()
              .fold(
                formWithErrors => BadRequest(page(individualTriageAnswers, formWithErrors, state.isRight)), { value =>
                  val updatedAnswers = updateState(value, individualTriageAnswers)
                  lazy val oldJourneyStatusWithNewJourneyStatus =
                    state.bimap(
                      s => s -> s.copy(newReturnTriageAnswers = updatedAnswers),
                      r => r -> r.copy(draftReturn            = r.draftReturn.copy(triageAnswers = updatedAnswers))
                    )

                  val result = for {
                    _ <- oldJourneyStatusWithNewJourneyStatus.fold(
                          _ => EitherT.pure(()), {
                            case (oldJourneyStatus, updatedJourneyStatus) =>
                              if (updatedJourneyStatus.draftReturn === oldJourneyStatus.draftReturn) EitherT.pure(())
                              else returnsService.storeDraftReturn(updatedJourneyStatus.draftReturn)
                          }
                        )
                    _ <- EitherT(
                          updateSession(sessionStore, request)(
                            _.copy(journeyStatus = Some(oldJourneyStatusWithNewJourneyStatus.bimap(_._2, _._2).merge))
                          )
                        )
                  } yield ()

                  result.fold(
                    { e =>
                      logger.warn("Could not update session", e)
                      errorHandler.errorResult()
                    },
                    _ => nextResult(value, updatedAnswers)
                  )
                }
              )
        }
    }

  private def displayIndividualTriagePage[R, A, Page: Writeable](
    requiredField: IndividualTriageAnswers => Option[R],
    redirectToIfNotValidJourney: => Call
  )(
    form: R => Form[A]
  )(extractField: IndividualTriageAnswers => Option[A], page: (IndividualTriageAnswers, Form[A], Boolean) => Page)(
    implicit request: RequestWithSessionData[_]
  ): Future[Result] =
    withIndividualTriageAnswers(request) {
      case (_, state) =>
        val individualTriageAnswers = individualTriageAnswersFomState(state)
        requiredField(individualTriageAnswers) match {
          case None => Redirect(redirectToIfNotValidJourney)
          case Some(r) =>
            val f = extractField(individualTriageAnswers)
              .fold(form(r))(form(r).fill)

            Ok(page(individualTriageAnswers, f, state.isRight))
        }
    }

  private def withIndividualTriageAnswers(request: RequestWithSessionData[_])(
    f: (SessionData, Either[StartingNewDraftReturn, FillingOutReturn]) => Future[Result]
  ): Future[Result] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some((session, s: StartingNewDraftReturn)) =>
        f(session, Left(s))

      case Some((session, r: FillingOutReturn)) =>
        f(session, Right(r))

      case _ =>
        Redirect(uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.routes.StartController.start())
    }

  private def backLink(currentState: IndividualTriageAnswers, ifIncomplete: Call): Call =
    currentState.fold(_ => ifIncomplete, _ => routes.CanTheyUseOurServiceController.checkYourAnswers())

  private def individualTriageAnswersFomState(
    state: Either[StartingNewDraftReturn, FillingOutReturn]
  ): IndividualTriageAnswers =
    state.bimap(_.newReturnTriageAnswers, _.draftReturn.triageAnswers).merge

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

  def disposalDateForm(maximumDateInclusive: LocalDate, taxYears: List[TaxYear]): Form[DisposalDate] = {
    val dateFormatter =
      LocalDateUtils.dateFormatter(
        Some(maximumDateInclusive),
        None,
        "disposalDate-day",
        "disposalDate-month",
        "disposalDate-year",
        "disposalDate"
      )

    val disposalDateFormatter: Formatter[DisposalDate] = new Formatter[DisposalDate] {
      override def bind(key: String, data: Map[String, String]): Either[Seq[FormError], DisposalDate] =
        dateFormatter.bind(key, data).flatMap { d =>
          Either
            .fromOption(
              taxYears.find(t => d < t.endDateExclusive && d >= (t.startDateInclusive)),
              Seq(FormError("disposalDate", "error.tooFarInPast"))
            )
            .map(DisposalDate(d, _))
        }

      override def unbind(key: String, value: DisposalDate): Map[String, String] =
        dateFormatter.unbind(key, value.value)

    }

    Form(
      mapping(
        "" -> of(disposalDateFormatter)
      )(identity)(Some(_))
    )
  }

  def completionDateForm(disposalDate: DisposalDate, maximumDateInclusive: LocalDate): Form[CompletionDate] = Form(
    mapping(
      "" -> of(
        LocalDateUtils.dateFormatter(
          Some(maximumDateInclusive),
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
