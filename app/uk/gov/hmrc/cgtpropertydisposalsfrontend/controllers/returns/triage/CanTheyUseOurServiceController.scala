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
import cats.data.EitherT
import cats.instances.future._
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
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.accounts.homepage.{routes => homeRoutes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.{routes => returnsRoutes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.Subscribed
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.LocalDateUtils.configs
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.UUIDGenerator
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.DisposalMethod.{Gifted, Sold}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.IndividualTriageAnswers.{CompleteIndividualTriageAnswers, IncompleteIndividualTriageAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.IndividualUserType.{Capacitor, PersonalRepresentative, Self}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.NumberOfProperties.{MoreThanOne, One}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{BooleanFormatter, LocalDateUtils, SessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService
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

  val clock: Clock = Clock.systemUTC()

  def today(): LocalDate = LocalDate.now(clock)

  def whoIsIndividualRepresenting(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withSubscribedUser(request) {
      case (_, subscribed) =>
        val state: Either[Option[IndividualTriageAnswers], DraftReturn] =
          (subscribed.newReturnIndividualTriageAnswers, subscribed.currentDraftReturn) match {
            case (individualTriageAnswers, None) => Left(individualTriageAnswers)
            case (_, Some(draftReturn))          => Right(draftReturn)
          }

        val individualTriageAnswers =
          state.map(draftReturn => Some(draftReturn.triageAnswers)).merge

        val form = individualTriageAnswers
          .flatMap(_.fold(_.individualUserType, c => Some(c.individualUserType)))
          .fold(whoAreYouReportingForForm)(whoAreYouReportingForForm.fill)

        val displayReturnToSummaryLink = subscribed.currentDraftReturn.isDefined

        val backLink: Option[Call] =
          state.fold(
            answers =>
              Some(
                answers
                  .map(
                    _.fold(
                      _ => homeRoutes.HomePageController.homepage(),
                      _ => routes.CanTheyUseOurServiceController.checkYourAnswers()
                    )
                  )
                  .getOrElse(homeRoutes.HomePageController.homepage())
              ),
            _ => None
          )

        Ok(whoAreYouReportingForPage(form, backLink, displayReturnToSummaryLink))
    }
  }

  def whoIsIndividualRepresentingSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async {
    implicit request =>
      withSubscribedUser(request) {
        case (_, subscribed) =>
          val state: Either[Option[IndividualTriageAnswers], DraftReturn] =
            (subscribed.newReturnIndividualTriageAnswers, subscribed.currentDraftReturn) match {
              case (individualTriageAnswers, None) => Left(individualTriageAnswers)
              case (_, Some(draftReturn))          => Right(draftReturn)
            }

          val individualTriageAnswers =
            state.map(draftReturn => Some(draftReturn.triageAnswers)).merge

          val displayReturnToSummaryLink = subscribed.currentDraftReturn.isDefined

          lazy val backLink: Option[Call] =
            state.fold(
              answers =>
                Some(
                  answers
                    .map(
                      _.fold(
                        _ => homeRoutes.HomePageController.homepage(),
                        _ => routes.CanTheyUseOurServiceController.checkYourAnswers()
                      )
                    )
                    .getOrElse(homeRoutes.HomePageController.homepage())
                ),
              _ => None
            )

          whoAreYouReportingForForm
            .bindFromRequest()
            .fold(
              formWithErrors =>
                BadRequest(whoAreYouReportingForPage(formWithErrors, backLink, displayReturnToSummaryLink)), {
                userType =>
                  val newAnswers =
                    individualTriageAnswers
                      .map(_.fold(_.copy(individualUserType = Some(userType)), _.copy(individualUserType = userType)))
                      .getOrElse(IncompleteIndividualTriageAnswers.empty.copy(individualUserType = Some(userType)))

                  val newSubscribed = state.fold(
                    _ => subscribed.copy(newReturnIndividualTriageAnswers = Some(newAnswers)),
                    draftReturn =>
                      subscribed.copy(currentDraftReturn = Some(draftReturn.copy(triageAnswers = newAnswers)))
                  )

                  val result = for {
                    _ <- state.fold(
                          _ => EitherT.pure(()), { draftReturn =>
                            val updatedDraftReturn = draftReturn.copy(triageAnswers = newAnswers)
                            if (updatedDraftReturn === draftReturn) EitherT.pure(())
                            else returnsService.storeDraftReturn(updatedDraftReturn)
                          }
                        )
                    _ <- EitherT(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(newSubscribed))))
                  } yield ()

                  result.fold(
                    { e =>
                      logger.warn("Could not update session", e)
                      errorHandler.errorResult()
                    },
                    _ =>
                      userType match {
                        case IndividualUserType.Self =>
                          newAnswers.fold(
                            _ => Redirect(routes.CanTheyUseOurServiceController.howManyProperties()),
                            _ => Redirect(routes.CanTheyUseOurServiceController.checkYourAnswers())
                          )

                        case other => Ok(s"$other not handled yet")
                      }
                  )
              }
            )
      }
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
        case (NumberOfProperties.MoreThanOne, updatedState) =>
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
            backLink(currentState, routes.CanTheyUseOurServiceController.whoIsIndividualRepresenting()),
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
              backLink(currentState, routes.CanTheyUseOurServiceController.whoIsIndividualRepresenting()),
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
    )(_ => disposalDateForm(today()))(
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
    )(_ => disposalDateForm(today()))(
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
    )(disposalDate => completionDateForm(disposalDate, today()))(
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
    )(disposalDate => completionDateForm(disposalDate, today()))(
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
      case (_, subscribed, state) =>
        state.map(_.triageAnswers).merge match {
          case c: CompleteIndividualTriageAnswers =>
            Ok(checkYourAnswersPage(c))

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

            val updatedSubscribed = state.fold(
              _ =>
                subscribed.copy(
                  newReturnIndividualTriageAnswers = Some(completeIndividualTriageAnswers)
                ),
              draftReturn =>
                subscribed.copy(
                  currentDraftReturn = Some(draftReturn.copy(triageAnswers = completeIndividualTriageAnswers))
                )
            )

            updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedSubscribed))).map {
              case Left(e) =>
                logger.warn("Could not update session", e)
                errorHandler.errorResult()

              case Right(_) =>
                Ok(checkYourAnswersPage(completeIndividualTriageAnswers))
            }
        }
    }
  }

  def checkYourAnswersSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withIndividualTriageAnswers(request) {
      case (_, subscribed, state) =>
        state.map(_.triageAnswers).merge match {
          case _: IncompleteIndividualTriageAnswers =>
            Redirect(routes.CanTheyUseOurServiceController.checkYourAnswers())

          case complete: CompleteIndividualTriageAnswers =>
            lazy val continueToTaskList = Redirect(returnsRoutes.TaskListController.taskList())

            lazy val handleNewDraftReturn = {
              val newDraftReturn =
                DraftReturn(uuidGenerator.nextId(), subscribed.subscribedDetails.cgtReference, complete)
              val result = for {
                _ <- returnsService.storeDraftReturn(newDraftReturn)
                _ <- EitherT(
                      updateSession(sessionStore, request)(
                        _.copy(journeyStatus = Some(
                          subscribed.copy(
                            newReturnIndividualTriageAnswers = None,
                            currentDraftReturn               = Some(newDraftReturn)
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
              _ => handleNewDraftReturn,
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
      case (_, subscribed, state) =>
        val individualTriageAnswers = state.map(_.triageAnswers).merge
        requiredField(individualTriageAnswers) match {
          case None => Redirect(redirectToIfNotValidJourney)
          case Some(r) =>
            form(r)
              .bindFromRequest()
              .fold(
                formWithErrors => BadRequest(page(individualTriageAnswers, formWithErrors, state.isRight)), { value =>
                  val updatedState = updateState(value, individualTriageAnswers)
                  lazy val newSubscribed =
                    state.fold(
                      _ => subscribed.copy(newReturnIndividualTriageAnswers = Some(updatedState)),
                      draftReturn =>
                        subscribed.copy(currentDraftReturn = Some(draftReturn.copy(triageAnswers = updatedState)))
                    )

                  val result = for {
                    _ <- state.fold(
                          _ => EitherT.pure(()), { draftReturn =>
                            val updatedDraftReturn = draftReturn.copy(triageAnswers = updatedState)
                            if (updatedDraftReturn === draftReturn) EitherT.pure(())
                            else returnsService.storeDraftReturn(updatedDraftReturn)
                          }
                        )
                    _ <- EitherT(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(newSubscribed))))
                  } yield ()

                  result.fold(
                    { e =>
                      logger.warn("Could not update session", e)
                      errorHandler.errorResult()
                    },
                    _ =>
                      updatedState.fold(
                        _ => nextResult(value, updatedState),
                        _ => Redirect(routes.CanTheyUseOurServiceController.checkYourAnswers())
                      )
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
      case (_, _, state) =>
        val individualTriageAnswers = state.map(_.triageAnswers).merge
        requiredField(individualTriageAnswers) match {
          case None => Redirect(redirectToIfNotValidJourney)
          case Some(r) =>
            val f = extractField(individualTriageAnswers)
              .fold(form(r))(form(r).fill)

            Ok(page(individualTriageAnswers, f, state.isRight))
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
    f: (SessionData, Subscribed, Either[IndividualTriageAnswers, DraftReturn]) => Future[Result]
  ): Future[Result] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some((s: SessionData, r @ Subscribed(_, _, _, Some(i), _, _))) =>
        f(s, r, Left(i))

      case Some((s: SessionData, r @ Subscribed(_, _, _, None, Some(d), _))) =>
        f(s, r, Right(d))

      case _ =>
        Redirect(uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.routes.StartController.start())
    }

  private def backLink(currentState: IndividualTriageAnswers, ifIncomplete: Call): Call =
    currentState.fold(_ => ifIncomplete, _ => routes.CanTheyUseOurServiceController.checkYourAnswers())

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
