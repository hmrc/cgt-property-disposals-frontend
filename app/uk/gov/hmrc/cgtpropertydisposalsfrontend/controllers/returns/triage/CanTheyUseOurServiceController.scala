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

import cats.data.EitherT
import cats.instances.future._
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
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Country
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.UUIDGenerator
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.AssetType.{IndirectDisposal, MixedUse, NonResidential, Residential}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.DisposalMethod.{Gifted, Sold}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.TriageAnswers.{CompleteTriageAnswers, IncompleteTriageAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.IndividualUserType.{Capacitor, PersonalRepresentative, Self}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.NumberOfProperties.{MoreThanOne, One}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{BooleanFormatter, FormUtils, LocalDateUtils, SessionData, TaxYear}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views.html.returns.{triage => triagePages}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

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
  countryOfResidencePage: triagePages.country_of_residence,
  assetTypeForNonUkResidentsPage: triagePages.asset_type_for_non_uk_residents,
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
    displayTriagePage(
      _ => Some(()),
      _ => homeRoutes.HomePageController.homepage()
    )(_ => whoAreYouReportingForForm)(
      _.fold(_.individualUserType, c => Some(c.individualUserType)), {
        case (_, form, isDraftReturn, _) =>
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
      handleTriagePageSubmit(
        _ => Some(()),
        _ => homeRoutes.HomePageController.homepage()
      )(_ => whoAreYouReportingForForm)(
        {
          case (_, form, isDraftReturn, _) =>
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
          case (IndividualUserType.Self, _) =>
            Redirect(routes.CanTheyUseOurServiceController.checkYourAnswers())

          case (other, _) => Ok(s"$other not handled yet")
        }
      )
  }

  def howManyProperties(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    displayTriagePage(
      _.fold(_.individualUserType, c => Some(c.individualUserType)),
      _ => routes.CanTheyUseOurServiceController.whoIsIndividualRepresenting()
    )(_ => numberOfPropertiesForm)(
      _.fold(_.numberOfProperties, c => Some(c.numberOfProperties)), {
        case (currentState, form, isDraftReturn, _) =>
          howManyPropertiesPage(
            form,
            backLink(currentState, routes.CanTheyUseOurServiceController.whoIsIndividualRepresenting()),
            isDraftReturn
          )
      }
    )
  }

  def howManyPropertiesSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    handleTriagePageSubmit(
      _.fold(_.individualUserType, c => Some(c.individualUserType)),
      _ => routes.CanTheyUseOurServiceController.whoIsIndividualRepresenting()
    )(_ => numberOfPropertiesForm)(
      {
        case (currentState, form, isDraftReturn, _) =>
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
        case (NumberOfProperties.One, _) =>
          Redirect(routes.CanTheyUseOurServiceController.checkYourAnswers())

        case (NumberOfProperties.MoreThanOne, _) =>
          Ok("multiple disposals not handled yet")
      }
    )
  }

  def howDidYouDisposeOfProperty(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    displayTriagePage(
      _.fold(_.numberOfProperties, c => Some(c.numberOfProperties)),
      _ => routes.CanTheyUseOurServiceController.howManyProperties()
    )(_ => disposalMethodForm)(
      extractField = _.fold(_.disposalMethod, c => Some(c.disposalMethod)),
      page = {
        case (currentState, form, isDraftReturn, _) =>
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
      handleTriagePageSubmit(
        _.fold(_.numberOfProperties, c => Some(c.numberOfProperties)),
        _ => routes.CanTheyUseOurServiceController.howManyProperties()
      )(_ => disposalMethodForm)(
        page = {
          case (currentState, form, isDraftReturn, _) =>
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
          case (_, _) =>
            Redirect(routes.CanTheyUseOurServiceController.checkYourAnswers())
        }
      )
  }

  def wereYouAUKResident(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    displayTriagePage(
      _.fold(_.disposalMethod, c => Some(c.disposalMethod)),
      _ => routes.CanTheyUseOurServiceController.howDidYouDisposeOfProperty()
    )(_ => wasAUkResidentForm)(
      extractField = _.fold(_.wasAUKResident, c => Some(c.countryOfResidence.isUk())),
      page = {
        case (currentState, form, isDraftReturn, _) =>
          wereYouAUKResidentPage(
            form,
            backLink(currentState, routes.CanTheyUseOurServiceController.howDidYouDisposeOfProperty()),
            isDraftReturn
          )
      }
    )
  }

  def wereYouAUKResidentSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    handleTriagePageSubmit(
      _.fold(_.disposalMethod, c => Some(c.disposalMethod)),
      _ => routes.CanTheyUseOurServiceController.howDidYouDisposeOfProperty()
    )(_ => wasAUkResidentForm)(
      page = {
        case (currentState, form, isDraftReturn, _) =>
          wereYouAUKResidentPage(
            form,
            backLink(currentState, routes.CanTheyUseOurServiceController.howDidYouDisposeOfProperty()),
            isDraftReturn
          )
      },
      updateState = {
        case (wasAUKResident, i) =>
          if (i.fold(_.wasAUKResident, c => Some(c.countryOfResidence.isUk())).contains(wasAUKResident)) {
            i
          } else {
            i.fold(
              _.copy(wasAUKResident = Some(wasAUKResident), countryOfResidence = None, assetType = None),
              complete =>
                IncompleteTriageAnswers(
                  Some(complete.individualUserType),
                  Some(complete.numberOfProperties),
                  Some(complete.disposalMethod),
                  Some(wasAUKResident),
                  None,
                  None,
                  Some(complete.disposalDate),
                  Some(complete.completionDate)
                )
            )
          }
      },
      nextResult = {
        case (_, _) =>
          Redirect(routes.CanTheyUseOurServiceController.checkYourAnswers())
      }
    )
  }

  def didYouDisposeOfAResidentialProperty(): Action[AnyContent] = authenticatedActionWithSessionData.async {
    implicit request =>
      displayTriagePage(
        _.fold(_.wasAUKResident, c => Some(c.countryOfResidence.isUk())),
        _ => routes.CanTheyUseOurServiceController.wereYouAUKResident()
      )(_ => wasResidentialPropertyForm)(
        extractField =
          _.fold(_.assetType.map(_ === AssetType.Residential), c => Some(c.assetType === AssetType.Residential)),
        page = {
          case (currentState, form, isDraftReturn, _) =>
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
      handleTriagePageSubmit(
        _.fold(_.wasAUKResident, c => Some(c.countryOfResidence.isUk())),
        _ => routes.CanTheyUseOurServiceController.wereYouAUKResident()
      )(_ => wasResidentialPropertyForm)(
        page = {
          case (currentState, form, isDraftReturn, _) =>
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
          case (wasResidentialProperty, _) =>
            if (wasResidentialProperty) {
              Redirect(routes.CanTheyUseOurServiceController.checkYourAnswers())
            } else {
              Ok("individuals can only report on residential properties")
            }
        }
      )
  }

  private def disposalDateBackLink(triageAnswers: TriageAnswers): Call =
    triageAnswers.fold(
      { incomplete =>
        if (incomplete.wasAUKResident.exists(identity))
          routes.CanTheyUseOurServiceController.didYouDisposeOfAResidentialProperty()
        else
          routes.CanTheyUseOurServiceController.assetTypeForNonUkResidents()
      },
      _ => routes.CanTheyUseOurServiceController.checkYourAnswers()
    )

  def whenWasDisposalDate(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    displayTriagePage(
      _.fold(_.assetType, c => Some(c.assetType)),
      answers => disposalDateBackLink(answers)
    )(_ => disposalDateForm(LocalDateUtils.today(), taxYears))(
      extractField = _.fold(_.disposalDate, c => Some(c.disposalDate)),
      page = {
        case (currentState, form, isDraftReturn, assetType) =>
          disposalDatePage(
            form,
            disposalDateBackLink(currentState),
            isDraftReturn,
            assetType
          )
      }
    )
  }

  def whenWasDisposalDateSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    handleTriagePageSubmit(
      _.fold(_.assetType, c => Some(c.assetType)),
      answers => disposalDateBackLink(answers)
    )(_ => disposalDateForm(LocalDateUtils.today(), taxYears))(
      page = {
        case (currentState, form, isDraftReturn, assetType) =>
          disposalDatePage(
            form,
            disposalDateBackLink(currentState),
            isDraftReturn,
            assetType
          )
      },
      updateState = {
        case (d, i) =>
          i.fold(
            _.copy(disposalDate = Some(d)),
            c =>
              IncompleteTriageAnswers(
                Some(c.individualUserType),
                Some(c.numberOfProperties),
                Some(c.disposalMethod),
                Some(c.countryOfResidence.isUk()),
                if (c.countryOfResidence.isUk()) None else Some(c.countryOfResidence),
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
          Redirect(routes.CanTheyUseOurServiceController.checkYourAnswers())
        }
      }
    )
  }

  def whenWasCompletionDate(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    displayTriagePage(
      _.fold(_.disposalDate, c => Some(c.disposalDate)),
      _ => routes.CanTheyUseOurServiceController.whenWasDisposalDate()
    )(disposalDate => completionDateForm(disposalDate, LocalDateUtils.today()))(
      extractField = _.fold(_.completionDate, c => Some(c.completionDate)),
      page = {
        case (currentState, form, isDraftReturn, _) =>
          completionDatePage(
            form,
            backLink(currentState, routes.CanTheyUseOurServiceController.whenWasDisposalDate()),
            isDraftReturn
          )
      }
    )
  }

  def whenWasCompletionDateSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    handleTriagePageSubmit(
      _.fold(_.disposalDate, c => Some(c.disposalDate)),
      _ => routes.CanTheyUseOurServiceController.whenWasDisposalDate()
    )(disposalDate => completionDateForm(disposalDate, LocalDateUtils.today()))(
      page = {
        case (currentState, form, isDraftReturn, _) =>
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

  def countryOfResidence(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    displayTriagePage(
      _.fold(_.wasAUKResident.filterNot(identity), c => Some(c.countryOfResidence.isUk()).filterNot(identity)),
      _ => routes.CanTheyUseOurServiceController.wereYouAUKResident()
    )(_ => countryOfResidenceForm)(
      extractField = _.fold(_.countryOfResidence, c => Some(c.countryOfResidence)),
      page = {
        case (currentState, form, isDraftReturn, _) =>
          countryOfResidencePage(
            form,
            backLink(currentState, routes.CanTheyUseOurServiceController.wereYouAUKResident()),
            isDraftReturn
          )
      }
    )
  }

  def countryOfResidenceSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    handleTriagePageSubmit(
      _.fold(_.wasAUKResident.filterNot(identity), c => Some(c.countryOfResidence.isUk()).filterNot(identity)),
      _ => routes.CanTheyUseOurServiceController.wereYouAUKResident()
    )(_ => countryOfResidenceForm)(
      page = {
        case (currentState, form, isDraftReturn, _) =>
          countryOfResidencePage(
            form,
            backLink(currentState, routes.CanTheyUseOurServiceController.wereYouAUKResident()),
            isDraftReturn
          )
      },
      updateState = { case (c, i) => i.fold(_.copy(countryOfResidence = Some(c)), _.copy(countryOfResidence = c)) },
      nextResult = {
        case (_, _) =>
          Redirect(routes.CanTheyUseOurServiceController.checkYourAnswers())
      }
    )
  }

  def assetTypeForNonUkResidents(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    displayTriagePage(
      _.fold(_.countryOfResidence, c => Some(c.countryOfResidence).filterNot(_.isUk())),
      _ => routes.CanTheyUseOurServiceController.countryOfResidence()
    )(_ => assetTypeForNonUkResidentsForm)(
      _.fold(_.assetType, c => Some(c.assetType)), {
        case (currentState, form, isDraftReturn, _) =>
          assetTypeForNonUkResidentsPage(
            form,
            backLink(currentState, routes.CanTheyUseOurServiceController.countryOfResidence()),
            isDraftReturn
          )
      }
    )
  }

  def assetTypeForNonUkResidentsSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async {
    implicit request =>
      handleTriagePageSubmit(
        _.fold(_.countryOfResidence, c => Some(c.countryOfResidence).filterNot(_.isUk())),
        _ => routes.CanTheyUseOurServiceController.countryOfResidence()
      )(_ => assetTypeForNonUkResidentsForm)(
        {
          case (currentState, form, isDraftReturn, _) =>
            assetTypeForNonUkResidentsPage(
              form,
              backLink(currentState, routes.CanTheyUseOurServiceController.countryOfResidence()),
              isDraftReturn
            )
        },
        updateState = {
          case (assetType, answers) =>
            if (answers.fold(_.assetType, c => Some(c.assetType)).contains(assetType)) {
              answers
            } else {
              answers.fold(
                i => i.copy(assetType = Some(assetType), disposalDate = None, completionDate = None),
                c =>
                  IncompleteTriageAnswers(
                    Some(c.individualUserType),
                    Some(c.numberOfProperties),
                    Some(c.disposalMethod),
                    Some(false),
                    Some(c.countryOfResidence),
                    Some(assetType),
                    None,
                    None
                  )
              )
            }
        },
        nextResult = {
          case (_, _) =>
            Redirect(routes.CanTheyUseOurServiceController.checkYourAnswers())
        }
      )
  }

  def checkYourAnswers(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withTriageAnswers(request) {
      case (_, state) =>
        lazy val displayReturnToSummaryLink = state.fold(_ => false, _ => true)

        triageAnswersFomState(state) match {
          case c: CompleteTriageAnswers =>
            Ok(checkYourAnswersPage(c, displayReturnToSummaryLink))

          case IncompleteTriageAnswers(None, _, _, _, _, _, _, _) =>
            Redirect(routes.CanTheyUseOurServiceController.whoIsIndividualRepresenting())

          case IncompleteTriageAnswers(_, None, _, _, _, _, _, _) =>
            Redirect(routes.CanTheyUseOurServiceController.howManyProperties())

          case IncompleteTriageAnswers(_, _, None, _, _, _, _, _) =>
            Redirect(routes.CanTheyUseOurServiceController.howDidYouDisposeOfProperty())

          case IncompleteTriageAnswers(_, _, _, None, _, _, _, _) =>
            Redirect(routes.CanTheyUseOurServiceController.wereYouAUKResident())

          case IncompleteTriageAnswers(_, _, _, Some(false), None, _, _, _) =>
            Redirect(routes.CanTheyUseOurServiceController.countryOfResidence())

          case IncompleteTriageAnswers(_, _, _, Some(false), Some(_), None, _, _) =>
            Redirect(routes.CanTheyUseOurServiceController.assetTypeForNonUkResidents())

          case IncompleteTriageAnswers(_, _, _, Some(true), _, None, _, _) =>
            Redirect(routes.CanTheyUseOurServiceController.didYouDisposeOfAResidentialProperty())

          case IncompleteTriageAnswers(_, _, _, _, _, Some(AssetType.IndirectDisposal), _, _) =>
            Ok("Indirect disposals not handled yet")

          case IncompleteTriageAnswers(_, _, _, _, _, _, None, _) =>
            Redirect(routes.CanTheyUseOurServiceController.whenWasDisposalDate())

          case IncompleteTriageAnswers(_, _, _, _, _, _, _, None) =>
            Redirect(routes.CanTheyUseOurServiceController.whenWasCompletionDate())

          case IncompleteTriageAnswers(Some(t), Some(n), Some(m), Some(true), _, Some(r), Some(d), Some(c)) =>
            updateAnswersAndShowCheckYourAnswersPage(
              state,
              CompleteTriageAnswers(t, n, m, Country.uk, r, d, c),
              displayReturnToSummaryLink
            )

          case IncompleteTriageAnswers(
              Some(t),
              Some(n),
              Some(m),
              Some(false),
              Some(country),
              Some(r),
              Some(d),
              Some(c)
              ) =>
            updateAnswersAndShowCheckYourAnswersPage(
              state,
              CompleteTriageAnswers(t, n, m, country, r, d, c),
              displayReturnToSummaryLink
            )
        }
    }
  }

  private def updateAnswersAndShowCheckYourAnswersPage(
    state: Either[StartingNewDraftReturn, FillingOutReturn],
    newCompleteTriageAnswers: CompleteTriageAnswers,
    displayReturnToSummaryLink: Boolean
  )(implicit request: RequestWithSessionData[_], hc: HeaderCarrier): Future[Result] = {
    val updatedJourney = state.fold(
      _.copy(newReturnTriageAnswers = newCompleteTriageAnswers),
      r => r.copy(draftReturn = r.draftReturn.copy(triageAnswers = newCompleteTriageAnswers))
    )

    updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney))).map {
      case Left(e) =>
        logger.warn("Could not update session", e)
        errorHandler.errorResult()

      case Right(_) =>
        Ok(checkYourAnswersPage(newCompleteTriageAnswers, displayReturnToSummaryLink))
    }
  }

  def checkYourAnswersSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withTriageAnswers(request) {
      case (_, state) =>
        triageAnswersFomState(state) match {
          case _: IncompleteTriageAnswers =>
            Redirect(routes.CanTheyUseOurServiceController.checkYourAnswers())

          case complete: CompleteTriageAnswers =>
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

  private def handleTriagePageSubmit[R, A, Page: Writeable](
    requiredField: TriageAnswers => Option[R],
    redirectToIfNotValidJourney: TriageAnswers => Call
  )(
    form: R => Form[A]
  )(
    page: (TriageAnswers, Form[A], Boolean, R) => Page,
    updateState: (A, TriageAnswers) => TriageAnswers,
    nextResult: (A, TriageAnswers) => Result
  )(
    implicit request: RequestWithSessionData[_]
  ): Future[Result] =
    withTriageAnswers(request) {
      case (_, state) =>
        val triageAnswers = triageAnswersFomState(state)
        requiredField(triageAnswers) match {
          case None => Redirect(redirectToIfNotValidJourney(triageAnswers))
          case Some(r) =>
            form(r)
              .bindFromRequest()
              .fold(
                formWithErrors => BadRequest(page(triageAnswers, formWithErrors, state.isRight, r)), { value =>
                  val updatedAnswers = updateState(value, triageAnswers)
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

  private def displayTriagePage[R, A, Page: Writeable](
    requiredField: TriageAnswers => Option[R],
    redirectToIfNotValidJourney: TriageAnswers => Call
  )(
    form: R => Form[A]
  )(extractField: TriageAnswers => Option[A], page: (TriageAnswers, Form[A], Boolean, R) => Page)(
    implicit request: RequestWithSessionData[_]
  ): Future[Result] =
    withTriageAnswers(request) {
      case (_, state) =>
        val triageAnswers = triageAnswersFomState(state)
        requiredField(triageAnswers) match {
          case None => Redirect(redirectToIfNotValidJourney(triageAnswers))
          case Some(r) =>
            val f = extractField(triageAnswers)
              .fold(form(r))(form(r).fill)

            Ok(page(triageAnswers, f, state.isRight, r))
        }
    }

  private def withTriageAnswers(request: RequestWithSessionData[_])(
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

  private def backLink(currentState: TriageAnswers, ifIncomplete: Call): Call =
    currentState.fold(_ => ifIncomplete, _ => routes.CanTheyUseOurServiceController.checkYourAnswers())

  private def triageAnswersFomState(
    state: Either[StartingNewDraftReturn, FillingOutReturn]
  ): TriageAnswers =
    state.bimap(_.newReturnTriageAnswers, _.draftReturn.triageAnswers).merge

}

object CanTheyUseOurServiceController {

  val whoAreYouReportingForForm: Form[IndividualUserType] = Form(
    mapping(
      "individualUserType" -> of(
        FormUtils.radioFormFormatter("individualUserType", List(Self, Capacitor, PersonalRepresentative))
      )
    )(identity)(Some(_))
  )

  val numberOfPropertiesForm: Form[NumberOfProperties] = Form(
    mapping(
      "numberOfProperties" -> of(FormUtils.radioFormFormatter("numberOfProperties", List(One, MoreThanOne)))
    )(identity)(Some(_))
  )

  val disposalMethodForm: Form[DisposalMethod] = Form(
    mapping(
      "disposalMethod" -> of(FormUtils.radioFormFormatter("disposalMethod", List(Sold, Gifted)))
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

  val countryOfResidenceForm: Form[Country] = Form(
    mapping(
      "countryCode" -> of(Country.formatter)
    )(identity)(Some(_))
  )

  val assetTypeForNonUkResidentsForm: Form[AssetType] = Form(
    mapping(
      "assetTypeForNonUkResidents" -> of(
        FormUtils.radioFormFormatter(
          "assetTypeForNonUkResidents",
          List(Residential, NonResidential, MixedUse, IndirectDisposal)
        )
      )
    )(identity)(Some(_))
  )

}
