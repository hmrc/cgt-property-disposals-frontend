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
import play.api.data.Form
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
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.{CompleteSingleDisposalTriageAnswers, IncompleteSingleDisposalTriageAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.IndividualUserType.{Capacitor, PersonalRepresentative, Self}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.NumberOfProperties.{MoreThanOne, One}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{BooleanFormatter, FormUtils, LocalDateUtils, SessionData, TaxYear}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views.html.returns.triage.{singledisposals => triagePages}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class SingleDisposalsTriageController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionStore,
  val errorHandler: ErrorHandler,
  returnsService: ReturnsService,
  uuidGenerator: UUIDGenerator,
  cc: MessagesControllerComponents,
  val config: Configuration,
  disposalMethodPage: triagePages.how_did_you_dispose,
  wereYouAUKResidentPage: triagePages.were_you_a_uk_resident,
  countryOfResidencePage: triagePages.country_of_residence,
  assetTypeForNonUkResidentsPage: triagePages.asset_type_for_non_uk_residents,
  didYouDisposeOfResidentialPropertyPage: triagePages.did_you_dispose_of_residential_property,
  disposalDatePage: triagePages.disposal_date,
  completionDatePage: triagePages.completion_date,
  disposalDateTooEarlyUkResidents: triagePages.disposal_date_too_early_uk_residents,
  disposalDateTooEarlyNonUkResidents: triagePages.disposal_date_too_early_non_uk_residents,
  checkYourAnswersPage: triagePages.check_your_answers
)(implicit viewConfig: ViewConfig, ec: ExecutionContext)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with Logging
    with SessionUpdates {

  import SingleDisposalsTriageController._

  val taxYears: List[TaxYear] =
    config.underlying.get[List[TaxYear]]("tax-years").value

  def howDidYouDisposeOfProperty(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    displayTriagePage(
      _.fold(incomplete => if (incomplete.hasConfirmedSingleDisposal) Some(()) else None, _ => Some(())),
      _ => routes.InitialTriageQuestionsController.howManyProperties()
    )(_ => disposalMethodForm)(
      extractField = _.fold(_.disposalMethod, c => Some(c.disposalMethod)),
      page = {
        case (currentState, form, isDraftReturn, _) =>
          disposalMethodPage(
            form,
            backLink(currentState, routes.InitialTriageQuestionsController.howManyProperties()),
            isDraftReturn
          )
      }
    )
  }

  def howDidYouDisposeOfPropertySubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async {
    implicit request =>
      handleTriagePageSubmit(
        _.fold(incomplete => if (incomplete.hasConfirmedSingleDisposal) Some(()) else None, _ => Some(())),
        _ => routes.InitialTriageQuestionsController.howManyProperties()
      )(_ => disposalMethodForm)(
        page = {
          case (currentState, form, isDraftReturn, _) =>
            disposalMethodPage(
              form,
              backLink(currentState, routes.InitialTriageQuestionsController.howManyProperties()),
              isDraftReturn
            )
        },
        updateState = {
          case (disposalMethod, i) =>
            i.fold(_.copy(disposalMethod = Some(disposalMethod)), _.copy(disposalMethod = disposalMethod))
        },
        nextResult = {
          case (_, _) =>
            Redirect(routes.SingleDisposalsTriageController.checkYourAnswers())
        }
      )
  }

  def wereYouAUKResident(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    displayTriagePage(
      _.fold(_.disposalMethod, c => Some(c.disposalMethod)),
      _ => routes.SingleDisposalsTriageController.howDidYouDisposeOfProperty()
    )(_ => wasAUkResidentForm)(
      extractField = _.fold(_.wasAUKResident, c => Some(c.countryOfResidence.isUk())),
      page = {
        case (currentState, form, isDraftReturn, _) =>
          wereYouAUKResidentPage(
            form,
            backLink(currentState, routes.SingleDisposalsTriageController.howDidYouDisposeOfProperty()),
            isDraftReturn
          )
      }
    )
  }

  def wereYouAUKResidentSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    handleTriagePageSubmit(
      _.fold(_.disposalMethod, c => Some(c.disposalMethod)),
      _ => routes.SingleDisposalsTriageController.howDidYouDisposeOfProperty()
    )(_ => wasAUkResidentForm)(
      page = {
        case (currentState, form, isDraftReturn, _) =>
          wereYouAUKResidentPage(
            form,
            backLink(currentState, routes.SingleDisposalsTriageController.howDidYouDisposeOfProperty()),
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
                IncompleteSingleDisposalTriageAnswers(
                  Some(complete.individualUserType),
                  true,
                  Some(complete.disposalMethod),
                  Some(wasAUKResident),
                  None,
                  None,
                  Some(complete.disposalDate),
                  Some(complete.completionDate),
                  None
                )
            )
          }
      },
      nextResult = {
        case (_, _) =>
          Redirect(routes.SingleDisposalsTriageController.checkYourAnswers())
      }
    )
  }

  def didYouDisposeOfAResidentialProperty(): Action[AnyContent] = authenticatedActionWithSessionData.async {
    implicit request =>
      displayTriagePage(
        _.fold(_.wasAUKResident, c => Some(c.countryOfResidence.isUk())),
        _ => routes.SingleDisposalsTriageController.wereYouAUKResident()
      )(_ => wasResidentialPropertyForm)(
        extractField =
          _.fold(_.assetType.map(_ === AssetType.Residential), c => Some(c.assetType === AssetType.Residential)),
        page = {
          case (currentState, form, isDraftReturn, _) =>
            didYouDisposeOfResidentialPropertyPage(
              form,
              backLink(currentState, routes.SingleDisposalsTriageController.wereYouAUKResident()),
              isDraftReturn
            )
        }
      )
  }

  def didYouDisposeOfAResidentialPropertySubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async {
    implicit request =>
      handleTriagePageSubmit(
        _.fold(_.wasAUKResident, c => Some(c.countryOfResidence.isUk())),
        _ => routes.SingleDisposalsTriageController.wereYouAUKResident()
      )(_ => wasResidentialPropertyForm)(
        page = {
          case (currentState, form, isDraftReturn, _) =>
            didYouDisposeOfResidentialPropertyPage(
              form,
              backLink(currentState, routes.SingleDisposalsTriageController.wereYouAUKResident()),
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
              Redirect(routes.SingleDisposalsTriageController.checkYourAnswers())
            } else {
              Ok("individuals can only report on residential properties")
            }
        }
      )
  }

  private def disposalDateBackLink(triageAnswers: SingleDisposalTriageAnswers): Call =
    triageAnswers.fold(
      { incomplete =>
        if (incomplete.wasAUKResident.exists(identity))
          routes.SingleDisposalsTriageController.didYouDisposeOfAResidentialProperty()
        else
          routes.SingleDisposalsTriageController.assetTypeForNonUkResidents()
      },
      _ => routes.SingleDisposalsTriageController.checkYourAnswers()
    )

  def whenWasDisposalDate(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    displayTriagePage(
      _.fold(_.assetType, c => Some(c.assetType)),
      answers => disposalDateBackLink(answers)
    )(_ => disposalDateForm(LocalDateUtils.today(), taxYears))(
      extractField =
        _.fold(i => i.disposalDate.map(_.value).orElse(i.tooEarlyDisposalDate), c => Some(c.disposalDate.value)),
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
    def taxYear(d: LocalDate): Option[TaxYear] =
      taxYears.find(t => d < t.endDateExclusive && d >= (t.startDateInclusive))

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
        case (d, answers) =>
          def updateCompleteAnswers(
            c: CompleteSingleDisposalTriageAnswers,
            date: Either[LocalDate, DisposalDate]
          ): IncompleteSingleDisposalTriageAnswers =
            IncompleteSingleDisposalTriageAnswers(
              Some(c.individualUserType),
              true,
              Some(c.disposalMethod),
              Some(c.countryOfResidence.isUk()),
              if (c.countryOfResidence.isUk()) None else Some(c.countryOfResidence),
              Some(c.assetType),
              date.toOption,
              None,
              date.swap.toOption
            )

          taxYear(d).fold {
            answers.fold(
              _.copy(disposalDate = None, tooEarlyDisposalDate = Some(d)),
              updateCompleteAnswers(_, Left(d))
            )
          } { taxYear =>
            answers.fold(
              _.copy(disposalDate = Some(DisposalDate(d, taxYear)), tooEarlyDisposalDate = None),
              updateCompleteAnswers(_, Right(DisposalDate(d, taxYear)))
            )
          }
      },
      nextResult = { (d, _) =>
        if (taxYear(d).isEmpty) {
          Redirect(routes.SingleDisposalsTriageController.disposalDateTooEarly())
        } else {
          Redirect(routes.SingleDisposalsTriageController.checkYourAnswers())
        }
      }
    )
  }

  def whenWasCompletionDate(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    displayTriagePage(
      _.fold(_.disposalDate, c => Some(c.disposalDate)),
      _ => routes.SingleDisposalsTriageController.whenWasDisposalDate()
    )(disposalDate => completionDateForm(disposalDate, LocalDateUtils.today()))(
      extractField = _.fold(_.completionDate, c => Some(c.completionDate)),
      page = {
        case (currentState, form, isDraftReturn, _) =>
          completionDatePage(
            form,
            backLink(currentState, routes.SingleDisposalsTriageController.whenWasDisposalDate()),
            isDraftReturn
          )
      }
    )
  }

  def whenWasCompletionDateSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    handleTriagePageSubmit(
      _.fold(_.disposalDate, c => Some(c.disposalDate)),
      _ => routes.SingleDisposalsTriageController.whenWasDisposalDate()
    )(disposalDate => completionDateForm(disposalDate, LocalDateUtils.today()))(
      page = {
        case (currentState, form, isDraftReturn, _) =>
          completionDatePage(
            form,
            backLink(currentState, routes.SingleDisposalsTriageController.whenWasDisposalDate()),
            isDraftReturn
          )
      },
      updateState = { case (d, i) => i.fold(_.copy(completionDate = Some(d)), _.copy(completionDate = d)) },
      nextResult = {
        case (_, _) =>
          Redirect(routes.SingleDisposalsTriageController.checkYourAnswers())
      }
    )
  }

  def countryOfResidence(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    displayTriagePage(
      _.fold(_.wasAUKResident.filterNot(identity), c => Some(c.countryOfResidence.isUk()).filterNot(identity)),
      _ => routes.SingleDisposalsTriageController.wereYouAUKResident()
    )(_ => countryOfResidenceForm)(
      extractField = _.fold(_.countryOfResidence, c => Some(c.countryOfResidence)),
      page = {
        case (currentState, form, isDraftReturn, _) =>
          countryOfResidencePage(
            form,
            backLink(currentState, routes.SingleDisposalsTriageController.wereYouAUKResident()),
            isDraftReturn
          )
      }
    )
  }

  def countryOfResidenceSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    handleTriagePageSubmit(
      _.fold(_.wasAUKResident.filterNot(identity), c => Some(c.countryOfResidence.isUk()).filterNot(identity)),
      _ => routes.SingleDisposalsTriageController.wereYouAUKResident()
    )(_ => countryOfResidenceForm)(
      page = {
        case (currentState, form, isDraftReturn, _) =>
          countryOfResidencePage(
            form,
            backLink(currentState, routes.SingleDisposalsTriageController.wereYouAUKResident()),
            isDraftReturn
          )
      },
      updateState = { case (c, i) => i.fold(_.copy(countryOfResidence = Some(c)), _.copy(countryOfResidence = c)) },
      nextResult = {
        case (_, _) =>
          Redirect(routes.SingleDisposalsTriageController.checkYourAnswers())
      }
    )
  }

  def assetTypeForNonUkResidents(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    displayTriagePage(
      _.fold(_.countryOfResidence, c => Some(c.countryOfResidence).filterNot(_.isUk())),
      _ => routes.SingleDisposalsTriageController.countryOfResidence()
    )(_ => assetTypeForNonUkResidentsForm)(
      _.fold(_.assetType, c => Some(c.assetType)), {
        case (currentState, form, isDraftReturn, _) =>
          assetTypeForNonUkResidentsPage(
            form,
            backLink(currentState, routes.SingleDisposalsTriageController.countryOfResidence()),
            isDraftReturn
          )
      }
    )
  }

  def assetTypeForNonUkResidentsSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async {
    implicit request =>
      handleTriagePageSubmit(
        _.fold(_.countryOfResidence, c => Some(c.countryOfResidence).filterNot(_.isUk())),
        _ => routes.SingleDisposalsTriageController.countryOfResidence()
      )(_ => assetTypeForNonUkResidentsForm)(
        {
          case (currentState, form, isDraftReturn, _) =>
            assetTypeForNonUkResidentsPage(
              form,
              backLink(currentState, routes.SingleDisposalsTriageController.countryOfResidence()),
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
                  IncompleteSingleDisposalTriageAnswers(
                    Some(c.individualUserType),
                    true,
                    Some(c.disposalMethod),
                    Some(false),
                    Some(c.countryOfResidence),
                    Some(assetType),
                    None,
                    None,
                    None
                  )
              )
            }
        },
        nextResult = {
          case (_, _) =>
            Redirect(routes.SingleDisposalsTriageController.checkYourAnswers())
        }
      )
  }

  def disposalDateTooEarly(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withSingleDisposalTriageAnswers(request) {
      case (_, _, triageAnswers) =>
        triageAnswers.fold(_.wasAUKResident, c => Some(c.countryOfResidence.isUk())) match {
          case None => Redirect(routes.SingleDisposalsTriageController.checkYourAnswers())
          case Some(wasUk) =>
            if (wasUk) Ok(disposalDateTooEarlyUkResidents())
            else Ok(disposalDateTooEarlyNonUkResidents())
        }
    }
  }

  def checkYourAnswers(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withSingleDisposalTriageAnswers(request) {
      case (_, state, triageAnswers) =>
        lazy val displayReturnToSummaryLink = state.fold(_ => false, _ => true)

        triageAnswers match {
          case c: CompleteSingleDisposalTriageAnswers =>
            Ok(checkYourAnswersPage(c, displayReturnToSummaryLink))

          case IncompleteSingleDisposalTriageAnswers(None, _, _, _, _, _, _, _, _) =>
            Redirect(routes.InitialTriageQuestionsController.whoIsIndividualRepresenting())

          case IncompleteSingleDisposalTriageAnswers(_, false, _, _, _, _, _, _, _) =>
            Redirect(routes.InitialTriageQuestionsController.howManyProperties())

          case IncompleteSingleDisposalTriageAnswers(_, _, None, _, _, _, _, _, _) =>
            Redirect(routes.SingleDisposalsTriageController.howDidYouDisposeOfProperty())

          case IncompleteSingleDisposalTriageAnswers(_, _, _, None, _, _, _, _, _) =>
            Redirect(routes.SingleDisposalsTriageController.wereYouAUKResident())

          case IncompleteSingleDisposalTriageAnswers(_, _, _, Some(false), None, _, _, _, _) =>
            Redirect(routes.SingleDisposalsTriageController.countryOfResidence())

          case IncompleteSingleDisposalTriageAnswers(_, _, _, Some(false), Some(_), None, _, _, _) =>
            Redirect(routes.SingleDisposalsTriageController.assetTypeForNonUkResidents())

          case IncompleteSingleDisposalTriageAnswers(_, _, _, Some(true), _, None, _, _, _) =>
            Redirect(routes.SingleDisposalsTriageController.didYouDisposeOfAResidentialProperty())

          case IncompleteSingleDisposalTriageAnswers(_, _, _, _, _, Some(AssetType.IndirectDisposal), _, _, _) =>
            Ok("Indirect disposals not handled yet")

          case IncompleteSingleDisposalTriageAnswers(_, _, _, _, _, _, None, _, _) =>
            Redirect(routes.SingleDisposalsTriageController.whenWasDisposalDate())

          case IncompleteSingleDisposalTriageAnswers(_, _, _, _, _, _, _, None, _) =>
            Redirect(routes.SingleDisposalsTriageController.whenWasCompletionDate())

          case IncompleteSingleDisposalTriageAnswers(
              Some(t),
              true,
              Some(m),
              Some(true),
              _,
              Some(r),
              Some(d),
              Some(c),
              _
              ) =>
            updateAnswersAndShowCheckYourAnswersPage(
              state,
              CompleteSingleDisposalTriageAnswers(t, m, Country.uk, r, d, c),
              displayReturnToSummaryLink
            )

          case IncompleteSingleDisposalTriageAnswers(
              Some(t),
              true,
              Some(m),
              Some(false),
              Some(country),
              Some(r),
              Some(d),
              Some(c),
              _
              ) =>
            updateAnswersAndShowCheckYourAnswersPage(
              state,
              CompleteSingleDisposalTriageAnswers(t, m, country, r, d, c),
              displayReturnToSummaryLink
            )
        }
    }
  }

  private def updateAnswersAndShowCheckYourAnswersPage(
    state: Either[StartingNewDraftReturn, FillingOutReturn],
    newCompleteTriageAnswers: CompleteSingleDisposalTriageAnswers,
    displayReturnToSummaryLink: Boolean
  )(implicit request: RequestWithSessionData[_], hc: HeaderCarrier): Future[Result] = {
    val updatedJourney = state.fold(
      _.copy(newReturnTriageAnswers = Right(newCompleteTriageAnswers)),
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
    withSingleDisposalTriageAnswers(request) {
      case (_, state, triageAnswers) =>
        triageAnswers match {
          case _: IncompleteSingleDisposalTriageAnswers =>
            Redirect(routes.SingleDisposalsTriageController.checkYourAnswers())

          case complete: CompleteSingleDisposalTriageAnswers =>
            lazy val continueToTaskList = Redirect(returnsRoutes.TaskListController.taskList())

            def toFillingOurNewReturn(startingNewDraftReturn: StartingNewDraftReturn): Future[Result] = {
              val newDraftReturn =
                DraftReturn(
                  uuidGenerator.nextId(),
                  state.fold(_.subscribedDetails.cgtReference, _.subscribedDetails.cgtReference),
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
    requiredField: SingleDisposalTriageAnswers => Option[R],
    redirectToIfNotValidJourney: SingleDisposalTriageAnswers => Call
  )(
    form: R => Form[A]
  )(
    page: (SingleDisposalTriageAnswers, Form[A], Boolean, R) => Page,
    updateState: (A, SingleDisposalTriageAnswers) => SingleDisposalTriageAnswers,
    nextResult: (A, SingleDisposalTriageAnswers) => Result
  )(
    implicit request: RequestWithSessionData[_]
  ): Future[Result] =
    withSingleDisposalTriageAnswers(request) {
      case (_, state, triageAnswers) =>
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
                      s => s -> s.copy(newReturnTriageAnswers = Right(updatedAnswers)),
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
    requiredField: SingleDisposalTriageAnswers => Option[R],
    redirectToIfNotValidJourney: SingleDisposalTriageAnswers => Call
  )(
    form: R => Form[A]
  )(
    extractField: SingleDisposalTriageAnswers => Option[A],
    page: (SingleDisposalTriageAnswers, Form[A], Boolean, R) => Page
  )(
    implicit request: RequestWithSessionData[_]
  ): Future[Result] =
    withSingleDisposalTriageAnswers(request) {
      case (_, state, triageAnswers) =>
        requiredField(triageAnswers) match {
          case None => Redirect(redirectToIfNotValidJourney(triageAnswers))
          case Some(r) =>
            val f = extractField(triageAnswers)
              .fold(form(r))(form(r).fill)

            Ok(page(triageAnswers, f, state.isRight, r))
        }
    }

  private def withSingleDisposalTriageAnswers(request: RequestWithSessionData[_])(
    f: (SessionData, Either[StartingNewDraftReturn, FillingOutReturn], SingleDisposalTriageAnswers) => Future[Result]
  ): Future[Result] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some((session, s @ StartingNewDraftReturn(_, _, _, Right(t)))) =>
        f(session, Left(s), t)

      case Some((session, r @ FillingOutReturn(_, _, _, d))) =>
        f(session, Right(r), d.triageAnswers)

      case _ =>
        Redirect(uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.routes.StartController.start())
    }

  private def backLink(currentState: SingleDisposalTriageAnswers, ifIncomplete: Call): Call =
    currentState.fold(_ => ifIncomplete, _ => routes.SingleDisposalsTriageController.checkYourAnswers())

}

object SingleDisposalsTriageController {

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

  def disposalDateForm(maximumDateInclusive: LocalDate, taxYears: List[TaxYear]): Form[LocalDate] =
    Form(
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
      )(identity)(Some(_))
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
