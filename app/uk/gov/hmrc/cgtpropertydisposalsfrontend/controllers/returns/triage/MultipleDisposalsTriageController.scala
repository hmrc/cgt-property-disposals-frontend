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
import cats.instances.boolean._
import cats.instances.future._
import cats.instances.list._
import cats.syntax.either._
import cats.syntax.eq._
import com.google.inject.{Inject, Singleton}
import play.api.Configuration
import play.api.data.Forms.{mapping, of}
import play.api.data.format.Formatter
import play.api.data.{Form, FormError, Forms}
import play.api.mvc._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.triage.MultipleDisposalsTriageController._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.FormUtils.readValue
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, StartingNewDraftReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Country
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.UUIDGenerator
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.MultipleDisposalsTriageAnswers._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.IncompleteSingleDisposalTriageAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{BooleanFormatter, Error, FormUtils, LocalDateUtils, SessionData, TaxYear}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.{ReturnsService, TaxYearService}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging.LoggerOps
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views.html.returns.triage.{multipledisposals => triagePages}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class MultipleDisposalsTriageController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionStore,
  val errorHandler: ErrorHandler,
  returnsService: ReturnsService,
  taxYearService: TaxYearService,
  uuidGenerator: UUIDGenerator,
  cc: MessagesControllerComponents,
  val config: Configuration,
  guidancePage: triagePages.guidance,
  howManyPropertiesPage: triagePages.how_many_properties,
  wereYouAUKResidentPage: triagePages.were_you_a_uk_resident,
  wereAllPropertiesResidentialPage: triagePages.were_all_properties_residential,
  countryOfResidencePage: triagePages.country_of_residence,
  taxYearExchangedPage: triagePages.tax_year_exchanged,
  assetTypeForNonUkResidentsPage: triagePages.asset_type_for_non_uk_residents,
  completionDatePage: triagePages.completion_date,
  checkYourAnswersPage: triagePages.check_you_answers
)(implicit viewConfig: ViewConfig, ec: ExecutionContext)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with Logging
    with SessionUpdates {

  def guidance(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withMultipleDisposalTriageAnswers(request) {
      case (_, state, answers) =>
        val backLink = answers.fold(
          _ => routes.CommonTriageQuestionsController.howManyProperties(),
          _ => routes.MultipleDisposalsTriageController.checkYourAnswers()
        )
        Ok(guidancePage(backLink, state.isRight))
    }
  }

  def guidanceSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withMultipleDisposalTriageAnswers(request) {
      case (_, _, answers) =>
        answers.fold(
          _ => Redirect(routes.MultipleDisposalsTriageController.howManyDisposals()),
          _ => Redirect(routes.MultipleDisposalsTriageController.checkYourAnswers())
        )
    }
  }

  def howManyDisposals(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withMultipleDisposalTriageAnswers(request) {
      case (_, state, answers) =>
        val numberOfDisposals = answers.fold(_.numberOfProperties, c => Some(c.numberOfProperties))
        val form              = numberOfDisposals.fold(numberOfPropertiesForm)(numberOfPropertiesForm.fill)
        val backLink = answers.fold(
          _ => routes.MultipleDisposalsTriageController.guidance(),
          _ => routes.MultipleDisposalsTriageController.checkYourAnswers()
        )
        Ok(howManyPropertiesPage(form, backLink, state.isRight))
    }
  }

  def howManyDisposalsSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withMultipleDisposalTriageAnswers(request) {
      case (_, state, answers) =>
        numberOfPropertiesForm
          .bindFromRequest()
          .fold(
            { formWithErrors =>
              val backLink = answers.fold(
                _ => routes.MultipleDisposalsTriageController.guidance(),
                _ => routes.MultipleDisposalsTriageController.checkYourAnswers()
              )
              BadRequest(
                howManyPropertiesPage(formWithErrors, backLink, state.isRight)
              )
            }, { numberOfProperties =>
              if (answers.fold(_.numberOfProperties, c => Some(c.numberOfProperties)).contains(numberOfProperties)) {
                Redirect(routes.MultipleDisposalsTriageController.checkYourAnswers())
              } else {
                val (newState, redirectTo) = updateNumberOfPropertiesWithRedirect(numberOfProperties, answers, state)
                updateStateAndThen(newState, Redirect(redirectTo))
              }

            }
          )
    }
  }

  private def updateNumberOfPropertiesWithRedirect(
    numberOfProperties: Int,
    currentAnswers: MultipleDisposalsTriageAnswers,
    currentState: Either[StartingNewDraftReturn, (FillingOutReturn, MultipleDisposalsDraftReturn)]
  ): (Either[StartingNewDraftReturn, FillingOutReturn], Call) = {
    val newAnswersWithRedirectTo =
      if (numberOfProperties > 1)
        Left[MultipleDisposalsTriageAnswers, IncompleteSingleDisposalTriageAnswers](
          currentAnswers.fold[MultipleDisposalsTriageAnswers](
            _.copy(numberOfProperties = Some(numberOfProperties)),
            _.copy(numberOfProperties = numberOfProperties)
          )
        ) -> routes.MultipleDisposalsTriageController.checkYourAnswers()
      else
        Right[MultipleDisposalsTriageAnswers, IncompleteSingleDisposalTriageAnswers](
          IncompleteSingleDisposalTriageAnswers.empty.copy(
            individualUserType         = currentAnswers.fold(_.individualUserType, _.individualUserType),
            hasConfirmedSingleDisposal = true
          )
        ) -> routes.SingleDisposalsTriageController.checkYourAnswers()

    val newState = currentState.bimap(
      _.copy(newReturnTriageAnswers = newAnswersWithRedirectTo._1), {
        case (r, d) =>
          val newDraftReturn = newAnswersWithRedirectTo._1.bimap(
            multipleDisposalsTriageAnswers => d.copy(triageAnswers = multipleDisposalsTriageAnswers),
            incompleteSingleDisposalTriageAnswers =>
              SingleDisposalDraftReturn(
                d.id,
                incompleteSingleDisposalTriageAnswers,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                LocalDateUtils.today()
              )
          )

          r.copy(draftReturn = newDraftReturn.merge)
      }
    )
    newState -> newAnswersWithRedirectTo._2
  }

  def wereYouAUKResident(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withMultipleDisposalTriageAnswers(request) {
      case (_, state, answers) =>
        val wereYouUKResident = answers.fold(_.wasAUKResident, c => Some(c.countryOfResidence.isUk()))
        val form              = wereYouUKResident.fold(wasAUkResidentForm)(wasAUkResidentForm.fill)
        val backLink = answers.fold(
          _ => routes.MultipleDisposalsTriageController.howManyDisposals(),
          _ => routes.MultipleDisposalsTriageController.checkYourAnswers()
        )

        Ok(wereYouAUKResidentPage(form, backLink, state.isRight))
    }
  }

  def wereYouAUKResidentSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withMultipleDisposalTriageAnswers(request) {
      case (_, state, answers) =>
        wasAUkResidentForm
          .bindFromRequest()
          .fold(
            { formWithErrors =>
              val backLink = answers.fold(
                _ => routes.MultipleDisposalsTriageController.howManyDisposals(),
                _ => routes.MultipleDisposalsTriageController.checkYourAnswers()
              )

              BadRequest(
                wereYouAUKResidentPage(formWithErrors, backLink, state.isRight)
              )
            }, { wereUKResident =>
              if (answers.fold(_.wasAUKResident, c => Some(c.countryOfResidence.isUk())).contains(wereUKResident)) {
                Redirect(routes.MultipleDisposalsTriageController.checkYourAnswers())
              } else {
                val updatedAnswers = answers.fold[MultipleDisposalsTriageAnswers](
                  incomplete =>
                    incomplete.copy(
                      wasAUKResident               = Some(wereUKResident),
                      countryOfResidence           = None,
                      wereAllPropertiesResidential = None,
                      assetTypes                   = None
                    ),
                  complete =>
                    IncompleteMultipleDisposalsTriageAnswers(
                      individualUserType           = complete.individualUserType,
                      numberOfProperties           = Some(complete.numberOfProperties),
                      wasAUKResident               = Some(wereUKResident),
                      countryOfResidence           = None,
                      wereAllPropertiesResidential = None,
                      assetTypes                   = None,
                      taxYearAfter6April2020       = None,
                      taxYear                      = None,
                      completionDate               = Some(complete.completionDate)
                    )
                )

                val newState = updateState(state, updatedAnswers)
                updateStateAndThen(newState, Redirect(routes.MultipleDisposalsTriageController.checkYourAnswers()))
              }
            }
          )
    }
  }

  def wereAllPropertiesResidential(): Action[AnyContent] = authenticatedActionWithSessionData.async {
    implicit request =>
      withMultipleDisposalTriageAnswers(request) {
        case (_, state, answers) =>
          val werePropertiesResidential =
            answers.fold(_.wereAllPropertiesResidential, c => Some(c.assetTypes == AssetType.Residential))
          val form =
            werePropertiesResidential.fold(wereAllPropertiesResidentialForm)(wereAllPropertiesResidentialForm.fill)
          val backLink = answers.fold(
            _ => routes.MultipleDisposalsTriageController.wereYouAUKResident(),
            _ => routes.MultipleDisposalsTriageController.checkYourAnswers()
          )
          Ok(wereAllPropertiesResidentialPage(form, backLink, state.isRight))
      }
  }

  def wereAllPropertiesResidentialSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async {
    implicit request =>
      withMultipleDisposalTriageAnswers(request) {
        case (_, state, answers) =>
          wereAllPropertiesResidentialForm
            .bindFromRequest()
            .fold(
              { formWithErrors =>
                val backLink = answers.fold(
                  _ => routes.MultipleDisposalsTriageController.wereYouAUKResident(),
                  _ => routes.MultipleDisposalsTriageController.checkYourAnswers()
                )
                BadRequest(
                  wereAllPropertiesResidentialPage(
                    formWithErrors,
                    backLink,
                    state.isRight
                  )
                )
              }, { wereAllPropertiesResidential =>
                if (answers
                      .fold(_.wereAllPropertiesResidential, c => Some(isResidentialAssetType(c.assetTypes)))
                      .contains(wereAllPropertiesResidential)) {
                  Redirect(routes.MultipleDisposalsTriageController.checkYourAnswers())
                } else {
                  val updatedAnswers = answers.fold[MultipleDisposalsTriageAnswers](
                    incomplete =>
                      incomplete.copy(
                        wereAllPropertiesResidential = Some(wereAllPropertiesResidential),
                        assetTypes                   = Some(assetType(wereAllPropertiesResidential))
                      ),
                    complete =>
                      IncompleteMultipleDisposalsTriageAnswers(
                        individualUserType           = complete.individualUserType,
                        numberOfProperties           = Some(complete.numberOfProperties),
                        wasAUKResident               = Some(true),
                        countryOfResidence           = Some(Country.uk),
                        wereAllPropertiesResidential = Some(wereAllPropertiesResidential),
                        assetTypes                   = Some(assetType(wereAllPropertiesResidential)),
                        taxYearAfter6April2020       = None,
                        taxYear                      = None,
                        completionDate               = None
                      )
                  )

                  val newState = updateState(state, updatedAnswers)
                  updateStateAndThen(newState, Redirect(routes.MultipleDisposalsTriageController.checkYourAnswers()))
                }
              }
            )
      }
  }

  def countryOfResidence(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withMultipleDisposalTriageAnswers(request) {
      case (_, state, answers) =>
        val wasUkResident = answers.fold(_.wasAUKResident, c => Some(c.countryOfResidence.isUk()))

        if (!wasUkResident.contains(false)) {
          Redirect(routes.MultipleDisposalsTriageController.checkYourAnswers())
        } else {
          val countryOfResidence =
            answers.fold(_.countryOfResidence, c => Some(c.countryOfResidence))
          val form =
            countryOfResidence.fold(countryOfResidenceForm)(countryOfResidenceForm.fill)
          val backLink = answers.fold(
            _ => routes.MultipleDisposalsTriageController.wereYouAUKResident(),
            _ => routes.MultipleDisposalsTriageController.checkYourAnswers()
          )
          Ok(countryOfResidencePage(form, backLink, state.isRight))
        }
    }
  }

  def countryOfResidenceSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withMultipleDisposalTriageAnswers(request) {
      case (_, state, answers) =>
        countryOfResidenceForm
          .bindFromRequest()
          .fold(
            { formWithErrors =>
              val backLink = answers.fold(
                _ => routes.MultipleDisposalsTriageController.wereYouAUKResident(),
                _ => routes.MultipleDisposalsTriageController.checkYourAnswers()
              )

              BadRequest(
                countryOfResidencePage(
                  formWithErrors,
                  backLink,
                  state.isRight
                )
              )
            }, { countryOfResidence =>
              if (answers
                    .fold(_.countryOfResidence, c => Some(c.countryOfResidence))
                    .contains(countryOfResidence)) {
                Redirect(routes.MultipleDisposalsTriageController.checkYourAnswers())
              } else {
                val updatedAnswers =
                  answers.fold[MultipleDisposalsTriageAnswers](
                    _.copy(countryOfResidence = Some(countryOfResidence)),
                    _.copy(countryOfResidence = countryOfResidence)
                  )

                val newState = updateState(state, updatedAnswers)
                updateStateAndThen(newState, Redirect(routes.MultipleDisposalsTriageController.checkYourAnswers()))
              }

            }
          )
    }
  }

  def whenWereContractsExchanged(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withMultipleDisposalTriageAnswers(request) {
      case (_, state, answers) =>
        val taxYearExchanged = answers.fold(_.taxYearAfter6April2020, _ => Some(true))
        val form             = taxYearExchanged.fold(taxYearExchangedForm)(taxYearExchangedForm.fill)
        val backLink = answers.fold(
          i => incompleteJourneyTaxYearBackLink(i.wasAUKResident.contains(true)),
          _ => routes.MultipleDisposalsTriageController.checkYourAnswers()
        )
        Ok(
          taxYearExchangedPage(
            form,
            backLink,
            state.isRight
          )
        )
    }
  }

  def whenWereContractsExchangedSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async {
    implicit request =>
      withMultipleDisposalTriageAnswers(request) {
        case (_, state, answers) =>
          taxYearExchangedForm
            .bindFromRequest()
            .fold(
              { formWithErrors =>
                val backLink = answers.fold(
                  i => incompleteJourneyTaxYearBackLink(i.wasAUKResident.contains(true)),
                  _ => routes.MultipleDisposalsTriageController.checkYourAnswers()
                )
                BadRequest(
                  taxYearExchangedPage(
                    formWithErrors,
                    backLink,
                    state.isRight
                  )
                )
              }, { taxYearAfter6April2020 =>
                if (answers.fold(_.taxYearAfter6April2020, _ => Some(true)).contains(taxYearAfter6April2020)) {
                  Redirect(routes.MultipleDisposalsTriageController.checkYourAnswers())
                } else {

                  val result =
                    for {
                      taxYear <- if (taxYearAfter6April2020) taxYearService.taxYear(LocalDateUtils.today())
                                else EitherT.pure[Future, Error](None)
                      updatedAnswers <- EitherT.fromEither[Future](
                                         updateTaxYearToAnswers(taxYearAfter6April2020, taxYear, answers)
                                       )
                      newState = updateState(state, updatedAnswers)
                      _ <- newState.fold(
                            _ => EitherT.pure[Future, Error](()),
                            r =>
                              returnsService.storeDraftReturn(
                                r.draftReturn,
                                r.subscribedDetails.cgtReference,
                                r.agentReferenceNumber
                              )
                          )
                      _ <- EitherT(
                            updateSession(sessionStore, request)(_.copy(journeyStatus = Some(newState.merge)))
                          )
                    } yield ()

                  result.fold(
                    { e =>
                      logger.warn("Could not find tax year or update session", e)
                      errorHandler.errorResult()
                    },
                    _ => Redirect(routes.MultipleDisposalsTriageController.checkYourAnswers())
                  )

                }
              }
            )
      }
  }

  def assetTypeForNonUkResidents(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withMultipleDisposalTriageAnswers(request) {
      case (_, state, answers) =>
        val assetType = answers.fold(_.assetTypes, c => Some(c.assetTypes))
        val form      = assetType.fold(assetTypeForNonUkResidentsForm)(assetTypeForNonUkResidentsForm.fill)
        val backLink = answers.fold(
          _ => routes.MultipleDisposalsTriageController.countryOfResidence(),
          _ => routes.MultipleDisposalsTriageController.checkYourAnswers()
        )
        Ok(assetTypeForNonUkResidentsPage(form, backLink, state.isRight))
    }
  }

  def assetTypeForNonUkResidentsSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async {
    implicit request =>
      withMultipleDisposalTriageAnswers(request) {
        case (_, state, answers) =>
          assetTypeForNonUkResidentsForm
            .bindFromRequest()
            .fold(
              { formWithErrors =>
                val backLink = answers.fold(
                  _ => routes.MultipleDisposalsTriageController.countryOfResidence(),
                  _ => routes.MultipleDisposalsTriageController.checkYourAnswers()
                )
                BadRequest(
                  assetTypeForNonUkResidentsPage(
                    formWithErrors,
                    backLink,
                    state.isRight
                  )
                )
              }, { assetTypes =>
                if (answers.fold(_.assetTypes, c => Some(c.assetTypes)).contains(assetTypes)) {
                  Redirect(routes.MultipleDisposalsTriageController.checkYourAnswers())
                } else {
                  val updatedAnswers =
                    answers.fold[MultipleDisposalsTriageAnswers](
                      _.copy(assetTypes = Some(assetTypes)),
                      complete =>
                        IncompleteMultipleDisposalsTriageAnswers(
                          complete.individualUserType,
                          Some(complete.numberOfProperties),
                          Some(false),
                          Some(complete.countryOfResidence),
                          None,
                          Some(assetTypes),
                          Some(true),
                          Some(complete.taxYear),
                          Some(complete.completionDate)
                        )
                    )

                  val newState = updateState(state, updatedAnswers)
                  updateStateAndThen(newState, Redirect(routes.MultipleDisposalsTriageController.checkYourAnswers()))
                }
              }
            )
      }
  }

  def completionDate(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withMultipleDisposalTriageAnswers(request) {
      case (_, state, answers) =>
        val completionDate = answers.fold(_.completionDate, c => Some(c.completionDate))
        val today          = LocalDateUtils.today()
        val form           = completionDate.fold(completionDateForm(today))(completionDateForm(today).fill)
        val backLink = answers.fold(
          _ => routes.MultipleDisposalsTriageController.whenWereContractsExchanged(),
          _ => routes.MultipleDisposalsTriageController.checkYourAnswers()
        )
        Ok(completionDatePage(form, backLink, state.isRight))
    }
  }

  def completionDateSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withMultipleDisposalTriageAnswers(request) {
      case (_, state, answers) =>
        completionDateForm(LocalDateUtils.today())
          .bindFromRequest()
          .fold(
            { formWithErrors =>
              val backLink = answers.fold(
                _ => routes.MultipleDisposalsTriageController.whenWereContractsExchanged(),
                _ => routes.MultipleDisposalsTriageController.checkYourAnswers()
              )
              BadRequest(
                completionDatePage(
                  formWithErrors,
                  backLink,
                  state.isRight
                )
              )
            }, { completionDate =>
              if (answers.fold(_.completionDate, c => Some(c.completionDate)).contains(completionDate)) {
                Redirect(routes.MultipleDisposalsTriageController.checkYourAnswers())
              } else {
                val updatedAnswers =
                  answers.fold[MultipleDisposalsTriageAnswers](
                    _.copy(completionDate = Some(completionDate)),
                    _.copy(completionDate = completionDate)
                  )
                val newState = updateState(state, updatedAnswers)
                updateStateAndThen(newState, Redirect(routes.MultipleDisposalsTriageController.checkYourAnswers()))

              }
            }
          )
    }
  }

  private def updateTaxYearToAnswers(
    taxYearAfter6April2020: Boolean,
    taxYear: Option[TaxYear],
    answers: MultipleDisposalsTriageAnswers
  ): Either[Error, MultipleDisposalsTriageAnswers] =
    taxYear match {
      case None if taxYearAfter6April2020 =>
        Left(Error("Could not find tax year"))
      case _ =>
        Right(
          answers.fold[MultipleDisposalsTriageAnswers](
            incomplete =>
              incomplete.copy(
                taxYearAfter6April2020 = Some(taxYearAfter6April2020),
                taxYear                = taxYear
              ),
            complete =>
              IncompleteMultipleDisposalsTriageAnswers(
                individualUserType           = complete.individualUserType,
                numberOfProperties           = Some(complete.numberOfProperties),
                wasAUKResident               = Some(complete.countryOfResidence.isUk()),
                countryOfResidence           = Some(complete.countryOfResidence),
                wereAllPropertiesResidential = Some(isResidentialAssetType(complete.assetTypes)),
                assetTypes                   = Some(complete.assetTypes),
                taxYearAfter6April2020       = Some(taxYearAfter6April2020),
                taxYear                      = taxYear,
                completionDate               = Some(complete.completionDate)
              )
          )
        )
    }

  def checkYourAnswers(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withMultipleDisposalTriageAnswers(request) {
      case (_, state, triageAnswers) =>
        val isIndividual = state.fold(_.subscribedDetails, _._1.subscribedDetails).userType().isRight

        triageAnswers match {
          case IncompleteMultipleDisposalsTriageAnswers(None, _, _, _, _, _, _, _, _) if isIndividual =>
            Redirect(routes.CommonTriageQuestionsController.whoIsIndividualRepresenting())

          case IncompleteMultipleDisposalsTriageAnswers(Some(IndividualUserType.Capacitor), _, _, _, _, _, _, _, _) =>
            Redirect(routes.CommonTriageQuestionsController.capacitorsAndPersonalRepresentativesNotHandled())

          case IncompleteMultipleDisposalsTriageAnswers(
              Some(IndividualUserType.PersonalRepresentative),
              _,
              _,
              _,
              _,
              _,
              _,
              _,
              _
              ) =>
            Redirect(routes.CommonTriageQuestionsController.capacitorsAndPersonalRepresentativesNotHandled())

          case IncompleteMultipleDisposalsTriageAnswers(_, None, _, _, _, _, _, _, _) =>
            Redirect(routes.MultipleDisposalsTriageController.guidance())

          case IncompleteMultipleDisposalsTriageAnswers(_, _, None, _, _, _, _, _, _) =>
            Redirect(routes.MultipleDisposalsTriageController.wereYouAUKResident())

          case IncompleteMultipleDisposalsTriageAnswers(_, _, Some(false), None, _, _, _, _, _) =>
            Redirect(routes.MultipleDisposalsTriageController.countryOfResidence())

          case IncompleteMultipleDisposalsTriageAnswers(_, _, Some(false), _, _, None, _, _, _) =>
            Redirect(routes.MultipleDisposalsTriageController.assetTypeForNonUkResidents())

          case IncompleteMultipleDisposalsTriageAnswers(_, _, Some(false), _, _, Some(assetTypes), _, _, _)
              if (assetTypes =!= List(AssetType.Residential) && assetTypes =!= List(AssetType.NonResidential)) =>
            Redirect(routes.CommonTriageQuestionsController.assetTypeNotYetImplemented())

          case IncompleteMultipleDisposalsTriageAnswers(_, _, Some(true), _, None, _, _, _, _) =>
            Redirect(routes.MultipleDisposalsTriageController.wereAllPropertiesResidential())

          case IncompleteMultipleDisposalsTriageAnswers(_, _, Some(true), _, Some(false), _, _, _, _) =>
            Redirect(routes.CommonTriageQuestionsController.ukResidentCanOnlyDisposeResidential())

          case IncompleteMultipleDisposalsTriageAnswers(_, _, _, _, _, _, None, _, _) =>
            Redirect(routes.MultipleDisposalsTriageController.whenWereContractsExchanged())

          case IncompleteMultipleDisposalsTriageAnswers(_, _, _, _, _, _, Some(false), _, _) =>
            Redirect(routes.CommonTriageQuestionsController.disposalDateTooEarly())

          case IncompleteMultipleDisposalsTriageAnswers(_, _, _, _, _, _, Some(true), None, _) =>
            logger.warn("No tax year was found when we expected one")
            errorHandler.errorResult()

          case IncompleteMultipleDisposalsTriageAnswers(_, _, _, _, _, _, _, _, None) =>
            Redirect(routes.MultipleDisposalsTriageController.completionDate())

          case IncompleteMultipleDisposalsTriageAnswers(
              i,
              Some(n),
              Some(true),
              _,
              Some(true),
              Some(a),
              Some(true),
              Some(t),
              Some(d)
              ) =>
            val completeAnswers = CompleteMultipleDisposalsTriageAnswers(i, n, Country.uk, a, t, d)
            updateStateAndThen(
              updateState(state, completeAnswers),
              Ok(checkYourAnswersPage(completeAnswers, state.isRight))
            )

          case IncompleteMultipleDisposalsTriageAnswers(
              i,
              Some(n),
              Some(false),
              Some(c),
              _,
              Some(a),
              Some(true),
              Some(t),
              Some(d)
              ) =>
            val completeAnswers = CompleteMultipleDisposalsTriageAnswers(i, n, c, a, t, d)
            updateStateAndThen(
              updateState(state, completeAnswers),
              Ok(checkYourAnswersPage(completeAnswers, state.isRight))
            )

          case c: CompleteMultipleDisposalsTriageAnswers =>
            Ok(checkYourAnswersPage(c, state.isRight))
        }
    }
  }

  def checkYourAnswersSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withMultipleDisposalTriageAnswers(request) {
      case (_, journey, answers) =>
        journey match {
          case Right(_) => Redirect(controllers.returns.routes.TaskListController.taskList())

          case Left(startingNewDraftReturn) =>
            answers match {
              case _: IncompleteMultipleDisposalsTriageAnswers =>
                Redirect(routes.MultipleDisposalsTriageController.checkYourAnswers())

              case c: CompleteMultipleDisposalsTriageAnswers =>
                val newDraftReturn = MultipleDisposalsDraftReturn(uuidGenerator.nextId(), c, LocalDateUtils.today())
                val newJourney = FillingOutReturn(
                  startingNewDraftReturn.subscribedDetails,
                  startingNewDraftReturn.ggCredId,
                  startingNewDraftReturn.agentReferenceNumber,
                  newDraftReturn
                )

                updateStateAndThen(
                  Right(newJourney),
                  Redirect(controllers.returns.routes.TaskListController.taskList())
                )
            }
        }

    }

  }

  private def incompleteJourneyTaxYearBackLink(wasAUKResident: Boolean): Call =
    if (wasAUKResident) routes.MultipleDisposalsTriageController.wereAllPropertiesResidential()
    else routes.MultipleDisposalsTriageController.assetTypeForNonUkResidents()

  private def isResidentialAssetType(assetType: List[AssetType]): Boolean =
    assetType match {
      case AssetType.Residential :: Nil => true
      case _                            => false
    }

  private def assetType(isResidential: Boolean): List[AssetType] =
    if (isResidential) List(AssetType.Residential) else List(AssetType.NonResidential)

  private def withMultipleDisposalTriageAnswers(request: RequestWithSessionData[_])(
    f: (
      SessionData,
      Either[StartingNewDraftReturn, (FillingOutReturn, MultipleDisposalsDraftReturn)],
      MultipleDisposalsTriageAnswers
    ) => Future[Result]
  ): Future[Result] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some((session, s @ StartingNewDraftReturn(_, _, _, Left(t)))) =>
        f(session, Left(s), t)

      case Some((session, r @ FillingOutReturn(_, _, _, m: MultipleDisposalsDraftReturn))) =>
        f(session, Right(r -> m), m.triageAnswers)

      case _ =>
        Redirect(uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.routes.StartController.start())
    }

  private def updateStateAndThen(
    updatedState: Either[StartingNewDraftReturn, FillingOutReturn],
    f: => Result
  )(implicit hc: HeaderCarrier, request: RequestWithSessionData[_]): Future[Result] = {
    val result = for {
      _ <- updatedState.fold(
            _ => EitherT.pure[Future, Error](()),
            r =>
              returnsService
                .storeDraftReturn(r.draftReturn, r.subscribedDetails.cgtReference, r.agentReferenceNumber)
          )
      _ <- EitherT(
            updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedState.merge)))
          )
    } yield ()

    result.fold({ e =>
      logger.warn("Could not update session", e)
      errorHandler.errorResult()
    }, _ => f)
  }

  private def updateState(
    currentState: Either[StartingNewDraftReturn, (FillingOutReturn, MultipleDisposalsDraftReturn)],
    newAnswers: MultipleDisposalsTriageAnswers
  ): Either[StartingNewDraftReturn, FillingOutReturn] =
    currentState.bimap(
      _.copy(newReturnTriageAnswers = Left(newAnswers)),
      { case (r, d) => r.copy(draftReturn = d.copy(triageAnswers = newAnswers)) }
    )

}

object MultipleDisposalsTriageController {

  val numberOfPropertiesForm: Form[Int] = {
    val numberOfDisposalsKey = "multipleDisposalsNumberOfProperties"

    val numberOfPropertiesFormatter: Formatter[Int] = {
      def validateNumberOfProperties(i: Int): Either[FormError, Int] =
        if (i <= 0) Left(FormError(numberOfDisposalsKey, "error.tooSmall"))
        else if (i > 999) Left(FormError(numberOfDisposalsKey, "error.tooLong"))
        else Right(i)

      new Formatter[Int] {
        override def bind(key: String, data: Map[String, String]): Either[Seq[FormError], Int] = {
          val result =
            FormUtils.readValue(key, data, _.toInt).flatMap(validateNumberOfProperties)
          result.leftMap(Seq(_))
        }
        override def unbind(key: String, value: Int): Map[String, String] =
          Map(key -> value.toString)
      }
    }

    Form(
      mapping(
        numberOfDisposalsKey -> of(numberOfPropertiesFormatter)
      )(identity)(Some(_))
    )
  }

  val wasAUkResidentForm: Form[Boolean] = Form(
    mapping(
      "multipleDisposalsWereYouAUKResident" -> of(BooleanFormatter.formatter)
    )(identity)(Some(_))
  )

  val wereAllPropertiesResidentialForm: Form[Boolean] = Form(
    mapping(
      "multipleDisposalsWereAllPropertiesResidential" -> of(BooleanFormatter.formatter)
    )(identity)(Some(_))
  )

  val countryOfResidenceForm: Form[Country] = Form(
    mapping(
      "countryCode" -> of(Country.formatter)
    )(identity)(Some(_))
  )

  val taxYearExchangedForm: Form[Boolean] =
    Form(
      mapping(
        "multipleDisposalsTaxYear" -> of(
          FormUtils.radioFormFormatter("multipleDisposalsTaxYear", List(true, false))
        )
      )(identity)(Some(_))
    )

  val assetTypeForNonUkResidentsForm: Form[List[AssetType]] = {
    val checkBoxAssetTypeFormFormatter: Formatter[AssetType] = new Formatter[AssetType] {

      override def bind(key: String, data: Map[String, String]): Either[Seq[FormError], AssetType] =
        readValue(key, data, identity)
          .flatMap {
            case "0" => Right(AssetType.Residential)
            case "1" => Right(AssetType.NonResidential)
            case "2" => Right(AssetType.MixedUse)
            case "3" => Right(AssetType.IndirectDisposal)
            case _   => Left(FormError(key, "error.invalid"))
          }
          .leftMap(Seq(_))

      override def unbind(key: String, value: AssetType): Map[String, String] =
        Map(key -> value.toString)
    }

    Form(
      mapping(
        "multipleDisposalsAssetTypeForNonUkResidents" -> Forms
          .list(of(checkBoxAssetTypeFormFormatter))
          .verifying("error.required", _.nonEmpty)
      )(identity)(Some(_))
    )
  }

  def completionDateForm(maximumDateInclusive: LocalDate): Form[CompletionDate] = Form(
    mapping(
      "" -> of(
        LocalDateUtils.dateFormatter(
          Some(maximumDateInclusive),
          None,
          "multipleDisposalsCompletionDate-day",
          "multipleDisposalsCompletionDate-month",
          "multipleDisposalsCompletionDate-year",
          "multipleDisposalsCompletionDate"
        )
      )
    )(CompletionDate(_))(d => Some(d.value))
  )

}
