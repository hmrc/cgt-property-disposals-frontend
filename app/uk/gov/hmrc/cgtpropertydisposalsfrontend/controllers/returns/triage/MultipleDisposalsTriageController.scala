/*
 * Copyright 2021 HM Revenue & Customs
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
import cats.syntax.order._
import cats.data.EitherT
import cats.instances.boolean._
import cats.instances.future._
import cats.instances.list._
import cats.instances.int._
import cats.syntax.either._
import com.google.inject.{Inject, Singleton}
import play.api.Configuration
import play.api.data.Forms.{mapping, of}
import play.api.data.format.Formatter
import play.api.data.{Form, FormError, Forms}
import play.api.mvc._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.{StartingToAmendToFillingOutReturnBehaviour, representee}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.triage.MultipleDisposalsTriageController._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.FormUtils.readValue
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, StartingNewDraftReturn, StartingToAmendReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Country
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.UUIDGenerator
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.AssetType.IndirectDisposal
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.MultipleDisposalsTriageAnswers._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.IncompleteSingleDisposalTriageAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.YearToDateLiabilityAnswers.{CalculatedYTDAnswers, NonCalculatedYTDAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.IndividualUserType.{PersonalRepresentative, PersonalRepresentativeInPeriodOfAdmin}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{BooleanFormatter, Error, FormUtils, SessionData, TaxYear, TimeUtils}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.{ReturnsService, TaxYearService}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging.LoggerOps
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views.html.returns.triage.{disposal_date_of_shares, multipledisposals => triagePages}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.triage.CommonTriageQuestionsController.sharesDisposalDateForm
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.TimeUtils.localDateOrder
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

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
  exchangedInDifferentTaxYearsPage: triagePages.exchanged_different_tax_years,
  assetTypeForNonUkResidentsPage: triagePages.asset_type_for_non_uk_residents,
  completionDatePage: triagePages.completion_date,
  disposalDateOfSharesForNonUk: disposal_date_of_shares,
  checkYourAnswersPage: triagePages.check_you_answers
)(implicit viewConfig: ViewConfig, ec: ExecutionContext)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with Logging
    with SessionUpdates
    with StartingToAmendToFillingOutReturnBehaviour {

  type JourneyState = Either[
    StartingNewDraftReturn,
    (FillingOutReturn, Either[DraftMultipleIndirectDisposalsReturn, DraftMultipleDisposalsReturn])
  ]

  def guidance(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withMultipleDisposalTriageAnswers { (_, state, answers) =>
        val backLink = answers.fold(
          _ => routes.CommonTriageQuestionsController.howManyProperties(),
          _ => routes.MultipleDisposalsTriageController.checkYourAnswers()
        )
        Ok(
          guidancePage(
            backLink,
            state.isRight,
            state.fold(
              _.subscribedDetails.isATrust,
              _._1.subscribedDetails.isATrust
            ),
            answers.representativeType()
          )
        )
      }

    }

  def guidanceSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withMultipleDisposalTriageAnswers { (_, _, answers) =>
        answers.fold(
          _ =>
            Redirect(
              routes.MultipleDisposalsTriageController.howManyDisposals()
            ),
          _ =>
            Redirect(
              routes.MultipleDisposalsTriageController.checkYourAnswers()
            )
        )
      }
    }

  def howManyDisposals(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withMultipleDisposalTriageAnswers { (_, state, answers) =>
        val numberOfDisposals =
          answers.fold(_.numberOfProperties, c => Some(c.numberOfProperties))
        val form              = numberOfDisposals.fold(numberOfPropertiesForm)(
          numberOfPropertiesForm.fill
        )
        val backLink          = answers.fold(
          _ => routes.MultipleDisposalsTriageController.guidance(),
          _ => routes.MultipleDisposalsTriageController.checkYourAnswers()
        )
        Ok(howManyPropertiesPage(form, backLink, state.isRight, isAmendReturn(state)))
      }
    }

  def howManyDisposalsSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withMultipleDisposalTriageAnswers { (_, state, answers) =>
        numberOfPropertiesForm
          .bindFromRequest()
          .fold(
            { formWithErrors =>
              val backLink = answers.fold(
                _ => routes.MultipleDisposalsTriageController.guidance(),
                _ => routes.MultipleDisposalsTriageController.checkYourAnswers()
              )
              BadRequest(
                howManyPropertiesPage(
                  formWithErrors,
                  backLink,
                  state.isRight,
                  isAmendReturn(state)
                )
              )
            },
            numberOfProperties =>
              if (
                answers
                  .fold(_.numberOfProperties, c => Some(c.numberOfProperties))
                  .contains(numberOfProperties)
              )
                Redirect(
                  routes.MultipleDisposalsTriageController.checkYourAnswers()
                )
              else {
                val (newState, redirectTo) =
                  updateNumberOfPropertiesWithRedirect(
                    numberOfProperties,
                    answers,
                    state
                  )
                updateStateAndThen(newState, Redirect(redirectTo))
              }
          )
      }
    }

  private def updateNumberOfPropertiesWithRedirect(
    numberOfProperties: Int,
    currentAnswers: MultipleDisposalsTriageAnswers,
    currentState: JourneyState
  ): (Either[StartingNewDraftReturn, FillingOutReturn], Call) = {
    val newAnswersWithRedirectTo =
      if (numberOfProperties > 1)
        Left[
          MultipleDisposalsTriageAnswers,
          IncompleteSingleDisposalTriageAnswers
        ](
          IncompleteMultipleDisposalsTriageAnswers.empty.copy(
            individualUserType = currentAnswers.fold(_.individualUserType, _.individualUserType),
            numberOfProperties = Some(numberOfProperties)
          )
        ) -> routes.MultipleDisposalsTriageController.checkYourAnswers()
      else
        Right[
          MultipleDisposalsTriageAnswers,
          IncompleteSingleDisposalTriageAnswers
        ](
          IncompleteSingleDisposalTriageAnswers.empty.copy(
            individualUserType = currentAnswers.fold(_.individualUserType, _.individualUserType),
            hasConfirmedSingleDisposal = true
          )
        ) -> routes.SingleDisposalsTriageController.checkYourAnswers()

    val newState                 = currentState.bimap(
      _.copy(newReturnTriageAnswers = newAnswersWithRedirectTo._1),
      {
        case (r, Right(d)) =>
          val newDraftReturn = newAnswersWithRedirectTo._1.bimap(
            DraftMultipleDisposalsReturn
              .newDraftReturn(d.id, _, d.representeeAnswers),
            DraftSingleDisposalReturn
              .newDraftReturn(d.id, _, d.representeeAnswers)
          )

          r.copy(draftReturn = newDraftReturn.merge).withForceDisplayGainOrLossAfterReliefsForAmends
        case (r, Left(d))  =>
          val newDraftReturn = newAnswersWithRedirectTo._1.bimap(
            DraftMultipleIndirectDisposalsReturn
              .newDraftReturn(d.id, _, d.representeeAnswers),
            DraftSingleIndirectDisposalReturn
              .newDraftReturn(d.id, _, d.representeeAnswers)
          )

          r.copy(draftReturn = newDraftReturn.merge).withForceDisplayGainOrLossAfterReliefsForAmends

      }
    )
    newState -> newAnswersWithRedirectTo._2
  }

  def wereYouAUKResident(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withMultipleDisposalTriageAnswers { (_, state, answers) =>
        val wereYouUKResident =
          answers.fold(_.wasAUKResident, c => Some(c.countryOfResidence.isUk()))
        val form              =
          wereYouUKResident.fold(wasAUkResidentForm)(wasAUkResidentForm.fill)
        val backLink          = answers.fold(
          _ => routes.MultipleDisposalsTriageController.howManyDisposals(),
          _ => routes.MultipleDisposalsTriageController.checkYourAnswers()
        )

        Ok(
          wereYouAUKResidentPage(
            form,
            backLink,
            state.isRight,
            state.fold(
              _.subscribedDetails.isATrust,
              _._1.subscribedDetails.isATrust
            ),
            answers.representativeType(),
            isAmendReturn(state)
          )
        )
      }
    }

  def wereYouAUKResidentSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withMultipleDisposalTriageAnswers { (_, state, answers) =>
        wasAUkResidentForm
          .bindFromRequest()
          .fold(
            { formWithErrors =>
              val backLink = answers.fold(
                _ => routes.MultipleDisposalsTriageController.howManyDisposals(),
                _ => routes.MultipleDisposalsTriageController.checkYourAnswers()
              )

              BadRequest(
                wereYouAUKResidentPage(
                  formWithErrors,
                  backLink,
                  state.isRight,
                  state.fold(
                    _.subscribedDetails.isATrust,
                    _._1.subscribedDetails.isATrust
                  ),
                  answers.representativeType(),
                  isAmendReturn(state)
                )
              )
            },
            wereUKResident =>
              if (
                answers
                  .fold(
                    _.wasAUKResident,
                    c => Some(c.countryOfResidence.isUk())
                  )
                  .contains(wereUKResident)
              )
                Redirect(
                  routes.MultipleDisposalsTriageController.checkYourAnswers()
                )
              else {
                val updatedAnswers = answers
                  .unset(_.countryOfResidence)
                  .unset(_.assetTypes)
                  .unset(_.wereAllPropertiesResidential)
                  .copy(wasAUKResident = Some(wereUKResident))

                val newState = updateState(
                  state,
                  updatedAnswers,
                  d =>
                    d.bimap(
                      multipleIndirect =>
                        multipleIndirect.copy(
                          exampleCompanyDetailsAnswers = multipleIndirect.exampleCompanyDetailsAnswers.map(
                            _.unset(_.address)
                              .unset(_.disposalPrice)
                              .unset(_.acquisitionPrice)
                          ),
                          yearToDateLiabilityAnswers = None,
                          supportingEvidenceAnswers = None,
                          gainOrLossAfterReliefs = None
                        ),
                      multiple =>
                        multiple.copy(
                          examplePropertyDetailsAnswers = multiple.examplePropertyDetailsAnswers.map(
                            _.unset(_.address)
                              .unset(_.disposalPrice)
                              .unset(_.acquisitionPrice)
                          ),
                          yearToDateLiabilityAnswers = None,
                          supportingEvidenceAnswers = None,
                          gainOrLossAfterReliefs = None
                        )
                    ),
                  forceDisplayGainOrLossAfterReliefsForAmends = true
                )
                updateStateAndThen(
                  newState,
                  Redirect(
                    routes.MultipleDisposalsTriageController.checkYourAnswers()
                  )
                )
              }
          )
      }
    }

  def wereAllPropertiesResidential(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withMultipleDisposalTriageAnswers { (_, state, answers) =>
        val werePropertiesResidential =
          answers.fold(
            _.wereAllPropertiesResidential,
            c => Some(c.assetTypes === List(AssetType.Residential))
          )
        val form                      =
          werePropertiesResidential.fold(wereAllPropertiesResidentialForm)(
            wereAllPropertiesResidentialForm.fill
          )
        val backLink                  = answers.fold(
          _ => routes.MultipleDisposalsTriageController.wereYouAUKResident(),
          _ => routes.MultipleDisposalsTriageController.checkYourAnswers()
        )
        Ok(wereAllPropertiesResidentialPage(form, backLink, state.isRight, state.fold(_ => false, _._1.isAmendReturn)))
      }
    }

  def wereAllPropertiesResidentialSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withMultipleDisposalTriageAnswers { (_, state, answers) =>
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
                  state.isRight,
                  state.fold(_ => false, _._1.isAmendReturn)
                )
              )
            },
            wereAllPropertiesResidential =>
              if (
                answers
                  .fold(
                    _.wereAllPropertiesResidential,
                    c => Some(isResidentialAssetType(c.assetTypes))
                  )
                  .contains(wereAllPropertiesResidential)
              )
                Redirect(
                  routes.MultipleDisposalsTriageController.checkYourAnswers()
                )
              else {
                val updatedAnswers =
                  answers
                    .fold(
                      identity,
                      IncompleteMultipleDisposalsTriageAnswers.fromCompleteAnswers
                    )
                    .copy(
                      wereAllPropertiesResidential = Some(wereAllPropertiesResidential),
                      assetTypes = Some(assetType(wereAllPropertiesResidential))
                    )

                val newState =
                  updateState(
                    state,
                    updatedAnswers,
                    _.leftFlatMap(indirect =>
                      Right(
                        DraftMultipleDisposalsReturn
                          .newDraftReturn(indirect.id, updatedAnswers, indirect.representeeAnswers)
                      )
                    ),
                    forceDisplayGainOrLossAfterReliefsForAmends = false
                  )
                updateStateAndThen(
                  newState,
                  Redirect(
                    routes.MultipleDisposalsTriageController.checkYourAnswers()
                  )
                )
              }
          )
      }
    }

  def countryOfResidence(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withMultipleDisposalTriageAnswers { (_, state, answers) =>
        val wasUkResident =
          answers.fold(_.wasAUKResident, c => Some(c.countryOfResidence.isUk()))

        if (!wasUkResident.contains(false))
          Redirect(routes.MultipleDisposalsTriageController.checkYourAnswers())
        else {
          val countryOfResidence =
            answers.fold(_.countryOfResidence, c => Some(c.countryOfResidence))
          val form               =
            countryOfResidence.fold(countryOfResidenceForm)(
              countryOfResidenceForm.fill
            )
          val backLink           = answers.fold(
            _ => routes.MultipleDisposalsTriageController.wereYouAUKResident(),
            _ => routes.MultipleDisposalsTriageController.checkYourAnswers()
          )
          Ok(
            countryOfResidencePage(
              form,
              backLink,
              state.isRight,
              state.fold(
                _.subscribedDetails.isATrust,
                _._1.subscribedDetails.isATrust
              ),
              answers.representativeType(),
              state.fold(_.representeeAnswers, _._2.fold(_.representeeAnswers, _.representeeAnswers)),
              isAmendReturn(state)
            )
          )
        }
      }
    }

  def countryOfResidenceSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withMultipleDisposalTriageAnswers { (_, state, answers) =>
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
                  state.isRight,
                  state.fold(
                    _.subscribedDetails.isATrust,
                    _._1.subscribedDetails.isATrust
                  ),
                  answers.representativeType(),
                  state.fold(_.representeeAnswers, _._2.fold(_.representeeAnswers, _.representeeAnswers)),
                  isAmendReturn(state)
                )
              )
            },
            countryOfResidence =>
              if (
                answers
                  .fold(_.countryOfResidence, c => Some(c.countryOfResidence))
                  .contains(countryOfResidence)
              )
                Redirect(
                  routes.MultipleDisposalsTriageController.checkYourAnswers()
                )
              else {
                val updatedAnswers =
                  answers.fold[MultipleDisposalsTriageAnswers](
                    _.copy(countryOfResidence = Some(countryOfResidence)),
                    _.copy(countryOfResidence = countryOfResidence)
                  )

                val newState = updateState(
                  state,
                  updatedAnswers,
                  d =>
                    d.bimap(
                      multipleIndirect => multipleIndirect.copy(yearToDateLiabilityAnswers = None),
                      multiple => multiple.copy(yearToDateLiabilityAnswers = None)
                    ),
                  forceDisplayGainOrLossAfterReliefsForAmends = false
                )
                updateStateAndThen(
                  newState,
                  Redirect(
                    routes.MultipleDisposalsTriageController.checkYourAnswers()
                  )
                )
              }
          )
      }
    }

  def whenWereContractsExchanged(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withMultipleDisposalTriageAnswers { (_, state, answers) =>
        if (answers.isIndirectDisposal())
          Redirect(routes.MultipleDisposalsTriageController.checkYourAnswers())
        else {
          val taxYearExchanged =
            answers.fold(_.taxYearExchanged, c => Some(c.taxYearExchanged))

          val taxYearOfDateOfDeath = getDateOfDeath(state) match {
            case Some(date) => TimeUtils.getTaxYearExchangedOfADate(date.value)
            case _          => TaxYearExchanged.TaxYearBefore2020
          }

          val representativeType: Option[RepresentativeType] =
            state.fold(
              _.newReturnTriageAnswers.fold(_.representativeType(), _.representativeType()),
              _._1.draftReturn.representativeType()
            )

          val form     = taxYearExchanged.fold(taxYearExchangedForm(taxYearOfDateOfDeath, representativeType))(
            taxYearExchangedForm(taxYearOfDateOfDeath, representativeType).fill
          )
          val backLink = answers.fold(
            i => incompleteJourneyTaxYearBackLink(i.wasAUKResident.contains(true)),
            _ => routes.MultipleDisposalsTriageController.checkYourAnswers()
          )
          val taxYears =
            for {
              taxYears <- taxYearService.availableTaxYears()
            } yield taxYears

          taxYears.fold(
            { e =>
              logger.warn("Could not find available tax years", e)
              errorHandler.errorResult()
            },
            availableTaxYears =>
              Ok(
                taxYearExchangedPage(
                  form,
                  backLink,
                  state.isRight,
                  state.fold(_ => false, _._1.isAmendReturn),
                  availableTaxYears
                )
              )
          )
        }
      }
    }

  def whenWereContractsExchangedSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withMultipleDisposalTriageAnswers { (_, state, answers) =>
        if (answers.isIndirectDisposal())
          Redirect(routes.MultipleDisposalsTriageController.checkYourAnswers())
        else {
          val taxYears =
            for {
              taxYears <- taxYearService.availableTaxYears()
            } yield taxYears

          val taxYearOfDateOfDeath = getDateOfDeath(state) match {
            case Some(date) => TimeUtils.getTaxYearExchangedOfADate(date.value)
            case _          => TaxYearExchanged.TaxYearBefore2020
          }

          val representativeType: Option[RepresentativeType] =
            state.fold(
              _.newReturnTriageAnswers.fold(_.representativeType(), _.representativeType()),
              _._1.draftReturn.representativeType()
            )

          taxYearExchangedForm(taxYearOfDateOfDeath, representativeType)
            .bindFromRequest()
            .fold(
              { formWithErrors =>
                val backLink = answers.fold(
                  i =>
                    incompleteJourneyTaxYearBackLink(
                      i.wasAUKResident.contains(true)
                    ),
                  _ => routes.MultipleDisposalsTriageController.checkYourAnswers()
                )

                taxYears.fold(
                  { e =>
                    logger.warn("Could not find available tax years", e)
                    errorHandler.errorResult()
                  },
                  availableTaxYears =>
                    BadRequest(
                      taxYearExchangedPage(
                        formWithErrors,
                        backLink,
                        state.isRight,
                        state.fold(_ => false, _._1.isAmendReturn),
                        availableTaxYears
                      )
                    )
                )
              },
              taxYearExchanged =>
                if (
                  answers
                    .fold(_.taxYearExchanged, c => Some(c.taxYearExchanged))
                    .contains(taxYearExchanged)
                )
                  Redirect(
                    routes.MultipleDisposalsTriageController.checkYourAnswers()
                  )
                else {
                  val result =
                    for {
                      taxYear <- taxYearExchanged match {
                                   case TaxYearExchanged.TaxYear2021 =>
                                     taxYearService.taxYear(TimeUtils.getTaxYearStartDate(2021))
                                   case TaxYearExchanged.TaxYear2020 =>
                                     taxYearService.taxYear(TimeUtils.getTaxYearStartDate(2020))
                                   case _                            => EitherT.pure[Future, Error](None)
                                 }

                      updatedAnswers <- EitherT.fromEither[Future](
                                          updateTaxYearToAnswers(
                                            taxYearExchanged,
                                            taxYear,
                                            answers
                                          )
                                        )
                      newState        = updateState(
                                          state,
                                          updatedAnswers,
                                          d =>
                                            d.bimap(
                                              mi => mi.copy(exampleCompanyDetailsAnswers = mi.exampleCompanyDetailsAnswers),
                                              m =>
                                                m.copy(examplePropertyDetailsAnswers =
                                                  m.examplePropertyDetailsAnswers
                                                    .map(_.unset(_.disposalDate))
                                                )
                                            ),
                                          forceDisplayGainOrLossAfterReliefsForAmends = true
                                        )
                      _              <- newState.fold(
                                          _ => EitherT.pure[Future, Error](()),
                                          returnsService.storeDraftReturn(_)
                                        )
                      _              <- EitherT(
                                          updateSession(sessionStore, request)(
                                            _.copy(journeyStatus = Some(newState.merge))
                                          )
                                        )
                    } yield ()

                  result.fold(
                    { e =>
                      logger.warn("Could not find tax year or update session", e)
                      errorHandler.errorResult()
                    },
                    _ =>
                      Redirect(
                        routes.MultipleDisposalsTriageController
                          .checkYourAnswers()
                      )
                  )

                }
            )
        }
      }
    }

  def exchangedInDifferentTaxYears(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      Ok(
        exchangedInDifferentTaxYearsPage(
          routes.MultipleDisposalsTriageController.whenWereContractsExchanged()
        )
      )
    }

  def assetTypeForNonUkResidents(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withMultipleDisposalTriageAnswers { (_, state, answers) =>
        val assetType = answers.fold(_.assetTypes, c => Some(c.assetTypes))
        val form      = assetType.fold(assetTypeForNonUkResidentsForm)(
          assetTypeForNonUkResidentsForm.fill
        )
        val backLink  = answers.fold(
          _ => routes.MultipleDisposalsTriageController.countryOfResidence(),
          _ => routes.MultipleDisposalsTriageController.checkYourAnswers()
        )
        Ok(
          assetTypeForNonUkResidentsPage(
            form,
            backLink,
            state.isRight,
            state.fold(
              _.subscribedDetails.isATrust,
              _._1.subscribedDetails.isATrust
            ),
            answers.representativeType(),
            isAmendReturn(state)
          )
        )
      }
    }

  def assetTypeForNonUkResidentsSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withMultipleDisposalTriageAnswers { (_, state, answers) =>
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
                  state.isRight,
                  state.fold(
                    _.subscribedDetails.isATrust,
                    _._1.subscribedDetails.isATrust
                  ),
                  answers.representativeType(),
                  isAmendReturn(state)
                )
              )
            },
            assetTypes =>
              if (
                answers
                  .fold(_.assetTypes, c => Some(c.assetTypes))
                  .contains(assetTypes)
              )
                Redirect(
                  routes.MultipleDisposalsTriageController.checkYourAnswers()
                )
              else {
                val oldAssetTypes: Option[List[AssetType]] = answers.fold(_.assetTypes, c => Some(c.assetTypes))
                val wasIndirectDisposal                    = oldAssetTypes.contains(List(IndirectDisposal))
                val isNowIndirectDisposal                  = assetTypes === List(IndirectDisposal)

                val newAnswers =
                  if (!wasIndirectDisposal === !isNowIndirectDisposal)
                    answers.unset(_.assetTypes).copy(assetTypes = Some(assetTypes))
                  else
                    answers.unset(_.completionDate).copy(assetTypes = Some(assetTypes))

                val newState = updateState(
                  state,
                  newAnswers,
                  {
                    case Left(indirectDisposalsReturn) =>
                      Right(
                        DraftMultipleDisposalsReturn
                          .newDraftReturn(
                            indirectDisposalsReturn.id,
                            newAnswers,
                            indirectDisposalsReturn.representeeAnswers
                          )
                      )
                    case Right(draftReturn)            =>
                      if (isNowIndirectDisposal)
                        Left(
                          DraftMultipleIndirectDisposalsReturn
                            .newDraftReturn(
                              draftReturn.id,
                              newAnswers,
                              draftReturn.representeeAnswers
                            )
                        )
                      else
                        Right(
                          draftReturn.copy(
                            triageAnswers = newAnswers,
                            examplePropertyDetailsAnswers = None,
                            yearToDateLiabilityAnswers =
                              if (state.fold(_.isFurtherReturn, _._1.isFurtherReturn).contains(true))
                                draftReturn.yearToDateLiabilityAnswers.map {
                                  case answers: CalculatedYTDAnswers    => answers.unset(_.hasEstimatedDetails)
                                  case answers: NonCalculatedYTDAnswers => answers.unset(_.hasEstimatedDetails)
                                }
                              else None,
                            supportingEvidenceAnswers = None
                          )
                        )
                  },
                  forceDisplayGainOrLossAfterReliefsForAmends = true
                )

                updateStateAndThen(
                  newState,
                  Redirect(
                    routes.MultipleDisposalsTriageController.checkYourAnswers()
                  )
                )
              }
          )
      }
    }

  def completionDate(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withMultipleDisposalTriageAnswers { (_, state, answers) =>
        val completionDate =
          answers.fold(_.completionDate, c => Some(c.completionDate))
        val maxDateAllowed = TimeUtils.getMaximumDateForDisposalsAndCompletion(
          viewConfig.enableFutureDateForDisposalAndCompletion,
          viewConfig.maxYearForDisposalsAndCompletion
        )

        val dateOfDeath                            = getDateOfDeath(state)
        val (dateOfDeathValue, isDateOfDeathValid) = dateOfDeath match {
          case Some(d) => (d.value, Some(true))
          case _       => (TaxYear.earliestTaxYearStartDate, Some(false))
        }
        val form                                   = completionDate.fold(completionDateForm(maxDateAllowed, dateOfDeathValue, isDateOfDeathValid))(
          completionDateForm(maxDateAllowed, dateOfDeathValue, isDateOfDeathValid).fill
        )
        val backLink                               = answers.fold(
          _ => routes.CommonTriageQuestionsController.haveYouAlreadySentSelfAssessment(),
          _ => routes.MultipleDisposalsTriageController.checkYourAnswers()
        )
        Ok(completionDatePage(form, backLink, state.isRight, state.fold(_ => false, _._1.isAmendReturn)))
      }
    }

  def completionDateSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withMultipleDisposalTriageAnswers { (_, state, answers) =>
        val maxDateAllowed = TimeUtils.getMaximumDateForDisposalsAndCompletion(
          viewConfig.enableFutureDateForDisposalAndCompletion,
          viewConfig.maxYearForDisposalsAndCompletion
        )

        val taxYearExchangedSelected: Option[TaxYearExchanged] =
          answers.fold(_.taxYearExchanged, c => Some(c.taxYearExchanged))

        val taxYearAtStart: Option[Int] =
          getTaxYearByTaxYearExchanged(taxYearExchangedSelected.getOrElse(TaxYearExchanged.DifferentTaxYears))

        val dateOfDeath                            = getDateOfDeath(state)
        val (dateOfDeathValue, isDateOfDeathValid) = dateOfDeath match {
          case Some(d) => (d.value, Some(true))
          case _       => (TaxYear.earliestTaxYearStartDate, Some(false))
        }

        completionDateForm(maxDateAllowed, dateOfDeathValue, isDateOfDeathValid, taxYearAtStart)
          .bindFromRequest()
          .fold(
            { formWithErrors =>
              val backLink = answers.fold(
                _ => routes.CommonTriageQuestionsController.haveYouAlreadySentSelfAssessment(),
                _ => routes.MultipleDisposalsTriageController.checkYourAnswers()
              )

              BadRequest(
                completionDatePage(
                  formWithErrors,
                  backLink,
                  state.isRight,
                  state.fold(_ => false, _._1.isAmendReturn)
                )
              )
            },
            completionDate =>
              if (
                answers
                  .fold(_.completionDate, c => Some(c.completionDate))
                  .contains(completionDate)
              )
                Redirect(
                  routes.MultipleDisposalsTriageController.checkYourAnswers()
                )
              else {
                val updatedAnswers =
                  answers.unset(_.completionDate).copy(completionDate = Some(completionDate))

                val newState = updateState(
                  state,
                  updatedAnswers,
                  d =>
                    d.bimap(
                      multipleIndirect =>
                        multipleIndirect.copy(
                          exampleCompanyDetailsAnswers = multipleIndirect.exampleCompanyDetailsAnswers.map(
                            _.unset(_.acquisitionPrice)
                          ),
                          yearToDateLiabilityAnswers = None,
                          gainOrLossAfterReliefs = None
                        ),
                      multiple =>
                        multiple.copy(
                          examplePropertyDetailsAnswers = multiple.examplePropertyDetailsAnswers.map(
                            _.unset(_.disposalDate).unset(_.acquisitionPrice)
                          ),
                          yearToDateLiabilityAnswers = None,
                          gainOrLossAfterReliefs = None
                        )
                    ),
                  forceDisplayGainOrLossAfterReliefsForAmends = true
                )
                updateStateAndThen(
                  newState,
                  Redirect(
                    routes.MultipleDisposalsTriageController.checkYourAnswers()
                  )
                )

              }
          )
      }
    }

  private def getTaxYearByTaxYearExchanged(taxYearExhanged: TaxYearExchanged): Option[Int] =
    taxYearExhanged match {
      case TaxYearExchanged.TaxYear2020 => Some(2020)
      case TaxYearExchanged.TaxYear2021 => Some(2021)
      case _                            => None
    }

  private def getDateOfDeath(state: JourneyState): Option[DateOfDeath] =
    state
      .fold(
        _.representeeAnswers,
        _._2.fold(_.representeeAnswers, _.representeeAnswers)
      )
      .flatMap(_.fold(_.dateOfDeath, _.dateOfDeath))

  private def updateTaxYearToAnswers(
    taxYearExchanged: TaxYearExchanged,
    taxYear: Option[TaxYear],
    answers: MultipleDisposalsTriageAnswers
  ): Either[Error, MultipleDisposalsTriageAnswers] =
    taxYear match {
      case None if isAValidCGTTaxTear(taxYearExchanged) =>
        Left(Error("Could not find tax year"))
      case _                                            =>
        Right(
          answers
            .unset(_.alreadySentSelfAssessment)
            .unset(_.completionDate)
            .copy(
              taxYearExchanged = Some(taxYearExchanged),
              taxYear = taxYear
            )
        )
    }

  def disposalDateOfShares(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withMultipleDisposalTriageAnswers { (_, state, answers) =>
        withPersonalRepresentativeDetails(state) { personalRepDetails =>
          val backLink       = answers.fold(
            _ =>
              routes.MultipleDisposalsTriageController
                .assetTypeForNonUkResidents(),
            _ => routes.MultipleDisposalsTriageController.checkYourAnswers()
          )
          val maxDateAllowed = TimeUtils.getMaximumDateForDisposalsAndCompletion(
            viewConfig.enableFutureDateForDisposalAndCompletion,
            viewConfig.maxYearForDisposalsAndCompletion
          )
          val form = {
            val blankForm = sharesDisposalDateForm(personalRepDetails, maxDateAllowed)
            answers
              .fold(_.completionDate, e => Some(e.completionDate))
              .fold(blankForm)(c => blankForm.fill(ShareDisposalDate(c.value)))
          }
          Ok(
            disposalDateOfSharesForNonUk(
              form,
              backLink,
              state.isRight,
              routes.MultipleDisposalsTriageController
                .disposalDateOfSharesSubmit(),
              isAmendReturn(state)
            )
          )
        }
      }
    }

  def disposalDateOfSharesSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withMultipleDisposalTriageAnswers { (_, state, answers) =>
        withPersonalRepresentativeDetails(state) { personalRepDetails =>
          val maxDateAllowed = TimeUtils.getMaximumDateForDisposalsAndCompletion(
            viewConfig.enableFutureDateForDisposalAndCompletion,
            viewConfig.maxYearForDisposalsAndCompletion
          )
          sharesDisposalDateForm(personalRepDetails, maxDateAllowed)
            .bindFromRequest()
            .fold(
              { formWithErrors =>
                val backLink = answers.fold(
                  _ =>
                    routes.MultipleDisposalsTriageController
                      .assetTypeForNonUkResidents(),
                  _ => routes.MultipleDisposalsTriageController.checkYourAnswers()
                )
                BadRequest(
                  disposalDateOfSharesForNonUk(
                    formWithErrors,
                    backLink,
                    state.isRight,
                    routes.MultipleDisposalsTriageController.disposalDateOfSharesSubmit(),
                    isAmendReturn(state)
                  )
                )
              },
              { shareDisposalDate =>
                val existingDisposalDate = answers.fold(_.completionDate, c => Some(c.completionDate))

                existingDisposalDate match {
                  case Some(existingDate) if existingDate.value === shareDisposalDate.value =>
                    Redirect(routes.MultipleDisposalsTriageController.checkYourAnswers())

                  case _ =>
                    val result = for {
                      taxYear        <- taxYearService.taxYear(shareDisposalDate.value)
                      updatedAnswers <- EitherT
                                          .fromEither[Future](
                                            Right(
                                              answers
                                                .unset(_.completionDate)
                                                .copy(
                                                  taxYear = taxYear,
                                                  completionDate = Some(CompletionDate(shareDisposalDate.value)),
                                                  taxYearExchanged = getTaxYearExchanged(taxYear)
                                                )
                                            )
                                          )
                      newState        = updateState(
                                          state,
                                          updatedAnswers,
                                          d =>
                                            d.bimap(
                                              multipleIndirect =>
                                                multipleIndirect.copy(
                                                  exampleCompanyDetailsAnswers = multipleIndirect.exampleCompanyDetailsAnswers,
                                                  yearToDateLiabilityAnswers = None,
                                                  gainOrLossAfterReliefs = None
                                                ),
                                              multiple =>
                                                multiple.copy(
                                                  examplePropertyDetailsAnswers = multiple.examplePropertyDetailsAnswers
                                                    .map(_.unset(_.disposalDate)),
                                                  yearToDateLiabilityAnswers = None,
                                                  gainOrLossAfterReliefs = None
                                                )
                                            ),
                                          forceDisplayGainOrLossAfterReliefsForAmends = true
                                        )
                      _              <- newState.fold(
                                          _ => EitherT.pure[Future, Error](()),
                                          returnsService.storeDraftReturn(_)
                                        )
                      _              <- EitherT(
                                          updateSession(sessionStore, request)(
                                            _.copy(journeyStatus = Some(newState.merge))
                                          )
                                        )
                    } yield taxYear

                    result.fold(
                      { e =>
                        logger.warn("Could not find tax year or update session", e)
                        errorHandler.errorResult()
                      },
                      taxYear => {
                        val amendReturnOriginalTaxYear =
                          state.map(_._1.amendReturnData.map(_.originalReturn.completeReturn.taxYear)).toOption.flatten
                        taxYear match {
                          case None if isAmendReturn(state) =>
                            Redirect(
                              routes.CommonTriageQuestionsController.amendReturnDisposalDateDifferentTaxYear()
                            )
                          case Some(t)
                              if amendReturnOriginalTaxYear
                                .map(_.startDateInclusive)
                                .exists(_ =!= t.startDateInclusive) =>
                            Redirect(routes.CommonTriageQuestionsController.amendReturnDisposalDateDifferentTaxYear())
                          case _                            => Redirect(routes.MultipleDisposalsTriageController.checkYourAnswers())
                        }

                      }
                    )
                }
              }
            )
        }
      }
    }

  private def getTaxYearExchanged(taxYear: Option[TaxYear]): Option[TaxYearExchanged] =
    taxYear match {
      case Some(t) if t.startDateInclusive.getYear === 2020 => Some(TaxYearExchanged.TaxYear2020)
      case Some(t) if t.startDateInclusive.getYear === 2021 => Some(TaxYearExchanged.TaxYear2021)
      case _                                                => None
    }

  private def isAmendReturn(state: JourneyState): Boolean =
    state.fold(_ => false, _._1.isAmendReturn)

  def checkYourAnswers(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withMultipleDisposalTriageAnswers { (_, state, triageAnswers) =>
        val isIndividual = state
          .fold(_.subscribedDetails, _._1.subscribedDetails)
          .userType()
          .isRight

        val representeeAnswers: Option[RepresenteeAnswers] = state
          .fold(
            _.representeeAnswers,
            fb => fb._2.fold(_.representeeAnswers, _.representeeAnswers)
          )

        val representeeAnswersIncomplete = !representeeAnswers
          .map(_.fold(_ => false, _ => true))
          .getOrElse(false)

        val originalSubmissionYear: Option[String] =
          state.toOption.flatMap(_._1.amendReturnData.map(_.originalReturn.summary.taxYear))

        triageAnswers match {
          case IncompleteMultipleDisposalsTriageAnswers(
                None,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _
              ) if isIndividual =>
            Redirect(
              routes.CommonTriageQuestionsController
                .whoIsIndividualRepresenting()
            )

          case IncompleteMultipleDisposalsTriageAnswers(
                Some(_: RepresentativeType),
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _
              ) if representeeAnswersIncomplete =>
            Redirect(
              representee.routes.RepresenteeController
                .checkYourAnswers()
            )

          case IncompleteMultipleDisposalsTriageAnswers(_, None, _, _, _, _, _, _, _, _) =>
            Redirect(routes.MultipleDisposalsTriageController.guidance())

          case IncompleteMultipleDisposalsTriageAnswers(
                _,
                _,
                None,
                _,
                _,
                _,
                _,
                _,
                _,
                _
              ) =>
            Redirect(
              routes.MultipleDisposalsTriageController.wereYouAUKResident()
            )

          case IncompleteMultipleDisposalsTriageAnswers(
                _,
                _,
                Some(false),
                None,
                _,
                _,
                _,
                _,
                _,
                _
              ) =>
            Redirect(
              routes.MultipleDisposalsTriageController.countryOfResidence()
            )

          case IncompleteMultipleDisposalsTriageAnswers(
                _,
                _,
                Some(false),
                _,
                _,
                None,
                _,
                _,
                _,
                _
              ) =>
            Redirect(
              routes.MultipleDisposalsTriageController
                .assetTypeForNonUkResidents()
            )
          case IncompleteMultipleDisposalsTriageAnswers(
                _,
                _,
                _,
                _,
                _,
                _,
                Some(taxYearExchanged),
                _,
                _,
                _
              )
              if isAmendReturn(state) && !isTaxYearWithinOriginalSubmissionTaxYear(
                taxYearExchanged,
                originalSubmissionYear
              ) =>
            Redirect(
              routes.CommonTriageQuestionsController.exchangedYearIncompatibleWithTaxYear()
            )

          case IncompleteMultipleDisposalsTriageAnswers(
                _,
                _,
                Some(true),
                _,
                None,
                _,
                _,
                _,
                _,
                _
              ) =>
            Redirect(
              routes.MultipleDisposalsTriageController
                .wereAllPropertiesResidential()
            )

          case IncompleteMultipleDisposalsTriageAnswers(
                _,
                _,
                Some(true),
                _,
                Some(false),
                _,
                _,
                _,
                _,
                _
              ) =>
            Redirect(
              routes.CommonTriageQuestionsController
                .ukResidentCanOnlyDisposeResidential()
            )

          case IncompleteMultipleDisposalsTriageAnswers(
                _,
                _,
                Some(false),
                _,
                _,
                Some(assetTypes),
                _,
                _,
                _,
                None
              )
              if assetTypes === List(
                IndirectDisposal
              ) =>
            Redirect(
              routes.MultipleDisposalsTriageController.disposalDateOfShares()
            )

          case IncompleteMultipleDisposalsTriageAnswers(
                _,
                _,
                _,
                _,
                _,
                _,
                None,
                _,
                _,
                _
              ) =>
            Redirect(
              routes.MultipleDisposalsTriageController
                .whenWereContractsExchanged()
            )

          case IncompleteMultipleDisposalsTriageAnswers(
                _,
                _,
                _,
                _,
                _,
                assetTypes,
                Some(taxYearExchanged),
                _,
                _,
                _
              ) if !isAValidCGTTaxTear(taxYearExchanged) =>
            val redirectPage =
              if (isAmendReturn(state))
                routes.CommonTriageQuestionsController.amendReturnDisposalDateDifferentTaxYear()
              else if (assetTypes.contains(List(IndirectDisposal)))
                routes.CommonTriageQuestionsController.disposalsOfSharesTooEarly()
              else if (taxYearExchanged === TaxYearExchanged.DifferentTaxYears)
                routes.MultipleDisposalsTriageController.exchangedInDifferentTaxYears()
              else
                routes.CommonTriageQuestionsController.disposalDateTooEarly()

            Redirect(redirectPage)

          case IncompleteMultipleDisposalsTriageAnswers(
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                Some(taxYear),
                None,
                None
              ) =>
            if (taxYear.isItInLatestTaxYear())
              Redirect(
                routes.MultipleDisposalsTriageController.completionDate()
              )
            else
              Redirect(
                routes.CommonTriageQuestionsController.haveYouAlreadySentSelfAssessment()
              )

          case IncompleteMultipleDisposalsTriageAnswers(
                _,
                _,
                _,
                _,
                _,
                _,
                Some(taxYearExchanged),
                None,
                _,
                _
              ) if isAValidCGTTaxTear(taxYearExchanged) =>
            logger.warn("No tax year was found when we expected one")
            errorHandler.errorResult()

          case IncompleteMultipleDisposalsTriageAnswers(
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                None
              ) =>
            Redirect(routes.MultipleDisposalsTriageController.completionDate())

          case IncompleteMultipleDisposalsTriageAnswers(
                individualUserType,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                Some(completionDate)
              ) if hasPreviousReturnWithSameCompletionDate(completionDate, individualUserType, state) =>
            Redirect(routes.CommonTriageQuestionsController.previousReturnExistsWithSameCompletionDate())

          case IncompleteMultipleDisposalsTriageAnswers(
                i,
                Some(n),
                Some(true),
                _,
                Some(true),
                Some(a),
                Some(taxYearExchanged),
                Some(t),
                sa,
                Some(d)
              ) =>
            val completeAnswers =
              CompleteMultipleDisposalsTriageAnswers(i, n, Country.uk, a, taxYearExchanged, t, sa, d)
            updateStateAndThen(
              updateState(state, completeAnswers, identity, forceDisplayGainOrLossAfterReliefsForAmends = true),
              Ok(
                checkYourAnswersPage(
                  completeAnswers,
                  state.isRight,
                  state.fold(
                    _.subscribedDetails.isATrust,
                    _._1.subscribedDetails.isATrust
                  ),
                  representeeAnswers
                )
              )
            )

          case IncompleteMultipleDisposalsTriageAnswers(
                i,
                Some(n),
                Some(false),
                Some(c),
                _,
                Some(a),
                Some(taxYearExchanged),
                Some(t),
                sa,
                Some(d)
              ) =>
            val completeAnswers =
              CompleteMultipleDisposalsTriageAnswers(i, n, c, a, taxYearExchanged, t, sa, d)
            updateStateAndThen(
              updateState(state, completeAnswers, identity, forceDisplayGainOrLossAfterReliefsForAmends = false),
              Ok(
                checkYourAnswersPage(
                  completeAnswers,
                  state.isRight,
                  state.fold(
                    _.subscribedDetails.isATrust,
                    _._1.subscribedDetails.isATrust
                  ),
                  representeeAnswers
                )
              )
            )

          case c: CompleteMultipleDisposalsTriageAnswers =>
            Ok(
              checkYourAnswersPage(
                c,
                state.isRight,
                state.fold(
                  _.subscribedDetails.isATrust,
                  _._1.subscribedDetails.isATrust
                ),
                representeeAnswers
              )
            )
        }
      }
    }

  def isAValidCGTTaxTear(taxYearExchanged: TaxYearExchanged): Boolean =
    !(taxYearExchanged === TaxYearExchanged.TaxYearBefore2020 || taxYearExchanged === TaxYearExchanged.DifferentTaxYears)

  def isTaxYearWithinOriginalSubmissionTaxYear(
    taxYearExchanged: TaxYearExchanged,
    originalSubmissionYear: Option[String]
  ): Boolean =
    getTaxYearByTaxYearExchanged(taxYearExchanged) match {
      case Some(tyExchanged) => originalSubmissionYear.exists(y => tyExchanged === y.toInt)
      case _                 => false
    }

  def checkYourAnswersSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withMultipleDisposalTriageAnswers { (_, journey, answers) =>
        journey match {
          case Right(_) =>
            Redirect(controllers.returns.routes.TaskListController.taskList())

          case Left(startingNewDraftReturn) =>
            answers match {
              case _: IncompleteMultipleDisposalsTriageAnswers =>
                Redirect(
                  routes.MultipleDisposalsTriageController.checkYourAnswers()
                )

              case complete: CompleteMultipleDisposalsTriageAnswers =>
                val newDraftReturn =
                  if (complete.assetTypes === List(IndirectDisposal))
                    DraftMultipleIndirectDisposalsReturn.newDraftReturn(
                      uuidGenerator.nextId(),
                      complete,
                      startingNewDraftReturn.representeeAnswers
                    )
                  else
                    DraftMultipleDisposalsReturn.newDraftReturn(
                      uuidGenerator.nextId(),
                      complete,
                      startingNewDraftReturn.representeeAnswers
                    )

                val newJourney = FillingOutReturn(
                  startingNewDraftReturn.subscribedDetails,
                  startingNewDraftReturn.ggCredId,
                  startingNewDraftReturn.agentReferenceNumber,
                  newDraftReturn,
                  startingNewDraftReturn.previousSentReturns,
                  None
                )

                updateStateAndThen(
                  Right(newJourney),
                  Redirect(
                    controllers.returns.routes.TaskListController.taskList()
                  )
                )
            }
        }

      }

    }

  private def incompleteJourneyTaxYearBackLink(wasAUKResident: Boolean): Call =
    if (wasAUKResident)
      routes.MultipleDisposalsTriageController.wereAllPropertiesResidential()
    else routes.MultipleDisposalsTriageController.assetTypeForNonUkResidents()

  private def isResidentialAssetType(assetType: List[AssetType]): Boolean =
    assetType match {
      case AssetType.Residential :: Nil => true
      case _                            => false
    }

  private def assetType(isResidential: Boolean): List[AssetType] =
    if (isResidential) List(AssetType.Residential)
    else List(AssetType.NonResidential)

  private def withMultipleDisposalTriageAnswers(
    f: (SessionData, JourneyState, MultipleDisposalsTriageAnswers) => Future[Result]
  )(implicit request: RequestWithSessionData[_]): Future[Result] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some((_, s: StartingToAmendReturn)) =>
        convertFromStartingAmendToFillingOutReturn(s, sessionStore, errorHandler, uuidGenerator)

      case Some((session, s @ StartingNewDraftReturn(_, _, _, Left(t), _, _))) =>
        f(session, Left(s), t)

      case Some(
            (
              session,
              r @ FillingOutReturn(_, _, _, m: DraftMultipleDisposalsReturn, _, _)
            )
          ) =>
        f(session, Right(r -> Right(m)), m.triageAnswers)

      case Some(
            (
              session,
              r @ FillingOutReturn(_, _, _, mi: DraftMultipleIndirectDisposalsReturn, _, _)
            )
          ) =>
        f(session, Right(r -> Left(mi)), mi.triageAnswers)

      case _ =>
        Redirect(
          uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.routes.StartController
            .start()
        )
    }

  private def updateStateAndThen(
    updatedState: Either[StartingNewDraftReturn, FillingOutReturn],
    f: => Result
  )(implicit
    hc: HeaderCarrier,
    request: RequestWithSessionData[_]
  ): Future[Result] = {
    val result = for {
      _ <- updatedState.fold(
             _ => EitherT.pure[Future, Error](()),
             returnsService
               .storeDraftReturn(_)
           )
      _ <- EitherT(
             updateSession(sessionStore, request)(
               _.copy(journeyStatus = Some(updatedState.merge))
             )
           )
    } yield ()

    result.fold(
      { e =>
        logger.warn("Could not update session", e)
        errorHandler.errorResult()
      },
      _ => f
    )
  }

  private def updateState(
    currentState: JourneyState,
    newAnswers: MultipleDisposalsTriageAnswers,
    modifyDraftReturn: Either[DraftMultipleIndirectDisposalsReturn, DraftMultipleDisposalsReturn] => Either[
      DraftMultipleIndirectDisposalsReturn,
      DraftMultipleDisposalsReturn
    ],
    forceDisplayGainOrLossAfterReliefsForAmends: Boolean
  ): Either[StartingNewDraftReturn, FillingOutReturn] =
    currentState.bimap(
      _.copy(newReturnTriageAnswers = Left(newAnswers)),
      { case (r, d) =>
        val newFillingOutReturn = r.copy(draftReturn =
          modifyDraftReturn(d).fold(
            _.copy(triageAnswers = newAnswers),
            _.copy(triageAnswers = newAnswers)
          )
        )

        if (forceDisplayGainOrLossAfterReliefsForAmends)
          newFillingOutReturn.withForceDisplayGainOrLossAfterReliefsForAmends
        else newFillingOutReturn
      }
    )

  private def hasPreviousReturnWithSameCompletionDate(
    completionDate: CompletionDate,
    individualUserType: Option[IndividualUserType],
    state: JourneyState
  ) = {
    val originalReturnId = state.toOption.flatMap(_._1.amendReturnData.map(_.originalReturn.summary.submissionId))

    individualUserType match {
      case Some(_: RepresentativeType) => false
      case _                           =>
        val previousSentCompletionDates =
          state
            .fold(_.previousSentReturns, _._1.previousSentReturns)
            .map(_.summaries)
            .getOrElse(List.empty)
            .filterNot(summary => originalReturnId.contains(summary.submissionId))
            .map(_.completionDate)
        previousSentCompletionDates.contains(completionDate.value)
    }
  }

  private def withPersonalRepresentativeDetails(state: JourneyState)(
    f: Option[PersonalRepresentativeDetails] => Future[Result]
  )(implicit request: RequestWithSessionData[_]): Future[Result] = {
    val personalRepresentativeDetails = state.fold(
      PersonalRepresentativeDetails.fromStartingNewDraftReturn,
      { case (fillingOutReturn, _) =>
        PersonalRepresentativeDetails.fromDraftReturn(fillingOutReturn.draftReturn)
      }
    )

    personalRepresentativeDetails.fold(
      { e =>
        logger.warn(s"Could not get personal representative details: $e")
        errorHandler.errorResult()
      },
      f
    )
  }

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
        override def bind(
          key: String,
          data: Map[String, String]
        ): Either[Seq[FormError], Int] = {
          val result =
            FormUtils
              .readValue(key, data, _.toInt)
              .flatMap(validateNumberOfProperties)
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
      "multipleDisposalsWereAllPropertiesResidential" -> of(
        BooleanFormatter.formatter
      )
    )(identity)(Some(_))
  )

  val countryOfResidenceForm: Form[Country] = Form(
    mapping(
      "countryCode" -> of(Country.formatter)
    )(identity)(Some(_))
  )

  def taxYearExchangedForm(
    taxYearOfDateOfDeath: TaxYearExchanged,
    representativeType: Option[RepresentativeType]
  ): Form[TaxYearExchanged] = {
    val conditionExpr1 = !(taxYearOfDateOfDeath === TaxYearExchanged.TaxYear2021)
    val conditionExpr2 = !(taxYearOfDateOfDeath === TaxYearExchanged.TaxYear2020)
    val conditionExpr3 =
      !(taxYearOfDateOfDeath === TaxYearExchanged.TaxYear2021 ||
        taxYearOfDateOfDeath === TaxYearExchanged.TaxYear2020)

    val taxYearExchangedFormFormatter: Formatter[TaxYearExchanged] =
      new Formatter[TaxYearExchanged] {
        override def bind(
          key: String,
          data: Map[String, String]
        ): Either[Seq[FormError], TaxYearExchanged] =
          readValue(key, data, identity)
            .flatMap {
              case "TaxYear2021"       =>
                if (representativeType.contains(PersonalRepresentative) && conditionExpr1)
                  Left(FormError(key, "error.before.invalid"))
                else if (representativeType.contains(PersonalRepresentativeInPeriodOfAdmin) && conditionExpr3)
                  Left(FormError(key, "error.after.invalid"))
                else
                  Right(TaxYearExchanged.TaxYear2021)
              case "TaxYear2020"       =>
                if (representativeType.contains(PersonalRepresentative) && conditionExpr3)
                  Left(FormError(key, "error.before.invalid"))
                else if (representativeType.contains(PersonalRepresentativeInPeriodOfAdmin) && conditionExpr2)
                  Left(FormError(key, "error.after.invalid"))
                else
                  Right(TaxYearExchanged.TaxYear2020)
              case "TaxYearBefore2020" => Right(TaxYearExchanged.TaxYearBefore2020)
              case "DifferentTaxYears" => Right(TaxYearExchanged.DifferentTaxYears)
              case _                   => Left(FormError(key, "error.required"))
            }
            .leftMap(Seq(_))

        override def unbind(
          key: String,
          value: TaxYearExchanged
        ): Map[String, String] =
          Map(key -> value.toString)
      }

    Form(
      mapping(
        "multipleDisposalsTaxYear" -> Forms
          .of(taxYearExchangedFormFormatter)
      )(identity)(Some(_))
    )
  }

  val assetTypeForNonUkResidentsForm: Form[List[AssetType]] = {
    val checkBoxAssetTypeFormFormatter: Formatter[AssetType] =
      new Formatter[AssetType] {
        override def bind(
          key: String,
          data: Map[String, String]
        ): Either[Seq[FormError], AssetType] =
          readValue(key, data, identity)
            .flatMap {
              case "0" => Right(AssetType.Residential)
              case "1" => Right(AssetType.NonResidential)
              case "2" => Right(AssetType.MixedUse)
              case "3" => Right(AssetType.IndirectDisposal)
              case _   => Left(FormError(key, "error.invalid"))
            }
            .leftMap(Seq(_))

        override def unbind(
          key: String,
          value: AssetType
        ): Map[String, String] =
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

  def completionDateForm(
    maximumDateInclusive: LocalDate,
    minimumDateInclusive: LocalDate,
    isDateOfDeathValid: Option[Boolean] = None,
    taxYearAtStart: Option[Int] = None
  ): Form[CompletionDate] =
    Form(
      mapping(
        "" -> of(
          TimeUtils.dateFormatterForMultiDisposals(
            Some(maximumDateInclusive),
            Some(minimumDateInclusive),
            "multipleDisposalsCompletionDate-day",
            "multipleDisposalsCompletionDate-month",
            "multipleDisposalsCompletionDate-year",
            "multipleDisposalsCompletionDate",
            taxYearAtStart,
            isDateOfDeathValid
          )
        )
      )(CompletionDate(_))(d => Some(d.value))
    )

}
