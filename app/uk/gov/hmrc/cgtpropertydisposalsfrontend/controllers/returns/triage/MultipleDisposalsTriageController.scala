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
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.representee
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.triage.MultipleDisposalsTriageController._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.FormUtils.readValue
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, StartingNewDraftReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Country
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.UUIDGenerator
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.AssetType.IndirectDisposal
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.MultipleDisposalsTriageAnswers._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.IncompleteSingleDisposalTriageAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.YearToDateLiabilityAnswers.{CalculatedYTDAnswers, NonCalculatedYTDAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{BooleanFormatter, Error, FormUtils, SessionData, TaxYear, TimeUtils}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.{ReturnsService, TaxYearService}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging.LoggerOps
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views.html.returns.triage.{disposal_date_of_shares, multipledisposals => triagePages}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.triage.CommonTriageQuestionsController.sharesDisposalDateForm
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
  disposalDateOfSharesForNonUk: disposal_date_of_shares,
  checkYourAnswersPage: triagePages.check_you_answers
)(implicit viewConfig: ViewConfig, ec: ExecutionContext)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with Logging
    with SessionUpdates {

  type JourneyState = Either[
    StartingNewDraftReturn,
    (FillingOutReturn, Either[DraftMultipleIndirectDisposalsReturn, DraftMultipleDisposalsReturn])
  ]

  private val indirectDisposalsEnabled: Boolean =
    config.underlying.getBoolean("indirect-disposals.enabled")

  private val mixedUseEnabled: Boolean =
    config.underlying.getBoolean("mixed-use.enabled")

  def guidance(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withMultipleDisposalTriageAnswers(request) { (_, state, answers) =>
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
      withMultipleDisposalTriageAnswers(request) { (_, _, answers) =>
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
      withMultipleDisposalTriageAnswers(request) { (_, state, answers) =>
        val numberOfDisposals =
          answers.fold(_.numberOfProperties, c => Some(c.numberOfProperties))
        val form              = numberOfDisposals.fold(numberOfPropertiesForm)(
          numberOfPropertiesForm.fill
        )
        val backLink          = answers.fold(
          _ => routes.MultipleDisposalsTriageController.guidance(),
          _ => routes.MultipleDisposalsTriageController.checkYourAnswers()
        )
        Ok(howManyPropertiesPage(form, backLink, state.isRight))
      }
    }

  def howManyDisposalsSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withMultipleDisposalTriageAnswers(request) { (_, state, answers) =>
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

          r.copy(draftReturn = newDraftReturn.merge)
        case (r, Left(d))  =>
          val newDraftReturn = newAnswersWithRedirectTo._1.bimap(
            DraftMultipleIndirectDisposalsReturn
              .newDraftReturn(d.id, _, d.representeeAnswers),
            DraftSingleIndirectDisposalReturn
              .newDraftReturn(d.id, _, d.representeeAnswers)
          )

          r.copy(draftReturn = newDraftReturn.merge)

      }
    )
    newState -> newAnswersWithRedirectTo._2
  }

  def wereYouAUKResident(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withMultipleDisposalTriageAnswers(request) { (_, state, answers) =>
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
            answers.representativeType()
          )
        )
      }
    }

  def wereYouAUKResidentSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withMultipleDisposalTriageAnswers(request) { (_, state, answers) =>
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
                  answers.representativeType()
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
                          supportingEvidenceAnswers = None
                        ),
                      multiple =>
                        multiple.copy(
                          examplePropertyDetailsAnswers = multiple.examplePropertyDetailsAnswers.map(
                            _.unset(_.address)
                              .unset(_.disposalPrice)
                              .unset(_.acquisitionPrice)
                          ),
                          yearToDateLiabilityAnswers = None,
                          supportingEvidenceAnswers = None
                        )
                    )
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
      withMultipleDisposalTriageAnswers(request) { (_, state, answers) =>
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
        Ok(wereAllPropertiesResidentialPage(form, backLink, state.isRight))
      }
    }

  def wereAllPropertiesResidentialSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withMultipleDisposalTriageAnswers(request) { (_, state, answers) =>
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

                val newState = updateState(state, updatedAnswers, identity)
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
      withMultipleDisposalTriageAnswers(request) { (_, state, answers) =>
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
              answers.representativeType()
            )
          )
        }
      }
    }

  def countryOfResidenceSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withMultipleDisposalTriageAnswers(request) { (_, state, answers) =>
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
                  answers.representativeType()
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
                      multipleIndirect =>
                        multipleIndirect.copy(
                          yearToDateLiabilityAnswers = multipleIndirect.yearToDateLiabilityAnswers.flatMap {
                            case _: CalculatedYTDAnswers    => None
                            case n: NonCalculatedYTDAnswers =>
                              Some(n.unset(_.hasEstimatedDetails).unset(_.taxDue))
                          }
                        ),
                      multiple =>
                        multiple.copy(
                          yearToDateLiabilityAnswers = multiple.yearToDateLiabilityAnswers.flatMap {
                            case _: CalculatedYTDAnswers    => None
                            case n: NonCalculatedYTDAnswers =>
                              Some(n.unset(_.hasEstimatedDetails).unset(_.taxDue))
                          }
                        )
                    )
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
      withMultipleDisposalTriageAnswers(request) { (_, state, answers) =>
        val taxYearExchanged =
          answers.fold(_.taxYearAfter6April2020, _ => Some(true))
        val form             =
          taxYearExchanged.fold(taxYearExchangedForm)(taxYearExchangedForm.fill)
        val backLink         = answers.fold(
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

  def whenWereContractsExchangedSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withMultipleDisposalTriageAnswers(request) { (_, state, answers) =>
        taxYearExchangedForm
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
              BadRequest(
                taxYearExchangedPage(
                  formWithErrors,
                  backLink,
                  state.isRight
                )
              )
            },
            taxYearAfter6April2020 =>
              if (
                answers
                  .fold(_.taxYearAfter6April2020, _ => Some(true))
                  .contains(taxYearAfter6April2020)
              )
                Redirect(
                  routes.MultipleDisposalsTriageController.checkYourAnswers()
                )
              else {

                val result =
                  for {
                    taxYear        <- if (taxYearAfter6April2020)
                                        taxYearService.taxYear(TimeUtils.today())
                                      else EitherT.pure[Future, Error](None)
                    updatedAnswers <- EitherT.fromEither[Future](
                                        updateTaxYearToAnswers(
                                          taxYearAfter6April2020,
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
                                          )
                                      )
                    _              <- newState.fold(
                                        _ => EitherT.pure[Future, Error](()),
                                        r =>
                                          returnsService.storeDraftReturn(
                                            r.draftReturn,
                                            r.subscribedDetails.cgtReference,
                                            r.agentReferenceNumber
                                          )
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

  def assetTypeForNonUkResidents(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withMultipleDisposalTriageAnswers(request) { (_, state, answers) =>
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
            answers.representativeType()
          )
        )
      }
    }

  def assetTypeForNonUkResidentsSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withMultipleDisposalTriageAnswers(request) { (_, state, answers) =>
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
                  answers.representativeType()
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
                            yearToDateLiabilityAnswers = None,
                            supportingEvidenceAnswers = None
                          )
                        )
                  }
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
      withMultipleDisposalTriageAnswers(request) { (_, state, answers) =>
        val completionDate =
          answers.fold(_.completionDate, c => Some(c.completionDate))
        val today          = TimeUtils.today()
        val form           = completionDate.fold(completionDateForm(today))(
          completionDateForm(today).fill
        )
        val backLink       = answers.fold(
          _ =>
            routes.MultipleDisposalsTriageController
              .whenWereContractsExchanged(),
          _ => routes.MultipleDisposalsTriageController.checkYourAnswers()
        )
        Ok(completionDatePage(form, backLink, state.isRight))
      }
    }

  def completionDateSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withMultipleDisposalTriageAnswers(request) { (_, state, answers) =>
        completionDateForm(TimeUtils.today())
          .bindFromRequest()
          .fold(
            { formWithErrors =>
              val backLink = answers.fold(
                _ =>
                  routes.MultipleDisposalsTriageController
                    .whenWereContractsExchanged(),
                _ => routes.MultipleDisposalsTriageController.checkYourAnswers()
              )
              BadRequest(
                completionDatePage(
                  formWithErrors,
                  backLink,
                  state.isRight
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
                  answers.fold[MultipleDisposalsTriageAnswers](
                    _.copy(completionDate = Some(completionDate)),
                    _.copy(completionDate = completionDate)
                  )
                val newState       = updateState(
                  state,
                  updatedAnswers,
                  d =>
                    d.bimap(
                      multipleIndirect =>
                        multipleIndirect.copy(
                          exampleCompanyDetailsAnswers = multipleIndirect.exampleCompanyDetailsAnswers,
                          yearToDateLiabilityAnswers = None
                        ),
                      multiple =>
                        multiple.copy(
                          examplePropertyDetailsAnswers = multiple.examplePropertyDetailsAnswers.map(
                            _.unset(_.disposalDate)
                          ),
                          yearToDateLiabilityAnswers = None
                        )
                    )
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

  private def updateTaxYearToAnswers(
    taxYearAfter6April2020: Boolean,
    taxYear: Option[TaxYear],
    answers: MultipleDisposalsTriageAnswers
  ): Either[Error, MultipleDisposalsTriageAnswers] =
    taxYear match {
      case None if taxYearAfter6April2020 =>
        Left(Error("Could not find tax year"))
      case _                              =>
        Right(
          answers
            .unset(_.completionDate)
            .copy(
              taxYearAfter6April2020 = Some(taxYearAfter6April2020),
              taxYear = taxYear
            )
        )
    }

  def disposalDateOfShares(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withMultipleDisposalTriageAnswers(request) { (_, state, answers) =>
        val backLink = answers.fold(
          _ =>
            routes.MultipleDisposalsTriageController
              .assetTypeForNonUkResidents(),
          _ => routes.MultipleDisposalsTriageController.checkYourAnswers()
        )
        val form     =
          answers.fold(_.completionDate, e => Some(e.completionDate)) match {
            case Some(value) =>
              sharesDisposalDateForm.fill(ShareDisposalDate(value.value))
            case None        => sharesDisposalDateForm
          }
        Ok(
          disposalDateOfSharesForNonUk(
            form,
            backLink,
            state.isRight,
            routes.MultipleDisposalsTriageController
              .disposalDateOfSharesSubmit()
          )
        )
      }
    }

  def disposalDateOfSharesSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withMultipleDisposalTriageAnswers(request) { (_, state, answers) =>
        sharesDisposalDateForm
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
                  routes.MultipleDisposalsTriageController
                    .disposalDateOfSharesSubmit()
                )
              )
            },
            shareDisposalDate =>
              if (
                answers
                  .fold(_.completionDate, c => Some(c.completionDate))
                  .contains(CompletionDate(shareDisposalDate.value))
              )
                Redirect(
                  routes.MultipleDisposalsTriageController.checkYourAnswers()
                )
              else {
                val result =
                  for {
                    taxYear        <- taxYearService.taxYear(shareDisposalDate.value)
                    updatedAnswers <- EitherT
                                        .fromEither[Future](
                                          Right(
                                            answers
                                              .unset(_.completionDate)
                                              .copy(
                                                taxYear = taxYear,
                                                completionDate = Some(CompletionDate(shareDisposalDate.value)),
                                                taxYearAfter6April2020 = Some(taxYear.isDefined)
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
                                                yearToDateLiabilityAnswers = None
                                              ),
                                            multiple =>
                                              multiple.copy(
                                                examplePropertyDetailsAnswers = multiple.examplePropertyDetailsAnswers
                                                  .map(_.unset(_.disposalDate)),
                                                yearToDateLiabilityAnswers = None
                                              )
                                          )
                                      )
                    _              <- newState.fold(
                                        _ => EitherT.pure[Future, Error](()),
                                        r =>
                                          returnsService.storeDraftReturn(
                                            r.draftReturn,
                                            r.subscribedDetails.cgtReference,
                                            r.agentReferenceNumber
                                          )
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
                  taxYear =>
                    if (taxYear.isEmpty)
                      Redirect(
                        routes.CommonTriageQuestionsController
                          .disposalsOfSharesTooEarly()
                      )
                    else
                      Redirect(
                        routes.MultipleDisposalsTriageController
                          .checkYourAnswers()
                      )
                )
              }
          )
      }
    }

  def checkYourAnswers(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withMultipleDisposalTriageAnswers(request) { (_, state, triageAnswers) =>
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
                _
              ) if isIndividual =>
            Redirect(
              routes.CommonTriageQuestionsController
                .whoIsIndividualRepresenting()
            )

          case IncompleteMultipleDisposalsTriageAnswers(
                Some(IndividualUserType.Capacitor),
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
                .enterName()
            )

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
              ) if representeeAnswersIncomplete =>
            Redirect(
              representee.routes.RepresenteeController
                .enterName()
            )

          case IncompleteMultipleDisposalsTriageAnswers(
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
                _
              ) =>
            Redirect(
              routes.MultipleDisposalsTriageController
                .assetTypeForNonUkResidents()
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
                _
              )
              if assetTypes.forall(a =>
                (a === AssetType.MixedUse && !mixedUseEnabled) ||
                  (a === AssetType.IndirectDisposal && !indirectDisposalsEnabled)
              ) =>
            Redirect(
              routes.CommonTriageQuestionsController
                .assetTypeNotYetImplemented()
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
                None
              )
              if assetTypes === List(
                IndirectDisposal
              ) && indirectDisposalsEnabled =>
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
                _,
                Some(false),
                _,
                _
              ) =>
            Redirect(
              routes.CommonTriageQuestionsController.disposalDateTooEarly()
            )

          case IncompleteMultipleDisposalsTriageAnswers(
                _,
                _,
                _,
                _,
                _,
                _,
                Some(true),
                None,
                _
              ) =>
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
                None
              ) =>
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
            val completeAnswers =
              CompleteMultipleDisposalsTriageAnswers(i, n, Country.uk, a, t, d)
            updateStateAndThen(
              updateState(state, completeAnswers, identity),
              Ok(
                checkYourAnswersPage(
                  completeAnswers,
                  state.isRight,
                  state.fold(
                    _.subscribedDetails.isATrust,
                    _._1.subscribedDetails.isATrust
                  )
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
                Some(true),
                Some(t),
                Some(d)
              ) =>
            val completeAnswers =
              CompleteMultipleDisposalsTriageAnswers(i, n, c, a, t, d)
            updateStateAndThen(
              updateState(state, completeAnswers, identity),
              Ok(
                checkYourAnswersPage(
                  completeAnswers,
                  state.isRight,
                  state.fold(
                    _.subscribedDetails.isATrust,
                    _._1.subscribedDetails.isATrust
                  )
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
                )
              )
            )
        }
      }
    }

  def checkYourAnswersSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withMultipleDisposalTriageAnswers(request) { (_, journey, answers) =>
        journey match {
          case Right(_)                     =>
            Redirect(controllers.returns.routes.TaskListController.taskList())

          case Left(startingNewDraftReturn) =>
            answers match {
              case _: IncompleteMultipleDisposalsTriageAnswers      =>
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
                  newDraftReturn
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
    request: RequestWithSessionData[_]
  )(
    f: (SessionData, JourneyState, MultipleDisposalsTriageAnswers) => Future[Result]
  ): Future[Result] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some((session, s @ StartingNewDraftReturn(_, _, _, Left(t), _))) =>
        f(session, Left(s), t)

      case Some(
            (
              session,
              r @ FillingOutReturn(_, _, _, m: DraftMultipleDisposalsReturn)
            )
          ) =>
        f(session, Right(r -> Right(m)), m.triageAnswers)

      case Some(
            (
              session,
              r @ FillingOutReturn(_, _, _, mi: DraftMultipleIndirectDisposalsReturn)
            )
          ) =>
        f(session, Right(r -> Left(mi)), mi.triageAnswers)

      case _                                                                =>
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
             r =>
               returnsService
                 .storeDraftReturn(
                   r.draftReturn,
                   r.subscribedDetails.cgtReference,
                   r.agentReferenceNumber
                 )
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
    ]
  ): Either[StartingNewDraftReturn, FillingOutReturn] =
    currentState.bimap(
      _.copy(newReturnTriageAnswers = Left(newAnswers)),
      {
        case (r, d) =>
          r.copy(draftReturn =
            modifyDraftReturn(d).fold(
              _.copy(triageAnswers = newAnswers),
              _.copy(triageAnswers = newAnswers)
            )
          )
      }
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

  val taxYearExchangedForm: Form[Boolean] =
    Form(
      mapping(
        "multipleDisposalsTaxYear" -> of(
          FormUtils
            .radioFormFormatter("multipleDisposalsTaxYear", List(true, false))
        )
      )(identity)(Some(_))
    )

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
    maximumDateInclusive: LocalDate
  ): Form[CompletionDate] =
    Form(
      mapping(
        "" -> of(
          TimeUtils.dateFormatter(
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
