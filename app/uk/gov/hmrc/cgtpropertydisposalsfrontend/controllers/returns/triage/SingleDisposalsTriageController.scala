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
import play.api.Configuration
import play.api.data.Form
import play.api.data.Forms.{mapping, of}
import play.api.http.Writeable
import play.api.mvc._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.representee
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.{routes => returnsRoutes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, StartingNewDraftReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.TimeUtils._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Country
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.UUIDGenerator
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.AssetType.{IndirectDisposal, MixedUse, NonResidential, Residential}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.DisposalMethod.{Gifted, Other, Sold}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.IndividualUserType.{Capacitor, PersonalRepresentative, Self}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.NumberOfProperties.{MoreThanOne, One}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.{CompleteSingleDisposalTriageAnswers, IncompleteSingleDisposalTriageAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.YearToDateLiabilityAnswers.{CalculatedYTDAnswers, NonCalculatedYTDAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.audit.DraftReturnStarted
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.AuditService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.{ReturnsService, TaxYearService}
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
  taxYearService: TaxYearService,
  auditService: AuditService,
  uuidGenerator: UUIDGenerator,
  cc: MessagesControllerComponents,
  val config: Configuration,
  disposalMethodPage: triagePages.how_did_you_dispose,
  wereYouAUKResidentPage: triagePages.were_you_a_uk_resident,
  countryOfResidencePage: triagePages.country_of_residence,
  assetTypeForNonUkResidentsPage: triagePages.asset_type_for_non_uk_residents,
  disposalDateOfSharesForNonUk: triagePages.disposal_date_of_shares,
  didYouDisposeOfResidentialPropertyPage: triagePages.did_you_dispose_of_residential_property,
  disposalDatePage: triagePages.disposal_date,
  completionDatePage: triagePages.completion_date,
  checkYourAnswersPage: triagePages.check_your_answers
)(implicit viewConfig: ViewConfig, ec: ExecutionContext)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with Logging
    with SessionUpdates {

  import SingleDisposalsTriageController._

  type JourneyState = Either[StartingNewDraftReturn, (DraftSingleDisposalReturn, FillingOutReturn)]

  def howDidYouDisposeOfProperty(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    displayTriagePage(
      _.fold(incomplete => if (incomplete.hasConfirmedSingleDisposal) Some(()) else None, _ => Some(())),
      _ => routes.CommonTriageQuestionsController.howManyProperties()
    )(_ => disposalMethodForm)(
      extractField = _.fold(_.disposalMethod, c => Some(c.disposalMethod)),
      page = { (journeyStatus, currentAnswers, form, isDraftReturn, _) =>
        val isATrust = journeyStatus
          .fold(_.subscribedDetails.isATrust, _._2.subscribedDetails.isATrust)
        disposalMethodPage(
          form,
          backLink(currentAnswers, routes.CommonTriageQuestionsController.howManyProperties()),
          isDraftReturn,
          isATrust
        )
      }
    )
  }

  def howDidYouDisposeOfPropertySubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async {
    implicit request =>
      handleTriagePageSubmit(
        _.fold(incomplete => if (incomplete.hasConfirmedSingleDisposal) Some(()) else None, _ => Some(())),
        _ => routes.CommonTriageQuestionsController.howManyProperties()
      )(_ => disposalMethodForm)(
        page = { (journeyStatus, currentAnswers, form, isDraftReturn, _) =>
          val isATrust = journeyStatus
            .fold(_.subscribedDetails.isATrust, _._2.subscribedDetails.isATrust)
          disposalMethodPage(
            form,
            backLink(currentAnswers, routes.CommonTriageQuestionsController.howManyProperties()),
            isDraftReturn,
            isATrust
          )
        },
        updateState = { (disposalMethod, state, answers) =>
          if (answers.fold(_.disposalMethod, c => Some(c.disposalMethod)).contains(disposalMethod)) {
            state.map(_._2)
          } else {
            val newAnswers =
              answers.fold(_.copy(disposalMethod = Some(disposalMethod)), _.copy(disposalMethod = disposalMethod))

            state.bimap(
              _.copy(newReturnTriageAnswers = Right(newAnswers)), {
                case (d, r) =>
                  r.copy(
                    draftReturn = d.copy(
                      triageAnswers = newAnswers,
                      disposalDetailsAnswers = d.disposalDetailsAnswers.map(
                        _.unset(_.disposalPrice)
                          .unset(_.disposalFees)
                      ),
                      initialGainOrLoss          = None,
                      reliefDetailsAnswers       = None,
                      exemptionAndLossesAnswers  = None,
                      yearToDateLiabilityAnswers = None,
                      supportingEvidenceAnswers  = None
                    )
                  )
              }
            )
          }
        }
      )
  }

  def wereYouAUKResident(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    displayTriagePage(
      _.fold(_.disposalMethod, c => Some(c.disposalMethod)),
      _ => routes.SingleDisposalsTriageController.howDidYouDisposeOfProperty()
    )(_ => wasAUkResidentForm)(
      extractField = _.fold(_.wasAUKResident, c => Some(c.countryOfResidence.isUk())),
      page = { (journeyStatus, currentAnswers, form, isDraftReturn, _) =>
        val isATrust = journeyStatus
          .fold(_.subscribedDetails.isATrust, _._2.subscribedDetails.isATrust)

        wereYouAUKResidentPage(
          form,
          backLink(currentAnswers, routes.SingleDisposalsTriageController.howDidYouDisposeOfProperty()),
          isDraftReturn,
          isATrust,
          representativeType(currentAnswers)
        )
      }
    )
  }

  def wereYouAUKResidentSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    handleTriagePageSubmit(
      _.fold(_.disposalMethod, c => Some(c.disposalMethod)),
      _ => routes.SingleDisposalsTriageController.howDidYouDisposeOfProperty()
    )(_ => wasAUkResidentForm)(
      page = { (journeyStatus, currentAnswers, form, isDraftReturn, _) =>
        val isATrust = journeyStatus
          .fold(_.subscribedDetails.isATrust, _._2.subscribedDetails.isATrust)

        wereYouAUKResidentPage(
          form,
          backLink(currentAnswers, routes.SingleDisposalsTriageController.howDidYouDisposeOfProperty()),
          isDraftReturn,
          isATrust,
          representativeType(currentAnswers)
        )
      },
      updateState = { (wasAUKResident, state, answers) =>
        if (answers.fold(_.wasAUKResident, c => Some(c.countryOfResidence.isUk())).contains(wasAUKResident)) {
          state.map(_._2)
        } else {
          val newAnswers = answers
            .unset(_.assetType)
            .unset(_.countryOfResidence)
            .copy(wasAUKResident = Some(wasAUKResident))

          state.bimap(
            _.copy(newReturnTriageAnswers = Right(newAnswers)), {
              case (d, r) =>
                r.copy(draftReturn =
                  d.copy(
                    triageAnswers              = newAnswers,
                    propertyAddress            = None,
                    disposalDetailsAnswers     = None,
                    acquisitionDetailsAnswers  = None,
                    initialGainOrLoss          = None,
                    reliefDetailsAnswers       = d.reliefDetailsAnswers.map(_.unsetPrrAndLettingRelief()),
                    exemptionAndLossesAnswers  = None,
                    yearToDateLiabilityAnswers = None,
                    supportingEvidenceAnswers  = None
                  )
                )
            }
          )
        }
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
        page = { (journeyStatus, currentAnswers, form, isDraftReturn, _) =>
          val isATrust = journeyStatus
            .fold(_.subscribedDetails.isATrust, _._2.subscribedDetails.isATrust)
          didYouDisposeOfResidentialPropertyPage(
            form,
            backLink(currentAnswers, routes.SingleDisposalsTriageController.wereYouAUKResident()),
            isDraftReturn,
            isATrust,
            representativeType(currentAnswers)
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
        page = { (journeyStatus, currentAnswers, form, isDraftReturn, _) =>
          val isATrust = journeyStatus
            .fold(_.subscribedDetails.isATrust, _._2.subscribedDetails.isATrust)
          didYouDisposeOfResidentialPropertyPage(
            form,
            backLink(currentAnswers, routes.SingleDisposalsTriageController.wereYouAUKResident()),
            isDraftReturn,
            isATrust,
            representativeType(currentAnswers)
          )
        },
        updateState = { (wasResidentialProperty, state, answers) =>
          val assetType = if (wasResidentialProperty) AssetType.Residential else AssetType.NonResidential

          if (answers.fold(_.assetType, c => Some(c.assetType)).contains(assetType)) {
            state.map(_._2)
          } else {
            // make sure we unset first to avoid being in a complete state with non residential
            val newAnswers =
              answers
                .unset(_.assetType)
                .copy(assetType = Some(assetType))

            state.bimap(
              _.copy(newReturnTriageAnswers = Right(newAnswers)),
              { case (d, r) => r.copy(draftReturn = d.copy(triageAnswers = newAnswers)) }
            )
          }
        }
      )
  }

  private def disposalDateBackLink(triageAnswers: SingleDisposalTriageAnswers): Call =
    triageAnswers.fold(
      incomplete =>
        if (incomplete.wasAUKResident.exists(identity))
          routes.SingleDisposalsTriageController.didYouDisposeOfAResidentialProperty()
        else
          routes.SingleDisposalsTriageController.assetTypeForNonUkResidents(),
      _ => routes.SingleDisposalsTriageController.checkYourAnswers()
    )

  def whenWasDisposalDate(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    displayTriagePage(
      _.fold(_.assetType, c => Some(c.assetType)),
      answers => disposalDateBackLink(answers)
    )(_ => disposalDateForm(TimeUtils.today()))(
      extractField =
        _.fold(i => i.disposalDate.map(_.value).orElse(i.tooEarlyDisposalDate), c => Some(c.disposalDate.value)),
      page = { (journeyStatus, currentAnswers, form, isDraftReturn, assetType) =>
        val isATrust = journeyStatus
          .fold(_.subscribedDetails.isATrust, _._2.subscribedDetails.isATrust)
        disposalDatePage(
          form,
          disposalDateBackLink(currentAnswers),
          isDraftReturn,
          assetType,
          isATrust
        )
      }
    )
  }

  def whenWasDisposalDateSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withSingleDisposalTriageAnswers(request) { (_, state, triageAnswers) =>
      val isATrust =
        state.fold(s => s.subscribedDetails.isATrust, f => f._2.subscribedDetails.isATrust)
      triageAnswers.fold(_.assetType, c => Some(c.assetType)) match {
        case None => Redirect(disposalDateBackLink(triageAnswers))
        case Some(assetType) =>
          disposalDateForm(TimeUtils.today())
            .bindFromRequest()
            .fold(
              formWithErrors =>
                BadRequest(
                  disposalDatePage(
                    formWithErrors,
                    disposalDateBackLink(triageAnswers),
                    state.isRight,
                    assetType,
                    isATrust
                  )
                ), { date =>
                val result = triageAnswers.fold(_.disposalDate, c => Some(c.disposalDate)) match {
                  case Some(existingDisposalDate) if (existingDisposalDate.value === date) =>
                    EitherT.pure(Some(existingDisposalDate.taxYear))
                  case _ =>
                    for {
                      taxYear <- taxYearService.taxYear(date)
                      updatedAnswers = updateDisposalDate(date, taxYear, triageAnswers)
                      newState = state.bimap(
                        _.copy(newReturnTriageAnswers = Right(updatedAnswers)), {
                          case (d, r) => r.copy(draftReturn = updateDraftReturnForDisposalDate(d, updatedAnswers))
                        }
                      )
                      _ <- newState.fold(
                            _ => EitherT.pure(()),
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
                    } yield taxYear
                }

                result.fold(
                  { e =>
                    logger.warn("Could not update session", e)
                    errorHandler.errorResult()
                  },
                  taxYear =>
                    if (taxYear.isEmpty)
                      Redirect(routes.CommonTriageQuestionsController.disposalDateTooEarly())
                    else
                      Redirect(routes.SingleDisposalsTriageController.checkYourAnswers())
                )
              }
            )
      }
    }
  }

  private def updateDraftReturnForDisposalDate(
    currentDraftReturn: DraftSingleDisposalReturn,
    newAnswers: IncompleteSingleDisposalTriageAnswers
  ): DraftSingleDisposalReturn =
    currentDraftReturn.copy(
      triageAnswers = newAnswers,
      acquisitionDetailsAnswers = currentDraftReturn.acquisitionDetailsAnswers.map(
        _.unset(_.acquisitionDate)
          .unset(_.acquisitionPrice)
          .unset(_.rebasedAcquisitionPrice)
          .unset(_.shouldUseRebase)
          .unset(_.improvementCosts)
          .unset(_.acquisitionFees)
      ),
      initialGainOrLoss          = None,
      reliefDetailsAnswers       = currentDraftReturn.reliefDetailsAnswers.map(_.unsetPrrAndLettingRelief()),
      yearToDateLiabilityAnswers = currentDraftReturn.yearToDateLiabilityAnswers.flatMap(_.unsetAllButIncomeDetails()),
      supportingEvidenceAnswers  = None
    )

  private def updateDisposalDate(
    d: LocalDate,
    taxYear: Option[TaxYear],
    answers: SingleDisposalTriageAnswers
  ): IncompleteSingleDisposalTriageAnswers = {
    def updateCompleteAnswers(
      c: CompleteSingleDisposalTriageAnswers,
      date: Either[LocalDate, DisposalDate]
    ): IncompleteSingleDisposalTriageAnswers =
      IncompleteSingleDisposalTriageAnswers
        .fromCompleteAnswers(c)
        .copy(
          disposalDate         = date.toOption,
          tooEarlyDisposalDate = date.swap.toOption,
          completionDate       = None
        )

    taxYear.fold {
      answers.fold(
        _.copy(disposalDate = None, tooEarlyDisposalDate = Some(d), completionDate = None),
        updateCompleteAnswers(_, Left(d))
      )
    } { taxYear =>
      answers.fold(
        _.copy(disposalDate = Some(DisposalDate(d, taxYear)), tooEarlyDisposalDate = None, completionDate = None),
        updateCompleteAnswers(_, Right(DisposalDate(d, taxYear)))
      )
    }
  }

  def whenWasCompletionDate(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    displayTriagePage(
      _.fold(_.disposalDate, c => Some(c.disposalDate)),
      _ => routes.SingleDisposalsTriageController.whenWasDisposalDate()
    )(disposalDate => completionDateForm(disposalDate, TimeUtils.today()))(
      extractField = _.fold(_.completionDate, c => Some(c.completionDate)),
      page = { (journeyStatus, currentAnswers, form, isDraftReturn, _) =>
        val isATrust = journeyStatus
          .fold(_.subscribedDetails.isATrust, _._2.subscribedDetails.isATrust)

        completionDatePage(
          form,
          backLink(currentAnswers, routes.SingleDisposalsTriageController.whenWasDisposalDate()),
          isDraftReturn,
          isATrust
        )
      }
    )
  }

  def whenWasCompletionDateSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    handleTriagePageSubmit(
      _.fold(_.disposalDate, c => Some(c.disposalDate)),
      _ => routes.SingleDisposalsTriageController.whenWasDisposalDate()
    )(disposalDate => completionDateForm(disposalDate, TimeUtils.today()))(
      page = { (journeyStatus, currentAnswers, form, isDraftReturn, _) =>
        val isATrust = journeyStatus
          .fold(_.subscribedDetails.isATrust, _._2.subscribedDetails.isATrust)
        completionDatePage(
          form,
          backLink(currentAnswers, routes.SingleDisposalsTriageController.whenWasDisposalDate()),
          isDraftReturn,
          isATrust
        )
      },
      updateState = { (date, state, answers) =>
        if (answers.fold(_.completionDate, c => Some(c.completionDate)).contains(date)) {
          state.map(_._2)
        } else {
          val newAnswers = answers.fold(_.copy(completionDate = Some(date)), _.copy(completionDate = date))

          state.bimap(
            _.copy(newReturnTriageAnswers = Right(newAnswers)), {
              case (d, r) =>
                r.copy(
                  draftReturn = d.copy(
                    triageAnswers = newAnswers,
                    acquisitionDetailsAnswers = d.acquisitionDetailsAnswers.map(
                      _.unset(_.acquisitionDate)
                        .unset(_.acquisitionPrice)
                        .unset(_.rebasedAcquisitionPrice)
                        .unset(_.shouldUseRebase)
                    ),
                    initialGainOrLoss          = None,
                    reliefDetailsAnswers       = d.reliefDetailsAnswers.map(_.unsetPrrAndLettingRelief()),
                    yearToDateLiabilityAnswers = d.yearToDateLiabilityAnswers.flatMap(_.unsetAllButIncomeDetails())
                  )
                )
            }
          )
        }
      }
    )
  }

  def countryOfResidence(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    displayTriagePage(
      _.fold(_.wasAUKResident.filterNot(identity), c => Some(c.countryOfResidence.isUk()).filterNot(identity)),
      _ => routes.SingleDisposalsTriageController.wereYouAUKResident()
    )(_ => countryOfResidenceForm)(
      extractField = _.fold(_.countryOfResidence, c => Some(c.countryOfResidence)),
      page = { (journeyStatus, currentAnswers, form, isDraftReturn, _) =>
        val isATrust = journeyStatus
          .fold(_.subscribedDetails.isATrust, _._2.subscribedDetails.isATrust)
        countryOfResidencePage(
          form,
          backLink(currentAnswers, routes.SingleDisposalsTriageController.wereYouAUKResident()),
          isDraftReturn,
          isATrust,
          representativeType(currentAnswers)
        )
      }
    )
  }

  def countryOfResidenceSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    handleTriagePageSubmit(
      _.fold(_.wasAUKResident.filterNot(identity), c => Some(c.countryOfResidence.isUk()).filterNot(identity)),
      _ => routes.SingleDisposalsTriageController.wereYouAUKResident()
    )(_ => countryOfResidenceForm)(
      page = { (journeyStatus, currentAnswers, form, isDraftReturn, _) =>
        val isATrust = journeyStatus
          .fold(_.subscribedDetails.isATrust, _._2.subscribedDetails.isATrust)
        countryOfResidencePage(
          form,
          backLink(currentAnswers, routes.SingleDisposalsTriageController.wereYouAUKResident()),
          isDraftReturn,
          isATrust,
          representativeType(currentAnswers)
        )
      },
      updateState = { (country, state, answers) =>
        if (answers.fold(_.countryOfResidence, c => Some(c.countryOfResidence)).contains(country)) {
          state.map(_._2)
        } else {
          val newAnswers =
            answers.fold(_.copy(countryOfResidence = Some(country)), _.copy(countryOfResidence = country))
          state.bimap(
            _.copy(newReturnTriageAnswers = Right(newAnswers)), {
              case (d, r) =>
                r.copy(
                  draftReturn = d.copy(
                    triageAnswers = newAnswers,
                    yearToDateLiabilityAnswers = d.yearToDateLiabilityAnswers.flatMap {
                      case _: CalculatedYTDAnswers => None
                      case n: NonCalculatedYTDAnswers =>
                        Some(
                          n.unset(
                            _.hasEstimatedDetails
                          )
                        )
                    }
                  )
                )
            }
          )
        }

      }
    )
  }

  def assetTypeForNonUkResidents(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    displayTriagePage(
      _.fold(_.countryOfResidence, c => Some(c.countryOfResidence).filterNot(_.isUk())),
      _ => routes.SingleDisposalsTriageController.countryOfResidence()
    )(_ => assetTypeForNonUkResidentsForm)(
      _.fold(_.assetType, c => Some(c.assetType)), { (journeyStatus, currentAnswers, form, isDraftReturn, _) =>
        val isATrust = journeyStatus
          .fold(_.subscribedDetails.isATrust, _._2.subscribedDetails.isATrust)
        assetTypeForNonUkResidentsPage(
          form,
          backLink(currentAnswers, routes.SingleDisposalsTriageController.countryOfResidence()),
          isDraftReturn,
          isATrust,
          representativeType(currentAnswers)
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
        { (journeyStatus, currentAnswers, form, isDraftReturn, _) =>
          val isATrust = journeyStatus
            .fold(_.subscribedDetails.isATrust, _._2.subscribedDetails.isATrust)
          assetTypeForNonUkResidentsPage(
            form,
            backLink(currentAnswers, routes.SingleDisposalsTriageController.countryOfResidence()),
            isDraftReturn,
            isATrust,
            representativeType(currentAnswers)
          )
        },
        updateState = { (assetType, state, answers) =>
          if (answers.fold(_.assetType, c => Some(c.assetType)).contains(assetType)) {
            state.map(_._2)
          } else {
            val newAnswers =
              answers.fold(
                _.copy(assetType = Some(assetType)),
                _.copy(assetType = assetType)
              )

            state.bimap(
              _.copy(newReturnTriageAnswers = Right(newAnswers)), {
                case (d, r) =>
                  r.copy(
                    draftReturn = d.copy(
                      triageAnswers              = newAnswers,
                      propertyAddress            = None,
                      disposalDetailsAnswers     = None,
                      acquisitionDetailsAnswers  = None,
                      initialGainOrLoss          = None,
                      reliefDetailsAnswers       = d.reliefDetailsAnswers.map(_.unsetPrrAndLettingRelief()),
                      yearToDateLiabilityAnswers = d.yearToDateLiabilityAnswers.flatMap(_.unsetAllButIncomeDetails()),
                      supportingEvidenceAnswers  = None
                    )
                  )
              }
            )

          }
        }
      )
  }

  def disposalDateOfShares(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    displayTriagePage(
      _.fold(_.assetType, c => Some(c.assetType).filterNot(e => e === IndirectDisposal)),
      _ => routes.SingleDisposalsTriageController.countryOfResidence()
    )(_ => sharesDisposalDateForm)(
      _.fold(
        _.disposalDate.map(d => ShareDisposalDate(d.value)),
        e => Some(ShareDisposalDate(e.disposalDate.value))
      ),
      (_, currentAnswers, form, isDraftReturn, _) =>
        disposalDateOfSharesForNonUk(
          form,
          backLink(currentAnswers, routes.SingleDisposalsTriageController.assetTypeForNonUkResidents()),
          isDraftReturn
        )
    )
  }

  def disposalDateOfSharesSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withSingleDisposalTriageAnswers(request) { (_, state, triageAnswers) =>
      triageAnswers.fold(_.assetType, c => Some(c.assetType)) match {
        case Some(assetType) if (assetType === AssetType.IndirectDisposal) =>
          sharesDisposalDateForm
            .bindFromRequest()
            .fold(
              formWithErrors =>
                BadRequest(
                  disposalDateOfSharesForNonUk(
                    formWithErrors,
                    backLink(triageAnswers, routes.SingleDisposalsTriageController.assetTypeForNonUkResidents()),
                    state.isRight
                  )
                ), { date =>
                val result = triageAnswers.fold(_.disposalDate, c => Some(c.disposalDate)) match {
                  case Some(existingDisposalDate) if (existingDisposalDate.value === date.value) =>
                    EitherT.pure(Some(existingDisposalDate.taxYear))
                  case _ =>
                    for {
                      taxYear <- taxYearService.taxYear(date.value)
                      updatedDisposalDate = updateDisposalDate(date.value, taxYear, triageAnswers)
                      updatedDisposalAndCompletionDate = updatedDisposalDate
                        .copy(completionDate = Some(CompletionDate(date.value)))
                      newState = state.bimap(
                        _.copy(newReturnTriageAnswers = Right(updatedDisposalAndCompletionDate)), {
                          case (d, r) =>
                            r.copy(draftReturn = updateDraftReturnForDisposalDate(d, updatedDisposalAndCompletionDate))
                        }
                      )
                      _ <- newState.fold(
                            _ => EitherT.pure(()),
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
                    } yield taxYear
                }

                result.fold(
                  { e =>
                    logger.warn("Could not update session", e)
                    errorHandler.errorResult()
                  },
                  taxYear =>
                    if (taxYear.isEmpty)
                      Redirect(routes.CommonTriageQuestionsController.disposalsOfSharesTooEarly())
                    else
                      Redirect(routes.SingleDisposalsTriageController.checkYourAnswers())
                )
              }
            )
        case _ => Redirect(disposalDateBackLink(triageAnswers))
      }
    }
  }

  def checkYourAnswers(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withSingleDisposalTriageAnswers(request) { (_, state, triageAnswers) =>
      lazy val displayReturnToSummaryLink = state.fold(_ => false, _ => true)
      val isIndividual                    = state.fold(_.subscribedDetails, _._2.subscribedDetails).userType().isRight

      val representeeAnswers = state
        .fold(
          _.representeeAnswers,
          _._1.representeeAnswers
        )
      val representeeAnswersIncomplete = !representeeAnswers.map(_.fold(_ => false, _ => true)).getOrElse(false)

      triageAnswers match {
        case c: CompleteSingleDisposalTriageAnswers =>
          val isATrust = state
            .bimap(
              _.subscribedDetails.isATrust,
              _._2.subscribedDetails.isATrust
            )
            .contains(true)
          Ok(checkYourAnswersPage(c, displayReturnToSummaryLink, isATrust, representativeType(c)))

        case IncompleteSingleDisposalTriageAnswers(None, _, _, _, _, _, _, _, __) if isIndividual =>
          Redirect(routes.CommonTriageQuestionsController.whoIsIndividualRepresenting())

        case IncompleteSingleDisposalTriageAnswers(Some(IndividualUserType.Capacitor), _, _, _, _, _, _, _, _)
            if representeeAnswersIncomplete =>
          Redirect(
            representee.routes.RepresenteeController
              .enterName()
          )

        case IncompleteSingleDisposalTriageAnswers(
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

        case IncompleteSingleDisposalTriageAnswers(_, false, _, _, _, _, _, _, _) =>
          Redirect(routes.CommonTriageQuestionsController.howManyProperties())

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

        case IncompleteSingleDisposalTriageAnswers(_, _, _, Some(true), _, Some(NonResidential), _, _, _) =>
          Redirect(routes.CommonTriageQuestionsController.ukResidentCanOnlyDisposeResidential())

        case IncompleteSingleDisposalTriageAnswers(_, _, _, _, _, Some(AssetType.IndirectDisposal), None, None, _) =>
          Redirect(routes.SingleDisposalsTriageController.disposalDateOfShares())

        case IncompleteSingleDisposalTriageAnswers(_, _, _, _, _, Some(AssetType.MixedUse), _, _, _) =>
          Redirect(routes.CommonTriageQuestionsController.assetTypeNotYetImplemented())

        case IncompleteSingleDisposalTriageAnswers(_, _, _, _, _, _, None, _, None) =>
          Redirect(routes.SingleDisposalsTriageController.whenWasDisposalDate())

        case IncompleteSingleDisposalTriageAnswers(_, _, _, _, _, _, _, None, None) =>
          Redirect(routes.SingleDisposalsTriageController.whenWasCompletionDate())

        case IncompleteSingleDisposalTriageAnswers(
            t,
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
            t,
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
    state: Either[StartingNewDraftReturn, (DraftSingleDisposalReturn, FillingOutReturn)],
    newCompleteTriageAnswers: CompleteSingleDisposalTriageAnswers,
    displayReturnToSummaryLink: Boolean
  )(implicit request: RequestWithSessionData[_], hc: HeaderCarrier): Future[Result] = {
    val updatedJourney = state.fold(
      _.copy(newReturnTriageAnswers = Right(newCompleteTriageAnswers)), {
        case (d, r) => r.copy(draftReturn = d.copy(triageAnswers = newCompleteTriageAnswers))
      }
    )

    updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney))).map {
      case Left(e) =>
        logger.warn("Could not update session", e)
        errorHandler.errorResult()

      case Right(_) =>
        val isATrust = state
          .bimap(
            _.subscribedDetails.isATrust,
            _._2.subscribedDetails.isATrust
          )
          .contains(true)

        Ok(
          checkYourAnswersPage(
            newCompleteTriageAnswers,
            displayReturnToSummaryLink,
            isATrust,
            representativeType(newCompleteTriageAnswers)
          )
        )
    }
  }

  def checkYourAnswersSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withSingleDisposalTriageAnswers(request) { (_, state, triageAnswers) =>
      triageAnswers match {
        case _: IncompleteSingleDisposalTriageAnswers =>
          Redirect(routes.SingleDisposalsTriageController.checkYourAnswers())

        case complete: CompleteSingleDisposalTriageAnswers =>
          lazy val continueToTaskList = Redirect(returnsRoutes.TaskListController.taskList())

          def toFillingOurNewReturn(startingNewDraftReturn: StartingNewDraftReturn): Future[Result] = {
            val newDraftReturn = DraftSingleDisposalReturn
              .newDraftReturn(uuidGenerator.nextId(), complete, startingNewDraftReturn.representeeAnswers)
            val result = for {
              _ <- returnsService.storeDraftReturn(
                    newDraftReturn,
                    startingNewDraftReturn.subscribedDetails.cgtReference,
                    startingNewDraftReturn.agentReferenceNumber
                  )
              newJourney = FillingOutReturn(
                startingNewDraftReturn.subscribedDetails,
                startingNewDraftReturn.ggCredId,
                startingNewDraftReturn.agentReferenceNumber,
                newDraftReturn
              )
              _ <- EitherT(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(newJourney))))
            } yield newJourney

            result.fold(
              e => {
                logger.warn("Could not store draft return", e)
                errorHandler.errorResult()
              }, { newJourney =>
                auditService.sendEvent(
                  "draftReturnStarted",
                  DraftReturnStarted(
                    newDraftReturn,
                    newJourney.subscribedDetails.cgtReference.value,
                    newJourney.agentReferenceNumber.map(_.value)
                  ),
                  "draft-return-started"
                )
                continueToTaskList
              }
            )
          }

          state.fold[Future[Result]](
            toFillingOurNewReturn,
            _ => continueToTaskList
          )
      }
    }
  }

  private def representativeType(
    triageAnswers: SingleDisposalTriageAnswers
  ): Option[Either[PersonalRepresentative.type, Capacitor.type]] =
    triageAnswers.fold[Option[IndividualUserType]](_.individualUserType, _.individualUserType) match {
      case Some(Capacitor)              => Some(Right(Capacitor))
      case Some(PersonalRepresentative) => Some(Left(PersonalRepresentative))
      case _                            => None
    }

  private def handleTriagePageSubmit[R, A, Page: Writeable](
    requiredField: SingleDisposalTriageAnswers => Option[R],
    redirectToIfNotValidJourney: SingleDisposalTriageAnswers => Call
  )(
    form: R => Form[A]
  )(
    page: (JourneyState, SingleDisposalTriageAnswers, Form[A], Boolean, R) => Page,
    updateState: (
      A,
      Either[StartingNewDraftReturn, (DraftSingleDisposalReturn, FillingOutReturn)],
      SingleDisposalTriageAnswers
    ) => Either[StartingNewDraftReturn, FillingOutReturn]
  )(
    implicit request: RequestWithSessionData[_]
  ): Future[Result] =
    withSingleDisposalTriageAnswers(request) { (_, state, triageAnswers) =>
      requiredField(triageAnswers) match {
        case None => Redirect(redirectToIfNotValidJourney(triageAnswers))
        case Some(r) =>
          form(r)
            .bindFromRequest()
            .fold(
              formWithErrors => BadRequest(page(state, triageAnswers, formWithErrors, state.isRight, r)), { value =>
                val updatedState = updateState(value, state, triageAnswers)

                val result = for {
                  _ <- updatedState.fold(
                        _ => EitherT.pure(()),
                        newFillingOutReturn =>
                          if (state.exists(_._2.draftReturn === newFillingOutReturn.draftReturn)) EitherT.pure(())
                          else
                            returnsService.storeDraftReturn(
                              newFillingOutReturn.draftReturn,
                              newFillingOutReturn.subscribedDetails.cgtReference,
                              newFillingOutReturn.agentReferenceNumber
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
                  _ => Redirect(routes.SingleDisposalsTriageController.checkYourAnswers())
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
    page: (JourneyState, SingleDisposalTriageAnswers, Form[A], Boolean, R) => Page
  )(
    implicit request: RequestWithSessionData[_]
  ): Future[Result] =
    withSingleDisposalTriageAnswers(request) { (_, state, triageAnswers) =>
      requiredField(triageAnswers) match {
        case None => Redirect(redirectToIfNotValidJourney(triageAnswers))
        case Some(r) =>
          val f = extractField(triageAnswers)
            .fold(form(r))(form(r).fill)

          Ok(page(state, triageAnswers, f, state.isRight, r))
      }
    }

  private def withSingleDisposalTriageAnswers(request: RequestWithSessionData[_])(
    f: (
      SessionData,
      Either[StartingNewDraftReturn, (DraftSingleDisposalReturn, FillingOutReturn)],
      SingleDisposalTriageAnswers
    ) => Future[Result]
  ): Future[Result] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some((session, s @ StartingNewDraftReturn(_, _, _, Right(t), _))) =>
        f(session, Left(s), t)

      case Some((session, r @ FillingOutReturn(_, _, _, d: DraftSingleDisposalReturn))) =>
        f(session, Right(d -> r), d.triageAnswers)

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
      "disposalMethod" -> of(FormUtils.radioFormFormatter("disposalMethod", List(Sold, Gifted, Other)))
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

  def disposalDateForm(maximumDateInclusive: LocalDate): Form[LocalDate] =
    Form(
      mapping(
        "" -> of(
          TimeUtils.dateFormatter(
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
        TimeUtils.dateFormatter(
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

  val sharesDisposalDateForm: Form[ShareDisposalDate] = {
    val key = "sharesDisposalDate"
    Form(
      mapping(
        "" -> of(
          TimeUtils.dateFormatter(
            Some(LocalDate.now()),
            None,
            s"$key-day",
            s"$key-month",
            s"$key-year",
            key
          )
        )
      )(ShareDisposalDate(_))(d => Some(d.value))
    )
  }

}
