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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.yeartodatelliability

import cats.Eq
import cats.data.EitherT
import cats.instances.future._
import cats.syntax.either._
import cats.syntax.eq._
import com.google.inject.{Inject, Singleton}
import play.api.Configuration
import play.api.data.Form
import play.api.data.Forms.{mapping, of}
import play.api.http.Writeable
import play.api.mvc._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.yeartodatelliability.YearToDateLiabilityController._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ConditionalRadioUtils.InnerOption
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, PreviousReturnData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.MoneyUtils.validateAmountOfMoney
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.{AmountInPence, MoneyUtils}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.AcquisitionDetailsAnswers.CompleteAcquisitionDetailsAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.DisposalDetailsAnswers.CompleteDisposalDetailsAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ExemptionAndLossesAnswers.CompleteExemptionAndLossesAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.IndividualUserType.PersonalRepresentativeInPeriodOfAdmin
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ReliefDetailsAnswers.CompleteReliefDetailsAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.CompleteSingleDisposalTriageAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.YearToDateLiabilityAnswers.CalculatedYTDAnswers.{CompleteCalculatedYTDAnswers, IncompleteCalculatedYTDAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.YearToDateLiabilityAnswers.NonCalculatedYTDAnswers.{CompleteNonCalculatedYTDAnswers, IncompleteNonCalculatedYTDAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.YearToDateLiabilityAnswers.{CalculatedYTDAnswers, NonCalculatedYTDAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.upscan.UpscanCallBack.{UpscanFailure, UpscanSuccess}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.upscan.{UpscanCallBack, UpscanUpload}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{BooleanFormatter, ConditionalRadioUtils, Error, FormUtils, SessionData, TaxYear}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.{CgtCalculationService, ReturnsService}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.upscan.UpscanService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging.LoggerOps
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views.html.returns.{ytdliability => pages}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class YearToDateLiabilityController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionStore,
  val errorHandler: ErrorHandler,
  returnsService: ReturnsService,
  cgtCalculationService: CgtCalculationService,
  upscanService: UpscanService,
  cc: MessagesControllerComponents,
  val config: Configuration,
  estimatedIncomePage: pages.estimated_income,
  personalAllowancePage: pages.personal_allowance,
  hasEstimatedDetailsPage: pages.has_estimated_details,
  taxDuePage: pages.tax_due,
  mandatoryEvidencePage: pages.upload_mandatory_evidence,
  mandatoryEvidenceUploadFailedPage: pages.upload_mandatory_evidence_failed,
  mandatoryEvidenceScanFailedPage: pages.upload_mandatory_evidence_scan_failed,
  calculatedCheckYouAnswersPage: pages.calculated_check_your_answers,
  taxableGainOrLossPage: pages.taxable_gain_or_loss,
  nonCalculatedEnterTaxDuePage: pages.non_calculated_enter_tax_due,
  furtherReturnCheckTaxDuePage: pages.further_return_check_tax_due,
  furtherReturnEnterTaxDuePage: pages.further_return_enter_tax_due,
  nonCalculatedCheckYourAnswersPage: pages.non_calculated_check_your_answers,
  expiredMandatoryEvidencePage: pages.expired_mandatory_evidence,
  mandatoryEvidenceScanProgressPage: pages.mandatory_evidence_scan_progress,
  furtherReturnsTaxableGainOrLossPage: pages.further_return_taxable_gain_or_loss,
  yearToDateLiabilityPage: pages.year_to_date_liability,
  repaymentPage: pages.repayment
)(implicit viewConfig: ViewConfig, ec: ExecutionContext)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with Logging
    with SessionUpdates {

  private def withFillingOutReturnAndYTDLiabilityAnswers(
    request: RequestWithSessionData[_]
  )(
    f: (
      SessionData,
      FillingOutReturn,
      YearToDateLiabilityAnswers
    ) => Future[Result]
  ): Future[Result] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {

      case Some((s, r: FillingOutReturn)) if r.isFurtherReturn.contains(true) =>
        r.draftReturn.yearToDateLiabilityAnswers.fold(
          f(s, r, IncompleteNonCalculatedYTDAnswers.empty)
        )(y => f(s, r, y))

      case Some(
            (s, r @ FillingOutReturn(_, _, _, d: DraftSingleDisposalReturn, _))
          ) =>
        d.yearToDateLiabilityAnswers match {
          case Some(y) => f(s, r, y)

          case None =>
            d.reliefDetailsAnswers match {
              case Some(
                    CompleteReliefDetailsAnswers(
                      _,
                      _,
                      Some(_: OtherReliefsOption.OtherReliefs)
                    )
                  ) =>
                f(s, r, IncompleteNonCalculatedYTDAnswers.empty)

              case Some(_: CompleteReliefDetailsAnswers) =>
                f(s, r, IncompleteCalculatedYTDAnswers.empty)

              case _                                     =>
                Redirect(
                  controllers.returns.routes.TaskListController.taskList()
                )
            }
        }

      case Some(
            (s, r @ FillingOutReturn(_, _, _, d: DraftMultipleDisposalsReturn, _))
          ) =>
        d.yearToDateLiabilityAnswers.fold[Future[Result]](
          f(s, r, IncompleteNonCalculatedYTDAnswers.empty)
        )(f(s, r, _))

      case Some(
            (
              s,
              r @ FillingOutReturn(
                _,
                _,
                _,
                d: DraftSingleIndirectDisposalReturn,
                _
              )
            )
          ) =>
        d.yearToDateLiabilityAnswers.fold[Future[Result]](
          f(s, r, IncompleteNonCalculatedYTDAnswers.empty)
        )(f(s, r, _))

      case Some(
            (
              s,
              r @ FillingOutReturn(
                _,
                _,
                _,
                d: DraftMultipleIndirectDisposalsReturn,
                _
              )
            )
          ) =>
        d.yearToDateLiabilityAnswers.fold[Future[Result]](
          f(s, r, IncompleteNonCalculatedYTDAnswers.empty)
        )(f(s, r, _))

      case Some(
            (
              s,
              r @ FillingOutReturn(
                _,
                _,
                _,
                d: DraftSingleMixedUseDisposalReturn,
                _
              )
            )
          ) =>
        d.yearToDateLiabilityAnswers.fold[Future[Result]](
          f(s, r, IncompleteNonCalculatedYTDAnswers.empty)
        )(f(s, r, _))

      case _                                                                  => Redirect(controllers.routes.StartController.start())
    }

  private def withCalculatedTaxDue(
    answers: CalculatedYTDAnswers,
    triageAnswers: CompleteSingleDisposalTriageAnswers,
    disposalDetailsAnswers: CompleteDisposalDetailsAnswers,
    acquisitionDetailsAnswers: CompleteAcquisitionDetailsAnswers,
    reliefDetailsAnswers: CompleteReliefDetailsAnswers,
    exemptionAndLossesAnswers: CompleteExemptionAndLossesAnswers,
    estimatedIncome: AmountInPence,
    personalAllowance: AmountInPence,
    calculateIfMissing: Boolean,
    fillingOutReturn: FillingOutReturn,
    draftReturn: DraftSingleDisposalReturn
  )(
    f: CalculatedTaxDue => Future[Result]
  )(implicit request: RequestWithSessionData[_]): Future[Result] =
    answers match {
      case complete: CompleteCalculatedYTDAnswers     =>
        f(complete.calculatedTaxDue)

      case incomplete: IncompleteCalculatedYTDAnswers =>
        incomplete.calculatedTaxDue match {
          case Some(c) => f(c)

          case None if !calculateIfMissing =>
            Redirect(routes.YearToDateLiabilityController.checkYourAnswers())

          case None if calculateIfMissing  =>
            val result =
              for {
                calculatedTaxDue <- cgtCalculationService
                                      .calculateTaxDue(
                                        CalculateCgtTaxDueRequest(
                                          triageAnswers,
                                          disposalDetailsAnswers,
                                          acquisitionDetailsAnswers,
                                          reliefDetailsAnswers,
                                          exemptionAndLossesAnswers,
                                          estimatedIncome,
                                          personalAllowance,
                                          draftReturn.initialGainOrLoss,
                                          isATrust(fillingOutReturn) || isPeriodOfAdmin(fillingOutReturn)
                                        )
                                      )
                _                <- EitherT(
                                      updateSession(sessionStore, request)(
                                        _.copy(
                                          journeyStatus = Some(
                                            fillingOutReturn.copy(draftReturn =
                                              draftReturn.copy(yearToDateLiabilityAnswers =
                                                Some(
                                                  incomplete.copy(calculatedTaxDue = Some(calculatedTaxDue))
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
              } yield calculatedTaxDue

            result
              .biSemiflatMap(
                { e =>
                  logger.warn("Could not get and store calculated tax due", e)
                  errorHandler.errorResult()
                },
                f
              )
              .merge
        }

    }

  private def withCompleteJourneys(draftReturn: DraftSingleDisposalReturn)(
    f: (
      CompleteSingleDisposalTriageAnswers,
      CompleteDisposalDetailsAnswers,
      CompleteAcquisitionDetailsAnswers,
      CompleteReliefDetailsAnswers,
      CompleteExemptionAndLossesAnswers
    ) => Future[Result]
  ): Future[Result] =
    (
      draftReturn.triageAnswers,
      draftReturn.disposalDetailsAnswers,
      draftReturn.acquisitionDetailsAnswers,
      draftReturn.reliefDetailsAnswers,
      draftReturn.exemptionAndLossesAnswers
    ) match {
      case (
            t: CompleteSingleDisposalTriageAnswers,
            Some(d: CompleteDisposalDetailsAnswers),
            Some(a: CompleteAcquisitionDetailsAnswers),
            Some(r: CompleteReliefDetailsAnswers),
            Some(e: CompleteExemptionAndLossesAnswers)
          ) =>
        f(t, d, a, r, e)

      case _ =>
        Redirect(controllers.returns.routes.TaskListController.taskList())
    }

  private def withAssetTypeAndResidentialStatus(
    draftReturn: DraftSingleDisposalReturn
  )(f: (AssetType, Boolean) => Future[Result]): Future[Result] =
    draftReturn.triageAnswers.fold(
      i => i.assetType -> i.wasAUKResident,
      c => Some(c.assetType) -> Some(c.countryOfResidence.isUk())
    ) match {
      case (Some(a), Some(w)) => f(a, w)
      case _                  =>
        Redirect(controllers.returns.routes.TaskListController.taskList())
    }

  private def withTaxYear(
    draftReturn: DraftReturn
  )(f: TaxYear => Future[Result]): Future[Result] =
    taxYear(draftReturn)
      .fold[Future[Result]](
        Redirect(controllers.returns.routes.TaskListController.taskList())
      )(f)

  private def taxYear(draftReturn: DraftReturn): Option[TaxYear] =
    draftReturn
      .triageAnswers()
      .fold(
        _.fold(_.taxYear, c => Some(c.taxYear)),
        _.fold(_.disposalDate.map(_.taxYear), c => Some(c.disposalDate.taxYear))
      )

  private def withEstimatedIncome(
    answers: CalculatedYTDAnswers
  )(f: AmountInPence => Future[Result]): Future[Result] =
    answers
      .fold(_.estimatedIncome, c => Some(c.estimatedIncome))
      .fold[Future[Result]](
        Redirect(routes.YearToDateLiabilityController.checkYourAnswers())
      )(f)

  private def withPersonalAllowance(
    answers: CalculatedYTDAnswers
  )(f: AmountInPence => Future[Result]): Future[Result] =
    withEstimatedIncome(answers) { estimatedIncome =>
      if (estimatedIncome.value > 0L)
        answers
          .fold(_.personalAllowance, c => c.personalAllowance)
          .fold[Future[Result]](
            Redirect(routes.YearToDateLiabilityController.checkYourAnswers())
          )(f)
      else
        f(AmountInPence.zero)
    }

  private def withHasEstimatedDetails(
    answers: YearToDateLiabilityAnswers
  )(f: Boolean => Future[Result]): Future[Result] =
    answers match {
      case _: NonCalculatedYTDAnswers              =>
        Redirect(routes.YearToDateLiabilityController.checkYourAnswers())

      case calculatedAnswers: CalculatedYTDAnswers =>
        calculatedAnswers
          .fold(_.hasEstimatedDetails, c => Some(c.hasEstimatedDetails))
          .fold[Future[Result]](
            Redirect(routes.YearToDateLiabilityController.checkYourAnswers())
          )(f)
    }

  private def withTaxDueAndCalculatedTaxDue(
    answers: CalculatedYTDAnswers
  )(f: (AmountInPence, CalculatedTaxDue) => Future[Result]): Future[Result] = {
    val taxDue           = answers.fold(_.taxDue, c => Some(c.taxDue))
    val calculatedTaxDue =
      answers.fold(_.calculatedTaxDue, c => Some(c.calculatedTaxDue))

    (taxDue, calculatedTaxDue) match {
      case (Some(t), Some(c)) => f(t, c)
      case _                  =>
        Redirect(routes.YearToDateLiabilityController.checkYourAnswers())
    }
  }

  private def commonDisplayBehaviour[
    Y <: YearToDateLiabilityAnswers,
    A,
    P : Writeable,
    R
  ](
    currentAnswers: Y
  )(form: Y => Form[A])(
    page: (Form[A], Call) => P
  )(
    requiredPreviousAnswer: Y => Option[R],
    redirectToIfNoRequiredPreviousAnswer: Call
  ): Future[Result] =
    if (requiredPreviousAnswer(currentAnswers).isDefined) {
      val backLink = currentAnswers match {
        case c: CalculatedYTDAnswers    =>
          c.fold(
            _ => redirectToIfNoRequiredPreviousAnswer,
            _ => routes.YearToDateLiabilityController.checkYourAnswers()
          )

        case n: NonCalculatedYTDAnswers =>
          n.fold(
            _ => redirectToIfNoRequiredPreviousAnswer,
            _ => routes.YearToDateLiabilityController.checkYourAnswers()
          )
      }

      Ok(page(form(currentAnswers), backLink))
    } else
      Redirect(redirectToIfNoRequiredPreviousAnswer)

  private def commonSubmitBehaviour[
    Y <: YearToDateLiabilityAnswers,
    D <: DraftReturn : Eq,
    A,
    P : Writeable,
    R
  ](
    currentFillingOutReturn: FillingOutReturn,
    currentDraftReturn: D,
    currentAnswers: Y
  )(form: Form[A])(
    page: (Form[A], Call) => P
  )(
    requiredPreviousAnswer: Y => Option[R],
    redirectToIfNoRequiredPreviousAnswer: Call
  )(
    updateAnswers: (A, D) => D
  )(implicit
    request: RequestWithSessionData[_]
  ): Future[Result] =
    if (requiredPreviousAnswer(currentAnswers).isDefined) {
      lazy val backLink = currentAnswers match {
        case c: CalculatedYTDAnswers    =>
          c.fold(
            _ => redirectToIfNoRequiredPreviousAnswer,
            _ => routes.YearToDateLiabilityController.checkYourAnswers()
          )

        case n: NonCalculatedYTDAnswers =>
          n.fold(
            _ => redirectToIfNoRequiredPreviousAnswer,
            _ => routes.YearToDateLiabilityController.checkYourAnswers()
          )
      }
      form
        .bindFromRequest()
        .fold(
          formWithErrors => BadRequest(page(formWithErrors, backLink)),
          { value =>
            val newDraftReturn = updateAnswers(value, currentDraftReturn)

            val result = for {
              _ <- if (newDraftReturn === currentDraftReturn)
                     EitherT.pure[Future, Error](())
                   else
                     returnsService.storeDraftReturn(
                       newDraftReturn,
                       currentFillingOutReturn.subscribedDetails.cgtReference,
                       currentFillingOutReturn.agentReferenceNumber
                     )
              _ <- EitherT(
                     updateSession(sessionStore, request)(
                       _.copy(journeyStatus =
                         Some(
                           currentFillingOutReturn
                             .copy(draftReturn = newDraftReturn)
                         )
                       )
                     )
                   )
            } yield ()
            result.fold(
              { e =>
                logger.warn("Could not update draft return", e)
                errorHandler.errorResult()
              },
              _ =>
                Redirect(
                  routes.YearToDateLiabilityController.checkYourAnswers()
                )
            )

          }
        )
    } else
      Redirect(redirectToIfNoRequiredPreviousAnswer)

  def estimatedIncome(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndYTDLiabilityAnswers(request) { (_, fillingOutReturn, answers) =>
        (answers, fillingOutReturn.draftReturn) match {
          case (_, _) if isATrust(fillingOutReturn) || isPeriodOfAdmin(fillingOutReturn) =>
            Redirect(routes.YearToDateLiabilityController.checkYourAnswers())
          case (
                calculatedAnswers: CalculatedYTDAnswers,
                draftReturn: DraftSingleDisposalReturn
              ) =>
            withAssetTypeAndResidentialStatus(draftReturn) { (_, wasUkResident) =>
              withTaxYear(draftReturn) { taxYear =>
                commonDisplayBehaviour(calculatedAnswers)(
                  form = _.fold(
                    _.estimatedIncome.fold(estimatedIncomeForm)(a => estimatedIncomeForm.fill(a.inPounds())),
                    c => estimatedIncomeForm.fill(c.estimatedIncome.inPounds())
                  )
                )(
                  page = estimatedIncomePage(
                    _,
                    _,
                    taxYear,
                    wasUkResident,
                    draftReturn.representativeType()
                  )
                )(
                  requiredPreviousAnswer = _ => Some(()),
                  controllers.returns.routes.TaskListController.taskList()
                )
              }
            }

          case _                                                                         =>
            Redirect(routes.YearToDateLiabilityController.checkYourAnswers())
        }
      }
    }

  def estimatedIncomeSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndYTDLiabilityAnswers(request) { (_, fillingOutReturn, answers) =>
        (answers, fillingOutReturn.draftReturn) match {
          case (_, _) if isATrust(fillingOutReturn) || isPeriodOfAdmin(fillingOutReturn) =>
            Redirect(routes.YearToDateLiabilityController.checkYourAnswers())
          case (
                calculatedAnswers: CalculatedYTDAnswers,
                draftReturn: DraftSingleDisposalReturn
              ) =>
            withAssetTypeAndResidentialStatus(draftReturn) { (_, wasUkResident) =>
              withTaxYear(draftReturn) { taxYear =>
                commonSubmitBehaviour(
                  fillingOutReturn,
                  draftReturn,
                  calculatedAnswers
                )(
                  form = estimatedIncomeForm
                )(
                  page = { (form, backLink) =>
                    estimatedIncomePage(
                      form,
                      backLink,
                      taxYear,
                      wasUkResident,
                      draftReturn.representativeType()
                    )
                  }
                )(
                  requiredPreviousAnswer = _ => Some(()),
                  redirectToIfNoRequiredPreviousAnswer = controllers.returns.routes.TaskListController.taskList()
                )(
                  updateAnswers = { (i, draftReturn) =>
                    val estimatedIncome = AmountInPence.fromPounds(i)

                    if (
                      calculatedAnswers
                        .fold(
                          _.estimatedIncome,
                          c => Some(c.estimatedIncome)
                        )
                        .contains(estimatedIncome)
                    )
                      draftReturn
                    else {
                      val newAnswers =
                        calculatedAnswers
                          .unset(_.personalAllowance)
                          .unset(_.hasEstimatedDetails)
                          .unset(_.calculatedTaxDue)
                          .unset(_.taxDue)
                          .unset(_.mandatoryEvidence)
                          .unset(_.expiredEvidence)
                          .unset(_.pendingUpscanUpload)
                          .copy(estimatedIncome = Some(AmountInPence.fromPounds(i)))

                      draftReturn.copy(yearToDateLiabilityAnswers = Some(newAnswers))
                    }
                  }
                )
              }
            }

          case _                                                                         =>
            Redirect(routes.YearToDateLiabilityController.checkYourAnswers())
        }
      }
    }

  def personalAllowance(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndYTDLiabilityAnswers(request) { (_, fillingOutReturn, answers) =>
        (answers, fillingOutReturn.draftReturn) match {
          case (_, _) if isATrust(fillingOutReturn) || isPeriodOfAdmin(fillingOutReturn) =>
            Redirect(routes.YearToDateLiabilityController.checkYourAnswers())
          case (
                calculatedAnswers: CalculatedYTDAnswers,
                draftReturn: DraftSingleDisposalReturn
              ) =>
            withAssetTypeAndResidentialStatus(draftReturn) { (_, wasUkResident) =>
              withTaxYear(draftReturn) { taxYear =>
                withEstimatedIncome(calculatedAnswers) { estimatedIncome =>
                  if (estimatedIncome.value > 0L)
                    commonDisplayBehaviour(calculatedAnswers)(
                      form = { a =>
                        val emptyForm = personalAllowanceForm(taxYear)
                        a.fold(
                          _.personalAllowance.fold(emptyForm)(a => emptyForm.fill(a.inPounds())),
                          _.personalAllowance.fold(emptyForm)(a => emptyForm.fill(a.inPounds()))
                        )
                      }
                    )(
                      page = personalAllowancePage(
                        _,
                        _,
                        taxYear,
                        wasUkResident,
                        draftReturn.representativeType()
                      )
                    )(
                      requiredPreviousAnswer = _.fold(
                        _.estimatedIncome,
                        c => Some(c.estimatedIncome)
                      ),
                      routes.YearToDateLiabilityController.estimatedIncome()
                    )
                  else
                    Redirect(
                      routes.YearToDateLiabilityController
                        .checkYourAnswers()
                    )
                }
              }
            }
          case _                                                                         =>
            Redirect(routes.YearToDateLiabilityController.checkYourAnswers())
        }
      }
    }

  def personalAllowanceSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndYTDLiabilityAnswers(request) { (_, fillingOutReturn, answers) =>
        (answers, fillingOutReturn.draftReturn) match {
          case (_, _) if isATrust(fillingOutReturn) || isPeriodOfAdmin(fillingOutReturn) =>
            Redirect(routes.YearToDateLiabilityController.checkYourAnswers())
          case (
                calculatedAnswers: CalculatedYTDAnswers,
                draftReturn: DraftSingleDisposalReturn
              ) =>
            withAssetTypeAndResidentialStatus(draftReturn) { (_, wasUkResident) =>
              withTaxYear(draftReturn) { taxYear =>
                withEstimatedIncome(calculatedAnswers) { estimatedIncome =>
                  if (estimatedIncome.value > 0L)
                    commonSubmitBehaviour(
                      fillingOutReturn,
                      draftReturn,
                      calculatedAnswers
                    )(
                      form = personalAllowanceForm(taxYear)
                    )(page =
                      (form, backLink) =>
                        personalAllowancePage(
                          form,
                          backLink,
                          taxYear,
                          wasUkResident,
                          draftReturn.representativeType()
                        )
                    )(
                      requiredPreviousAnswer = _.fold(
                        _.estimatedIncome,
                        c => Some(c.estimatedIncome)
                      ),
                      routes.YearToDateLiabilityController.estimatedIncome()
                    ) { (p, draftReturn) =>
                      val personalAllowance = AmountInPence.fromPounds(p)
                      if (
                        calculatedAnswers
                          .fold(_.personalAllowance, _.personalAllowance)
                          .contains(personalAllowance)
                      )
                        draftReturn
                      else {
                        val newAnswers = calculatedAnswers
                          .unset(_.hasEstimatedDetails)
                          .unset(_.calculatedTaxDue)
                          .unset(_.taxDue)
                          .unset(_.mandatoryEvidence)
                          .unset(_.expiredEvidence)
                          .unset(_.pendingUpscanUpload)
                          .copy(personalAllowance = Some(AmountInPence.fromPounds(p)))

                        draftReturn.copy(yearToDateLiabilityAnswers = Some(newAnswers))

                      }
                    }
                  else
                    Redirect(
                      routes.YearToDateLiabilityController
                        .checkYourAnswers()
                    )
                }
              }
            }
          case _                                                                         =>
            Redirect(routes.YearToDateLiabilityController.checkYourAnswers())
        }
      }
    }

  def hasEstimatedDetails(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndYTDLiabilityAnswers(request) { (_, fillingOutReturn, answers) =>
        (answers, fillingOutReturn.draftReturn) match {
          case (
                nonCalculatedAnswers: NonCalculatedYTDAnswers,
                _: DraftReturn
              ) =>
            commonDisplayBehaviour(nonCalculatedAnswers)(
              form = _.fold(
                _.hasEstimatedDetails.fold(hasEstimatedDetailsForm)(
                  hasEstimatedDetailsForm.fill
                ),
                c => hasEstimatedDetailsForm.fill(c.hasEstimatedDetails)
              )
            )(
              page = hasEstimatedDetailsPage(
                _,
                _,
                isATrust(fillingOutReturn),
                fillingOutReturn.draftReturn.representativeType(),
                fillingOutReturn.isFurtherReturn
              )
            )(
              requiredPreviousAnswer = answers =>
                answers.fold(
                  _.taxableGainOrLoss,
                  c => Some(c.taxableGainOrLoss)
                ),
              redirectToIfNoRequiredPreviousAnswer = routes.YearToDateLiabilityController.taxableGainOrLoss()
            )

          case (
                calculatedAnswers: CalculatedYTDAnswers,
                _: DraftSingleDisposalReturn
              ) if !isATrust(fillingOutReturn) && !isPeriodOfAdmin(fillingOutReturn) =>
            withEstimatedIncome(calculatedAnswers) { estimatedIncome =>
              displayForEstimatedDetails(
                fillingOutReturn,
                calculatedAnswers,
                Some(estimatedIncome)
              )
            }

          case (
                calculatedAnswers: CalculatedYTDAnswers,
                _: DraftSingleDisposalReturn
              ) if isATrust(fillingOutReturn) || isPeriodOfAdmin(fillingOutReturn) =>
            displayForEstimatedDetails(
              fillingOutReturn,
              calculatedAnswers,
              None
            )

          case _ =>
            Redirect(routes.YearToDateLiabilityController.checkYourAnswers())

        }
      }
    }

  private def displayForEstimatedDetails(
    fillingOutReturn: FillingOutReturn,
    calculatedAnswers: CalculatedYTDAnswers,
    estimatedIncome: Option[AmountInPence]
  )(implicit request: RequestWithSessionData[_]) =
    commonDisplayBehaviour(calculatedAnswers)(
      form = _.fold(
        _.hasEstimatedDetails
          .fold(hasEstimatedDetailsForm)(hasEstimatedDetailsForm.fill),
        c => hasEstimatedDetailsForm.fill(c.hasEstimatedDetails)
      )
    )(
      page = hasEstimatedDetailsPage(
        _,
        _,
        isATrust(fillingOutReturn),
        fillingOutReturn.draftReturn.representativeType(),
        fillingOutReturn.isFurtherReturn
      )
    )(
      requiredPreviousAnswer = { a =>
        estimatedIncome match {
          case None                      => Some(AmountInPence(0))
          case Some(ei) if ei.value > 0L =>
            a.fold(_.personalAllowance, _.personalAllowance)
          case _                         => a.fold(_.estimatedIncome, c => Some(c.estimatedIncome))
        }
      },
      redirectToIfNoRequiredPreviousAnswer = estimatedIncome match {
        case None                      => controllers.returns.routes.TaskListController.taskList()
        case Some(ei) if ei.value > 0L =>
          routes.YearToDateLiabilityController.personalAllowance()
        case _                         => routes.YearToDateLiabilityController.estimatedIncome()
      }
    )

  def hasEstimatedDetailsSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndYTDLiabilityAnswers(request) { (_, fillingOutReturn, answers) =>
        (answers, fillingOutReturn.draftReturn) match {
          case (
                nonCalculatedAnswers: NonCalculatedYTDAnswers,
                draftReturn: DraftReturn
              ) =>
            handleNonCalculatedEstimatedDetailsSubmit(
              nonCalculatedAnswers,
              draftReturn,
              fillingOutReturn
            )

          case (
                calculatedAnswers: CalculatedYTDAnswers,
                draftReturn: DraftSingleDisposalReturn
              ) =>
            handleNonCalculatedEstimatedDetailsSubmit(
              calculatedAnswers,
              draftReturn,
              fillingOutReturn
            )

          case _ =>
            Redirect(routes.YearToDateLiabilityController.checkYourAnswers())
        }
      }
    }

  private def handleNonCalculatedEstimatedDetailsSubmit(
    calculatedAnswers: CalculatedYTDAnswers,
    draftReturn: DraftSingleDisposalReturn,
    fillingOutReturn: FillingOutReturn
  )(implicit request: RequestWithSessionData[_]): Future[Result] =
    if (isATrust(fillingOutReturn) || isPeriodOfAdmin(fillingOutReturn))
      estimatedDetailsSubmit(
        calculatedAnswers,
        draftReturn,
        fillingOutReturn,
        None
      )
    else
      withEstimatedIncome(calculatedAnswers) { estimatedIncome =>
        estimatedDetailsSubmit(
          calculatedAnswers,
          draftReturn,
          fillingOutReturn,
          Some(estimatedIncome)
        )
      }

  private def estimatedDetailsSubmit(
    calculatedAnswers: CalculatedYTDAnswers,
    draftReturn: DraftSingleDisposalReturn,
    fillingOutReturn: FillingOutReturn,
    estimatedIncome: Option[AmountInPence]
  )(implicit request: RequestWithSessionData[_]) =
    commonSubmitBehaviour(fillingOutReturn, draftReturn, calculatedAnswers)(
      form = hasEstimatedDetailsForm
    )(
      page = hasEstimatedDetailsPage(
        _,
        _,
        isATrust(fillingOutReturn),
        draftReturn.representativeType(),
        fillingOutReturn.isFurtherReturn
      )
    )(
      requiredPreviousAnswer = { a =>
        estimatedIncome match {
          case None                      => Some(AmountInPence(0))
          case Some(ei) if ei.value > 0L =>
            a.fold(_.personalAllowance, _.personalAllowance)
          case _                         => a.fold(_.estimatedIncome, c => Some(c.estimatedIncome))
        }
      },
      redirectToIfNoRequiredPreviousAnswer = estimatedIncome match {
        case None                      => controllers.returns.routes.TaskListController.taskList()
        case Some(ei) if ei.value > 0L =>
          routes.YearToDateLiabilityController.personalAllowance()
        case _                         => routes.YearToDateLiabilityController.estimatedIncome()
      }
    ) { (hasEstimated, draftReturn) =>
      if (
        calculatedAnswers
          .fold(_.hasEstimatedDetails, c => Some(c.hasEstimatedDetails))
          .contains(hasEstimated)
      )
        draftReturn
      else {
        val newAnswers = calculatedAnswers
          .unset(_.calculatedTaxDue)
          .unset(_.taxDue)
          .unset(_.mandatoryEvidence)
          .unset(_.pendingUpscanUpload)
          .copy(hasEstimatedDetails = Some(hasEstimated))

        draftReturn.copy(yearToDateLiabilityAnswers = Some(newAnswers))
      }
    }

  private def handleNonCalculatedEstimatedDetailsSubmit(
    nonCalculatedAnswers: NonCalculatedYTDAnswers,
    draftReturn: DraftReturn,
    fillingOutReturn: FillingOutReturn
  )(implicit request: RequestWithSessionData[_]): Future[Result] =
    commonSubmitBehaviour(fillingOutReturn, draftReturn, nonCalculatedAnswers)(
      form = hasEstimatedDetailsForm
    )(
      page = hasEstimatedDetailsPage(
        _,
        _,
        isATrust(fillingOutReturn),
        draftReturn.representativeType(),
        fillingOutReturn.isFurtherReturn
      )
    )(
      requiredPreviousAnswer = answers => answers.fold(_.taxableGainOrLoss, c => Some(c.taxableGainOrLoss)),
      redirectToIfNoRequiredPreviousAnswer = routes.YearToDateLiabilityController.taxableGainOrLoss()
    ) { (hasEstimated, draftReturn) =>
      if (
        nonCalculatedAnswers
          .fold(_.hasEstimatedDetails, c => Some(c.hasEstimatedDetails))
          .contains(hasEstimated)
      )
        draftReturn
      else {
        val newAnswers =
          if (fillingOutReturn.isFurtherReturn.contains(true))
            nonCalculatedAnswers
              .unset(_.checkForRepayment)
              .unset(_.expiredEvidence)
              .unset(_.mandatoryEvidence)
              .unset(_.pendingUpscanUpload)
              .unset(_.taxDue)
              .unset(_.yearToDateLiability)
              .copy(hasEstimatedDetails = Some(hasEstimated))
          else
            nonCalculatedAnswers
              .unset(_.taxDue)
              .unset(_.mandatoryEvidence)
              .unset(_.expiredEvidence)
              .unset(_.pendingUpscanUpload)
              .copy(hasEstimatedDetails = Some(hasEstimated))

        updateDraftReturn(newAnswers, draftReturn)
      }
    }

  def taxDue(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndYTDLiabilityAnswers(request) { (_, fillingOutReturn, answers) =>
        (answers, fillingOutReturn.draftReturn) match {
          case (
                calculatedAnswers: CalculatedYTDAnswers,
                draftReturn: DraftSingleDisposalReturn
              ) if isATrust(fillingOutReturn) || isPeriodOfAdmin(fillingOutReturn) =>
            taxDueDisplay(
              fillingOutReturn,
              calculatedAnswers,
              draftReturn,
              AmountInPence(0),
              AmountInPence(0)
            )
          case (
                calculatedAnswers: CalculatedYTDAnswers,
                draftReturn: DraftSingleDisposalReturn
              ) =>
            withEstimatedIncome(calculatedAnswers) { estimatedIncome =>
              withPersonalAllowance(calculatedAnswers) { personalAllowance =>
                taxDueDisplay(
                  fillingOutReturn,
                  calculatedAnswers,
                  draftReturn,
                  estimatedIncome,
                  personalAllowance
                )
              }
            }

          case _ =>
            Redirect(routes.YearToDateLiabilityController.checkYourAnswers())

        }
      }
    }

  private def taxDueDisplay(
    fillingOutReturn: FillingOutReturn,
    calculatedAnswers: CalculatedYTDAnswers,
    draftReturn: DraftSingleDisposalReturn,
    estimatedIncome: AmountInPence,
    personalAllowance: AmountInPence
  )(implicit request: RequestWithSessionData[_]) =
    withHasEstimatedDetails(calculatedAnswers) { _ =>
      withCompleteJourneys(draftReturn) {
        (
          triage,
          disposalDetails,
          acquisitionDetails,
          reliefDetails,
          exemptionsAndLossesDetails
        ) =>
          withCalculatedTaxDue(
            calculatedAnswers,
            triage,
            disposalDetails,
            acquisitionDetails,
            reliefDetails,
            exemptionsAndLossesDetails,
            estimatedIncome,
            personalAllowance,
            calculateIfMissing = true,
            fillingOutReturn,
            draftReturn
          ) { calculatedTaxDue =>
            commonDisplayBehaviour(calculatedAnswers)(
              form = _.fold(
                _.taxDue.fold(taxDueForm)(t => taxDueForm.fill(t.inPounds())),
                c => taxDueForm.fill(c.taxDue.inPounds())
              )
            )(page =
              taxDuePage(
                _,
                _,
                triage,
                disposalDetails,
                acquisitionDetails,
                reliefDetails,
                exemptionsAndLossesDetails,
                estimatedIncome,
                personalAllowance,
                calculatedTaxDue,
                fillingOutReturn.subscribedDetails.isATrust,
                draftReturn.representativeType()
              )
            )(
              _.fold(
                _.hasEstimatedDetails,
                c => Some(c.hasEstimatedDetails)
              ),
              routes.YearToDateLiabilityController.hasEstimatedDetails()
            )
          }
      }
    }

  def taxDueSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndYTDLiabilityAnswers(request) { (_, fillingOutReturn, answers) =>
        (answers, fillingOutReturn.draftReturn) match {
          case (
                calculatedAnswers: CalculatedYTDAnswers,
                draftReturn: DraftSingleDisposalReturn
              ) if isATrust(fillingOutReturn) || isPeriodOfAdmin(fillingOutReturn) =>
            baseTaxDueSubmit(
              fillingOutReturn,
              calculatedAnswers,
              draftReturn,
              AmountInPence(0),
              AmountInPence(0)
            )
          case (
                calculatedAnswers: CalculatedYTDAnswers,
                draftReturn: DraftSingleDisposalReturn
              ) =>
            withEstimatedIncome(calculatedAnswers) { estimatedIncome =>
              withPersonalAllowance(calculatedAnswers) { personalAllowance =>
                baseTaxDueSubmit(
                  fillingOutReturn,
                  calculatedAnswers,
                  draftReturn,
                  estimatedIncome,
                  personalAllowance
                )
              }
            }

          case _ =>
            Redirect(routes.YearToDateLiabilityController.checkYourAnswers())

        }
      }
    }

  private def baseTaxDueSubmit(
    fillingOutReturn: FillingOutReturn,
    calculatedAnswers: CalculatedYTDAnswers,
    draftReturn: DraftSingleDisposalReturn,
    estimatedIncome: AmountInPence,
    personalAllowance: AmountInPence
  )(implicit request: RequestWithSessionData[_]) =
    withHasEstimatedDetails(calculatedAnswers) { _ =>
      withCompleteJourneys(draftReturn) {
        (
          triage,
          disposalDetails,
          acquisitionDetails,
          reliefDetails,
          exemptionsAndLossesDetails
        ) =>
          withCalculatedTaxDue(
            calculatedAnswers,
            triage,
            disposalDetails,
            acquisitionDetails,
            reliefDetails,
            exemptionsAndLossesDetails,
            estimatedIncome,
            personalAllowance,
            calculateIfMissing = false,
            fillingOutReturn,
            draftReturn
          ) { calculatedTaxDue =>
            commonSubmitBehaviour(
              fillingOutReturn,
              draftReturn,
              calculatedAnswers
            )(
              form = taxDueForm
            )(page =
              taxDuePage(
                _,
                _,
                triage,
                disposalDetails,
                acquisitionDetails,
                reliefDetails,
                exemptionsAndLossesDetails,
                estimatedIncome,
                personalAllowance,
                calculatedTaxDue,
                fillingOutReturn.subscribedDetails.isATrust,
                draftReturn.representativeType()
              )
            )(
              _.fold(
                _.hasEstimatedDetails,
                c => Some(c.hasEstimatedDetails)
              ),
              routes.YearToDateLiabilityController.hasEstimatedDetails()
            ) { (t, draftReturn) =>
              val taxDue = AmountInPence.fromPounds(t)
              if (
                calculatedAnswers
                  .fold(_.taxDue, c => Some(c.taxDue))
                  .contains(taxDue)
              )
                draftReturn
              else {
                val updatedAnswers =
                  calculatedAnswers
                    .unset(_.mandatoryEvidence)
                    .unset(_.expiredEvidence)
                    .unset(_.pendingUpscanUpload)
                    .copy(taxDue = Some(taxDue))
                draftReturn.copy(yearToDateLiabilityAnswers = Some(updatedAnswers))
              }
            }
          }
      }
    }

  def uploadMandatoryEvidence(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndYTDLiabilityAnswers(request) { (_, fillingOutReturn, answers) =>
        def commonDisposalMandatoryEvidence(
          backLink: Call
        ): Future[Result] = {
          val result = for {
            upscanUpload <- upscanService
                              .initiate(
                                routes.YearToDateLiabilityController
                                  .uploadMandatoryEvidenceFailure(),
                                _ =>
                                  routes.YearToDateLiabilityController
                                    .scanningMandatoryEvidence()
                              )
            _            <- updatePendingUpscanUpload(
                              answers,
                              fillingOutReturn,
                              upscanUpload
                            )
          } yield upscanUpload

          result.fold(
            { e =>
              logger.warn("Could not initiate upscan", e)
              errorHandler.errorResult()
            },
            upscanUpload => Ok(mandatoryEvidencePage(upscanUpload, backLink, fillingOutReturn.isFurtherReturn))
          )
        }

        (answers, fillingOutReturn.draftReturn) match {
          case (
                calculatedAnswers: CalculatedYTDAnswers,
                _: DraftSingleDisposalReturn
              ) =>
            withTaxDueAndCalculatedTaxDue(calculatedAnswers) { (taxDue, calculatedTaxDue) =>
              if (calculatedTaxDue.amountOfTaxDue === taxDue)
                Redirect(
                  routes.YearToDateLiabilityController.checkYourAnswers()
                )
              else {
                val backLink = calculatedAnswers.fold(
                  _ => routes.YearToDateLiabilityController.taxDue(),
                  _ => routes.YearToDateLiabilityController.checkYourAnswers()
                )
                commonDisposalMandatoryEvidence(backLink)
              }
            }

          case (nonCalculatedYTDAnswers: NonCalculatedYTDAnswers, _) =>
            val backLink = nonCalculatedYTDAnswers.fold(
              _ =>
                if (fillingOutReturn.isFurtherReturn.contains(true))
                  routes.YearToDateLiabilityController.repayment()
                else
                  routes.YearToDateLiabilityController.nonCalculatedEnterTaxDue(),
              _ => routes.YearToDateLiabilityController.checkYourAnswers()
            )
            commonDisposalMandatoryEvidence(backLink)

          case _                                                     =>
            Redirect(routes.YearToDateLiabilityController.checkYourAnswers())

        }
      }
    }

  private def updatePendingUpscanUpload(
    currentAnswers: YearToDateLiabilityAnswers,
    currentJourney: FillingOutReturn,
    upscanUpload: UpscanUpload
  )(implicit
    request: RequestWithSessionData[_]
  ): EitherT[Future, Error, Unit] = {
    val newAnswers = currentAnswers match {
      case c: CalculatedYTDAnswers    =>
        c.unset(_.expiredEvidence)
          .unset(_.mandatoryEvidence)
          .copy(pendingUpscanUpload = Some(upscanUpload))
      case n: NonCalculatedYTDAnswers =>
        n.unset(_.expiredEvidence)
          .unset(_.mandatoryEvidence)
          .copy(pendingUpscanUpload = Some(upscanUpload))
    }

    val newDraftReturn = updateDraftReturn(newAnswers, currentJourney.draftReturn)

    for {
      _ <- returnsService.storeDraftReturn(
             newDraftReturn,
             currentJourney.subscribedDetails.cgtReference,
             currentJourney.agentReferenceNumber
           )
      _ <- EitherT(
             updateSession(sessionStore, request)(
               _.copy(journeyStatus = Some(currentJourney.copy(draftReturn = newDraftReturn)))
             )
           )
    } yield ()
  }

  def taxableGainOrLoss(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndYTDLiabilityAnswers(request) { (_, fillingOutReturn, answers) =>
        answers match {
          case (_: CalculatedYTDAnswers)                       =>
            Redirect(routes.YearToDateLiabilityController.checkYourAnswers())

          case (nonCalculatedAnswers: NonCalculatedYTDAnswers) =>
            withTaxYear(fillingOutReturn.draftReturn) { taxYear =>
              commonDisplayBehaviour(
                nonCalculatedAnswers
              )(form =
                _.fold(
                  _.taxableGainOrLoss.fold(taxableGainOrLossForm)(a => taxableGainOrLossForm.fill(a.inPounds())),
                  c => taxableGainOrLossForm.fill(c.taxableGainOrLoss.inPounds())
                )
              )(
                page =
                  if (fillingOutReturn.isFurtherReturn.contains(true))
                    furtherReturnsTaxableGainOrLossPage(
                      _,
                      _,
                      fillingOutReturn.subscribedDetails.isATrust,
                      fillingOutReturn.draftReturn.representativeType(),
                      taxYear
                    )
                  else
                    taxableGainOrLossPage(
                      _,
                      _,
                      fillingOutReturn.subscribedDetails.isATrust,
                      fillingOutReturn.draftReturn match {
                        case _: DraftMultipleDisposalsReturn         => true
                        case _: DraftSingleDisposalReturn            => false
                        case _: DraftSingleIndirectDisposalReturn    => false
                        case _: DraftMultipleIndirectDisposalsReturn => true
                        case _: DraftSingleMixedUseDisposalReturn    => false
                      },
                      fillingOutReturn.draftReturn.representativeType()
                    )
              )(
                requiredPreviousAnswer = _ => Some(()),
                redirectToIfNoRequiredPreviousAnswer = controllers.returns.routes.TaskListController.taskList()
              )
            }
        }
      }
    }

  def taxableGainOrLossSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndYTDLiabilityAnswers(request) { (_, fillingOutReturn, answers) =>
        answers match {
          case _: CalculatedYTDAnswers                       =>
            Redirect(routes.YearToDateLiabilityController.checkYourAnswers())

          case nonCalculatedAnswers: NonCalculatedYTDAnswers =>
            withTaxYear(fillingOutReturn.draftReturn) { taxYear =>
              commonSubmitBehaviour(
                fillingOutReturn,
                fillingOutReturn.draftReturn,
                nonCalculatedAnswers
              )(form = taxableGainOrLossForm)(
                page =
                  if (fillingOutReturn.isFurtherReturn.contains(true))
                    furtherReturnsTaxableGainOrLossPage(
                      _,
                      _,
                      fillingOutReturn.subscribedDetails.isATrust,
                      fillingOutReturn.draftReturn.representativeType(),
                      taxYear
                    )
                  else
                    taxableGainOrLossPage(
                      _,
                      _,
                      fillingOutReturn.subscribedDetails.isATrust,
                      fillingOutReturn.draftReturn match {
                        case _: DraftMultipleDisposalsReturn         => true
                        case _: DraftSingleDisposalReturn            => false
                        case _: DraftSingleIndirectDisposalReturn    => false
                        case _: DraftMultipleIndirectDisposalsReturn => true
                        case _: DraftSingleMixedUseDisposalReturn    => false
                      },
                      fillingOutReturn.draftReturn.representativeType()
                    )
              )(
                requiredPreviousAnswer = _ => Some(()),
                redirectToIfNoRequiredPreviousAnswer = controllers.returns.routes.TaskListController.taskList()
              ) { (amount, draftReturn) =>
                val taxableGainOrLoss = AmountInPence.fromPounds(amount)
                if (
                  nonCalculatedAnswers
                    .fold(_.taxableGainOrLoss, c => Some(c.taxableGainOrLoss))
                    .contains(taxableGainOrLoss)
                )
                  draftReturn
                else {
                  val newAnswers =
                    if (fillingOutReturn.isFurtherReturn.contains(true))
                      nonCalculatedAnswers
                        .unset(_.hasEstimatedDetails)
                        .unset(_.yearToDateLiability)
                        .unset(_.taxDue)
                        .unset(_.checkForRepayment)
                        .unset(_.mandatoryEvidence)
                        .copy(taxableGainOrLoss = Some(taxableGainOrLoss))
                    else
                      nonCalculatedAnswers
                        .unset(_.hasEstimatedDetails)
                        .unset(_.taxDue)
                        .unset(_.mandatoryEvidence)
                        .unset(_.expiredEvidence)
                        .unset(_.pendingUpscanUpload)
                        .copy(taxableGainOrLoss = Some(taxableGainOrLoss))

                  updateDraftReturn(newAnswers, draftReturn)
                }
              }
            }
        }
      }
    }

  def nonCalculatedEnterTaxDue(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndYTDLiabilityAnswers(request) { (_, fillingOutReturn, answers) =>
        answers match {
          case _: CalculatedYTDAnswers                       =>
            Redirect(routes.YearToDateLiabilityController.checkYourAnswers())

          case nonCalculatedAnswers: NonCalculatedYTDAnswers =>
            (fillingOutReturn.previousSentReturns, fillingOutReturn.draftReturn.representativeType()) match {
              case (Some(PreviousReturnData(_, Some(previousYtd))), None) =>
                nonCalculatedAnswers.fold(_.yearToDateLiability, _.yearToDateLiability) match {
                  case None                      =>
                    Redirect(routes.YearToDateLiabilityController.checkYourAnswers())

                  case Some(yearToDateLiability) =>
                    if (nonCalculatedAnswers.fold(_.hasEstimatedDetails, c => Some(c.hasEstimatedDetails)).isEmpty)
                      Redirect(routes.YearToDateLiabilityController.hasEstimatedDetails())
                    else
                      Ok(
                        furtherReturnCheckTaxDuePage(
                          routes.YearToDateLiabilityController.hasEstimatedDetails(),
                          yearToDateLiability,
                          previousYtd,
                          fillingOutReturn.subscribedDetails.isATrust
                        )
                      )
                }

              case _                                                      =>
                withTaxYear(fillingOutReturn.draftReturn) { taxYear =>
                  commonDisplayBehaviour(
                    nonCalculatedAnswers
                  )(form =
                    _.fold(
                      _.taxDue.fold(nonCalculatedTaxDueForm)(a => nonCalculatedTaxDueForm.fill(a.inPounds())),
                      c => nonCalculatedTaxDueForm.fill(c.taxDue.inPounds())
                    )
                  )(
                    page =
                      if (fillingOutReturn.isFurtherReturn.contains(true))
                        furtherReturnEnterTaxDuePage(
                          _,
                          _,
                          taxYear,
                          fillingOutReturn.subscribedDetails.isATrust,
                          fillingOutReturn.draftReturn.representativeType
                        )
                      else
                        nonCalculatedEnterTaxDuePage(_, _)
                  )(
                    requiredPreviousAnswer = _.fold(_.hasEstimatedDetails, c => Some(c.hasEstimatedDetails)),
                    redirectToIfNoRequiredPreviousAnswer = routes.YearToDateLiabilityController.hasEstimatedDetails()
                  )
                }
            }

        }
      }
    }

  def nonCalculatedEnterTaxDueSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndYTDLiabilityAnswers(request) { (_, fillingOutReturn, answers) =>
        answers match {
          case _: CalculatedYTDAnswers                       =>
            Redirect(routes.YearToDateLiabilityController.checkYourAnswers())

          case nonCalculatedAnswers: NonCalculatedYTDAnswers =>
            (fillingOutReturn.previousSentReturns, fillingOutReturn.draftReturn.representativeType()) match {
              case (Some(PreviousReturnData(_, Some(previousYtd))), None) =>
                nonCalculatedAnswers.fold(_.yearToDateLiability, _.yearToDateLiability) match {
                  case None                      =>
                    Redirect(routes.YearToDateLiabilityController.checkYourAnswers())

                  case Some(yearToDateLiability) =>
                    handledConfirmFurtherReturnTaxDue(
                      nonCalculatedAnswers,
                      previousYtd,
                      yearToDateLiability,
                      fillingOutReturn
                    )
                }

              case _                                                      =>
                withTaxYear(fillingOutReturn.draftReturn) { taxYear =>
                  commonSubmitBehaviour(
                    fillingOutReturn,
                    fillingOutReturn.draftReturn,
                    nonCalculatedAnswers
                  )(form = nonCalculatedTaxDueForm)(
                    page =
                      if (fillingOutReturn.isFurtherReturn.contains(true))
                        furtherReturnEnterTaxDuePage(
                          _,
                          _,
                          taxYear,
                          fillingOutReturn.subscribedDetails.isATrust,
                          fillingOutReturn.draftReturn.representativeType
                        )
                      else
                        nonCalculatedEnterTaxDuePage(_, _)
                  )(
                    requiredPreviousAnswer = _.fold(
                      _.hasEstimatedDetails,
                      c => Some(c.hasEstimatedDetails)
                    ),
                    redirectToIfNoRequiredPreviousAnswer = routes.YearToDateLiabilityController.hasEstimatedDetails()
                  ) { (amount, draftReturn) =>
                    val taxDue = AmountInPence.fromPounds(amount)

                    if (
                      nonCalculatedAnswers
                        .fold(_.taxDue, c => Some(c.taxDue))
                        .contains(taxDue)
                    )
                      draftReturn
                    else {
                      val newAnswers = nonCalculatedAnswers
                        .unset(_.mandatoryEvidence)
                        .unset(_.expiredEvidence)
                        .unset(_.pendingUpscanUpload)
                        .copy(taxDue = Some(taxDue))

                      updateDraftReturn(newAnswers, draftReturn)
                    }
                  }
                }
            }
        }
      }
    }

  private def handledConfirmFurtherReturnTaxDue(
    nonCalculatedAnswers: NonCalculatedYTDAnswers,
    previousYearToDateLiability: AmountInPence,
    yearToDateLiability: AmountInPence,
    fillingOutReturn: FillingOutReturn
  )(implicit request: RequestWithSessionData[_]): Future[Result] =
    if (nonCalculatedAnswers.fold(_.hasEstimatedDetails, c => Some(c.hasEstimatedDetails)).isEmpty)
      Redirect(routes.YearToDateLiabilityController.hasEstimatedDetails())
    else {
      val taxDue = (yearToDateLiability -- previousYearToDateLiability: AmountInPence).withFloorZero
      if (nonCalculatedAnswers.fold(_.taxDue, c => Some(c.taxDue)).contains(taxDue))
        Redirect(routes.YearToDateLiabilityController.checkYourAnswers())
      else {
        val newAnswers = {
          if (fillingOutReturn.isFurtherReturn.contains(true))
            nonCalculatedAnswers
              .unset(_.checkForRepayment)
              .unset(_.mandatoryEvidence)
              .unset(_.expiredEvidence)
              .unset(_.pendingUpscanUpload)
              .copy(taxDue = Some(taxDue))
          else
            nonCalculatedAnswers
              .unset(_.mandatoryEvidence)
              .unset(_.expiredEvidence)
              .unset(_.pendingUpscanUpload)
              .copy(taxDue = Some(taxDue))
        }

        val newDraftReturn = updateDraftReturn(newAnswers, fillingOutReturn.draftReturn)
        val result         = for {
          _ <- returnsService.storeDraftReturn(
                 newDraftReturn,
                 fillingOutReturn.subscribedDetails.cgtReference,
                 fillingOutReturn.agentReferenceNumber
               )
          _ <- EitherT(
                 updateSession(sessionStore, request)(
                   _.copy(journeyStatus = Some(fillingOutReturn.copy(draftReturn = newDraftReturn)))
                 )
               )
        } yield ()

        result.fold(
          { e =>
            logger.warn("Could not store draft return or update session", e)
            errorHandler.errorResult()
          },
          _ => Redirect(routes.YearToDateLiabilityController.checkYourAnswers())
        )
      }

    }

  def mandatoryEvidenceExpired(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndYTDLiabilityAnswers(request) { (_, fillingOutReturn, answers) =>
        answers match {
          case IncompleteNonCalculatedYTDAnswers(
                _,
                _,
                _,
                _,
                Some(expired),
                _,
                _,
                _
              ) =>
            Ok(expiredMandatoryEvidencePage(expired, fillingOutReturn.isFurtherReturn))

          case IncompleteCalculatedYTDAnswers(
                _,
                _,
                _,
                _,
                _,
                _,
                Some(expired),
                _
              ) =>
            Ok(expiredMandatoryEvidencePage(expired, fillingOutReturn.isFurtherReturn))

          case _ =>
            Redirect(routes.YearToDateLiabilityController.checkYourAnswers())
        }
      }
    }

  def scanningMandatoryEvidence(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndYTDLiabilityAnswers(request) { (_, fillingOutReturn, answers) =>
        def checkAndHandleUpscanStatus(
          pendingUpscanUpload: UpscanUpload,
          answers: Either[
            IncompleteNonCalculatedYTDAnswers,
            IncompleteCalculatedYTDAnswers
          ]
        ): Future[Result] = {
          val result = for {
            newUpscanUpload <- upscanService.getUpscanUpload(
                                 pendingUpscanUpload.uploadReference
                               )
            _               <- newUpscanUpload.upscanCallBack match {
                                 case None           => EitherT.pure[Future, Error](())
                                 case Some(callback) =>
                                   storeUpscanSuccessOrFailure(
                                     newUpscanUpload,
                                     callback,
                                     answers,
                                     fillingOutReturn
                                   )
                               }
          } yield newUpscanUpload

          result.fold(
            { e =>
              logger.warn(
                "Error while trying to get and handle an upscan status",
                e
              )
              errorHandler.errorResult()
            },
            upscanUpload => handleUpscanCallback(upscanUpload.upscanCallBack)
          )
        }

        def handleUpscanCallback(
          upscanCallBack: Option[UpscanCallBack]
        ): Result =
          upscanCallBack match {
            case None                   => Ok(mandatoryEvidenceScanProgressPage(fillingOutReturn.isFurtherReturn))
            case Some(_: UpscanFailure) => Ok(mandatoryEvidenceScanFailedPage())
            case Some(_: UpscanSuccess) =>
              Redirect(
                routes.YearToDateLiabilityController.checkYourAnswers()
              )
          }

        answers match {
          case n @ IncompleteNonCalculatedYTDAnswers(
                _,
                _,
                _,
                _,
                _,
                Some(pendingUpscanUpload),
                _,
                _
              ) =>
            if (pendingUpscanUpload.upscanCallBack.nonEmpty)
              handleUpscanCallback(pendingUpscanUpload.upscanCallBack)
            else checkAndHandleUpscanStatus(pendingUpscanUpload, Left(n))

          case c @ IncompleteCalculatedYTDAnswers(
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                Some(pendingUpscanUpload)
              ) =>
            if (pendingUpscanUpload.upscanCallBack.nonEmpty)
              handleUpscanCallback(pendingUpscanUpload.upscanCallBack)
            else checkAndHandleUpscanStatus(pendingUpscanUpload, Right(c))

          case _ =>
            Redirect(routes.YearToDateLiabilityController.checkYourAnswers())
        }
      }
    }

  def scanningMandatoryEvidenceSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData { _ =>
      Redirect(routes.YearToDateLiabilityController.scanningMandatoryEvidence())
    }

  def documentDidNotUpload(): Action[AnyContent] =
    authenticatedActionWithSessionData.async(implicit request => Ok(mandatoryEvidenceUploadFailedPage()))

  def handleUpscanCallBackFailures(): Action[AnyContent] =
    authenticatedActionWithSessionData(implicit request => Ok(mandatoryEvidenceUploadFailedPage()))

  def uploadMandatoryEvidenceFailure(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndYTDLiabilityAnswers(request) { (_, fillingOutReturn, answers) =>
        def handlePendingUpscanUpload(
          answers: Either[
            IncompleteNonCalculatedYTDAnswers,
            IncompleteCalculatedYTDAnswers
          ]
        ): Future[Result] =
          removePendingUpscanUpload(answers, fillingOutReturn).fold(
            { e =>
              logger.warn("Could not update session", e)
              errorHandler.errorResult()
            },
            _ => Redirect(routes.YearToDateLiabilityController.documentDidNotUpload())
          )

        answers match {
          case n: IncompleteNonCalculatedYTDAnswers if n.pendingUpscanUpload.isDefined =>
            handlePendingUpscanUpload(Left(n))
          case c: IncompleteCalculatedYTDAnswers if c.pendingUpscanUpload.isDefined    =>
            handlePendingUpscanUpload(Right(c))
          case _                                                                       =>
            Redirect(routes.YearToDateLiabilityController.checkYourAnswers())
        }
      }
    }

  def yearToDateLiability(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndYTDLiabilityAnswers(request) { (_, fillingOutReturn, answers) =>
        (fillingOutReturn, answers) match {
          case (f, nonCalculatedAnswers: NonCalculatedYTDAnswers) if f.isFurtherReturn.contains(true) =>
            withTaxYear(fillingOutReturn.draftReturn) { taxYear =>
              commonDisplayBehaviour(
                nonCalculatedAnswers
              )(form =
                _.fold(_.yearToDateLiability, _.yearToDateLiability)
                  .fold(yearToDateLiabilityForm(taxYear))(a => yearToDateLiabilityForm(taxYear).fill(a.inPounds()))
              )(
                page = yearToDateLiabilityPage(
                  _,
                  _,
                  taxYear,
                  fillingOutReturn.subscribedDetails.isATrust,
                  fillingOutReturn.draftReturn.representativeType()
                )
              )(
                requiredPreviousAnswer = _.fold(_.hasEstimatedDetails, c => Some(c.hasEstimatedDetails)),
                redirectToIfNoRequiredPreviousAnswer = routes.YearToDateLiabilityController.hasEstimatedDetails()
              )
            }

          case _                                                                                      =>
            Redirect(routes.YearToDateLiabilityController.checkYourAnswers())

        }
      }

    }

  def yearToDateLiabilitySubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndYTDLiabilityAnswers(request) { (_, fillingOutReturn, answers) =>
        (fillingOutReturn, answers) match {
          case (f, nonCalculatedAnswers: NonCalculatedYTDAnswers) if f.isFurtherReturn.contains(true) =>
            withTaxYear(fillingOutReturn.draftReturn) { taxYear =>
              commonSubmitBehaviour(
                fillingOutReturn,
                fillingOutReturn.draftReturn,
                nonCalculatedAnswers
              )(form = yearToDateLiabilityForm(taxYear))(
                page = yearToDateLiabilityPage(
                  _,
                  _,
                  taxYear,
                  fillingOutReturn.subscribedDetails.isATrust,
                  fillingOutReturn.draftReturn.representativeType()
                )
              )(
                requiredPreviousAnswer = _.fold(_.hasEstimatedDetails, c => Some(c.hasEstimatedDetails)),
                redirectToIfNoRequiredPreviousAnswer = routes.YearToDateLiabilityController.hasEstimatedDetails()
              ) { (amount, draftReturn) =>
                val yearToDateLiability = AmountInPence.fromPounds(amount)
                if (
                  nonCalculatedAnswers
                    .fold(_.yearToDateLiability, _.yearToDateLiability)
                    .contains(yearToDateLiability)
                )
                  draftReturn
                else {
                  val newAnswers =
                    nonCalculatedAnswers
                      .unset(_.yearToDateLiability)
                      .unset(_.mandatoryEvidence)
                      .unset(_.taxDue)
                      .copy(yearToDateLiability = Some(yearToDateLiability))

                  updateDraftReturn(newAnswers, draftReturn)
                }
              }
            }

          case _                                                                                      =>
            Redirect(routes.YearToDateLiabilityController.checkYourAnswers())
        }
      }
    }

  def repayment(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndYTDLiabilityAnswers(request) { (_, fillingOutReturn, answers) =>
        (fillingOutReturn, answers) match {
          case (f, nonCalculatedAnswers: NonCalculatedYTDAnswers) if f.isFurtherReturn.contains(true) =>
            commonDisplayBehaviour(
              nonCalculatedAnswers
            )(form =
              _.fold(_.checkForRepayment, _.checkForRepayment)
                .fold(repaymentForm)(repaymentForm.fill)
            )(
              page = repaymentPage(
                _,
                _,
                fillingOutReturn.subscribedDetails.isATrust,
                fillingOutReturn.draftReturn.representativeType()
              )
            )(
              requiredPreviousAnswer = _.fold(_.taxDue, c => Some(c.taxDue)),
              redirectToIfNoRequiredPreviousAnswer = routes.YearToDateLiabilityController.taxDue()
            )

          case _                                                                                      =>
            Redirect(routes.YearToDateLiabilityController.checkYourAnswers())

        }
      }

    }

  def repaymentSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndYTDLiabilityAnswers(request) { (_, fillingOutReturn, answers) =>
        (fillingOutReturn, answers) match {
          case (f, nonCalculatedAnswers: NonCalculatedYTDAnswers) if f.isFurtherReturn.contains(true) =>
            commonSubmitBehaviour(
              fillingOutReturn,
              fillingOutReturn.draftReturn,
              nonCalculatedAnswers
            )(form = repaymentForm)(
              page = repaymentPage(
                _,
                _,
                fillingOutReturn.subscribedDetails.isATrust,
                fillingOutReturn.draftReturn.representativeType()
              )
            )(
              requiredPreviousAnswer = _.fold(_.taxDue, c => Some(c.taxDue)),
              redirectToIfNoRequiredPreviousAnswer = routes.YearToDateLiabilityController.taxDue()
            ) { (checkForRepayment, draftReturn) =>
              if (
                nonCalculatedAnswers
                  .fold(_.checkForRepayment, _.checkForRepayment)
                  .contains(checkForRepayment)
              )
                draftReturn
              else {
                val newAnswers = nonCalculatedAnswers
                  .unset(_.checkForRepayment)
                  .copy(checkForRepayment = Some(checkForRepayment))

                updateDraftReturn(newAnswers, draftReturn)
              }
            }

          case _                                                                                      =>
            Redirect(routes.YearToDateLiabilityController.checkYourAnswers())
        }
      }
    }

  private def removePendingUpscanUpload(
    answers: Either[
      IncompleteNonCalculatedYTDAnswers,
      IncompleteCalculatedYTDAnswers
    ],
    fillingOutReturn: FillingOutReturn
  )(implicit
    request: RequestWithSessionData[_]
  ): EitherT[Future, Error, Unit] = {
    val newAnswers     = answers.fold(
      _.copy(pendingUpscanUpload = None),
      _.copy(pendingUpscanUpload = None)
    )
    val newDraftReturn = updateDraftReturn(newAnswers, fillingOutReturn.draftReturn)

    for {
      _ <- returnsService.storeDraftReturn(
             newDraftReturn,
             fillingOutReturn.subscribedDetails.cgtReference,
             fillingOutReturn.agentReferenceNumber
           )
      _ <- EitherT(
             updateSession(sessionStore, request)(
               _.copy(journeyStatus = Some(fillingOutReturn.copy(draftReturn = newDraftReturn)))
             )
           )
    } yield ()
  }

  private def storeUpscanSuccessOrFailure(
    upscanUpload: UpscanUpload,
    upscanCallBack: UpscanCallBack,
    answers: Either[
      IncompleteNonCalculatedYTDAnswers,
      IncompleteCalculatedYTDAnswers
    ],
    fillingOutReturn: FillingOutReturn
  )(implicit
    request: RequestWithSessionData[_],
    hc: HeaderCarrier
  ): EitherT[Future, Error, Unit] = {
    val newAnswers =
      upscanCallBack match {
        case success: UpscanSuccess =>
          val mandatoryEvidence =
            MandatoryEvidence(
              upscanUpload.uploadReference,
              upscanUpload.upscanUploadMeta,
              upscanUpload.uploadedOn,
              success,
              success.fileName
            )
          answers.fold(
            _.unset(_.pendingUpscanUpload)
              .copy(mandatoryEvidence = Some(mandatoryEvidence)),
            _.unset(_.pendingUpscanUpload)
              .copy(mandatoryEvidence = Some(mandatoryEvidence))
          )

        case _: UpscanFailure       =>
          answers.fold(
            _.copy(pendingUpscanUpload = Some(upscanUpload)),
            _.copy(pendingUpscanUpload = Some(upscanUpload))
          )

      }

    val newDraftReturn = updateDraftReturn(newAnswers, fillingOutReturn.draftReturn)

    for {
      _ <- returnsService.storeDraftReturn(
             newDraftReturn,
             fillingOutReturn.subscribedDetails.cgtReference,
             fillingOutReturn.agentReferenceNumber
           )
      _ <- EitherT(
             updateSession(sessionStore, request)(
               _.copy(journeyStatus =
                 Some(
                   fillingOutReturn.copy(draftReturn = newDraftReturn)
                 )
               )
             )
           )
    } yield ()
  }

  def checkYourAnswers(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndYTDLiabilityAnswers(request) { (_, fillingOutReturn, answers) =>
        (answers, fillingOutReturn.draftReturn) match {
          case (c: CalculatedYTDAnswers, s: DraftSingleDisposalReturn)         =>
            withAssetTypeAndResidentialStatus(s)((_, wasUkResident) =>
              withTaxYear(s)(taxYear =>
                checkYourAnswersHandleCalculated(
                  c,
                  fillingOutReturn,
                  s,
                  taxYear,
                  wasUkResident
                )
              )
            )

          case (n: NonCalculatedYTDAnswers, d)                                 =>
            withTaxYear(d)(taxYear => checkYourAnswersHandleNonCalculated(n, fillingOutReturn, d, taxYear))
          case (_: CalculatedYTDAnswers, _: DraftMultipleDisposalsReturn)      =>
            logger.warn(
              "Found calculated year to date liability answers on a multiple disposals draft return"
            )
            errorHandler.errorResult()

          case (_: CalculatedYTDAnswers, _: DraftSingleMixedUseDisposalReturn) =>
            logger.warn(
              "Found calculated year to date liability answers on a single mixed use draft return"
            )
            errorHandler.errorResult()

          case (
                _: CalculatedYTDAnswers,
                _: DraftSingleIndirectDisposalReturn
              ) =>
            logger.warn(
              "Found calculated year to date liability answers on a single indirect disposal draft return"
            )
            errorHandler.errorResult()

          case (
                _: CalculatedYTDAnswers,
                _: DraftMultipleIndirectDisposalsReturn
              ) =>
            logger.warn(
              "Found calculated year to date liability answers on a multiple indirect disposals draft return"
            )
            errorHandler.errorResult()

        }
      }

    }

  private def checkYourAnswersHandleNonCalculated(
    answers: NonCalculatedYTDAnswers,
    fillingOutReturn: FillingOutReturn,
    draftReturn: DraftReturn,
    taxYear: TaxYear
  )(implicit request: RequestWithSessionData[_]): Future[Result] =
    answers match {
      case IncompleteNonCalculatedYTDAnswers(
            _,
            _,
            _,
            _,
            Some(_),
            _,
            _,
            _
          ) =>
        Redirect(
          routes.YearToDateLiabilityController.mandatoryEvidenceExpired()
        )

      case n @ IncompleteNonCalculatedYTDAnswers(
            _,
            _,
            _,
            _,
            _,
            Some(_),
            _,
            _
          ) =>
        removePendingUpscanUpload(Left(n), fillingOutReturn).fold(
          { e =>
            logger.warn("Could not remove pending upscan upload", e)
            errorHandler.errorResult()
          },
          _ =>
            Redirect(
              routes.YearToDateLiabilityController.uploadMandatoryEvidence()
            )
        )

      case IncompleteNonCalculatedYTDAnswers(None, _, _, _, _, _, _, _) =>
        Redirect(routes.YearToDateLiabilityController.taxableGainOrLoss())

      case IncompleteNonCalculatedYTDAnswers(_, None, _, _, _, _, _, _) =>
        Redirect(routes.YearToDateLiabilityController.hasEstimatedDetails())

      case IncompleteNonCalculatedYTDAnswers(_, _, _, _, _, _, None, _)
          if fillingOutReturn.isFurtherReturn.contains(true) =>
        Redirect(routes.YearToDateLiabilityController.yearToDateLiability())

      case IncompleteNonCalculatedYTDAnswers(_, _, None, _, _, _, _, _) =>
        Redirect(
          routes.YearToDateLiabilityController.nonCalculatedEnterTaxDue()
        )

      case IncompleteNonCalculatedYTDAnswers(_, _, _, _, _, _, _, None)
          if fillingOutReturn.isFurtherReturn.contains(true) =>
        Redirect(
          routes.YearToDateLiabilityController.repayment()
        )

      case IncompleteNonCalculatedYTDAnswers(_, _, _, None, _, _, _, _) =>
        Redirect(routes.YearToDateLiabilityController.uploadMandatoryEvidence())

      case IncompleteNonCalculatedYTDAnswers(
            Some(t),
            Some(e),
            Some(d),
            Some(m),
            _,
            _,
            y,
            r
          ) =>
        val completeAnswers    = CompleteNonCalculatedYTDAnswers(t, e, d, m, y, r)
        val updatedDraftReturn = draftReturn.fold(
          _.copy(yearToDateLiabilityAnswers = Some(completeAnswers)),
          _.copy(yearToDateLiabilityAnswers = Some(completeAnswers)),
          _.copy(yearToDateLiabilityAnswers = Some(completeAnswers)),
          _.copy(yearToDateLiabilityAnswers = Some(completeAnswers)),
          _.copy(yearToDateLiabilityAnswers = Some(completeAnswers))
        )
        val updatedJourney     =
          fillingOutReturn.copy(draftReturn = updatedDraftReturn)
        val result             = for {
          _ <- returnsService.storeDraftReturn(
                 updatedDraftReturn,
                 fillingOutReturn.subscribedDetails.cgtReference,
                 fillingOutReturn.agentReferenceNumber
               )
          _ <- EitherT(
                 updateSession(sessionStore, request)(
                   _.copy(journeyStatus = Some(updatedJourney))
                 )
               )
        } yield ()

        result.fold(
          { e =>
            logger
              .warn("Cold not store complete year to date liability answers", e)
            errorHandler.errorResult()
          },
          _ =>
            Ok(
              nonCalculatedCheckYourAnswersPage(
                completeAnswers,
                fillingOutReturn.draftReturn match {
                  case _: DraftMultipleDisposalsReturn         => true
                  case _: DraftSingleDisposalReturn            => false
                  case _: DraftSingleIndirectDisposalReturn    => false
                  case _: DraftMultipleIndirectDisposalsReturn => true
                  case _: DraftSingleMixedUseDisposalReturn    => false
                },
                fillingOutReturn.subscribedDetails.isATrust,
                draftReturn.representativeType,
                fillingOutReturn.isFurtherReturn,
                taxYear
              )
            )
        )

      case c: CompleteNonCalculatedYTDAnswers                           =>
        Ok(
          nonCalculatedCheckYourAnswersPage(
            c,
            fillingOutReturn.draftReturn match {
              case _: DraftMultipleDisposalsReturn         => true
              case _: DraftSingleDisposalReturn            => false
              case _: DraftSingleIndirectDisposalReturn    => false
              case _: DraftMultipleIndirectDisposalsReturn => true
              case _: DraftSingleMixedUseDisposalReturn    => false
            },
            fillingOutReturn.subscribedDetails.isATrust,
            draftReturn.representativeType,
            fillingOutReturn.isFurtherReturn,
            taxYear
          )
        )

    }

  private def isATrust(fillingOutReturn: FillingOutReturn): Boolean =
    fillingOutReturn.subscribedDetails.isATrust

  private def isPeriodOfAdmin(fillingOutReturn: FillingOutReturn): Boolean =
    fillingOutReturn.draftReturn.representativeType().contains(PersonalRepresentativeInPeriodOfAdmin)

  private def checkYourAnswersHandleCalculated(
    answers: CalculatedYTDAnswers,
    fillingOutReturn: FillingOutReturn,
    draftReturn: DraftSingleDisposalReturn,
    taxYear: TaxYear,
    wasUkResident: Boolean
  )(implicit request: RequestWithSessionData[_]): Future[Result] =
    answers match {
      case c: CompleteCalculatedYTDAnswers                           =>
        Ok(
          calculatedCheckYouAnswersPage(
            c,
            taxYear,
            fillingOutReturn.subscribedDetails.isATrust,
            draftReturn.representativeType(),
            wasUkResident
          )
        )

      case IncompleteCalculatedYTDAnswers(
            _,
            _,
            _,
            _,
            _,
            _,
            Some(_),
            _
          ) =>
        Redirect(
          routes.YearToDateLiabilityController.mandatoryEvidenceExpired()
        )

      case c @ IncompleteCalculatedYTDAnswers(
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            Some(_)
          ) =>
        removePendingUpscanUpload(Right(c), fillingOutReturn).fold(
          { e =>
            logger.warn("Could not remove pending upscan upload", e)
            errorHandler.errorResult()
          },
          _ =>
            Redirect(
              routes.YearToDateLiabilityController.uploadMandatoryEvidence()
            )
        )

      case IncompleteCalculatedYTDAnswers(None, _, _, _, _, _, _, _)
          if !isATrust(fillingOutReturn) && !isPeriodOfAdmin(fillingOutReturn) =>
        Redirect(routes.YearToDateLiabilityController.estimatedIncome())

      case IncompleteCalculatedYTDAnswers(Some(p), None, _, _, _, _, _, _)
          if !isATrust(fillingOutReturn) && !isPeriodOfAdmin(fillingOutReturn) && p.value > 0L =>
        Redirect(routes.YearToDateLiabilityController.personalAllowance())

      case IncompleteCalculatedYTDAnswers(_, _, None, _, _, _, _, _) =>
        Redirect(routes.YearToDateLiabilityController.hasEstimatedDetails())

      case IncompleteCalculatedYTDAnswers(_, _, _, None, _, _, _, _) =>
        Redirect(routes.YearToDateLiabilityController.taxDue())

      case IncompleteCalculatedYTDAnswers(_, _, _, _, None, _, _, _) =>
        Redirect(routes.YearToDateLiabilityController.taxDue())

      case IncompleteCalculatedYTDAnswers(
            _,
            _,
            Some(_),
            Some(calculatedTaxDue),
            Some(taxDue),
            None,
            _,
            _
          ) if calculatedTaxDue.amountOfTaxDue =!= taxDue =>
        Redirect(routes.YearToDateLiabilityController.uploadMandatoryEvidence())

      case IncompleteCalculatedYTDAnswers(
            _,
            _,
            Some(h),
            Some(c),
            Some(t),
            m,
            _,
            _
          ) if isATrust(fillingOutReturn) || isPeriodOfAdmin(fillingOutReturn) =>
        fromIncompleteToCompleteCalculatedYTDAnswers(
          fillingOutReturn,
          draftReturn,
          AmountInPence(0),
          Some(AmountInPence(0)),
          h,
          c,
          t,
          m,
          taxYear,
          wasUkResident
        )
      case IncompleteCalculatedYTDAnswers(
            Some(e),
            p,
            Some(h),
            Some(c),
            Some(t),
            m,
            _,
            _
          ) =>
        fromIncompleteToCompleteCalculatedYTDAnswers(
          fillingOutReturn,
          draftReturn,
          e,
          p,
          h,
          c,
          t,
          m,
          taxYear,
          wasUkResident
        )
    }

  private def fromIncompleteToCompleteCalculatedYTDAnswers(
    fillingOutReturn: FillingOutReturn,
    draftReturn: DraftSingleDisposalReturn,
    e: AmountInPence,
    p: Option[AmountInPence],
    h: Boolean,
    c: CalculatedTaxDue,
    t: AmountInPence,
    m: Option[MandatoryEvidence],
    taxYear: TaxYear,
    wasUkResident: Boolean
  )(implicit request: RequestWithSessionData[_]): Future[Result] = {
    val completeAnswers = CompleteCalculatedYTDAnswers(e, p, h, c, t, m)
    val newDraftReturn  =
      draftReturn.copy(yearToDateLiabilityAnswers = Some(completeAnswers))

    val result = for {
      _ <- returnsService.storeDraftReturn(
             newDraftReturn,
             fillingOutReturn.subscribedDetails.cgtReference,
             fillingOutReturn.agentReferenceNumber
           )
      _ <- EitherT(
             updateSession(sessionStore, request)(
               _.copy(journeyStatus = Some(fillingOutReturn.copy(draftReturn = newDraftReturn)))
             )
           )
    } yield ()

    result.fold(
      { e =>
        logger.warn("Could not update session", e)
        errorHandler.errorResult()
      },
      _ =>
        Ok(
          calculatedCheckYouAnswersPage(
            completeAnswers,
            taxYear,
            fillingOutReturn.subscribedDetails.isATrust,
            draftReturn.representativeType(),
            wasUkResident
          )
        )
    )
  }

  def checkYourAnswersSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndYTDLiabilityAnswers(request) {
        case _ =>
          Redirect(controllers.returns.routes.TaskListController.taskList())
      }
    }

  private def updateDraftReturn(newAnswers: YearToDateLiabilityAnswers, draftReturn: DraftReturn): DraftReturn =
    draftReturn.fold(
      _.copy(yearToDateLiabilityAnswers = Some(newAnswers)),
      _.copy(yearToDateLiabilityAnswers = Some(newAnswers)),
      _.copy(yearToDateLiabilityAnswers = Some(newAnswers)),
      _.copy(yearToDateLiabilityAnswers = Some(newAnswers)),
      _.copy(yearToDateLiabilityAnswers = Some(newAnswers))
    )

}

object YearToDateLiabilityController {

  val estimatedIncomeForm: Form[BigDecimal] =
    Form(
      mapping(
        "estimatedIncome" -> of(
          MoneyUtils
            .amountInPoundsFormatter(_ < 0, _ > MoneyUtils.maxAmountOfPounds)
        )
      )(identity)(Some(_))
    )

  def personalAllowanceForm(taxYear: TaxYear): Form[BigDecimal] =
    Form(
      mapping(
        "personalAllowance" -> of(
          MoneyUtils.amountInPoundsFormatter(
            _ < 0,
            _ > taxYear.maxPersonalAllowance.inPounds(),
            List(
              MoneyUtils.formatAmountOfMoneyWithPoundSign(
                taxYear.maxPersonalAllowance.inPounds()
              )
            ),
            List(
              MoneyUtils.formatAmountOfMoneyWithPoundSign(
                taxYear.maxPersonalAllowance.inPounds()
              ),
              taxYear.startDateInclusive.getYear.toString,
              taxYear.endDateExclusive.getYear.toString
            )
          )
        )
      )(identity)(Some(_))
    )

  val hasEstimatedDetailsForm: Form[Boolean] =
    Form(
      mapping(
        "hasEstimatedDetails" -> of(BooleanFormatter.formatter)
      )(identity)(Some(_))
    )

  val taxDueForm: Form[BigDecimal] =
    Form(
      mapping(
        "taxDue" -> of(
          MoneyUtils
            .amountInPoundsFormatter(_ < 0, _ > MoneyUtils.maxAmountOfPounds)
        )
      )(identity)(Some(_))
    )

  val taxableGainOrLossForm: Form[BigDecimal] = {
    val (outerId, gainId, lossId) =
      ("taxableGainOrLoss", "taxableGain", "netLoss")

    def innerOption(id: String): InnerOption[BigDecimal] =
      InnerOption { data =>
        FormUtils
          .readValue(id, data, identity)
          .flatMap(
            validateAmountOfMoney(
              id,
              _ <= 0,
              _ > MoneyUtils.maxAmountOfPounds
            )(_)
          )
          .leftMap(Seq(_))
      }

    val formatter = ConditionalRadioUtils.formatter("taxableGainOrLoss")(
      List(
        Left(innerOption(gainId)),
        Left(innerOption(lossId).map(_ * -1)),
        Right(BigDecimal(0))
      )
    ) { d =>
      if (d > 0)
        Map(
          outerId -> "0",
          gainId  -> MoneyUtils.formatAmountOfMoneyWithoutPoundSign(d)
        )
      else if (d < 0)
        Map(
          outerId   -> "1",
          lossId    -> MoneyUtils.formatAmountOfMoneyWithoutPoundSign((d * -1))
        )
      else
        Map(outerId -> "2")
    }

    Form(
      mapping(
        "" -> of(formatter)
      )(identity)(Some(_))
    )
  }

  val nonCalculatedTaxDueForm: Form[BigDecimal] =
    Form(
      mapping(
        "nonCalculatedTaxDue" -> of(
          MoneyUtils
            .amountInPoundsFormatter(_ < 0, _ > MoneyUtils.maxAmountOfPounds)
        )
      )(identity)(Some(_))
    )

  def yearToDateLiabilityForm(taxYear: TaxYear): Form[BigDecimal] =
    Form(
      mapping(
        "yearToDateLiability" -> of(
          MoneyUtils.amountInPoundsFormatter(
            _ <= 0,
            _ > MoneyUtils.maxAmountOfPounds,
            requiredErrorArgs = List(
              taxYear.startDateInclusive.getYear.toString,
              taxYear.endDateExclusive.getYear.toString
            )
          )
        )
      )(identity)(Some(_))
    )

  val repaymentForm: Form[Boolean] =
    Form(
      mapping(
        "repayment" -> of(BooleanFormatter.formatter)
      )(identity)(Some(_))
    )

}
