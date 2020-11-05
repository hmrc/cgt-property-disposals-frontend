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
import cats.instances.bigDecimal._
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
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.StartingToAmendToFillingOutReturnBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.yeartodatelliability.YearToDateLiabilityController._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ConditionalRadioUtils.InnerOption
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, PreviousReturnData, StartingToAmendReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address.UkAddress
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
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.FurtherReturnCalculationEligibility.{Eligible, Ineligible}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.{CgtCalculationService, FurtherReturnCalculationEligibility, FurtherReturnCalculationEligibilityUtil, ReturnsService}
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
  furtherReturnCalculationEligibilityUtil: FurtherReturnCalculationEligibilityUtil,
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
    with SessionUpdates
    with StartingToAmendToFillingOutReturnBehaviour {

  private def withFillingOutReturnAndYTDLiabilityAnswers(
    f: (
      SessionData,
      FillingOutReturn,
      YearToDateLiabilityAnswers
    ) => Future[Result]
  )(implicit request: RequestWithSessionData[_]): Future[Result] = {
    def emptyNonCalculatedAnswers(fillingOutReturn: FillingOutReturn): IncompleteNonCalculatedYTDAnswers =
      fillingOutReturn.amendReturnData.fold(
        IncompleteNonCalculatedYTDAnswers.empty
      ) { amendReturnData =>
        if (amendReturnData.preserveEstimatesAnswer)
          IncompleteNonCalculatedYTDAnswers.empty
            .copy(hasEstimatedDetails = Some(amendReturnData.originalReturn.completeReturn.hasEstimatedDetails))
        else
          IncompleteNonCalculatedYTDAnswers.empty
      }

    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some((_, s: StartingToAmendReturn)) =>
        markUnmetDependency(s, sessionStore, errorHandler)

      case Some((s, r: FillingOutReturn)) if r.isFurtherOrAmendReturn.contains(true) =>
        r.draftReturn.yearToDateLiabilityAnswers.fold(
          f(s, r, emptyNonCalculatedAnswers(r))
        )(y => f(s, r, y))

      case Some(
            (s, r @ FillingOutReturn(_, _, _, d: DraftSingleDisposalReturn, _, _))
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

              case _ =>
                Redirect(
                  controllers.returns.routes.TaskListController.taskList()
                )
            }
        }

      case Some(
            (s, r @ FillingOutReturn(_, _, _, d: DraftMultipleDisposalsReturn, _, _))
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
                _,
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
                _,
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
                _,
                _
              )
            )
          ) =>
        d.yearToDateLiabilityAnswers.fold[Future[Result]](
          f(s, r, IncompleteNonCalculatedYTDAnswers.empty)
        )(f(s, r, _))

      case _ => Redirect(controllers.routes.StartController.start())
    }
  }

  private def withCalculatedTaxDue(
    answers: CalculatedYTDAnswers,
    triageAnswers: CompleteSingleDisposalTriageAnswers,
    address: UkAddress,
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
      case complete: CompleteCalculatedYTDAnswers =>
        f(complete.calculatedTaxDue)

      case incomplete: IncompleteCalculatedYTDAnswers =>
        incomplete.calculatedTaxDue match {
          case Some(c) => f(c)

          case None if !calculateIfMissing =>
            Redirect(routes.YearToDateLiabilityController.checkYourAnswers())

          case None if calculateIfMissing =>
            val result =
              for {
                calculatedTaxDue <- cgtCalculationService
                                      .calculateTaxDue(
                                        CalculateCgtTaxDueRequest(
                                          triageAnswers,
                                          address,
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
      UkAddress,
      CompleteDisposalDetailsAnswers,
      CompleteAcquisitionDetailsAnswers,
      CompleteReliefDetailsAnswers,
      CompleteExemptionAndLossesAnswers
    ) => Future[Result]
  ): Future[Result] =
    (
      draftReturn.triageAnswers,
      draftReturn.propertyAddress,
      draftReturn.disposalDetailsAnswers,
      draftReturn.acquisitionDetailsAnswers,
      draftReturn.reliefDetailsAnswers,
      draftReturn.exemptionAndLossesAnswers
    ) match {
      case (
            t: CompleteSingleDisposalTriageAnswers,
            Some(u: UkAddress),
            Some(d: CompleteDisposalDetailsAnswers),
            Some(a: CompleteAcquisitionDetailsAnswers),
            Some(r: CompleteReliefDetailsAnswers),
            Some(e: CompleteExemptionAndLossesAnswers)
          ) =>
        f(t, u, d, a, r, e)

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

  private def withResidentialStatus(
    draftReturn: DraftReturn
  )(f: Boolean => Future[Result]): Future[Result] =
    draftReturn.triageAnswers.fold(
      _.fold(
        _.wasAUKResident,
        c => Some(c.countryOfResidence.isUk())
      ),
      _.fold(
        _.wasAUKResident,
        c => Some(c.countryOfResidence.isUk())
      )
    ) match {
      case Some(u) => f(u)
      case _       => Redirect(controllers.returns.routes.TaskListController.taskList())
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
    answers: YearToDateLiabilityAnswers
  )(f: AmountInPence => Future[Result]): Future[Result] = {
    val estimatedIncome = answers match {
      case y: CalculatedYTDAnswers    => y.fold(_.estimatedIncome, c => Some(c.estimatedIncome))
      case n: NonCalculatedYTDAnswers => n.fold(_.estimatedIncome, _.estimatedIncome)
    }

    estimatedIncome
      .fold[Future[Result]](
        Redirect(routes.YearToDateLiabilityController.checkYourAnswers())
      )(f)
  }

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
      case _: NonCalculatedYTDAnswers =>
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
        case c: CalculatedYTDAnswers =>
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
        case c: CalculatedYTDAnswers =>
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
            val newJourney     = currentFillingOutReturn.copy(draftReturn = newDraftReturn)

            val result = for {
              _ <- if (newDraftReturn === currentDraftReturn)
                     EitherT.pure[Future, Error](())
                   else
                     returnsService.storeDraftReturn(newJourney)
              _ <- EitherT(
                     updateSession(sessionStore, request)(
                       _.copy(journeyStatus = Some(newJourney))
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
      withFillingOutReturnAndYTDLiabilityAnswers { (_, fillingOutReturn, answers) =>
        withFurtherReturnCalculationEligibilityCheck(fillingOutReturn) { furtherReturnCalculationEligibility =>
          (answers, fillingOutReturn.draftReturn) match {
            case (_, _) if isATrust(fillingOutReturn) || isPeriodOfAdmin(fillingOutReturn) =>
              Redirect(routes.YearToDateLiabilityController.checkYourAnswers())

            case (
                  answers: YearToDateLiabilityAnswers,
                  draftReturn: DraftSingleDisposalReturn
                ) if furtherReturnCalculationEligibility.forall(_.isEligible) =>
              withAssetTypeAndResidentialStatus(draftReturn) { (_, wasUkResident) =>
                withTaxYear(draftReturn) { taxYear =>
                  commonDisplayBehaviour(answers)(
                    form = { answers =>
                      val estimatedIncome = answers match {
                        case y: CalculatedYTDAnswers    => y.fold(_.estimatedIncome, c => Some(c.estimatedIncome))
                        case n: NonCalculatedYTDAnswers => n.fold(_.estimatedIncome, _.estimatedIncome)
                      }
                      estimatedIncome.fold(estimatedIncomeForm)(a => estimatedIncomeForm.fill(a.inPounds()))
                    }
                  )(
                    page = estimatedIncomePage(
                      _,
                      _,
                      taxYear,
                      wasUkResident,
                      draftReturn.representativeType(),
                      fillingOutReturn.isAmendReturn
                    )
                  )(
                    requiredPreviousAnswer = _ =>
                      furtherReturnCalculationEligibility.fold[Option[Unit]](Some(())) { _ =>
                        answers match {
                          case _: CalculatedYTDAnswers    => Some(())
                          case n: NonCalculatedYTDAnswers =>
                            n.fold(_.taxableGainOrLoss, c => Some(c.taxableGainOrLoss)).map(_ => ())
                        }
                      },
                    furtherReturnCalculationEligibility.fold(controllers.returns.routes.TaskListController.taskList())(
                      _ => routes.YearToDateLiabilityController.taxableGainOrLoss()
                    )
                  )
                }
              }

            case _ =>
              Redirect(routes.YearToDateLiabilityController.checkYourAnswers())
          }
        }
      }
    }

  def estimatedIncomeSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndYTDLiabilityAnswers { (_, fillingOutReturn, answers) =>
        withFurtherReturnCalculationEligibilityCheck(fillingOutReturn) { furtherReturnCalculationEligibility =>
          (answers, fillingOutReturn.draftReturn) match {
            case (_, _) if isATrust(fillingOutReturn) || isPeriodOfAdmin(fillingOutReturn) =>
              Redirect(routes.YearToDateLiabilityController.checkYourAnswers())
            case (
                  answers: YearToDateLiabilityAnswers,
                  draftReturn: DraftSingleDisposalReturn
                ) if furtherReturnCalculationEligibility.forall(_.isEligible) =>
              withAssetTypeAndResidentialStatus(draftReturn) { (_, wasUkResident) =>
                withTaxYear(draftReturn) { taxYear =>
                  commonSubmitBehaviour(
                    fillingOutReturn,
                    draftReturn,
                    answers
                  )(
                    form = estimatedIncomeForm
                  )(
                    page = { (form, backLink) =>
                      estimatedIncomePage(
                        form,
                        backLink,
                        taxYear,
                        wasUkResident,
                        draftReturn.representativeType(),
                        fillingOutReturn.isAmendReturn
                      )
                    }
                  )(
                    requiredPreviousAnswer = _ =>
                      furtherReturnCalculationEligibility.fold[Option[Unit]](Some(())) { _ =>
                        answers match {
                          case _: CalculatedYTDAnswers    => Some(())
                          case n: NonCalculatedYTDAnswers =>
                            n.fold(_.taxableGainOrLoss, c => Some(c.taxableGainOrLoss)).map(_ => ())
                        }
                      },
                    furtherReturnCalculationEligibility.fold(controllers.returns.routes.TaskListController.taskList())(
                      _ => routes.YearToDateLiabilityController.taxableGainOrLoss()
                    )
                  )(
                    updateAnswers = { (i, draftReturn) =>
                      val existingEstimatedIncome = answers match {
                        case c: CalculatedYTDAnswers    => c.fold(_.estimatedIncome, c => Some(c.estimatedIncome))
                        case n: NonCalculatedYTDAnswers => n.fold(_.estimatedIncome, _.estimatedIncome)
                      }

                      val estimatedIncome = AmountInPence.fromPounds(i)

                      if (existingEstimatedIncome.contains(estimatedIncome))
                        draftReturn
                      else {
                        val newAnswers = answers match {
                          case calculatedAnswers: CalculatedYTDAnswers =>
                            calculatedAnswers
                              .unset(_.personalAllowance)
                              .unset(_.hasEstimatedDetails)
                              .unset(_.calculatedTaxDue)
                              .unset(_.taxDue)
                              .unset(_.mandatoryEvidence)
                              .unset(_.expiredEvidence)
                              .unset(_.pendingUpscanUpload)
                              .copy(estimatedIncome = Some(AmountInPence.fromPounds(i)))
                          case n: NonCalculatedYTDAnswers              =>
                            IncompleteNonCalculatedYTDAnswers.empty.copy(
                              taxableGainOrLoss = n.fold(_.taxableGainOrLoss, c => Some(c.taxableGainOrLoss)),
                              estimatedIncome = Some(estimatedIncome),
                              hasEstimatedDetails =
                                if (fillingOutReturn.amendReturnData.exists(_.preserveEstimatesAnswer))
                                  n.fold(_.hasEstimatedDetails, c => Some(c.hasEstimatedDetails))
                                else
                                  None,
                              taxableGainOrLossCalculation =
                                n.fold(_.taxableGainOrLossCalculation, _.taxableGainOrLossCalculation)
                            )
                        }

                        draftReturn.copy(yearToDateLiabilityAnswers = Some(newAnswers))
                      }
                    }
                  )
                }
              }

            case _ =>
              Redirect(routes.YearToDateLiabilityController.checkYourAnswers())
          }
        }
      }
    }

  def personalAllowance(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndYTDLiabilityAnswers { (_, fillingOutReturn, answers) =>
        withFurtherReturnCalculationEligibilityCheck(fillingOutReturn) { furtherReturnCalculationEligibility =>
          (answers, fillingOutReturn.draftReturn) match {
            case (_, _) if isATrust(fillingOutReturn) || isPeriodOfAdmin(fillingOutReturn) =>
              Redirect(routes.YearToDateLiabilityController.checkYourAnswers())
            case (
                  answers: YearToDateLiabilityAnswers,
                  draftReturn: DraftSingleDisposalReturn
                ) if furtherReturnCalculationEligibility.forall(_.isEligible) =>
              withAssetTypeAndResidentialStatus(draftReturn) { (_, wasUkResident) =>
                withTaxYear(draftReturn) { taxYear =>
                  withEstimatedIncome(answers) { estimatedIncome =>
                    if (estimatedIncome.value > 0L)
                      commonDisplayBehaviour(answers)(
                        form = { _ =>
                          val emptyForm         = personalAllowanceForm(taxYear)
                          val personalAllowance = answers match {
                            case y: CalculatedYTDAnswers    => y.fold(_.personalAllowance, _.personalAllowance)
                            case n: NonCalculatedYTDAnswers => n.fold(_.personalAllowance, _.personalAllowance)
                          }

                          personalAllowance.fold(emptyForm)(a => emptyForm.fill(a.inPounds()))
                        }
                      )(
                        page = personalAllowancePage(
                          _,
                          _,
                          taxYear,
                          wasUkResident,
                          draftReturn.representativeType(),
                          fillingOutReturn.isAmendReturn
                        )
                      )(
                        requiredPreviousAnswer = {
                          case y: CalculatedYTDAnswers    =>
                            y.fold(_.estimatedIncome, c => Some(c.estimatedIncome))
                          case n: NonCalculatedYTDAnswers =>
                            n.fold(_.estimatedIncome, _.estimatedIncome)
                        },
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
    }

  def personalAllowanceSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndYTDLiabilityAnswers { (_, fillingOutReturn, answers) =>
        withFurtherReturnCalculationEligibilityCheck(fillingOutReturn) { furtherReturnCalculationEligibility =>
          (answers, fillingOutReturn.draftReturn) match {
            case (_, _) if isATrust(fillingOutReturn) || isPeriodOfAdmin(fillingOutReturn) =>
              Redirect(routes.YearToDateLiabilityController.checkYourAnswers())
            case (
                  answers: YearToDateLiabilityAnswers,
                  draftReturn: DraftSingleDisposalReturn
                ) if furtherReturnCalculationEligibility.forall(_.isEligible) =>
              withAssetTypeAndResidentialStatus(draftReturn) { (_, wasUkResident) =>
                withTaxYear(draftReturn) { taxYear =>
                  withEstimatedIncome(answers) { estimatedIncome =>
                    if (estimatedIncome.value > 0L)
                      commonSubmitBehaviour(
                        fillingOutReturn,
                        draftReturn,
                        answers
                      )(
                        form = personalAllowanceForm(taxYear)
                      )(page =
                        (form, backLink) =>
                          personalAllowancePage(
                            form,
                            backLink,
                            taxYear,
                            wasUkResident,
                            draftReturn.representativeType(),
                            fillingOutReturn.isAmendReturn
                          )
                      )(
                        requiredPreviousAnswer = {
                          case y: CalculatedYTDAnswers    =>
                            y.fold(_.estimatedIncome, c => Some(c.estimatedIncome))
                          case n: NonCalculatedYTDAnswers =>
                            n.fold(_.estimatedIncome, _.estimatedIncome)
                        },
                        routes.YearToDateLiabilityController.estimatedIncome()
                      ) { (p, draftReturn) =>
                        val existingPersonalAllowance = answers match {
                          case c: CalculatedYTDAnswers    => c.fold(_.personalAllowance, _.personalAllowance)
                          case n: NonCalculatedYTDAnswers => n.fold(_.personalAllowance, _.personalAllowance)
                        }
                        val personalAllowance         = AmountInPence.fromPounds(p)
                        if (existingPersonalAllowance.contains(personalAllowance))
                          draftReturn
                        else {
                          val newAnswers = {
                            answers match {
                              case calculatedAnswers: CalculatedYTDAnswers =>
                                calculatedAnswers
                                  .unset(_.hasEstimatedDetails)
                                  .unset(_.calculatedTaxDue)
                                  .unset(_.taxDue)
                                  .unset(_.mandatoryEvidence)
                                  .unset(_.expiredEvidence)
                                  .unset(_.pendingUpscanUpload)
                                  .copy(personalAllowance = Some(AmountInPence.fromPounds(p)))

                              case n: NonCalculatedYTDAnswers =>
                                IncompleteNonCalculatedYTDAnswers.empty.copy(
                                  taxableGainOrLoss = n.fold(_.taxableGainOrLoss, c => Some(c.taxableGainOrLoss)),
                                  estimatedIncome = Some(estimatedIncome),
                                  personalAllowance = Some(AmountInPence.fromPounds(p)),
                                  hasEstimatedDetails =
                                    if (fillingOutReturn.amendReturnData.exists(_.preserveEstimatesAnswer))
                                      n.fold(_.hasEstimatedDetails, c => Some(c.hasEstimatedDetails))
                                    else
                                      None,
                                  taxableGainOrLossCalculation =
                                    n.fold(_.taxableGainOrLossCalculation, _.taxableGainOrLossCalculation)
                                )
                            }
                          }

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

            case _ =>
              Redirect(routes.YearToDateLiabilityController.checkYourAnswers())
          }
        }
      }
    }

  def hasEstimatedDetails(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndYTDLiabilityAnswers { (_, fillingOutReturn, answers) =>
        if (fillingOutReturn.amendReturnData.exists(_.preserveEstimatesAnswer))
          Redirect(routes.YearToDateLiabilityController.checkYourAnswers())
        else
          (answers, fillingOutReturn.draftReturn) match {
            case (
                  nonCalculatedAnswers: NonCalculatedYTDAnswers,
                  _: DraftReturn
                ) =>
              withFurtherReturnCalculationEligibilityCheck(fillingOutReturn) { furtherReturnCalculationEligibility =>
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
                    fillingOutReturn.isFurtherOrAmendReturn,
                    fillingOutReturn.isAmendReturn
                  )
                )(
                  requiredPreviousAnswer = answers =>
                    if (furtherReturnCalculationEligibility.exists(_.isEligible) && !isATrust(fillingOutReturn))
                      answers.fold(_.personalAllowance, _.personalAllowance)
                    else
                      answers.fold(
                        _.taxableGainOrLoss,
                        c => Some(c.taxableGainOrLoss)
                      ),
                  redirectToIfNoRequiredPreviousAnswer =
                    if (furtherReturnCalculationEligibility.exists(_.isEligible) && !isATrust(fillingOutReturn))
                      routes.YearToDateLiabilityController.personalAllowance()
                    else
                      routes.YearToDateLiabilityController.taxableGainOrLoss()
                )
              }

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
        fillingOutReturn.isFurtherOrAmendReturn,
        fillingOutReturn.isAmendReturn
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
      withFillingOutReturnAndYTDLiabilityAnswers { (_, fillingOutReturn, answers) =>
        if (fillingOutReturn.amendReturnData.exists(_.preserveEstimatesAnswer))
          Redirect(routes.YearToDateLiabilityController.checkYourAnswers())
        else
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
        fillingOutReturn.isFurtherOrAmendReturn,
        fillingOutReturn.isAmendReturn
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
    withFurtherReturnCalculationEligibilityCheck(fillingOutReturn) { furtherReturnCalculationEligibility =>
      commonSubmitBehaviour(fillingOutReturn, draftReturn, nonCalculatedAnswers)(
        form = hasEstimatedDetailsForm
      )(
        page = hasEstimatedDetailsPage(
          _,
          _,
          isATrust(fillingOutReturn),
          draftReturn.representativeType(),
          fillingOutReturn.isFurtherOrAmendReturn,
          fillingOutReturn.isAmendReturn
        )
      )(
        requiredPreviousAnswer = answers =>
          if (furtherReturnCalculationEligibility.exists(_.isEligible) && !isATrust(fillingOutReturn))
            answers.fold(_.personalAllowance, _.personalAllowance)
          else
            answers.fold(
              _.taxableGainOrLoss,
              c => Some(c.taxableGainOrLoss)
            ),
        redirectToIfNoRequiredPreviousAnswer =
          if (furtherReturnCalculationEligibility.exists(_.isEligible) && !isATrust(fillingOutReturn))
            routes.YearToDateLiabilityController.personalAllowance()
          else
            routes.YearToDateLiabilityController.taxableGainOrLoss()
      ) { (hasEstimated, draftReturn) =>
        if (
          nonCalculatedAnswers
            .fold(_.hasEstimatedDetails, c => Some(c.hasEstimatedDetails))
            .contains(hasEstimated)
        )
          draftReturn
        else {
          val newAnswers =
            if (fillingOutReturn.isFurtherOrAmendReturn.contains(true))
              nonCalculatedAnswers
                .unset(_.checkForRepayment)
                .unset(_.expiredEvidence)
                .unset(_.mandatoryEvidence)
                .unset(_.pendingUpscanUpload)
                .unset(_.yearToDateLiability)
                .unset(_.yearToDateLiabilityCalculation)
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
    }

  def taxDue(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndYTDLiabilityAnswers { (_, fillingOutReturn, answers) =>
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
          address,
          disposalDetails,
          acquisitionDetails,
          reliefDetails,
          exemptionsAndLossesDetails
        ) =>
          withCalculatedTaxDue(
            calculatedAnswers,
            triage,
            address,
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
              form = { answers =>
                val form = taxDueForm(calculatedTaxDue)
                answers.fold(
                  _.taxDue.fold(form)(t => form.fill(t.inPounds())),
                  c => form.fill(c.taxDue.inPounds())
                )
              }
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
                draftReturn.representativeType(),
                fillingOutReturn.isAmendReturn
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
      withFillingOutReturnAndYTDLiabilityAnswers { (_, fillingOutReturn, answers) =>
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
          address,
          disposalDetails,
          acquisitionDetails,
          reliefDetails,
          exemptionsAndLossesDetails
        ) =>
          withCalculatedTaxDue(
            calculatedAnswers,
            triage,
            address,
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
              form = taxDueForm(calculatedTaxDue)
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
                draftReturn.representativeType(),
                fillingOutReturn.isAmendReturn
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
      withFillingOutReturnAndYTDLiabilityAnswers { (_, fillingOutReturn, answers) =>
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
            upscanUpload =>
              Ok(
                mandatoryEvidencePage(
                  upscanUpload,
                  backLink,
                  fillingOutReturn.isFurtherOrAmendReturn,
                  fillingOutReturn.isAmendReturn,
                  fillingOutReturn.draftReturn.triageAnswers.isLeft
                )
              )
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
                if (fillingOutReturn.isFurtherOrAmendReturn.contains(true))
                  routes.YearToDateLiabilityController.repayment()
                else
                  routes.YearToDateLiabilityController.nonCalculatedEnterTaxDue(),
              _ => routes.YearToDateLiabilityController.checkYourAnswers()
            )

            withFurtherReturnCalculationEligibilityCheck(fillingOutReturn) { furtherReturnEligibility =>
              if (
                shouldAskForMandatoryEvidence(
                  furtherReturnEligibility,
                  fillingOutReturn.draftReturn,
                  nonCalculatedYTDAnswers
                )
              )
                commonDisposalMandatoryEvidence(backLink)
              else
                Redirect(routes.YearToDateLiabilityController.checkYourAnswers())
            }

          case _ =>
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
    val newJourney     = currentJourney.copy(draftReturn = newDraftReturn)

    for {
      _ <- returnsService.storeDraftReturn(newJourney)
      _ <- EitherT(
             updateSession(sessionStore, request)(_.copy(journeyStatus = Some(newJourney)))
           )
    } yield ()
  }

  private def getFurtherReturnTaxableGainOrLossCalculationData(
    fillingOutReturn: FillingOutReturn
  )(implicit
    hc: HeaderCarrier,
    request: RequestWithSessionData[_]
  ): EitherT[Future, Error, Option[(TaxableGainOrLossCalculation, CompleteExemptionAndLossesAnswers)]]               =
    for {
      requiredAnswers <-
        EitherT.fromEither(
          (
            fillingOutReturn.draftReturn.gainOrLossAfterReliefs,
            fillingOutReturn.draftReturn.exemptionAndLossesAnswers
          ) match {
            case (Some(gainOrLossAfterReliefs), Some(e: CompleteExemptionAndLossesAnswers)) =>
              Right(gainOrLossAfterReliefs -> e)
            case _                                                                          =>
              Left(Error("Could not find complete exemptions and losses answers and gain or loss after reliefs"))
          }
        )
      eligibility     <-
        furtherReturnCalculationEligibilityUtil.isEligibleForFurtherReturnOrAmendCalculation(fillingOutReturn)
      calculation     <- eligibility match {
                           case _: Ineligible                                       => EitherT.pure[Future, Error](None)
                           case Eligible(_, previousReturnCalculationData, address) =>
                             cgtCalculationService
                               .calculateTaxableGainOrLoss(
                                 TaxableGainOrLossCalculationRequest(
                                   calculationData = previousReturnCalculationData,
                                   requiredAnswers._1,
                                   requiredAnswers._2,
                                   address
                                 )
                               )
                               .map(Some(_))
                         }
    } yield calculation.map(_ -> requiredAnswers._2)

  private def getTaxableGainOrLossPage(
    fillingOutReturn: FillingOutReturn,
    taxYear: TaxYear
  )(implicit
    hc: HeaderCarrier,
    request: RequestWithSessionData[_]
  ): EitherT[Future, Error, (Option[TaxableGainOrLossCalculation], (Form[BigDecimal], Call) => play.twirl.api.Html)] =
    if (fillingOutReturn.isFurtherOrAmendReturn.contains(true)) {
      getFurtherReturnTaxableGainOrLossCalculationData(fillingOutReturn)
        .map { calculationWithLossesAndExemptionAnswers =>
          val page = furtherReturnsTaxableGainOrLossPage(
            _,
            _,
            fillingOutReturn.subscribedDetails.isATrust,
            fillingOutReturn.draftReturn.representativeType(),
            taxYear,
            fillingOutReturn.isAmendReturn,
            calculationWithLossesAndExemptionAnswers
          )
          calculationWithLossesAndExemptionAnswers.map(_._1) -> page
        }
    } else {
      val page = taxableGainOrLossPage(
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
        fillingOutReturn.draftReturn.representativeType(),
        fillingOutReturn.isAmendReturn
      )
      EitherT.pure[Future, Error](None -> page)
    }

  def taxableGainOrLoss(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndYTDLiabilityAnswers { (_, fillingOutReturn, answers) =>
        answers match {
          case (_: CalculatedYTDAnswers) =>
            Redirect(routes.YearToDateLiabilityController.checkYourAnswers())

          case (nonCalculatedAnswers: NonCalculatedYTDAnswers) =>
            withTaxYear(fillingOutReturn.draftReturn) { taxYear =>
              def displayPageBehaviour(page: (Form[BigDecimal], Call) => play.twirl.api.Html): Future[Result] =
                commonDisplayBehaviour(
                  nonCalculatedAnswers
                )(form =
                  _.fold(
                    _.taxableGainOrLoss.fold(taxableGainOrLossForm)(a => taxableGainOrLossForm.fill(a.inPounds())),
                    c => taxableGainOrLossForm.fill(c.taxableGainOrLoss.inPounds())
                  )
                )(
                  page = page
                )(
                  requiredPreviousAnswer = _ => Some(()),
                  redirectToIfNoRequiredPreviousAnswer = controllers.returns.routes.TaskListController.taskList()
                )

              getTaxableGainOrLossPage(fillingOutReturn, taxYear)
                .leftMap { e =>
                  logger.warn("Could not determine page to be used", e)
                  errorHandler.errorResult()
                }
                .semiflatMap(r => displayPageBehaviour(r._2))
                .merge
            }
        }
      }
    }

  def taxableGainOrLossSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndYTDLiabilityAnswers { (_, fillingOutReturn, answers) =>
        answers match {
          case _: CalculatedYTDAnswers =>
            Redirect(routes.YearToDateLiabilityController.checkYourAnswers())

          case nonCalculatedAnswers: NonCalculatedYTDAnswers =>
            withTaxYear(fillingOutReturn.draftReturn) { taxYear =>
              def submitBehaviour(
                calculation: Option[TaxableGainOrLossCalculation],
                page: (Form[BigDecimal], Call) => play.twirl.api.Html
              ): Future[Result] =
                commonSubmitBehaviour(
                  fillingOutReturn,
                  fillingOutReturn.draftReturn,
                  nonCalculatedAnswers
                )(form = taxableGainOrLossForm)(
                  page = page
                )(
                  requiredPreviousAnswer = _ => Some(()),
                  redirectToIfNoRequiredPreviousAnswer = controllers.returns.routes.TaskListController.taskList()
                ) { (amount, draftReturn) =>
                  import cats.instances.option._

                  val taxableGainOrLoss = AmountInPence.fromPounds(amount)
                  if (
                    nonCalculatedAnswers
                      .fold(_.taxableGainOrLoss, c => Some(c.taxableGainOrLoss))
                      .contains(taxableGainOrLoss)
                    && nonCalculatedAnswers
                      .fold(_.taxableGainOrLossCalculation, _.taxableGainOrLossCalculation) === calculation
                  )
                    draftReturn
                  else {
                    val newAnswers =
                      if (fillingOutReturn.isFurtherOrAmendReturn.contains(true))
                        if (fillingOutReturn.amendReturnData.exists(_.preserveEstimatesAnswer))
                          nonCalculatedAnswers
                            .unset(_.yearToDateLiability)
                            .unset(_.taxDue)
                            .unset(_.checkForRepayment)
                            .unset(_.mandatoryEvidence)
                            .unset(_.yearToDateLiabilityCalculation)
                            .copy(
                              taxableGainOrLoss = Some(taxableGainOrLoss),
                              taxableGainOrLossCalculation = calculation
                            )
                        else
                          nonCalculatedAnswers
                            .unset(_.hasEstimatedDetails)
                            .unset(_.yearToDateLiability)
                            .unset(_.taxDue)
                            .unset(_.checkForRepayment)
                            .unset(_.mandatoryEvidence)
                            .unset(_.yearToDateLiabilityCalculation)
                            .copy(
                              taxableGainOrLoss = Some(taxableGainOrLoss),
                              taxableGainOrLossCalculation = calculation
                            )
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

              getTaxableGainOrLossPage(fillingOutReturn, taxYear)
                .leftMap { e =>
                  logger.warn("Could not determine page to be used", e)
                  errorHandler.errorResult()
                }
                .semiflatMap((submitBehaviour _).tupled)
                .merge
            }
        }
      }
    }

  def nonCalculatedEnterTaxDue(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndYTDLiabilityAnswers { (_, fillingOutReturn, answers) =>
        val isFurtherOrAmendReturn = fillingOutReturn.isFurtherOrAmendReturn.contains(true)
        answers match {
          case _: CalculatedYTDAnswers =>
            Redirect(routes.YearToDateLiabilityController.checkYourAnswers())

          case nonCalculatedAnswers: NonCalculatedYTDAnswers =>
            (fillingOutReturn.previousSentReturns, fillingOutReturn.draftReturn.representativeType()) match {
              case (Some(PreviousReturnData(_, Some(previousYtd), _, _)), None) =>
                nonCalculatedAnswers.fold(_.yearToDateLiability, _.yearToDateLiability) match {
                  case None =>
                    Redirect(routes.YearToDateLiabilityController.checkYourAnswers())

                  case Some(yearToDateLiability) =>
                    if (nonCalculatedAnswers.fold(_.yearToDateLiability, _.yearToDateLiability).isEmpty)
                      Redirect(routes.YearToDateLiabilityController.yearToDateLiability())
                    else {
                      val taxOwedOnOriginalReturn = fillingOutReturn.amendReturnData
                        .map(
                          _.originalReturn.completeReturn.fold(
                            _.yearToDateLiabilityAnswers.taxDue,
                            _.yearToDateLiabilityAnswers.fold(_.taxDue, _.taxDue),
                            _.yearToDateLiabilityAnswers.taxDue,
                            _.yearToDateLiabilityAnswers.taxDue,
                            _.yearToDateLiabilityAnswers.taxDue
                          )
                        )

                      Ok(
                        furtherReturnCheckTaxDuePage(
                          routes.YearToDateLiabilityController.yearToDateLiability(),
                          yearToDateLiability,
                          previousYtd,
                          taxOwedOnOriginalReturn,
                          fillingOutReturn.subscribedDetails.isATrust
                        )
                      )
                    }
                }

              case _ =>
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
                      if (isFurtherOrAmendReturn)
                        furtherReturnEnterTaxDuePage(
                          _,
                          _,
                          taxYear,
                          fillingOutReturn.subscribedDetails.isATrust,
                          fillingOutReturn.draftReturn.representativeType,
                          fillingOutReturn.isAmendReturn
                        )
                      else
                        nonCalculatedEnterTaxDuePage(_, _, fillingOutReturn.isAmendReturn)
                  )(
                    requiredPreviousAnswer = { answers =>
                      if (isFurtherOrAmendReturn)
                        answers.fold(_.yearToDateLiability, _.yearToDateLiability).map(_ => ())
                      else answers.fold(_.hasEstimatedDetails, c => Some(c.hasEstimatedDetails)).map(_ => ())
                    },
                    redirectToIfNoRequiredPreviousAnswer =
                      if (isFurtherOrAmendReturn) routes.YearToDateLiabilityController.yearToDateLiability()
                      else routes.YearToDateLiabilityController.hasEstimatedDetails()
                  )
                }
            }

        }
      }
    }

  def nonCalculatedEnterTaxDueSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndYTDLiabilityAnswers { (_, fillingOutReturn, answers) =>
        val isFurtherReturnorAmendReturn = fillingOutReturn.isFurtherOrAmendReturn.contains(true)

        answers match {
          case _: CalculatedYTDAnswers =>
            Redirect(routes.YearToDateLiabilityController.checkYourAnswers())

          case nonCalculatedAnswers: NonCalculatedYTDAnswers =>
            (fillingOutReturn.previousSentReturns, fillingOutReturn.draftReturn.representativeType()) match {
              case (Some(PreviousReturnData(_, Some(previousYtd), _, _)), None) =>
                nonCalculatedAnswers.fold(_.yearToDateLiability, _.yearToDateLiability) match {
                  case None =>
                    Redirect(routes.YearToDateLiabilityController.checkYourAnswers())

                  case Some(yearToDateLiability) =>
                    handledConfirmFurtherReturnTaxDue(
                      nonCalculatedAnswers,
                      previousYtd,
                      yearToDateLiability,
                      fillingOutReturn
                    )
                }

              case _ =>
                withTaxYear(fillingOutReturn.draftReturn) { taxYear =>
                  commonSubmitBehaviour(
                    fillingOutReturn,
                    fillingOutReturn.draftReturn,
                    nonCalculatedAnswers
                  )(form = nonCalculatedTaxDueForm)(
                    page =
                      if (isFurtherReturnorAmendReturn)
                        furtherReturnEnterTaxDuePage(
                          _,
                          _,
                          taxYear,
                          fillingOutReturn.subscribedDetails.isATrust,
                          fillingOutReturn.draftReturn.representativeType,
                          fillingOutReturn.isAmendReturn
                        )
                      else
                        nonCalculatedEnterTaxDuePage(_, _, fillingOutReturn.isAmendReturn)
                  )(
                    requiredPreviousAnswer = { answers =>
                      if (isFurtherReturnorAmendReturn)
                        answers.fold(_.yearToDateLiability, _.yearToDateLiability).map(_ => ())
                      else answers.fold(_.hasEstimatedDetails, c => Some(c.hasEstimatedDetails)).map(_ => ())
                    },
                    redirectToIfNoRequiredPreviousAnswer =
                      if (isFurtherReturnorAmendReturn) routes.YearToDateLiabilityController.yearToDateLiability()
                      else routes.YearToDateLiabilityController.hasEstimatedDetails()
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
                        .unset(_.checkForRepayment)
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
    if (nonCalculatedAnswers.fold(_.yearToDateLiability, c => c.yearToDateLiability).isEmpty)
      Redirect(routes.YearToDateLiabilityController.yearToDateLiability())
    else {
      val taxOwedOnOriginalReturn = fillingOutReturn.amendReturnData
        .map(
          _.originalReturn.completeReturn.fold(
            _.yearToDateLiabilityAnswers.taxDue,
            _.yearToDateLiabilityAnswers.fold(_.taxDue, _.taxDue),
            _.yearToDateLiabilityAnswers.taxDue,
            _.yearToDateLiabilityAnswers.taxDue,
            _.yearToDateLiabilityAnswers.taxDue
          )
        )
      val taxOwed                 = taxOwedOnOriginalReturn.getOrElse(AmountInPence.zero)
      val taxDue                  = (taxOwed ++ (yearToDateLiability -- previousYearToDateLiability): AmountInPence).withFloorZero
      if (nonCalculatedAnswers.fold(_.taxDue, c => Some(c.taxDue)).contains(taxDue))
        Redirect(routes.YearToDateLiabilityController.checkYourAnswers())
      else {
        val newAnswers = {
          if (fillingOutReturn.isFurtherOrAmendReturn.contains(true))
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
        val newJourney     = fillingOutReturn.copy(draftReturn = newDraftReturn)

        val result = for {
          _ <- returnsService.storeDraftReturn(newJourney)
          _ <- EitherT(
                 updateSession(sessionStore, request)(
                   _.copy(journeyStatus = Some(newJourney))
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
      withFillingOutReturnAndYTDLiabilityAnswers { (_, fillingOutReturn, answers) =>
        answers match {
          case IncompleteNonCalculatedYTDAnswers(
                _,
                _,
                _,
                _,
                Some(expired),
                _,
                _,
                _,
                _,
                _,
                _,
                _
              ) =>
            Ok(expiredMandatoryEvidencePage(expired, fillingOutReturn.isFurtherOrAmendReturn))

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
            Ok(expiredMandatoryEvidencePage(expired, fillingOutReturn.isFurtherOrAmendReturn))

          case _ =>
            Redirect(routes.YearToDateLiabilityController.checkYourAnswers())
        }
      }
    }

  def scanningMandatoryEvidence(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndYTDLiabilityAnswers { (_, fillingOutReturn, answers) =>
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
            case None                   => Ok(mandatoryEvidenceScanProgressPage(fillingOutReturn.isFurtherOrAmendReturn))
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
                _,
                _,
                _,
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
      withFillingOutReturnAndYTDLiabilityAnswers { (_, fillingOutReturn, answers) =>
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

  private def getFurtherReturnYtdLiabilityCalculation(
    fillingOutReturn: FillingOutReturn,
    ytdAnswers: NonCalculatedYTDAnswers
  )(implicit
    hc: HeaderCarrier,
    request: RequestWithSessionData[_]
  ): EitherT[Future, Error, Option[YearToDateLiabilityCalculation]] = {
    def requiredAnswers(
      taxableGainOrLoss: AmountInPence,
      estimatedIncome: AmountInPence,
      personalAllowance: AmountInPence
    ) =
      fillingOutReturn.draftReturn match {
        case d: DraftSingleDisposalReturn =>
          d.triageAnswers.fold(
            _ => Left(Error("Could not find complete single disposal triage answers")),
            completeTriageAnswers =>
              Right(Some((completeTriageAnswers, taxableGainOrLoss, estimatedIncome, personalAllowance)))
          )
        case _                            =>
          Left(
            Error(
              "Did not get DraftSingleDisposalReturn for further/amend return eligible for a calcualtion"
            )
          )
      }

    for {
      eligibility     <-
        furtherReturnCalculationEligibilityUtil.isEligibleForFurtherReturnOrAmendCalculation(fillingOutReturn)
      requiredAnswers <- eligibility match {
                           case _: Ineligible =>
                             EitherT.pure[Future, Error](None)
                           case _: Eligible   =>
                             EitherT.fromEither(
                               (
                                 ytdAnswers.fold(_.taxableGainOrLoss, c => Some(c.taxableGainOrLoss)),
                                 ytdAnswers.fold(_.estimatedIncome, _.estimatedIncome),
                                 ytdAnswers.fold(_.personalAllowance, _.personalAllowance)
                               ) match {
                                 case (Some(taxableGainOrLoss), Some(income), Some(allowance))
                                     if !isATrust(fillingOutReturn) =>
                                   requiredAnswers(taxableGainOrLoss, income, allowance)

                                 case (Some(taxableGainOrLoss), _, _) if isATrust(fillingOutReturn) =>
                                   requiredAnswers(taxableGainOrLoss, AmountInPence.zero, AmountInPence.zero)

                                 case _ =>
                                   Left(
                                     Error(
                                       "Could not find taxable gain or loss, estimated income and personal allowance"
                                     )
                                   )
                               }
                             )
                         }
      calculation     <- requiredAnswers match {
                           case None                                                                   =>
                             EitherT.pure[Future, Error](None)
                           case Some((triageAnswers, taxableGain, estimatedIncome, personalAllowance)) =>
                             cgtCalculationService
                               .calculateYearToDateLiability(
                                 YearToDateLiabilityCalculationRequest(
                                   triageAnswers,
                                   taxableGain,
                                   estimatedIncome,
                                   personalAllowance,
                                   fillingOutReturn.subscribedDetails.isATrust
                                 )
                               )
                               .map(Some(_))
                         }
    } yield calculation
  }

  private def getYtdLiabilityPage(
    fillingOutReturn: FillingOutReturn,
    ytdAnswers: NonCalculatedYTDAnswers,
    taxYear: TaxYear
  )(implicit
    hc: HeaderCarrier,
    request: RequestWithSessionData[_]
  ): EitherT[Future, Error, (Option[YearToDateLiabilityCalculation], (Form[BigDecimal], Call) => play.twirl.api.Html)] =
    getFurtherReturnYtdLiabilityCalculation(fillingOutReturn, ytdAnswers)
      .map { calculation =>
        val page = yearToDateLiabilityPage(
          _,
          _,
          taxYear,
          fillingOutReturn.subscribedDetails.isATrust,
          fillingOutReturn.draftReturn.representativeType(),
          fillingOutReturn.isAmendReturn,
          calculation
        )
        calculation -> page
      }

  def yearToDateLiability(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndYTDLiabilityAnswers { (_, fillingOutReturn, answers) =>
        (fillingOutReturn, answers) match {
          case (f, nonCalculatedAnswers: NonCalculatedYTDAnswers) if f.isFurtherOrAmendReturn.contains(true) =>
            withTaxYear(fillingOutReturn.draftReturn) { taxYear =>
              getYtdLiabilityPage(fillingOutReturn, nonCalculatedAnswers, taxYear).foldF(
                { e =>
                  logger.warn("Could not determine eligibility for calculation or perform calculation", e)
                  errorHandler.errorResult()
                },
                { case (_, page) =>
                  commonDisplayBehaviour(
                    nonCalculatedAnswers
                  )(form =
                    _.fold(_.yearToDateLiability, _.yearToDateLiability)
                      .fold(yearToDateLiabilityForm(taxYear))(a => yearToDateLiabilityForm(taxYear).fill(a.inPounds()))
                  )(
                    page
                  )(
                    requiredPreviousAnswer = { answers =>
                      if (fillingOutReturn.amendReturnData.exists(_.preserveEstimatesAnswer))
                        answers.fold(_.taxableGainOrLoss, c => Some(c.taxableGainOrLoss)).map(_ => ())
                      else
                        answers.fold(_.hasEstimatedDetails, c => Some(c.hasEstimatedDetails)).map(_ => ())
                    },
                    redirectToIfNoRequiredPreviousAnswer =
                      if (fillingOutReturn.amendReturnData.exists(_.preserveEstimatesAnswer))
                        routes.YearToDateLiabilityController.taxableGainOrLoss()
                      else
                        routes.YearToDateLiabilityController.hasEstimatedDetails()
                  )
                }
              )

            }

          case _ =>
            Redirect(routes.YearToDateLiabilityController.checkYourAnswers())

        }
      }

    }

  def yearToDateLiabilitySubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndYTDLiabilityAnswers { (_, fillingOutReturn, answers) =>
        (fillingOutReturn, answers) match {
          case (f, nonCalculatedAnswers: NonCalculatedYTDAnswers) if f.isFurtherOrAmendReturn.contains(true) =>
            withTaxYear(fillingOutReturn.draftReturn) { taxYear =>
              getYtdLiabilityPage(fillingOutReturn, nonCalculatedAnswers, taxYear).foldF(
                { e =>
                  logger.warn("Could not determine eligibility for calculation or perform calculation", e)
                  errorHandler.errorResult()
                },
                { case (calculation, page) =>
                  commonSubmitBehaviour(
                    fillingOutReturn,
                    fillingOutReturn.draftReturn,
                    nonCalculatedAnswers
                  )(form = yearToDateLiabilityForm(taxYear))(
                    page
                  )(
                    requiredPreviousAnswer = { answers =>
                      if (fillingOutReturn.amendReturnData.exists(_.preserveEstimatesAnswer))
                        answers.fold(_.taxableGainOrLoss, c => Some(c.taxableGainOrLoss)).map(_ => ())
                      else
                        answers.fold(_.hasEstimatedDetails, c => Some(c.hasEstimatedDetails)).map(_ => ())
                    },
                    redirectToIfNoRequiredPreviousAnswer =
                      if (fillingOutReturn.amendReturnData.exists(_.preserveEstimatesAnswer))
                        routes.YearToDateLiabilityController.taxableGainOrLoss()
                      else
                        routes.YearToDateLiabilityController.hasEstimatedDetails()
                  ) { (amount, draftReturn) =>
                    import cats.instances.option._

                    val yearToDateLiability = AmountInPence.fromPounds(amount)
                    if (
                      nonCalculatedAnswers
                        .fold(_.yearToDateLiability, _.yearToDateLiability)
                        .contains(yearToDateLiability)
                      && nonCalculatedAnswers
                        .fold(_.yearToDateLiabilityCalculation, _.yearToDateLiabilityCalculation) === calculation
                    )
                      draftReturn
                    else {
                      val newAnswers =
                        nonCalculatedAnswers
                          .unset(_.taxDue)
                          .unset(_.checkForRepayment)
                          .unset(_.mandatoryEvidence)
                          .copy(
                            yearToDateLiability = Some(yearToDateLiability),
                            yearToDateLiabilityCalculation = calculation
                          )

                      updateDraftReturn(newAnswers, draftReturn)
                    }
                  }
                }
              )
            }

          case _ =>
            Redirect(routes.YearToDateLiabilityController.checkYourAnswers())
        }
      }
    }

  def repayment(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndYTDLiabilityAnswers { (_, fillingOutReturn, answers) =>
        (fillingOutReturn, answers) match {
          case (f, nonCalculatedAnswers: NonCalculatedYTDAnswers) if f.isFurtherOrAmendReturn.contains(true) =>
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
                fillingOutReturn.draftReturn.representativeType(),
                fillingOutReturn.isAmendReturn
              )
            )(
              requiredPreviousAnswer = _.fold(_.taxDue, c => Some(c.taxDue)),
              redirectToIfNoRequiredPreviousAnswer = routes.YearToDateLiabilityController.nonCalculatedEnterTaxDue()
            )

          case _ =>
            Redirect(routes.YearToDateLiabilityController.checkYourAnswers())

        }
      }

    }

  def repaymentSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndYTDLiabilityAnswers { (_, fillingOutReturn, answers) =>
        (fillingOutReturn, answers) match {
          case (f, nonCalculatedAnswers: NonCalculatedYTDAnswers) if f.isFurtherOrAmendReturn.contains(true) =>
            commonSubmitBehaviour(
              fillingOutReturn,
              fillingOutReturn.draftReturn,
              nonCalculatedAnswers
            )(form = repaymentForm)(
              page = repaymentPage(
                _,
                _,
                fillingOutReturn.subscribedDetails.isATrust,
                fillingOutReturn.draftReturn.representativeType(),
                fillingOutReturn.isAmendReturn
              )
            )(
              requiredPreviousAnswer = _.fold(_.taxDue, c => Some(c.taxDue)),
              redirectToIfNoRequiredPreviousAnswer = routes.YearToDateLiabilityController.nonCalculatedEnterTaxDue()
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

          case _ =>
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
    val newJourney     = fillingOutReturn.copy(draftReturn = newDraftReturn)

    for {
      _ <- returnsService.storeDraftReturn(newJourney)
      _ <- EitherT(
             updateSession(sessionStore, request)(
               _.copy(journeyStatus = Some(newJourney))
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

        case _: UpscanFailure =>
          answers.fold(
            _.copy(pendingUpscanUpload = Some(upscanUpload)),
            _.copy(pendingUpscanUpload = Some(upscanUpload))
          )

      }

    val newDraftReturn = updateDraftReturn(newAnswers, fillingOutReturn.draftReturn)
    val newJourney     = fillingOutReturn.copy(draftReturn = newDraftReturn)
    for {
      _ <- returnsService.storeDraftReturn(newJourney)
      _ <- EitherT(
             updateSession(sessionStore, request)(
               _.copy(journeyStatus = Some(newJourney))
             )
           )
    } yield ()
  }

  def checkYourAnswers(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndYTDLiabilityAnswers { (_, fillingOutReturn, answers) =>
        (answers, fillingOutReturn.draftReturn) match {
          case (c: CalculatedYTDAnswers, s: DraftSingleDisposalReturn) =>
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

          case (n: NonCalculatedYTDAnswers, d) =>
            withResidentialStatus(d) { wasUkResident =>
              withTaxYear(d)(taxYear =>
                checkYourAnswersHandleNonCalculated(n, fillingOutReturn, d, taxYear, wasUkResident)
              )
            }

          case (_: CalculatedYTDAnswers, _: DraftMultipleDisposalsReturn) =>
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
    taxYear: TaxYear,
    wasUkResident: Boolean
  )(implicit request: RequestWithSessionData[_]): Future[Result] =
    withFurtherReturnCalculationEligibilityCheck(fillingOutReturn) { furtherReturnEligibility =>
      answers match {
        case IncompleteNonCalculatedYTDAnswers(
              _,
              _,
              _,
              _,
              Some(_),
              _,
              _,
              _,
              _,
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
              _,
              _,
              _,
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

        case IncompleteNonCalculatedYTDAnswers(None, _, _, _, _, _, _, _, _, _, _, _) =>
          Redirect(routes.YearToDateLiabilityController.taxableGainOrLoss())

        case IncompleteNonCalculatedYTDAnswers(_, _, _, _, _, _, _, _, None, _, _, _)
            if furtherReturnEligibility.exists(_.isEligible) && !isATrust(fillingOutReturn) =>
          Redirect(routes.YearToDateLiabilityController.estimatedIncome())

        case IncompleteNonCalculatedYTDAnswers(_, _, _, _, _, _, _, _, _, None, _, _)
            if furtherReturnEligibility.exists(_.isEligible) && !isATrust(fillingOutReturn) =>
          Redirect(routes.YearToDateLiabilityController.personalAllowance())

        case IncompleteNonCalculatedYTDAnswers(_, None, _, _, _, _, _, _, _, _, _, _) =>
          Redirect(routes.YearToDateLiabilityController.hasEstimatedDetails())

        case IncompleteNonCalculatedYTDAnswers(_, _, _, _, _, _, None, _, _, _, _, _)
            if fillingOutReturn.isFurtherOrAmendReturn.contains(true) =>
          Redirect(routes.YearToDateLiabilityController.yearToDateLiability())

        case IncompleteNonCalculatedYTDAnswers(_, _, None, _, _, _, _, _, _, _, _, _) =>
          Redirect(
            routes.YearToDateLiabilityController.nonCalculatedEnterTaxDue()
          )

        case IncompleteNonCalculatedYTDAnswers(_, _, _, _, _, _, _, None, _, _, _, _)
            if fillingOutReturn.isFurtherOrAmendReturn.contains(true) =>
          Redirect(
            routes.YearToDateLiabilityController.repayment()
          )

        case n @ IncompleteNonCalculatedYTDAnswers(_, _, _, None, _, _, _, _, _, _, _, _)
            if shouldAskForMandatoryEvidence(furtherReturnEligibility, draftReturn, n) =>
          Redirect(routes.YearToDateLiabilityController.uploadMandatoryEvidence())

        case IncompleteNonCalculatedYTDAnswers(
              Some(t),
              Some(e),
              Some(d),
              m,
              _,
              _,
              y,
              r,
              i,
              p,
              taxableGainOrLossCalculation,
              ytdCalculation
            ) =>
          val completeAnswers    =
            CompleteNonCalculatedYTDAnswers(t, e, d, m, y, r, i, p, taxableGainOrLossCalculation, ytdCalculation)
          val updatedDraftReturn = draftReturn.fold(
            _.copy(yearToDateLiabilityAnswers = Some(completeAnswers)),
            _.copy(yearToDateLiabilityAnswers = Some(completeAnswers)),
            _.copy(yearToDateLiabilityAnswers = Some(completeAnswers)),
            _.copy(yearToDateLiabilityAnswers = Some(completeAnswers)),
            _.copy(yearToDateLiabilityAnswers = Some(completeAnswers))
          )
          val updatedJourney     =
            fillingOutReturn.copy(draftReturn = updatedDraftReturn)

          val result = for {
            _ <- returnsService.storeDraftReturn(updatedJourney)
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
                  fillingOutReturn.isFurtherOrAmendReturn,
                  fillingOutReturn.isAmendReturn,
                  taxYear,
                  fillingOutReturn.amendReturnData.exists(_.preserveEstimatesAnswer),
                  wasUkResident
                )
              )
          )

        case c: CompleteNonCalculatedYTDAnswers =>
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
              fillingOutReturn.isFurtherOrAmendReturn,
              fillingOutReturn.isAmendReturn,
              taxYear,
              fillingOutReturn.amendReturnData.exists(_.preserveEstimatesAnswer),
              wasUkResident
            )
          )

      }
    }

  private def shouldAskForMandatoryEvidence(
    furtherReturnCalculationEligibility: Option[FurtherReturnCalculationEligibility],
    draftReturn: DraftReturn,
    answers: NonCalculatedYTDAnswers
  ) =
    furtherReturnCalculationEligibility match {
      case Some(Eligible(glarCalculation, _, _)) =>
        import cats.instances.option._

        val agreedWithGlarCalculation        =
          draftReturn.gainOrLossAfterReliefs.contains(glarCalculation.gainOrLossAfterReliefs)
        val agreedWithTaxableGainCalculation =
          answers
            .fold(_.taxableGainOrLossCalculation, _.taxableGainOrLossCalculation)
            .map(_.taxableGainOrLoss) === answers.fold(_.taxableGainOrLoss, c => Some(c.taxableGainOrLoss))
        val agreedWithYtdCalculation         =
          answers
            .fold(_.yearToDateLiabilityCalculation, _.yearToDateLiabilityCalculation)
            .map(_.yearToDateLiability) === answers.fold(_.yearToDateLiability, _.yearToDateLiability)

        !(agreedWithGlarCalculation && agreedWithTaxableGainCalculation && agreedWithYtdCalculation)

      case _ =>
        true

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
      case c: CompleteCalculatedYTDAnswers =>
        Ok(
          calculatedCheckYouAnswersPage(
            c,
            taxYear,
            fillingOutReturn.subscribedDetails.isATrust,
            draftReturn.representativeType(),
            wasUkResident,
            fillingOutReturn.amendReturnData.exists(_.preserveEstimatesAnswer)
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
    val newJourney      = fillingOutReturn.copy(draftReturn = newDraftReturn)

    val result = for {
      _ <- returnsService.storeDraftReturn(newJourney)
      _ <- EitherT(
             updateSession(sessionStore, request)(
               _.copy(journeyStatus = Some(newJourney))
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
            wasUkResident,
            fillingOutReturn.amendReturnData.exists(_.preserveEstimatesAnswer)
          )
        )
    )
  }

  def checkYourAnswersSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndYTDLiabilityAnswers { case _ =>
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

  private def withFurtherReturnCalculationEligibilityCheck(fillingOutReturn: FillingOutReturn)(
    f: Option[FurtherReturnCalculationEligibility] => Future[Result]
  )(implicit r: RequestWithSessionData[_], hc: HeaderCarrier): Future[Result] = {
    val result =
      if (fillingOutReturn.isFurtherOrAmendReturn.contains(true))
        furtherReturnCalculationEligibilityUtil
          .isEligibleForFurtherReturnOrAmendCalculation(fillingOutReturn)
          .map(Some(_))
      else EitherT.pure(None)

    result.foldF(
      { e =>
        logger.warn(s"Could not check eligibility fro further return calculation", e)
        errorHandler.errorResult()
      },
      f
    )
  }

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

  def taxDueForm(calculatedTaxDue: CalculatedTaxDue): Form[BigDecimal] = {
    val calculatedTaxDueInPounds = calculatedTaxDue.amountOfTaxDue.inPounds()
    val (outerId, innerId)       = "agreeWithCalculation" -> "taxDue"

    val innerOption = InnerOption { data =>
      FormUtils
        .readValue(innerId, data, identity)
        .flatMap(
          validateAmountOfMoney(
            innerId,
            _ < 0,
            _ > MoneyUtils.maxAmountOfPounds
          )(_)
        )
        .leftMap(Seq(_))
    }

    val formatter = ConditionalRadioUtils.formatter(outerId)(
      List(
        Right(calculatedTaxDueInPounds),
        Left(innerOption)
      )
    ) { d =>
      if (d === calculatedTaxDueInPounds)
        Map(
          outerId -> "0"
        )
      else
        Map(
          outerId -> "1",
          innerId -> MoneyUtils.formatAmountOfMoneyWithoutPoundSign(d)
        )
    }

    Form(
      mapping(
        "" -> of(formatter)
      )(identity)(Some(_))
    )
  }

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
            _ < 0,
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
