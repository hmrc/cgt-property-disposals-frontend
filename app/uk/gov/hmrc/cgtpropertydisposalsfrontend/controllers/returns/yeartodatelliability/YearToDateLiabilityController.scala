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
import cats.instances.boolean._
import cats.instances.future._
import cats.syntax.either._
import cats.syntax.eq._
import com.google.inject.{Inject, Singleton}
import play.api.Configuration
import play.api.data.Form
import play.api.data.Forms.{mapping, nonEmptyText, of}
import play.api.http.Writeable
import play.api.mvc._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.yeartodatelliability.YearToDateLiabilityController._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ConditionalRadioUtils.InnerOption
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.FillingOutReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.MoneyUtils.validateAmountOfMoney
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.{AmountInPence, MoneyUtils}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.AcquisitionDetailsAnswers.CompleteAcquisitionDetailsAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.DisposalDetailsAnswers.CompleteDisposalDetailsAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ExemptionAndLossesAnswers.CompleteExemptionAndLossesAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ReliefDetailsAnswers.CompleteReliefDetailsAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.CompleteSingleDisposalTriageAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.YearToDateLiabilityAnswers.CalculatedYearToDateLiabilityAnswers.{CompleteCalculatedYearToDateLiabilityAnswers, IncompleteCalculatedYearToDateLiabilityAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.YearToDateLiabilityAnswers.NonCalculatedYearToDateLiabilityAnswers.{CompleteNonCalculatedYearToDateLiabilityAnswers, IncompleteNonCalculatedYearToDateLiabilityAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.YearToDateLiabilityAnswers.{CalculatedYearToDateLiabilityAnswers, NonCalculatedYearToDateLiabilityAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{BooleanFormatter, ConditionalRadioUtils, FormUtils, SessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.{CgtCalculationService, ReturnsService}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging.LoggerOps
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views.html.returns.{ytdliability => pages}
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class YearToDateLiabilityController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionStore,
  val errorHandler: ErrorHandler,
  returnsService: ReturnsService,
  cgtCalculationService: CgtCalculationService,
  cc: MessagesControllerComponents,
  val config: Configuration,
  estimatedIncomePage: pages.estimated_income,
  personalAllowancePage: pages.personal_allowance,
  hasEstimatedDetailsPage: pages.has_estimated_details,
  taxDuePage: pages.tax_due,
  mandatoryEvidencePage: pages.upload_mandatory_evidence,
  calculatedCheckYouAnswersPage: pages.calculated_check_your_answers,
  taxableGainOrLossPage: pages.taxable_gain_or_loss,
  nonCalculatedEnterTaxDuePage: pages.non_calculated_enter_tax_due,
  nonCalculatedCheckYourAnswersPage: pages.non_calculated_check_your_answers
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
      case Some((s, r @ FillingOutReturn(_, _, _, d: SingleDisposalDraftReturn))) =>
        d.yearToDateLiabilityAnswers match {
          case Some(y) => f(s, r, y)

          case None =>
            d.reliefDetailsAnswers match {
              case Some(CompleteReliefDetailsAnswers(_, _, Some(_: OtherReliefsOption.OtherReliefs))) =>
                f(s, r, IncompleteNonCalculatedYearToDateLiabilityAnswers.empty)

              case Some(_: CompleteReliefDetailsAnswers) =>
                f(s, r, IncompleteCalculatedYearToDateLiabilityAnswers.empty)

              case _ =>
                Redirect(controllers.returns.routes.TaskListController.taskList())
            }
        }

      case Some((s, r @ FillingOutReturn(_, _, _, d: MultipleDisposalsDraftReturn))) =>
        d.yearToDateLiabilityAnswers.fold[Future[Result]](
          f(s, r, IncompleteNonCalculatedYearToDateLiabilityAnswers.empty)
        )(f(s, r, _))

      case _ => Redirect(controllers.routes.StartController.start())
    }

  private def withCalculatedTaxDue(
    answers: CalculatedYearToDateLiabilityAnswers,
    triageAnswers: CompleteSingleDisposalTriageAnswers,
    disposalDetailsAnswers: CompleteDisposalDetailsAnswers,
    acquisitionDetailsAnswers: CompleteAcquisitionDetailsAnswers,
    reliefDetailsAnswers: CompleteReliefDetailsAnswers,
    exemptionAndLossesAnswers: CompleteExemptionAndLossesAnswers,
    estimatedIncome: AmountInPence,
    personalAllowance: AmountInPence,
    calculateIfMissing: Boolean,
    fillingOutReturn: FillingOutReturn,
    draftReturn: SingleDisposalDraftReturn
  )(f: CalculatedTaxDue => Future[Result])(implicit request: RequestWithSessionData[_]): Future[Result] =
    answers match {
      case complete: CompleteCalculatedYearToDateLiabilityAnswers => f(complete.calculatedTaxDue)

      case incomplete: IncompleteCalculatedYearToDateLiabilityAnswers =>
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
                                         disposalDetailsAnswers,
                                         acquisitionDetailsAnswers,
                                         reliefDetailsAnswers,
                                         exemptionAndLossesAnswers,
                                         estimatedIncome,
                                         personalAllowance
                                       )
                                     )
                _ <- EitherT(
                      updateSession(sessionStore, request)(
                        _.copy(
                          journeyStatus = Some(
                            fillingOutReturn.copy(draftReturn = draftReturn.copy(yearToDateLiabilityAnswers = Some(
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
              .biSemiflatMap({ e =>
                logger.warn("Could not get and store calculated tax due", e)
                errorHandler.errorResult()
              }, f)
              .merge
        }

    }

  private def withCompleteJourneys(draftReturn: SingleDisposalDraftReturn)(
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

  private def withDisposalDate(
    draftReturn: SingleDisposalDraftReturn
  )(f: DisposalDate => Future[Result]): Future[Result] =
    draftReturn.triageAnswers
      .fold(_.disposalDate, c => Some(c.disposalDate))
      .fold[Future[Result]](
        Redirect(controllers.returns.routes.TaskListController.taskList())
      )(f)

  private def withEstimatedIncome(
    answers: CalculatedYearToDateLiabilityAnswers
  )(f: AmountInPence => Future[Result]): Future[Result] =
    answers
      .fold(_.estimatedIncome, c => Some(c.estimatedIncome))
      .fold[Future[Result]](Redirect(routes.YearToDateLiabilityController.checkYourAnswers()))(f)

  private def withPersonalAllowance(
    answers: CalculatedYearToDateLiabilityAnswers
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
      case _: NonCalculatedYearToDateLiabilityAnswers =>
        Redirect(routes.YearToDateLiabilityController.checkYourAnswers())

      case calculatedAnswers: CalculatedYearToDateLiabilityAnswers =>
        calculatedAnswers
          .fold(_.hasEstimatedDetails, c => Some(c.hasEstimatedDetails))
          .fold[Future[Result]](
            Redirect(routes.YearToDateLiabilityController.checkYourAnswers())
          )(f)
    }

  private def withTaxDueAndCalculatedTaxDue(
    answers: CalculatedYearToDateLiabilityAnswers
  )(f: (AmountInPence, CalculatedTaxDue) => Future[Result]): Future[Result] = {
    val taxDue           = answers.fold(_.taxDue, c => Some(c.taxDue))
    val calculatedTaxDue = answers.fold(_.calculatedTaxDue, c => Some(c.calculatedTaxDue))

    (taxDue, calculatedTaxDue) match {
      case (Some(t), Some(c)) => f(t, c)
      case _                  => Redirect(routes.YearToDateLiabilityController.checkYourAnswers())
    }
  }

  private def commonDisplayBehaviour[Y <: YearToDateLiabilityAnswers, A, P: Writeable, R](
    currentAnswers: Y
  )(form: Y => Form[A])(
    page: (Form[A], Call) => P
  )(
    requiredPreviousAnswer: Y => Option[R],
    redirectToIfNoRequiredPreviousAnswer: Call
  ): Future[Result] =
    if (requiredPreviousAnswer(currentAnswers).isDefined) {
      val backLink = currentAnswers match {
        case c: CalculatedYearToDateLiabilityAnswers =>
          c.fold(
            _ => redirectToIfNoRequiredPreviousAnswer,
            _ => routes.YearToDateLiabilityController.checkYourAnswers()
          )

        case n: NonCalculatedYearToDateLiabilityAnswers =>
          n.fold(
            _ => redirectToIfNoRequiredPreviousAnswer,
            _ => routes.YearToDateLiabilityController.checkYourAnswers()
          )
      }

      Ok(page(form(currentAnswers), backLink))
    } else {
      Redirect(redirectToIfNoRequiredPreviousAnswer)
    }

  private def commonSubmitBehaviour[Y <: YearToDateLiabilityAnswers, D <: DraftReturn: Eq, A, P: Writeable, R](
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
  )(
    implicit request: RequestWithSessionData[_]
  ): Future[Result] =
    if (requiredPreviousAnswer(currentAnswers).isDefined) {
      lazy val backLink = currentAnswers match {
        case c: CalculatedYearToDateLiabilityAnswers =>
          c.fold(
            _ => redirectToIfNoRequiredPreviousAnswer,
            _ => routes.YearToDateLiabilityController.checkYourAnswers()
          )

        case n: NonCalculatedYearToDateLiabilityAnswers =>
          n.fold(
            _ => redirectToIfNoRequiredPreviousAnswer,
            _ => routes.YearToDateLiabilityController.checkYourAnswers()
          )
      }
      form
        .bindFromRequest()
        .fold(
          formWithErrors => BadRequest(page(formWithErrors, backLink)), { value =>
            val newDraftReturn = updateAnswers(value, currentDraftReturn)

            val result = for {
              _ <- if (newDraftReturn === currentDraftReturn) EitherT.pure(())
                  else
                    returnsService.storeDraftReturn(
                      newDraftReturn,
                      currentFillingOutReturn.subscribedDetails.cgtReference,
                      currentFillingOutReturn.agentReferenceNumber
                    )
              _ <- EitherT(
                    updateSession(sessionStore, request)(
                      _.copy(journeyStatus = Some(currentFillingOutReturn.copy(draftReturn = newDraftReturn)))
                    )
                  )
            } yield ()
            result.fold(
              { e =>
                logger.warn("Could not update draft return", e)
                errorHandler.errorResult()
              },
              _ => Redirect(routes.YearToDateLiabilityController.checkYourAnswers())
            )

          }
        )
    } else {
      Redirect(redirectToIfNoRequiredPreviousAnswer)
    }

  def estimatedIncome(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withFillingOutReturnAndYTDLiabilityAnswers(request) {
      case (_, fillingOutReturn, answers) =>
        (answers, fillingOutReturn.draftReturn) match {
          case (calculatedAnswers: CalculatedYearToDateLiabilityAnswers, draftReturn: SingleDisposalDraftReturn) =>
            withDisposalDate(draftReturn) { disposalDate =>
              commonDisplayBehaviour(calculatedAnswers)(
                form = _.fold(
                  _.estimatedIncome.fold(estimatedIncomeForm)(a => estimatedIncomeForm.fill(a.inPounds())),
                  c => estimatedIncomeForm.fill(c.estimatedIncome.inPounds())
                )
              )(
                page = estimatedIncomePage(_, _, disposalDate)
              )(
                requiredPreviousAnswer = _ => Some(()),
                controllers.returns.routes.TaskListController.taskList()
              )
            }

          case _ =>
            Redirect(routes.YearToDateLiabilityController.checkYourAnswers())
        }
    }
  }

  def estimatedIncomeSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withFillingOutReturnAndYTDLiabilityAnswers(request) {
      case (_, fillingOutReturn, answers) =>
        (answers, fillingOutReturn.draftReturn) match {
          case (calculatedAnswers: CalculatedYearToDateLiabilityAnswers, draftReturn: SingleDisposalDraftReturn) =>
            withDisposalDate(draftReturn) { disposalDate =>
              commonSubmitBehaviour(fillingOutReturn, draftReturn, calculatedAnswers)(
                form = estimatedIncomeForm
              )(
                page = {
                  case (form, backLink) =>
                    estimatedIncomePage(form, backLink, disposalDate)
                }
              )(
                requiredPreviousAnswer               = _ => Some(()),
                redirectToIfNoRequiredPreviousAnswer = controllers.returns.routes.TaskListController.taskList()
              )(
                updateAnswers = {
                  case (i, draftReturn) =>
                    val estimatedIncome = AmountInPence.fromPounds(i)

                    if (calculatedAnswers
                          .fold(_.estimatedIncome, c => Some(c.estimatedIncome))
                          .contains(estimatedIncome)) {
                      draftReturn
                    } else {
                      val newAnswers =
                        calculatedAnswers.fold(
                          { incomplete =>
                            val hadRequiredPersonalAllowance = incomplete.estimatedIncome.exists(_.value > 0L)
                            val nowRequiresPersonalAllowance = i > 0

                            if (hadRequiredPersonalAllowance =!= nowRequiresPersonalAllowance)
                              incomplete.copy(estimatedIncome = Some(estimatedIncome), personalAllowance = None)
                            else
                              incomplete.copy(estimatedIncome = Some(estimatedIncome))
                          },
                          _ =>
                            IncompleteCalculatedYearToDateLiabilityAnswers.empty
                              .copy(estimatedIncome = Some(estimatedIncome))
                        )

                      draftReturn.copy(yearToDateLiabilityAnswers = Some(newAnswers))
                    }
                }
              )
            }

          case _ =>
            Redirect(routes.YearToDateLiabilityController.checkYourAnswers())
        }
    }
  }

  def personalAllowance(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withFillingOutReturnAndYTDLiabilityAnswers(request) {
      case (_, fillingOutReturn, answers) =>
        (answers, fillingOutReturn.draftReturn) match {
          case (calculatedAnswers: CalculatedYearToDateLiabilityAnswers, draftReturn: SingleDisposalDraftReturn) =>
            withDisposalDate(draftReturn) { disposalDate =>
              withEstimatedIncome(calculatedAnswers) { estimatedIncome =>
                if (estimatedIncome.value > 0L) {
                  commonDisplayBehaviour(calculatedAnswers)(
                    form = { a =>
                      val emptyForm = personalAllowanceForm(disposalDate)
                      a.fold(
                        _.personalAllowance.fold(emptyForm)(a => emptyForm.fill(a.inPounds())),
                        _.personalAllowance.fold(emptyForm)(a => emptyForm.fill(a.inPounds()))
                      )
                    }
                  )(
                    page = personalAllowancePage(_, _, disposalDate)
                  )(
                    requiredPreviousAnswer = _.fold(_.estimatedIncome, c => Some(c.estimatedIncome)),
                    routes.YearToDateLiabilityController.estimatedIncome()
                  )
                } else {
                  Redirect(routes.YearToDateLiabilityController.checkYourAnswers())
                }
              }
            }

          case _ =>
            Redirect(routes.YearToDateLiabilityController.checkYourAnswers())
        }
    }
  }

  def personalAllowanceSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withFillingOutReturnAndYTDLiabilityAnswers(request) {
      case (_, fillingOutReturn, answers) =>
        (answers, fillingOutReturn.draftReturn) match {
          case (calculatedAnswers: CalculatedYearToDateLiabilityAnswers, draftReturn: SingleDisposalDraftReturn) =>
            withDisposalDate(draftReturn) { disposalDate =>
              withEstimatedIncome(calculatedAnswers) { estimatedIncome =>
                if (estimatedIncome.value > 0L) {
                  commonSubmitBehaviour(fillingOutReturn, draftReturn, calculatedAnswers)(
                    form = personalAllowanceForm(disposalDate)
                  )(page = {
                    case (form, backLink) =>
                      val updatedForm = form.copy(errors = form.errors.map(
                        _.copy(args = Seq(
                          MoneyUtils
                            .formatAmountOfMoneyWithoutPoundSign(
                              disposalDate.taxYear.personalAllowance.inPounds()
                            )
                        )
                        )
                      )
                      )

                      personalAllowancePage(updatedForm, backLink, disposalDate)
                  })(
                    requiredPreviousAnswer = _.fold(_.estimatedIncome, c => Some(c.estimatedIncome)),
                    routes.YearToDateLiabilityController.estimatedIncome()
                  ) {
                    case (p, draftReturn) =>
                      val personalAllowance = AmountInPence.fromPounds(p)
                      if (calculatedAnswers
                            .fold(_.personalAllowance, _.personalAllowance)
                            .contains(personalAllowance)) {
                        draftReturn
                      } else {
                        draftReturn.copy(
                          yearToDateLiabilityAnswers = Some(
                            calculatedAnswers.fold(
                              _.copy(personalAllowance = Some(personalAllowance)),
                              complete =>
                                IncompleteCalculatedYearToDateLiabilityAnswers.empty.copy(
                                  estimatedIncome   = Some(complete.estimatedIncome),
                                  personalAllowance = Some(personalAllowance)
                                )
                            )
                          )
                        )
                      }
                  }
                } else {
                  Redirect(routes.YearToDateLiabilityController.checkYourAnswers())
                }
              }
            }

          case _ =>
            Redirect(routes.YearToDateLiabilityController.checkYourAnswers())
        }
    }
  }

  def hasEstimatedDetails(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withFillingOutReturnAndYTDLiabilityAnswers(request) {
      case (_, fillingOutReturn, answers) =>
        (answers, fillingOutReturn.draftReturn) match {
          case (nonCalculatedAnswers: NonCalculatedYearToDateLiabilityAnswers, _: DraftReturn) =>
            commonDisplayBehaviour(nonCalculatedAnswers)(
              form = _.fold(
                _.hasEstimatedDetails.fold(hasEstimatedDetailsForm)(hasEstimatedDetailsForm.fill),
                c => hasEstimatedDetailsForm.fill(c.hasEstimatedDetails)
              )
            )(
              page = hasEstimatedDetailsPage(_, _)
            )(
              requiredPreviousAnswer               = _.fold(_.taxableGainOrLoss, c => Some(c.taxableGainOrLoss)),
              redirectToIfNoRequiredPreviousAnswer = routes.YearToDateLiabilityController.taxableGainOrLoss()
            )

          case (calculatedAnswers: CalculatedYearToDateLiabilityAnswers, _: SingleDisposalDraftReturn) =>
            withEstimatedIncome(calculatedAnswers) { estimatedIncome =>
              commonDisplayBehaviour(calculatedAnswers)(
                form = _.fold(
                  _.hasEstimatedDetails.fold(hasEstimatedDetailsForm)(hasEstimatedDetailsForm.fill),
                  c => hasEstimatedDetailsForm.fill(c.hasEstimatedDetails)
                )
              )(
                page = hasEstimatedDetailsPage(_, _)
              )(
                requiredPreviousAnswer = { a =>
                  if (estimatedIncome.value > 0L)
                    a.fold(_.personalAllowance, _.personalAllowance)
                  else
                    a.fold(_.estimatedIncome, c => Some(c.estimatedIncome))
                },
                redirectToIfNoRequiredPreviousAnswer =
                  if (estimatedIncome.value > 0L)
                    routes.YearToDateLiabilityController.personalAllowance()
                  else
                    routes.YearToDateLiabilityController.estimatedIncome()
              )
            }

          case _ =>
            Redirect(routes.YearToDateLiabilityController.checkYourAnswers())

        }
    }
  }

  def hasEstimatedDetailsSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withFillingOutReturnAndYTDLiabilityAnswers(request) {
      case (_, fillingOutReturn, answers) =>
        (answers, fillingOutReturn.draftReturn) match {
          case (nonCalculatedAnswers: NonCalculatedYearToDateLiabilityAnswers, draftReturn: DraftReturn) =>
            handleNonCalculatedEstimatedDetailsSubmit(nonCalculatedAnswers, draftReturn, fillingOutReturn)

          case (calculatedAnswers: CalculatedYearToDateLiabilityAnswers, draftReturn: SingleDisposalDraftReturn) =>
            handleNonCalculatedEstimatedDetailsSubmit(calculatedAnswers, draftReturn, fillingOutReturn)

          case _ =>
            Redirect(routes.YearToDateLiabilityController.checkYourAnswers())
        }
    }
  }

  private def handleNonCalculatedEstimatedDetailsSubmit(
    calculatedAnswers: CalculatedYearToDateLiabilityAnswers,
    draftReturn: SingleDisposalDraftReturn,
    fillingOutReturn: FillingOutReturn
  )(implicit request: RequestWithSessionData[_]): Future[Result] =
    withEstimatedIncome(calculatedAnswers) { estimatedIncome =>
      commonSubmitBehaviour(fillingOutReturn, draftReturn, calculatedAnswers)(
        form = hasEstimatedDetailsForm
      )(
        page = hasEstimatedDetailsPage(_, _)
      )(
        requiredPreviousAnswer = { a =>
          if (estimatedIncome.value > 0L)
            a.fold(_.personalAllowance, _.personalAllowance)
          else
            a.fold(_.estimatedIncome, c => Some(c.estimatedIncome))
        },
        redirectToIfNoRequiredPreviousAnswer =
          if (estimatedIncome.value > 0L)
            routes.YearToDateLiabilityController.personalAllowance()
          else
            routes.YearToDateLiabilityController.estimatedIncome()
      ) {
        case (hasEstimated, draftReturn) =>
          if (calculatedAnswers
                .fold(_.hasEstimatedDetails, c => Some(c.hasEstimatedDetails))
                .contains(hasEstimated)) {
            draftReturn
          } else {
            draftReturn.copy(
              yearToDateLiabilityAnswers = Some(
                calculatedAnswers.fold(
                  _.copy(
                    hasEstimatedDetails = Some(hasEstimated),
                    calculatedTaxDue    = None,
                    mandatoryEvidence   = None
                  ),
                  complete =>
                    IncompleteCalculatedYearToDateLiabilityAnswers(
                      Some(complete.estimatedIncome),
                      complete.personalAllowance,
                      Some(hasEstimated),
                      None,
                      None,
                      None
                    )
                )
              )
            )
          }
      }
    }

  private def handleNonCalculatedEstimatedDetailsSubmit(
    nonCalculatedAnswers: NonCalculatedYearToDateLiabilityAnswers,
    draftReturn: DraftReturn,
    fillingOutReturn: FillingOutReturn
  )(implicit request: RequestWithSessionData[_]): Future[Result] =
    commonSubmitBehaviour(fillingOutReturn, draftReturn, nonCalculatedAnswers)(
      form = hasEstimatedDetailsForm
    )(
      page = hasEstimatedDetailsPage(_, _)
    )(
      requiredPreviousAnswer               = _.fold(_.taxableGainOrLoss, c => Some(c.taxableGainOrLoss)),
      redirectToIfNoRequiredPreviousAnswer = routes.YearToDateLiabilityController.taxableGainOrLoss()
    ) {
      case (hasEstimated, draftReturn) =>
        if (nonCalculatedAnswers
              .fold(_.hasEstimatedDetails, c => Some(c.hasEstimatedDetails))
              .contains(hasEstimated)) {
          draftReturn
        } else {
          val updatedAnswers = nonCalculatedAnswers.fold(
            _.copy(hasEstimatedDetails = Some(hasEstimated)),
            _.copy(hasEstimatedDetails = hasEstimated)
          )

          draftReturn.fold(
            _.copy(
              yearToDateLiabilityAnswers = Some(updatedAnswers)
            ),
            _.copy(yearToDateLiabilityAnswers = Some(updatedAnswers))
          )
        }
    }

  def taxDue(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withFillingOutReturnAndYTDLiabilityAnswers(request) {
      case (_, fillingOutReturn, answers) =>
        (answers, fillingOutReturn.draftReturn) match {
          case (calculatedAnswers: CalculatedYearToDateLiabilityAnswers, draftReturn: SingleDisposalDraftReturn) =>
            withEstimatedIncome(calculatedAnswers) { estimatedIncome =>
              withPersonalAllowance(calculatedAnswers) { personalAllowance =>
                withHasEstimatedDetails(calculatedAnswers) { _ =>
                  withCompleteJourneys(draftReturn) {
                    case (triage, disposalDetails, acquisitionDetails, reliefDetails, exemptionsAndLossesDetails) =>
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
                        )(page = taxDuePage(
                          _,
                          _,
                          triage,
                          disposalDetails,
                          acquisitionDetails,
                          reliefDetails,
                          exemptionsAndLossesDetails,
                          estimatedIncome,
                          personalAllowance,
                          calculatedTaxDue
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
              }
            }

          case _ =>
            Redirect(routes.YearToDateLiabilityController.checkYourAnswers())

        }
    }
  }

  def taxDueSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withFillingOutReturnAndYTDLiabilityAnswers(request) {
      case (_, fillingOutReturn, answers) =>
        (answers, fillingOutReturn.draftReturn) match {
          case (calculatedAnswers: CalculatedYearToDateLiabilityAnswers, draftReturn: SingleDisposalDraftReturn) =>
            withEstimatedIncome(calculatedAnswers) { estimatedIncome =>
              withPersonalAllowance(calculatedAnswers) { personalAllowance =>
                withHasEstimatedDetails(calculatedAnswers) { _ =>
                  withCompleteJourneys(draftReturn) {
                    case (triage, disposalDetails, acquisitionDetails, reliefDetails, exemptionsAndLossesDetails) =>
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
                        commonSubmitBehaviour(fillingOutReturn, draftReturn, calculatedAnswers)(
                          form = taxDueForm
                        )(page = taxDuePage(
                          _,
                          _,
                          triage,
                          disposalDetails,
                          acquisitionDetails,
                          reliefDetails,
                          exemptionsAndLossesDetails,
                          estimatedIncome,
                          personalAllowance,
                          calculatedTaxDue
                        )
                        )(
                          _.fold(
                            _.hasEstimatedDetails,
                            c => Some(c.hasEstimatedDetails)
                          ),
                          routes.YearToDateLiabilityController.hasEstimatedDetails()
                        ) {
                          case (t, draftReturn) =>
                            val taxDue = AmountInPence.fromPounds(t)
                            if (calculatedAnswers.fold(_.taxDue, c => Some(c.taxDue)).contains(taxDue)) {
                              draftReturn
                            } else {
                              val updatedAnswers =
                                calculatedAnswers.fold(
                                  _.copy(taxDue = Some(taxDue), mandatoryEvidence = None),
                                  complete =>
                                    IncompleteCalculatedYearToDateLiabilityAnswers(
                                      Some(complete.estimatedIncome),
                                      complete.personalAllowance,
                                      Some(complete.hasEstimatedDetails),
                                      Some(calculatedTaxDue),
                                      Some(taxDue),
                                      None
                                    )
                                )
                              draftReturn.copy(yearToDateLiabilityAnswers = Some(updatedAnswers))
                            }
                        }
                      }
                  }
                }
              }
            }

          case _ =>
            Redirect(routes.YearToDateLiabilityController.checkYourAnswers())

        }
    }
  }

  def uploadMandatoryEvidence(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withFillingOutReturnAndYTDLiabilityAnswers(request) {
      case (_, fillingOutReturn, answers) =>
        (answers, fillingOutReturn.draftReturn) match {
          case (calculatedAnswers: CalculatedYearToDateLiabilityAnswers, _: SingleDisposalDraftReturn) =>
            withTaxDueAndCalculatedTaxDue(calculatedAnswers) { (taxDue, calculatedTaxDue) =>
              if (calculatedTaxDue.amountOfTaxDue === taxDue) {
                Redirect(routes.YearToDateLiabilityController.checkYourAnswers())
              } else {
                val form = calculatedAnswers
                  .fold(_.mandatoryEvidence, _.mandatoryEvidence)
                  .fold(mandatoryEvidenceForm)(mandatoryEvidenceForm.fill)
                val backLink = calculatedAnswers.fold(
                  _ => routes.YearToDateLiabilityController.taxDue(),
                  _ => routes.YearToDateLiabilityController.checkYourAnswers()
                )

                Ok(mandatoryEvidencePage(form, backLink))
              }
            }

          case _ =>
            Redirect(routes.YearToDateLiabilityController.checkYourAnswers())

        }
    }
  }

  def uploadMandatoryEvidenceSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async {
    implicit request =>
      withFillingOutReturnAndYTDLiabilityAnswers(request) {
        case (_, fillingOutReturn, answers) =>
          (answers, fillingOutReturn.draftReturn) match {
            case (calculatedAnswers: CalculatedYearToDateLiabilityAnswers, draftReturn: SingleDisposalDraftReturn) =>
              withTaxDueAndCalculatedTaxDue(calculatedAnswers) { (taxDue, calculatedTaxDue) =>
                if (calculatedTaxDue.amountOfTaxDue === taxDue) {
                  Redirect(routes.YearToDateLiabilityController.checkYourAnswers())
                } else {
                  lazy val backLink = calculatedAnswers.fold(
                    _ => routes.YearToDateLiabilityController.taxDue(),
                    _ => routes.YearToDateLiabilityController.checkYourAnswers()
                  )
                  mandatoryEvidenceForm
                    .bindFromRequest()
                    .fold(
                      formWithErrors => BadRequest(mandatoryEvidencePage(formWithErrors, backLink)), { s =>
                        val updatedDraftReturn = draftReturn.copy(
                          yearToDateLiabilityAnswers = Some(
                            calculatedAnswers.fold(
                              _.copy(mandatoryEvidence = Some(s)),
                              _.copy(mandatoryEvidence = Some(s))
                            )
                          )
                        )

                        val result =
                          for {
                            _ <- returnsService.storeDraftReturn(
                                  updatedDraftReturn,
                                  fillingOutReturn.subscribedDetails.cgtReference,
                                  fillingOutReturn.agentReferenceNumber
                                )
                            _ <- EitherT(
                                  updateSession(sessionStore, request)(
                                    _.copy(journeyStatus = Some(fillingOutReturn.copy(draftReturn = updatedDraftReturn))
                                    )
                                  )
                                )
                          } yield ()

                        result.fold(
                          { e =>
                            logger.warn("Could not update return", e)
                            errorHandler.errorResult()
                          },
                          _ => Redirect(routes.YearToDateLiabilityController.checkYourAnswers())
                        )
                      }
                    )

                }
              }

            case _ =>
              Redirect(routes.YearToDateLiabilityController.checkYourAnswers())

          }
      }
  }

  def taxableGainOrLoss(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withFillingOutReturnAndYTDLiabilityAnswers(request) {
      case (_, _, answers) =>
        answers match {
          case _: CalculatedYearToDateLiabilityAnswers =>
            Redirect(routes.YearToDateLiabilityController.checkYourAnswers())

          case nonCalculatedAnswers: NonCalculatedYearToDateLiabilityAnswers =>
            commonDisplayBehaviour(
              nonCalculatedAnswers
            )(form = _.fold(
              _.taxableGainOrLoss.fold(taxableGainOrLossForm)(a => taxableGainOrLossForm.fill(a.inPounds())),
              c => taxableGainOrLossForm.fill(c.taxableGainOrLoss.inPounds())
            )
            )(
              page = taxableGainOrLossPage(_, _)
            )(
              requiredPreviousAnswer               = _ => Some(()),
              redirectToIfNoRequiredPreviousAnswer = controllers.returns.routes.TaskListController.taskList()
            )
        }
    }
  }

  def taxableGainOrLossSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withFillingOutReturnAndYTDLiabilityAnswers(request) {
      case (_, fillingOutReturn, answers) =>
        answers match {
          case _: CalculatedYearToDateLiabilityAnswers =>
            Redirect(routes.YearToDateLiabilityController.checkYourAnswers())

          case nonCalculatedAnswers: NonCalculatedYearToDateLiabilityAnswers =>
            commonSubmitBehaviour(
              fillingOutReturn,
              fillingOutReturn.draftReturn,
              nonCalculatedAnswers
            )(form = taxableGainOrLossForm)(
              page = taxableGainOrLossPage(_, _)
            )(
              requiredPreviousAnswer               = _ => Some(()),
              redirectToIfNoRequiredPreviousAnswer = controllers.returns.routes.TaskListController.taskList()
            ) {
              case (amount, draftReturn) =>
                val newAnswers = nonCalculatedAnswers.fold(
                  _.copy(taxableGainOrLoss = Some(AmountInPence.fromPounds(amount))),
                  _.copy(taxableGainOrLoss = AmountInPence.fromPounds(amount))
                )
                draftReturn.fold(
                  _.copy(yearToDateLiabilityAnswers = Some(newAnswers)),
                  _.copy(yearToDateLiabilityAnswers = Some(newAnswers))
                )
            }
        }
    }
  }

  def nonCalculatedEnterTaxDue(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withFillingOutReturnAndYTDLiabilityAnswers(request) {
      case (_, _, answers) =>
        answers match {
          case _: CalculatedYearToDateLiabilityAnswers =>
            Redirect(routes.YearToDateLiabilityController.checkYourAnswers())

          case nonCalculatedAnswers: NonCalculatedYearToDateLiabilityAnswers =>
            commonDisplayBehaviour(
              nonCalculatedAnswers
            )(form = _.fold(
              _.taxDue.fold(nonCalculatedTaxDueForm)(a => nonCalculatedTaxDueForm.fill(a.inPounds())),
              c => nonCalculatedTaxDueForm.fill(c.taxDue.inPounds())
            )
            )(
              page = nonCalculatedEnterTaxDuePage(_, _)
            )(
              requiredPreviousAnswer               = _.fold(_.hasEstimatedDetails, c => Some(c.hasEstimatedDetails)),
              redirectToIfNoRequiredPreviousAnswer = routes.YearToDateLiabilityController.hasEstimatedDetails()
            )

        }
    }
  }

  def nonCalculatedEnterTaxDueSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async {
    implicit request =>
      withFillingOutReturnAndYTDLiabilityAnswers(request) {
        case (_, fillingOutReturn, answers) =>
          answers match {
            case _: CalculatedYearToDateLiabilityAnswers =>
              Redirect(routes.YearToDateLiabilityController.checkYourAnswers())

            case nonCalculatedAnswers: NonCalculatedYearToDateLiabilityAnswers =>
              commonSubmitBehaviour(
                fillingOutReturn,
                fillingOutReturn.draftReturn,
                nonCalculatedAnswers
              )(form = nonCalculatedTaxDueForm)(
                page = nonCalculatedEnterTaxDuePage(_, _)
              )(
                requiredPreviousAnswer               = _.fold(_.hasEstimatedDetails, c => Some(c.hasEstimatedDetails)),
                redirectToIfNoRequiredPreviousAnswer = routes.YearToDateLiabilityController.hasEstimatedDetails()
              ) {
                case (amount, draftReturn) =>
                  val newAnswers = nonCalculatedAnswers.fold(
                    _.copy(taxDue = Some(AmountInPence.fromPounds(amount))),
                    _.copy(taxDue = AmountInPence.fromPounds(amount))
                  )
                  draftReturn.fold(
                    _.copy(yearToDateLiabilityAnswers = Some(newAnswers)),
                    _.copy(yearToDateLiabilityAnswers = Some(newAnswers))
                  )
              }
          }
      }
  }

  def checkYourAnswers(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withFillingOutReturnAndYTDLiabilityAnswers(request) {
      case (_, fillingOutReturn, answers) =>
        (answers, fillingOutReturn.draftReturn) match {
          case (c: CalculatedYearToDateLiabilityAnswers, s: SingleDisposalDraftReturn) =>
            checkYourAnswersHandleCalculated(c, fillingOutReturn, s)

          case (n: NonCalculatedYearToDateLiabilityAnswers, d) =>
            checkYourAnswersHandleNonCalculated(n, fillingOutReturn, d)

          case (_: CalculatedYearToDateLiabilityAnswers, _: MultipleDisposalsDraftReturn) =>
            logger.warn("Found calculated year to date liability answers on a multiple disposals draft return")
            errorHandler.errorResult()

        }

    }
  }

  private def checkYourAnswersHandleNonCalculated(
    answers: NonCalculatedYearToDateLiabilityAnswers,
    fillingOutReturn: FillingOutReturn,
    draftReturn: DraftReturn
  )(implicit request: RequestWithSessionData[_]): Future[Result] =
    answers match {
      case IncompleteNonCalculatedYearToDateLiabilityAnswers(None, _, _) =>
        Redirect(routes.YearToDateLiabilityController.taxableGainOrLoss())

      case IncompleteNonCalculatedYearToDateLiabilityAnswers(_, None, _) =>
        Redirect(routes.YearToDateLiabilityController.hasEstimatedDetails())

      case IncompleteNonCalculatedYearToDateLiabilityAnswers(_, _, None) =>
        Redirect(routes.YearToDateLiabilityController.nonCalculatedEnterTaxDue())

      case IncompleteNonCalculatedYearToDateLiabilityAnswers(Some(t), Some(e), Some(d)) =>
        val completeAnswers = CompleteNonCalculatedYearToDateLiabilityAnswers(t, e, d)
        val updatedDraftReturn = draftReturn.fold(
          _.copy(yearToDateLiabilityAnswers = Some(completeAnswers)),
          _.copy(yearToDateLiabilityAnswers = Some(completeAnswers))
        )
        val updatedJourney = fillingOutReturn.copy(draftReturn = updatedDraftReturn)
        val result = for {
          _ <- returnsService.storeDraftReturn(
                updatedDraftReturn,
                fillingOutReturn.subscribedDetails.cgtReference,
                fillingOutReturn.agentReferenceNumber
              )
          _ <- EitherT(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney))))
        } yield ()

        result.fold(
          { e =>
            logger.warn("Cold not store complete year to date liability answers", e)
            errorHandler.errorResult()
          },
          _ => Ok(nonCalculatedCheckYourAnswersPage(completeAnswers))
        )

      case c: CompleteNonCalculatedYearToDateLiabilityAnswers =>
        Ok(nonCalculatedCheckYourAnswersPage(c))

    }

  private def checkYourAnswersHandleCalculated(
    answers: CalculatedYearToDateLiabilityAnswers,
    fillingOutReturn: FillingOutReturn,
    draftReturn: SingleDisposalDraftReturn
  )(implicit request: RequestWithSessionData[_]): Future[Result] =
    answers match {
      case c: CompleteCalculatedYearToDateLiabilityAnswers =>
        Ok(calculatedCheckYouAnswersPage(c))

      case IncompleteCalculatedYearToDateLiabilityAnswers(None, _, _, _, _, _) =>
        Redirect(routes.YearToDateLiabilityController.estimatedIncome())

      case IncompleteCalculatedYearToDateLiabilityAnswers(Some(p), None, _, _, _, _) if p.value > 0L =>
        Redirect(routes.YearToDateLiabilityController.personalAllowance())

      case IncompleteCalculatedYearToDateLiabilityAnswers(_, _, None, _, _, _) =>
        Redirect(routes.YearToDateLiabilityController.hasEstimatedDetails())

      case IncompleteCalculatedYearToDateLiabilityAnswers(_, _, _, None, _, _) =>
        Redirect(routes.YearToDateLiabilityController.taxDue())

      case IncompleteCalculatedYearToDateLiabilityAnswers(_, _, _, _, None, _) =>
        Redirect(routes.YearToDateLiabilityController.taxDue())

      case IncompleteCalculatedYearToDateLiabilityAnswers(
          _,
          _,
          Some(_),
          Some(calculatedTaxDue),
          Some(taxDue),
          None
          ) if calculatedTaxDue.amountOfTaxDue =!= taxDue =>
        Redirect(routes.YearToDateLiabilityController.uploadMandatoryEvidence())

      case IncompleteCalculatedYearToDateLiabilityAnswers(Some(e), p, Some(h), Some(c), Some(t), m) =>
        val completeAnswers = CompleteCalculatedYearToDateLiabilityAnswers(e, p, h, c, t, m)
        val newDraftReturn =
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

        result.fold({ e =>
          logger.warn("Could not update session", e)
          errorHandler.errorResult()
        }, _ => Ok(calculatedCheckYouAnswersPage(completeAnswers)))
    }

  def checkYourAnswersSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withFillingOutReturnAndYTDLiabilityAnswers(request) {
      case _ =>
        Redirect(controllers.returns.routes.TaskListController.taskList())
    }
  }

}

object YearToDateLiabilityController {

  val estimatedIncomeForm: Form[BigDecimal] =
    Form(
      mapping(
        "estimatedIncome" -> of(MoneyUtils.amountInPoundsFormatter(_ < 0, _ > MoneyUtils.maxAmountOfPounds))
      )(identity)(Some(_))
    )

  def personalAllowanceForm(disposalDate: DisposalDate): Form[BigDecimal] =
    Form(
      mapping(
        "personalAllowance" -> of(
          MoneyUtils.amountInPoundsFormatter(_ < 0, _ > disposalDate.taxYear.personalAllowance.inPounds())
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
        "taxDue" -> of(MoneyUtils.amountInPoundsFormatter(_ < 0, _ > MoneyUtils.maxAmountOfPounds))
      )(identity)(Some(_))
    )

  val mandatoryEvidenceForm: Form[String] =
    Form(
      mapping(
        "mandatoryEvidence" -> nonEmptyText
      )(identity)(Some(_))
    )

  val taxableGainOrLossForm: Form[BigDecimal] = {
    val (outerId, gainId, lossId) = ("taxableGainOrLoss", "taxableGain", "netLoss")

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
        Map(outerId -> "0", gainId -> MoneyUtils.formatAmountOfMoneyWithoutPoundSign(d))
      else if (d < 0)
        Map(outerId -> "1", lossId -> MoneyUtils.formatAmountOfMoneyWithoutPoundSign((d * -1)))
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
        "nonCalculatedTaxDue" -> of(MoneyUtils.amountInPoundsFormatter(_ < 0, _ > MoneyUtils.maxAmountOfPounds))
      )(identity)(Some(_))
    )

}
