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

import cats.data.EitherT
import cats.instances.boolean._
import cats.instances.future._
import cats.syntax.eq._
import com.google.inject.Inject
import play.api.Configuration
import play.api.data.Form
import play.api.data.Forms.{mapping, of}
import play.api.http.Writeable
import play.api.mvc._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.yeartodatelliability.YearToDateLiabilityFirstReturnController._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.FillingOutReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.AcquisitionDetailsAnswers.CompleteAcquisitionDetailsAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.DisposalDetailsAnswers.CompleteDisposalDetailsAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ExemptionAndLossesAnswers.CompleteExemptionAndLossesAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.IndividualTriageAnswers.CompleteIndividualTriageAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ReliefDetailsAnswers.CompleteReliefDetailsAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.YearToDateLiabilityAnswers.{CompleteYearToDateLiabilityAnswers, IncompleteYearToDateLiabilityAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{AmountInPence, BooleanFormatter, MoneyUtils, SessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging.LoggerOps
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views.html.returns.{ytdliability => pages}
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

class YearToDateLiabilityFirstReturnController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionStore,
  val errorHandler: ErrorHandler,
  returnsService: ReturnsService,
  cc: MessagesControllerComponents,
  val config: Configuration,
  estimatedIncomePage: pages.estimated_income,
  personalAllowancePage: pages.personal_allowance,
  hasEstimatedDetailsPage: pages.has_estimated_details,
  taxDuePage: pages.tax_due,
  checkYouAnswersPage: pages.check_your_answers
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
      case Some((s, r: FillingOutReturn)) =>
        r.draftReturn.yearToDateLiabilityAnswers.fold[Future[Result]](
          f(s, r, IncompleteYearToDateLiabilityAnswers.empty)
        )(f(s, r, _))
      case _ => Redirect(controllers.routes.StartController.start())
    }

  private def withCompleteJourneys(fillingOutReturn: FillingOutReturn)(
    f: (
      CompleteIndividualTriageAnswers,
      CompleteDisposalDetailsAnswers,
      CompleteAcquisitionDetailsAnswers,
      CompleteReliefDetailsAnswers,
      CompleteExemptionAndLossesAnswers
    ) => Future[Result]
  ): Future[Result] =
    (
      fillingOutReturn.draftReturn.triageAnswers,
      fillingOutReturn.draftReturn.disposalDetailsAnswers,
      fillingOutReturn.draftReturn.acquisitionDetailsAnswers,
      fillingOutReturn.draftReturn.reliefDetailsAnswers,
      fillingOutReturn.draftReturn.exemptionAndLossesAnswers
    ) match {
      case (
          t: CompleteIndividualTriageAnswers,
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
    fillingOutReturn: FillingOutReturn
  )(f: DisposalDate => Future[Result]): Future[Result] =
    fillingOutReturn.draftReturn.triageAnswers
      .fold(_.disposalDate, c => Some(c.disposalDate))
      .fold[Future[Result]](
        Redirect(controllers.returns.routes.TaskListController.taskList())
      )(f)

  private def withEstimatedIncome(
    answers: YearToDateLiabilityAnswers
  )(f: AmountInPence => Future[Result]): Future[Result] =
    answers
      .fold(_.estimatedIncome, c => Some(c.estimatedIncome))
      .fold[Future[Result]](Redirect(routes.YearToDateLiabilityFirstReturnController.checkYourAnswers()))(f)

  private def withPersonalAllowance(
    answers: YearToDateLiabilityAnswers
  )(f: AmountInPence => Future[Result]): Future[Result] =
    withEstimatedIncome(answers) { estimatedIncome =>
      if (estimatedIncome.value > 0L)
        answers
          .fold(_.personalAllowance, c => c.personalAllowance)
          .fold[Future[Result]](
            Redirect(routes.YearToDateLiabilityFirstReturnController.checkYourAnswers())
          )(f)
      else
        f(AmountInPence.zero)
    }

  private def withHasEstimatedDetailsAndCalculatedTax(
    answers: YearToDateLiabilityAnswers
  )(f: HasEstimatedDetailsWithCalculatedTaxDue => Future[Result]): Future[Result] =
    answers
      .fold(_.hasEstimatedDetails, c => Some(c.hasEstimatedDetails))
      .fold[Future[Result]](
        Redirect(routes.YearToDateLiabilityFirstReturnController.checkYourAnswers())
      )(f)

  private def commonDisplayBehaviour[A, P: Writeable, R](
    currentAnswers: YearToDateLiabilityAnswers
  )(form: YearToDateLiabilityAnswers => Form[A])(
    page: (Form[A], Call) => P
  )(
    requiredPreviousAnswer: YearToDateLiabilityAnswers => Option[R],
    redirectToIfNoRequiredPreviousAnswer: Call
  ): Future[Result] =
    if (requiredPreviousAnswer(currentAnswers).isDefined) {
      val backLink = currentAnswers.fold(
        _ => redirectToIfNoRequiredPreviousAnswer,
        _ => routes.YearToDateLiabilityFirstReturnController.checkYourAnswers()
      )
      Ok(page(form(currentAnswers), backLink))
    } else {
      Redirect(redirectToIfNoRequiredPreviousAnswer)
    }

  private def commonSubmitBehaviour[A, P: Writeable, R](
    currentFillingOutReturn: FillingOutReturn,
    currentAnswers: YearToDateLiabilityAnswers
  )(form: Form[A])(
    page: (Form[A], Call) => P
  )(
    requiredPreviousAnswer: YearToDateLiabilityAnswers => Option[R],
    redirectToIfNoRequiredPreviousAnswer: Call
  )(
    updateAnswers: (A, DraftReturn) => DraftReturn
  )(
    implicit request: RequestWithSessionData[_]
  ): Future[Result] =
    if (requiredPreviousAnswer(currentAnswers).isDefined) {
      lazy val backLink = currentAnswers.fold(
        _ => redirectToIfNoRequiredPreviousAnswer,
        _ => controllers.returns.yeartodatelliability.routes.YearToDateLiabilityFirstReturnController.checkYourAnswers()
      )
      form
        .bindFromRequest()
        .fold(
          formWithErrors => BadRequest(page(formWithErrors, backLink)), { value =>
            val newDraftReturn = updateAnswers(value, currentFillingOutReturn.draftReturn)

            val result = for {
              _ <- if (newDraftReturn === currentFillingOutReturn.draftReturn) EitherT.pure(())
                  else returnsService.storeDraftReturn(newDraftReturn)
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
              _ => Redirect(routes.YearToDateLiabilityFirstReturnController.checkYourAnswers())
            )

          }
        )
    } else {
      Redirect(redirectToIfNoRequiredPreviousAnswer)
    }

  def estimatedIncome(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withFillingOutReturnAndYTDLiabilityAnswers(request) {
      case (_, fillingOutReturn, answers) =>
        withDisposalDate(fillingOutReturn) { disposalDate =>
          commonDisplayBehaviour(answers)(
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
    }
  }

  def estimatedIncomeSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withFillingOutReturnAndYTDLiabilityAnswers(request) {
      case (_, fillingOutReturn, answers) =>
        withDisposalDate(fillingOutReturn) { disposalDate =>
          commonSubmitBehaviour(fillingOutReturn, answers)(
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

                val newAnswers =
                  answers.fold(
                    { incomplete =>
                      val hadRequiredPersonalAllowance = incomplete.estimatedIncome.exists(_.value > 0L)
                      val nowRequiresPersonalAllowance = i > 0

                      if (hadRequiredPersonalAllowance =!= nowRequiresPersonalAllowance)
                        incomplete.copy(estimatedIncome = Some(estimatedIncome), personalAllowance = None)
                      else
                        incomplete.copy(estimatedIncome = Some(estimatedIncome))
                    },
                    _ => IncompleteYearToDateLiabilityAnswers.empty.copy(estimatedIncome = Some(estimatedIncome))
                  )

                draftReturn.copy(yearToDateLiabilityAnswers = Some(newAnswers))
            }
          )
        }
    }
  }

  def personalAllowance(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withFillingOutReturnAndYTDLiabilityAnswers(request) {
      case (_, fillingOutReturn, answers) =>
        withDisposalDate(fillingOutReturn) { disposalDate =>
          withEstimatedIncome(answers) { estimatedIncome =>
            if (estimatedIncome.value > 0L) {
              commonDisplayBehaviour(answers)(
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
                routes.YearToDateLiabilityFirstReturnController.estimatedIncome()
              )
            } else {
              Redirect(routes.YearToDateLiabilityFirstReturnController.checkYourAnswers())
            }
          }
        }
    }
  }

  def personalAllowanceSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withFillingOutReturnAndYTDLiabilityAnswers(request) {
      case (_, fillingOutReturn, answers) =>
        withDisposalDate(fillingOutReturn) { disposalDate =>
          withEstimatedIncome(answers) { estimatedIncome =>
            if (estimatedIncome.value > 0L) {
              commonSubmitBehaviour(fillingOutReturn, answers)(
                form = personalAllowanceForm(disposalDate)
              )(
                page = personalAllowancePage(_, _, disposalDate)
              )(
                requiredPreviousAnswer = _.fold(_.estimatedIncome, c => Some(c.estimatedIncome)),
                routes.YearToDateLiabilityFirstReturnController.estimatedIncome()
              ) {
                case (p, draftReturn) =>
                  val personalAllowance = AmountInPence.fromPounds(p)
                  draftReturn.copy(
                    yearToDateLiabilityAnswers = Some(
                      answers.fold(
                        _.copy(personalAllowance = Some(personalAllowance)),
                        complete =>
                          IncompleteYearToDateLiabilityAnswers(
                            Some(complete.estimatedIncome),
                            Some(personalAllowance),
                            None,
                            None
                          )
                      )
                    )
                  )
              }
            } else {
              Redirect(routes.YearToDateLiabilityFirstReturnController.checkYourAnswers())
            }
          }
        }
    }
  }

  def hasEstimatedDetails(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withFillingOutReturnAndYTDLiabilityAnswers(request) {
      case (_, _, answers) =>
        withEstimatedIncome(answers) { estimatedIncome =>
          commonDisplayBehaviour(answers)(
            form = _.fold(
              _.hasEstimatedDetails.fold(hasEstimatedDetailsForm)(h =>
                hasEstimatedDetailsForm.fill(h.hasEstimatedDetails)
              ),
              c => hasEstimatedDetailsForm.fill(c.hasEstimatedDetails.hasEstimatedDetails)
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
                routes.YearToDateLiabilityFirstReturnController.personalAllowance()
              else
                routes.YearToDateLiabilityFirstReturnController.estimatedIncome()
          )
        }
    }
  }

  def hasEstimatedDetailsSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withFillingOutReturnAndYTDLiabilityAnswers(request) {
      case (_, fillingOutReturn, answers) =>
        withEstimatedIncome(answers) { estimatedIncome =>
          withPersonalAllowance(answers) { personalAllowance =>
            withCompleteJourneys(fillingOutReturn) {
              case (triage, disposalDetails, acquisitionDetails, reliefDetails, exemptionsAndLossesDetails) =>
                commonSubmitBehaviour(fillingOutReturn, answers)(
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
                      routes.YearToDateLiabilityFirstReturnController.personalAllowance()
                    else
                      routes.YearToDateLiabilityFirstReturnController.estimatedIncome()
                ) {
                  case (h, draftReturn) =>
                    val calculatedTaxDue =
                      CalculatedTaxDue.calculate(
                        triage,
                        disposalDetails,
                        acquisitionDetails,
                        reliefDetails,
                        exemptionsAndLossesDetails,
                        estimatedIncome,
                        personalAllowance
                      )
                    val hasEstimatedDetailsWithCalculatedTaxDue =
                      HasEstimatedDetailsWithCalculatedTaxDue(h, calculatedTaxDue)

                    draftReturn.copy(
                      yearToDateLiabilityAnswers = Some(
                        answers.fold(
                          _.copy(hasEstimatedDetails = Some(hasEstimatedDetailsWithCalculatedTaxDue)),
                          complete =>
                            IncompleteYearToDateLiabilityAnswers(
                              Some(complete.estimatedIncome),
                              complete.personalAllowance,
                              Some(hasEstimatedDetailsWithCalculatedTaxDue),
                              None
                            )
                        )
                      )
                    )
                }
            }
          }
        }
    }
  }

  def taxDue(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withFillingOutReturnAndYTDLiabilityAnswers(request) {
      case (_, fillingOutReturn, answers) =>
        withEstimatedIncome(answers) { estimatedIncome =>
          withPersonalAllowance(answers) { personalAllowance =>
            withHasEstimatedDetailsAndCalculatedTax(answers) { estimatedDetailsAndCalculatedTax =>
              withCompleteJourneys(fillingOutReturn) {
                case (triage, disposalDetails, acquisitionDetails, reliefDetails, exemptionsAndLossesDetails) =>
                  commonDisplayBehaviour(answers)(
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
                    estimatedDetailsAndCalculatedTax.calculatedTaxDue
                  )
                  )(
                    _.fold(_.hasEstimatedDetails, c => Some(c.hasEstimatedDetails)),
                    routes.YearToDateLiabilityFirstReturnController.hasEstimatedDetails()
                  )
              }
            }
          }
        }
    }
  }

  def taxDueSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withFillingOutReturnAndYTDLiabilityAnswers(request) {
      case (_, fillingOutReturn, answers) =>
        withEstimatedIncome(answers) { estimatedIncome =>
          withPersonalAllowance(answers) { personalAllowance =>
            withHasEstimatedDetailsAndCalculatedTax(answers) { estimatedDetailsAndCalculatedTax =>
              withCompleteJourneys(fillingOutReturn) {
                case (triage, disposalDetails, acquisitionDetails, reliefDetails, exemptionsAndLossesDetails) =>
                  commonSubmitBehaviour(fillingOutReturn, answers)(
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
                    estimatedDetailsAndCalculatedTax.calculatedTaxDue
                  )
                  )(
                    _.fold(_.hasEstimatedDetails, c => Some(c.hasEstimatedDetails)),
                    routes.YearToDateLiabilityFirstReturnController.hasEstimatedDetails()
                  ) {
                    case (t, draftReturn) =>
                      val updatedAnswers = answers.fold(
                        _.copy(taxDue = Some(AmountInPence.fromPounds(t))),
                        _.copy(taxDue = AmountInPence.fromPounds(t))
                      )
                      draftReturn.copy(yearToDateLiabilityAnswers = Some(updatedAnswers))
                  }
              }
            }
          }
        }
    }
  }

  def checkYourAnswers(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withFillingOutReturnAndYTDLiabilityAnswers(request) {
      case (_, fillingOutReturn, answers) =>
        answers match {
          case c: CompleteYearToDateLiabilityAnswers =>
            Ok(checkYouAnswersPage(c))

          case IncompleteYearToDateLiabilityAnswers(None, _, _, _) =>
            Redirect(routes.YearToDateLiabilityFirstReturnController.estimatedIncome())

          case IncompleteYearToDateLiabilityAnswers(Some(p), None, _, _) if p.value > 0L =>
            Redirect(routes.YearToDateLiabilityFirstReturnController.personalAllowance())

          case IncompleteYearToDateLiabilityAnswers(_, _, None, _) =>
            Redirect(routes.YearToDateLiabilityFirstReturnController.hasEstimatedDetails())

          case IncompleteYearToDateLiabilityAnswers(_, _, _, None) =>
            Redirect(routes.YearToDateLiabilityFirstReturnController.taxDue())

          case IncompleteYearToDateLiabilityAnswers(Some(e), p, Some(h), Some(t)) =>
            val completeAnswers = CompleteYearToDateLiabilityAnswers(e, p, h, t)
            val newDraftReturn =
              fillingOutReturn.draftReturn.copy(yearToDateLiabilityAnswers = Some(completeAnswers))

            val result = for {
              _ <- returnsService.storeDraftReturn(newDraftReturn)
              _ <- EitherT(
                    updateSession(sessionStore, request)(
                      _.copy(journeyStatus = Some(fillingOutReturn.copy(draftReturn = newDraftReturn)))
                    )
                  )
            } yield ()

            result.fold({ e =>
              logger.warn("Could not update session", e)
              errorHandler.errorResult()
            }, _ => Ok(checkYouAnswersPage(completeAnswers)))
        }
    }
  }

  def checkYourAnswersSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withFillingOutReturnAndYTDLiabilityAnswers(request) {
      case _ =>
        Redirect(controllers.returns.routes.TaskListController.taskList())
    }
  }

}

object YearToDateLiabilityFirstReturnController {

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

}
