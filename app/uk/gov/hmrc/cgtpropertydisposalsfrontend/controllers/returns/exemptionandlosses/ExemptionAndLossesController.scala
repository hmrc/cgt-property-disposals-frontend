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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.exemptionandlosses

import cats.data.EitherT
import cats.instances.future._
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
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.exemptionandlosses.ExemptionAndLossesController._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, StartingToAmendReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.{AmountInPence, MoneyUtils}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ExemptionAndLossesAnswers.{CompleteExemptionAndLossesAnswers, IncompleteExemptionAndLossesAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{DisposalDate, DraftReturn, ExemptionAndLossesAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging.LoggerOps
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views.html.returns.{exemptionandlosses => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class ExemptionAndLossesController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionStore,
  val errorHandler: ErrorHandler,
  returnsService: ReturnsService,
  cc: MessagesControllerComponents,
  val config: Configuration,
  inYearLossesPage: pages.in_year_losses,
  furtherReturnInYearLossesPage: pages.further_return_in_year_losses,
  previousYearsLossesPage: pages.previous_years_losses,
  furtherReturnPreviousYearsLossesPage: pages.further_return_previous_years_losses,
  annualExemptAmountPage: pages.annual_exempt_amount,
  checkYourAnswersPage: pages.check_your_answers
)(implicit viewConfig: ViewConfig, ec: ExecutionContext)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with Logging
    with SessionUpdates
    with StartingToAmendToFillingOutReturnBehaviour {

  private def withFillingOutReturnAndAnswers(
    f: (
      SessionData,
      FillingOutReturn,
      DraftReturn,
      ExemptionAndLossesAnswers
    ) => Future[Result]
  )(implicit request: RequestWithSessionData[_]): Future[Result] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {

      case Some((_, s: StartingToAmendReturn)) =>
        markUnmetDependency(s, sessionStore, errorHandler)

      case Some((s, r @ FillingOutReturn(_, _, _, d, _, _, _))) if r.isFurtherOrAmendReturn.contains(true) =>
        val answers = d
          .fold(
            _.exemptionAndLossesAnswers,
            _.exemptionAndLossesAnswers,
            _.exemptionAndLossesAnswers,
            _.exemptionAndLossesAnswers,
            _.exemptionAndLossesAnswers
          )
          .getOrElse(
            IncompleteExemptionAndLossesAnswers.empty.copy(
              annualExemptAmount = Some(AmountInPence.zero)
            )
          )
        f(s, r, d, answers)

      case Some((s, r @ FillingOutReturn(_, _, _, d, _, _, _))) =>
        val answers = d
          .fold(
            _.exemptionAndLossesAnswers,
            _.exemptionAndLossesAnswers,
            _.exemptionAndLossesAnswers,
            _.exemptionAndLossesAnswers,
            _.exemptionAndLossesAnswers
          )
          .getOrElse(IncompleteExemptionAndLossesAnswers.empty)
        f(s, r, d, answers)

      case _ =>
        Redirect(controllers.routes.StartController.start())
    }

  private def withDisposalDate(
    draftReturn: DraftReturn
  )(f: DisposalDate => Future[Result]): Future[Result] = {
    val disposalDate = draftReturn.fold(
      _.examplePropertyDetailsAnswers.flatMap(
        _.fold(
          _.disposalDate,
          c => Some(c.disposalDate)
        )
      ),
      _.triageAnswers.fold(
        _.disposalDate,
        c => Some(c.disposalDate)
      ),
      _.triageAnswers.fold(
        _.disposalDate,
        c => Some(c.disposalDate)
      ),
      _.triageAnswers.fold(
        i => i.completionDate.flatMap(d => i.taxYear.map(t => DisposalDate(d.value, t))),
        c => Some(DisposalDate(c.completionDate.value, c.taxYear))
      ),
      _.triageAnswers.fold(
        _.disposalDate,
        c => Some(c.disposalDate)
      )
    )

    disposalDate match {
      case Some(d) => f(d)
      case None    =>
        Redirect(controllers.returns.routes.TaskListController.taskList())
    }
  }

  private def withWasAUkResident(
    draftReturn: DraftReturn
  )(f: Boolean => Future[Result]): Future[Result] = {
    val wasUk =
      draftReturn
        .triageAnswers()
        .fold(
          _.fold(_.wasAUKResident, c => Some(c.countryOfResidence.isUk())),
          _.fold(_.wasAUKResident, c => Some(c.countryOfResidence.isUk()))
        )

    wasUk match {
      case Some(d) => f(d)
      case None    =>
        Redirect(controllers.returns.routes.TaskListController.taskList())
    }
  }

  private def commonDisplayBehaviour[A, P : Writeable, R](
    currentAnswers: ExemptionAndLossesAnswers
  )(form: ExemptionAndLossesAnswers => Form[A])(
    page: (Form[A], Call) => P
  )(
    requiredPreviousAnswer: ExemptionAndLossesAnswers => Option[R],
    redirectToIfNoRequiredPreviousAnswer: Call
  ): Future[Result] =
    if (requiredPreviousAnswer(currentAnswers).isDefined) {
      val backLink = currentAnswers.fold(
        _ => redirectToIfNoRequiredPreviousAnswer,
        _ => routes.ExemptionAndLossesController.checkYourAnswers()
      )

      Ok(page(form(currentAnswers), backLink))
    } else
      Redirect(redirectToIfNoRequiredPreviousAnswer)

  private def commonSubmitBehaviour[A, P : Writeable, R](
    currentFillingOutReturn: FillingOutReturn,
    currentDraftReturn: DraftReturn,
    currentAnswers: ExemptionAndLossesAnswers
  )(form: Form[A])(
    page: (Form[A], Call) => P
  )(
    requiredPreviousAnswer: ExemptionAndLossesAnswers => Option[R],
    redirectToIfNoRequiredPreviousAnswer: Call
  )(
    updateAnswers: (A, ExemptionAndLossesAnswers) => ExemptionAndLossesAnswers
  )(implicit
    request: RequestWithSessionData[_]
  ): Future[Result] =
    if (requiredPreviousAnswer(currentAnswers).isDefined) {
      lazy val backLink = currentAnswers.fold(
        _ => redirectToIfNoRequiredPreviousAnswer,
        _ => routes.ExemptionAndLossesController.checkYourAnswers()
      )
      form
        .bindFromRequest()
        .fold(
          formWithErrors => BadRequest(page(formWithErrors, backLink)),
          { value =>
            val newAnswers = updateAnswers(value, currentAnswers)

            if (currentAnswers === newAnswers)
              Redirect(routes.ExemptionAndLossesController.checkYourAnswers())
            else {
              val newDraftReturn = currentDraftReturn.fold(
                m =>
                  m.copy(
                    exemptionAndLossesAnswers = Some(newAnswers),
                    yearToDateLiabilityAnswers = m.yearToDateLiabilityAnswers
                      .flatMap(_.unsetAllButIncomeDetails())
                  ),
                s =>
                  s.copy(
                    exemptionAndLossesAnswers = Some(newAnswers),
                    yearToDateLiabilityAnswers = s.yearToDateLiabilityAnswers
                      .flatMap(_.unsetAllButIncomeDetails())
                  ),
                i =>
                  i.copy(
                    exemptionAndLossesAnswers = Some(newAnswers),
                    yearToDateLiabilityAnswers = i.yearToDateLiabilityAnswers
                      .flatMap(_.unsetAllButIncomeDetails())
                  ),
                m =>
                  m.copy(
                    exemptionAndLossesAnswers = Some(newAnswers),
                    yearToDateLiabilityAnswers = m.yearToDateLiabilityAnswers
                      .flatMap(_.unsetAllButIncomeDetails())
                  ),
                m =>
                  m.copy(
                    exemptionAndLossesAnswers = Some(newAnswers),
                    yearToDateLiabilityAnswers = m.yearToDateLiabilityAnswers
                      .flatMap(_.unsetAllButIncomeDetails())
                  )
              )

              val updatedJourney = currentFillingOutReturn
                .copy(draftReturn = newDraftReturn)

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
                  logger.warn("Could not update draft return", e)
                  errorHandler.errorResult()
                },
                _ =>
                  Redirect(
                    routes.ExemptionAndLossesController.checkYourAnswers()
                  )
              )
            }
          }
        )
    } else
      Redirect(redirectToIfNoRequiredPreviousAnswer)

  def inYearLosses(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndAnswers { (_, fillingOutReturn, draftReturn, answers) =>
        withDisposalDate(draftReturn) { disposalDate =>
          withWasAUkResident(draftReturn) { _ =>
            commonDisplayBehaviour(
              answers
            )(form =
              _.fold(
                _.inYearLosses.fold(inYearLossesForm)(l => inYearLossesForm.fill(l.inPounds())),
                c => inYearLossesForm.fill(c.inYearLosses.inPounds())
              )
            )(
              page =
                if (fillingOutReturn.isFurtherOrAmendReturn.contains(true))
                  furtherReturnInYearLossesPage(
                    _,
                    _,
                    fillingOutReturn.subscribedDetails.isATrust,
                    draftReturn.representativeType(),
                    fillingOutReturn.isAmendReturn
                  )
                else
                  inYearLossesPage(
                    _,
                    _,
                    disposalDate,
                    fillingOutReturn.subscribedDetails.isATrust,
                    draftReturn.representativeType(),
                    fillingOutReturn.isAmendReturn
                  )
            )(
              _ => Some(()),
              controllers.returns.routes.TaskListController.taskList()
            )
          }
        }
      }
    }

  def inYearLossesSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndAnswers { (_, fillingOutReturn, draftReturn, answers) =>
        withDisposalDate(draftReturn) { disposalDate =>
          withWasAUkResident(draftReturn) { _ =>
            commonSubmitBehaviour(
              fillingOutReturn,
              draftReturn,
              answers
            )(form = inYearLossesForm)(
              page =
                if (fillingOutReturn.isFurtherOrAmendReturn.contains(true))
                  furtherReturnInYearLossesPage(
                    _,
                    _,
                    fillingOutReturn.subscribedDetails.isATrust,
                    draftReturn.representativeType(),
                    fillingOutReturn.isAmendReturn
                  )
                else
                  inYearLossesPage(
                    _,
                    _,
                    disposalDate,
                    fillingOutReturn.subscribedDetails.isATrust,
                    draftReturn.representativeType(),
                    fillingOutReturn.isAmendReturn
                  )
            )(
              _ => Some(()),
              controllers.returns.routes.TaskListController.taskList()
            ) { (inYearLosses, answers) =>
              if (fillingOutReturn.isFurtherOrAmendReturn.contains(true)) {
                answers
                  .unset(_.previousYearsLosses)
                  .copy(
                    inYearLosses = Some(AmountInPence.fromPounds(inYearLosses))
                  )
              } else {
                answers.fold(
                  _.copy(inYearLosses = Some(AmountInPence.fromPounds(inYearLosses))),
                  _.copy(inYearLosses = AmountInPence.fromPounds(inYearLosses))
                )
              }

            }
          }
        }
      }
    }

  def previousYearsLosses(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndAnswers { (_, fillingOutReturn, draftReturn, answers) =>
        withWasAUkResident(draftReturn) { wasAUkResident =>
          commonDisplayBehaviour(
            answers
          )(form =
            _.fold(
              _.previousYearsLosses.fold(previousYearsLossesForm)(l => previousYearsLossesForm.fill(l.inPounds())),
              c => previousYearsLossesForm.fill(c.previousYearsLosses.inPounds())
            )
          )(
            page =
              if (fillingOutReturn.isFurtherOrAmendReturn.contains(true))
                furtherReturnPreviousYearsLossesPage(
                  _,
                  _,
                  fillingOutReturn.subscribedDetails.isATrust,
                  draftReturn.representativeType(),
                  fillingOutReturn.isAmendReturn
                )
              else
                previousYearsLossesPage(
                  _,
                  _,
                  wasAUkResident,
                  fillingOutReturn.subscribedDetails.isATrust,
                  draftReturn.representativeType(),
                  fillingOutReturn.isAmendReturn
                )
          )(
            requiredPreviousAnswer = _.fold(
              _.inYearLosses,
              c => Some(c.inYearLosses)
            ),
            redirectToIfNoRequiredPreviousAnswer = routes.ExemptionAndLossesController.inYearLosses()
          )
        }
      }
    }

  def previousYearsLossesSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndAnswers { (_, fillingOutReturn, draftReturn, answers) =>
        withWasAUkResident(draftReturn) { wasAUkResident =>
          commonSubmitBehaviour(
            fillingOutReturn,
            draftReturn,
            answers
          )(form = previousYearsLossesForm)(
            page =
              if (fillingOutReturn.isFurtherOrAmendReturn.contains(true))
                furtherReturnPreviousYearsLossesPage(
                  _,
                  _,
                  fillingOutReturn.subscribedDetails.isATrust,
                  draftReturn.representativeType(),
                  fillingOutReturn.isAmendReturn
                )
              else
                previousYearsLossesPage(
                  _,
                  _,
                  wasAUkResident,
                  fillingOutReturn.subscribedDetails.isATrust,
                  draftReturn.representativeType(),
                  fillingOutReturn.isAmendReturn
                )
          )(
            requiredPreviousAnswer = _.fold(
              _.inYearLosses,
              c => Some(c.inYearLosses)
            ),
            redirectToIfNoRequiredPreviousAnswer = routes.ExemptionAndLossesController.inYearLosses()
          ) { (previousYearLosses, answers) =>
            answers.fold(
              _.copy(previousYearsLosses = Some(AmountInPence.fromPounds(previousYearLosses))),
              _.copy(previousYearsLosses = AmountInPence.fromPounds(previousYearLosses))
            )
          }
        }
      }
    }

  def annualExemptAmount(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndAnswers { (_, fillingOutReturn, draftReturn, answers) =>
        withDisposalDate(draftReturn) { disposalDate =>
          commonDisplayBehaviour(
            answers
          )(form = { answers =>
            val emptyForm = annualExemptAmountForm(
              disposalDate.taxYear.annualExemptAmountGeneral
            )
            answers.fold(
              _.annualExemptAmount.fold(emptyForm)(a => emptyForm.fill(a.inPounds())),
              c => emptyForm.fill(c.annualExemptAmount.inPounds())
            )
          })(
            page = annualExemptAmountPage(
              _,
              _,
              disposalDate,
              fillingOutReturn.subscribedDetails.isATrust,
              draftReturn.representativeType(),
              fillingOutReturn.isAmendReturn
            )
          )(
            requiredPreviousAnswer = _.fold(
              _.previousYearsLosses,
              c => Some(c.previousYearsLosses)
            ),
            redirectToIfNoRequiredPreviousAnswer = routes.ExemptionAndLossesController.previousYearsLosses()
          )
        }
      }
    }

  def annualExemptAmountSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndAnswers { (_, fillingOutReturn, draftReturn, answers) =>
        withDisposalDate(draftReturn) { disposalDate =>
          commonSubmitBehaviour(
            fillingOutReturn,
            draftReturn,
            answers
          )(form =
            annualExemptAmountForm(
              disposalDate.taxYear.annualExemptAmountGeneral
            )
          )(
            page = { (form, backlink) =>
              val updatedForm = form.copy(errors =
                form.errors.map(
                  _.copy(args =
                    Seq(
                      MoneyUtils
                        .formatAmountOfMoneyWithoutPoundSign(
                          disposalDate.taxYear.annualExemptAmountGeneral
                            .inPounds()
                        )
                    )
                  )
                )
              )
              annualExemptAmountPage(
                updatedForm,
                backlink,
                disposalDate,
                fillingOutReturn.subscribedDetails.isATrust,
                draftReturn.representativeType(),
                fillingOutReturn.isAmendReturn
              )
            }
          )(
            requiredPreviousAnswer = _.fold(
              _.previousYearsLosses,
              c => Some(c.previousYearsLosses)
            ),
            redirectToIfNoRequiredPreviousAnswer = routes.ExemptionAndLossesController.previousYearsLosses()
          ) { (annualExemptAmount, answers) =>
            answers.fold(
              _.copy(annualExemptAmount = Some(AmountInPence.fromPounds(annualExemptAmount))),
              _.copy(annualExemptAmount = AmountInPence.fromPounds(annualExemptAmount))
            )
          }
        }
      }
    }

  def checkYourAnswers(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndAnswers { (_, fillingOutReturn, draftReturn, answers) =>
        withDisposalDate(draftReturn) { disposalDate =>
          answers match {
            case c: CompleteExemptionAndLossesAnswers =>
              Ok(
                checkYourAnswersPage(
                  c,
                  disposalDate,
                  fillingOutReturn.subscribedDetails.isATrust,
                  draftReturn.representativeType,
                  fillingOutReturn.isFurtherOrAmendReturn
                )
              )

            case IncompleteExemptionAndLossesAnswers(None, _, _) =>
              Redirect(routes.ExemptionAndLossesController.inYearLosses())

            case IncompleteExemptionAndLossesAnswers(_, None, _) =>
              Redirect(
                routes.ExemptionAndLossesController.previousYearsLosses()
              )

            case IncompleteExemptionAndLossesAnswers(_, _, None) =>
              Redirect(
                routes.ExemptionAndLossesController.annualExemptAmount()
              )

            case IncompleteExemptionAndLossesAnswers(
                  Some(i),
                  Some(p),
                  Some(a)
                ) =>
              val completeAnswers = CompleteExemptionAndLossesAnswers(i, p, a)
              val newDraftReturn  =
                draftReturn.fold(
                  _.copy(exemptionAndLossesAnswers = Some(completeAnswers)),
                  _.copy(exemptionAndLossesAnswers = Some(completeAnswers)),
                  _.copy(exemptionAndLossesAnswers = Some(completeAnswers)),
                  _.copy(exemptionAndLossesAnswers = Some(completeAnswers)),
                  _.copy(exemptionAndLossesAnswers = Some(completeAnswers))
                )

              val newJourney = fillingOutReturn.copy(draftReturn = newDraftReturn)

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
                  logger.warn("Could not update the session", e)
                  errorHandler.errorResult()
                },
                _ =>
                  Ok(
                    checkYourAnswersPage(
                      completeAnswers,
                      disposalDate,
                      fillingOutReturn.subscribedDetails.isATrust,
                      draftReturn.representativeType,
                      fillingOutReturn.isFurtherOrAmendReturn
                    )
                  )
              )
          }
        }

      }
    }

  def checkYourAnswersSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndAnswers { case _ =>
        Redirect(controllers.returns.routes.TaskListController.taskList())
      }
    }

}

object ExemptionAndLossesController {

  val inYearLossesForm: Form[BigDecimal] =
    MoneyUtils.amountInPoundsYesNoForm("inYearLosses", "inYearLossesValue")

  val previousYearsLossesForm: Form[BigDecimal] =
    MoneyUtils.amountInPoundsYesNoForm(
      "previousYearsLosses",
      "previousYearsLossesValue"
    )

  def annualExemptAmountForm(
    maximumAnnualExemptAmount: AmountInPence
  ): Form[BigDecimal] =
    Form(
      mapping(
        "annualExemptAmount" -> of(
          MoneyUtils.amountInPoundsFormatter(
            _ < 0,
            _ > maximumAnnualExemptAmount.inPounds()
          )
        )
      )(identity)(Some(_))
    )

}
