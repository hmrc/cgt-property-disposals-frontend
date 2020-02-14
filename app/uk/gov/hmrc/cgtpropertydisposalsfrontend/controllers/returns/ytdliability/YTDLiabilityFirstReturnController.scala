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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.ytdliability

import cats.data.EitherT
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
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.ytdliability.YTDLiabilityFirstReturnController._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.FillingOutReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.YTDLiabilityAnswers.{CompleteYTDLiabilityAnswers, IncompleteYTDLiabilityAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{DraftReturn, YTDLiabilityAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{AmountInPence, MoneyUtils, SessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging.LoggerOps
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views.html.returns.{ytdliability => pages}
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

class YTDLiabilityFirstReturnController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionStore,
  val errorHandler: ErrorHandler,
  returnsService: ReturnsService,
  cc: MessagesControllerComponents,
  val config: Configuration,
  estimatedIncomePage: pages.estimated_income,
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
      YTDLiabilityAnswers
    ) => Future[Result]
  ): Future[Result] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some((s, r: FillingOutReturn)) =>
        r.draftReturn.ytdLiabilityAnswers.fold[Future[Result]](
          f(s, r, IncompleteYTDLiabilityAnswers.empty)
        )(f(s, r, _))
      case _ => Redirect(controllers.routes.StartController.start())
    }

  private def commonDisplayBehaviour[A, P: Writeable, R](
    currentAnswers: YTDLiabilityAnswers
  )(form: YTDLiabilityAnswers => Form[A])(
    page: (Form[A], Call) => P
  )(
    requiredPreviousAnswer: YTDLiabilityAnswers => Option[R],
    redirectToIfNoRequiredPreviousAnswer: Call
  ): Future[Result] =
    if (requiredPreviousAnswer(currentAnswers).isDefined) {
      val backLink = currentAnswers.fold(
        _ => redirectToIfNoRequiredPreviousAnswer,
        _ => routes.YTDLiabilityFirstReturnController.checkYourAnswers()
      )
      Ok(page(form(currentAnswers), backLink))
    } else {
      Redirect(redirectToIfNoRequiredPreviousAnswer)
    }

  private def commonSubmitBehaviour[A, P: Writeable, R](
    currentFillingOutReturn: FillingOutReturn,
    currentAnswers: YTDLiabilityAnswers
  )(form: Form[A])(
    page: (Form[A], Call) => P
  )(
    requiredPreviousAnswer: YTDLiabilityAnswers => Option[R],
    redirectToIfNoRequiredPreviousAnswer: Call
  )(
    updateAnswers: (A, DraftReturn) => DraftReturn
  )(
    implicit request: RequestWithSessionData[_]
  ): Future[Result] =
    if (requiredPreviousAnswer(currentAnswers).isDefined) {
      lazy val backLink = currentAnswers.fold(
        _ => redirectToIfNoRequiredPreviousAnswer,
        _ => controllers.returns.ytdliability.routes.YTDLiabilityFirstReturnController.checkYourAnswers()
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
              _ => Redirect(routes.YTDLiabilityFirstReturnController.checkYourAnswers())
            )

          }
        )
    } else {
      Redirect(redirectToIfNoRequiredPreviousAnswer)
    }

  def estimatedIncome(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withFillingOutReturnAndYTDLiabilityAnswers(request) {
      case (_, _, answers) =>
        commonDisplayBehaviour(answers)(
          form = _.fold(
            _.estimatedIncome.fold(estimatedIncomeForm)(a => estimatedIncomeForm.fill(a.inPounds())),
            c => estimatedIncomeForm.fill(c.estimatedIncome.inPounds())
          )
        )(
          page = estimatedIncomePage(_, _)
        )(
          requiredPreviousAnswer = _ => Some(()),
          controllers.returns.routes.TaskListController.taskList()
        )
    }
  }

  def estimatedIncomeSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withFillingOutReturnAndYTDLiabilityAnswers(request) {
      case (_, fillingOutReturn, answers) =>
        commonSubmitBehaviour(fillingOutReturn, answers)(
          form = estimatedIncomeForm
        )(
          page = {
            case (form, backLink) =>
              estimatedIncomePage(form, backLink)
          }
        )(
          requiredPreviousAnswer               = _ => Some(()),
          redirectToIfNoRequiredPreviousAnswer = controllers.returns.routes.TaskListController.taskList()
        )(
          updateAnswers = {
            case (p, draftReturn) =>
              draftReturn.copy(
                ytdLiabilityAnswers = Some(
                  answers.fold(
                    _.copy(estimatedIncome = Some(AmountInPence.fromPounds(p))),
                    _.copy(estimatedIncome = AmountInPence.fromPounds(p))
                  )
                )
              )
          }
        )
    }
  }

  def checkYourAnswers(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withFillingOutReturnAndYTDLiabilityAnswers(request) {
      case (_, fillingOutReturn, answers) =>
        answers match {
          case c: CompleteYTDLiabilityAnswers =>
            Ok(checkYouAnswersPage(c))

          case IncompleteYTDLiabilityAnswers(None) =>
            Redirect(routes.YTDLiabilityFirstReturnController.estimatedIncome())

          case IncompleteYTDLiabilityAnswers(Some(ei)) =>
            val completeAnswers = CompleteYTDLiabilityAnswers(ei)
            val newDraftReturn =
              fillingOutReturn.draftReturn.copy(ytdLiabilityAnswers = Some(completeAnswers))

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

object YTDLiabilityFirstReturnController {

  val estimatedIncomeForm: Form[Double] =
    Form(
      mapping(
        "estimatedIncome" -> of(MoneyUtils.amountInPoundsFormatter(_ <= 0, _ > MoneyUtils.maxAmountOfPounds))
      )(identity)(Some(_))
    )

}
