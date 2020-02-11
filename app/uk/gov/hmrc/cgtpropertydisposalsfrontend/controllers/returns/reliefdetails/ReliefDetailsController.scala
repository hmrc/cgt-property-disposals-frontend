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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.reliefdetails

import cats.data.EitherT
import com.google.inject.Inject
import play.api.Configuration
import play.api.data.Form
import play.api.http.Writeable
import cats.instances.bigDecimal._
import cats.instances.future._
import cats.syntax.eq._
import play.api.mvc._
import play.api.mvc.{Action, AnyContent, Call, MessagesControllerComponents, Result}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.FillingOutReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.reliefdetails.ReliefDetailsController._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{AmountInPence, LocalDateUtils, MoneyUtils, SessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ReliefDetailsAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ReliefDetailsAnswers.{CompleteReliefDetailsAnswers, IncompleteReliefDetailsAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.AmountInPence._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging.LoggerOps
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views.html.returns.{reliefdetails => pages}
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

class ReliefDetailsController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionStore,
  val errorHandler: ErrorHandler,
  returnsService: ReturnsService,
  cc: MessagesControllerComponents,
  val config: Configuration,
  privateResidentsReliefPage: pages.private_residents_relief,
  lettingsReliefPage: pages.lettings_relief,
  otherReliefsPage: pages.other_reliefs,
  checkYouAnswersPage: pages.check_your_answers
)(implicit viewConfig: ViewConfig, ec: ExecutionContext)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with Logging
    with SessionUpdates {

  private def withFillingOutReturnAndReliefDetailsAnswers(
    request: RequestWithSessionData[_]
  )(
    f: (
      SessionData,
      FillingOutReturn,
      ReliefDetailsAnswers
    ) => Future[Result]
  ): Future[Result] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some((s, r: FillingOutReturn)) =>
        r.draftReturn.reliefDetailsAnswers.fold[Future[Result]](
          f(s, r, IncompleteReliefDetailsAnswers.empty)
        )(f(s, r, _))
      case _ => Redirect(controllers.routes.StartController.start())
    }

  private def commonDisplayBehaviour[A, P: Writeable, R](
    currentAnswers: ReliefDetailsAnswers
  )(form: ReliefDetailsAnswers => Form[A])(
    page: (Form[A], Call) => P
  )(
    requiredPreviousAnswer: ReliefDetailsAnswers => Option[R],
    redirectToIfNoRequiredPreviousAnswer: Call
  ): Future[Result] =
    if (requiredPreviousAnswer(currentAnswers).isDefined) {
      val backLink = currentAnswers.fold(
        _ => redirectToIfNoRequiredPreviousAnswer,
        _ => routes.ReliefDetailsController.checkYourAnswers()
      )
      Ok(page(form(currentAnswers), backLink))
    } else {
      Redirect(redirectToIfNoRequiredPreviousAnswer)
    }

  private def commonSubmitBehaviour[A, P: Writeable, R](
    currentFillingOutReturn: FillingOutReturn,
    currentAnswers: ReliefDetailsAnswers
  )(form: Form[A])(
    page: (Form[A], Call) => P
  )(
    requiredPreviousAnswer: ReliefDetailsAnswers => Option[R],
    redirectToIfNoRequiredPreviousAnswer: Call
  )(
    updateAnswers: (A, ReliefDetailsAnswers) => ReliefDetailsAnswers
  )(
    implicit request: RequestWithSessionData[_]
  ): Future[Result] =
    if (requiredPreviousAnswer(currentAnswers).isDefined) {
      lazy val backLink = currentAnswers.fold(
        _ => redirectToIfNoRequiredPreviousAnswer,
        _ => controllers.returns.reliefdetails.routes.ReliefDetailsController.checkYourAnswers()
      )
      form
        .bindFromRequest()
        .fold(
          formWithErrors => BadRequest(page(formWithErrors, backLink)), { value =>
            val newAnswers     = updateAnswers(value, currentAnswers)
            val newDraftReturn = currentFillingOutReturn.draftReturn.copy(reliefDetailsAnswers = Some(newAnswers))

            val result = for {
              _ <- if (newDraftReturn === currentFillingOutReturn.draftReturn) EitherT.pure(())
                  else returnsService.storeDraftReturn(newDraftReturn)
              _ <- EitherT(
                    updateSession(sessionStore, request)(
                      _.copy(journeyStatus = Some(currentFillingOutReturn.copy(draftReturn = newDraftReturn)))
                    )
                  )
            } yield ()
            result.fold({ e =>
              logger.warn("Could not update draft return", e)
              errorHandler.errorResult()
            }, _ => Redirect(routes.ReliefDetailsController.checkYourAnswers()))

          }
        )
    } else {
      Redirect(redirectToIfNoRequiredPreviousAnswer)
    }

  def privateResidentsRelief(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withFillingOutReturnAndReliefDetailsAnswers(request) {
      case (_, _, answers) =>
        commonDisplayBehaviour(answers)(
          form = _.fold(
            _.privateResidentsRelief.fold(privateResidentsReliefForm)(a => privateResidentsReliefForm.fill(a.inPounds())
            ),
            c => privateResidentsReliefForm.fill(c.privateResidentsRelief.inPounds())
          )
        )(
          page = privateResidentsReliefPage(_, _)
        )(
          requiredPreviousAnswer = _ => Some(()),
          controllers.returns.routes.TaskListController.taskList()
        )
    }
  }

  def privateResidentsReliefSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async {
    implicit request =>
      withFillingOutReturnAndReliefDetailsAnswers(request) {
        case (_, fillingOutReturn, answers) =>
          commonSubmitBehaviour(fillingOutReturn, answers)(
            form = privateResidentsReliefForm
          )(
            page = {
              case (form, backLink) =>
                privateResidentsReliefPage(form, backLink)
            }
          )(
            requiredPreviousAnswer               = _ => Some(()),
            redirectToIfNoRequiredPreviousAnswer = controllers.returns.routes.TaskListController.taskList()
          )(
            updateAnswers = {
              case (p, answers) =>
                answers.fold(
                  _.copy(privateResidentsRelief = Some(AmountInPence.fromPounds(p))),
                  _.copy(privateResidentsRelief = AmountInPence.fromPounds(p))
                )
            }
          )
      }
  }

  def lettingsRelief(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withFillingOutReturnAndReliefDetailsAnswers(request) {
      case (_, _, answers) =>
        commonDisplayBehaviour(answers)(
          form = _.fold(
            _.lettingsRelief.fold(lettingsReliefForm)(a => lettingsReliefForm.fill(a.inPounds())),
            c => lettingsReliefForm.fill(c.lettingsRelief.inPounds())
          )
        )(
          page = lettingsReliefPage(_, _)
        )(
          requiredPreviousAnswer = _.fold(
            _.privateResidentsRelief,
            c => Some(c.privateResidentsRelief)
          ),
          routes.ReliefDetailsController.privateResidentsRelief()
        )
    }
  }

  def lettingsReliefSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withFillingOutReturnAndReliefDetailsAnswers(request) {
      case (_, fillingOutReturn, answers) =>
        commonSubmitBehaviour(fillingOutReturn, answers)(
          form = lettingsReliefForm
        )(page = lettingsReliefPage(_, _))(
          requiredPreviousAnswer = _.fold(
            _.privateResidentsRelief,
            c => Some(c.privateResidentsRelief)
          ),
          redirectToIfNoRequiredPreviousAnswer = controllers.returns.routes.TaskListController.taskList()
        )(
          updateAnswers = {
            case (p, answers) =>
              answers.fold(
                _.copy(lettingsRelief = Some(AmountInPence.fromPounds(p))),
                _.copy(lettingsRelief = AmountInPence.fromPounds(p))
              )
          }
        )
    }
  }

  def otherReliefs(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withFillingOutReturnAndReliefDetailsAnswers(request) {
      case (_, _, answers) =>
        commonDisplayBehaviour(answers)(
          form = _.fold(
            _.otherReliefs.fold(otherReliefsForm)(a => otherReliefsForm.fill(a.inPounds())),
            c => otherReliefsForm.fill(c.otherReliefs.inPounds())
          )
        )(
          page = otherReliefsPage(_, _)
        )(
          requiredPreviousAnswer = _.fold(
            _.lettingsRelief,
            c => Some(c.lettingsRelief)
          ),
          routes.ReliefDetailsController.lettingsRelief()
        )
    }
  }

  def otherReliefsSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withFillingOutReturnAndReliefDetailsAnswers(request) {
      case (_, fillingOutReturn, answers) =>
        commonSubmitBehaviour(fillingOutReturn, answers)(
          form = otherReliefsForm
        )(page = otherReliefsPage(_, _))(
          requiredPreviousAnswer = _.fold(
            _.lettingsRelief,
            c => Some(c.lettingsRelief)
          ),
          redirectToIfNoRequiredPreviousAnswer = controllers.returns.routes.TaskListController.taskList()
        )(
          updateAnswers = {
            case (p, answers) =>
              answers.fold(
                _.copy(otherReliefs = Some(AmountInPence.fromPounds(p))),
                _.copy(otherReliefs = AmountInPence.fromPounds(p))
              )
          }
        )
    }
  }

  def checkYourAnswers(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withFillingOutReturnAndReliefDetailsAnswers(request) {
      case (_, fillingOutReturn, answers) =>
        answers match {
          case c: CompleteReliefDetailsAnswers =>
            Ok(checkYouAnswersPage(c))

          case IncompleteReliefDetailsAnswers(None, _, _) =>
            Redirect(routes.ReliefDetailsController.privateResidentsRelief())

          case IncompleteReliefDetailsAnswers(_, None, _) =>
            Redirect(routes.ReliefDetailsController.lettingsRelief())

          case IncompleteReliefDetailsAnswers(_, _, None) =>
            Redirect(routes.ReliefDetailsController.otherReliefs())

          case IncompleteReliefDetailsAnswers(Some(prr), Some(lr), Some(or)) =>
            val completeAnswers = CompleteReliefDetailsAnswers(prr, lr, or)
            val newDraftReturn =
              fillingOutReturn.draftReturn.copy(reliefDetailsAnswers = Some(completeAnswers))

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
    Redirect(controllers.returns.routes.TaskListController.taskList())
  }

}

object ReliefDetailsController {

  val privateResidentsReliefForm: Form[Double] =
    MoneyUtils.amountInPoundsYesNoForm("privateResidentsRelief", "privateResidentsReliefValue")

  val lettingsReliefForm: Form[Double] =
    MoneyUtils.amountInPoundsYesNoForm("lettingsRelief", "lettingsReliefValue")

  val otherReliefsForm: Form[Double] =
    MoneyUtils.amountInPoundsYesNoForm("otherReliefs", "otherReliefsValue")

}
