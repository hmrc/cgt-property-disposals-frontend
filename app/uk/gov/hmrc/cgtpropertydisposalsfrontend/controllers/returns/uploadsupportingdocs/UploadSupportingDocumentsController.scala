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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.uploadsupportingdocs
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
import cats.Eq
import cats.data.EitherT
import cats.instances.future._
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
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.uploadsupportingdocs.UploadSupportingDocumentsController.hasSupportingDocsToUploadForm
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.FillingOutReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.UploadSupportingDocuments.{CompleteUploadSupportingDocuments, IncompleteUploadSupportingDocuments}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{DraftReturn, MultipleDisposalsDraftReturn, SingleDisposalDraftReturn, UploadSupportingDocuments}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{BooleanFormatter, SessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging.LoggerOps
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views.html.returns.{uploadsupportingdocs => pages}
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class UploadSupportingDocumentsController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionStore,
  val errorHandler: ErrorHandler,
  returnsService: ReturnsService,
  cc: MessagesControllerComponents,
  val config: Configuration,
  hasSupportingDocsToUploadPage: pages.has_supporting_docs_to_upload
)(implicit viewConfig: ViewConfig, ec: ExecutionContext)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with Logging
    with SessionUpdates {

  private def withSupportingDocumentChoices(
    request: RequestWithSessionData[_]
  )(
    f: (
      SessionData,
      FillingOutReturn,
      UploadSupportingDocuments
    ) => Future[Result]
  ): Future[Result] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some((s, r @ FillingOutReturn(_, _, _, d: SingleDisposalDraftReturn))) =>
        d.uploadSupportingDocuments match {
          case Some(value) => f(s, r, value)
          case None        => f(s, r, IncompleteUploadSupportingDocuments.empty)
        }
      case Some((s, r @ FillingOutReturn(_, _, _, d: MultipleDisposalsDraftReturn))) =>
        d.uploadSupportingDocuments match {
          case Some(value) => f(s, r, value)
          case None        => f(s, r, IncompleteUploadSupportingDocuments.empty)
        }
      case _ => Redirect(controllers.routes.StartController.start())
    }

  private def withSupportingDocsAnswers(
    answers: UploadSupportingDocuments
  )(f: Boolean => Future[Result]): Future[Result] =
    answers
      .fold(_.hasSupportingDocuments, c => Some(c.hasSupportingDocuments))
      .fold[Future[Result]](Future.successful(Redirect(routes.UploadSupportingDocumentsController.checkYourAnswers())))(
        f
      )

  private def commonDisplayBehaviour[A, P: Writeable, R](
    currentAnswers: UploadSupportingDocuments
  )(form: UploadSupportingDocuments => Form[A])(
    page: (Form[A], Call) => P
  )(
    requiredPreviousAnswer: UploadSupportingDocuments => Option[R],
    redirectToIfNoRequiredPreviousAnswer: Call
  ): Future[Result] =
    if (requiredPreviousAnswer(currentAnswers).isDefined) {
      val backLink = currentAnswers.fold(
        _ => redirectToIfNoRequiredPreviousAnswer,
        _ => routes.UploadSupportingDocumentsController.checkYourAnswers()
      )
      Ok(page(form(currentAnswers), backLink))
    } else {
      Redirect(redirectToIfNoRequiredPreviousAnswer)
    }

  def hasSupportingDocsToUpload(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withSupportingDocumentChoices(request) { (_, _, answers) =>
      commonDisplayBehaviour(answers)(
        form = _.fold(
          _.hasSupportingDocuments.fold(hasSupportingDocsToUploadForm)(hasSupportingDocsToUploadForm.fill),
          c => hasSupportingDocsToUploadForm.fill(c.hasSupportingDocuments)
        )
      )(
        page = hasSupportingDocsToUploadPage(_, _)
      )(
        requiredPreviousAnswer = { _ =>
          Some(())
        },
        redirectToIfNoRequiredPreviousAnswer = controllers.returns.routes.TaskListController.taskList()
      )
    }
  }

  def hasSupportingDocsToUploadSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async {
    implicit request =>
      withSupportingDocumentChoices(request) {
        case (_, fillingOutReturn, answers) =>
          commonSubmitBehaviour(fillingOutReturn, fillingOutReturn.draftReturn, answers)(
            form = hasSupportingDocsToUploadForm
          )(page = {
            case (form, backLink) =>
              val updatedForm = form
              hasSupportingDocsToUploadPage(updatedForm, backLink)
          })(
            requiredPreviousAnswer = { _ =>
              Some(())
            },
            redirectToIfNoRequiredPreviousAnswer = controllers.returns.routes.TaskListController.taskList()
          )(
            updateAnswers = (b, draftReturn) => {
              draftReturn match {
                case s @ SingleDisposalDraftReturn(
                      _,
                      _,
                      _,
                      _,
                      _,
                      _,
                      _,
                      _,
                      _,
                      uploadSupportingDocuments,
                      _
                    ) =>
                  s.copy(uploadSupportingDocuments = Some(CompleteUploadSupportingDocuments(b)))
                case m @ MultipleDisposalsDraftReturn(
                      _,
                      _,
                      _,
                      _,
                      uploadSupportingDocuments,
                      _
                    ) =>
                  m.copy(uploadSupportingDocuments = Some(CompleteUploadSupportingDocuments(b)))
              }

            }
          )
      }
  }

  def checkYourAnswers(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withSupportingDocumentChoices(request) {
      case (_, fillingOutReturn, answers) =>
        (answers, fillingOutReturn.draftReturn) match {
          case (c: UploadSupportingDocuments, s: SingleDisposalDraftReturn) =>
            checkYourAnswersHandleCalculated(c, fillingOutReturn, s)

          case (c: UploadSupportingDocuments, s: MultipleDisposalsDraftReturn) => //TODO: fix - collapse into one
            checkYourAnswersHandleCalculated(c, fillingOutReturn, s)

          case _ =>
            logger.warn("error processing cya") //TODO: fix error message
            errorHandler.errorResult()

        }
    }
  }

  private def commonSubmitBehaviour[Y <: UploadSupportingDocuments, D <: DraftReturn: Eq, A, P: Writeable, R](
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
    form
      .bindFromRequest()
      .fold(
        formWithErrors => BadRequest(page(formWithErrors, controllers.returns.routes.TaskListController.taskList())), {
          value =>
            val newDraftReturn = updateAnswers(value, currentDraftReturn)
            //.fold(_ => CompleteUploadSupportingDocuments(true), _ => CompleteUploadSupportingDocuments(true))

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
              _ => Redirect(routes.UploadSupportingDocumentsController.checkYourAnswers())
            )
        }
      )

  private def checkYourAnswersHandleCalculated(
    answers: UploadSupportingDocuments,
    fillingOutReturn: FillingOutReturn,
    draftReturn: DraftReturn
  )(implicit request: RequestWithSessionData[_]): Future[Result] =
    answers match {
      case IncompleteUploadSupportingDocuments(hasSupportingDocuments) =>
        Redirect(routes.UploadSupportingDocumentsController.hasSupportingDocsToUpload())
      case CompleteUploadSupportingDocuments(hasSupportingDocuments) =>
        Ok("checkYourAnswersPage(c)") //TODO: needs to be enriched to show all the docs uploaded
    }

}

object UploadSupportingDocumentsController {

  val hasSupportingDocsToUploadForm: Form[Boolean] =
    Form(
      mapping(
        "hasSupportingDocsToUpload" -> of(BooleanFormatter.formatter)
      )(identity)(Some(_))
    )

}
