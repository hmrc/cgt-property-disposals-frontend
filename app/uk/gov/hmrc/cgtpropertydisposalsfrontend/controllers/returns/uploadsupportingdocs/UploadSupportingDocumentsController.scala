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
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.uploadsupportingdocs.UploadSupportingDocumentsController.doYouWantToUploadSupportingDocumentsForm
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.FillingOutReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.UploadSupportingDocumentAnswers.{CompleteUploadSupportingDocumentAnswers, IncompleteUploadSupportingDocumentAnswers, SupportingDocuments}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{DraftMultipleDisposalsReturn, DraftReturn, DraftSingleDisposalReturn, UploadSupportingDocumentAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{BooleanFormatter, SessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.UpscanService
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
  upscanService: UpscanService,
  cc: MessagesControllerComponents,
  val config: Configuration,
  doYouWantToUploadSupportingDocumentsPage: pages.do_you_want_to_upload_supporting_documents,
  checkYourAnswersPage: pages.check_your_answers,
  changeOrDeletePage: pages.change_or_delete_supporting_document,
  uploadDocumentWithSupportingEvidencePage: pages.upload_document_with_supporting_evidence
)(implicit viewConfig: ViewConfig, ec: ExecutionContext)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with Logging
    with SessionUpdates {

  private def withUploadSupportingDocumentsAnswers(
    request: RequestWithSessionData[_]
  )(
    f: (
      SessionData,
      FillingOutReturn,
      UploadSupportingDocumentAnswers
    ) => Future[Result]
  ): Future[Result] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some((s, r @ FillingOutReturn(_, _, _, d: DraftReturn))) =>
        d match {
          case DraftSingleDisposalReturn(_, _, _, _, _, _, _, _, _, maybeSupportingDocumentsAnswers, _) =>
            maybeSupportingDocumentsAnswers.fold[Future[Result]](
              f(s, r, IncompleteUploadSupportingDocumentAnswers.empty)
            )(f(s, r, _))
          case DraftMultipleDisposalsReturn(_, _, _, _, _, maybeSupportingDocumentsAnswers, _) =>
            maybeSupportingDocumentsAnswers.fold[Future[Result]](
              f(s, r, IncompleteUploadSupportingDocumentAnswers.empty)
            )(f(s, r, _))
        }
      case _ => Redirect(controllers.routes.StartController.start())
    }

  private def withSupportingDocsAnswers(
    answers: UploadSupportingDocumentAnswers
  )(f: Boolean => Future[Result]): Future[Result] =
    answers
      .fold(_.doYouWantToUploadSupportingDocuments, c => Some(c.doYouWantToUploadSupportingDocuments))
      .fold[Future[Result]](Future.successful(Redirect(routes.UploadSupportingDocumentsController.checkYourAnswers())))(
        f
      )

  private def commonDisplayBehaviour[A, P: Writeable, R](
    currentAnswers: UploadSupportingDocumentAnswers
  )(form: UploadSupportingDocumentAnswers => Form[A])(
    page: (Form[A], Call) => P
  )(
    requiredPreviousAnswer: UploadSupportingDocumentAnswers => Option[R],
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

  private def commonSubmitBehaviour[Y <: UploadSupportingDocumentAnswers, D <: DraftReturn: Eq, A, P: Writeable, R](
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
      form
        .bindFromRequest()
        .fold(
          formWithErrors =>
            BadRequest(page(formWithErrors, controllers.returns.routes.TaskListController.taskList())), { //TODO: fix the back link here
            value =>
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
                _ => Redirect(routes.UploadSupportingDocumentsController.checkYourAnswers())
              )
          }
        )
    } else {
      Redirect(redirectToIfNoRequiredPreviousAnswer)
    }

  def doYouWantToUploadSupportingDocuments(): Action[AnyContent] = authenticatedActionWithSessionData.async {
    implicit request =>
      withUploadSupportingDocumentsAnswers(request) { (_, _, answers) =>
        commonDisplayBehaviour(answers)(
          form = _.fold(
            _.doYouWantToUploadSupportingDocuments
              .fold(doYouWantToUploadSupportingDocumentsForm)(doYouWantToUploadSupportingDocumentsForm.fill),
            c => doYouWantToUploadSupportingDocumentsForm.fill(c.doYouWantToUploadSupportingDocuments)
          )
        )(
          page = doYouWantToUploadSupportingDocumentsPage(_, _)
        )(
          requiredPreviousAnswer = { _ =>
            Some(())
          },
          redirectToIfNoRequiredPreviousAnswer = controllers.returns.routes.TaskListController.taskList()
        )
      }
  }

  def doYouWantToUploadSupportingDocumentsSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async {
    implicit request =>
      withUploadSupportingDocumentsAnswers(request) { (_, fillingOutReturn, answers) =>
        commonSubmitBehaviour(fillingOutReturn, fillingOutReturn.draftReturn, answers)(
          form = doYouWantToUploadSupportingDocumentsForm
        )(page = { (form, backLink) =>
          val updatedForm = form
          doYouWantToUploadSupportingDocumentsPage(updatedForm, backLink)
        })(
          requiredPreviousAnswer = { _ =>
            Some(())
          },
          redirectToIfNoRequiredPreviousAnswer = routes.UploadSupportingDocumentsController.checkYourAnswers()
        )(
          updateAnswers = (doYouWantToUploadSupportingDocumentsAnswer, draftReturn) => {
            draftReturn match {
              case s @ DraftSingleDisposalReturn(
                    _,
                    _,
                    _,
                    _,
                    _,
                    _,
                    _,
                    _,
                    _,
                    _,
                    _
                  ) =>
                s.copy(uploadSupportingDocuments =
                  Some(IncompleteUploadSupportingDocumentAnswers(Some(doYouWantToUploadSupportingDocumentsAnswer)))
                )
              case m @ DraftMultipleDisposalsReturn(
                    _,
                    _,
                    _,
                    _,
                    _,
                    _,
                    _
                  ) =>
                m.copy(uploadSupportingDocuments =
                  Some(IncompleteUploadSupportingDocumentAnswers(Some(doYouWantToUploadSupportingDocumentsAnswer)))
                )
            }
          }
        )
      }
  }

  def uploadDocumentWithSupportingEvidence(): Action[AnyContent] = authenticatedActionWithSessionData.async {
    implicit request =>
      withUploadSupportingDocumentsAnswers(request) { (_, _, answers) =>
        commonDisplayBehaviour(answers)(
          form = _.fold(
            _.doYouWantToUploadSupportingDocuments
              .fold(doYouWantToUploadSupportingDocumentsForm)(doYouWantToUploadSupportingDocumentsForm.fill),
            c => doYouWantToUploadSupportingDocumentsForm.fill(c.doYouWantToUploadSupportingDocuments)
          )
        )(
          page = uploadDocumentWithSupportingEvidencePage(_, _, "")
        )(
          requiredPreviousAnswer =
            _.fold(_.doYouWantToUploadSupportingDocuments, c => Some(c.doYouWantToUploadSupportingDocuments)),
          redirectToIfNoRequiredPreviousAnswer = controllers.returns.routes.TaskListController.taskList() //TODO: do I really want to go here??
        )
      }
  }

  def changeOrDeleteFile() = authenticatedActionWithSessionData.async { implicit request =>
    Ok(
      changeOrDeletePage(
        Some(SupportingDocuments("1", "filename")),
        routes.UploadSupportingDocumentsController.checkYourAnswers()
      )
    )
  }

  def changeOrDeleteFileSubmit() = authenticatedActionWithSessionData.async { implicit request =>
    Ok("submitted change")
  }

  def checkYourAnswers(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withUploadSupportingDocumentsAnswers(request) {
      case (_, fillingOutReturn, answers) =>
        (answers, fillingOutReturn.draftReturn) match {
          case (c: UploadSupportingDocumentAnswers, s: DraftSingleDisposalReturn) =>
            checkYourAnswersHandleCalculated(c, fillingOutReturn, s)

          case (c: UploadSupportingDocumentAnswers, s: DraftMultipleDisposalsReturn) => //TODO: fix - collapse into one
            checkYourAnswersHandleCalculated(c, fillingOutReturn, s)

          case _ =>
            logger.warn("error processing cya") //TODO: fix error message
            errorHandler.errorResult()
        }
    }
  }

  def checkYourAnswersSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    Ok("check your answers submit")
  }

  private def checkYourAnswersHandleCalculated(
    answers: UploadSupportingDocumentAnswers,
    fillingOutReturn: FillingOutReturn,
    draftReturn: DraftReturn
  )(implicit request: RequestWithSessionData[_]): Future[Result] =
    answers match {
      case IncompleteUploadSupportingDocumentAnswers(None) =>
        Redirect(routes.UploadSupportingDocumentsController.doYouWantToUploadSupportingDocuments())
      case IncompleteUploadSupportingDocumentAnswers(Some(true)) =>
        Redirect(routes.UploadSupportingDocumentsController.uploadDocumentWithSupportingEvidence())
      case IncompleteUploadSupportingDocumentAnswers(Some(false)) =>
        Ok(checkYourAnswersPage(CompleteUploadSupportingDocumentAnswers(false, List.empty)))
      case CompleteUploadSupportingDocumentAnswers(hasSupportingDocuments, uploadeddocs) => //FIXME: list value
        Ok(checkYourAnswersPage(CompleteUploadSupportingDocumentAnswers(true, List(SupportingDocuments("2", "f1")))))
    }

}

object UploadSupportingDocumentsController {

  val doYouWantToUploadSupportingDocumentsForm: Form[Boolean] =
    Form(
      mapping(
        "do-you-want-to-upload-supporting-documents" -> of(BooleanFormatter.formatter)
      )(identity)(Some(_))
    )

}
