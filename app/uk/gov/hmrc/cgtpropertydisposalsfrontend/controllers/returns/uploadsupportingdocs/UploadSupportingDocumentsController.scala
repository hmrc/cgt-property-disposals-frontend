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
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.UploadSupportingDocuments.IncompleteUploadSupportingDocuments
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{DraftMultipleDisposalsReturn, DraftReturn, DraftSingleDisposalReturn, UploadSupportingDocuments}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{BooleanFormatter, SessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, _}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views.html.returns.{uploadsupportingdocs => pages}
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class UploadSupportingDocumentsController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionStore,
  val errorHandler: ErrorHandler,
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
      DraftReturn,
      UploadSupportingDocuments
    ) => Future[Result]
  ): Future[Result] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some((s, r @ FillingOutReturn(_, _, _, d: DraftReturn))) =>
        d match {
          case DraftSingleDisposalReturn(_, _, _, _, _, _, _, _, _, uploadSupportingDocuments, _) =>
            uploadSupportingDocuments.fold[Future[Result]](
              f(s, r, d, IncompleteUploadSupportingDocuments.empty)
            )(f(s, r, d, _))
          case DraftMultipleDisposalsReturn(_, _, _, _, _, uploadSupportingDocuments, _) =>
            uploadSupportingDocuments.fold[Future[Result]](
              f(s, r, d, IncompleteUploadSupportingDocuments.empty)
            )(f(s, r, d, _))
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

  def hasSupportingDocsToUpload() = authenticatedActionWithSessionData.async { implicit request =>
    withSupportingDocumentChoices(request) {
      case (_, _, _, answers) =>
        withSupportingDocsAnswers(answers) { e =>
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
  }

  def checkYourAnswers() = Action { implicit request =>
    Ok("TODO - check your answers stub") // dummy action for now
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
