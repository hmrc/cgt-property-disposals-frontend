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
import java.time.LocalDateTime
import java.util.UUID

import akka.stream.scaladsl.{FileIO, Source}
import akka.util.ByteString
import cats.Eq
import cats.data.EitherT
import cats.instances.future._
import cats.syntax.eq._
import com.google.inject.{Inject, Singleton}
import configs.Configs
import play.api.Configuration
import play.api.data.Form
import play.api.data.Forms.{mapping, of, _}
import play.api.http.Writeable
import play.api.mvc._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.{controllers, models}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.uploadsupportingdocs.UploadSupportingDocumentsController._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.FillingOutReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.{CgtReference, DraftReturnId}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.SubscribedDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.UploadSupportingDocumentAnswers.{CompleteUploadSupportingDocumentAnswers, IncompleteUploadSupportingDocumentAnswers, SupportingDocuments}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{DraftMultipleDisposalsReturn, DraftReturn, DraftSingleDisposalReturn, UploadSupportingDocumentAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.upscan.UpscanInitiateResponse.{FailedToGetUpscanSnapshot, MaximumFileUploadReached, UpscanInitiateSuccess}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{BooleanFormatter, Error, SessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.UpscanService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging.LoggerOps
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views.html.returns.{uploadsupportingdocs => pages}
import uk.gov.hmrc.play.bootstrap.controller.FrontendController
import configs.Configs
import configs.syntax._
import play.api.libs.Files
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.UpscanConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.upscan.UpscanFileDescriptor.UpscanFileDescriptorStatus.UPLOADED
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.upscan.{UpscanFileDescriptor, UpscanInitiateReference}

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class UploadSupportingDocumentsController @Inject() ( //FIXME : change name to UploadSupportingEvidenceController
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionStore,
  val errorHandler: ErrorHandler,
  returnsService: ReturnsService,
  upscanService: UpscanService,
  upscanConnector: UpscanConnector,
  cc: MessagesControllerComponents,
  val configuration: Configuration,
  doYouWantToUploadSupportingEvidencePage: pages.do_you_want_to_upload_supporting_evidence,
  uploadSupportingEvidencePage: pages.upload_supporting_evidence,
  deleteSupportingEvidencePage: pages.delete_supporting_evidence,
  changeSupportingEvidencePage: pages.change_support_evidence,
  checkYourAnswersPage: pages.check_your_answers,
  changeOrDeletePage: pages.change_or_delete_supporting_document //FIXME: remove this
)(implicit viewConfig: ViewConfig, ec: ExecutionContext)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with Logging
    with SessionUpdates {

  private def getUpscanInitiateConfig[A: Configs](key: String): A =
    configuration.underlying
      .get[A](s"microservice.services.upscan-initiate.$key")
      .value

  private val maxFileSize: Int = getUpscanInitiateConfig[Int]("max-file-size")

  private def withUploadSupportingDocumentsAnswers(
    request: RequestWithSessionData[_]
  )(
    f: (
      UUID,
      CgtReference,
      SessionData,
      FillingOutReturn,
      UploadSupportingDocumentAnswers
    ) => Future[Result]
  ): Future[Result] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some((s, r @ FillingOutReturn(c: SubscribedDetails, _, _, d: DraftReturn))) =>
        d match {
          case DraftSingleDisposalReturn(i, _, _, _, _, _, _, _, _, maybeSupportingDocumentsAnswers, _) =>
            maybeSupportingDocumentsAnswers.fold[Future[Result]](
              f(i, c.cgtReference, s, r, IncompleteUploadSupportingDocumentAnswers.empty)
            )(f(i, c.cgtReference, s, r, _))
          case DraftMultipleDisposalsReturn(i, _, _, _, _, maybeSupportingDocumentsAnswers, _) =>
            maybeSupportingDocumentsAnswers.fold[Future[Result]](
              f(i, c.cgtReference, s, r, IncompleteUploadSupportingDocumentAnswers.empty)
            )(f(i, c.cgtReference, s, r, _))
        }
      case _ => Redirect(controllers.routes.StartController.start())
    }

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
      withUploadSupportingDocumentsAnswers(request) { (_, _, _, _, answers) =>
        commonDisplayBehaviour(answers)(
          form = _.fold(
            _.doYouWantToUploadSupportingDocuments
              .fold(doYouWantToUploadSupportingDocumentsForm)(doYouWantToUploadSupportingDocumentsForm.fill),
            c => doYouWantToUploadSupportingDocumentsForm.fill(c.doYouWantToUploadSupportingDocuments)
          )
        )(
          page = doYouWantToUploadSupportingEvidencePage(_, _)
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
      withUploadSupportingDocumentsAnswers(request) { (_, _, _, fillingOutReturn, answers) =>
        commonSubmitBehaviour(fillingOutReturn, fillingOutReturn.draftReturn, answers)(
          form = doYouWantToUploadSupportingDocumentsForm
        )(page = { (form, backLink) =>
          val updatedForm = form
          doYouWantToUploadSupportingEvidencePage(updatedForm, backLink)
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
                s.copy(uploadSupportingDocuments = Some(
                  IncompleteUploadSupportingDocumentAnswers(
                    Some(doYouWantToUploadSupportingDocumentsAnswer),
                    List.empty
                  )
                ) //FIXME: list should be passed in
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
                m.copy(uploadSupportingDocuments = Some(
                  IncompleteUploadSupportingDocumentAnswers(
                    Some(doYouWantToUploadSupportingDocumentsAnswer),
                    List.empty
                  )
                ) //FIXME: list should be passed in
                )
            }
          }
        )
      }
  }

  // This method does not deal with answers so we don't need it here???? - this is the design work - how to handle this and bring the data into the main data structure that
  // works the other pages eg Incomplete and Complete...
  def uploadSupportingEvidence(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withUploadSupportingDocumentsAnswers(request) { (draftReturnId, cgtRef, _, fillingOutReturn, answers) =>
      upscanService.initiate(DraftReturnId(draftReturnId.toString), cgtRef, LocalDateTime.now()).value.map {
        case Left(error) =>
          logger.warn(s"could not initiate upscan due to $error")
          errorHandler.errorResult(None)
        case Right(upscanInitiateResponse) =>
          upscanInitiateResponse match {
            case FailedToGetUpscanSnapshot        => errorHandler.errorResult(None)
            case MaximumFileUploadReached         => Redirect(routes.UploadSupportingDocumentsController.checkYourAnswers())
            case UpscanInitiateSuccess(reference) => Ok(uploadSupportingEvidencePage(reference)) //FIXME: backlink???
          }
      }

//        commonDisplayBehaviour(answers)(
//          form = _.fold(
//            _.doYouWantToUploadSupportingDocuments
//              .fold(uploadSupportingDocumentForm)(uploadSupportingDocumentForm.fill), //TODO: this needs to change as it is a form upload
//            c => uploadSupportingDocumentForm.fill(c.doYouWantToUploadSupportingDocuments)
//          )
//        )(
//          page = uploadSupportingEvidencePage(_, _, "")
//        )(
//          requiredPreviousAnswer =
//            _.fold(_.doYouWantToUploadSupportingDocuments, c => Some(c.doYouWantToUploadSupportingDocuments)),
//          redirectToIfNoRequiredPreviousAnswer = controllers.returns.routes.TaskListController.taskList() //TODO: do I really want to go here??
//        )
    }
  }

//  def uploadSupportingEvidenceSubmit() = authenticatedActionWithSessionData.async { implicit request =>
//    //TODO: do form validation - client check if file has been chosen and block if not and als of file types validation  - Ali do this
//    //TODO: check for success status from upload:
//    //TODO: if success, store the upscan reference in the answers list and redirect to status page with check virus scan results
//    //TODO: if error, show error page and do not store
//    Future.successful(Ok("uploaded"))
//  }
  @SuppressWarnings(Array("org.wartremover.warts.Var", "org.wartremover.warts.Any"))
  def uploadSupportingEvidenceSubmit(): Action[MultipartFormData[Files.TemporaryFile]] =
    authenticatedActionWithSessionData(parse.multipartFormData(maxFileSize)).async { implicit request =>
      withUploadSupportingDocumentsAnswers(request) { (draftReturnId, _, _, _, _) =>
        val multipart: MultipartFormData[Files.TemporaryFile] = request.body
        val result = for {
          reference <- EitherT.fromOption(
                        multipart.dataParts.get("reference").flatMap(_.headOption),
                        Error("missing upscan file descriptor id")
                      )
          maybeUpscanFileDescriptor <- upscanService
                                        .getUpscanFileDescriptor(
                                          DraftReturnId(draftReturnId.toString), //TODO: make sure this is used downstream
                                          UpscanInitiateReference(reference)
                                        )
          upscanFileDescriptor <- EitherT.fromOption(
                                   maybeUpscanFileDescriptor,
                                   Error("failed to retrieve upscan file descriptor details")
                                 )
          prepared <- EitherT.fromEither(handleGetFileDescriptorResult(multipart, upscanFileDescriptor))
          _        <- upscanConnector.upload(upscanFileDescriptor.fileDescriptor.uploadRequest.href, prepared)
          _        <- upscanConnector.updateUpscanFileDescriptorStatus(upscanFileDescriptor.copy(status = UPLOADED))
        //TODO: update session store as well here - need to add file name and upscan reference

        } yield ()

        result.fold(
          error => {
            logger.warn(s"failed to upload file with error: $error")
            errorHandler.errorResult(None)
          },
          _ => {

            Redirect(routes.UploadSupportingDocumentsController.checkYourAnswers())
          } // TODO: need to update the session store with new answers based on this uploaded file (just reference )
        )
      }
    }

  @SuppressWarnings(Array("org.wartremover.warts.Var", "org.wartremover.warts.Any"))
  private def handleGetFileDescriptorResult(
    multipart: MultipartFormData[Files.TemporaryFile],
    upscanFileDescriptor: UpscanFileDescriptor
  ): Either[Error, MultipartFormData[Source[ByteString, Any]]] = {
    val userFile =
      multipart.files
        .map(file => file.copy(ref = FileIO.fromPath(file.ref.path): Source[ByteString, Any]))
    val prepared: MultipartFormData[Source[ByteString, Any]] =
      multipart
        .copy(
          files = userFile,
          dataParts = upscanFileDescriptor.fileDescriptor.uploadRequest.fields
            .mapValues(fieldValue => Seq(fieldValue))
        )
    Right(prepared)
  }

  def changeSupportingEvidence() = authenticatedActionWithSessionData.async { implicit request =>
    Ok(
      changeOrDeletePage(
        Some(SupportingDocuments("1", "filename")),
        routes.UploadSupportingDocumentsController.checkYourAnswers()
      )
    )
  }

  def changeSupportingEvidenceSubmit() = authenticatedActionWithSessionData.async { implicit request =>
    Ok("submitted change")
  }

  def deleteSupportingEvidence() = authenticatedActionWithSessionData.async { implicit request =>
    Ok(
      changeOrDeletePage(
        Some(SupportingDocuments("1", "filename")),
        routes.UploadSupportingDocumentsController.checkYourAnswers()
      )
    )
  }

  def deleteSupportingEvidenceSubmit() = authenticatedActionWithSessionData.async { implicit request =>
    Ok("deleted file")
  }

  def checkYourAnswers(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withUploadSupportingDocumentsAnswers(request) { (_, _, _, fillingOutReturn, answers) =>
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
    withUploadSupportingDocumentsAnswers(request) {
      case _ =>
        Redirect(controllers.returns.routes.TaskListController.taskList())
    }
  }

  //TODO: need to keep track of number of files uploaded so far and so they cannot get to the page where they can upload more - they have to go to the CYA page
  private def checkYourAnswersHandleCalculated(
    answers: UploadSupportingDocumentAnswers,
    fillingOutReturn: FillingOutReturn,
    draftReturn: DraftReturn
  )(implicit request: RequestWithSessionData[_]): Future[Result] =
    answers match {
      //FIXME: what should be passed in as list or what do I do with the lists?
      case IncompleteUploadSupportingDocumentAnswers(None, _) => // TODO: change name to IncompleteUploadSupportingEvidenceAnswers
        Redirect(routes.UploadSupportingDocumentsController.doYouWantToUploadSupportingDocuments())
      case IncompleteUploadSupportingDocumentAnswers(Some(true), List.empty) =>
        Redirect(routes.UploadSupportingDocumentsController.uploadSupportingEvidence())
      case IncompleteUploadSupportingDocumentAnswers(Some(true), _) =>
        Ok(checkYourAnswersPage(CompleteUploadSupportingDocumentAnswers(true, List(SupportingDocuments("2", "f1"))))) //TODO: remove this
      case IncompleteUploadSupportingDocumentAnswers(Some(false), _) =>
        val completeAnswers = CompleteUploadSupportingDocumentAnswers(false, List.empty)
        val newDraftReturn =
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
              s.copy(uploadSupportingDocuments = Some(completeAnswers))
            case m @ DraftMultipleDisposalsReturn(
                  _,
                  _,
                  _,
                  _,
                  _,
                  _,
                  _
                ) =>
              m.copy(uploadSupportingDocuments = Some(completeAnswers))
          }

        //TODO: here you need to check if they select "No" to do you want to upload - if so then we delete all the uploaded files from the upscan repo
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
        }, _ => Ok(checkYourAnswersPage(completeAnswers)))

      case c @ CompleteUploadSupportingDocumentAnswers(_, _) =>
        Ok(checkYourAnswersPage(c))
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
