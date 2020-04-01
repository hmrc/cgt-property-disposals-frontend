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
import cats.instances.string._
import cats.instances.boolean._
import cats.syntax.eq._
import com.google.inject.{Inject, Singleton}
import configs.Configs
import configs.syntax._
import play.api.Configuration
import play.api.data.Form
import play.api.data.Forms.{mapping, of, _}
import play.api.http.Writeable
import play.api.libs.Files
import play.api.mvc._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.UpscanConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.uploadsupportingdocs.UploadSupportingEvidenceController._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.FillingOutReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.{CgtReference, DraftReturnId}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.SubscribedDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.UploadSupportingDocumentAnswers.{CompleteUploadSupportingDocumentAnswers, IncompleteUploadSupportingDocumentAnswers, SupportingDocuments}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{DraftMultipleDisposalsReturn, DraftReturn, DraftSingleDisposalReturn, UploadSupportingDocumentAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.upscan.UpscanFileDescriptor.UpscanFileDescriptorStatus.{FAILED, UPLOADED}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.upscan.UpscanInitiateResponse.{FailedToGetUpscanSnapshot, UpscanInitiateSuccess}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.upscan.{UpscanFileDescriptor, UpscanInitiateReference, _}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{BooleanFormatter, Error, SessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.UpscanService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging.LoggerOps
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views.html.returns.uploadsupportingdocs.expired_supporting_evidence
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views.html.returns.{uploadsupportingdocs => pages}
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class UploadSupportingEvidenceController @Inject() (
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
  uploadSupportingEvidenceUpscanCheckPage: pages.upload_supporting_evidence_upscan_check,
  changeSupportingEvidencePage: pages.change_upload_supporting_evidence,
  expiredSupportingEvidencePage: expired_supporting_evidence,
  checkYourAnswersPage: pages.check_your_answers
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
        _ => routes.UploadSupportingEvidenceController.checkYourAnswers()
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
            BadRequest(page(formWithErrors, controllers.returns.routes.TaskListController.taskList())), { value =>
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
              _ => Redirect(routes.UploadSupportingEvidenceController.checkYourAnswers())
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

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  def doYouWantToUploadSupportingDocumentsSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async {
    implicit request =>
      withUploadSupportingDocumentsAnswers(request) { (draftReturnId, _, _, fillingOutReturn, answers) =>
        doYouWantToUploadSupportingDocumentsForm.bindFromRequest.fold(
          errors =>
            BadRequest(
              doYouWantToUploadSupportingEvidencePage(errors, controllers.returns.routes.TaskListController.taskList())
            ),
          doYouWantToUploadSupportingEvidencePageAnswer =>
            if (doYouWantToUploadSupportingEvidencePageAnswer) {
              val updatedAnswers = answers match {
                case IncompleteUploadSupportingDocumentAnswers(None, b) => {
                  IncompleteUploadSupportingDocumentAnswers(Some(doYouWantToUploadSupportingEvidencePageAnswer), b)
                }
                case IncompleteUploadSupportingDocumentAnswers(Some(a), b) => {
                  if (doYouWantToUploadSupportingEvidencePageAnswer =!= a) {
                    IncompleteUploadSupportingDocumentAnswers(
                      Some(doYouWantToUploadSupportingEvidencePageAnswer),
                      List.empty
                    )
                  } else {
                    IncompleteUploadSupportingDocumentAnswers(Some(doYouWantToUploadSupportingEvidencePageAnswer), b)
                  }
                }
                case CompleteUploadSupportingDocumentAnswers(a, b) => {
                  if (doYouWantToUploadSupportingEvidencePageAnswer =!= a) {
                    IncompleteUploadSupportingDocumentAnswers(
                      Some(doYouWantToUploadSupportingEvidencePageAnswer),
                      List.empty
                    )
                  } else {
                    IncompleteUploadSupportingDocumentAnswers(Some(doYouWantToUploadSupportingEvidencePageAnswer), b)
                  }
                }
              }

              val newDraftReturn = fillingOutReturn.draftReturn match {
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
                  s.copy(uploadSupportingDocuments = Some(updatedAnswers))
                case m @ DraftMultipleDisposalsReturn(
                      _,
                      _,
                      _,
                      _,
                      _,
                      _,
                      _
                    ) =>
                  m.copy(uploadSupportingDocuments = Some(updatedAnswers))
              }

              val result = for {
                _ <- returnsService
                      .storeDraftReturn(
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
              }, _ => Redirect(routes.UploadSupportingEvidenceController.checkYourAnswers()))

            } else {
              val result = for {
                _ <- upscanService.removeAllFiles(DraftReturnId(draftReturnId.toString))
                updatedAnswers: UploadSupportingDocumentAnswers = answers match {
                  case IncompleteUploadSupportingDocumentAnswers(_, _) =>
                    IncompleteUploadSupportingDocumentAnswers(Some(false), List.empty)
                  case CompleteUploadSupportingDocumentAnswers(_, _) =>
                    CompleteUploadSupportingDocumentAnswers(false, List.empty)
                }
                newDraftReturn = fillingOutReturn.draftReturn match {
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
                    s.copy(uploadSupportingDocuments = Some(updatedAnswers))
                  case m @ DraftMultipleDisposalsReturn(
                        _,
                        _,
                        _,
                        _,
                        _,
                        _,
                        _
                      ) =>
                    m.copy(uploadSupportingDocuments = Some(updatedAnswers))
                }

                _ <- returnsService
                      .storeDraftReturn(
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
              }, _ => Redirect(routes.UploadSupportingEvidenceController.checkYourAnswers()))

            }
        )
//        commonSubmitBehaviour(fillingOutReturn, fillingOutReturn.draftReturn, answers)(
//          form = doYouWantToUploadSupportingDocumentsForm
//        )(page = { (form, backLink) =>
//          val updatedForm = form
//          doYouWantToUploadSupportingEvidencePage(updatedForm, backLink)
//        })(
//          requiredPreviousAnswer = { _ =>
//            Some(())
//          },
//          redirectToIfNoRequiredPreviousAnswer = routes.UploadSupportingEvidenceController.checkYourAnswers()
//        )(
//          updateAnswers = (doYouWantToUploadSupportingDocumentsAnswer, draftReturn) => {
//            if (doYouWantToUploadSupportingDocumentsAnswer) {
//              draftReturn match {
//                case s @ DraftSingleDisposalReturn(
//                      _,
//                      _,
//                      _,
//                      _,
//                      _,
//                      _,
//                      _,
//                      _,
//                      _,
//                      _,
//                      _
//                    ) =>
//                  s.copy(uploadSupportingDocuments = Some(
//                    IncompleteUploadSupportingDocumentAnswers(
//                      Some(doYouWantToUploadSupportingDocumentsAnswer),
//                      List.empty
//                    )
//                  ) //FIXME: list should be passed in
//                  )
//                case m @ DraftMultipleDisposalsReturn(
//                      _,
//                      _,
//                      _,
//                      _,
//                      _,
//                      _,
//                      _
//                    ) =>
//                  m.copy(uploadSupportingDocuments = Some(
//                    IncompleteUploadSupportingDocumentAnswers(
//                      Some(doYouWantToUploadSupportingDocumentsAnswer),
//                      List.empty
//                    )
//                  ) //FIXME: list should be passed in
//                  )
//              }
//            } else { // Selects no
//
//              draftReturn match {
//                case s @ DraftSingleDisposalReturn(
//                      _,
//                      _,
//                      _,
//                      _,
//                      _,
//                      _,
//                      _,
//                      _,
//                      _,
//                      _,
//                      _
//                    ) =>
//                  s.copy(uploadSupportingDocuments = Some(
//                    IncompleteUploadSupportingDocumentAnswers(
//                      Some(doYouWantToUploadSupportingDocumentsAnswer),
//                      List.empty
//                    )
//                  ) //FIXME: list should be passed in
//                  )
//                case m @ DraftMultipleDisposalsReturn(
//                      _,
//                      _,
//                      _,
//                      _,
//                      _,
//                      _,
//                      _
//                    ) =>
//                  m.copy(uploadSupportingDocuments = Some(
//                    IncompleteUploadSupportingDocumentAnswers(
//                      Some(doYouWantToUploadSupportingDocumentsAnswer),
//                      List.empty
//                    )
//                  ) //FIXME: list should be passed in
//                  )
//              }
//            }
//          }
//        )
      }
  }

  // This method does not deal with answers so we don't need it here???? - this is the design work - how to handle this and bring the data into the main data structure that
  // works the other pages eg Incomplete and Complete...
  def uploadSupportingEvidence(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withUploadSupportingDocumentsAnswers(request) { (draftReturnId, cgtRef, _, fillingOutReturn, answers) =>
      upscanService.initiate(DraftReturnId(draftReturnId.toString), cgtRef, LocalDateTime.now()).value.map {
        case Left(error) =>
          logger.warn(s"could not initiate upscan due to $error")
          error.value match {
            case Right(MaxFileUploadsReached2) => //FIXME: fix this constant name
              Redirect(routes.UploadSupportingEvidenceController.checkYourAnswers()) //TODO: this is not working - it is letting add more than 4
          }
        case Right(upscanInitiateResponse) =>
          upscanInitiateResponse match {
            case FailedToGetUpscanSnapshot        => errorHandler.errorResult(None) //FIXME : this cann't happen remove this
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
      withUploadSupportingDocumentsAnswers(request) { (draftReturnId, _, _, fillingOutReturn, answers) =>
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
          updatedAnswers: UploadSupportingDocumentAnswers = answers match {
            case IncompleteUploadSupportingDocumentAnswers(a, b) =>
              IncompleteUploadSupportingDocumentAnswers(a, b :+ SupportingDocuments(reference, ""))
            case CompleteUploadSupportingDocumentAnswers(a, b) =>
              IncompleteUploadSupportingDocumentAnswers(Some(a), b :+ SupportingDocuments(reference, ""))
          }
          newDraftReturn = fillingOutReturn.draftReturn match {
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
              s.copy(uploadSupportingDocuments = Some(updatedAnswers))
            case m @ DraftMultipleDisposalsReturn(
                  _,
                  _,
                  _,
                  _,
                  _,
                  _,
                  _
                ) =>
              m.copy(uploadSupportingDocuments = Some(updatedAnswers))
          }
          _ <- returnsService
                .storeDraftReturn(
                  newDraftReturn,
                  fillingOutReturn.subscribedDetails.cgtReference,
                  fillingOutReturn.agentReferenceNumber
                )
          _ <- EitherT(
                updateSession(sessionStore, request)(
                  _.copy(journeyStatus = Some(fillingOutReturn.copy(draftReturn = newDraftReturn)))
                )
              )

        } yield (reference)

        result.fold(
          error => {
            logger.warn(s"failed to upload file with error: $error")
            errorHandler.errorResult(None)
          },
          ref => Redirect(routes.UploadSupportingEvidenceController.uploadSupportingEvidenceVirusCheck(ref))
          // TODO: need to update the session store with new answers based on this uploaded file (just reference )
        )
      }
    }

  def uploadSupportingEvidenceVirusCheck(reference: String) = authenticatedActionWithSessionData.async {
    implicit request =>
      withUploadSupportingDocumentsAnswers(request) { (draftReturnId, cgtRef, _, fillingOutReturn, answers) =>
        //TODO: go to the database and ge tthe status of this uploaded file using the reference and draft if and pass it into here
        val result = for {
          fd <- upscanService
                 .getUpscanFileDescriptor(DraftReturnId(draftReturnId.toString), UpscanInitiateReference(reference))
        } yield (fd)

        result.fold(
          error => {
            logger.warn(s"failed to upload file with error: $error")
            errorHandler.errorResult(None)
          }, {
            case Some(value) =>
              if (value.status === UPLOADED | value.status === FAILED) {
                Ok(uploadSupportingEvidenceUpscanCheckPage(UpscanInitiateReference(reference), value.status))
              } else {
                Redirect(routes.UploadSupportingEvidenceController.checkYourAnswers())
              }
            //FIXME: make sure correct status is pumped in for this file by looking at DB
            case None => BadRequest("Could not find reference") //FIXME change to error page
          }
          // TODO: need to update the session store with new answers based on this uploaded file (just reference )
        )
      }
  }

  def uploadSupportingEvidenceVirusCheckSubmit(reference: String) = authenticatedActionWithSessionData.async {
    implicit request =>
      Redirect(routes.UploadSupportingEvidenceController.uploadSupportingEvidenceVirusCheck(reference))
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

  //TODO: the id is the id of the file that needs to be replaced
  def changeSupportingEvidence(deleteId: String) = authenticatedActionWithSessionData.async { implicit request =>
    withUploadSupportingDocumentsAnswers(request) { (draftReturnId, cgtRef, _, fillingOutReturn, answers) =>
      upscanService.initiate(DraftReturnId(draftReturnId.toString), cgtRef, LocalDateTime.now()).value.map {
        case Left(error) =>
          logger.warn(s"could not initiate upscan due to $error")
          error.value match {
            case Right(MaxFileUploadsReached2) => //FIXME: fix this constant name
              Redirect(routes.UploadSupportingEvidenceController.checkYourAnswers()) //TODO: this is not working - it is letting add more than 4
          }
        case Right(upscanInitiateResponse) =>
          upscanInitiateResponse match {
            case FailedToGetUpscanSnapshot => errorHandler.errorResult(None) //FIXME : this cann't happen remove this
            case UpscanInitiateSuccess(reference) =>
              Ok(changeSupportingEvidencePage(reference, deleteId)) //FIXME: backlink???
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

  //TODO: is going to take two parameters, one is the id that represents the file to be deleted from upscan db and hte other is the file that needs to be inserted
  @SuppressWarnings(Array("org.wartremover.warts.Var", "org.wartremover.warts.Any"))
  def changeSupportingEvidenceSubmit(ref: String, del: String) =
    authenticatedActionWithSessionData(parse.multipartFormData(maxFileSize)).async { implicit request =>
      withUploadSupportingDocumentsAnswers(request) { (draftReturnId, _, _, fillingOutReturn, answers) =>
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
          updatedAnswers: UploadSupportingDocumentAnswers = answers match {
            case IncompleteUploadSupportingDocumentAnswers(a, b) => {
              val removeOldDoc = b.filterNot(c => c.reference === del)
              IncompleteUploadSupportingDocumentAnswers(a, removeOldDoc :+ SupportingDocuments(reference, ""))
            }
            case CompleteUploadSupportingDocumentAnswers(a, b) => {
              val removeOldDoc = b.filterNot(c => c.reference === del)
              IncompleteUploadSupportingDocumentAnswers(Some(a), removeOldDoc :+ SupportingDocuments(reference, ""))
            }
          }
          newDraftReturn = fillingOutReturn.draftReturn match {
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
              s.copy(uploadSupportingDocuments = Some(updatedAnswers))
            case m @ DraftMultipleDisposalsReturn(
                  _,
                  _,
                  _,
                  _,
                  _,
                  _,
                  _
                ) =>
              m.copy(uploadSupportingDocuments = Some(updatedAnswers))
          }
          _ <- returnsService
                .storeDraftReturn(
                  newDraftReturn,
                  fillingOutReturn.subscribedDetails.cgtReference,
                  fillingOutReturn.agentReferenceNumber
                )
          _ <- EitherT(
                updateSession(sessionStore, request)(
                  _.copy(journeyStatus = Some(fillingOutReturn.copy(draftReturn = newDraftReturn)))
                )
              )
          _ <- upscanService.removeFile(DraftReturnId(draftReturnId.toString), UpscanInitiateReference(del))
        } yield (reference)

        result.fold(
          error => {
            logger.warn(s"failed to upload file with error: $error")
            errorHandler.errorResult(None)
          },
          ref => Redirect(routes.UploadSupportingEvidenceController.uploadSupportingEvidenceVirusCheck(ref))
          // TODO: need to update the session store with new answers based on this uploaded file (just reference )
        )
      }
    }

  def deleteSupportingEvidence(id: String) = authenticatedActionWithSessionData.async { implicit request =>
    withUploadSupportingDocumentsAnswers(request) { (draftReturnId, _, _, fillingOutReturn, answers) =>
      //TODO: delete session data - done
      //TODO: delete draft return - done
      //TODO: delete upscan db - done

      val result = for {
        _ <- upscanService.removeFile(DraftReturnId(draftReturnId.toString), UpscanInitiateReference(id))
        updatedAnswers: UploadSupportingDocumentAnswers = answers match {
          case IncompleteUploadSupportingDocumentAnswers(a, b) =>
            IncompleteUploadSupportingDocumentAnswers(a, b.filterNot(c => c.reference == id))
          case CompleteUploadSupportingDocumentAnswers(a, b) =>
            CompleteUploadSupportingDocumentAnswers(a, b.filterNot(c => c.reference == id))
        }
        newDraftReturn = fillingOutReturn.draftReturn match {
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
            s.copy(uploadSupportingDocuments = Some(updatedAnswers))
          case m @ DraftMultipleDisposalsReturn(
                _,
                _,
                _,
                _,
                _,
                _,
                _
              ) =>
            m.copy(uploadSupportingDocuments = Some(updatedAnswers))
        }

        _ <- returnsService
              .storeDraftReturn(
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
      }, _ => Redirect(routes.UploadSupportingEvidenceController.checkYourAnswers()))
    }
  }

  //TODO: remove this and the endpoint
  def deleteSupportingEvidenceSubmit() = authenticatedActionWithSessionData.async { implicit request =>
    Ok("deleted file")
  }

  //TODO: it shows files which have failed - shouldn't do that - filter them out
  def checkYourAnswers(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withUploadSupportingDocumentsAnswers(request) { (_, _, _, fillingOutReturn, answers) =>
      (answers, fillingOutReturn.draftReturn) match {
        case (c: UploadSupportingDocumentAnswers, s: DraftSingleDisposalReturn) => {
          checkYourAnswersHandleCalculated(c, fillingOutReturn, s)
        }

        case (c: UploadSupportingDocumentAnswers, s: DraftMultipleDisposalsReturn) => //TODO: fix - collapse into one
          checkYourAnswersHandleCalculated(c, fillingOutReturn, s)

        case _ =>
          logger.warn("error processing cya") //TODO: fix error message
          errorHandler.errorResult()
      }
    }
  }

  def checkYourAnswersSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withUploadSupportingDocumentsAnswers(request) { (_, _, _, fillingOutReturn, answers) =>
        //TODO: update datatbase with CompleteStatus

        val updatedAnswers: UploadSupportingDocumentAnswers = answers match {
          case IncompleteUploadSupportingDocumentAnswers(Some(a), b) =>
            CompleteUploadSupportingDocumentAnswers(a, b)
          case CompleteUploadSupportingDocumentAnswers(a, b) =>
            CompleteUploadSupportingDocumentAnswers(a, b)
        }
        val newDraftReturn = fillingOutReturn.draftReturn match {
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
            s.copy(uploadSupportingDocuments = Some(updatedAnswers))
          case m @ DraftMultipleDisposalsReturn(
                _,
                _,
                _,
                _,
                _,
                _,
                _
              ) =>
            m.copy(uploadSupportingDocuments = Some(updatedAnswers))
        }

        val result = for {

          _ <- returnsService
                .storeDraftReturn(
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
        }, _ => Redirect(controllers.returns.routes.TaskListController.taskList()))
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
        Redirect(routes.UploadSupportingEvidenceController.doYouWantToUploadSupportingDocuments())
      case IncompleteUploadSupportingDocumentAnswers(Some(a), list) =>
        if (a) {
          list match {
            case Nil => Redirect(routes.UploadSupportingEvidenceController.uploadSupportingEvidence())
            case ::(head, tl) => {
              val ul = for {
                l <- upscanService.getAll(DraftReturnId(draftReturn.id.toString))
              } yield (l)

              // Expired
              ul.fold(
                e => InternalServerError("some thing went wrong get all the files"),
                s => {
                  val (expired, nonexpired) = s.partition(p => p.timestamp.plusDays(7).isBefore(LocalDateTime.now))
                  val expiredList = expired.map(e =>
                    SupportingDocuments(
                      e.fileDescriptor.reference,
                      e.fileDescriptor.uploadRequest.fields.getOrElse("fileName", "could not get filename")
                    )
                  )

                  if (!expiredList.isEmpty) {
                    Ok(expiredSupportingEvidencePage(expiredList))
                  } else {
                    Ok(checkYourAnswersPage(CompleteUploadSupportingDocumentAnswers(a, list)))
                  }
                }
              )

            }
          }
        } else {
          //TODO: for this draft id we need to go and grab all the files in the upscan store to see which have expired
          val ul = for {
            l <- upscanService.getAll(DraftReturnId(draftReturn.id.toString))
          } yield (l)

          ul.fold(
            e => InternalServerError("some thing went wrong get all the files"),
            s => {
              val (expired, nonexpired) = s.partition(p => p.timestamp.plusDays(7).isBefore(LocalDateTime.now))
              val expiredList = expired.map(e =>
                SupportingDocuments(
                  e.fileDescriptor.reference,
                  e.fileDescriptor.uploadRequest.fields.getOrElse("fileName", "could not get filename")
                )
              )

              if (!expiredList.isEmpty) {
                Ok(expiredSupportingEvidencePage(expiredList))
              } else {
                Ok(checkYourAnswersPage(CompleteUploadSupportingDocumentAnswers(a, list)))
              }
            }
          )

        }
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
          _ <- returnsService
                .storeDraftReturn( // TODO: we don't need to do this?? we just need to update the filloutreturn structure with the new draftreturn
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

      case c @ CompleteUploadSupportingDocumentAnswers(a, list) => {

        val ul = for {
          l <- upscanService.getAll(DraftReturnId(draftReturn.id.toString))
        } yield (l)

        ul.fold(
          e => InternalServerError("some thing went wrong get all the files"),
          s => {
            val (expired, nonexpired) = s.partition(p => p.timestamp.plusDays(7).isBefore(LocalDateTime.now))
            val expiredList = expired.map(e =>
              SupportingDocuments(
                e.fileDescriptor.reference,
                e.fileDescriptor.uploadRequest.fields.getOrElse("fileName", "could not get filename")
              )
            )

            if (!expiredList.isEmpty) {
              Ok(expiredSupportingEvidencePage(expiredList))
            } else {
              Ok(checkYourAnswersPage(CompleteUploadSupportingDocumentAnswers(a, list)))
            }
          }
        )

      }
    }

}

object UploadSupportingEvidenceController {

  val doYouWantToUploadSupportingDocumentsForm: Form[Boolean] =
    Form(
      mapping(
        "do-you-want-to-upload-supporting-documents" -> of(BooleanFormatter.formatter)
      )(identity)(Some(_))
    )

  val scanCheckForm: Form[String] =
    Form(
      mapping(
        "reference" -> nonEmptyText
      )(identity)(Some(_))
    )

}
