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
import cats.data.EitherT
import cats.instances.boolean._
import cats.instances.future._
import cats.instances.string._
import cats.syntax.eq._
import com.google.inject.{Inject, Singleton}
import configs.Configs
import configs.syntax._
import play.api.Configuration
import play.api.data.Form
import play.api.data.Forms.{mapping, of}
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
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.upscan.UpscanInitiateResponse.UpscanInitiateSuccess
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

  private val maxFileSize: Int       = getUpscanInitiateConfig[Int]("max-file-size")
  private val upscanStoreExpiry: Int = getUpscanInitiateConfig[Int]("upscan-store-expiry-time")

  private def withUploadSupportingEvidenceAnswers(
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

  def doYouWantToUploadSupportingDocuments(): Action[AnyContent] = authenticatedActionWithSessionData.async {
    implicit request =>
      withUploadSupportingEvidenceAnswers(request) { (_, _, _, _, answers) =>
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
      withUploadSupportingEvidenceAnswers(request) { (draftReturnId, _, _, fillingOutReturn, answers) =>
        doYouWantToUploadSupportingDocumentsForm.bindFromRequest.fold(
          errors =>
            BadRequest(
              doYouWantToUploadSupportingEvidencePage(errors, controllers.returns.routes.TaskListController.taskList())
            ),
          newDoYouWantToUploadSupportingEvidenceAnswer =>
            if (newDoYouWantToUploadSupportingEvidenceAnswer) {
              val updatedAnswers = answers match {
                case IncompleteUploadSupportingDocumentAnswers(None, supportingEvidences) =>
                  IncompleteUploadSupportingDocumentAnswers(
                    Some(newDoYouWantToUploadSupportingEvidenceAnswer),
                    supportingEvidences
                  )
                case IncompleteUploadSupportingDocumentAnswers(
                    Some(oldDoYouWantToUploadSupportingEvidenceAnswer),
                    supportingEvidences
                    ) =>
                  if (newDoYouWantToUploadSupportingEvidenceAnswer =!= oldDoYouWantToUploadSupportingEvidenceAnswer) {
                    IncompleteUploadSupportingDocumentAnswers(
                      Some(newDoYouWantToUploadSupportingEvidenceAnswer),
                      List.empty
                    )
                  } else {
                    IncompleteUploadSupportingDocumentAnswers(
                      Some(newDoYouWantToUploadSupportingEvidenceAnswer),
                      supportingEvidences
                    )
                  }
                case CompleteUploadSupportingDocumentAnswers(
                    oldDoYouWantToUploadSupportingEvidenceAnswer,
                    supportingEvidences
                    ) =>
                  if (newDoYouWantToUploadSupportingEvidenceAnswer =!= oldDoYouWantToUploadSupportingEvidenceAnswer) {
                    IncompleteUploadSupportingDocumentAnswers(
                      Some(newDoYouWantToUploadSupportingEvidenceAnswer),
                      List.empty
                    )
                  } else {
                    IncompleteUploadSupportingDocumentAnswers(
                      Some(newDoYouWantToUploadSupportingEvidenceAnswer),
                      supportingEvidences
                    )
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
                    CompleteUploadSupportingDocumentAnswers(doYouWantToUploadSupportingDocuments = false, List.empty)
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
      }
  }

  def uploadSupportingEvidence(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withUploadSupportingEvidenceAnswers(request) { (draftReturnId, cgtRef, _, _, _) =>
      upscanService.initiate(DraftReturnId(draftReturnId.toString), cgtRef, LocalDateTime.now()).value.map {
        case Left(error) =>
          error.value match {
            case Left(_) => errorHandler.errorResult(None)
            case Right(MaxUploads) =>
              Redirect(routes.UploadSupportingEvidenceController.checkYourAnswers())
            case Right(_) => errorHandler.errorResult(None)
          }
        case Right(upscanInitiateResponse) =>
          upscanInitiateResponse match {
            case UpscanInitiateSuccess(reference) => Ok(uploadSupportingEvidencePage(reference)) //FIXME: backlink???
          }
      }
    }
  }

  @SuppressWarnings(Array("org.wartremover.warts.Var", "org.wartremover.warts.Any"))
  def uploadSupportingEvidenceSubmit(): Action[MultipartFormData[Files.TemporaryFile]] =
    authenticatedActionWithSessionData(parse.multipartFormData(maxFileSize)).async { implicit request =>
      withUploadSupportingEvidenceAnswers(request) { (draftReturnId, _, _, fillingOutReturn, answers) =>
        val multipart: MultipartFormData[Files.TemporaryFile] = request.body
        val result = for {
          reference <- EitherT.fromOption(
                        multipart.dataParts.get("reference").flatMap(_.headOption),
                        Error("missing upscan file descriptor id")
                      )
          maybeUpscanFileDescriptor <- upscanService
                                        .getUpscanFileDescriptor(
                                          DraftReturnId(draftReturnId.toString),
                                          UpscanInitiateReference(reference)
                                        )
          upscanFileDescriptor <- EitherT.fromOption(
                                   maybeUpscanFileDescriptor,
                                   Error("failed to retrieve upscan file descriptor details")
                                 )
          prepared <- EitherT.fromEither(handleGetFileDescriptorResult(multipart, upscanFileDescriptor))
          _        <- upscanConnector.upload(upscanFileDescriptor.fileDescriptor.uploadRequest.href, prepared)
          _        <- upscanConnector.updateUpscanFileDescriptorStatus(upscanFileDescriptor.copy(status = UPLOADED))
          updatedAnswers: UploadSupportingDocumentAnswers = answers match {
            case IncompleteUploadSupportingDocumentAnswers(
                doYouWantToUploadSupportingEvidenceAnswer,
                supportingEvidences
                ) =>
              IncompleteUploadSupportingDocumentAnswers(
                doYouWantToUploadSupportingEvidenceAnswer,
                supportingEvidences :+ SupportingDocuments(reference, "")
              )
            case CompleteUploadSupportingDocumentAnswers(
                doYouWantToUploadSupportingEvidenceAnswer,
                supportingEvidences
                ) =>
              IncompleteUploadSupportingDocumentAnswers(
                Some(doYouWantToUploadSupportingEvidenceAnswer),
                supportingEvidences :+ SupportingDocuments(reference, "")
              )
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
        )
      }
    }

  def uploadSupportingEvidenceVirusCheck(reference: String): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withUploadSupportingEvidenceAnswers(request) { (draftReturnId, _, _, _, _) =>
        val result = for {
          fd <- upscanService
                 .getUpscanFileDescriptor(DraftReturnId(draftReturnId.toString), UpscanInitiateReference(reference))
        } yield fd

        result.fold(
          error => {
            logger.warn(s"failed to get file descriptor information: $error")
            errorHandler.errorResult(None)
          }, {
            case Some(upscanFileDescriptor) =>
              if (upscanFileDescriptor.status === UPLOADED | upscanFileDescriptor.status === FAILED) {
                Ok(
                  uploadSupportingEvidenceUpscanCheckPage(
                    UpscanInitiateReference(reference),
                    upscanFileDescriptor.status
                  )
                )
              } else {
                Redirect(routes.UploadSupportingEvidenceController.checkYourAnswers())
              }
            case None => errorHandler.errorResult(None)
          }
        )
      }
    }

  def uploadSupportingEvidenceVirusCheckSubmit(reference: String): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
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
    withUploadSupportingEvidenceAnswers(request) { (draftReturnId, cgtRef, _, fillingOutReturn, answers) =>
      upscanService.initiate(DraftReturnId(draftReturnId.toString), cgtRef, LocalDateTime.now()).value.map {
        case Left(error) =>
          error.value match {
            case Left(_) => errorHandler.errorResult(None)
            case Right(MaxUploads) =>
              Redirect(routes.UploadSupportingEvidenceController.checkYourAnswers())
            case Right(_) => errorHandler.errorResult(None)
          }
        case Right(upscanInitiateResponse) =>
          upscanInitiateResponse match {
            case UpscanInitiateSuccess(reference) =>
              Ok(changeSupportingEvidencePage(reference, deleteId)) //FIXME: backlink???
          }
      }
    }
  }

  @SuppressWarnings(Array("org.wartremover.warts.Var", "org.wartremover.warts.Any"))
  def changeSupportingEvidenceSubmit(ref: String, del: String): Action[MultipartFormData[Files.TemporaryFile]] =
    authenticatedActionWithSessionData(parse.multipartFormData(maxFileSize)).async { implicit request =>
      withUploadSupportingEvidenceAnswers(request) { (draftReturnId, _, _, fillingOutReturn, answers) =>
        val multipart: MultipartFormData[Files.TemporaryFile] = request.body
        val result = for {
          reference <- EitherT.fromOption(
                        multipart.dataParts.get("reference").flatMap(_.headOption),
                        Error("missing upscan file descriptor id")
                      )
          maybeUpscanFileDescriptor <- upscanService
                                        .getUpscanFileDescriptor(
                                          DraftReturnId(draftReturnId.toString),
                                          UpscanInitiateReference(reference)
                                        )
          upscanFileDescriptor <- EitherT.fromOption(
                                   maybeUpscanFileDescriptor,
                                   Error("failed to retrieve upscan file descriptor details")
                                 )
          prepared <- EitherT.fromEither(handleGetFileDescriptorResult(multipart, upscanFileDescriptor))
          _        <- upscanConnector.upload(upscanFileDescriptor.fileDescriptor.uploadRequest.href, prepared)
          _        <- upscanConnector.updateUpscanFileDescriptorStatus(upscanFileDescriptor.copy(status = UPLOADED))

          updatedAnswers: UploadSupportingDocumentAnswers = answers match {
            case IncompleteUploadSupportingDocumentAnswers(
                doYouWantToUploadSupportingEvidenceAnswer,
                supportingEvidences
                ) => {
              val removeOldDoc = supportingEvidences.filterNot(c => c.reference === del)
              IncompleteUploadSupportingDocumentAnswers(
                doYouWantToUploadSupportingEvidenceAnswer,
                removeOldDoc :+ SupportingDocuments(reference, "")
              )
            }
            case CompleteUploadSupportingDocumentAnswers(
                doYouWantToUploadSupportingEvidenceAnswer,
                supportingEvidences
                ) => {
              val removeOldDoc = supportingEvidences.filterNot(c => c.reference === del)
              IncompleteUploadSupportingDocumentAnswers(
                Some(doYouWantToUploadSupportingEvidenceAnswer),
                removeOldDoc :+ SupportingDocuments(reference, "")
              )
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
        )
      }
    }

  def deleteSupportingEvidence(id: String): Action[AnyContent] = authenticatedActionWithSessionData.async {
    implicit request =>
      withUploadSupportingEvidenceAnswers(request) { (draftReturnId, _, _, fillingOutReturn, answers) =>
        val result = for {
          _ <- upscanService.removeFile(DraftReturnId(draftReturnId.toString), UpscanInitiateReference(id))
          updatedAnswers = answers match {
            case IncompleteUploadSupportingDocumentAnswers(
                doYouWantToUploadSupportingEvidenceAnswer,
                supportingEvidences
                ) =>
              IncompleteUploadSupportingDocumentAnswers(
                doYouWantToUploadSupportingEvidenceAnswer,
                supportingEvidences.filterNot(c => c.reference == id)
              )
            case CompleteUploadSupportingDocumentAnswers(
                doYouWantToUploadSupportingEvidenceAnswer,
                supportingEvidences
                ) =>
              CompleteUploadSupportingDocumentAnswers(
                doYouWantToUploadSupportingEvidenceAnswer,
                supportingEvidences.filterNot(c => c.reference == id)
              )
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

  def checkYourAnswers(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withUploadSupportingEvidenceAnswers(request) { (_, _, _, fillingOutReturn, answers) =>
      (answers, fillingOutReturn.draftReturn) match {
        case (c: UploadSupportingDocumentAnswers, s: DraftSingleDisposalReturn) => {
          checkYourAnswersHandler(c, fillingOutReturn, s)
        }
        case (c: UploadSupportingDocumentAnswers, s: DraftMultipleDisposalsReturn) =>
          checkYourAnswersHandler(c, fillingOutReturn, s)
        case _ =>
          logger.warn("error processing check your answers")
          errorHandler.errorResult()
      }
    }
  }

  def checkYourAnswersSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withUploadSupportingEvidenceAnswers(request) { (_, _, _, fillingOutReturn, answers) =>
        val updatedAnswers: UploadSupportingDocumentAnswers = answers match {
          case IncompleteUploadSupportingDocumentAnswers(None, _) =>
            CompleteUploadSupportingDocumentAnswers(doYouWantToUploadSupportingDocuments = false, List.empty) // shut the compiler up - this can never happen actually
          case IncompleteUploadSupportingDocumentAnswers(
              Some(doYouWantToUploadSupportingDocumentAnswer),
              supportingDocuments
              ) =>
            CompleteUploadSupportingDocumentAnswers(doYouWantToUploadSupportingDocumentAnswer, supportingDocuments)
          case CompleteUploadSupportingDocumentAnswers(
              doYouWantToUploadSupportingDocumentAnswer,
              supportingDocuments
              ) =>
            CompleteUploadSupportingDocumentAnswers(doYouWantToUploadSupportingDocumentAnswer, supportingDocuments)
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

  private def checkYourAnswersHandler(
    answers: UploadSupportingDocumentAnswers,
    fillingOutReturn: FillingOutReturn,
    draftReturn: DraftReturn
  )(implicit request: RequestWithSessionData[_]): Future[Result] =
    answers match {
      case IncompleteUploadSupportingDocumentAnswers(None, _) => // TODO: change name to IncompleteUploadSupportingEvidenceAnswers
        Redirect(routes.UploadSupportingEvidenceController.doYouWantToUploadSupportingDocuments())
      case IncompleteUploadSupportingDocumentAnswers(Some(doYouWantToUploadSupportingEvidence), supportingEvidences) =>
        if (doYouWantToUploadSupportingEvidence) {
          supportingEvidences match {
            case Nil => Redirect(routes.UploadSupportingEvidenceController.uploadSupportingEvidence())
            case ::(_, _) =>
              val uploadedSupportingEvidences = for {
                documents <- upscanService.getAll(DraftReturnId(draftReturn.id.toString))
              } yield documents
              uploadedSupportingEvidences.fold(
                e => InternalServerError(s"could not get all uploaded supporting evidence: $e"),
                s => {
                  val expiredSupportingEvidence = findExpiredSupportingEvidence(s)
                  if (expiredSupportingEvidence.nonEmpty) {
                    Ok(expiredSupportingEvidencePage(expiredSupportingEvidence))
                  } else {
                    Ok(
                      checkYourAnswersPage(
                        CompleteUploadSupportingDocumentAnswers(
                          doYouWantToUploadSupportingEvidence,
                          supportingEvidences
                        )
                      )
                    )
                  }
                }
              )
          }
        } else {
          val uploadedSupportingDocuments = for {
            documents <- upscanService.getAll(DraftReturnId(draftReturn.id.toString))
          } yield documents

          uploadedSupportingDocuments.fold(
            e => InternalServerError(s"could not get all uploaded supporting evidence: $e"),
            se => {
              val expiredSupportingEvidence = findExpiredSupportingEvidence(se)
              if (expiredSupportingEvidence.nonEmpty) {
                Ok(expiredSupportingEvidencePage(expiredSupportingEvidence))
              } else {
                Ok(
                  checkYourAnswersPage(
                    CompleteUploadSupportingDocumentAnswers(doYouWantToUploadSupportingEvidence, supportingEvidences)
                  )
                )
              }
            }
          )
        }

      case CompleteUploadSupportingDocumentAnswers(doYouWantToUploadSupportingEvidenceAnswer, supportingEvidences) =>
        val uploadedSupportingDocuments = for {
          documents <- upscanService.getAll(DraftReturnId(draftReturn.id.toString))
        } yield documents

        uploadedSupportingDocuments.fold(
          e => InternalServerError(s"could not get all uploaded supporting evidence: $e"),
          se => {
            val expiredSupportingEvidence = findExpiredSupportingEvidence(se)
            if (expiredSupportingEvidence.nonEmpty) {
              Ok(expiredSupportingEvidencePage(expiredSupportingEvidence))
            } else {
              Ok(
                checkYourAnswersPage(
                  CompleteUploadSupportingDocumentAnswers(
                    doYouWantToUploadSupportingEvidenceAnswer,
                    supportingEvidences
                  )
                )
              )
            }
          }
        )
    }

  private def findExpiredSupportingEvidence(fd: List[UpscanFileDescriptor]): List[SupportingDocuments] = {
    val (expired, _) = fd.partition(p => p.timestamp.plusDays(upscanStoreExpiry).isBefore(LocalDateTime.now))
    expired.map(e =>
      SupportingDocuments(
        e.fileDescriptor.reference,
        e.fileDescriptor.uploadRequest.fields.getOrElse("fileName", "could not get filename")
      )
    )
  }

}

object UploadSupportingEvidenceController {
  val doYouWantToUploadSupportingDocumentsForm: Form[Boolean] =
    Form(
      mapping(
        "do-you-want-to-upload-supporting-documents" -> of(BooleanFormatter.formatter)
      )(identity)(Some(_))
    )
}
