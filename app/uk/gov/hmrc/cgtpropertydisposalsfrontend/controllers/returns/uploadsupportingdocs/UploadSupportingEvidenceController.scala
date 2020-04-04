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

import java.nio.file.Files.readAllBytes
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
import play.api.data.Forms.{mapping, nonEmptyText, of}
import play.api.http.Writeable
import play.api.libs.Files
import play.api.mvc.{MultipartFormData, _}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.UpscanConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.uploadsupportingdocs.UploadSupportingEvidenceController._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.FillingOutReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.{CgtReference, DraftReturnId}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.SubscribedDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.UploadSupportingEvidenceAnswers.{CompleteUploadSupportingEvidenceAnswers, IncompleteUploadSupportingEvidenceAnswers, SupportingEvidence}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{DraftMultipleDisposalsReturn, DraftReturn, DraftSingleDisposalReturn, UploadSupportingEvidenceAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.upscan.UpscanFileDescriptor.UpscanFileDescriptorStatus.{FAILED, UPLOADED}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.upscan.{UpscanFileDescriptor, UpscanInitiateReference}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{BooleanFormatter, Error, SessionData, TimeUtils}
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

  private val maxUploads: Int = getUpscanInitiateConfig[Int]("max-uploads")

  private val maxFileSize: Int = getUpscanInitiateConfig[Int]("max-file-size")

  private def withUploadSupportingEvidenceAnswers(
    request: RequestWithSessionData[_]
  )(
    f: (
      UUID,
      CgtReference,
      SessionData,
      FillingOutReturn,
      UploadSupportingEvidenceAnswers
    ) => Future[Result]
  ): Future[Result] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some((s, r @ FillingOutReturn(c: SubscribedDetails, _, _, d: DraftReturn))) =>
        d match {
          case DraftSingleDisposalReturn(i, _, _, _, _, _, _, _, _, maybeSupportingDocumentsAnswers, _) =>
            maybeSupportingDocumentsAnswers.fold[Future[Result]](
              f(i, c.cgtReference, s, r, IncompleteUploadSupportingEvidenceAnswers.empty)
            )(f(i, c.cgtReference, s, r, _))
          case DraftMultipleDisposalsReturn(i, _, _, _, _, maybeSupportingDocumentsAnswers, _) =>
            maybeSupportingDocumentsAnswers.fold[Future[Result]](
              f(i, c.cgtReference, s, r, IncompleteUploadSupportingEvidenceAnswers.empty)
            )(f(i, c.cgtReference, s, r, _))
        }
      case _ => Redirect(controllers.routes.StartController.start())
    }

  private def commonDisplayBehaviour[A, P: Writeable, R](
    currentAnswers: UploadSupportingEvidenceAnswers
  )(form: UploadSupportingEvidenceAnswers => Form[A])(
    page: (Form[A], Call) => P
  )(
    requiredPreviousAnswer: UploadSupportingEvidenceAnswers => Option[R],
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
            _.doYouWantToUploadSupportingEvidence
              .fold(doYouWantToUploadSupportingDocumentsForm)(doYouWantToUploadSupportingDocumentsForm.fill),
            c => doYouWantToUploadSupportingDocumentsForm.fill(c.doYouWantToUploadSupportingEvidence)
          )
        )(
          page = doYouWantToUploadSupportingEvidencePage(_, _)
        )(
          requiredPreviousAnswer               = { _ => Some(()) },
          redirectToIfNoRequiredPreviousAnswer = controllers.returns.routes.TaskListController.taskList()
        )
      }
  }

  def doYouWantToUploadSupportingDocumentsSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async {
    implicit request =>
      withUploadSupportingEvidenceAnswers(request) { (_, _, _, fillingOutReturn, answers) =>
        doYouWantToUploadSupportingDocumentsForm.bindFromRequest.fold(
          errors =>
            BadRequest(
              doYouWantToUploadSupportingEvidencePage(errors, controllers.returns.routes.TaskListController.taskList())
            ),
          newDoYouWantToUploadSupportingEvidenceAnswer =>
            if (newDoYouWantToUploadSupportingEvidenceAnswer) {
              val updatedAnswers = answers match {
                case IncompleteUploadSupportingEvidenceAnswers(None, supportingEvidences, expiredEvidences) =>
                  IncompleteUploadSupportingEvidenceAnswers(
                    Some(newDoYouWantToUploadSupportingEvidenceAnswer),
                    supportingEvidences,
                    expiredEvidences
                  )
                case IncompleteUploadSupportingEvidenceAnswers(
                    Some(oldDoYouWantToUploadSupportingEvidenceAnswer),
                    supportingEvidences,
                    expiredEvidences
                    ) =>
                  if (newDoYouWantToUploadSupportingEvidenceAnswer =!= oldDoYouWantToUploadSupportingEvidenceAnswer) {
                    IncompleteUploadSupportingEvidenceAnswers(
                      Some(newDoYouWantToUploadSupportingEvidenceAnswer),
                      List.empty,
                      expiredEvidences
                    )
                  } else {
                    IncompleteUploadSupportingEvidenceAnswers(
                      Some(newDoYouWantToUploadSupportingEvidenceAnswer),
                      supportingEvidences,
                      expiredEvidences
                    )
                  }
                case CompleteUploadSupportingEvidenceAnswers(
                    oldDoYouWantToUploadSupportingEvidenceAnswer,
                    supportingEvidences
                    ) =>
                  if (newDoYouWantToUploadSupportingEvidenceAnswer =!= oldDoYouWantToUploadSupportingEvidenceAnswer) {
                    IncompleteUploadSupportingEvidenceAnswers(
                      Some(newDoYouWantToUploadSupportingEvidenceAnswer),
                      List.empty,
                      List.empty
                    )
                  } else {
                    IncompleteUploadSupportingEvidenceAnswers(
                      Some(newDoYouWantToUploadSupportingEvidenceAnswer),
                      supportingEvidences,
                      List.empty
                    )
                  }
              }

              val newDraftReturn = fillingOutReturn.draftReturn match {
                case s: DraftSingleDisposalReturn    => s.copy(uploadSupportingDocuments = Some(updatedAnswers))
                case m: DraftMultipleDisposalsReturn => m.copy(uploadSupportingDocuments = Some(updatedAnswers))
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
              val updatedAnswers: UploadSupportingEvidenceAnswers = answers.fold(
                _ => IncompleteUploadSupportingEvidenceAnswers(Some(false), List.empty, List.empty),
                _ => CompleteUploadSupportingEvidenceAnswers(doYouWantToUploadSupportingEvidence = false, List.empty)
              )
              val newDraftReturn = fillingOutReturn.draftReturn.fold(
                _.copy(uploadSupportingDocuments = Some(updatedAnswers)),
                _.copy(uploadSupportingDocuments = Some(updatedAnswers))
              )

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

            }
        )
      }
  }

  def uploadSupportingEvidence(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withUploadSupportingEvidenceAnswers(request) { (draftReturnId, cgtRef, _, _, answers) =>
        if (answers.fold(_.evidences, _.evidences).length >= maxUploads)
          Redirect(routes.UploadSupportingEvidenceController.checkYourAnswers())
        else {
          upscanService
            .initiate(DraftReturnId(draftReturnId.toString), cgtRef, TimeUtils.now())
            .fold(
              { e =>
                logger.warn("Could not perform upscan initiate", e)
                errorHandler.errorResult()
              },
              success =>
                Ok(
                  uploadSupportingEvidencePage(
                    uploadEvidenceForm,
                    success.upscanInitiateReference,
                    routes.UploadSupportingEvidenceController.doYouWantToUploadSupportingDocuments()
                  )
                )
            )
        }
      }
    }

  @SuppressWarnings(
    Array("org.wartremover.warts.Var", "org.wartremover.warts.Any", "org.wartremover.warts.NonUnitStatements")
  )
  def uploadSupportingEvidenceSubmit(): Action[MultipartFormData[Files.TemporaryFile]] =
    authenticatedActionWithSessionData(parse.multipartFormData(maxFileSize)).async { implicit request =>
      withUploadSupportingEvidenceAnswers(request) { (draftReturnId, _, _, fillingOutReturn, answers) =>
        if (answers.fold(_.evidences, _.evidences).length >= maxUploads)
          Redirect(routes.UploadSupportingEvidenceController.checkYourAnswers())
        else {
          val multipart: MultipartFormData[Files.TemporaryFile] = request.body
          multipart
            .file("file")
            .map { supportingEvidence =>
              val filesize = readAllBytes(supportingEvidence.ref.path).length

              if (supportingEvidence.filename.trim().isEmpty) {
                multipart.asFormUrlEncoded.get("reference") match {
                  case Some(reference) =>
                    Future.successful(
                      BadRequest(
                        uploadSupportingEvidencePage(
                          uploadEvidenceForm.bindFromRequest(request.body.asFormUrlEncoded),
                          UpscanInitiateReference(
                            reference.headOption.getOrElse("upscan reference not found")
                          ),
                          routes.UploadSupportingEvidenceController.doYouWantToUploadSupportingDocuments()
                        )
                      )
                    )

                  case None => Future.successful(errorHandler.errorResult())
                }
              } else {
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
                  upscanFileDescriptor <- EitherT
                                           .fromOption(
                                             maybeUpscanFileDescriptor,
                                             Error("failed to retrieve upscan file descriptor details")
                                           )
                  prepared <- EitherT
                               .fromEither(handleGetFileDescriptorResult(multipart, upscanFileDescriptor))
                  _ <- upscanConnector
                        .upload(upscanFileDescriptor.fileDescriptor.uploadRequest.href, prepared, filesize)
                  _ <- upscanConnector
                        .updateUpscanFileDescriptorStatus(upscanFileDescriptor.copy(status = UPLOADED))
                  updatedAnswers: UploadSupportingEvidenceAnswers = answers match {
                    case IncompleteUploadSupportingEvidenceAnswers(
                        doYouWantToUploadSupportingEvidenceAnswer,
                        supportingEvidences,
                        expiredEvidences
                        ) =>
                      IncompleteUploadSupportingEvidenceAnswers(
                        doYouWantToUploadSupportingEvidenceAnswer,
                        supportingEvidences :+ SupportingEvidence(
                          reference,
                          supportingEvidence.filename,
                          upscanFileDescriptor.timestamp
                        ),
                        expiredEvidences
                      )
                    case CompleteUploadSupportingEvidenceAnswers(
                        doYouWantToUploadSupportingEvidenceAnswer,
                        supportingEvidences
                        ) =>
                      IncompleteUploadSupportingEvidenceAnswers(
                        Some(doYouWantToUploadSupportingEvidenceAnswer),
                        supportingEvidences :+ SupportingEvidence(
                          reference,
                          supportingEvidence.filename,
                          upscanFileDescriptor.timestamp
                        ),
                        List.empty
                      )
                  }
                  newDraftReturn = fillingOutReturn.draftReturn match {
                    case s: DraftSingleDisposalReturn    => s.copy(uploadSupportingDocuments = Some(updatedAnswers))
                    case m: DraftMultipleDisposalsReturn => m.copy(uploadSupportingDocuments = Some(updatedAnswers))
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

                } yield reference

                result.fold(
                  error => {
                    logger.warn(s"failed to upload file with error: $error")
                    errorHandler.errorResult()
                  },
                  ref => Redirect(routes.UploadSupportingEvidenceController.uploadSupportingEvidenceVirusCheck(ref))
                )
              }
            }
            .getOrElse {
              logger.warn("missing file key")
              Future.successful(errorHandler.errorResult())
            }
        }
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
            errorHandler.errorResult()
          }, {
            case Some(upscanFileDescriptor) =>
              if (upscanFileDescriptor.status === UPLOADED || upscanFileDescriptor.status === FAILED) {
                Ok(
                  uploadSupportingEvidenceUpscanCheckPage(
                    UpscanInitiateReference(reference),
                    upscanFileDescriptor.status
                  )
                )
              } else {
                Redirect(routes.UploadSupportingEvidenceController.checkYourAnswers())
              }
            case None => errorHandler.errorResult()
          }
        )
      }
    }

  def uploadSupportingEvidenceVirusCheckSubmit(reference: String): Action[AnyContent] =
    authenticatedActionWithSessionData.async { _ =>
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

  def changeSupportingEvidence(deleteId: String): Action[AnyContent] = authenticatedActionWithSessionData.async {
    implicit request =>
      withUploadSupportingEvidenceAnswers(request) { (draftReturnId, cgtRef, _, _, answers) =>
        if (answers.fold(_.evidences, _.evidences).length >= maxUploads)
          Redirect(routes.UploadSupportingEvidenceController.checkYourAnswers())
        else {
          upscanService
            .initiate(DraftReturnId(draftReturnId.toString), cgtRef, TimeUtils.now())
            .fold(
              { e =>
                logger.warn("Could not perform upscan initiate", e)
                errorHandler.errorResult()
              },
              success =>
                Ok(
                  changeSupportingEvidencePage(
                    uploadEvidenceForm,
                    success.upscanInitiateReference,
                    deleteId,
                    routes.UploadSupportingEvidenceController.checkYourAnswers()
                  )
                )
            )
        }
      }
  }

  @SuppressWarnings(Array("org.wartremover.warts.Var", "org.wartremover.warts.Any"))
  def changeSupportingEvidenceSubmit(ref: String, del: String): Action[MultipartFormData[Files.TemporaryFile]] =
    authenticatedActionWithSessionData(parse.multipartFormData(maxFileSize)).async { implicit request =>
      val multipart: MultipartFormData[Files.TemporaryFile] = request.body
      withUploadSupportingEvidenceAnswers(request) { (draftReturnId, _, _, fillingOutReturn, answers) =>
        multipart
          .file("file")
          .map { supportingEvidence =>
            val filesize = readAllBytes(supportingEvidence.ref.path).length
            if (supportingEvidence.filename === "") {
              multipart.asFormUrlEncoded.get("reference") match {
                case Some(reference) => {
                  Future.successful(
                    BadRequest(
                      changeSupportingEvidencePage(
                        uploadEvidenceForm.bindFromRequest(request.body.asFormUrlEncoded),
                        UpscanInitiateReference(
                          reference.headOption.getOrElse("upscan reference not found")
                        ),
                        del,
                        routes.UploadSupportingEvidenceController.doYouWantToUploadSupportingDocuments()
                      )
                    )
                  )
                }
                case None => Future.successful(errorHandler.errorResult())
              }
            } else {
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
                upscanFileDescriptor <- EitherT
                                         .fromOption(
                                           maybeUpscanFileDescriptor,
                                           Error("failed to retrieve upscan file descriptor details")
                                         )
                prepared <- EitherT
                             .fromEither(handleGetFileDescriptorResult(multipart, upscanFileDescriptor))
                _ <- upscanConnector
                      .upload(upscanFileDescriptor.fileDescriptor.uploadRequest.href, prepared, filesize)
                _ <- upscanConnector
                      .updateUpscanFileDescriptorStatus(upscanFileDescriptor.copy(status = UPLOADED))

                updatedAnswers: UploadSupportingEvidenceAnswers = answers match {
                  case IncompleteUploadSupportingEvidenceAnswers(
                      doYouWantToUploadSupportingEvidenceAnswer,
                      supportingEvidences,
                      expiredEvidences
                      ) => {
                    val removeOldDoc = supportingEvidences.filterNot(c => c.reference === del)
                    IncompleteUploadSupportingEvidenceAnswers(
                      doYouWantToUploadSupportingEvidenceAnswer,
                      removeOldDoc :+ SupportingEvidence(
                        reference,
                        supportingEvidence.filename,
                        upscanFileDescriptor.timestamp
                      ),
                      expiredEvidences.filterNot(_.reference === del)
                    )
                  }
                  case CompleteUploadSupportingEvidenceAnswers(
                      doYouWantToUploadSupportingEvidenceAnswer,
                      supportingEvidences
                      ) => {
                    val removeOldDoc = supportingEvidences.filterNot(c => c.reference === del)
                    IncompleteUploadSupportingEvidenceAnswers(
                      Some(doYouWantToUploadSupportingEvidenceAnswer),
                      removeOldDoc :+ SupportingEvidence(
                        reference,
                        supportingEvidence.filename,
                        upscanFileDescriptor.timestamp
                      ),
                      List.empty
                    )
                  }
                }

                newDraftReturn = fillingOutReturn.draftReturn match {
                  case s: DraftSingleDisposalReturn    => s.copy(uploadSupportingDocuments = Some(updatedAnswers))
                  case m: DraftMultipleDisposalsReturn => m.copy(uploadSupportingDocuments = Some(updatedAnswers))
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

              } yield reference

              result.fold(
                error => {
                  logger.warn(s"failed to upload file with error: $error")
                  errorHandler.errorResult()
                },
                ref => Redirect(routes.UploadSupportingEvidenceController.uploadSupportingEvidenceVirusCheck(ref))
              )
            }

          }
          .getOrElse {
            logger.warn("missing file key")
            Future.successful(errorHandler.errorResult())
          }
      }
    }

  def deleteSupportingEvidence(id: String): Action[AnyContent] = authenticatedActionWithSessionData.async {
    implicit request =>
      withUploadSupportingEvidenceAnswers(request) { (_, _, _, fillingOutReturn, answers) =>
        val updatedAnswers = answers.fold(
          incomplete =>
            incomplete.copy(
              evidences        = incomplete.evidences.filterNot(_.reference === id),
              expiredEvidences = incomplete.expiredEvidences.filterNot(_.reference === id)
            ), { complete =>
            val newEvidences = complete.evidences.filterNot(_.reference === id)
            if (newEvidences.isEmpty)
              IncompleteUploadSupportingEvidenceAnswers(
                Some(complete.doYouWantToUploadSupportingEvidence),
                List.empty,
                List.empty
              )
            else
              complete.copy(evidences = newEvidences)
          }
        )

        val newDraftReturn = fillingOutReturn.draftReturn match {
          case s: DraftSingleDisposalReturn    => s.copy(uploadSupportingDocuments = Some(updatedAnswers))
          case m: DraftMultipleDisposalsReturn => m.copy(uploadSupportingDocuments = Some(updatedAnswers))
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
      }
  }

  def checkYourAnswers(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withUploadSupportingEvidenceAnswers(request) { (_, _, _, fillingOutReturn, answers) =>
      checkYourAnswersHandler(answers, fillingOutReturn, fillingOutReturn.draftReturn)
    }
  }

  def checkYourAnswersSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withUploadSupportingEvidenceAnswers(request) { (_, _, _, fillingOutReturn, answers) =>
        val updatedAnswers: UploadSupportingEvidenceAnswers = answers match {
          case IncompleteUploadSupportingEvidenceAnswers(None, _, _) =>
            CompleteUploadSupportingEvidenceAnswers(
              doYouWantToUploadSupportingEvidence = false,
              List.empty
            ) // shut the compiler up - this can never happen actually
          case IncompleteUploadSupportingEvidenceAnswers(
              Some(doYouWantToUploadSupportingDocumentAnswer),
              supportingDocuments,
              _
              ) =>
            CompleteUploadSupportingEvidenceAnswers(doYouWantToUploadSupportingDocumentAnswer, supportingDocuments)
          case CompleteUploadSupportingEvidenceAnswers(
              doYouWantToUploadSupportingDocumentAnswer,
              supportingDocuments
              ) =>
            CompleteUploadSupportingEvidenceAnswers(doYouWantToUploadSupportingDocumentAnswer, supportingDocuments)
        }
        val newDraftReturn = fillingOutReturn.draftReturn match {
          case s: DraftSingleDisposalReturn    => s.copy(uploadSupportingDocuments = Some(updatedAnswers))
          case m: DraftMultipleDisposalsReturn => m.copy(uploadSupportingDocuments = Some(updatedAnswers))
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
    answers: UploadSupportingEvidenceAnswers,
    fillingOutReturn: FillingOutReturn,
    draftReturn: DraftReturn
  )(implicit request: RequestWithSessionData[_]): Future[Result] =
    answers match {
      case IncompleteUploadSupportingEvidenceAnswers(_, _, expiredEvidences) if expiredEvidences.nonEmpty =>
        Redirect(routes.UploadSupportingEvidenceController.supportingEvidenceExpired())

      case IncompleteUploadSupportingEvidenceAnswers(None, _, _) =>
        Redirect(routes.UploadSupportingEvidenceController.doYouWantToUploadSupportingDocuments())

      case IncompleteUploadSupportingEvidenceAnswers(Some(true), supportingEvidences, _)
          if supportingEvidences.isEmpty =>
        Redirect(routes.UploadSupportingEvidenceController.uploadSupportingEvidence())

      case IncompleteUploadSupportingEvidenceAnswers(
          Some(doYouWantToUploadSupportingEvidence),
          supportingEvidences,
          _
          ) =>
        Ok(
          checkYourAnswersPage(
            CompleteUploadSupportingEvidenceAnswers(
              doYouWantToUploadSupportingEvidence,
              supportingEvidences
            ),
            maxUploads
          )
        )

      case CompleteUploadSupportingEvidenceAnswers(doYouWantToUploadSupportingEvidenceAnswer, supportingEvidences) =>
        Ok(
          checkYourAnswersPage(
            CompleteUploadSupportingEvidenceAnswers(
              doYouWantToUploadSupportingEvidenceAnswer,
              supportingEvidences
            ),
            maxUploads
          )
        )
    }

  def supportingEvidenceExpired(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withUploadSupportingEvidenceAnswers(request) { (_, _, _, _, answers) =>
      answers match {
        case IncompleteUploadSupportingEvidenceAnswers(_, _, expired) if expired.nonEmpty =>
          Ok(expiredSupportingEvidencePage(expired))

        case _ =>
          Redirect(routes.UploadSupportingEvidenceController.checkYourAnswers())
      }
    }
  }

}

object UploadSupportingEvidenceController {
  val doYouWantToUploadSupportingDocumentsForm: Form[Boolean] =
    Form(
      mapping(
        "do-you-want-to-upload-supporting-evidence" -> of(BooleanFormatter.formatter)
      )(identity)(Some(_))
    )

  final case class FileUpload(filename: String, upscanReference: String)

  val uploadEvidenceForm: Form[FileUpload] =
    Form(
      mapping(
        "file"      -> nonEmptyText,
        "reference" -> nonEmptyText
      )(FileUpload.apply)(FileUpload.unapply)
    )

}
