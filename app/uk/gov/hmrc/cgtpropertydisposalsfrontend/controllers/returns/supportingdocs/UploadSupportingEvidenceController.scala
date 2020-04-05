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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.supportingdocs

import java.util.UUID

import cats.data.EitherT
import cats.instances.boolean._
import cats.instances.future._
import cats.syntax.eq._
import com.google.inject.{Inject, Singleton}
import configs.Configs
import configs.syntax._
import play.api.Configuration
import play.api.data.Form
import play.api.data.Forms.{mapping, nonEmptyText, of}
import play.api.http.Writeable
import play.api.i18n.Messages
import play.api.libs.Files
import play.api.mvc.{MultipartFormData, Result, _}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.upscan.UpscanConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.supportingdocs.SupportingEvidenceController._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.FillingOutReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.CgtReference
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.SubscribedDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SupportingEvidenceAnswers.{CompleteSupportingEvidenceAnswers, IncompleteSupportingEvidenceAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{DraftMultipleDisposalsReturn, DraftReturn, DraftSingleDisposalReturn, SupportingEvidenceAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.upscan.UploadReference
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.upscan.UpscanUploadStatus.Uploaded
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{BooleanFormatter, SessionData, TimeUtils}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.upscan.UpscanService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging.LoggerOps
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views.html.returns.uploadsupportingdocs.expired_supporting_evidence
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views.html.returns.{uploadsupportingdocs => pages}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.{controllers, views}
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class SupportingEvidenceController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionStore,
  val errorHandler: ErrorHandler,
  returnsService: ReturnsService,
  upscanService: UpscanService,
  upscanConnector: UpscanConnector,
  cc: MessagesControllerComponents,
  val configuration: Configuration,
  error_template: views.html.error_template,
  doYouWantToUploadSupportingEvidencePage: pages.do_you_want_to_upload_supporting_evidence,
  uploadSupportingEvidencePage: pages.upload_supporting_evidence,
  uploadSupportingEvidenceUpscanCheckPage: pages.upload_supporting_evidence_upscan_check,
  changeSupportingEvidencePage: pages.change_upload_supporting_evidence,
  expiredSupportingEvidencePage: expired_supporting_evidence,
  checkYourAnswersPage: pages.check_your_answers,
  uploadSupportingEvidenceWithoutCallBackStatus: pages.upload_supporting_evidence_without_call_back_status
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
      SupportingEvidenceAnswers
    ) => Future[Result]
  ): Future[Result] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some((s, r @ FillingOutReturn(c: SubscribedDetails, _, _, d: DraftReturn))) =>
        d match {
          case DraftSingleDisposalReturn(i, _, _, _, _, _, _, _, _, maybeSupportingDocumentsAnswers, _) =>
            maybeSupportingDocumentsAnswers.fold[Future[Result]](
              f(i, c.cgtReference, s, r, IncompleteSupportingEvidenceAnswers.empty)
            )(f(i, c.cgtReference, s, r, _))
          case DraftMultipleDisposalsReturn(i, _, _, _, _, maybeSupportingDocumentsAnswers, _) =>
            maybeSupportingDocumentsAnswers.fold[Future[Result]](
              f(i, c.cgtReference, s, r, IncompleteSupportingEvidenceAnswers.empty)
            )(f(i, c.cgtReference, s, r, _))
        }
      case _ => Redirect(controllers.routes.StartController.start())
    }

  private def commonDisplayBehaviour[A, P: Writeable, R](
    currentAnswers: SupportingEvidenceAnswers
  )(form: SupportingEvidenceAnswers => Form[A])(
    page: (Form[A], Call) => P
  )(
    requiredPreviousAnswer: SupportingEvidenceAnswers => Option[R],
    redirectToIfNoRequiredPreviousAnswer: Call
  ): Future[Result] =
    if (requiredPreviousAnswer(currentAnswers).isDefined) {
      val backLink = currentAnswers.fold(
        _ => redirectToIfNoRequiredPreviousAnswer,
        _ => routes.SupportingEvidenceController.checkYourAnswers()
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
                case IncompleteSupportingEvidenceAnswers(None, supportingEvidences, expiredEvidences) =>
                  IncompleteSupportingEvidenceAnswers(
                    Some(newDoYouWantToUploadSupportingEvidenceAnswer),
                    supportingEvidences,
                    expiredEvidences
                  )
                case IncompleteSupportingEvidenceAnswers(
                    Some(oldDoYouWantToUploadSupportingEvidenceAnswer),
                    supportingEvidences,
                    expiredEvidences
                    ) =>
                  if (newDoYouWantToUploadSupportingEvidenceAnswer =!= oldDoYouWantToUploadSupportingEvidenceAnswer) {
                    IncompleteSupportingEvidenceAnswers(
                      Some(newDoYouWantToUploadSupportingEvidenceAnswer),
                      List.empty,
                      expiredEvidences
                    )
                  } else {
                    IncompleteSupportingEvidenceAnswers(
                      Some(newDoYouWantToUploadSupportingEvidenceAnswer),
                      supportingEvidences,
                      expiredEvidences
                    )
                  }
                case CompleteSupportingEvidenceAnswers(
                    oldDoYouWantToUploadSupportingEvidenceAnswer,
                    supportingEvidences
                    ) =>
                  if (newDoYouWantToUploadSupportingEvidenceAnswer =!= oldDoYouWantToUploadSupportingEvidenceAnswer) {
                    IncompleteSupportingEvidenceAnswers(
                      Some(newDoYouWantToUploadSupportingEvidenceAnswer),
                      List.empty,
                      List.empty
                    )
                  } else {
                    IncompleteSupportingEvidenceAnswers(
                      Some(newDoYouWantToUploadSupportingEvidenceAnswer),
                      supportingEvidences,
                      List.empty
                    )
                  }
              }

              val newDraftReturn = fillingOutReturn.draftReturn match {
                case s: DraftSingleDisposalReturn    => s.copy(supportingEvidenceAnswers = Some(updatedAnswers))
                case m: DraftMultipleDisposalsReturn => m.copy(supportingEvidenceAnswers = Some(updatedAnswers))
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
              }, _ => Redirect(routes.SupportingEvidenceController.checkYourAnswers()))

            } else {
              val updatedAnswers: SupportingEvidenceAnswers = answers.fold(
                _ => IncompleteSupportingEvidenceAnswers(Some(false), List.empty, List.empty),
                _ => CompleteSupportingEvidenceAnswers(doYouWantToUploadSupportingEvidence = false, List.empty)
              )
              val newDraftReturn = fillingOutReturn.draftReturn.fold(
                _.copy(supportingEvidenceAnswers = Some(updatedAnswers)),
                _.copy(supportingEvidenceAnswers = Some(updatedAnswers))
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
              }, _ => Redirect(routes.SupportingEvidenceController.checkYourAnswers()))

            }
        )
      }
  }

  def uploadSupportingEvidenceError(): Action[AnyContent] =
    Action { implicit request =>
      InternalServerError(
        error_template(
          None,
          Messages("global.error.InternalServerError500.title"),
          Messages("global.error.InternalServerError500.heading"),
          Messages("global.error.InternalServerError500.message")
        )
      )
    }

  def uploadSupportingEvidence(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withUploadSupportingEvidenceAnswers(request) { (_, _, _, _, answers) =>
        if (answers.fold(_.evidences, _.evidences).length >= maxUploads)
          Redirect(routes.SupportingEvidenceController.checkYourAnswers())
        else {
          upscanService
            .initiate(
              routes.SupportingEvidenceController.uploadSupportingEvidenceError(),
              routes.SupportingEvidenceController.uploadSupportingEvidenceVirusCheck
            )
            .fold(
              { e =>
                logger.warn("could not start upload supporting evidence", e)
                errorHandler.errorResult()
              },
              uploadUpscan =>
                Ok(
                  uploadSupportingEvidencePage(
                    uploadEvidenceForm,
                    uploadUpscan,
                    routes.SupportingEvidenceController.doYouWantToUploadSupportingDocuments()
                  )
                )
            )
        }
      }
    }

  def uploadSupportingEvidenceVirusCheck(uploadReference: UploadReference): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withUploadSupportingEvidenceAnswers(request) { (_, _, _, _, _) =>
        val result = for {
          upscanUpload <- upscanService.getUpscanUpload(uploadReference)
          updatedUpscanUpload = upscanUpload.copy(upscanUploadStatus = Uploaded, uploadedOn = TimeUtils.now())
          _ <- upscanService.updateUpscanUpload(uploadReference, updatedUpscanUpload)
        } yield updatedUpscanUpload

        result.fold(
          e => {
            logger.warn(s"could not update the status of upscan upload to uploaded : $e")
            errorHandler.errorResult()
          },
          s => Ok(uploadSupportingEvidenceWithoutCallBackStatus(s))
        )
//        upscanService.getUpscanUpload(uploadReference).value.map {
//          case Left(error) => {
//            logger.warn(s"could not get upscan upload : $error")
//            errorHandler.errorResult()
//          }
//          case Right(upscanUpload) => {
//            println(s"\n\n\n\n ${upscanUpload.toString}\n\n\n\n")
//            //            upscanUpload.upscanCallBack match {
//            //              case Some(value) =>
//            //                value match {
//            //                  case UpscanCallBack.UpscanSuccess(reference, fileStatus, downloadUrl, uploadDetails) => {
//            //                    Ok(uploadSupportingEvidenceUpscanCheckPage(fileStatus, uploadReference))
//            //                    //Redirect(routes.SupportingEvidenceController.checkYourAnswers())
//            //                  }
//            //                  case UpscanCallBack.UpscanFailure(reference, fileStatus, failureDetails) => {
//            //                    Ok(
//            //                      uploadSupportingEvidenceUpscanCheckPage(
//            //                        fileStatus,
//            //                        uploadReference
//            //                      )
//            //                    )
//            //                  }
//            //                }
//            //              case None => {
//            //                //logger.warn(s"could not find upscan upload with reference $uploadReference")
//            Ok(uploadSupportingEvidenceWithoutCallBackStatus(upscanUpload))
//          }
      //}
      //}

      }
    }

  def uploadSupportingEvidenceVirusCheckSubmit(reference: String): Action[AnyContent] =
    authenticatedActionWithSessionData.async { _ =>
      Redirect(routes.SupportingEvidenceController.uploadSupportingEvidenceVirusCheck(UploadReference(reference)))
    }

//  @SuppressWarnings(Array("org.wartremover.warts.Var", "org.wartremover.warts.Any"))
//  private def handleGetFileDescriptorResult(
//    multipart: MultipartFormData[Files.TemporaryFile],
//    upscanFileDescriptor: UpscanFileDescriptor
//  ): Either[Error, MultipartFormData[Source[ByteString, Any]]] = {
//    val userFile =
//      multipart.files
//        .map(file => file.copy(ref = FileIO.fromPath(file.ref.path): Source[ByteString, Any]))
//    val prepared: MultipartFormData[Source[ByteString, Any]] =
//      multipart
//        .copy(
//          files = userFile,
//          dataParts = upscanFileDescriptor.fileDescriptor.uploadRequest.fields
//            .mapValues(fieldValue => Seq(fieldValue))
//        )
//    Right(prepared)
//  }

  def changeSupportingEvidence(deleteId: String): Action[AnyContent] = authenticatedActionWithSessionData.async {
    implicit request =>
      withUploadSupportingEvidenceAnswers(request) { (draftReturnId, cgtRef, _, _, answers) =>
        Future.successful(Ok("change supporting evidence"))
//        if (answers.fold(_.evidences, _.evidences).length >= maxUploads)
//          Redirect(routes.SupportingEvidenceController.checkYourAnswers())
//        else {
//          upscanService
//            .initiate(DraftReturnId(draftReturnId.toString), cgtRef, TimeUtils.now())
//            .fold(
//              { e =>
//                logger.warn("Could not perform upscan initiate", e)
//                errorHandler.errorResult()
//              },
//              success =>
//                Ok(
//                  changeSupportingEvidencePage(
//                    uploadEvidenceForm,
//                    success.upscanInitiateReference,
//                    deleteId,
//                    routes.SupportingEvidenceController.checkYourAnswers()
//                  )
//                )
//            )
      }
    //}
  }

  @SuppressWarnings(Array("org.wartremover.warts.Var", "org.wartremover.warts.Any"))
  def changeSupportingEvidenceSubmit(ref: String, del: String): Action[MultipartFormData[Files.TemporaryFile]] =
    authenticatedActionWithSessionData(parse.multipartFormData(maxFileSize)).async { implicit request =>
      val multipart: MultipartFormData[Files.TemporaryFile] = request.body
      withUploadSupportingEvidenceAnswers(request) { (draftReturnId, _, _, fillingOutReturn, answers) =>
        Future.successful(Ok("change supporting evidence"))
//        multipart
//          .file("file")
//          .map { supportingEvidence =>
//            val filesize = readAllBytes(supportingEvidence.ref.path).length
//            if (supportingEvidence.filename === "") {
//              multipart.asFormUrlEncoded.get("reference") match {
//                case Some(reference) => {
//                  Future.successful(
//                    BadRequest(
//                      changeSupportingEvidencePage(
//                        uploadEvidenceForm.bindFromRequest(request.body.asFormUrlEncoded),
//                        UpscanInitiateReference(
//                          reference.headOption.getOrElse("upscan reference not found")
//                        ),
//                        del,
//                        routes.SupportingEvidenceController.doYouWantToUploadSupportingDocuments()
//                      )
//                    )
//                  )
//                }
//                case None => Future.successful(errorHandler.errorResult())
//              }
//            } else {
//              val result = for {
//                reference <- EitherT.fromOption(
//                              multipart.dataParts.get("reference").flatMap(_.headOption),
//                              Error("missing upscan file descriptor id")
//                            )
//                maybeUpscanFileDescriptor <- upscanService
//                                              .getUpscanFileDescriptor(
//                                                DraftReturnId(draftReturnId.toString),
//                                                UpscanInitiateReference(reference)
//                                              )
//                upscanFileDescriptor <- EitherT
//                                         .fromOption(
//                                           maybeUpscanFileDescriptor,
//                                           Error("failed to retrieve upscan file descriptor details")
//                                         )
//                prepared <- EitherT
//                             .fromEither(handleGetFileDescriptorResult(multipart, upscanFileDescriptor))
//                _ <- upscanConnector
//                      .upload(upscanFileDescriptor.fileDescriptor.uploadRequest.href, prepared, filesize)
//                _ <- upscanConnector
//                      .updateUpscanFileDescriptorStatus(upscanFileDescriptor.copy(status = UPLOADED))
//
//                updatedAnswers: SupportingEvidenceAnswers = answers match {
//                  case IncompleteSupportingEvidenceAnswers(
//                      doYouWantToUploadSupportingEvidenceAnswer,
//                      supportingEvidences,
//                      expiredEvidences
//                      ) => {
//                    val removeOldDoc = supportingEvidences.filterNot(c => c.reference === del)
//                    IncompleteSupportingEvidenceAnswers(
//                      doYouWantToUploadSupportingEvidenceAnswer,
//                      removeOldDoc :+ SupportingEvidence(
//                        reference,
//                        supportingEvidence.filename,
//                        upscanFileDescriptor.timestamp
//                      ),
//                      expiredEvidences.filterNot(_.reference === del)
//                    )
//                  }
//                  case CompleteSupportingEvidenceAnswers(
//                      doYouWantToUploadSupportingEvidenceAnswer,
//                      supportingEvidences
//                      ) => {
//                    val removeOldDoc = supportingEvidences.filterNot(c => c.reference === del)
//                    IncompleteSupportingEvidenceAnswers(
//                      Some(doYouWantToUploadSupportingEvidenceAnswer),
//                      removeOldDoc :+ SupportingEvidence(
//                        reference,
//                        supportingEvidence.filename,
//                        upscanFileDescriptor.timestamp
//                      ),
//                      List.empty
//                    )
//                  }
//                }
//
//                newDraftReturn = fillingOutReturn.draftReturn match {
//                  case s: DraftSingleDisposalReturn    => s.copy(supportingEvidenceAnswers = Some(updatedAnswers))
//                  case m: DraftMultipleDisposalsReturn => m.copy(supportingEvidenceAnswers = Some(updatedAnswers))
//                }
//                _ <- returnsService
//                      .storeDraftReturn(
//                        newDraftReturn,
//                        fillingOutReturn.subscribedDetails.cgtReference,
//                        fillingOutReturn.agentReferenceNumber
//                      )
//                _ <- EitherT(
//                      updateSession(sessionStore, request)(
//                        _.copy(journeyStatus = Some(fillingOutReturn.copy(draftReturn = newDraftReturn)))
//                      )
//                    )
//
//              } yield reference
//
//              result.fold(
//                error => {
//                  logger.warn(s"failed to upload file with error: $error")
//                  errorHandler.errorResult()
//                },
//                ref => Redirect(routes.SupportingEvidenceController.uploadSupportingEvidenceVirusCheck(ref))
//              )
//            }
//
//          }
//          .getOrElse {
//            logger.warn("missing file key")
//            Future.successful(errorHandler.errorResult())
      //  }
      }
    }

  def deleteSupportingEvidence(id: String): Action[AnyContent] = authenticatedActionWithSessionData.async {
    implicit request =>
      withUploadSupportingEvidenceAnswers(request) { (_, _, _, fillingOutReturn, answers) =>
        Future.successful(Ok("deleted"))
//        val updatedAnswers = answers.fold(
//          incomplete =>
//            incomplete.copy(
//              evidences        = incomplete.evidences.filterNot(_.reference === id),
//              expiredEvidences = incomplete.expiredEvidences.filterNot(_.reference === id)
//            ), { complete =>
//            val newEvidences = complete.evidences.filterNot(_.reference === id)
//            if (newEvidences.isEmpty)
//              IncompleteSupportingEvidenceAnswers(
//                Some(complete.doYouWantToUploadSupportingEvidence),
//                List.empty,
//                List.empty
//              )
//            else
//              complete.copy(evidences = newEvidences)
//          }
//        )
//
//        val newDraftReturn = fillingOutReturn.draftReturn match {
//          case s: DraftSingleDisposalReturn    => s.copy(supportingEvidenceAnswers = Some(updatedAnswers))
//          case m: DraftMultipleDisposalsReturn => m.copy(supportingEvidenceAnswers = Some(updatedAnswers))
//        }
//
//        val result = for {
//          _ <- returnsService
//                .storeDraftReturn(
//                  newDraftReturn,
//                  fillingOutReturn.subscribedDetails.cgtReference,
//                  fillingOutReturn.agentReferenceNumber
//                )
//          _ <- EitherT(
//                updateSession(sessionStore, request)(
//                  _.copy(journeyStatus = Some(fillingOutReturn.copy(draftReturn = newDraftReturn)))
//                )
//              )
//        } yield ()
//
//        result.fold({ e =>
//          logger.warn("Could not update session", e)
//          errorHandler.errorResult()
//        }, _ => Redirect(routes.SupportingEvidenceController.checkYourAnswers()))
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
        val updatedAnswers: SupportingEvidenceAnswers = answers match {
          case IncompleteSupportingEvidenceAnswers(None, _, _) =>
            CompleteSupportingEvidenceAnswers(
              doYouWantToUploadSupportingEvidence = false,
              List.empty
            ) // shut the compiler up - this can never happen actually
          case IncompleteSupportingEvidenceAnswers(
              Some(doYouWantToUploadSupportingDocumentAnswer),
              supportingDocuments,
              _
              ) =>
            CompleteSupportingEvidenceAnswers(doYouWantToUploadSupportingDocumentAnswer, supportingDocuments)
          case CompleteSupportingEvidenceAnswers(
              doYouWantToUploadSupportingDocumentAnswer,
              supportingDocuments
              ) =>
            CompleteSupportingEvidenceAnswers(doYouWantToUploadSupportingDocumentAnswer, supportingDocuments)
        }
        val newDraftReturn = fillingOutReturn.draftReturn match {
          case s: DraftSingleDisposalReturn    => s.copy(supportingEvidenceAnswers = Some(updatedAnswers))
          case m: DraftMultipleDisposalsReturn => m.copy(supportingEvidenceAnswers = Some(updatedAnswers))
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
    answers: SupportingEvidenceAnswers,
    fillingOutReturn: FillingOutReturn,
    draftReturn: DraftReturn
  )(implicit request: RequestWithSessionData[_]): Future[Result] =
    answers match {
      case IncompleteSupportingEvidenceAnswers(_, _, expiredEvidences) if expiredEvidences.nonEmpty =>
        //Redirect(routes.SupportingEvidenceController.supportingEvidenceExpired()) //TODO: fix
        Ok("expired")

      case IncompleteSupportingEvidenceAnswers(None, _, _) =>
        Redirect(routes.SupportingEvidenceController.doYouWantToUploadSupportingDocuments())

      case IncompleteSupportingEvidenceAnswers(Some(true), supportingEvidences, _) if supportingEvidences.isEmpty =>
        Redirect(routes.SupportingEvidenceController.uploadSupportingEvidence())

      case IncompleteSupportingEvidenceAnswers(
          Some(doYouWantToUploadSupportingEvidence),
          supportingEvidences,
          _
          ) =>
        Ok(
          checkYourAnswersPage(
            CompleteSupportingEvidenceAnswers(
              doYouWantToUploadSupportingEvidence,
              supportingEvidences
            ),
            maxUploads
          )
        )

      case CompleteSupportingEvidenceAnswers(doYouWantToUploadSupportingEvidenceAnswer, supportingEvidences) =>
        Ok(
          checkYourAnswersPage(
            CompleteSupportingEvidenceAnswers(
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
        case IncompleteSupportingEvidenceAnswers(_, _, expired) if expired.nonEmpty =>
          Ok(expiredSupportingEvidencePage(expired))

        case _ =>
          Redirect(routes.SupportingEvidenceController.checkYourAnswers())
      }
    }
  }

}

object SupportingEvidenceController {
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
