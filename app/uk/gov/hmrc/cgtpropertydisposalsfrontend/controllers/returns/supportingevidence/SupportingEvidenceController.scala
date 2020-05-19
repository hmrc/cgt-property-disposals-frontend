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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.supportingevidence

import cats.data.EitherT
import cats.instances.future._
import cats.syntax.eq._
import com.google.inject.{Inject, Singleton}
import configs.Configs
import configs.syntax._
import play.api.Configuration
import play.api.data.Form
import play.api.data.Forms.{mapping, of}
import play.api.http.Writeable
import play.api.mvc.{Result, _}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.supportingevidence.SupportingEvidenceController._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.FillingOutReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.SubscribedDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SupportingEvidenceAnswers.{CompleteSupportingEvidenceAnswers, IncompleteSupportingEvidenceAnswers, SupportingEvidence}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.upscan.UpscanCallBack.{UpscanFailure, UpscanSuccess}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.upscan.{UploadReference, UpscanUpload}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{BooleanFormatter, Error, SessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.upscan.UpscanService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging.LoggerOps
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views.html.returns.{supportingevidence => pages}
import uk.gov.hmrc.http.HeaderCarrier
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
  cc: MessagesControllerComponents,
  val configuration: Configuration,
  doYouWantToUploadPage: pages.do_you_want_to_upload,
  uploadPage: pages.upload,
  expiredPage: pages.expired,
  checkYourAnswersPage: pages.check_your_answers,
  uploadPendingPage: pages.upload_pending,
  uploadFailedPage: pages.upload_failed
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

  private def withUploadSupportingEvidenceAnswers(
    request: RequestWithSessionData[_]
  )(
    f: (
      SessionData,
      FillingOutReturn,
      SupportingEvidenceAnswers
    ) => Future[Result]
  ): Future[Result] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some((s, r @ FillingOutReturn(_: SubscribedDetails, _, _, d: DraftReturn))) =>
        d match {
          case DraftSingleDisposalReturn(_, _, _, _, _, _, _, _, _, maybeSupportingEvidenceAnswers, _, _) =>
            maybeSupportingEvidenceAnswers.fold[Future[Result]](
              f(s, r, IncompleteSupportingEvidenceAnswers.empty)
            )(f(s, r, _))
          case DraftMultipleDisposalsReturn(_, _, _, _, _, maybeSupportingDocumentsAnswers, _, _) =>
            maybeSupportingDocumentsAnswers.fold[Future[Result]](
              f(s, r, IncompleteSupportingEvidenceAnswers.empty)
            )(f(s, r, _))
          case DraftSingleIndirectDisposalReturn(_, _, _, _, _, _, _, maybeSupportingDocumentsAnswers, _, _) =>
            maybeSupportingDocumentsAnswers.fold[Future[Result]](
              f(s, r, IncompleteSupportingEvidenceAnswers.empty)
            )(f(s, r, _))
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
      withUploadSupportingEvidenceAnswers(request) { (_, _, answers) =>
        commonDisplayBehaviour(answers)(
          form = _.fold(
            _.doYouWantToUploadSupportingEvidence
              .fold(doYouWantToUploadForm)(doYouWantToUploadForm.fill),
            c => doYouWantToUploadForm.fill(c.doYouWantToUploadSupportingEvidence)
          )
        )(
          page = doYouWantToUploadPage(_, _)
        )(
          requiredPreviousAnswer               = { _ => Some(()) },
          redirectToIfNoRequiredPreviousAnswer = controllers.returns.routes.TaskListController.taskList()
        )
      }
  }

  def doYouWantToUploadSupportingDocumentsSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async {
    implicit request =>
      withUploadSupportingEvidenceAnswers(request) { (_, fillingOutReturn, answers) =>
        doYouWantToUploadForm.bindFromRequest.fold(
          errors =>
            BadRequest(
              doYouWantToUploadPage(errors, controllers.returns.routes.TaskListController.taskList())
            ),
          newDoYouWantToUploadSupportingEvidenceAnswer =>
            if (newDoYouWantToUploadSupportingEvidenceAnswer) {
              val updatedAnswers: SupportingEvidenceAnswers = answers match {
                case IncompleteSupportingEvidenceAnswers(
                    _,
                    evidences,
                    expiredEvidences
                    ) =>
                  IncompleteSupportingEvidenceAnswers(Some(true), evidences, expiredEvidences)
                case CompleteSupportingEvidenceAnswers(_, evidences) =>
                  IncompleteSupportingEvidenceAnswers(Some(true), evidences, List.empty)
              }

              val newDraftReturn = fillingOutReturn.draftReturn match {
                case s: DraftSingleDisposalReturn         => s.copy(supportingEvidenceAnswers = Some(updatedAnswers))
                case m: DraftMultipleDisposalsReturn      => m.copy(supportingEvidenceAnswers = Some(updatedAnswers))
                case i: DraftSingleIndirectDisposalReturn => i.copy(supportingEvidenceAnswers = Some(updatedAnswers))
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

              result.fold(
                { e =>
                  logger.warn("Could not update session", e)
                  errorHandler.errorResult()
                },
                _ => Redirect(routes.SupportingEvidenceController.checkYourAnswers())
              )
            }
        )
      }
  }

  def uploadSupportingEvidence(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withUploadSupportingEvidenceAnswers(request) { (_, _, answers) =>
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
                  uploadPage(
                    uploadUpscan,
                    routes.SupportingEvidenceController.doYouWantToUploadSupportingDocuments()
                  )
                )
            )
        }
      }
    }

  def uploadSupportingEvidenceError(): Action[AnyContent] = authenticatedActionWithSessionData { implicit request =>
    errorHandler.errorResult()
  }

  def uploadSupportingEvidenceVirusCheck(uploadReference: UploadReference): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withUploadSupportingEvidenceAnswers(request) { (_, fillingOutReturn, answers) =>
        answers match {
          case _: CompleteSupportingEvidenceAnswers =>
            Redirect(routes.SupportingEvidenceController.checkYourAnswers())

          case incompleteAnswers: IncompleteSupportingEvidenceAnswers =>
            val result = for {
              upscanUpload <- upscanService.getUpscanUpload(uploadReference)
              _ <- upscanUpload.upscanCallBack match {
                    case Some(s: UpscanSuccess) =>
                      storeUpscanSuccess(upscanUpload, s, incompleteAnswers, fillingOutReturn)
                    case _ =>
                      EitherT.pure[Future, Error](())
                  }
            } yield upscanUpload

            result.fold(
              e => {
                logger.warn(s"could not update the status of upscan upload to uploaded : $e")
                errorHandler.errorResult()
              },
              upscanUpload =>
                upscanUpload.upscanCallBack match {
                  case Some(_: UpscanSuccess) =>
                    Redirect(routes.SupportingEvidenceController.checkYourAnswers())
                  case Some(_: UpscanFailure) =>
                    Ok(uploadFailedPage())
                  case None =>
                    Ok(uploadPendingPage(upscanUpload))
                }
            )
        }

      }

    }

  def uploadSupportingEvidenceVirusCheckSubmit(uploadReference: String): Action[AnyContent] =
    authenticatedActionWithSessionData.async { _ =>
      Redirect(routes.SupportingEvidenceController.uploadSupportingEvidenceVirusCheck(UploadReference(uploadReference)))
    }

  private def storeUpscanSuccess(
    upscanUpload: UpscanUpload,
    upscanCallBack: UpscanSuccess,
    answers: IncompleteSupportingEvidenceAnswers,
    fillingOutReturn: FillingOutReturn
  )(implicit request: RequestWithSessionData[_], hc: HeaderCarrier): EitherT[Future, Error, Unit] = {
    val newAnswers =
      upscanCallBack match {
        case success: UpscanSuccess =>
          val supportingEvidence =
            SupportingEvidence(
              upscanUpload.uploadReference,
              upscanUpload.upscanUploadMeta,
              upscanUpload.uploadedOn,
              success,
              success.fileName
            )
          answers.copy(evidences = supportingEvidence :: answers.evidences)

      }

    val newDraftReturn = fillingOutReturn.draftReturn.fold(
      _.copy(supportingEvidenceAnswers = Some(newAnswers)),
      _.copy(supportingEvidenceAnswers = Some(newAnswers)),
      _.copy(supportingEvidenceAnswers = Some(newAnswers))
    )

    for {
      _ <- returnsService.storeDraftReturn(
            newDraftReturn,
            fillingOutReturn.subscribedDetails.cgtReference,
            fillingOutReturn.agentReferenceNumber
          )
      _ <- EitherT(
            updateSession(sessionStore, request)(
              _.copy(journeyStatus =
                Some(
                  fillingOutReturn.copy(draftReturn = newDraftReturn)
                )
              )
            )
          )
    } yield ()
  }

  def deleteSupportingEvidence(uploadReference: UploadReference, addNew: Boolean): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withUploadSupportingEvidenceAnswers(request) { (_, fillingOutReturn, answers) =>
        val updatedAnswers = answers.fold(
          incomplete =>
            incomplete.copy(
              evidences        = incomplete.evidences.filterNot(_.uploadReference === uploadReference),
              expiredEvidences = incomplete.expiredEvidences.filterNot(_.uploadReference === uploadReference)
            ), { complete =>
            val newEvidences = complete.evidences.filterNot(_.uploadReference === uploadReference)
            IncompleteSupportingEvidenceAnswers(
              Some(complete.doYouWantToUploadSupportingEvidence),
              newEvidences,
              List.empty
            )
          }
        )

        val newDraftReturn = fillingOutReturn.draftReturn match {
          case s: DraftSingleDisposalReturn         => s.copy(supportingEvidenceAnswers = Some(updatedAnswers))
          case m: DraftMultipleDisposalsReturn      => m.copy(supportingEvidenceAnswers = Some(updatedAnswers))
          case i: DraftSingleIndirectDisposalReturn => i.copy(supportingEvidenceAnswers = Some(updatedAnswers))
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

        result.fold(
          { e =>
            logger.warn("Could not update session", e)
            errorHandler.errorResult()
          },
          _ =>
            if (addNew)
              Redirect(routes.SupportingEvidenceController.uploadSupportingEvidence())
            else
              Redirect(routes.SupportingEvidenceController.checkYourAnswers())
        )
      }
    }

  def checkYourAnswers(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withUploadSupportingEvidenceAnswers(request) { (_, fillingOutReturn, answers) =>
      checkYourAnswersHandler(answers, fillingOutReturn, fillingOutReturn.draftReturn)
    }
  }

  def checkYourAnswersSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withUploadSupportingEvidenceAnswers(request) { (_, fillingOutReturn, answers) =>
        val updatedAnswers: SupportingEvidenceAnswers = answers match {
          case IncompleteSupportingEvidenceAnswers(None, _, _) =>
            sys.error(
              "Could not find answer to 'do you want to upload?' question in incomplete supporting evidence answers"
            )

          case IncompleteSupportingEvidenceAnswers(
              Some(doYouWantToUploadSupportingEvidence),
              evidences,
              _
              ) =>
            CompleteSupportingEvidenceAnswers(doYouWantToUploadSupportingEvidence, evidences)
          case CompleteSupportingEvidenceAnswers(doYouWantToUploadSupportingEvidence, evidences) =>
            CompleteSupportingEvidenceAnswers(doYouWantToUploadSupportingEvidence, evidences)
        }

        val newDraftReturn = fillingOutReturn.draftReturn match {
          case s: DraftSingleDisposalReturn         => s.copy(supportingEvidenceAnswers = Some(updatedAnswers))
          case m: DraftMultipleDisposalsReturn      => m.copy(supportingEvidenceAnswers = Some(updatedAnswers))
          case i: DraftSingleIndirectDisposalReturn => i.copy(supportingEvidenceAnswers = Some(updatedAnswers))
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
        Redirect(routes.SupportingEvidenceController.supportingEvidenceExpired())

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
    withUploadSupportingEvidenceAnswers(request) { (_, _, answers) =>
      answers match {
        case IncompleteSupportingEvidenceAnswers(_, _, expired) if expired.nonEmpty =>
          Ok(expiredPage(expired))
        case _ =>
          Redirect(routes.SupportingEvidenceController.checkYourAnswers())
      }
    }
  }

  def supportingEvidenceExpiredSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async {
    implicit request =>
      withUploadSupportingEvidenceAnswers(request) { (_, fillingOutReturn, answers) =>
        answers match {
          case IncompleteSupportingEvidenceAnswers(_, _, expired) if expired.nonEmpty =>
            val updatedAnswers = answers.fold(
              _.copy(expiredEvidences = List.empty),
              complete =>
                IncompleteSupportingEvidenceAnswers(
                  Some(complete.doYouWantToUploadSupportingEvidence),
                  complete.evidences,
                  List.empty
                )
            )
            val updatedDraftReturn = fillingOutReturn.draftReturn.fold(
              _.copy(supportingEvidenceAnswers = Some(updatedAnswers)),
              _.copy(supportingEvidenceAnswers = Some(updatedAnswers)),
              _.copy(supportingEvidenceAnswers = Some(updatedAnswers))
            )

            val result = for {
              _ <- returnsService.storeDraftReturn(
                    updatedDraftReturn,
                    fillingOutReturn.subscribedDetails.cgtReference,
                    fillingOutReturn.agentReferenceNumber
                  )
              _ <- EitherT(
                    updateSession(sessionStore, request)(
                      _.copy(journeyStatus = Some(fillingOutReturn.copy(draftReturn = updatedDraftReturn)))
                    )
                  )
            } yield ()

            result.fold(
              { e =>
                logger.warn("Could not update draft return", e)
                errorHandler.errorResult()
              }, { _ =>
                logger.info(s"Deleted expired evidence}")
                Redirect(routes.SupportingEvidenceController.checkYourAnswers())
              }
            )

          case _ =>
            Redirect(routes.SupportingEvidenceController.checkYourAnswers())
        }
      }
  }

}

object SupportingEvidenceController {
  val doYouWantToUploadForm: Form[Boolean] =
    Form(
      mapping(
        "supporting-evidence.do-you-want-to-upload" -> of(BooleanFormatter.formatter)
      )(identity)(Some(_))
    )
}
