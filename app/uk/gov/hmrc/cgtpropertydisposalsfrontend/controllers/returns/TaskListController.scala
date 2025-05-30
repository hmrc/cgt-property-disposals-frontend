/*
 * Copyright 2023 HM Revenue & Customs
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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns

import cats.data.{EitherT, NonEmptyList}
import cats.instances.future._
import com.google.inject.{Inject, Singleton}
import play.api.Configuration
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.ErrorHandler
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{SessionUpdates, routes => baseRoutes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.FillingOutReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SupportingEvidenceAnswers.IncompleteSupportingEvidenceAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.YearToDateLiabilityAnswers.{CalculatedYTDAnswers, NonCalculatedYTDAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, TimeUtils}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, given}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views.html.returns.{tasklist => taskListPages}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import java.time.LocalDateTime
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class TaskListController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionStore,
  val errorHandler: ErrorHandler,
  configuration: Configuration,
  returnsService: ReturnsService,
  cc: MessagesControllerComponents,
  singleDisposalTaskListPage: taskListPages.single_disposal_task_list,
  multipleDisposalsTaskListPage: taskListPages.multiple_disposals_task_list,
  singleIndirectDisposalTaskListPage: taskListPages.single_indirect_disposal_task_list,
  singleMixedUseDisposalTaskListPage: taskListPages.single_mixed_use_disposal_task_list,
  multipleIndirectDisposalTaskListPage: taskListPages.multiple_indirect_disposals_task_list
)(implicit ec: ExecutionContext)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionUpdates
    with Logging {
  private val s3UrlExpirySeconds =
    configuration.get[FiniteDuration]("microservice.services.upscan-initiate.s3-url-expiry-duration").toSeconds

  def taskList(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      request.sessionData.flatMap(_.journeyStatus) match {
        case Some(f @ FillingOutReturn(_, _, _, draftReturn, _, _)) =>
          handleExpiredFiles(draftReturn, f).fold(
            { e =>
              logger.warn("Could not handle expired files", e)
              errorHandler.errorResult()
            },
            _.fold(
              m => Ok(multipleDisposalsTaskListPage(m, f)),
              s => Ok(singleDisposalTaskListPage(s, f)),
              si => Ok(singleIndirectDisposalTaskListPage(si, f)),
              mi => Ok(multipleIndirectDisposalTaskListPage(mi, f)),
              sm => Ok(singleMixedUseDisposalTaskListPage(sm, f))
            )
          )
        case _                                                      =>
          Redirect(baseRoutes.StartController.start())
      }
    }

  private def handleExpiredFiles(
    draftReturn: DraftReturn,
    journey: FillingOutReturn
  )(implicit
    request: RequestWithSessionData[?],
    hc: HeaderCarrier
  ) = {
    val updatedUploadSupportingEvidenceAnswers =
      getExpiredSupportingEvidence(draftReturn).map { case (expired, answers) =>
        val incompleteAnswers =
          answers.fold(
            identity,
            c =>
              IncompleteSupportingEvidenceAnswers(
                Some(c.doYouWantToUploadSupportingEvidence),
                c.evidences,
                List.empty
              )
          )
        incompleteAnswers.copy(
          evidences = incompleteAnswers.evidences.diff(expired.toList),
          expiredEvidences = expired.toList ::: incompleteAnswers.expiredEvidences
        )
      }

    val updatedYearToDateAnswers =
      getExpiredMandatoryEvidence(draftReturn).map { case (expired, answers) =>
        answers match {
          case c: CalculatedYTDAnswers    =>
            c.unset(_.mandatoryEvidence)
              .unset(_.pendingUpscanUpload)
              .copy(expiredEvidence = Some(expired))
          case n: NonCalculatedYTDAnswers =>
            n.unset(_.mandatoryEvidence)
              .unset(_.pendingUpscanUpload)
              .copy(expiredEvidence = Some(expired))
        }
      }

    if (updatedUploadSupportingEvidenceAnswers.isEmpty && updatedYearToDateAnswers.isEmpty) {
      EitherT.pure[Future, Error](draftReturn)
    } else {
      val updatedDraftReturn = draftReturn.fold(
        multiple =>
          multiple.copy(
            yearToDateLiabilityAnswers = updatedYearToDateAnswers.fold(
              multiple.yearToDateLiabilityAnswers
            )(Some(_)),
            supportingEvidenceAnswers = updatedUploadSupportingEvidenceAnswers
              .fold(multiple.supportingEvidenceAnswers)(Some(_))
          ),
        single =>
          single.copy(
            yearToDateLiabilityAnswers = updatedYearToDateAnswers.fold(
              single.yearToDateLiabilityAnswers
            )(Some(_)),
            supportingEvidenceAnswers = updatedUploadSupportingEvidenceAnswers
              .fold(single.supportingEvidenceAnswers)(Some(_))
          ),
        singleIndirect =>
          singleIndirect.copy(
            yearToDateLiabilityAnswers = updatedYearToDateAnswers.fold(
              singleIndirect.yearToDateLiabilityAnswers
            )(Some(_)),
            supportingEvidenceAnswers = updatedUploadSupportingEvidenceAnswers.fold(
              singleIndirect.supportingEvidenceAnswers
            )(Some(_))
          ),
        multipleIndirect =>
          multipleIndirect.copy(
            yearToDateLiabilityAnswers = updatedYearToDateAnswers.fold(
              multipleIndirect.yearToDateLiabilityAnswers
            )(Some(_)),
            supportingEvidenceAnswers = updatedUploadSupportingEvidenceAnswers
              .fold(multipleIndirect.supportingEvidenceAnswers)(Some(_))
          ),
        singleMixedUse =>
          singleMixedUse.copy(
            yearToDateLiabilityAnswers = updatedYearToDateAnswers.fold(
              singleMixedUse.yearToDateLiabilityAnswers
            )(Some(_)),
            supportingEvidenceAnswers = updatedUploadSupportingEvidenceAnswers.fold(
              singleMixedUse.supportingEvidenceAnswers
            )(Some(_))
          )
      )

      for {
        _ <- returnsService.storeDraftReturn(journey)
        _ <- EitherT(
               updateSession(sessionStore, request.toSession)(
                 _.copy(journeyStatus = Some(journey.copy(draftReturn = updatedDraftReturn)))
               )
             )
      } yield updatedDraftReturn
    }
  }

  private def getExpiredSupportingEvidence(
    draftReturn: DraftReturn
  ) =
    draftReturn
      .fold(
        _.supportingEvidenceAnswers,
        _.supportingEvidenceAnswers,
        _.supportingEvidenceAnswers,
        _.supportingEvidenceAnswers,
        _.supportingEvidenceAnswers
      )
      .flatMap { answers =>
        val supportingEvidence = answers.fold(_.evidences, _.evidences)
        supportingEvidence.filter(f => fileHasExpired(f.uploadedOn)) match {
          case h :: t => Some(NonEmptyList(h, t) -> answers)
          case Nil    => None
        }
      }

  private def getExpiredMandatoryEvidence(
    draftReturn: DraftReturn
  ) =
    draftReturn
      .fold(
        _.yearToDateLiabilityAnswers,
        _.yearToDateLiabilityAnswers,
        _.yearToDateLiabilityAnswers,
        _.yearToDateLiabilityAnswers,
        _.yearToDateLiabilityAnswers
      )
      .flatMap { answers =>
        val mandatoryEvidence = answers match {
          case c: CalculatedYTDAnswers    =>
            c.fold(_.mandatoryEvidence, _.mandatoryEvidence)
          case n: NonCalculatedYTDAnswers =>
            n.fold(_.mandatoryEvidence, _.mandatoryEvidence)
        }

        mandatoryEvidence
          .filter(m => fileHasExpired(m.uploadedOn))
          .map(_ -> answers)
      }

  private def fileHasExpired(createdOnTimestamp: LocalDateTime) =
    createdOnTimestamp.plusSeconds(s3UrlExpirySeconds).isBefore(TimeUtils.now())
}
