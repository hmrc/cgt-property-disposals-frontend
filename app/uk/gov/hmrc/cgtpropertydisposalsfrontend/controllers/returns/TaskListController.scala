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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns

import java.time.LocalDateTime

import cats.data.{EitherT, NonEmptyList}
import cats.instances.future._
import com.google.inject.{Inject, Singleton}
import configs.syntax._
import play.api.Configuration
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{SessionUpdates, routes => baseRoutes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.FillingOutReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SupportingEvidenceAnswers.{IncompleteSupportingEvidenceAnswers, SupportingEvidence}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.YearToDateLiabilityAnswers.{CalculatedYTDAnswers, NonCalculatedYTDAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, TimeUtils}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

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
  singleDisposalTaskListPage: views.html.returns.single_disposal_task_list,
  multipleDisposalsTaskListPage: views.html.returns.multiple_disposals_task_list
)(implicit viewConfig: ViewConfig, ec: ExecutionContext)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionUpdates
    with Logging {

  private val s3UrlExpirySeconds: Long =
    configuration.underlying
      .get[FiniteDuration]("microservice.services.upscan-initiate.s3-url-expiry-duration")
      .value
      .toSeconds

  def taskList(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    request.sessionData.flatMap(_.journeyStatus) match {
      case Some(f @ FillingOutReturn(_, _, _, draftReturn)) =>
        handleExpiredFiles(draftReturn, f).fold(
          { e =>
            logger.warn("Could not handle expired files", e)
            errorHandler.errorResult()
          },
          _.fold(
            m => Ok(multipleDisposalsTaskListPage(m)),
            s => Ok(singleDisposalTaskListPage(s))
          )
        )

      case _ =>
        Redirect(baseRoutes.StartController.start())

    }

  }

  private def handleExpiredFiles(
    draftReturn: DraftReturn,
    journey: FillingOutReturn
  )(implicit request: RequestWithSessionData[_], hc: HeaderCarrier): EitherT[Future, Error, DraftReturn] = {
    val updatedUploadSupportingEvidenceAnswers = getExpiredSupportingEvidence(draftReturn).map {
      case (expired, answers) =>
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
          evidences        = incompleteAnswers.evidences.diff(expired.toList),
          expiredEvidences = expired.toList ::: incompleteAnswers.expiredEvidences
        )
    }

    val updatedYearToDateAnswers = getExpiredMandatoryEvidence(draftReturn).map {
      case (expired, answers) =>
        answers match {
          case c: CalculatedYTDAnswers =>
            c.unset(_.mandatoryEvidence).unset(_.pendingUpscanUpload).copy(expiredEvidence = Some(expired))
          case n: NonCalculatedYTDAnswers =>
            n.unset(_.mandatoryEvidence).unset(_.pendingUpscanUpload).copy(expiredEvidence = Some(expired))
        }
    }

    if (updatedUploadSupportingEvidenceAnswers.isEmpty && updatedYearToDateAnswers.isEmpty)
      EitherT.pure[Future, Error](draftReturn)
    else {
      val updatedDraftReturn = draftReturn.fold(
        m =>
          m.copy(
            yearToDateLiabilityAnswers = updatedYearToDateAnswers.fold(m.yearToDateLiabilityAnswers)(Some(_)),
            supportingEvidenceAnswers =
              updatedUploadSupportingEvidenceAnswers.fold(m.supportingEvidenceAnswers)(Some(_))
          ),
        s =>
          s.copy(
            yearToDateLiabilityAnswers = updatedYearToDateAnswers.fold(s.yearToDateLiabilityAnswers)(Some(_)),
            supportingEvidenceAnswers =
              updatedUploadSupportingEvidenceAnswers.fold(s.supportingEvidenceAnswers)(Some(_))
          )
      )

      for {
        _ <- returnsService.storeDraftReturn(
              updatedDraftReturn,
              journey.subscribedDetails.cgtReference,
              journey.agentReferenceNumber
            )
        _ <- EitherT(
              updateSession(sessionStore, request)(
                _.copy(journeyStatus = Some(journey.copy(draftReturn = updatedDraftReturn)))
              )
            )
      } yield updatedDraftReturn
    }

  }

  private def getExpiredSupportingEvidence(
    draftReturn: DraftReturn
  ): Option[(NonEmptyList[SupportingEvidence], SupportingEvidenceAnswers)] =
    draftReturn
      .fold(_.supportingEvidenceAnswers, _.supportingEvidenceAnswers)
      .flatMap { answers =>
        val supportingEvidence = answers.fold(_.evidences, _.evidences)
        supportingEvidence.filter(f => fileHasExpired(f.uploadedOn)) match {
          case h :: t => Some(NonEmptyList(h, t) -> answers)
          case Nil    => None
        }
      }

  private def getExpiredMandatoryEvidence(
    draftReturn: DraftReturn
  ): Option[(MandatoryEvidence, YearToDateLiabilityAnswers)] =
    draftReturn
      .fold(_.yearToDateLiabilityAnswers, _.yearToDateLiabilityAnswers)
      .flatMap { answers =>
        val mandatoryEvidence = answers match {
          case c: CalculatedYTDAnswers    => c.fold(_.mandatoryEvidence, _.mandatoryEvidence)
          case n: NonCalculatedYTDAnswers => n.fold(_.mandatoryEvidence, c => Some(c.mandatoryEvidence))
        }

        mandatoryEvidence
          .filter(m => fileHasExpired(m.uploadedOn))
          .map(_ -> answers)
      }

  private def fileHasExpired(createdOnTimestamp: LocalDateTime): Boolean =
    createdOnTimestamp.plusSeconds(s3UrlExpirySeconds).isBefore(TimeUtils.now())

}
