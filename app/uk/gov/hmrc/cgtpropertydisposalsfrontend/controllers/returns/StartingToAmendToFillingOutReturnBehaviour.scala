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

import play.api.mvc.Result
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.ErrorHandler
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.RequestWithSessionData
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, StartingToAmendReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.TimeUtils
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.UUIDGenerator
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.CompleteReturn.{CompleteMultipleDisposalsReturn, CompleteMultipleIndirectDisposalReturn, CompleteSingleDisposalReturn, CompleteSingleIndirectDisposalReturn, CompleteSingleMixedUseDisposalReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{AmendReturnData, DraftMultipleDisposalsReturn, DraftMultipleIndirectDisposalsReturn, DraftSingleDisposalReturn, DraftSingleIndirectDisposalReturn, DraftSingleMixedUseDisposalReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging.LoggerOps
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

trait StartingToAmendToFillingOutReturnBehaviour { this: FrontendController with SessionUpdates with Logging =>

  def convertFromStartingAmendToFillingOutReturn(
    startingToAmendReturn: StartingToAmendReturn,
    sessionStore: SessionStore,
    errorHandler: ErrorHandler,
    uuidGenerator: UUIDGenerator,
    redirectUrlOverride: Option[String] = None
  )(implicit request: RequestWithSessionData[_], hc: HeaderCarrier, ec: ExecutionContext): Future[Result] = {
    val fillingOutReturn = toFillingOutReturn(startingToAmendReturn, uuidGenerator)

    updateSession(sessionStore, request)(
      _.copy(
        journeyStatus = Some(fillingOutReturn)
      )
    ).map {
      case Left(e)  =>
        logger.warn("Could not convert from StartingToAmend to FillingOutReturn", e)
        errorHandler.errorResult()

      case Right(_) =>
        logger.info("Converted from StartingToAmend to FillingOutReturn")
        Redirect(redirectUrlOverride.getOrElse(request.uri))
    }

  }

  def markUnmetDependency(
    startingToAmendReturn: StartingToAmendReturn,
    sessionStore: SessionStore,
    errorHandler: ErrorHandler
  )(implicit
    request: RequestWithSessionData[_],
    hc: HeaderCarrier,
    ec: ExecutionContext
  ): Future[Result] =
    updateSession(sessionStore, request)(
      _.copy(journeyStatus = Some(startingToAmendReturn.copy(unmetDependencyFieldUrl = Some(request.uri))))
    ).map {
      case Left(e)  =>
        logger.warn("Could not mark unmet dependency", e)
        errorHandler.errorResult()

      case Right(_) =>
        Redirect(amend.routes.AmendReturnController.unmetDependency())
    }

  private def toFillingOutReturn(s: StartingToAmendReturn, uuidGenerator: UUIDGenerator): FillingOutReturn = {
    val id          = uuidGenerator.nextId()
    val now         = TimeUtils.now().toLocalDate
    val draftReturn = s.originalReturn.completeReturn match {
      case m: CompleteMultipleDisposalsReturn        =>
        DraftMultipleDisposalsReturn(
          id,
          m.triageAnswers,
          Some(m.examplePropertyDetailsAnswers),
          None,
          None,
          None,
          None,
          m.gainOrLossAfterReliefs,
          now
        )
      case s: CompleteSingleDisposalReturn           =>
        DraftSingleDisposalReturn(
          id,
          s.triageAnswers,
          Some(s.propertyAddress),
          Some(s.disposalDetails),
          Some(s.acquisitionDetails),
          Some(s.reliefDetails),
          None,
          None,
          None,
          None,
          None,
          s.gainOrLossAfterReliefs,
          now
        )

      case s: CompleteSingleIndirectDisposalReturn   =>
        DraftSingleIndirectDisposalReturn(
          id,
          s.triageAnswers,
          Some(s.companyAddress),
          Some(s.disposalDetails),
          Some(s.acquisitionDetails),
          None,
          None,
          None,
          None,
          s.gainOrLossAfterReliefs,
          now
        )

      case m: CompleteMultipleIndirectDisposalReturn =>
        DraftMultipleIndirectDisposalsReturn(
          id,
          m.triageAnswers,
          Some(m.exampleCompanyDetailsAnswers),
          None,
          None,
          None,
          None,
          m.gainOrLossAfterReliefs,
          now
        )

      case s: CompleteSingleMixedUseDisposalReturn   =>
        DraftSingleMixedUseDisposalReturn(
          id,
          s.triageAnswers,
          Some(s.propertyDetailsAnswers),
          None,
          None,
          None,
          None,
          s.gainOrLossAfterReliefs,
          now
        )
    }

    FillingOutReturn(
      s.subscribedDetails,
      s.ggCredId,
      s.agentReferenceNumber,
      draftReturn,
      s.previousSentReturns,
      Some(
        AmendReturnData(
          s.originalReturn,
          s.originalReturn.completeReturn.gainOrLossAfterReliefs.isDefined
        )
      )
    )
  }

}
