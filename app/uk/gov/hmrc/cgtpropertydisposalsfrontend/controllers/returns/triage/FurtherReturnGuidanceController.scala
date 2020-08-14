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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.triage

import com.google.inject.Inject
import play.api.mvc.{Action, AnyContent, Call, MessagesControllerComponents, Result}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, StartingNewDraftReturn, StartingToAmendReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.SessionData
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.RepresentativeType
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.Future

class FurtherReturnGuidanceController @Inject() (
  errorHandler: ErrorHandler,
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  cc: MessagesControllerComponents,
  guidancePage: views.html.returns.triage.further_return_guidance
)(implicit viewConfig: ViewConfig)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionUpdates
    with Logging {

  import FurtherReturnGuidanceController._

  def guidance(back: String): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withJourneyState(request) { (_, state) =>
        backLinkMappings.get(back) match {
          case None           =>
            logger.warn(s"Could not find back link location for '$back'")
            errorHandler.errorResult()

          case Some(backLink) =>
            Ok(
              guidancePage(
                backLink,
                state.fold(
                  _.fold(
                    _.subscribedDetails.isATrust,
                    _.subscribedDetails.isATrust
                  ),
                  _.subscribedDetails.isATrust
                ),
                getRepresentativeType(state)
              )
            )
        }

      }
    }

  def getRepresentativeType(
    state: Either[Either[StartingToAmendReturn, StartingNewDraftReturn], FillingOutReturn]
  ): Option[RepresentativeType] =
    state
      .fold(
        _.fold(
          _ => None,
          _.newReturnTriageAnswers.fold(
            _.representativeType(),
            _.representativeType()
          )
        ),
        _.draftReturn.representativeType()
      )

  private def withJourneyState(request: RequestWithSessionData[_])(
    f: (
      SessionData,
      Either[Either[StartingToAmendReturn, StartingNewDraftReturn], FillingOutReturn]
    ) => Future[Result]
  ): Future[Result] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some((session, s: StartingToAmendReturn))  =>
        f(session, Left(Left(s)))

      case Some((session, s: StartingNewDraftReturn)) =>
        f(session, Left(Right(s)))

      case Some((session, r: FillingOutReturn))       =>
        f(session, Right(r))

      case _                                          =>
        Redirect(
          uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.routes.StartController
            .start()
        )
    }

}

object FurtherReturnGuidanceController {

  object BackLinkLocations {

    val furtherReturnStart: String = "furtherReturnStart"
    val inYearLosses: String       = "inYearLosses"
    val calculateAmounts: String   = "calculateAmounts"

  }

  val backLinkMappings: Map[String, Call] =
    Map(
      BackLinkLocations.furtherReturnStart -> returns.triage.routes.CommonTriageQuestionsController.furtherReturnHelp(),
      BackLinkLocations.inYearLosses       -> returns.exemptionandlosses.routes.ExemptionAndLossesController
        .inYearLosses(),
      BackLinkLocations.calculateAmounts   -> returns.amend.routes.AmendReturnController
        .youNeedToCalculate()
    )

}
