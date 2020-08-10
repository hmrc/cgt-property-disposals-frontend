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
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.ViewConfig
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, StartingNewDraftReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.SessionData
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.RepresentativeType
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

//import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.{Future}

class FurtherReturnGuidanceController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  cc: MessagesControllerComponents,
  guidancePage: views.html.returns.triage.further_return_guidance
)(implicit viewConfig: ViewConfig) //, ec: ExecutionContext)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionUpdates
    with Logging {

  def guidance(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withState(request) { (_, state) =>
        Ok(
          guidancePage(
            backLink,
            state.fold(
              _.subscribedDetails.isATrust,
              _.subscribedDetails.isATrust
            ),
            getRepresentativeType(state)
          )
        )
      }
    }

  def guidanceSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withState(request) { (_, state) =>
        Ok(
          guidancePage(backLink, true, getRepresentativeType(state))
        )
      }
    }

  private def backLink(): Call =
    uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.routes.StartController
      .start()

  def getRepresentativeType(
    state: Either[StartingNewDraftReturn, FillingOutReturn]
  ): Option[RepresentativeType] =
    state
      .fold(
        _.newReturnTriageAnswers.fold(
          _.representativeType(),
          _.representativeType()
        ),
        _.draftReturn.representativeType()
      )

  private def withState(request: RequestWithSessionData[_])(
    f: (
      SessionData,
      Either[StartingNewDraftReturn, FillingOutReturn]
    ) => Future[Result]
  ): Future[Result] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some((session, s: StartingNewDraftReturn)) =>
        f(session, Left(s))

      case Some((session, r: FillingOutReturn))       =>
        f(session, Right(r))

      case _                                          =>
        Redirect(
          uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.routes.StartController
            .start()
        )
    }

}
