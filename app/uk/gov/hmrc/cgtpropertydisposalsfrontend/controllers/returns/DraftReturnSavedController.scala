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

import cats.instances.future._
import com.google.inject.{Inject, Singleton}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.FillingOutReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.TimeUtils
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class DraftReturnSavedController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionStore,
  val errorHandler: ErrorHandler,
  val returnsService: ReturnsService,
  cc: MessagesControllerComponents,
  returnSavedPage: views.html.returns.return_saved
)(implicit val ec: ExecutionContext, viewConfig: ViewConfig)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with Logging {

  def draftReturnSaved(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      request.sessionData.flatMap(_.journeyStatus) match {
        case Some(fillingOutReturn: FillingOutReturn) =>
          if (fillingOutReturn.isAmendReturn)
            Redirect(routes.TaskListController.taskList())
          else {
            val draftReturnWithLastUpdated = fillingOutReturn.draftReturn.fold(
              _.copy(lastUpdatedDate = TimeUtils.today()),
              _.copy(lastUpdatedDate = TimeUtils.today()),
              _.copy(lastUpdatedDate = TimeUtils.today()),
              _.copy(lastUpdatedDate = TimeUtils.today()),
              _.copy(lastUpdatedDate = TimeUtils.today())
            )

            val response =
              returnsService.storeDraftReturn(fillingOutReturn.copy(draftReturn = draftReturnWithLastUpdated))

            response.fold(
              { e =>
                logger.error(
                  s"For cgt reference ${fillingOutReturn.subscribedDetails.cgtReference.value}, got the following error ${e.value}"
                )
                errorHandler.errorResult()
              },
              _ =>
                Ok(
                  returnSavedPage(
                    draftReturnWithLastUpdated,
                    fillingOutReturn.subscribedDetails.isATrust
                  )
                )
            )
          }

        case _ =>
          Future.successful(Redirect(baseRoutes.StartController.start()))
      }

    }

}
