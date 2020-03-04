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

import java.time.{Clock, LocalDate}

import com.google.inject.{Inject, Singleton}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.FillingOutReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}
import cats.instances.future._
import cats.syntax.either._
import cats.syntax._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.LocalDateUtils

@Singleton
class ConfirmDraftReturnController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionStore,
  val errorHandler: ErrorHandler,
  val returnsService: ReturnsService,
  cc: MessagesControllerComponents,
  confirmationDraftReturnPage: views.html.returns.confirmation_of_draft_return
)(implicit val ec: ExecutionContext, viewConfig: ViewConfig)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with Logging {

  def confirmDraftReturn(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    request.sessionData.flatMap(_.journeyStatus) match {
      case Some(FillingOutReturn(_, _, _, draftReturn)) => {
        val draftReturnWithLastUpdated = draftReturn.copy(lastUpdatedDate = LocalDateUtils.today())

        val response = returnsService.storeDraftReturn(draftReturnWithLastUpdated)

        response.fold({ e =>
          logger.error(
            s"For cgt reference ${draftReturn.cgtReference.value}, got the following error ${e.value}")
          errorHandler.errorResult()
        }, _ => Ok(confirmationDraftReturnPage(draftReturnWithLastUpdated)))
      }
      case _ =>
        Future.successful(Redirect(baseRoutes.StartController.start()))
    }

  }

}
