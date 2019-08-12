/*
 * Copyright 2019 HM Revenue & Customs
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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers

import cats.data.EitherT
import cats.instances.future._
import com.google.inject.Inject
import play.api.mvc.{ Action, AnyContent, MessagesControllerComponents }
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.ErrorHandler
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{ AuthenticatedAction, SessionDataAction, WithActions }
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{ Error, SubscriptionDetails }
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.BusinessPartnerRecordService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.toFuture
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.{ ExecutionContext, Future }

class StartController @Inject() (
  bprService: BusinessPartnerRecordService,
  sessionStore: SessionStore,
  errorHandler: ErrorHandler, cc: MessagesControllerComponents,
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction)(implicit ec: ExecutionContext) extends FrontendController(cc) with WithActions with Logging with SessionUpdates {

  def start(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    (request.sessionData.flatMap(_.businessPartnerRecord), request.sessionData.flatMap(_.subscriptionDetails)) match {
      case (_, Some(_)) =>
        SeeOther(routes.SubscriptionController.checkYourDetails().url)

      case (maybeBpr, None) =>
        val result = for {
          bpr <- EitherT(maybeBpr.fold(bprService.getBusinessPartnerRecord(request.authenticatedRequest.nino))(b => Future.successful(Right(b))))
          subscriptionDetails <- EitherT.fromEither[Future](SubscriptionDetails.fromBusinessPartnerRecord(bpr)).leftMap(Error(_))
          _ <- EitherT(updateSession(sessionStore, request)(_.copy(subscriptionDetails = Some(subscriptionDetails))))
        } yield subscriptionDetails

        result.value.map {
          case Left(e) =>
            logger.warn("Error while getting subscription details", e)
            errorHandler.errorResult()

          case Right(_) =>
            SeeOther(routes.SubscriptionController.checkYourDetails().url)
        }
    }
  }

}
