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
import com.google.inject.{Inject, Singleton}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithActions}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.BusinessPartnerRecordService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class SubscriptionController @Inject() (
    bprService: BusinessPartnerRecordService,
    sessionStore: SessionStore,
    errorHandler: ErrorHandler,
    cc: MessagesControllerComponents,
    val authenticatedAction: AuthenticatedAction,
    val sessionDataAction: SessionDataAction,
    checkYourDetailsPage: views.html.subscription.check_your_details
)(implicit viewConfig: ViewConfig, ec: ExecutionContext) extends FrontendController(cc) with WithActions with Logging {

  def checkYourDetails(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    request.sessionData.flatMap(_.businessPartnerRecord) match {
      case Some(bpr) =>
        Ok(checkYourDetailsPage(bpr))

      case None =>
        val nino = NINO("AB123456C")
        val result = for {
          bpr <- EitherT(bprService.getBusinessPartnerRecord(nino))
          _ <- EitherT(updateSession(_.copy(businessPartnerRecord = Some(bpr))))
        } yield bpr

        result.value.map {
          case Left(e) =>
            logger.warn("Error while getting BPR", e)
            InternalServerError(errorHandler.internalServerErrorTemplate)

          case Right(bpr) =>
            Ok(checkYourDetailsPage(bpr))
        }
    }
  }

  def checkYourDetailsSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    request.sessionData.flatMap(_.businessPartnerRecord) match {
      case None =>
        SeeOther(routes.SubscriptionController.checkYourDetails().url)

      case Some(_) =>
        Ok("confirmed")
    }
  }

  private def updateSession(update: SessionData => SessionData)(implicit request: RequestWithSessionData[_]): Future[Either[Error, Unit]] =
    sessionStore.store(update(request.sessionData.getOrElse(SessionData.empty)))

}

