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

import cats.data.{EitherT, NonEmptyList}
import cats.instances.future._
import com.google.inject.Inject
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.ErrorHandler
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, AuthenticatedRequest, RequestWithSessionData, SessionDataAction, WithActions}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.SubscriptionDetails.MissingData
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.SubscriptionStatus.{SubscriptionComplete, SubscriptionMissingData, SubscriptionReady}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{BusinessPartnerRecord, Error, SubscriptionDetails}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.BusinessPartnerRecordService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

class StartController @Inject()(
  bprService: BusinessPartnerRecordService,
  sessionStore: SessionStore,
  errorHandler: ErrorHandler,
  cc: MessagesControllerComponents,
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction
)(implicit ec: ExecutionContext)
    extends FrontendController(cc)
    with WithActions
    with Logging
    with SessionUpdates {

  def start(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    request.sessionData.flatMap(_.subscriptionStatus) match {
      case Some(_: SubscriptionReady)         => SeeOther(routes.SubscriptionController.checkYourDetails().url)
      case Some(_: SubscriptionComplete)      => SeeOther(routes.SubscriptionController.subscribed().url)
      case Some(SubscriptionMissingData(bpr)) => buildSubscriptionData(Some(bpr))
      case None                               => buildSubscriptionData(None)
    }
  }

  def buildSubscriptionData(maybeBpr: Option[BusinessPartnerRecord])(
    implicit
    hc: HeaderCarrier,
    request: RequestWithSessionData[_]
  ): Future[Result] = {
    val result = for {
      bpr <- maybeBpr.fold(
              bprService.getBusinessPartnerRecord(
                request.authenticatedRequest.nino,
                request.authenticatedRequest.name,
                request.authenticatedRequest.dateOfBirth
              ))(EitherT.pure(_))
      maybeSubscriptionDetails <- EitherT.pure(SubscriptionDetails(bpr, request.authenticatedRequest.name))
      _ <- EitherT(
            maybeSubscriptionDetails.fold[Future[Either[Error, Unit]]](
              _ =>
                updateSession(sessionStore, request)(_.copy(subscriptionStatus = Some(SubscriptionMissingData(bpr)))),
              subscriptionDetails =>
                updateSession(sessionStore, request)(
                  _.copy(subscriptionStatus = Some(SubscriptionReady(subscriptionDetails))))
            )
          )
    } yield maybeSubscriptionDetails

    result.fold(
      { e =>
        logger.warn("Error while getting subscription details", e)
        errorHandler.errorResult()
      }, {
        case Left(missingData) =>
          logger.info(
            s"Could not find the following data for subscription details: ${missingData.toList.mkString(",")}")
          missingData.head match {
            case MissingData.Email => SeeOther(routes.EmailController.enterEmail().url)
          }

        case Right(_) =>
          SeeOther(routes.SubscriptionController.checkYourDetails().url)
      }
    )
  }

}
