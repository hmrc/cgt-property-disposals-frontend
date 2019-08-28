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
import cats.syntax.either._
import com.google.inject.Inject
import play.api.Configuration
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, MessagesRequest, Request, Result}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.ErrorHandler
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedActionWithRetrievedData, RequestWithSessionDataAndRetrievedData, SessionDataActionWithRetrievedData, WithAuthRetrievalsAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.SubscriptionDetails.MissingData
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.SubscriptionStatus.{SubscriptionComplete, SubscriptionMissingData, SubscriptionReady}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.UserType.Individual
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{BusinessPartnerRecord, Error, NINO, Name, SessionData, SubscriptionDetails, SubscriptionStatus, UserType}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.BusinessPartnerRecordService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

class StartController @Inject()(
                                 bprService: BusinessPartnerRecordService,
                                 val sessionStore: SessionStore,
                                 val errorHandler: ErrorHandler,
                                 cc: MessagesControllerComponents,
                                 val authenticatedActionWithRetrievedData: AuthenticatedActionWithRetrievedData,
                                 val sessionDataAction: SessionDataActionWithRetrievedData,
                                 val config: Configuration
                               )(implicit ec: ExecutionContext)
    extends FrontendController(cc)
    with WithAuthRetrievalsAndSessionDataAction
    with Logging
    with SessionUpdates
    with IvBehaviour {


  def start(): Action[AnyContent] = authenticatedActionWithRetrievedDataAndSessionData.async { implicit request =>
    (request.authenticatedRequest.userType,
      request.sessionData.flatMap(_.subscriptionStatus)
    ) match {
      case (_, Some(SubscriptionStatus.InsufficientConfidenceLevel)) =>
        SeeOther(routes.InsufficientConfidenceLevelController.doYouHaveNINO().url)

      case (UserType.InsufficientConfidenceLevel(maybeNino), _) =>
        handleInsufficientConfidenceLevel(maybeNino)

      case (_, Some(_: SubscriptionReady))         =>
        SeeOther(routes.SubscriptionController.checkYourDetails().url)

     case (_, Some(_: SubscriptionComplete))      =>
        SeeOther(routes.SubscriptionController.subscribed().url)

      case (i: UserType.Individual, Some(SubscriptionMissingData(bpr, _))) =>
        buildSubscriptionData(i, Some(bpr))

      case (i: UserType.Individual, None) =>
        buildSubscriptionData(i, None)
    }
  }

  private def handleInsufficientConfidenceLevel(maybeNino: Option[NINO])(
    implicit request: RequestWithSessionDataAndRetrievedData[AnyContent]
  ): Future[Result] = maybeNino match {
    case None =>
      updateSession(sessionStore, request)(_.copy(subscriptionStatus = Some(SubscriptionStatus.InsufficientConfidenceLevel)))
        .map{
          case Left(e) =>
          logger.warn("Could not update session with insufficient confidence level", e)
          errorHandler.errorResult()

          case Right(_) =>
            SeeOther(routes.InsufficientConfidenceLevelController.doYouHaveNINO().url)
        }

    case Some(_) =>
      redirectToIv
  }

  private def buildSubscriptionData(individual: Individual,
                             maybeBpr: Option[BusinessPartnerRecord])(
    implicit
    hc: HeaderCarrier,
    request: RequestWithSessionDataAndRetrievedData[_]
  ): Future[Result] = {
    val result = for {
      bpr <- maybeBpr.fold(
              bprService.getBusinessPartnerRecord(
                individual.nino,
                individual.name,
                individual.dateOfBirth
              ))(EitherT.pure(_))
      maybeSubscriptionDetails <- EitherT.pure(
        SubscriptionDetails(bpr, individual.name, individual.email)
      )
      _ <- EitherT(
            maybeSubscriptionDetails.fold[Future[Either[Error, Unit]]](
              _ =>
                updateSession(sessionStore, request)(
                  _.copy(subscriptionStatus = Some(SubscriptionMissingData(bpr, individual.name)))),
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
