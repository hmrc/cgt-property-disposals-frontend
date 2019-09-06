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
import play.api.Configuration
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.ErrorHandler
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedActionWithRetrievedData, RequestWithSessionDataAndRetrievedData, SessionDataActionWithRetrievedData, WithAuthRetrievalsAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.SubscriptionDetails.MissingData
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.SubscriptionStatus.{SubscriptionComplete, SubscriptionMissingData, SubscriptionReady}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.UserType._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models._
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
      case (_, Some(_: SubscriptionStatus.IndividualWithInsufficientConfidenceLevel)) =>
        SeeOther(routes.InsufficientConfidenceLevelController.doYouHaveNINO().url)

      case (UserType.InsufficientConfidenceLevel(maybeNino), _) =>
        handleInsufficientConfidenceLevel(maybeNino)

      case (_, Some(_: SubscriptionReady))         =>
        SeeOther(routes.SubscriptionController.checkYourDetails().url)

     case (_, Some(_: SubscriptionComplete))      =>
        SeeOther(routes.SubscriptionController.subscribed().url)

      case (i: UserType.Individual, Some(SubscriptionMissingData(bpr, _))) =>
        buildIndividualSubscriptionData(i, Some(bpr))

      case (i: UserType.Individual, None) =>
        buildIndividualSubscriptionData(i, None)

      case (t: UserType.Trust, _) =>
        buildTrustSubscriptionData(t)

      case (UserType.OrganisationUnregisteredTrust, _) | (_, Some(SubscriptionStatus.OrganisationUnregisteredTrust)) =>
        handleNonTrustOrganisation()
    }
  }

  private def handleNonTrustOrganisation()(
    implicit request: RequestWithSessionDataAndRetrievedData[_]
  ): Future[Result] = {
    lazy val redirectToRegisterTrustPage =
      SeeOther(routes.RegisterTrustController.registerYourTrust().url)

    if(request.sessionData.flatMap(_.subscriptionStatus).contains(SubscriptionStatus.OrganisationUnregisteredTrust)){
      redirectToRegisterTrustPage
    } else {
      updateSession(sessionStore, request)(_.copy(subscriptionStatus = Some(SubscriptionStatus.OrganisationUnregisteredTrust)))
        .map{
          case Left(e) =>
            logger.warn("Could not update session", e)
            errorHandler.errorResult()

          case Right(_) =>
            redirectToRegisterTrustPage
        }
    }
  }


  private def handleInsufficientConfidenceLevel(maybeNino: Option[NINO])(
    implicit request: RequestWithSessionDataAndRetrievedData[AnyContent]
  ): Future[Result] = maybeNino match {
    case None =>
      updateSession(sessionStore, request)(_.copy(subscriptionStatus = Some(SubscriptionStatus.IndividualWithInsufficientConfidenceLevel(None, None))))
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

  private def buildTrustSubscriptionData(trust: Trust)(
    implicit  hc: HeaderCarrier,
    request: RequestWithSessionDataAndRetrievedData[_]): Future[Result] = {
    val result =
      for{
        bprWithNameAndEmail <- bprService.getBusinessPartnerRecord(Left(trust))
          .subflatMap[Error,(BusinessPartnerRecord,TrustName,Email)]{ bpr =>
          (bpr.organisationName, bpr.emailAddress) match {
            case (None, None) =>
              Left(Error("Could not find trust name or email address from business partner record"))
            case (Some(_), None) =>
              Left(Error("Could not find trust name from business partner record"))
            case (None, Some(_)) =>
              Left(Error("Could not find email address from business partner record"))
            case (Some(n), Some(e)) =>
              Right((bpr, TrustName(n), Email(e)))
          }
        }
      subscriptionDetails = SubscriptionDetails(
        Left(bprWithNameAndEmail._2),
        bprWithNameAndEmail._3.value,
        bprWithNameAndEmail._1.address,
        bprWithNameAndEmail._1.sapNumber
      )
      _ <- EitherT(updateSession(sessionStore, request)(_.copy(subscriptionStatus = Some(SubscriptionReady(subscriptionDetails)))))
      } yield ()

    result.fold({ e =>
      logger.warn(s"Could not build subscription data for trust with SAUTR ${trust.sautr}", e)
      errorHandler.errorResult()
    },
      _ => SeeOther(routes.SubscriptionController.checkYourDetails().url)
    )
  }

  private def buildIndividualSubscriptionData(individual: Individual,
                                              maybeBpr: Option[BusinessPartnerRecord])(
    implicit
    hc: HeaderCarrier,
    request: RequestWithSessionDataAndRetrievedData[_]
  ): Future[Result] = {
    val result = for {
      bpr <- maybeBpr.fold(bprService.getBusinessPartnerRecord(Right(individual)))(EitherT.pure(_))
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
