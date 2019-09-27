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
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.ErrorHandler
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedActionWithRetrievedData, RequestWithSessionDataAndRetrievedData, SessionDataActionWithRetrievedData, WithAuthRetrievalsAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.bpr.BusinessPartnerRecordRequest.{IndividualBusinessPartnerRecordRequest, TrustBusinessPartnerRecordRequest}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{RegistrationStatus, SubscriptionStatus}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.SubscriptionDetails.MissingData
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.UserType._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.bpr.BusinessPartnerRecord
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.{GGCredId, NINO, SAUTR}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.BusinessPartnerRecordService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.toFuture
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

    (request.authenticatedRequest.userType, request.sessionData.flatMap(_.journeyStatus)) match {

      case (_, Some(_: SubscriptionStatus.SubscriptionReady)) =>
        SeeOther(routes.SubscriptionController.checkYourDetails().url)

      case (_, Some(_: SubscriptionStatus.SubscriptionComplete)) =>
        SeeOther(routes.SubscriptionController.subscribed().url)

      case (_, Some(i: SubscriptionStatus.IndividualWithInsufficientConfidenceLevel)) =>
        // this is not the first time a person with individual insufficient confidence level has come to start
        SeeOther(routes.InsufficientConfidenceLevelController.doYouHaveNINO().url)

      case (_, Some(_: RegistrationStatus.RegistrationReady)) =>
        SeeOther(routes.RegistrationController.checkYourAnswers().url)

      case (_, Some(_: RegistrationStatus.IndividualSupplyingInformation)) =>
        SeeOther(routes.RegistrationController.startRegistration().url)

      case (_, Some(RegistrationStatus.IndividualWantsToRegisterTrust)) =>
        SeeOther(routes.RegistrationController.startRegistration().url)

      case (UserType.IndividualWithInsufficientConfidenceLevel(maybeNino, maybeSautr, maybeEmail, ggCredId), None) =>
        // this is the first time a person with individual insufficient confidence level has come to start
        handleInsufficientConfidenceLevel(maybeNino, maybeSautr, maybeEmail, ggCredId)

      case (i: UserType.Individual, Some(SubscriptionStatus.SubscriptionMissingData(bpr))) =>
        handleSubscriptionMissingData(bpr, i.email)

      case (
          i: UserType.IndividualWithInsufficientConfidenceLevel,
          Some(SubscriptionStatus.SubscriptionMissingData(bpr))
          ) =>
        handleSubscriptionMissingData(bpr, i.email)

      case (t: UserType.Trust, Some(SubscriptionStatus.SubscriptionMissingData(bpr))) =>
        handleSubscriptionMissingData(bpr, t.email)

      case (i: UserType.Individual, None) =>
        buildIndividualSubscriptionData(i)

      case (t: UserType.Trust, _) =>
        buildTrustSubscriptionData(t)

      case (UserType.OrganisationUnregisteredTrust, _) | (_, Some(SubscriptionStatus.UnregisteredTrust)) =>
        handleNonTrustOrganisation()
    }
  }

  private def handleNonTrustOrganisation()(
    implicit request: RequestWithSessionDataAndRetrievedData[_]
  ): Future[Result] = {
    lazy val redirectToRegisterTrustPage =
      SeeOther(routes.RegisterTrustController.registerYourTrust().url)

    if (request.sessionData.flatMap(_.journeyStatus).contains(SubscriptionStatus.UnregisteredTrust)) {
      redirectToRegisterTrustPage
    } else {
      updateSession(sessionStore, request)(_.copy(journeyStatus = Some(SubscriptionStatus.UnregisteredTrust)))
        .map {
          case Left(e) =>
            logger.warn("Could not update session", e)
            errorHandler.errorResult()

          case Right(_) =>
            redirectToRegisterTrustPage
        }
    }
  }

  private def handleInsufficientConfidenceLevel(
    maybeNino: Option[NINO],
    maybeSautr: Option[SAUTR],
    maybeEmail: Option[Email],
    ggCredId: GGCredId
  )(
    implicit request: RequestWithSessionDataAndRetrievedData[AnyContent]
  ): Future[Result] = maybeNino match {
    case None =>
      maybeSautr match {
        case Some(sautr) =>
          buildIndividualSubscriptionData(Individual(Left(sautr), maybeEmail))

        case None =>
          val subscriptionStatus =
            SubscriptionStatus.IndividualWithInsufficientConfidenceLevel(None, None, maybeEmail, ggCredId)
          updateSession(sessionStore, request)(_.copy(journeyStatus = Some(subscriptionStatus)))
            .map {
              case Left(e) =>
                logger.warn("Could not update session with insufficient confidence level", e)
                errorHandler.errorResult()

              case Right(_) =>
                SeeOther(routes.InsufficientConfidenceLevelController.doYouHaveNINO().url)
            }
      }

    case Some(_) =>
      redirectToIv
  }

  private def buildTrustSubscriptionData(
    trust: Trust
  )(implicit hc: HeaderCarrier, request: RequestWithSessionDataAndRetrievedData[_]): Future[Result] = {
    val result =
      for {
        bprResponse <- bprService.getBusinessPartnerRecord(TrustBusinessPartnerRecordRequest(trust.sautr))

        bprWithTrustName <- EitherT.fromEither[Future](
          Either.fromOption(bprResponse.businessPartnerRecord, Error("Could not find BPR for trust"))
            .flatMap(bpr =>
              bpr.name.fold[Either[Error,(BusinessPartnerRecord,TrustName)]](
                trustName => Right((bpr, trustName)),
                _ => Left(Error("Found individual name but expected trust name in business partner record"))
          )))
        maybeSubscriptionDetails <- EitherT.pure(
                                     bprWithTrustName._1.emailAddress
                                       .orElse(trust.email.map(_.value))
                                       .fold[Either[MissingData.Email.type, SubscriptionDetails]](
                                         Left(SubscriptionDetails.MissingData.Email)
                                       ) { email =>
                                         Right(
                                           SubscriptionDetails(
                                             Left(bprWithTrustName._2),
                                             email,
                                             bprWithTrustName._1.address,
                                             bprWithTrustName._1.sapNumber
                                           )
                                         )
                                       }
                                   )
        _ <- EitherT(
              maybeSubscriptionDetails.fold(
                _ =>
                  updateSession(sessionStore, request)(
                    _.copy(
                      journeyStatus =
                        Some(SubscriptionStatus.SubscriptionMissingData(bprWithTrustName._1))
                    )
                  ),
                subscriptionDetails =>
                  updateSession(sessionStore, request)(
                    _.copy(journeyStatus = Some(SubscriptionStatus.SubscriptionReady(subscriptionDetails)))
                  )
              )
            )
      } yield maybeSubscriptionDetails

    result.fold(
      { e =>
        logger.warn(s"Could not build subscription data for trust with SAUTR ${trust.sautr}", e)
        errorHandler.errorResult()
      }, {
        case Left(MissingData.Email) => SeeOther(routes.EmailController.enterEmail().url)
        case Right(_)                => SeeOther(routes.SubscriptionController.checkYourDetails().url)
      }
    )
  }

  private def handleSubscriptionMissingData(bpr: BusinessPartnerRecord, retrievedEmail: Option[Email])(
    implicit request: RequestWithSessionDataAndRetrievedData[_]
  ): Future[Result] =
    SubscriptionDetails(bpr, retrievedEmail).fold(
      { missingData =>
        logger.info(s"Could not find the following data for subscription details: ${missingData.toList.mkString(",")}")
        missingData.head match {
          case MissingData.Email => SeeOther(routes.EmailController.enterEmail().url)
        }
      },
      subscriptionDetails =>
        updateSession(sessionStore, request)(
          _.copy(journeyStatus = Some(SubscriptionStatus.SubscriptionReady(subscriptionDetails)))
        ).map { _ =>
          SeeOther(routes.SubscriptionController.checkYourDetails().url)
        }
    )

  private def buildIndividualSubscriptionData(individual: Individual)(
    implicit
    hc: HeaderCarrier,
    request: RequestWithSessionDataAndRetrievedData[_]
  ): Future[Result] = {
    val result = for {
      bprResponse <- bprService.getBusinessPartnerRecord(IndividualBusinessPartnerRecordRequest(individual.id, None))
      bpr <- EitherT.fromEither[Future](Either.fromOption(bprResponse.businessPartnerRecord, Error("Could not find BPR for individual")))
      maybeSubscriptionDetails <- EitherT.pure(SubscriptionDetails(bpr, individual.email))
      _ <- EitherT(
            maybeSubscriptionDetails.fold[Future[Either[Error, Unit]]](
              _ =>
                updateSession(sessionStore, request)(
                  _.copy(journeyStatus = Some(SubscriptionStatus.SubscriptionMissingData(bpr)))
                ),
              subscriptionDetails =>
                updateSession(sessionStore, request)(
                  _.copy(journeyStatus = Some(SubscriptionStatus.SubscriptionReady(subscriptionDetails)))
                )
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
            s"Could not find the following data for subscription details: ${missingData.toList.mkString(",")}"
          )
          missingData.head match {
            case MissingData.Email     => SeeOther(routes.EmailController.enterEmail().url)
          }

        case Right(_) =>
          SeeOther(routes.SubscriptionController.checkYourDetails().url)
      }
    )
  }

}
