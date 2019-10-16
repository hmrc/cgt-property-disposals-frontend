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
import com.google.inject.{Inject, Singleton}
import play.api.Configuration
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{RegistrationStatus, Subscribed, SubscriptionStatus}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.SubscriptionDetails.MissingData
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.UserType._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address.UkAddress
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.bpr.BusinessPartnerRecord
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.bpr.BusinessPartnerRecordRequest.{IndividualBusinessPartnerRecordRequest, TrustBusinessPartnerRecordRequest}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.{GGCredId, NINO, SAUTR}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.{ContactName, IndividualName, TrustName}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.BusinessPartnerRecordService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class StartController @Inject()(
  bprService: BusinessPartnerRecordService,
  val sessionStore: SessionStore,
  val errorHandler: ErrorHandler,
  cc: MessagesControllerComponents,
  val authenticatedActionWithRetrievedData: AuthenticatedActionWithRetrievedData,
  val sessionDataActionWithRetrievedData: SessionDataActionWithRetrievedData,
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val config: Configuration,
  weNeedMoreDetailsPage: views.html.we_need_more_details
)(implicit viewConfig: ViewConfig, ec: ExecutionContext)
    extends FrontendController(cc)
    with WithAuthRetrievalsAndSessionDataAction
    with WithAuthAndSessionDataAction
    with Logging
    with SessionUpdates
    with IvBehaviour {

  def start(): Action[AnyContent] = authenticatedActionWithRetrievedDataAndSessionData.async { implicit request =>
    (request.authenticatedRequest.userType, request.sessionData.flatMap(_.journeyStatus)) match {

      case (UserType.Subscribed(cgtReference, gGCredId), None) =>
        // TODO: take this out when the Display API is integrated
        val dummyAccountDetails = AccountDetails(
          Right(IndividualName("Joe", "Bloggs")),
          Email("joe.bloggs@gmail.com"),
          UkAddress("line1", None, None, None, "KO11OK"),
          ContactName("Joe Bloggs"),
          cgtReference
        )
        handleSubscribedUser(dummyAccountDetails)

      case (_, Some(_: Subscribed)) =>
        Redirect(routes.HomeController.start())

      case (_, Some(_: SubscriptionStatus.SubscriptionReady)) =>
        Redirect(routes.SubscriptionController.checkYourDetails())

      case (_, Some(i: SubscriptionStatus.TryingToGetIndividualsFootprint)) =>
        // this is not the first time a person with individual insufficient confidence level has come to start
        Redirect(routes.InsufficientConfidenceLevelController.doYouHaveNINO())

      case (_, Some(_: RegistrationStatus.RegistrationReady)) =>
        Redirect(routes.RegistrationController.checkYourAnswers())

      case (_, Some(_: RegistrationStatus.IndividualSupplyingInformation)) =>
        Redirect(routes.RegistrationController.selectEntityType())

      case (_, Some(_: RegistrationStatus.IndividualMissingEmail)) =>
        Redirect(email.routes.RegistrationEnterEmailController.enterEmail())

      case (_, Some(RegistrationStatus.IndividualWantsToRegisterTrust)) =>
        Redirect(routes.RegistrationController.selectEntityType())

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

  def weNeedMoreDetails(): Action[AnyContent] = authenticatedActionWithSessionData { implicit request =>
    request.sessionData.flatMap(_.needMoreDetailsContinueUrl) match {
      case None              => Redirect(routes.StartController.start())
      case Some(continueUrl) => Ok(weNeedMoreDetailsPage(continueUrl))
    }
  }

  private def handleSubscribedUser(accountDetails: AccountDetails)(
    implicit request: RequestWithSessionDataAndRetrievedData[_]
  ): Future[Result] = {
    lazy val redirectToHomePage =
      Redirect(routes.HomeController.start())

    if (request.sessionData.flatMap(_.journeyStatus).contains(Subscribed(accountDetails))) {
      redirectToHomePage
    } else {
      updateSession(sessionStore, request)(_.copy(journeyStatus = Some(Subscribed(accountDetails))))
        .map {
          case Left(e) =>
            logger.warn("Could not update session", e)
            errorHandler.errorResult()

          case Right(_) =>
            redirectToHomePage
        }
    }
  }

  private def handleNonTrustOrganisation()(
    implicit request: RequestWithSessionDataAndRetrievedData[_]
  ): Future[Result] = {
    lazy val redirectToRegisterTrustPage =
      Redirect(routes.RegisterTrustController.registerYourTrust())

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
            SubscriptionStatus.TryingToGetIndividualsFootprint(None, None, maybeEmail, ggCredId)
          updateSession(sessionStore, request)(
            _.copy(
              journeyStatus              = Some(subscriptionStatus),
              needMoreDetailsContinueUrl = Some(routes.InsufficientConfidenceLevelController.doYouHaveNINO().url)
            )
          ).map {
            case Left(e) =>
              logger.warn("Could not update session with insufficient confidence level", e)
              errorHandler.errorResult()

            case Right(_) =>
              Redirect(routes.StartController.weNeedMoreDetails())
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
                             Either
                               .fromOption(bprResponse.businessPartnerRecord, Error("Could not find BPR for trust"))
                               .flatMap(
                                 bpr =>
                                   bpr.name.fold[Either[Error, (BusinessPartnerRecord, TrustName)]](
                                     trustName => Right((bpr, trustName)),
                                     _ =>
                                       Left(
                                         Error(
                                           "Found individual name but expected trust name in business partner record"
                                         )
                                       )
                                   )
                               )
                           )
        maybeSubscriptionDetails <- EitherT.pure(
                                     bprWithTrustName._1.emailAddress
                                       .orElse(trust.email)
                                       .fold[Either[MissingData.Email.type, SubscriptionDetails]](
                                         Left(SubscriptionDetails.MissingData.Email)
                                       ) { email =>
                                         Right(
                                           SubscriptionDetails(
                                             Left(bprWithTrustName._2),
                                             email,
                                             bprWithTrustName._1.address,
                                             ContactName(bprWithTrustName._2.value),
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
                      journeyStatus              = Some(SubscriptionStatus.SubscriptionMissingData(bprWithTrustName._1)),
                      needMoreDetailsContinueUrl = Some(email.routes.SubscriptionEnterEmailController.enterEmail().url)
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
        case Left(MissingData.Email) => Redirect(routes.StartController.weNeedMoreDetails())
        case Right(_)                => Redirect(routes.SubscriptionController.checkYourDetails())
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
          case MissingData.Email => Redirect(email.routes.SubscriptionEnterEmailController.enterEmail())
        }
      },
      subscriptionDetails =>
        updateSession(sessionStore, request)(
          _.copy(journeyStatus = Some(SubscriptionStatus.SubscriptionReady(subscriptionDetails)))
        ).map { _ =>
          Redirect(routes.SubscriptionController.checkYourDetails())
        }
    )

  private def buildIndividualSubscriptionData(individual: Individual)(
    implicit
    hc: HeaderCarrier,
    request: RequestWithSessionDataAndRetrievedData[_]
  ): Future[Result] = {
    val result = for {
      bprResponse <- bprService.getBusinessPartnerRecord(IndividualBusinessPartnerRecordRequest(individual.id, None))
      bpr <- EitherT.fromEither[Future](
              Either.fromOption(bprResponse.businessPartnerRecord, Error("Could not find BPR for individual"))
            )
      maybeSubscriptionDetails <- EitherT.pure(SubscriptionDetails(bpr, individual.email))
      _ <- EitherT(
            maybeSubscriptionDetails.fold[Future[Either[Error, Unit]]](
              _ =>
                updateSession(sessionStore, request)(
                  _.copy(
                    journeyStatus              = Some(SubscriptionStatus.SubscriptionMissingData(bpr)),
                    needMoreDetailsContinueUrl = Some(email.routes.SubscriptionEnterEmailController.enterEmail().url)
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
        logger.warn("Error while getting subscription details", e)
        errorHandler.errorResult()
      }, {
        case Left(missingData) =>
          logger.info(
            s"Could not find the following data for subscription details: ${missingData.toList.mkString(",")}"
          )
          missingData.head match {
            case MissingData.Email => Redirect(routes.StartController.weNeedMoreDetails())
          }

        case Right(_) =>
          Redirect(routes.SubscriptionController.checkYourDetails())
      }
    )
  }

}
