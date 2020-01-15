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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers

import cats.data.EitherT
import cats.instances.future._
import cats.syntax.either._
import com.google.inject.{Inject, Singleton}
import play.api.Configuration
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.IvBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.SubscriptionStatus.SubscriptionMissingData
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.RetrievedUserType.{Individual, NonGovernmentGatewayRetrievedUser, Trust}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.AddressSource
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.{CgtReference, GGCredId, NINO, SAUTR}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.{ContactName, ContactNameSource, TrustName}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.SubscriptionDetails.MissingData
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.bpr.BusinessPartnerRecord
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.bpr.BusinessPartnerRecordRequest.{IndividualBusinessPartnerRecordRequest, TrustBusinessPartnerRecordRequest}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.email.{Email, EmailSource}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.{NeedMoreDetailsDetails, SubscriptionDetails}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.onboarding.{BusinessPartnerRecordService, OnboardingAuditService, SubscriptionService}
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
  val auditService: OnboardingAuditService,
  val config: Configuration,
  subscriptionService: SubscriptionService,
  weNeedMoreDetailsPage: views.html.onboarding.we_need_more_details,
  weOnlySupportGGPage: views.html.onboarding.we_only_support_gg,
  timedOutPage: views.html.timed_out
)(implicit viewConfig: ViewConfig, ec: ExecutionContext)
    extends FrontendController(cc)
    with WithAuthRetrievalsAndSessionDataAction
    with WithAuthAndSessionDataAction
    with Logging
    with SessionUpdates
    with IvBehaviour {

  def start(): Action[AnyContent] = authenticatedActionWithRetrievedDataAndSessionData.async { implicit request =>
    (
      request.authenticatedRequest.journeyUserType,
      request.sessionData.journeyStatus
    ) match {
      case (_, Some(journeyStatus))                  => handleSessionJourneyStatus(journeyStatus)
      case (retrievedUserType: RetrievedUserType, _) => handleRetrievedUserType(retrievedUserType)
    }
  }

  def weNeedMoreDetails(): Action[AnyContent] = authenticatedActionWithSessionData { implicit request =>
    request.sessionData.flatMap(_.needMoreDetailsDetails) match {
      case None          => Redirect(routes.StartController.start())
      case Some(details) => Ok(weNeedMoreDetailsPage(details))
    }
  }

  def weOnlySupportGG(): Action[AnyContent] = authenticatedActionWithSessionData { implicit request =>
    request.sessionData.flatMap(_.journeyStatus) match {
      case Some(NonGovernmentGatewayJourney) => Ok(weOnlySupportGGPage())
      case _                                 => Redirect(routes.StartController.start())
    }
  }

  def signOutAndRegisterForGG(): Action[AnyContent] = authenticatedActionWithSessionData { implicit request =>
    request.sessionData.flatMap(_.journeyStatus) match {
      case Some(NonGovernmentGatewayJourney) => Redirect(viewConfig.ggCreateAccountUrl).withNewSession
      case _                                 => Redirect(routes.StartController.start())
    }
  }

  def signOutAndSignIn(): Action[AnyContent] = authenticatedActionWithSessionData { implicit request =>
    request.sessionData.flatMap(_.journeyStatus) match {
      case Some(NonGovernmentGatewayJourney) => Redirect(routes.StartController.start()).withNewSession
      case _                                 => Redirect(routes.StartController.start())
    }
  }

  def keepAlive(): Action[AnyContent] = authenticatedActionWithSessionData { implicit request =>
    Ok("")
  }

  def timedOut(): Action[AnyContent] = Action { implicit request =>
    Ok(timedOutPage())
  }


  private def handleSessionJourneyStatus(journeyStatus: JourneyStatus)(implicit request: RequestWithSessionDataAndRetrievedData[AnyContent]): Future[Result] = journeyStatus match {
    case _: Subscribed =>
      Redirect(controllers.accounts.homepage.routes.HomePageController.homepage())

    case AlreadySubscribedWithDifferentGGAccount(_) =>
      Redirect(onboarding.routes.SubscriptionController.alreadySubscribedWithDifferentGGAccount())

    case _: SubscriptionStatus.SubscriptionReady =>
      Redirect(onboarding.routes.SubscriptionController.checkYourDetails())

    case i: SubscriptionStatus.TryingToGetIndividualsFootprint =>
      // this is not the first time a person with individual insufficient confidence level has come to start
      Redirect(onboarding.routes.InsufficientConfidenceLevelController.doYouHaveNINO())

    case _: RegistrationStatus.RegistrationReady =>
      Redirect(onboarding.routes.RegistrationController.checkYourAnswers())

    case _: RegistrationStatus.IndividualSupplyingInformation =>
      Redirect(onboarding.routes.RegistrationController.selectEntityType())

    case _: RegistrationStatus.IndividualMissingEmail =>
      Redirect(onboarding.email.routes.RegistrationEnterEmailController.enterEmail())

    case RegistrationStatus.IndividualWantsToRegisterTrust(_) =>
      Redirect(onboarding.routes.RegistrationController.selectEntityType())

    case SubscriptionStatus.DeterminingIfOrganisationIsTrust(ggCredId, ggEmail, _, _) =>
      handleNonTrustOrganisation(ggCredId, ggEmail)

    case NonGovernmentGatewayJourney =>
      Redirect(routes.StartController.weOnlySupportGG())

    case s: SubscriptionStatus.SubscriptionMissingData =>
      handleSubscriptionMissingData(s)

    case _: AgentStatus.AgentSupplyingClientDetails =>
      Redirect(agents.routes.AgentAccessController.enterClientsCgtRef())
  }


  private def handleRetrievedUserType(retrievedUserType: RetrievedUserType)(implicit request: RequestWithSessionDataAndRetrievedData[AnyContent]): Future[Result] = retrievedUserType match {
    case RetrievedUserType.Agent(ggCredId) =>
      updateSession(sessionStore, request)(_.copy(
        journeyStatus = Some(AgentStatus.AgentSupplyingClientDetails(ggCredId, None)),
        userType = Some(UserType.Agent)
      )).map{
        case Left(e) =>
          logger.warn("Could not update session", e)
          errorHandler.errorResult(Some(UserType.Agent))

        case Right(_) =>
          Redirect(agents.routes.AgentAccessController.enterClientsCgtRef())
      }

    case RetrievedUserType.Subscribed(cgtReference, ggCredId) =>
      handleSubscribedUser(cgtReference, ggCredId)

    case RetrievedUserType.IndividualWithInsufficientConfidenceLevel(maybeNino, maybeSautr, ggEmail, ggCredId) =>
      // this is the first time a person with individual insufficient confidence level has come to start
      handleInsufficientConfidenceLevel(maybeNino, maybeSautr, ggEmail, ggCredId)

    case i: RetrievedUserType.Individual =>
      buildIndividualSubscriptionData(i, i.email, i.ggCredId)

    case t: RetrievedUserType.Trust =>
      buildTrustSubscriptionData(t, t.email, t.ggCredId)

    case RetrievedUserType.OrganisationUnregisteredTrust(_, ggCredId) =>
      handleNonTrustOrganisation(ggCredId, None)

    case u: RetrievedUserType.NonGovernmentGatewayRetrievedUser =>
      handleNonGovernmentGatewayUser(u)
  }



  private def handleNonGovernmentGatewayUser(
    nonGovernmentGatewayUser: NonGovernmentGatewayRetrievedUser
  )(implicit request: RequestWithSessionDataAndRetrievedData[_]): Future[Result] = {
    logger.warn(s"User logged in with unsupported provider: ${nonGovernmentGatewayUser.authProvider}")

    updateSession(sessionStore, request)(
      _.copy(userType = request.authenticatedRequest.userType, journeyStatus = Some(NonGovernmentGatewayJourney))
    ).map {
      case Left(e) =>
        logger.warn("Could not update session", e)
        errorHandler.errorResult(request.authenticatedRequest.userType)

      case Right(_) =>
        Redirect(routes.StartController.weOnlySupportGG())
    }

  }

  private def handleSubscribedUser(cgtReference: CgtReference, ggCredId: GGCredId)(
    implicit request: RequestWithSessionDataAndRetrievedData[_]
  ): Future[Result] = {
    val result = for {
      subscribedDetails <- subscriptionService.getSubscribedDetails(cgtReference)
      _ <- EitherT(
            updateSession(sessionStore, request)(
              _.copy(
                userType      = request.authenticatedRequest.userType,
                journeyStatus = Some(Subscribed(subscribedDetails, ggCredId, None))
              )
            )
          )
    } yield ()

    result.fold(
      { e =>
        logger.warn("Could not get subscribed details", e)
        errorHandler.errorResult(request.authenticatedRequest.userType)
      },
      _ => Redirect(controllers.accounts.homepage.routes.HomePageController.homepage())
    )
  }

  private def handleNonTrustOrganisation(ggCredId: GGCredId, ggEmail: Option[Email])(
    implicit request: RequestWithSessionDataAndRetrievedData[_]
  ): Future[Result] = {
    val newSessionData =
      request.sessionData.journeyStatus match {
        case Some(d: SubscriptionStatus.DeterminingIfOrganisationIsTrust) => d
        case _                                                            => SubscriptionStatus.DeterminingIfOrganisationIsTrust(ggCredId, ggEmail, None, None)
      }

    updateSession(sessionStore, request)(
      _.copy(
        userType      = request.authenticatedRequest.userType,
        journeyStatus = Some(newSessionData),
        needMoreDetailsDetails = Some(
          NeedMoreDetailsDetails(
            onboarding.routes.DeterminingIfOrganisationIsTrustController.doYouWantToReportForATrust().url,
            NeedMoreDetailsDetails.AffinityGroup.Organisation
          )
        )
      )
    ).map {
      case Left(e) =>
        logger.warn("Could not update session", e)
        errorHandler.errorResult(request.authenticatedRequest.userType)

      case Right(_) =>
        Redirect(routes.StartController.weNeedMoreDetails())
    }
  }

  private def handleInsufficientConfidenceLevel(
    maybeNino: Option[NINO],
    maybeSautr: Option[SAUTR],
    ggEmail: Option[Email],
    ggCredId: GGCredId
  )(
    implicit request: RequestWithSessionDataAndRetrievedData[AnyContent]
  ): Future[Result] =
    maybeNino match {
      case None =>
        maybeSautr match {
          case Some(sautr) =>
            buildIndividualSubscriptionData(Individual(Left(sautr), ggEmail, ggCredId), ggEmail, ggCredId)

          case None =>
            val subscriptionStatus =
              SubscriptionStatus.TryingToGetIndividualsFootprint(None, None, ggEmail, ggCredId)

            updateSession(sessionStore, request)(
              s =>
                s.copy(
                  userType      = request.authenticatedRequest.userType,
                  journeyStatus = Some(subscriptionStatus),
                  needMoreDetailsDetails = Some(
                    NeedMoreDetailsDetails(
                      onboarding.routes.InsufficientConfidenceLevelController.doYouHaveNINO().url,
                      NeedMoreDetailsDetails.AffinityGroup.Individual
                    )
                  )
                )
            ).map {
              case Left(e) =>
                logger.warn("Could not update session with insufficient confidence level", e)
                errorHandler.errorResult(request.authenticatedRequest.userType)

              case Right(_) =>
                Redirect(routes.StartController.weNeedMoreDetails())
            }
        }

      case Some(_) =>
        auditService.sendHandOffToIvEvent(ggCredId, request.uri)
        redirectToIv
    }

  private def buildTrustSubscriptionData(
    trust: Trust,
    ggEmail: Option[Email],
    ggCredId: GGCredId
  )(implicit hc: HeaderCarrier, request: RequestWithSessionDataAndRetrievedData[_]): Future[Result] = {
    val result =
      for {
        bprResponse <- bprService.getBusinessPartnerRecord(TrustBusinessPartnerRecordRequest(Right(trust.sautr), None))

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
        maybeSubscriptionDetails <- {
          EitherT.pure(
            bprWithTrustName._1.emailAddress
              .map(_ -> EmailSource.BusinessPartnerRecord)
              .orElse(trust.email.map(_ -> EmailSource.GovernmentGateway))
              .fold[Either[MissingData.Email.type, SubscriptionDetails]](
                Left(SubscriptionDetails.MissingData.Email)
              ) { emailWithSource =>
                {
                  Right(
                    SubscriptionDetails(
                      Left(bprWithTrustName._2),
                      emailWithSource._1,
                      bprWithTrustName._1.address,
                      ContactName(bprWithTrustName._2.value),
                      bprWithTrustName._1.sapNumber,
                      emailWithSource._2,
                      AddressSource.BusinessPartnerRecord,
                      ContactNameSource.DerivedFromBusinessPartnerRecord
                    )
                  )
                }
              }
          )
        }
        _ <- EitherT(
              maybeSubscriptionDetails.fold(
                _ =>
                  updateSession(sessionStore, request)(
                    _.copy(
                      userType = request.authenticatedRequest.userType,
                      journeyStatus =
                        Some(SubscriptionStatus.SubscriptionMissingData(bprWithTrustName._1, None, ggCredId, ggEmail)),
                      needMoreDetailsDetails = Some(
                        NeedMoreDetailsDetails(
                          onboarding.email.routes.SubscriptionEnterEmailController.enterEmail().url,
                          NeedMoreDetailsDetails.AffinityGroup.Organisation
                        )
                      )
                    )
                  ),
                subscriptionDetails =>
                  updateSession(sessionStore, request)(
                    _.copy(
                      journeyStatus = Some(SubscriptionStatus.SubscriptionReady(subscriptionDetails, ggCredId))
                    )
                  )
              )
            )
      } yield maybeSubscriptionDetails

    result.fold(
      { e =>
        logger.warn(s"Could not build subscription data for trust with SAUTR ${trust.sautr}", e)
        errorHandler.errorResult(request.authenticatedRequest.userType)
      }, {
        case Left(MissingData.Email) => Redirect(routes.StartController.weNeedMoreDetails())
        case Right(_)                => Redirect(onboarding.routes.SubscriptionController.checkYourDetails())
      }
    )
  }

  private def handleSubscriptionMissingData(s: SubscriptionMissingData)(
    implicit request: RequestWithSessionDataAndRetrievedData[_]
  ): Future[Result] =
    SubscriptionDetails(s.businessPartnerRecord, s.ggEmail, s.manuallyEnteredEmail).fold(
      { missingData =>
        logger.info(s"Could not find the following data for subscription details: ${missingData.toList.mkString(",")}")
        missingData.head match {
          case MissingData.Email => Redirect(onboarding.email.routes.SubscriptionEnterEmailController.enterEmail())
        }
      },
      subscriptionDetails =>
        updateSession(sessionStore, request)(
          _.copy(
            userType      = request.authenticatedRequest.userType,
            journeyStatus = Some(SubscriptionStatus.SubscriptionReady(subscriptionDetails, s.ggCredId))
          )
        ).map { _ =>
          Redirect(onboarding.routes.SubscriptionController.checkYourDetails())
        }
    )

  private def buildIndividualSubscriptionData(individual: Individual, ggEmail: Option[Email], ggCredId: GGCredId)(
    implicit
    hc: HeaderCarrier,
    request: RequestWithSessionDataAndRetrievedData[_]
  ): Future[Result] = {
    val result = for {
      bprResponse <- bprService.getBusinessPartnerRecord(IndividualBusinessPartnerRecordRequest(individual.id, None))
      bpr <- EitherT.fromEither[Future](
              Either.fromOption(bprResponse.businessPartnerRecord, Error("Could not find BPR for individual"))
            )
      maybeSubscriptionDetails <- EitherT.pure(SubscriptionDetails(bpr, individual.email, None))
      _ <- EitherT(
            maybeSubscriptionDetails.fold[Future[Either[Error, Unit]]](
              _ =>
                updateSession(sessionStore, request)(
                  _.copy(
                    userType      = request.authenticatedRequest.userType,
                    journeyStatus = Some(SubscriptionStatus.SubscriptionMissingData(bpr, None, ggCredId, ggEmail)),
                    needMoreDetailsDetails = Some(
                      NeedMoreDetailsDetails(
                        onboarding.email.routes.SubscriptionEnterEmailController.enterEmail().url,
                        NeedMoreDetailsDetails.AffinityGroup.Individual
                      )
                    )
                  )
                ),
              subscriptionDetails =>
                updateSession(sessionStore, request)(
                  _.copy(
                    userType      = request.authenticatedRequest.userType,
                    journeyStatus = Some(SubscriptionStatus.SubscriptionReady(subscriptionDetails, ggCredId))
                  )
                )
            )
          )
    } yield maybeSubscriptionDetails

    result.fold(
      { e =>
        logger.warn("Error while getting subscription details", e)
        errorHandler.errorResult(request.authenticatedRequest.userType)
      }, {
        case Left(missingData) =>
          logger.info(
            s"Could not find the following data for subscription details: ${missingData.toList.mkString(",")}"
          )
          missingData.head match {
            case MissingData.Email => Redirect(routes.StartController.weNeedMoreDetails())
          }

        case Right(_) =>
          Redirect(onboarding.routes.SubscriptionController.checkYourDetails())
      }
    )
  }

}
