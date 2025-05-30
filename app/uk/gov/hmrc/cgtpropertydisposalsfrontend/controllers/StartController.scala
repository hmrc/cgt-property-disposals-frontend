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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers

import cats.data.EitherT
import cats.instances.future._
import cats.syntax.either._
import com.google.inject.{Inject, Singleton}
import play.api.Configuration
import play.api.mvc._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.StartController.BuildSubscriptionDataError
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.agents.{routes => agentsRoutes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.address.{routes => addressRoutes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.email.{routes => emailRoutes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.{IvBehaviour, routes => onboardingRoutes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.SubscriptionStatus.SubscriptionMissingData
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{Registering, SubmittingReturn, _}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.RetrievedUserType.{Individual, NonGovernmentGatewayRetrievedUser, Trust}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.AddressSource
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.email.{Email, EmailSource}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.{CgtReference, GGCredId, NINO, SAUTR}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.{ContactName, ContactNameSource, TrustName}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.NeedMoreDetailsDetails.AffinityGroup
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.SubscriptionDetails.MissingData
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.audit.{HandOffTIvEvent, WrongGGAccountEvent}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.bpr.BusinessPartnerRecord
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.bpr.BusinessPartnerRecordRequest.{IndividualBusinessPartnerRecordRequest, TrustBusinessPartnerRecordRequest}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.{NeedMoreDetailsDetails, SubscribedDetails, SubscriptionDetails}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.AuditService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.onboarding.{BusinessPartnerRecordService, SubscriptionService}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, given}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.{controllers, views}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class StartController @Inject() (
  bprService: BusinessPartnerRecordService,
  val sessionStore: SessionStore,
  val errorHandler: ErrorHandler,
  cc: MessagesControllerComponents,
  val authenticatedActionWithRetrievedData: AuthenticatedActionWithRetrievedData,
  val sessionDataActionWithRetrievedData: SessionDataActionWithRetrievedData,
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val auditService: AuditService,
  returnsService: ReturnsService,
  val config: Configuration,
  subscriptionService: SubscriptionService,
  weNeedMoreDetailsPage: views.html.onboarding.we_need_more_details,
  weOnlySupportGGPage: views.html.onboarding.we_only_support_gg,
  timedOutPage: views.html.timed_out,
  agentNoEnrolmentPage: views.html.agent_no_enrolment
)(implicit viewConfig: ViewConfig, ec: ExecutionContext)
    extends FrontendController(cc)
    with WithAuthRetrievalsAndSessionDataAction
    with WithAuthAndSessionDataAction
    with Logging
    with SessionUpdates
    with IvBehaviour {

  def start(): Action[AnyContent] =
    authenticatedActionWithRetrievedDataAndSessionData.async { implicit request =>
      (
        request.authenticatedRequest.journeyUserType,
        request.sessionData.journeyStatus
      ) match {
        // agents coming through start will be handled in the same way
        case (agent: RetrievedUserType.Agent, _) =>
          handleRetrievedUserType(agent)
        case (_, Some(journeyStatus))            =>
          handleSessionJourneyStatus(journeyStatus)
        case (retrievedUserType, _)              =>
          handleRetrievedUserType(retrievedUserType)
      }
    }

  def weNeedMoreDetails(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      request.sessionData.flatMap(_.needMoreDetailsDetails) match {
        case None          => Future.successful(Redirect(routes.StartController.start()))
        case Some(details) =>
          updateSession(sessionStore, request.toSession)(
            _.copy(journeyType = Some(OnBoarding))
          ).map {
            case Left(e) =>
              logger.warn("Could not update session", e)
              errorHandler.errorResult(Some(UserType.Agent))

            case Right(_) =>
              Ok(weNeedMoreDetailsPage(details))
          }
      }
    }

  def weOnlySupportGG(): Action[AnyContent] =
    authenticatedActionWithSessionData { implicit request =>
      request.sessionData.flatMap(_.journeyStatus) match {
        case Some(NonGovernmentGatewayJourney) => Ok(weOnlySupportGGPage())
        case _                                 => Redirect(routes.StartController.start())
      }
    }

  def signOutAndRegisterForGG(): Action[AnyContent] =
    authenticatedActionWithSessionData { implicit request =>
      request.sessionData.flatMap(_.journeyStatus) match {
        case Some(NonGovernmentGatewayJourney) =>
          Redirect(viewConfig.ggCreateAccountUrl).withNewSession
        case _                                 => Redirect(routes.StartController.start())
      }
    }

  def signOutAndSignIn(): Action[AnyContent] =
    authenticatedActionWithSessionData { implicit request =>
      request.sessionData.flatMap(_.journeyStatus) match {
        case Some(NonGovernmentGatewayJourney | AgentWithoutAgentEnrolment) =>
          Redirect(routes.StartController.start()).withNewSession
        case _                                                              => Redirect(routes.StartController.start())
      }
    }

  def signOutAndRedirectToFeedback(): Action[AnyContent] =
    authenticatedActionWithSessionData { implicit request =>
      val feedbackUrl = request.sessionData.flatMap(_.journeyType) match {
        case Some(OnBoarding) => viewConfig.onboardingExitSurveyUrl
        case Some(Returns)    => viewConfig.returnsExitSurveyUrl
        case Some(Amend)      => viewConfig.amendsExitSurveyUrl
        case _                => viewConfig.generalExistSurveyUrl
      }

      Redirect(s"${viewConfig.signOutUrl}?continue=$feedbackUrl")
    }

  def keepAlive(): Action[AnyContent] =
    authenticatedActionWithSessionData(_ => Ok(""))

  def timedOut(): Action[AnyContent] =
    Action(implicit request => Ok(timedOutPage()))

  def agentNoEnrolment(): Action[AnyContent] =
    authenticatedActionWithSessionData { implicit request =>
      request.sessionData.flatMap(_.journeyStatus) match {
        case Some(AgentWithoutAgentEnrolment) => Ok(agentNoEnrolmentPage())
        case _                                => Redirect(routes.StartController.start())
      }
    }

  private def handleSessionJourneyStatus(
    journeyStatus: JourneyStatus
  )(implicit
    request: RequestWithSessionDataAndRetrievedData[AnyContent]
  ): Future[Result] =
    journeyStatus match {
      case _: Subscribed | Registering =>
        Redirect(
          controllers.accounts.homepage.routes.HomePageController.homepage()
        )

      case AgentWithoutAgentEnrolment =>
        Redirect(routes.StartController.agentNoEnrolment())

      case AlreadySubscribedWithDifferentGGAccount(_, _) =>
        Redirect(onboardingRoutes.SubscriptionController.alreadySubscribedWithDifferentGGAccount())

      case _: SubscriptionStatus.SubscriptionReady =>
        Redirect(onboardingRoutes.SubscriptionController.checkYourDetails())

      case _: SubscriptionStatus.TryingToGetIndividualsFootprint =>
        // this is not the first time a person with individual insufficient confidence level has come to start
        Redirect(onboardingRoutes.InsufficientConfidenceLevelController.doYouHaveNINO())

      case _: RegistrationStatus.RegistrationReady =>
        Redirect(onboardingRoutes.RegistrationController.checkYourAnswers())

      case _: RegistrationStatus.IndividualSupplyingInformation =>
        Redirect(onboardingRoutes.RegistrationController.selectEntityType())

      case _: RegistrationStatus.IndividualMissingEmail =>
        Redirect(emailRoutes.RegistrationEnterEmailController.enterEmail())

      case RegistrationStatus.IndividualWantsToRegisterTrust(_) =>
        Redirect(onboardingRoutes.RegistrationController.selectEntityType())

      case SubscriptionStatus
            .DeterminingIfOrganisationIsTrust(ggCredId, ggEmail, _, _) =>
        handleNonTrustOrganisation(ggCredId, ggEmail)

      case NonGovernmentGatewayJourney =>
        Redirect(routes.StartController.weOnlySupportGG())

      case s: SubscriptionStatus.SubscriptionMissingData =>
        handleSubscriptionMissingData(s)

      case _: AgentStatus.AgentSupplyingClientDetails =>
        Redirect(agentsRoutes.AgentAccessController.enterClientsCgtRef())

      case _: StartingNewDraftReturn | _: FillingOutReturn | _: ViewingReturn | _: SubmitReturnFailed |
          _: SubmittingReturn =>
        Redirect(accounts.homepage.routes.HomePageController.homepage())

      case _: JustSubmittedReturn =>
        Redirect(
          controllers.returns.routes.CheckAllAnswersAndSubmitController
            .confirmationOfSubmission()
        )

      case _: StartingToAmendReturn =>
        Redirect(
          controllers.returns.amend.routes.AmendReturnController.checkYourAnswers()
        )

      case NewEnrolmentCreatedForMissingEnrolment(subscribedDetails, ggCredId) =>
        handleSubscribedUser(subscribedDetails.cgtReference, ggCredId, Some(subscribedDetails))
    }

  private def handleRetrievedUserType(
    retrievedUserType: RetrievedUserType
  )(implicit
    request: RequestWithSessionDataAndRetrievedData[AnyContent]
  ): Future[Result] =
    retrievedUserType match {
      case RetrievedUserType.Agent(ggCredId, arn) =>
        val journeyStatusWithRedirect =
          arn.fold[(JourneyStatus, Call)](
            AgentWithoutAgentEnrolment -> routes.StartController
              .agentNoEnrolment()
          )(a =>
            AgentStatus.AgentSupplyingClientDetails(
              a,
              ggCredId,
              None
            ) -> agentsRoutes.AgentAccessController.enterClientsCgtRef()
          )

        updateSession(sessionStore, request.sessionData)(
          _.copy(
            journeyStatus = Some(journeyStatusWithRedirect._1),
            userType = Some(UserType.Agent)
          )
        ).map {
          case Left(e) =>
            logger.warn("Could not update session", e)
            errorHandler.errorResult(Some(UserType.Agent))

          case Right(_) =>
            Redirect(journeyStatusWithRedirect._2)
        }

      case RetrievedUserType.Subscribed(cgtReference, ggCredId) =>
        handleSubscribedUser(cgtReference, ggCredId, None)

      case RetrievedUserType.IndividualWithInsufficientConfidenceLevel(
            maybeNino,
            maybeSautr,
            ggEmail,
            ggCredId
          ) =>
        // this is the first time a person with individual insufficient confidence level has come to start
        handleInsufficientConfidenceLevel(
          maybeNino,
          maybeSautr,
          ggEmail,
          ggCredId
        )

      case i: RetrievedUserType.Individual =>
        buildIndividualSubscriptionData(i)

      case t: RetrievedUserType.Trust =>
        buildTrustSubscriptionData(t)

      case RetrievedUserType.OrganisationUnregisteredTrust(_, ggCredId) =>
        handleNonTrustOrganisation(ggCredId, None)

      case u: RetrievedUserType.NonGovernmentGatewayRetrievedUser =>
        handleNonGovernmentGatewayUser(u)
    }

  private def handleNonGovernmentGatewayUser(
    nonGovernmentGatewayUser: NonGovernmentGatewayRetrievedUser
  )(implicit
    request: RequestWithSessionDataAndRetrievedData[?]
  ): Future[Result] = {
    logger.warn(
      s"User logged in with unsupported provider: ${nonGovernmentGatewayUser.authProvider}"
    )

    updateSession(sessionStore, request.sessionData)(
      _.copy(
        userType = request.authenticatedRequest.userType,
        journeyStatus = Some(NonGovernmentGatewayJourney)
      )
    ).map {
      case Left(e) =>
        logger.warn("Could not update session", e)
        errorHandler.errorResult(request.authenticatedRequest.userType)

      case Right(_) =>
        Redirect(routes.StartController.weOnlySupportGG())
    }

  }

  private def handleSubscribedUser(
    cgtReference: CgtReference,
    ggCredId: GGCredId,
    subscribedDetails: Option[SubscribedDetails]
  )(implicit
    request: RequestWithSessionDataAndRetrievedData[?]
  ): Future[Result] =
    (for {
      subscribedDetails                         <-
        subscribedDetails match {
          case None          =>
            subscriptionService
              .getSubscribedDetails(cgtReference)
              .subflatMap(
                _.toRight(Error(s"Could not find subscribed details for cgt reference ${cgtReference.value}"))
              )
          case Some(details) => EitherT.pure[Future, Error](details)
        }
      sentReturns                               <- returnsService.listReturns(cgtReference)
      draftReturns                              <- returnsService.getDraftReturns(cgtReference, sentReturns)
      unsetDraftReturnFlagAndUpdatedSentReturns <-
        returnsService.updateCorrectTaxYearToSentReturns(subscribedDetails.cgtReference, sentReturns)

      updatedDraftReturns = if (unsetDraftReturnFlagAndUpdatedSentReturns._1) {
                              draftReturns.map(returnsService.unsetUnwantedSectionsToDraftReturn)
                            } else {
                              draftReturns
                            }

      _ <- EitherT(
             updateSession(sessionStore, request.sessionData)(
               _.copy(
                 userType = request.authenticatedRequest.userType,
                 journeyStatus = Some(
                   Subscribed(
                     subscribedDetails,
                     ggCredId,
                     None,
                     updatedDraftReturns,
                     unsetDraftReturnFlagAndUpdatedSentReturns._2
                   )
                 )
               )
             )
           )
    } yield Redirect(controllers.accounts.homepage.routes.HomePageController.homepage()))
      .valueOr { e =>
        logger.warn("Could not get subscribed details", e)
        errorHandler.errorResult(request.authenticatedRequest.userType)
      }

  private def handleNonTrustOrganisation(
    ggCredId: GGCredId,
    ggEmail: Option[Email]
  )(implicit
    request: RequestWithSessionDataAndRetrievedData[?]
  ): Future[Result] = {
    val newSessionData =
      request.sessionData.journeyStatus match {
        case Some(d: SubscriptionStatus.DeterminingIfOrganisationIsTrust) => d
        case _                                                            =>
          SubscriptionStatus
            .DeterminingIfOrganisationIsTrust(ggCredId, ggEmail, None, None)
      }

    updateSession(sessionStore, request.sessionData)(
      _.copy(
        userType = request.authenticatedRequest.userType,
        journeyStatus = Some(newSessionData),
        needMoreDetailsDetails = Some(
          NeedMoreDetailsDetails(
            onboardingRoutes.DeterminingIfOrganisationIsTrustController
              .doYouWantToReportForATrust()
              .url,
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
  )(implicit
    request: RequestWithSessionDataAndRetrievedData[AnyContent]
  ): Future[Result] =
    maybeNino match {
      case None =>
        maybeSautr match {
          case Some(sautr) =>
            buildIndividualSubscriptionData(
              Individual(Left(sautr), ggEmail, ggCredId)
            )

          case None =>
            val subscriptionStatus =
              SubscriptionStatus
                .TryingToGetIndividualsFootprint(None, None, ggEmail, ggCredId)

            updateSession(sessionStore, request.sessionData)(s =>
              s.copy(
                userType = request.authenticatedRequest.userType,
                journeyStatus = Some(subscriptionStatus),
                needMoreDetailsDetails = Some(
                  NeedMoreDetailsDetails(
                    onboardingRoutes.InsufficientConfidenceLevelController
                      .doYouHaveNINO()
                      .url,
                    NeedMoreDetailsDetails.AffinityGroup.Individual
                  )
                )
              )
            ).map {
              case Left(e) =>
                logger.warn(
                  "Could not update session with insufficient confidence level",
                  e
                )
                errorHandler.errorResult(request.authenticatedRequest.userType)

              case Right(_) =>
                Redirect(routes.StartController.weNeedMoreDetails())
            }
        }

      case Some(_) =>
        auditService.sendEvent(
          "handOffToIv",
          HandOffTIvEvent(ggCredId.value, request.uri),
          "handoff-to-iv"
        )
        redirectToIv
    }

  private def buildTrustSubscriptionData(
    trust: Trust
  )(implicit
    hc: HeaderCarrier,
    request: RequestWithSessionDataAndRetrievedData[?]
  ): Future[Result] = {
    val result =
      for {
        bprResponse <- bprService.getBusinessPartnerRecord(
                         TrustBusinessPartnerRecordRequest(
                           Right(trust.sautr),
                           None,
                           trust.ggCredId.value,
                           createNewEnrolmentIfMissing = true
                         ),
                         request.messages.lang
                       )

        bprWithTrustName         <- EitherT.fromEither[Future](
                                      Either
                                        .fromOption(
                                          bprResponse.businessPartnerRecord,
                                          Error("Could not find BPR for trust")
                                        )
                                        .flatMap(bpr =>
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
          val emailAndAddress = for {
            emailWithSource <- bprWithTrustName._1.emailAddress
                                 .map(_ -> EmailSource.BusinessPartnerRecord)
                                 .orElse(trust.email.map(_ -> EmailSource.GovernmentGateway))
            address         <- bprWithTrustName._1.address.map(_ -> AddressSource.BusinessPartnerRecord)
          } yield emailWithSource -> address

          EitherT.pure[Future, Error](
            emailAndAddress
              .fold[Either[BuildSubscriptionDataError, SubscriptionDetails]](
                Left(
                  BuildSubscriptionDataError.DataMissing(bprWithTrustName._1)
                )
              ) { case (emailWithSource, addressWithSource) =>
                bprResponse.cgtReference.fold[Either[BuildSubscriptionDataError, SubscriptionDetails]](
                  Right(
                    SubscriptionDetails(
                      Left(bprWithTrustName._2),
                      emailWithSource._1,
                      addressWithSource._1,
                      ContactName(bprWithTrustName._2.value),
                      bprWithTrustName._1.sapNumber,
                      emailWithSource._2,
                      addressWithSource._2,
                      ContactNameSource.DerivedFromBusinessPartnerRecord
                    )
                  )
                )(cgtReference =>
                  bprResponse.newEnrolmentSubscribedDetails
                    .fold[Either[BuildSubscriptionDataError, SubscriptionDetails]](
                      Left(
                        BuildSubscriptionDataError.AlreadySubscribedToCGT(cgtReference)
                      )
                    )(newEnrolmentSubscribedDetails =>
                      Left(
                        BuildSubscriptionDataError.NewEnrolmentCreatedForMissingEnrolment(
                          newEnrolmentSubscribedDetails
                        )
                      )
                    )
                )
              }
          )
        }
        _                        <- updateSession(
                                      maybeSubscriptionDetails,
                                      trust.email,
                                      trust.ggCredId,
                                      AffinityGroup.Organisation
                                    )
      } yield maybeSubscriptionDetails

    result.fold(
      { e =>
        logger.warn(
          s"Could not build subscription data for trust with SAUTR ${trust.sautr}",
          e
        )
        errorHandler.errorResult(request.authenticatedRequest.userType)
      },
      {
        case Left(BuildSubscriptionDataError.DataMissing(_)) =>
          Redirect(routes.StartController.weNeedMoreDetails())

        case Left(
              BuildSubscriptionDataError.AlreadySubscribedToCGT(cgtReference)
            ) =>
          auditService.sendEvent(
            "accessWithWrongGGAccount",
            WrongGGAccountEvent(Some(cgtReference.value), trust.ggCredId.value),
            "access-with-wrong-gg-account"
          )
          Redirect(onboardingRoutes.SubscriptionController.alreadySubscribedWithDifferentGGAccount())

        case Left(BuildSubscriptionDataError.NewEnrolmentCreatedForMissingEnrolment(subscribedDetails)) =>
          logger.info(
            s"New enrolment created after missing enrolment found for cgt reference ${subscribedDetails.cgtReference.value}"
          )
          Redirect(routes.StartController.start())

        case Right(_) =>
          Redirect(onboardingRoutes.SubscriptionController.checkYourDetails())
      }
    )
  }

  private def handleSubscriptionMissingData(s: SubscriptionMissingData)(implicit
    request: RequestWithSessionDataAndRetrievedData[?]
  ): Future[Result] =
    SubscriptionDetails(
      s.businessPartnerRecord,
      s.ggEmail,
      s.manuallyEnteredEmail,
      s.manuallyEnteredAddress
    ).fold(
      { missingData =>
        logger.info(
          s"Could not find the following data for subscription details: ${missingData.toList.mkString(",")}"
        )
        missingData.head match {
          case MissingData.Address =>
            Redirect(addressRoutes.SubscriptionEnterAddressController.isUk())

          case MissingData.Email =>
            Redirect(emailRoutes.SubscriptionEnterEmailController.enterEmail())
        }
      },
      subscriptionDetails =>
        updateSession(sessionStore, request.sessionData)(
          _.copy(
            userType = request.authenticatedRequest.userType,
            journeyStatus = Some(
              SubscriptionStatus
                .SubscriptionReady(subscriptionDetails, s.ggCredId)
            )
          )
        ).map(_ => Redirect(onboardingRoutes.SubscriptionController.checkYourDetails()))
    )

  private def buildIndividualSubscriptionData(individual: Individual)(implicit
    hc: HeaderCarrier,
    request: RequestWithSessionDataAndRetrievedData[?]
  ): Future[Result] = {
    val result = for {
      bprResponse              <- bprService.getBusinessPartnerRecord(
                                    IndividualBusinessPartnerRecordRequest(
                                      individual.id,
                                      None,
                                      individual.ggCredId.value,
                                      createNewEnrolmentIfMissing = true
                                    ),
                                    request.messages.lang
                                  )
      bpr                      <- EitherT.fromEither[Future](
                                    Either.fromOption(
                                      bprResponse.businessPartnerRecord,
                                      Error("Could not find BPR for individual")
                                    )
                                  )
      maybeSubscriptionDetails <-
        EitherT.pure[Future, Error](
          bprResponse.cgtReference
            .fold[Either[BuildSubscriptionDataError, SubscriptionDetails]](
              SubscriptionDetails(bpr, individual.email, None, None)
                .leftMap(_ => BuildSubscriptionDataError.DataMissing(bpr))
            )(cgtReference =>
              bprResponse.newEnrolmentSubscribedDetails.fold[Either[BuildSubscriptionDataError, SubscriptionDetails]](
                Left(
                  BuildSubscriptionDataError.AlreadySubscribedToCGT(cgtReference)
                )
              )(newEnrolmentSubscribedDetails =>
                Left(
                  BuildSubscriptionDataError.NewEnrolmentCreatedForMissingEnrolment(newEnrolmentSubscribedDetails)
                )
              )
            )
        )
      _                        <- updateSession(
                                    maybeSubscriptionDetails,
                                    individual.email,
                                    individual.ggCredId,
                                    AffinityGroup.Individual
                                  )
    } yield maybeSubscriptionDetails

    result.fold(
      { e =>
        logger.warn("Error while getting subscription details", e)
        errorHandler.errorResult(request.authenticatedRequest.userType)
      },
      {
        case Left(BuildSubscriptionDataError.DataMissing(_)) =>
          Redirect(routes.StartController.weNeedMoreDetails())

        case Left(
              BuildSubscriptionDataError.AlreadySubscribedToCGT(cgtReference)
            ) =>
          auditService.sendEvent(
            "accessWithWrongGGAccount",
            WrongGGAccountEvent(
              Some(cgtReference.value),
              individual.ggCredId.value
            ),
            "access-with-wrong-gg-account"
          )
          Redirect(onboardingRoutes.SubscriptionController.alreadySubscribedWithDifferentGGAccount())

        case Left(BuildSubscriptionDataError.NewEnrolmentCreatedForMissingEnrolment(subscribedDetails)) =>
          logger.info(
            s"New enrolment created after missing enrolment found for cgt reference ${subscribedDetails.cgtReference.value}"
          )
          Redirect(routes.StartController.start())

        case Right(_) =>
          Redirect(onboardingRoutes.SubscriptionController.checkYourDetails())
      }
    )
  }

  private def updateSession(
    x: Either[BuildSubscriptionDataError, SubscriptionDetails],
    ggEmail: Option[Email],
    ggCredId: GGCredId,
    affinityGroup: AffinityGroup
  )(implicit
    request: RequestWithSessionDataAndRetrievedData[?]
  ): EitherT[Future, Error, Unit] =
    EitherT[Future, Error, Unit](
      x.fold(
        {
          case BuildSubscriptionDataError.DataMissing(bpr) =>
            updateSession(sessionStore, request.sessionData)(
              _.copy(
                journeyStatus = Some(
                  SubscriptionStatus
                    .SubscriptionMissingData(bpr, None, None, ggCredId, ggEmail)
                ),
                needMoreDetailsDetails = Some(
                  NeedMoreDetailsDetails(
                    routes.StartController.start().url,
                    affinityGroup
                  )
                )
              )
            )
          case BuildSubscriptionDataError.AlreadySubscribedToCGT(
                cgtReference
              ) =>
            updateSession(sessionStore, request.sessionData)(
              _.copy(
                journeyStatus = Some(
                  AlreadySubscribedWithDifferentGGAccount(
                    ggCredId,
                    Some(cgtReference)
                  )
                )
              )
            )

          case BuildSubscriptionDataError.NewEnrolmentCreatedForMissingEnrolment(subscribedDetails) =>
            updateSession(sessionStore, request.sessionData)(
              _.copy(
                journeyStatus = Some(
                  NewEnrolmentCreatedForMissingEnrolment(
                    subscribedDetails,
                    ggCredId
                  )
                )
              )
            )

        },
        subscriptionDetails =>
          updateSession(sessionStore, request.sessionData)(
            _.copy(
              journeyStatus = Some(
                SubscriptionStatus
                  .SubscriptionReady(subscriptionDetails, ggCredId)
              )
            )
          )
      )
    )

}

object StartController {

  sealed trait BuildSubscriptionDataError extends Product with Serializable

  object BuildSubscriptionDataError {

    final case class AlreadySubscribedToCGT(cgtReference: CgtReference) extends BuildSubscriptionDataError

    final case class DataMissing(bpr: BusinessPartnerRecord) extends BuildSubscriptionDataError

    final case class NewEnrolmentCreatedForMissingEnrolment(subscribedDetails: SubscribedDetails)
        extends BuildSubscriptionDataError

  }

}
