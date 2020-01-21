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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding

import cats.data.EitherT
import cats.instances.future._
import org.joda.time.{LocalDate => JodaLocalDate}
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.i18n.MessagesApi
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.libs.json.Writes
import play.api.mvc.{AnyContent, Request, Result}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.ConfidenceLevel.L50
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.auth.core.retrieve.Credentials
import uk.gov.hmrc.cgtpropertydisposalsfrontend._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.EnrolmentConfig._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.email.{routes => emailRoutes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.{routes => onboardingRoutes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport, StartController, agents}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.RegistrationStatus.{IndividualMissingEmail, RegistrationReady}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.SubscriptionStatus._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{AgentStatus, AlreadySubscribedWithDifferentGGAccount, NonGovernmentGatewayJourney, RegistrationStatus, Subscribed, SubscriptionStatus}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.RetrievedUserType.Individual
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address.UkAddress
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.{Address, AddressSource, Postcode}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.{ContactName, ContactNameSource, IndividualName, TrustName}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.audit.HandOffTIvEvent
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.bpr.BusinessPartnerRecordRequest._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.bpr.{BusinessPartnerRecord, BusinessPartnerRecordRequest, BusinessPartnerRecordResponse}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.email.{Email, EmailSource}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.{NeedMoreDetailsDetails, SubscribedDetails, SubscriptionDetails}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.AuditService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.onboarding.{BusinessPartnerRecordService, SubscriptionService}
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, Future}

class StartControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with IvBehaviourSupport
    with ScalaCheckDrivenPropertyChecks
    with RedirectToStartBehaviour {

  val mockBprService = mock[BusinessPartnerRecordService]

  val mockAuditService = mock[AuditService]

  override val overrideBindings =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore),
      bind[BusinessPartnerRecordService].toInstance(mockBprService),
      bind[SubscriptionService].toInstance(mockSubscriptionService),
      bind[AuditService].toInstance(mockAuditService)
    )

  override lazy val additionalConfig = ivConfig(useRelativeUrls = false)

  lazy val controller = instanceOf[StartController]

  lazy implicit val messagesApi: MessagesApi = controller.messagesApi

  def mockGetBusinessPartnerRecord(request: BusinessPartnerRecordRequest)(
    result: Either[Error, BusinessPartnerRecordResponse]
  ) =
    (mockBprService
      .getBusinessPartnerRecord(_: BusinessPartnerRecordRequest)(_: HeaderCarrier))
      .expects(request, *)
      .returning(EitherT.fromEither[Future](result))

  def mockHasSubscription()(response: Either[Error, Option[CgtReference]]) =
    (mockSubscriptionService
      .hasSubscription()(_: HeaderCarrier))
      .expects(*)
      .returning(EitherT(Future.successful(response)))

  def mockGetSubscribedDetails(cgtReference: CgtReference)(response: Either[Error, SubscribedDetails]) =
    (mockSubscriptionService
      .getSubscribedDetails(_: CgtReference)(_: HeaderCarrier))
      .expects(cgtReference, *)
      .returning(EitherT(Future.successful(response)))

  def mockSendHandOffToIvAuditEvent(ggCredId: GGCredId, ivRedirectUrl: String)(response: Unit) =
    (mockAuditService
      .sendEvent(_: String, _: HandOffTIvEvent, _: String)(
        _: ExecutionContext,
        _: HeaderCarrier,
        _: Writes[HandOffTIvEvent],
        _: Request[_]
      ))
      .expects("handOffToIv", HandOffTIvEvent(ggCredId.value, ivRedirectUrl), "handoff-to-iv", *, *, *, *)
      .returning(response)

  val nino                 = NINO("AB123456C")
  val name                 = IndividualName("forename", "surname")
  val trustName            = TrustName("trust")
  val retrievedDateOfBirth = JodaLocalDate.parse("2000-04-10")
  val emailAddress         = Email("email")

  val retrievedGGCredId = Credentials("gg", "GovernmentGateway")
  val ggCredId          = GGCredId(retrievedGGCredId.providerId)
  val individual        = Individual(Right(nino), None, ggCredId)

  "The StartController" when {

    "handling requests to start a journey" when {

      def performAction(rh: Request[AnyContent] = FakeRequest()): Future[Result] =
        controller.start()(rh)

      "the session data indicates the user has already subscribed with a different gg account" must {

        "redirect to the already subscribed with a different gg account page" in {
          inSequence {
            mockAuthWithAllRetrievals(
              ConfidenceLevel.L50,
              Some(AffinityGroup.Organisation),
              None,
              None,
              None,
              Set.empty,
              Some(retrievedGGCredId)
            )
            mockHasSubscription()(Right(None))
            mockGetSession(
              Future.successful(
                Right(
                  Some(
                    SessionData.empty.copy(
                      userType      = Some(UserType.Organisation),
                      journeyStatus = Some(AlreadySubscribedWithDifferentGGAccount(ggCredId))
                    )
                  )
                )
              )
            )
          }

          checkIsRedirect(
            performAction(),
            onboardingRoutes.SubscriptionController.alreadySubscribedWithDifferentGGAccount()
          )
        }

      }

      "handling non trust organisations" must {

        val determiningIfOrganisationIsTrustSession =
          SessionData.empty.copy(
            userType      = Some(UserType.Organisation),
            journeyStatus = Some(SubscriptionStatus.DeterminingIfOrganisationIsTrust(ggCredId, None, None, None))
          )

        lazy val needMoreDetailsContinueUrl =
          onboardingRoutes.DeterminingIfOrganisationIsTrustController.doYouWantToReportForATrust().url

        "show an error page" when {

          "there is an error storing the session in mongo" in {
            inSequence {
              mockAuthWithAllRetrievals(
                ConfidenceLevel.L50,
                Some(AffinityGroup.Organisation),
                None,
                None,
                None,
                Set.empty,
                Some(retrievedGGCredId)
              )
              mockHasSubscription()(Right(None))
              mockGetSession(Future.successful(Right(Some(SessionData.empty))))
              mockStoreSession(
                determiningIfOrganisationIsTrustSession.copy(
                  needMoreDetailsDetails = Some(
                    NeedMoreDetailsDetails(
                      needMoreDetailsContinueUrl,
                      NeedMoreDetailsDetails.AffinityGroup.Organisation
                    )
                  ),
                  userType = Some(UserType.Organisation)
                )
              )(Future.successful(Left(Error(""))))
            }

            checkIsTechnicalErrorPage(performAction(FakeRequest()))
          }

        }

        "redirect to the 'we need more details' page with a continue to the 'are you " +
          "reporting for a trust' page " when {

          "the session already has the relevant journey status in it" in {
            val journey     = DeterminingIfOrganisationIsTrust(ggCredId, None, Some(true), Some(true))
            val sessionData = SessionData.empty.copy(journeyStatus = Some(journey))
            inSequence {
              mockAuthWithAllRetrievals(
                ConfidenceLevel.L50,
                Some(AffinityGroup.Organisation),
                None,
                None,
                None,
                Set.empty,
                Some(retrievedGGCredId)
              )
              mockHasSubscription()(Right(None))
              mockGetSession(Future.successful(Right(Some(sessionData))))
              mockStoreSession(
                sessionData.copy(
                  needMoreDetailsDetails = Some(
                    NeedMoreDetailsDetails(
                      needMoreDetailsContinueUrl,
                      NeedMoreDetailsDetails.AffinityGroup.Organisation
                    )
                  ),
                  userType = Some(UserType.Organisation)
                )
              )(Future.successful(Right(())))
            }

            checkIsRedirect(performAction(FakeRequest()), controllers.routes.StartController.weNeedMoreDetails())
          }

          "the session data is updated when there is no relevant journey status in it" in {
            inSequence {
              mockAuthWithAllRetrievals(
                ConfidenceLevel.L50,
                Some(AffinityGroup.Organisation),
                None,
                None,
                None,
                Set.empty,
                Some(retrievedGGCredId)
              )
              mockHasSubscription()(Right(None))
              mockGetSession(Future.successful(Right(None)))
              mockStoreSession(
                determiningIfOrganisationIsTrustSession.copy(
                  needMoreDetailsDetails = Some(
                    NeedMoreDetailsDetails(
                      needMoreDetailsContinueUrl,
                      NeedMoreDetailsDetails.AffinityGroup.Organisation
                    )
                  ),
                  userType = Some(UserType.Organisation)
                )
              )(Future.successful(Right(())))

            }

            checkIsRedirect(performAction(FakeRequest()), controllers.routes.StartController.weNeedMoreDetails())
          }
        }

      }

      "handling individual users with insufficient confidence level" when {

        "the session has not been updated to indicate so" must {

          "show an error" when {

            "the session cannot be updated when there is not a nino in the auth record" in {
              inSequence {
                mockAuthWithAllRetrievals(
                  ConfidenceLevel.L50,
                  Some(AffinityGroup.Individual),
                  None,
                  None,
                  None,
                  Set.empty,
                  Some(retrievedGGCredId)
                )
                mockHasSubscription()(Right(None))
                mockGetSession(Future.successful(Right(Some(SessionData.empty))))
                mockStoreSession(
                  SessionData.empty.copy(
                    journeyStatus = Some(SubscriptionStatus.TryingToGetIndividualsFootprint(None, None, None, ggCredId)),
                    needMoreDetailsDetails = Some(
                      NeedMoreDetailsDetails(
                        onboardingRoutes.InsufficientConfidenceLevelController.doYouHaveNINO().url,
                        NeedMoreDetailsDetails.AffinityGroup.Individual
                      )
                    ),
                    userType = Some(UserType.Individual)
                  )
                )(Future.successful(Left(Error(""))))
              }

              checkIsTechnicalErrorPage(performAction())
            }

          }

          "redirect to the IV journey" when {

            "the user does not have sufficient confidence level and there is a NINO in the auth record" in {
              val request = FakeRequest("GET", "/uri")

              inSequence {
                mockAuthWithAllRetrievals(
                  ConfidenceLevel.L50,
                  Some(AffinityGroup.Individual),
                  Some("nino"),
                  None,
                  None,
                  Set.empty,
                  Some(retrievedGGCredId)
                )
                mockHasSubscription()(Right(None))
                mockGetSession(Future.successful(Right(Some(SessionData.empty))))
                mockSendHandOffToIvAuditEvent(ggCredId, "/uri")(())
              }

              checkIsRedirectToIv(performAction(request), false)
            }

            "the user does not have sufficient confidence level and there is a NINO in the auth record and " +
              "the application is configured to use absoluate URLs for IV" in new ControllerSpec {
              override val overrideBindings =
                List[GuiceableModule](
                  bind[AuthConnector].toInstance(mockAuthConnector),
                  bind[SessionStore].toInstance(mockSessionStore),
                  bind[BusinessPartnerRecordService].toInstance(mockBprService),
                  bind[SubscriptionService].toInstance(mockSubscriptionService)
                )

              override lazy val additionalConfig = ivConfig(useRelativeUrls = true)
              val controller                     = instanceOf[StartController]
              val request                        = FakeRequest("GET", "/uri")

              inSequence {
                mockAuthWithAllRetrievals(
                  ConfidenceLevel.L50,
                  Some(AffinityGroup.Individual),
                  Some("nino"),
                  None,
                  None,
                  Set.empty,
                  Some(retrievedGGCredId)
                )
                mockHasSubscription()(Right(None))
                mockGetSession(Future.successful(Right(Some(SessionData.empty))))
              }

              checkIsRedirectToIv(controller.start()(request), true)
            }

          }

          "redirect to the we need more details page" when {

            "the user does not have sufficient confidence level and there is no NINO in the auth record" in {
              inSequence {
                mockAuthWithAllRetrievals(
                  ConfidenceLevel.L50,
                  Some(AffinityGroup.Individual),
                  None,
                  None,
                  None,
                  Set.empty,
                  Some(retrievedGGCredId)
                )
                mockHasSubscription()(Right(None))
                mockGetSession(Future.successful(Right(Some(SessionData.empty))))
                mockStoreSession(
                  SessionData.empty.copy(
                    journeyStatus = Some(
                      SubscriptionStatus.TryingToGetIndividualsFootprint(None, None, None, ggCredId)
                    ),
                    needMoreDetailsDetails = Some(
                      NeedMoreDetailsDetails(
                        onboardingRoutes.InsufficientConfidenceLevelController.doYouHaveNINO().url,
                        NeedMoreDetailsDetails.AffinityGroup.Individual
                      )
                    ),
                    userType = Some(UserType.Individual)
                  )
                )(Future.successful(Right(())))
              }

              checkIsRedirect(performAction(), controllers.routes.StartController.weNeedMoreDetails())
            }

            "the session data indicates they do not have sufficient confidence level" in {
              inSequence {
                mockAuthWithAllRetrievals(
                  ConfidenceLevel.L50,
                  Some(AffinityGroup.Individual),
                  None,
                  None,
                  None,
                  Set.empty,
                  Some(retrievedGGCredId)
                )
                mockHasSubscription()(Right(None))
                mockGetSession(
                  Future.successful(
                    Right(
                      Some(
                        SessionData.empty.copy(
                          journeyStatus = Some(
                            SubscriptionStatus.TryingToGetIndividualsFootprint(None, None, None, ggCredId)
                          ),
                          userType = Some(UserType.Individual)
                        )
                      )
                    )
                  )
                )
              }

              checkIsRedirect(performAction(), onboardingRoutes.InsufficientConfidenceLevelController.doYouHaveNINO())
            }

          }

        }

        "the session data has been updated to indicate so" when {

          "redirect to the do you have a nino page" in {
            def sessionData = SessionData.empty.copy(
              journeyStatus = Some(TryingToGetIndividualsFootprint(Some(false), None, None, ggCredId)),
              userType      = Some(UserType.Individual)
            )

            inSequence {
              mockAuthWithAllRetrievals(
                L50,
                Some(AffinityGroup.Individual),
                None,
                None,
                None,
                Set.empty,
                Some(retrievedGGCredId)
              )
              mockHasSubscription()(Right(None))
              mockGetSession(Future.successful(Right(Some(sessionData))))
            }

            checkIsRedirect(performAction(), onboardingRoutes.InsufficientConfidenceLevelController.doYouHaveNINO())
          }

        }

        "the session data indicates that some subscription details are missing" must {

          val bprWithNoEmail = BusinessPartnerRecord(
            None,
            sample[Address],
            sample[SapNumber],
            Right(name)
          )

          "redirect to check your details" when {

            "the session data indicates there is subscription data missing and there is now enough " +
              "data to proceed to the check your details page" in {
              val individualSubscriptionDetails =
                SubscriptionDetails(
                  Right(name),
                  emailAddress,
                  bprWithNoEmail.address,
                  ContactName(name.makeSingleName),
                  bprWithNoEmail.sapNumber,
                  EmailSource.ManuallyEntered,
                  AddressSource.BusinessPartnerRecord,
                  ContactNameSource.DerivedFromBusinessPartnerRecord
                )

              val session = SessionData.empty.copy(
                journeyStatus = Some(SubscriptionMissingData(bprWithNoEmail, Some(emailAddress), ggCredId, None)),
                userType      = Some(UserType.Individual)
              )

              val updatedSession =
                SessionData.empty.copy(
                  userType      = Some(UserType.Individual),
                  journeyStatus = Some(SubscriptionReady(individualSubscriptionDetails, ggCredId))
                )

              inSequence {
                mockAuthWithAllRetrievals(
                  ConfidenceLevel.L50,
                  Some(AffinityGroup.Individual),
                  None,
                  None,
                  None,
                  Set.empty,
                  Some(retrievedGGCredId)
                )
                mockHasSubscription()(Right(None))
                mockGetSession(Future.successful(Right(Some(session))))
                mockStoreSession(updatedSession)(Future.successful(Right(())))
              }
              checkIsRedirect(performAction(), onboardingRoutes.SubscriptionController.checkYourDetails())
            }
          }

          "redirect to enter email" when {

            "the email address is still missing" in {
              val sessionData =
                SessionData.empty.copy(
                  journeyStatus = Some(SubscriptionMissingData(bprWithNoEmail, None, ggCredId, None))
                )

              inSequence {
                mockAuthWithAllRetrievals(
                  ConfidenceLevel.L50,
                  Some(AffinityGroup.Individual),
                  None,
                  None,
                  None,
                  Set.empty,
                  Some(retrievedGGCredId)
                )
                mockHasSubscription()(Right(None))
                mockGetSession(Future.successful(Right(Some(sessionData))))
              }

              checkIsRedirect(performAction(), emailRoutes.SubscriptionEnterEmailController.enterEmail().url)
            }

          }

        }

      }

      "the user is not enrolled and is not subscribed in ETMP" when {

        val name = sample[IndividualName]

        val bpr = models.onboarding.bpr.BusinessPartnerRecord(
          Some(emailAddress),
          sample[UkAddress],
          sample[SapNumber],
          Right(name)
        )

        "handling individuals" must {

          val individualSubscriptionDetails = SubscriptionDetails(
            Right(name),
            emailAddress,
            bpr.address,
            ContactName(name.makeSingleName()),
            bpr.sapNumber,
            EmailSource.BusinessPartnerRecord,
            AddressSource.BusinessPartnerRecord,
            ContactNameSource.DerivedFromBusinessPartnerRecord
          )

          "redirect to check subscription details" when {

            "there are subscription details in session" in {
              val session =
                SessionData.empty.copy(journeyStatus = Some(SubscriptionReady(individualSubscriptionDetails, ggCredId)))
              inSequence {
                mockAuthWithCl200AndWithAllIndividualRetrievals(nino.value, None, retrievedGGCredId)
                mockHasSubscription()(Right(None))
                mockGetSession(Future.successful(Right(Some(session))))
              }

              checkIsRedirect(performAction(), onboardingRoutes.SubscriptionController.checkYourDetails())
            }

            "there is no session data and a bpr is successfully retrieved using the retrieved auth NINO" in {
              List(
                Some(SessionData.empty),
                None
              ).foreach { maybeSession =>
                val session =
                  SessionData.empty.copy(
                    userType      = Some(UserType.Individual),
                    journeyStatus = Some(SubscriptionReady(individualSubscriptionDetails, ggCredId))
                  )

                inSequence {
                  mockAuthWithCl200AndWithAllIndividualRetrievals(nino.value, None, retrievedGGCredId)
                  mockHasSubscription()(Right(None))
                  mockGetSession(Future.successful(Right(maybeSession)))
                  mockGetBusinessPartnerRecord(IndividualBusinessPartnerRecordRequest(Right(nino), None))(
                    Right(BusinessPartnerRecordResponse(Some(bpr)))
                  )
                  mockStoreSession(session)(Future.successful(Right(())))
                }

                checkIsRedirect(performAction(), onboardingRoutes.SubscriptionController.checkYourDetails())
              }
            }

            "the user has CL<200 and there is no session data and a bpr is successfully retrieved " +
              "using the retrieved auth SA UTR" in {
              List(
                Some(SessionData.empty),
                None
              ).foreach { maybeSession =>
                val session =
                  SessionData.empty.copy(
                    userType      = Some(UserType.Individual),
                    journeyStatus = Some(SubscriptionReady(individualSubscriptionDetails, ggCredId))
                  )

                inSequence {
                  mockAuthWithAllRetrievals(
                    ConfidenceLevel.L50,
                    Some(AffinityGroup.Individual),
                    None,
                    Some("sautr"),
                    None,
                    Set.empty,
                    Some(retrievedGGCredId)
                  )
                  mockHasSubscription()(Right(None))
                  mockGetSession(Future.successful(Right(maybeSession)))
                  mockGetBusinessPartnerRecord(IndividualBusinessPartnerRecordRequest(Left(SAUTR("sautr")), None))(
                    Right(BusinessPartnerRecordResponse(Some(bpr)))
                  )
                  mockStoreSession(session)(Future.successful(Right(())))
                }

                checkIsRedirect(performAction(), onboardingRoutes.SubscriptionController.checkYourDetails())
              }
            }

            "an email does not exist in the retrieved BPR but one has been retrieved in the auth record" in {
              val bprWithNoEmail = bpr.copy(emailAddress = None)
              val ggEmail        = Email("email")
              val session = SessionData.empty.copy(
                userType = Some(UserType.Individual),
                journeyStatus = Some(
                  SubscriptionReady(
                    individualSubscriptionDetails.copy(
                      emailAddress = ggEmail,
                      emailSource  = EmailSource.GovernmentGateway
                    ),
                    ggCredId
                  )
                )
              )

              inSequence {
                mockAuthWithCl200AndWithAllIndividualRetrievals(nino.value, Some(ggEmail.value), retrievedGGCredId)
                mockHasSubscription()(Right(None))
                mockGetSession(Future.successful(Right(Some(SessionData.empty))))
                mockGetBusinessPartnerRecord(IndividualBusinessPartnerRecordRequest(Right(nino), None))(
                  Right(BusinessPartnerRecordResponse(Some(bprWithNoEmail)))
                )
                mockStoreSession(session)(Future.successful(Right(())))
              }

              checkIsRedirect(performAction(), onboardingRoutes.SubscriptionController.checkYourDetails())
            }

            "the user has CL<200 and an email does not exist in the retrieved BPR but one has been retrieved in the auth record" in {
              val bprWithNoEmail = bpr.copy(emailAddress = None)
              val ggEmail        = Email("email")
              val session = SessionData.empty.copy(
                userType = Some(UserType.Individual),
                journeyStatus = Some(
                  SubscriptionReady(
                    individualSubscriptionDetails.copy(
                      emailAddress = ggEmail,
                      emailSource  = EmailSource.GovernmentGateway
                    ),
                    ggCredId
                  )
                )
              )
              val sautr = SAUTR("sautr")

              inSequence {
                mockAuthWithAllRetrievals(
                  ConfidenceLevel.L50,
                  Some(AffinityGroup.Individual),
                  None,
                  Some(sautr.value),
                  Some(ggEmail.value),
                  Set.empty,
                  Some(retrievedGGCredId)
                )
                mockHasSubscription()(Right(None))
                mockGetSession(Future.successful(Right(Some(SessionData.empty))))
                mockGetBusinessPartnerRecord(IndividualBusinessPartnerRecordRequest(Left(sautr), None))(
                  Right(BusinessPartnerRecordResponse(Some(bprWithNoEmail)))
                )
                mockStoreSession(session)(Future.successful(Right(())))
              }

              checkIsRedirect(performAction(), onboardingRoutes.SubscriptionController.checkYourDetails())
            }

            "the session data indicates there is subscription data missing and there is now enough data to proceed" in {
              val session =
                SessionData.empty.copy(
                  journeyStatus = Some(SubscriptionMissingData(bpr, Some(emailAddress), ggCredId, None))
                )

              val updatedSession =
                SessionData.empty.copy(
                  userType = Some(UserType.Individual),
                  journeyStatus = Some(
                    SubscriptionReady(
                      individualSubscriptionDetails.copy(emailSource = EmailSource.ManuallyEntered),
                      ggCredId
                    )
                  )
                )

              inSequence {
                mockAuthWithCl200AndWithAllIndividualRetrievals(nino.value, None, retrievedGGCredId)
                mockHasSubscription()(Right(None))
                mockGetSession(Future.successful(Right(Some(session))))
                mockStoreSession(updatedSession)(Future.successful(Right(())))
              }
              checkIsRedirect(performAction(), onboardingRoutes.SubscriptionController.checkYourDetails())
            }

          }

          "redirect to the subscription confirmation page" when {

            "the session data indicates the user has already subscribed" in {
              val subscriptionStatus = Subscribed(
                SubscribedDetails(
                  individualSubscriptionDetails.name,
                  individualSubscriptionDetails.emailAddress,
                  individualSubscriptionDetails.address,
                  individualSubscriptionDetails.contactName,
                  CgtReference("number"),
                  None,
                  registeredWithId = true
                ),
                ggCredId,
                None,
                None
              )
              val session =
                SessionData.empty.copy(userType = Some(UserType.Individual), journeyStatus = Some(subscriptionStatus))

              inSequence {
                mockAuthWithCgtEnrolmentRetrievals()
                mockGetSession(Future.successful(Right(Some(session))))
              }

              checkIsRedirect(performAction(), controllers.accounts.homepage.routes.HomePageController.homepage())
            }

          }

          "redirect to the register start page" when {

            "the session data indicates that someone has started the registration journey" in {
              List(
                RegistrationStatus.IndividualWantsToRegisterTrust(ggCredId),
                sample[RegistrationStatus.IndividualSupplyingInformation].copy(ggCredId = ggCredId)
              ).foreach { registrationStatus =>
                withClue(s"For registration status $registrationStatus: ") {
                  val session = SessionData.empty
                    .copy(userType = Some(UserType.Individual), journeyStatus = Some(registrationStatus))

                  inSequence {
                    mockAuthWithAllRetrievals(
                      ConfidenceLevel.L50,
                      Some(AffinityGroup.Individual),
                      None,
                      None,
                      None,
                      Set.empty,
                      Some(retrievedGGCredId)
                    )
                    mockHasSubscription()(Right(None))
                    mockGetSession(Future.successful(Right(Some(session))))
                  }

                  checkIsRedirect(performAction(), onboardingRoutes.RegistrationController.selectEntityType())
                }

              }
            }

          }

          "redirect to the registration check your answers page" when {

            "the session indicates the user is ready to register" in {
              val session = SessionData.empty
                .copy(userType = Some(UserType.Individual), journeyStatus = Some(sample[RegistrationReady]))

              inSequence {
                mockAuthWithAllRetrievals(
                  ConfidenceLevel.L50,
                  Some(AffinityGroup.Individual),
                  None,
                  None,
                  None,
                  Set.empty,
                  Some(retrievedGGCredId)
                )
                mockHasSubscription()(Right(None))
                mockGetSession(Future.successful(Right(Some(session))))
              }

              checkIsRedirect(performAction(), onboardingRoutes.RegistrationController.checkYourAnswers())
            }

            "redirect to the registration enter email page" when {

              "the session data indicates that the user is missing an email for registration" in {
                val session = SessionData.empty.copy(
                  userType = Some(UserType.Individual),
                  journeyStatus = Some(
                    IndividualMissingEmail(sample[IndividualName], sample[Address], ggCredId)
                  )
                )

                inSequence {
                  mockAuthWithAllRetrievals(
                    ConfidenceLevel.L50,
                    Some(AffinityGroup.Individual),
                    None,
                    None,
                    None,
                    Set.empty,
                    Some(retrievedGGCredId)
                  )
                  mockHasSubscription()(Right(None))
                  mockGetSession(Future.successful(Right(Some(session))))
                }

                checkIsRedirect(performAction(), emailRoutes.RegistrationEnterEmailController.enterEmail())
              }
            }

          }

          "display an error page" when {
            "the call to get the BPR fails" in {
              inSequence {
                mockAuthWithCl200AndWithAllIndividualRetrievals(nino.value, None, retrievedGGCredId)
                mockHasSubscription()(Right(None))
                mockGetSession(Future.successful(Right(Some(SessionData.empty))))
                mockGetBusinessPartnerRecord(IndividualBusinessPartnerRecordRequest(Right(nino), None))(
                  Left(Error("error"))
                )
              }
              checkIsTechnicalErrorPage(performAction())
            }

            "the call to get a BPR returns no data for a user with CL200" in {
              inSequence {
                mockAuthWithCl200AndWithAllIndividualRetrievals(nino.value, None, retrievedGGCredId)
                mockHasSubscription()(Right(None))
                mockGetSession(Future.successful(Right(Some(SessionData.empty))))
                mockGetBusinessPartnerRecord(IndividualBusinessPartnerRecordRequest(Right(nino), None))(
                  Right(BusinessPartnerRecordResponse(None))
                )
              }

              checkIsTechnicalErrorPage(performAction())
            }

            "the user has CL200 and the call to get BPR succeeds but it cannot be written to session" in {
              val session =
                SessionData.empty.copy(
                  userType      = Some(UserType.Individual),
                  journeyStatus = Some(SubscriptionReady(individualSubscriptionDetails, ggCredId))
                )

              inSequence {
                mockAuthWithCl200AndWithAllIndividualRetrievals(nino.value, None, retrievedGGCredId)
                mockHasSubscription()(Right(None))
                mockGetSession(Future.successful(Right(Some(SessionData.empty))))
                mockGetBusinessPartnerRecord(IndividualBusinessPartnerRecordRequest(Right(nino), None))(
                  Right(BusinessPartnerRecordResponse(Some(bpr)))
                )
                mockStoreSession(session)(Future.successful(Left(Error("Oh no!"))))
              }

              checkIsTechnicalErrorPage(performAction())
            }

            "the user has CL<200 call to get a BPR returns no data for a user with CL200" in {

              inSequence {
                mockAuthWithAllRetrievals(
                  ConfidenceLevel.L50,
                  Some(AffinityGroup.Individual),
                  None,
                  Some("sautr"),
                  None,
                  Set.empty,
                  Some(retrievedGGCredId)
                )
                mockHasSubscription()(Right(None))
                mockGetSession(Future.successful(Right(Some(SessionData.empty))))
                mockGetBusinessPartnerRecord(IndividualBusinessPartnerRecordRequest(Left(SAUTR("sautr")), None))(
                  Right(BusinessPartnerRecordResponse(None))
                )
              }

              checkIsTechnicalErrorPage(performAction())
            }

            "the user has CL<200 and the call to get BPR succeeds but it cannot be written to session" in {
              val session =
                SessionData.empty.copy(
                  userType      = Some(UserType.Individual),
                  journeyStatus = Some(SubscriptionReady(individualSubscriptionDetails, ggCredId))
                )

              inSequence {
                mockAuthWithAllRetrievals(
                  ConfidenceLevel.L50,
                  Some(AffinityGroup.Individual),
                  None,
                  Some("sautr"),
                  None,
                  Set.empty,
                  Some(retrievedGGCredId)
                )
                mockHasSubscription()(Right(None))
                mockGetSession(Future.successful(Right(Some(SessionData.empty))))
                mockGetBusinessPartnerRecord(IndividualBusinessPartnerRecordRequest(Left(SAUTR("sautr")), None))(
                  Right(BusinessPartnerRecordResponse(Some(bpr)))
                )
                mockStoreSession(session)(Future.successful(Left(Error("Oh no!"))))
              }

              checkIsTechnicalErrorPage(performAction())
            }

          }

          "redirect to enter we need more details page" when {

            "there is no email in the BPR or the auth record for a user with CL 200" in {
              val bprWithNoEmail = bpr.copy(emailAddress = None)
              val updatedSession =
                SessionData.empty.copy(
                  userType      = Some(UserType.Individual),
                  journeyStatus = Some(SubscriptionMissingData(bprWithNoEmail, None, ggCredId, None)),
                  needMoreDetailsDetails = Some(
                    NeedMoreDetailsDetails(
                      emailRoutes.SubscriptionEnterEmailController.enterEmail().url,
                      NeedMoreDetailsDetails.AffinityGroup.Individual
                    )
                  )
                )

              inSequence {
                mockAuthWithCl200AndWithAllIndividualRetrievals(nino.value, None, retrievedGGCredId)
                mockHasSubscription()(Right(None))
                mockGetSession(Future.successful(Right(Some(SessionData.empty))))
                mockGetBusinessPartnerRecord(IndividualBusinessPartnerRecordRequest(Right(nino), None))(
                  Right(BusinessPartnerRecordResponse(Some(bprWithNoEmail)))
                )
                mockStoreSession(updatedSession)(Future.successful(Right(())))
              }

              checkIsRedirect(performAction(), controllers.routes.StartController.weNeedMoreDetails())
            }

            "there is no email in the BPR or the auth record for a user with CL < 200" in {
              val bprWithNoEmail = bpr.copy(emailAddress = None)
              val updatedSession =
                SessionData.empty.copy(
                  userType      = Some(UserType.Individual),
                  journeyStatus = Some(SubscriptionMissingData(bprWithNoEmail, None, ggCredId, None)),
                  needMoreDetailsDetails = Some(
                    NeedMoreDetailsDetails(
                      emailRoutes.SubscriptionEnterEmailController.enterEmail().url,
                      NeedMoreDetailsDetails.AffinityGroup.Individual
                    )
                  )
                )

              inSequence {
                mockAuthWithAllRetrievals(
                  ConfidenceLevel.L50,
                  Some(AffinityGroup.Individual),
                  None,
                  Some("sautr"),
                  None,
                  Set.empty,
                  Some(retrievedGGCredId)
                )
                mockHasSubscription()(Right(None))
                mockGetSession(Future.successful(Right(Some(SessionData.empty))))
                mockGetBusinessPartnerRecord(IndividualBusinessPartnerRecordRequest(Left(SAUTR("sautr")), None))(
                  Right(BusinessPartnerRecordResponse(Some(bprWithNoEmail)))
                )
                mockStoreSession(updatedSession)(Future.successful(Right(())))
              }

              checkIsRedirect(performAction(), controllers.routes.StartController.weNeedMoreDetails())
            }

            "the session data indicates there is data missing for subscription and the email address " +
              "is still missing" in {
              val bprWithNoEmail = bpr.copy(emailAddress = None)
              val sessionData =
                SessionData.empty.copy(
                  userType      = Some(UserType.Individual),
                  journeyStatus = Some(SubscriptionMissingData(bprWithNoEmail, None, ggCredId, None))
                )

              inSequence {
                mockAuthWithCl200AndWithAllIndividualRetrievals(nino.value, None, retrievedGGCredId)
                mockHasSubscription()(Right(None))
                mockGetSession(Future.successful(Right(Some(sessionData))))
              }

              checkIsRedirect(performAction(), emailRoutes.SubscriptionEnterEmailController.enterEmail().url)
            }

          }

        }

        "handling trusts" must {

          val sautr     = SAUTR("sautr")
          val trustName = TrustName("trustname")
          val address   = UkAddress("line 1", None, None, None, Postcode("postcode"))
          val sapNumber = SapNumber("sap")
          val bpr       = BusinessPartnerRecord(Some(emailAddress), address, sapNumber, Left(trustName))
          val trustSubscriptionDetails =
            SubscriptionDetails(
              Left(trustName),
              emailAddress,
              bpr.address,
              ContactName(trustName.value),
              bpr.sapNumber,
              EmailSource.BusinessPartnerRecord,
              AddressSource.BusinessPartnerRecord,
              ContactNameSource.DerivedFromBusinessPartnerRecord
            )

          "redirect to the subscription confirmation page" when {

            "the session data indicates the user has already subscribed" in {
              val subscriptionStatus = Subscribed(
                SubscribedDetails(
                  trustSubscriptionDetails.name,
                  trustSubscriptionDetails.emailAddress,
                  trustSubscriptionDetails.address,
                  trustSubscriptionDetails.contactName,
                  CgtReference("number"),
                  None,
                  registeredWithId = false
                ),
                ggCredId,
                None,
                None
              )
              val session =
                SessionData.empty.copy(userType = Some(UserType.Organisation), journeyStatus = Some(subscriptionStatus))

              inSequence {
                mockAuthWithAllTrustRetrievals(sautr, None, retrievedGGCredId)
                mockHasSubscription()(Right(None))
                mockGetSession(Future.successful(Right(Some(session))))
              }

              checkIsRedirect(performAction(), controllers.accounts.homepage.routes.HomePageController.homepage())
            }

          }

          "show an error page" when {

            "there is an error getting the BPR" in {
              inSequence {
                mockAuthWithAllTrustRetrievals(sautr, None, retrievedGGCredId)
                mockHasSubscription()(Right(None))
                mockGetSession(Future.successful(Right(None)))
                mockGetBusinessPartnerRecord(TrustBusinessPartnerRecordRequest(Right(sautr), None))(Left(Error("")))
              }

              checkIsTechnicalErrorPage(performAction())
            }

            "the BPR doesn't contain an organisation name" in {
              inSequence {
                mockAuthWithAllTrustRetrievals(sautr, None, retrievedGGCredId)
                mockHasSubscription()(Right(None))
                mockGetSession(Future.successful(Right(None)))
                mockGetBusinessPartnerRecord(TrustBusinessPartnerRecordRequest(Right(sautr), None))(
                  Right(BusinessPartnerRecordResponse(Some(bpr.copy(name = Right(IndividualName("", ""))))))
                )
              }

              checkIsTechnicalErrorPage(performAction())
            }

            "there is an error updating the session" in {
              inSequence {
                mockAuthWithAllTrustRetrievals(sautr, None, retrievedGGCredId)
                mockHasSubscription()(Right(None))
                mockGetSession(Future.successful(Right(None)))
                mockGetBusinessPartnerRecord(TrustBusinessPartnerRecordRequest(Right(sautr), None))(
                  Right(BusinessPartnerRecordResponse(Some(bpr)))
                )
                mockStoreSession(
                  SessionData.empty.copy(
                    userType      = Some(UserType.Organisation),
                    journeyStatus = Some(SubscriptionReady(trustSubscriptionDetails, ggCredId))
                  )
                )(Future.successful(Left(Error(""))))
              }

              checkIsTechnicalErrorPage(performAction())
            }

            "a BPR cannot be found" in {
              inSequence {
                mockAuthWithAllTrustRetrievals(sautr, None, retrievedGGCredId)
                mockHasSubscription()(Right(None))
                mockGetSession(Future.successful(Right(None)))
                mockGetBusinessPartnerRecord(TrustBusinessPartnerRecordRequest(Right(sautr), None))(
                  Right(BusinessPartnerRecordResponse(None))
                )
              }

              checkIsTechnicalErrorPage(performAction())
            }

          }

          "redirect to check your details" when {

            "there is an email and organisation name in the BPR and the session has been updated" in {
              inSequence {
                mockAuthWithAllTrustRetrievals(sautr, None, retrievedGGCredId)
                mockHasSubscription()(Right(None))
                mockGetSession(Future.successful(Right(None)))
                mockGetBusinessPartnerRecord(TrustBusinessPartnerRecordRequest(Right(sautr), None))(
                  Right(BusinessPartnerRecordResponse(Some(bpr)))
                )
                mockStoreSession(
                  SessionData.empty.copy(
                    userType      = Some(UserType.Organisation),
                    journeyStatus = Some(SubscriptionReady(trustSubscriptionDetails, ggCredId))
                  )
                )(Future.successful(Right(())))
              }

              checkIsRedirect(performAction(), onboardingRoutes.SubscriptionController.checkYourDetails())
            }

            "there is an organisation name in the BPR but no email but there is an email in the " +
              "auth record" in {
              inSequence {
                mockAuthWithAllTrustRetrievals(sautr, Some("email"), retrievedGGCredId)
                mockHasSubscription()(Right(None))
                mockGetSession(Future.successful(Right(None)))
                mockGetBusinessPartnerRecord(TrustBusinessPartnerRecordRequest(Right(sautr), None))(
                  Right(BusinessPartnerRecordResponse(Some(bpr.copy(emailAddress = None))))
                )
                mockStoreSession(
                  SessionData.empty.copy(
                    userType = Some(UserType.Organisation),
                    journeyStatus = Some(
                      SubscriptionReady(
                        trustSubscriptionDetails.copy(
                          emailAddress = Email("email"),
                          emailSource  = EmailSource.GovernmentGateway
                        ),
                        ggCredId
                      )
                    )
                  )
                )(Future.successful(Right(())))
              }

              checkIsRedirect(performAction(), onboardingRoutes.SubscriptionController.checkYourDetails())
            }

            "there are subscription details in session" in {
              val session =
                SessionData.empty.copy(
                  userType      = Some(UserType.Organisation),
                  journeyStatus = Some(SubscriptionReady(trustSubscriptionDetails, ggCredId))
                )

              inSequence {
                mockAuthWithAllTrustRetrievals(sautr, None, retrievedGGCredId)
                mockHasSubscription()(Right(None))
                mockGetSession(Future.successful(Right(Some(session))))
              }

              checkIsRedirect(performAction(), onboardingRoutes.SubscriptionController.checkYourDetails())
            }

            "the session data indicates there is subscription data missing and there is now enough data to proceed" in {
              val session = SessionData.empty.copy(
                userType = Some(UserType.Organisation),
                journeyStatus =
                  Some(SubscriptionMissingData(bpr.copy(emailAddress = None), Some(emailAddress), ggCredId, None))
              )

              val updatedSession =
                SessionData.empty.copy(
                  userType = Some(UserType.Organisation),
                  journeyStatus = Some(
                    SubscriptionReady(
                      trustSubscriptionDetails.copy(emailSource = EmailSource.ManuallyEntered),
                      ggCredId
                    )
                  )
                )

              inSequence {
                mockAuthWithAllTrustRetrievals(sautr, None, retrievedGGCredId)
                mockHasSubscription()(Right(None))
                mockGetSession(Future.successful(Right(Some(session))))
                mockStoreSession(updatedSession)(Future.successful(Right(())))
              }
              checkIsRedirect(performAction(), onboardingRoutes.SubscriptionController.checkYourDetails())
            }

            "the session data indicates there is subscription data missing and there is now enough data to proceed " +
              "for a trust without a trust enrolment" in {
              val session = SessionData.empty.copy(
                userType = Some(UserType.Organisation),
                journeyStatus =
                  Some(SubscriptionMissingData(bpr.copy(emailAddress = None), Some(emailAddress), ggCredId, None))
              )
              val updatedSession =
                SessionData.empty.copy(
                  userType = Some(UserType.Organisation),
                  journeyStatus = Some(
                    SubscriptionReady(
                      trustSubscriptionDetails.copy(emailSource = EmailSource.ManuallyEntered),
                      ggCredId
                    )
                  )
                )

              inSequence {
                mockAuthWithAllRetrievals(
                  ConfidenceLevel.L50,
                  Some(AffinityGroup.Organisation),
                  None,
                  None,
                  Some("email"),
                  Set.empty,
                  Some(retrievedGGCredId)
                )
                mockHasSubscription()(Right(None))
                mockGetSession(Future.successful(Right(Some(session))))
                mockStoreSession(updatedSession)(Future.successful(Right(())))
              }

              checkIsRedirect(performAction(), onboardingRoutes.SubscriptionController.checkYourDetails())
            }

          }

          "redirect to the enter email page" when {

            "the session data indicates there is subscription data missing and no email can be found " +
              "for a trust without a trust enrolment" in {
              val session =
                SessionData.empty.copy(
                  userType      = Some(UserType.Organisation),
                  journeyStatus = Some(SubscriptionMissingData(bpr.copy(emailAddress = None), None, ggCredId, None))
                )

              inSequence {
                mockAuthWithAllRetrievals(
                  ConfidenceLevel.L50,
                  Some(AffinityGroup.Organisation),
                  None,
                  None,
                  None,
                  Set.empty,
                  Some(retrievedGGCredId)
                )
                mockHasSubscription()(Right(None))
                mockGetSession(Future.successful(Right(Some(session))))
              }

              checkIsRedirect(performAction(), emailRoutes.SubscriptionEnterEmailController.enterEmail())
            }

          }

          "redirect to the we need more details page" when {

            "there is no email in the BPR or in the auth record" in {
              val bprWithNoEmail = bpr.copy(emailAddress = None)

              inSequence {
                mockAuthWithAllTrustRetrievals(sautr, None, retrievedGGCredId)
                mockHasSubscription()(Right(None))
                mockGetSession(Future.successful(Right(None)))
                mockGetBusinessPartnerRecord(TrustBusinessPartnerRecordRequest(Right(sautr), None))(
                  Right(BusinessPartnerRecordResponse(Some(bprWithNoEmail)))
                )
                mockStoreSession(
                  SessionData.empty.copy(
                    userType      = Some(UserType.Organisation),
                    journeyStatus = Some(SubscriptionMissingData(bprWithNoEmail, None, ggCredId, None)),
                    needMoreDetailsDetails = Some(
                      NeedMoreDetailsDetails(
                        emailRoutes.SubscriptionEnterEmailController.enterEmail().url,
                        NeedMoreDetailsDetails.AffinityGroup.Organisation
                      )
                    )
                  )
                )(Future.successful(Right(())))
              }

              checkIsRedirect(performAction(), controllers.routes.StartController.weNeedMoreDetails())
            }

            "the session data indicates there is data missing for subscription and the email address " +
              "is still missing" in {
              val bprWithNoEmail = bpr.copy(emailAddress = None)
              val sessionData =
                SessionData.empty.copy(
                  journeyStatus = Some(SubscriptionMissingData(bprWithNoEmail, None, ggCredId, None))
                )

              inSequence {
                mockAuthWithAllTrustRetrievals(sautr, None, retrievedGGCredId)
                mockHasSubscription()(Right(None))
                mockGetSession(Future.successful(Right(Some(sessionData))))
              }

              checkIsRedirect(performAction(), emailRoutes.SubscriptionEnterEmailController.enterEmail().url)
            }

          }

        }

      }

      "the user has a CGT enrolment" when {

        val cgtReference = sample[CgtReference]
        val cgtEnrolment = Enrolment(
          CgtEnrolment.key,
          Seq(EnrolmentIdentifier(CgtEnrolment.cgtReferenceIdentifier, cgtReference.value)),
          ""
        )
        val subscribedDetails = sample[SubscribedDetails].copy(cgtReference = cgtReference)

        val sessionWithSubscribed = SessionData.empty.copy(
          userType      = Some(UserType.Individual),
          journeyStatus = Some(Subscribed(subscribedDetails, ggCredId, None, None))
        )

        "the session data indicates they have subscribed" must {

          "redirect to the homepage" in {
            inSequence {
              mockAuthWithAllRetrievals(
                ConfidenceLevel.L200,
                Some(AffinityGroup.Individual),
                None,
                None,
                None,
                Set(cgtEnrolment),
                Some(retrievedGGCredId)
              )
              mockGetSession(Future.successful(Right(Some(sessionWithSubscribed))))
            }

            checkIsRedirect(performAction(), controllers.accounts.homepage.routes.HomePageController.homepage())
          }

        }

        "the session data does not indicate they have subscribed" must {

          "show an error page" when {

            "the call to get the subscribed details fails" in {
              inSequence {
                mockAuthWithAllRetrievals(
                  ConfidenceLevel.L200,
                  Some(AffinityGroup.Individual),
                  None,
                  None,
                  None,
                  Set(cgtEnrolment),
                  Some(retrievedGGCredId)
                )
                mockGetSession(Future.successful(Right(Some(SessionData.empty))))
                mockGetSubscribedDetails(cgtReference)(Left(Error("")))
              }

              checkIsTechnicalErrorPage(performAction())
            }

            "the session cannot be updated with the subscribed details" in {
              inSequence {
                mockAuthWithAllRetrievals(
                  ConfidenceLevel.L200,
                  Some(AffinityGroup.Individual),
                  None,
                  None,
                  None,
                  Set(cgtEnrolment),
                  Some(retrievedGGCredId)
                )
                mockGetSession(Future.successful(Right(Some(SessionData.empty))))
                mockGetSubscribedDetails(cgtReference)(Right(subscribedDetails))
                mockStoreSession(sessionWithSubscribed.copy(userType = Some(UserType.Individual)))(Future.successful(Left(Error(""))))
              }

              checkIsTechnicalErrorPage(performAction())
            }

          }

          "redirect to the homepage when the subscribed details are obtained and the " +
            "session data has been successfully updated" in {
            inSequence {
              mockAuthWithAllRetrievals(
                ConfidenceLevel.L200,
                Some(AffinityGroup.Individual),
                None,
                None,
                None,
                Set(cgtEnrolment),
                Some(retrievedGGCredId)
              )
              mockGetSession(Future.successful(Right(Some(SessionData.empty))))
              mockGetSubscribedDetails(cgtReference)(Right(subscribedDetails))
              mockStoreSession(sessionWithSubscribed.copy(userType = Some(UserType.Individual)))(Future.successful(Right(())))
            }

            checkIsRedirect(performAction(), controllers.accounts.homepage.routes.HomePageController.homepage())
          }

        }

      }

      "the user has not logged in with government gateway" must {

        val nonGGCreds = Credentials("id", "provider")

        "show an error page" when {

          "the session cannot be updated" in {
            inSequence {
              mockAuthWithAllRetrievals(
                ConfidenceLevel.L50,
                Some(AffinityGroup.Individual),
                None,
                None,
                None,
                Set.empty,
                Some(nonGGCreds)
              )
              mockGetSession(Future.successful(Right(None)))
              mockStoreSession(
                SessionData.empty.copy(
                  userType      = Some(UserType.NonGovernmentGatewayUser),
                  journeyStatus = Some(NonGovernmentGatewayJourney)
                )
              )(
                Future.successful(Left(Error("")))
              )
            }

            checkIsTechnicalErrorPage(performAction())
          }

        }

        "redirect to the 'we only support gg' page" when {

          "the session is updated if the session has not been updated yet" in {
            inSequence {
              mockAuthWithAllRetrievals(
                ConfidenceLevel.L50,
                Some(AffinityGroup.Individual),
                None,
                None,
                None,
                Set.empty,
                Some(nonGGCreds)
              )
              mockGetSession(Future.successful(Right(None)))
              mockStoreSession(
                SessionData.empty.copy(
                  userType      = Some(UserType.NonGovernmentGatewayUser),
                  journeyStatus = Some(NonGovernmentGatewayJourney)
                )
              )(
                Future.successful(Right(()))
              )
            }

            checkIsRedirect(performAction(), controllers.routes.StartController.weOnlySupportGG())
          }

          "the session data has already been updated" in {
            inSequence {
              mockAuthWithAllRetrievals(
                ConfidenceLevel.L50,
                Some(AffinityGroup.Individual),
                None,
                None,
                None,
                Set.empty,
                Some(nonGGCreds)
              )
              mockGetSession(
                Future.successful(
                  Right(
                    Some(
                      SessionData.empty.copy(
                        userType      = Some(UserType.Individual),
                        journeyStatus = Some(NonGovernmentGatewayJourney)
                      )
                    )
                  )
                )
              )
            }

            checkIsRedirect(performAction(), controllers.routes.StartController.weOnlySupportGG())
          }

        }

      }

      "handling agents" must {

        val arn = sample[AgentReferenceNumber]

        val agentsEnrolment = Enrolment(
          AgentsEnrolment.key,
          Seq(EnrolmentIdentifier(AgentsEnrolment.agentReferenceNumberIdentifier, arn.value)),
          ""
        )

        "redirect to the enter client's cgt reference page" when {

          "the session indicates that they are entering a client's details" in {
            inSequence {
              mockAuthWithAllRetrievals(
                ConfidenceLevel.L50,
                Some(AffinityGroup.Agent),
                None,
                None,
                None,
                Set(agentsEnrolment),
                Some(retrievedGGCredId)
              )
              mockGetSession(
                Future.successful(
                  Right(
                    Some(
                      SessionData.empty.copy(
                        userType      = Some(UserType.Agent),
                        journeyStatus = Some(AgentStatus.AgentSupplyingClientDetails(arn, ggCredId, None))
                      )
                    )
                  )
                )
              )
            }

            checkIsRedirect(
              performAction(),
              agents.routes.AgentAccessController.enterClientsCgtRef()
            )
          }

          "there is initially no session data and the journey status is updated successfully" in {
            inSequence {
              mockAuthWithAllRetrievals(
                ConfidenceLevel.L50,
                Some(AffinityGroup.Agent),
                None,
                None,
                None,
                Set(agentsEnrolment),
                Some(retrievedGGCredId)
              )
              mockGetSession(Future.successful(Right(None)))
              mockStoreSession(
                SessionData.empty.copy(
                  userType      = Some(UserType.Agent),
                  journeyStatus = Some(AgentStatus.AgentSupplyingClientDetails(arn, ggCredId, None))
                )
              )(Future.successful(Right(())))
            }

            checkIsRedirect(
              performAction(),
              agents.routes.AgentAccessController.enterClientsCgtRef()
            )
          }

        }

        "show an error page" when {

          "there is initially no session data and the journey status cannot be updated successfully" in {
            inSequence {
              mockAuthWithAllRetrievals(
                ConfidenceLevel.L50,
                Some(AffinityGroup.Agent),
                None,
                None,
                None,
                Set(agentsEnrolment),
                Some(retrievedGGCredId)
              )
              mockGetSession(Future.successful(Right(None)))
              mockStoreSession(
                SessionData.empty.copy(
                  userType      = Some(UserType.Agent),
                  journeyStatus = Some(AgentStatus.AgentSupplyingClientDetails(arn, ggCredId, None))
                )
              )(Future.successful(Left(Error(""))))
            }

            checkIsTechnicalErrorPage(performAction())
          }

        }

      }

    }

    "handling requests to display the need more details page" must {

      def performAction(): Future[Result] = controller.weNeedMoreDetails()(FakeRequest())

      "redirect to the start endpoint" when {

        "there is no continue url in session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              Future.successful(
                Right(
                  Some(
                    SessionData.empty.copy(journeyStatus = Some(sample[JourneyStatus]))
                  )
                )
              )
            )
          }

          checkIsRedirect(performAction(), controllers.routes.StartController.start())
        }

      }

      "display the page" when {

        "there is a continue url in session" in {
          val continueUrl = "/continue/url"

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              Future.successful(
                Right(
                  Some(
                    SessionData.empty.copy(
                      userType      = Some(UserType.Individual),
                      journeyStatus = Some(sample[JourneyStatus]),
                      needMoreDetailsDetails = Some(
                        NeedMoreDetailsDetails(
                          continueUrl,
                          NeedMoreDetailsDetails.AffinityGroup.Individual
                        )
                      )
                    )
                  )
                )
              )
            )
          }

          val result  = performAction()
          val content = contentAsString(result)

          status(result) shouldBe OK
          content        should include(message("weNeedMoreDetails.title"))
          content        should include(continueUrl)
        }
      }

    }

    "handling requests to display the 'we only support gg' page" must {

      def performAction(): Future[Result] = controller.weOnlySupportGG()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(
        performAction, {
          case NonGovernmentGatewayJourney => true
          case _                           => false
        }
      )

      "display the page" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            Future.successful(
              Right(
                Some(
                  SessionData.empty.copy(journeyStatus = Some(NonGovernmentGatewayJourney))
                )
              )
            )
          )
        }

        val result = performAction()
        status(result)          shouldBe OK
        contentAsString(result) should include(message("weOnlySupportGG.title"))
      }
    }

    "handling requests to sign out and register for GG" must {

      def performAction(sessionData: Seq[(String, String)]): Future[Result] =
        controller.signOutAndRegisterForGG()(FakeRequest().withSession(sessionData: _*))

      behave like redirectToStartWhenInvalidJourney(
        () => performAction(Seq.empty), {
          case NonGovernmentGatewayJourney => true
          case _                           => false
        }
      )

      "trash the session adn redirect to the gg registration service" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            Future.successful(
              Right(
                Some(
                  SessionData.empty
                    .copy(userType = Some(UserType.Individual), journeyStatus = Some(NonGovernmentGatewayJourney))
                )
              )
            )
          )
        }

        val result = performAction(Seq("key" -> "value"))
        checkIsRedirect(result, viewConfig.ggCreateAccountUrl)
        session(result).data shouldBe Map.empty
      }

    }

    "handling requests to sign out and sign in" must {

      def performAction(sessionData: Seq[(String, String)]): Future[Result] =
        controller.signOutAndSignIn()(FakeRequest().withSession(sessionData: _*))

      behave like redirectToStartWhenInvalidJourney(
        () => performAction(Seq.empty), {
          case NonGovernmentGatewayJourney => true
          case _                           => false
        }
      )

      "trash the session adn redirect to the gg registration service" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            Future.successful(
              Right(
                Some(
                  SessionData.empty.copy(journeyStatus = Some(NonGovernmentGatewayJourney))
                )
              )
            )
          )
        }

        val result = performAction(Seq("key" -> "value"))
        checkIsRedirect(result, controllers.routes.StartController.start())
        session(result).data shouldBe Map.empty
      }

    }

    "handling requests to keep alive" must {

      "return an ok response with an empty body" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(Future.successful(Right(None)))
        }

        val result = controller.keepAlive()(FakeRequest())
        status(result)          shouldBe OK
        contentAsString(result) shouldBe ""
      }

    }

    "handling requests to display the timed out page" must {

      "display the page" in {
        val result = controller.timedOut()(FakeRequest())
        status(result)          shouldBe OK
        contentAsString(result) should include(message("timed-out.title"))
      }

    }

  }

}
