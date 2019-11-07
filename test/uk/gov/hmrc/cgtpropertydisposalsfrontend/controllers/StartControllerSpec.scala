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
import org.joda.time.{LocalDate => JodaLocalDate}
import org.scalacheck.ScalacheckShapeless._
import play.api.i18n.MessagesApi
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.{AnyContent, Request, Result}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.ConfidenceLevel.L50
import uk.gov.hmrc.auth.core.retrieve.Credentials
import uk.gov.hmrc.auth.core.{AffinityGroup, AuthConnector, ConfidenceLevel, Enrolment, EnrolmentIdentifier}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.RegistrationStatus.{IndividualMissingEmail, RegistrationReady}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.SubscriptionStatus._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{AlreadySubscribedWithDifferentGGAccount, RegistrationStatus, Subscribed, SubscriptionStatus}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.UserType.Individual
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address.UkAddress
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.bpr.BusinessPartnerRecordRequest._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.bpr.{BusinessPartnerRecord, BusinessPartnerRecordRequest, BusinessPartnerRecordResponse}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.{CgtReference, GGCredId, NINO, SAUTR}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.{ContactName, IndividualName, TrustName}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.{BusinessPartnerRecordService, SubscriptionService}
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class StartControllerSpec extends ControllerSpec with AuthSupport with SessionSupport with IvBehaviourSupport {

  val mockBprService = mock[BusinessPartnerRecordService]

  override val overrideBindings =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore),
      bind[BusinessPartnerRecordService].toInstance(mockBprService),
      bind[SubscriptionService].toInstance(mockSubscriptionService)
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

  val nino                 = NINO("AB123456C")
  val name                 = IndividualName("forename", "surname")
  val trustName            = TrustName("trust")
  val retrievedDateOfBirth = JodaLocalDate.parse("2000-04-10")
  val emailAddress         = Email("email")

  val individual        = Individual(Right(nino), None)
  val retrievedGGCredId = Credentials("gg", "GovernmentGateway")
  val ggCredId          = GGCredId(retrievedGGCredId.providerId)

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
            mockGetSession(Future.successful(Right(Some(SessionData.empty.copy(
              journeyStatus = Some(AlreadySubscribedWithDifferentGGAccount)
            )))))
          }

          checkIsRedirect(performAction(), routes.SubscriptionController.alreadySubscribedWithDifferentGGAccount())
        }

      }

      "handling non trust organisations" must {

        val determiningIfOrganisationIsTrustSession =
          SessionData.empty.copy(journeyStatus = Some(SubscriptionStatus.DeterminingIfOrganisationIsTrust(ggCredId, None, None)))

        lazy val needMoreDetailsContinueUrl = routes.DeterminingIfOrganisationIsTrustController.doYouWantToReportForATrust().url

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
              mockStoreSession(determiningIfOrganisationIsTrustSession.copy(
                needMoreDetailsContinueUrl = Some(needMoreDetailsContinueUrl)
              ))(Future.successful(Left(Error(""))))
            }

            checkIsTechnicalErrorPage(performAction(FakeRequest()))
          }

        }

        "redirect to the 'we need more details' page with a continue to the 'are you " +
          "reporting for a trust' page " when {

          "the session already has the relevant journey status in it" in {
            val journey = DeterminingIfOrganisationIsTrust(ggCredId, Some(true), Some(true))
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
                mockStoreSession(sessionData.copy(
                needMoreDetailsContinueUrl = Some(needMoreDetailsContinueUrl)
              ))(Future.successful(Right(())))
            }

            checkIsRedirect(performAction(FakeRequest()), routes.StartController.weNeedMoreDetails())
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
              mockStoreSession(determiningIfOrganisationIsTrustSession.copy(
                needMoreDetailsContinueUrl = Some(needMoreDetailsContinueUrl)
              ))(Future.successful(Right(())))

            }

            checkIsRedirect(performAction(FakeRequest()), routes.StartController.weNeedMoreDetails())
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
                    journeyStatus              = Some(SubscriptionStatus.TryingToGetIndividualsFootprint(None, None, None, ggCredId)),
                    needMoreDetailsContinueUrl = Some(routes.InsufficientConfidenceLevelController.doYouHaveNINO().url)
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
                    needMoreDetailsContinueUrl = Some(routes.InsufficientConfidenceLevelController.doYouHaveNINO().url)
                  )
                )(Future.successful(Right(())))
              }

              checkIsRedirect(performAction(), routes.StartController.weNeedMoreDetails())
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
                          )
                        )
                      )
                    )
                  )
                )
              }

              checkIsRedirect(performAction(), routes.InsufficientConfidenceLevelController.doYouHaveNINO())
            }

          }

        }

        "the session data has been updated to indicate so" when {

          "redirect to the do you have a nino page" in {
            def sessionData = SessionData.empty.copy(
              journeyStatus = Some(TryingToGetIndividualsFootprint(Some(false), None, None, ggCredId))
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

            checkIsRedirect(performAction(), routes.InsufficientConfidenceLevelController.doYouHaveNINO())
          }

        }

        "the session data indicates that some subscription details are missing" must {

          val bpr = models.bpr.BusinessPartnerRecord(
            Some(emailAddress),
            sample[Address],
            "sap",
            Right(name)
          )

          "redirect to check your details" when {

            "the session data indicates there is subscription data missing and there is now enough " +
              "data to proceed to the check your details page" in {
              val individualSubscriptionDetails =
                SubscriptionDetails(
                  Right(name),
                  emailAddress,
                  bpr.address,
                  ContactName(name.makeSingleName),
                  bpr.sapNumber
                )

              val session = SessionData.empty.copy(journeyStatus = Some(SubscriptionMissingData(bpr)))

              val updatedSession =
                SessionData.empty.copy(journeyStatus = Some(SubscriptionReady(individualSubscriptionDetails)))

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
              checkIsRedirect(performAction(), routes.SubscriptionController.checkYourDetails())
            }
          }

          "redirect to enter email" when {

            "the email address is still missing" in {
              val bprWithNoEmail = bpr.copy(emailAddress = None)
              val sessionData =
                SessionData.empty.copy(journeyStatus = Some(SubscriptionMissingData(bprWithNoEmail)))

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

              checkIsRedirect(performAction(), email.routes.SubscriptionEnterEmailController.enterEmail().url)
            }

          }

        }

      }

      "the user is not enrolled and is not subscribed in ETMP" when {

        val name = sample[IndividualName]

        val bpr = models.bpr.BusinessPartnerRecord(
          Some(emailAddress),
          sample[UkAddress],
          "sap",
          Right(name)
        )

        "handling individuals" must {

          val individualSubscriptionDetails = SubscriptionDetails(
            Right(name),
            emailAddress,
            bpr.address,
            ContactName(name.makeSingleName()),
            bpr.sapNumber
          )

          "redirect to check subscription details" when {

            "there are subscription details in session" in {
              val session =
                SessionData.empty.copy(journeyStatus = Some(SubscriptionReady(individualSubscriptionDetails)))
              inSequence {
                mockAuthWithCl200AndWithAllIndividualRetrievals(nino.value, None)
                mockHasSubscription()(Right(None))
                mockGetSession(Future.successful(Right(Some(session))))
              }

              checkIsRedirect(performAction(), routes.SubscriptionController.checkYourDetails())
            }

            "there is no session data and a bpr is successfully retrieved using the retrieved auth NINO" in {
              List(
                Some(SessionData.empty),
                None
              ).foreach { maybeSession =>
                val session =
                  SessionData.empty.copy(journeyStatus = Some(SubscriptionReady(individualSubscriptionDetails)))

                inSequence {
                  mockAuthWithCl200AndWithAllIndividualRetrievals(nino.value, None)
                  mockHasSubscription()(Right(None))
                  mockGetSession(Future.successful(Right(maybeSession)))
                  mockGetBusinessPartnerRecord(IndividualBusinessPartnerRecordRequest(Right(nino), None))(
                    Right(BusinessPartnerRecordResponse(Some(bpr)))
                  )
                  mockStoreSession(session)(Future.successful(Right(())))
                }

                checkIsRedirect(performAction(), routes.SubscriptionController.checkYourDetails())
              }
            }

            "the user has CL<200 and there is no session data and a bpr is successfully retrieved " +
              "using the retrieved auth SA UTR" in {
              List(
                Some(SessionData.empty),
                None
              ).foreach { maybeSession =>
                val session =
                  SessionData.empty.copy(journeyStatus = Some(SubscriptionReady(individualSubscriptionDetails)))

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

                checkIsRedirect(performAction(), routes.SubscriptionController.checkYourDetails())
              }
            }

            "an email does not exist in the retrieved BPR but one has been retrieved in the auth record" in {
              val bprWithNoEmail = bpr.copy(emailAddress = None)
              val ggEmail        = Email("email")
              val session = SessionData.empty.copy(
                journeyStatus = Some(
                  SubscriptionReady(individualSubscriptionDetails.copy(emailAddress = ggEmail))
                )
              )

              inSequence {
                mockAuthWithCl200AndWithAllIndividualRetrievals(nino.value, Some(ggEmail.value))
                mockHasSubscription()(Right(None))
                mockGetSession(Future.successful(Right(Some(SessionData.empty))))
                mockGetBusinessPartnerRecord(IndividualBusinessPartnerRecordRequest(Right(nino), None))(
                  Right(BusinessPartnerRecordResponse(Some(bprWithNoEmail)))
                )
                mockStoreSession(session)(Future.successful(Right(())))
              }

              checkIsRedirect(performAction(), routes.SubscriptionController.checkYourDetails())
            }

            "the user has CL<200 and an email does not exist in the retrieved BPR but one has been retrieved in the auth record" in {
              val bprWithNoEmail = bpr.copy(emailAddress = None)
              val ggEmail        = Email("email")
              val session = SessionData.empty.copy(
                journeyStatus = Some(
                  SubscriptionReady(individualSubscriptionDetails.copy(emailAddress = ggEmail))
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

              checkIsRedirect(performAction(), routes.SubscriptionController.checkYourDetails())
            }

            "the session data indicates there is subscription data missing and there is now enough data to proceed" in {
              val session = SessionData.empty.copy(journeyStatus = Some(SubscriptionMissingData(bpr)))
              val updatedSession =
                SessionData.empty.copy(journeyStatus = Some(SubscriptionReady(individualSubscriptionDetails)))

              inSequence {
                mockAuthWithCl200AndWithAllIndividualRetrievals(nino.value, None)
                mockHasSubscription()(Right(None))
                mockGetSession(Future.successful(Right(Some(session))))
                mockStoreSession(updatedSession)(Future.successful(Right(())))
              }
              checkIsRedirect(performAction(), routes.SubscriptionController.checkYourDetails())
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
                )
              )
              val session = SessionData.empty.copy(journeyStatus = Some(subscriptionStatus))

              inSequence {
                mockAuthWithCgtEnrolmentRetrievals()
                mockGetSession(Future.successful(Right(Some(session))))
              }

              checkIsRedirect(performAction(), routes.HomeController.homepage())
            }

          }

          "redirect to the register start page" when {

            "the session data indicates that someone has started the registration journey" in {
              List(
                RegistrationStatus.IndividualWantsToRegisterTrust,
                sample[RegistrationStatus.IndividualSupplyingInformation]
              ).foreach { registrationStatus =>
                withClue(s"For registration status $registrationStatus: ") {
                  val session = SessionData.empty.copy(journeyStatus = Some(registrationStatus))

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

                  checkIsRedirect(performAction(), routes.RegistrationController.selectEntityType())
                }

              }
            }

          }

          "redirect to the registration check your answers page" when {

            "the session indicates the user is ready to register" in {
              val session = SessionData.empty.copy(journeyStatus = Some(sample[RegistrationReady]))

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

              checkIsRedirect(performAction(), routes.RegistrationController.checkYourAnswers())
            }

            "redirect to the registration enter email page" when {

              "the session data indicates that the user is missing an email for registration" in {
                val session = SessionData.empty.copy(
                  journeyStatus = Some(
                    IndividualMissingEmail(sample[IndividualName], sample[Address])
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

                checkIsRedirect(performAction(), email.routes.RegistrationEnterEmailController.enterEmail())
              }
            }

          }

          "display an error page" when {
            "the call to get the BPR fails" in {
              inSequence {
                mockAuthWithCl200AndWithAllIndividualRetrievals(nino.value, None)
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
                mockAuthWithCl200AndWithAllIndividualRetrievals(nino.value, None)
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
                SessionData.empty.copy(journeyStatus = Some(SubscriptionReady(individualSubscriptionDetails)))

              inSequence {
                mockAuthWithCl200AndWithAllIndividualRetrievals(nino.value, None)
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
                SessionData.empty.copy(journeyStatus = Some(SubscriptionReady(individualSubscriptionDetails)))

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
                  journeyStatus              = Some(SubscriptionMissingData(bprWithNoEmail)),
                  needMoreDetailsContinueUrl = Some(email.routes.SubscriptionEnterEmailController.enterEmail().url)
                )

              inSequence {
                mockAuthWithCl200AndWithAllIndividualRetrievals(nino.value, None)
                mockHasSubscription()(Right(None))
                mockGetSession(Future.successful(Right(Some(SessionData.empty))))
                mockGetBusinessPartnerRecord(IndividualBusinessPartnerRecordRequest(Right(nino), None))(
                  Right(BusinessPartnerRecordResponse(Some(bprWithNoEmail)))
                )
                mockStoreSession(updatedSession)(Future.successful(Right(())))
              }

              checkIsRedirect(performAction(), routes.StartController.weNeedMoreDetails())
            }

            "there is no email in the BPR or the auth record for a user with CL < 200" in {
              val bprWithNoEmail = bpr.copy(emailAddress = None)
              val updatedSession =
                SessionData.empty.copy(
                  journeyStatus              = Some(SubscriptionMissingData(bprWithNoEmail)),
                  needMoreDetailsContinueUrl = Some(email.routes.SubscriptionEnterEmailController.enterEmail().url)
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

              checkIsRedirect(performAction(), routes.StartController.weNeedMoreDetails())
            }

            "the session data indicates there is data missing for subscription and the email address " +
              "is still missing" in {
              val bprWithNoEmail = bpr.copy(emailAddress = None)
              val sessionData =
                SessionData.empty.copy(journeyStatus = Some(SubscriptionMissingData(bprWithNoEmail)))

              inSequence {
                mockAuthWithCl200AndWithAllIndividualRetrievals(nino.value, None)
                mockHasSubscription()(Right(None))
                mockGetSession(Future.successful(Right(Some(sessionData))))
              }

              checkIsRedirect(performAction(), email.routes.SubscriptionEnterEmailController.enterEmail().url)
            }

          }

        }

        "handling trusts" must {

          val sautr     = SAUTR("sautr")
          val trustName = TrustName("trustname")
          val address   = UkAddress("line 1", None, None, None, "postcode")
          val sapNumber = "sap"
          val bpr       = BusinessPartnerRecord(Some(emailAddress), address, sapNumber, Left(trustName))
          val trustSubscriptionDetails =
            SubscriptionDetails(Left(trustName), emailAddress, bpr.address, ContactName(trustName.value), bpr.sapNumber)

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
                )
              )
              val session = SessionData.empty.copy(journeyStatus = Some(subscriptionStatus))

              inSequence {
                mockAuthWithAllTrustRetrievals(sautr, None)
                mockHasSubscription()(Right(None))
                mockGetSession(Future.successful(Right(Some(session))))
              }

              checkIsRedirect(performAction(), routes.HomeController.homepage())
            }

          }

          "show an error page" when {

            "there is an error getting the BPR" in {
              inSequence {
                mockAuthWithAllTrustRetrievals(sautr, None)
                mockHasSubscription()(Right(None))
                mockGetSession(Future.successful(Right(None)))
                mockGetBusinessPartnerRecord(TrustBusinessPartnerRecordRequest(Right(sautr), None))(Left(Error("")))
              }

              checkIsTechnicalErrorPage(performAction())
            }

            "the BPR doesn't contain an organisation name" in {
              inSequence {
                mockAuthWithAllTrustRetrievals(sautr, None)
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
                mockAuthWithAllTrustRetrievals(sautr, None)
                mockHasSubscription()(Right(None))
                mockGetSession(Future.successful(Right(None)))
                mockGetBusinessPartnerRecord(TrustBusinessPartnerRecordRequest(Right(sautr), None))(
                  Right(BusinessPartnerRecordResponse(Some(bpr)))
                )
                mockStoreSession(
                  SessionData.empty.copy(journeyStatus = Some(SubscriptionReady(trustSubscriptionDetails)))
                )(Future.successful(Left(Error(""))))
              }

              checkIsTechnicalErrorPage(performAction())
            }

            "a BPR cannot be found" in {
              inSequence {
                mockAuthWithAllTrustRetrievals(sautr, None)
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
                mockAuthWithAllTrustRetrievals(sautr, None)
                mockHasSubscription()(Right(None))
                mockGetSession(Future.successful(Right(None)))
                mockGetBusinessPartnerRecord(TrustBusinessPartnerRecordRequest(Right(sautr), None))(
                  Right(BusinessPartnerRecordResponse(Some(bpr)))
                )
                mockStoreSession(
                  SessionData.empty.copy(journeyStatus = Some(SubscriptionReady(trustSubscriptionDetails)))
                )(Future.successful(Right(())))
              }

              checkIsRedirect(performAction(), routes.SubscriptionController.checkYourDetails())
            }

            "there is an organisation name in the BPR but no email but there is an email in the " +
              "auth record" in {
              inSequence {
                mockAuthWithAllTrustRetrievals(sautr, Some("email"))
                mockHasSubscription()(Right(None))
                mockGetSession(Future.successful(Right(None)))
                mockGetBusinessPartnerRecord(TrustBusinessPartnerRecordRequest(Right(sautr), None))(
                  Right(BusinessPartnerRecordResponse(Some(bpr.copy(emailAddress = None))))
                )
                mockStoreSession(
                  SessionData.empty.copy(
                    journeyStatus =
                      Some(SubscriptionReady(trustSubscriptionDetails.copy(emailAddress = Email("email"))))
                  )
                )(Future.successful(Right(())))
              }

              checkIsRedirect(performAction(), routes.SubscriptionController.checkYourDetails())
            }

            "there are subscription details in session" in {
              val session = SessionData.empty.copy(journeyStatus = Some(SubscriptionReady(trustSubscriptionDetails)))
              inSequence {
                mockAuthWithAllTrustRetrievals(sautr, None)
                mockHasSubscription()(Right(None))
                mockGetSession(Future.successful(Right(Some(session))))
              }

              checkIsRedirect(performAction(), routes.SubscriptionController.checkYourDetails())
            }

            "the session data indicates there is subscription data missing and there is now enough data to proceed" in {
              val session = SessionData.empty.copy(journeyStatus = Some(SubscriptionMissingData(bpr)))
              val updatedSession =
                SessionData.empty.copy(journeyStatus = Some(SubscriptionReady(trustSubscriptionDetails)))

              inSequence {
                mockAuthWithAllTrustRetrievals(sautr, None)
                mockHasSubscription()(Right(None))
                mockGetSession(Future.successful(Right(Some(session))))
                mockStoreSession(updatedSession)(Future.successful(Right(())))
              }
              checkIsRedirect(performAction(), routes.SubscriptionController.checkYourDetails())
            }

            "the session data indicates there is subscription data missing and there is now enough data to proceed " +
              "for a trust without a trust enrolment" in {
              val session = SessionData.empty.copy(journeyStatus = Some(SubscriptionMissingData(bpr)))
              val updatedSession =
                SessionData.empty.copy(journeyStatus = Some(SubscriptionReady(trustSubscriptionDetails)))

              inSequence {
                mockAuthWithAllRetrievals(
                  ConfidenceLevel.L50,
                  Some(AffinityGroup.Organisation),
                  None,
                  None,
                  Some("email"),
                  Set.empty,
                  Some(retrievedGGCredId))
                mockHasSubscription()(Right(None))
                mockGetSession(Future.successful(Right(Some(session))))
                mockStoreSession(updatedSession)(Future.successful(Right(())))
              }

              checkIsRedirect(performAction(), routes.SubscriptionController.checkYourDetails())
            }


          }

          "redirect to the enter email page" when {

            "the session data indicates there is subscription data missing and no email can be found " +
              "for a trust without a trust enrolment" in {
              val session = SessionData.empty.copy(journeyStatus = Some(SubscriptionMissingData(bpr.copy(emailAddress = None))))

              inSequence {
                mockAuthWithAllRetrievals(
                  ConfidenceLevel.L50,
                  Some(AffinityGroup.Organisation),
                  None,
                  None,
                  None,
                  Set.empty,
                  Some(retrievedGGCredId))
                mockHasSubscription()(Right(None))
                mockGetSession(Future.successful(Right(Some(session))))
              }

              checkIsRedirect(performAction(), email.routes.SubscriptionEnterEmailController.enterEmail())
            }

          }

          "redirect to the we need more details page" when {

            "there is no email in the BPR or in the auth record" in {
              val bprWithNoEmail = bpr.copy(emailAddress = None)

              inSequence {
                mockAuthWithAllTrustRetrievals(sautr, None)
                mockHasSubscription()(Right(None))
                mockGetSession(Future.successful(Right(None)))
                mockGetBusinessPartnerRecord(TrustBusinessPartnerRecordRequest(Right(sautr), None))(
                  Right(BusinessPartnerRecordResponse(Some(bprWithNoEmail)))
                )
                mockStoreSession(
                  SessionData.empty.copy(
                    journeyStatus              = Some(SubscriptionMissingData(bprWithNoEmail)),
                    needMoreDetailsContinueUrl = Some(email.routes.SubscriptionEnterEmailController.enterEmail().url)
                  )
                )(Future.successful(Right(())))
              }

              checkIsRedirect(performAction(), routes.StartController.weNeedMoreDetails())
            }

            "the session data indicates there is data missing for subscription and the email address " +
              "is still missing" in {
              val bprWithNoEmail = bpr.copy(emailAddress = None)
              val sessionData =
                SessionData.empty.copy(journeyStatus = Some(SubscriptionMissingData(bprWithNoEmail)))

              inSequence {
                mockAuthWithAllTrustRetrievals(sautr, None)
                mockHasSubscription()(Right(None))
                mockGetSession(Future.successful(Right(Some(sessionData))))
              }

              checkIsRedirect(performAction(), email.routes.SubscriptionEnterEmailController.enterEmail().url)
            }

          }

        }

      }

      "the user has a CGT enrolment" when {

        val cgtReference = sample[CgtReference]
        val cgtEnrolment = Enrolment(
          "HMRC-CGT-PD",
          Seq(EnrolmentIdentifier("CGTPDRef", cgtReference.value)),
        "")
        val subscribedDetails = sample[SubscribedDetails].copy(cgtReference = cgtReference)

        val sessionWithSubscribed = SessionData.empty.copy(
          journeyStatus = Some(Subscribed(subscribedDetails))
        )

        "the session data indicates they have subscribed" must {

          "redirect to the homepage" in {
            inSequence{
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

            checkIsRedirect(performAction(), routes.HomeController.homepage())
          }

        }

        "the session data does not indicate they have subscribed" must {

          "show an error page" when {

            "the call to get the subscribed details fails" in {
              inSequence{
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
              inSequence{
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
                mockStoreSession(sessionWithSubscribed)(Future.successful(Left(Error(""))))
              }

              checkIsTechnicalErrorPage(performAction())
            }

          }

          "redirect to the homepage when the subscribed details are obtained and the " +
            "ssion data has been successfully updated" in {
            inSequence{
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
              mockStoreSession(sessionWithSubscribed)(Future.successful(Right(())))
            }

            checkIsRedirect(performAction(), routes.HomeController.homepage())
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
                    SessionData.empty.copy(
                      journeyStatus = Some(sample[JourneyStatus])
                    )
                  )
                )
              )
            )
          }

          checkIsRedirect(performAction(), routes.StartController.start())
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
                      journeyStatus              = Some(sample[JourneyStatus]),
                      needMoreDetailsContinueUrl = Some(continueUrl)
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

  }

}
