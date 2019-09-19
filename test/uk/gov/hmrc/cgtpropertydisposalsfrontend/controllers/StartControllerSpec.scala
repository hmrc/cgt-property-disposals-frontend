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

import java.time.LocalDate

import cats.data.EitherT
import cats.instances.future._
import org.scalacheck.ScalacheckShapeless._
import org.joda.time.{LocalDate => JodaLocalDate}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.{AnyContent, Request, Result}
import play.api.test.FakeRequest
import uk.gov.hmrc.auth.core.ConfidenceLevel.L50
import uk.gov.hmrc.auth.core.{AffinityGroup, AuthConnector, ConfidenceLevel}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Address.UkAddress
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{RegistrationStatus, SubscriptionStatus}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.SubscriptionStatus._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.UserType.{Individual, Trust}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.BusinessPartnerRecordService
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class StartControllerSpec extends ControllerSpec with AuthSupport with SessionSupport with IvBehaviourSupport {

  val mockService = mock[BusinessPartnerRecordService]

  override val overrideBindings =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore),
      bind[BusinessPartnerRecordService].toInstance(mockService)
    )

  override lazy val additionalConfig = ivConfig(useRelativeUrls = false)

  lazy val controller = instanceOf[StartController]

  def mockGetBusinessPartnerRecord(entity: Either[Trust, Individual], requiresNameMatch: Boolean)(
    result: Either[Error, BusinessPartnerRecord]
  ) =
    (mockService
      .getBusinessPartnerRecord(_: Either[Trust, Individual], _: Boolean)(_: HeaderCarrier))
      .expects(entity, requiresNameMatch, *)
      .returning(EitherT.fromEither[Future](result))

  val nino = NINO("AB123456C")
  val name = Name("forename", "surname")
  val trustName = TrustName("trust")
  val retrievedDateOfBirth = JodaLocalDate.parse("2000-04-10")
  val dateOfBirth = DateOfBirth(LocalDate.of(2000, 4, 10))
  val emailAddress = "email"
  val bpr = BusinessPartnerRecord(
    Some(emailAddress),
    UkAddress("line1", None, None, None, "postcode"),
    "sap",
    Some("org")
  )
  val individual = Individual(Right(nino), name, Some(dateOfBirth), None)

  "The StartController" when {

    "handling requests to start a journey" when {

      def performAction(rh: Request[AnyContent] = FakeRequest()): Future[Result] =
        controller.start()(rh)

      "handling non trust organisations" must {

        val nonTrustOrganisationSession =
          SessionData.empty.copy(journeyStatus = Some(SubscriptionStatus.UnregisteredTrust))

        "show an error page" when {

          "there is an error storing the session in mongo" in {

            inSequence {
              mockAuthWithAllRetrievals(ConfidenceLevel.L50, Some(AffinityGroup.Organisation), None, None, None, None, None, Set.empty)
              mockGetSession(Future.successful(Right(Some(SessionData.empty))))
              mockStoreSession(nonTrustOrganisationSession)(Future.successful(Left(Error(""))))
            }

            checkIsTechnicalErrorPage(performAction(FakeRequest()))
          }

        }

        "redirect to the register trust page" when {
          "the session does not need updating" in {
            inSequence {
              mockAuthWithAllRetrievals(ConfidenceLevel.L50, Some(AffinityGroup.Organisation), None, None, None, None, None, Set.empty)
              mockGetSession(Future.successful(Right(Some(nonTrustOrganisationSession))))
            }

            checkIsRedirect(performAction(FakeRequest()), routes.RegisterTrustController.registerYourTrust())
          }

          "the session data is updated when it is required" in {
            inSequence {
              mockAuthWithAllRetrievals(ConfidenceLevel.L50, Some(AffinityGroup.Organisation), None, None, None, None, None, Set.empty)
              mockGetSession(Future.successful(Right(None)))
              mockStoreSession(nonTrustOrganisationSession)(Future.successful(Right(())))

            }

            checkIsRedirect(performAction(FakeRequest()), routes.RegisterTrustController.registerYourTrust())
          }
        }


      }

      "handling individual users with insufficient confidence level" when {

        "the session has not been updated to indicate so" must {

          "show an error" when {

            "the session cannot be updated when there is not a nino in the auth record" in {
              inSequence {
                mockAuthWithAllRetrievals(ConfidenceLevel.L50, Some(AffinityGroup.Individual), None, None, Some(name), None, None, Set.empty)
                mockGetSession(Future.successful(Right(Some(SessionData.empty))))
                mockStoreSession(SessionData.empty.copy(journeyStatus = Some(SubscriptionStatus.IndividualWithInsufficientConfidenceLevel(None, None, name, None))))(Future.successful(Left(Error(""))))
              }

              checkIsTechnicalErrorPage(performAction())
            }

            "a name cannot be retrieved" in {
              mockAuthWithAllRetrievals(ConfidenceLevel.L50, Some(AffinityGroup.Individual), None, None, None, None, None, Set.empty)

              checkIsTechnicalErrorPage(performAction())

            }

          }

          "redirect to the IV journey" when {

            "the user does not have sufficient confidence level and there is a NINO in the auth record" in {
              val request = FakeRequest("GET", "/uri")

              inSequence {
                mockAuthWithAllRetrievals(ConfidenceLevel.L50, Some(AffinityGroup.Individual), Some("nino"), None, Some(name), None, None, Set.empty)
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
                  bind[BusinessPartnerRecordService].toInstance(mockService)
                )

              override lazy val additionalConfig = ivConfig(useRelativeUrls = true)
              val controller = instanceOf[StartController]
              val request = FakeRequest("GET", "/uri")

              inSequence {
                mockAuthWithAllRetrievals(ConfidenceLevel.L50, Some(AffinityGroup.Individual), Some("nino"), None, Some(name), None, None, Set.empty)
                mockGetSession(Future.successful(Right(Some(SessionData.empty))))
              }

              checkIsRedirectToIv(controller.start()(request), true)
            }

          }

          "redirect to the do you have a nino page" when {

            "the user does not have sufficient confidence level and there is no NINO in the auth record" in {
              inSequence {
                mockAuthWithAllRetrievals(ConfidenceLevel.L50, Some(AffinityGroup.Individual), None, None, Some(name), None, None, Set.empty)
                mockGetSession(Future.successful(Right(Some(SessionData.empty))))
                mockStoreSession(SessionData.empty.copy(journeyStatus = Some(
                  SubscriptionStatus.IndividualWithInsufficientConfidenceLevel(None, None, name, None)
                )))(Future.successful(Right(())))
              }

              checkIsRedirect(performAction(), routes.InsufficientConfidenceLevelController.doYouHaveNINO())
            }

            "the session data indicates they do not have sufficient confidence level" in {
              inSequence {
                mockAuthWithAllRetrievals(ConfidenceLevel.L50, Some(AffinityGroup.Individual), None, None, Some(name), None, None, Set.empty)
                mockGetSession(Future.successful(Right(Some(SessionData.empty.copy(journeyStatus = Some(
                  SubscriptionStatus.IndividualWithInsufficientConfidenceLevel(None, None, name, None)
                ))))))
              }

              checkIsRedirect(performAction(), routes.InsufficientConfidenceLevelController.doYouHaveNINO())
            }

          }

        }


        "the session data has been updated to indicate so" when {

          "the user has not indicated that they have an SA UTR" must {

            "redirect to the do you have a nino page" in {
              def sessionData = SessionData.empty.copy(
                journeyStatus = Some(
                  IndividualWithInsufficientConfidenceLevel(
                    Some(false), None, name, None))
              )

              inSequence {
                mockAuthWithAllRetrievals(L50, Some(AffinityGroup.Individual), None, None, Some(name), None, None, Set.empty)
                mockGetSession(Future.successful(Right(Some(sessionData))))
              }

              checkIsRedirect(performAction(), routes.InsufficientConfidenceLevelController.doYouHaveNINO())
            }

          }

          "the user has indicated that they have an SA UTR" must {

            val sautr = SAUTR("1234567890")

            val email = Email("email")

            def sessionData(email: Option[Email]) = SessionData.empty.copy(
              journeyStatus = Some(
                IndividualWithInsufficientConfidenceLevel(
                  Some(false), Some(HasSAUTR(Some(sautr))), name, email))
            )

            def individual(email: Option[Email]) =
              Individual(Left(sautr), name, None, email)

            val individualSubscriptionDetails = SubscriptionDetails(Right(name), emailAddress, bpr.address, bpr.sapNumber)

            "display an error page" when {
              "the call to get the BPR fails" in {
                inSequence {
                  mockAuthWithAllRetrievals(L50, Some(AffinityGroup.Individual), None, None, Some(name), None, None, Set.empty)
                  mockGetSession(Future.successful(Right(Some(sessionData(None)))))
                  mockGetBusinessPartnerRecord(Right(individual(None)), true)(Left(Error("error")))
                }
                checkIsTechnicalErrorPage(performAction())
              }

              "the call to get BPR succeeds but it cannot be written to session" in {
                val session = SessionData.empty.copy(journeyStatus = Some(SubscriptionReady(individualSubscriptionDetails)))

                inSequence {
                  mockAuthWithAllRetrievals(L50, Some(AffinityGroup.Individual), None, None, Some(name), None, None, Set.empty)
                  mockGetSession(Future.successful(Right(Some(sessionData(None)))))
                  mockGetBusinessPartnerRecord(Right(individual(None)), true)(Right(bpr))
                  mockStoreSession(session)(Future.successful(Left(Error("Oh no!"))))
                }

                checkIsTechnicalErrorPage(performAction())
              }

            }

            "redirect to check subscription details" when {

              "it is successfully retrieved" in {
                val session = SessionData.empty.copy(journeyStatus = Some(
                  SubscriptionReady(individualSubscriptionDetails)
                ))

                inSequence {
                  mockAuthWithAllRetrievals(L50, Some(AffinityGroup.Individual), None, None, Some(name), None, None, Set.empty)
                  mockGetSession(Future.successful(Right(Some(sessionData(Some(email))))))
                  mockGetBusinessPartnerRecord(Right(individual(Some(email))), true)(Right(bpr))
                  mockStoreSession(session)(Future.successful(Right(())))
                }

                checkIsRedirect(performAction(), routes.SubscriptionController.checkYourDetails())
              }

              "an email does not exist in the retrieved BPR but one has been retrieved in the auth record" in {
                val bprWithNoEmail = bpr.copy(emailAddress = None)
                val ggEmail = Email("gg-email")
                val updatedSession = SessionData.empty.copy(journeyStatus = Some(
                  SubscriptionReady(individualSubscriptionDetails.copy(emailAddress = ggEmail.value))
                ))

                inSequence {
                  mockAuthWithAllRetrievals(L50, Some(AffinityGroup.Individual), None, None, Some(name), None, None, Set.empty)
                  mockGetSession(Future.successful(Right(Some(sessionData(Some(ggEmail))))))
                  mockGetBusinessPartnerRecord(Right(individual(Some(ggEmail))), true)(Right(bprWithNoEmail))
                  mockStoreSession(updatedSession)(Future.successful(Right(())))
                }

                checkIsRedirect(performAction(), routes.SubscriptionController.checkYourDetails())
              }

            }

            "redirect to enter email" when {

              "there is no email in the BPR or the auth record" in {
                val bprWithNoEmail = bpr.copy(emailAddress = None)
                val updatedSession =
                  SessionData.empty.copy(journeyStatus = Some(SubscriptionMissingData(bprWithNoEmail, Right(name))))

                inSequence {
                  mockAuthWithAllRetrievals(L50, Some(AffinityGroup.Individual), None, None, Some(name), None, None, Set.empty)
                  mockGetSession(Future.successful(Right(Some(sessionData(None)))))
                  mockGetBusinessPartnerRecord(Right(individual(None)), true)(Right(bprWithNoEmail))
                  mockStoreSession(updatedSession)(Future.successful(Right(())))
                }

                checkIsRedirect(performAction(), routes.EmailController.enterEmail().url)
              }

            }
          }

        }

        "the session data indicates that some subscription details are missing" must {

          "redirect to check your details" when {

            "the session data indicates there is subscription data missing and there is now enough " +
              "data to proceed to the check your details page" in {
              val individualSubscriptionDetails = SubscriptionDetails(Right(name), emailAddress, bpr.address, bpr.sapNumber)
                val session = SessionData.empty.copy(journeyStatus = Some(SubscriptionMissingData(bpr, Right(name))))
                val updatedSession =
                  SessionData.empty.copy(journeyStatus = Some(SubscriptionReady(individualSubscriptionDetails)))

                inSequence {
                  mockAuthWithAllRetrievals(ConfidenceLevel.L50, Some(AffinityGroup.Individual), None, None, Some(name), None, None, Set.empty)
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
                  SessionData.empty.copy(journeyStatus = Some(SubscriptionMissingData(bprWithNoEmail, Right(name))))

                inSequence {
                  mockAuthWithAllRetrievals(ConfidenceLevel.L50, Some(AffinityGroup.Individual), None, None, Some(name), None, None, Set.empty)
                  mockGetSession(Future.successful(Right(Some(sessionData))))
                }

                checkIsRedirect(performAction(), routes.EmailController.enterEmail().url)
            }

          }

        }

      }

      "the user is not enrolled and is not subscribed in ETMP" when {

        "handling individuals" must {

          val individualSubscriptionDetails = SubscriptionDetails(Right(name), emailAddress, bpr.address, bpr.sapNumber)

          "redirect to check subscription details" when {

            "there are subscription details in session" in {
              val session = SessionData.empty.copy(journeyStatus = Some(SubscriptionReady(individualSubscriptionDetails)))
              inSequence {
                mockAuthWithCl200AndWithAllIndividualRetrievals(nino.value, name, retrievedDateOfBirth, None)
                mockGetSession(Future.successful(Right(Some(session))))
              }

              checkIsRedirect(performAction(), routes.SubscriptionController.checkYourDetails())
            }

            "there is no session data and a bpr is successfully retrieved using the retrieved auth NINO, name and date of birth" in {
              List(
                Some(SessionData.empty),
                None
              ).foreach { maybeSession =>
                val session = SessionData.empty.copy(journeyStatus = Some(SubscriptionReady(individualSubscriptionDetails)))

                inSequence {
                  mockAuthWithCl200AndWithAllIndividualRetrievals(nino.value, name, retrievedDateOfBirth, None)
                  mockGetSession(Future.successful(Right(maybeSession)))
                  mockGetBusinessPartnerRecord(Right(individual), false)(Right(bpr))
                  mockStoreSession(session)(Future.successful(Right(())))
                }

                checkIsRedirect(performAction(), routes.SubscriptionController.checkYourDetails())
              }
            }

          "the user has CL<200 and there is no session data and a bpr is successfully retrieved " +
            "using the retrieved auth SA UTR, name and date of birth " in {
            List(
              Some(SessionData.empty),
              None
            ).foreach { maybeSession =>
              val expectedIndividual = individual.copy(id = Left(SAUTR("sautr")), dateOfBirth = None)
              val session = SessionData.empty.copy(journeyStatus = Some(SubscriptionReady(individualSubscriptionDetails)))

              inSequence {
                mockAuthWithAllRetrievals(ConfidenceLevel.L50, Some(AffinityGroup.Individual), None, Some("sautr"), Some(name), None, None, Set.empty)
                mockGetSession(Future.successful(Right(maybeSession)))
                mockGetBusinessPartnerRecord(Right(expectedIndividual), false)(Right(bpr))
                mockStoreSession(session)(Future.successful(Right(())))
              }

              checkIsRedirect(performAction(), routes.SubscriptionController.checkYourDetails())
            }
          }

          "an email does not exist in the retrieved BPR but one has been retrieved in the auth record" in {
            val bprWithNoEmail = bpr.copy(emailAddress = None)
            val ggEmail = "email"
            val session = SessionData.empty.copy(journeyStatus = Some(
              SubscriptionReady(individualSubscriptionDetails.copy(emailAddress = ggEmail))
            ))

            inSequence {
              mockAuthWithCl200AndWithAllIndividualRetrievals(nino.value, name, retrievedDateOfBirth, Some(ggEmail))
              mockGetSession(Future.successful(Right(Some(SessionData.empty))))
              mockGetBusinessPartnerRecord(Right(individual.copy(email = Some(Email(ggEmail)))), false)(Right(bprWithNoEmail))
              mockStoreSession(session)(Future.successful(Right(())))
            }

            checkIsRedirect(performAction(), routes.SubscriptionController.checkYourDetails())
          }

          "the user has CL<200 and an email does not exist in the retrieved BPR but one has been retrieved in the auth record" in {
            val bprWithNoEmail = bpr.copy(emailAddress = None)
            val ggEmail = "email"
            val session = SessionData.empty.copy(journeyStatus = Some(
              SubscriptionReady(individualSubscriptionDetails.copy(emailAddress = ggEmail))
            ))
            val sautr = SAUTR("sautr")

            inSequence {
              mockAuthWithAllRetrievals(ConfidenceLevel.L50, Some(AffinityGroup.Individual), None,
                Some(sautr.value), Some(name), None, Some(ggEmail), Set.empty)
              mockGetSession(Future.successful(Right(Some(SessionData.empty))))
              mockGetBusinessPartnerRecord(
                Right(individual.copy(id = Left(sautr), dateOfBirth = None, email = Some(Email(ggEmail)))),
                false
              )(Right(bprWithNoEmail))
              mockStoreSession(session)(Future.successful(Right(())))
            }

            checkIsRedirect(performAction(), routes.SubscriptionController.checkYourDetails())
          }

          "the session data indicates there is subscription data missing and there is now enough data to proceed" in {
            val session = SessionData.empty.copy(journeyStatus = Some(SubscriptionMissingData(bpr, Right(name))))
            val updatedSession =
              SessionData.empty.copy(journeyStatus = Some(SubscriptionReady(individualSubscriptionDetails)))

            inSequence {
              mockAuthWithCl200AndWithAllIndividualRetrievals(nino.value, name, retrievedDateOfBirth, None)
              mockGetSession(Future.successful(Right(Some(session))))
              mockStoreSession(updatedSession)(Future.successful(Right(())))
            }
            checkIsRedirect(performAction(), routes.SubscriptionController.checkYourDetails())
          }

          }

          "redirect to the subscription confirmation page" when {

            "the session data indicates the user has already subscribed" in {
              val subscriptionStatus = SubscriptionComplete(individualSubscriptionDetails, SubscriptionResponse(""))
              val session = SessionData.empty.copy(journeyStatus = Some(subscriptionStatus))

              inSequence {
                mockAuthWithCl200AndWithAllIndividualRetrievals(nino.value, name, retrievedDateOfBirth, None)
                mockGetSession(Future.successful(Right(Some(session))))
              }

              checkIsRedirect(performAction(), routes.SubscriptionController.subscribed())
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
                    mockAuthWithAllRetrievals(ConfidenceLevel.L50, Some(AffinityGroup.Individual), None, None, Some(name), None, None, Set.empty)
                    mockGetSession(Future.successful(Right(Some(session))))
                  }

                  checkIsRedirect(performAction(), routes.RegistrationController.startRegistration())
                }

              }
            }


          }

          "display an error page" when {
            "the call to get the BPR fails" in {
              inSequence {
                mockAuthWithCl200AndWithAllIndividualRetrievals(nino.value, name, retrievedDateOfBirth, None)
                mockGetSession(Future.successful(Right(Some(SessionData.empty))))
                mockGetBusinessPartnerRecord(Right(individual), false)(Left(Error("error")))
              }
              checkIsTechnicalErrorPage(performAction())
            }

            "the user has CL200 and the call to get BPR succeeds but it cannot be written to session" in {
              val session = SessionData.empty.copy(journeyStatus = Some(SubscriptionReady(individualSubscriptionDetails)))

              inSequence {
                mockAuthWithCl200AndWithAllIndividualRetrievals(nino.value, name, retrievedDateOfBirth, None)
                mockGetSession(Future.successful(Right(Some(SessionData.empty))))
                mockGetBusinessPartnerRecord(Right(individual), false)(Right(bpr))
                mockStoreSession(session)(Future.successful(Left(Error("Oh no!"))))
              }

              checkIsTechnicalErrorPage(performAction())
            }

            "the user has CL<200 and the call to get BPR succeeds but it cannot be written to session" in {
              val session = SessionData.empty.copy(journeyStatus = Some(SubscriptionReady(individualSubscriptionDetails)))
              val expectedIndividual = individual.copy(id = Left(SAUTR("sautr")), dateOfBirth = None)

              inSequence {
                mockAuthWithAllRetrievals(ConfidenceLevel.L50, Some(AffinityGroup.Individual), None, Some("sautr"), Some(name), None, None, Set.empty)
                mockGetSession(Future.successful(Right(Some(SessionData.empty))))
                mockGetBusinessPartnerRecord(Right(expectedIndividual), false)(Right(bpr))
                mockStoreSession(session)(Future.successful(Left(Error("Oh no!"))))
              }

              checkIsTechnicalErrorPage(performAction())
            }

          }

          "redirect to enter email" when {

            "there is no email in the BPR or the auth record for a user with CL 200" in {
              val bprWithNoEmail = bpr.copy(emailAddress = None)
              val updatedSession =
                SessionData.empty.copy(journeyStatus = Some(SubscriptionMissingData(bprWithNoEmail, Right(name))))

              inSequence {
                mockAuthWithCl200AndWithAllIndividualRetrievals(nino.value, name, retrievedDateOfBirth, None)
                mockGetSession(Future.successful(Right(Some(SessionData.empty))))
                mockGetBusinessPartnerRecord(Right(individual), false)(Right(bprWithNoEmail))
                mockStoreSession(updatedSession)(Future.successful(Right(())))
              }

              checkIsRedirect(performAction(), routes.EmailController.enterEmail().url)
            }

            "there is no email in the BPR or the auth record for a user with CL < 200" in {
              val expectedIndividual = individual.copy(id = Left(SAUTR("sautr")), dateOfBirth = None)
              val bprWithNoEmail = bpr.copy(emailAddress = None)
              val updatedSession =
                SessionData.empty.copy(journeyStatus = Some(SubscriptionMissingData(bprWithNoEmail, Right(name))))

              inSequence {
                mockAuthWithAllRetrievals(ConfidenceLevel.L50, Some(AffinityGroup.Individual), None, Some("sautr"), Some(name), None, None, Set.empty)
                mockGetSession(Future.successful(Right(Some(SessionData.empty))))
                mockGetBusinessPartnerRecord(Right(expectedIndividual), false)(Right(bprWithNoEmail))
                mockStoreSession(updatedSession)(Future.successful(Right(())))
              }

              checkIsRedirect(performAction(), routes.EmailController.enterEmail().url)
            }

            "the session data indicates there is data missing for subscription and the email address " +
              "is still missing" in {
                val bprWithNoEmail = bpr.copy(emailAddress = None)
                val sessionData =
                  SessionData.empty.copy(journeyStatus = Some(SubscriptionMissingData(bprWithNoEmail, Right(name))))

                inSequence {
                  mockAuthWithCl200AndWithAllIndividualRetrievals(nino.value, name, retrievedDateOfBirth, None)
                  mockGetSession(Future.successful(Right(Some(sessionData))))
                }

                checkIsRedirect(performAction(), routes.EmailController.enterEmail().url)
              }

          }

        }

        "handling trusts" must {

          val sautr = SAUTR("sautr")
          val trust = Trust(sautr, None)
          val trustName = TrustName("trustname")
          val address = UkAddress("line 1", None, None, None, "postcode")
          val sapNumber = "sap"
          val bpr = BusinessPartnerRecord(Some(emailAddress), address, sapNumber, Some(trustName.value))
          val trustSubscriptionDetails = SubscriptionDetails(Left(trustName), emailAddress, bpr.address, bpr.sapNumber)

          "redirect to the subscription confirmation page" when {

            "the session data indicates the user has already subscribed" in {
              val subscriptionStatus = SubscriptionComplete(trustSubscriptionDetails, SubscriptionResponse(""))
              val session = SessionData.empty.copy(journeyStatus = Some(subscriptionStatus))

              inSequence {
                mockAuthWithAllTrustRetrievals(sautr, None)
                mockGetSession(Future.successful(Right(Some(session))))
              }

              checkIsRedirect(performAction(), routes.SubscriptionController.subscribed())
            }

          }

          "show an error page" when {

            "there is an error getting the BPR" in {
              inSequence {
                mockAuthWithAllTrustRetrievals(sautr, None)
                mockGetSession(Future.successful(Right(None)))
                mockGetBusinessPartnerRecord(Left(trust), false)(Left(Error("")))
              }

              checkIsTechnicalErrorPage(performAction())
            }

            "the BPR doesn't contain an organisation name" in {
              inSequence {
                mockAuthWithAllTrustRetrievals(sautr, None)
                mockGetSession(Future.successful(Right(None)))
                mockGetBusinessPartnerRecord(Left(trust), false)(Right(bpr.copy(organisationName = None)))
              }

              checkIsTechnicalErrorPage(performAction())
            }

            "there is an error updating the session" in {
              inSequence {
                mockAuthWithAllTrustRetrievals(sautr, None)
                mockGetSession(Future.successful(Right(None)))
                mockGetBusinessPartnerRecord(Left(trust), false)(Right(bpr))
                mockStoreSession(
                  SessionData.empty.copy(journeyStatus = Some(SubscriptionReady(trustSubscriptionDetails)))
                )(Future.successful(Left(Error(""))))
              }

              checkIsTechnicalErrorPage(performAction())
            }

            "the session data indicates there is data missing for subscription and the trust name" +
              "is missing" in {
              val bprWithNoTrustName = bpr.copy(organisationName = None)
              val sessionData =
                SessionData.empty.copy(journeyStatus = Some(SubscriptionMissingData(bprWithNoTrustName, Right(name))))

              inSequence {
                mockAuthWithAllTrustRetrievals(sautr, None)
                mockGetSession(Future.successful(Right(Some(sessionData))))
              }

              checkIsTechnicalErrorPage(performAction())
            }

          }

          "redirect to check your details" when {

            "there is an email and organisation name in the BPR and the session has been updated" in {
              inSequence {
                mockAuthWithAllTrustRetrievals(sautr, None)
                mockGetSession(Future.successful(Right(None)))
                mockGetBusinessPartnerRecord(Left(trust), false)(Right(bpr))
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
                mockGetSession(Future.successful(Right(None)))
                mockGetBusinessPartnerRecord(Left(trust.copy(email = Some(Email("email")))), false)(Right(bpr.copy(emailAddress = None)))
                mockStoreSession(
                  SessionData.empty.copy(journeyStatus = Some(
                    SubscriptionReady(trustSubscriptionDetails.copy(emailAddress = "email"))))
                )(Future.successful(Right(())))
              }

              checkIsRedirect(performAction(), routes.SubscriptionController.checkYourDetails())
            }


            "there are subscription details in session" in {
              val session = SessionData.empty.copy(journeyStatus = Some(SubscriptionReady(trustSubscriptionDetails)))
              inSequence {
                mockAuthWithAllTrustRetrievals(sautr, None)
                mockGetSession(Future.successful(Right(Some(session))))
              }

              checkIsRedirect(performAction(), routes.SubscriptionController.checkYourDetails())
            }

            "the session data indicates there is subscription data missing and there is now enough data to proceed" in {
              val session = SessionData.empty.copy(journeyStatus = Some(SubscriptionMissingData(bpr, Left(trustName))))
              val updatedSession =
                SessionData.empty.copy(journeyStatus = Some(SubscriptionReady(trustSubscriptionDetails)))

              inSequence {
                mockAuthWithAllTrustRetrievals(sautr, None)
                mockGetSession(Future.successful(Right(Some(session))))
                mockStoreSession(updatedSession)(Future.successful(Right(())))
              }
              checkIsRedirect(performAction(), routes.SubscriptionController.checkYourDetails())
            }
          }

          "redirect to the enter email page" when {

            "there is no email in the BPR or in the auth record" in {
              val bprWithNoEmail = bpr.copy(emailAddress = None)

              inSequence {
                mockAuthWithAllTrustRetrievals(sautr, None)
                mockGetSession(Future.successful(Right(None)))
                mockGetBusinessPartnerRecord(Left(trust), false)(Right(bprWithNoEmail))
                mockStoreSession(
                  SessionData.empty.copy(journeyStatus = Some(
                    SubscriptionMissingData(bprWithNoEmail, Left(trustName))))
                )(Future.successful(Right(())))
              }

              checkIsRedirect(performAction(), routes.EmailController.enterEmail())
            }

            "the session data indicates there is data missing for subscription and the email address " +
              "is still missing" in {
              val bprWithNoEmail = bpr.copy(emailAddress = None)
              val sessionData =
                SessionData.empty.copy(journeyStatus = Some(SubscriptionMissingData(bprWithNoEmail, Right(name))))

              inSequence {
                mockAuthWithAllTrustRetrievals(sautr, None)
                mockGetSession(Future.successful(Right(Some(sessionData))))
              }

              checkIsRedirect(performAction(), routes.EmailController.enterEmail().url)
            }

          }

        }

      }
    }

  }
}

