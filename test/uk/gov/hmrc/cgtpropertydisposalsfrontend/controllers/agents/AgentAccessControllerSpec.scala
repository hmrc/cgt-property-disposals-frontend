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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.agents

import cats.data.EitherT
import cats.instances.future._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.Configuration
import play.api.i18n.MessagesApi
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.CSRFTokenHelper._
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.auth.core.retrieve.EmptyRetrieval
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.EnrolmentConfig._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, ControllerSpec, PostcodeFormValidationTests, SessionSupport}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.AgentStatus.{AgentSupplyingClientDetails, VerifierMatchingDetails}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.Subscribed
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address.{NonUkAddress, UkAddress}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.{Address, Country, Postcode}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.agents.UnsuccessfulVerifierAttempts
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.{AgentReferenceNumber, CgtReference, GGCredId}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.SubscribedDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.DraftReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, SessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.agents.AgentVerifierMatchRetryStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.onboarding.SubscriptionService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class AgentAccessControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with RedirectToStartBehaviour
    with ScalaCheckDrivenPropertyChecks
    with PostcodeFormValidationTests {

  val mockAgentVerifierMatchRetryStore: AgentVerifierMatchRetryStore =
    mock[AgentVerifierMatchRetryStore]

  val mockReturnsService = mock[ReturnsService]

  val maxVerifierMatchAttempts = 5

  override lazy val additionalConfig: Configuration = Configuration(
    "agent-verifier-match.max-retries" -> maxVerifierMatchAttempts
  )

  override val overrideBindings =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore),
      bind[SubscriptionService].toInstance(mockSubscriptionService),
      bind[AgentVerifierMatchRetryStore].toInstance(mockAgentVerifierMatchRetryStore),
      bind[ReturnsService].toInstance(mockReturnsService)
    )

  lazy val controller = instanceOf[AgentAccessController]

  lazy implicit val messagesApi: MessagesApi = controller.messagesApi

  val agentGGCredId = sample[GGCredId]

  val agentReferenceNumber = sample[AgentReferenceNumber]

  val validCgtReference = CgtReference("XYCGTP123456789")

  val (nonUkCountryCode, nonUkCountryName) =
    Country.countryCodeToCountryName.headOption.getOrElse(sys.error("Could not find country"))

  val nonUkAddress = sample[NonUkAddress].copy(country = Country(nonUkCountryCode, Some(nonUkCountryName)))

  val ukAddress = sample[UkAddress].copy(postcode = Postcode("ZZ011ZZ"))

  val ukClientDetails = newClientDetails(validCgtReference, ukAddress)

  val nonUkClientDetails = newClientDetails(validCgtReference, nonUkAddress)

  def newClientDetails(cgtReference: CgtReference, address: Address): SubscribedDetails =
    sample[SubscribedDetails].copy(cgtReference = validCgtReference, address = address)

  def sessionData(clientDetails: SubscribedDetails, correctVerifierSupplied: Boolean) = SessionData.empty.copy(
    journeyStatus = Some(
      AgentSupplyingClientDetails(
        agentReferenceNumber,
        agentGGCredId,
        Some(VerifierMatchingDetails(clientDetails, correctVerifierSupplied))
      )
    )
  )

  def mockDelegatedAuthCheck(cgtReference: CgtReference)(result: Future[Unit]): Unit =
    mockAuth(
      Enrolment(CgtEnrolment.key)
        .withIdentifier(CgtEnrolment.cgtReferenceIdentifier, cgtReference.value)
        .withDelegatedAuthRule(CgtEnrolment.delegateAuthRule),
      EmptyRetrieval
    )(result)

  def mockGetSubscriptionDetails(cgtReference: CgtReference)(result: Either[Error, SubscribedDetails]) =
    (mockSubscriptionService
      .getSubscribedDetails(_: CgtReference)(_: HeaderCarrier))
      .expects(cgtReference, *)
      .returning(EitherT.fromEither[Future](result))

  def mockGetUnsuccessfulVerifierAttempts(
    agentGGCredId: GGCredId,
    clientCgtReference: CgtReference
  )(result: Either[Error, Option[UnsuccessfulVerifierAttempts]]) =
    (mockAgentVerifierMatchRetryStore
      .get(_: GGCredId, _: CgtReference))
      .expects(agentGGCredId, clientCgtReference)
      .returning(Future.successful(result))

  def mockStoreUnsuccessfulVerifierAttempts(
    agentGGCredId: GGCredId,
    clientCgtReference: CgtReference,
    unsuccessfulVerifierAttempts: UnsuccessfulVerifierAttempts
  )(
    result: Either[Error, Unit]
  ) =
    (mockAgentVerifierMatchRetryStore
      .store(_: GGCredId, _: CgtReference, _: UnsuccessfulVerifierAttempts))
      .expects(agentGGCredId, clientCgtReference, unsuccessfulVerifierAttempts)
      .returning(Future.successful(result))

  def mockGetDraftReturns(cgtReference: CgtReference)(response: Either[Error, List[DraftReturn]]) =
    (mockReturnsService
      .getDraftReturns(_: CgtReference)(_: HeaderCarrier))
      .expects(cgtReference, *)
      .returning(EitherT.fromEither[Future](response))

  "AgentAccessController" when {

    "handling requests to display the enter client's cgt reference page" must {

      val initialAgentSessionData =
        SessionData.empty.copy(
          journeyStatus = Some(AgentSupplyingClientDetails(agentReferenceNumber, agentGGCredId, None))
        )

      def performAction(): Future[Result] = controller.enterClientsCgtRef()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(
        performAction, {
          case _: AgentSupplyingClientDetails => true
          case _                              => false
        }
      )

      "display the page" when {

        "the session data is valid" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(initialAgentSessionData))))
          }

          val result = performAction()
          status(result)          shouldBe OK
          contentAsString(result) should include(message("agent.enter-client-cgt-ref.title"))
        }

      }
    }

    "handling submitted client's cgt references" must {

      val initialAgentSessionData =
        SessionData.empty.copy(
          journeyStatus = Some(AgentSupplyingClientDetails(agentReferenceNumber, agentGGCredId, None))
        )

      def performAction(formData: (String, String)*): Future[Result] =
        controller.enterClientsCgtRefSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*).withCSRFToken)

      behave like redirectToStartWhenInvalidJourney(
        () => performAction("cgtReference" -> validCgtReference.value), {
          case _: AgentSupplyingClientDetails => true
          case _                              => false
        }
      )

      "show a form error" when {

        def testInvalidCgtReference(value: String, errorMessageKey: String): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(initialAgentSessionData))))
          }

          val result = performAction("cgtReference" -> value)
          status(result)          shouldBe BAD_REQUEST
          contentAsString(result) should include(message("agent.enter-client-cgt-ref.title"))
          contentAsString(result) should include(message(errorMessageKey))
        }

        "the submitted value is more than 15 characters long" in {
          testInvalidCgtReference("a" * 16, "cgtReference.error.tooLong")
        }

        "the submitted value is less than 15 characters long" in {
          (1 to 14).foreach { i =>
            withClue(s"For value of length $i ") {
              testInvalidCgtReference("a" * i, "cgtReference.error.tooShort")
            }
          }
        }

        "the submitted value is empty" in {
          testInvalidCgtReference("", "cgtReference.error.required")
        }

        "the submitted value contains invalid characters" in {
          testInvalidCgtReference("123456789-ABCDE", "cgtReference.error.invalidCharacters")
        }

        "the submitted value contains valid characters but is not of the correct format" in {
          testInvalidCgtReference("1234567890ABCDE", "cgtReference.error.pattern")
        }

        "the submitted value is valid but the agent does not have permission to access the client" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(initialAgentSessionData))))
            mockDelegatedAuthCheck(validCgtReference)(Future.failed(InsufficientEnrolments()))
          }

          val result = performAction("cgtReference" -> validCgtReference.value)
          status(result)          shouldBe BAD_REQUEST
          contentAsString(result) should include(message("agent.enter-client-cgt-ref.title"))
          contentAsString(result) should include(message("cgtReference.error.notPermitted"))

        }

      }

      "show an error page" when {

        "the cgt reference is valid and the agent has permission to access the client " +
          "but the session data cannot be updated" in {
          val clientDetails = newClientDetails(validCgtReference, sample[Address](addressGen))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(initialAgentSessionData))))
            mockDelegatedAuthCheck(validCgtReference)(Future.successful(()))
            mockGetSubscriptionDetails(validCgtReference)(Right(clientDetails))
            mockStoreSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  AgentSupplyingClientDetails(
                    agentReferenceNumber,
                    agentGGCredId,
                    Some(VerifierMatchingDetails(clientDetails, false))
                  )
                )
              )
            )(Future.successful(Left(Error(""))))
          }

          val result = performAction("cgtReference" -> validCgtReference.value)
          checkIsTechnicalErrorPage(result)
        }

        "the cgt reference is valid and the agent has permission to access to the client " +
          "but no details can be found for the cgt reference" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(initialAgentSessionData))))
            mockDelegatedAuthCheck(validCgtReference)(Future.successful(()))
            mockGetSubscriptionDetails(validCgtReference)(Left(Error("")))
          }

          val result = performAction("cgtReference" -> validCgtReference.value)
          checkIsTechnicalErrorPage(result)
        }

        "the delegated auth check comes back with an error" in {
          List[AuthorisationException](
            InsufficientConfidenceLevel(),
            UnsupportedAffinityGroup(),
            UnsupportedCredentialRole(),
            UnsupportedAuthProvider(),
            IncorrectCredentialStrength(),
            BearerTokenExpired(),
            MissingBearerToken(),
            InvalidBearerToken(),
            SessionRecordNotFound(),
            InternalError(),
            FailedRelationship()
          ).foreach { authException =>
            withClue(s"For authorisation exception '$authException'") {
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(Future.successful(Right(Some(initialAgentSessionData))))
                mockDelegatedAuthCheck(validCgtReference)(Future.failed(authException))
              }

              val result = performAction("cgtReference" -> validCgtReference.value)
              checkIsTechnicalErrorPage(result)
            }

          }

        }

      }

      "redirect to the enter postcode page" when {

        "the value submitted is valid and the agent has permission for the client and the client's " +
          "address is in the uk" in {
          val ukClientDetails = newClientDetails(validCgtReference, sample[UkAddress])

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(initialAgentSessionData))))
            mockDelegatedAuthCheck(validCgtReference)(Future.successful(()))
            mockGetSubscriptionDetails(validCgtReference)(Right(ukClientDetails))
            mockStoreSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  AgentSupplyingClientDetails(
                    agentReferenceNumber,
                    agentGGCredId,
                    Some(VerifierMatchingDetails(ukClientDetails, false))
                  )
                )
              )
            )(Future.successful(Right(())))
          }

          val result = performAction("cgtReference" -> validCgtReference.value)
          checkIsRedirect(result, routes.AgentAccessController.enterClientsPostcode())
        }

      }

      "redirect to the enter country page" when {

        "the value submitted is valid and the agent has permission for the client and the client's " +
          "address is not in the uk" in {
          val nonUkClientDetails = newClientDetails(validCgtReference, sample[NonUkAddress])

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(initialAgentSessionData))))
            mockDelegatedAuthCheck(validCgtReference)(Future.successful(()))
            mockGetSubscriptionDetails(validCgtReference)(Right(nonUkClientDetails))
            mockStoreSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  AgentSupplyingClientDetails(
                    agentReferenceNumber,
                    agentGGCredId,
                    Some(VerifierMatchingDetails(nonUkClientDetails, false))
                  )
                )
              )
            )(Future.successful(Right(())))
          }

          val result = performAction("cgtReference" -> validCgtReference.value)
          checkIsRedirect(result, routes.AgentAccessController.enterClientsCountry())
        }

      }

    }

    "handling requests to display the enter client's postcode page" must {

      def performAction(): Future[Result] = controller.enterClientsPostcode()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(
        performAction, {
          case AgentSupplyingClientDetails(_, _, Some(_)) => true
          case _                                          => false
        }
      )

      behave like commonRedirectToTooManyAttemptsBehaviour(performAction)

      "redirect to the enter country page" when {

        "the client's contact address is not in the uk" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              Future.successful(
                Right(
                  Some(
                    SessionData.empty.copy(
                      journeyStatus = Some(
                        AgentSupplyingClientDetails(
                          agentReferenceNumber,
                          agentGGCredId,
                          Some(VerifierMatchingDetails(ukClientDetails.copy(address = sample[NonUkAddress]), false))
                        )
                      )
                    )
                  )
                )
              )
            )
            mockGetUnsuccessfulVerifierAttempts(agentGGCredId, ukClientDetails.cgtReference)(Right(None))
          }

          checkIsRedirect(performAction(), routes.AgentAccessController.enterClientsCountry())
        }

      }

      "display the page" when {

        "the session data is valid and the agent has not yet supplied the correct postcode" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              Future.successful(Right(Some(sessionData(ukClientDetails, correctVerifierSupplied = false))))
            )
            mockGetUnsuccessfulVerifierAttempts(agentGGCredId, ukClientDetails.cgtReference)(Right(None))
          }

          val result = performAction()
          status(result) shouldBe OK
          val content = contentAsString(result)
          content should include(message("agent.enter-client-postcode.title"))
          content should not include (ukAddress.postcode.value)
        }

        "the session data is valid and the agent has already supplied the correct postcode" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionData(ukClientDetails, correctVerifierSupplied = true)))))
            mockGetUnsuccessfulVerifierAttempts(agentGGCredId, ukClientDetails.cgtReference)(Right(None))
          }

          val result = performAction()
          status(result) shouldBe OK
          val content = contentAsString(result)
          content should include(message("agent.enter-client-postcode.title"))
          content should include(ukAddress.postcode.value)
        }
      }
    }

    "handling submitted postcodes" must {

      val incorrectPostcode = Postcode("AB12CD")

      val otherIncorrectPostcode = Postcode("BC12DE")

      def performAction(formData: (String, String)*): Future[Result] =
        controller.enterClientsPostcodeSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*).withCSRFToken)

      behave like redirectToStartWhenInvalidJourney(
        () => performAction("postcode" -> ukAddress.postcode.value), {
          case AgentSupplyingClientDetails(_, _, Some(_)) => true
          case _                                          => false
        }
      )

      behave like commonPostcodeFormValidationTests(
        performAction,
        () =>
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              Future.successful(Right(Some(sessionData(ukClientDetails, correctVerifierSupplied = false))))
            )
            mockGetUnsuccessfulVerifierAttempts(agentGGCredId, ukClientDetails.cgtReference)(Right(None))
          }
      )

      behave like commonRedirectToTooManyAttemptsBehaviour(() => performAction("postcode" -> ukAddress.postcode.value))

      "show a form error" when {

        "the submitted postcode is valid but it doesn't match the client's one and " +
          "the agent has not previously made an attempt" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              Future.successful(Right(Some(sessionData(ukClientDetails, correctVerifierSupplied = false))))
            )
            mockGetUnsuccessfulVerifierAttempts(agentGGCredId, ukClientDetails.cgtReference)(Right(None))
            mockStoreUnsuccessfulVerifierAttempts(
              agentGGCredId,
              ukClientDetails.cgtReference,
              UnsuccessfulVerifierAttempts(1, Right(incorrectPostcode))
            )(Right(()))
          }

          val result = performAction("postcode" -> incorrectPostcode.value)
          status(result) shouldBe BAD_REQUEST
          val content = contentAsString(result)
          content should include(message("agent.enter-client-postcode.title"))
          content should include(message("postcode.error.noMatch"))
        }

        "the submitted postcode is valid but it doesn't match the client's one and " +
          "the agent has previously made an attempt" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              Future.successful(Right(Some(sessionData(ukClientDetails, correctVerifierSupplied = false))))
            )
            mockGetUnsuccessfulVerifierAttempts(agentGGCredId, ukClientDetails.cgtReference)(
              Right(Some(UnsuccessfulVerifierAttempts(1, Right(otherIncorrectPostcode))))
            )
            mockStoreUnsuccessfulVerifierAttempts(
              agentGGCredId,
              ukClientDetails.cgtReference,
              UnsuccessfulVerifierAttempts(2, Right(incorrectPostcode))
            )(Right(()))
          }

          val result = performAction("postcode" -> incorrectPostcode.value)
          status(result) shouldBe BAD_REQUEST
          val content = contentAsString(result)
          content should include(message("agent.enter-client-postcode.title"))
          content should include(message("postcode.error.noMatch"))
        }

        "the submitted postcode is valid but it doesn't match the client's one and" +
          "the agent has previously made an attempt and the submitted postcode is the same " +
          "as the one in the previous attempt" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              Future.successful(Right(Some(sessionData(ukClientDetails, correctVerifierSupplied = false))))
            )
            mockGetUnsuccessfulVerifierAttempts(agentGGCredId, ukClientDetails.cgtReference)(
              Right(Some(UnsuccessfulVerifierAttempts(1, Right(incorrectPostcode))))
            )
          }

          val result = performAction("postcode" -> incorrectPostcode.value)
          status(result) shouldBe BAD_REQUEST
          val content = contentAsString(result)
          content should include(message("agent.enter-client-postcode.title"))
          content should include(message("postcode.error.noMatch"))
        }

      }

      "redirect to the enter country page" when {

        "the client's contact address is not in the uk" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              Future.successful(
                Right(
                  Some(
                    SessionData.empty.copy(
                      journeyStatus = Some(
                        AgentSupplyingClientDetails(
                          agentReferenceNumber,
                          agentGGCredId,
                          Some(VerifierMatchingDetails(ukClientDetails.copy(address = sample[NonUkAddress]), false))
                        )
                      )
                    )
                  )
                )
              )
            )
            mockGetUnsuccessfulVerifierAttempts(agentGGCredId, ukClientDetails.cgtReference)(Right(None))
          }

          checkIsRedirect(performAction(), routes.AgentAccessController.enterClientsCountry())
        }

      }

      "show an error page" when {

        "the submitted postcode is valid and matches the client's one but there is a problem updating the session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              Future.successful(Right(Some(sessionData(ukClientDetails, correctVerifierSupplied = false))))
            )
            mockGetUnsuccessfulVerifierAttempts(agentGGCredId, ukClientDetails.cgtReference)(Right(None))
            mockStoreSession(sessionData(ukClientDetails, correctVerifierSupplied = true))(
              Future.successful(Left(Error("")))
            )
          }

          val result = performAction("postcode" -> ukAddress.postcode.value)
          checkIsTechnicalErrorPage(result)
        }

        "the submitted postcode does not match and there is a problem updating the store" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              Future.successful(Right(Some(sessionData(ukClientDetails, correctVerifierSupplied = false))))
            )
            mockGetUnsuccessfulVerifierAttempts(agentGGCredId, ukClientDetails.cgtReference)(Right(None))
            mockStoreUnsuccessfulVerifierAttempts(
              agentGGCredId,
              ukClientDetails.cgtReference,
              UnsuccessfulVerifierAttempts(1, Right(incorrectPostcode))
            )(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction("postcode" -> incorrectPostcode.value))
        }

      }

      "redirect to the too many attempts page" when {

        "the agent has not previously made too many attempts but they submit an incorrect postcode and " +
          "they have now made too many attempts" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              Future.successful(Right(Some(sessionData(ukClientDetails, correctVerifierSupplied = false))))
            )
            mockGetUnsuccessfulVerifierAttempts(agentGGCredId, ukClientDetails.cgtReference)(
              Right(Some(UnsuccessfulVerifierAttempts(maxVerifierMatchAttempts - 1, Right(otherIncorrectPostcode))))
            )
            mockStoreUnsuccessfulVerifierAttempts(
              agentGGCredId,
              ukClientDetails.cgtReference,
              UnsuccessfulVerifierAttempts(maxVerifierMatchAttempts, Right(incorrectPostcode))
            )(Right(()))
          }

          checkIsRedirect(
            performAction("postcode" -> incorrectPostcode.value),
            routes.AgentAccessController.tooManyVerifierMatchAttempts()
          )
        }

      }

      "redirect to the confirm client page" when {

        "the submitted postcode is valid and matches the client's one and the session is updated" in {
          List(
            ukAddress.postcode.value,
            ukAddress.postcode.value.toLowerCase,
            " " + ukAddress.postcode.value + " "
          ).foreach { postcode =>
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                Future.successful(Right(Some(sessionData(ukClientDetails, correctVerifierSupplied = false))))
              )
              mockGetUnsuccessfulVerifierAttempts(agentGGCredId, ukClientDetails.cgtReference)(Right(None))
              mockStoreSession(sessionData(ukClientDetails, correctVerifierSupplied = true))(
                Future.successful(Right(()))
              )
            }

            val result = performAction("postcode" -> postcode)
            checkIsRedirect(result, routes.AgentAccessController.confirmClient())
          }

        }

      }

    }

    "handling requests to display the enter client's country page" must {

      def performAction(): Future[Result] = controller.enterClientsCountry()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(
        performAction, {
          case AgentSupplyingClientDetails(_, _, Some(_)) => true
          case _                                          => false
        }
      )

      behave like commonRedirectToTooManyAttemptsBehaviour(performAction)

      "redirect to the enter postcode page" when {

        "the client's contact address is in the uk" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              Future.successful(
                Right(
                  Some(
                    SessionData.empty.copy(
                      journeyStatus = Some(
                        AgentSupplyingClientDetails(
                          agentReferenceNumber,
                          agentGGCredId,
                          Some(VerifierMatchingDetails(nonUkClientDetails.copy(address = sample[UkAddress]), false))
                        )
                      )
                    )
                  )
                )
              )
            )
            mockGetUnsuccessfulVerifierAttempts(agentGGCredId, nonUkClientDetails.cgtReference)(Right(None))
          }

          checkIsRedirect(performAction(), routes.AgentAccessController.enterClientsPostcode())
        }

      }

      "display the page" when {

        "the session data is valid and the agent has not yet supplied the correct country" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              Future.successful(Right(Some(sessionData(nonUkClientDetails, correctVerifierSupplied = false))))
            )
            mockGetUnsuccessfulVerifierAttempts(agentGGCredId, nonUkClientDetails.cgtReference)(Right(None))
          }

          val result = performAction()
          status(result) shouldBe OK
          val content = contentAsString(result)
          content should include(message("agent.enter-client-country.title"))
          content should not include (s""""$nonUkCountryCode" selected""")
        }

        "the session data is valid and the agent has already supplied the correct country" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              Future.successful(Right(Some(sessionData(nonUkClientDetails, correctVerifierSupplied = true))))
            )
            mockGetUnsuccessfulVerifierAttempts(agentGGCredId, nonUkClientDetails.cgtReference)(Right(None))
          }

          val result = performAction()
          status(result) shouldBe OK
          val content = contentAsString(result)
          content should include(message("agent.enter-client-country.title"))
          content should include(s""""$nonUkCountryCode" selected""")
        }
      }
    }

    "handling submitted countries" must {

      val incorrectCountry = Country("HK", Some("Hong Kong"))

      val otherIncorrectCountry = Country("FI", Some("Finland"))

      def performAction(formData: (String, String)*): Future[Result] =
        controller.enterClientsCountrySubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*).withCSRFToken)

      behave like redirectToStartWhenInvalidJourney(
        () => performAction("countryCode" -> nonUkCountryCode), {
          case AgentSupplyingClientDetails(_, _, Some(_)) => true
          case _                                          => false
        }
      )

      behave like commonRedirectToTooManyAttemptsBehaviour(() => performAction("countryCode" -> nonUkCountryCode))

      "show a form error" when {

        def testFormValidationError(formData: (String, String)*)(
          errorKey: String,
          currentUnsuccessfulAttempt: Option[UnsuccessfulVerifierAttempts],
          updatedUnsucessfulAttempt: Option[UnsuccessfulVerifierAttempts]
        ): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              Future.successful(Right(Some(sessionData(nonUkClientDetails, correctVerifierSupplied = false))))
            )
            mockGetUnsuccessfulVerifierAttempts(agentGGCredId, nonUkClientDetails.cgtReference)(
              Right(currentUnsuccessfulAttempt)
            )
            updatedUnsucessfulAttempt.foreach(
              mockStoreUnsuccessfulVerifierAttempts(agentGGCredId, nonUkClientDetails.cgtReference, _)(Right(()))
            )
          }

          val result = performAction(formData: _*)
          status(result) shouldBe BAD_REQUEST
          val content = contentAsString(result)
          content should include(message("agent.enter-client-country.title"))
          content should include(message(errorKey))
        }

        "no country is submitted" in {
          testFormValidationError()("countryCode.error.required", None, None)
        }

        "the country submitted doesn't exist" in {
          testFormValidationError("countryCode" -> "XY")("countryCode.error.notFound", None, None)
        }

        "the submitted country is valid but it doesn't match the client's one and" +
          "the agent has not previously made an attempt" in {
          testFormValidationError("countryCode" -> incorrectCountry.code)(
            "countryCode.error.noMatch",
            None,
            Some(UnsuccessfulVerifierAttempts(1, Left(incorrectCountry)))
          )
        }

        "the submitted country is valid but it doesn't match the client's one and" +
          "the agent has previously made an attempt" in {
          testFormValidationError("countryCode" -> incorrectCountry.code)(
            "countryCode.error.noMatch",
            Some(UnsuccessfulVerifierAttempts(1, Left(otherIncorrectCountry))),
            Some(UnsuccessfulVerifierAttempts(2, Left(incorrectCountry)))
          )
        }

        "the submitted country is valid but it doesn't match the client's one and" +
          "the agent has previously made an attempt and the submitted country is the same " +
          "as the one in the previous attempt" in {
          testFormValidationError("countryCode" -> incorrectCountry.code)(
            "countryCode.error.noMatch",
            Some(UnsuccessfulVerifierAttempts(1, Left(incorrectCountry))),
            None
          )
        }

      }

      "redirect to the enter postcode page" when {

        "the client's contact address is in the uk" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              Future.successful(
                Right(
                  Some(
                    SessionData.empty.copy(
                      journeyStatus = Some(
                        AgentSupplyingClientDetails(
                          agentReferenceNumber,
                          agentGGCredId,
                          Some(VerifierMatchingDetails(nonUkClientDetails.copy(address = sample[UkAddress]), false))
                        )
                      )
                    )
                  )
                )
              )
            )
            mockGetUnsuccessfulVerifierAttempts(agentGGCredId, nonUkClientDetails.cgtReference)(Right(None))
          }

          checkIsRedirect(performAction(), routes.AgentAccessController.enterClientsPostcode())
        }

      }

      "show an error page" when {

        "the submitted country is valid and matches the client's one but there is a problem updating the session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              Future.successful(Right(Some(sessionData(nonUkClientDetails, correctVerifierSupplied = false))))
            )
            mockGetUnsuccessfulVerifierAttempts(agentGGCredId, nonUkClientDetails.cgtReference)(Right(None))
            mockStoreSession(sessionData(nonUkClientDetails, correctVerifierSupplied = true))(
              Future.successful(Left(Error("")))
            )
          }

          val result = performAction("countryCode" -> nonUkCountryCode)
          checkIsTechnicalErrorPage(result)
        }

        "the submitted postcode does not match and there is a problem updating the store" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              Future.successful(Right(Some(sessionData(nonUkClientDetails, correctVerifierSupplied = false))))
            )
            mockGetUnsuccessfulVerifierAttempts(agentGGCredId, nonUkClientDetails.cgtReference)(Right(None))
            mockStoreUnsuccessfulVerifierAttempts(
              agentGGCredId,
              nonUkClientDetails.cgtReference,
              UnsuccessfulVerifierAttempts(1, Left(incorrectCountry))
            )(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction("countryCode" -> incorrectCountry.code))
        }

      }

      "redirect to the too many attempts page" when {

        "the agent has not previously made too many attempts but they submit an incorrect postcode and " +
          "they have now made too many attempts" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              Future.successful(Right(Some(sessionData(nonUkClientDetails, correctVerifierSupplied = false))))
            )
            mockGetUnsuccessfulVerifierAttempts(agentGGCredId, nonUkClientDetails.cgtReference)(
              Right(Some(UnsuccessfulVerifierAttempts(maxVerifierMatchAttempts - 1, Left(otherIncorrectCountry))))
            )
            mockStoreUnsuccessfulVerifierAttempts(
              agentGGCredId,
              nonUkClientDetails.cgtReference,
              UnsuccessfulVerifierAttempts(maxVerifierMatchAttempts, Left(incorrectCountry))
            )(Right(()))
          }

          checkIsRedirect(
            performAction("countryCode" -> incorrectCountry.code),
            routes.AgentAccessController.tooManyVerifierMatchAttempts()
          )
        }

      }

      "redirect to the confirm client page" when {

        "the submitted postcode is valid and matches the client's one and the session is updated" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              Future.successful(Right(Some(sessionData(nonUkClientDetails, correctVerifierSupplied = false))))
            )
            mockGetUnsuccessfulVerifierAttempts(agentGGCredId, nonUkClientDetails.cgtReference)(Right(None))
            mockStoreSession(sessionData(nonUkClientDetails, correctVerifierSupplied = true))(
              Future.successful(Right(()))
            )
          }

          val result = performAction("countryCode" -> nonUkCountryCode)
          checkIsRedirect(result, routes.AgentAccessController.confirmClient())
        }

      }

    }

    "handling requests to display the confirm client details page" must {

      def performAction(): Future[Result] = controller.confirmClient()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(
        performAction, {
          case AgentSupplyingClientDetails(_, _, Some(_)) => true
          case _                                          => false
        }
      )

      "redirect to the enter postcode page" when {

        "the client's address is in the uk but the agent has not yet submitted it" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              Future.successful(Right(Some(sessionData(ukClientDetails, correctVerifierSupplied = false))))
            )
            mockGetUnsuccessfulVerifierAttempts(agentGGCredId, ukClientDetails.cgtReference)(Right(None))
          }

          checkIsRedirect(performAction(), routes.AgentAccessController.enterClientsPostcode())
        }
      }

      "redirect to the enter country page" when {

        "the client's address is in the not uk but the agent has not yet submitted it" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              Future.successful(Right(Some(sessionData(nonUkClientDetails, correctVerifierSupplied = false))))
            )
            mockGetUnsuccessfulVerifierAttempts(agentGGCredId, nonUkClientDetails.cgtReference)(Right(None))
          }

          checkIsRedirect(performAction(), routes.AgentAccessController.enterClientsCountry())
        }
      }

      "display the page" when {

        def testPageIsDisplayed(clientDetails: SubscribedDetails): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionData(clientDetails, correctVerifierSupplied = true)))))
            mockGetUnsuccessfulVerifierAttempts(agentGGCredId, clientDetails.cgtReference)(Right(None))
          }

          val result = performAction()
          status(result) shouldBe OK

          val content = contentAsString(result)
          content should include(message("agent.confirm-client.title"))
          content should include(clientDetails.makeAccountName())
          content should include(clientDetails.cgtReference.value)
        }

        "the agent has submitted the correct verifier for a uk client" in {
          testPageIsDisplayed(ukClientDetails)
        }

        "the agent has submitted the correct verifier for a non uk client" in {
          testPageIsDisplayed(nonUkClientDetails)

        }

      }

    }

    "handling requests to confirm a client" must {

      def performAction(): Future[Result] = controller.confirmClientSubmit()(FakeRequest())

      val draftReturns = List(sample[DraftReturn])

      behave like redirectToStartWhenInvalidJourney(
        performAction, {
          case AgentSupplyingClientDetails(_, _, Some(_)) => true
          case _                                          => false
        }
      )

      behave like commonRedirectToTooManyAttemptsBehaviour(performAction)

      "redirect to the enter postcode page" when {

        "the client's address is in the uk but the agent has not yet submitted it" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              Future.successful(Right(Some(sessionData(ukClientDetails, correctVerifierSupplied = false))))
            )
            mockGetUnsuccessfulVerifierAttempts(agentGGCredId, ukClientDetails.cgtReference)(Right(None))
          }

          checkIsRedirect(performAction(), routes.AgentAccessController.enterClientsPostcode())
        }
      }

      "redirect to the enter country page" when {

        "the client's address is in the not uk but the agent has not yet submitted it" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              Future.successful(Right(Some(sessionData(nonUkClientDetails, correctVerifierSupplied = false))))
            )
            mockGetUnsuccessfulVerifierAttempts(agentGGCredId, nonUkClientDetails.cgtReference)(Right(None))
          }

          checkIsRedirect(performAction(), routes.AgentAccessController.enterClientsCountry())
        }
      }

      "show an error page" when {

        "there is an error getting the client's draft returns" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionData(ukClientDetails, correctVerifierSupplied = true)))))
            mockGetUnsuccessfulVerifierAttempts(agentGGCredId, ukClientDetails.cgtReference)(Right(None))
            mockGetDraftReturns(ukClientDetails.cgtReference)(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction())
        }

        "the session data cannot be updated" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionData(ukClientDetails, correctVerifierSupplied = true)))))
            mockGetUnsuccessfulVerifierAttempts(agentGGCredId, ukClientDetails.cgtReference)(Right(None))
            mockGetDraftReturns(ukClientDetails.cgtReference)(Right(draftReturns))
            mockStoreSession(
              SessionData.empty
                .copy(journeyStatus =
                  Some(Subscribed(ukClientDetails, agentGGCredId, Some(agentReferenceNumber), draftReturns))
                )
            )(Future.successful(Left(Error(""))))
          }

          checkIsTechnicalErrorPage(performAction())
        }
      }

      "redirect to the homepage" when {

        def test(clientDetails: SubscribedDetails): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionData(clientDetails, correctVerifierSupplied = true)))))
            mockGetUnsuccessfulVerifierAttempts(agentGGCredId, clientDetails.cgtReference)(Right(None))
            mockGetDraftReturns(ukClientDetails.cgtReference)(Right(draftReturns))
            mockStoreSession(
              SessionData.empty.copy(
                journeyStatus = Some(Subscribed(clientDetails, agentGGCredId, Some(agentReferenceNumber), draftReturns))
              )
            )(Future.successful(Right(())))
          }

          checkIsRedirect(performAction(), controllers.accounts.homepage.routes.HomePageController.homepage())
        }

        "the agent has submitted the correct verifier for a uk client" in {
          test(ukClientDetails)
        }

        "the agent has submitted the correct verifier for a non uk client" in {
          test(nonUkClientDetails)
        }

      }

    }

    "handling requests to display the too many attempts page" must {

      def performAction(): Future[Result] =
        controller.tooManyVerifierMatchAttempts()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(
        performAction, {
          case _: AgentSupplyingClientDetails => true
          case _                              => false
        }
      )

      "display the page" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            Future.successful(Right(Some(sessionData(sample[SubscribedDetails], correctVerifierSupplied = true))))
          )
        }

        val result = performAction()
        status(result)          shouldBe OK
        contentAsString(result) should include(message("agent.too-many-attempts.title"))

      }

    }

  }

  def commonRedirectToTooManyAttemptsBehaviour(performAction: () => Future[Result]): Unit = {
    val clientDetails = sample[SubscribedDetails]

    "redirect to the too many attempts page" when {

      "the stored data indicates the agent has already tried to enter in the client's verifier too many times" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            Future.successful(Right(Some(sessionData(clientDetails, correctVerifierSupplied = false))))
          )
          mockGetUnsuccessfulVerifierAttempts(agentGGCredId, clientDetails.cgtReference)(
            Right(Some(UnsuccessfulVerifierAttempts(maxVerifierMatchAttempts, Right(sample[Postcode]))))
          )
        }

        checkIsRedirect(performAction(), routes.AgentAccessController.tooManyVerifierMatchAttempts())
      }

    }

    "show an error page" when {

      "there is an error checking the number of attempts already made" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            Future.successful(Right(Some(sessionData(clientDetails, correctVerifierSupplied = false))))
          )
          mockGetUnsuccessfulVerifierAttempts(agentGGCredId, clientDetails.cgtReference)(Left(Error("")))
        }

        checkIsTechnicalErrorPage(performAction())
      }

    }

  }

}
