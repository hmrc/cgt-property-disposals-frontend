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
import play.api.i18n.MessagesApi
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.CSRFTokenHelper._
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.auth.core.retrieve.EmptyRetrieval
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.CgtEnrolment
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.accounts.SubscribedChangeEmailControllerSpec
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, ControllerSpec, PostcodeFormValidationTests, SessionSupport}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.AgentStatus.{AgentSupplyingClientDetails, VerifierMatchingDetails}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address.{NonUkAddress, UkAddress}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.{Address, Country, Postcode}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.{CgtReference, GGCredId}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.SubscribedDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, SessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.onboarding.SubscriptionService
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

  override val overrideBindings =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore),
      bind[SubscriptionService].toInstance(mockSubscriptionService)
    )

  lazy val controller = instanceOf[AgentAccessController]

  lazy implicit val messagesApi: MessagesApi = controller.messagesApi

  val ggCredId = sample[GGCredId]

  val validCgtReference = CgtReference("XYCGTP123456789")

  val (nonUkCountryCode, nonUkCountryName) =
    Country.countryCodeToCountryName.headOption.getOrElse(sys.error("Could not find country"))

  val nonUkAddress  = sample[NonUkAddress].copy(country = Country(nonUkCountryCode, Some(nonUkCountryName)))

  val ukAddress     = sample[UkAddress].copy(postcode = Postcode("ZZ011ZZ"))

  val ukClientDetails = newClientDetails(validCgtReference, ukAddress)

  val nonUkClientDetails = newClientDetails(validCgtReference, nonUkAddress)

  def newClientDetails(cgtReference: CgtReference, address: Address): SubscribedDetails =
    sample[SubscribedDetails].copy(cgtReference = validCgtReference, address = address)

  def sessionData(clientDetails: SubscribedDetails, correctVerifierSupplied: Boolean) = SessionData.empty.copy(
    journeyStatus = Some(
      AgentSupplyingClientDetails(
        ggCredId,
        Some(VerifierMatchingDetails(clientDetails, correctVerifierSupplied))
      )
    )
  )

  def mockDelegatedAuthCheck(cgtReference: CgtReference)(result: Future[Unit]): Unit =
    mockAuth(
      Enrolment(CgtEnrolment.enrolmentKey)
        .withIdentifier(CgtEnrolment.enrolmentIdentifier, cgtReference.value)
        .withDelegatedAuthRule(CgtEnrolment.delegateAuthRule),
      EmptyRetrieval
    )(result)

  def mockGetSubscriptionDetails(cgtReference: CgtReference)(result: Either[Error, SubscribedDetails]) =
    (mockSubscriptionService
      .getSubscribedDetails(_: CgtReference)(_: HeaderCarrier))
      .expects(cgtReference, *)
      .returning(EitherT.fromEither[Future](result))

  "AgentAccessController" when {

    "handling requests to display the enter client's cgt reference page" must {

      val initialAgentSessionData =
        SessionData.empty.copy(
          journeyStatus = Some(AgentSupplyingClientDetails(ggCredId, None))
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
          journeyStatus = Some(AgentSupplyingClientDetails(ggCredId, None))
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
                journeyStatus =
                  Some(AgentSupplyingClientDetails(ggCredId, Some(VerifierMatchingDetails(clientDetails, false))))
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
                journeyStatus =
                  Some(AgentSupplyingClientDetails(ggCredId, Some(VerifierMatchingDetails(ukClientDetails, false))))
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
                journeyStatus =
                  Some(AgentSupplyingClientDetails(ggCredId, Some(VerifierMatchingDetails(nonUkClientDetails, false))))
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
          case AgentSupplyingClientDetails(_, Some(_)) => true
          case _                                       => false
        }
      )

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
                          ggCredId,
                          Some(VerifierMatchingDetails(ukClientDetails.copy(address = sample[NonUkAddress]), false))
                        )
                      )
                    )
                  )
                )
              )
            )
          }

          checkIsRedirect(performAction(), routes.AgentAccessController.enterClientsCountry())
        }

      }

      "display the page" when {

        "the session data is valid and the agent has not yet supplied the correct postcode" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionData(ukClientDetails, correctVerifierSupplied = false)))))
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

      def performAction(formData: (String, String)*): Future[Result] =
        controller.enterClientsPostcodeSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*).withCSRFToken)

      behave like redirectToStartWhenInvalidJourney(
        () => performAction("postcode" -> ukAddress.postcode.value), {
          case AgentSupplyingClientDetails(_, Some(_)) => true
          case _                                       => false
        }
      )

      behave like commonPostcodeFormValidationTests(
        performAction,
        () =>
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionData(ukClientDetails, correctVerifierSupplied = false)))))
          }
      )

      "show a form error" when {

        "the submitted postcode is valid but it doesn't match the client's one" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionData(ukClientDetails, correctVerifierSupplied = false)))))
          }

          val result = performAction("postcode" -> "AB12CD")
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
                          ggCredId,
                          Some(VerifierMatchingDetails(ukClientDetails.copy(address = sample[NonUkAddress]), false))
                        )
                      )
                    )
                  )
                )
              )
            )
          }

          checkIsRedirect(performAction(), routes.AgentAccessController.enterClientsCountry())
        }

      }

      "show an error page" when {

        "the submitted postcode is valid and matches the client's one but there is a problem updating the session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionData(ukClientDetails, correctVerifierSupplied = false)))))
            mockStoreSession(sessionData(ukClientDetails, correctVerifierSupplied = true))(Future.successful(Left(Error(""))))
          }

          val result = performAction("postcode" -> ukAddress.postcode.value)
          checkIsTechnicalErrorPage(result)
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
              mockGetSession(Future.successful(Right(Some(sessionData(ukClientDetails, correctVerifierSupplied = false)))))
              mockStoreSession(sessionData(ukClientDetails, correctVerifierSupplied = true))(Future.successful(Right(())))
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
          case AgentSupplyingClientDetails(_, Some(_)) => true
          case _                                       => false
        }
      )

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
                          ggCredId,
                          Some(VerifierMatchingDetails(nonUkClientDetails.copy(address = sample[UkAddress]), false))
                        )
                      )
                    )
                  )
                )
              )
            )
          }

          checkIsRedirect(performAction(), routes.AgentAccessController.enterClientsPostcode())
        }

      }

      "display the page" when {

        "the session data is valid and the agent has not yet supplied the correct country" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionData(nonUkClientDetails, correctVerifierSupplied = false)))))
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
            mockGetSession(Future.successful(Right(Some(sessionData(nonUkClientDetails, correctVerifierSupplied = true)))))
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

      def performAction(formData: (String, String)*): Future[Result] =
        controller.enterClientsCountrySubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*).withCSRFToken)

      behave like redirectToStartWhenInvalidJourney(
        () => performAction("countryCode" -> nonUkCountryCode), {
          case AgentSupplyingClientDetails(_, Some(_)) => true
          case _                                       => false
        }
      )

      "show a form error" when {

        def testFormValidationError(formData: (String, String)*)(errorKey: String): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionData(nonUkClientDetails, correctVerifierSupplied = false)))))
          }

          val result = performAction(formData: _*)
          status(result) shouldBe BAD_REQUEST
          val content = contentAsString(result)
          content should include(message("agent.enter-client-country.title"))
          content should include(message(errorKey))
        }

        "no country is submitted" in {
          testFormValidationError()("countryCode.error.required")
        }

        "the country submitted doesn't exist" in {
          testFormValidationError("countryCode" -> "XY")("countryCode.error.notFound")
        }

        "the submitted country is valid but it doesn't match the client's one" in {
          testFormValidationError("countryCode" -> "HK")("countryCode.error.noMatch")
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
                          ggCredId,
                          Some(VerifierMatchingDetails(nonUkClientDetails.copy(address = sample[UkAddress]), false))
                        )
                      )
                    )
                  )
                )
              )
            )
          }

          checkIsRedirect(performAction(), routes.AgentAccessController.enterClientsPostcode())
        }

      }

      "show an error page" when {

        "the submitted country is valid and matches the client's one but there is a problem updating the session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionData(nonUkClientDetails, correctVerifierSupplied = false)))))
            mockStoreSession(sessionData(nonUkClientDetails, correctVerifierSupplied = true))(Future.successful(Left(Error(""))))
          }

          val result = performAction("countryCode" -> nonUkCountryCode)
          checkIsTechnicalErrorPage(result)
        }

      }

      "show a dummy page" when {

        "the submitted postcode is valid and matches the client's one and the session is updated" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionData(nonUkClientDetails, correctVerifierSupplied = false)))))
            mockStoreSession(sessionData(nonUkClientDetails, correctVerifierSupplied = true))(Future.successful(Right(())))
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
          case AgentSupplyingClientDetails(_, Some(_)) => true
          case _                                       => false
        }
      )

      "redirect to the enter postcode page" when {

        "the client's address is in the uk but the agent has not yet submitted it" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionData(ukClientDetails, correctVerifierSupplied = false)))))
          }

          checkIsRedirect(performAction(), routes.AgentAccessController.enterClientsPostcode())
        }
      }

      "redirect to the enter country page" when {

        "the client's address is in the not uk but the agent has not yet submitted it" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionData(nonUkClientDetails, correctVerifierSupplied = false)))))
          }

          checkIsRedirect(performAction(), routes.AgentAccessController.enterClientsCountry())
        }
      }

      "display the page" when {

        def testPageIsDisplayed(clientDetails: SubscribedDetails): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionData(clientDetails, correctVerifierSupplied = true)))))
          }

          val result = performAction()
          status(result) shouldBe OK

          val content = contentAsString(result)
          content should include(message("agent.confirm-client.title"))
          content should include(clientDetails.contactName.value)
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

      behave like redirectToStartWhenInvalidJourney(
        performAction, {
          case AgentSupplyingClientDetails(_, Some(_)) => true
          case _                                       => false
        }
      )

      "redirect to the enter postcode page" when {

        "the client's address is in the uk but the agent has not yet submitted it" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionData(ukClientDetails, correctVerifierSupplied = false)))))
          }

          checkIsRedirect(performAction(), routes.AgentAccessController.enterClientsPostcode())
        }
      }

      "redirect to the enter country page" when {

        "the client's address is in the not uk but the agent has not yet submitted it" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionData(nonUkClientDetails, correctVerifierSupplied = false)))))
          }

          checkIsRedirect(performAction(), routes.AgentAccessController.enterClientsCountry())
        }
      }

      "show a dummy page" when {

        def test(clientDetails: SubscribedDetails): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionData(clientDetails, correctVerifierSupplied = true)))))
          }

          val result = performAction()
          status(result) shouldBe OK
          contentAsString(result) shouldBe "confirmed"
        }

        "the agent has submitted the correct verifier for a uk client" in {
          test(ukClientDetails)
        }

        "the agent has submitted the correct verifier for a non uk client" in {
          test(nonUkClientDetails)
        }

      }

    }

  }

}
