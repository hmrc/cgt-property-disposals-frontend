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
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.AgentStatus.AgentSupplyingClientDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.{CgtReference, GGCredId}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, SessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore

import scala.concurrent.Future

class AgentAccessControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with RedirectToStartBehaviour
    with ScalaCheckDrivenPropertyChecks {

  override val overrideBindings =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore)
    )

  lazy val controller = instanceOf[AgentAccessController]

  lazy implicit val messagesApi: MessagesApi = controller.messagesApi

  val ggCredId = sample[GGCredId]

  val validCgtReference = CgtReference("XYCGTP123456789")

  def mockDelegatedAuthCheck(cgtReference: CgtReference)(result: Future[Unit]): Unit =
    mockAuth(
      Enrolment(CgtEnrolment.enrolmentKey)
        .withIdentifier(CgtEnrolment.enrolmentIdentifier, cgtReference.value)
        .withDelegatedAuthRule(CgtEnrolment.delegateAuthRule),
      EmptyRetrieval
    )(result)

  "AgentAccessController" when {

    val initialAgentSessionData =
      SessionData.empty.copy(
        journeyStatus = Some(AgentSupplyingClientDetails(ggCredId, None))
      )

    "handling requests to display the enter client's cgt reference page" must {

      def performAction(): Future[Result] = controller.enterClientsCgtRef()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(
        performAction, {
          case _: AgentSupplyingClientDetails => true
          case _                              => false
        }
      )

      "display the page" when {

        "the session data is valid and the agent has not already supplied a cgt reference" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(initialAgentSessionData))))
          }

          val result = performAction()
          status(result)          shouldBe OK
          contentAsString(result) should include(message("agent.enter-client-cgt-ref.title"))
        }

        "the session data is valid and the agent has already supplied a cgt reference" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(initialAgentSessionData))))
          }

          val result = performAction()
          status(result)          shouldBe OK
          contentAsString(result) should include(message("agent.enter-client-cgt-ref.title"))
          contentAsString(result) should include(validCgtReference.value)
        }
      }
    }

    "handling submitted client's cgt references" must {

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
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(initialAgentSessionData))))
            mockDelegatedAuthCheck(validCgtReference)(Future.successful(()))
            mockStoreSession(
              SessionData.empty.copy(
                journeyStatus = Some(AgentSupplyingClientDetails(ggCredId, Some(validCgtReference)))
              )
            )(Future.successful(Left(Error(""))))
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

      "show a dummy ok page" when {

        "the value submitted is valid and the agent has permission for the client" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(initialAgentSessionData))))
            mockDelegatedAuthCheck(validCgtReference)(Future.successful(()))
            mockStoreSession(
              SessionData.empty.copy(
                journeyStatus = Some(AgentSupplyingClientDetails(ggCredId, Some(validCgtReference)))
              )
            )(Future.successful(Right(())))
          }

          val result = performAction("cgtReference" -> validCgtReference.value)
          status(result) shouldBe OK
          contentAsString(result) shouldBe s"Got $validCgtReference"
        }

      }


    }

  }

}
