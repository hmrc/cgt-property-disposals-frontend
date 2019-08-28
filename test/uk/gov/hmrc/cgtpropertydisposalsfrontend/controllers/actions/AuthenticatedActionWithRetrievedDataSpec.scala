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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions

import java.net.URLEncoder
import java.time.LocalDate

import julienrf.json.derived
import org.joda.time.{LocalDate => JodaLocalDate}
import org.scalamock.scalatest.MockFactory
import play.api.i18n.MessagesApi
import play.api.libs.json.{Json, OFormat}
import play.api.mvc.{MessagesRequest, Result}
import play.api.mvc.Results.Ok
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.auth.core.authorise.EmptyPredicate
import uk.gov.hmrc.auth.core.retrieve.v2.Retrievals
import uk.gov.hmrc.auth.core.retrieve.{ItmpName, ~, Name => GGName}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.ErrorHandler
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.RetrievalOps
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{ControllerSpec, SessionSupport}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{DateOfBirth, Email, Error, NINO, Name, SessionData, UserType}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class AuthenticatedActionWithRetrievedDataSpec
  extends ControllerSpec with MockFactory with SessionSupport with AuthActionSpec {

  class TestEnvironment(useRelativeUrls: Boolean = true) {
    val config = newConfig(useRelativeUrls)

    val authenticatedAction =
      new AuthenticatedActionWithRetrievedData(mockAuthConnector, config, instanceOf[ErrorHandler], mockSessionStore)

    implicit val ninoFormat: OFormat[NINO] = Json.format[NINO]
    implicit val userTypeFormat: OFormat[UserType] = derived.oformat[UserType]
    
    def performAction[A](r: FakeRequest[A]): Future[Result] = {
      @SuppressWarnings(Array("org.wartremover.warts.Any"))
      val request = new MessagesRequest[A](r, stub[MessagesApi])
      authenticatedAction.invokeBlock(request, { a: AuthenticatedRequestWithRetrievedData[A] =>
      a.request.messagesApi shouldBe request.messagesApi
        Future.successful(Ok(Json.toJson(a.userType)))
      })
    }
  }

  def urlEncode(s: String): String = URLEncoder.encode(s, "UTF-8")

  val retrievals =
   Retrievals.confidenceLevel and Retrievals.affinityGroup and Retrievals.nino and
     Retrievals.itmpName and Retrievals.name and Retrievals.itmpDateOfBirth and Retrievals.email
  
  "AuthenticatedActionWithRetrievedData" when {

    "handling a not logged in user" must {

      "redirect to the login page" in new TestEnvironment {
        val requestUri = "/abc"

        List[NoActiveSession](
          BearerTokenExpired(),
          MissingBearerToken(),
          InvalidBearerToken(),
          SessionRecordNotFound()
        ).foreach { e =>
          withClue(s"For error $e: ") {
            mockAuth(EmptyPredicate, retrievals)(Future.failed(e))

            val result = performAction(FakeRequest("GET", requestUri))
            status(result) shouldBe SEE_OTHER

            val redirectTo = redirectLocation(result)
            redirectTo shouldBe Some(s"$signInUrl?continue=${urlEncode(selfBaseUrl + requestUri)}&origin=$origin")
          }
        }
      }
    }

    "handling individuals" when {

    "handling a logged in user with CL200 and all necessary data" must {
      val retrievalsResult = Future successful (
        new ~(ConfidenceLevel.L200, Some(AffinityGroup.Individual)) and
          Some("nino") and
          Some(ItmpName(Some("givenName"), Some("middleName"), Some("familyName"))) and
          Some(GGName(Some("forename"), Some("surname"))) and
          Some(new JodaLocalDate(2000, 4, 10)) and
          Some("email")
        )

      val expectedRetrieval =
        UserType.Individual(
          NINO("nino"),
          Name("givenName", "familyName"),
          DateOfBirth(LocalDate.of(2000, 4, 10)),
          Some(Email("email")))

      "effect the requested action" in new TestEnvironment {
        mockAuth(EmptyPredicate, retrievals)(retrievalsResult)

        val result = performAction(FakeRequest())
        status(result)          shouldBe OK
        contentAsJson(result) shouldBe Json.toJson(expectedRetrieval)
      }
    }

    "handling a logged in user with CL200 and a defined but empty email" must {
      val retrievalsResult = Future successful (
        new ~(ConfidenceLevel.L200, Some(AffinityGroup.Individual)) and
          Some("nino") and
          Some(ItmpName(Some("givenName"), Some("middleName"), Some("familyName"))) and
          Some(GGName(Some("forename"), Some("surname"))) and
          Some(new JodaLocalDate(2000, 4, 10)) and
          Some("")
        )

      val expectedRetrieval =
        UserType.Individual(
          NINO("nino"),
          Name("givenName", "familyName"),
          DateOfBirth(LocalDate.of(2000, 4, 10)),
          None)

      "effect the requested action" in new TestEnvironment {
        mockAuth(EmptyPredicate, retrievals)(retrievalsResult)

        val result = performAction(FakeRequest())
        status(result)        shouldBe OK
        contentAsJson(result) shouldBe Json.toJson(expectedRetrieval)
      }
    }


    "handling a logged in user with CL200 and a NINO, and an incomplete ITMP name" must {
      val retrievalsResult = Future successful (
        new ~(ConfidenceLevel.L200, Some(AffinityGroup.Individual)) and
          Some("nino") and
          Some(ItmpName(None, Some("middleName"), Some("familyName"))) and
          None and
          Some(new JodaLocalDate(2000, 4, 10)) and
          Some("email")
        )
      "show an error" in new TestEnvironment {
        mockAuth(EmptyPredicate, retrievals)(retrievalsResult)

        val result = performAction(FakeRequest())
        checkIsTechnicalErrorPage(result)
      }
    }

    "handling a logged in user with CL200 and a NINO, and no ITMP name and complete non-ITMP name" must {
      val retrievalsResult = Future successful (
        new ~(ConfidenceLevel.L200, Some(AffinityGroup.Individual)) and
          Some("nino") and
          None and
          Some(GGName(Some("first-name second-name"), None)) and
          Some(new JodaLocalDate(2000, 4, 10)) and
          None
        )
      val expectedRetrieval =
        UserType.Individual(
          NINO("nino"),
          Name("first-name", "second-name"),
          DateOfBirth(LocalDate.of(2000, 4, 10)),
          None)

      "effect the requested action" in new TestEnvironment {
        mockAuth(EmptyPredicate, retrievals)(retrievalsResult)

        val result = performAction(FakeRequest())
        status(result)          shouldBe OK
        contentAsJson(result) shouldBe Json.toJson(expectedRetrieval)
      }
    }

    "handling a logged in user with CL200 and a NINO, and no ITMP name and complete non-ITMP name " +
      "with more than two parts should only retrieve the first and last name" must {
      val retrievalsResult = Future successful (
        new ~(ConfidenceLevel.L200, Some(AffinityGroup.Individual)) and
          Some("nino") and
          None and
          Some(GGName(Some("first-name second-name third-name"), None)) and
          Some(new JodaLocalDate(2000, 4, 10)) and
          Some("email")
        )

      val expectedRetrieval =
        UserType.Individual(
          NINO("nino"),
          Name("first-name", "third-name"),
          DateOfBirth(LocalDate.of(2000, 4, 10)),
          Some(Email("email")))

      "effect the requested action" in new TestEnvironment {
        mockAuth(EmptyPredicate, retrievals)(retrievalsResult)

        val result = performAction(FakeRequest())
        status(result)          shouldBe OK
        contentAsJson(result) shouldBe Json.toJson(expectedRetrieval)
      }
    }

    "handling a logged in user with CL200 and a NINO, and no ITMP name and incomplete non-ITMP name" must {
    
      def retrievalsResult(ggName: Option[String]) = Future successful (
        new ~(ConfidenceLevel.L200, Some(AffinityGroup.Individual)) and
          Some("nino") and
          None and
          Some(GGName(ggName, None)) and
          Some(new JodaLocalDate(2000, 4, 10)) and
          Some("email")
        )
        
      "show an error" in new TestEnvironment {
        List(
          Some("first-name-only"),
          Some(""),
          None
        ).foreach{ name =>
          withClue(s"For name '$name': "){
            mockAuth(EmptyPredicate, retrievals)(retrievalsResult(name))

            val result = performAction(FakeRequest())
            checkIsTechnicalErrorPage(result)
          }
        }
      }
    }

    "handling a logged in user with CL200 and a NINO and no ITMP name" must {
      val retrievalsResult = Future successful (
        new ~(ConfidenceLevel.L200, Some(AffinityGroup.Individual)) and
          Some("nino") and
          None and
          None and
          Some(new JodaLocalDate(2000, 4, 10)) and
          None
        )
      
      "show an error" in new TestEnvironment {
        mockAuth(EmptyPredicate, retrievals)(retrievalsResult)

        val result = performAction(FakeRequest())
        checkIsTechnicalErrorPage(result)
      }
    }

    "handling a logged in user" when {

      "the CL is less than 200" ignore {

        val requestUri = "/uri"

        "show an error response " when {

          "the IV continue url can't be stored in session" in new TestEnvironment {
            inSequence {
              mockAuth(
                EmptyPredicate,
                retrievals
              )(
                Future.failed(InsufficientConfidenceLevel())
              )
              mockStoreSession(SessionData.empty.copy(ivContinueUrl = Some(selfBaseUrl + requestUri)))(
                Future.successful(Left(Error("Oh no!")))
              )
            }

            val result = performAction(FakeRequest("GET", requestUri))
            checkIsTechnicalErrorPage(result)
          }

        }

        "redirect to IV" ignore {

          "the IV continue URL is stored in session" in new TestEnvironment {
            inSequence {
              mockAuth(
                EmptyPredicate,
                retrievals
              )(
                Future.failed(InsufficientConfidenceLevel())
              )
              mockStoreSession(SessionData.empty.copy(ivContinueUrl = Some(selfBaseUrl + requestUri)))(
                Future.successful(Right(()))
              )
            }

            val result = performAction(FakeRequest("GET", requestUri))
            status(result) shouldBe SEE_OTHER
            redirectLocation(result) shouldBe Some(
              s"$ivUrl/mdtp/uplift?" +
                s"origin=$ivOrigin&" +
                s"confidenceLevel=200&" +
                s"completionURL=${urlEncode(ivSuccessRelativeUrl)}&" +
                s"failureURL=${urlEncode(ivFailureRelativeUrl)}"
            )
          }

          "the IV continue URL is stored in session and use absolute urls when configured to do so" in new TestEnvironment(
            useRelativeUrls = false
          ) {
            inSequence {
              mockAuth(
                ConfidenceLevel.L200,
                retrievals
              )(
                Future.failed(InsufficientConfidenceLevel())
              )
              mockStoreSession(SessionData.empty.copy(ivContinueUrl = Some(selfBaseUrl + requestUri)))(
                Future.successful(Right(()))
              )
            }

            val result = performAction(FakeRequest("GET", requestUri))
            status(result) shouldBe SEE_OTHER
            redirectLocation(result) shouldBe Some(
              s"$ivUrl/mdtp/uplift?" +
                s"origin=$ivOrigin&" +
                s"confidenceLevel=200&" +
                s"completionURL=${urlEncode(selfBaseUrl + ivSuccessRelativeUrl)}&" +
                s"failureURL=${urlEncode(selfBaseUrl + ivFailureRelativeUrl)}"
            )
          }

        }

      }
    }
    }

    "handling the case when an authorisation exception is thrown" must {

      "throw an exception" in new TestEnvironment {
        List[AuthorisationException](
          InsufficientEnrolments(),
          UnsupportedAffinityGroup(),
          UnsupportedCredentialRole(),
          UnsupportedAuthProvider(),
          IncorrectCredentialStrength(),
          InternalError()
        ).foreach { e =>
          withClue(s"For error $e: ") {
            val exception = intercept[AuthorisationException] {
              mockAuth(EmptyPredicate, retrievals)(Future.failed(e))

              await(performAction(FakeRequest()))
            }

            exception shouldBe e
          }
        }
      }
    }

  }

}
