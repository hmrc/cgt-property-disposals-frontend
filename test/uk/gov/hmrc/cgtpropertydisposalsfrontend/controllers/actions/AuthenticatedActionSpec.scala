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

import com.typesafe.config.ConfigFactory
import org.joda.time.LocalDate
import org.scalamock.scalatest.MockFactory
import play.api.Configuration
import play.api.i18n.MessagesApi
import play.api.mvc.MessagesRequest
import play.api.mvc.Results.Ok
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.auth.core.authorise.Predicate
import uk.gov.hmrc.auth.core.retrieve.v2.Retrievals
import uk.gov.hmrc.auth.core.retrieve.{ItmpName, Name, Retrieval, ~}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.ErrorHandler
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{ControllerSpec, SessionSupport}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, SessionData}
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, Future}

class AuthenticatedActionSpec extends ControllerSpec with MockFactory with SessionSupport {

  val mockAuthConnector: AuthConnector = mock[AuthConnector]

  val (signInUrl, origin, selfBaseUrl, ivUrl, ivOrigin) =
    ("sign-in", "origin", "self-base-url", "ivUrl", "ivOrigin")

  val (ivSuccessRelativeUrl, ivFailureRelativeUrl) = "/success" -> "/failure"

  class TestEnvironment(useRelativeUrls: Boolean = true) {
    val config = Configuration(
      ConfigFactory.parseString(
        s"""
         |gg.url    = "$signInUrl"
         |gg.origin = "$origin"
         |self.url  = "$selfBaseUrl"
         |iv {
         |  url         = "$ivUrl"
         |  origin      = "$ivOrigin"
         |  success-relative-url = "$ivSuccessRelativeUrl"
         |  failure-relative-url = "$ivFailureRelativeUrl"
         |  use-relative-urls = $useRelativeUrls
         |}
    """.stripMargin
      )
    )

    val authenticatedAction =
      new AuthenticatedAction(mockAuthConnector, config, instanceOf[ErrorHandler], mockSessionStore)

    def mockAuth[R](predicate: Predicate, retrieval: Retrieval[R])(result: Future[R]): Unit =
      (mockAuthConnector
        .authorise(_: Predicate, _: Retrieval[R])(_: HeaderCarrier, _: ExecutionContext))
        .expects(predicate, retrieval, *, *)
        .returning(result)

    def performAction[A](r: FakeRequest[A]) = {
      @SuppressWarnings(Array("org.wartremover.warts.Any"))
      val request = new MessagesRequest[A](r, stub[MessagesApi])
      authenticatedAction.invokeBlock(request, { a: AuthenticatedRequest[A] =>
        Future.successful(
          Ok(
            List(
              a.nino.value,
              a.name.forename,
              a.name.surname,
              a.dateOfBirth.value.toString,
              a.email.map(_.value).toString
            ).mkString("|")
          ))
      })
    }
  }

  def urlEncode(s: String): String = URLEncoder.encode(s, "UTF-8")

  val retrievals =
    Retrievals.nino and Retrievals.itmpName and Retrievals.name and Retrievals.itmpDateOfBirth and Retrievals.email
  
  "AuthenticatedAction" when {

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
            mockAuth(
              ConfidenceLevel.L200,
              retrievals
            )(
              Future.failed(e)
            )

            val result = performAction(FakeRequest("GET", requestUri))
            status(result) shouldBe SEE_OTHER

            val redirectTo = redirectLocation(result)
            redirectTo shouldBe Some(s"$signInUrl?continue=${urlEncode(selfBaseUrl + requestUri)}&origin=$origin")
          }
        }
      }
    }

    "handling a logged in user with CL200 and all necessary data" must {
      val retrievalsResult = Future successful (
        new ~(
          new ~(
            new ~(
              new ~(Some("nino"), Some(ItmpName(Some("givenName"), Some("middleName"), Some("familyName")))),
            Some(Name(Some("forename"), Some("surname")))
          ),
          Some(new LocalDate(2000, 4, 10))
          ),
          Some("email")
        )
      )
      "effect the requested action" in new TestEnvironment {
        mockAuth(ConfidenceLevel.L200, retrievals)(retrievalsResult)

        val result = performAction(FakeRequest())
        status(result)          shouldBe OK
        contentAsString(result) shouldBe "nino|givenName|familyName|2000-04-10|Some(email)"
      }
    }

    "handling a logged in user with CL200 and a defined but empty email" must {
      val retrievalsResult = Future successful (
        new ~(
          new ~(
            new ~(
              new ~(Some("nino"), Some(ItmpName(Some("givenName"), Some("middleName"), Some("familyName")))),
              Some(Name(Some("forename"), Some("surname")))
            ),
            Some(new LocalDate(2000, 4, 10))
          ),
          Some("")
        )
        )
      "effect the requested action" in new TestEnvironment {
        mockAuth(ConfidenceLevel.L200, retrievals)(retrievalsResult)

        val result = performAction(FakeRequest())
        status(result)          shouldBe OK
        contentAsString(result) shouldBe "nino|givenName|familyName|2000-04-10|None"
      }
    }

    "handling a logged in user with CL200 and a NINO, and an incomplete ITMP name" must {
      val retrievalsResult = Future successful (
        new ~(
          new ~(
            new ~(
            new ~(Some("nino"), Some(ItmpName(None, Some("middleName"), Some("familyName")))),
            None
          ),
          Some(new LocalDate(2000, 4, 10))
        ),
      None
        )
      )
      "effect the requested action" in new TestEnvironment {
        mockAuth(ConfidenceLevel.L200, retrievals)(retrievalsResult)

        val result = performAction(FakeRequest())
        checkIsTechnicalErrorPage(result)
      }
    }

    "handling a logged in user with CL200 and a NINO, and no ITMP name and complete non-ITMP name" must {
      
      val retrievalsResult = Future successful (
        new ~(
          new ~(
            new ~(
            new ~(Some("nino"), None),
            Some(Name(Some("first-name second-name"), None))
          ),
          Some(new LocalDate(2000, 4, 10))
          ),
          None
        )
      )
      "effect the requested action" in new TestEnvironment {
        mockAuth(ConfidenceLevel.L200, retrievals)(retrievalsResult)

        val result = performAction(FakeRequest())
        status(result)          shouldBe OK
        contentAsString(result) shouldBe "nino|first-name|second-name|2000-04-10|None"
      }
    }

    "handling a logged in user with CL200 and a NINO, and no ITMP name and complete non-ITMP name with more than two parts should only retrieve the first and last name" must {
      
      val retrievalsResult = Future successful (
        new ~(
          new ~(
            new ~(
            new ~(Some("nino"), None),
            Some(Name(Some("first-name second-name third-name"), None))
          ),
          Some(new LocalDate(2000, 4, 10))
          ),
          Some("email")
        )
      )
      "effect the requested action" in new TestEnvironment {
        mockAuth(ConfidenceLevel.L200, retrievals)(retrievalsResult)

        val result = performAction(FakeRequest())
        status(result)          shouldBe OK
        contentAsString(result) shouldBe "nino|first-name|third-name|2000-04-10|Some(email)"
      }
    }

    "handling a logged in user with CL200 and a NINO, and no ITMP name and incomplete non-ITMP name" must {
      
      val retrievalsResult = Future successful (
        new ~(
          new ~(
            new ~(
            new ~(Some("nino"), None),
            Some(Name(Some("first-name-only"), None))
          ),
          Some(new LocalDate(2000, 4, 10))
          ),
          Some("email")
        )
      )
      "effect the requested action" in new TestEnvironment {
        mockAuth(ConfidenceLevel.L200, retrievals)(retrievalsResult)

        val result = performAction(FakeRequest())
        checkIsTechnicalErrorPage(result)
      }
    }

    "handling a logged in user with CL200 and a NINO and no ITMP name" must {
      
      val retrievalsResult = Future successful (
        new ~(
          new ~(
            new ~(
            new ~(Some("nino"), None),
            None
          ),
          Some(new LocalDate(2000, 4, 10))
        ),
          None
        )
      )
      "effect the requested action" in new TestEnvironment {
        mockAuth(ConfidenceLevel.L200, retrievals)(retrievalsResult)

        val result = performAction(FakeRequest())
        checkIsTechnicalErrorPage(result)
      }
    }

    "handling a logged in user" when {

      "the CL is less than 200" must {

        val requestUri = "/uri"

        "show an error response " when {

          "the IV continue url can't be stored in session" in new TestEnvironment {
            inSequence {
              mockAuth(
                ConfidenceLevel.L200,
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

        "redirect to IV" when {

          "the IV continue URL is stored in session" in new TestEnvironment {
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
              mockAuth(
                ConfidenceLevel.L200,
                retrievals
              )(
                Future.failed(e)
              )

              await(performAction(FakeRequest()))
            }

            exception shouldBe e
          }
        }
      }
    }

  }

}
