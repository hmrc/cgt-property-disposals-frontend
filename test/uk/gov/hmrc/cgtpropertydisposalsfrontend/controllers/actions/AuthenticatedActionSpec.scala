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
import org.scalatest.prop.TableDrivenPropertyChecks._
import play.api.Configuration
import play.api.i18n.MessagesApi
import play.api.mvc.MessagesRequest
import play.api.mvc.Results.Ok
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.auth.core.authorise.Predicate
import uk.gov.hmrc.auth.core.retrieve.v2.Retrievals
import uk.gov.hmrc.auth.core.retrieve.{Name, Retrieval, ~}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.ErrorHandler
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{ControllerSpec, SessionSupport}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, SessionData}
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, Future}

class AuthenticatedActionSpec extends ControllerSpec with MockFactory with SessionSupport {

  val mockAuthConnector: AuthConnector = mock[AuthConnector]

  val (signInUrl, origin, selfBaseUrl, ivUrl, ivOrigin) = ("sign-in", "origin", "self-base-url", "ivUrl", "ivOrigin")

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
        Future.successful(Ok(a.nino.value))
      })
    }
  }

  def urlEncode(s: String): String = URLEncoder.encode(s, "UTF-8")

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
            mockAuth(ConfidenceLevel.L200, Retrievals.nino and Retrievals.name and Retrievals.dateOfBirth)(
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

    "handling a logged in user with CL200 and a NINO, name, and date of birth can be retrieved" must {
      val retrievals = Retrievals.nino and Retrievals.name and Retrievals.dateOfBirth
      val retrievalsResult = Future.successful(
        new ~(new ~(Some("nino"), Some(Name(Some("forename"), Some("surname")))), Some(new LocalDate(2000, 4, 10)))
      )
      "effect the requested action" in new TestEnvironment {
        mockAuth(ConfidenceLevel.L200, retrievals)(retrievalsResult)

        val result = performAction(FakeRequest())
        status(result)          shouldBe OK
        contentAsString(result) shouldBe "nino"
      }
    }

    val failedRetrievalCombinations =
      Table(
        ("nino", "name", "dateOfBirth"),
        (Some("nino"), None, None),
        (Some("nino"), Some(Name(None, None)), None),
        (Some("nino"), Some(Name(Some("forename"), None)), None),
        (Some("nino"), Some(Name(None, Some("surname"))), None),
        (Some("nino"), Some(Name(Some("forename"), Some("surname"))), None),
        (Some("nino"), None, Some(new LocalDate(2000, 4, 10))),
        (Some("nino"), Some(Name(None, None)), Some(new LocalDate(2000, 4, 10))),
        (Some("nino"), Some(Name(Some("forename"), None)), Some(new LocalDate(2000, 4, 10))),
        (Some("nino"), Some(Name(None, Some("surname"))), Some(new LocalDate(2000, 4, 10))),
        (None, Some(Name(None, None)), None),
        (None, Some(Name(Some("forename"), None)), None),
        (None, Some(Name(None, Some("surname"))), None),
        (None, Some(Name(Some("forename"), Some("surname"))), None),
        (None, None, Some(new LocalDate(2000, 4, 10))),
        (None, Some(Name(None, None)), Some(new LocalDate(2000, 4, 10))),
        (None, Some(Name(Some("forename"), None)), Some(new LocalDate(2000, 4, 10))),
        (None, Some(Name(None, Some("surname"))), Some(new LocalDate(2000, 4, 10))),
        (None, Some(Name(Some("forename"), Some("surname"))), Some(new LocalDate(2000, 4, 10))),
        (Some("nino"), None, None),
        (Some("nino"), None, None),
        (Some("nino"), None, None),
        (Some("nino"), None, None),
        (Some("nino"), None, None),
        (Some("nino"), None, Some(new LocalDate(2000, 4, 10))),
        (Some("nino"), None, Some(new LocalDate(2000, 4, 10))),
        (Some("nino"), None, Some(new LocalDate(2000, 4, 10))),
        (Some("nino"), None, Some(new LocalDate(2000, 4, 10))),
        (Some("nino"), None, Some(new LocalDate(2000, 4, 10))),
        (None, None, None)
      )

    "handling a logged in user with CL200 and some auth records cannot be retrieved" must {
      val retrievals = Retrievals.nino and Retrievals.name and Retrievals.dateOfBirth
      "effect the requested action" in new TestEnvironment {
        forAll(failedRetrievalCombinations) {
          (nino: Option[String], name: Option[Name], dateOfBirth: Option[LocalDate]) =>
            val retrievalsResult = Future.successful(
              new ~(new ~(nino, name), dateOfBirth)
            )
            mockAuth(ConfidenceLevel.L200, retrievals)(retrievalsResult)
            val result = performAction(FakeRequest())
            checkIsTechnicalErrorPage(result)
        }
      }
    }

    "handling a logged in user" when {

      "the CL is less than 200" must {

        val requestUri = "/uri"

        "show an error response " when {

          "the IV continue url can't be stored in session" in new TestEnvironment {
            inSequence {
              mockAuth(ConfidenceLevel.L200, Retrievals.nino and Retrievals.name and Retrievals.dateOfBirth)(
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
              mockAuth(ConfidenceLevel.L200, Retrievals.nino and Retrievals.name and Retrievals.dateOfBirth)(
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
              mockAuth(ConfidenceLevel.L200, Retrievals.nino and Retrievals.name and Retrievals.dateOfBirth)(
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

      //TODO: refactor tests to handle all the cases where we don't get the various pieces of information back
//      "the CL is 200 or more but the NINO  can't be retrieved" must {
//
//        "show an error page" in new TestEnvironment {
//          mockAuth(ConfidenceLevel.L200, Retrievals.nino and Retrievals.name and Retrievals.dateOfBirth)(
//            Future.successful(None))
//
//          val result = performAction(FakeRequest())
//          checkIsTechnicalErrorPage(result)
//        }
//
//      }

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
              mockAuth(ConfidenceLevel.L200, Retrievals.nino and Retrievals.name and Retrievals.dateOfBirth)(
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
