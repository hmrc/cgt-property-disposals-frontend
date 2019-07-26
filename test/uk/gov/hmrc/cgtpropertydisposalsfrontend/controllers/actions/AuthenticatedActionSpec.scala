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
import org.scalamock.scalatest.MockFactory
import org.scalatest.{Matchers, WordSpec}
import play.api.Configuration
import play.api.i18n.MessagesApi
import play.api.mvc.Results.Ok
import play.api.mvc.{DefaultPlayBodyParsers, MessagesRequest}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.auth.core.authorise.{EmptyPredicate, Predicate}
import uk.gov.hmrc.auth.core.retrieve.{EmptyRetrieval, Retrieval}
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.ExecutionContext.Implicits.global

class AuthenticatedActionSpec extends WordSpec with Matchers with MockFactory {

  val mockAuthConnector: AuthConnector = mock[AuthConnector]

  val (signInUrl, origin, selfBaseUrl) = ("sign-in", "origin", "self-base-url")

  val config = Configuration(ConfigFactory.parseString(
    s"""
       |gg.url    = "$signInUrl"
       |gg.origin = "$origin"
       |self.url  = "$selfBaseUrl"
    """.stripMargin
  ))

  val bodyParser = mock[DefaultPlayBodyParsers]

  val authenticatedAction = new AuthenticatedAction(mockAuthConnector, config, bodyParser)

  def mockAuth[R](predicate: Predicate, retrieval: Retrieval[R])(result: Future[R]): Unit =
    (mockAuthConnector.authorise(_: Predicate, _: Retrieval[R])(_: HeaderCarrier, _: ExecutionContext))
      .expects(predicate, retrieval, *, *)
      .returning(result)

  def urlEncode(s: String): String = URLEncoder.encode(s, "UTF-8")

  "AuthenticatedAction" must {

      def performAction[A](r: FakeRequest[A]) = {
        @SuppressWarnings(Array("org.wartremover.warts.Any"))
        val request = new MessagesRequest[A](r, stub[MessagesApi])
        authenticatedAction.invokeBlock(request, { _: MessagesRequest[A] => Future.successful(Ok) })
      }

    "effect the requested action if the user is logged in" in {
      mockAuth(EmptyPredicate, EmptyRetrieval)(Future.successful(EmptyRetrieval))

      status(performAction(FakeRequest())) shouldBe OK
    }

    "redirect to the login page if the user is not logged in" in {
      val requestUri = "/abc"

      List[NoActiveSession](
        BearerTokenExpired(),
        MissingBearerToken(),
        InvalidBearerToken(),
        SessionRecordNotFound()
      ).foreach { e =>
          withClue(s"For error $e: ") {
            mockAuth(EmptyPredicate, EmptyRetrieval)(Future.failed[Unit](e))

            val result = performAction(FakeRequest("GET", requestUri))
            status(result) shouldBe SEE_OTHER

            val redirectTo = redirectLocation(result)
            redirectTo shouldBe Some(s"$signInUrl?continue=${urlEncode(selfBaseUrl + requestUri)}&origin=$origin")
          }
        }
    }

    "throw an exception if there is an authorisation exception" in {
      List[AuthorisationException](
        InsufficientConfidenceLevel(),
        InsufficientEnrolments(),
        UnsupportedAffinityGroup(),
        UnsupportedCredentialRole(),
        UnsupportedAuthProvider(),
        IncorrectCredentialStrength(),
        InternalError()
      ).foreach { e =>
          withClue(s"For error $e: ") {
            val exception = intercept[AuthorisationException] {
              mockAuth(EmptyPredicate, EmptyRetrieval)(Future.failed[Unit](e))

              await(performAction(FakeRequest()))
            }

            exception shouldBe e
          }
        }

    }
  }

}
