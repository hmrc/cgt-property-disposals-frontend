/*
 * Copyright 2021 HM Revenue & Customs
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

import org.scalamock.scalatest.MockFactory
import play.api.i18n.MessagesApi
import play.api.mvc.Results.Ok
import play.api.mvc.{MessagesRequest, Result}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.auth.core.authorise.EmptyPredicate
import uk.gov.hmrc.auth.core.retrieve.EmptyRetrieval
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.ErrorHandler
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{ControllerSpec, SessionSupport}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class AuthenticatedActionSpec extends ControllerSpec with MockFactory with SessionSupport with AuthActionSpec {

  val authenticatedAction =
    new AuthenticatedAction(
      mockAuthConnector,
      config,
      instanceOf[ErrorHandler],
      mockSessionStore
    )

  def performAction[A](r: FakeRequest[A]): Future[Result] = {
    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    val request = new MessagesRequest[A](r, stub[MessagesApi])
    authenticatedAction.invokeBlock(
      request,
      { a: AuthenticatedRequest[A] =>
        a.request.messagesApi shouldBe request.messagesApi
        Future.successful(Ok)
      }
    )
  }

  "AuthenticatedAction" when {

    "handling a not logged in user" must {

      "redirect to the login page" in {
        val requestUri = "/abc"

        List[NoActiveSession](
          BearerTokenExpired(),
          MissingBearerToken(),
          InvalidBearerToken(),
          SessionRecordNotFound()
        ).foreach { e =>
          withClue(s"For error $e: ") {
            mockAuth(EmptyPredicate, EmptyRetrieval)(Future.failed(e))

            val result = performAction(FakeRequest("GET", requestUri))
            status(result) shouldBe SEE_OTHER

            val redirectTo = redirectLocation(result)
            redirectTo shouldBe Some(
              s"$signInUrl?continue_url=$selfBaseUrl$requestUri&origin=$origin"
            )
          }
        }
      }
    }

    "handling a logged in user" must {

      "effect the request action" in {
        mockAuth(EmptyPredicate, EmptyRetrieval)(Future.successful(()))

        val result = performAction(FakeRequest())
        status(result) shouldBe OK
      }

    }

    "handling the case when an authorisation exception is thrown" must {

      "throw an exception" in {
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
              mockAuth(EmptyPredicate, EmptyRetrieval)(Future.failed(e))
              await(performAction(FakeRequest()))
            }

            exception shouldBe e
          }
        }
      }
    }

  }

}
