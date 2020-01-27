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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.filters

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import org.scalamock.scalatest.MockFactory
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpec}
import play.api.Configuration
import play.api.mvc.Results._
import play.api.mvc.{Cookie, RequestHeader, Result}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.otac._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.routes
import uk.gov.hmrc.http.{HeaderCarrier, SessionKeys}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class EmailWhitelistingFilterSpec extends WordSpec with Matchers with MockFactory with BeforeAndAfterAll {

  implicit val system: ActorSystem = ActorSystem()

  val materializer = ActorMaterializer()

  val otacAuthConnector = mock[OtacAuthConnector]

  val (otacUrl, selfUrl) = "/otac" -> "/self"

  val otacToken = "test-otac-token"

  def mockVerifyPasscode(otacToken: String, rh: RequestHeader)(result: Future[OtacAuthorisationResult]) =
    (otacAuthConnector
      .authorise(_: String, _: HeaderCarrier, _: Option[String]))
      .expects(
        "capital-gains-tax-property-disposals",
        *,
        Some(otacToken)
      )
      .returning(result)

  def newFilter(whitelistingEnabled: Boolean): EmailWhitelistingFilter = {
    val config = Configuration(
      "passcodeAuthentication.enabled" -> whitelistingEnabled,
      "otac.url"                       -> otacUrl,
      "self.url"                       -> selfUrl
    )
    new EmailWhitelistingFilter(materializer, otacAuthConnector, config)
  }

  override def afterAll(): Unit = {
    super.afterAll()
    await(system.terminate())
  }

  "EmailWhitelistingFilter" when {

    val testAction: RequestHeader => Future[Result] = _ => Future.successful(Ok(""))

    "whitelisting is disabled" must {

      val filter = newFilter(whitelistingEnabled = false)

      "allow calls through" in {
        val result = filter(testAction)(FakeRequest("GET", "/test"))
        status(result) shouldBe OK
      }

    }

    "whitelisting is enabled" must {

      val filter = newFilter(whitelistingEnabled = true)

      "allow calls through" when {

        "the user has not been whitelisted but the page has been excluded from whitelisting" in {
          List(
            uk.gov.hmrc.play.health.routes.HealthController.ping(),
            uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.routes.EmailWhitelistingController.thereIsAProblem(),
            controllers.template.routes.Template.at("abc"),
            controllers.routes.Assets.at("123")
          ).foreach { call =>
            withClue(s"For call '$call': ") {
              val result = filter(testAction)(FakeRequest(call.method, call.url))
              status(result) shouldBe OK
            }
          }

        }

        "the request contains a valid otac token as a query parameter" in {
          val request = FakeRequest("GET", s"/test?p=$otacToken")

          mockVerifyPasscode(otacToken, request)(Future.successful(Authorised))

          val result = filter(testAction)(request)
          status(result) shouldBe OK
        }

        "the request contains a valid otac token in the session cookie" in {
          val request = FakeRequest("GET", s"/test").withSession(SessionKeys.otacToken -> otacToken)

          mockVerifyPasscode(otacToken, request)(Future.successful(Authorised))

          val result = filter(testAction)(request)
          status(result) shouldBe OK
        }

        "the request contains a valid otac token in the whitelisting cookie" in {
          val request = FakeRequest("GET", s"/test").withCookies(Cookie("whitelisting", otacToken))

          mockVerifyPasscode(otacToken, request)(Future.successful(Authorised))

          val result = filter(testAction)(request)
          status(result) shouldBe OK
        }

      }

      "redirect to the there is a problem page" when {

        "no otac token can be found" in {
          val request = FakeRequest("GET", s"/test")
          val result  = filter(testAction)(request)
          status(result)           shouldBe SEE_OTHER
          redirectLocation(result) shouldBe Some(routes.EmailWhitelistingController.thereIsAProblem().url)
        }

      }

      "redirect to the otac auth url" when {

        "an otac token can be found but a successful check on it cannot be done" in {
          List[Either[Throwable, OtacAuthorisationResult]](
            Right(NoOtacTokenInSession),
            Right(Unauthorised),
            Right(UnexpectedError(0)),
            Left(OtacFailureThrowable(NoOtacTokenInSession))
          ).foreach { otachAuthResult =>
            val request = FakeRequest("GET", s"/test?p=$otacToken")

            mockVerifyPasscode(otacToken, request)(
              otachAuthResult.fold(Future.failed, Future.successful)
            )

            val result = filter(testAction)(request)
            status(result)           shouldBe SEE_OTHER
            redirectLocation(result) shouldBe Some(s"$otacUrl?p=$otacToken")
            session(result).data shouldBe Map(
              SessionKeys.redirect  -> s"/test?p=$otacToken",
              SessionKeys.otacToken -> otacToken
            )
          }

        }

      }

    }
  }

}
