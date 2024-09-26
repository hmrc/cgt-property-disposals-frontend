/*
 * Copyright 2023 HM Revenue & Customs
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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors

import com.typesafe.config.ConfigFactory
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.json.{JsString, JsValue, Json}
import play.api.mvc.Call
import play.api.test.Helpers._
import play.api.{Application, Configuration}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.email.Email
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.http.AcceptLanguage
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.ContactName
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.WireMockMethods
import uk.gov.hmrc.http.test.WireMockSupport
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}

import scala.concurrent.ExecutionContext

class EmailVerificationConnectorImplSpec extends AnyWordSpec with Matchers with MockFactory
  with WireMockSupport with WireMockMethods with GuiceOneAppPerSuite with EitherValues {

  val protocol = "http"
  val templateId             = "id"
  val linkExpiryTimeMinutes  = 30
  val selfUrl                = "self"

  private val config = Configuration(
    ConfigFactory.parseString(
      s"""
        |microservice.services.email-verification{
        |  protocol    = "$protocol"
        |  host        = "$wireMockHost"
        |  port        = "$wireMockPort"
        |  template-id = "$templateId"
        |  link-expiry-time = "$linkExpiryTimeMinutes minutes"
        |}
        |self.url = "$selfUrl"
        |""".stripMargin
    )
  )

  override def fakeApplication(): Application = new GuiceApplicationBuilder().configure(config).build()

  val connector: EmailVerificationConnector = app.injector.instanceOf[EmailVerificationConnector]
  implicit val hc: HeaderCarrier = HeaderCarrier()
  implicit lazy val ec: ExecutionContext = app.injector.instanceOf[ExecutionContext]
  private val emptyJsonBody = "{}"

  "EmailVerificationConnectorImpl" when {

    "handling requests to verify emails in English" must {

      implicit val hc: HeaderCarrier = HeaderCarrier()
      val expectedUrl                = "/email-verification/verification-requests"
      val email                      = Email("email@test.com")
      val name                       = ContactName("Bob Lob")
      val trustName                  = ContactName("trust")
      val continueCall: Call         = Call("GET", s"/url")

      def body(name: ContactName): JsValue =
        Json.parse(
          s"""
             |{
             |"email": "${email.value}",
             |"templateId": "$templateId",
             |"linkExpiryDuration" : "PT${linkExpiryTimeMinutes}M",
             |"continueUrl" : "$selfUrl${continueCall.url}",
             |"templateParameters": { "name" : "${name.value}" }
             |}
             |""".stripMargin
        )

      "send a request to the email verification service with the correct details " +
        "and return the response" when {

          "handling individuals" in {
            List(
              HttpResponse(200, JsString("hi"), Map[String, Seq[String]]().empty),
              HttpResponse(409, emptyJsonBody),
              HttpResponse(500, emptyJsonBody)
            ).foreach { response =>
              when(
                method = POST,
                uri = expectedUrl,
                body = Some(body(name).toString())
              ).thenReturn(response.status,response.body)

              val result = await(
                connector.verifyEmail(email, name, continueCall, AcceptLanguage.EN).value
              ).value

              result.status shouldBe response.status
              result.body shouldBe response.body

            }
          }

          "handling trusts" in {
            val response = HttpResponse(200, JsString("hi"), Map[String, Seq[String]]().empty)
            when(
              method = POST,
              uri = expectedUrl,
              body = Some(body(trustName).toString())
            ).thenReturn(response.status,response.body)


            val result = await(
              connector.verifyEmail(email, trustName, continueCall, AcceptLanguage.EN).value
            ).value

            result.status shouldBe response.status
            result.body shouldBe response.body

          }
        }

      "return an error" when {
        "the future fails" in {

          wireMockServer.stop()

          await(
            connector.verifyEmail(email, name, continueCall, AcceptLanguage.EN).value
          ).isLeft shouldBe true

          wireMockServer.start()
        }

      }

    }
    "handling requests to verify emails in Welsh" must {

      implicit val hc: HeaderCarrier = HeaderCarrier()
      val expectedUrl                = "/email-verification/verification-requests"
      val email                      = Email("email@test.com")
      val name                       = ContactName("Bob Lob")
      val trustName                  = ContactName("trust")
      val continueCall: Call         = Call("GET", s"/url")

      def body(name: ContactName): JsValue =
        Json.parse(
          s"""
           |{
           |"email": "${email.value}",
           |"templateId": "${templateId + "_" + "cy"}",
           |"linkExpiryDuration" : "PT${linkExpiryTimeMinutes}M",
           |"continueUrl" : "$selfUrl${continueCall.url}",
           |"templateParameters": { "name" : "${name.value}" }
           |}
           |""".stripMargin
        )

      "send a request to the email verification service with the correct details " +
        "and return the response" when {

          "handling individuals" in {
            List(
              HttpResponse(200, JsString("hi"), Map[String, Seq[String]]().empty),
              HttpResponse(409, emptyJsonBody),
              HttpResponse(500, emptyJsonBody)
            ).foreach { response =>
              when(
                method = POST,
                uri = expectedUrl,
                body = Some(body(name).toString())
              ).thenReturn(response.status,response.body)

              val result = await(
                connector.verifyEmail(email, name, continueCall, AcceptLanguage.CY).value
              ).value


              result.status shouldBe response.status
              result.body shouldBe response.body
            }
          }

          "handling trusts" in {
            val response = HttpResponse(200, JsString("hi"), Map[String, Seq[String]]().empty)

            when(
              method = POST,
              uri = expectedUrl,
              body = Some(body(trustName).toString())
            ).thenReturn(response.status,response.body)

            val result = await(
              connector.verifyEmail(email, trustName, continueCall, AcceptLanguage.CY).value
            ).value

            result.status shouldBe response.status
            result.body shouldBe response.body

          }
        }
    }
  }

}
