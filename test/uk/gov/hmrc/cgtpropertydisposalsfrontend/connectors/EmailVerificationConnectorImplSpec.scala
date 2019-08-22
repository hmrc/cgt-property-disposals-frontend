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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors

import java.util.UUID

import com.typesafe.config.ConfigFactory
import org.scalamock.scalatest.MockFactory
import org.scalatest.{Matchers, WordSpec}
import play.api.Configuration
import play.api.libs.json.{JsString, Json}
import play.api.test.Helpers._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.routes
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Email
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}

import scala.concurrent.ExecutionContext.Implicits.global

class EmailVerificationConnectorImplSpec extends WordSpec with Matchers with MockFactory with HttpSupport {

  val (protocol, host, port) = ("http", "host", "port")
  val templateId             = "id"
  val linkExpiryTimeMinutes  = 30
  val selfUrl                = "self"

  val config = Configuration(
    ConfigFactory.parseString(
      s"""
        |microservice.services.email-verification{
        |  protocol    = "$protocol"
        |  host        = "$host"
        |  port        = "$port"
        |  template-id = "$templateId"
        |  link-expiry-time = "$linkExpiryTimeMinutes minutes"
        |}
        |self.url = "$selfUrl"
        |""".stripMargin
    )
  )

  val connector = new EmailVerificationConnectorImpl(mockHttp, config)

  "EmailVerificationConnectorImpl" when {

    "handling requests to verify emails" must {

      implicit val hc: HeaderCarrier = HeaderCarrier()
      val expectedUrl =
        s"$protocol://$host:$port/email-verification/verification-requests"
      val email = Email("email@test.com")
      val id    = UUID.randomUUID()
      val name  = "Bob"

      val body = Json.parse(
        s"""
           |{
           |"email": "${email.value}",
           |"templateId": "$templateId",
           |"templateParameters": { "name" : "$name" },
           |"linkExpiryDuration" : "PT${linkExpiryTimeMinutes}M",
           |"continueUrl" : "$selfUrl${routes.EmailController
             .verifyEmail(id)
             .url}"
           |}
           |""".stripMargin
      )

      "send a request to the email verification service with the correct details " +
        "and return the response" in {
        List(
          HttpResponse(200, Some(JsString("hi"))),
          HttpResponse(409),
          HttpResponse(500)
        ).foreach { response =>
          mockPost(expectedUrl, Map.empty[String, String], body)(Some(response))

          await(connector.verifyEmail(email, id, name).value) shouldBe Right(response)
        }
      }

      "return an error" when {

        "the future fails" in {
          mockPost(expectedUrl, Map.empty[String, String], body)(None)

          await(connector.verifyEmail(email, id, name).value).isLeft shouldBe true
        }

      }

    }

  }

}
