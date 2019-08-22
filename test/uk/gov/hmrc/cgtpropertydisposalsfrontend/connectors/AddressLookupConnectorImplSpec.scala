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

import com.typesafe.config.ConfigFactory
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FunSuite, Matchers, WordSpec}
import play.api.libs.json.JsString
import play.api.test.Helpers._
import play.api.{Configuration, Mode}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{NINO, Postcode}
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}
import uk.gov.hmrc.play.bootstrap.config.{RunMode, ServicesConfig}

import scala.concurrent.ExecutionContext.Implicits.global

class AddressLookupConnectorImplSpec extends WordSpec with Matchers with MockFactory with HttpSupport {

  val config = Configuration(
    ConfigFactory.parseString(
      """
      |microservice {
      |  services {
      |    address-lookup {
      |      protocol = http
      |      host     = host
      |      port     = 123
      |      user-agent = agent
      |    }
      |  }
      |}
      |""".stripMargin
    )
  )

  val connector = new AddressLookupConnectorImpl(mockHttp, new ServicesConfig(config, new RunMode(config, Mode.Test)))
  "AddressLookupConnectorImpl" when {

    "handling request to lookup addresses" must {

      implicit val hc: HeaderCarrier = HeaderCarrier()
      val postcode                   = Postcode("WC1X9BH")

      "do a get http call and return the result" in {
        List(
          HttpResponse(200),
          HttpResponse(200, Some(JsString("hi"))),
          HttpResponse(500)
        ).foreach { httpResponse =>
          withClue(s"For http response [${httpResponse.toString}]") {
            mockGet(
              s"http://host:123/v2/uk/addresses",
              Map("postcode"   -> postcode.value),
              Map("User-Agent" -> "agent")
            )(Some(httpResponse))

            await(connector.lookupAddress(postcode).value) shouldBe Right(httpResponse)
          }
        }
      }

      "get rid of all spaces and turn all lower case letters to upper case letters" in {
        val response = HttpResponse(200)

        mockGet(
          s"http://host:123/v2/uk/addresses",
          Map("postcode"   -> "AB12CD"),
          Map("User-Agent" -> "agent")
        )(Some(response))

        await(connector.lookupAddress(Postcode(" ab1 2C d ")).value) shouldBe Right(response)
      }

      "return an error" when {

        "the future fails" in {
          mockGet(
            s"http://host:123/v2/uk/addresses",
            Map("postcode"   -> postcode.value),
            Map("User-Agent" -> "agent")
          )(None)

          await(connector.lookupAddress(postcode).value).isLeft shouldBe true
        }

      }

    }

  }

}
