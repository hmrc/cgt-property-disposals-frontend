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
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import play.api.Configuration
import play.api.libs.json.JsString
import play.api.test.Helpers._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.AddressLookupConnector.LookupAddressByPostcode
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Postcode
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import scala.concurrent.ExecutionContext.Implicits.global

class AddressLookupConnectorImplSpec extends AnyWordSpec with Matchers with MockFactory with HttpSupport {

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

  val addresssLookupUrl = "http://host:123/lookup"

  val connector             = new AddressLookupConnectorImpl(
    mockHttp,
    new ServicesConfig(config)
  )
  private val emptyJsonBody = "{}"

  "AddressLookupConnectorImpl" when {

    "handling request to lookup addresses" must {

      implicit val hc: HeaderCarrier = HeaderCarrier()
      val postcode                   = Postcode("WC1X9BH")

      "do a get http call and return the result" in {

        val lookupAddressByPostcode = LookupAddressByPostcode(
          postcode.value.replaceAllLiterally(" ", "").toUpperCase,
          None
        )

        List(
          HttpResponse(200, emptyJsonBody),
          HttpResponse(200, JsString("hi"), Map[String, Seq[String]]().empty),
          HttpResponse(500, emptyJsonBody)
        ).foreach { httpResponse =>
          withClue(s"For http response [${httpResponse.toString}]") {
            mockPost(
              addresssLookupUrl,
              Seq("User-Agent" -> "agent"),
              lookupAddressByPostcode
            )(Some(httpResponse))

            await(connector.lookupAddress(postcode, None).value) shouldBe Right(
              httpResponse
            )
          }
        }
      }

      "include the filter in the query parameters if one is passed in" in {
        val filter: String          = "8"
        val lookupAddressByPostcode = LookupAddressByPostcode(
          postcode.value.replaceAllLiterally(" ", "").toUpperCase,
          Some(filter)
        )
        val httpResponse            = HttpResponse(200, emptyJsonBody)
        mockPost(
          addresssLookupUrl,
          Seq("User-Agent" -> "agent"),
          lookupAddressByPostcode
        )(Some(httpResponse))

        await(
          connector.lookupAddress(postcode, Some(filter)).value
        ) shouldBe Right(httpResponse)
      }

      "get rid of all spaces and turn all lower case letters to upper case letters" in {
        val response = HttpResponse(200, emptyJsonBody)
        val postcode = Postcode("AB12CD")

        val lookupAddressByPostcode = LookupAddressByPostcode(
          postcode.value.replaceAllLiterally(" ", "").toUpperCase,
          None
        )
        mockPost(
          addresssLookupUrl,
          Seq(("User-Agent" -> "agent")),
          lookupAddressByPostcode
        )(Some(response))

        await(
          connector.lookupAddress(Postcode(" ab1 2C d "), None).value
        ) shouldBe Right(response)
      }

      "return an error" when {
        val lookupAddressByPostcode = LookupAddressByPostcode(
          postcode.value.replaceAllLiterally(" ", "").toUpperCase,
          None
        )

        "the future fails" in {
          mockPost(
            addresssLookupUrl,
            Seq("User-Agent" -> "agent"),
            lookupAddressByPostcode
          )(None)

          await(
            connector.lookupAddress(postcode, None).value
          ).isLeft shouldBe true
        }

      }

    }

  }

}
