/*
 * Copyright 2024 HM Revenue & Customs
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

import com.github.tomakehurst.wiremock.WireMockServer
import com.github.tomakehurst.wiremock.client.WireMock
import com.github.tomakehurst.wiremock.client.WireMock._
import com.github.tomakehurst.wiremock.core.WireMockConfiguration
import org.scalatest.BeforeAndAfterEach
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.Application
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.json.Json
import play.api.test.Helpers._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.AddressLookupConnector.LookupAddressByPostcode
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Error
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Postcode
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.UKAddressLookupServiceImpl.{AddressLookupResponse, RawAddress}
import uk.gov.hmrc.http.HeaderCarrier

import scala.util.chaining.scalaUtilChainingOps

class LookupConnectorSpec extends AnyWordSpec with Matchers with BeforeAndAfterEach {
  private implicit val hc: HeaderCarrier = HeaderCarrier()
  val Port                               = 11119
  val Host                               = "localhost"
  val wireMockServer                     = new WireMockServer(WireMockConfiguration.wireMockConfig().port(Port))

  lazy val fakeApplication: Application = new GuiceApplicationBuilder()
    .bindings()
    .configure(
      "microservice.services.address-lookup.port" -> "11119"
    )
    .build()

  private val con = fakeApplication.injector.instanceOf[AddressLookupConnector]

  override def beforeEach(): Unit = {
    wireMockServer.start()
    wireMockServer.resetAll()
    WireMock.configureFor(Host, Port)
  }

  override def afterEach(): Unit =
    wireMockServer.stop()

  "getDraftReturns" should {
    val correctBody =
      """
        |[
        |  {
        |    "address": {
        |      "lines": [ "line1", "line2" ],
        |      "town": "town",
        |      "county" : "county",
        |      "postcode": "ABC 123"
        |    }
        |  }
        |]
        |""".stripMargin
    val url         = "/lookup"
    "call the correct endpoint" in {
      stubFor(
        post(urlPathMatching(".*")).willReturn(
          aResponse().withStatus(200).withHeader("Content-Type", "application/json").withBody(correctBody)
        )
      )

      await(con.lookupAddress(Postcode("ABC 123"), Some("filter")).value)

      val expectedJson = Json.toJson(LookupAddressByPostcode("ABC123", Some("filter")))
      verify(postRequestedFor(urlEqualTo(url)).withRequestBody(equalTo(expectedJson.toString())))
    }

    "return some parsed JSON on success" in {
      stubFor(
        post(urlPathMatching(".*")).willReturn(
          aResponse().withStatus(200).withHeader("Content-Type", "application/json").withBody(correctBody)
        )
      )

      val response = await(con.lookupAddress(Postcode("ABC 123"), Some("filter")).value)

      response shouldBe
        (RawAddress(
          lines = List("line1", "line2"),
          town = "town",
          county = Some("county"),
          postcode = "ABC 123"
        ) pipe (List(_)) pipe AddressLookupResponse pipe (Right(_)))
    }

    "Return error if Reject if JSON is missing fields" in {
      val invalidBody =
        """
          |[
          |  {
          |    "address": {
          |      "town": "town",
          |      "county" : "county",
          |      "postcode": "ABC 123"
          |    }
          |  }
          |]
          |""".stripMargin
      stubFor(
        post(urlPathMatching(".*")).willReturn(
          aResponse().withStatus(200).withHeader("Content-Type", "application/json").withBody(invalidBody)
        )
      )

      val response = await(con.lookupAddress(Postcode("ABC 123"), Some("filter")).value)
      response.isLeft shouldBe true
    }

    "Return error if malformed JSON is returned" in {
      val invalidBody = "definitely not json"
      stubFor(
        post(urlPathMatching(".*")).willReturn(
          aResponse().withStatus(200).withHeader("Content-Type", "application/json").withBody(invalidBody)
        )
      )

      val response = await(con.lookupAddress(Postcode("ABC 123"), Some("filter")).value)
      response.isLeft shouldBe true
    }

    "Return error if 404 is returned" in {
      stubFor(post(urlPathMatching(".*")).willReturn(aResponse().withStatus(404)))

      val response = await(con.lookupAddress(Postcode("ABC 123"), Some("filter")).value)

      response shouldBe Left(Error(Left("Response to address lookup came back with status 404")))
    }

    "Return error if 500 is returned" in {
      stubFor(post(urlPathMatching(".*")).willReturn(aResponse().withStatus(500)))

      val response = await(con.lookupAddress(Postcode("ABC 123"), Some("filter")).value)

      response shouldBe Left(Error(Left("Response to address lookup came back with status 500")))
    }
  }
}
