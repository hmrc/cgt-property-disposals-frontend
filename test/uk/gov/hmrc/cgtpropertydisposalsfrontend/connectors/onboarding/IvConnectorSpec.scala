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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.onboarding

import com.github.tomakehurst.wiremock.client.WireMock._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.test.Helpers._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Error
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.onboarding.IvServiceImpl.IvStatusResponse
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.ConnectorSupport
import uk.gov.hmrc.http.HeaderCarrier

import java.util.UUID

class IvConnectorSpec extends AnyWordSpec with Matchers with ConnectorSupport {
  override lazy val serviceId = "iv"

  private val con = app.injector.instanceOf[IvConnector]

  implicit val hc: HeaderCarrier = HeaderCarrier()

  "getDraftReturns" should {
    val correctBody = """{ "result": "foobar" }"""
    val uuid        = UUID.randomUUID()
    val url         = s"/mdtp/journey/journeyId/$uuid"

    "call the correct endpoint" in {
      stubFor(get(urlPathMatching(".*")).willReturn(jsonResponse(200, correctBody)))

      await(con.getFailedJourneyStatus(uuid).value)

      verify(getRequestedFor(urlEqualTo(url)))
    }

    "return some parsed JSON on success" in {
      stubFor(get(urlPathMatching(".*")).willReturn(jsonResponse(200, correctBody)))

      val response = await(con.getFailedJourneyStatus(uuid).value)

      response shouldBe Right(IvStatusResponse("foobar"))
    }

    "Return error if Reject if JSON is missing fields" in {
      stubFor(get(urlPathMatching(".*")).willReturn(jsonResponse(200, "[]")))

      val response = await(con.getFailedJourneyStatus(uuid).value)
      response.isLeft shouldBe true
    }

    "Return error if malformed JSON is returned" in {
      stubFor(get(urlPathMatching(".*")).willReturn(jsonResponse(200, "definitely not json")))

      val response = await(con.getFailedJourneyStatus(uuid).value)

      response.isLeft shouldBe true
    }

    "Return error if 404 is returned" in {
      stubFor(get(urlPathMatching(".*")).willReturn(aResponse().withStatus(404)))

      val response = await(con.getFailedJourneyStatus(uuid).value)

      response shouldBe Left(Error(Left("Response to address lookup came back with status 404")))
    }

    "Return error if 500 is returned" in {
      stubFor(get(urlPathMatching(".*")).willReturn(aResponse().withStatus(500)))

      val response = await(con.getFailedJourneyStatus(uuid).value)

      response shouldBe Left(Error(Left("Response to address lookup came back with status 500")))
    }
  }
}
