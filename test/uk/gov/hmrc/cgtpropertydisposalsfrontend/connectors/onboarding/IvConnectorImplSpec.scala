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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.onboarding

import com.github.tomakehurst.wiremock.WireMockServer
import com.github.tomakehurst.wiremock.client.WireMock
import com.github.tomakehurst.wiremock.client.WireMock.{aResponse, equalTo, get, getRequestedFor, status, stubFor, urlEqualTo, urlMatching, verify}
import com.github.tomakehurst.wiremock.core.WireMockConfiguration.wireMockConfig
import com.typesafe.config.ConfigFactory
import org.scalamock.scalatest.MockFactory
import org.scalatest.BeforeAndAfterEach
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.Configuration
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.common.{FakeRequestHelper, WithCommonFakeApplication}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.{ConnectorSpec, HttpSupport}
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import java.util.UUID
import scala.concurrent.ExecutionContext.Implicits.global

class IvConnectorImplSpec
    extends AnyWordSpec
    with Matchers
    with MockFactory
    with HttpSupport
    with ConnectorSpec
    with FakeRequestHelper
    with BeforeAndAfterEach
    with WithCommonFakeApplication {

  val Port           = 11119
  val Host           = "localhost"
  val wireMockServer = new WireMockServer(wireMockConfig().port(Port))

  implicit val hc: HeaderCarrier = HeaderCarrier()

  private val con = fakeApplication.injector.instanceOf[IvConnectorImpl]

  override def beforeEach: Unit = {
    wireMockServer.start()
    wireMockServer.resetAll()
    WireMock.configureFor(Host, Port)
  }

  override def afterEach: Unit =
    wireMockServer.stop()

  "IvConnectorImpl" when {

    "handling requests to get a failed journey status" must {

      val journeyId = UUID.randomUUID()
      val url       = s"/mdtp/journey/journeyId/${journeyId.toString}"

      stubFor(get(urlMatching(".*")).willReturn(aResponse().withStatus(200)))
      val response = con.getFailedJourneyStatus(journeyId)

      response shouldBe Some(status(200))
      verify(
        getRequestedFor(urlEqualTo(url))
          .withHeader("Content-Type", equalTo("application/json"))
      )

    }
  }
}
