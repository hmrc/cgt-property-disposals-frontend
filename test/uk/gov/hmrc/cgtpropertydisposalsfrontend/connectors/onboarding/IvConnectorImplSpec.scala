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
import com.github.tomakehurst.wiremock.client.WireMock._
import com.github.tomakehurst.wiremock.core.WireMockConfiguration.wireMockConfig
import org.scalamock.scalatest.MockFactory
import org.scalatest.BeforeAndAfterEach
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.Application
import play.api.inject.guice.{GuiceApplicationBuilder, GuiceableModule}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.{ConnectorSpec, HttpSupport}
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}

import java.util.UUID

class IvConnectorImplSpec
    extends AnyWordSpec
    with Matchers
    with MockFactory
    with HttpSupport
    with ConnectorSpec
    with BeforeAndAfterEach {

  val Port           = 11119
  val Host           = "localhost"
  val wireMockServer = new WireMockServer(wireMockConfig().port(Port))

  private implicit val hc: HeaderCarrier = HeaderCarrier()

  lazy val fakeApplication: Application = new GuiceApplicationBuilder()
    .bindings(bindModules: _*)
    .configure(
      "microservice.services.iv.port" -> "11119"
    )
    .build()

  private val con = fakeApplication.injector.instanceOf[IvConnectorImpl]

  def bindModules: Seq[GuiceableModule] = Seq()

  override def beforeEach: Unit = {
    wireMockServer.start()
    wireMockServer.resetAll()
    WireMock.configureFor(Host, Port)
  }

  override def afterEach: Unit =
    wireMockServer.stop()

  "IvConnectorImpl" should {

    "handling requests to get a failed journey status" in {

      val journeyId = UUID.randomUUID()
      val url       = s"/mdtp/journey/journeyId/${journeyId.toString}"

      stubFor(get(urlMatching(".*")).willReturn(aResponse().withStatus(200)))
      val response = con.getFailedJourneyStatus(journeyId)

      response shouldBe (HttpResponse(200, "{}"))
      verify(
        getRequestedFor(urlEqualTo(url))
      )

    }
  }
}
