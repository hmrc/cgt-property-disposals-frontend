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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.util

import com.github.tomakehurst.wiremock.WireMockServer
import com.github.tomakehurst.wiremock.client.WireMock.aResponse
import com.github.tomakehurst.wiremock.client.{ResponseDefinitionBuilder, WireMock}
import com.github.tomakehurst.wiremock.core.WireMockConfiguration
import org.scalatest.{BeforeAndAfterEach, Suite}
import play.api.Application
import play.api.inject.guice.GuiceApplicationBuilder
import uk.gov.hmrc.http.HeaderCarrier

trait ConnectorSupport extends BeforeAndAfterEach {
  this: Suite =>

  lazy val serviceId: String = throw new Exception("You must override `serviceId` in your test class")

  implicit val hc: HeaderCarrier = HeaderCarrier()
  val Port                       = 11119
  val Host                       = "localhost"
  val wireMockServer             = new WireMockServer(WireMockConfiguration.wireMockConfig().port(Port))

  lazy val fakeApplication: Application = new GuiceApplicationBuilder()
    .bindings()
    .configure(s"microservice.services.$serviceId.port" -> "11119")
    .build()

  override def beforeEach(): Unit = {
    wireMockServer.start()
    wireMockServer.resetAll()
    WireMock.configureFor(Host, Port)
  }

  override def afterEach(): Unit =
    wireMockServer.stop()

  def jsonResponse(status: Int, body: String): ResponseDefinitionBuilder =
    aResponse().withStatus(200).withHeader("Content-Type", "application/json").withBody(body)
}
