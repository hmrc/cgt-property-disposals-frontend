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

import com.github.tomakehurst.wiremock.client.ResponseDefinitionBuilder
import com.github.tomakehurst.wiremock.client.WireMock.aResponse
import org.scalatest.{Suite, TestSuite}
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.Application
import play.api.inject.guice.GuiceApplicationBuilder
import uk.gov.hmrc.http.test.WireMockSupport

trait ConnectorSupport extends WireMockSupport with GuiceOneAppPerSuite {
  this: Suite & TestSuite =>
  def serviceId: String

  val selfUrl = "https://self:999"

  override def fakeApplication(): Application = GuiceApplicationBuilder()
    .configure(s"microservice.services.$serviceId.port" -> s"$wireMockPort")
    .configure("self.url" -> s"$selfUrl")
    .build()

  def jsonResponse(status: Int, body: String): ResponseDefinitionBuilder =
    aResponse().withStatus(status).withHeader("Content-Type", "application/json").withBody(body)
}
