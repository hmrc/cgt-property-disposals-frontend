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
