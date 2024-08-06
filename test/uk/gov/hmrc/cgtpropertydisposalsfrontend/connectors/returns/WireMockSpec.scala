package uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.returns

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
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.CgtReference
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.DraftReturn
import uk.gov.hmrc.http.HeaderCarrier

class WireMockSpec extends AnyWordSpec with Matchers with BeforeAndAfterEach {
  private implicit val hc: HeaderCarrier = HeaderCarrier()
  val Port                               = 11119
  val Host                               = "localhost"
  val wireMockServer                     = new WireMockServer(WireMockConfiguration.wireMockConfig().port(Port))

  lazy val fakeApplication: Application = new GuiceApplicationBuilder()
    .bindings()
    .configure(
      "microservice.services.cgt-property-disposals.port" -> "11119"
    )
    .build()

  private val con = fakeApplication.injector.instanceOf[ReturnsConnector]

  override def beforeEach: Unit = {
    wireMockServer.start()
    wireMockServer.resetAll()
    WireMock.configureFor(Host, Port)
  }

  override def afterEach: Unit =
    wireMockServer.stop()

  "getDraftReturns" should {
    val url = "/draft-returns/CGT1234589"
    "return some parsed JSON on success" in {
      val expected = Seq[DraftReturn]()
      stubFor(
        get(urlMatching(".*")).willReturn(
          aResponse()
            .withStatus(200)
            .withHeader("Content-Type", "application/json")
            .withBody(Json.toJson(expected).toString())
        )
      )

      val response = await(con.getDraftReturns(CgtReference("CGT1234589")).value)

      verify(getRequestedFor(urlEqualTo(url)))

      response.isRight              shouldBe true
      response.toOption.get.status  shouldBe 200
      response.toOption.get.body    shouldBe Json.toJson(expected).toString()
      response.toOption.get.headers should contain ("Content-Type" -> Seq("application/json"))
    }
  }
}
