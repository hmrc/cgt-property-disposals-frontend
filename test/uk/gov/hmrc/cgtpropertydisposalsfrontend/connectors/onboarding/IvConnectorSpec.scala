package uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.onboarding

import com.github.tomakehurst.wiremock.WireMockServer
import com.github.tomakehurst.wiremock.client.WireMock
import com.github.tomakehurst.wiremock.client.WireMock._
import com.github.tomakehurst.wiremock.core.WireMockConfiguration
import org.scalatest.BeforeAndAfterEach
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.Application
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.test.Helpers._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Error
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.onboarding.IvServiceImpl.IvStatusResponse
import uk.gov.hmrc.http.HeaderCarrier

import java.util.UUID

class IvConnectorSpec extends AnyWordSpec with Matchers with BeforeAndAfterEach {
  private implicit val hc: HeaderCarrier = HeaderCarrier()
  val Port                               = 11119
  val Host                               = "localhost"
  val wireMockServer                     = new WireMockServer(WireMockConfiguration.wireMockConfig().port(Port))

  lazy val fakeApplication: Application = new GuiceApplicationBuilder()
    .bindings()
    .configure("microservice.services.iv.port" -> "11119")
    .build()

  private val con = fakeApplication.injector.instanceOf[IvConnector]

  override def beforeEach(): Unit = {
    wireMockServer.start()
    wireMockServer.resetAll()
    WireMock.configureFor(Host, Port)
  }

  override def afterEach(): Unit =
    wireMockServer.stop()

  "getDraftReturns" should {
    val correctBody = """{ "result": "foobar" }"""
    val uuid        = UUID.randomUUID()
    val url         = s"/mdtp/journey/journeyId/$uuid"
    "call the correct endpoint" in {
      stubFor(
        get(urlPathMatching(".*")).willReturn(
          aResponse().withStatus(200).withHeader("Content-Type", "application/json").withBody(correctBody)
        )
      )

      await(con.getFailedJourneyStatus(uuid).value)

      verify(getRequestedFor(urlEqualTo(url)))
    }

    "return some parsed JSON on success" in {
      stubFor(
        get(urlPathMatching(".*")).willReturn(
          aResponse().withStatus(200).withHeader("Content-Type", "application/json").withBody(correctBody)
        )
      )

      val response = await(con.getFailedJourneyStatus(uuid).value)

      response shouldBe Right(IvStatusResponse("foobar"))
    }

    "Return error if Reject if JSON is missing fields" in {
      val invalidBody = """[]"""
      stubFor(
        get(urlPathMatching(".*")).willReturn(
          aResponse().withStatus(200).withHeader("Content-Type", "application/json").withBody(invalidBody)
        )
      )

      val response = await(con.getFailedJourneyStatus(uuid).value)
      response.isLeft shouldBe true
    }

    "Return error if malformed JSON is returned" in {
      val invalidBody = "definitely not json"
      stubFor(
        get(urlPathMatching(".*")).willReturn(
          aResponse().withStatus(200).withHeader("Content-Type", "application/json").withBody(invalidBody)
        )
      )

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
