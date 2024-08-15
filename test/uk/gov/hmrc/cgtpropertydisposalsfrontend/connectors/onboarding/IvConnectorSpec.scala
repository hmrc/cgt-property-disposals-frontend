package uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.onboarding

import com.github.tomakehurst.wiremock.client.WireMock._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.test.Helpers._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Error
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.onboarding.IvServiceImpl.IvStatusResponse
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.ConnectorSupport

import java.util.UUID

class IvConnectorSpec extends AnyWordSpec with Matchers with ConnectorSupport {
  override lazy val serviceId = "iv"

  private val con = fakeApplication.injector.instanceOf[IvConnector]

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
