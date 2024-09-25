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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.upscan

import cats.data.EitherT
import com.typesafe.config.ConfigFactory
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.{Application, Configuration}
import play.api.libs.json.{JsString, Json}
import play.api.mvc.Call
import play.api.test.Helpers._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.HttpSupport
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Error
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.FileUploadGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.IdGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.upscan.{UploadReference, UpscanInitiateRequest, UpscanUpload}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.WireMockMethods
import uk.gov.hmrc.http.test.WireMockSupport
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}
import scala.concurrent.Future

class UpscanConnectorImplSpec
    extends AnyWordSpec
    with Matchers
    with MockFactory
    with GuiceOneAppPerSuite
    with WireMockSupport
    with WireMockMethods
    with EitherValues
    with HttpSupport {
  private val config = Configuration(
    ConfigFactory.parseString(
      s"""
        | self {
        |   url = host1.com
        |  },
        |  microservice {
        |    services {
        |      upscan-initiate {
        |        protocol = http
        |        host     = $wireMockHost
        |        port     = $wireMockPort
        |        user-agent = agent
        |        max-file-size = 1234
        |      },
        |      cgt-property-disposals {
        |        protocol = http
        |        host     = $wireMockHost
        |        port     = $wireMockPort
        |      }
        |   }
        |}
        |""".stripMargin
    )
  )

  override def fakeApplication(): Application = new GuiceApplicationBuilder().configure(config).build()
  val connector: UpscanConnector              = app.injector.instanceOf[UpscanConnector]

  private val emptyJsonBody = "{}"

  "UpscanConnectorImplSpec" when {
    implicit val hc: HeaderCarrier = HeaderCarrier()
    val reference                  = sample[UploadReference]
    val upload                     = sample[UpscanUpload]

    "Initialising" must {
      val expectedUrl = "/upscan/v2/initiate"
      val mockSuccess = Call("GET", "/mock-success")
      val mockFailure = Call("GET", "/mock-fail")

      val payload = UpscanInitiateRequest(
        s"http://$wireMockHost:$wireMockPort/cgt-property-disposals/upscan-call-back/upload-reference/${reference.value}",
        s"host1.com${mockSuccess.url}",
        s"host1.com${mockFailure.url}",
        0,
        1234
      )
      behave like upscanConnectorBehaviour(
        when(POST, expectedUrl, body = Some(Json.toJson(payload).toString())),
        () => connector.initiate(mockFailure, mockSuccess, reference)
      )

    }

    "getting the upscan upload" must {
      val expectedUrl = s"/cgt-property-disposals/upscan/upload-reference/${reference.value}"
      behave like upscanConnectorBehaviour(
        when(GET, expectedUrl),
        () => connector.getUpscanUpload(reference)
      )
    }

    "saving upscan upload" when {
      behave like upscanConnectorBehaviour(
        when(POST, "/cgt-property-disposals/upscan", body = Some(Json.toJson(upload).toString())),
        () => connector.saveUpscanUpload(upload)
      )
    }
  }

  private def upscanConnectorBehaviour(
    mockResponse: Mapping,
    performCall: () => EitherT[Future, Error, HttpResponse]
  ): Unit = {
    "do a get http call and return the result" in {
      List(
        HttpResponse(200, emptyJsonBody),
        HttpResponse(200, JsString("hi"), Map[String, Seq[String]]().empty)
      ).foreach { httpResponse =>
        withClue(s"For http response [${httpResponse.toString}]") {
          mockResponse.thenReturn(httpResponse.status, httpResponse.body)
          val result = await(performCall().value).value
          result.status shouldBe httpResponse.status
          result.body   shouldBe httpResponse.body
        }
      }
    }

    "return an error" when {
      "Internal server error" in {
        List(
          HttpResponse(500, emptyJsonBody)
        ).foreach { httpResponse =>
          withClue(s"For http response [${httpResponse.toString}]") {
            mockResponse.thenReturn(httpResponse.status, httpResponse.body)
            await(performCall().value).isLeft shouldBe true
          }
        }
      }

      "the future fails" in {
        wireMockServer.stop()
        await(performCall().value).isLeft shouldBe true
        wireMockServer.start()
      }
    }
  }
}
