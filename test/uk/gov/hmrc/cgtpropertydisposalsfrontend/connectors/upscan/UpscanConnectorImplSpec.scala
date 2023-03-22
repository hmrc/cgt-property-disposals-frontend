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
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.Configuration
import play.api.libs.json.JsString
import play.api.mvc.Call
import play.api.test.Helpers._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.HttpSupport
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Error
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.FileUploadGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.IdGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.upscan.{UploadReference, UpscanInitiateRequest, UpscanUpload}
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class UpscanConnectorImplSpec extends AnyWordSpec with Matchers with MockFactory with HttpSupport {

  val config = Configuration(
    ConfigFactory.parseString(
      """
        | self {
        |   url = host1.com
        |  },
        |  microservice {
        |    services {
        |      upscan-initiate {
        |        protocol = http
        |        host     = host2
        |        port     = 123
        |        user-agent = agent
        |        max-file-size = 1234
        |      },
        |      cgt-property-disposals {
        |        protocol = http
        |        host     = host3
        |        port     = 123
        |      }
        |   }
        |}
        |""".stripMargin
    )
  )

  val connector =
    new UpscanConnectorImpl(
      mockHttp,
      config,
      new ServicesConfig(config)
    )

  private val emptyJsonBody = "{}"

  "UpscanConnectorImplSpec" when {

    implicit val hc: HeaderCarrier = HeaderCarrier()
    val reference                  = sample[UploadReference]
    val upload                     = sample[UpscanUpload]
    val baseUrl                    = "http://host3:123/cgt-property-disposals"

    "Inititalising" must {
      val expectedUrl  = "http://host2:123/upscan/v2/initiate"
      val mockSuccess  = Call("GET", "/mock-success")
      val mockFailiure = Call("GET", "/mock-fail")

      val payload = UpscanInitiateRequest(
        s"$baseUrl/upscan-call-back/upload-reference/${reference.value}",
        s"host1.com${mockSuccess.url}",
        s"host1.com${mockFailiure.url}",
        0,
        1234
      )
      behave like upscanConnectorBehaviour(
        mockPost[UpscanInitiateRequest](
          expectedUrl,
          Seq.empty,
          payload
        ),
        () => connector.initiate(mockFailiure, mockSuccess, reference)
      )
    }

    "getting the upscan upload" must {
      val expectedUrl = s"$baseUrl/upscan/upload-reference/${reference.value}"
      behave like upscanConnectorBehaviour(
        mockGet[HttpResponse](expectedUrl),
        () => connector.getUpscanUpload(reference)
      )
    }

    "saving upscan upload" when {
      behave like upscanConnectorBehaviour(
        mockPost[UpscanUpload](s"$baseUrl/upscan", Seq.empty, upload),
        () => connector.saveUpscanUpload(upload)
      )
    }

  }

  def upscanConnectorBehaviour(
    mockResponse: Option[HttpResponse] => Unit,
    performCall: () => EitherT[Future, Error, HttpResponse]
  ) = {
    "do a get http call and return the result" in {
      List(
        HttpResponse(200, emptyJsonBody),
        HttpResponse(200, JsString("hi"), Map[String, Seq[String]]().empty)
      ).foreach { httpResponse =>
        withClue(s"For http response [${httpResponse.toString}]") {
          mockResponse(Some(httpResponse))

          await(performCall().value) shouldBe Right(httpResponse)
        }
      }
    }

    "return an error" when {

      "Internal server error" in {
        List(
          HttpResponse(500, emptyJsonBody)
        ).foreach { httpResponse =>
          withClue(s"For http response [${httpResponse.toString}]") {
            mockResponse(Some(httpResponse))

            await(performCall().value).isLeft shouldBe true
          }
        }
      }
      "the future fails" in {
        mockResponse(None)
        await(performCall().value).isLeft shouldBe true
      }

    }
  }
}
