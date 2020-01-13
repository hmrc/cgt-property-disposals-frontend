/*
 * Copyright 2020 HM Revenue & Customs
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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors

import akka.stream.scaladsl.Source
import akka.util.ByteString
import com.typesafe.config.ConfigFactory
import org.scalamock.scalatest.MockFactory
import org.scalatest.{Matchers, WordSpec}
import play.api.Configuration
import play.api.libs.json.JsString
import play.api.libs.ws.ahc.AhcWSResponse
import play.api.libs.ws.ahc.cache.{CacheableHttpResponseBodyPart, CacheableHttpResponseHeaders, CacheableHttpResponseStatus}
import play.api.mvc.MultipartFormData
import play.api.test.Helpers._
import play.shaded.ahc.io.netty.handler.codec.http.DefaultHttpHeaders
import play.shaded.ahc.org.asynchttpclient.Response
import play.shaded.ahc.org.asynchttpclient.uri.Uri
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Error
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.CgtReference
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}

@SuppressWarnings(Array("org.wartremover.warts.Var", "org.wartremover.warts.Any"))
class UpscanConnectorSpec extends WordSpec with Matchers with MockFactory with HttpSupport {

  private def buildWsResponse(status: Int) =
    new AhcWSResponse(
      new Response.ResponseBuilder()
        .accumulate(
          new CacheableHttpResponseStatus(
            Uri.create("https://bucketname.s3.eu-west-2.amazonaws.com"),
            status,
            "status text",
            "protocols!"
          )
        )
        .accumulate(new CacheableHttpResponseHeaders(false, new DefaultHttpHeaders().add("My-Header", "value")))
        .accumulate(new CacheableHttpResponseBodyPart("error body".getBytes(), true))
        .build()
    )

  val config = Configuration(
    ConfigFactory.parseString(
      """
        | microservice.services {
        |    upscan-initiate {
        |        protocol = http
        |        host = host
        |        port = 123
        |        max-uploads = 5
        |        min-file-size = 0
        |        max-file-size = 5242880
        |        upscan-store.expiry-time = 7 days
        |    }
        | }
        | self.url = "http://localhost:7020"
        |""".stripMargin
    )
  )

  val connector = new UpscanConnectorImpl(mockHttp, mockWsClient, config)

  "UpscanConnectorImpl" when {

    "handling upscan initiate requests" must {

      implicit val hc: HeaderCarrier = HeaderCarrier()

      val expectedUrl       = s"http://host:123/upscan/v2/initiate"
      val cgtReference      = sample[CgtReference]
      val callBackUrl       = s"http://localhost:7020/capital-gains-tax-uk-property/upscan-call-back/cgt-reference/${cgtReference.value}"
      val successUrl        = s"http://localhost:7020/capital-gains-tax-uk-property/upscan-success/cgt-reference/${cgtReference.value}"
      val errorUrl          = s"http://localhost:7020/capital-gains-tax-uk-property/upscan-error/cgt-reference/${cgtReference.value}"
      val expectedInitiated = UpscanInitiateRequest(callBackUrl, successUrl, errorUrl, 0, 5242880)

      "process unsuccessful post calls from S3" in {
        List(
          HttpResponse(400),
          HttpResponse(500, Some(JsString("error")))
        ).foreach { httpResponse =>
          withClue(s"For http response [${httpResponse.toString}]") {
            mockPost(expectedUrl, Map.empty, expectedInitiated)(Some(httpResponse))
            await(connector.initiate(cgtReference).value) shouldBe Left(Error("S3 did not return 200 status code"))
          }
        }
      }

      "process successful calls from S3" in {
        List(
          HttpResponse(200),
          HttpResponse(200, Some(JsString("hi")))
        ).foreach { httpResponse =>
          withClue(s"For http response [${httpResponse.toString}]") {
            mockPost(expectedUrl, Map.empty, expectedInitiated)(Some(httpResponse))
            await(connector.initiate(cgtReference).value) shouldBe Right(httpResponse)
          }
        }
      }
    }
  }

  "handling file upload requests with error responses" must {

    implicit val hc: HeaderCarrier = HeaderCarrier()

    val s3Url        = s"https://bucketname.s3.eu-west-2.amazonaws.com"
    val cgtReference = sample[CgtReference]
    val callBackUrl  = s"http://localhost:7020/upscan-call-back/cgt-reference/${cgtReference.value}"
    val successUrl   = s"http://localhost:7020/upscan-success/cgt-reference/${cgtReference.value}"
    val errorUrl     = s"http://localhost:7020/upscan-error/cgt-reference/${cgtReference.value}"

    val parts: Source[MultipartFormData.Part[Source[ByteString, _]], _] =
      Source.apply(Map("key" -> List("V1")).flatMap {
        case (key, values) =>
          values.map(value => MultipartFormData.DataPart(key, value): MultipartFormData.Part[Source[ByteString, _]])
      })

    val S3_4xx = new AhcWSResponse(
      new Response.ResponseBuilder()
        .accumulate(new CacheableHttpResponseStatus(Uri.create(callBackUrl), 400, "status text", "protocols!"))
        .accumulate(new CacheableHttpResponseHeaders(false, new DefaultHttpHeaders().add("My-Header", "value")))
        .accumulate(new CacheableHttpResponseBodyPart("error body".getBytes(), true))
        .build()
    )

    val S3_5xx = new AhcWSResponse(
      new Response.ResponseBuilder()
        .accumulate(new CacheableHttpResponseStatus(Uri.create(callBackUrl), 500, "status text", "protocols!"))
        .accumulate(new CacheableHttpResponseHeaders(false, new DefaultHttpHeaders().add("My-Header", "value")))
        .accumulate(new CacheableHttpResponseBodyPart("error body".getBytes(), true))
        .build()
    )

    val S3_204 = new AhcWSResponse(
      new Response.ResponseBuilder()
        .accumulate(new CacheableHttpResponseStatus(Uri.create(callBackUrl), 204, "status text", "protocols!"))
        .accumulate(new CacheableHttpResponseHeaders(false, new DefaultHttpHeaders().add("My-Header", "value")))
        .accumulate(new CacheableHttpResponseBodyPart("error body".getBytes(), true))
        .build()
    )

    "process unsuccessful responses from S3" in {
      List(
        buildWsResponse(400),
        buildWsResponse(500)
      ).foreach { httpResponse =>
        withClue(s"For http response [${httpResponse.toString}]") {
          mockPostMultiPartForm(s3Url, parts)(Some(httpResponse))
          await(
            connector
              .upload(
                s3Url,
                MultipartFormData(Map("key" -> List("V1")), Seq.empty, Seq.empty): MultipartFormData[
                  Source[ByteString, _]
                ]
              )
              .value
          ) shouldBe Left(
            Error("error body")
          )
        }
      }
    }

    "process successful responses from S3" in {
      List(
        buildWsResponse(204)
      ).foreach { httpResponse =>
        withClue(s"For http response [${httpResponse.toString}]") {
          mockPostMultiPartForm(s3Url, parts)(Some(httpResponse))
          await(
            connector
              .upload(
                s3Url,
                MultipartFormData(Map("key" -> List("V1")), Seq.empty, Seq.empty): MultipartFormData[
                  Source[ByteString, _]
                ]
              )
              .value
          ) shouldBe Right(())
        }
      }
    }
  }
}
