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

import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, Materializer}
import com.typesafe.config.ConfigFactory
import org.scalamock.scalatest.MockFactory
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpec}
import play.api.http.HeaderNames.USER_AGENT
import play.api.libs.json.{JsString, Json}
import play.api.test.Helpers._
import play.api.{Configuration, Mode}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Error
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.{CgtReference, DraftReturnId}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.upscan.{UpscanFileDescriptor, UpscanInitiateReference}
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}
import uk.gov.hmrc.play.bootstrap.config.{RunMode, ServicesConfig}

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

@SuppressWarnings(Array("org.wartremover.warts.Var", "org.wartremover.warts.Any"))
class UpscanConnectorSpec extends WordSpec with Matchers with MockFactory with HttpSupport with BeforeAndAfterAll {

  val config = Configuration(
    ConfigFactory.parseString(
      """
        | microservice.services {
        |   cgt-property-disposals {
        |      host = localhost
        |      port = 7021
        |   }
        |
        |    upscan-initiate {
        |        protocol = http
        |        host = host
        |        port = 123
        |        max-uploads = 5
        |        max-file-size = 5242880
        |        upscan-store.expiry-time = 7 days
        |    }
        | }
        | self.url = "http://localhost:7020"
        |""".stripMargin
    )
  )

  implicit val system: ActorSystem = ActorSystem()

  implicit val mat: Materializer = ActorMaterializer()

  override def afterAll(): Unit = {
    Await.result(system.terminate(), 10.seconds)
    super.afterAll()
  }

  val connector =
    new UpscanConnectorImpl(mockHttp, mockWsClient, config, new ServicesConfig(config, new RunMode(config, Mode.Test)))

  "UpscanConnectorImpl" when {

    val draftReturnId = sample[DraftReturnId]

    "handling requests to get file descriptor status" must {
      implicit val hc: HeaderCarrier = HeaderCarrier()

      val upscanInitiateReference = UpscanInitiateReference("some-upscan-ref")
      val expectedUrl =
        s"http://localhost:7021/cgt-property-disposals/upscan-fd/draft-return-id/${draftReturnId.value}/upscan-reference/${upscanInitiateReference.value}"
      val cgtReference         = sample[CgtReference]
      val upscanFileDescriptor = sample[UpscanFileDescriptor].copy(cgtReference = cgtReference)

      "process unsuccessful calls from backend service" in {
        List(
          HttpResponse(400),
          HttpResponse(500, Some(JsString("error")))
        ).foreach { httpResponse =>
          withClue(s"For http response [${httpResponse.toString}]") {
            mockGet(expectedUrl, Map.empty)(Some(httpResponse))
            await(connector.getFileDescriptor(draftReturnId, upscanInitiateReference).value).isLeft shouldBe true
          }
        }
      }
      "process successful calls from backend service" in {
        List(
          HttpResponse(200, Some(Json.toJson[UpscanFileDescriptor](upscanFileDescriptor)))
        ).foreach { httpResponse =>
          withClue(s"For http response [${httpResponse.toString}]") {
            mockGet(expectedUrl, Map.empty)(Some(httpResponse))
            await(connector.getFileDescriptor(draftReturnId, upscanInitiateReference).value) shouldBe Right(
              Some(upscanFileDescriptor)
            )
          }
        }
      }
    }

    "handling requests to update file descriptor status" must {
      implicit val hc: HeaderCarrier = HeaderCarrier()

      val upscanFileDescriptor = sample[UpscanFileDescriptor]
      val expectedUrl          = s"http://localhost:7021/cgt-property-disposals/upscan-file-descriptor/status"

      "process unsuccessful calls from backend service" in {
        List(
          HttpResponse(400),
          HttpResponse(500, Some(JsString("error")))
        ).foreach { httpResponse =>
          withClue(s"For http response [${httpResponse.toString}]") {
            mockPut(expectedUrl, upscanFileDescriptor, Seq.empty)(Some(httpResponse))
            await(connector.updateUpscanFileDescriptorStatus(upscanFileDescriptor).value).isLeft shouldBe true
          }
        }
      }
      "process successful calls from backend service" in {
        List(
          HttpResponse(200, Some(Json.toJson[UpscanFileDescriptor](upscanFileDescriptor)))
        ).foreach { httpResponse =>
          withClue(s"For http response [${httpResponse.toString}]") {
            mockPut(expectedUrl, upscanFileDescriptor, Seq.empty)(Some(httpResponse))
            await(connector.updateUpscanFileDescriptorStatus(upscanFileDescriptor).value) shouldBe Right(())
          }
        }
      }
    }

    "handling requests to save upscan initiate response" must {
      implicit val hc: HeaderCarrier = HeaderCarrier()

      val upscanFileDescriptor = sample[UpscanFileDescriptor]
      val expectedUrl          = s"http://localhost:7021/cgt-property-disposals/upscan-file-descriptor"

      "process unsuccessful calls from backend service" in {
        List(
          HttpResponse(400),
          HttpResponse(500, Some(JsString("error")))
        ).foreach { httpResponse =>
          withClue(s"For http response [${httpResponse.toString}]") {
            mockPost(expectedUrl, Map.empty, upscanFileDescriptor)(Some(httpResponse))
            await(connector.saveUpscanFileDescriptors(upscanFileDescriptor).value).isLeft shouldBe true
          }
        }
      }
      "process successful calls from backend service" in {
        List(
          HttpResponse(200, Some(Json.toJson[UpscanFileDescriptor](upscanFileDescriptor)))
        ).foreach { httpResponse =>
          withClue(s"For http response [${httpResponse.toString}]") {
            mockPost(expectedUrl, Map.empty, upscanFileDescriptor)(Some(httpResponse))
            await(connector.saveUpscanFileDescriptors(upscanFileDescriptor).value) shouldBe Right(())
          }
        }
      }
    }

    "handling upscan initiate requests" must {

      implicit val hc: HeaderCarrier = HeaderCarrier()

      val expectedUrl = s"http://host:123/upscan/initiate"
      val callBackUrl =
        s"http://localhost:7021/cgt-property-disposals/upscan-call-back/draft-return-id/${draftReturnId.value}"
      val expectedInitiated = UpscanInitiateRequest(callBackUrl, 5242880)

      "process unsuccessful post calls from S3" in {
        List(
          HttpResponse(400),
          HttpResponse(500, Some(JsString("error")))
        ).foreach { httpResponse =>
          withClue(s"For http response [${httpResponse.toString}]") {
            mockPost(expectedUrl, Map(USER_AGENT -> "cgt-property-disposals-frontend"), expectedInitiated)(
              Some(httpResponse)
            )
            await(connector.initiate(draftReturnId).value) shouldBe Left(Error("S3 did not return 200 status code"))
          }
        }
      }

      "process successful calls from S3" in {
        List(
          HttpResponse(200),
          HttpResponse(200, Some(JsString("hi")))
        ).foreach { httpResponse =>
          withClue(s"For http response [${httpResponse.toString}]") {
            mockPost(expectedUrl, Map(USER_AGENT -> "cgt-property-disposals-frontend"), expectedInitiated)(
              Some(httpResponse)
            )
            await(connector.initiate(draftReturnId).value) shouldBe Right(httpResponse)
          }
        }
      }
    }

//    "handling file upload requests with error responses" must {
//
//      implicit val hc: HeaderCarrier = HeaderCarrier()
//
//      val s3Url = s"https://bucketname.s3.eu-west-2.amazonaws.com"
//
//      val parts: Source[MultipartFormData.Part[Source[ByteString, _]], _] =
//        Source.apply(Map("key" -> List("V1")).flatMap {
//          case (key, values) =>
//            values.map(value => MultipartFormData.DataPart(key, value): MultipartFormData.Part[Source[ByteString, _]])
//        })
//
//      "process unsuccessful responses from S3" in {
//        List(
//          buildWsResponse(400),
//          buildWsResponse(500)
//        ).foreach { httpResponse =>
//          withClue(s"For http response [${httpResponse.toString}]") {
//            mockPostMultiPartForm(s3Url, parts, 0)(Some(httpResponse))
//            await(
//              connector
//                .upload(
//                  s3Url,
//                  MultipartFormData(Map("key" -> List("V1")), Seq.empty, Seq.empty): MultipartFormData[
//                    Source[ByteString, _]
//                  ],
//                  0
//                )
//                .value
//            ) shouldBe Left(
//              Error(s"S3 file upload failed due to: ${httpResponse.body} with http status: ${httpResponse.status}")
//            )
//          }
//        }
//      }
//
//      "process successful responses from S3" in {
//        List(
//          buildWsResponse(204)
//        ).foreach { httpResponse =>
//          withClue(s"For http response [${httpResponse.toString}]") {
//            mockPostMultiPartForm(s3Url, parts, 0)(Some(httpResponse))
//            await(
//              connector
//                .upload(
//                  s3Url,
//                  MultipartFormData(Map("key" -> List("V1")), Seq.empty, Seq.empty): MultipartFormData[
//                    Source[ByteString, _]
//                  ],
//                  0
//                )
//                .value
//            ) shouldBe Right(())
//          }
//        }
//      }
//    }
  }
}
