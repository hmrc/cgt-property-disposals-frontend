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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.services

import cats.data.EitherT
import com.typesafe.config.ConfigFactory
import org.scalamock.scalatest.MockFactory
import org.scalatest.{Matchers, WordSpec}
import play.api.Configuration
import play.api.libs.json.Json
import play.api.test.Helpers._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.UpscanConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Error
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.CgtReference
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.upscan.UpscanStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.UpscanService.UpscanNotifyResponse
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.UpscanService.UpscanServiceResponse.{UpscanDescriptor, UpscanNotifyEvent, UpscanRequest, UpscanResponse}
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}

import scala.concurrent.Future
class UpscanServiceImplSpec extends WordSpec with Matchers with MockFactory {

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

  @SuppressWarnings(Array("org.wartremover.warts.Var", "org.wartremover.warts.Any"))
  val mockConnector: UpscanConnector = mock[UpscanConnector]
  val mockStore: UpscanStore         = mock[UpscanStore]

  val service = new UpscanServiceImpl(mockConnector, mockStore, config)

  def mockUpscanInitiate(cgtReference: CgtReference)(response: Either[Error, HttpResponse]) =
    (mockConnector
      .initiate(_: CgtReference)(_: HeaderCarrier))
      .expects(cgtReference, *)
      .returning(EitherT(Future.successful(response)))

  def mockFileUploadCount(cgtRefence: CgtReference)(response: Either[Error, Long]) =
    (mockStore
      .fileUploadCount(_: CgtReference))
      .expects(cgtRefence)
      .returning(EitherT(Future.successful(response)))

  def mockUpscanStoreInsert(cgtReference: CgtReference, upscanNotifyResponse: UpscanNotifyResponse)(
    response: Either[Error, Unit]
  ) =
    (mockStore
      .insert(_: UpscanNotifyEvent))
      .expects(UpscanNotifyEvent(cgtReference.value, upscanNotifyResponse))
      .returning(EitherT(Future.successful(response)))

  "UpscanServiceImpl" when {

    val cgtReference = sample[CgtReference]

    implicit val hc: HeaderCarrier = HeaderCarrier()

    "handling request initiate upscan" must {
      "return an error" when {
        "the call to mongodb fails" in {
          mockFileUploadCount(cgtReference)(Left(Error("Some issue with mongodb")))
          await(service.initiate(cgtReference).value).isLeft shouldBe true
        }
        "the http call fails" in {
          mockFileUploadCount(cgtReference)(Right(3))
          mockUpscanInitiate(cgtReference)(Left(Error("Some issue calling S3")))
          await(service.initiate(cgtReference).value).isLeft shouldBe true
        }
      }

      "return the S3 descriptor if the initiate call succeeded" in {
        val S3descriptor = Json.parse(
          s"""
             |{
             |    "reference": "11370e18-6e24-453e-b45a-76d3e32ea33d",
             |    "uploadRequest": {
             |        "href": "https://xxxx/upscan-upload-proxy/bucketName",
             |        "fields": {
             |            "acl": "private"
             |        }
             |    }
             |}
             |""".stripMargin
        )

        mockFileUploadCount(cgtReference)(Right(3))
        mockUpscanInitiate(cgtReference)(Right(HttpResponse(responseStatus = 200, responseJson = Some(S3descriptor))))
        await(service.initiate(cgtReference).value) shouldBe Right(
          UpscanResponse(
            cgtReference.value,
            UpscanDescriptor(
              "11370e18-6e24-453e-b45a-76d3e32ea33d",
              UpscanRequest("https://xxxx/upscan-upload-proxy/bucketName", Map("acl" -> "private"))
            )
          )
        )
      }
    }

    "handling request to store upscan notify event" must {

      val upscanNotifyResponse =
        UpscanNotifyResponse("UUID-1", "QUEUED", "http://s3.download", Map("field-1" -> "value-1"))

      "return an error" when {
        "the call to mongodb fails" in {
          mockUpscanStoreInsert(cgtReference, upscanNotifyResponse)(Left(Error("Some issue with mongodb")))
          await(service.storeNotifyEvent(cgtReference, upscanNotifyResponse).value).isLeft shouldBe true
        }
      }

      "return successful response if upscan notify event store successfully" in {
        mockUpscanStoreInsert(cgtReference, upscanNotifyResponse)(Right(()))
        await(service.storeNotifyEvent(cgtReference, upscanNotifyResponse).value) shouldBe Right(())
      }
    }
  }
}
