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
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.upscan.UpscanFileDescriptor.UpscanFileDescriptorStatus.READY_TO_UPLOAD
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.upscan.UpscanInitiateResponse.UpscanInitiateSuccess
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.upscan.{FileDescriptor, UpscanFileDescriptor, UpscanInitiateRawResponse, UpscanSnapshot}
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

  val service = new UpscanServiceImpl(mockConnector, config)

  def mockUpscanInitiate(cgtReference: CgtReference)(response: Either[Error, HttpResponse]) =
    (mockConnector
      .initiate(_: CgtReference)(_: HeaderCarrier))
      .expects(cgtReference, *)
      .returning(EitherT(Future.successful(response)))

  def mockGetUpscanSnapshot(cgtReference: CgtReference)(response: Either[Error, UpscanSnapshot]) =
    (mockConnector
      .getUpscanSnapshot(_: CgtReference)(_: HeaderCarrier))
      .expects(cgtReference, *)
      .returning(EitherT(Future.successful(response)))

  def mockSaveFileDescriptor(upscanFileDescriptor: UpscanFileDescriptor)(response: Either[Error, Unit]) =
    (mockConnector
      .saveUpscanFileDescriptors(_: UpscanFileDescriptor)(_: HeaderCarrier))
      .expects(upscanFileDescriptor, *)
      .returning(EitherT(Future.successful(response)))

  "UpscanServiceImpl" when {

    val cgtReference              = sample[CgtReference]
    val upscanFileDescriptor      = sample[UpscanFileDescriptor]
    val upscanInitiateRawResponse = sample[UpscanInitiateRawResponse]

    implicit val hc: HeaderCarrier = HeaderCarrier()

    "handling request initiate upscan" must {
      "return an error" when {
        "the call to get the upscan snapshot details fails" in {
          mockGetUpscanSnapshot(cgtReference)(Left(Error("backend service error")))
          await(service.initiate(cgtReference).value).isLeft shouldBe true
        }
        "the call to get the upscan initiate fails" in {
          inSequence {
            mockGetUpscanSnapshot(cgtReference)(Right(UpscanSnapshot(1)))
            mockUpscanInitiate(cgtReference)(Left(Error("upscan initiate end service error")))
          }
          await(service.initiate(cgtReference).value).isLeft shouldBe true
        }
        "the call to save the upscan descriptor details fails" in {
          inSequence {
            mockGetUpscanSnapshot(cgtReference)(Right(UpscanSnapshot(1)))
            mockUpscanInitiate(cgtReference)(
              Right(HttpResponse(200, Some(Json.toJson[UpscanFileDescriptor](upscanFileDescriptor))))
            )
          }
          await(service.initiate(cgtReference).value).isLeft shouldBe true
        }
      }

      "return upscan initiate reference if upscan initiate call is successful" in {

        val fd = upscanFileDescriptor.copy(
          key            = upscanInitiateRawResponse.reference,
          fileDescriptor = FileDescriptor(upscanInitiateRawResponse.reference, upscanInitiateRawResponse.uploadRequest),
          cgtReference   = cgtReference,
          status         = READY_TO_UPLOAD
        )

        inSequence {
          mockGetUpscanSnapshot(cgtReference)(Right(UpscanSnapshot(1)))
          mockUpscanInitiate(cgtReference)(
            Right(HttpResponse(200, Some(Json.toJson[UpscanInitiateRawResponse](upscanInitiateRawResponse))))
          )

          mockSaveFileDescriptor(fd)(
            Right(())
          )
        }
        await(service.initiate(cgtReference).value) shouldBe Right(
          UpscanInitiateSuccess(fd.fileDescriptor.reference)
        )
      }
    }
  }
}
