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

import java.time.LocalDateTime

import cats.data.EitherT
import org.scalamock.scalatest.MockFactory
import org.scalatest.{Matchers, WordSpec}
import play.api.libs.json.Json
import play.api.test.Helpers._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.upscan.UpscanConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Error
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.{CgtReference, DraftReturnId}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.upscan.UpscanFileDescriptor.UpscanFileDescriptorStatus.READY_TO_UPLOAD
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.upscan.UpscanInitiateResponse.UpscanInitiateSuccess
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.upscan._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.upscan.UpscanServiceImpl
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

class UpscanServiceImplSpec extends WordSpec with Matchers with MockFactory {

  @SuppressWarnings(Array("org.wartremover.warts.Var", "org.wartremover.warts.Any"))
  val mockConnector: UpscanConnector = mock[UpscanConnector]

  val service = new UpscanServiceImpl(mockConnector)

  def mockUpscanInitiate(draftReturnId: DraftReturnId)(response: Either[Error, HttpResponse]) =
    (mockConnector
      .initiate(_: DraftReturnId)(_: HeaderCarrier))
      .expects(draftReturnId, *)
      .returning(EitherT(Future.successful(response)))

  def mockSaveFileDescriptor(upscanFileDescriptor: UpscanFileDescriptor)(response: Either[Error, Unit]) =
    (mockConnector
      .saveUpscanFileDescriptors(_: UpscanFileDescriptor)(_: HeaderCarrier))
      .expects(upscanFileDescriptor, *)
      .returning(EitherT(Future.successful(response)))

  "UpscanServiceImpl" when {

    val draftReturnId             = sample[DraftReturnId]
    val cgtReference              = sample[CgtReference]
    val upscanFileDescriptor      = sample[UpscanFileDescriptor].copy(draftReturnId = draftReturnId)
    val upscanInitiateRawResponse = sample[UpscanInitiateRawResponse]
    val ts                        = LocalDateTime.of(2020, 11, 26, 13, 55, 36, 123)

    implicit val hc: HeaderCarrier = HeaderCarrier()

    val fd = upscanFileDescriptor.copy(
      upscanInitiateReference = UpscanInitiateReference(upscanInitiateRawResponse.reference),
      fileDescriptor          = FileDescriptor(upscanInitiateRawResponse.reference, upscanInitiateRawResponse.uploadRequest),
      cgtReference            = cgtReference,
      status                  = READY_TO_UPLOAD,
      timestamp               = ts
    )

    "handling request initiate upscan" must {
      "return an error" when {
        "the call to get the upscan initiate fails" in {
          mockUpscanInitiate(draftReturnId)(Left(Error("upscan initiate end service error")))

          await(service.initiate(draftReturnId, cgtReference, ts).value).isLeft shouldBe true
        }
        "the call to save the upscan descriptor details fails" in {
          inSequence {
            mockUpscanInitiate(draftReturnId)(
              Right(HttpResponse(200, Some(Json.toJson[UpscanInitiateRawResponse](upscanInitiateRawResponse))))
            )
            mockSaveFileDescriptor(fd)(Left(Error("")))
          }

          await(service.initiate(draftReturnId, cgtReference, ts).value).isLeft shouldBe true
        }
      }

      "return upscan initiate reference if upscan initiate call is successful" in {
        inSequence {
          mockUpscanInitiate(draftReturnId)(
            Right(HttpResponse(200, Some(Json.toJson[UpscanInitiateRawResponse](upscanInitiateRawResponse))))
          )
          mockSaveFileDescriptor(fd)(
            Right(())
          )
        }

        await(service.initiate(draftReturnId, cgtReference, ts).value) shouldBe Right(
          UpscanInitiateSuccess(UpscanInitiateReference(fd.fileDescriptor.reference))
        )
      }
    }
  }
}
