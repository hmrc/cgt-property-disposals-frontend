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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.services.upscan

import cats.data.EitherT
import cats.instances.future._
import org.scalamock.scalatest.MockFactory
import org.scalatest.{Matchers, WordSpec}
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.libs.json.{JsString, Json}
import play.api.mvc.{Call, Request}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.upscan.UpscanConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.upscan.{UploadReference, UploadRequest, UpscanUpload, UpscanUploadMeta}
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class UpscanServiceImplSpec extends WordSpec with Matchers with ScalaCheckDrivenPropertyChecks with MockFactory {

  val mockConnector                = mock[UpscanConnector]
  val service                      = new UpscanServiceImpl(mockConnector)
  val reference                    = sample[UploadReference]
  val upload                       = sample[UpscanUpload]
  implicit val hc: HeaderCarrier   = HeaderCarrier()
  implicit val request: Request[_] = FakeRequest()

  "UpscanServiceImplSpec" when {

    "hangling requests for upscan" must {

      "getting upscan upload" when {
        "Response is OK" in {
          val response = Right(HttpResponse(OK, Some(Json.toJson(upload))))
          (mockConnector
            .getUpscanUpload(_: UploadReference)(_: HeaderCarrier))
            .expects(reference, *)
            .returning(EitherT.fromEither[Future](response))
          await(service.getUpscanUpload(reference).value).isRight shouldBe true
        }

        "Malformed response" in {
          val response = Right(HttpResponse(OK, Some(JsString("Nope"))))
          (mockConnector
            .getUpscanUpload(_: UploadReference)(_: HeaderCarrier))
            .expects(reference, *)
            .returning(EitherT.fromEither[Future](response))
          await(service.getUpscanUpload(reference).value).isLeft shouldBe true
        }

        "Internal server error" in {
          val response = Right(HttpResponse(INTERNAL_SERVER_ERROR))
          (mockConnector
            .getUpscanUpload(_: UploadReference)(_: HeaderCarrier))
            .expects(reference, *)
            .returning(EitherT.fromEither[Future](response))
          await(service.getUpscanUpload(reference).value).isLeft shouldBe true
        }
      }

      "initialising" when {
        "initialise" in {
          val mockSuccess = Call("GET", "/mock-success")
          val mockFailure = Call("GET", "/mock-fail")
          val response    = Right(HttpResponse(OK, Some(Json.toJson(UpscanUploadMeta("metadata", sample[UploadRequest])))))
          (mockConnector
            .initiate(_: Call, _: Call, _: UploadReference)(_: HeaderCarrier))
            .expects(mockFailure, mockSuccess, *, *)
            .returning(EitherT.fromEither[Future](response))
          (mockConnector
            .saveUpscanUpload(_: UpscanUpload)(_: HeaderCarrier))
            .expects(*, *)
            .returning(EitherT.fromEither[Future](Right(HttpResponse(OK))))
          await(service.initiate(mockFailure, (_: UploadReference) => mockSuccess).value).isRight shouldBe true
        }
      }
    }
  }
}
