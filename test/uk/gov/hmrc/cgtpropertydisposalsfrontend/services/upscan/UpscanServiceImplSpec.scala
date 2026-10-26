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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.services.upscan

import cats.data.EitherT
import cats.instances.future.*
import org.scalamock.scalatest.MockFactory
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.libs.json.{JsString, Json}
import play.api.mvc.{Call, Request}
import play.api.test.FakeRequest
import play.api.test.Helpers.*
import uk.gov.hmrc.cgtpropertydisposalsfrontend.SampledScalaCheck
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.upscan.UpscanConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.FillingOutReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.SessionData
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.FileUploadGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.IdGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.JourneyStatusGen.fillingOutReturnGen
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.GGCredId
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.upscan.{UploadReference, UploadRequest, UpscanUpload, UpscanUploadMeta}
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class UpscanServiceImplSpec extends AnyWordSpec with Matchers with SampledScalaCheck with MockFactory {

  private val mockConnector        = mock[UpscanConnector]
  val service                      = new UpscanServiceImpl(mockConnector)
  private val reference            = sample[UploadReference]
  private val ggCredId             = sample[GGCredId]
  private val journey              = sample[FillingOutReturn].copy(ggCredId = ggCredId)
  private val sessionData          = SessionData.empty.copy(journeyStatus = Some(journey))
  private val upload               = sample[UpscanUpload]
  implicit val hc: HeaderCarrier   = HeaderCarrier()
  implicit val request: Request[?] = FakeRequest()

  private val emptyJsonBody = "{}"

  "UpscanServiceImplSpec" when {

    "hangling requests for upscan" must {

      "getting upscan upload" when {
        "Response is OK" in {
          val response = Right(HttpResponse(OK, Json.toJson(upload), Map[String, Seq[String]]().empty))
          (mockConnector
            .getUpscanUpload(_: UploadReference, _: GGCredId)(using _: HeaderCarrier))
            .expects(reference, ggCredId, *)
            .returning(EitherT.fromEither[Future](response))
          await(service.getUpscanUpload(reference, Some(sessionData)).value).isRight shouldBe true
        }

        "Malformed response" in {
          val response = Right(HttpResponse(OK, JsString("Nope"), Map[String, Seq[String]]().empty))
          (mockConnector
            .getUpscanUpload(_: UploadReference, _: GGCredId)(using _: HeaderCarrier))
            .expects(reference, ggCredId, *)
            .returning(EitherT.fromEither[Future](response))
          await(service.getUpscanUpload(reference, Some(sessionData)).value).isLeft shouldBe true
        }

        "Internal server error" in {
          val response = Right(HttpResponse(INTERNAL_SERVER_ERROR, emptyJsonBody))
          (mockConnector
            .getUpscanUpload(_: UploadReference, _: GGCredId)(using _: HeaderCarrier))
            .expects(reference, ggCredId, *)
            .returning(EitherT.fromEither[Future](response))
          await(service.getUpscanUpload(reference, Some(sessionData)).value).isLeft shouldBe true
        }
      }

      "initialising" when {
        "initialise" in {
          val mockSuccess = Call("GET", "/mock-success")
          val mockFailure = Call("GET", "/mock-fail")
          val response    = Right(
            HttpResponse(
              OK,
              Json.toJson(UpscanUploadMeta("metadata", sample[UploadRequest])),
              Map[String, Seq[String]]().empty
            )
          )
          (mockConnector
            .initiate(_: Call, _: Call, _: UploadReference, _: GGCredId)(using _: HeaderCarrier))
            .expects(mockFailure, mockSuccess, *, *, *)
            .returning(EitherT.fromEither[Future](response))
          (mockConnector
            .saveUpscanUpload(_: UpscanUpload, _: GGCredId)(using _: HeaderCarrier))
            .expects(*, *, *)
            .returning(EitherT.fromEither[Future](Right(HttpResponse(OK, emptyJsonBody))))
          await(
            service
              .initiate(mockFailure, (_: UploadReference) => mockSuccess, Some(sessionData))
              .value
          ).isRight shouldBe true
        }
      }
    }
  }
}
