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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.services.onboarding

import cats.data.EitherT
import cats.instances.future._
import org.scalamock.scalatest.MockFactory
import org.scalatest.{Matchers, WordSpec}
import play.api.libs.json.{JsNumber, Json}
import play.api.test.Helpers._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.CGTPropertyDisposalsConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.CgtReference
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.bpr.{BusinessPartnerRecord, BusinessPartnerRecordRequest, BusinessPartnerRecordResponse}
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class BusinessPartnerRecordServiceImplSpec extends WordSpec with Matchers with MockFactory {

  val mockConnector = mock[CGTPropertyDisposalsConnector]

  val service = new BusinessPartnerRecordServiceImpl(mockConnector)

  def mockGetBPR(
    request: BusinessPartnerRecordRequest
  )(response: Either[Error, HttpResponse]) =
    (mockConnector
      .getBusinessPartnerRecord(_: BusinessPartnerRecordRequest)(
        _: HeaderCarrier
      ))
      .expects(request, *)
      .returning(EitherT.fromEither[Future](response))

  implicit val hc: HeaderCarrier = HeaderCarrier()
  val bprRequest                 = sample[BusinessPartnerRecordRequest]
  val bpr                        = sample[BusinessPartnerRecord]

  "The BusinessPartnerRecordServiceImpl" when {

    "getting a BusinessPartnerRecord" must {

      "return an error" when {

        def testError(response: => Either[Error, HttpResponse]) = {
          mockGetBPR(bprRequest)(response)

          await(
            service.getBusinessPartnerRecord(bprRequest).value
          ).isLeft shouldBe true
        }

        "the connector fails to make the call" in {
          testError(Left(Error(new Exception("Uh oh"))))
        }

        "the HttpResponse comes back with a status other than 200" in {
          List(400, 401, 403, 404, 500, 501, 502).foreach(status => testError(Right(HttpResponse(status))))
        }

        "the json body in the http response cannot be parsed" in {
          testError(Right(HttpResponse(200, Some(JsNumber(0)))))
        }

        "there is no json body in the http response" in {
          testError(Right(HttpResponse(200)))
        }

      }

      "return the bpr when the http response comes back with status 200 and " +
        "the json body returns a bpr" in {
        val response = BusinessPartnerRecordResponse(Some(bpr), None)

        mockGetBPR(bprRequest)(
          Right(HttpResponse(200, Some(Json.toJson(response))))
        )

        await(
          service.getBusinessPartnerRecord(bprRequest).value
        ) shouldBe Right(response)
      }

      "return nothing when the http response comes back with status 200 and " +
        "the json body does not contain a bpr" in {
        val response =
          BusinessPartnerRecordResponse(Some(bpr), Some(sample[CgtReference]))

        mockGetBPR(bprRequest)(
          Right(HttpResponse(200, Some(Json.toJson(response))))
        )

        await(
          service.getBusinessPartnerRecord(bprRequest).value
        ) shouldBe Right(response)
      }
    }

  }

}
