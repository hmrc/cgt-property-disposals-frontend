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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns

import java.time.LocalDate

import cats.data.EitherT
import cats.instances.future._
import org.scalamock.scalatest.MockFactory
import org.scalatest.{Matchers, WordSpec}
import play.api.libs.json.{JsNumber, Json}
import play.api.test.Helpers._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.returns.ReturnsConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.CgtReference
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{DraftReturn, SubmitReturnRequest, SubmitReturnResponse}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{AmountInPence, Error}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsServiceImpl.GetDraftReturnResponse
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class ReturnsServiceImplSpec extends WordSpec with Matchers with MockFactory {

  val mockConnector = mock[ReturnsConnector]

  def mockStoreDraftReturn(draftReturn: DraftReturn)(response: Either[Error, HttpResponse]) =
    (mockConnector
      .storeDraftReturn(_: DraftReturn)(_: HeaderCarrier))
      .expects(draftReturn, *)
      .returning(EitherT.fromEither[Future](response))

  def mockGetDraftReturns(cgtReference: CgtReference)(response: Either[Error, HttpResponse]) =
    (mockConnector
      .getDraftReturns(_: CgtReference)(_: HeaderCarrier))
      .expects(cgtReference, *)
      .returning(EitherT.fromEither[Future](response))

  def mockSubmitReturn(submitReturnRequest: SubmitReturnRequest)(response: Either[Error, HttpResponse]) =
    (mockConnector
      .submitReturn(_: SubmitReturnRequest)(_: HeaderCarrier))
      .expects(submitReturnRequest, *)
      .returning(EitherT.fromEither[Future](response))

  val service = new ReturnsServiceImpl(mockConnector)

  implicit val hc: HeaderCarrier = HeaderCarrier()

  "ReturnsServiceImpl" when {

    "handling requests to store draft returns" must {

      val draftReturn = sample[DraftReturn]

      "return an error" when {

        "the http call fails" in {
          mockStoreDraftReturn(draftReturn)(Left(Error("")))

          await(service.storeDraftReturn(draftReturn).value).isLeft shouldBe true
        }

        "the http call came back with a status other than 200" in {
          mockStoreDraftReturn(draftReturn)(Right(HttpResponse(INTERNAL_SERVER_ERROR)))

          await(service.storeDraftReturn(draftReturn).value).isLeft shouldBe true
        }

      }

      "return an ok response" when {

        "the http call came back with a 200" in {
          mockStoreDraftReturn(draftReturn)(Right(HttpResponse(OK)))

          await(service.storeDraftReturn(draftReturn).value) shouldBe Right(())
        }

      }

    }

    "handling requests to get draft returns" must {

      val cgtReference = sample[CgtReference]

      "return an error" when {

        "the http response does not come back with status 200" in {
          mockGetDraftReturns(cgtReference)(Right(HttpResponse(INTERNAL_SERVER_ERROR)))

          await(service.getDraftReturns(cgtReference).value).isLeft shouldBe true
        }

        "the http response comes back with status 200 but the body cannot be parsed" in {
          mockGetDraftReturns(cgtReference)(Left(Error("")))

          await(service.getDraftReturns(cgtReference).value).isLeft shouldBe true
        }

      }

      "return an ok response" when {

        val draftReturnsResponse = GetDraftReturnResponse(List(sample[DraftReturn]))

        "the http call came back with a 200 and the body can be parsed" in {
          mockGetDraftReturns(cgtReference)(Right(HttpResponse(OK, Some(Json.toJson(draftReturnsResponse)))))

          await(service.getDraftReturns(cgtReference).value) shouldBe Right(draftReturnsResponse.draftReturns)
        }

      }

    }

    "handling requests to submit a return" must {

      val submitReturnRequest = sample[SubmitReturnRequest]

      "return an error" when {

        def test(response: Either[Error, HttpResponse]) = {
          mockSubmitReturn(submitReturnRequest)(response)

          await(service.submitReturn(submitReturnRequest).value).isLeft shouldBe true
        }

        "the http call fails" in {
          test(Left(Error("")))
        }

        "the http call came back with a status other than 200" in {
          test(Right(HttpResponse(INTERNAL_SERVER_ERROR)))
        }

        "the http call came back with status 200 but there is no JSON body" in {
          test(Right(HttpResponse(OK)))
        }

        "the http call came back with status 200 and JSON body, but the JSON cannot be parsed" in {
          test(Right(HttpResponse(OK, Some(JsNumber(1)))))
        }

      }

      "return an ok response" when {

        "the http call came back with a 200 and the JSON " in {
          val response = SubmitReturnResponse("charge", AmountInPence(123L), LocalDate.of(2000, 1, 2), "bundleId")

          mockSubmitReturn(submitReturnRequest)(
            Right(
              HttpResponse(
                OK,
                Some(Json.parse("""
                |{
                |  "chargeReference": "charge",
                |   "amount": 123,
                |   "dueDate": "2000-01-02",
                |   "formBundleId": "bundleId"
                |}
                |""".stripMargin))
              )
            )
          )

          await(service.submitReturn(submitReturnRequest).value) shouldBe Right(response)
        }

      }

    }

  }

}
