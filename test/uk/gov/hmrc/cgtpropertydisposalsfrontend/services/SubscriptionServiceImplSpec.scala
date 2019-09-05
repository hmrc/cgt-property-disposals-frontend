/*
 * Copyright 2019 HM Revenue & Customs
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
import org.scalacheck.ScalacheckShapeless._
import org.scalamock.scalatest.MockFactory
import org.scalatest.{Matchers, WordSpec}
import play.api.libs.json.{JsNumber, Json}
import play.api.test.Helpers._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.CGTPropertyDisposalsConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, SubscriptionDetails, SubscriptionResponse, sample}
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class SubscriptionServiceImplSpec extends WordSpec with Matchers with MockFactory {

  val mockConnector = mock[CGTPropertyDisposalsConnector]

  val service = new SubscriptionServiceImpl(mockConnector)

  def mockSubscribe(expectedSubscriptionDetails: SubscriptionDetails)(response: Either[Error, HttpResponse]) =
    (mockConnector
      .subscribe(_: SubscriptionDetails)(_: HeaderCarrier))
      .expects(expectedSubscriptionDetails, *)
      .returning(EitherT(Future.successful(response)))

  "SubscriptionServiceImpl" when {

    "handling requests to subscribe" must {

      implicit val hc: HeaderCarrier = HeaderCarrier()
      val subscriptionDetails        = sample[SubscriptionDetails]

      "return an error" when {

        "the http call comes back with a status other than 200" in {
          mockSubscribe(subscriptionDetails)(Right(HttpResponse(500)))

          await(service.subscribe(subscriptionDetails).value).isLeft shouldBe true
        }

        "there is no JSON in the body of the http response" in {
          mockSubscribe(subscriptionDetails)(Right(HttpResponse(200)))

          await(service.subscribe(subscriptionDetails).value).isLeft shouldBe true
        }

        "the JSON body of the response cannot be parsed" in {
          mockSubscribe(subscriptionDetails)(Right(HttpResponse(200, Some(JsNumber(1)))))

          await(service.subscribe(subscriptionDetails).value).isLeft shouldBe true
        }

      }

      "return the subscription response if the call comes back with a " +
        "200 status and the JSON body can be parsed" in {
        val cgtReferenceNumber = "number"
        val jsonBody = Json.parse(
          s"""
             |{
             |  "cgtReferenceNumber" : "$cgtReferenceNumber"
             |}
             |""".stripMargin
        )

        mockSubscribe(subscriptionDetails)(Right(HttpResponse(200, Some(jsonBody))))

        await(service.subscribe(subscriptionDetails).value) shouldBe Right(SubscriptionResponse(cgtReferenceNumber))
      }
    }

  }

}
