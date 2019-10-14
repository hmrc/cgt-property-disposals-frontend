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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors

import com.typesafe.config.ConfigFactory
import org.scalacheck.ScalacheckShapeless._
import org.scalamock.scalatest.MockFactory
import org.scalatest.{Matchers, WordSpec}
import play.api.libs.json.{JsString, Json}
import play.api.test.Helpers._
import play.api.{Configuration, Mode}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.bpr.BusinessPartnerRecordRequest
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}
import uk.gov.hmrc.play.bootstrap.config.{RunMode, ServicesConfig}

import scala.concurrent.ExecutionContext.Implicits.global

class CGTPropertyDisposalsConnectorImplSpec extends WordSpec with Matchers with MockFactory with HttpSupport {

  val config = Configuration(
    ConfigFactory.parseString(
      """
      |microservice {
      |  services {
      |    cgt-property-disposals {
      |      protocol = http
      |      host     = host
      |      port     = 123
      |    }
      |  }
      |}
      |""".stripMargin
    )
  )

  val connector =
    new CGTPropertyDisposalsConnectorImpl(mockHttp, new ServicesConfig(config, new RunMode(config, Mode.Test)))

  "CGTPropertyDisposalsConnectorImpl" when {

    implicit val hc: HeaderCarrier = HeaderCarrier()

    "handling request to get the subscription status" must {

      val subscriptionStatusUrl = "http://host:123/cgt-property-disposals/check-subscription-status"

      "do a GET http call and return the result" in {
        List(
          HttpResponse(204),
          HttpResponse(200, Some(JsString("hi"))),
          HttpResponse(500)
        ).foreach { httpResponse =>
          withClue(s"For http response [${httpResponse.toString}]") {
            mockGet(subscriptionStatusUrl, Map.empty)(Some(httpResponse))

            await(
              connector
                .getSubscriptionStatus()
                .value
            ) shouldBe Right(httpResponse)
          }
        }
      }

      "return an error" when {

        "the future fails" in {
          mockGet(subscriptionStatusUrl, Map.empty)(None)

          await(
            connector
              .getSubscriptionStatus()
              .value
          ).isLeft shouldBe true
        }

      }
    }

    "handling request to get the business partner record" must {

      val bprUrl     = "http://host:123/cgt-property-disposals/business-partner-record"
      val bprRequest = sample[BusinessPartnerRecordRequest]

      "do a POST http call and return the result" in {
        List(
          HttpResponse(200),
          HttpResponse(200, Some(JsString("hi"))),
          HttpResponse(500)
        ).foreach { httpResponse =>
          withClue(s"For http response [${httpResponse.toString}]") {
            mockPost(bprUrl, Map.empty, Json.toJson(bprRequest))(Some(httpResponse))

            await(
              connector
                .getBusinessPartnerRecord(bprRequest)
                .value
            ) shouldBe Right(httpResponse)
          }
        }
      }

      "return an error" when {

        "the future fails" in {
          mockPost(bprUrl, Map.empty, Json.toJson(bprRequest))(None)

          await(
            connector
              .getBusinessPartnerRecord(bprRequest)
              .value
          ).isLeft shouldBe true
        }

      }

    }

    "handling request to subscribe" must {
      val subscriptionDetails = sample[SubscriptionDetails]

      "do a post http call and return the result" in {
        List(
          HttpResponse(200),
          HttpResponse(200, Some(JsString("hi"))),
          HttpResponse(500)
        ).foreach { httpResponse =>
          withClue(s"For http response [${httpResponse.toString}]") {
            mockPost(s"http://host:123/cgt-property-disposals/subscribe", Map.empty, Json.toJson(subscriptionDetails))(
              Some(httpResponse)
            )

            await(connector.subscribe(subscriptionDetails).value) shouldBe Right(httpResponse)
          }
        }
      }

      "return an error" when {

        "the future fails" in {
          mockPost(s"http://host:123/cgt-property-disposals/subscribe", Map.empty, Json.toJson(subscriptionDetails))(
            None
          )

          await(connector.subscribe(subscriptionDetails).value).isLeft shouldBe true
        }

      }
    }

  }

}
