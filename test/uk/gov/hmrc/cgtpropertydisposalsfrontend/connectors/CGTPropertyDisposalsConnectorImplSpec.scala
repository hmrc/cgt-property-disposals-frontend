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
import play.api.libs.json.{JsNull, JsString, Json}
import play.api.test.Helpers._
import play.api.{Configuration, Mode}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.CGTPropertyDisposalsConnectorImpl.BprRequest
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{DateOfBirth, NINO, Name, SAUTR, SubscriptionDetails, sample}
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}
import uk.gov.hmrc.play.bootstrap.config.{RunMode, ServicesConfig}
import java.time._

import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.UserType.{Individual, Trust}

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

    "handling request to get the business partner record" when {

      "handling individuals with NINOs" must {
        val nino = NINO("AB123456C")
        val name = Name("forename", "surname")
        val dateOfBirth = DateOfBirth(LocalDate.of(2000, 4, 10))
        val payload = Json.toJson(BprRequest(name.firstName, name.lastName, Some(dateOfBirth.value)))
        val individual = Individual(Right(nino), name, Some(dateOfBirth), None)

        "do a POST http call and return the result" in {
          List(
            HttpResponse(200),
            HttpResponse(200, Some(JsString("hi"))),
            HttpResponse(500)
          ).foreach { httpResponse =>
            withClue(s"For http response [${httpResponse.toString}]") {
              mockPost(s"http://host:123/cgt-property-disposals/business-partner-record/nino/${nino.value}", Map.empty, payload)(
                Some(httpResponse))

              await(
                connector
                  .getBusinessPartnerRecord(Right(individual))
                  .value) shouldBe Right(httpResponse)
            }
          }
        }

        "return an error" when {

          "the future fails" in {
            mockPost(s"http://host:123/cgt-property-disposals/business-partner-record/nino/${nino.value}", Map.empty, payload)(None)

            await(
              connector
                .getBusinessPartnerRecord(Right(individual))
                .value).isLeft shouldBe true
          }

        }
      }

      "handling individuals with SAUTRs" must {

        val sautr = SAUTR("1234567890")
        val name = Name("forename", "surname")
        val dateOfBirth = DateOfBirth(LocalDate.of(2000, 4, 10))
        val payload = Json.toJson(BprRequest(name.firstName, name.lastName, Some(dateOfBirth.value)))
        val individual = Individual(Left(sautr), name, Some(dateOfBirth), None)

        "do a POST http call and return the result" in {
          List(
            HttpResponse(200),
            HttpResponse(200, Some(JsString("hi"))),
            HttpResponse(500)
          ).foreach { httpResponse =>
            withClue(s"For http response [${httpResponse.toString}]") {
              mockPost(
                s"http://host:123/cgt-property-disposals/business-partner-record/sautr/${sautr.value}",
                Map.empty,
                payload)(Some(httpResponse))

              await(
                connector
                  .getBusinessPartnerRecord(Right(individual))
                  .value) shouldBe Right(httpResponse)
            }
          }
        }

        "return an error" when {

          "the future fails" in {
            mockPost(
              s"http://host:123/cgt-property-disposals/business-partner-record/sautr/${sautr.value}",
              Map.empty,
              payload)(None)

            await(
              connector
                .getBusinessPartnerRecord(Right(individual))
                .value).isLeft shouldBe true
          }

        }
      }

      "handling organisations with SAUTRs" must {
        val sautr = SAUTR("sautr")
        val trust = Trust(sautr)

        "do a POST http call and return the result" in {
          List(
            HttpResponse(200),
            HttpResponse(200, Some(JsString("hi"))),
            HttpResponse(500)
          ).foreach { httpResponse =>
            withClue(s"For http response [${httpResponse.toString}]") {
              mockPost(
                s"http://host:123/cgt-property-disposals/business-partner-record/sautr/${sautr.value}",
                Map.empty,
                JsNull
              )(Some(httpResponse))

              await(
                connector
                  .getBusinessPartnerRecord(Left(trust))
                  .value) shouldBe Right(httpResponse)
            }
          }
        }

        "return an error" when {

          "the future fails" in {
            mockPost(
              s"http://host:123/cgt-property-disposals/business-partner-record/sautr/${sautr.value}",
              Map.empty,
              JsNull
            )(None)

            await(
              connector
                .getBusinessPartnerRecord(Left(trust))
                .value).isLeft shouldBe true
          }

        }
      }

    }

    "handling request to subscribe" must {
      val subscriptionDetails        = sample[SubscriptionDetails]

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
