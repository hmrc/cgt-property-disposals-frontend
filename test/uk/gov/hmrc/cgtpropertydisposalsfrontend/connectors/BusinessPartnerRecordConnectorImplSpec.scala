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

import java.time.LocalDate

import cats.data.NonEmptyList
import com.typesafe.config.ConfigFactory
import org.scalamock.scalatest.MockFactory
import org.scalatest.{Matchers, WordSpec}
import play.api.libs.json.{JsString, Json}
import play.api.{Configuration, Mode}
import play.api.test.Helpers._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Address, BusinessPartnerRecord, DateOfBirth, NINO}
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}
import uk.gov.hmrc.play.bootstrap.config.{RunMode, ServicesConfig}

import scala.concurrent.ExecutionContext.Implicits.global

class BusinessPartnerRecordConnectorImplSpec extends WordSpec with Matchers with MockFactory with HttpSupport {

  val config = Configuration(ConfigFactory.parseString(
    """
      |microservice {
      |  services {
      |      business-partner-record {
      |      protocol = http
      |      host     = host
      |      port     = 123
      |    }
      |  }
      |}
      |""".stripMargin
  ))

  val connector = new BusinessPartnerRecordConnectorImpl(mockHttp, new ServicesConfig(config, new RunMode(config, Mode.Test)))

  "BusinessPartnerRecordConnectorImpl" when {

    "handling request to get the business partner record" must {

      "do a get http call and return the result" in {
        implicit val hc: HeaderCarrier = HeaderCarrier()
        val nino = NINO("AB123456C")

        List(
          HttpResponse(200),
          HttpResponse(200, Some(JsString("hi"))),
          HttpResponse(500)
        ).foreach { httpResponse =>
            withClue(s"For http response [${httpResponse.toString}]") {
              mockGet(s"http://host:123/${nino.value}/business-partner-record")(Some(httpResponse))

              await(connector.getBusinessPartnerRecord(nino)) shouldBe httpResponse
            }
          }
      }
    }

  }

}
