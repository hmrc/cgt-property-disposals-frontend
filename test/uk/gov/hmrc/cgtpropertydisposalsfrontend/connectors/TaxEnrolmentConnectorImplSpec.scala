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
import org.scalamock.scalatest.MockFactory
import org.scalatest.{Matchers, WordSpec}
import play.api.test.Helpers._
import play.api.{Configuration, Mode}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.EnrolmentRequest
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}
import uk.gov.hmrc.play.bootstrap.config.{RunMode, ServicesConfig}

import scala.concurrent.ExecutionContext.Implicits.global

class TaxEnrolmentConnectorImplSpec extends WordSpec with Matchers with MockFactory with HttpSupport {
  val config = Configuration(
    ConfigFactory.parseString(
      """
        |microservice {
        |  services {
        |    tax-enrolments {
        |      protocol = http
        |      host     = host
        |      port     = 123
        |    }
        |  }
        |}
        |""".stripMargin
    )
  )

  val connector = new TaxEnrolmentConnectorImpl(mockHttp, new ServicesConfig(config, new RunMode(config, Mode.Test)))

  "Tax Enrolment Connector" when {

    implicit val hc: HeaderCarrier = HeaderCarrier()
    val cgtReference               = "XACGTP123456789"
    val enrolmentRequest           = EnrolmentRequest(List.empty, List.empty)

    "it receives a request to enrol a user it" must {

      "make a http put call and return a result" in {
        List(
          HttpResponse(204),
          HttpResponse(401),
          HttpResponse(400)
        ).foreach { httpResponse =>
          withClue(s"For http response [${httpResponse.toString}]") {
            mockPut[EnrolmentRequest](
              s"http://host:123/tax-enrolments/enrolments/HMRC-CGT-PD~$cgtReference",
              enrolmentRequest
            )(Some(httpResponse))

            await(connector.allocateEnrolmentToGroup(cgtReference, enrolmentRequest).value) shouldBe Right(httpResponse)
          }
        }
      }
    }
    "return an error" when {
      "the future fails" in {
        mockPut[EnrolmentRequest](
          s"http://host:123/tax-enrolments/enrolments/HMRC-CGT-PD~$cgtReference",
          enrolmentRequest
        )(None)
        await(connector.allocateEnrolmentToGroup(cgtReference, enrolmentRequest).value).isLeft shouldBe true
      }
    }
  }
}
