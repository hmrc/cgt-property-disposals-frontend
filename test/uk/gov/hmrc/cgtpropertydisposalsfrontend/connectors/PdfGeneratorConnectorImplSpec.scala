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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors

import com.typesafe.config.ConfigFactory
import org.scalamock.scalatest.MockFactory
import org.scalatest.{Matchers, WordSpec}
import play.api.libs.json.JsString
import play.api.test.Helpers._
import play.api.{Configuration, Mode}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.pdf.PdfGeneratorConnectorImpl
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Error
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}
import uk.gov.hmrc.play.bootstrap.config.{RunMode, ServicesConfig}

import scala.concurrent.ExecutionContext.Implicits.global
class PdfGeneratorConnectorImplSpec extends WordSpec with Matchers with MockFactory with HttpSupport {

  val config = Configuration(
    ConfigFactory.parseString(
      """
        | microservice.services {
        |    pdf-generator {
        |        host = localhost
        |        port = 9852
        |    }
        | }
        |""".stripMargin
    )
  )
  val connector = new PdfGeneratorConnectorImpl(new ServicesConfig(config, new RunMode(config, Mode.Test)), mockHttp)

  "PdfGeneratorConnectorImpl" when {

    "handling pdf generation requests" must {

      implicit val hc: HeaderCarrier = HeaderCarrier()

      val expectedUrl = s"http://localhost:9852/pdf-generator-service/generate"

      "process unsuccessful post calls from PDF Generation Service " in {
        List(
          HttpResponse(400),
          HttpResponse(500, Some(JsString("error")))
        ).foreach { httpResponse =>
          withClue(s"For http response [${httpResponse.toString}]") {
            mockPostForm(expectedUrl, Map.empty, Map.empty)(Some(httpResponse))
            await(connector.generatePDF(Map.empty, Seq.empty).value) shouldBe Left(
              Error("Could not generate PDF")
            )
          }
        }
      }

      "process successful calls from PDF generation service" in {
        List(
          HttpResponse(200, Some(JsString("hi")), Map.empty, Some("some pdf"))
        ).foreach { httpResponse =>
          withClue(s"For http response [${httpResponse.toString}]") {
            mockPostForm(expectedUrl, Map.empty, Map.empty)(Some(httpResponse))
            await(connector.generatePDF(Map.empty, Seq.empty).value) shouldBe Right(
              Stream('s', 'o', 'm', 'e', ' ', 'p', 'd', 'f')
            )
          }
        }
      }
    }
  }
}
