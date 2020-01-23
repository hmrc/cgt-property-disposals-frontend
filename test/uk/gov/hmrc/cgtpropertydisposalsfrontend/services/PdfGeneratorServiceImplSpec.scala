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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.services

import akka.util.Timeout
import cats.data.EitherT
import com.typesafe.config.ConfigFactory
import org.scalamock.scalatest.MockFactory
import org.scalatest.{Matchers, WordSpec}
import play.api.Configuration
import play.api.test.Helpers.await
import play.mvc.Http.{HeaderNames, MimeTypes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.pdf.PdfGeneratorConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Error
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.Future
import scala.concurrent.duration._

class PdfGeneratorServiceImplSpec extends WordSpec with Matchers with MockFactory {
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

  implicit val timeout: Timeout            = Timeout(5 second)
  val mockConnector: PdfGeneratorConnector = mock[PdfGeneratorConnector]

  val service = new PdfGeneratorServiceImpl(mockConnector)

  def mockPdfGeneratorServiceCall(payload: Map[String, Seq[String]], headers: Seq[(String, String)])(
    response: Either[Error, Stream[Char]]
  ) =
    (mockConnector
      .generatePDF(_: Map[String, Seq[String]], _: Seq[(String, String)])(_: HeaderCarrier))
      .expects(payload, headers, *)
      .returning(EitherT(Future.successful(response)))

  "PdfGeneratorServiceImpl" when {

    implicit val hc: HeaderCarrier = HeaderCarrier()

    "handling request to generate pdf" must {
      "return an error" when {
        "the http call fails" in {
          mockPdfGeneratorServiceCall(
            Map("html" -> Seq("<!DOCTYPE html>"), "force-pdfa" -> Seq("false")),
            Seq((HeaderNames.CONTENT_TYPE, MimeTypes.FORM))
          )(Left(Error("Some issue calling PDF generator service")))
          await(service.generatePdfBytes("<!DOCTYPE html>").value).isLeft shouldBe true
        }
      }

      "return the stream of bytes for the pdf if the call to the pdf service completed" in {
        mockPdfGeneratorServiceCall(
          Map("html" -> Seq("some html"), "force-pdfa" -> Seq("false")),
          Seq((HeaderNames.CONTENT_TYPE, MimeTypes.FORM))
        )(Right(Stream('s', 'o', 'm', 'e', 'h', 't', 'm', 'l')))
        await(service.generatePdfBytes("some html").value) shouldBe Right(
          Stream('s', 'o', 'm', 'e', 'h', 't', 'm', 'l')
        )
      }
    }
  }
}
