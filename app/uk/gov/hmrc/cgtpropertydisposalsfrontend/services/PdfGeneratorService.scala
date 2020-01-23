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

import cats.data.EitherT
import com.google.inject.{ImplementedBy, Inject, Singleton}
import play.mvc.Http.{HeaderNames, MimeTypes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.pdf.PdfGeneratorConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Error
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.Future
@ImplementedBy(classOf[PdfGeneratorServiceImpl])
trait PdfGeneratorService {
  def generatePdfBytes(html: String)(implicit hc: HeaderCarrier): EitherT[Future, Error, Stream[Char]]
}

@Singleton
class PdfGeneratorServiceImpl @Inject() (pdfGeneratorConnector: PdfGeneratorConnector)
    extends PdfGeneratorService
    with Logging {

  override def generatePdfBytes(html: String)(implicit hc: HeaderCarrier): EitherT[Future, Error, Stream[Char]] = {
    val headers = Seq((HeaderNames.CONTENT_TYPE, MimeTypes.FORM))
    val body    = Map("html" -> Seq(html), "force-pdfa" -> Seq("false"))
    pdfGeneratorConnector.generatePDF(body, headers)
  }
}
