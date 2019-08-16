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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.http

import play.api.libs.json.Writes
import uk.gov.hmrc.http._

import scala.concurrent.{ExecutionContext, Future}

object HttpClient {

  // this HttpReads instance for HttpResponse is preferred over the default
  // uk.gov.hmrc.http.RawReads.readRaw as this custom one doesn't throw exceptions
  private class RawHttpReads extends HttpReads[HttpResponse] {
    override def read(method: String, url: String, response: HttpResponse): HttpResponse = response
  }

  private val rawHttpReads = new RawHttpReads

  implicit class HttpClientOps(val http: uk.gov.hmrc.play.bootstrap.http.HttpClient) extends AnyVal {
    def get(
      url: String,
      queryParams: Map[String, String] = Map.empty[String, String],
      headers: Map[String, String]     = Map.empty[String, String])(
      implicit hc: HeaderCarrier,
      ec: ExecutionContext): Future[HttpResponse] =
      http.GET(url, queryParams.toSeq)(rawHttpReads, hc.withExtraHeaders(headers.toSeq: _*), ec)

    def post[A](url: String, body: A, headers: Map[String, String] = Map.empty[String, String])(
      implicit w: Writes[A],
      hc: HeaderCarrier,
      ec: ExecutionContext): Future[HttpResponse] =
      http.POST(url, body, headers.toSeq)(w, rawHttpReads, hc, ec)

  }

}
