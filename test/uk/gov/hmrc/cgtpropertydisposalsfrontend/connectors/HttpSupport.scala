/*
 * Copyright 2023 HM Revenue & Customs
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

import org.scalamock.scalatest.MockFactory
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.Writes
import uk.gov.hmrc.http.{HeaderCarrier, HttpClient, HttpReads, HttpResponse}

import scala.concurrent.{ExecutionContext, Future}

trait HttpSupport { this: MockFactory with Matchers =>

  val mockHttp: HttpClient = mock[HttpClient]

  protected def mockGet[A](
    url: String
  )(
    response: Option[A]
  ) =
    (mockHttp
      .GET(_: String, _: Seq[(String, String)], _: Seq[(String, String)])(
        _: HttpReads[A],
        _: HeaderCarrier,
        _: ExecutionContext
      ))
      .expects(where {
        (
          u: String,
          q: Seq[(String, String)],
          r: Seq[(String, String)],
          _: HttpReads[A],
          h: HeaderCarrier,
          _: ExecutionContext
        ) =>
          // use matchers here to get useful error messages when the following predicates
          // are not satisfied - otherwise it is difficult to tell in the logs what went wrong
          u              shouldBe url
          q              shouldBe Seq.empty
          r              shouldBe Seq.empty
          h.extraHeaders shouldBe Seq.empty
          true
      })
      .returning(response.fold(Future.failed[A](new Exception("Test exception message")))(Future.successful))

  protected def mockGetWithQueryWithHeaders[A](
    url: String,
    queryParams: Seq[(String, String)],
    headers: Seq[(String, String)]
  )(
    response: Option[A]
  ): Any =
    (mockHttp
      .GET(_: String, _: Seq[(String, String)], _: Seq[(String, String)])(
        _: HttpReads[A],
        _: HeaderCarrier,
        _: ExecutionContext
      ))
      .expects(where {
        (
          u: String,
          q: Seq[(String, String)],
          hdrs: Seq[(String, String)],
          _: HttpReads[A],
          _: HeaderCarrier,
          _: ExecutionContext
        ) =>
          // use matchers here to get useful error messages when the following predicates
          // are not satisfied - otherwise it is difficult to tell in the logs what went wrong
          u    shouldBe url
          q    shouldBe queryParams
          hdrs shouldBe headers
          true
      })
      .returning(
        response.fold(
          Future.failed[A](new Exception("Test exception message"))
        )(Future.successful)
      )

  protected def mockPost[A](url: String, headers: Seq[(String, String)], body: A)(
    result: Option[HttpResponse]
  ): Unit =
    (mockHttp
      .POST(_: String, _: A, _: Seq[(String, String)])(
        _: Writes[A],
        _: HttpReads[HttpResponse],
        _: HeaderCarrier,
        _: ExecutionContext
      ))
      .expects(url, body, headers, *, *, *, *)
      .returning(
        result.fold[Future[HttpResponse]](
          Future.failed(new Exception("Test exception message"))
        )(Future.successful)
      )

  protected def mockPut[A](
    url: String,
    body: A,
    headers: Seq[(String, String)] = Seq.empty
  )(result: Option[HttpResponse]): Unit =
    (mockHttp
      .PUT(_: String, _: A, _: Seq[(String, String)])(
        _: Writes[A],
        _: HttpReads[HttpResponse],
        _: HeaderCarrier,
        _: ExecutionContext
      ))
      .expects(url, body, headers, *, *, *, *)
      .returning(
        result.fold[Future[HttpResponse]](
          Future.failed(new Exception("Test exception message"))
        )(Future.successful)
      )

}
