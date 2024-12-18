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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.testonly

import cats.data.EitherT
import com.google.inject.{ImplementedBy, Inject, Singleton}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Error
import uk.gov.hmrc.http.HttpReads.Implicits.readRaw
import uk.gov.hmrc.http.client.HttpClientV2
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse, StringContextOps}
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig
import uk.gov.hmrc.http.HttpReads.Implicits._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal

@ImplementedBy(classOf[TestOnlyDraftReturnsConnectorImpl])
trait TestOnlyDraftReturnsConnector {
  def deleteDraftReturn(cgtReference: String): EitherT[Future, Error, HttpResponse]
}

@Singleton
class TestOnlyDraftReturnsConnectorImpl @Inject() (
  http: HttpClientV2,
  servicesConfig: ServicesConfig
)(implicit
  ec: ExecutionContext
) extends TestOnlyDraftReturnsConnector {
  implicit val hc: HeaderCarrier = HeaderCarrier()
  private val baseUrl            = servicesConfig.baseUrl("cgt-property-disposals")

  def deleteDraftReturnsUrl(cgtReference: String) =
    s"$baseUrl/test-only/cgt-property-disposals/draftReturn/$cgtReference"

  def deleteDraftReturn(
    cgtReference: String
  ): EitherT[Future, Error, HttpResponse] =
    EitherT[Future, Error, HttpResponse](
      http
        .delete(url"${deleteDraftReturnsUrl(cgtReference)}")
        .execute[HttpResponse]
        .map(Right(_))
        .recover { case NonFatal(e) =>
          Left(Error(e))
        }
    )
}
