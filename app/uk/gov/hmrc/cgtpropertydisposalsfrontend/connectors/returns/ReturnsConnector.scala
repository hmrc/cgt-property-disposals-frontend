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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.returns

import cats.data.EitherT
import com.google.inject.{ImplementedBy, Inject, Singleton}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.http.HttpClient._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Error
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.CgtReference
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.DraftReturn
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig
import uk.gov.hmrc.play.bootstrap.http.HttpClient

import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal

@ImplementedBy(classOf[ReturnsConnectorImpl])
trait ReturnsConnector {

  def storeDraftReturn(draftReturn: DraftReturn)(implicit hc: HeaderCarrier): EitherT[Future, Error, HttpResponse]

  def getDraftReturns(cgtReference: CgtReference)(implicit hc: HeaderCarrier): EitherT[Future, Error, HttpResponse]

}

@Singleton
class ReturnsConnectorImpl @Inject() (http: HttpClient, servicesConfig: ServicesConfig)(
  implicit ec: ExecutionContext
) extends ReturnsConnector {

  val baseUrl: String = servicesConfig.baseUrl("cgt-property-disposals")

  val storeDraftReturnUrl: String = s"$baseUrl/draft-return"

  def getDraftReturnsUrl(cgtReference: CgtReference): String = s"$baseUrl/draft-returns/${cgtReference.value}"

  override def storeDraftReturn(
    draftReturn: DraftReturn
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, HttpResponse] =
    EitherT[Future, Error, HttpResponse](
      http
        .post(storeDraftReturnUrl, draftReturn)
        .map(Right(_))
        .recover {
          case NonFatal(e) => Left(Error(e))
        }
    )

  override def getDraftReturns(
    cgtReference: CgtReference
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, HttpResponse] =
    EitherT[Future, Error, HttpResponse](
      http
        .get(getDraftReturnsUrl(cgtReference))
        .map(Right(_))
        .recover {
          case NonFatal(e) => Left(Error(e))
        }
    )

}
