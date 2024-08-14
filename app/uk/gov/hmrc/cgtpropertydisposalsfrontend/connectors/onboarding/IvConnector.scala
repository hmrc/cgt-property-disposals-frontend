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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.onboarding

import cats.data.EitherT
import com.google.inject.{ImplementedBy, Inject, Singleton}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Error
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.onboarding.IvServiceImpl.IvStatusResponse
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging
import uk.gov.hmrc.http.HttpReads.Implicits._
import uk.gov.hmrc.http.{HeaderCarrier, HttpClient, HttpResponse, UpstreamErrorResponse}
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import java.util.UUID
import scala.concurrent.{ExecutionContext, Future}
import scala.util.chaining.scalaUtilChainingOps

@ImplementedBy(classOf[IvConnectorImpl])
trait IvConnector {
  def getFailedJourneyStatus(journeyId: UUID)(implicit hc: HeaderCarrier): EitherT[Future, Error, IvStatusResponse]
}

@Singleton
class IvConnectorImpl @Inject() (
  http: HttpClient,
  servicesConfig: ServicesConfig
)(implicit
  ec: ExecutionContext
) extends IvConnector with Logging {

  val baseUrl: String = servicesConfig.baseUrl("iv")

  def url(journeyId: UUID): String = s"$baseUrl/mdtp/journey/journeyId/${journeyId.toString}"

  override def getFailedJourneyStatus(
    journeyId: UUID
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, IvStatusResponse] =
    http
      .GET[Either[UpstreamErrorResponse, IvStatusResponse]](url(journeyId))
      .map(_.left.map { error =>
        logger.error(s"GET to ${url(journeyId)} failed", error)
        Error(s"Response to address lookup came back with status ${error.statusCode}")
      })
      .recover(e => Left(Error(e.getMessage)))
      .pipe(EitherT(_))
}
