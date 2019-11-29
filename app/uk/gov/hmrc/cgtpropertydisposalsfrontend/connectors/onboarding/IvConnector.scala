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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.onboarding

import java.util.UUID

import cats.data.EitherT
import com.google.inject.{ImplementedBy, Inject, Singleton}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.http.HttpClient._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Error
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig
import uk.gov.hmrc.play.bootstrap.http.HttpClient

import scala.concurrent.{ExecutionContext, Future}
@ImplementedBy(classOf[IvConnectorImpl])
trait IvConnector {

  def getFailedJourneyStatus(journeyId: UUID)(implicit hc: HeaderCarrier): EitherT[Future, Error, HttpResponse]

}

@Singleton
class IvConnectorImpl @Inject()(http: HttpClient, servicesConfig: ServicesConfig)(
  implicit ec: ExecutionContext
) extends IvConnector {

  val baseUrl: String = servicesConfig.baseUrl("iv")

  def url(journeyId: UUID): String = s"$baseUrl/mdtp/journey/journeyId/${journeyId.toString}"

  override def getFailedJourneyStatus(
    journeyId: UUID
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, HttpResponse] =
    EitherT[Future, Error, HttpResponse](
      http
        .get(url(journeyId))
        .map(Right(_))
        .recover {
          case e ⇒
            Left(Error(e))
        }
    )
}
