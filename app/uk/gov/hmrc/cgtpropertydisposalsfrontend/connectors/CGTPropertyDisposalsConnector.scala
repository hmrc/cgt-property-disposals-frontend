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

import java.time.LocalDate

import cats.data.EitherT
import com.google.inject.{ImplementedBy, Inject, Singleton}
import play.api.libs.json.{JsNull, Json, OFormat}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.CGTPropertyDisposalsConnectorImpl.BprRequest
import uk.gov.hmrc.cgtpropertydisposalsfrontend.http.HttpClient._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.UserType.{Individual, Trust}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, NINO, SAUTR, SubscriptionDetails}
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig
import uk.gov.hmrc.play.bootstrap.http.HttpClient

import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[CGTPropertyDisposalsConnectorImpl])
trait CGTPropertyDisposalsConnector {

  def getBusinessPartnerRecord(entity: Either[Trust,Individual])(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, HttpResponse]

  def subscribe(subscriptionDetails: SubscriptionDetails)(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, HttpResponse]
}

@Singleton
class CGTPropertyDisposalsConnectorImpl @Inject()(http: HttpClient, servicesConfig: ServicesConfig)(
  implicit ec: ExecutionContext
) extends CGTPropertyDisposalsConnector {

  val baseUrl: String = servicesConfig.baseUrl("cgt-property-disposals") + "/cgt-property-disposals"

  def bprUrl(id: Either[SAUTR,NINO]): String =
    id.fold(
     sautr => s"$baseUrl/business-partner-record/sautr/${sautr.value}",
     nino => s"$baseUrl/business-partner-record/nino/${nino.value}"
    )

  val subscribeUrl: String = s"$baseUrl/subscribe"

  def getBusinessPartnerRecord(entity: Either[Trust,Individual])(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, HttpResponse] =
   EitherT[Future, Error, HttpResponse](
    entity.fold(
      t => http.post(bprUrl(Left(t.sautr)), JsNull),
      i => http.post(bprUrl(i.id), Json.toJson(BprRequest(i.name.firstName, i.name.lastName, i.dateOfBirth.map(_.value))))
    )
      .map(Right(_))
      .recover { case e => Left(Error(e)) }
  )

  def subscribe(
    subscriptionDetails: SubscriptionDetails
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, HttpResponse] = {
    EitherT[Future, Error, HttpResponse](
      http
        .post(subscribeUrl, Json.toJson(subscriptionDetails))
        .map(Right(_))
        .recover { case e => Left(Error(e)) }
    )
  }

}

object CGTPropertyDisposalsConnectorImpl {

  final case class BprRequest(forename: String, surname: String, dateOfBirth: Option[LocalDate])

  object BprRequest {

    implicit val format: OFormat[BprRequest] = Json.format[BprRequest]

  }

}
