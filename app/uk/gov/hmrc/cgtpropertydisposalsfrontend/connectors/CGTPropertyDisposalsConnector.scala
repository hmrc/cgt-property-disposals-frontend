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
import cats.syntax.either._
import com.google.inject.{ImplementedBy, Inject, Singleton}
import julienrf.json.derived
import play.api.libs.json.{JsNull, Json, OFormat, OWrites, Reads, Writes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.CGTPropertyDisposalsConnectorImpl.OutgoingBprRequest
import uk.gov.hmrc.cgtpropertydisposalsfrontend.http.HttpClient._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.UserType.{Individual, Trust}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, SubscriptionDetails}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.EitherFormat.eitherFormat
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig
import uk.gov.hmrc.play.bootstrap.http.HttpClient

import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[CGTPropertyDisposalsConnectorImpl])
trait CGTPropertyDisposalsConnector {

  def getBusinessPartnerRecord(entity: Either[Trust,Individual], requiresNameMatch: Boolean)(
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

  val bprUrl: String = s"$baseUrl/business-partner-record"

  val subscribeUrl: String = s"$baseUrl/subscribe"

  def getBusinessPartnerRecord(entity: Either[Trust,Individual], requiresNameMatch: Boolean)(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, HttpResponse] = {
    val outgoingRequest = entity.fold[OutgoingBprRequest](
      trust => OutgoingBprRequest.OrganisationBprRequest(trust.sautr.value, requiresNameMatch),
      individual => OutgoingBprRequest.IndividualBprRequest(
        individual.id.bimap(_.value, _.value),
        individual.name.firstName,
        individual.name.lastName,
        individual.dateOfBirth.map(_.value),
        requiresNameMatch
      )
    )

    EitherT[Future, Error, HttpResponse](
       http.post(bprUrl, Json.toJson(outgoingRequest))
        .map(Right(_))
        .recover { case e => Left(Error(e)) }
    )
  }

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

  private sealed trait OutgoingBprRequest

  private object OutgoingBprRequest {
    final case class IndividualBprRequest(id: Either[String,String],
                                          forename: String,
                                          surname: String,
                                          dateOfBirth: Option[LocalDate],
                                          requiresNameMatch: Boolean
                                         ) extends OutgoingBprRequest

    final case class OrganisationBprRequest(sautr: String,
                                            requiresNameMatch: Boolean
                                           ) extends OutgoingBprRequest


    implicit val individualBprRequestWrites: Writes[IndividualBprRequest] = Json.writes[IndividualBprRequest]

    @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
    implicit val format: OWrites[OutgoingBprRequest] = derived.owrites[OutgoingBprRequest]

  }

}
