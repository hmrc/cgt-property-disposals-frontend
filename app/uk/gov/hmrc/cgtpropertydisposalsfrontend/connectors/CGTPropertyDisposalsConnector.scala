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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors

import cats.data.EitherT
import com.google.inject.{ImplementedBy, Inject, Singleton}
import play.api.libs.json.Json
import uk.gov.hmrc.cgtpropertydisposalsfrontend.http.HttpClient._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Error
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.CgtReference
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.bpr.BusinessPartnerRecordRequest
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.{RegistrationDetails, SubscribedUpdateDetails, SubscriptionDetails}
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig
import uk.gov.hmrc.play.bootstrap.http.HttpClient

import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[CGTPropertyDisposalsConnectorImpl])
trait CGTPropertyDisposalsConnector {

  def getBusinessPartnerRecord(request: BusinessPartnerRecordRequest)(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, HttpResponse]

  def subscribe(subscriptionDetails: SubscriptionDetails)(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, HttpResponse]

  def registerWithoutId(registrationDetails: RegistrationDetails)(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, HttpResponse]

  def getSubscriptionStatus()(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, HttpResponse]

  def getSubscribedDetails(cgtReference: CgtReference)(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, HttpResponse]

  def updateSubscribedDetails(subscribedAndVerifierDetails: SubscribedUpdateDetails)(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, HttpResponse]
}

@Singleton
class CGTPropertyDisposalsConnectorImpl @Inject() (http: HttpClient, servicesConfig: ServicesConfig)(
  implicit ec: ExecutionContext
) extends CGTPropertyDisposalsConnector {

  val baseUrl: String = servicesConfig.baseUrl("cgt-property-disposals") + "/cgt-property-disposals"

  val bprUrl: String = s"$baseUrl/business-partner-record"

  val subscribeUrl: String = s"$baseUrl/subscription"

  val registerWithoutIdAndSubscribeUrl: String = s"$baseUrl/register-without-id"

  val subscriptionStatusUrl: String = s"$baseUrl/check-subscription-status"

  val subscriptionUpdateUrl: String = s"$baseUrl/subscription"

  def getSubscribedDetailsUrl(cgtReference: CgtReference): String = s"$baseUrl/subscription/${cgtReference.value}"

  override def getBusinessPartnerRecord(request: BusinessPartnerRecordRequest)(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, HttpResponse] =
    makeCall(_.post(bprUrl, Json.toJson(request)))

  override def subscribe(
    subscriptionDetails: SubscriptionDetails
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, HttpResponse] =
    makeCall(_.post(subscribeUrl, Json.toJson(subscriptionDetails)))

  override def registerWithoutId(registrationDetails: RegistrationDetails)(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, HttpResponse] =
    makeCall(_.post(registerWithoutIdAndSubscribeUrl, Json.toJson(registrationDetails)))

  override def getSubscriptionStatus()(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, HttpResponse] =
    makeCall(_.get(subscriptionStatusUrl))

  override def getSubscribedDetails(cgtReference: CgtReference)(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, HttpResponse] =
    makeCall(_.get(getSubscribedDetailsUrl(cgtReference)))

  override def updateSubscribedDetails(subscribedUpdateDetails: SubscribedUpdateDetails)(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, HttpResponse] =
    makeCall(_.put(subscriptionUpdateUrl, subscribedUpdateDetails))

  private def makeCall(call: HttpClient => Future[HttpResponse]): EitherT[Future, Error, HttpResponse] =
    EitherT[Future, Error, HttpResponse](
      call(http)
        .map(Right(_))
        .recover { case e => Left(Error(e)) }
    )

}
