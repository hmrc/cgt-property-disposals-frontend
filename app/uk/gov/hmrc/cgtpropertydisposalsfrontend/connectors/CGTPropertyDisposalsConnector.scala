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

import cats.data.EitherT
import com.google.inject.{ImplementedBy, Inject, Singleton}
import play.api.http.HeaderNames
import play.api.i18n.Lang
import play.api.libs.json.{JsValue, Json}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Error
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.CgtReference
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.bpr.BusinessPartnerRecordRequest
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.{RegistrationDetails, SubscribedUpdateDetails, SubscriptionDetails}
import uk.gov.hmrc.http.{HeaderCarrier, HttpClient, HttpResponse}
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig
import uk.gov.hmrc.http.HttpReads.Implicits._

import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[CGTPropertyDisposalsConnectorImpl])
trait CGTPropertyDisposalsConnector {

  def getBusinessPartnerRecord(request: BusinessPartnerRecordRequest, lang: Lang)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, HttpResponse]

  def subscribe(subscriptionDetails: SubscriptionDetails, lang: Lang)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, HttpResponse]

  def registerWithoutId(registrationDetails: RegistrationDetails)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, HttpResponse]

  def getSubscriptionStatus()(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, HttpResponse]

  def getSubscribedDetails(cgtReference: CgtReference)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, HttpResponse]

  def updateSubscribedDetails(
    subscribedAndVerifierDetails: SubscribedUpdateDetails
  )(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, HttpResponse]

  def testSubmitToDms(cgtReference: CgtReference)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, HttpResponse]
}

@Singleton
class CGTPropertyDisposalsConnectorImpl @Inject() (
  http: HttpClient,
  servicesConfig: ServicesConfig
)(implicit
  ec: ExecutionContext
) extends CGTPropertyDisposalsConnector {

  val baseUrl: String =
    servicesConfig.baseUrl("cgt-property-disposals") + "/cgt-property-disposals"

  val bprUrl: String = s"$baseUrl/business-partner-record"

  val subscribeUrl: String = s"$baseUrl/subscription"

  val registerWithoutIdAndSubscribeUrl: String = s"$baseUrl/register-without-id"

  val subscriptionStatusUrl: String = s"$baseUrl/check-subscription-status"

  val subscriptionUpdateUrl: String = s"$baseUrl/subscription"

  def getSubscribedDetailsUrl(cgtReference: CgtReference): String =
    s"$baseUrl/subscription/${cgtReference.value}"

  override def getBusinessPartnerRecord(request: BusinessPartnerRecordRequest, lang: Lang)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, HttpResponse] =
    makeCall(
      _.POST[JsValue, HttpResponse](bprUrl, Json.toJson(request), Seq(HeaderNames.ACCEPT_LANGUAGE -> lang.language))
    )

  override def subscribe(
    subscriptionDetails: SubscriptionDetails,
    lang: Lang
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, HttpResponse] =
    makeCall(
      _.POST[JsValue, HttpResponse](
        subscribeUrl,
        Json.toJson(subscriptionDetails),
        Seq(HeaderNames.ACCEPT_LANGUAGE -> lang.language)
      )
    )

  override def registerWithoutId(registrationDetails: RegistrationDetails)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, HttpResponse] =
    makeCall(
      _.POST[JsValue, HttpResponse](registerWithoutIdAndSubscribeUrl, Json.toJson(registrationDetails))
    )

  override def getSubscriptionStatus()(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, HttpResponse] =
    makeCall(_.GET[HttpResponse](subscriptionStatusUrl))

  override def getSubscribedDetails(cgtReference: CgtReference)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, HttpResponse] =
    makeCall(_.GET[HttpResponse](getSubscribedDetailsUrl(cgtReference)))

  override def updateSubscribedDetails(
    subscribedUpdateDetails: SubscribedUpdateDetails
  )(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, HttpResponse] =
    makeCall(_.PUT[SubscribedUpdateDetails, HttpResponse](subscriptionUpdateUrl, subscribedUpdateDetails))

  override def testSubmitToDms(
    cgtReference: CgtReference
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, HttpResponse] =
    makeCall(_.GET[HttpResponse](baseUrl + "/dms-test"))

  private def makeCall(
    call: HttpClient => Future[HttpResponse]
  ): EitherT[Future, Error, HttpResponse] =
    EitherT[Future, Error, HttpResponse](
      call(http)
        .map(Right(_))
        .recover { case e => Left(Error(e)) }
    )

}
