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
import play.api.libs.json.Json
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Error
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.CgtReference
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.bpr.BusinessPartnerRecordRequest
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.{RegistrationDetails, SubscribedUpdateDetails, SubscriptionDetails}
import uk.gov.hmrc.http.HttpReads.Implicits._
import uk.gov.hmrc.http.client.HttpClientV2
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse, StringContextOps}
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import scala.concurrent.{ExecutionContext, Future}
import play.api.libs.ws.writeableOf_JsValue

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
  http: HttpClientV2,
  servicesConfig: ServicesConfig
)(implicit
  ec: ExecutionContext
) extends CGTPropertyDisposalsConnector {
  private val baseUrl =
    servicesConfig.baseUrl("cgt-property-disposals") + "/cgt-property-disposals"

  private val bprUrl = s"$baseUrl/business-partner-record"

  private val subscribeUrl = s"$baseUrl/subscription"

  private val registerWithoutIdAndSubscribeUrl = s"$baseUrl/register-without-id"

  private val subscriptionStatusUrl: String = s"$baseUrl/check-subscription-status"

  private val subscriptionUpdateUrl = s"$baseUrl/subscription"

  private def getSubscribedDetailsUrl(cgtReference: CgtReference) =
    s"$baseUrl/subscription/${cgtReference.value}"

  def getBusinessPartnerRecord(request: BusinessPartnerRecordRequest, lang: Lang)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, HttpResponse] =
    makeCall(
      _.post(url"$bprUrl")
        .withBody(Json.toJson(request))
        .setHeader((HeaderNames.ACCEPT_LANGUAGE, lang.language))
        .execute[HttpResponse]
    )

  def subscribe(
    subscriptionDetails: SubscriptionDetails,
    lang: Lang
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, HttpResponse] =
    makeCall(
      _.post(url"$subscribeUrl")
        .withBody(Json.toJson(subscriptionDetails))
        .setHeader((HeaderNames.ACCEPT_LANGUAGE, lang.language))
        .execute[HttpResponse]
    )

  def registerWithoutId(registrationDetails: RegistrationDetails)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, HttpResponse] =
    makeCall(
      _.post(url"$registerWithoutIdAndSubscribeUrl").withBody(Json.toJson(registrationDetails)).execute[HttpResponse]
    )

  def getSubscriptionStatus()(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, HttpResponse] =
    makeCall(_.get(url"$subscriptionStatusUrl").execute[HttpResponse])

  def getSubscribedDetails(cgtReference: CgtReference)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, HttpResponse] =
    makeCall(_.get(url"${getSubscribedDetailsUrl(cgtReference)}").execute[HttpResponse])

  def updateSubscribedDetails(
    subscribedUpdateDetails: SubscribedUpdateDetails
  )(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, HttpResponse] =
    makeCall(
      _.put(url"$subscriptionUpdateUrl").withBody(Json.toJson(subscribedUpdateDetails)).execute[HttpResponse]
    )

  def testSubmitToDms(
    cgtReference: CgtReference
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, HttpResponse] =
    makeCall(_.get(url"$baseUrl/dms-test").execute[HttpResponse])

  private def makeCall(
    call: HttpClientV2 => Future[HttpResponse]
  ) =
    EitherT[Future, Error, HttpResponse](
      call(http)
        .map(Right(_))
        .recover { case e => Left(Error(e)) }
    )
}
