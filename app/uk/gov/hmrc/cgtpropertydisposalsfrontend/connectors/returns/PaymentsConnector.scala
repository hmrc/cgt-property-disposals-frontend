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
import play.api.libs.json.{Json, Writes}
import play.api.mvc.Call
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.returns.PaymentsConnectorImpl.StartPaymentJourneyRequest
import uk.gov.hmrc.cgtpropertydisposalsfrontend.http.HttpClient.HttpClientOps
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{AmountInPence, Error}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.CgtReference
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig
import uk.gov.hmrc.play.bootstrap.http.HttpClient

import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal

@ImplementedBy(classOf[PaymentsConnectorImpl])
trait PaymentsConnector {

  def startPaymentJourney(
    cgtReference: CgtReference,
    chargeReference: String,
    amount: AmountInPence,
    returnUrl: Call,
    backUrl: Call
  )(implicit headerCarrier: HeaderCarrier): EitherT[Future, Error, HttpResponse]

}

@Singleton
class PaymentsConnectorImpl @Inject() (http: HttpClient, servicesConfig: ServicesConfig)(implicit ec: ExecutionContext)
    extends PaymentsConnector {

  val paymentsBaseUrl: String = servicesConfig.baseUrl("payments")

  val selfBaseUrl: String = servicesConfig.getString("self.url")

  val startPaymentJourneyUrl: String = s"$paymentsBaseUrl/pay-api/capital-gains-tax/journey/start"

  override def startPaymentJourney(
    cgtReference: CgtReference,
    chargeReference: String,
    amount: AmountInPence,
    returnUrl: Call,
    backUrl: Call
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, HttpResponse] =
    EitherT(
      http
        .post(
          startPaymentJourneyUrl,
          Json.toJson(
            StartPaymentJourneyRequest(
              cgtReference.value,
              chargeReference,
              amount.value,
              s"$selfBaseUrl${returnUrl.url}",
              s"$selfBaseUrl${backUrl.url}"
            )
          )
        )
        .map(
          Right(_)
        )
        .recover {
          case NonFatal(e) => Left(Error(e))
        }
    )

}

object PaymentsConnectorImpl {

  final case class StartPaymentJourneyRequest(
    cgtReference: String,
    chargeReference: String,
    amountInPence: Long,
    returnUrl: String,
    backUrl: String
  )

  object StartPaymentJourneyRequest {
    implicit val format: Writes[StartPaymentJourneyRequest] = Json.writes
  }

}
