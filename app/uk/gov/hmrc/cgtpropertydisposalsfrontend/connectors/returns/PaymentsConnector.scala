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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.returns

import cats.data.EitherT
import com.google.inject.{ImplementedBy, Inject, Singleton}
import play.api.libs.json.{Json, Writes}
import play.api.mvc.Call
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.returns.PaymentsConnectorImpl.StartPaymentJourneyRequest
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Error
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.AmountInPence
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.CgtReference
import uk.gov.hmrc.http.HttpReads.Implicits._
import uk.gov.hmrc.http.client.HttpClientV2
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse, StringContextOps}
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import java.time.LocalDate
import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal

@ImplementedBy(classOf[PaymentsConnectorImpl])
trait PaymentsConnector {
  def startPaymentJourney(
    cgtReference: CgtReference,
    chargeReference: Option[String],
    amount: AmountInPence,
    dueDate: Option[LocalDate],
    returnUrl: Call,
    backUrl: Call
  )(implicit headerCarrier: HeaderCarrier): EitherT[Future, Error, HttpResponse]
}

@Singleton
class PaymentsConnectorImpl @Inject() (
  http: HttpClientV2,
  servicesConfig: ServicesConfig
)(implicit ec: ExecutionContext)
    extends PaymentsConnector {
  private val paymentsBaseUrl = servicesConfig.baseUrl("payments")

  private val selfBaseUrl = servicesConfig.getString("self.url")

  private val startPaymentJourneyUrl =
    s"$paymentsBaseUrl/pay-api/capital-gains-tax/journey/start"

  def startPaymentJourney(
    cgtReference: CgtReference,
    chargeReference: Option[String],
    amount: AmountInPence,
    dueDate: Option[LocalDate],
    returnUrl: Call,
    backUrl: Call
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, HttpResponse] = {
    val body = StartPaymentJourneyRequest(
      cgtReference.value,
      chargeReference,
      amount.value,
      dueDate,
      s"$selfBaseUrl${returnUrl.url}",
      s"$selfBaseUrl${backUrl.url}"
    )

    EitherT(
      http
        .post(url"$startPaymentJourneyUrl")
        .withBody(Json.toJson(body))
        .execute[HttpResponse]
        .map(
          Right(_)
        )
        .recover { case NonFatal(e) =>
          Left(Error(e))
        }
    )
  }
}

object PaymentsConnectorImpl {
  final case class StartPaymentJourneyRequest(
    cgtReference: String,
    chargeReference: Option[String],
    amountInPence: Long,
    dueDate: Option[LocalDate],
    returnUrl: String,
    backUrl: String
  )

  object StartPaymentJourneyRequest {
    implicit val format: Writes[StartPaymentJourneyRequest] = Json.writes
  }
}
