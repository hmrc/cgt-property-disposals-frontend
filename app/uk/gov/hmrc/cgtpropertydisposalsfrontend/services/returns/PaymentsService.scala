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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns

import cats.data.EitherT
import cats.instances.future._
import cats.instances.int._
import cats.syntax.either._
import cats.syntax.eq._
import com.google.inject.{ImplementedBy, Inject, Singleton}
import play.api.http.Status.CREATED
import play.api.mvc.{Call, Request}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.returns.PaymentsConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Error
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.{AmountInPence, PaymentsJourney}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.CgtReference
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.audit.PaymentInitiated
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.AuditService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.HttpResponseOps._
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[PaymentsServiceImpl])
trait PaymentsService {

  def startPaymentJourney(
    cgtReference: CgtReference,
    chargeReference: Option[String],
    amount: AmountInPence,
    returnUrl: Call,
    backUrl: Call
  )(implicit
    headerCarrier: HeaderCarrier,
    request: Request[_]
  ): EitherT[Future, Error, PaymentsJourney]

}

@Singleton
class PaymentsServiceImpl @Inject() (
  connector: PaymentsConnector,
  auditService: AuditService
)(implicit
  ec: ExecutionContext
) extends PaymentsService {

  def startPaymentJourney(
    cgtReference: CgtReference,
    chargeReference: Option[String],
    amount: AmountInPence,
    returnUrl: Call,
    backUrl: Call
  )(implicit
    headerCarrier: HeaderCarrier,
    request: Request[_]
  ): EitherT[Future, Error, PaymentsJourney] =
    connector
      .startPaymentJourney(
        cgtReference,
        chargeReference,
        amount,
        returnUrl,
        backUrl
      )
      .subflatMap { response =>
        if (response.status =!= CREATED)
          Left(
            Error(
              s"Call to start payments journey came back with status other than 201 (CREATED): ${response.status}"
            )
          )
        else
          response
            .parseJSON[PaymentsJourney]()
            .leftMap(Error(_))
            .map { journey =>
              auditService.sendEvent(
                "paymentInitiated",
                PaymentInitiated(
                  cgtReference.value,
                  chargeReference,
                  amount.inPounds()
                ),
                "payment-initiated"
              )
              journey
            }
      }

}
