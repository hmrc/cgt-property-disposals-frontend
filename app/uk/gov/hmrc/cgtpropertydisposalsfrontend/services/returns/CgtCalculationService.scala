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
import play.api.http.Status.OK
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.returns.ReturnsConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Error
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.HttpResponseOps._
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[CgtCalculationServiceImpl])
trait CgtCalculationService {

  def calculateTaxDue(
    request: CalculateCgtTaxDueRequest
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, CalculatedTaxDue]

  def calculateTaxableGainOrLoss(
    request: TaxableGainOrLossCalculationRequest
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, TaxableGainOrLossCalculation]

  def calculateYearToDateLiability(
    request: YearToDateLiabilityCalculationRequest
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, YearToDateLiabilityCalculation]

}

@Singleton
class CgtCalculationServiceImpl @Inject() (connector: ReturnsConnector)(implicit
  ec: ExecutionContext
) extends CgtCalculationService {

  def calculateTaxDue(
    request: CalculateCgtTaxDueRequest
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, CalculatedTaxDue] =
    connector.calculateTaxDue(request).subflatMap { response =>
      if (response.status === OK) {
        response
          .parseJSON[CalculatedTaxDue]()
          .leftMap(Error(_))
      } else {
        Left(
          Error(
            s"Call to calculate cgt tax due came back with status ${response.status}"
          )
        )
      }
    }

  def calculateTaxableGainOrLoss(
    request: TaxableGainOrLossCalculationRequest
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, TaxableGainOrLossCalculation] =
    connector.calculateTaxableGainOrLoss(request).subflatMap { response =>
      if (response.status === OK) {
        response
          .parseJSON[TaxableGainOrLossCalculation]()
          .leftMap(Error(_))
      } else {
        Left(
          Error(
            s"Call to calculate taxable gain or loss came back with status ${response.status}"
          )
        )
      }
    }

  def calculateYearToDateLiability(
    request: YearToDateLiabilityCalculationRequest
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, YearToDateLiabilityCalculation] =
    connector.calculateYearToDateLiability(request).subflatMap { response =>
      if (response.status === OK) {
        response
          .parseJSON[YearToDateLiabilityCalculation]()
          .leftMap(Error(_))
      } else {
        Left(
          Error(
            s"Call to calculate year to date liability came back with status ${response.status}"
          )
        )
      }
    }
}
