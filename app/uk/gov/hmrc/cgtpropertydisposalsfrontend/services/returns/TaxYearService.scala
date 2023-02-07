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
import play.api.libs.json.{Json, OFormat}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.returns.ReturnsConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, TaxYear}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.TaxYearServiceImpl.{AvailableTaxYearsResponse, TaxYearResponse}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.HttpResponseOps._
import uk.gov.hmrc.http.HeaderCarrier

import java.time.LocalDate
import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[TaxYearServiceImpl])
trait TaxYearService {

  def taxYear(date: LocalDate)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, Option[TaxYear]]

  def availableTaxYears()(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, List[Int]]

}

@Singleton
class TaxYearServiceImpl @Inject() (connector: ReturnsConnector)(implicit
  ec: ExecutionContext
) extends TaxYearService {

  override def taxYear(
    date: LocalDate
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, Option[TaxYear]] =
    connector.taxYear(date).subflatMap { response =>
      if (response.status === OK)
        response
          .parseJSON[TaxYearResponse]()
          .bimap(Error(_), _.value)
      else
        Left(
          Error(
            s"Call to get tax year came back with unexpected status ${response.status}"
          )
        )
    }

  override def availableTaxYears()(implicit hc: HeaderCarrier): EitherT[Future, Error, List[Int]] =
    connector.availableTaxYears().subflatMap { response =>
      if (response.status === OK)
        response
          .parseJSON[AvailableTaxYearsResponse]()
          .bimap(Error(_), _.value)
      else
        Left(
          Error(
            s"Call to get tax year came back with unexpected status ${response.status}"
          )
        )
    }

}

object TaxYearServiceImpl {

  final case class TaxYearResponse(value: Option[TaxYear])
  final case class AvailableTaxYearsResponse(value: List[Int])

  implicit val taxYearResponseFormat: OFormat[TaxYearResponse]                     = Json.format
  implicit val availableTaxYearsResponseFormat: OFormat[AvailableTaxYearsResponse] = Json.format

}
