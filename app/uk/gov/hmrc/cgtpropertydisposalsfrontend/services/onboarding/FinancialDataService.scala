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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.services.onboarding

import java.time.{Clock, LocalDate}

import cats.data.EitherT
import cats.instances.future._
import cats.instances.int._
import cats.syntax.either._
import cats.syntax.eq._
import com.google.inject.{ImplementedBy, Inject, Singleton}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.onboarding.FinancialDataConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.metrics.Metrics
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, TaxYear}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.homepage.{FinancialDataResponse, FinancialTransaction}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.HttpResponseOps._
import play.api.http.Status.OK
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.CgtReference

import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[FinancialDataServiceImpl])
trait FinancialDataService {

  def getFinancialData(cgtReference: CgtReference)(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, List[FinancialTransaction]]

}

@Singleton
class FinancialDataServiceImpl @Inject() (connector: FinancialDataConnector, metrics: Metrics)(
  implicit ec: ExecutionContext
) extends FinancialDataService {

  override def getFinancialData(cgtReference: CgtReference)(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, List[FinancialTransaction]] =
    connector.getFinancialData(cgtReference, fromDate, toDate).subflatMap { response =>
      if (response.status === OK)
        response
          .parseJSON[FinancialDataResponse]()
          .map(_.financialTransactions)
          .leftMap(Error(_))
      else
        Left(Error(s"Call to get financial data came back with status ${response.status}"))
    }

  val fromDate = TaxYear.thisTaxYearStartDate()

  val toDate = fromDate.plusYears(1L).minusDays(1L)

}
