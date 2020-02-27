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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.onboarding

import java.time.LocalDate
import java.time.format.DateTimeFormatter

import cats.data.EitherT
import com.google.inject.{ImplementedBy, Inject, Singleton}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Error
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig
import uk.gov.hmrc.play.bootstrap.http.HttpClient
import uk.gov.hmrc.cgtpropertydisposalsfrontend.http.HttpClient._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.CgtReference

import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[FinancialDataConnectorImpl])
trait FinancialDataConnector {

  def getFinancialData(cgtReference: CgtReference, fromDate: LocalDate, toDate: LocalDate)(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, HttpResponse]

}

@Singleton
class FinancialDataConnectorImpl @Inject() (http: HttpClient, servicesConfig: ServicesConfig)(
  implicit ec: ExecutionContext
) extends FinancialDataConnector {

  val baseUrl: String = servicesConfig.baseUrl("cgt-property-disposals")

  def financialDataUrl(cgtReference: CgtReference, fromDate: LocalDate, toDate: LocalDate) = {
    val cgtRef = cgtReference.value
    val from   = fromDate.format(DateTimeFormatter.ISO_DATE)
    val to     = toDate.format(DateTimeFormatter.ISO_DATE)
    s"$baseUrl/cgt-property-disposals/financial-data/$cgtRef/$from/$to"
  }

  override def getFinancialData(cgtReference: CgtReference, fromDate: LocalDate, toDate: LocalDate)(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, HttpResponse] =
    makeCall(_.get(financialDataUrl(cgtReference, fromDate, toDate)))

  private def makeCall(call: HttpClient => Future[HttpResponse]): EitherT[Future, Error, HttpResponse] =
    EitherT[Future, Error, HttpResponse](
      call(http)
        .map(Right(_))
        .recover { case e => Left(Error(e)) }
    )
}
