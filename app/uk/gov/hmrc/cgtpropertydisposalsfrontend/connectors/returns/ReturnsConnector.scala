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
import play.api.http.HeaderNames
import play.api.i18n.Lang
import play.api.libs.json.{Json, OFormat}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.returns.ReturnsConnector.DeleteDraftReturnsRequest
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Error
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.CgtReference
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsServiceImpl.{GetDraftReturnResponse, ListReturnsResponse}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.TaxYearServiceImpl.{AvailableTaxYearsResponse, TaxYearResponse}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging
import uk.gov.hmrc.http.HttpReads.Implicits._
import uk.gov.hmrc.http.{HeaderCarrier, HttpClient, UpstreamErrorResponse}
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import java.util.UUID
import scala.concurrent.{ExecutionContext, Future}
import scala.util.chaining.scalaUtilChainingOps
import scala.util.control.NonFatal

@ImplementedBy(classOf[ReturnsConnectorImpl])
trait ReturnsConnector {

  def storeDraftReturn(draftReturn: DraftReturn, cgtReference: CgtReference)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, Unit]

  def getDraftReturns(cgtReference: CgtReference)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, GetDraftReturnResponse]

  def deleteDraftReturns(draftReturnIds: List[UUID])(implicit hc: HeaderCarrier): EitherT[Future, Error, Unit]

  def submitReturn(submitReturnRequest: SubmitReturnRequest, lang: Lang)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, SubmitReturnResponse]

  def listReturns(cgtReference: CgtReference, fromDate: LocalDate, toDate: LocalDate)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, ListReturnsResponse]

  def displayReturn(cgtReference: CgtReference, submissionId: String)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, DisplayReturn]

  def calculateTaxDue(
    request: CalculateCgtTaxDueRequest
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, CalculatedTaxDue]

  def calculateTaxableGainOrLoss(
    request: TaxableGainOrLossCalculationRequest
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, TaxableGainOrLossCalculation]

  def calculateYearToDateLiability(
    request: YearToDateLiabilityCalculationRequest
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, YearToDateLiabilityCalculation]

  def taxYear(date: LocalDate)(implicit hc: HeaderCarrier): EitherT[Future, Error, TaxYearResponse]

  def availableTaxYears()(implicit hc: HeaderCarrier): EitherT[Future, Error, AvailableTaxYearsResponse]

}

@Singleton
class ReturnsConnectorImpl @Inject() (http: HttpClient, servicesConfig: ServicesConfig)(implicit ec: ExecutionContext)
    extends ReturnsConnector
    with Logging {

  private val baseUrl: String = servicesConfig.baseUrl("cgt-property-disposals")

  private def getDraftReturnsUrl(cgtReference: CgtReference): String =
    s"$baseUrl/draft-returns/${cgtReference.value}"

  private val deleteDraftReturnsUrl: String = s"$baseUrl/draft-returns/delete"

  private val submitReturnUrl: String = s"$baseUrl/return"

  private val calculateCgtTaxDueUrl: String = s"$baseUrl/calculate-tax-due"

  private val calculateTaxableGainOrLossUrl: String = s"$baseUrl/calculate-taxable-gain-or-loss"

  private val calculateYearToDateLiabilityUrl: String = s"$baseUrl/calculate-year-to-date-liability"

  private def handleErrors[T](
    callName: String
  )(f: Future[Either[UpstreamErrorResponse, T]]): EitherT[Future, Error, T] =
    f.map(_.left.map { error =>
      val errorMessage = s"$callName came back with with status ${error.statusCode}"
      logger.error(errorMessage, error)
      Error(errorMessage)
    }).recover { case NonFatal(e) => Left(Error(e.getMessage)) }
      .pipe(EitherT(_))

  override def storeDraftReturn(draftReturn: DraftReturn, cgtReference: CgtReference)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, Unit] = {
    val url = s"$baseUrl/draft-return/${cgtReference.value}"
    http
      .POST[DraftReturn, Unit](url, draftReturn)
      .map(Right(_))
      .pipe(handleErrors(s"POST to $url"))
  }

  override def getDraftReturns(
    cgtReference: CgtReference
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, GetDraftReturnResponse] =
    http.GET[Either[UpstreamErrorResponse, GetDraftReturnResponse]](getDraftReturnsUrl(cgtReference)) pipe
      handleErrors(s"GET to ${getDraftReturnsUrl(cgtReference)}")

  def deleteDraftReturns(draftReturnIds: List[UUID])(implicit hc: HeaderCarrier): EitherT[Future, Error, Unit] =
    http.POST[DeleteDraftReturnsRequest, Either[UpstreamErrorResponse, Unit]](
      deleteDraftReturnsUrl,
      DeleteDraftReturnsRequest(draftReturnIds)
    ) pipe handleErrors(s"POST to $deleteDraftReturnsUrl")

  def submitReturn(
    submitReturnRequest: SubmitReturnRequest,
    lang: Lang
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, SubmitReturnResponse] =
    http.POST[SubmitReturnRequest, Either[UpstreamErrorResponse, SubmitReturnResponse]](
      submitReturnUrl,
      submitReturnRequest,
      Seq(HeaderNames.ACCEPT_LANGUAGE -> lang.language)
    ) pipe handleErrors(s"POST to $submitReturnUrl")

  def listReturns(cgtReference: CgtReference, fromDate: LocalDate, toDate: LocalDate)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, ListReturnsResponse] = {
    val url: String =
      s"$baseUrl/returns/${cgtReference.value}/${fromDate.format(dateFormatter)}/${toDate.format(dateFormatter)}"
    http.GET[Either[UpstreamErrorResponse, ListReturnsResponse]](url) pipe handleErrors(s"GET to $url")
  }

  def displayReturn(cgtReference: CgtReference, submissionId: String)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, DisplayReturn] = {
    val url = s"$baseUrl/return/${cgtReference.value}/$submissionId"
    http.GET[Either[UpstreamErrorResponse, DisplayReturn]](url) pipe handleErrors(s"GET to $url")
  }

  def calculateTaxDue(
    request: CalculateCgtTaxDueRequest
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, CalculatedTaxDue] =
    http
      .POST[CalculateCgtTaxDueRequest, Either[UpstreamErrorResponse, CalculatedTaxDue]](calculateCgtTaxDueUrl, request)
      .pipe(handleErrors(s"POST to $calculateCgtTaxDueUrl"))

  def calculateTaxableGainOrLoss(
    request: TaxableGainOrLossCalculationRequest
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, TaxableGainOrLossCalculation] =
    http.POST[TaxableGainOrLossCalculationRequest, Either[UpstreamErrorResponse, TaxableGainOrLossCalculation]](
      calculateTaxableGainOrLossUrl,
      request
    ) pipe handleErrors(s"POST to $calculateCgtTaxDueUrl")

  def calculateYearToDateLiability(
    request: YearToDateLiabilityCalculationRequest
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, YearToDateLiabilityCalculation] =
    http.POST[YearToDateLiabilityCalculationRequest, Either[UpstreamErrorResponse, YearToDateLiabilityCalculation]](
      calculateYearToDateLiabilityUrl,
      request
    ) pipe handleErrors(s"POST to $calculateYearToDateLiabilityUrl")

  def taxYear(date: LocalDate)(implicit hc: HeaderCarrier): EitherT[Future, Error, TaxYearResponse] = {
    val url = s"$baseUrl/tax-year/${date.format(dateFormatter)}"
    http.GET[Either[UpstreamErrorResponse, TaxYearResponse]](url) pipe handleErrors(s"GET to $url")
  }

  def availableTaxYears()(implicit hc: HeaderCarrier): EitherT[Future, Error, AvailableTaxYearsResponse] = {
    val url = s"$baseUrl/available-tax-years"
    http.GET[Either[UpstreamErrorResponse, AvailableTaxYearsResponse]](url) pipe handleErrors(s"GET to $url")
  }

  private val dateFormatter: DateTimeFormatter = DateTimeFormatter.ISO_DATE

}

object ReturnsConnector {

  final case class DeleteDraftReturnsRequest(draftReturnIds: List[UUID])

  object DeleteDraftReturnsRequest {

    implicit val format: OFormat[DeleteDraftReturnsRequest] = Json.format

  }

}
