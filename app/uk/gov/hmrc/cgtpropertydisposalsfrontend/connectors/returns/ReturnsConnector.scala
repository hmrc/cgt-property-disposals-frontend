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
import uk.gov.hmrc.http.client.HttpClientV2
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse, StringContextOps, UpstreamErrorResponse}
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
class ReturnsConnectorImpl @Inject() (http: HttpClientV2, servicesConfig: ServicesConfig)(implicit ec: ExecutionContext)
    extends ReturnsConnector
    with Logging {
  private val baseUrl = servicesConfig.baseUrl("cgt-property-disposals")

  private def getDraftReturnsUrl(cgtReference: CgtReference) =
    s"$baseUrl/draft-returns/${cgtReference.value}"

  private val deleteDraftReturnsUrl = s"$baseUrl/draft-returns/delete"

  private val submitReturnUrl = s"$baseUrl/return"

  private val calculateCgtTaxDueUrl = s"$baseUrl/calculate-tax-due"

  private val calculateTaxableGainOrLossUrl = s"$baseUrl/calculate-taxable-gain-or-loss"

  private val calculateYearToDateLiabilityUrl = s"$baseUrl/calculate-year-to-date-liability"

  private def checkAndIgnoreBody(callName: String)(f: Future[HttpResponse]) =
    f.map { response =>
      response.status match {
        case 200   => Right(())
        case other => Left(Error(s"$callName came back with with status $other"))
      }
    }.pipe(EitherT(_))

  private def handleErrors[T](
    callName: String
  )(f: Future[Either[UpstreamErrorResponse, T]]) =
    f.map(_.left.map { error =>
      val errorMessage = s"$callName came back with with status ${error.statusCode}"
      logger.error(errorMessage, error)
      Error(errorMessage)
    }).recover { case NonFatal(e) => Left(Error(e.getMessage)) }
      .pipe(EitherT(_))

  def storeDraftReturn(draftReturn: DraftReturn, cgtReference: CgtReference)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, Unit] = {
    val url = s"$baseUrl/draft-return/${cgtReference.value}"
    http.post(url"$url").withBody(Json.toJson(draftReturn)).execute[HttpResponse] pipe checkAndIgnoreBody(
      s"POST to $url"
    )
  }

  def getDraftReturns(
    cgtReference: CgtReference
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, GetDraftReturnResponse] =
    http
      .get(url"${getDraftReturnsUrl(cgtReference)}")
      .execute[Either[UpstreamErrorResponse, GetDraftReturnResponse]] pipe handleErrors(
      s"GET to ${getDraftReturnsUrl(cgtReference)}"
    )

  def deleteDraftReturns(draftReturnIds: List[UUID])(implicit hc: HeaderCarrier): EitherT[Future, Error, Unit] =
    http
      .post(url"$deleteDraftReturnsUrl")
      .withBody(Json.toJson(DeleteDraftReturnsRequest(draftReturnIds)))
      .execute[HttpResponse] pipe checkAndIgnoreBody(s"POST to $deleteDraftReturnsUrl")

  def submitReturn(
    submitReturnRequest: SubmitReturnRequest,
    lang: Lang
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, SubmitReturnResponse] =
    http
      .post(url"$submitReturnUrl")
      .withBody(Json.toJson(submitReturnRequest))
      .setHeader((HeaderNames.ACCEPT_LANGUAGE, lang.language))
      .execute[Either[UpstreamErrorResponse, SubmitReturnResponse]] pipe handleErrors(s"POST to $submitReturnUrl")

  def listReturns(cgtReference: CgtReference, fromDate: LocalDate, toDate: LocalDate)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, ListReturnsResponse] = {
    val url: String =
      s"$baseUrl/returns/${cgtReference.value}/${fromDate.format(dateFormatter)}/${toDate.format(dateFormatter)}"
    http.get(url"$url").execute[Either[UpstreamErrorResponse, ListReturnsResponse]] pipe handleErrors(s"GET to $url")
  }

  def displayReturn(cgtReference: CgtReference, submissionId: String)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, DisplayReturn] = {
    val url = s"$baseUrl/return/${cgtReference.value}/$submissionId"
    http.get(url"$url").execute[Either[UpstreamErrorResponse, DisplayReturn]] pipe handleErrors(s"GET to $url")
  }

  def calculateTaxDue(
    request: CalculateCgtTaxDueRequest
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, CalculatedTaxDue] =
    http
      .post(url"$calculateCgtTaxDueUrl")
      .withBody(Json.toJson(request))
      .execute[Either[UpstreamErrorResponse, CalculatedTaxDue]]
      .pipe(handleErrors(s"POST to $calculateCgtTaxDueUrl"))

  def calculateTaxableGainOrLoss(
    request: TaxableGainOrLossCalculationRequest
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, TaxableGainOrLossCalculation] =
    http
      .post(url"$calculateTaxableGainOrLossUrl")
      .withBody(Json.toJson(request))
      .execute[Either[UpstreamErrorResponse, TaxableGainOrLossCalculation]] pipe handleErrors(
      s"POST to $calculateTaxableGainOrLossUrl"
    )

  def calculateYearToDateLiability(
    request: YearToDateLiabilityCalculationRequest
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, YearToDateLiabilityCalculation] =
    http
      .post(url"$calculateYearToDateLiabilityUrl")
      .withBody(Json.toJson(request))
      .execute[Either[UpstreamErrorResponse, YearToDateLiabilityCalculation]] pipe handleErrors(
      s"POST to $calculateYearToDateLiabilityUrl"
    )

  def taxYear(date: LocalDate)(implicit hc: HeaderCarrier): EitherT[Future, Error, TaxYearResponse] = {
    val url = s"$baseUrl/tax-year/${date.format(dateFormatter)}"
    http.get(url"$url").execute[Either[UpstreamErrorResponse, TaxYearResponse]] pipe handleErrors(s"GET to $url")
  }

  def availableTaxYears()(implicit hc: HeaderCarrier): EitherT[Future, Error, AvailableTaxYearsResponse] = {
    val url = s"$baseUrl/available-tax-years"
    http.get(url"$url").execute[Either[UpstreamErrorResponse, AvailableTaxYearsResponse]] pipe handleErrors(
      s"GET to $url"
    )
  }

  private val dateFormatter = DateTimeFormatter.ISO_DATE
}

object ReturnsConnector {
  final case class DeleteDraftReturnsRequest(draftReturnIds: List[UUID])

  object DeleteDraftReturnsRequest {
    implicit val format: OFormat[DeleteDraftReturnsRequest] = Json.format
  }
}
