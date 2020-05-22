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

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import java.util.UUID

import cats.data.EitherT
import com.google.inject.{ImplementedBy, Inject, Singleton}
import play.api.libs.json.{Json, OFormat}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.returns.ReturnsConnector.DeleteDraftReturnsRequest
import uk.gov.hmrc.cgtpropertydisposalsfrontend.http.HttpClient._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Error
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.CgtReference
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{CalculateCgtTaxDueRequest, DraftReturn, SubmitReturnRequest}
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig
import uk.gov.hmrc.play.bootstrap.http.HttpClient

import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal

@ImplementedBy(classOf[ReturnsConnectorImpl])
trait ReturnsConnector {

  def storeDraftReturn(draftReturn: DraftReturn, cgtReference: CgtReference)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, HttpResponse]

  def getDraftReturns(cgtReference: CgtReference)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, HttpResponse]

  def deleteDraftReturns(draftReturnIds: List[UUID])(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, HttpResponse]

  def submitReturn(submitReturnRequest: SubmitReturnRequest)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, HttpResponse]

  def listReturns(
    cgtReference: CgtReference,
    fromDate: LocalDate,
    toDate: LocalDate
  )(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, HttpResponse]

  def displayReturn(cgtReference: CgtReference, submissionId: String)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, HttpResponse]

  def calculateTaxDue(
    request: CalculateCgtTaxDueRequest
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, HttpResponse]

  def taxYear(date: LocalDate)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, HttpResponse]

}

@Singleton
class ReturnsConnectorImpl @Inject() (
  http: HttpClient,
  servicesConfig: ServicesConfig
)(implicit
  ec: ExecutionContext
) extends ReturnsConnector {

  val baseUrl: String = servicesConfig.baseUrl("cgt-property-disposals")

  def getDraftReturnsUrl(cgtReference: CgtReference): String =
    s"$baseUrl/draft-returns/${cgtReference.value}"

  val deleteDraftReturnsUrl: String = s"$baseUrl/draft-returns/delete"

  val submitReturnUrl: String = s"$baseUrl/return"

  val calculateCgtTaxDueUrl: String = s"$baseUrl/calculate-tax-due"

  override def storeDraftReturn(
    draftReturn: DraftReturn,
    cgtReference: CgtReference
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, HttpResponse] = {
    val storeDraftReturnUrl: String =
      s"$baseUrl/draft-return/${cgtReference.value}"

    EitherT[Future, Error, HttpResponse](
      http
        .post(storeDraftReturnUrl, draftReturn)
        .map(Right(_))
        .recover {
          case NonFatal(e) => Left(Error(e))
        }
    )
  }

  override def getDraftReturns(
    cgtReference: CgtReference
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, HttpResponse] =
    EitherT[Future, Error, HttpResponse](
      http
        .get(getDraftReturnsUrl(cgtReference))
        .map(Right(_))
        .recover {
          case NonFatal(e) => Left(Error(e))
        }
    )

  def deleteDraftReturns(
    draftReturnIds: List[UUID]
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, HttpResponse] =
    EitherT[Future, Error, HttpResponse](
      http
        .post(deleteDraftReturnsUrl, DeleteDraftReturnsRequest(draftReturnIds))
        .map(Right(_))
        .recover {
          case NonFatal(e) => Left(Error(e))
        }
    )

  def submitReturn(
    submitReturnRequest: SubmitReturnRequest
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, HttpResponse] =
    EitherT[Future, Error, HttpResponse](
      http
        .post(submitReturnUrl, submitReturnRequest)
        .map(Right(_))
        .recover {
          case NonFatal(e) => Left(Error(e))
        }
    )

  def listReturns(
    cgtReference: CgtReference,
    fromDate: LocalDate,
    toDate: LocalDate
  )(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, HttpResponse] = {
    val url: String =
      s"$baseUrl/returns/${cgtReference.value}/${fromDate.format(dateFormatter)}/${toDate.format(dateFormatter)}"

    EitherT[Future, Error, HttpResponse](
      http
        .get(url)
        .map(Right(_))
        .recover { case e => Left(Error(e)) }
    )
  }

  def displayReturn(cgtReference: CgtReference, submissionId: String)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, HttpResponse] = {
    val url: String = s"$baseUrl/return/${cgtReference.value}/$submissionId"

    EitherT[Future, Error, HttpResponse](
      http
        .get(url)
        .map(Right(_))
        .recover { case e => Left(Error(e)) }
    )
  }

  def calculateTaxDue(
    request: CalculateCgtTaxDueRequest
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, HttpResponse] =
    EitherT[Future, Error, HttpResponse](
      http
        .post(calculateCgtTaxDueUrl, request)
        .map(Right(_))
        .recover {
          case NonFatal(e) => Left(Error(e))
        }
    )

  def taxYear(
    date: LocalDate
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, HttpResponse] =
    EitherT[Future, Error, HttpResponse](
      http
        .get(s"$baseUrl/tax-year/${date.format(dateFormatter)}")
        .map(Right(_))
        .recover { case e => Left(Error(e)) }
    )

  private val dateFormatter: DateTimeFormatter = DateTimeFormatter.ISO_DATE

}

object ReturnsConnector {

  final case class DeleteDraftReturnsRequest(draftReturnIds: List[UUID])

  object DeleteDraftReturnsRequest {

    implicit val format: OFormat[DeleteDraftReturnsRequest] = Json.format

  }

}
