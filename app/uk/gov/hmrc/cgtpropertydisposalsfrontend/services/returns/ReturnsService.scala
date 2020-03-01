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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns

import java.time.LocalDate

import cats.data.EitherT
import cats.instances.future._
import cats.instances.int._
import cats.syntax.either._
import cats.syntax.eq._
import com.google.inject.{ImplementedBy, Inject, Singleton}
import play.api.http.Status.OK
import play.api.libs.json.{JsValue, Json, OFormat}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.returns.ReturnsConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address.UkAddress
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.{AmountInPence, Charge, ChargeWithPayments, FinancialTransaction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.CgtReference
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{ReturnSummary, _}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, TaxYear}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsServiceImpl.{GetDraftReturnResponse, ListReturnsResponse, ReturnSummaryWithoutPaymentInfo}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.HttpResponseOps._
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[ReturnsServiceImpl])
trait ReturnsService {

  def storeDraftReturn(draftReturn: DraftReturn)(implicit hc: HeaderCarrier): EitherT[Future, Error, Unit]

  def getDraftReturns(cgtReference: CgtReference)(implicit hc: HeaderCarrier): EitherT[Future, Error, List[DraftReturn]]

  def submitReturn(submitReturnRequest: SubmitReturnRequest)(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, SubmitReturnResponse]

  def listReturns(cgtReference: CgtReference)(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, List[ReturnSummary]]

  // TODO: convert response to complete return
  def displayReturn(cgtReference: CgtReference, submissionId: String)(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, JsValue]

}

@Singleton
class ReturnsServiceImpl @Inject() (connector: ReturnsConnector, financialDataService: FinancialDataService)(
  implicit ec: ExecutionContext
) extends ReturnsService {

  def storeDraftReturn(draftReturn: DraftReturn)(implicit hc: HeaderCarrier): EitherT[Future, Error, Unit] =
    connector.storeDraftReturn(draftReturn).subflatMap { httpResponse =>
      if (httpResponse.status === OK) {
        Right(())
      } else {
        Left(Error(s"Call to store draft return came back with status ${httpResponse.status}}"))
      }
    }

  def getDraftReturns(
    cgtReference: CgtReference
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, List[DraftReturn]] =
    connector.getDraftReturns(cgtReference).subflatMap { httpResponse =>
      if (httpResponse.status === OK) {
        httpResponse
          .parseJSON[GetDraftReturnResponse]()
          .leftMap(Error(_))
          .map(_.draftReturns)
      } else {
        Left(Error(s"Call to get draft returns came back with status ${httpResponse.status}}"))
      }
    }

  def submitReturn(
    submitReturnRequest: SubmitReturnRequest
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, SubmitReturnResponse] =
    connector.submitReturn(submitReturnRequest).subflatMap { httpResponse =>
      if (httpResponse.status === OK) {
        httpResponse
          .parseJSON[SubmitReturnResponse]()
          .leftMap(Error(_))
      } else {
        Left(Error(s"Call to get submit return came back with status ${httpResponse.status}}"))
      }
    }

  def listReturns(cgtReference: CgtReference)(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, List[ReturnSummary]] = {
    lazy val returns = {
      val fromDate = TaxYear.thisTaxYearStartDate()
      val toDate   = fromDate.plusYears(1L).minusDays(1L)

      connector.listReturns(cgtReference, fromDate, toDate).subflatMap { response =>
        if (response.status === OK) {
          response
            .parseJSON[ListReturnsResponse]()
            .bimap(Error(_), _.returns)
        } else {
          Left(Error(s"call to list returns came back with status ${response.status}"))
        }
      }
    }

    for {
      returns                <- returns
      financialDate          <- financialDataService.getFinancialData(cgtReference)
      returnsWithPaymentInfo <- EitherT.fromEither[Future](returnsWithPaymentInfo(returns, financialDate))
    } yield returnsWithPaymentInfo
  }

  def displayReturn(cgtReference: CgtReference, submissionId: String)(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, JsValue] =
    connector.displayReturn(cgtReference, submissionId).subflatMap { response =>
      if (response.status === OK) {
        response.parseJSON[JsValue]().leftMap(Error(_))
      } else {
        Left(Error(s"call to list returns came back with status ${response.status}"))
      }
    }

  private def returnsWithPaymentInfo(
    returns: List[ReturnSummaryWithoutPaymentInfo],
    financialTransactions: List[FinancialTransaction]
  ): Either[Error, List[ReturnSummary]] = {
    val chargeReferenceToFinancialData = financialTransactions.map(t => t.chargeReference -> t).toMap

    val (returnSummaries, errors) = returns.foldLeft(List.empty[ReturnSummary] -> List.empty[String]) {
      case ((acc, errors), returnSummaryWithoutPaymentInfo) =>
        toReturnSummary(returnSummaryWithoutPaymentInfo, chargeReferenceToFinancialData)
          .bimap(
            e => acc        -> (e ::: errors),
            r => (r :: acc) -> errors
          )
          .merge
    }

    if (errors.nonEmpty)
      Left(Error(s"Could not construct return summary data with payment information: [${errors.mkString("; ")}]"))
    else
      Right(returnSummaries)

  }

  private def toReturnSummary(
    returnSummaryWithoutPaymentInfo: ReturnSummaryWithoutPaymentInfo,
    chargeReferenceToFinancialTransactions: Map[String, FinancialTransaction]
  ): Either[List[String], ReturnSummary] = {
    val (chargesWithPaymentInfo, paymentInfoErrors) =
      returnSummaryWithoutPaymentInfo.charges.foldLeft(List.empty[ChargeWithPayments] -> List.empty[String]) {
        case ((acc, errors), charge) =>
          val chargeWithPaymentInfo = chargeReferenceToFinancialTransactions
            .get(charge.chargeReference)
            .fold[Either[String, ChargeWithPayments]](
              Left(
                s"Could not find payment information for charge in return summary with charge reference ${charge.chargeReference}"
              )
            )(t =>
              Right(
                ChargeWithPayments(
                  charge.chargeDescription,
                  charge.chargeReference,
                  charge.amount,
                  charge.dueDate,
                  t.payments
                )
              )
            )
          chargeWithPaymentInfo
            .bimap(
              error => acc    -> (error :: errors),
              c => (c :: acc) -> errors
            )
            .merge
      }

    if (paymentInfoErrors.nonEmpty)
      Left(paymentInfoErrors)
    else
      Right(
        ReturnSummary(
          returnSummaryWithoutPaymentInfo.submissionId,
          returnSummaryWithoutPaymentInfo.submissionDate,
          returnSummaryWithoutPaymentInfo.completionDate,
          returnSummaryWithoutPaymentInfo.lastUpdatedDate,
          returnSummaryWithoutPaymentInfo.totalCGTLiability,
          returnSummaryWithoutPaymentInfo.totalOutstanding,
          returnSummaryWithoutPaymentInfo.propertyAddress,
          chargesWithPaymentInfo
        )
      )

  }

}

object ReturnsServiceImpl {

  final case class GetDraftReturnResponse(draftReturns: List[DraftReturn])

  object GetDraftReturnResponse {

    implicit val format: OFormat[GetDraftReturnResponse] = Json.format[GetDraftReturnResponse]
  }

  final case class ListReturnsResponse(returns: List[ReturnSummaryWithoutPaymentInfo])

  object ListReturnsResponse {

    implicit val format: OFormat[ListReturnsResponse] = Json.format

  }

  final case class ReturnSummaryWithoutPaymentInfo(
    submissionId: String,
    submissionDate: LocalDate,
    completionDate: LocalDate,
    lastUpdatedDate: Option[LocalDate],
    totalCGTLiability: AmountInPence,
    totalOutstanding: AmountInPence,
    propertyAddress: UkAddress,
    charges: List[Charge]
  )

  implicit val ukAddressFormat: OFormat[UkAddress] = Json.format

  implicit val returnSummaryWithoutPaymentInfoFormat: OFormat[ReturnSummaryWithoutPaymentInfo] = Json.format

}
