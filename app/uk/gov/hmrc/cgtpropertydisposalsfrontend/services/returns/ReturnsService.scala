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

import java.util.UUID

import cats.data.EitherT
import cats.instances.future._
import cats.instances.int._
import cats.syntax.either._
import cats.syntax.eq._
import com.google.inject.{ImplementedBy, Inject, Singleton}
import play.api.http.Status.OK
import play.api.libs.json.{Json, OFormat}
import play.api.mvc.Request
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.returns.ReturnsConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address.{NonUkAddress, UkAddress}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Country.CountryCode
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.{Address, Postcode}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.{AgentReferenceNumber, CgtReference}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.audit.DraftReturnUpdated
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{ReturnSummary, _}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, TaxYear}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.AuditService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsServiceImpl.{GetDraftReturnResponse, ListReturnsResponse}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.HttpResponseOps._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging.LoggerOps
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[ReturnsServiceImpl])
trait ReturnsService {

  def storeDraftReturn(
    draftReturn: DraftReturn,
    cgtReference: CgtReference,
    agentReferenceNumber: Option[AgentReferenceNumber]
  )(implicit
    hc: HeaderCarrier,
    request: Request[_]
  ): EitherT[Future, Error, Unit]

  def getDraftReturns(
    cgtReference: CgtReference,
    sentReturns: List[ReturnSummary]
  )(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, List[DraftReturn]]

  def submitReturn(submitReturnRequest: SubmitReturnRequest)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, SubmitReturnResponse]

  def listReturns(cgtReference: CgtReference)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, List[ReturnSummary]]

  def displayReturn(cgtReference: CgtReference, submissionId: String)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, CompleteReturn]

}

@Singleton
class ReturnsServiceImpl @Inject() (
  connector: ReturnsConnector,
  auditService: AuditService
)(implicit
  ec: ExecutionContext
) extends ReturnsService
    with Logging {

  def storeDraftReturn(
    draftReturn: DraftReturn,
    cgtReference: CgtReference,
    agentReferenceNumber: Option[AgentReferenceNumber]
  )(implicit
    hc: HeaderCarrier,
    request: Request[_]
  ): EitherT[Future, Error, Unit] =
    connector.storeDraftReturn(draftReturn, cgtReference).subflatMap { httpResponse =>
      if (httpResponse.status === OK) {
        auditService.sendEvent(
          "draftReturnUpdated",
          DraftReturnUpdated(
            draftReturn,
            cgtReference.value,
            agentReferenceNumber.map(_.value)
          ),
          "draft-return-updated"
        )
        Right(())
      } else
        Left(
          Error(
            s"Call to store draft return came back with status ${httpResponse.status}}"
          )
        )
    }

  def deleteDraftReturns(
    draftReturnIds: List[UUID]
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, Unit] =
    connector.deleteDraftReturns(draftReturnIds).subflatMap { httpResponse =>
      if (httpResponse.status === OK)
        Right(())
      else
        Left(
          Error(
            s"Call to delete draft returns came back with status ${httpResponse.status}"
          )
        )
    }

  def getDraftReturns(
    cgtReference: CgtReference,
    sentReturns: List[ReturnSummary]
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, List[DraftReturn]] =
    for {
      httpResponse                          <- connector
                        .getDraftReturns(cgtReference)
                        .subflatMap(r =>
                          if (r.status === OK) Right(r)
                          else
                            Left(
                              Error(
                                s"Call to get draft returns came back with status ${r.status}}"
                              )
                            )
                        )
      draftReturns                          <- EitherT.fromEither(
                        httpResponse
                          .parseJSON[GetDraftReturnResponse]()
                          .leftMap(Error(_))
                          .map(_.draftReturns)
                      )
      (sentDraftReturns, unsentDraftReturns) = draftReturns.partition(hasBeenSent(sentReturns))
      _                                     <- if (sentDraftReturns.nonEmpty)
             EitherT.liftF[Future, Error, Unit](
               deleteSentDraftReturns(sentDraftReturns)
             )
           else EitherT.rightT[Future, Error](())
    } yield unsentDraftReturns

  private def deleteSentDraftReturns(
    sentDraftReturns: List[DraftReturn]
  )(implicit hc: HeaderCarrier): Future[Unit] = {
    val ids = sentDraftReturns.map(_.id)
    logger.info(
      s"Deleting draft returns that have been sent: ${ids.mkString(", ")}"
    )
    deleteDraftReturns(ids).fold(
      e =>
        logger.warn(
          s"Could not delete draft returns with ids [${ids.mkString(" ")}]",
          e
        ),
      _ => logger.info(s"Deleted draft returns with ids [${ids.mkString(" ")}] ")
    )
  }

  private def extractCountryCodeOrPostcode(
    a: Address
  ): Either[CountryCode, Postcode] =
    a match {
      case u: UkAddress    => Right(u.postcode)
      case n: NonUkAddress => Left(n.country.code)
    }

  private def hasBeenSent(
    sentReturns: List[ReturnSummary]
  )(draftReturn: DraftReturn): Boolean = {
    val (draftReturnTaxYear, draftReturnCompletionDate) = draftReturn.fold(
      _.triageAnswers.fold(
        i => i.taxYear -> i.completionDate,
        c => Some(c.taxYear) -> Some(c.completionDate)
      ),
      _.triageAnswers.fold(
        i => i.disposalDate.map(_.taxYear) -> i.completionDate,
        c => Some(c.disposalDate.taxYear) -> Some(c.completionDate)
      ),
      _.triageAnswers.fold(
        i => i.disposalDate.map(_.taxYear) -> i.completionDate,
        c => Some(c.disposalDate.taxYear) -> Some(c.completionDate)
      ),
      _.triageAnswers.fold(
        i => i.disposalDate.map(_.taxYear) -> i.completionDate,
        c => Some(c.disposalDate.taxYear) -> Some(c.completionDate)
      )
    )

    val draftCountryOrPostcode =
      draftReturn.fold[Option[Either[CountryCode, Postcode]]](
        _.examplePropertyDetailsAnswers
          .flatMap(
            _.fold(_.address.map(_.postcode), c => Some(c.address.postcode))
          )
          .map(Right(_)),
        _.propertyAddress.map(a => Right(a.postcode)),
        _.companyAddress.map(extractCountryCodeOrPostcode),
        _.examplePropertyDetailsAnswers
          .flatMap(
            _.fold(_.address.map(_.postcode), c => Some(c.address.postcode))
          )
          .map(Right(_))
      )

    sentReturns.exists { r =>
      draftReturnTaxYear
        .map(_.startDateInclusive.getYear.toString)
        .contains(r.taxYear) &&
      draftReturnCompletionDate.map(_.value).contains(r.completionDate) &&
      draftCountryOrPostcode.contains(
        extractCountryCodeOrPostcode(r.propertyAddress)
      )
    }
  }

  def submitReturn(
    submitReturnRequest: SubmitReturnRequest
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, SubmitReturnResponse] =
    connector.submitReturn(submitReturnRequest).subflatMap { httpResponse =>
      if (httpResponse.status === OK)
        httpResponse
          .parseJSON[SubmitReturnResponse]()
          .leftMap(Error(_))
      else
        Left(
          Error(
            s"Call to get submit return came back with status ${httpResponse.status}}"
          )
        )
    }

  def listReturns(cgtReference: CgtReference)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, List[ReturnSummary]] = {
    val fromDate = TaxYear.thisTaxYearStartDate()
    val toDate   = fromDate.plusYears(1L).minusDays(1L)

    connector.listReturns(cgtReference, fromDate, toDate).subflatMap { response =>
      if (response.status === OK)
        response
          .parseJSON[ListReturnsResponse]()
          .bimap(Error(_), _.returns)
      else
        Left(
          Error(
            s"call to list returns came back with status ${response.status}"
          )
        )
    }
  }

  def displayReturn(cgtReference: CgtReference, submissionId: String)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, CompleteReturn] =
    connector.displayReturn(cgtReference, submissionId).subflatMap { response =>
      if (response.status === OK)
        response.parseJSON[CompleteReturn]().leftMap(Error(_))
      else
        Left(
          Error(
            s"call to list returns came back with status ${response.status}"
          )
        )
    }

}

object ReturnsServiceImpl {

  final case class GetDraftReturnResponse(draftReturns: List[DraftReturn])

  object GetDraftReturnResponse {

    implicit val format: OFormat[GetDraftReturnResponse] =
      Json.format[GetDraftReturnResponse]
  }

  final case class ListReturnsResponse(returns: List[ReturnSummary])

  object ListReturnsResponse {

    implicit val format: OFormat[ListReturnsResponse] = Json.format

  }

}
