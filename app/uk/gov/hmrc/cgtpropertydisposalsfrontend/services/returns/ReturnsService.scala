/*
 * Copyright 2021 HM Revenue & Customs
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
import java.util.UUID

import cats.data.EitherT
import cats.instances.future._
import cats.instances.int._
import cats.syntax.either._
import cats.syntax.order._
import com.google.inject.{ImplementedBy, Inject, Singleton}
import play.api.http.Status.OK
import play.api.i18n.Lang
import play.api.libs.json.{Json, OFormat}
import play.api.mvc.Request
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.ViewConfig
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.returns.ReturnsConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.FillingOutReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.TimeUtils.localDateOrder
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address.{NonUkAddress, UkAddress}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Country.CountryCode
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.{Address, Postcode}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.CgtReference
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.IndividualUserType.{PersonalRepresentative, PersonalRepresentativeInPeriodOfAdmin}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.audit.DraftReturnUpdated
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{ReturnSummary, _}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, TimeUtils}
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
    fillingOutReturn: FillingOutReturn
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

  def submitReturn(submitReturnRequest: SubmitReturnRequest, lang: Lang)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, SubmitReturnResponse]

  def listReturns(cgtReference: CgtReference)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, List[ReturnSummary]]

  def displayReturn(cgtReference: CgtReference, submissionId: String)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, DisplayReturn]

}

@Singleton
class ReturnsServiceImpl @Inject() (
  connector: ReturnsConnector,
  auditService: AuditService,
  viewConfig: ViewConfig
)(implicit
  ec: ExecutionContext
) extends ReturnsService
    with Logging {

  def storeDraftReturn(
    fillingOutReturn: FillingOutReturn
  )(implicit
    hc: HeaderCarrier,
    request: Request[_]
  ): EitherT[Future, Error, Unit] =
    if (fillingOutReturn.isAmendReturn)
      EitherT.pure(())
    else
      connector
        .storeDraftReturn(fillingOutReturn.draftReturn, fillingOutReturn.subscribedDetails.cgtReference)
        .subflatMap { httpResponse =>
          if (httpResponse.status === OK) {
            auditService.sendEvent(
              "draftReturnUpdated",
              DraftReturnUpdated(
                fillingOutReturn.draftReturn,
                fillingOutReturn.subscribedDetails.cgtReference.value,
                fillingOutReturn.agentReferenceNumber.map(_.value)
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
      httpResponse                            <- connector
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
      draftReturns                            <- EitherT.fromEither(
                                                   httpResponse
                                                     .parseJSON[GetDraftReturnResponse]()
                                                     .leftMap(Error(_))
                                                     .map(_.draftReturns)
                                                 )
      (validDraftReturns, invalidDraftReturns) = draftReturns.partition(isValid(_, cgtReference))
      (sentDraftReturns, unsentDraftReturns)   = validDraftReturns.partition(hasBeenSent(sentReturns))
      unsentDraftReturnsTaxYearExchanged       = unsentDraftReturns.map(updateTaxYearExchangedToDraftReturn)

      unsentDraftReturnsWithTaxYearExchangedAndSAStatus =
        unsentDraftReturnsTaxYearExchanged.map(updateSAStatusToDraftReturn)

      toDelete = invalidDraftReturns ::: sentDraftReturns
      _       <- if (toDelete.nonEmpty)
                   EitherT.liftF[Future, Error, Unit](
                     deleteSentOrInvalidDraftReturns(toDelete)
                   )
                 else EitherT.rightT[Future, Error](())
    } yield unsentDraftReturnsWithTaxYearExchangedAndSAStatus

  private def updateTaxYearExchangedToDraftReturn(draftReturn: DraftReturn): DraftReturn =
    draftReturn.fold(
      whenMultiple =>
        whenMultiple.copy(
          triageAnswers = updateTaxYearExchangedToMultipleDisposalsTriageAnswers(whenMultiple.triageAnswers)
        ),
      whenSingle => whenSingle,
      whenSingleIndirect => whenSingleIndirect,
      whenMultipleIndirect =>
        whenMultipleIndirect.copy(
          triageAnswers = updateTaxYearExchangedToMultipleDisposalsTriageAnswers(whenMultipleIndirect.triageAnswers)
        ),
      whenSingleMixedUse => whenSingleMixedUse
    )

  private def updateTaxYearExchangedToMultipleDisposalsTriageAnswers(
    answers: MultipleDisposalsTriageAnswers
  ): MultipleDisposalsTriageAnswers = {
    val taxYear          = answers.fold(_.taxYear, c => Some(c.taxYear))
    val date             = taxYear.map(_.startDateInclusive).getOrElse(TimeUtils.today())
    val taxYearExchanged = TimeUtils.getTaxYearExchangedOfADate(date)

    val actualtaxYearExchanged = answers.fold(_.taxYearExchanged, _.taxYearExchanged)
    if (actualtaxYearExchanged.isDefined)
      answers
    else
      answers.fold[MultipleDisposalsTriageAnswers](
        _.copy(taxYearExchanged = Some(taxYearExchanged)),
        _.copy(taxYearExchanged = Some(taxYearExchanged))
      )
  }

  private def deleteSentOrInvalidDraftReturns(
    sentDraftReturns: List[DraftReturn]
  )(implicit hc: HeaderCarrier): Future[Unit] = {
    val ids = sentDraftReturns.map(_.id)
    logger.info(
      s"Deleting draft returns that have been sent or are invalid: ${ids.mkString(", ")}"
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

  private def updateSAStatusToDraftReturn(draftReturn: DraftReturn): DraftReturn =
    draftReturn.fold(
      whenMultiple =>
        if (isSAStatusUpdatedToMultipleDisposalsTriageAnswers(whenMultiple.triageAnswers))
          whenMultiple
        else
          whenMultiple.copy(triageAnswers = unsetSAStatusToMultipleDisposalsTriageAnswers(whenMultiple.triageAnswers)),
      whenSingle =>
        if (isSAStatusUpdatedToSingleDisposalTriageAnswers(whenSingle.triageAnswers))
          whenSingle
        else
          whenSingle.copy(triageAnswers = unsetSAStatusToSingleDisposalTriageAnswers(whenSingle.triageAnswers)),
      whenSingleIndirect =>
        if (isSAStatusUpdatedToSingleDisposalTriageAnswers(whenSingleIndirect.triageAnswers))
          whenSingleIndirect
        else
          whenSingleIndirect.copy(triageAnswers =
            unsetSAStatusToSingleDisposalTriageAnswers(whenSingleIndirect.triageAnswers)
          ),
      whenMultipleIndirect =>
        if (isSAStatusUpdatedToMultipleDisposalsTriageAnswers(whenMultipleIndirect.triageAnswers))
          whenMultipleIndirect
        else
          whenMultipleIndirect.copy(triageAnswers =
            unsetSAStatusToMultipleDisposalsTriageAnswers(whenMultipleIndirect.triageAnswers)
          ),
      whenSingleMixedUse =>
        if (isSAStatusUpdatedToSingleDisposalTriageAnswers(whenSingleMixedUse.triageAnswers))
          whenSingleMixedUse
        else
          whenSingleMixedUse.copy(triageAnswers =
            unsetSAStatusToSingleDisposalTriageAnswers(whenSingleMixedUse.triageAnswers)
          )
    )

  private def unsetSAStatusToMultipleDisposalsTriageAnswers(
    answers: MultipleDisposalsTriageAnswers
  ): MultipleDisposalsTriageAnswers =
    answers.fold(i => i, c => c.unset(_.alreadySentSelfAssessment))

  private def unsetSAStatusToSingleDisposalTriageAnswers(
    answers: SingleDisposalTriageAnswers
  ): SingleDisposalTriageAnswers =
    answers.fold(i => i, c => c.unset(_.alreadySentSelfAssessment))

  private def isSAStatusUpdatedToMultipleDisposalsTriageAnswers(answers: MultipleDisposalsTriageAnswers): Boolean =
    answers.fold(_.alreadySentSelfAssessment.isDefined, _.alreadySentSelfAssessment.isDefined) ||
      answers
        .fold(
          _.taxYear.map(_.isItInLatestTaxYear(viewConfig.enableFutureDates)),
          c => Some(c.taxYear.isItInLatestTaxYear(viewConfig.enableFutureDates))
        )
        .contains(true)

  private def isSAStatusUpdatedToSingleDisposalTriageAnswers(answers: SingleDisposalTriageAnswers): Boolean =
    answers.fold(_.alreadySentSelfAssessment.isDefined, _.alreadySentSelfAssessment.isDefined) ||
      answers
        .fold(
          _.disposalDate.map(_.taxYear.isItInLatestTaxYear(viewConfig.enableFutureDates)),
          c => Some(c.disposalDate.taxYear.isItInLatestTaxYear(viewConfig.enableFutureDates))
        )
        .contains(true)

  private def isValid(draftReturn: DraftReturn, cgtReference: CgtReference): Boolean = {
    val dateOfDeath =
      draftReturn.representeeAnswers.flatMap(_.fold(_.dateOfDeath, _.dateOfDeath))

    val disposalDate = {
      def fromTriageAnswers(s: SingleDisposalTriageAnswers) =
        s.fold(_.disposalDate.map(_.value), c => Some(c.disposalDate.value))

      draftReturn.fold(
        _.examplePropertyDetailsAnswers.flatMap(_.fold(_.disposalDate.map(_.value), c => Some(c.disposalDate.value))),
        single => fromTriageAnswers(single.triageAnswers),
        singleIndirect => fromTriageAnswers(singleIndirect.triageAnswers),
        _.triageAnswers.fold(_.completionDate.map(_.value), c => Some(c.completionDate.value)),
        singleMixedUse => fromTriageAnswers(singleMixedUse.triageAnswers)
      )
    }

    val periodOfAdminDateOfDeathValid =
      if (draftReturn.representativeType().contains(PersonalRepresentativeInPeriodOfAdmin))
        dateOfDeath.forall(death => disposalDate.forall(_ > death.value))
      else
        true

    val nonPeriodOfAdminDateOfDeathValid =
      if (draftReturn.representativeType().contains(PersonalRepresentative))
        dateOfDeath.forall(death => disposalDate.forall(_ <= death.value))
      else true

    if (!periodOfAdminDateOfDeathValid)
      logger.warn(
        s"Found draft return for cgt reference ${cgtReference.value} for period of admin personal rep with invalid disposal or completion date"
      )
    if (!nonPeriodOfAdminDateOfDeathValid)
      logger.warn(
        s"Found draft return for cgt reference $cgtReference non-period of admin personal rep with invalid disposal or completion date"
      )

    periodOfAdminDateOfDeathValid && nonPeriodOfAdminDateOfDeathValid
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
    val (draftReturnTaxYear, draftReturnCompletionDate) =
      draftReturn
        .triageAnswers()
        .fold(
          _.fold(
            i => i.taxYear -> i.completionDate,
            c => Some(c.taxYear) -> Some(c.completionDate)
          ),
          _.fold(
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
        _.exampleCompanyDetailsAnswers
          .flatMap(
            _.fold(
              _.address.map(extractCountryCodeOrPostcode),
              c => Some(extractCountryCodeOrPostcode(c.address))
            )
          ),
        _.mixedUsePropertyDetailsAnswers
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
      draftCountryOrPostcode
        .map(toUpperCaseWithoutSpaces)
        .contains(
          toUpperCaseWithoutSpaces(extractCountryCodeOrPostcode(r.propertyAddress))
        )
    }
  }

  private def toUpperCaseWithoutSpaces(
    countryOrPostcode: Either[CountryCode, Postcode]
  ): Either[CountryCode, Postcode] = {
    def format(s: String): CountryCode = s.replaceAllLiterally(" ", "").toUpperCase()
    countryOrPostcode.bimap(format, p => Postcode(format(p.value)))
  }

  def submitReturn(
    submitReturnRequest: SubmitReturnRequest,
    lang: Lang
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, SubmitReturnResponse] =
    connector.submitReturn(submitReturnRequest, lang).subflatMap { httpResponse =>
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
    val fromDate = LocalDate.of(2020, 4, 6)
    val toDate   = fromDate.plusYears(viewConfig.numberOfTaxYearsForReturns).minusDays(1L)

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
  ): EitherT[Future, Error, DisplayReturn] =
    connector.displayReturn(cgtReference, submissionId).subflatMap { response =>
      if (response.status === OK)
        response.parseJSON[DisplayReturn]().leftMap(Error(_))
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
