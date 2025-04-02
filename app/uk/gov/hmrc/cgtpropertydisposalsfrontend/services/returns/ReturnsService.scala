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
import cats.instances.string._
import cats.syntax.either._
import cats.syntax.order._
import com.google.inject.{ImplementedBy, Inject, Singleton}
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
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.audit.DraftReturnUpdated
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, TaxYear, TimeUtils}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.AuditService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging.LoggerOps
import uk.gov.hmrc.http.HeaderCarrier

import java.time.LocalDate
import java.util.{Locale, UUID}
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

  def updateCorrectTaxYearToSentReturns(
    cgtReference: CgtReference,
    returnSummary: List[ReturnSummary]
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, (Boolean, List[ReturnSummary])]

  def unsetUnwantedSectionsToDraftReturn(draftReturn: DraftReturn): DraftReturn

  def updateSAStatusToSentReturn(submissionDate: LocalDate, sentReturn: DisplayReturn): DisplayReturn

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
    if (fillingOutReturn.isAmendReturn) {
      EitherT.pure(())
    } else {
      for {
        _ <- connector.storeDraftReturn(fillingOutReturn.draftReturn, fillingOutReturn.subscribedDetails.cgtReference)
      } yield auditService.sendEvent(
        "draftReturnUpdated",
        DraftReturnUpdated(
          fillingOutReturn.draftReturn,
          fillingOutReturn.subscribedDetails.cgtReference.value,
          fillingOutReturn.agentReferenceNumber.map(_.value)
        ),
        "draft-return-updated"
      )
    }

  def deleteDraftReturns(draftReturnIds: List[UUID])(implicit hc: HeaderCarrier): EitherT[Future, Error, Unit] =
    connector.deleteDraftReturns(draftReturnIds)

  def getDraftReturns(
    cgtReference: CgtReference,
    sentReturns: List[ReturnSummary]
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, List[DraftReturn]] =
    for {
      draftReturns                            <- connector.getDraftReturns(cgtReference).map(_.draftReturns)
      (validDraftReturns, invalidDraftReturns) = draftReturns.partition(isValid(_, cgtReference))
      (sentDraftReturns, unsentDraftReturns)   = validDraftReturns.partition(hasBeenSent(sentReturns))
      unsentDraftReturnsTaxYearExchanged       = unsentDraftReturns.map { draftReturn =>
                                                   updateTaxYearExchangedToDraftReturn(draftReturn)
                                                 }

      unsentDraftReturnsWithTaxYearExchangedAndSAStatus =
        unsentDraftReturnsTaxYearExchanged.map(updateSAStatusToDraftReturn)

      toDelete = invalidDraftReturns ::: sentDraftReturns
      _       <- if (toDelete.nonEmpty) {
                   EitherT.liftF[Future, Error, Unit](
                     deleteSentOrInvalidDraftReturns(toDelete)
                   )
                 } else {
                   EitherT.rightT[Future, Error](())
                 }
    } yield unsentDraftReturnsWithTaxYearExchangedAndSAStatus

  def updateCorrectTaxYearToSentReturns(
    cgtReference: CgtReference,
    returnSummary: List[ReturnSummary]
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, (Boolean, List[ReturnSummary])] = {
    import cats.instances.list._
    import cats.syntax.traverse._
    for {
      result              <- returnSummary
                               .map(r => updateCorrectTaxYearToSentReturn(r, cgtReference))
                               .sequence
      usentDraftReturnFlag = result.exists(_._1)
      updatedSentReturns   = result.map(_._2)
    } yield (usentDraftReturnFlag, updatedSentReturns)
  }

  private def updateCorrectTaxYearToSentReturn(
    returnSummary: ReturnSummary,
    cgtReference: CgtReference
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, (Boolean, ReturnSummary)] =
    for {
      return_ <- displayReturn(cgtReference, returnSummary.submissionId)
    } yield {
      val expectedTaxYear =
        return_.completeReturn
          .fold(
            _.triageAnswers.taxYear,
            _.triageAnswers.disposalDate.taxYear,
            _.triageAnswers.disposalDate.taxYear,
            _.triageAnswers.taxYear,
            _.triageAnswers.disposalDate.taxYear
          )
          .startDateInclusive
          .getYear
          .toString
      val actualTaxYear   = returnSummary.taxYear
      if (expectedTaxYear === actualTaxYear) {
        (false, returnSummary)
      } else {
        (true, returnSummary.copy(taxYear = expectedTaxYear))
      }
    }

  def unsetUnwantedSectionsToDraftReturn(draftReturn: DraftReturn): DraftReturn =
    draftReturn.fold(
      _.copy(
        gainOrLossAfterReliefs = None,
        exemptionAndLossesAnswers = None,
        yearToDateLiabilityAnswers = None,
        supportingEvidenceAnswers = None
      ),
      _.copy(
        gainOrLossAfterReliefs = None,
        exemptionAndLossesAnswers = None,
        yearToDateLiabilityAnswers = None,
        supportingEvidenceAnswers = None
      ),
      _.copy(
        gainOrLossAfterReliefs = None,
        exemptionAndLossesAnswers = None,
        yearToDateLiabilityAnswers = None,
        supportingEvidenceAnswers = None
      ),
      _.copy(
        gainOrLossAfterReliefs = None,
        exemptionAndLossesAnswers = None,
        yearToDateLiabilityAnswers = None,
        supportingEvidenceAnswers = None
      ),
      _.copy(
        gainOrLossAfterReliefs = None,
        exemptionAndLossesAnswers = None,
        yearToDateLiabilityAnswers = None,
        supportingEvidenceAnswers = None
      )
    )

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
          triageAnswers = updateTaxYearExchangedToMultipleDisposalsTriageAnswers(
            whenMultipleIndirect.triageAnswers
          )
        ),
      whenSingleMixedUse => whenSingleMixedUse
    )

  private def updateTaxYearExchangedToMultipleDisposalsTriageAnswers(
    answers: MultipleDisposalsTriageAnswers
  ): MultipleDisposalsTriageAnswers = {
    val actualTaxYearExchanged = answers.fold(_.taxYearExchanged, _.taxYearExchanged)
    if (actualTaxYearExchanged.isDefined) {
      answers
    } else {
      val taxYear = answers.fold(_.taxYear, c => Some(c.taxYear))
      taxYear match {
        case Some(t) =>
          val taxYearExchanged = TimeUtils.getTaxYearExchangedOfADate(t.startDateInclusive)
          answers.fold[MultipleDisposalsTriageAnswers](
            _.copy(taxYearExchanged = Some(taxYearExchanged)),
            _.copy(taxYearExchanged = Some(taxYearExchanged))
          )
        case None    => answers
      }
    }
  }

  private def deleteSentOrInvalidDraftReturns(
    sentDraftReturns: List[DraftReturn]
  )(implicit hc: HeaderCarrier): Future[Unit] = {
    val ids = sentDraftReturns.map(_.id)
    logger.info(
      s"Deleting draft returns that have been sent or are invalid: ${ids.mkString(", ")}"
    )
    deleteDraftReturns(ids).fold(
      e => logger.warn(s"Could not delete draft returns with ids [${ids.mkString(" ")}]", e),
      _ => logger.info(s"Deleted draft returns with ids [${ids.mkString(" ")}] ")
    )
  }

  def updateSAStatusToSentReturn(submissionDate: LocalDate, sentReturn: DisplayReturn): DisplayReturn =
    sentReturn.completeReturn.fold(
      whenMultiple =>
        if (
          isSAStatusRequiredToUpdateSentMultipleDisposalsTriageAnswers(whenMultiple.triageAnswers, submissionDate)
            .contains(true)
        ) {
          sentReturn.copy(
            completeReturn = whenMultiple.copy(
              triageAnswers = whenMultiple.triageAnswers.copy(alreadySentSelfAssessment = Some(false))
            )
          )
        } else {
          sentReturn
        },
      whenSingle =>
        if (
          isSAStatusRequiredToUpdateSentSingleDisposalTriageAnswers(whenSingle.triageAnswers, submissionDate)
            .contains(true)
        ) {
          sentReturn.copy(
            completeReturn = whenSingle.copy(
              triageAnswers = whenSingle.triageAnswers.copy(alreadySentSelfAssessment = Some(false))
            )
          )
        } else {
          sentReturn
        },
      whenSingleIndirect =>
        if (
          isSAStatusRequiredToUpdateSentSingleDisposalTriageAnswers(whenSingleIndirect.triageAnswers, submissionDate)
            .contains(true)
        ) {
          sentReturn.copy(
            completeReturn = whenSingleIndirect.copy(
              triageAnswers = whenSingleIndirect.triageAnswers.copy(alreadySentSelfAssessment = Some(false))
            )
          )
        } else {
          sentReturn
        },
      whenMultipleIndirect =>
        if (
          isSAStatusRequiredToUpdateSentMultipleDisposalsTriageAnswers(
            whenMultipleIndirect.triageAnswers,
            submissionDate
          ).contains(true)
        ) {
          sentReturn.copy(
            completeReturn = whenMultipleIndirect.copy(
              triageAnswers = whenMultipleIndirect.triageAnswers.copy(alreadySentSelfAssessment = Some(false))
            )
          )
        } else {
          sentReturn
        },
      whenSingleMixedUse =>
        if (
          isSAStatusRequiredToUpdateSentSingleDisposalTriageAnswers(whenSingleMixedUse.triageAnswers, submissionDate)
            .contains(true)
        ) {
          sentReturn.copy(
            completeReturn = whenSingleMixedUse.copy(
              triageAnswers = whenSingleMixedUse.triageAnswers.copy(alreadySentSelfAssessment = Some(false))
            )
          )
        } else {
          sentReturn
        }
    )

  private def isSAStatusRequiredToUpdateSentMultipleDisposalsTriageAnswers(
    answers: MultipleDisposalsTriageAnswers,
    submissionDate: LocalDate
  ): Option[Boolean] = {
    val taxYear = answers.fold(_.taxYear, c => Some(c.taxYear))
    isSAStatusRequiredToUpdateSentReturn(taxYear, submissionDate)
  }

  private def isSAStatusRequiredToUpdateSentSingleDisposalTriageAnswers(
    answers: SingleDisposalTriageAnswers,
    submissionDate: LocalDate
  ): Option[Boolean] = {
    val taxYear = answers.fold(_.disposalDate, c => Some(c.disposalDate)).map(_.taxYear)
    isSAStatusRequiredToUpdateSentReturn(taxYear, submissionDate)
  }

  private def isSAStatusRequiredToUpdateSentReturn(
    taxYear: Option[TaxYear],
    submissionDate: LocalDate
  ): Option[Boolean] = {
    val disposalDateTaxYearStartYear   = taxYear.map(_.startDateInclusive.getYear)
    val todaysTaxYearStartYear         = TimeUtils.taxYearStart(TimeUtils.today()).getYear
    val submissionDateTaxYearStartYear = TimeUtils.taxYearStart(submissionDate).getYear

    disposalDateTaxYearStartYear.map(startYear =>
      startYear < submissionDateTaxYearStartYear && startYear < todaysTaxYearStartYear
    )
  }

  private def updateSAStatusToDraftReturn(draftReturn: DraftReturn): DraftReturn =
    draftReturn.fold(
      whenMultiple =>
        if (isSAStatusUpdatedToMultipleDisposalsTriageAnswers(whenMultiple.triageAnswers)) {
          whenMultiple
        } else {
          whenMultiple.copy(triageAnswers = unsetSAStatusToMultipleDisposalsTriageAnswers(whenMultiple.triageAnswers))
        },
      whenSingle =>
        if (isSAStatusUpdatedToSingleDisposalTriageAnswers(whenSingle.triageAnswers)) {
          whenSingle
        } else {
          whenSingle.copy(triageAnswers = unsetSAStatusToSingleDisposalTriageAnswers(whenSingle.triageAnswers))
        },
      whenSingleIndirect =>
        if (isSAStatusUpdatedToSingleDisposalTriageAnswers(whenSingleIndirect.triageAnswers)) {
          whenSingleIndirect
        } else {
          whenSingleIndirect.copy(triageAnswers =
            unsetSAStatusToSingleDisposalTriageAnswers(whenSingleIndirect.triageAnswers)
          )
        },
      whenMultipleIndirect =>
        if (isSAStatusUpdatedToMultipleDisposalsTriageAnswers(whenMultipleIndirect.triageAnswers)) {
          whenMultipleIndirect
        } else {
          whenMultipleIndirect.copy(triageAnswers =
            unsetSAStatusToMultipleDisposalsTriageAnswers(whenMultipleIndirect.triageAnswers)
          )
        },
      whenSingleMixedUse =>
        if (isSAStatusUpdatedToSingleDisposalTriageAnswers(whenSingleMixedUse.triageAnswers)) {
          whenSingleMixedUse
        } else {
          whenSingleMixedUse.copy(triageAnswers =
            unsetSAStatusToSingleDisposalTriageAnswers(whenSingleMixedUse.triageAnswers)
          )
        }
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
          _.taxYear.map(_.isItInLatestTaxYear(viewConfig.futureDatesEnabled)),
          c => Some(c.taxYear.isItInLatestTaxYear(viewConfig.futureDatesEnabled))
        )
        .contains(true)

  private def isSAStatusUpdatedToSingleDisposalTriageAnswers(answers: SingleDisposalTriageAnswers): Boolean =
    answers.fold(_.alreadySentSelfAssessment.isDefined, _.alreadySentSelfAssessment.isDefined) ||
      answers
        .fold(
          _.disposalDate.map(_.taxYear.isItInLatestTaxYear(viewConfig.futureDatesEnabled)),
          c => Some(c.disposalDate.taxYear.isItInLatestTaxYear(viewConfig.futureDatesEnabled))
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
      if (draftReturn.representativeType().contains(PersonalRepresentativeInPeriodOfAdmin)) {
        dateOfDeath.forall(death => disposalDate.forall(_ > death.value))
      } else {
        true
      }

    val nonPeriodOfAdminDateOfDeathValid =
      if (draftReturn.representativeType().contains(PersonalRepresentative)) {
        dateOfDeath.forall(death => disposalDate.forall(_ <= death.value))
      } else {
        true
      }

    if (!periodOfAdminDateOfDeathValid) {
      logger.warn(
        s"Found draft return for cgt reference ${cgtReference.value} for period of admin personal rep with invalid disposal or completion date"
      )
    }
    if (!nonPeriodOfAdminDateOfDeathValid) {
      logger.warn(
        s"Found draft return for cgt reference $cgtReference non-period of admin personal rep with invalid disposal or completion date"
      )
    }

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
    def format(s: String): CountryCode = s.replace(" ", "").toUpperCase(Locale.UK)
    countryOrPostcode.bimap(format, p => Postcode(format(p.value)))
  }

  def submitReturn(submitReturnRequest: SubmitReturnRequest, lang: Lang)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, SubmitReturnResponse] = connector.submitReturn(submitReturnRequest, lang)

  def listReturns(cgtReference: CgtReference)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, List[ReturnSummary]] = {
    val today    = LocalDate.now()
    val fromDate = TimeUtils.getTaxYearStartDate(TaxYearExchanged.cutoffTaxYear)
    // ETMP build queries from our date range in tax year batches, they have logic to reject an
    // end date that is before the end of the tax year that the toDate falls within, so we need to
    // satisfy this logic by using the end of the current tax year as our toDate
    val toDate   = TimeUtils.getTaxYearEndDateInclusive(today)
    connector.listReturns(cgtReference, fromDate, toDate).map(_.returns)
  }

  def displayReturn(cgtReference: CgtReference, submissionId: String)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, DisplayReturn] = connector.displayReturn(cgtReference, submissionId)
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
