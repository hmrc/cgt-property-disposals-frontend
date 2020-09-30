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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.services

import cats.data.EitherT
import cats.instances.future._
import cats.instances.string._
import cats.syntax.either._
import cats.syntax.eq._
import com.google.inject.{ImplementedBy, Inject, Singleton}
import play.api.Configuration
import play.api.libs.json.{Reads, Writes}
import play.api.mvc.Request
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.EitherUtils.EitherOps
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{NameMatchServiceError, UnsuccessfulNameMatchAttempts}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.{CgtReference, GGCredId}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.{IndividualName, TrustName}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.SubscribedDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.audit.{BusinessPartnerRecordNameMatchAttemptEvent, BusinessPartnerRecordNameMatchAuditDetails, NameMatchAccountLocked}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.bpr.BusinessPartnerRecordRequest.{IndividualBusinessPartnerRecordRequest, TrustBusinessPartnerRecordRequest}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.UnsuccessfulNameMatchAttempts.NameMatchDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.UnsuccessfulNameMatchAttempts.NameMatchDetails.{IndividualRepresenteeNameMatchDetails, IndividualSautrNameMatchDetails, TrustNameMatchDetails}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.bpr._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.RepresenteeReferenceId
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.RepresenteeReferenceId.{NoReferenceId, RepresenteeCgtReference, RepresenteeNino, RepresenteeSautr}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.audit.CgtAccountNameMatchAttemptEvent
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.NameMatchRetryStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.onboarding.{BusinessPartnerRecordService, SubscriptionService}
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[NameMatchRetryServiceImpl])
trait NameMatchRetryService {

  def getNumberOfUnsuccessfulAttempts[A <: NameMatchDetails : Reads](
    ggCredId: GGCredId
  )(implicit
    hc: HeaderCarrier,
    request: Request[_]
  ): EitherT[Future, NameMatchServiceError[A], Option[
    UnsuccessfulNameMatchAttempts[A]
  ]]

  def attemptBusinessPartnerRecordNameMatch(
    nameMatchDetails: IndividualSautrNameMatchDetails,
    ggCredId: GGCredId,
    previousUnsuccessfulNameMatchAttempts: Option[
      UnsuccessfulNameMatchAttempts[IndividualSautrNameMatchDetails]
    ]
  )(implicit
    hc: HeaderCarrier,
    request: Request[_]
  ): EitherT[Future, NameMatchServiceError[
    IndividualSautrNameMatchDetails
  ], (BusinessPartnerRecord, Option[CgtReference])]

  def attemptBusinessPartnerRecordNameMatch(
    nameMatchDetails: TrustNameMatchDetails,
    ggCredId: GGCredId,
    previousUnsuccessfulNameMatchAttempts: Option[
      UnsuccessfulNameMatchAttempts[TrustNameMatchDetails]
    ]
  )(implicit
    hc: HeaderCarrier,
    request: Request[_]
  ): EitherT[Future, NameMatchServiceError[
    TrustNameMatchDetails
  ], (BusinessPartnerRecord, Option[CgtReference])]

  def attemptNameMatch(
    nameMatchDetails: IndividualRepresenteeNameMatchDetails,
    ggCredId: GGCredId,
    previousUnsuccessfulNameMatchAttempts: Option[
      UnsuccessfulNameMatchAttempts[IndividualRepresenteeNameMatchDetails]
    ]
  )(implicit
    hc: HeaderCarrier,
    request: Request[_]
  ): EitherT[Future, NameMatchServiceError[
    IndividualRepresenteeNameMatchDetails
  ], RepresenteeReferenceId]

}

@Singleton
class NameMatchRetryServiceImpl @Inject() (
  bprService: BusinessPartnerRecordService,
  nameMatchRetryStore: NameMatchRetryStore,
  subscriptionService: SubscriptionService,
  auditService: AuditService,
  config: Configuration
)(implicit ec: ExecutionContext)
    extends NameMatchRetryService {

  val maxUnsuccessfulAttempts: Int =
    config.underlying.getInt("bpr-name-match.max-retries")

  def getNumberOfUnsuccessfulAttempts[A <: NameMatchDetails : Reads](
    ggCredId: GGCredId
  )(implicit
    hc: HeaderCarrier,
    request: Request[_]
  ): EitherT[Future, NameMatchServiceError[A], Option[
    UnsuccessfulNameMatchAttempts[A]
  ]] =
    EitherT(nameMatchRetryStore.get(ggCredId))
      .leftMap(NameMatchServiceError.BackendError)
      .subflatMap {
        _.fold[Either[NameMatchServiceError[A], Option[
          UnsuccessfulNameMatchAttempts[A]
        ]]](Right(None))((unsuccessfulAttempts: UnsuccessfulNameMatchAttempts[A]) =>
          if (unsuccessfulAttempts.unsuccessfulAttempts >= maxUnsuccessfulAttempts) {
            auditTooManyAttempts(unsuccessfulAttempts)
            Left(NameMatchServiceError.TooManyUnsuccessfulAttempts())
          } else
            Right(Some(unsuccessfulAttempts))
        )
      }

  def attemptBusinessPartnerRecordNameMatch(
    nameMatchDetails: IndividualSautrNameMatchDetails,
    ggCredId: GGCredId,
    previousUnsuccessfulNameMatchAttempts: Option[
      UnsuccessfulNameMatchAttempts[IndividualSautrNameMatchDetails]
    ]
  )(implicit
    hc: HeaderCarrier,
    request: Request[_]
  ): EitherT[Future, NameMatchServiceError[
    IndividualSautrNameMatchDetails
  ], (BusinessPartnerRecord, Option[CgtReference])] =
    withValidPreviousUnsuccessfulNameMatchAttempts(
      previousUnsuccessfulNameMatchAttempts,
      nameMatchDetails
    ) {
      handleBprNameMatch(
        IndividualBusinessPartnerRecordRequest(
          Left(nameMatchDetails.sautr),
          Some(nameMatchDetails.name)
        ),
        nameMatchDetails,
        ggCredId,
        _
      )
    }

  def attemptBusinessPartnerRecordNameMatch(
    nameMatchDetails: TrustNameMatchDetails,
    ggCredId: GGCredId,
    previousUnsuccessfulNameMatchAttempts: Option[
      UnsuccessfulNameMatchAttempts[TrustNameMatchDetails]
    ]
  )(implicit
    hc: HeaderCarrier,
    request: Request[_]
  ): EitherT[Future, NameMatchServiceError[
    TrustNameMatchDetails
  ], (BusinessPartnerRecord, Option[CgtReference])] =
    withValidPreviousUnsuccessfulNameMatchAttempts(
      previousUnsuccessfulNameMatchAttempts,
      nameMatchDetails
    ) {
      handleBprNameMatch(
        TrustBusinessPartnerRecordRequest(
          Left(nameMatchDetails.trn),
          Some(nameMatchDetails.name)
        ),
        nameMatchDetails,
        ggCredId,
        _
      )
    }

  def attemptNameMatch(
    nameMatchDetails: IndividualRepresenteeNameMatchDetails,
    ggCredId: GGCredId,
    previousUnsuccessfulNameMatchAttempts: Option[
      UnsuccessfulNameMatchAttempts[IndividualRepresenteeNameMatchDetails]
    ]
  )(implicit
    hc: HeaderCarrier,
    request: Request[_]
  ): EitherT[Future, NameMatchServiceError[
    IndividualRepresenteeNameMatchDetails
  ], RepresenteeReferenceId] =
    withValidPreviousUnsuccessfulNameMatchAttempts(
      previousUnsuccessfulNameMatchAttempts,
      nameMatchDetails
    ) { u =>
      nameMatchDetails.id match {
        case NoReferenceId             => EitherT.pure(NoReferenceId)
        case n @ RepresenteeNino(nino) =>
          handleBprNameMatch(
            IndividualBusinessPartnerRecordRequest(
              Right(nino),
              Some(nameMatchDetails.name)
            ),
            nameMatchDetails,
            ggCredId,
            u
          ).map(_ => n)

        case s @ RepresenteeSautr(sautr) =>
          handleBprNameMatch(
            IndividualBusinessPartnerRecordRequest(
              Left(sautr),
              Some(nameMatchDetails.name)
            ),
            nameMatchDetails,
            ggCredId,
            u
          ).map(_ => s)

        case c @ RepresenteeCgtReference(cgtReference) =>
          handleCgtAccountNameMatch(
            cgtReference,
            nameMatchDetails.name,
            nameMatchDetails,
            ggCredId,
            u
          ).map(_ => c)
      }
    }

  private def withValidPreviousUnsuccessfulNameMatchAttempts[
    A <: NameMatchDetails,
    B
  ](
    previousUnsuccessfulNameMatchAttempts: Option[
      UnsuccessfulNameMatchAttempts[A]
    ],
    nameMatchDetails: A
  )(
    f: Option[UnsuccessfulNameMatchAttempts[A]] => EitherT[
      Future,
      NameMatchServiceError[A],
      B
    ]
  )(implicit
    hc: HeaderCarrier,
    request: Request[_]
  ): EitherT[Future, NameMatchServiceError[A], B] =
    previousUnsuccessfulNameMatchAttempts match {
      case Some(UnsuccessfulNameMatchAttempts(previousAttempts, _, _)) if previousAttempts >= maxUnsuccessfulAttempts =>
        auditNameMatchEvent(previousAttempts, nameMatchDetails)
        EitherT.fromEither[Future](
          Left(NameMatchServiceError.TooManyUnsuccessfulAttempts())
        )

      case Some(
            u @ UnsuccessfulNameMatchAttempts(
              unsuccessfulAttempts,
              _,
              `nameMatchDetails`
            )
          ) =>
        auditNameMatchEvent(unsuccessfulAttempts, nameMatchDetails)
        EitherT.fromEither[Future](
          Left(NameMatchServiceError.NameMatchFailed(u))
        )

      case maybeAttempts =>
        auditNameMatchEvent(
          maybeAttempts.map(_.unsuccessfulAttempts + 1).getOrElse(1),
          nameMatchDetails
        )
        f(maybeAttempts)
    }

  private def handleBprNameMatch[A <: NameMatchDetails : Writes](
    bprRequest: BusinessPartnerRecordRequest,
    nameMatchDetails: A,
    ggCredId: GGCredId,
    previousUnsuccessfulNameMatchAttempts: Option[
      UnsuccessfulNameMatchAttempts[A]
    ]
  )(implicit
    hc: HeaderCarrier
  ): EitherT[Future, NameMatchServiceError[
    A
  ], (BusinessPartnerRecord, Option[CgtReference])] =
    for {
      bprResponse <- bprService
                       .getBusinessPartnerRecord(bprRequest)
                       .leftMap(NameMatchServiceError.BackendError)
      bpr         <- extractBpr(
                       bprResponse,
                       nameMatchDetails,
                       ggCredId,
                       previousUnsuccessfulNameMatchAttempts
                     )
    } yield bpr

  private def auditNameMatchEvent(
    numberOfAttempts: Int,
    nameMatchDetails: NameMatchDetails
  )(implicit
    hc: HeaderCarrier,
    request: Request[_]
  ): Unit =
    nameMatchDetails match {
      case IndividualRepresenteeNameMatchDetails(
            name,
            RepresenteeCgtReference(cgtReference)
          ) =>
        auditService.sendEvent(
          "cgtAccountNameMatchAttempt",
          CgtAccountNameMatchAttemptEvent(
            numberOfAttempts,
            maxUnsuccessfulAttempts,
            name.firstName,
            name.lastName,
            cgtReference.value
          ),
          "cgt-account-name-match-attempt"
        )

      case _ =>
        BusinessPartnerRecordNameMatchAuditDetails
          .fromNameMatchDetails(nameMatchDetails)
          .foreach(auditEvent =>
            auditService.sendEvent(
              "businessPartnerRecordNameMatchAttempt",
              BusinessPartnerRecordNameMatchAttemptEvent(
                numberOfAttempts,
                maxUnsuccessfulAttempts,
                auditEvent
              ),
              "business-partner-record-name-match-attempt"
            )
          )
    }

  private def auditTooManyAttempts[A <: NameMatchDetails](
    details: UnsuccessfulNameMatchAttempts[A]
  )(implicit
    hc: HeaderCarrier,
    request: Request[_]
  ): Unit = {
    def eventWithoutId(name: Either[TrustName, IndividualName]) = NameMatchAccountLocked(
      details.unsuccessfulAttempts,
      details.maximumAttempts,
      name.map(_.firstName).toOption,
      name.map(_.lastName).toOption,
      name.swap.map(_.value).toOption,
      None,
      None,
      None,
      None
    )

    val auditEvent = details.lastDetailsTried match {
      case n: IndividualSautrNameMatchDetails =>
        eventWithoutId(Right(n.name)).copy(sautr = Some(n.sautr.value))

      case t: TrustNameMatchDetails =>
        eventWithoutId(Left(t.name)).copy(trn = Some(t.trn.value))

      case r: IndividualRepresenteeNameMatchDetails =>
        r.id match {
          case RepresenteeNino(nino)   =>
            eventWithoutId(Right(r.name)).copy(nino = Some(nino.value))
          case RepresenteeSautr(sautr) =>
            eventWithoutId(Right(r.name)).copy(sautr = Some(sautr.value))

          case RepresenteeCgtReference(cgtRef) =>
            eventWithoutId(Right(r.name)).copy(cgtReference = Some(cgtRef.value))

          case RepresenteeReferenceId.NoReferenceId =>
            eventWithoutId(Right(r.name))
        }

    }

    auditService.sendEvent(
      "NameMatchAccountLocked",
      auditEvent,
      "name-match-account-locked"
    )
  }

  def handleCgtAccountNameMatch[A <: NameMatchDetails : Writes](
    cgtReference: CgtReference,
    name: IndividualName,
    nameMatchDetails: A,
    ggCredId: GGCredId,
    previousUnsuccessfulNameMatchAttempts: Option[
      UnsuccessfulNameMatchAttempts[NameMatchDetails]
    ]
  )(implicit
    hc: HeaderCarrier
  ): EitherT[Future, NameMatchServiceError[A], SubscribedDetails] = {
    def doNamesMatch(
      name: IndividualName,
      comparedName: IndividualName
    ): Boolean = {
      def format(word: String): String =
        s"${word.toLowerCase().filterNot(_.isWhitespace).trim}"

      (format(name.firstName) === format(comparedName.firstName)) && (format(
        name.lastName
      ) === format(
        comparedName.lastName
      ))
    }
    for {
      subscription   <- subscriptionService
                          .getSubscribedDetails(cgtReference)
                          .leftMap(NameMatchServiceError.BackendError)
      matchedDetails <- {
        val nameMatchResult = Either.fromOption(
          subscription.filter(_.name.exists(doNamesMatch(_, name))),
          updateNumberOfUnsuccessfulNameMatchAttempts(
            nameMatchDetails,
            ggCredId,
            previousUnsuccessfulNameMatchAttempts
          )
        )
        EitherT[Future, NameMatchServiceError[A], SubscribedDetails] {
          nameMatchResult.leftSequence[Future, NameMatchServiceError[A]]
        }
      }
    } yield matchedDetails

  }

  private def extractBpr[A <: NameMatchDetails : Writes](
    bprResponse: BusinessPartnerRecordResponse,
    nameMatchDetails: A,
    ggCredId: GGCredId,
    previousUnsuccessfulNameMatchAttempts: Option[
      UnsuccessfulNameMatchAttempts[A]
    ]
  ): EitherT[Future, NameMatchServiceError[
    A
  ], (BusinessPartnerRecord, Option[CgtReference])] = {
    val result: Either[Future[
      NameMatchServiceError[A]
    ], (BusinessPartnerRecord, Option[CgtReference])] = Either
      .fromOption(
        bprResponse.businessPartnerRecord.map(bpr => bpr -> bprResponse.cgtReference),
        updateNumberOfUnsuccessfulNameMatchAttempts(
          nameMatchDetails,
          ggCredId,
          previousUnsuccessfulNameMatchAttempts
        )
      )
    EitherT[Future, NameMatchServiceError[
      A
    ], (BusinessPartnerRecord, Option[CgtReference])] {
      result.leftSequence[Future, NameMatchServiceError[A]]
    }
  }

  private def updateNumberOfUnsuccessfulNameMatchAttempts[
    A <: NameMatchDetails : Writes
  ](
    nameMatchDetails: A,
    ggCredId: GGCredId,
    previousUnsuccessfulNameMatchAttempts: Option[
      UnsuccessfulNameMatchAttempts[NameMatchDetails]
    ]
  ): Future[NameMatchServiceError[A]] = {
    val updatedNumberOfUnsuccessfulAttempts =
      previousUnsuccessfulNameMatchAttempts
        .map(_.unsuccessfulAttempts + 1)
        .getOrElse(1)

    val unsuccessfulNameMatchAttempts =
      UnsuccessfulNameMatchAttempts(
        updatedNumberOfUnsuccessfulAttempts,
        maxUnsuccessfulAttempts,
        nameMatchDetails
      )

    EitherT(
      nameMatchRetryStore.store(
        ggCredId,
        unsuccessfulNameMatchAttempts
      )
    ).leftMap(NameMatchServiceError.BackendError)
      .map(_ =>
        if (updatedNumberOfUnsuccessfulAttempts >= maxUnsuccessfulAttempts)
          NameMatchServiceError.TooManyUnsuccessfulAttempts()
        else
          NameMatchServiceError.NameMatchFailed(unsuccessfulNameMatchAttempts)
      )
      .merge
  }

}
