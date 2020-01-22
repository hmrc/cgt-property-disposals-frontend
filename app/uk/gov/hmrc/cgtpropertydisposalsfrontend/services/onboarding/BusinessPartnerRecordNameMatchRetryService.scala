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

import cats.data.EitherT
import cats.instances.future._
import cats.syntax.either._
import com.google.inject.{ImplementedBy, Inject, Singleton}
import play.api.Configuration
import play.api.libs.json.{Reads, Writes}
import play.api.mvc.Request
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.EitherUtils.EitherOps
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.{CgtReference, GGCredId}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.audit.{BusinessPartnerRecordNameMatchAttemptEvent, BusinessPartnerRecordNameMatchDetails}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.bpr.BusinessPartnerRecordRequest.{IndividualBusinessPartnerRecordRequest, TrustBusinessPartnerRecordRequest}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.bpr.UnsuccessfulNameMatchAttempts.NameMatchDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.bpr.UnsuccessfulNameMatchAttempts.NameMatchDetails.IndividualNameMatchDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.bpr._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.onboarding.BusinessPartnerRecordNameMatchRetryStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.AuditService
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[BusinessPartnerRecordNameMatchRetryServiceImpl])
trait BusinessPartnerRecordNameMatchRetryService {

  def getNumberOfUnsuccessfulAttempts[A <: NameMatchDetails: Reads](ggCredId: GGCredId)(
    implicit ec: ExecutionContext
  ): EitherT[Future, NameMatchError[A], Option[UnsuccessfulNameMatchAttempts[A]]]

  def attemptBusinessPartnerRecordNameMatch[A <: NameMatchDetails: Writes](
    nameMatchDetails: A,
    ggCredId: GGCredId,
    previousUnsuccessfulNameMatchAttempts: Option[UnsuccessfulNameMatchAttempts[A]]
  )(
    implicit ec: ExecutionContext,
    hc: HeaderCarrier,
    request: Request[_]
  ): EitherT[Future, NameMatchError[A], (BusinessPartnerRecord, Option[CgtReference])]

}

@Singleton
class BusinessPartnerRecordNameMatchRetryServiceImpl @Inject() (
  bprService: BusinessPartnerRecordService,
  bprNameMatchRetryStore: BusinessPartnerRecordNameMatchRetryStore,
  auditService: AuditService,
  config: Configuration
) extends BusinessPartnerRecordNameMatchRetryService {

  val maxUnsuccessfulAttempts: Int = config.underlying.getInt("bpr-name-match.max-retries")

  def getNumberOfUnsuccessfulAttempts[A <: NameMatchDetails: Reads](
    ggCredId: GGCredId
  )(implicit ec: ExecutionContext): EitherT[Future, NameMatchError[A], Option[UnsuccessfulNameMatchAttempts[A]]] =
    EitherT(bprNameMatchRetryStore.get(ggCredId))
      .leftMap(NameMatchError.BackendError)
      .subflatMap {
        _.fold[Either[NameMatchError[A], Option[UnsuccessfulNameMatchAttempts[A]]]](Right(None))(
          (unsuccessfulAttempts: UnsuccessfulNameMatchAttempts[A]) =>
            if (unsuccessfulAttempts.unsuccessfulAttempts >= maxUnsuccessfulAttempts) {
              Left(NameMatchError.TooManyUnsuccessfulAttempts())
            } else {
              Right(Some(unsuccessfulAttempts))
            }
        )
      }

  def attemptBusinessPartnerRecordNameMatch[A <: NameMatchDetails: Writes](
    nameMatchDetails: A,
    ggCredId: GGCredId,
    previousUnsuccessfulNameMatchAttempts: Option[UnsuccessfulNameMatchAttempts[A]]
  )(
    implicit ec: ExecutionContext,
    hc: HeaderCarrier,
    request: Request[_]
  ): EitherT[Future, NameMatchError[A], (BusinessPartnerRecord, Option[CgtReference])] = {
    def auditNameMatchEvent(numberOfAttempts: Int): Unit =
      auditService.sendEvent(
        "businessPartnerRecordNameMatchAttempt",
        BusinessPartnerRecordNameMatchAttemptEvent(
          numberOfAttempts,
          maxUnsuccessfulAttempts,
          BusinessPartnerRecordNameMatchDetails.fromNameMatchDetails(nameMatchDetails)
        ),
        "business-partner-record-name-match-attempt"
      )

    previousUnsuccessfulNameMatchAttempts match {
      case Some(UnsuccessfulNameMatchAttempts(previousAttempts, _, _)) if previousAttempts >= maxUnsuccessfulAttempts =>
        auditNameMatchEvent(previousAttempts)
        EitherT.fromEither[Future](Left(NameMatchError.TooManyUnsuccessfulAttempts()))

      case Some(u @ UnsuccessfulNameMatchAttempts(unsuccessfulAttempts, _, `nameMatchDetails`)) =>
        auditNameMatchEvent(unsuccessfulAttempts)
        EitherT.fromEither[Future](Left(NameMatchError.NameMatchFailed(u)))

      case maybeAttempts =>
        auditNameMatchEvent(maybeAttempts.map(_.unsuccessfulAttempts + 1).getOrElse(1))
        for {
          bprResponse <- bprService
                          .getBusinessPartnerRecord(nameMatchDetailsToBprRequest(nameMatchDetails))
                          .leftMap(NameMatchError.BackendError)
          bpr <- extractBpr(bprResponse, nameMatchDetails, ggCredId, previousUnsuccessfulNameMatchAttempts)
        } yield bpr
    }
  }

  private def nameMatchDetailsToBprRequest(nameMatchDetails: NameMatchDetails): BusinessPartnerRecordRequest =
    nameMatchDetails match {
      case IndividualNameMatchDetails(name, sautr)           => IndividualBusinessPartnerRecordRequest(Left(sautr), Some(name))
      case NameMatchDetails.TrustNameMatchDetails(name, trn) => TrustBusinessPartnerRecordRequest(Left(trn), Some(name))
    }

  private def extractBpr[A <: NameMatchDetails: Writes](
    bprResponse: BusinessPartnerRecordResponse,
    nameMatchDetails: A,
    ggCredId: GGCredId,
    previousUnsuccessfulNameMatchAttempts: Option[UnsuccessfulNameMatchAttempts[A]]
  )(
    implicit ec: ExecutionContext
  ): EitherT[Future, NameMatchError[A], (BusinessPartnerRecord, Option[CgtReference])] = {
    val result: Either[Future[NameMatchError[A]], (BusinessPartnerRecord, Option[CgtReference])] = Either
      .fromOption(
        bprResponse.businessPartnerRecord.map(_ -> bprResponse.cgtReference),
        handleBprNotFound(nameMatchDetails, ggCredId, previousUnsuccessfulNameMatchAttempts)
      )
    EitherT[Future, NameMatchError[A], (BusinessPartnerRecord, Option[CgtReference])] {
      result.leftSequence[Future, NameMatchError[A]]
    }
  }

  private def handleBprNotFound[A <: NameMatchDetails: Writes](
    nameMatchDetails: A,
    ggCredId: GGCredId,
    previousUnsuccessfulNameMatchAttempts: Option[UnsuccessfulNameMatchAttempts[A]]
  )(
    implicit ec: ExecutionContext
  ): Future[NameMatchError[A]] = {
    val updatedNumberOfUnsuccessfulAttempts =
      previousUnsuccessfulNameMatchAttempts.map(_.unsuccessfulAttempts + 1).getOrElse(1)

    val unsuccessfulNameMatchAttempts =
      UnsuccessfulNameMatchAttempts(updatedNumberOfUnsuccessfulAttempts, maxUnsuccessfulAttempts, nameMatchDetails)

    EitherT(
      bprNameMatchRetryStore.store(
        ggCredId,
        unsuccessfulNameMatchAttempts
      )
    ).leftMap(NameMatchError.BackendError)
      .map(_ =>
        if (updatedNumberOfUnsuccessfulAttempts >= maxUnsuccessfulAttempts) {
          NameMatchError.TooManyUnsuccessfulAttempts()
        } else {
          NameMatchError.NameMatchFailed(unsuccessfulNameMatchAttempts)
        }
      )
      .merge
  }

}
