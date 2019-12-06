/*
 * Copyright 2019 HM Revenue & Customs
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
import play.api.libs.json.{Json, Reads, Writes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.EitherUtils.EitherOps
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.{GGCredId, SAUTR, TRN}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.audit.BusinessPartnerRecordNameMatchDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.audit.BusinessPartnerRecordNameMatchDetails.{IndividualNameWithSaUtrAuditDetails, TrustNameWithTrnAuditDetails}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.bpr.BusinessPartnerRecordRequest.{IndividualBusinessPartnerRecordRequest, TrustBusinessPartnerRecordRequest}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.bpr.UnsuccessfulNameMatchAttempts.NameMatchDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.bpr.UnsuccessfulNameMatchAttempts.NameMatchDetails.IndividualNameMatchDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.bpr._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.onboarding.BusinessPartnerRecordNameMatchRetryStore
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
  )(implicit ec: ExecutionContext, hc: HeaderCarrier): EitherT[Future, NameMatchError[A], BusinessPartnerRecord]

}

@Singleton
class BusinessPartnerRecordNameMatchRetryServiceImpl @Inject()(
  bprService: BusinessPartnerRecordService,
  bprNameMatchRetryStore: BusinessPartnerRecordNameMatchRetryStore,
  auditService: OnboardingAuditService,
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

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  def attemptBusinessPartnerRecordNameMatch[A <: NameMatchDetails: Writes](
    nameMatchDetails: A,
    ggCredId: GGCredId,
    previousUnsuccessfulNameMatchAttempts: Option[UnsuccessfulNameMatchAttempts[A]]
  )(implicit ec: ExecutionContext, hc: HeaderCarrier): EitherT[Future, NameMatchError[A], BusinessPartnerRecord] = {

    previousUnsuccessfulNameMatchAttempts match {
      case Some(UnsuccessfulNameMatchAttempts(attempts, _, _)) => {
        auditService. sendBusinessPartnerRecordNameMatchAttemptEvent(
          attempts,
          maxUnsuccessfulAttempts,
          nameMatchDetailsToAuditEvent(nameMatchDetails)
        )
      }
      case None => ()
    }

    previousUnsuccessfulNameMatchAttempts match {
      case Some(UnsuccessfulNameMatchAttempts(previousAttempts, _, _)) if previousAttempts >= maxUnsuccessfulAttempts =>
        EitherT.fromEither[Future](Left(NameMatchError.TooManyUnsuccessfulAttempts()))

      case Some(u @ UnsuccessfulNameMatchAttempts(_, _, `nameMatchDetails`)) =>
        EitherT.fromEither[Future](Left(NameMatchError.NameMatchFailed(u)))

      case _ =>
        for {
          bprResponse <- bprService
                          .getBusinessPartnerRecord(nameMatchDetailsToBprRequest(nameMatchDetails))
                          .leftMap(NameMatchError.BackendError)
          bpr <- extractBpr(bprResponse, nameMatchDetails, ggCredId, previousUnsuccessfulNameMatchAttempts)
        } yield bpr
    }
  }

  private def nameMatchDetailsToAuditEvent(
    nameMatchDetails: NameMatchDetails
  ): BusinessPartnerRecordNameMatchDetails =
    nameMatchDetails match {
      case IndividualNameMatchDetails(name, sautr) =>
        IndividualNameWithSaUtrAuditDetails(name.firstName, name.lastName, sautr.value)
      case NameMatchDetails.TrustNameMatchDetails(name, trn) => TrustNameWithTrnAuditDetails(name.value, trn.value)
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
  )(implicit ec: ExecutionContext): EitherT[Future, NameMatchError[A], BusinessPartnerRecord] =
    EitherT[Future, NameMatchError[A], BusinessPartnerRecord] {
      Either
        .fromOption(
          bprResponse.businessPartnerRecord,
          handleBprNotFound(nameMatchDetails, ggCredId, previousUnsuccessfulNameMatchAttempts)
        )
        .leftSequence[Future, NameMatchError[A]]
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
      .map(
        _ =>
          if (updatedNumberOfUnsuccessfulAttempts >= maxUnsuccessfulAttempts) {
            NameMatchError.TooManyUnsuccessfulAttempts()
          } else {
            NameMatchError.NameMatchFailed(unsuccessfulNameMatchAttempts)
          }
      )
      .merge
  }

}
