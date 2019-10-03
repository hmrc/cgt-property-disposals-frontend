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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.services

import cats.data.EitherT
import cats.instances.future._
import cats.syntax.either._
import cats.syntax.eq._
import com.google.inject.{ImplementedBy, Inject, Singleton}
import play.api.Configuration
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.EitherUtils.EitherOps
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.bpr.BusinessPartnerRecordRequest.IndividualBusinessPartnerRecordRequest
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.bpr.{BusinessPartnerRecord, BusinessPartnerRecordResponse, NameMatchError, UnsuccessfulNameMatchAttempts}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.{GGCredId, SAUTR}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.IndividualName
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.BusinessPartnerRecordNameMatchRetryStore
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[BusinessPartnerRecordNameMatchRetryServiceImpl])
trait BusinessPartnerRecordNameMatchRetryService {

  def getNumberOfUnsuccessfulAttempts(ggCredId: GGCredId)(
    implicit ec: ExecutionContext
  ): EitherT[Future, NameMatchError, Option[UnsuccessfulNameMatchAttempts]]

  def attemptBusinessPartnerRecordNameMatch(
                                             sautr: SAUTR,
                                             name: IndividualName,
                                             ggCredId: GGCredId,
                                             previousUnsuccessfulNameMatchAttempts: Option[UnsuccessfulNameMatchAttempts]
  )(implicit ec: ExecutionContext, hc: HeaderCarrier): EitherT[Future, NameMatchError, BusinessPartnerRecord]

}

@Singleton
class BusinessPartnerRecordNameMatchRetryServiceImpl @Inject()(
  bprService: BusinessPartnerRecordService,
  bprNameMatchRetryStore: BusinessPartnerRecordNameMatchRetryStore,
  config: Configuration
) extends BusinessPartnerRecordNameMatchRetryService {

  val maxUnsuccessfulAttempts: Int = config.underlying.getInt("bpr-name-match.max-retries")

  def getNumberOfUnsuccessfulAttempts(
    ggCredId: GGCredId
  )(implicit ec: ExecutionContext): EitherT[Future, NameMatchError, Option[UnsuccessfulNameMatchAttempts]] =
    EitherT(bprNameMatchRetryStore.get(ggCredId))
      .leftMap(NameMatchError.BackendError)
      .subflatMap {
        _.fold[Either[NameMatchError, Option[UnsuccessfulNameMatchAttempts]]](Right(None))(
          unsuccessfulAttempts =>
            if (unsuccessfulAttempts.unsuccesfulAttempts >= maxUnsuccessfulAttempts) {
              Left(NameMatchError.TooManyUnsuccessfulAttempts())
            } else {
              Right(Some(unsuccessfulAttempts))
            }
        )
      }

  def attemptBusinessPartnerRecordNameMatch(
                                             sautr: SAUTR,
                                             name: IndividualName,
                                             ggCredId: GGCredId,
                                             previousUnsuccessfulNameMatchAttempts: Option[UnsuccessfulNameMatchAttempts]
  )(implicit ec: ExecutionContext, hc: HeaderCarrier): EitherT[Future, NameMatchError, BusinessPartnerRecord] =
    previousUnsuccessfulNameMatchAttempts match {
      case Some(UnsuccessfulNameMatchAttempts(previousAttempts, _, _, _))
          if previousAttempts >= maxUnsuccessfulAttempts =>
        EitherT.fromEither[Future](Left(NameMatchError.TooManyUnsuccessfulAttempts()))

      case Some(u @ UnsuccessfulNameMatchAttempts(_, _, previousName, previousSautr))
          if previousName === name & previousSautr === sautr =>
        EitherT.fromEither[Future](Left(NameMatchError.NameMatchFailed(u)))

      case _ =>
        for {
          bprResponse <- bprService
                          .getBusinessPartnerRecord(IndividualBusinessPartnerRecordRequest(Left(sautr), Some(name)))
                          .leftMap(NameMatchError.BackendError)
          bpr <- extractBpr(bprResponse, sautr, name, ggCredId, previousUnsuccessfulNameMatchAttempts)
        } yield bpr
    }

  private def extractBpr(
                          bprResponse: BusinessPartnerRecordResponse,
                          sautr: SAUTR,
                          name: IndividualName,
                          ggCredId: GGCredId,
                          previousUnsuccessfulNameMatchAttempts: Option[UnsuccessfulNameMatchAttempts]
  )(implicit ec: ExecutionContext): EitherT[Future, NameMatchError, BusinessPartnerRecord] =
    EitherT[Future, NameMatchError, BusinessPartnerRecord] {
      Either
        .fromOption(
          bprResponse.businessPartnerRecord,
          handleBprNotFound(sautr, name, ggCredId, previousUnsuccessfulNameMatchAttempts)
        )
        .leftSequence[Future, NameMatchError]
    }

  private def handleBprNotFound(
                                 sautr: SAUTR,
                                 name: IndividualName,
                                 ggCredId: GGCredId,
                                 previousUnsuccessfulNameMatchAttempts: Option[UnsuccessfulNameMatchAttempts]
  )(
    implicit ec: ExecutionContext
  ): Future[NameMatchError] = {
    val updatedNumberOfUnsuccessfulAttempts =
      previousUnsuccessfulNameMatchAttempts.map(_.unsuccesfulAttempts + 1).getOrElse(1)

    val unsuccessfulNameMatchAttempts =
      UnsuccessfulNameMatchAttempts(updatedNumberOfUnsuccessfulAttempts, maxUnsuccessfulAttempts, name, sautr)

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
