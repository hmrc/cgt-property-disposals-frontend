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
import com.google.inject.{ImplementedBy, Inject, Singleton}
import play.api.Configuration
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.EitherUtils.EitherOps
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Name
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.bpr.BusinessPartnerRecordRequest.IndividualBusinessPartnerRecordRequest
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.bpr.{BusinessPartnerRecord, BusinessPartnerRecordResponse, NameMatchError, NumberOfUnsuccessfulNameMatchAttempts}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.{GGCredId, SAUTR}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.BusinessPartnerRecordNameMatchRetryStore
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[BusinessPartnerRecordNameMatchRetryServiceImpl])
trait BusinessPartnerRecordNameMatchRetryService {

  def getNumberOfUnsuccessfulAttempts(ggCredId: GGCredId)(
    implicit ec: ExecutionContext
  ): EitherT[Future, NameMatchError, Option[NumberOfUnsuccessfulNameMatchAttempts]]

  def attemptBusinessPartnerRecordNameMatch(
    sautr: SAUTR,
    name: Name,
    ggCredId: GGCredId,
    previousNumberOfUnsuccessfulAttempts: Int
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
  )(implicit ec: ExecutionContext): EitherT[Future, NameMatchError, Option[NumberOfUnsuccessfulNameMatchAttempts]] =
    EitherT(bprNameMatchRetryStore.get(ggCredId))
      .leftMap(NameMatchError.BackendError)
      .subflatMap {
        _.fold[Either[NameMatchError,Option[NumberOfUnsuccessfulNameMatchAttempts]]](
          Right(None))(
        numberOfPreviousUnsuccessfulAttempts =>
        if(numberOfPreviousUnsuccessfulAttempts >= maxUnsuccessfulAttempts) {
          Left(NameMatchError.TooManyUnsuccessfulAttempts())
        } else {
          Right(Some(NumberOfUnsuccessfulNameMatchAttempts(
            numberOfPreviousUnsuccessfulAttempts,
            maxUnsuccessfulAttempts
          )))
        })
      }

  def attemptBusinessPartnerRecordNameMatch(
    sautr: SAUTR,
    name: Name,
    ggCredId: GGCredId,
    previousNumberOfUnsuccessfulAttempts: Int
  )(implicit ec: ExecutionContext, hc: HeaderCarrier): EitherT[Future, NameMatchError, BusinessPartnerRecord] =
    if (previousNumberOfUnsuccessfulAttempts >= maxUnsuccessfulAttempts) {
      EitherT.leftT[Future, BusinessPartnerRecord](NameMatchError.TooManyUnsuccessfulAttempts())
    } else {
      val result = for {
        bprResponse <- bprService
                        .getBusinessPartnerRecord(IndividualBusinessPartnerRecordRequest(Left(sautr), Some(name)))
                        .leftMap(NameMatchError.BackendError)
        bpr <- extractBpr(bprResponse, sautr, name, ggCredId, previousNumberOfUnsuccessfulAttempts)
      } yield bpr
      result
    }

  private def extractBpr(
    bprResponse: BusinessPartnerRecordResponse,
    sautr: SAUTR,
    name: Name,
    ggCredId: GGCredId,
    previousNumberOfUnsuccessfulAttempts: Int
  )(implicit ec: ExecutionContext): EitherT[Future, NameMatchError, BusinessPartnerRecord] =
    EitherT[Future, NameMatchError, BusinessPartnerRecord] {
      Either
        .fromOption(
          bprResponse.businessPartnerRecord,
          handleBprNotFound(sautr, name, ggCredId, previousNumberOfUnsuccessfulAttempts)
        )
        .leftSequence[Future, NameMatchError]
    }

  private def handleBprNotFound(
    sautr: SAUTR,
    name: Name,
    ggCredId: GGCredId,
    previousNumberOfUnsuccessfulAttempts: Int
  )(
    implicit ec: ExecutionContext
  ): Future[NameMatchError] = {
    val updatedNumberOfUnsuccessfulAttempts = previousNumberOfUnsuccessfulAttempts + 1

    EitherT(bprNameMatchRetryStore.store(ggCredId, updatedNumberOfUnsuccessfulAttempts))
      .leftMap(NameMatchError.BackendError)
      .map(
        _ =>
          if (updatedNumberOfUnsuccessfulAttempts >= maxUnsuccessfulAttempts) {
            NameMatchError.TooManyUnsuccessfulAttempts()
          } else {
            NameMatchError.NameMatchFailed(
              sautr,
              name,
              NumberOfUnsuccessfulNameMatchAttempts(
              updatedNumberOfUnsuccessfulAttempts, maxUnsuccessfulAttempts
              ))
          }
      )
      .merge
  }

}
