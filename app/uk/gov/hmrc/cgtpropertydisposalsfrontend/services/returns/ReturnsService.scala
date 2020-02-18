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
import play.api.libs.json.{Json, OFormat}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.returns.ReturnsConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Error
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.CgtReference
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{CompleteReturn, DraftReturn, SubmitReturnResponse}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsServiceImpl.GetDraftReturnResponse
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.HttpResponseOps._
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[ReturnsServiceImpl])
trait ReturnsService {

  def storeDraftReturn(draftReturn: DraftReturn)(implicit hc: HeaderCarrier): EitherT[Future, Error, Unit]

  def getDraftReturns(cgtReference: CgtReference)(implicit hc: HeaderCarrier): EitherT[Future, Error, List[DraftReturn]]

  def submitReturn(completeReturn: CompleteReturn)(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, SubmitReturnResponse]

}

@Singleton
class ReturnsServiceImpl @Inject() (connector: ReturnsConnector)(implicit ec: ExecutionContext) extends ReturnsService {

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

  val test: Boolean = false

  def submitReturn(
    completeReturn: CompleteReturn
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, SubmitReturnResponse] =
    if (!test) {
      connector.submitReturn(completeReturn).subflatMap { httpResponse =>
        if (httpResponse.status === OK) {
          httpResponse
            .parseJSON[SubmitReturnResponse]()
            .leftMap(Error(_))
        } else {
          Left(Error(s"Call to get submit return came back with status ${httpResponse.status}}"))
        }
      }
    } else {
      EitherT.pure(
        SubmitReturnResponse(
          "XDCGTX100006",
          completeReturn.yearToDateLiabilityAnswers.taxDue,
          LocalDate.of(2021, 2, 1),
          "0987654321AB"
        )
      )
    }

}

object ReturnsServiceImpl {

  final case class GetDraftReturnResponse(draftReturns: List[DraftReturn])

  object GetDraftReturnResponse {

    implicit val format: OFormat[GetDraftReturnResponse] = Json.format[GetDraftReturnResponse]
  }

}
