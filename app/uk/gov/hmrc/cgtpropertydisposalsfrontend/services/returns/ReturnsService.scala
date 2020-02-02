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
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.returns.ReturnsConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Error
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.CgtReference
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.DraftReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsServiceImpl.GetDraftReturnResponse
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.HttpResponseOps._
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[ReturnsServiceImpl])
trait ReturnsService {

  def storeDraftReturn(draftReturn: DraftReturn)(implicit hc: HeaderCarrier): EitherT[Future, Error, Unit]

  def getDraftReturns(cgtReference: CgtReference)(implicit hc: HeaderCarrier): EitherT[Future, Error, List[DraftReturn]]

}

@Singleton
class ReturnsServiceImpl @Inject() (connector: ReturnsConnector)(implicit ec: ExecutionContext) extends ReturnsService {

  @SuppressWarnings(Array("org.wartremover.warts.Var"))
  var draftReturns: Map[UUID, DraftReturn] = Map.empty

  // TODO: take out
  val test: Boolean = false

  def storeDraftReturn(draftReturn: DraftReturn)(implicit hc: HeaderCarrier): EitherT[Future, Error, Unit] =
    if (test) {
      draftReturns = draftReturns.updated(draftReturn.id, draftReturn)
      EitherT.pure[Future, Error](())
    } else {
      connector.storeDraftReturn(draftReturn).subflatMap { httpResponse =>
        if (httpResponse.status === OK) {
          Right(())
        } else {
          Left(Error(s"Call to store draft return came back with status ${httpResponse.status}}"))
        }
      }
    }

  def getDraftReturns(
    cgtReference: CgtReference
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, List[DraftReturn]] =
    if (test)
      EitherT.pure(draftReturns.values.toList)
    else
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

}

object ReturnsServiceImpl {

  final case class GetDraftReturnResponse(draftReturns: List[DraftReturn])

  object GetDraftReturnResponse {

    implicit val format: OFormat[GetDraftReturnResponse] = Json.format[GetDraftReturnResponse]
  }

}
