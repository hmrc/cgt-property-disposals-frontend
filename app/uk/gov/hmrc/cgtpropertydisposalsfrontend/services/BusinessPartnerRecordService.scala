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
import play.api.http.Status.OK
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.CGTPropertyDisposalsConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{BusinessPartnerRecord, DateOfBirth, Error, NINO, Name}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.HttpResponseOps._
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[BusinessPartnerRecordServiceImpl])
trait BusinessPartnerRecordService {

  def getBusinessPartnerRecord(nino: NINO, name: Name, dob: DateOfBirth)(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, BusinessPartnerRecord]

}

@Singleton
class BusinessPartnerRecordServiceImpl @Inject()(connector: CGTPropertyDisposalsConnector)(
  implicit ec: ExecutionContext
) extends BusinessPartnerRecordService {

  override def getBusinessPartnerRecord(
    nino: NINO,
    name: Name,
    dob: DateOfBirth
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, BusinessPartnerRecord] =
    connector
      .getBusinessPartnerRecord(nino, name, dob)
      .subflatMap { response =>
        response.status match {
          case OK =>
            response.parseJSON[BusinessPartnerRecord]().leftMap(Error.apply)
          case other =>
            Left(Error(s"Call to get BPR came back with status $other"))
        }
      }

}
