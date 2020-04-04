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

import java.time.LocalDateTime

import cats.data.EitherT
import cats.instances.future._
import com.google.inject.{ImplementedBy, Singleton}
import javax.inject.Inject
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.UpscanConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Error
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.{CgtReference, DraftReturnId}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.upscan.UpscanFileDescriptor.UpscanFileDescriptorStatus.READY_TO_UPLOAD
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.upscan.UpscanInitiateResponse.UpscanInitiateSuccess
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.upscan._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[UpscanServiceImpl])
trait UpscanService {
  def initiate(draftReturnId: DraftReturnId, cgtReference: CgtReference, timestamp: LocalDateTime)(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, UpscanInitiateSuccess]

  def getUpscanFileDescriptor(draftReturnId: DraftReturnId, upscanInitiateReference: UpscanInitiateReference)(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, Option[UpscanFileDescriptor]]

  def updateUpscanFileDescriptorStatus(upscanFileDescriptor: UpscanFileDescriptor)(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, Unit]

}

@Singleton
class UpscanServiceImpl @Inject() (
  upscanConnector: UpscanConnector
)(implicit ec: ExecutionContext)
    extends UpscanService
    with Logging {

  private def augmentUpscanInitiateResponse(
    draftReturnId: DraftReturnId,
    cgtReference: CgtReference,
    upscanInitiateRawResponse: UpscanInitiateRawResponse,
    timestamp: LocalDateTime
  ): Either[Error, UpscanFileDescriptor] =
    Right(
      UpscanFileDescriptor(
        upscanInitiateReference = UpscanInitiateReference(upscanInitiateRawResponse.reference),
        draftReturnId           = draftReturnId,
        cgtReference            = cgtReference,
        fileDescriptor          = FileDescriptor(upscanInitiateRawResponse.reference, upscanInitiateRawResponse.uploadRequest),
        timestamp               = timestamp,
        status                  = READY_TO_UPLOAD
      )
    )

  override def initiate(
    draftReturnId: DraftReturnId,
    cgtReference: CgtReference,
    timestamp: LocalDateTime
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, UpscanInitiateSuccess] =
    for {
      httpResponse <- upscanConnector.initiate(draftReturnId)
      upscanInitiateRawResponse <- EitherT.fromOption(
                                    httpResponse.json.validate[UpscanInitiateRawResponse].asOpt,
                                    Error("could not parse upscan initiate response")
                                  )
      upscanFileDescriptor <- EitherT.fromEither(
                               augmentUpscanInitiateResponse(
                                 draftReturnId,
                                 cgtReference,
                                 upscanInitiateRawResponse,
                                 timestamp
                               )
                             )
      _ <- upscanConnector.saveUpscanFileDescriptors(upscanFileDescriptor)
    } yield UpscanInitiateSuccess(UpscanInitiateReference(upscanFileDescriptor.fileDescriptor.reference))

  override def getUpscanFileDescriptor(draftReturnId: DraftReturnId, upscanInitiateReference: UpscanInitiateReference)(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, Option[UpscanFileDescriptor]] =
    upscanConnector.getFileDescriptor(draftReturnId, upscanInitiateReference)

  override def updateUpscanFileDescriptorStatus(upscanFileDescriptor: UpscanFileDescriptor)(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, Unit] =
    upscanConnector.updateUpscanFileDescriptorStatus(upscanFileDescriptor)

}
