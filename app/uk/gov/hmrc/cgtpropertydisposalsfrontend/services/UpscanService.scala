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
import configs.Configs
import configs.syntax._
import javax.inject.Inject
import play.api.Configuration
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.UpscanConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Error
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.{CgtReference, DraftReturnId}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.upscan.UpscanFileDescriptor.UpscanFileDescriptorStatus.READY_TO_UPLOAD
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.upscan.UpscanInitiateResponse.UpscanInitiateSuccess
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.upscan._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

@ImplementedBy(classOf[UpscanServiceImpl])
trait UpscanService {
  def initiate(draftReturnId: DraftReturnId, cgtReference: CgtReference, timestamp: LocalDateTime)(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, UpscanInitiateResponse]

  def getUpscanFileDescriptor(draftReturnId: DraftReturnId, upscanInitiateReference: UpscanInitiateReference)(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, Option[UpscanFileDescriptor]]

  def updateUpscanFileDescriptorStatus(upscanFileDescriptor: UpscanFileDescriptor)(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, Unit]

  def removeFile(draftReturnId: DraftReturnId, upscanInitiateReference: UpscanInitiateReference)(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, Unit]

  def removeAllFiles(draftReturnId: DraftReturnId)(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, Unit]

  def getAll(draftReturnId: DraftReturnId)(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, List[UpscanFileDescriptor]]

}

@Singleton
class UpscanServiceImpl @Inject() (
  upscanConnector: UpscanConnector,
  configuration: Configuration
) extends UpscanService
    with Logging {

  private def getUpscanInitiateConfig[A: Configs](key: String): A =
    configuration.underlying
      .get[A](s"microservice.services.upscan-initiate.$key")
      .value

  private val maxUploads: Int = getUpscanInitiateConfig[Int]("max-uploads")

  private def hasReachedMaxFileUpload(upscanSnapshot: UpscanSnapshot): Either[Error, Unit] =
    if (upscanSnapshot.fileUploadCount >= maxUploads) {
      Left(Error(MaxFileUploadsReached2))
    } else {
      Right(())
    }

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
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, UpscanInitiateResponse] =
    for {
      snapshot     <- upscanConnector.getUpscanSnapshot(draftReturnId)
      _            <- EitherT.fromEither(hasReachedMaxFileUpload(snapshot))
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

  override def removeFile(draftReturnId: DraftReturnId, upscanInitiateReference: UpscanInitiateReference)(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, Unit] =
    upscanConnector.removeFile(draftReturnId, upscanInitiateReference)

  override def removeAllFiles(draftReturnId: DraftReturnId)(implicit hc: HeaderCarrier): EitherT[Future, Error, Unit] =
    upscanConnector.removeAllFiles(draftReturnId)

  override def getAll(
    draftReturnId: DraftReturnId
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, List[UpscanFileDescriptor]] =
    upscanConnector.getAll(draftReturnId)

}
