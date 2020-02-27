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
import com.google.inject.{ImplementedBy, Singleton}
import configs.Configs
import configs.syntax._
import javax.inject.Inject
import play.api.Configuration
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.UpscanConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Error
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.CgtReference
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.upscan.UpscanFileDescriptor.UpscanFileDescriptorStatus.READY_TO_UPLOAD
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.upscan.UpscanInitiateResponse.{FailedToGetUpscanSnapshot, MaximumFileUploadReached, UpscanInititateResponseStored}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.upscan.{UpscanCallBack, UpscanFileDescriptor, UpscanInitiateReference, UpscanInitiateResponse}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

@ImplementedBy(classOf[UpscanServiceImpl])
trait UpscanService {
  def initiate(cgtReference: CgtReference)(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, UpscanInitiateResponse]

  def getUpscanFileDescriptor(cgtReference: CgtReference, upscanInitiateReference: UpscanInitiateReference)(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, Option[UpscanFileDescriptor]]

  def updateUpscanFileDescriptorStatus(upscanFileDescriptor: UpscanFileDescriptor)(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, Unit]

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

  override def initiate(
    cgtReference: CgtReference
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, UpscanInitiateResponse] =
    upscanConnector.getUpscanSnapshot(cgtReference).flatMap {
      case Some(upscanSnapshot) => {
        if (upscanSnapshot.fileUploadCount > maxUploads) {
          EitherT.rightT(MaximumFileUploadReached)
        } else {
          for {
            response <- upscanConnector.initiate(cgtReference)
            upscanFileDescriptor <- EitherT.fromOption(
                                     response.json.validate[UpscanFileDescriptor].asOpt,
                                     Error("S3 Json payload structure is not as expected")
                                   )
            _ <- upscanConnector
                  .saveUpscanInititateResponse(upscanFileDescriptor.copy(status = READY_TO_UPLOAD))
          } yield UpscanInititateResponseStored(upscanFileDescriptor.fileDescriptor.reference)
        }
      }
      case None => EitherT.rightT(FailedToGetUpscanSnapshot)
    }

  override def getUpscanFileDescriptor(cgtReference: CgtReference, upscanInitiateReference: UpscanInitiateReference)(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, Option[UpscanFileDescriptor]] =
    upscanConnector.getFileDescriptor(cgtReference, upscanInitiateReference)

  override def updateUpscanFileDescriptorStatus(upscanFileDescriptor: UpscanFileDescriptor)(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, Unit] =
    upscanConnector.updateUpscanFileDescriptorStatus(upscanFileDescriptor)

}
