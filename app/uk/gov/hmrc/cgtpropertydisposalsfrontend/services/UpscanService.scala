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
import play.api.libs.json.{Json, OFormat}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.UpscanConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Error
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.CgtReference
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.upscan.UpscanStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.UpscanService.UpscanServiceResponse.{UpscanDescriptor, UpscanNotifyEvent, UpscanResponse}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.UpscanService.{UpscanNotifyResponse, UpscanServiceResponse}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

@ImplementedBy(classOf[UpscanServiceImpl])
trait UpscanService {
  def initiate(cgtReference: CgtReference)(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, UpscanServiceResponse]

  def storeNotifyEvent(cgtReference: CgtReference, upscanNotifyResponse: UpscanNotifyResponse)(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, Unit]
}

@Singleton
class UpscanServiceImpl @Inject()(
  upscanConnector: UpscanConnector,
  upscanStore: UpscanStore,
  configuration: Configuration
) extends UpscanService
    with Logging {

  private def getUpscanInitiateConfig[A: Configs](key: String): A =
    configuration.underlying
      .get[A](s"microservice.services.upscan-initiate.$key")
      .value

  private val maxUploads: Int = getUpscanInitiateConfig[Int]("max-uploads")

  override def storeNotifyEvent(cgtReference: CgtReference, upscanNotifyResponse: UpscanNotifyResponse)(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, Unit] =
    upscanStore
      .insert(UpscanNotifyEvent(cgtReference.value, upscanNotifyResponse))
      .leftMap { error =>
        logger.warn(s"Could not store upscan notify event due to $error")
        error
      }

  override def initiate(
    cgtReference: CgtReference
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, UpscanServiceResponse] =
    upscanStore.fileUploadCount(cgtReference).flatMap { uploadCount =>
      if (uploadCount > maxUploads) {
        EitherT.rightT(UpscanServiceResponse.MaximumFileUploadReached)
      } else {
        for {
          response <- upscanConnector.initiate(cgtReference)
          descriptor <- EitherT.fromOption(
                         response.json.validate[UpscanDescriptor].asOpt,
                         Error("S3 Json payload structure is not as expected")
                       )
        } yield UpscanResponse(cgtReference.value, descriptor)
      }
    }

}

object UpscanService {

  final case class UpscanNotifyResponse(
    reference: String,
    fileStatus: String,
    downloadUrl: String,
    uploadDetails: Map[String, String]
  )

  object UpscanNotifyResponse {
    implicit val format: OFormat[UpscanNotifyResponse] = Json.format[UpscanNotifyResponse]
  }

  sealed trait UpscanServiceResponse

  object UpscanServiceResponse {

    final case class UpscanNotifyEvent(cgtReference: String, upscanNotifyResponse: UpscanNotifyResponse)

    object UpscanNotifyEvent {
      implicit val format: OFormat[UpscanNotifyEvent] = Json.format[UpscanNotifyEvent]
    }

    final case class UpscanRequest(href: String, fields: Map[String, String])

    object UpscanRequest {
      implicit val format: OFormat[UpscanRequest] = Json.format[UpscanRequest]
    }

    final case class UpscanDescriptor(reference: String, uploadRequest: UpscanRequest)

    object UpscanDescriptor {
      implicit val upscanDescriptorFormat: OFormat[UpscanDescriptor] = Json.format[UpscanDescriptor]
    }

    case object MaximumFileUploadReached extends UpscanServiceResponse

    final case class UpscanResponse(
      cgtReference: String,
      upscanDescriptor: UpscanDescriptor
    ) extends UpscanServiceResponse

    object UpscanResponse {
      implicit val format: OFormat[UpscanResponse] = Json.format[UpscanResponse]
    }

  }

}
