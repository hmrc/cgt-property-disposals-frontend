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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors

import akka.stream.scaladsl.Source
import akka.util.ByteString
import cats.data.EitherT
import com.google.inject.{ImplementedBy, Inject, Singleton}
import configs.Configs
import configs.syntax._
import play.api.Configuration
import play.api.http.HeaderNames.USER_AGENT
import play.api.libs.json.{JsError, JsSuccess, Json, OFormat}
import play.api.libs.ws.WSClient
import play.api.mvc.MultipartFormData
import play.mvc.Http.Status
import uk.gov.hmrc.cgtpropertydisposalsfrontend.http.HttpClient._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.DraftReturnId
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.upscan._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{upscan => _, _}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig
import uk.gov.hmrc.play.bootstrap.http.HttpClient

import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal

final case class UpscanInitiateRequest(
  callbackUrl: String,
  minimumFileSize: Int,
  maximumFileSize: Long
)

final object UpscanInitiateRequest {
  implicit val format: OFormat[UpscanInitiateRequest] = Json.format[UpscanInitiateRequest]
}

@ImplementedBy(classOf[UpscanConnectorImpl])
trait UpscanConnector {

  def getUpscanSnapshot(draftReturnId: DraftReturnId)(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, UpscanSnapshot]

  def initiate(draftReturnId: DraftReturnId)(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, HttpResponse]

  def saveUpscanFileDescriptors(upscanFileDescriptor: UpscanFileDescriptor)(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, Unit]

  def getFileDescriptor(draftReturnId: DraftReturnId, upscanInitiateReference: UpscanInitiateReference)(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, Option[UpscanFileDescriptor]]

  def upload(href: String, form: MultipartFormData[Source[ByteString, _]])(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, Unit]

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
class UpscanConnectorImpl @Inject() (
  http: HttpClient,
  wsClient: WSClient,
  config: Configuration,
  servicesConfig: ServicesConfig
)(
  implicit ec: ExecutionContext
) extends UpscanConnector
    with Logging {

  private def getUpscanInitiateConfig[A: Configs](key: String): A =
    config.underlying
      .get[A](s"microservice.services.upscan-initiate.$key")
      .value

  private val url: String = {
    val protocol = getUpscanInitiateConfig[String]("protocol")
    val host     = getUpscanInitiateConfig[String]("host")
    val port     = getUpscanInitiateConfig[String]("port")
    s"$protocol://$host:$port/upscan/initiate"
  }

  val baseUrl: String = servicesConfig.baseUrl("cgt-property-disposals")

  val selfBaseUrl: String = config.underlying.get[String]("self.url").value

  private val minFileSize: Int  = getUpscanInitiateConfig[Int]("min-file-size")
  private val maxFileSize: Long = getUpscanInitiateConfig[Long]("max-file-size")

  override def getUpscanSnapshot(
    draftReturnId: DraftReturnId
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, UpscanSnapshot] = {
    val url = baseUrl + s"/cgt-property-disposals/upscan-snapshot-info/draft-return-id/${draftReturnId.value}"
    EitherT[Future, Error, UpscanSnapshot](
      http
        .get(url)
        .map { httpResponse =>
          Json.fromJson[UpscanSnapshot](httpResponse.json) match {
            case JsSuccess(upscanSnapshot, _) => Right(upscanSnapshot)
            case JsError(errors)              => Left(Error(s"failed to get upscan snapshot: $errors"))
          }
        }
        .recover {
          case NonFatal(e) => Left(Error(e))
        }
    )
  }

  override def removeFile(
    draftReturnId: DraftReturnId,
    upscanInitiateReference: UpscanInitiateReference
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, Unit] = {
    val url = baseUrl + s"/cgt-property-disposals/upscan-file-descriptor/draft-return-id/${draftReturnId.value}/upscan-reference/${upscanInitiateReference.value}"
    EitherT[Future, Error, Unit](
      http
        .get(url)
        .map { _ =>
          Right(())
        }
        .recover {
          case NonFatal(e) => Left(Error(e))
        }
    )
  }

  override def removeAllFiles(
    draftReturnId: DraftReturnId
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, Unit] = {
    val url = baseUrl + s"/cgt-property-disposals/upscan-file-descriptor/delete-all-files/${draftReturnId.value}"
    EitherT[Future, Error, Unit](
      http
        .get(url)
        .map { _ =>
          Right(())
        }
        .recover {
          case NonFatal(e) => Left(Error(e))
        }
    )
  }

  override def initiate(draftReturnId: DraftReturnId)(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, HttpResponse] = {
    val payload = UpscanInitiateRequest(
      baseUrl + s"/cgt-property-disposals/upscan-call-back/draft-return-id/${draftReturnId.value}",
      minFileSize,
      maxFileSize
    )
    EitherT[Future, Error, HttpResponse](
      http
        .post[UpscanInitiateRequest](url, payload, Map(USER_AGENT -> "cgt-property-disposals-frontend"))
        .map[Either[Error, HttpResponse]] { response =>
          if (response.status != Status.OK) {
            Left(Error("S3 did not return 200 status code"))
          } else {
            logger.warn("")
            Right(response)
          }
        }
        .recover { case e => Left(Error(e)) }
    )
  }

  override def saveUpscanFileDescriptors(upscanFileDescriptor: UpscanFileDescriptor)(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, Unit] = {
    val url = baseUrl + s"/cgt-property-disposals/upscan-file-descriptor"
    EitherT[Future, Error, Unit](
      http
        .post[UpscanFileDescriptor](url, upscanFileDescriptor)
        .map { httpResponse =>
          httpResponse.status match {
            case Status.OK => Right(())
            case Status.BAD_REQUEST | Status.INTERNAL_SERVER_ERROR =>
              Left(Error(s"failed to save upscan initiate response: $httpResponse"))
          }
        }
        .recover { case e => Left(Error(e)) }
    )
  }

  override def getFileDescriptor(draftReturnId: DraftReturnId, upscanInitiateReference: UpscanInitiateReference)(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, Option[UpscanFileDescriptor]] = {
    val url = baseUrl + s"/cgt-property-disposals/upscan-fd/draft-return-id/${draftReturnId.value}/upscan-reference/${upscanInitiateReference.value}"

    EitherT[Future, Error, Option[UpscanFileDescriptor]](
      http
        .get(url)
        .map { httpResponse =>
          httpResponse.status match {
            case Status.OK => Right(Json.fromJson[UpscanFileDescriptor](httpResponse.json).asOpt)
            case Status.BAD_REQUEST | Status.INTERNAL_SERVER_ERROR =>
              Left(Error(s"failed to get upscan file descriptor: $httpResponse"))
          }
        }
        .recover {
          case NonFatal(e) => Left(Error(e))
        }
    )
  }

  override def getAll(draftReturnId: DraftReturnId)(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, List[UpscanFileDescriptor]] = {
    val url = baseUrl + s"/cgt-property-disposals/upscan-file-descriptor/all/${draftReturnId.value}"
    EitherT[Future, Error, List[UpscanFileDescriptor]](
      http
        .get(url)
        .map { httpResponse =>
          httpResponse.status match {
            case Status.OK =>
              Right(Json.fromJson[List[UpscanFileDescriptor]](httpResponse.json).asOpt match {
                case Some(fd) => fd
                case None     => List()
              })
            case Status.BAD_REQUEST | Status.INTERNAL_SERVER_ERROR =>
              Left(Error(s"failed to get upscan file descriptor: $httpResponse"))
          }
        }
        .recover {
          case NonFatal(e) => Left(Error(e))
        }
    )
  }

  override def updateUpscanFileDescriptorStatus(upscanFileDescriptor: UpscanFileDescriptor)(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, Unit] = {
    val url = baseUrl + s"/cgt-property-disposals/upscan-file-descriptor/status"
    EitherT[Future, Error, Unit](
      http
        .put[UpscanFileDescriptor](url, upscanFileDescriptor)
        .map { httpResponse =>
          httpResponse.status match {
            case Status.OK => Right(())
            case Status.BAD_REQUEST | Status.INTERNAL_SERVER_ERROR =>
              Left(Error(s"failed to update upscan file descriptor status: $httpResponse"))
          }
        }
        .recover { case e => Left(Error(e)) }
    )
  }
  @SuppressWarnings(Array("org.wartremover.warts.Var", "org.wartremover.warts.Any"))
  override def upload(href: String, form: MultipartFormData[Source[ByteString, _]])(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, Unit] = {

    val parts: Source[MultipartFormData.Part[Source[ByteString, _]], _] = Source.apply(form.dataParts.flatMap {
      case (key, values) =>
        values.map(value => MultipartFormData.DataPart(key, value): MultipartFormData.Part[Source[ByteString, _]])
    } ++ form.files)

    EitherT[Future, Error, Unit](
      wsClient
        .url(href)
        .post(parts)
        .map { response =>
          response.status match {
            case 204 => Right(())
            case _ =>
              Left(Error(s"S3 file upload failed due to: ${response.body} with http status: ${response.status}"))
          }
        }
    )
  }
}
