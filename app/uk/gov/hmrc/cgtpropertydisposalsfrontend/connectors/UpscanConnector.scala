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
import play.api.libs.json.{JsObject, JsValue, Json, OFormat}
import play.api.libs.ws.WSClient
import play.api.mvc.MultipartFormData
import play.mvc.Http.Status
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.http.HttpClient._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.CgtReference
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

  def getUpscanSnapshot(cgtReference: CgtReference)(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, Option[UpscanSnapshot]]

  def initiate(cgtReference: CgtReference)(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, HttpResponse]

  def saveUpscanInititateResponse(upscanFileDescriptor: UpscanFileDescriptor)(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, Unit]

  def getFileDescriptor(cgtReference: CgtReference, upscanInitiateReference: UpscanInitiateReference)(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, Option[UpscanFileDescriptor]]

  def upload(href: String, form: MultipartFormData[Source[ByteString, _]])(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, Unit]

  def updateUpscanFileDescriptorStatus(upscanFileDescriptor: UpscanFileDescriptor)(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, Unit]

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
    cgtReference: CgtReference
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, Option[UpscanSnapshot]] = {
    val url = baseUrl + s"/cgt-property-disposals/upscan-snapshot-info/cgt-reference/${cgtReference.value}"
    EitherT[Future, Error, Option[UpscanSnapshot]](
      http
        .get(url)
        .map(httpResponse => Right(Json.fromJson[UpscanSnapshot](httpResponse.json).asOpt))
        .recover {
          case NonFatal(e) => Left(Error(e))
        }
    )
  }

  override def initiate(cgtReference: CgtReference)(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, HttpResponse] = {
    val payload = UpscanInitiateRequest(
      baseUrl + s"/cgt-property-disposals/upscan-call-back/cgt-reference/${cgtReference.value}",
      minFileSize,
      maxFileSize
    )
    EitherT[Future, Error, HttpResponse](
      http
        .post[UpscanInitiateRequest](url, payload)
        .map[Either[Error, HttpResponse]] { response =>
          if (response.status != Status.OK) {
            Left(Error("S3 did not return 200 status code"))
          } else {
            Right(response)
          }
        }
        .recover { case e => Left(Error(e)) }
    )
  }

  override def saveUpscanInititateResponse(upscanFileDescriptor: UpscanFileDescriptor)(
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

  override def getFileDescriptor(cgtReference: CgtReference, upscanInitiateReference: UpscanInitiateReference)(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, Option[UpscanFileDescriptor]] = {
    val url = baseUrl + s"/cgt-property-disposals/upscan-file-descriptor/${upscanInitiateReference.value}"
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
            case status if (is4xx(status) || is5xx(status)) =>
              logger.warn(s"S3 file upload failed due to ${response.body}")
              Left(Error(response.body))
            case _ => Left(Error("Invalid HTTP response status from S3"))
          }
        }
    )
  }

  private def is4xx(status: Int): Boolean = status >= 400 && status < 500

  private def is5xx(status: Int): Boolean = status >= 500 && status < 600
}
