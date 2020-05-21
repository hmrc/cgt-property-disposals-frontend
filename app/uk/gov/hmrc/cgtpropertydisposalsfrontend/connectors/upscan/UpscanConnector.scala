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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.upscan

import cats.data.EitherT
import com.google.inject.{ImplementedBy, Inject, Singleton}
import configs.Configs
import configs.syntax._
import play.api.Configuration
import play.api.http.HeaderNames.USER_AGENT
import play.api.mvc.Call
import play.mvc.Http.Status
import uk.gov.hmrc.cgtpropertydisposalsfrontend.http.HttpClient._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.upscan._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{upscan => _, _}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig
import uk.gov.hmrc.play.bootstrap.http.HttpClient

import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[UpscanConnectorImpl])
trait UpscanConnector {

  def getUpscanUpload(
    uploadReference: UploadReference
  )(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, HttpResponse]

  def saveUpscanUpload(upscanUpload: UpscanUpload)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, HttpResponse]

  def initiate(
    errorRedirect: Call,
    successRedirect: Call,
    uploadReference: UploadReference
  )(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, HttpResponse]

}

@Singleton
class UpscanConnectorImpl @Inject() (
  http: HttpClient,
  config: Configuration,
  servicesConfig: ServicesConfig
)(implicit
  ec: ExecutionContext
) extends UpscanConnector
    with Logging {

  private def getUpscanInitiateConfig[A : Configs](key: String): A =
    config.underlying
      .get[A](s"microservice.services.upscan-initiate.$key")
      .value

  private val upscanInitiateUrl: String = {
    val protocol = getUpscanInitiateConfig[String]("protocol")
    val host     = getUpscanInitiateConfig[String]("host")
    val port     = getUpscanInitiateConfig[String]("port")
    s"$protocol://$host:$port/upscan/v2/initiate"
  }

  val backEndBaseUrl: String = servicesConfig.baseUrl("cgt-property-disposals")

  val selfBaseUrl: String = config.underlying.get[String]("self.url").value

  private val maxFileSize: Long = getUpscanInitiateConfig[Long]("max-file-size")

  override def initiate(
    errorRedirect: Call,
    successRedirect: Call,
    uploadReference: UploadReference
  )(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, HttpResponse] = {

    val payload = UpscanInitiateRequest(
      backEndBaseUrl + s"/cgt-property-disposals/upscan-call-back/upload-reference/${uploadReference.value}",
      selfBaseUrl + successRedirect.url,
      selfBaseUrl + errorRedirect.url,
      0,
      maxFileSize
    )

    logger.info(
      "make upscan initiate call with " +
        s"call back url ${payload.callbackUrl} " +
        s"| success redirect url ${payload.successRedirect} " +
        s"| error redirect url ${payload.errorRedirect}"
    )

    EitherT[Future, Error, HttpResponse](
      http
        .post[UpscanInitiateRequest](
          upscanInitiateUrl,
          payload,
          Map(USER_AGENT -> "cgt-property-disposals-frontend")
        )
        .map[Either[Error, HttpResponse]] { response =>
          if (response.status != Status.OK) {
            logger.warn(
              s"could not initiate upscan: received http " +
                s"status ${response.status} and body ${response.body}"
            )
            Left(Error("could not initiate upscan"))
          } else
            Right(response)
        }
        .recover { case e => Left(Error(e)) }
    )
  }

  override def getUpscanUpload(
    uploadReference: UploadReference
  )(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, HttpResponse] = {

    val url =
      backEndBaseUrl + s"/cgt-property-disposals/upscan/upload-reference/${uploadReference.value}"

    EitherT[Future, Error, HttpResponse](
      http
        .get(url)
        .map[Either[Error, HttpResponse]] { response =>
          if (response.status != Status.OK) {
            logger.warn(
              s"could not get upscan upload: received http " +
                s"status ${response.status} and body ${response.body}"
            )
            Left(Error("could not get upscan upload"))
          } else
            Right(response)
        }
        .recover { case e => Left(Error(e)) }
    )
  }

  override def saveUpscanUpload(
    upscanUpload: UpscanUpload
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, HttpResponse] = {
    val url = backEndBaseUrl + s"/cgt-property-disposals/upscan"

    EitherT[Future, Error, HttpResponse](
      http
        .post[UpscanUpload](url, upscanUpload)
        .map { response =>
          response.status match {
            case Status.OK                                         => Right(response)
            case Status.BAD_REQUEST | Status.INTERNAL_SERVER_ERROR =>
              logger.warn("could not save upscan upload")
              Left(Error(s"failed to save upscan upload"))
          }
        }
        .recover { case e => Left(Error(e)) }
    )
  }

}
