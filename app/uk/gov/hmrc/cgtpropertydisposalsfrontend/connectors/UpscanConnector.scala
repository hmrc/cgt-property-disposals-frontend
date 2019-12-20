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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors

import cats.data.EitherT
import com.google.inject.{ImplementedBy, Inject, Singleton}
import configs.Configs
import configs.syntax._
import play.api.Configuration
import play.api.libs.json.{Json, OFormat, Reads}
import play.mvc.Http.Status
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.routes
import uk.gov.hmrc.cgtpropertydisposalsfrontend.http.HttpClient._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Error
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}
import uk.gov.hmrc.play.bootstrap.http.HttpClient

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

final case class UpscanInitiateRequest(
  callbackUrl: String,
  successRedirect: String,
  errorRedirect: String,
  minimumFileSize: Int,
  maximumFileSize: Long
)

final object UpscanInitiateRequest {
  implicit val format: OFormat[UpscanInitiateRequest] = Json.format[UpscanInitiateRequest]
}

@ImplementedBy(classOf[UpscanConnectorImpl])
trait UpscanConnector {

  def initiate()(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, HttpResponse]

}

@Singleton
class UpscanConnectorImpl @Inject()(http: HttpClient, config: Configuration) extends UpscanConnector with Logging {

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

  private val minFileSize: Int  = getUpscanInitiateConfig[Int]("min-file-size")
  private val maxFileSize: Long = getUpscanInitiateConfig[Long]("max-file-size")

  override def initiate()(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, HttpResponse] = {

    val payload = UpscanInitiateRequest(
      routes.UpscanController.callBack().url,
      routes.UpscanController.successCallBack().url,
      routes.UpscanController.errorCallBack().url,
      minFileSize,
      maxFileSize
    )

    EitherT[Future, Error, HttpResponse](
      http
        .post[UpscanInitiateRequest](url, payload)
        .map[Either[Error, HttpResponse]] { response =>
          if (response.status != Status.NO_CONTENT) {
            Left(Error("S3 did not return 204 status code"))
          } else {
            Right(response)
          }
        }
        .recover { case e => Left(Error(e)) }
    )

  }

}
