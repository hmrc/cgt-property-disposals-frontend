/*
 * Copyright 2023 HM Revenue & Customs
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
import play.api.libs.json.{Json, OFormat}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.AddressLookupConnector._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Error
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Postcode
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.UKAddressLookupServiceImpl.AddressLookupResponse
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging
import uk.gov.hmrc.http.HttpReads.Implicits._
import uk.gov.hmrc.http.client.HttpClientV2
import uk.gov.hmrc.http.{HeaderCarrier, StringContextOps, UpstreamErrorResponse}
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import java.net.URLEncoder
import java.util.Locale
import scala.concurrent.{ExecutionContext, Future}
import scala.util.chaining.scalaUtilChainingOps

import play.api.libs.ws.writeableOf_JsValue

@ImplementedBy(classOf[AddressLookupConnectorImpl])
trait AddressLookupConnector {
  def lookupAddress(postcode: Postcode, filter: Option[String])(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, AddressLookupResponse]
}

@Singleton
class AddressLookupConnectorImpl @Inject() (
  http: HttpClientV2,
  servicesConfig: ServicesConfig
)(implicit
  ec: ExecutionContext
) extends AddressLookupConnector
    with Logging {
  private val url = servicesConfig.baseUrl("address-lookup") + "/lookup"

  private val headers = {
    val userAgent = servicesConfig.getString("microservice.services.address-lookup.user-agent")
    Seq("User-Agent" -> userAgent)
  }

  def lookupAddress(postcode: Postcode, filter: Option[String])(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, AddressLookupResponse] = {
    val body = LookupAddressByPostcode(
      postcode.value.replace(" ", "").toUpperCase(Locale.UK),
      filter.map(f => URLEncoder.encode(f, "UTF-8"))
    )
    http
      .post(url"$url")
      .withBody(Json.toJson(body))
      .setHeader(headers*)
      .execute[Either[UpstreamErrorResponse, AddressLookupResponse]]
      .map(_.left.map { error =>
        logger.error(s"POST to $url failed", error)
        Error(s"Response to address lookup came back with status ${error.statusCode}")
      })
      .recover(e => Left(Error(e.getMessage)))
      .pipe(EitherT(_))
  }
}

object AddressLookupConnector {
  final case class LookupAddressByPostcode(postcode: String, filter: Option[String])

  object LookupAddressByPostcode {
    implicit val writes: OFormat[LookupAddressByPostcode] = Json.format[LookupAddressByPostcode]
  }
}
