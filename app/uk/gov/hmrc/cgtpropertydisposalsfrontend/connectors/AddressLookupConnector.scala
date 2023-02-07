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

import java.net.URLEncoder

import cats.data.EitherT
import com.google.inject.{ImplementedBy, Inject, Singleton}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Error
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Postcode
import uk.gov.hmrc.http.{HeaderCarrier, HttpClient, HttpResponse}
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig
import uk.gov.hmrc.http.HttpReads.Implicits._
import scala.concurrent.{ExecutionContext, Future}
import play.api.libs.json.{Json, OFormat}
import AddressLookupConnector._

@ImplementedBy(classOf[AddressLookupConnectorImpl])
trait AddressLookupConnector {

  def lookupAddress(postcode: Postcode, filter: Option[String])(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, HttpResponse]

}

@Singleton
class AddressLookupConnectorImpl @Inject() (
  http: HttpClient,
  servicesConfig: ServicesConfig
)(implicit
  ec: ExecutionContext
) extends AddressLookupConnector {

  val url: String =
    servicesConfig.baseUrl("address-lookup") + "/lookup"

  val headers: Seq[(String, String)] = {
    val userAgent = servicesConfig.getString(
      "microservice.services.address-lookup.user-agent"
    )
    Seq("User-Agent" -> userAgent)
  }

  override def lookupAddress(postcode: Postcode, filter: Option[String])(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, HttpResponse] = {

    val lookupAddressByPostcode = LookupAddressByPostcode(
      postcode.value.replaceAllLiterally(" ", "").toUpperCase,
      filter.map(f => URLEncoder.encode(f, "UTF-8"))
    )

    EitherT[Future, Error, HttpResponse](
      http
        .POST[LookupAddressByPostcode, HttpResponse](url, lookupAddressByPostcode, headers)
        .map(Right(_))
        .recover { case e => Left(Error(e)) }
    )
  }
}

object AddressLookupConnector {

  final case class LookupAddressByPostcode(postcode: String, filter: Option[String])

  object LookupAddressByPostcode {
    implicit val writes: OFormat[LookupAddressByPostcode] = Json.format[LookupAddressByPostcode]
  }

}
