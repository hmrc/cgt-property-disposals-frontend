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
import cats.instances.either._
import cats.instances.future._
import cats.instances.int._
import cats.instances.list._
import cats.syntax.either._
import cats.syntax.eq._
import cats.syntax.traverse._
import com.google.inject.{ImplementedBy, Inject, Singleton}
import play.api.http.Status.OK
import play.api.libs.json.{JsResult, JsValue, Json, OFormat, Reads}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.AddressLookupConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.metrics.Metrics
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Error
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address.UkAddress
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.{Address, AddressLookupResult, Postcode}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Postcode.postcodeRegexPredicate
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.UKAddressLookupServiceImpl.{AddressLookupResponse, RawAddress}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.HttpResponseOps._
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[UKAddressLookupServiceImpl])
trait UKAddressLookupService {

  def lookupAddress(postcode: Postcode, filter: Option[String])(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, AddressLookupResult]

}

@Singleton
class UKAddressLookupServiceImpl @Inject() (
  connector: AddressLookupConnector,
  metrics: Metrics
)(implicit
  ec: ExecutionContext
) extends UKAddressLookupService {

  override def lookupAddress(
    postcode: Postcode,
    filter: Option[String]
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, AddressLookupResult] = {
    val timer = metrics.postcodeLookupTimer.time()

    connector.lookupAddress(postcode, filter).subflatMap { response =>
      timer.close()
      if (response.status === OK)
        response
          .parseJSON[AddressLookupResponse]()
          .flatMap(toAddressLookupResult(_, postcode, filter))
          .leftMap { e =>
            metrics.postcodeLookupErrorCounter.inc()
            Error(e)
          }
      else {
        metrics.postcodeLookupErrorCounter.inc()
        Left(
          Error(
            s"Response to address lookup came back with status ${response.status}"
          )
        )
      }
    }
  }

  def toAddressLookupResult(
    r: AddressLookupResponse,
    postcode: Postcode,
    filter: Option[String]
  ): Either[String, AddressLookupResult] = {

    def isValidAddressLine(s: String): Boolean =
      (s.length <= Address.addressLineMaxLength) &&
        s.forall(Address.addressLineAllowedCharacters.contains(_))

    def validateAddressFields(address: UkAddress): Boolean =
      List(
        isValidAddressLine(address.line1),
        postcodeRegexPredicate.test(address.postcode.value),
        address.line2.forall(isValidAddressLine),
        address.town.forall(isValidAddressLine),
        address.county.forall(isValidAddressLine)
      ).forall(identity)

    def toAddress(a: RawAddress): Either[String, UkAddress] = {
      val lines: Either[String, (String, Option[String])] = a.lines match {
        case Nil       => Left("Could not find any lines of address")
        case a1 :: Nil => Right(a1 -> None)
        case a1 :: as  => Right(a1 -> Some(as.mkString(", ")))
      }

      lines.map { case (l1, l2) =>
        UkAddress(l1, l2, Some(a.town), a.county, Postcode(a.postcode))
      }
    }

    r.addresses
      .map(toAddress)
      .sequence[Either[String, *], UkAddress]
      .map(addresses =>
        AddressLookupResult(
          postcode,
          filter,
          addresses.filter(validateAddressFields)
        )
      )
  }

}

object UKAddressLookupServiceImpl {

  final case class Country(code: String)

  final case class RawAddress(
    lines: List[String],
    town: String,
    county: Option[String],
    postcode: String
  )

  object RawAddress {
    implicit val format: OFormat[RawAddress] = Json.format[RawAddress]
  }

  final case class AddressLookupResponse(addresses: List[RawAddress])

  private final case class InnerAddress(address: RawAddress)

  implicit val addressLookupResponseReads: Reads[AddressLookupResponse] =
    new Reads[AddressLookupResponse] {
      implicit val reads: Reads[InnerAddress] = Json.reads[InnerAddress]

      override def reads(json: JsValue): JsResult[AddressLookupResponse] =
        json
          .validate[List[InnerAddress]]
          .map(l => AddressLookupResponse(l.map(_.address)))
    }

}
