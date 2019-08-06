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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.services

import cats.instances.either._
import cats.instances.int._
import cats.instances.list._
import cats.instances.string._
import cats.syntax.either._
import cats.syntax.eq._
import cats.syntax.traverse._

import com.google.inject.{Inject, Singleton}
import play.api.http.Status.OK
import play.api.libs.json.{JsResult, JsValue, Json, Reads}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.AddressLookupConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Address.{NonUkAddress, UkAddress}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Address, AddressLookupResult, Error, Postcode}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.AddressLookupServiceImpl.{AddressLookupResponse, RawAddress}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.HttpResponseOps._
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

trait AddressLookupService {

  def lookupAddress(postcode: Postcode)(implicit hc: HeaderCarrier): Future[Either[Error, AddressLookupResult]]

}

@Singleton
class AddressLookupServiceImpl @Inject() (connector: AddressLookupConnector)(implicit ec: ExecutionContext)
  extends AddressLookupService {

  override def lookupAddress(postcode: Postcode)(implicit hc: HeaderCarrier): Future[Either[Error, AddressLookupResult]] =
    connector.lookupAddress(postcode).map { response =>
      if (response.status === OK) {
        response.parseJSON[AddressLookupResponse]()
          .flatMap(toAddressLookupResult(_, postcode))
          .leftMap(Error(_))
      } else {
        Left(Error(s"Response to address lookup came back with status ${response.status}"))
      }
    }.recover {
      case e => Left(Error(e))
    }

  def toAddressLookupResult(r: AddressLookupResponse, postcode: Postcode): Either[String, AddressLookupResult] = {
      def toAddress(a: RawAddress): Either[String, Address] = {
        val lines: Either[String, (String, Option[String], Option[String], Option[String])] =
          (a.lines ::: List(a.town, a.county.getOrElse(""))).filter(_.nonEmpty) match {
            case Nil                         => Left("Could not find any lines of addesss")
            case a1 :: Nil                   => Right((a1, None, None, None))
            case a1 :: a2 :: Nil             => Right((a1, Some(a2), None, None))
            case a1 :: a2 :: a3 :: Nil       => Right((a1, Some(a2), Some(a3), None))
            case a1 :: a2 :: a3 :: a4 :: Nil => Right((a1, Some(a2), Some(a3), Some(a4)))
            case a1 :: a2 :: a3 :: as        => Right((a1, Some(a2), Some(a3), Some(as.mkString(", "))))
          }

        lines.map { case (l1, l2, l3, l4) =>
          if (a.country.code === "GB") {
            UkAddress(l1, l2, l3, l4, a.postcode)
          } else {
            NonUkAddress(l1, l2, l3, l4, Some(a.postcode), a.country.code)
          }
        }
      }

    r.addresses.map(toAddress)
      .sequence[Either[String, ?], Address]
      .map(AddressLookupResult(postcode, _))
  }

}

object AddressLookupServiceImpl {

  final case class Country(code: String)

  final case class RawAddress(
      lines: List[String],
      town: String,
      county: Option[String],
      postcode: String,
      country: Country
  )

  final case class AddressLookupResponse(addresses: List[RawAddress])

  implicit val addressLookupResponseReads: Reads[AddressLookupResponse] = new Reads[AddressLookupResponse] {
    case class Inner(address: RawAddress)

    implicit val countryReads: Reads[Country] = Json.reads[Country].map { c =>
      if (c.code === "UK") Country("GB") else c
    }

    implicit val rawAddressReads: Reads[RawAddress] = Json.reads

    implicit val innerReads: Reads[Inner] = Json.reads[Inner]

    override def reads(json: JsValue): JsResult[AddressLookupResponse] =
      json.validate[List[Inner]].map(l => AddressLookupResponse(l.map(_.address)))
  }

}
