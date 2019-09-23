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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.models

import cats.Eq
import cats.syntax.either._
import julienrf.json.derived
import play.api.data.Forms.{mapping, nonEmptyText, number, of, optional, text}
import play.api.data.format.Formatter
import play.api.data.{Form, FormError}
import play.api.libs.json.OFormat

sealed trait Address extends Product with Serializable

object Address {

  final case class UkAddress(
    line1: String,
    line2: Option[String],
    town: Option[String],
    county: Option[String],
    postcode: String
  ) extends Address {
    val countryCode: String = "GB"
  }

  final case class NonUkAddress(
    line1: String,
    line2: Option[String],
    line3: Option[String],
    line4: Option[String],
    postcode: Option[String],
    country: Country
  ) extends Address

  // the format instance using the play-json-derived-codecs library wraps
  // the case class inside a JsObject with case class type name as the key
  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  implicit val format: OFormat[Address] = derived.oformat

  implicit val eq: Eq[Address] = Eq.fromUniversalEquals

  // address is selected by the index of the address in the given list
  def addressSelectForm(addresses: List[Address]): Form[Address] =
    Form(
      mapping(
        "address-select" -> number
          .verifying("invalid", i => i >= 0 && i < addresses.size)
          .transform[Address](addresses.apply, addresses.indexOf(_))
      )(identity)(Some(_))
    )

  def ukAddressForm: Form[UkAddress] =
    Form(
      mapping(
        "address-line1"  -> nonEmptyText,
        "address-line2"  -> optional(text),
        "address-town"   -> optional(text),
        "address-county" -> optional(text),
        "postcode"       -> nonEmptyText
      )(UkAddress.apply)(UkAddress.unapply)
    )

  def nonUkAddressForm: Form[NonUkAddress] = {
    val countryFormatter = new Formatter[Country] {
      override def bind(key: String, data: Map[String, String]): Either[Seq[FormError], Country] = data.get(key) match {
        case Some(c) =>
          Either.fromOption(
            Country.countryCodeToCountryName.get(c).map(name => Country(c, Some(name))),
            Seq(FormError(key, "error.notFound"))
          )
        case None => Left(Seq(FormError(key, "error.required")))
      }

      override def unbind(key: String, value: Country): Map[String, String] = Map(key -> value.code)
    }
    Form(
      mapping(
        "nonUkAddress-line1" -> nonEmptyText,
        "nonUkAddress-line2" -> optional(text),
        "nonUkAddress-line3" -> optional(text),
        "nonUkAddress-line4" -> optional(text),
        "postcode"           -> optional(text),
        "countryCode"        -> of(countryFormatter)
      )(NonUkAddress.apply)(NonUkAddress.unapply)
    )
  }

  def isUkForm: Form[Boolean] =
    Form(
      mapping(
        "isUk" -> of(BooleanFormatter.formatter)
      )(identity)(Some(_))
    )

}
