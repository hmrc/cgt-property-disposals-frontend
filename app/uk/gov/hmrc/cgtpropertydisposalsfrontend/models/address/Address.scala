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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address

import cats.Eq
import play.api.data.Forms.{mapping => formMapping, nonEmptyText, number, of, optional, text}
import play.api.data.validation.{Constraint, Invalid, Valid, ValidationResult}
import play.api.data.{Form, Mapping}
import play.api.i18n.Messages
import play.api.libs.json._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.BooleanFormatter

sealed trait Address extends Product with Serializable

object Address {

  val addressLineAllowedCharacters: List[Char] =
    ('A' to 'Z').toList ::: ('a' to 'z').toList ::: ('0' to '9').toList :::
      List(' ', '-', ',', '.', '&', '\'', '/')

  val addressLineMaxLength: Int = 35

  final case class UkAddress(
    line1: String,
    line2: Option[String],
    town: Option[String],
    county: Option[String],
    postcode: Postcode
  ) extends Address {
    val countryCode: String = Country.uk.code
  }

  final case class NonUkAddress(
    line1: String,
    line2: Option[String],
    line3: Option[String],
    line4: Option[String],
    postcode: Option[String],
    country: Country
  ) extends Address

  implicit val ukAddressFormat: OFormat[UkAddress]       = Json.format[UkAddress]
  implicit val nonUkAddressFormat: OFormat[NonUkAddress] = Json.format[NonUkAddress]

  implicit val format: OFormat[Address] = new OFormat[Address] {
    override def reads(json: JsValue): JsResult[Address] =
      (json \ "UkAddress", json \ "NonUkAddress") match {
        case (JsDefined(ukJson), _)    => ukJson.validate[UkAddress]
        case (_, JsDefined(nonUkJson)) => nonUkJson.validate[NonUkAddress]
        case _                         =>
          (json \ "_type").validate[String] match {
            case JsSuccess("uk", _)    => json.validate[UkAddress]
            case JsSuccess("nonUk", _) => json.validate[NonUkAddress]
            case JsSuccess(other, _)   => JsError(s"Unknown _type: $other")
            case JsError(_)            =>
              if ((json \ "countryCode").isDefined) json.validate[NonUkAddress]
              else json.validate[UkAddress]
          }
      }

    override def writes(address: Address): JsObject = address match {
      case uk: UkAddress       => Json.obj("UkAddress" -> Json.toJson(uk)(ukAddressFormat))
      case nonUk: NonUkAddress => Json.obj("NonUkAddress" -> Json.toJson(nonUk)(nonUkAddressFormat))
    }
  }

  implicit val eq: Eq[Address] = Eq.fromUniversalEquals

  // address is selected by the index of the address in the given list
  def addressSelectForm(addresses: List[Address]): Form[Address] =
    Form(
      formMapping(
        "address-select" -> number
          .verifying("invalid", i => i >= 0 && i < addresses.size)
          .transform[Address](addresses.apply, addresses.indexOf(_))
      )(identity)(Some(_))
    )

  val addressLineMapping: Mapping[String] = {

    def validateAddressLine(s: String): ValidationResult =
      if (s.length > addressLineMaxLength) {
        Invalid("error.tooLong")
      } else if (!s.forall(addressLineAllowedCharacters.contains(_))) {
        Invalid("error.pattern")
      } else {
        Valid
      }

    nonEmptyText
      .transform[String](_.trim, identity)
      .verifying(Constraint[String](validateAddressLine))
  }

  val ukAddressForm: Form[UkAddress] =
    Form(
      formMapping(
        "address-line1"  -> addressLineMapping,
        "address-line2"  -> optional(addressLineMapping),
        "address-town"   -> optional(addressLineMapping),
        "address-county" -> optional(addressLineMapping),
        "postcode"       -> Postcode.mapping
      )(UkAddress.apply)(o => Some((o.line1, o.line2, o.town, o.county, o.postcode)))
    )

  val nonUkAddressForm: Form[NonUkAddress] =
    Form(
      formMapping(
        "nonUkAddress-line1" -> addressLineMapping,
        "nonUkAddress-line2" -> optional(addressLineMapping),
        "nonUkAddress-line3" -> optional(addressLineMapping),
        "nonUkAddress-line4" -> optional(addressLineMapping),
        "postcode"           -> optional(text),
        "countryCode"        -> of(Country.formatter)
      )(NonUkAddress.apply)(o => Some((o.line1, o.line2, o.line3, o.line4, o.postcode, o.country)))
    )

  val isUkForm: Form[Boolean] =
    Form(
      formMapping(
        "isUk" -> of(BooleanFormatter.formatter)
      )(identity)(Some(_))
    )

  implicit class AddressOps(private val a: Address) extends AnyVal {
    def getAddressLines(implicit messages: Messages): List[String] = {
      val lines = a match {
        case u: UkAddress    => List(Some(u.line1), u.line2, u.town, u.county, Some(u.postcode.value))
        case n: NonUkAddress =>
          List(Some(n.line1), n.line2, n.line3, n.line4, messages.translate(s"country.${n.country.code}", Seq.empty))
      }
      lines.collect { case Some(s) => s }
    }
  }

}
