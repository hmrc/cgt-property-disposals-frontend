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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address

import java.util.function.Predicate

import cats.Eq
import julienrf.json.derived
import play.api.data.Forms.{nonEmptyText, number, of, optional, text, mapping => formMapping}
import play.api.data.validation.{Constraint, Invalid, Valid, ValidationResult}
import play.api.data.{Form, Mapping}
import play.api.i18n.Messages
import play.api.libs.json.OFormat
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.BooleanFormatter

sealed trait Address extends Product with Serializable

object Address {

  val addressLineRegexPredicate: Predicate[String] = "^[A-Za-z0-9 \\-,.&'/]{0,35}$".r.pattern.asPredicate()

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

  // the format instance using the play-json-derived-codecs library wraps
  // the case class inside a JsObject with case class type name as the key
  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  implicit val format: OFormat[Address] = derived.oformat()

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

    def validateName(s: String): ValidationResult =
      if (s.length > 35) Invalid("error.tooLong")
      else if (!addressLineRegexPredicate.test(s)) Invalid("error.pattern")
      else Valid

    nonEmptyText
      .transform[String](_.trim, identity)
      .verifying(Constraint[String](validateName(_)))
  }

  val ukAddressForm: Form[UkAddress] =
    Form(
      formMapping(
        "address-line1"  -> addressLineMapping,
        "address-line2"  -> optional(addressLineMapping),
        "address-town"   -> optional(addressLineMapping),
        "address-county" -> optional(addressLineMapping),
        "postcode"       -> Postcode.mapping
      )(UkAddress.apply)(UkAddress.unapply)
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
      )(NonUkAddress.apply)(NonUkAddress.unapply)
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
