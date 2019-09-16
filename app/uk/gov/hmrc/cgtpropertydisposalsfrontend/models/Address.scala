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
import cats.syntax.eq._
import cats.instances.string._
import julienrf.json.derived
import play.api.data.{Form, FormError}
import play.api.data.Forms.{mapping, nonEmptyText, number, of, optional, text}
import play.api.data.format.Formatter
import play.api.libs.json.{Json, OFormat}

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
    countryCode: Country
  ) extends Address

  final case class Country(
    code: String,
    name: String
  )

  implicit val countryFormat: OFormat[Country] = Json.format[Country]

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
        "postcode" -> nonEmptyText
      )(UkAddress.apply)(UkAddress.unapply)
    )

  def nonUkAddressForm: Form[NonUkAddress] = {
    val countryFormatter = new Formatter[Country] {
      override def bind(key: String, data: Map[String, String]): Either[Seq[FormError], Country] = data.get(key) match {
        case Some(c) => Either.fromOption(CountryCodes.find(_.code === c), Seq(FormError(key, "error.notFound")))
        case None => Left(Seq(FormError(key, "error.required")))
      }

      override def unbind(key: String, value: Country): Map[String, String] = Map(key -> value.code)
    }
    Form(
      mapping(
        "nonUkAddress-line1"  -> nonEmptyText,
        "nonUkAddress-line2"  -> optional(text),
        "nonUkAddress-line3"   -> optional(text),
        "nonUkAddress-line4" -> optional(text),
        "postcode" -> optional(text),
        "countryCode" -> of(countryFormatter)
      )(NonUkAddress.apply)(NonUkAddress.unapply)
    )
  }

  def isUkForm: Form[Boolean] =
    Form(
      mapping(
        "isUk" -> of(BooleanFormat.formatter)
      )(identity)(Some(_))
    )

  val CountryCodes: List[Country] = List(
    Country("AD","Andorra"),
    Country("AE","United Arab Emirates"),
    Country("AF","Afghanistan"),
    Country("AG","Antigua and Barbuda"),
    Country("AI","Anguilla"),
    Country("AL","Albania"),
    Country("AM","Armenia"),
    Country("AO","Angola"),
    Country("AQ","Antarctica"),
    Country("AR","Argentina"),
    Country("AS","American Samoa"),
    Country("AT","Austria"),
    Country("AU","Australia"),
    Country("AW","Aruba"),
    Country("AX","Åland Islands"),
    Country("AZ","Azerbaijan"),
    Country("BA","Bosnia and Herzegovina"),
    Country("BB","Barbados"),
    Country("BD","Bangladesh"),
    Country("BE","Belgium"),
    Country("BF","Burkina Faso"),
    Country("BG","Bulgaria"),
    Country("BH","Bahrain"),
    Country("BI","Burundi"),
    Country("BJ","Benin"),
    Country("BL","Saint Barthélemy"),
    Country("BM","Bermuda"),
    Country("BN","Brunei"),
    Country("BO","Bolivia"),
    Country("BR","Brazil"),
    Country("BS","The Bahamas"),
    Country("BT","Bhutan"),
    Country("BV","Bouvet Island"),
    Country("BW","Botswana"),
    Country("BY","Belarus"),
    Country("BZ","Belize"),
    Country("CA","Canada"),
    Country("CC","Cocos (Keeling) Islands"),
    Country("CD","Congo (Democratic Republic)"),
    Country("CF","Central African Republic"),
    Country("CG","Congo"),
    Country("CH","Switzerland"),
    Country("CI","Ivory Coast"),
    Country("CK","Cook Islands"),
    Country("CL","Chile"),
    Country("CM","Cameroon"),
    Country("CN","China"),
    Country("CO","Colombia"),
    Country("CR","Costa Rica"),
    Country("CS","Czechoslovakia"),
    Country("CU","Cuba"),
    Country("CV","Cape Verde"),
    Country("CW","Curaçao"),
    Country("CX","Christmas Island"),
    Country("CY","Cyprus"),
    Country("CZ","Czechia"),
    Country("DE","Germany"),
    Country("DJ","Djibouti"),
    Country("DK","Denmark"),
    Country("DM","Dominica"),
    Country("DO","Dominican Republic"),
    Country("DZ","Algeria"),
    Country("EC","Ecuador"),
    Country("EE","Estonia"),
    Country("EG","Egypt"),
    Country("EH","Western Sahara"),
    Country("ER","Eritrea"),
    Country("ES","Spain"),
    Country("ET","Ethiopia"),
    Country("FI","Finland"),
    Country("FJ","Fiji"),
    Country("FK","Falkland Islands"),
    Country("FM","Micronesia"),
    Country("FO","Faroe Islands"),
    Country("FR","France"),
    Country("GA","Gabon"),
    Country("GD","Grenada"),
    Country("GE","Georgia"),
    Country("GF","French Guiana"),
    Country("GG","Guernsey"),
    Country("GH","Ghana"),
    Country("GI","Gibraltar"),
    Country("GL","Greenland"),
    Country("GM","The Gambia"),
    Country("GN","Guinea"),
    Country("GP","Guadeloupe"),
    Country("GQ","Equatorial Guinea"),
    Country("GR","Greece"),
    Country("GS","South Georgia and South Sandwich Islands"),
    Country("GT","Guatemala"),
    Country("GU","Guam"),
    Country("GW","Guinea-Bissau"),
    Country("GY","Guyana"),
    Country("HK","Hong Kong"),
    Country("HM","Heard Island and McDonald Islands"),
    Country("HN","Honduras"),
    Country("HR","Croatia"),
    Country("HT","Haiti"),
    Country("HU","Hungary"),
    Country("ID","Indonesia"),
    Country("IE","Ireland"),
    Country("IL","Israel"),
    Country("IM","Isle of Man"),
    Country("IN","India"),
    Country("IO","British Indian Ocean Territory"),
    Country("IQ","Iraq"),
    Country("IR","Iran"),
    Country("IS","Iceland"),
    Country("IT","Italy"),
    Country("JE","Jersey"),
    Country("JM","Jamaica"),
    Country("JO","Jordan"),
    Country("JP","Japan"),
    Country("KE","Kenya"),
    Country("KG","Kyrgyzstan"),
    Country("KH","Cambodia"),
    Country("KI","Kiribati"),
    Country("KM","Comoros"),
    Country("KN","St Kitts and Nevis"),
    Country("KP","North Korea"),
    Country("KR","South Korea"),
    Country("KW","Kuwait"),
    Country("KY","Cayman Islands"),
    Country("KZ","Kazakhstan"),
    Country("LA","Laos"),
    Country("LB","Lebanon"),
    Country("LC","St Lucia"),
    Country("LI","Liechtenstein"),
    Country("LK","Sri Lanka"),
    Country("LR","Liberia"),
    Country("LS","Lesotho"),
    Country("LT","Lithuania"),
    Country("LU","Luxembourg"),
    Country("LV","Latvia"),
    Country("LY","Libya"),
    Country("MA","Morocco"),
    Country("MC","Monaco"),
    Country("MD","Moldova"),
    Country("ME","Montenegro"),
    Country("MF","Saint-Martin (French part)"),
    Country("MG","Madagascar"),
    Country("MH","Marshall Islands"),
    Country("MK","North Macedonia"),
    Country("ML","Mali"),
    Country("MM","Myanmar (Burma)"),
    Country("MN","Mongolia"),
    Country("MO","Macao"),
    Country("MP","Northern Mariana Islands"),
    Country("MQ","Martinique"),
    Country("MR","Mauritania"),
    Country("MS","Montserrat"),
    Country("MT","Malta"),
    Country("MU","Mauritius"),
    Country("MV","Maldives"),
    Country("MW","Malawi"),
    Country("MX","Mexico"),
    Country("MY","Malaysia"),
    Country("MZ","Mozambique"),
    Country("NA","Namibia"),
    Country("NC","New Caledonia"),
    Country("NE","Niger"),
    Country("NF","Norfolk Island"),
    Country("NG","Nigeria"),
    Country("NI","Nicaragua"),
    Country("NL","Netherlands"),
    Country("NO","Norway"),
    Country("NP","Nepal"),
    Country("NR","Nauru"),
    Country("NU","Niue"),
    Country("NZ","New Zealand"),
    Country("OM","Oman"),
    Country("PA","Panama"),
    Country("PE","Peru"),
    Country("PF","French Polynesia"),
    Country("PG","Papua New Guinea"),
    Country("PH","Philippines"),
    Country("PK","Pakistan"),
    Country("PL","Poland"),
    Country("PM","Saint Pierre and Miquelon"),
    Country("PN","Pitcairn, Henderson, Ducie and Oeno Islands"),
    Country("PR","Puerto Rico"),
    Country("PS","Occupied Palestinian Territories"),
    Country("PT","Portugal"),
    Country("PW","Palau"),
    Country("PY","Paraguay"),
    Country("QA","Qatar"),
    Country("RE","Réunion"),
    Country("RO","Romania"),
    Country("RS","Serbia"),
    Country("RU","Russia"),
    Country("RW","Rwanda"),
    Country("SA","Saudi Arabia"),
    Country("SB","Solomon Islands"),
    Country("SC","Seychelles"),
    Country("SD","Sudan"),
    Country("SE","Sweden"),
    Country("SG","Singapore"),
    Country("SI","Slovenia"),
    Country("SJ","Svalbard and Jan Mayen"),
    Country("SK","Slovakia"),
    Country("SL","Sierra Leone"),
    Country("SM","San Marino"),
    Country("SN","Senegal"),
    Country("SO","Somalia"),
    Country("SR","Suriname"),
    Country("SS","South Sudan"),
    Country("ST","Sao Tome and Principe"),
    Country("SV","El Salvador"),
    Country("SX","Sint Maarten (Dutch part)"),
    Country("SY","Syria"),
    Country("SZ","Eswatini"),
    Country("TC","Turks and Caicos Islands"),
    Country("TD","Chad"),
    Country("TF","French Southern Territories"),
    Country("TG","Togo"),
    Country("TH","Thailand"),
    Country("TJ","Tajikistan"),
    Country("TK","Tokelau"),
    Country("TL","East Timor"),
    Country("TM","Turkmenistan"),
    Country("TN","Tunisia"),
    Country("TO","Tonga"),
    Country("TR","Turkey"),
    Country("TT","Trinidad and Tobago"),
    Country("TV","Tuvalu"),
    Country("TW","Taiwan"),
    Country("TZ","Tanzania"),
    Country("UA","Ukraine"),
    Country("UG","Uganda"),
    Country("US","United States"),
    Country("UY","Uruguay"),
    Country("UZ","Uzbekistan"),
    Country("VA","Vatican City"),
    Country("VC","St Vincent"),
    Country("VE","Venezuela"),
    Country("VG","British Virgin Islands"),
    Country("VI","United States Virgin Islands"),
    Country("VN","Vietnam"),
    Country("VU","Vanuatu"),
    Country("WF","Wallis and Futuna"),
    Country("WS","Samoa"),
    Country("YE","Yemen"),
    Country("YT","Mayotte"),
    Country("ZA","South Africa"),
    Country("ZM","Zambia"),
    Country("ZW","Zimbabwe")
  )

}
