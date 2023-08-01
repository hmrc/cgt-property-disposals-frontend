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
import cats.syntax.eq._
import play.api.data.FormError
import play.api.data.format.Formatter
import play.api.libs.json.{Json, OFormat}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Country.CountryCode

import scala.io.{Codec, Source}

final case class Country(
  code: CountryCode
)

object Country {

  val uk: Country = Country("GB")

  implicit val countryFormat: OFormat[Country] = Json.format[Country]

  implicit val eq: Eq[Country] = Eq.fromUniversalEquals

  type CountryCode = String

  val countryCodes: List[CountryCode] = {
    val source = Source.fromInputStream(
      getClass.getResourceAsStream("/resources/countries.txt")
    )(Codec.UTF8)
    try source.getLines().toList
    finally source.close()
  }

  implicit class CountryOps(private val c: Country) extends AnyVal {
    def isUk(): Boolean = c === Country.uk
  }

  val formatter: Formatter[Country] = new Formatter[Country] {
    override def bind(
      key: String,
      data: Map[String, String]
    ): Either[Seq[FormError], Country] =
      data.get(key).filter(_.nonEmpty) match {
        case Some(c) =>
          if (countryCodes.contains(c)) Right(Country(c))
          else Left(Seq(FormError(key, "error.notFound")))
        case None    => Left(Seq(FormError(key, "error.required")))
      }

    override def unbind(key: String, value: Country): Map[String, String] =
      Map(key -> value.code)
  }

}
