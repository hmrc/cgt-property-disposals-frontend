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

import play.api.libs.json.{Json, OFormat}
import scala.io.Source

final case class Country(
  code: String,
  name: Option[String]
)

object Country {

  implicit val countryFormat: OFormat[Country] = Json.format[Country]

  val countries: List[Country] = {
    val source = Source.fromInputStream(getClass.getResourceAsStream("/resources/countries.json"))
    try {
      val jsonString = source.getLines().toList.mkString("")
      Json.parse(jsonString).validate[List[Country]].fold(e => sys.error(s"could not parse countries.json file: $e"), identity)
    } finally {
      source.close()
    }
  }

}
