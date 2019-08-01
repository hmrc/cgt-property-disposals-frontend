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

import play.api.data.Form
import play.api.data.Forms.{mapping, text}
import play.api.libs.functional.syntax._
import play.api.libs.json.Format

import scala.util.matching.Regex

final case class NINO(value: String) extends AnyVal

object NINO {

  implicit val format: Format[NINO] = implicitly[Format[String]].inmap(NINO(_), _.value)

  val ninoForm: Form[NINO] = {
    val ninoRegex: Regex = """^((?!(BG|GB|KN|NK|NT|TN|ZZ)|(D|F|I|Q|U|V)[A-Z]|[A-|Z](D|F|I|O|Q|U|V))[A-Z]{2})[0-9]{6}[A-D]$""".r

    Form(
      mapping(
        "nino" -> text.transform[String](_.replaceAllLiterally(" ", ""), identity).verifying("invalid", ninoRegex.pattern.matcher(_).matches())
      )(NINO.apply)(NINO.unapply)
    )
  }

}
