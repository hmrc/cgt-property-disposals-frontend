/*
 * Copyright 2022 HM Revenue & Customs
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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids

import play.api.data.Forms.nonEmptyText
import play.api.data.Mapping
import play.api.data.validation.{Constraint, Invalid, Valid, ValidationResult}
import play.api.libs.json.{Format, Json}

final case class TRN(value: String) extends AnyVal

object TRN {

  implicit val format: Format[TRN] = Json.format[TRN]

  val mapping: Mapping[String] = {
    val regexPredicate = "^[a-zA-Z0-9]{15}$".r.pattern.asPredicate()

    def validateTrn(s: String): ValidationResult =
      if (s.length > 15) Invalid("error.tooLong")
      else if (s.length < 15) Invalid("error.tooShort")
      else if (!regexPredicate.test(s)) Invalid("error.pattern")
      else Valid

    nonEmptyText
      .transform[String](_.trim, identity)
      .verifying(Constraint { t: String =>
        validateTrn(t.replaceAllLiterally(" ", ""))
      })
  }

}
