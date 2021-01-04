/*
 * Copyright 2021 HM Revenue & Customs
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

import play.api.data.Forms.text
import play.api.data.Mapping
import play.api.data.validation.{Constraint, Invalid, Valid, ValidationResult}
import play.api.libs.json.{Json, OFormat}

final case class CgtReference(value: String) extends AnyVal

object CgtReference {

  implicit val format: OFormat[CgtReference] = Json.format

  val mapping: Mapping[CgtReference] = {
    val regexPredicate = "^(X[A-Z]CGTP[0-9]{9})$".r.pattern.asPredicate()

    def validateCgtReference(s: String): ValidationResult =
      if (s.length > 15) Invalid("error.tooLong")
      else if (s.isEmpty) Invalid("error.required")
      else if (s.length < 15) Invalid("error.tooShort")
      else if (s.exists(!_.isLetterOrDigit)) Invalid("error.invalidCharacters")
      else if (!regexPredicate.test(s)) Invalid("error.pattern")
      else Valid

    text
      .transform[String](_.replaceAllLiterally(" ", ""), identity)
      .verifying(Constraint(validateCgtReference(_)))
      .transform[CgtReference](CgtReference(_), _.value)
  }

}
