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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address

import java.util.function.Predicate

import play.api.data.Forms.nonEmptyText
import play.api.data.Mapping
import play.api.data.validation.{Constraint, Invalid, Valid, ValidationResult}
import play.api.libs.functional.syntax._
import play.api.libs.json.Format

final case class Postcode(value: String) extends AnyVal

object Postcode {

  val postcodeRegexPredicate: Predicate[String] =
    "^[A-Z]{1,2}[0-9][0-9A-Z]?\\s?[0-9][A-Z]{2}$|BFPO\\s?[0-9]{1,3}$".r.pattern
      .asPredicate()

  implicit val format: Format[Postcode] =
    implicitly[Format[String]].inmap(Postcode(_), _.value)

  val mapping: Mapping[Postcode] = {

    def validatePostcode(p: Postcode): ValidationResult = {
      val postcodeWithoutSpaces =
        p.value.toUpperCase.replaceAllLiterally(" ", "")
      if (p.value.length > 8) Invalid("error.tooLong")
      else if (!postcodeWithoutSpaces.forall(_.isLetterOrDigit))
        Invalid("error.invalidCharacters")
      else if (!postcodeRegexPredicate.test(postcodeWithoutSpaces))
        Invalid("error.pattern")
      else Valid
    }

    nonEmptyText
      .transform[Postcode](p => Postcode(p.trim), _.value)
      .verifying(Constraint[Postcode](validatePostcode(_)))
  }

}
