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

import play.api.data.Forms.nonEmptyText
import play.api.data.Mapping
import play.api.data.validation.{Constraint, Invalid, Valid, ValidationResult}
import play.api.libs.functional.syntax._
import play.api.libs.json.Format

import java.util.Locale
import java.util.function.Predicate

final case class Postcode(value: String) extends AnyVal

object Postcode {

  val postcodeRegexPredicate: Predicate[String] =
    "^([A-Z]{1,2}[\\d]{1,2}[A-Z]?)\\s*([\\d][A-Z]{2})$|BFPO\\s?(\\d\\s*){1,3}$".r.pattern
      .asPredicate()

  implicit val format: Format[Postcode] =
    implicitly[Format[String]].inmap(Postcode(_), _.value)

  val mapping: Mapping[Postcode] = {

    def validatePostcode(p: Postcode): ValidationResult =
      if (!postcodeRegexPredicate.test(p.value.toUpperCase(Locale.UK))) {
        Invalid("error.pattern")
      } else {
        Valid
      }

    nonEmptyText
      .transform[Postcode](p => Postcode(p.trim), _.value)
      .verifying(Constraint[Postcode](validatePostcode(_)))
  }

}
