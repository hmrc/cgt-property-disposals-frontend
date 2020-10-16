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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name

import play.api.data.Forms.nonEmptyText
import play.api.data.Mapping
import play.api.data.validation.{Constraint, Invalid, Valid, ValidationResult}
import play.api.libs.json.{Format, Json}

final case class TrustName(value: String) extends AnyVal

object TrustName {

  implicit val format: Format[TrustName] = Json.format[TrustName]

  val mapping: Mapping[String] = {
    val regexPredicate =
      "^[a-zA-Z0-9 &`\\-\\'^]{1,105}$".r.pattern.asPredicate()

    def validateTrustName(s: String): ValidationResult =
      if (s.length > 105) Invalid("error.tooLong")
      else if (!regexPredicate.test(s)) Invalid("error.pattern")
      else Valid

    nonEmptyText
      .transform[String](_.trim, identity)
      .verifying(Constraint[String](validateTrustName(_)))
  }

}
