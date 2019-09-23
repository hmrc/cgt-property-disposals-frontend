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

import play.api.data.{Form, Mapping}
import play.api.data.Forms.{mapping => formMapping, nonEmptyText}
import play.api.data.validation.{Constraint, Invalid, Valid, ValidationResult}
import play.api.libs.json.{Json, OFormat}

final case class Name(firstName: String, lastName: String)

object Name {

  implicit val format: OFormat[Name] = Json.format[Name]

  val mapping: Mapping[String] = {
    val nameRegexPredicate =
      "^[a-zA-Z &`\\-'^]{1,35}$".r.pattern
        .asPredicate()

    def validateName(s: String): ValidationResult =
      if(s.length > 35) Invalid("error.tooLong")
      else if(!nameRegexPredicate.test(s)) Invalid("error.pattern")
      else Valid

    nonEmptyText
      .transform[String](_.trim, identity)
      .verifying(Constraint[String](validateName(_)))
  }

  val form: Form[Name] =
    Form(
      formMapping(
        "firstName" -> mapping,
        "lastName" -> mapping
      )(Name.apply)(Name.unapply)
    )
}
