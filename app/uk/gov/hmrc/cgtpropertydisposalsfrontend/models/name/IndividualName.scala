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

import cats.Eq
import cats.instances.string._
import cats.syntax.eq._
import play.api.data.Forms.{nonEmptyText, mapping => formMapping}
import play.api.data.validation.{Constraint, Invalid, Valid, ValidationResult}
import play.api.data.{Form, Mapping}
import play.api.libs.json.{Json, OFormat}

final case class IndividualName(firstName: String, lastName: String)

object IndividualName {

  implicit val format: OFormat[IndividualName] = Json.format[IndividualName]

  implicit val eq: Eq[IndividualName] = Eq.instance {
    case (n1, n2) => n1.firstName === n2.firstName && n1.lastName === n2.lastName
  }

  implicit class IndividualNameOps(private val name: IndividualName) extends AnyVal {
    def makeSingleName(): String = name.firstName + " " + name.lastName
  }

  val mapping: Mapping[String] = {
    val nameRegexPredicate =
      "^[a-zA-Z &`\\-'^]{1,35}$".r.pattern
        .asPredicate()

    def validateName(s: String): ValidationResult =
      if (s.length > 35) Invalid("error.tooLong")
      else if (!nameRegexPredicate.test(s)) Invalid("error.pattern")
      else Valid

    nonEmptyText
      .transform[String](_.trim, identity)
      .verifying(Constraint[String](validateName(_)))
  }

  def form(firstNameKey: String, lastNameKey: String): Form[IndividualName] =
    Form(
      formMapping(
        firstNameKey -> mapping,
        lastNameKey  -> mapping
      )(IndividualName.apply)(IndividualName.unapply)
    )
}
