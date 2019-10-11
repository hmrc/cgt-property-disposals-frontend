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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name

import cats.Eq
import cats.instances.string._
import cats.syntax.eq._
import play.api.data.Forms.{nonEmptyText, mapping => formMapping}
import play.api.data.validation.{Constraint, Invalid, Valid, ValidationResult}
import play.api.data.{Form, Mapping}
import play.api.libs.functional.syntax._
import play.api.libs.json.Format

final case class ContactName(value: String) extends AnyVal

object ContactName {

  implicit val format: Format[ContactName] =
    implicitly[Format[String]].inmap(ContactName(_), _.value)

  implicit val eq: Eq[ContactName] = Eq.instance{
    case (n1, n2) => n1.value === n2.value
  }

  val mapping: Mapping[String] = {
    val regexPredicate = "^[a-zA-Z0-9 &,`\\-\'.^]{1,105}$".r.pattern.asPredicate()

    def validateContactName(s: String): ValidationResult =
      if(s.length > 105) Invalid("error.tooLong")
      else if(!regexPredicate.test(s)) Invalid("error.pattern")
      else Valid

    nonEmptyText
      .transform[String](_.trim, identity)
      .verifying(Constraint[String](validateContactName(_)))
  }

  val form: Form[ContactName] =
    Form(
      formMapping(
        "contactName" -> mapping
      )(ContactName.apply)(ContactName.unapply)
    )

}
