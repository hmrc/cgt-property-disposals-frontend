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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.models.email

import cats.Eq
import cats.instances.string._
import cats.syntax.eq._
import play.api.data.Forms.nonEmptyText
import play.api.data.Mapping
import play.api.libs.functional.syntax._
import play.api.libs.json.Format

import java.util.function.Predicate

final case class Email(value: String) extends AnyVal

object Email {

  val emailRegex: Predicate[String] =
    """^[a-zA-Z0-9\.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$""".r.pattern
      .asPredicate()

  implicit val format: Format[Email] =
    implicitly[Format[String]].inmap(Email(_), _.value)

  implicit val eq: Eq[Email] = Eq.instance(_.value === _.value)

  val mapping: Mapping[Email] =
    nonEmptyText
      .transform[Email](s => Email(s), _.value)
      .verifying("invalid", e => emailRegex.test(e.value))
}
