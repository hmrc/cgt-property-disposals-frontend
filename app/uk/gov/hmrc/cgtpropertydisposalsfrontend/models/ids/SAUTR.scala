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

import cats.Eq
import cats.instances.string._
import cats.syntax.eq._
import play.api.data.Forms.nonEmptyText
import play.api.data.Mapping
import play.api.libs.json.{Format, Json}

final case class SAUTR(value: String) extends AnyVal

object SAUTR {

  implicit val format: Format[SAUTR] = Json.format[SAUTR]

  implicit val eq: Eq[SAUTR] = Eq.instance(_.value === _.value)

  val mapping: Mapping[SAUTR] = {
    val regexPredicate = "^[0-9]{10}$".r.pattern.asPredicate()
    nonEmptyText
      .transform[SAUTR](s => SAUTR(s.trim()), _.value)
      .verifying("error.pattern", s => regexPredicate.test(s.value))
  }

}
