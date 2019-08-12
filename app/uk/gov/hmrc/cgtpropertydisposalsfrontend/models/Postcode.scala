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

import cats.instances.char._
import cats.syntax.eq._
import play.api.data.Form
import play.api.data.Forms.{ mapping, text }
import play.api.libs.functional.syntax._
import play.api.libs.json.Format

final case class Postcode(value: String) extends AnyVal

object Postcode {

  implicit val format: Format[Postcode] = implicitly[Format[String]].inmap(Postcode(_), _.value)

  val form: Form[Postcode] = Form(
    mapping(
      "postcode" -> text
        .transform[String](_.trim, identity)
        .verifying("invalid", s => s.forall(c => c.isLetterOrDigit || c === ' ') && s.exists(_.isLetterOrDigit)))(Postcode.apply)(Postcode.unapply))

}
