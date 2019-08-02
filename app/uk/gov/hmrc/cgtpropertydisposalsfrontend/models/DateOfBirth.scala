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

import java.time.{Clock, LocalDate}

import cats.Eq
import cats.syntax.either._
import play.api.data.Forms.{mapping, of}
import play.api.data.format.Formatter
import play.api.data.{Form, FormError}
import play.api.libs.functional.syntax._
import play.api.libs.json.Format

import scala.util.Try

final case class DateOfBirth(value: LocalDate) extends AnyVal

object DateOfBirth {

  implicit val format: Format[DateOfBirth] = implicitly[Format[LocalDate]].inmap(DateOfBirth(_), _.value)

}
