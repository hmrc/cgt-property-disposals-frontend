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

import cats.syntax.either._
import java.time.{Clock, LocalDate}

import play.api.data.Forms.{mapping, of}
import play.api.data.format.Formatter
import play.api.data.{Form, FormError}
import play.api.libs.json.{Format, Json}

import scala.util.Try

final case class DateOfBirth(value: LocalDate) extends AnyVal

object DateOfBirth {

  implicit val format: Format[DateOfBirth] = Json.format

  object Ids {
    val day = "dob-day"
    val month = "dob-month"
    val year = "dob-year"
    val dob = "dob"
  }

  private val dateOfBirthFormatter: Formatter[LocalDate] = new Formatter[LocalDate] {
    def validatedFromBoolean[A](a: A)(predicate: A ⇒ Boolean, ifFalse: ⇒ String): Either[String, A] =
      if (predicate(a)) Right(a) else Left(ifFalse)

    override def bind(key: String, data: Map[String, String]): Either[Seq[FormError], LocalDate] = {
      lazy val today = LocalDate.now(Clock.systemUTC())
      lazy val todayMinus16Years = today.minusYears(16L)

      val dob: Option[LocalDate] = for {
        d <- data.get(Ids.day).map(_.trim)
        m <- data.get(Ids.month).map(_.trim)
        y <- data.get(Ids.year).map(_.trim)
        dob <- Try(LocalDate.of(y.toInt, m.toInt, d.toInt)).toOption
      } yield dob

      val result = for {
        d <- Either.fromOption(dob, "invalid")
        _ <- validatedFromBoolean(d)(d => d.isBefore(todayMinus16Years) || d.isEqual(todayMinus16Years), "invalid")
      } yield d

      result.leftMap(e => Seq(FormError(Ids.dob, e)))
    }

    override def unbind(key: String, value: LocalDate): Map[String, String] =
      Map(Ids.day -> value.getDayOfMonth.toString, Ids.month -> value.getMonthValue.toString, Ids.year -> value.getYear.toString)
  }

  val dobForm: Form[DateOfBirth] =
    Form(
      mapping(
        Ids.dob -> of(dateOfBirthFormatter)
      )(DateOfBirth.apply)(DateOfBirth.unapply)
    )

}
