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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.models

import java.time.LocalDate

import cats.syntax.either._
import play.api.data.FormError
import play.api.data.Forms.number
import play.api.data.format.Formatter

import scala.util.Try

object LocalDateUtils {

  private def errorResult(key: String, errorMessage: String): Seq[FormError] =
    Seq(FormError(key, errorMessage))

  private def dateFieldFormatter(maxValue: Option[Int]): Formatter[Int] =
    new Formatter[Int] {
      override def bind(key: String, data: Map[String, String]): Either[Seq[FormError], Int] =
        data
          .get(key)
          .fold[Either[Seq[FormError], Int]](
            Left(errorResult(key, "error.required"))
          ) { stringValue =>
            Either.fromOption(
              Try(BigDecimal(stringValue).toIntExact).toOption.filter(
                i => i > 0 && maxValue.forall(i <= _)
              ),
              errorResult(key, "error.invalid")
            )
          }

      override def unbind(key: String, value: Int): Map[String, String] = number.withPrefix(key).unbind(value)
    }

  val dayOfMonthFormatter: Formatter[Int] = dateFieldFormatter(maxValue = Some(31))

  val monthFormatter: Formatter[Int] = dateFieldFormatter(maxValue = Some(12))

  val yearFormatter: Formatter[Int] = dateFieldFormatter(maxValue = None)

  def dateOfBirthFormatter(maximumDateInclusive: LocalDate,
                           dayKey: String,
                           monthKey:String,
                           yearKey: String,
                           dateKey: String
                          ): Formatter[LocalDate] = new Formatter[LocalDate] {
    override def bind(key: String, data: Map[String, String]): Either[Seq[FormError], LocalDate] = {
      val dob: Option[LocalDate] = for {
        day ← data.get(dayKey).map(_.trim)
        month ← data.get(monthKey).map(_.trim)
        year ← data.get(yearKey).map(_.trim)
        dob ← Try(LocalDate.of(year.toInt, month.toInt, day.toInt)).toOption
      } yield dob

      Either.fromOption(dob, errorResult(dateKey, "error.invalid"))
        .flatMap( date =>
          if(date.isAfter(maximumDateInclusive)) Left(errorResult(dateKey, "error.tooFarInFuture"))
          else Right(date)
      )
    }

    override def unbind(key: String, value: LocalDate): Map[String, String] = Map.empty[String, String]
  }

}
