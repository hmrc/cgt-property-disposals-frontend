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
import play.api.data.format.Formatter

import scala.util.Try

object LocalDateUtils {

  private def errorResult(key: String, errorMessage: String): Seq[FormError] =
    Seq(FormError(key, errorMessage))

  def dateFormatter(maximumDateInclusive: LocalDate,
                    dayKey: String,
                    monthKey:String,
                    yearKey: String,
                    dateKey: String
                          ): Formatter[LocalDate] = new Formatter[LocalDate] {


    def getDateField(key: String, data: Map[String,String], maxValue: Option[Int]): Either[Seq[FormError], Int] =
      data
        .get(key)
        .map(_.trim())
        .filter(_.nonEmpty)
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

    override def bind(key: String, data: Map[String, String]): Either[Seq[FormError], LocalDate] = {
      for {
        day ← getDateField(dayKey, data, Some(31))
        month ← getDateField(monthKey, data, Some(12))
        year ← getDateField(yearKey, data, None)
        date ← Either.fromTry(Try(LocalDate.of(year, month, day)))
          .leftMap(_ => errorResult(dateKey, "error.invalid"))
          .flatMap( date =>
            if(date.isAfter(maximumDateInclusive)) Left(errorResult(dateKey, "error.tooFarInFuture"))
            else Right(date)
          )
      } yield  date
    }

    override def unbind(key: String, value: LocalDate): Map[String, String] = Map.empty[String, String]
  }

}
