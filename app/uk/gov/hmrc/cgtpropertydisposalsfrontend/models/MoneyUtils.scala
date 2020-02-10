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

import java.util.Locale

import cats.syntax.either._
import cats.syntax.eq._
import cats.instances.char._
import play.api.data.FormError
import play.api.data.format.Formatter

import scala.util.Try

object MoneyUtils {

  val maxAmountOfPounds: Double = 5e10

  val currencyFormatter = java.text.NumberFormat.getCurrencyInstance(Locale.UK)

  def amountInPoundsFormatter(isTooSmall: BigDecimal => Boolean, isTooLarge: BigDecimal => Boolean): Formatter[Double] =
    new Formatter[Double] {
      override def bind(key: String, data: Map[String, String]): Either[Seq[FormError], Double] = {
        def validatePercentage(d: BigDecimal): Either[FormError, BigDecimal] =
          if (isTooSmall(d)) Left(FormError(key, "error.tooSmall"))
          else if (isTooLarge(d)) Left(FormError(key, "error.tooLarge"))
          else if (NumberUtils.numberHasMoreThanNDecimalPlaces(d, 2)) Left(FormError(key, "error.tooManyDecimals"))
          else Right(d)

        val result = data
          .get(key)
          .map(_.trim().filter(c => c =!= ',' && c =!= '£'))
          .filter(_.nonEmpty)
          .fold[Either[FormError, BigDecimal]](Left(FormError(key, "error.required"))) { s =>
            Try(BigDecimal(s)).toEither
              .leftMap(_ => FormError(key, "error.invalid"))
              .flatMap(validatePercentage)
          }

        result.bimap(Seq(_), _.toDouble)
      }

      override def unbind(key: String, value: Double): Map[String, String] =
        Map(key -> value.toString)
    }

}
