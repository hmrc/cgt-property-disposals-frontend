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
import cats.instances.bigDecimal._
import cats.instances.char._
import play.api.data.Forms.{mapping, of}
import play.api.data.{Form, FormError}
import play.api.data.format.Formatter

import scala.util.Try

object MoneyUtils {

  val maxAmountOfPounds: Double = 5e10

  private val currencyFormatter = java.text.NumberFormat.getCurrencyInstance(Locale.UK)

  def formatAmountOfMoneyWithPoundSign(d: Double): String = currencyFormatter.format(d).stripSuffix(".00")

  def formatAmountOfMoneyWithoutPoundSign(d: Double): String = formatAmountOfMoneyWithPoundSign(d).stripPrefix("£")

  def validateAmountOfMoney(key: String, isTooSmall: BigDecimal => Boolean, isTooLarge: BigDecimal => Boolean)(
    s: String
  ): Either[FormError, BigDecimal] =
    Try(BigDecimal(cleanupAmountOfMoneyString(s))).toEither
      .leftMap(_ => FormError(key, "error.invalid"))
      .flatMap { d =>
        if (isTooSmall(d)) Left(FormError(key, "error.tooSmall"))
        else if (isTooLarge(d)) Left(FormError(key, "error.tooLarge"))
        else if (NumberUtils.numberHasMoreThanNDecimalPlaces(d, 2)) Left(FormError(key, "error.tooManyDecimals"))
        else Right(d)
      }

  def amountInPoundsFormatter(isTooSmall: BigDecimal => Boolean, isTooLarge: BigDecimal => Boolean): Formatter[Double] =
    new Formatter[Double] {
      override def bind(key: String, data: Map[String, String]): Either[Seq[FormError], Double] = {
        val result = data
          .get(key)
          .map(cleanupAmountOfMoneyString)
          .filter(_.nonEmpty)
          .fold[Either[FormError, BigDecimal]](Left(FormError(key, "error.required"))) {
            validateAmountOfMoney(key, isTooSmall, isTooLarge)(_)
          }

        result.bimap(Seq(_), _.toDouble)
      }

      override def unbind(key: String, value: Double): Map[String, String] =
        Map(key -> formatAmountOfMoneyWithoutPoundSign(value))
    }

  // form for yes/no radio page with no mapping to £0 and yes expecting an amount of money
  // to be submitted
  def amountInPoundsYesNoForm(optionId: String, valueId: String): Form[Double] = {
    val formatter =
      ConditionalRadioUtils.formatter(optionId)(
        List(
          Left(
            ConditionalRadioUtils.InnerOption(
              valueId,
              MoneyUtils.validateAmountOfMoney(
                valueId,
                _ <= 0,
                _ > MoneyUtils.maxAmountOfPounds
              )
            )
          ),
          Right(BigDecimal(0))
        )
      ) { d =>
        if (d === BigDecimal(0)) {
          Map(optionId -> "1")
        } else {
          Map(
            optionId -> "0",
            valueId  -> MoneyUtils.formatAmountOfMoneyWithoutPoundSign(d.toDouble)
          )
        }
      }

    Form(
      mapping(
        "" -> of(formatter).transform[Double](_.toDouble, BigDecimal(_))
      )(identity)(Some(_))
    )
  }

  private def cleanupAmountOfMoneyString(s: String): String =
    s.trim().filter(c => c =!= ',' && c =!= '£')

}
