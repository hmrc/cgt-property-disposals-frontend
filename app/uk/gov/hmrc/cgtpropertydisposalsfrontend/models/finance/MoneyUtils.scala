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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance

import java.util.Locale

import cats.instances.bigDecimal._
import cats.instances.char._
import cats.syntax.either._
import cats.syntax.eq._

import play.api.data.format.Formatter
import play.api.data.{Form, FormError}
import play.api.data.Forms.{mapping, of}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ConditionalRadioUtils.InnerOption
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{ConditionalRadioUtils, FormUtils, NumberUtils}

import scala.util.Try

object MoneyUtils {

  val maxAmountOfPounds: BigDecimal = BigDecimal("5e10")

  private val currencyFormatter = java.text.NumberFormat.getCurrencyInstance(Locale.UK)

  def formatAmountOfMoneyWithPoundSign(d: BigDecimal): String = currencyFormatter.format(d).stripSuffix(".00")

  def formatAmountOfMoneyWithoutPoundSign(d: BigDecimal): String =
    formatAmountOfMoneyWithPoundSign(d).replaceAllLiterally("£", "")

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

  def validateLimit(key: String, exceedsLimit: BigDecimal => Boolean, limit: AmountInPence)(
    value: BigDecimal
  ): Either[FormError, BigDecimal] =
    if (exceedsLimit(value)) Left(FormError(key, "error.amountOverLimit", List(limit.inPounds().toString())))
    else Right(value)

  def validateLimitLessThanOther(key: String, comparedOtherKey: String, exceedsLimit: BigDecimal => Boolean)(
    s: String
  ): Either[FormError, BigDecimal] =
    Try(BigDecimal(cleanupAmountOfMoneyString(s))).toEither
      .leftMap(_ => FormError(key, "error.invalid"))
      .flatMap { d =>
        if (exceedsLimit(d)) Left(FormError(key, s"error.$comparedOtherKey"))
        else Right(d)
      }

  def amountInPoundsFormatter(
    isTooSmall: BigDecimal => Boolean,
    isTooLarge: BigDecimal => Boolean
  ): Formatter[BigDecimal] =
    new Formatter[BigDecimal] {
      override def bind(key: String, data: Map[String, String]): Either[Seq[FormError], BigDecimal] = {
        val result =
          FormUtils.readValue(key, data, identity).flatMap(validateAmountOfMoney(key, isTooSmall, isTooLarge)(_))
        result.leftMap(Seq(_))
      }

      override def unbind(key: String, value: BigDecimal): Map[String, String] =
        Map(key -> formatAmountOfMoneyWithoutPoundSign(value))
    }

  // form for yes/no radio page with no mapping to £0 and yes expecting an amount of money
  // to be submitted
  def amountInPoundsYesNoForm(
    optionId: String,
    valueId: String,
    maybeInnerOption: Option[InnerOption[BigDecimal]] = None
  ): Form[BigDecimal] = {
    val innerOption = maybeInnerOption.getOrElse(InnerOption { data =>
      FormUtils
        .readValue(valueId, data, identity)
        .flatMap(
          validateAmountOfMoney(
            valueId,
            _ <= 0,
            _ > maxAmountOfPounds
          )(_)
        )
        .leftMap(Seq(_))
    })

    val formatter =
      ConditionalRadioUtils.formatter(optionId)(
        List(
          Left(innerOption),
          Right(BigDecimal("0"))
        )
      ) { d =>
        if (d === BigDecimal("0")) {
          Map(optionId -> "1")
        } else {
          Map(
            optionId -> "0",
            valueId  -> MoneyUtils.formatAmountOfMoneyWithoutPoundSign(d)
          )
        }
      }

    Form(
      mapping(
        "" -> of(formatter)
      )(identity)(Some(_))
    )
  }

  private def cleanupAmountOfMoneyString(s: String): String =
    s.trim().filter(c => c =!= ',' && c =!= '£')

}
