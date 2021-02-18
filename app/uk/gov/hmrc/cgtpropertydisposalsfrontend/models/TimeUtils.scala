/*
 * Copyright 2021 HM Revenue & Customs
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

import java.time.format.DateTimeFormatter
import java.time.{Clock, LocalDate, LocalDateTime}

import cats.Order
import cats.syntax.either._
import cats.syntax.order._
import configs.Configs
import play.api.data.FormError
import play.api.data.format.Formatter
import play.api.i18n.Messages
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.PersonalRepresentativeDetails

import scala.util.Try

object TimeUtils {

  implicit val configs: Configs[LocalDate] = Configs.fromTry { case (config, key) =>
    LocalDate.parse(config.getString(key), DateTimeFormatter.ISO_DATE)
  }

  val clock: Clock = Clock.systemUTC()

  val minimumDate: LocalDate = LocalDate.of(1900, 1, 1)

  def getTaxYearStartDate(year: Int): LocalDate = LocalDate.of(year, 4, 6)

  def today(): LocalDate = LocalDate.now(clock)

  def now(): LocalDateTime = LocalDateTime.now(clock)

  def dateFormatter(
    maximumDateInclusive: Option[LocalDate],
    minimumDateInclusive: Option[LocalDate],
    dayKey: String,
    monthKey: String,
    yearKey: String,
    dateKey: String,
    extraValidation: List[LocalDate => Either[FormError, Unit]] = List.empty
  ): Formatter[LocalDate] =
    new Formatter[LocalDate] {
      def dateFieldStringValues(
        data: Map[String, String]
      ): Either[FormError, (String, String, String)] =
        List(dayKey, monthKey, yearKey)
          .map(data.get(_).map(_.trim).filter(_.nonEmpty)) match {
          case Some(dayString) :: Some(monthString) :: Some(
                yearString
              ) :: Nil =>
            Right((dayString, monthString, yearString))
          case None :: Some(_) :: Some(_) :: Nil =>
            Left(FormError(dayKey, "error.required"))
          case Some(_) :: None :: Some(_) :: Nil =>
            Left(FormError(monthKey, "error.required"))
          case Some(_) :: Some(_) :: None :: Nil =>
            Left(FormError(yearKey, "error.required"))
          case Some(_) :: None :: None :: Nil    =>
            Left(FormError(monthKey, "error.monthAndYearRequired"))
          case None :: Some(_) :: None :: Nil    =>
            Left(FormError(dayKey, "error.dayAndYearRequired"))
          case None :: None :: Some(_) :: Nil    =>
            Left(FormError(dayKey, "error.dayAndMonthRequired"))
          case _                                 => Left(FormError(dateKey, "error.required"))
        }

      def toValidInt(
        key: String,
        stringValue: String,
        maxValue: Option[Int]
      ): Either[FormError, Int] =
        Either.fromOption(
          Try(BigDecimal(stringValue).toIntExact).toOption.filter(i => i > 0 && maxValue.forall(i <= _)),
          FormError(key, "error.invalid")
        )

      override def bind(
        key: String,
        data: Map[String, String]
      ): Either[Seq[FormError], LocalDate] = {
        val result = for {
          dateFieldStrings <- dateFieldStringValues(data)
          day ← toValidInt(dayKey, dateFieldStrings._1, Some(31))
          month ← toValidInt(monthKey, dateFieldStrings._2, Some(12))
          year ← toValidInt(yearKey, dateFieldStrings._3, None)
          date ← Either
                   .fromTry(Try(LocalDate.of(year, month, day)))
                   .leftMap(_ => FormError(dateKey, "error.invalid"))
                   .flatMap(date =>
                     if (maximumDateInclusive.exists(_.isBefore(date)))
                       Left(FormError(dateKey, "error.tooFarInFuture"))
                     else if (minimumDateInclusive.exists(_.isAfter(date)))
                       Left(FormError(dateKey, "error.tooFarInPast"))
                     else if (date.isBefore(minimumDate))
                       Left(FormError(dateKey, "error.before1900"))
                     else
                       extraValidation
                         .map(_(date))
                         .find(_.isLeft)
                         .getOrElse(Right(()))
                         .map(_ => date)
                   )
        } yield date

        result.leftMap(Seq(_))
      }

      override def unbind(key: String, value: LocalDate): Map[String, String] =
        Map(
          dayKey   -> value.getDayOfMonth.toString,
          monthKey -> value.getMonthValue.toString,
          yearKey  -> value.getYear.toString
        )

    }

  def govDisplayFormat(date: LocalDate)(implicit messages: Messages): String =
    s"""${date.getDayOfMonth()} ${messages(
      s"date.${date.getMonthValue()}"
    )} ${date.getYear()}"""

  def govShortDisplayFormat(
    date: LocalDate
  )(implicit messages: Messages): String =
    s"""${date.getDayOfMonth()} ${messages(
      s"date.short.${date.getMonthValue()}"
    )} ${date.getYear()}"""

  implicit val localDateOrder: Order[LocalDate] = Order.from(_ compareTo _)

  // what's the  start date of the tax year the given date falls into?
  def taxYearStart(date: LocalDate): LocalDate = {
    val currentCalendarTaxYearStart = LocalDate.of(date.getYear, 4, 6)
    if (date < currentCalendarTaxYearStart) currentCalendarTaxYearStart.minusYears(1L)
    else currentCalendarTaxYearStart
  }

  def personalRepresentativeDateValidation(
    personalRepresentativeDetails: Option[PersonalRepresentativeDetails],
    formErrorKey: String
  )(
    date: LocalDate
  ): Either[FormError, Unit] =
    personalRepresentativeDetails.fold[Either[FormError, Unit]](
      Right(())
    ) { case PersonalRepresentativeDetails(personalRepType, dateOfDeath) =>
      personalRepType.fold(
        _ =>
          if (date > dateOfDeath.value) Right(())
          else Left(FormError(formErrorKey, "error.periodOfAdminDeathNotAfterDate")),
        _ =>
          if (date > dateOfDeath.value)
            Left(FormError(formErrorKey, "error.nonPeriodOfAdminDeathAfterDate"))
          else Right(())
      )

    }

  def getMaximumDateForDisposalsAndCompletion(
    enableFutureDateForDisposalAndCompletion: Boolean,
    maxYearForDisposalsAndCompletion: Int
  ): LocalDate =
    if (enableFutureDateForDisposalAndCompletion)
      TimeUtils.getMaximumDateOf(TimeUtils.today(), LocalDate.of(maxYearForDisposalsAndCompletion, 4, 5))
    else
      TimeUtils.today()


  def getMaximumDateOf(date1: LocalDate, date2: LocalDate): LocalDate =
    if (date1.isAfter(date2))
      date1
    else date2

}
