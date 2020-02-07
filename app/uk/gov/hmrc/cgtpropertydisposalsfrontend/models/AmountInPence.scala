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

import cats.syntax.either._
import cats.syntax.eq._
import cats.instances.char._
import play.api.data.{FormError, Mapping}
import play.api.data.format.Formatter
import play.api.data.validation.{Invalid, Valid, ValidationResult}
import play.api.libs.functional.syntax._
import play.api.libs.json.Format

import scala.util.Try

final case class AmountInPence(value: Long)

object AmountInPence {

  def fromPounds(amount: Double): AmountInPence = AmountInPence((amount * 100d).toLong)

  implicit class AmountInPenceOps(private val a: AmountInPence) extends AnyVal {
    def inPounds(): Double = a.value / 100d
  }

  val amountInPoundsFormatter: Formatter[Double] = new Formatter[Double] {
    override def bind(key: String, data: Map[String, String]): Either[Seq[FormError], Double] = {
      def validatePercentage(d: BigDecimal): Either[FormError, BigDecimal] =
        // todo: make max value configurable
        if (d > 5e10) Left(FormError(key, "error.tooLarge"))
        else if (d <= 0) Left(FormError(key, "error.tooSmall"))
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

  implicit val format: Format[AmountInPence] =
    implicitly[Format[Long]].inmap(AmountInPence(_), _.value)

}
