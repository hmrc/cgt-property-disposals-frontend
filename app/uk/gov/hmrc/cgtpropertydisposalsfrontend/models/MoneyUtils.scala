package uk.gov.hmrc.cgtpropertydisposalsfrontend.models

import cats.syntax.either._
import cats.syntax.eq._
import cats.instances.char._
import play.api.data.FormError
import play.api.data.format.Formatter

import scala.util.Try

object MoneyUtils {

  val maxAmountOfPounds: Double = 5e10

  def amountInPoundsFormatter(isTooSmall: BigDecimal => Boolean, isTooLarge: BigDecimal => Boolean): Formatter[Double] =
    new Formatter[Double] {
      override def bind(key: String, data: Map[String, String]): Either[Seq[FormError], Double] = {
        def validatePercentage(d: BigDecimal): Either[FormError, BigDecimal] =
          if (isTooSmall(d)) Left(FormError(key, "error.tooLarge"))
          else if (isTooLarge(d)) Left(FormError(key, "error.tooSmall"))
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
