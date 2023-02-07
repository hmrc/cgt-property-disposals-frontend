/*
 * Copyright 2023 HM Revenue & Customs
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
import play.api.data.FormError
import play.api.data.format.Formatter

import scala.util.Try

object ConditionalRadioUtils {

  final case class InnerOption[+A](
    readValue: Map[String, String] => Either[Seq[FormError], A]
  ) {
    def map[B](f: A => B): InnerOption[B] =
      InnerOption(
        readValue(_).map(f)
      )
  }

  def formatter[A](outerKey: String, requiredErrorArgs: Seq[String] = Seq.empty)(
    options: List[Either[InnerOption[A], A]]
  )(
    unbindValue: A => Map[String, String]
  ): Formatter[A] =
    new Formatter[A] {
      def readValue[T](
        key: String,
        data: Map[String, String],
        f: String => T
      ): Either[FormError, T] =
        data
          .get(key)
          .map(_.trim())
          .filter(_.nonEmpty)
          .fold[Either[FormError, T]](Left(FormError(key, "error.required", requiredErrorArgs))) { stringValue =>
            Either
              .fromTry(Try(f(stringValue)))
              .leftMap(_ => FormError(key, "error.invalid", requiredErrorArgs))
          }

      override def bind(
        key: String,
        data: Map[String, String]
      ): Either[Seq[FormError], A] =
        readValue(outerKey, data, _.toInt)
          .leftMap(Seq(_))
          .flatMap { i =>
            options.lift(i) match {
              case Some(Right(value))      => Right(value)
              case Some(Left(innerOption)) =>
                innerOption.readValue(data)

              case None => Left(Seq(FormError(outerKey, "error.invalid", requiredErrorArgs)))
            }
          }

      override def unbind(key: String, value: A): Map[String, String] =
        unbindValue(value)

    }

}
