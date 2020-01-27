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

import cats.Applicative
import cats.instances.either._
import cats.syntax.traverse._
import play.api.libs.json.{Format, JsObject, JsResult, JsValue, Json}

object EitherUtils {

  implicit def eitherFormat[A, B](implicit aFormat: Format[A], bFormat: Format[B]): Format[Either[A, B]] =
    new Format[Either[A, B]] {
      override def reads(json: JsValue): JsResult[Either[A, B]] =
        (json \ "l")
          .validate[A]
          .map[Either[A, B]](Left(_))
          .orElse((json \ "r").validate[B].map(Right(_)))

      override def writes(o: Either[A, B]): JsValue =
        o.fold(
          a ⇒ JsObject(Seq("l" → Json.toJson(a))),
          b ⇒ JsObject(Seq("r" → Json.toJson(b)))
        )
    }

  implicit class EitherOps[A, B](private val e: Either[A, B]) extends AnyVal {

    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    // go from Either[F[C],B] to F[Either[C,B]]
    def leftSequence[F[_], C](implicit ap: Applicative[F], ev: A <:< F[C]): F[Either[C, B]] =
      ap.map(e.swap.sequence[F, C])(_.swap)

  }

}
