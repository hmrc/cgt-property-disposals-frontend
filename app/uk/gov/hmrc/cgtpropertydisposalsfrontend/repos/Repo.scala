/*
 * Copyright 2022 HM Revenue & Customs
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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.repos

import cats.data.OptionT
import cats.instances.either._
import cats.syntax.either._
import play.api.libs.json.{Reads, Writes}
import uk.gov.hmrc.mongo.cache.{DataKey, MongoCacheRepository}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Error
import uk.gov.hmrc.play.http.logging.Mdc.preservingMdc

import scala.concurrent.{ExecutionContext, Future}

trait Repo {

  val cacheRepository: MongoCacheRepository[String]

  val sessionKey: String

  protected def get[A : Reads](
    id: String
  )(implicit ec: ExecutionContext): Future[Either[Error, Option[A]]] =
    preservingMdc {
      cacheRepository
        .findById(id)
        .map { maybeCache =>
          val response: OptionT[Either[Error, *], A] = for {
            cache ← OptionT.fromOption[Either[Error, *]](maybeCache)
            data ← OptionT.fromOption[Either[Error, *]](Some(cache.data))
            result ← OptionT.liftF[Either[Error, *], A](
                       (data \ sessionKey)
                         .validate[A]
                         .asEither
                         .leftMap(e ⇒
                           Error(
                             s"Could not parse session data from mongo: ${e.mkString("; ")}"
                           )
                         )
                     )
          } yield result

          response.value
        }
        .recover { case e ⇒ Left(Error(e)) }
    }

  protected def store[A : Writes](id: String, a: A)(implicit ec: ExecutionContext): Future[Either[Error, Unit]] =
    cacheRepository
      .put(id)(DataKey(sessionKey), a)
      .map(_ => Right(()))
      .recover { case e => Left(Error(s"Error in insertion $e")) }
}
