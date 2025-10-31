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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.repos

import play.api.libs.json.{Reads, Writes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Error
import uk.gov.hmrc.mongo.cache.{DataKey, MongoCacheRepository}
import uk.gov.hmrc.mdc.Mdc.preservingMdc

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

trait Repo {
  val cacheRepository: MongoCacheRepository[String]

  val sessionKey: String

  protected def get[A : Reads](
    id: String
  )(implicit ec: ExecutionContext): Future[Either[Error, Option[A]]] =
    preservingMdc {
      cacheRepository.get(id)(DataKey(sessionKey)).map(d => Right(d))
    }

  protected def clearCache(id: String)(implicit ec: ExecutionContext): Future[Either[Error, Unit]] =
    preservingMdc {
      Try {
        cacheRepository.delete(id)(DataKey(sessionKey))
      } match {
        case Success(futureResult) =>
          futureResult
            .map(_ => Right(()))
            .recover { case _: Exception =>
              Left(Error("unknown error during clearing session data in mongo"))
            }
        case Failure(_)            =>
          Future.successful(Left(Error("unknown error during clearing session data in mongo")))
      }
    }

  protected def store[A : Writes](id: String, a: A)(implicit ec: ExecutionContext): Future[Either[Error, Unit]] =
    preservingMdc {
      cacheRepository
        .put(id)(DataKey(sessionKey), a)
        .map(_ => Right(()))
        .recover { case _: Exception =>
          Left(Error("unknown error during inserting session data in mongo"))
        }
    }
}
