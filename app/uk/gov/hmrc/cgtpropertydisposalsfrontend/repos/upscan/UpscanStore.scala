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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.upscan

import cats.data.EitherT
import com.google.inject.{ImplementedBy, Inject, Singleton}
import configs.syntax._
import play.api.Configuration
import play.api.libs.json.Json
import play.modules.reactivemongo.ReactiveMongoComponent
import reactivemongo.api.commands.WriteResult
import reactivemongo.api.indexes.{Index, IndexType}
import reactivemongo.api.{Cursor, ReadConcern}
import reactivemongo.play.json.ImplicitBSONHandlers.JsObjectDocumentWriter
import uk.gov.hmrc.cache.repository.CacheMongoRepository
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Error
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.CgtReference
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.UpscanService.UpscanServiceResponse.UpscanNotifyEvent
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[UpscanStoreImpl])
trait UpscanStore {

  def fileUploadCount(cgtReference: CgtReference): EitherT[Future, Error, Long]

  def insert(upscan: UpscanNotifyEvent): EitherT[Future, Error, Unit]

  def getAllByCgtReference(cgtReference: CgtReference): EitherT[Future, Error, List[UpscanNotifyEvent]]

  def deleteAllByCgtReference(cgtReference: CgtReference): EitherT[Future, Error, Int]

}

@SuppressWarnings(Array("org.wartremover.warts.Var", "org.wartremover.warts.NonUnitStatements"))
@Singleton
class UpscanStoreImpl @Inject()(mongo: ReactiveMongoComponent, configuration: Configuration)(
  implicit ec: ExecutionContext
) extends UpscanStore
    with Logging {

  val message: String = "Failed to ensure index"

  val cacheRepository: CacheMongoRepository = {
    val expireAfter: FiniteDuration = configuration.underlying
      .get[FiniteDuration]("microservice.upscan-initiate.upscan-store.expiry-time")
      .value

    new CacheMongoRepository("upscan-event", expireAfter.toSeconds)(mongo.mongoConnector.db, ec)
  }

  private def indexes: Seq[Index] = Seq(
    Index(
      key  = Seq("cgtReference" → IndexType.Ascending),
      name = Some("cgtReferenceIndex")
    )
  )

  private def ensureIndex(index: Index)(implicit ec: ExecutionContext): Future[Boolean] =
    cacheRepository.collection.indexesManager
      .create(index)
      .map(wr => wr.ok)
      .recover {
        case t =>
          logger.error(s"$message (${index.eventualName})", t)
          false
      }

  private def ensureIndexes(implicit ec: ExecutionContext): Future[Seq[Boolean]] =
    Future.sequence(indexes.map(ensureIndex))

  ensureIndexes(scala.concurrent.ExecutionContext.Implicits.global)

  private val defaultReadConcern: ReadConcern = mongo.mongoConnector.helper.connectionOptions.readConcern

  val upscanKey = "upscan"

  override def fileUploadCount(cgtReference: CgtReference): EitherT[Future, Error, Long] = {
    val query = Json.obj("cgtReference" -> cgtReference.value)
    EitherT[Future, Error, Long](
      cacheRepository.collection
        .count(Some(query), limit = None, skip = 0, hint = None, readConcern = defaultReadConcern)
        .map[Either[Error, Long]] { c =>
          Right(c)
        }
        .recover {
          case exception => Left(Error(exception.getMessage))
        }
    )
  }

  override def insert(upscan: UpscanNotifyEvent): EitherT[Future, Error, Unit] =
    EitherT[Future, Error, Unit](
      cacheRepository.collection.insert
        .one[UpscanNotifyEvent](upscan)
        .map[Either[Error, Unit]] { result: WriteResult =>
          if (result.ok) {
            println(result.n)
            Right(())
          }
          else
            Left(
              Error(
                s"Could not insert upscan notify response due to :${result.writeErrors}"
              )
            )
        }
        .recover {
          case exception => Left(Error(exception))
        }
    )

  override def getAllByCgtReference(cgtReference: CgtReference): EitherT[Future, Error, List[UpscanNotifyEvent]] =
    EitherT[Future, Error, List[UpscanNotifyEvent]](
      cacheRepository.collection
        .find(Json.obj("cgtReference" -> cgtReference.value), None)
        .cursor[UpscanNotifyEvent]()
        .collect[List](5, Cursor.FailOnError[List[UpscanNotifyEvent]]())
        .map { upscanNotifyEvents =>
          Right(upscanNotifyEvents)
        }
        .recover {
          case exception => Left(Error(exception.getMessage))
        }
    )

  override def deleteAllByCgtReference(cgtReference: CgtReference): EitherT[Future, Error, Int] =
    EitherT[Future, Error, Int](
      cacheRepository.collection.delete
        .one(Json.obj("cgtReference" -> cgtReference.value), None)
        .map { result: WriteResult =>
          Right(result.n)
        }
        .recover {
          case exception => Left(Error(exception.getMessage))
        }
    )
}
