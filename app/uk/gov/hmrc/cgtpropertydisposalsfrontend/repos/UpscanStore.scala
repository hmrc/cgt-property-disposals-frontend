/*
 * Copyright 2019 HM Revenue & Customs
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
import java.time.LocalDateTime

import cats.data.EitherT
import com.google.inject.{ImplementedBy, Inject, Singleton}
import play.api.Configuration
import play.api.libs.json.{Json, OFormat}
import play.modules.reactivemongo.ReactiveMongoComponent
import reactivemongo.api.Cursor
import reactivemongo.api.commands.WriteResult
import reactivemongo.api.indexes.{Index, IndexType}
import reactivemongo.bson.BSONObjectID
import reactivemongo.play.json.ImplicitBSONHandlers.JsObjectDocumentWriter
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Error
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.CgtReference
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.UpscanDescriptor
import uk.gov.hmrc.mongo.ReactiveRepository
import uk.gov.hmrc.mongo.json.ReactiveMongoFormats

import scala.concurrent.{ExecutionContext, Future}

final case class UpscanResponse(cgtReference: String, upscanDescriptor: UpscanDescriptor, timestamp: LocalDateTime)
object UpscanResponse {
  implicit val format: OFormat[UpscanResponse] = Json.format[UpscanResponse]
}

@ImplementedBy(classOf[UpscanStoreImpl])
trait UpscanStore {

  def insert(upscan: UpscanResponse): EitherT[Future, Error, Unit]

  def getAll(cgtReference: CgtReference): EitherT[Future, Error, List[UpscanResponse]]

  def deleteAll(cgtReference: CgtReference): EitherT[Future, Error, Int]

}

@Singleton
class UpscanStoreImpl @Inject()(mongo: ReactiveMongoComponent, configuration: Configuration)(
  implicit ec: ExecutionContext
) extends ReactiveRepository[UpscanResponse, BSONObjectID](
      collectionName = "upscan",
      mongo          = mongo.mongoConnector.db,
      UpscanResponse.format,
      ReactiveMongoFormats.objectIdFormats
    )
    with UpscanStore {

  override def indexes: Seq[Index] = Seq(
    Index(
      key  = Seq("cgtReference" → IndexType.Ascending),
      name = Some("cgtReferenceIndex")
    )
  )

  override def insert(upscan: UpscanResponse): EitherT[Future, Error, Unit] =
    EitherT[Future, Error, Unit](
      collection.insert
        .one[UpscanResponse](upscan)
        .map[Either[Error, Unit]] { result: WriteResult =>
          if (result.ok)
            Right(())
          else
            Left(
              Error(
                s"Could not insert upscan file descriptor: got write errors :${result.writeErrors}"
              )
            )
        }
        .recover {
          case exception => Left(Error(exception))
        }
    )

  override def getAll(cgtReference: CgtReference): EitherT[Future, Error, List[UpscanResponse]] =
    EitherT[Future, Error, List[UpscanResponse]](
      collection
        .find(Json.obj("cgtReference" -> cgtReference.value), None)
        .cursor[UpscanResponse]()
        .collect[List](5, Cursor.FailOnError[List[UpscanResponse]]())
        .map { upscanResponses =>
          Right(upscanResponses)
        }
        .recover {
          case exception => Left(Error(exception.getMessage))
        }
    )

  override def deleteAll(cgtReference: CgtReference): EitherT[Future, Error, Int] = {
    EitherT[Future, Error, Int](
      collection.delete
        .one(Json.obj("ggCredId" -> ggCredId))
        .map { result: WriteResult =>
          Right(result.n)
        }
        .recover {
          case exception => Left(Error(exception.getMessage))
        }
    )

  }
}
