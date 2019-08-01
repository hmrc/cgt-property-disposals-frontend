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

import cats.data.OptionT
import cats.instances.either._
import cats.syntax.either._
import configs.syntax._
import com.google.inject.{ImplementedBy, Inject, Singleton}
import play.api.Configuration
import play.api.libs.json.Json
import play.modules.reactivemongo.ReactiveMongoComponent
import uk.gov.hmrc.cache.model.Id
import uk.gov.hmrc.cache.repository.CacheMongoRepository
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, SessionData}
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration._

@ImplementedBy(classOf[SessionStoreImpl])
trait SessionStore {

  def get()(implicit hc: HeaderCarrier): Future[Either[Error, Option[SessionData]]]

  def store(sessionData: SessionData)(implicit hc: HeaderCarrier): Future[Either[Error, Unit]]

}

@Singleton
class SessionStoreImpl @Inject() (
    mongo: ReactiveMongoComponent,
    configuration: Configuration
)(implicit ec: ExecutionContext) extends SessionStore {

  val cacheRepository: CacheMongoRepository = {
    val expireAfter: FiniteDuration = configuration.underlying.get[FiniteDuration]("session-store.expiry-time").value

    new CacheMongoRepository("sessions", expireAfter.toSeconds)(mongo.mongoConnector.db, ec)
  }

  val sessionKey = "cgtpd-session"

  def get()(implicit hc: HeaderCarrier): Future[Either[Error, Option[SessionData]]] =
    hc.sessionId.map(_.value) match {
      case Some(sessionId) ⇒
        cacheRepository.findById(Id(sessionId)).map { maybeCache =>
          val response: OptionT[Either[Error, ?], SessionData] = for {
            cache ← OptionT.fromOption[Either[Error, ?]](maybeCache)
            data ← OptionT.fromOption[Either[Error, ?]](cache.data)
            result ← OptionT.liftF[Either[Error, ?], SessionData](
              (data \ sessionKey).validate[SessionData].asEither.leftMap(e ⇒ Error(s"Could not parse session data from mongo: ${e.mkString("; ")}"))
            )
          } yield result

          response.value
        }.recover { case e ⇒ Left(Error(e)) }

      case None =>
        Future.successful(Left(Error("no session id found in headers - cannot query mongo")))
    }

  def store(sessionData: SessionData)(implicit hc: HeaderCarrier): Future[Either[Error, Unit]] =
    hc.sessionId.map(_.value) match {
      case Some(sessionId) ⇒
        cacheRepository.createOrUpdate(Id(sessionId), sessionKey, Json.toJson(sessionData))
          .map[Either[Error, Unit]] {
            dbUpdate ⇒
              if (dbUpdate.writeResult.inError) {
                Left(Error(dbUpdate.writeResult.errmsg.getOrElse("unknown error during inserting session data in mongo")))
              } else {
                Right(())
              }
          }.recover { case e ⇒ Left(Error(e)) }

      case None ⇒
        Future.successful(Left(Error("no session id found in headers - cannot store data in mongo")))
    }

}
