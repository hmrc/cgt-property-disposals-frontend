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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.repos

import com.google.inject.{ImplementedBy, Inject, Singleton}
import configs.syntax._
import play.api.Configuration
import play.modules.reactivemongo.ReactiveMongoComponent
import uk.gov.hmrc.cache.repository.CacheMongoRepository
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, SessionData}
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[SessionStoreImpl])
trait SessionStore {

  def get()(implicit hc: HeaderCarrier): Future[Either[Error, Option[SessionData]]]

  def store(sessionData: SessionData)(implicit hc: HeaderCarrier): Future[Either[Error, Unit]]

}

@Singleton
class SessionStoreImpl @Inject() (mongo: ReactiveMongoComponent, configuration: Configuration)(
  implicit ec: ExecutionContext
) extends SessionStore
    with Repo {

  val cacheRepository: CacheMongoRepository = {
    val expireAfter: FiniteDuration = configuration.underlying
      .get[FiniteDuration]("session-store.expiry-time")
      .value

    new CacheMongoRepository("sessions", expireAfter.toSeconds)(mongo.mongoConnector.db, ec)
  }

  val sessionKey = "cgtpd-session"

  def get()(implicit hc: HeaderCarrier): Future[Either[Error, Option[SessionData]]] =
    hc.sessionId.map(_.value) match {
      case Some(sessionId) ⇒ get[SessionData](sessionId)
      case None => Future.successful(Left(Error("no session id found in headers - cannot query mongo")))
    }

  def store(sessionData: SessionData)(implicit hc: HeaderCarrier): Future[Either[Error, Unit]] =
    hc.sessionId.map(_.value) match {
      case Some(sessionId) ⇒ store(sessionId, sessionData)
      case None ⇒ Future.successful(Left(Error("no session id found in headers - cannot store data in mongo")))
    }

}
