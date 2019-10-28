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

import com.google.inject.{ImplementedBy, Inject, Singleton}
import configs.syntax._
import play.api.Configuration
import play.api.libs.json.{Reads, Writes}
import play.modules.reactivemongo.ReactiveMongoComponent
import uk.gov.hmrc.cache.repository.CacheMongoRepository
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Error
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.bpr.UnsuccessfulNameMatchAttempts
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.bpr.UnsuccessfulNameMatchAttempts.NameMatchDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.GGCredId

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[BusinessPartnerRecordNameMatchRetryStoreImpl])
trait BusinessPartnerRecordNameMatchRetryStore {

  def get[A <: NameMatchDetails : Reads](ggCredId: GGCredId): Future[Either[Error, Option[UnsuccessfulNameMatchAttempts[A]]]]

  def store[A <: NameMatchDetails : Writes](ggCredId: GGCredId, unsuccessfulAttempts: UnsuccessfulNameMatchAttempts[A]): Future[Either[Error, Unit]]

}

@Singleton
class BusinessPartnerRecordNameMatchRetryStoreImpl @Inject()(
  mongo: ReactiveMongoComponent,
  configuration: Configuration
)(
  implicit ec: ExecutionContext
) extends BusinessPartnerRecordNameMatchRetryStore
    with Repo {

  val cacheRepository: CacheMongoRepository = {
    val expireAfter: FiniteDuration = configuration.underlying
      .get[FiniteDuration]("bpr-name-match.store.expiry-time")
      .value

    new CacheMongoRepository("bpr-name-match-retries", expireAfter.toSeconds)(mongo.mongoConnector.db, ec)
  }

  val sessionKey = "bpr-name-match-retries"

  override def get[A <: NameMatchDetails : Reads](ggCredId: GGCredId): Future[Either[Error, Option[UnsuccessfulNameMatchAttempts[A]]]] =
    get[UnsuccessfulNameMatchAttempts[A]](ggCredId.value)

  override def store[A <: NameMatchDetails : Writes](
    ggCredId: GGCredId,
    unsuccessfulAttempts: UnsuccessfulNameMatchAttempts[A]
  ): Future[Either[Error, Unit]] =
    store(ggCredId.value, unsuccessfulAttempts)

}
