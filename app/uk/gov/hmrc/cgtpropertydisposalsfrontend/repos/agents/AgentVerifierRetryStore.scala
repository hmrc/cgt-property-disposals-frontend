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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.agents

import com.google.inject.{ImplementedBy, Inject, Singleton}
import play.api.Configuration
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Error
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.agents.UnsuccessfulVerifierAttempts
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.{CgtReference, GGCredId}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.Repo
import uk.gov.hmrc.mongo.cache.{CacheIdType, MongoCacheRepository}
import uk.gov.hmrc.mongo.{CurrentTimestampSupport, MongoComponent}

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[AgentVerifierMatchRetryStoreImpl])
trait AgentVerifierMatchRetryStore {
  def get(
    agentCredId: GGCredId,
    clientCgtReference: CgtReference
  ): Future[Either[Error, Option[UnsuccessfulVerifierAttempts]]]

  def store(
    agentCredId: GGCredId,
    clientCgtReference: CgtReference,
    unsuccessfulAttempts: UnsuccessfulVerifierAttempts
  ): Future[Either[Error, Unit]]
}

@Singleton
class AgentVerifierMatchRetryStoreImpl @Inject() (
  mongo: MongoComponent,
  configuration: Configuration
)(implicit
  ec: ExecutionContext
) extends AgentVerifierMatchRetryStore
    with Repo {
  val cacheRepository: MongoCacheRepository[String] = {
    val expireAfter: FiniteDuration = configuration
      .get[FiniteDuration]("agent-verifier-match.store.expiry-time")

    new MongoCacheRepository[String](
      mongoComponent = mongo,
      collectionName = "agent-verifier-match-retries",
      ttl = expireAfter,
      timestampSupport = new CurrentTimestampSupport,
      cacheIdType = CacheIdType.SimpleCacheId
    )(ec)
  }

  val sessionKey = "agent-verifier-match-retries"

  private def toId(agentCredId: GGCredId, clientCgtReference: CgtReference) =
    s"${agentCredId.value}#${clientCgtReference.value}"

  override def get(
    agentCredId: GGCredId,
    clientCgtReference: CgtReference
  ): Future[Either[Error, Option[UnsuccessfulVerifierAttempts]]] =
    get[UnsuccessfulVerifierAttempts](toId(agentCredId, clientCgtReference))

  override def store(
    agentCredId: GGCredId,
    clientCgtReference: CgtReference,
    unsuccessfulAttempts: UnsuccessfulVerifierAttempts
  ): Future[Either[Error, Unit]] =
    store(toId(agentCredId, clientCgtReference), unsuccessfulAttempts)
}
