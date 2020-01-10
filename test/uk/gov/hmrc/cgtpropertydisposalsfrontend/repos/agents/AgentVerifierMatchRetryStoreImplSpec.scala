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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.agents

import com.typesafe.config.ConfigFactory
import org.scalatest.concurrent.Eventually
import org.scalatest.{Matchers, WordSpec}
import play.api.Configuration
import play.api.libs.json.{JsObject, JsString}
import play.api.test.Helpers._
import uk.gov.hmrc.cache.model.{Cache, Id}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.agents.UnsuccessfulVerifierAttempts
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.{CgtReference, GGCredId}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.bpr.UnsuccessfulNameMatchAttempts.NameMatchDetails.IndividualNameMatchDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.MongoSupport
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.agents.AgentVerifierMatchRetryStoreImplSpec._
import uk.gov.hmrc.mongo.DatabaseUpdate

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration._

object AgentVerifierMatchRetryStoreImplSpec {

  val config = Configuration(
    ConfigFactory.parseString(
      """
        |agent-verifier-match.store.expiry-time = 30minutes
        |""".stripMargin
    )
  )

  class TestEnvironment {
    val agentGGCredId        = sample[GGCredId]
    val clientCgtReference   = sample[CgtReference]
    val unsuccessfulAttempts = sample[UnsuccessfulVerifierAttempts]
  }

}
class AgentVerifierMatchRetryStoreImplSpec extends WordSpec with Matchers with MongoSupport with Eventually {

  val retryStore = new AgentVerifierMatchRetryStoreImpl(reactiveMongoComponent, config)

  override implicit val patienceConfig: PatienceConfig =
    PatienceConfig(5.seconds, 500.millis)

  "AgentVerifierMatchRetryStoreImpl" must {

    "be able to insert retry data into mongo and read it back" in new TestEnvironment {
      val result = retryStore.store(agentGGCredId, clientCgtReference, unsuccessfulAttempts)

      await(result) should be(Right(()))

      eventually {
        val getResult = retryStore.get(agentGGCredId, clientCgtReference)
        await(getResult) should be(Right(Some(unsuccessfulAttempts)))
      }

    }

    "return no retry data if there is no data in mongo" in new TestEnvironment {
      await(retryStore.get(sample[GGCredId], sample[CgtReference])) should be(Right(None))
    }

    "return an error" when {

      "the data in mongo cannot be parsed" in new TestEnvironment {
        val invalidData = JsObject(Map("unsuccessfulAttempts" -> JsString("1")))
        val create: Future[DatabaseUpdate[Cache]] =
          retryStore.cacheRepository.createOrUpdate(
            Id(s"${agentGGCredId.value}#${clientCgtReference.value}"),
            retryStore.sessionKey,
            invalidData
          )
        await(create).writeResult.inError                               shouldBe false
        await(retryStore.get(agentGGCredId, clientCgtReference)).isLeft shouldBe true
      }

    }

  }

}

class AgentVerifierMatchRetryStoreImplFailureSpec extends WordSpec with Matchers with MongoSupport {

  val retryStore = new AgentVerifierMatchRetryStoreImpl(reactiveMongoComponent, config)

  reactiveMongoComponent.mongoConnector.helper.driver.close()

  val mongoIsBrokenAndAttemptingTo = new AfterWord("mongo is broken and attempting to")

  "BusinessPartnerRecordNameMatchRetryStore" must {

    "return an error" when mongoIsBrokenAndAttemptingTo {

      "insert a record" in new TestEnvironment {
        await(retryStore.get(agentGGCredId, clientCgtReference)).isLeft shouldBe true
      }

      "read a record" in new TestEnvironment {
        await(retryStore.store(agentGGCredId, clientCgtReference, unsuccessfulAttempts)).isLeft shouldBe true
      }

    }

  }

}
