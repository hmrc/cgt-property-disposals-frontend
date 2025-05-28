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

import com.typesafe.config.ConfigFactory
import org.scalatest.concurrent.Eventually
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.Configuration
import play.api.test.Helpers._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.agents.UnsuccessfulVerifierAttempts
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.IdGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.VerifierMatchGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.{CgtReference, GGCredId}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.MongoSupportSpec
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.agents.AgentVerifierMatchRetryStoreImplSpec._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

object AgentVerifierMatchRetryStoreImplSpec {

  val config: Configuration = Configuration(
    ConfigFactory.parseString(
      """
        |agent-verifier-match.store.expiry-time = 30minutes
        |""".stripMargin
    )
  )

  class TestEnvironment {
    protected val agentGGCredId: GGCredId                            = sample[GGCredId]
    protected val clientCgtReference: CgtReference                   = sample[CgtReference]
    protected val unsuccessfulAttempts: UnsuccessfulVerifierAttempts = sample[UnsuccessfulVerifierAttempts]
  }

}
class AgentVerifierMatchRetryStoreImplSpec extends AnyWordSpec with Matchers with MongoSupportSpec with Eventually {

  val retryStore = new AgentVerifierMatchRetryStoreImpl(mongoComponent, config)

  override implicit val patienceConfig: PatienceConfig =
    PatienceConfig(5.seconds, 500.millis)
  "AgentVerifierMatchRetryStoreImpl" must {

    "be able to insert retry data into mongo and read it back" in new TestEnvironment {
      private val result = retryStore
        .store(agentGGCredId, clientCgtReference, unsuccessfulAttempts)

      await(result) should be(Right(()))

      eventually {
        val getResult = retryStore.get(agentGGCredId, clientCgtReference)
        await(getResult) should be(Right(Some(unsuccessfulAttempts)))
      }

    }

    "return no retry data if there is no data in mongo" in new TestEnvironment {
      await(retryStore.get(sample[GGCredId], sample[CgtReference])) should be(
        Right(None)
      )
    }
  }

}

class AgentVerifierMatchRetryStoreImplFailureSpec extends AnyWordSpec with Matchers with MongoSupportSpec {

  val retryStore = new AgentVerifierMatchRetryStoreImpl(mongoComponent, config)

}
