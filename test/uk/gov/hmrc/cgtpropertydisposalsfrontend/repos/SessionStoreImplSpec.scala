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

import com.typesafe.config.ConfigFactory
import org.scalatest.concurrent.Eventually
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import uk.gov.hmrc.cgtpropertydisposalsfrontend.SampledScalaCheck
import play.api.Configuration
import play.api.test.Helpers._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.Generators.{arb, sample}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.SessionDataGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStoreSpec._
import uk.gov.hmrc.http.{HeaderCarrier, SessionId}
import uk.gov.hmrc.mongo.TimestampSupport

import java.util.UUID
import scala.concurrent.ExecutionContext.Implicits.global

class SessionStoreImplSpec
    extends AnyWordSpec
    with Matchers
    with MongoSupportSpec
    with Eventually
    with GuiceOneAppPerSuite
    with SampledScalaCheck {
  private val timestampSupport: TimestampSupport = app.injector.instanceOf[TimestampSupport]
  val sessionStore                               = new SessionStoreImpl(mongoComponent, config, timestampSupport)

  "SessionStoreImpl" must {

    "be able to insert SessionData into mongo and read it back" in new TestEnvironment {
      forAll { (sessionData: SessionData) =>
        val result = sessionStore.store(sessionData)

        await(result) should be(Right(()))

        eventually {
          val getResult = sessionStore.get()
          await(getResult) should be(Right(Some(sessionData)))
        }
      }
    }

    "return no SessionData if there is no data in mongo" in new TestEnvironment {
      await(sessionStore.get()) should be(Right(None))
    }

    "return an error" when {

      "there is no session id in the header carrier" in {
        implicit val hc: HeaderCarrier = HeaderCarrier()
        val sessionData                = sample[SessionData]

        await(sessionStore.store(sessionData)).isLeft shouldBe true
        await(sessionStore.get()).isLeft              shouldBe true
      }

    }

  }

}

class SessionStoreFailureSpec extends AnyWordSpec with Matchers with MongoSupportSpec with GuiceOneAppPerSuite {

  private val timestampSupport: TimestampSupport = app.injector.instanceOf[TimestampSupport]

  val sessionStore = new SessionStoreImpl(mongoComponent, config, timestampSupport)

}

object SessionStoreSpec {

  val config: Configuration = Configuration(
    ConfigFactory.parseString(
      """
        | upscan-store.expiry-time = 2 days,
        | session-store.expiry-time = 7 days
        |""".stripMargin
    )
  )

  class TestEnvironment {

    protected val sessionId: SessionId = SessionId(UUID.randomUUID().toString)

    implicit val hc: HeaderCarrier = HeaderCarrier(sessionId = Some(sessionId))

  }

}
