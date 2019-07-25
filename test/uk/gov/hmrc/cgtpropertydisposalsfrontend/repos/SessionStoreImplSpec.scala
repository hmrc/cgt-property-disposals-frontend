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

import java.time.LocalDate
import java.util.UUID

import com.typesafe.config.ConfigFactory
import org.scalatest.{Matchers, WordSpec}
import org.scalatest.concurrent.ScalaFutures
import play.api.Configuration
import play.api.libs.json.{JsNumber, JsObject}
import uk.gov.hmrc.cache.model.{Cache, Id}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{DateOfBirth, NINO, SessionData}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.logging.SessionId
import uk.gov.hmrc.mongo.DatabaseUpdate

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration._

class SessionStoreImplSpec extends WordSpec with Matchers with MongoSupport with ScalaFutures {

  val config = Configuration(ConfigFactory.parseString(
    """
      |session-store.expiry-time = 30minutes
      |""".stripMargin
  ))

  val sessionStore = new SessionStoreImpl(reactiveMongoComponent, config)

  override implicit val patienceConfig: PatienceConfig = PatienceConfig(5.seconds, 500.millis)

  "SessionStoreImpl" must {

    val sessionData = SessionData(Some(NINO("AB123456C")), Some(DateOfBirth(LocalDate.ofEpochDay(0L))))

    "be able to insert SessionData into mongo and read it back" in new TestEnvironment {
      val result = sessionStore.store(sessionData)

      result.futureValue should be(Right(()))

      val getResult = sessionStore.get()
      getResult.futureValue should be(Right(Some(sessionData)))
    }

    "return no SessionData if there is no data in mongo" in new TestEnvironment {
      sessionStore.get().futureValue should be(Right(None))
    }

    "return an error" when {

      "the connection to mongo is broken" in new TestEnvironment {
        withBrokenMongo { brokenMongo =>
          val sessionStore = new SessionStoreImpl(brokenMongo, config)
          sessionStore.get().futureValue.isLeft shouldBe true
          sessionStore.store(sessionData).futureValue.isLeft shouldBe true
        }
      }

      "the data in mongo cannot be parsed" in new TestEnvironment {
        val invalidData = JsObject(Map("nino" -> JsNumber(1)))
        val create: Future[DatabaseUpdate[Cache]] = sessionStore.cacheRepository.createOrUpdate(Id(sessionId.value), sessionStore.sessionKey, invalidData)
        create.futureValue.writeResult.inError shouldBe false
        sessionStore.get().futureValue.isLeft shouldBe true
      }

      "there is no session id in the header carrier" in {
        implicit val hc: HeaderCarrier = HeaderCarrier()
        sessionStore.store(sessionData).futureValue.isLeft shouldBe true
        sessionStore.get().futureValue.isLeft shouldBe true
      }

    }

  }

  class TestEnvironment {

    val sessionId = SessionId(UUID.randomUUID().toString)

    implicit val hc: HeaderCarrier = HeaderCarrier(sessionId = Some(sessionId))

  }

}
