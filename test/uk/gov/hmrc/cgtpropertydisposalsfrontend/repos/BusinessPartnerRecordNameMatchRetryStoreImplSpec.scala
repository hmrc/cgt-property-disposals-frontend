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

import com.typesafe.config.ConfigFactory
import org.scalacheck.ScalacheckShapeless._
import org.scalatest.{Matchers, WordSpec}
import org.scalatest.concurrent.{Eventually, ScalaFutures}
import play.api.Configuration
import play.api.libs.json.{JsObject, JsString}
import uk.gov.hmrc.cache.model.{Cache, Id}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.GGCredId
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.sample
import uk.gov.hmrc.mongo.DatabaseUpdate

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.Random

class BusinessPartnerRecordNameMatchRetryStoreImplSpec extends WordSpec with Matchers with MongoSupport with ScalaFutures with Eventually {

  val config = Configuration(
    ConfigFactory.parseString(
      """
        |bpr-name-match.store.expiry-time = 30minutes
        |""".stripMargin
    )
  )

  val sessionStore = new BusinessPartnerRecordNameMatchRetryStoreImpl(reactiveMongoComponent, config)

  override implicit val patienceConfig: PatienceConfig =
    PatienceConfig(5.seconds, 500.millis)

  "BusinessPartnerRecordNameMatchRetryStoreImpl" must {

    "be able to insert retry data into mongo and read it back" in new TestEnvironment {
      val result = sessionStore.store(ggCredId, numberOfRetries)

      result.futureValue should be(Right(()))

      eventually {
        val getResult = sessionStore.get(ggCredId)
        getResult.futureValue should be(Right(Some(numberOfRetries)))
      }

    }

    "return no retry data if there is no data in mongo" in new TestEnvironment {
      sessionStore.get(sample[GGCredId]).futureValue should be(Right(None))
    }

    "return an error" when {

      "the connection to mongo is broken" in new TestEnvironment {
        withBrokenMongo { brokenMongo =>
          val sessionStore = new BusinessPartnerRecordNameMatchRetryStoreImpl(brokenMongo, config)
          sessionStore.get(ggCredId).futureValue.isLeft              shouldBe true
          sessionStore.store(ggCredId, numberOfRetries).futureValue.isLeft shouldBe true
        }
      }

      "the data in mongo cannot be parsed" in new TestEnvironment {
        val invalidData = JsObject(Map("numberOfRetriesDone" -> JsString("1")))
        val create: Future[DatabaseUpdate[Cache]] =
          sessionStore.cacheRepository.createOrUpdate(Id(ggCredId.value), sessionStore.sessionKey, invalidData)
        create.futureValue.writeResult.inError shouldBe false
        sessionStore.get(ggCredId).futureValue.isLeft  shouldBe true
      }


    }

  }

  class TestEnvironment {
    val ggCredId = sample[GGCredId]
    val numberOfRetries = Random.nextInt()
  }

}
