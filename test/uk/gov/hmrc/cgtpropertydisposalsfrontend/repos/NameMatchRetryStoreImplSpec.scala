/*
 * Copyright 2021 HM Revenue & Customs
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
import org.scalatest.{Matchers, WordSpec}
import play.api.Configuration
import play.api.libs.json.{JsObject, JsString}
import play.api.test.Helpers._
import uk.gov.hmrc.cache.model.{Cache, Id}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.UnsuccessfulNameMatchAttempts.NameMatchDetails.IndividualSautrNameMatchDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.GGCredId
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.IdGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.NameMatchGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.UnsuccessfulNameMatchAttempts
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.BusinessPartnerRecordNameMatchRetryStoreSpec._
import uk.gov.hmrc.mongo.DatabaseUpdate

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration._

object BusinessPartnerRecordNameMatchRetryStoreSpec {

  val config = Configuration(
    ConfigFactory.parseString(
      """
        |bpr-name-match.store.expiry-time = 30minutes
        |""".stripMargin
    )
  )

  class TestEnvironment {
    val ggCredId             = sample[GGCredId]
    val unsuccessfulAttempts =
      sample[UnsuccessfulNameMatchAttempts[IndividualSautrNameMatchDetails]]
  }

}

class NameMatchRetryStoreImplSpec extends WordSpec with Matchers with MongoSupport with Eventually {

  val retryStore = new NameMatchRetryStoreImpl(reactiveMongoComponent, config)

  override implicit val patienceConfig: PatienceConfig =
    PatienceConfig(5.seconds, 500.millis)

  "NameMatchRetryStoreImpl" must {

    "be able to insert retry data into mongo and read it back" in new TestEnvironment {
      val result = retryStore.store(ggCredId, unsuccessfulAttempts)

      await(result) should be(Right(()))

      eventually {
        val getResult = retryStore.get(ggCredId)
        await(getResult) should be(Right(Some(unsuccessfulAttempts)))
      }

    }

    "return no retry data if there is no data in mongo" in new TestEnvironment {
      await(
        retryStore.get[IndividualSautrNameMatchDetails](sample[GGCredId])
      ) should be(Right(None))
    }

    "return an error" when {

      "the data in mongo cannot be parsed" in new TestEnvironment {
        val invalidData                           = JsObject(Map("numberOfRetriesDone" -> JsString("1")))
        val create: Future[DatabaseUpdate[Cache]] =
          retryStore.cacheRepository.createOrUpdate(
            Id(ggCredId.value),
            retryStore.sessionKey,
            invalidData
          )
        await(create).writeResult.inError shouldBe false
        await(
          retryStore.get[IndividualSautrNameMatchDetails](ggCredId)
        ).isLeft                          shouldBe true
      }

    }

  }

}

class NameMatchRetryStoreFailureSpec extends WordSpec with Matchers with MongoSupport {

  val retryStore = new NameMatchRetryStoreImpl(reactiveMongoComponent, config)
  reactiveMongoComponent.mongoConnector.helper.driver.close()

  val mongoIsBrokenAndAttemptingTo = new AfterWord(
    "mongo is broken and attempting to"
  )

  "NameMatchRetryStore" must {

    "return an error" when mongoIsBrokenAndAttemptingTo {

      "insert a record" in new TestEnvironment {
        await(
          retryStore.get[IndividualSautrNameMatchDetails](ggCredId)
        ).isLeft shouldBe true
      }

      "read a record" in new TestEnvironment {
        await(
          retryStore.store(ggCredId, unsuccessfulAttempts)
        ).isLeft shouldBe true
      }

    }

  }

}
