/*
 * Copyright 2022 HM Revenue & Customs
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
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.Configuration
import play.api.test.Helpers._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.UnsuccessfulNameMatchAttempts.NameMatchDetails.IndividualSautrNameMatchDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.GGCredId
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.IdGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.NameMatchGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.UnsuccessfulNameMatchAttempts
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.BusinessPartnerRecordNameMatchRetryStoreSpec._
import uk.gov.hmrc.mongo.TimestampSupport

import scala.concurrent.ExecutionContext.Implicits.global
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

class NameMatchRetryStoreImplSpec
    extends AnyWordSpec
    with Matchers
    with MongoSupportSpec
    with Eventually
    with GuiceOneAppPerSuite {

  private val timestampSupport: TimestampSupport = app.injector.instanceOf[TimestampSupport]

  val retryStore = new NameMatchRetryStoreImpl(mongoComponent, config, timestampSupport)

  override implicit val patienceConfig: PatienceConfig =
    PatienceConfig(5.seconds, 500.millis)

  "NameMatchRetryStoreImpl" must {

    "be able to insert retry data into mongo and read it back" in new TestEnvironment {}

    "return no retry data if there is no data in mongo" in new TestEnvironment {
      await(
        retryStore.get[IndividualSautrNameMatchDetails](sample[GGCredId])
      ) should be(Right(None))
    }
  }

}

class NameMatchRetryStoreFailureSpec extends AnyWordSpec with Matchers with MongoSupportSpec with GuiceOneAppPerSuite {

  private val timestampSupport: TimestampSupport = app.injector.instanceOf[TimestampSupport]

  val retryStore = new NameMatchRetryStoreImpl(mongoComponent, config, timestampSupport)
  mongoComponent.database.drop()
}
