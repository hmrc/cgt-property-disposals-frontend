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

import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach, Suite}
import org.scalatest.matchers.should.Matchers
import uk.gov.hmrc.mongo.test.MongoSupport

trait MongoSupportSpec extends MongoSupport with BeforeAndAfterEach with BeforeAndAfterAll { this: Suite with Matchers ⇒

  abstract override def beforeEach(): Unit = {
    super.beforeEach()
    dropDatabase()
  }

  abstract override def afterAll(): Unit = {
    super.afterAll()
    mongoComponent.client.close()
  }

}
