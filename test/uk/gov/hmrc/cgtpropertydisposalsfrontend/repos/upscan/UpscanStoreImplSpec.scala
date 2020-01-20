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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.upscan

import cats.instances.future._
import com.typesafe.config.ConfigFactory
import org.scalatest.concurrent.Eventually
import org.scalatest.{Matchers, WordSpec}
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.Configuration
import play.api.test.Helpers._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.CgtReference
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.MongoSupport
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.upscan.UpscanStoreImplSpec._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.UpscanService.UpscanNotifyResponse
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.UpscanService.UpscanServiceResponse.UpscanNotifyEvent

import scala.concurrent.ExecutionContext.Implicits.global

class UpscanStoreImplSpec
    extends WordSpec
    with Matchers
    with MongoSupport
    with Eventually
    with ScalaCheckDrivenPropertyChecks {

  val upscanStore = new UpscanStoreImpl(reactiveMongoComponent, config)

  "UpscanStoreImpl" must {

    val cgtReference = sample[CgtReference]

    "be able to insert upscan response to file upload" in {

      val result = upscanStore.insert(UpscanNotifyEvent(cgtReference.value, sample[UpscanNotifyResponse])).value

      await(result) should be(Right(()))

    }

    "be able to count the number of files uploaded for a given CGT reference" in {

      val fileCount = for {
        _  <- upscanStore.insert(UpscanNotifyEvent(cgtReference.value, sample[UpscanNotifyResponse]))
        fc <- upscanStore.fileUploadCount(cgtReference)
      } yield (fc)

      await(fileCount.value) should be(Right(1))

    }

    "be able to get a list of the all the files uploaded against a particular CGT reference" in {

      val upscanNotifyEvent = sample[UpscanNotifyEvent]

      val fileCount = for {
        _    <- upscanStore.insert(upscanNotifyEvent)
        file <- upscanStore.getAllByCgtReference(CgtReference(upscanNotifyEvent.cgtReference))
      } yield (file)

      await(fileCount.value) should be(Right(List(upscanNotifyEvent)))

    }

    "be able to delete all the files uploaded against a particular CGT reference" in  {
      val upscanNotifyEvent = sample[UpscanNotifyEvent]

      val rowsEffected = for {
        _     <- upscanStore.insert(upscanNotifyEvent)
        count <- upscanStore.deleteAllByCgtReference(CgtReference(upscanNotifyEvent.cgtReference))
      } yield (count)

      await(rowsEffected.value) should be(Right(1))

    }

  }

}

object UpscanStoreImplSpec {

  val config = Configuration(
    ConfigFactory.parseString(
      """
        | microservice.services.upscan-initiate.upscan-store.expiry-time = 7 days
        |""".stripMargin
    )
  )

}
