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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.returns

import java.time.LocalDate

import com.typesafe.config.ConfigFactory
import org.scalamock.scalatest.MockFactory
import org.scalatest.{Matchers, WordSpec}
import play.api.{Configuration, Mode}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.{ConnectorSpec, HttpSupport}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.CgtReference
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{DraftReturn, SubmitReturnRequest}
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}
import uk.gov.hmrc.play.bootstrap.config.{RunMode, ServicesConfig}

import scala.concurrent.ExecutionContext.Implicits.global

class ReturnsConnectorImplSpec extends WordSpec with Matchers with MockFactory with HttpSupport with ConnectorSpec {

  val config = Configuration(
    ConfigFactory.parseString(
      """
        |microservice {
        |  services {
        |    cgt-property-disposals {
        |      protocol = http
        |      host     = host
        |      port     = 123
        |    }
        |  }
        |}
        |""".stripMargin
    )
  )

  val connector = new ReturnsConnectorImpl(mockHttp, new ServicesConfig(config, new RunMode(config, Mode.Test)))

  "ReturnsConnectorImpl" when {

    implicit val hc: HeaderCarrier = HeaderCarrier()

    "handling requests to store a draft return" must {

      val expectedUrl = s"http://host:123/draft-return"
      val draftReturn = sample[DraftReturn]

      behave like connectorBehaviour(
        mockPost(expectedUrl, Map.empty, draftReturn),
        () => connector.storeDraftReturn(draftReturn)
      )
    }

    "handling requests to get a draft returns" must {

      val cgtReference = sample[CgtReference]
      val expectedUrl  = s"http://host:123/draft-returns/${cgtReference.value}"

      behave like connectorBehaviour(
        mockGet[HttpResponse](expectedUrl, Map.empty, Map.empty),
        () => connector.getDraftReturns(cgtReference)
      )
    }

    "handling request to submit a return" must {
      val submitReturnRequest = sample[SubmitReturnRequest]
      val expectedUrl         = s"http://host:123/return"

      behave like connectorBehaviour(
        mockPost(expectedUrl, Map.empty, submitReturnRequest),
        () => connector.submitReturn(submitReturnRequest)
      )

    }

    "handling requests to list returns" must {

      val cgtReference       = sample[CgtReference]
      val (fromDate, toDate) = LocalDate.of(2020, 1, 2) -> LocalDate.of(2020, 3, 4)
      val expectedUrl        = s"http://host:123/returns/${cgtReference.value}/2020-01-02/2020-03-04"

      behave like connectorBehaviour(
        mockGet[HttpResponse](expectedUrl, Map.empty, Map.empty),
        () => connector.listReturns(cgtReference, fromDate, toDate)
      )
    }

    "handling requests to display a return" must {

      val cgtReference = sample[CgtReference]
      val submissionId = "id"
      val expectedUrl  = s"http://host:123/return/${cgtReference.value}/id"

      behave like connectorBehaviour(
        mockGet[HttpResponse](expectedUrl, Map.empty, Map.empty),
        () => connector.displayReturn(cgtReference, submissionId)
      )
    }

  }
}
