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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.returns

import com.typesafe.config.ConfigFactory
import org.scalamock.scalatest.MockFactory
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.Configuration
import play.api.libs.json.Json
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.{ConnectorSpec, HttpSupport}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.AmountInPence
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.IdGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.MoneyGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.CgtReference
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import scala.concurrent.ExecutionContext.Implicits.global

class PaymentsConnectorImplSpec extends AnyWordSpec with Matchers with MockFactory with HttpSupport with ConnectorSpec {

  val selfUrl = "https://self:999"

  private val config = Configuration(
    ConfigFactory.parseString(
      s"""
        |microservice {
        |  services {
        |    payments {
        |      protocol = https
        |      host     = host
        |      port     = 123
        |    }
        |  }
        |}
        |
        |self.url = "$selfUrl"
        |""".stripMargin
    )
  )

  val connector = new PaymentsConnectorImpl(
    mockHttp,
    new ServicesConfig(config)
  )

  implicit val hc: HeaderCarrier = HeaderCarrier()

  "PaymentsConnectorImpl" when {

    "handling requests to start a payments journey" must {
      val cgtReference    = sample[CgtReference]
      val chargeReference = sample[String]
      val amount          = sample[AmountInPence]
      val returnCall      = controllers.routes.StartController.start()
      val backCall        = controllers.returns.routes.TaskListController.taskList()
      val expectedUrl     =
        "https://host:123/pay-api/capital-gains-tax/journey/start"

      behave like connectorBehaviour(
        mockPost(
          expectedUrl,
          Seq.empty,
          Json.parse(
            s"""
              |{
              |  "cgtReference": "${cgtReference.value}",
              |  "chargeReference": "$chargeReference",
              |  "amountInPence": ${amount.value},
              |  "returnUrl": "$selfUrl${returnCall.url}",
              |  "backUrl": "$selfUrl${backCall.url}"
              |}
              |""".stripMargin
          )
        ),
        () =>
          connector.startPaymentJourney(
            cgtReference,
            Some(chargeReference),
            amount,
            returnCall,
            backCall
          )
      )

    }

  }

}
