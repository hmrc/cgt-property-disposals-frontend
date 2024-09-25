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

import org.scalatest.EitherValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.libs.json.{JsString, Json}
import play.api.test.Helpers.{await, defaultAwaitTimeout}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.AmountInPence
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.IdGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.MoneyGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.CgtReference
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{ConnectorSupport, WireMockMethods}
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}

import java.time.LocalDate

class PaymentsConnectorImplSpec
    extends AnyWordSpec
    with Matchers
    with ConnectorSupport
    with WireMockMethods
    with EitherValues {
  override lazy val serviceId = "payments"

  private val connector = app.injector.instanceOf[PaymentsConnector]

  implicit val hc: HeaderCarrier = HeaderCarrier()

  "PaymentsConnectorImpl" when {
    "handling requests to start a payments journey" must {
      val cgtReference    = sample[CgtReference]
      val chargeReference = sample[String]
      val amount          = sample[AmountInPence]
      val dueDate         = LocalDate.parse("2023-04-05")
      val returnCall      = controllers.routes.StartController.start()
      val backCall        = controllers.returns.routes.TaskListController.taskList()
      val expectedUrl     = "/pay-api/capital-gains-tax/journey/start"

      "do a get http call and return the result" in {
        List(
          HttpResponse(200, "{}"),
          HttpResponse(200, JsString("hi"), Map.empty[String, Seq[String]]),
          HttpResponse(500, "{}")
        ).foreach { httpResponse =>
          withClue(s"For http response [${httpResponse.toString}]") {
            when(
              POST,
              expectedUrl,
              body = Some(
                Json
                  .parse(
                    s"""
                   |{
                   |  "cgtReference": "${cgtReference.value}",
                   |  "chargeReference": "$chargeReference",
                   |  "amountInPence": ${amount.value},
                   |  "dueDate": "2023-04-05",
                   |  "returnUrl": "$selfUrl${returnCall.url}",
                   |  "backUrl": "$selfUrl${backCall.url}"
                   |}
                   |""".stripMargin
                  )
                  .toString()
              )
            ).thenReturn(
              httpResponse.status,
              httpResponse.body
            )
            val result = await(
              connector
                .startPaymentJourney(cgtReference, Some(chargeReference), amount, Some(dueDate), returnCall, backCall)
                .value
            ).value

            result.status shouldBe httpResponse.status
            result.body   shouldBe httpResponse.body
          }
        }
      }

      "return an error" when {
        "the future fails" in {
          wireMockServer.stop()
          when(
            POST,
            expectedUrl,
            body = Some(
              Json
                .parse(
                  s"""
                 |{
                 |  "cgtReference": "${cgtReference.value}",
                 |  "chargeReference": "$chargeReference",
                 |  "amountInPence": ${amount.value},
                 |  "dueDate": "2023-04-05",
                 |  "returnUrl": "$selfUrl${returnCall.url}",
                 |  "backUrl": "$selfUrl${backCall.url}"
                 |}
                 |""".stripMargin
                )
                .toString()
            )
          )

          await(
            connector
              .startPaymentJourney(cgtReference, Some(chargeReference), amount, Some(dueDate), returnCall, backCall)
              .value
          ).isLeft shouldBe true
          wireMockServer.start()
        }
      }
    }
  }
}
