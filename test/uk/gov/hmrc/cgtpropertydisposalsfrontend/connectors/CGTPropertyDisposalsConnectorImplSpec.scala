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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors

import com.typesafe.config.ConfigFactory
import org.scalamock.scalatest.MockFactory
import org.scalatest.{Matchers, WordSpec}
import play.api.libs.json.{JsString, Json}
import play.api.test.Helpers._
import play.api.{Configuration, Mode}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address.UkAddress
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.bpr.BusinessPartnerRecordRequest
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.CgtReference
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.{ContactName, IndividualName}
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}
import uk.gov.hmrc.play.bootstrap.config.{RunMode, ServicesConfig}
import org.scalacheck.ScalacheckShapeless._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class CGTPropertyDisposalsConnectorImplSpec extends WordSpec with Matchers with MockFactory with HttpSupport {

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

  val connector =
    new CGTPropertyDisposalsConnectorImpl(mockHttp, new ServicesConfig(config, new RunMode(config, Mode.Test)))

  "CGTPropertyDisposalsConnectorImpl" when {

    implicit val hc: HeaderCarrier = HeaderCarrier()

    def commonBehaviour(
      performAction: () => Future[Either[Error, HttpResponse]],
      mockActions: Option[HttpResponse] => Unit
    ): Unit = {
      "do a http call and return the result" in {
        List(
          HttpResponse(204),
          HttpResponse(200, Some(JsString("hi"))),
          HttpResponse(500)
        ).foreach { httpResponse =>
          withClue(s"For http response [${httpResponse.toString}]") {
            mockActions(Some(httpResponse))

            await(performAction()) shouldBe Right(httpResponse)
          }
        }
      }

      "return an error" when {

        "the future fails" in {
          mockActions(None)

          await(performAction()).isLeft shouldBe true
        }

      }
    }

    "handling request to update to the subscription details" must {

      val subscriptionStatusUrl = "http://host:123/cgt-property-disposals/subscription"

      val subscribedDetails = SubscribedAndVerifierDetails(
        Right(IndividualName("Stephen", "Wood")),
        Email("stephen@abc.co.uk"),
        UkAddress(
          "100 Sutton Street",
          Some("Wokingham"),
          Some("Surrey"),
          Some("London"),
          "DH14EJ"
        ),
        None,
        ContactName("Stephen Wood"),
        CgtReference("XFCGT123456789"),
        Some(TelephoneNumber("(+013)32752856")),
        true
      )

      behave like commonBehaviour(
        () => connector.updateSubscribedDetails(subscribedDetails).value,
        mockPut(subscriptionStatusUrl, subscribedDetails)(_)
      )
    }

    "handling request to get the subscription status" must {

      val subscriptionStatusUrl = "http://host:123/cgt-property-disposals/check-subscription-status"

      behave like commonBehaviour(
        () => connector.getSubscriptionStatus().value,
        mockGet(subscriptionStatusUrl, Map.empty)(_)
      )
    }

    "handling request to get the business partner record" must {

      val bprUrl     = "http://host:123/cgt-property-disposals/business-partner-record"
      val bprRequest = sample[BusinessPartnerRecordRequest]

      behave like commonBehaviour(
        () => connector.getBusinessPartnerRecord(bprRequest).value,
        mockPost(bprUrl, Map.empty, Json.toJson(bprRequest))(_)
      )

    }

    "handling request to subscribe" must {
      val subscriptionDetails = sample[SubscriptionDetails]

      behave like commonBehaviour(
        () => connector.subscribe(subscriptionDetails).value,
        mockPost("http://host:123/cgt-property-disposals/subscription", Map.empty, Json.toJson(subscriptionDetails))(_)
      )
    }

    "handling request to register without id and subscribe" must {
      val registrationDetails = sample[RegistrationDetails]

      behave like commonBehaviour(
        () => connector.registerWithoutIdAndSubscribe(registrationDetails).value,
        mockPost(
          "http://host:123/cgt-property-disposals/register-without-id-and-subscribe",
          Map.empty,
          Json.toJson(registrationDetails)
        )(_)
      )

    }

    "handling request to get subscribed details" must {
      val cgtReference = sample[CgtReference]

      behave like commonBehaviour(
        () => connector.getSubscribedDetails(cgtReference).value,
        mockGet(
          s"http://host:123/cgt-property-disposals/subscription/${cgtReference.value}",
          Map.empty
        )(_)
      )

    }
  }

}
