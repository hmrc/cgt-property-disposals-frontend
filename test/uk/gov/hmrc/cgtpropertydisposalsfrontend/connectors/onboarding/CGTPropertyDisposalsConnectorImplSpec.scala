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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.onboarding

import com.typesafe.config.ConfigFactory
import org.scalamock.scalatest.MockFactory
import org.scalatest.{Matchers, WordSpec}
import play.api.libs.json.Json
import play.api.{Configuration, Mode}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.{ConnectorSpec, HttpSupport}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.TelephoneNumber
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address.UkAddress
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Postcode
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.CgtReference
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.{ContactName, IndividualName}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.bpr.BusinessPartnerRecordRequest
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.email.Email
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.{RegistrationDetails, SubscribedDetails, SubscribedUpdateDetails, SubscriptionDetails}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.config.{RunMode, ServicesConfig}

import scala.concurrent.ExecutionContext.Implicits.global

class CGTPropertyDisposalsConnectorImplSpec
    extends WordSpec
    with Matchers
    with MockFactory
    with HttpSupport
    with ConnectorSpec {

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

    "handling request to update to the subscription details" must {

      val subscriptionStatusUrl = "http://host:123/cgt-property-disposals/subscription"

      val newSubscribedDetails = SubscribedDetails(
        Right(IndividualName("Stephen", "Wood")),
        Email("stephen@abc.co.uk"),
        UkAddress(
          "100 Sutton Street",
          Some("Wokingham"),
          Some("Surrey"),
          Some("London"),
          Postcode("DH14EJ")
        ),
        ContactName("Stephen Wood"),
        CgtReference("XFCGT123456789"),
        Some(TelephoneNumber("(+013)32752856")),
        true
      )

      val previousSubscribedDetails = SubscribedDetails(
        Right(IndividualName("Stephen", "Wood")),
        Email("stephen@abc.co.uk"),
        UkAddress(
          "100 Sutton Street",
          Some("Wokingham"),
          Some("Surrey"),
          Some("London"),
          Postcode("DH14EJ")
        ),
        ContactName("John Wick"),
        CgtReference("XFCGT123456789"),
        Some(TelephoneNumber("(+013)32752856")),
        true
      )

      val subscribedUpdateDetails = SubscribedUpdateDetails(newSubscribedDetails, previousSubscribedDetails)

      behave like connectorBehaviour(
        mockPut(subscriptionStatusUrl, subscribedUpdateDetails)(_),
        () => connector.updateSubscribedDetails(subscribedUpdateDetails)
      )
    }

    "handling request to get the subscription status" must {

      val subscriptionStatusUrl = "http://host:123/cgt-property-disposals/check-subscription-status"

      behave like connectorBehaviour(
        mockGet(subscriptionStatusUrl, Map.empty)(_),
        () => connector.getSubscriptionStatus()
      )
    }

    "handling request to get the business partner record" must {

      val bprUrl     = "http://host:123/cgt-property-disposals/business-partner-record"
      val bprRequest = sample[BusinessPartnerRecordRequest]

      behave like connectorBehaviour(
        mockPost(bprUrl, Map.empty, Json.toJson(bprRequest))(_),
        () => connector.getBusinessPartnerRecord(bprRequest)
      )

    }

    "handling request to subscribe" must {
      val subscriptionDetails = sample[SubscriptionDetails]

      behave like connectorBehaviour(
        mockPost("http://host:123/cgt-property-disposals/subscription", Map.empty, Json.toJson(subscriptionDetails))(_),
        () => connector.subscribe(subscriptionDetails)
      )
    }

    "handling request to register without id" must {
      val registrationDetails = sample[RegistrationDetails]

      behave like connectorBehaviour(
        mockPost(
          "http://host:123/cgt-property-disposals/register-without-id",
          Map.empty,
          Json.toJson(registrationDetails)
        )(_),
        () => connector.registerWithoutId(registrationDetails)
      )

    }

    "handling request to get subscribed details" must {
      val cgtReference = sample[CgtReference]

      behave like connectorBehaviour(
        mockGet(
          s"http://host:123/cgt-property-disposals/subscription/${cgtReference.value}",
          Map.empty
        )(_),
        () => connector.getSubscribedDetails(cgtReference)
      )

    }
  }

}
