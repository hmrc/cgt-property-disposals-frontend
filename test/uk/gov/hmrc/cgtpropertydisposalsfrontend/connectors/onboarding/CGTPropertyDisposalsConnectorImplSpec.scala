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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.onboarding

import org.scalatest.EitherValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.i18n.Lang
import play.api.libs.json.{JsString, Json}
import play.api.test.Helpers.{await, defaultAwaitTimeout}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.CGTPropertyDisposalsConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.TelephoneNumber
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address.UkAddress
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Postcode
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.email.Email
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.BusinessPartnerRecordGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.IdGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.OnboardingDetailsGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.CgtReference
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.{ContactName, IndividualName}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.bpr.BusinessPartnerRecordRequest
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.{RegistrationDetails, SubscribedDetails, SubscribedUpdateDetails, SubscriptionDetails}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{ConnectorSupport, WireMockMethods}
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}

class CGTPropertyDisposalsConnectorImplSpec
    extends AnyWordSpec
    with Matchers
    with ConnectorSupport
    with WireMockMethods
    with EitherValues {
  override lazy val serviceId = "cgt-property-disposals"

  private val connector = app.injector.instanceOf[CGTPropertyDisposalsConnector]

  "CGTPropertyDisposalsConnectorImpl" when {
    implicit val hc: HeaderCarrier = HeaderCarrier()

    "handling request to update to the subscription details" must {
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
        registeredWithId = true
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
        registeredWithId = true
      )

      val subscribedUpdateDetails =
        SubscribedUpdateDetails(newSubscribedDetails, previousSubscribedDetails)

      "do a get http call and return the result" in {
        List(
          HttpResponse(200, "{}"),
          HttpResponse(200, JsString("hi"), Map.empty[String, Seq[String]]),
          HttpResponse(500, "{}")
        ).foreach { httpResponse =>
          withClue(s"For http response [${httpResponse.toString}]") {
            when(
              PUT,
              "/cgt-property-disposals/subscription",
              body = Some(Json.toJson(subscribedUpdateDetails).toString())
            )
              .thenReturn(httpResponse.status, httpResponse.body)

            val result = await(connector.updateSubscribedDetails(subscribedUpdateDetails).value).value
            result.status shouldBe httpResponse.status
            result.body   shouldBe httpResponse.body
          }
        }
      }

      "return an error" when {
        "the future fails" in {
          when(
            PUT,
            "/cgt-property-disposals/subscription",
            body = Some(Json.toJson(subscribedUpdateDetails).toString())
          ).thenFail

          await(connector.updateSubscribedDetails(subscribedUpdateDetails).value).isLeft shouldBe true
        }
      }
    }

    "handling request to get the subscription status" must {
      "do a get http call and return the result" in {
        List(
          HttpResponse(200, "{}"),
          HttpResponse(200, JsString("hi"), Map.empty[String, Seq[String]]),
          HttpResponse(500, "{}")
        ).foreach { httpResponse =>
          withClue(s"For http response [${httpResponse.toString}]") {
            when(
              GET,
              "/cgt-property-disposals/check-subscription-status"
            )
              .thenReturn(httpResponse.status, httpResponse.body)

            val result = await(connector.getSubscriptionStatus().value).value
            result.status shouldBe httpResponse.status
            result.body   shouldBe httpResponse.body
          }
        }
      }

      "return an error" when {
        "the future fails" in {
          when(
            GET,
            "/cgt-property-disposals/check-subscription-status"
          ).thenFail

          await(connector.getSubscriptionStatus().value).isLeft shouldBe true
        }
      }
    }

    "handling request to get the business partner record" must {
      val bprRequest = sample[BusinessPartnerRecordRequest]
      val lang       = Lang.defaultLang

      "do a get http call and return the result" in {
        List(
          HttpResponse(200, "{}"),
          HttpResponse(200, JsString("hi"), Map.empty[String, Seq[String]]),
          HttpResponse(500, "{}")
        ).foreach { httpResponse =>
          withClue(s"For http response [${httpResponse.toString}]") {
            when(
              POST,
              "/cgt-property-disposals/business-partner-record",
              body = Some(Json.toJson(bprRequest).toString())
            )
              .thenReturn(httpResponse.status, httpResponse.body)

            val result = await(connector.getBusinessPartnerRecord(bprRequest, lang).value).value
            result.status shouldBe httpResponse.status
            result.body   shouldBe httpResponse.body
          }
        }
      }

      "return an error" when {
        "the future fails" in {
          when(
            POST,
            "/cgt-property-disposals/business-partner-record",
            body = Some(Json.toJson(bprRequest).toString())
          ).thenFail

          await(connector.getBusinessPartnerRecord(bprRequest, lang).value).isLeft shouldBe true
        }
      }
    }

    "handling request to subscribe" must {
      val subscriptionDetails = sample[SubscriptionDetails]

      val lang = Lang("CY")

      "do a get http call and return the result" in {
        List(
          HttpResponse(200, "{}"),
          HttpResponse(200, JsString("hi"), Map.empty[String, Seq[String]]),
          HttpResponse(500, "{}")
        ).foreach { httpResponse =>
          withClue(s"For http response [${httpResponse.toString}]") {
            when(
              POST,
              "/cgt-property-disposals/subscription",
              body = Some(Json.toJson(subscriptionDetails).toString())
            )
              .thenReturn(httpResponse.status, httpResponse.body)

            val result = await(connector.subscribe(subscriptionDetails, lang).value).value
            result.status shouldBe httpResponse.status
            result.body   shouldBe httpResponse.body
          }
        }
      }

      "return an error" when {
        "the future fails" in {
          when(
            POST,
            "/cgt-property-disposals/subscription",
            body = Some(Json.toJson(subscriptionDetails).toString())
          ).thenFail

          await(connector.subscribe(subscriptionDetails, lang).value).isLeft shouldBe true
        }
      }
    }

    "handling request to register without id" must {
      val registrationDetails = sample[RegistrationDetails]

      "do a get http call and return the result" in {
        List(
          HttpResponse(200, "{}"),
          HttpResponse(200, JsString("hi"), Map.empty[String, Seq[String]]),
          HttpResponse(500, "{}")
        ).foreach { httpResponse =>
          withClue(s"For http response [${httpResponse.toString}]") {
            when(
              POST,
              "/cgt-property-disposals/register-without-id",
              body = Some(Json.toJson(registrationDetails).toString())
            )
              .thenReturn(httpResponse.status, httpResponse.body)

            val result = await(connector.registerWithoutId(registrationDetails).value).value
            result.status shouldBe httpResponse.status
            result.body   shouldBe httpResponse.body
          }
        }
      }

      "return an error" when {
        "the future fails" in {
          when(
            POST,
            "/cgt-property-disposals/register-without-id",
            body = Some(Json.toJson(registrationDetails).toString())
          ).thenFail

          await(connector.registerWithoutId(registrationDetails).value).isLeft shouldBe true
        }
      }
    }

    "handling request to get subscribed details" must {
      val cgtReference = sample[CgtReference]

      "do a get http call and return the result" in {
        List(
          HttpResponse(200, "{}"),
          HttpResponse(200, JsString("hi"), Map.empty[String, Seq[String]]),
          HttpResponse(500, "{}")
        ).foreach { httpResponse =>
          withClue(s"For http response [${httpResponse.toString}]") {
            when(
              GET,
              s"/cgt-property-disposals/subscription/${cgtReference.value}"
            )
              .thenReturn(httpResponse.status, httpResponse.body)

            val result = await(connector.getSubscribedDetails(cgtReference).value).value
            result.status shouldBe httpResponse.status
            result.body   shouldBe httpResponse.body
          }
        }
      }

      "return an error" when {
        "the future fails" in {
          when(
            GET,
            s"/cgt-property-disposals/subscription/${cgtReference.value}"
          ).thenFail

          await(connector.getSubscribedDetails(cgtReference).value).isLeft shouldBe true
        }
      }
    }
  }
}
