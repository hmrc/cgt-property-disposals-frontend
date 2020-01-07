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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.services.onboarding

import cats.data.EitherT
import org.scalamock.scalatest.MockFactory
import org.scalatest.{Matchers, WordSpec}
import play.api.libs.json.{JsNumber, Json}
import play.api.test.Helpers._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.onboarding.CGTPropertyDisposalsConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.metrics.MockMetrics
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.SubscriptionResponse.{AlreadySubscribed, SubscriptionSuccessful}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.CgtReference
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.{RegistrationDetails, SubscribedDetails, SubscribedUpdateDetails, SubscriptionDetails}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Error
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators._
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class SubscriptionServiceImplSpec extends WordSpec with Matchers with MockFactory {

  val mockConnector = mock[CGTPropertyDisposalsConnector]

  val service = new SubscriptionServiceImpl(mockConnector, MockMetrics.metrics)

  def mockSubscribe(expectedSubscriptionDetails: SubscriptionDetails)(response: Either[Error, HttpResponse]) =
    (mockConnector
      .subscribe(_: SubscriptionDetails)(_: HeaderCarrier))
      .expects(expectedSubscriptionDetails, *)
      .returning(EitherT(Future.successful(response)))

  def mockHasSubscription()(response: Either[Error, HttpResponse]) =
    (mockConnector
      .getSubscriptionStatus()(_: HeaderCarrier))
      .expects(*)
      .returning(EitherT(Future.successful(response)))

  def mockRegisterWithoutIdAndSubscribe(
    expectedRegistrationDetails: RegistrationDetails
  )(response: Either[Error, HttpResponse]) =
    (mockConnector
      .registerWithoutIdAndSubscribe(_: RegistrationDetails)(_: HeaderCarrier))
      .expects(expectedRegistrationDetails, *)
      .returning(EitherT(Future.successful(response)))

  def mockGetSusbcribedDetails(cgtReference: CgtReference)(response: Either[Error, HttpResponse]) =
    (mockConnector
      .getSubscribedDetails(_: CgtReference)(_: HeaderCarrier))
      .expects(cgtReference, *)
      .returning(EitherT(Future.successful(response)))

  def mockUpdateSubscriptionDetails(
    subscribedAndVerifierDetails: SubscribedUpdateDetails
  )(response: Either[Error, HttpResponse]) =
    (mockConnector
      .updateSubscribedDetails(_: SubscribedUpdateDetails)(_: HeaderCarrier))
      .expects(subscribedAndVerifierDetails, *)
      .returning(EitherT(Future.successful(response)))

  "SubscriptionServiceImpl" when {

    implicit val hc: HeaderCarrier = HeaderCarrier()

    "handling request to check if subscribed" must {

      "return an error" when {

        "the http call comes back with a status other than 200 or 204" in {
          mockHasSubscription()(Right(HttpResponse(400)))
          await(service.hasSubscription().value).isLeft shouldBe true
        }

        "there is no JSON in the body of the http response" in {
          mockHasSubscription()(Right(HttpResponse(200)))
          await(service.hasSubscription().value).isLeft shouldBe true
        }

      }

      "return the cgt reference if the call comes back with a 200" in {
        val cgtReferenceNumber = "number"
        val jsonBody = Json.parse(
          s"""
             |{
             |  "value" : "$cgtReferenceNumber"
             |}
             |""".stripMargin
        )

        mockHasSubscription()(Right(HttpResponse(200, Some(jsonBody))))
        await(service.hasSubscription().value) shouldBe Right(Some(CgtReference(cgtReferenceNumber)))
      }

      "return None if the call comes back with a 204" in {
        mockHasSubscription()(Right(HttpResponse(204)))
        await(service.hasSubscription().value) shouldBe Right(None)
      }
    }

    "handling requests to subscribe" must {

      val subscriptionDetails = sample[SubscriptionDetails]

      "return an error" when {

        "the http call comes back with a status other than 200" in {
          mockSubscribe(subscriptionDetails)(Right(HttpResponse(500)))
          await(service.subscribe(subscriptionDetails).value).isLeft shouldBe true
        }

        "there is no JSON in the body of the http response" in {
          mockSubscribe(subscriptionDetails)(Right(HttpResponse(200)))
          await(service.subscribe(subscriptionDetails).value).isLeft shouldBe true
        }

        "the JSON body of the response cannot be parsed" in {
          mockSubscribe(subscriptionDetails)(Right(HttpResponse(200, Some(JsNumber(1)))))
          await(service.subscribe(subscriptionDetails).value).isLeft shouldBe true
        }

      }

      "return the subscription response if the call comes back with a " +
        "200 status and the JSON body can be parsed" in {
        val cgtReferenceNumber = "number"
        val jsonBody = Json.parse(
          s"""
             |{
             |  "cgtReferenceNumber" : "$cgtReferenceNumber"
             |}
             |""".stripMargin
        )

        mockSubscribe(subscriptionDetails)(Right(HttpResponse(200, Some(jsonBody))))
        await(service.subscribe(subscriptionDetails).value) shouldBe Right(SubscriptionSuccessful(cgtReferenceNumber))
      }

      "return an already subscribed response" when {

        "the response comes back with status 409 (conflict)" in {
          mockSubscribe(subscriptionDetails)(Right(HttpResponse(409)))

          await(service.subscribe(subscriptionDetails).value) shouldBe Right(AlreadySubscribed)
        }

      }

    }

    "handling requests to register without id and subscribe" must {

      val registrationDetails = sample[RegistrationDetails]

      "return an error" when {

        "the http call comes back with a status other than 200" in {
          mockRegisterWithoutIdAndSubscribe(registrationDetails)(Right(HttpResponse(500)))

          await(service.registerWithoutIdAndSubscribe(registrationDetails).value).isLeft shouldBe true
        }

        "there is no JSON in the body of the http response" in {
          mockRegisterWithoutIdAndSubscribe(registrationDetails)(Right(HttpResponse(200)))

          await(service.registerWithoutIdAndSubscribe(registrationDetails).value).isLeft shouldBe true
        }

        "the JSON body of the response cannot be parsed" in {
          mockRegisterWithoutIdAndSubscribe(registrationDetails)(Right(HttpResponse(200, Some(JsNumber(1)))))

          await(service.registerWithoutIdAndSubscribe(registrationDetails).value).isLeft shouldBe true
        }

      }

      "return the subscription response if the call comes back with a " +
        "200 status and the JSON body can be parsed" in {
        val cgtReferenceNumber = "number"
        val jsonBody = Json.parse(
          s"""
             |{
             |  "cgtReferenceNumber" : "$cgtReferenceNumber"
             |}
             |""".stripMargin
        )

        mockRegisterWithoutIdAndSubscribe(registrationDetails)(Right(HttpResponse(200, Some(jsonBody))))

        await(service.registerWithoutIdAndSubscribe(registrationDetails).value) shouldBe Right(
          SubscriptionSuccessful(cgtReferenceNumber)
        )
      }

      "return an already subscribed response" when {

        "the response comes back with status 409 (conflict)" in {
          mockRegisterWithoutIdAndSubscribe(registrationDetails)(Right(HttpResponse(409)))

          await(service.registerWithoutIdAndSubscribe(registrationDetails).value) shouldBe Right(AlreadySubscribed)
        }

      }
    }

    "handling requests to get subscribed details" must {

      val cgtReference = sample[CgtReference]

      "return an error" when {

        "the http call comes back with a status other than 200" in {
          mockGetSusbcribedDetails(cgtReference)(Right(HttpResponse(500)))

          await(service.getSubscribedDetails(cgtReference).value).isLeft shouldBe true
        }

        "there is no JSON in the body of the http response" in {
          mockGetSusbcribedDetails(cgtReference)(Right(HttpResponse(200)))

          await(service.getSubscribedDetails(cgtReference).value).isLeft shouldBe true
        }

        "the JSON body of the response cannot be parsed" in {
          mockGetSusbcribedDetails(cgtReference)(Right(HttpResponse(200, Some(JsNumber(1)))))

          await(service.getSubscribedDetails(cgtReference).value).isLeft shouldBe true
        }
      }

      "return subscribed details if the call comes back with status 200 and the JSON " +
        "body of the response can be parsed" in {
        val subscribedDetails = sample[SubscribedDetails]

        mockGetSusbcribedDetails(cgtReference)(Right(HttpResponse(200, Some(Json.toJson(subscribedDetails)))))

        await(service.getSubscribedDetails(cgtReference).value) shouldBe Right(subscribedDetails)
      }

    }

    "handling requests to update subscribed details" must {

      val subscribedDetails = sample[SubscribedUpdateDetails]

      "return an error" when {

        "the http call comes back with a status other than 200" in {
          mockUpdateSubscriptionDetails(subscribedDetails)(Right(HttpResponse(500)))

          await(service.updateSubscribedDetails(subscribedDetails).value).isLeft shouldBe true
        }

      }

      "return subscribed details if the call comes back with status 200 and the JSON " +
        "body of the response can be parsed" in {
        val subscribedDetails = sample[SubscribedUpdateDetails]

        mockUpdateSubscriptionDetails(subscribedDetails)(Right(HttpResponse(200, Some(Json.toJson(subscribedDetails)))))

        await(service.updateSubscribedDetails(subscribedDetails).value) shouldBe Right(())
      }

    }

  }

}
