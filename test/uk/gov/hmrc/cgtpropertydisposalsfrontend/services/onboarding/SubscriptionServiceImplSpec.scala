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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.services.onboarding

import cats.data.EitherT
import org.scalamock.scalatest.MockFactory
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.play.guice.GuiceOneServerPerSuite
import play.api.i18n.Lang
import play.api.libs.json.{JsNumber, JsObject, Json}
import play.api.test.Helpers.*
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.CGTPropertyDisposalsConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.metrics.Metrics
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Error
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.AddressSource.ManuallyEntered as ManuallyEnteredAddress
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.email.EmailSource.ManuallyEntered as ManuallyEnteredEmail
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.IdGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.OnboardingDetailsGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.RepresenteeAnswersGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.SubscribedDetailsGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.{CgtReference, SapNumber}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.ContactNameSource.ManuallyEntered as ManuallyEnteredContactName
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.*
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.SubscriptionResponse.{AlreadySubscribed, SubscriptionSuccessful}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.RepresenteeAnswers.CompleteRepresenteeAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.RepresenteeReferenceId.RepresenteeCgtReference
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class SubscriptionServiceImplSpec extends AnyWordSpec with Matchers with MockFactory with GuiceOneServerPerSuite {

  private val mockConnector = mock[CGTPropertyDisposalsConnector]

  val service = new SubscriptionServiceImpl(mockConnector, app.injector.instanceOf[Metrics])

  private def mockSubscribe(
    expectedSubscriptionDetails: SubscriptionDetails,
    expectedLang: Lang
  )(response: Either[Error, HttpResponse]) =
    (mockConnector
      .subscribe(_: SubscriptionDetails, _: Lang)(_: HeaderCarrier))
      .expects(expectedSubscriptionDetails, expectedLang, *)
      .returning(EitherT(Future.successful(response)))

  private def mockHasSubscription()(response: Either[Error, HttpResponse]) =
    (mockConnector
      .getSubscriptionStatus()(_: HeaderCarrier))
      .expects(*)
      .returning(EitherT(Future.successful(response)))

  private def mockRegisterWithoutId(
    expectedRegistrationDetails: RegistrationDetails
  )(response: Either[Error, HttpResponse]) =
    (mockConnector
      .registerWithoutId(_: RegistrationDetails)(_: HeaderCarrier))
      .expects(expectedRegistrationDetails, *)
      .returning(EitherT(Future.successful(response)))

  private def mockGetSubscribedDetails(
    cgtReference: CgtReference
  )(response: Either[Error, HttpResponse]) =
    (mockConnector
      .getSubscribedDetails(_: CgtReference)(_: HeaderCarrier))
      .expects(cgtReference, *)
      .returning(EitherT(Future.successful(response)))

  private def mockUpdateSubscriptionDetails(
    subscribedAndVerifierDetails: SubscribedUpdateDetails
  )(response: Either[Error, HttpResponse]) =
    (mockConnector
      .updateSubscribedDetails(_: SubscribedUpdateDetails)(_: HeaderCarrier))
      .expects(subscribedAndVerifierDetails, *)
      .returning(EitherT(Future.successful(response)))

  private val emptyJsonBody               = "{}"
  private val noJsonInBody                = ""
  private val cgtReferenceNumber          = "number"
  private val successfulSubscribeResponse = Json.parse(
    s"""
       |{
       |  "cgtReferenceNumber" : "$cgtReferenceNumber"
       |}
       |""".stripMargin
  )
  private val sapNumber                   = "number"
  private val successfulRegisterResponse  = Json.parse(
    s"""
       |{
       |  "sapNumber" : "$sapNumber"
       |}
       |""".stripMargin
  )

  "SubscriptionServiceImpl" when {

    implicit val hc: HeaderCarrier = HeaderCarrier()

    "handling request to check if subscribed" must {

      "return an error" when {

        "the http call comes back with a status other than 200 or 204" in {
          mockHasSubscription()(Right(HttpResponse(400, emptyJsonBody)))
          await(service.hasFailedCgtEnrolment().value).isLeft shouldBe true
        }

        "there is no JSON in the body of the http response" in {
          mockHasSubscription()(Right(HttpResponse(200, emptyJsonBody)))
          await(service.hasFailedCgtEnrolment().value).isLeft shouldBe true
        }

      }

      "return the cgt reference if the call comes back with a 200" in {
        val cgtReferenceNumber = "number"
        val jsonBody           = Json.parse(
          s"""
             |{
             |  "value" : "$cgtReferenceNumber"
             |}
             |""".stripMargin
        )
        mockHasSubscription()(Right(HttpResponse(200, jsonBody, Map[String, Seq[String]]().empty)))
        await(service.hasFailedCgtEnrolment().value) shouldBe Right(
          Some(CgtReference(cgtReferenceNumber))
        )
      }

      "return None if the call comes back with a 204" in {
        mockHasSubscription()(Right(HttpResponse(204, emptyJsonBody)))
        await(service.hasFailedCgtEnrolment().value) shouldBe Right(None)
      }
    }

    "handling requests to subscribe" must {

      val subscriptionDetails = sample[SubscriptionDetails]

      val lang = Lang("FR")

      "return an error" when {

        "the http call comes back with a status other than 200" in {
          mockSubscribe(subscriptionDetails, lang)(Right(HttpResponse(500, emptyJsonBody)))
          await(
            service.subscribe(subscriptionDetails, lang).value
          ).isLeft shouldBe true
        }

        "there is no JSON in the body of the http response" in {
          mockSubscribe(subscriptionDetails, lang)(Right(HttpResponse(200, emptyJsonBody)))
          await(
            service.subscribe(subscriptionDetails, lang).value
          ).isLeft shouldBe true
        }

        "the JSON body of the response cannot be parsed" in {
          mockSubscribe(subscriptionDetails, lang)(
            Right(HttpResponse(200, JsNumber(1), Map[String, Seq[String]]().empty))
          )
          await(
            service.subscribe(subscriptionDetails, lang).value
          ).isLeft shouldBe true
        }

      }

      "return the subscription response if the call comes back with a " +
        "200 status and the JSON body can be parsed" in {
          mockSubscribe(subscriptionDetails, lang)(
            Right(HttpResponse(200, successfulSubscribeResponse, Map[String, Seq[String]]().empty))
          )
          await(service.subscribe(subscriptionDetails, lang).value) shouldBe Right(
            SubscriptionSuccessful(cgtReferenceNumber)
          )
        }

      "return an already subscribed response" when {

        "the response comes back with status 409 (conflict)" in {
          mockSubscribe(subscriptionDetails, lang)(Right(HttpResponse(409, emptyJsonBody)))

          await(service.subscribe(subscriptionDetails, lang).value) shouldBe Right(
            AlreadySubscribed
          )
        }

      }

    }

    "handling requests to register without id" must {

      val registrationDetails = sample[RegistrationDetails]

      "return an error" when {

        "the http call comes back with a status other than 200" in {
          mockRegisterWithoutId(registrationDetails)(Right(HttpResponse(500, emptyJsonBody)))

          await(
            service.registerWithoutId(registrationDetails).value
          ).isLeft shouldBe true
        }

        "there is no JSON in the body of the http response" in {
          mockRegisterWithoutId(registrationDetails)(Right(HttpResponse(200, emptyJsonBody)))

          await(
            service.registerWithoutId(registrationDetails).value
          ).isLeft shouldBe true
        }

        "the JSON body of the response cannot be parsed" in {
          mockRegisterWithoutId(registrationDetails)(
            Right(HttpResponse(200, JsNumber(1), Map[String, Seq[String]]().empty))
          )

          await(
            service.registerWithoutId(registrationDetails).value
          ).isLeft shouldBe true
        }

      }

      "return the response if the call comes back with a " +
        "200 status and the JSON body can be parsed" in {
          mockRegisterWithoutId(registrationDetails)(
            Right(HttpResponse(200, successfulRegisterResponse, Map[String, Seq[String]]().empty))
          )

          await(
            service.registerWithoutId(registrationDetails).value
          ) shouldBe Right(
            RegisteredWithoutId(SapNumber(sapNumber))
          )
        }

    }

    "handling requests to get subscribed details" must {

      val cgtReference = sample[CgtReference]

      "return an error" when {

        "the http call comes back with a status other than 200" in {
          mockGetSubscribedDetails(cgtReference)(Right(HttpResponse(500, emptyJsonBody)))

          await(
            service.getSubscribedDetails(cgtReference).value
          ).isLeft shouldBe true
        }

        "there is no JSON in the body of the http response" in {
          mockGetSubscribedDetails(cgtReference)(Right(HttpResponse(200, noJsonInBody)))

          await(
            service.getSubscribedDetails(cgtReference).value
          ).isLeft shouldBe true
        }

        "the JSON body of the response cannot be parsed" in {
          mockGetSubscribedDetails(cgtReference)(
            Right(HttpResponse(200, JsNumber(1), Map[String, Seq[String]]().empty))
          )

          await(
            service.getSubscribedDetails(cgtReference).value
          ).isLeft shouldBe true
        }
      }

      "return subscribed details if the call comes back with status 200 and the JSON " +
        "body of the response can be parsed" in {
          val subscribedDetails = sample[SubscribedDetails]

          mockGetSubscribedDetails(cgtReference)(
            Right(
              HttpResponse(
                200,
                JsObject(
                  Map("subscribedDetails" -> Json.toJson(subscribedDetails))
                ),
                Map[String, Seq[String]]().empty
              )
            )
          )

          await(service.getSubscribedDetails(cgtReference).value) shouldBe Right(
            Some(subscribedDetails)
          )
        }

      "return None if the call comes back with status 200 and the JSON " +
        "body of the response doesn't contain subscribed details" in {
          mockGetSubscribedDetails(cgtReference)(
            Right(HttpResponse(200, Json.parse("{}"), Map[String, Seq[String]]().empty))
          )

          await(service.getSubscribedDetails(cgtReference).value) shouldBe Right(
            None
          )
        }

    }

    "handling requests to update subscribed details" must {

      val subscribedDetails = sample[SubscribedUpdateDetails]

      "return an error" when {

        "the http call comes back with a status other than 200" in {
          mockUpdateSubscriptionDetails(subscribedDetails)(
            Right(HttpResponse(500, emptyJsonBody))
          )

          await(
            service.updateSubscribedDetails(subscribedDetails).value
          ).isLeft shouldBe true
        }

      }

      "return subscribed details if the call comes back with status 200 and the JSON " +
        "body of the response can be parsed" in {
          val subscribedDetails = sample[SubscribedUpdateDetails]

          mockUpdateSubscriptionDetails(subscribedDetails)(
            Right(HttpResponse(200, Json.toJson(subscribedDetails), Map[String, Seq[String]]().empty))
          )

          await(
            service.updateSubscribedDetails(subscribedDetails).value
          ) shouldBe Right(())
        }

    }

    "handing request to register without id and subscribe" must {

      val representeeAnswers = sample[CompleteRepresenteeAnswers]

      val registrationDetails = RegistrationDetails(
        representeeAnswers.name,
        representeeAnswers.contactDetails.emailAddress,
        representeeAnswers.contactDetails.address,
        ManuallyEnteredEmail
      )

      val subscriptionDetails = SubscriptionDetails(
        Right(representeeAnswers.name),
        representeeAnswers.contactDetails.emailAddress,
        representeeAnswers.contactDetails.address,
        representeeAnswers.contactDetails.contactName,
        SapNumber(sapNumber),
        ManuallyEnteredEmail,
        ManuallyEnteredAddress,
        ManuallyEnteredContactName
      )

      val lang = Lang("IT")

      "successfully register and subscribed returning correct cgt reference" in {
        inSequence {
          mockRegisterWithoutId(registrationDetails)(
            Right(HttpResponse(200, successfulRegisterResponse, Map[String, Seq[String]]().empty))
          )
          mockSubscribe(subscriptionDetails, lang)(
            Right(HttpResponse(200, successfulSubscribeResponse, Map[String, Seq[String]]().empty))
          )
        }
        await(service.registerWithoutIdAndSubscribe(representeeAnswers, lang).value) shouldBe
          Right(RepresenteeCgtReference(CgtReference(cgtReferenceNumber)))
      }

      "fail when registration fails" in {
        mockRegisterWithoutId(registrationDetails)(
          Right(HttpResponse(500, successfulRegisterResponse, Map[String, Seq[String]]().empty))
        )
        await(service.registerWithoutIdAndSubscribe(representeeAnswers, lang).value) shouldBe
          Left(Error("Call to register without id came back with status 500"))
      }

      "fail when subscription fails" in {
        inSequence {
          mockRegisterWithoutId(registrationDetails)(
            Right(HttpResponse(200, successfulRegisterResponse, Map[String, Seq[String]]().empty))
          )
          mockSubscribe(subscriptionDetails, lang)(
            Right(HttpResponse(500, successfulSubscribeResponse, Map[String, Seq[String]]().empty))
          )
        }
        await(service.registerWithoutIdAndSubscribe(representeeAnswers, lang).value) shouldBe
          Left(Error("call to subscribe came back with status 500"))
      }

      "fail when subscription comes back with an 'already subscribed' response" in {
        inSequence {
          mockRegisterWithoutId(registrationDetails)(
            Right(HttpResponse(200, successfulRegisterResponse, Map[String, Seq[String]]().empty))
          )
          mockSubscribe(subscriptionDetails, lang)(
            Right(HttpResponse(409, successfulSubscribeResponse, Map[String, Seq[String]]().empty))
          )
        }
        await(service.registerWithoutIdAndSubscribe(representeeAnswers, lang).value) shouldBe
          Left(Error("User is already subscribed"))
      }

    }

  }

}
