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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns

import cats.data.EitherT
import cats.instances.future._
import org.scalamock.scalatest.MockFactory
import org.scalatest.{Matchers, WordSpec}
import play.api.libs.json.{JsNumber, Json}
import play.api.mvc.Call
import play.api.test.Helpers._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.returns.PaymentsConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.CgtReference
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.PaymentsJourney
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{AmountInPence, Error}
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class PaymentsServiceImplSpec extends WordSpec with Matchers with MockFactory {

  val mockConnector = mock[PaymentsConnector]

  def mockStartPaymentJourney(
    cgtReference: CgtReference,
    chargeReference: String,
    amount: AmountInPence,
    returnUrl: Call,
    backUrl: Call
  )(response: Either[Error, HttpResponse]) =
    (mockConnector
      .startPaymentJourney(_: CgtReference, _: String, _: AmountInPence, _: Call, _: Call)(_: HeaderCarrier))
      .expects(cgtReference, chargeReference, amount, returnUrl, backUrl, *)
      .returning(EitherT.fromEither[Future](response))

  val service = new PaymentsServiceImpl(mockConnector)

  implicit val hc: HeaderCarrier = HeaderCarrier()

  "PaymentsServiceImpl" when {

    "handling requests to start a payments journey" must {

      val cgtReference    = sample[CgtReference]
      val chargeReference = sample[String]
      val amount          = sample[AmountInPence]
      val returnCall      = controllers.routes.StartController.start()
      val backCall        = controllers.routes.EmailWhitelistingController.thereIsAProblem()

      "return an error" when {

        def test(response: Either[Error, HttpResponse]) = {
          mockStartPaymentJourney(cgtReference, chargeReference, amount, returnCall, backCall)(response)

          await(service.startPaymentJourney(cgtReference, chargeReference, amount, returnCall, backCall).value).isLeft shouldBe true
        }

        "the http call fails" in {
          test(Left(Error("")))
        }

        "the http call came back with a status other than 201" in {
          test(Right(HttpResponse(INTERNAL_SERVER_ERROR)))
        }

        "the http call came back with status 201 but there is no JSON body" in {
          test(Right(HttpResponse(CREATED)))
        }

        "the http call came back with status 201 and JSON body, but the JSON cannot be parsed" in {
          test(Right(HttpResponse(CREATED, Some(JsNumber(1)))))
        }

      }

      "return an ok response" when {

        "the http call came back with a 201 and the JSON " in {
          val response = PaymentsJourney("/next-url", "id")

          mockStartPaymentJourney(cgtReference, chargeReference, amount, returnCall, backCall)(
            Right(
              HttpResponse(
                CREATED,
                Some(Json.parse("""
                                  |{
                                  |  "journeyId": "id",
                                  |   "nextUrl": "/next-url"
                                  |}
                                  |""".stripMargin))
              )
            )
          )

          await(service.startPaymentJourney(cgtReference, chargeReference, amount, returnCall, backCall).value) shouldBe Right(
            response
          )
        }

      }

    }

  }
}
