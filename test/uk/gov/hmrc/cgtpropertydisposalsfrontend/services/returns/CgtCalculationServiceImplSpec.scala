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
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.libs.json.{JsString, Json}
import play.api.test.Helpers._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.returns.ReturnsConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Error
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.ReturnAPIGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.YearToDateLiabilityAnswersGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{CalculateCgtTaxDueRequest, CalculatedTaxDue}
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class CgtCalculationServiceImplSpec
    extends WordSpec
    with Matchers
    with ScalaCheckDrivenPropertyChecks
    with MockFactory {

  val mockReturnsConnector = mock[ReturnsConnector]

  val service = new CgtCalculationServiceImpl(mockReturnsConnector)

  def mockCalculateTaxDue(
    request: CalculateCgtTaxDueRequest
  )(response: Either[Error, HttpResponse]) =
    (mockReturnsConnector
      .calculateTaxDue(_: CalculateCgtTaxDueRequest)(_: HeaderCarrier))
      .expects(request, *)
      .returning(EitherT.fromEither[Future](response))

  private val emptyJsonBody = "{}"

  "CgtCalculationServiceImpl" when {

    "handling requests to calculate tax due" must {

      implicit val hc: HeaderCarrier = HeaderCarrier()

      val request = sample[CalculateCgtTaxDueRequest]

      "return an error" when {

        "there is an error making the http call" in {
          mockCalculateTaxDue(request)(Left(Error("")))

          await(service.calculateTaxDue(request).value).isLeft shouldBe true
        }

        "the http call comes back with a status other than 200" in {
          mockCalculateTaxDue(request)(Right(HttpResponse(500, emptyJsonBody)))

          await(service.calculateTaxDue(request).value).isLeft shouldBe true
        }

        "the http call comes back with status 200 but the body cannot be parsed" in {
          mockCalculateTaxDue(request)(
            Right(HttpResponse(200, JsString("hello"), Map[String, Seq[String]]().empty))
          )

          await(service.calculateTaxDue(request).value).isLeft shouldBe true
        }
      }

      "return the calculation" when {

        "the call is successful and the response body can be parsed" in {
          val calculatedTaxDue = sample[CalculatedTaxDue]

          mockCalculateTaxDue(request)(
            Right(HttpResponse(200, Json.toJson(calculatedTaxDue), Map[String, Seq[String]]().empty))
          )

          await(service.calculateTaxDue(request).value) shouldBe Right(
            calculatedTaxDue
          )
        }

      }

    }

  }

}
