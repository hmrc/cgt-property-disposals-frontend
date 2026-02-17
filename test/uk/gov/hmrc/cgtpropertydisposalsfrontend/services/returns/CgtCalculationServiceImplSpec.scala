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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns

import cats.data.EitherT
import cats.instances.future._
import org.scalamock.scalatest.MockFactory
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import uk.gov.hmrc.cgtpropertydisposalsfrontend.SampledScalaCheck
import play.api.test.Helpers._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.returns.ReturnsConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Error
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.FurtherReturnCalculationGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.ReturnAPIGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.YearToDateLiabilityAnswersGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns._
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class CgtCalculationServiceImplSpec extends AnyWordSpec with Matchers with SampledScalaCheck with MockFactory {
  private val mockReturnsConnector = mock[ReturnsConnector]

  val service = new CgtCalculationServiceImpl(mockReturnsConnector)

  private def mockCalculateTaxDue(
    request: CalculateCgtTaxDueRequest
  )(response: Either[Error, CalculatedTaxDue]) =
    (mockReturnsConnector
      .calculateTaxDue(_: CalculateCgtTaxDueRequest)(using _: HeaderCarrier))
      .expects(request, *)
      .returning(EitherT.fromEither[Future](response))

  private def mockCalculateTaxableGainOrLoss(
    request: TaxableGainOrLossCalculationRequest
  )(response: Either[Error, TaxableGainOrLossCalculation]) =
    (mockReturnsConnector
      .calculateTaxableGainOrLoss(_: TaxableGainOrLossCalculationRequest)(using _: HeaderCarrier))
      .expects(request, *)
      .returning(EitherT.fromEither[Future](response))

  private def mockCalculateYearToDateLiability(
    request: YearToDateLiabilityCalculationRequest
  )(response: Either[Error, YearToDateLiabilityCalculation]) =
    (mockReturnsConnector
      .calculateYearToDateLiability(_: YearToDateLiabilityCalculationRequest)(using _: HeaderCarrier))
      .expects(request, *)
      .returning(EitherT.fromEither[Future](response))

  "CgtCalculationServiceImpl" when {
    "handling requests to calculate tax due" must {
      implicit val hc: HeaderCarrier = HeaderCarrier()

      val request = sample[CalculateCgtTaxDueRequest]

      "return an error" when {
        "the connector fails for any reason" in {
          mockCalculateTaxDue(request)(Left(Error("some error")))

          val response = await(service.calculateTaxDue(request).value)

          response shouldBe Left(Error("some error"))
        }
      }

      "return the calculation" when {
        "the call is successful and the response body can be parsed" in {
          val calculatedTaxDue = sample[CalculatedTaxDue]

          mockCalculateTaxDue(request)(Right(calculatedTaxDue))

          await(service.calculateTaxDue(request).value) shouldBe Right(calculatedTaxDue)
        }
      }
    }

    "handling requests to calculate taxable gain or loss" must {
      implicit val hc: HeaderCarrier = HeaderCarrier()

      val request = sample[TaxableGainOrLossCalculationRequest]

      "return an error" when {
        "the connector fails for any reason" in {
          mockCalculateTaxableGainOrLoss(request)(Left(Error("some error")))

          val result = await(service.calculateTaxableGainOrLoss(request).value)

          result shouldBe Left(Error("some error"))
        }
      }

      "return the calculation" when {
        "the call is successful and the response body can be parsed" in {
          val calculation = sample[TaxableGainOrLossCalculation]

          mockCalculateTaxableGainOrLoss(request)(Right(calculation))

          await(service.calculateTaxableGainOrLoss(request).value) shouldBe Right(calculation)
        }
      }
    }

    "handling requests to calculate year to date liability" must {
      implicit val hc: HeaderCarrier = HeaderCarrier()

      val request = sample[YearToDateLiabilityCalculationRequest]

      "return an error" when {
        "the connector fails for any reason" in {
          mockCalculateYearToDateLiability(request)(Left(Error("some error")))

          val result = await(service.calculateYearToDateLiability(request).value)

          result shouldBe Left(Error("some error"))
        }
      }

      "return the calculation" when {
        "the call is successful and the response body can be parsed" in {
          val calculation = sample[YearToDateLiabilityCalculation]

          mockCalculateYearToDateLiability(request)(Right(calculation))

          await(service.calculateYearToDateLiability(request).value) shouldBe Right(calculation)
        }
      }
    }
  }
}
