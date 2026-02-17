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
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.TaxYearGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, TaxYear}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.TaxYearServiceImpl.{AvailableTaxYearsResponse, TaxYearResponse}
import uk.gov.hmrc.http.HeaderCarrier

import java.time.LocalDate
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class TaxYearServiceImplSpec extends AnyWordSpec with Matchers with SampledScalaCheck with MockFactory {

  private val mockReturnsConnector = mock[ReturnsConnector]

  val service = new TaxYearServiceImpl(mockReturnsConnector)

  private def mockGetTaxYear(date: LocalDate)(response: Either[Error, TaxYearResponse]) =
    (mockReturnsConnector
      .taxYear(_: LocalDate)(using _: HeaderCarrier))
      .expects(date, *)
      .returning(EitherT.fromEither[Future](response))

  private def mockAvailableTaxYears()(response: Either[Error, AvailableTaxYearsResponse]) =
    (mockReturnsConnector
      .availableTaxYears()(using _: HeaderCarrier))
      .expects(*)
      .returning(EitherT.fromEither[Future](response))

  "TaxYearServiceImpl" when {

    "handling requests to get tax years" must {

      implicit val hc: HeaderCarrier = HeaderCarrier()

      val date = LocalDate.now()

      "return an error" when {
        "the connector fails for any reason" in {
          mockGetTaxYear(date)(Left(Error("some error")))

          val result = await(service.taxYear(date).value)

          result shouldBe Left(Error("some error"))
        }
      }

      "return the calculation" when {

        "the call is successful and a tax year was found" in {
          val taxYear = sample[TaxYear]
          mockGetTaxYear(date)(Right(TaxYearResponse(Some(taxYear))))

          await(service.taxYear(date).value) shouldBe Right(Some(taxYear))
        }

        "the call is successful and a tax year was not found" in {
          mockGetTaxYear(date)(Right(TaxYearResponse(None)))
          await(service.taxYear(date).value) shouldBe Right(None)
        }
      }
    }

    "handling requests to get available tax years" must {

      implicit val hc: HeaderCarrier = HeaderCarrier()

      "return an error" when {
        "the connector fail for any reason" in {
          mockAvailableTaxYears()(Left(Error("some error")))

          val response = await(service.availableTaxYears().value)

          response shouldBe Left(Error("some error"))
        }
      }

      "return the available tax years" when {

        "the call is successful and a tax year was found" in {
          val taxYears = List(2020, 2021)

          mockAvailableTaxYears()(Right(AvailableTaxYearsResponse(taxYears)))

          await(service.availableTaxYears().value) shouldBe Right(taxYears)
        }

        "the call is successful and a tax year was not found" in {
          mockAvailableTaxYears()(Right(AvailableTaxYearsResponse(List.empty)))

          await(service.availableTaxYears().value) shouldBe Right(List.empty)
        }
      }
    }
  }
}
