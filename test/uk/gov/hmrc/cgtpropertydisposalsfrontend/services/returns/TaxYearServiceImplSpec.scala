/*
 * Copyright 2021 HM Revenue & Customs
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

import java.time.LocalDate
import cats.data.EitherT
import cats.instances.future._
import org.scalamock.scalatest.MockFactory
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.libs.json.{JsString, Json}
import play.api.test.Helpers._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.returns.ReturnsConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.TaxYearGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, TaxYear}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.TaxYearServiceImpl.{AvailableTaxYearsResponse, TaxYearResponse}
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class TaxYearServiceImplSpec extends AnyWordSpec with Matchers with ScalaCheckDrivenPropertyChecks with MockFactory {

  val mockReturnsConnector = mock[ReturnsConnector]

  val service = new TaxYearServiceImpl(mockReturnsConnector)

  private val emptyJsonBody = "{}"

  def mockGetTaxYear(date: LocalDate)(response: Either[Error, HttpResponse]) =
    (mockReturnsConnector
      .taxYear(_: LocalDate)(_: HeaderCarrier))
      .expects(date, *)
      .returning(EitherT.fromEither[Future](response))

  def mockAvailableTaxYears()(response: Either[Error, HttpResponse]) =
    (mockReturnsConnector
      .availableTaxYears()(_: HeaderCarrier))
      .expects(*)
      .returning(EitherT.fromEither[Future](response))

  "TaxYearServiceImpl" when {

    "handling requests to get tax years" must {

      implicit val hc: HeaderCarrier = HeaderCarrier()

      val date = LocalDate.now()

      "return an error" when {

        "there is an error making the http call" in {
          mockGetTaxYear(date)(Left(Error("")))

          await(service.taxYear(date).value).isLeft shouldBe true
        }

        "the http call comes back with a status other than 200" in {
          mockGetTaxYear(date)(Right(HttpResponse(500, emptyJsonBody)))

          await(service.taxYear(date).value).isLeft shouldBe true
        }

        "the http call comes back with status 200 but the body cannot be parsed" in {
          mockGetTaxYear(date)(
            Right(HttpResponse(200, JsString("hello"), Map[String, Seq[String]]().empty))
          )

          await(service.taxYear(date).value).isLeft shouldBe true
        }
      }

      "return the calculation" when {

        "the call is successful and a tax year was found" in {
          val taxYear = sample[TaxYear]
          mockGetTaxYear(date)(
            Right(
              HttpResponse(
                200,
                Json.toJson(TaxYearResponse(Some(taxYear))),
                Map[String, Seq[String]]().empty
              )
            )
          )

          await(service.taxYear(date).value) shouldBe Right(Some(taxYear))
        }

        "the call is successful and a tax year was not found" in {
          mockGetTaxYear(date)(
            Right(HttpResponse(200, Json.toJson(TaxYearResponse(None)), Map[String, Seq[String]]().empty))
          )
          await(service.taxYear(date).value) shouldBe Right(None)
        }

      }

    }

    "handling requests to get available tax years" must {

      implicit val hc: HeaderCarrier = HeaderCarrier()

      "return an error" when {

        "there is an error making the http call" in {
          mockAvailableTaxYears()(Left(Error("")))

          await(service.availableTaxYears().value).isLeft shouldBe true
        }

        "the http call comes back with a status other than 200" in {
          mockAvailableTaxYears()(Right(HttpResponse(500, emptyJsonBody)))

          await(service.availableTaxYears().value).isLeft shouldBe true
        }

        "the http call comes back with status 200 but the body cannot be parsed" in {
          mockAvailableTaxYears()(
            Right(HttpResponse(200, JsString("hello"), Map[String, Seq[String]]().empty))
          )

          await(service.availableTaxYears().value).isLeft shouldBe true
        }

      }

      "return the available tax years" when {

        "the call is successful and a tax year was found" in {
          val taxYears = List(2020, 2021)
          mockAvailableTaxYears()(
            Right(
              HttpResponse(
                200,
                Json.toJson(AvailableTaxYearsResponse(taxYears)),
                Map[String, Seq[String]]().empty
              )
            )
          )

          await(service.availableTaxYears().value) shouldBe Right(taxYears)
        }

        "the call is successful and a tax year was not found" in {
          mockAvailableTaxYears()(
            Right(
              HttpResponse(200, Json.toJson(AvailableTaxYearsResponse(List.empty)), Map[String, Seq[String]]().empty)
            )
          )
          await(service.availableTaxYears().value) shouldBe Right(List.empty)
        }

      }

    }

  }

}
