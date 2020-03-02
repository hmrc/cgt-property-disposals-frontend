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

import java.time.LocalDate

import cats.data.EitherT
import org.scalamock.scalatest.MockFactory
import org.scalatest.{Matchers, WordSpec}
import play.api.libs.json.{JsNumber, Json}
import play.api.test.Helpers._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.onboarding.FinancialDataConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.metrics.MockMetrics
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Error
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators.sample
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.CgtReference
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.FinancialTransaction
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.FinancialDataServiceImpl
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.FinancialDataServiceImpl.FinancialDataResponse

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class FinancialDataServiceSpec extends WordSpec with Matchers with MockFactory {

  val mockConnector = mock[FinancialDataConnector]

  val service = new FinancialDataServiceImpl(mockConnector, MockMetrics.metrics)

  def mockGetFinancialData(cgtReference: CgtReference)(response: Either[Error, HttpResponse]) =
    (mockConnector
      .getFinancialData(_: CgtReference, _: LocalDate, _: LocalDate)(_: HeaderCarrier))
      .expects(cgtReference, fromDate, toDate, *)
      .returning(EitherT(Future.successful(response)))

  val cgtReference       = sample[CgtReference]
  val (fromDate, toDate) = LocalDate.of(2019, 4, 6) -> LocalDate.of(2020, 4, 5)

  "FinancialDataServiceImpl" when {

    implicit val hc: HeaderCarrier = HeaderCarrier()
    val financialTransactions      = List(sample[FinancialTransaction])

    "handling request to get financial data" must {

      "return an error" when {

        "the http call comes back with a status other than 200 or 204" in {
          mockGetFinancialData(cgtReference)(Right(HttpResponse(400, Some(Json.toJson(financialTransactions)))))
          await(service.getFinancialData(cgtReference).value).isLeft shouldBe true
        }

        "the JSON body of the response cannot be parsed" in {
          mockGetFinancialData(cgtReference)(Right(HttpResponse(200, Some(JsNumber(1)))))

          await(service.getFinancialData(cgtReference).value).isLeft shouldBe true
        }

        "the http response comes back with status 200 but the body cannot be parsed" in {
          mockGetFinancialData(cgtReference)(Left(Error("")))

          await(service.getFinancialData(cgtReference).value).isLeft shouldBe true
        }

      }

      "return an ok response" when {
        val financialDataResponse = FinancialDataResponse(List(sample[FinancialTransaction]))

        "the http call came back with a 200 and the body can be parsed" in {
          mockGetFinancialData(cgtReference)(Right(HttpResponse(OK, Some(Json.toJson(financialDataResponse)))))

          await(service.getFinancialData(cgtReference).value) shouldBe Right(
            financialDataResponse.financialTransactions
          )
        }

      }

    }

  }
}
