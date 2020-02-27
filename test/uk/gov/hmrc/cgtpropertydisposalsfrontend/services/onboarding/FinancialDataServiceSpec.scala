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

import java.time.{Clock, LocalDate}

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
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.homepage.{FinancialDataResponse, FinancialTransaction}
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class FinancialDataServiceSpec extends WordSpec with Matchers with MockFactory {

  val mockConnector = mock[FinancialDataConnector]

  val service = new FinancialDataServiceImpl(mockConnector, MockMetrics.metrics)

  def mockGetFinancialData(cgtReference: String)(response: Either[Error, HttpResponse]) =
    (mockConnector
      .getFinancialData(_: String, _: String, _: String)(_: HeaderCarrier))
      .expects(cgtReference, fromDate, toDate, *)
      .returning(EitherT(Future.successful(response)))

  val cgtReference = sample[CgtReference]

  def fromDate: String = {
    val today = LocalDate.now(Clock.systemUTC())
    val startYear =
      if (LocalDate.now().isAfter(LocalDate.of(today.getYear, 4, 6)))
        today.getYear
      else
        today.getYear - 1
    LocalDate.of(startYear, 4, 6).toString
  }

  def toDate: String = LocalDate.now.toString

//  val (fromDate, toDate) = LocalDate.of(2020, 1, 31) -> LocalDate.of(2020, 11, 2)

  "FinancialDataServiceImpl" when {

    implicit val hc: HeaderCarrier = HeaderCarrier()

    "handling request to get financial data" must {

//      val jsonBody = Json.parse(
//        s"""
//           |{
//           |  "financialTransactions" : [
//           |  "outstandingAmount":"1000.0"
//           |  ]
//           |}
//           |""".stripMargin
//      )

      "return an error" when {
        val financialDataResponse = FinancialDataResponse(List(sample[FinancialTransaction]))

        "the http call comes back with a status other than 200 or 204" in {
          mockGetFinancialData(cgtReference.value)(Right(HttpResponse(400, Some(Json.toJson(financialDataResponse)))))
          await(service.getFinancialData(cgtReference.value).value).isLeft shouldBe true
        }

        "the JSON body of the response cannot be parsed" in {
          mockGetFinancialData(cgtReference.value)(Right(HttpResponse(200, Some(JsNumber(1)))))

          await(service.getFinancialData(cgtReference.value).value).isLeft shouldBe true
        }

        "the http response comes back with status 200 but the body cannot be parsed" in {
          mockGetFinancialData(cgtReference.value)(Left(Error("")))

          await(service.getFinancialData(cgtReference.value).value).isLeft shouldBe true
        }

      }

      "return an ok response" when {

        val financialDataResponse = FinancialDataResponse(List(sample[FinancialTransaction]))

        "the http call came back with a 200 and the body can be parsed" in {
          mockGetFinancialData(cgtReference.value)(Right(HttpResponse(OK, Some(Json.toJson(financialDataResponse)))))

          await(service.getFinancialData(cgtReference.value).value) shouldBe Right(financialDataResponse)
        }

      }

    }

  }
}
