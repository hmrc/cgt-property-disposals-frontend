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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.returns

import java.time.LocalDate
import java.util.UUID

import com.typesafe.config.ConfigFactory
import org.scalamock.scalatest.MockFactory
import org.scalatest.{Matchers, WordSpec}
import play.api.Configuration
import play.api.i18n.Lang
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.returns.ReturnsConnector.DeleteDraftReturnsRequest
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.{ConnectorSpec, HttpSupport}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.DraftReturnGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.FurtherReturnCalculationGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.IdGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.ReturnAPIGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.CgtReference
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{CalculateCgtTaxDueRequest, DraftReturn, SubmitReturnRequest, TaxableGainOrLossCalculationRequest, YearToDateLiabilityCalculationRequest}
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import scala.concurrent.ExecutionContext.Implicits.global

class ReturnsConnectorImplSpec extends WordSpec with Matchers with MockFactory with HttpSupport with ConnectorSpec {

  val config = Configuration(
    ConfigFactory.parseString(
      """
        |microservice {
        |  services {
        |    cgt-property-disposals {
        |      protocol = http
        |      host     = host
        |      port     = 123
        |    }
        |  }
        |}
        |""".stripMargin
    )
  )

  val connector = new ReturnsConnectorImpl(
    mockHttp,
    new ServicesConfig(config)
  )

  "ReturnsConnectorImpl" when {

    implicit val hc: HeaderCarrier = HeaderCarrier()

    "handling requests to store a draft return" must {

      val draftReturn  = sample[DraftReturn]
      val cgtReference = sample[CgtReference]
      val expectedUrl  = s"http://host:123/draft-return/${cgtReference.value}"

      behave like connectorBehaviour(
        mockPost(expectedUrl, Seq.empty, draftReturn),
        () => connector.storeDraftReturn(draftReturn, cgtReference)
      )
    }

    "handling requests to delete a draft returns" must {

      val ids         = List.fill(3)(UUID.randomUUID())
      val expectedUrl = s"http://host:123/draft-returns/delete"

      behave like connectorBehaviour(
        mockPost(expectedUrl, Seq.empty, DeleteDraftReturnsRequest(ids)),
        () => connector.deleteDraftReturns(ids)
      )
    }

    "handling requests to get a draft returns" must {

      val cgtReference = sample[CgtReference]
      val expectedUrl  = s"http://host:123/draft-returns/${cgtReference.value}"

      behave like connectorBehaviour(
        mockGet[HttpResponse](expectedUrl),
        () => connector.getDraftReturns(cgtReference)
      )
    }

    "handling request to submit a return" must {
      val submitReturnRequest = sample[SubmitReturnRequest]
      val expectedUrl         = s"http://host:123/return"
      val language            = Seq("Accept-Language" -> "en")

      behave like connectorBehaviour(
        mockPost(expectedUrl, language, submitReturnRequest),
        () => connector.submitReturn(submitReturnRequest, Lang("en"))
      )

    }

    "handling requests to list returns" must {

      val cgtReference       = sample[CgtReference]
      val (fromDate, toDate) =
        LocalDate.of(2020, 1, 2) -> LocalDate.of(2020, 3, 4)
      val expectedUrl =
        s"http://host:123/returns/${cgtReference.value}/2020-01-02/2020-03-04"

      behave like connectorBehaviour(
        mockGet[HttpResponse](expectedUrl),
        () => connector.listReturns(cgtReference, fromDate, toDate)
      )
    }

    "handling requests to display a return" must {

      val cgtReference = sample[CgtReference]
      val submissionId = "id"
      val expectedUrl  = s"http://host:123/return/${cgtReference.value}/id"

      behave like connectorBehaviour(
        mockGet[HttpResponse](expectedUrl),
        () => connector.displayReturn(cgtReference, submissionId)
      )
    }

    "handling requests to calculate cgt tax due" must {

      val request     = sample[CalculateCgtTaxDueRequest]
      val expectedUrl = s"http://host:123/calculate-tax-due"

      behave like connectorBehaviour(
        mockPost(expectedUrl, Seq.empty, request),
        () => connector.calculateTaxDue(request)
      )
    }

    "handling requests to calculate taxable gain or loss" must {

      val request     = sample[TaxableGainOrLossCalculationRequest]
      val expectedUrl = s"http://host:123/calculate-taxable-gain-or-loss"

      behave like connectorBehaviour(
        mockPost(expectedUrl, Seq.empty, request),
        () => connector.calculateTaxableGainOrLoss(request)
      )
    }

    "handling requests to calculate year to date liability" must {

      val request     = sample[YearToDateLiabilityCalculationRequest]
      val expectedUrl = s"http://host:123/calculate-year-to-date-liability"

      behave like connectorBehaviour(
        mockPost(expectedUrl, Seq.empty, request),
        () => connector.calculateYearToDateLiability(request)
      )
    }

    "handling requests to get tax years" must {
      val date        = LocalDate.of(2020, 1, 31)
      val expectedUrl = s"http://host:123/tax-year/2020-01-31"

      behave like connectorBehaviour(
        mockGet[HttpResponse](expectedUrl),
        () => connector.taxYear(date)
      )
    }

    "handling requests to get available tax years" must {
      val expectedUrl = s"http://host:123/available-tax-years"

      behave like connectorBehaviour(
        mockGet[HttpResponse](expectedUrl),
        () => connector.availableTaxYears()
      )
    }

  }
}
