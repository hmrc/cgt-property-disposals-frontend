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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.returns

import com.github.tomakehurst.wiremock.client.WireMock._
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.Tables.Table
import org.scalatest.wordspec.AnyWordSpec
import play.api.i18n.Lang
import play.api.libs.json.Json
import play.api.test.Helpers.{await, defaultAwaitTimeout}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.HttpSupport
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.returns.ReturnsConnector.DeleteDraftReturnsRequest
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, TaxYear}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.DraftReturnGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.ReturnAPIGen.{calculateCgtTaxDueRequestGen, listReturnsResponseGen, submitReturnRequestGen, submitReturnResponseGen}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.ReturnGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.YearToDateLiabilityAnswersGen.calculatedTaxDueGen
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.FurtherReturnCalculationGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.CgtReference
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsServiceImpl.{GetDraftReturnResponse, ListReturnsResponse}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.TaxYearServiceImpl.{TaxYearResponse, taxYearResponseFormat}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.ConnectorSupport
import uk.gov.hmrc.http.HeaderCarrier

import java.time.LocalDate
import java.util.{Locale, UUID}


class ReturnsConnectorImplSpec
    extends AnyWordSpec
    with Matchers
    with MockFactory
    with HttpSupport
    with ConnectorSupport {

  override lazy val serviceId = "cgt-property-disposals"
  private val con             = fakeApplication.injector.instanceOf[ReturnsConnector]

  "ReturnsConnectorImpl" when {

    implicit val hc: HeaderCarrier = HeaderCarrier()

    "handling requests to store a draft return" must {
      "call the right endpoint with draft return body" in {
        val draftReturn = sample[DraftReturn]
        stubFor(post(urlPathMatching(".*")).willReturn(jsonResponse(200, "")))

        await(con.storeDraftReturn(draftReturn, CgtReference("CGT12345678")).value)

        val url = "/draft-return/CGT12345678"
        verify(postRequestedFor(urlEqualTo(url)).withRequestBody(equalTo(Json.toJson(draftReturn).toString())))
      }

      "return Unit on success" in {
        val draftReturn = sample[DraftReturn]
        stubFor(post(urlPathMatching(".*")).willReturn(jsonResponse(200, "")))

        val result = await(con.storeDraftReturn(draftReturn, CgtReference("CGT12345678")).value)

        result shouldBe Right(())
      }

      "return Left on error" in {
        val table = Table("status", 403, 404, 500)
        for (status <- table) {
          val draftReturn = sample[DraftReturn]
          stubFor(post(urlPathMatching(".*")).willReturn(jsonResponse(status, "{}")))

          val result = await(con.storeDraftReturn(draftReturn, CgtReference("CGT12345678")).value)

          result shouldBe
            Left(Error(s"POST to http://localhost:11119/draft-return/CGT12345678 came back with with status $status"))
        }
      }
    }

    "handling requests to get a draft return" must {
      implicit val gen: Gen[GetDraftReturnResponse] = Gen.listOf(draftReturnGen).map(GetDraftReturnResponse(_))
      "call the right endpoint with draft return body" in {
        val response = sample[GetDraftReturnResponse]
        stubFor(get(urlPathMatching(".*")).willReturn(jsonResponse(200, Json.toJson(response).toString())))

        await(con.getDraftReturns(CgtReference("CGT12345678")).value)

        val url = "/draft-returns/CGT12345678"
        verify(getRequestedFor(urlEqualTo(url)))
      }

      "return Unit on success" in {
        val response = sample[GetDraftReturnResponse]
        stubFor(get(urlPathMatching(".*")).willReturn(jsonResponse(200, Json.toJson(response).toString())))

        val result = await(con.getDraftReturns(CgtReference("CGT12345678")).value)

        result shouldBe Right(response)
      }

      "return Left on error" in {
        val table = Table("status", 403, 404, 500)
        for (status <- table) {
          val response = sample[GetDraftReturnResponse]
          stubFor(get(urlPathMatching(".*")).willReturn(jsonResponse(status, Json.toJson(response).toString())))

          val result = await(con.getDraftReturns(CgtReference("CGT12345678")).value)

          result shouldBe
            Left(Error(s"GET to http://localhost:11119/draft-returns/CGT12345678 came back with with status $status"))
        }
      }
    }

    "handling requests to delete a draft return" must {
      "call the right endpoint with draft return body" in {
        val uuidList = List(UUID.randomUUID(), UUID.randomUUID())

        stubFor(post(urlPathMatching(".*")).willReturn(jsonResponse(200, "")))

        await(con.deleteDraftReturns(uuidList).value)

        val url = "/draft-returns/delete"
        val expectedRequest = DeleteDraftReturnsRequest(uuidList)
        verify(postRequestedFor(urlEqualTo(url)).withRequestBody(equalTo(Json.toJson(expectedRequest).toString())))
      }

      "return Unit on success" in {
        val uuidList = List(UUID.randomUUID(), UUID.randomUUID())
        stubFor(post(urlPathMatching(".*")).willReturn(jsonResponse(200, "")))

        val result =  await(con.deleteDraftReturns(uuidList).value)

        result shouldBe Right(())
      }

      "return Left on error" in {
        val table = Table("status", 403, 404, 500)
        for (status <- table) {
          val uuidList = List(UUID.randomUUID(), UUID.randomUUID())
          stubFor(post(urlPathMatching(".*")).willReturn(jsonResponse(status, "")))

          val result = await(con.deleteDraftReturns(uuidList).value)

          result shouldBe
            Left(Error(s"POST to http://localhost:11119/draft-returns/delete came back with with status $status"))
        }
      }
    }


    "handling requests to submit a return" must {
      "call the right endpoint with submit return body" in {
        val submitReturnRequest = sample[SubmitReturnRequest]
        val lang : Lang = new Lang(Locale.UK)

        stubFor(post(urlPathMatching(".*")).willReturn(jsonResponse(200, "")))

        await(con.submitReturn(submitReturnRequest,lang).value)

        val url = "/return"
        verify(postRequestedFor(urlEqualTo(url)).withRequestBody(equalTo(Json.toJson(submitReturnRequest).toString())))
      }

      "return Unit on success" in {
        val submitReturnRequest = sample[SubmitReturnRequest]
        val response = sample[SubmitReturnResponse]
        val lang : Lang = new Lang(Locale.UK)

        stubFor(post(urlPathMatching(".*")).willReturn(jsonResponse(200, Json.toJson(response).toString())))

        val result =  await(con.submitReturn(submitReturnRequest,lang).value)

        result shouldBe Right(response)
      }

      "return Left on error" in {
        val table = Table("status", 403, 404, 500)
        for (status <- table) {
          val uuidList = List(UUID.randomUUID(), UUID.randomUUID())
          stubFor(post(urlPathMatching(".*")).willReturn(jsonResponse(status, "")))

          val result = await(con.deleteDraftReturns(uuidList).value)

          result shouldBe
            Left(Error(s"POST to http://localhost:11119/draft-returns/delete came back with with status $status"))
        }
      }
    }

    "handling requests to list return" must {
      "call the right endpoint with list return body" in {
        val startDate = LocalDate.now()
        val response =  sample[ListReturnsResponse]

        stubFor(get(urlPathMatching(".*")).willReturn(jsonResponse(200, Json.toJson(response).toString())))

        await(con.listReturns(CgtReference("CGT12345678"),startDate,startDate).value)

        val url = "/returns/CGT12345678/2024-08-21/2024-08-21"

       verify(getRequestedFor(urlEqualTo(url)))

      }


      "return Unit on success" in {
        val startDate = LocalDate.now()
        val response =  sample[ListReturnsResponse]
        stubFor(get(urlPathMatching(".*")).willReturn(jsonResponse(200, Json.toJson(response).toString())))

        val result =  await(con.listReturns(CgtReference("CGT12345678"),startDate,startDate).value)

        result shouldBe Right(response)
      }

      "return Left on error" in {
        val startDate = LocalDate.now()
        val response =  sample[ListReturnsResponse]
        val table = Table("status", 403, 404, 500)
        for (status <- table) {
          stubFor(get(urlPathMatching(".*")).willReturn(jsonResponse(status, Json.toJson(response).toString())))

          val result = await(con.listReturns(CgtReference("CGT12345678"),startDate,startDate).value)

          result shouldBe
            Left(Error(s"GET to http://localhost:11119/returns/CGT12345678/2024-08-21/2024-08-21 came back with with status $status"))
        }
      }
    }

    "handling requests to display return" must {
      implicit val displayReturnGen: Gen[DisplayReturn] = for {
        completeReturn <- completeReturnGen
        returnType <- returnTypeGen
      } yield DisplayReturn (completeReturn,returnType)
      val response = sample[DisplayReturn]
      "call the right endpoint with display return body" in {

        stubFor(get(urlPathMatching(".*")).willReturn(jsonResponse(200, Json.toJson(response).toString())))

        await(con.displayReturn(CgtReference("CGT12345678"),"12345").value)

        val url = "/return/CGT12345678/12345"

        verify(getRequestedFor(urlEqualTo(url)))

        }
       "return Unit on success" in {
         val response = sample[DisplayReturn]

         stubFor(get(urlPathMatching(".*")).willReturn(jsonResponse(200, Json.toJson(response).toString())))

         val result =  await(con.displayReturn(CgtReference("CGT12345678"),"1234").value)

         result shouldBe Right(())
       }

       "return Left on error" in {
         val table = Table("status", 403, 404, 500)
         for (status <- table) {
           stubFor(get(urlPathMatching(".*")).willReturn(jsonResponse(status, Json.toJson(response).toString())))
           val result = await(con.displayReturn(CgtReference("CGT12345678"),"1234").value)
           result shouldBe
             Left(Error(s"GET to http://localhost:11119/return/CGT12345678/1234 came back with with status $status"))
         }
       }
    }

    "handling requests to calculate tax due" must {
      val response = sample(calculatedTaxDueGen)
      val calculateCgtTaxDueRequest = sample(calculateCgtTaxDueRequestGen)
      "call the right endpoint with display return body" in {

        stubFor(post(urlPathMatching(".*")).willReturn(jsonResponse(200, Json.toJson(response).toString())))

        await(con.calculateTaxDue(calculateCgtTaxDueRequest).value)

        val url = "/calculate-tax-due"

        verify(postRequestedFor(urlEqualTo(url)))

      }

      "return Unit on success" in {

        stubFor(post(urlPathMatching(".*")).willReturn(jsonResponse(200, Json.toJson(response).toString())))

        val result =  await(con.calculateTaxDue(calculateCgtTaxDueRequest).value)

        result shouldBe Right((response))
      }

      "return Left on error" in {
        val table = Table("status", 403, 404, 500)
        for (status <- table) {
          stubFor(post(urlPathMatching(".*")).willReturn(jsonResponse(status, "")))
          val result = await(con.calculateTaxDue(calculateCgtTaxDueRequest).value)
          result shouldBe
            Left(Error(s"POST to http://localhost:11119/calculate-tax-due came back with with status $status"))
        }
      }
    }


    "handling requests to calculate taxable gain or loss" must {
      val response = sample(taxableGainOrLossCalculationGen)
      val taxableGainOrLossCalculationRequest = sample(taxableGainOrLossCalculationRequestGen)
      "call the right endpoint with display return body" in {

        stubFor(post(urlPathMatching(".*")).willReturn(jsonResponse(200, Json.toJson(response).toString())))

        await(con.calculateTaxableGainOrLoss(taxableGainOrLossCalculationRequest).value)

        val url = "/calculate-taxable-gain-or-loss"

        verify(postRequestedFor(urlEqualTo(url)))

      }

      "return Unit on success" in {

        stubFor(post(urlPathMatching(".*")).willReturn(jsonResponse(200, Json.toJson(response).toString())))

        val result =  await(con.calculateTaxableGainOrLoss(taxableGainOrLossCalculationRequest).value)

        result shouldBe Right((response))
      }

      "return Left on error" in {
        val table = Table("status", 403, 404, 500)
        for (status <- table) {
          stubFor(post(urlPathMatching(".*")).willReturn(jsonResponse(status, "")))
          val result = await(con.calculateTaxableGainOrLoss(taxableGainOrLossCalculationRequest).value)
          result shouldBe
            Left(Error(s"POST to http://localhost:11119/calculate-taxable-gain-or-loss came back with with status $status"))
        }
      }
    }

    "handling requests to calculate tear to date liability" must {
      val response = sample(yearToDateLiabilityCalculationGen)
      val yearToDateLiabilityCalculationRequest = sample(yearToDateLiabilityCalculationRequestGen)
      "call the right endpoint with display return body" in {

        stubFor(post(urlPathMatching(".*")).willReturn(jsonResponse(200, Json.toJson(response).toString())))

        await(con.calculateYearToDateLiability(yearToDateLiabilityCalculationRequest).value)

        val url = "/calculate-year-to-date-liability"

        verify(postRequestedFor(urlEqualTo(url)))

      }

      "return Unit on success" in {

        stubFor(post(urlPathMatching(".*")).willReturn(jsonResponse(200, Json.toJson(response).toString())))

        val result =  await(con.calculateYearToDateLiability(yearToDateLiabilityCalculationRequest).value)

        result shouldBe Right((response))
      }

      "return Left on error" in {
        val table = Table("status", 403, 404, 500)
        for (status <- table) {
          stubFor(post(urlPathMatching(".*")).willReturn(jsonResponse(status, "")))
          val result = await(con.calculateYearToDateLiability(yearToDateLiabilityCalculationRequest).value)
          result shouldBe
            Left(Error(s"POST to http://localhost:11119/calculate-year-to-date-liability came back with with status $status"))
        }
      }
    }

    "handling requests to get taxYear" must {

      var taxYear = sample(TaxYear)

      var opTaxYear : Option[TaxYear]= Option.empty

     // opTaxYear = Some(taxYear).asInstanceOf[Option[TaxYear]]

      val response = TaxYearResponse(opTaxYear)

      val date = LocalDate.now()
      "call the right endpoint with display return body" in {

        stubFor(get(urlPathMatching(".*")).willReturn(jsonResponse(200, Json.toJson(response).toString())))

        await(con.taxYear(date).value)

        val url = s"/tax-year/2024-08-22"

        verify(getRequestedFor(urlEqualTo(url)))

      }
/*
      "return Unit on success" in {

        stubFor(post(urlPathMatching(".*")).willReturn(jsonResponse(200, Json.toJson(response).toString())))

        val result =  await(con.taxYear(date).value)

        result shouldBe Right((response))
      }

      "return Left on error" in {
        val table = Table("status", 403, 404, 500)
        for (status <- table) {
          stubFor(post(urlPathMatching(".*")).willReturn(jsonResponse(status, "")))
          val result = await(con.taxYear(date).value)
          result shouldBe
            Left(Error(s"POST to http://localhost:11119/tax-year/2024-08-21 came back with with status $status"))
        }
      }*/
    }



  }
}
