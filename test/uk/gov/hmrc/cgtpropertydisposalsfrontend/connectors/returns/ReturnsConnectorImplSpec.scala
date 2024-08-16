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
import play.api.libs.json.Json
import play.api.test.Helpers.{await, defaultAwaitTimeout}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.HttpSupport
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Error
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.DraftReturnGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.CgtReference
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsServiceImpl.GetDraftReturnResponse
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.ConnectorSupport
import uk.gov.hmrc.http.HeaderCarrier

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
  }
}
