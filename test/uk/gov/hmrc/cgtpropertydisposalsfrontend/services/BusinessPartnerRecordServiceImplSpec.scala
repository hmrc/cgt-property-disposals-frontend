/*
 * Copyright 2019 HM Revenue & Customs
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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.services

import java.time.LocalDate

import org.scalamock.scalatest.MockFactory
import org.scalatest.{Matchers, WordSpec}
import play.api.libs.json.{JsNumber, Json}
import play.api.test.Helpers._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.BusinessPartnerRecordConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Address, BusinessPartnerRecord, DateOfBirth, NINO}
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class BusinessPartnerRecordServiceImplSpec extends WordSpec with Matchers with MockFactory {

  val mockConnector = mock[BusinessPartnerRecordConnector]

  val service = new BusinessPartnerRecordServiceImpl(mockConnector)

  def mockGetBPR(nino: NINO)(response: Future[HttpResponse]) =
    (mockConnector.getBusinessPartnerRecord(_: NINO)(_: HeaderCarrier))
      .expects(nino, *)
      .returning(response)

  implicit val hc: HeaderCarrier = HeaderCarrier()
  val nino = NINO("AB123456C")
  val dob = DateOfBirth(LocalDate.ofEpochDay(0L))
  val address = Address.UkAddress("line1", Some("line2"), None, None, "postcode")
  val bpr = BusinessPartnerRecord("name", "surname", dob, Some("email"), address)

  "The BusinessPartnerRecordServiceImpl" when {

    "getting a BusinessPartnerRecord" must {

      "return an error" when {

          def testError(response: => Future[HttpResponse]) = {
            mockGetBPR(nino)(response)

            await(service.getBusinessPartnerRecord(nino, dob)).isLeft shouldBe true
          }

        "the connector fails to make the call" in {
          testError(Future.failed(new Exception("Uh oh")))
        }

        "the HttpResponse comes back with a status other than 200" in {
          List(400, 401, 403, 404, 500, 501, 502).foreach { status =>
            testError(Future.successful(HttpResponse(status)))
          }
        }

        "the json body in the http response cannot be parsed" in {
          testError(Future.successful(HttpResponse(200, Some(JsNumber(0)))))
        }

        "there is no json body in the http response" in {
          testError(Future.successful(HttpResponse(200)))
        }

        "the date of birth in the BPR doesn't match the date of birth given" in {
          val otherDob = DateOfBirth(dob.value.plusDays(1L))
          testError(Future.successful(HttpResponse(200, Some(Json.toJson(bpr.copy(dateOfBirth = otherDob))))))
        }

      }
      "return the bpr when the http response comes back with status 200 and " +
        "the json body returns a bpr with a matching dob" in {
          mockGetBPR(nino)(Future.successful(HttpResponse(200, Some(Json.toJson(bpr)))))

          await(service.getBusinessPartnerRecord(nino, dob)) shouldBe Right(bpr)
        }

    }

  }

}
