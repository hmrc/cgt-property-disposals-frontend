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

import cats.data.EitherT
import cats.instances.future._
import java.time._

import org.scalamock.scalatest.MockFactory
import org.scalatest.{Matchers, WordSpec}
import play.api.libs.json.{JsNumber, Json}
import play.api.test.Helpers._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.CGTPropertyDisposalsConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.UserType.{Individual, Trust}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models._
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class BusinessPartnerRecordServiceImplSpec extends WordSpec with Matchers with MockFactory {

  val mockConnector = mock[CGTPropertyDisposalsConnector]

  val service = new BusinessPartnerRecordServiceImpl(mockConnector)

  def mockGetBPR(entity: Either[Trust,Individual])(response: Either[Error, HttpResponse]) =
    (mockConnector
      .getBusinessPartnerRecord(_: Either[Trust,Individual])(_: HeaderCarrier))
      .expects(entity, *)
      .returning(EitherT.fromEither[Future](response))

  implicit val hc: HeaderCarrier = HeaderCarrier()
  val sautr                      = SAUTR("1234566789")
  val nino                       = NINO("AB123456C")
  val name                       = Name("forename", "surname")
  val dateOfBirth                = DateOfBirth(LocalDate.of(2000, 4, 10))
  val individual                 = Individual(nino, name, dateOfBirth, None)
  val trust                      = Trust(sautr)
  val address =
    Address.UkAddress("line1", Some("line2"), None, None, "postcode")
  val bpr = BusinessPartnerRecord(Some("email"), address, "sap", Some("org"))

  "The BusinessPartnerRecordServiceImpl" when {

    "getting a BusinessPartnerRecord" must {

      "return an error" when {

        def testError(response: => Either[Error, HttpResponse]) = {
          mockGetBPR(Right(individual))(response)

          await(service.getBusinessPartnerRecord(Right(individual)).value).isLeft shouldBe true
        }

        "the connector fails to make the call" in {
          testError(Left(Error(new Exception("Uh oh"))))
        }

        "the HttpResponse comes back with a status other than 200" in {
          List(400, 401, 403, 404, 500, 501, 502).foreach { status =>
            testError(Right(HttpResponse(status)))
          }
        }

        "the json body in the http response cannot be parsed" in {
          testError(Right(HttpResponse(200, Some(JsNumber(0)))))
        }

        "there is no json body in the http response" in {
          testError(Right(HttpResponse(200)))
        }

      }
      "return the bpr when the http response comes back with status 200 and " +
        "the json body returns a bpr with a matching dob" in {
        mockGetBPR(Left(trust))(Right(HttpResponse(200, Some(Json.toJson(bpr)))))

        await(service.getBusinessPartnerRecord(Left(trust)).value) shouldBe Right(bpr)
      }

    }

  }

}
