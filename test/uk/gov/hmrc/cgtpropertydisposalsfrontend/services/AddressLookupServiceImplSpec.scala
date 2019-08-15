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
import org.scalamock.scalatest.MockFactory
import org.scalatest.{Matchers, WordSpec}
import play.api.libs.json.{JsNumber, Json}
import play.api.test.Helpers._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.AddressLookupConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Address.{NonUkAddress, UkAddress}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Address, AddressLookupResult, Error, Postcode}
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

class AddressLookupServiceImplSpec extends WordSpec with Matchers with MockFactory {

  val mockConnector = mock[AddressLookupConnector]

  def mockLookupAddress(expectedPostcode: Postcode)(result: Either[Error, HttpResponse]) =
    (mockConnector.lookupAddress(_: Postcode)(_: HeaderCarrier))
      .expects(expectedPostcode, *)
      .returning(EitherT.fromEither[Future](result))

  val service = new AddressLookupServiceImpl(mockConnector)

  "AddressLookupServiceImpl" when {

    "handling address lookups" must {

      implicit val hc: HeaderCarrier = HeaderCarrier()
      val postcode = Postcode("postcode")

      "return an error" when {

        "the http response does not come back with status 200" in {
          mockLookupAddress(postcode)(Right(HttpResponse(500)))

          await(service.lookupAddress(postcode).value).isLeft shouldBe true
        }

        "when the call to the connector fails" in {
          mockLookupAddress(postcode)(Left(Error("uh oh!")))

          await(service.lookupAddress(postcode).value).isLeft shouldBe true
        }

        "there is no JSON in the body of the response" in {
          mockLookupAddress(postcode)(Right(HttpResponse(200)))

          await(service.lookupAddress(postcode).value).isLeft shouldBe true
        }

        "the JSON in the body of the response cannot be parsed" in {
          mockLookupAddress(postcode)(Right(HttpResponse(200, Some(JsNumber(1)))))

          await(service.lookupAddress(postcode).value).isLeft shouldBe true
        }

        "there are no lines of address found in the response" in {
          val json = Json.parse(
            """
              |[
              |  {
              |    "address": {
              |      "lines": [ ],
              |      "town": "",
              |      "postcode": "SO51 7UR",
              |      "country": {
              |        "code": "GB",
              |        "name": "United Kingdom"
              |      }
              |    }
              |  }
              |]
              |""".stripMargin
          )

          mockLookupAddress(postcode)(Right(HttpResponse(200, Some(json))))

          await(service.lookupAddress(postcode).value).isLeft shouldBe true
        }

      }

      "return a successful response when a 200 status is returned and the JSON can be parsed" in {
        val json = Json.parse(
          """
            |[
            |  {
            |    "address": {
            |      "lines": [ ],
            |      "town": "town",
            |      "postcode": "ABC 123",
            |      "country": {
            |        "code": "GB"
            |      }
            |    }
            |  },
            |  {
            |    "address": {
            |      "lines": [ ],
            |      "town": "town",
            |      "county" : "county",
            |      "postcode": "ABC 123",
            |      "country": {
            |        "code": "GB"
            |      }
            |    }
            |  },
            |  {
            |    "address": {
            |      "lines": [ "line1", "line2" ],
            |      "town": "town",
            |      "postcode": "ABC 123",
            |      "country": {
            |        "code": "FR"
            |      }
            |    }
            |  },
            |  {
            |    "address": {
            |      "lines": [ "line1", "line2", "line3" ],
            |      "town": "town",
            |      "postcode": "ABC 123",
            |      "country": {
            |        "code": "GB"
            |      }
            |    }
            |  }
            |]
            |""".stripMargin
        )

        mockLookupAddress(postcode)(Right(HttpResponse(200, Some(json))))

        await(service.lookupAddress(postcode).value) shouldBe Right(
          AddressLookupResult(
            postcode,
            List[Address](
              UkAddress("town", None, None, None, "ABC 123"),
              UkAddress("town", Some("county"), None, None, "ABC 123"),
              NonUkAddress("line1", Some("line2"), Some("town"), None, Some("ABC 123"), "FR"),
              UkAddress("line1", Some("line2"), Some("line3"), Some("town"), "ABC 123")
            )
          )
        )
      }

      "convert country codes of 'UK' to 'GB'" in {
        val json = Json.parse(
          """
            |[
            |  {
            |    "address": {
            |      "lines": [ "line1" ],
            |      "town": "town",
            |      "postcode": "ABC 123",
            |      "country": {
            |        "code": "UK",
            |        "name": "United Kingdom"
            |      }
            |    }
            |  }
            |]
            |""".stripMargin
        )

        mockLookupAddress(postcode)(Right(HttpResponse(200, Some(json))))

        await(service.lookupAddress(postcode).value) shouldBe Right(
          AddressLookupResult(
            postcode,
            List(
              UkAddress("line1", Some("town"), None, None, "ABC 123")
            )
          )
        )
      }

      "concatenate lines of address when there are more than four in the response" in {
        val json = Json.parse(
          """
            |[
            |  {
            |    "address": {
            |      "lines": [ "line1", "line2", "line3", "line4" ],
            |      "town": "town",
            |      "postcode": "ABC 123",
            |      "country": {
            |        "code": "UK",
            |        "name": "United Kingdom"
            |      }
            |    }
            |  }
            |]
            |""".stripMargin
        )

        mockLookupAddress(postcode)(Right(HttpResponse(200, Some(json))))

        await(service.lookupAddress(postcode).value) shouldBe Right(
          AddressLookupResult(
            postcode,
            List(
              UkAddress("line1", Some("line2"), Some("line3"), Some("line4, town"), "ABC 123")
            )
          )
        )
      }

    }

  }

}
