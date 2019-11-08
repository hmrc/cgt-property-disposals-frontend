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
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Error
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address.UkAddress
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.{AddressLookupResult, Postcode}
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class UKAddressLookupServiceImplSpec extends WordSpec with Matchers with MockFactory {

  val mockConnector = mock[AddressLookupConnector]

  def mockLookupAddress(expectedPostcode: Postcode, filter: Option[String] = None)(
    result: Either[Error, HttpResponse]
  ) =
    (mockConnector
      .lookupAddress(_: Postcode, _: Option[String])(_: HeaderCarrier))
      .expects(expectedPostcode, filter, *)
      .returning(EitherT.fromEither[Future](result))

  val service = new UKAddressLookupServiceImpl(mockConnector)

  "AddressLookupServiceImpl" when {

    "handling address lookups" must {

      implicit val hc: HeaderCarrier = HeaderCarrier()
      val postcode                   = Postcode("postcode")

      "return an error" when {

        "the http response does not come back with status 200" in {
          mockLookupAddress(postcode)(Right(HttpResponse(500)))

          await(service.lookupAddress(postcode, None).value).isLeft shouldBe true
        }

        "when the call to the connector fails" in {
          mockLookupAddress(postcode)(Left(Error("uh oh!")))

          await(service.lookupAddress(postcode, None).value).isLeft shouldBe true
        }

        "there is no JSON in the body of the response" in {
          mockLookupAddress(postcode)(Right(HttpResponse(200)))

          await(service.lookupAddress(postcode, None).value).isLeft shouldBe true
        }

        "the JSON in the body of the response cannot be parsed" in {
          mockLookupAddress(postcode)(Right(HttpResponse(200, Some(JsNumber(1)))))

          await(service.lookupAddress(postcode, None).value).isLeft shouldBe true
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

          await(service.lookupAddress(postcode, None).value).isLeft shouldBe true
        }

      }

      "return a successful response when a 200 status is returned and the JSON can be parsed" in {
        val json = Json.parse(
          """
            |[
            |  {
            |    "address": {
            |      "lines": [ "line1", "line2" ],
            |      "town": "town",
            |      "county" :"county",
            |      "postcode": "ABC 123"
            |    }
            |  },
            |  {
            |    "address": {
            |      "lines": [ "line1" ],
            |      "town": "town",
            |      "postcode": "ABC 123"
            |    }
            |  },
            |  {
            |    "address": {
            |      "lines": [ "line1" ],
            |      "town": "town",
            |      "county" : "county",
            |      "postcode": "ABC 123"
            |    }
            |  },
            |  {
            |    "address": {
            |      "lines": [ "line1", "line2" ],
            |      "town": "town",
            |      "postcode": "ABC 123"
            |    }
            |  },
            |  {
            |    "address": {
            |      "lines": [ "line1", "line2", "line3" ],
            |      "town": "town",
            |      "postcode": "ABC 123"
            |    }
            |  }
            |]
            |""".stripMargin
        )

        mockLookupAddress(postcode)(Right(HttpResponse(200, Some(json))))

        await(service.lookupAddress(postcode, None).value) shouldBe Right(
          AddressLookupResult(
            postcode,
            None,
            List(
              UkAddress("line1", Some("line2"), Some("town"), Some("county"), Postcode("ABC 123")),
              UkAddress("line1", None, Some("town"), None, Postcode("ABC 123")),
              UkAddress("line1", None, Some("town"), Some("county"), Postcode("ABC 123")),
              UkAddress("line1", Some("line2"), Some("town"), None, Postcode("ABC 123")),
              UkAddress("line1", Some("line2, line3"), Some("town"), None, Postcode("ABC 123"))
            )
          )
        )
      }

    }

  }

}
