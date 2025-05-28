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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.services

import cats.data.EitherT
import cats.instances.future._
import org.scalamock.scalatest.MockFactory
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.play.guice.GuiceOneServerPerSuite
import play.api.test.Helpers._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.AddressLookupConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.metrics.Metrics
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Error
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address.UkAddress
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.{AddressLookupResult, Postcode}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.UKAddressLookupServiceImpl.{AddressLookupResponse, RawAddress}
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.chaining.scalaUtilChainingOps

class UKAddressLookupServiceImplSpec extends AnyWordSpec with Matchers with MockFactory with GuiceOneServerPerSuite {

  private val mockConnector = mock[AddressLookupConnector]

  private def mockLookupAddress(
    expectedPostcode: Postcode,
    filter: Option[String] = None
  )(
    result: Either[Error, AddressLookupResponse]
  ) =
    (mockConnector
      .lookupAddress(_: Postcode, _: Option[String])(_: HeaderCarrier))
      .expects(expectedPostcode, filter, *)
      .returning(EitherT.fromEither[Future](result))

  val service = new UKAddressLookupServiceImpl(mockConnector, app.injector.instanceOf[Metrics])

  private def response(addresses: List[RawAddress]) = Right(AddressLookupResponse(addresses))

  "AddressLookupServiceImpl" when {

    "handling address lookups" must {

      implicit val hc: HeaderCarrier = HeaderCarrier()
      val postcode                   = Postcode("postcode")

      "return an error" when {

        "the http response fails for any reason" in {
          mockLookupAddress(postcode)(Left(Error("Connector failed with status 500")))

          await(service.lookupAddress(postcode, None).value) shouldBe Left(Error("Connector failed with status 500"))
        }

        "there are no lines of address found in the response" in {
          RawAddress(
            lines = List.empty,
            town = "",
            county = None,
            postcode = "S051 7UR"
          ) pipe (List(_)) pipe response pipe mockLookupAddress(postcode)

          await(service.lookupAddress(postcode, None).value).isLeft shouldBe true
        }
      }

      "filter out invalid addresses" when {

        "address line1 is more than 35 characters" in {
          RawAddress(
            lines = List("address line1 is more than 35 characters"),
            town = "town",
            county = Some("county"),
            postcode = "ZZ1Z 4AB"
          ) pipe (List(_)) pipe response pipe mockLookupAddress(postcode)

          await(service.lookupAddress(postcode, None).value) shouldBe
            Right(AddressLookupResult(postcode, None, List.empty))
        }

        "address line2 is more than 35 characters" in {
          RawAddress(
            lines = List("line1", "address line2 is more than 35 characters"),
            town = "town",
            county = Some("county"),
            postcode = "ZZ1Z 4AB"
          ) pipe (List(_)) pipe response pipe mockLookupAddress(postcode)

          await(service.lookupAddress(postcode, None).value) shouldBe
            Right(AddressLookupResult(postcode, None, List.empty))
        }

        "town is more than 35 characters" in {
          RawAddress(
            lines = List("line1", "line2"),
            town = "town field length is more than 35 characters",
            county = Some("county"),
            postcode = "ZZ1Z 4AB"
          ) pipe (List(_)) pipe response pipe mockLookupAddress(postcode)

          await(service.lookupAddress(postcode, None).value) shouldBe
            Right(AddressLookupResult(postcode, None, List.empty))
        }

        "county is more than 35 characters" in {
          RawAddress(
            lines = List("line1", "line2"),
            town = "town",
            county = Some("county field length is more than 35 characters"),
            postcode = "ZZ1Z 4AB"
          ) pipe (List(_)) pipe response pipe mockLookupAddress(postcode)

          await(
            service.lookupAddress(postcode, None).value
          ) shouldBe Right(AddressLookupResult(postcode, None, List.empty))
        }

        "postcode is not valid" in {
          RawAddress(
            lines = List("line1", "line2"),
            town = "town",
            county = Some("county field length is more than 35 characters"),
            postcode = "AB 123"
          ) pipe (List(_)) pipe response pipe mockLookupAddress(postcode)

          await(service.lookupAddress(postcode, None).value) shouldBe
            Right(AddressLookupResult(postcode, None, List.empty))
        }

      }

      "return a successful response when a 200 status is returned and the JSON can be parsed" in {
        List(
          RawAddress(
            lines = List("line1", "line2"),
            town = "town",
            county = Some("county"),
            postcode = "ZZ1Z 4AB"
          ),
          RawAddress(
            lines = List("line1"),
            town = "town",
            county = None,
            postcode = "ZZ1Z 4AB"
          ),
          RawAddress(
            lines = List("line1"),
            town = "town",
            county = Some("county"),
            postcode = "ZZ1Z 4AB"
          ),
          RawAddress(
            lines = List("line1", "line2"),
            town = "town",
            county = None,
            postcode = "ZZ1Z 4AB"
          ),
          RawAddress(
            lines = List("line1", "line2", "line3"),
            town = "town",
            county = None,
            postcode = "ZZ1Z 4AB"
          )
        ) pipe response pipe mockLookupAddress(postcode)

        await(service.lookupAddress(postcode, None).value) shouldBe Right(
          AddressLookupResult(
            postcode,
            None,
            List(
              UkAddress(
                "line1",
                None,
                Some("town"),
                None,
                Postcode("ZZ1Z 4AB")
              ),
              UkAddress(
                "line1",
                None,
                Some("town"),
                Some("county"),
                Postcode("ZZ1Z 4AB")
              ),
              UkAddress(
                "line1",
                Some("line2"),
                Some("town"),
                Some("county"),
                Postcode("ZZ1Z 4AB")
              ),
              UkAddress(
                "line1",
                Some("line2"),
                Some("town"),
                None,
                Postcode("ZZ1Z 4AB")
              ),
              UkAddress(
                "line1",
                Some("line2, line3"),
                Some("town"),
                None,
                Postcode("ZZ1Z 4AB")
              )
            )
          )
        )
      }
    }
  }
}
