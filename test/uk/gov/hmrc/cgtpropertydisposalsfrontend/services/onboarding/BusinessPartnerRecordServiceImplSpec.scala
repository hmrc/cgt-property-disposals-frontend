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

import cats.data.EitherT
import cats.instances.future._
import org.scalamock.scalatest.MockFactory
import org.scalatest.{Matchers, WordSpec}
import play.api.i18n.Lang
import play.api.libs.json.{JsNumber, Json}
import play.api.test.Helpers._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.CGTPropertyDisposalsConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.BusinessPartnerRecordGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.IdGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address.{NonUkAddress, UkAddress}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.{Address, Country, Postcode}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.CgtReference
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.{IndividualName, TrustName}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.bpr.{BusinessPartnerRecord, BusinessPartnerRecordRequest, BusinessPartnerRecordResponse}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.email.Email
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class BusinessPartnerRecordServiceImplSpec extends WordSpec with Matchers with MockFactory {

  val mockConnector = mock[CGTPropertyDisposalsConnector]

  val service = new BusinessPartnerRecordServiceImpl(mockConnector)

  def mockGetBPR(
    request: BusinessPartnerRecordRequest,
    lang: Lang
  )(response: Either[Error, HttpResponse]) =
    (mockConnector
      .getBusinessPartnerRecord(_: BusinessPartnerRecordRequest, _: Lang)(
        _: HeaderCarrier
      ))
      .expects(request, lang, *)
      .returning(EitherT.fromEither[Future](response))

  implicit val hc: HeaderCarrier = HeaderCarrier()
  val bprRequest                 = sample[BusinessPartnerRecordRequest]
  val bpr                        = sample[BusinessPartnerRecord].copy(emailAddress = Some(Email("abc@test.com")))
  private val emptyJsonBody      = "{}"
  private val noJsonInBody       = ""
  val lang                       = Lang.defaultLang

  "The BusinessPartnerRecordServiceImpl" when {

    "getting a BusinessPartnerRecord" must {

      "return an error" when {

        def testError(response: => Either[Error, HttpResponse]) = {
          mockGetBPR(bprRequest, lang)(response)

          await(
            service.getBusinessPartnerRecord(bprRequest, lang).value
          ).isLeft shouldBe true
        }

        "the connector fails to make the call" in {
          testError(Left(Error(new Exception("Uh oh"))))
        }

        "the HttpResponse comes back with a status other than 200" in {
          List(400, 401, 403, 404, 500, 501, 502).foreach(status =>
            testError(Right(HttpResponse(status, emptyJsonBody)))
          )
        }

        "the json body in the http response cannot be parsed" in {
          testError(Right(HttpResponse(200, JsNumber(0), Map[String, Seq[String]]().empty)))
        }

        "there is no json body in the http response" in {
          testError(Right(HttpResponse(200, noJsonInBody)))
        }

      }

      "return the bpr when the http response comes back with status 200 and " +
        "the json body returns a bpr" in {
          val response = BusinessPartnerRecordResponse(Some(bpr), None, None)

          mockGetBPR(bprRequest, lang)(
            Right(HttpResponse(200, Json.toJson(response), Map[String, Seq[String]]().empty))
          )

          await(
            service.getBusinessPartnerRecord(bprRequest, lang).value
          ) shouldBe Right(response)
        }

      "return nothing when the http response comes back with status 200 and " +
        "the json body does not contain a bpr" in {
          val response =
            BusinessPartnerRecordResponse(Some(bpr), Some(sample[CgtReference]), None)

          mockGetBPR(bprRequest, lang)(
            Right(HttpResponse(200, Json.toJson(response), Map[String, Seq[String]]().empty))
          )

          await(
            service.getBusinessPartnerRecord(bprRequest, lang).value
          ) shouldBe Right(response)
        }

      "filter out invalid characters in address lines if a uk address is found" in {
        val address = UkAddress(
          "line1 (abc)",
          Some("line2, abc/def"),
          Some("line3 + abc"),
          Some("line4 #1"),
          Postcode("abc")
        )

        val sanitisedAddress = UkAddress(
          "line1 abc",
          Some("line2, abcdef"),
          Some("line3  abc"),
          Some("line4 1"),
          Postcode("abc")
        )

        def response(a: Address) =
          BusinessPartnerRecordResponse(Some(bpr.copy(address = Some(a))), None, None)

        mockGetBPR(bprRequest, lang)(
          Right(HttpResponse(200, Json.toJson(response(address)), Map[String, Seq[String]]().empty))
        )

        await(
          service.getBusinessPartnerRecord(bprRequest, lang).value
        ) shouldBe Right(response(sanitisedAddress))
      }

      "filter out invalid characters in address lines if a non-uk address is found" in {
        val country = Country("HK")

        val address = NonUkAddress(
          "line1 (abc)",
          Some("line2, abc/def"),
          Some("line3 + abc"),
          Some("line4 #1"),
          Some("abc"),
          country
        )

        val sanitisedAddress = NonUkAddress(
          "line1 abc",
          Some("line2, abcdef"),
          Some("line3  abc"),
          Some("line4 1"),
          Some("abc"),
          country
        )

        def response(a: Address) =
          BusinessPartnerRecordResponse(Some(bpr.copy(address = Some(a))), None, None)

        mockGetBPR(bprRequest, lang)(
          Right(HttpResponse(200, Json.toJson(response(address)), Map[String, Seq[String]]().empty))
        )

        await(
          service.getBusinessPartnerRecord(bprRequest, lang).value
        ) shouldBe Right(response(sanitisedAddress))
      }

      "filter out invalid characters in trust names if a trust name is found" in {
        def response(t: TrustName) =
          BusinessPartnerRecordResponse(Some(bpr.copy(name = Left(t))), None, None)

        val trustName          = TrustName("Trust (name)")
        val sanitisedTrustName = TrustName("Trust name")

        mockGetBPR(bprRequest, lang)(
          Right(HttpResponse(200, Json.toJson(response(trustName)), Map[String, Seq[String]]().empty))
        )

        await(
          service.getBusinessPartnerRecord(bprRequest, lang).value
        ) shouldBe Right(response(sanitisedTrustName))
      }

      "filter out invalid characters in individual names if an individual name is found" in {
        def response(n: IndividualName) =
          BusinessPartnerRecordResponse(Some(bpr.copy(name = Right(n))), None, None)

        val name          = IndividualName("First (name)", "Last name!")
        val sanitisedName = IndividualName("First name", "Last name")

        mockGetBPR(bprRequest, lang)(
          Right(HttpResponse(200, Json.toJson(response(name)), Map[String, Seq[String]]().empty))
        )

        await(
          service.getBusinessPartnerRecord(bprRequest, lang).value
        ) shouldBe Right(response(sanitisedName))
      }

      "filter out invalid email addresses if an email is found" in {
        def response(e: Option[Email]) =
          BusinessPartnerRecordResponse(Some(bpr.copy(emailAddress = e)), None, None)

        mockGetBPR(bprRequest, lang)(
          Right(HttpResponse(200, Json.toJson(response(Some(Email("invalid")))), Map[String, Seq[String]]().empty))
        )

        await(
          service.getBusinessPartnerRecord(bprRequest, lang).value
        ) shouldBe Right(response(None))
      }

    }

  }

}
