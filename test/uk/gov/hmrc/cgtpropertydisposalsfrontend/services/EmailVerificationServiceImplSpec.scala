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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.services

import cats.data.EitherT
import cats.instances.future._
import org.scalamock.scalatest.MockFactory
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import play.api.mvc.Call
import play.api.test.Helpers._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.EmailVerificationConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.metrics.MockMetrics
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Error
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.http.AcceptLanguage
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.ContactName
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.email.Email
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.EmailVerificationService.EmailVerificationResponse.{EmailAlreadyVerified, EmailVerificationRequested}
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class EmailVerificationServiceImplSpec extends AnyWordSpec with Matchers with MockFactory {

  val mockConnector = mock[EmailVerificationConnector]

  def mockVerifyEmail(
    expectedEmail: Email,
    expectedName: ContactName,
    expectedContinueCall: Call,
    expectedLanguage: AcceptLanguage
  )(
    result: Either[Error, HttpResponse]
  ) =
    (mockConnector
      .verifyEmail(_: Email, _: ContactName, _: Call, _: AcceptLanguage)(_: HeaderCarrier))
      .expects(expectedEmail, expectedName, expectedContinueCall, expectedLanguage, *)
      .returning(EitherT.fromEither[Future](result))

  val service =
    new EmailVerificationServiceImpl(mockConnector, MockMetrics.metrics)

  private val emptyJsonBody = "{}"

  "EmailVerificationServiceImpl" when {

    "verifying emails" must {

      implicit val hc: HeaderCarrier = HeaderCarrier()
      val email                      = Email("email")
      val name                       = ContactName("Fred Bread")
      val continueCall               = Call("GET", "/")

      "indicate when the email verification request has been requested" in {
        mockVerifyEmail(email, name, continueCall, AcceptLanguage.EN)(Right(HttpResponse(CREATED, emptyJsonBody)))

        await(
          service.verifyEmail(email, name, continueCall, AcceptLanguage.EN).value
        ) shouldBe Right(EmailVerificationRequested)
      }

      "indicate when the email address has already been verified" in {
        mockVerifyEmail(email, name, continueCall, AcceptLanguage.EN)(
          Right(HttpResponse(CONFLICT, emptyJsonBody))
        )

        await(
          service.verifyEmail(email, name, continueCall, AcceptLanguage.EN).value
        ) shouldBe Right(EmailAlreadyVerified)
      }

      "indicate when there is an error verifying the email address" in {
        List[Either[Error, HttpResponse]](
          Left(Error(new Exception("uh oh"))),
          Right(HttpResponse(INTERNAL_SERVER_ERROR, emptyJsonBody))
        ).foreach { response =>
          mockVerifyEmail(email, name, continueCall, AcceptLanguage.EN)(response)

          await(
            service.verifyEmail(email, name, continueCall, AcceptLanguage.EN).value
          ).isLeft shouldBe true

        }

      }

    }

  }

}
