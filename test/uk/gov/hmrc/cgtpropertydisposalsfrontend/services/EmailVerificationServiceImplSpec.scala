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

import java.util.UUID

import cats.data.EitherT
import cats.instances.future._
import org.scalamock.scalatest.MockFactory
import org.scalatest.{Matchers, WordSpec}
import play.api.test.Helpers._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.EmailVerificationConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Email, Error, Name}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.EmailVerificationService.EmailVerificationResponse.{EmailAlreadyVerified, EmailVerificationRequested}
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

class EmailVerificationServiceImplSpec extends WordSpec with Matchers with MockFactory {

  val mockConnector = mock[EmailVerificationConnector]

  def mockVerifyEmail(expectedEmail: Email, expectedId: UUID, expectedName: Name)(
    result: Either[Error, HttpResponse]
  ) =
    (mockConnector
      .verifyEmail(_: Email, _: UUID, _: Name)(_: HeaderCarrier))
      .expects(expectedEmail, expectedId, expectedName, *)
      .returning(EitherT.fromEither[Future](result))

  val service = new EmailVerificationServiceImpl(mockConnector)
  "EmailVerificationServiceImpl" when {

    "verifying emails" must {

      implicit val hc: HeaderCarrier = HeaderCarrier()
      val email                      = Email("email")
      val id                         = UUID.randomUUID()
      val name                       = Name("Fred", "Bread")

      "indicate when the email verification request has been requested" in {
        mockVerifyEmail(email, id, name)(Right(HttpResponse(CREATED)))

        await(service.verifyEmail(email, id, name).value) shouldBe Right(EmailVerificationRequested)
      }

      "indicate when the email address has already been verified" in {
        mockVerifyEmail(email, id, name)(Right(HttpResponse(CONFLICT)))

        await(service.verifyEmail(email, id, name).value) shouldBe Right(EmailAlreadyVerified)
      }

      "indicate when there is an error verifying the email address" in {
        List[Either[Error, HttpResponse]](
          Left(Error(new Exception("uh oh"))),
          Right(HttpResponse(INTERNAL_SERVER_ERROR))
        ).foreach { response =>
          mockVerifyEmail(email, id, name)(response)

          await(service.verifyEmail(email, id, name).value).isLeft shouldBe true

        }

      }

    }

  }

}
