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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers
import java.util.UUID

import org.scalamock.handlers.CallHandler0
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.test.CSRFTokenHelper._
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Email, EmailToBeVerified, Error, NINO, SessionData, UUIDGenerator, bprGen, sample}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.EmailVerificationService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.EmailVerificationService.EmailVerificationResponse
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.EmailVerificationService.EmailVerificationResponse.{EmailAlreadyVerified, EmailVerificationRequested}
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.Future

class EmailControllerSpec extends ControllerSpec with AuthSupport with SessionSupport {

  val mockService = mock[EmailVerificationService]

  val mockUuidGenerator = mock[UUIDGenerator]

  override val overrideBindings =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore),
      bind[EmailVerificationService].toInstance(mockService),
      bind[UUIDGenerator].toInstance(mockUuidGenerator)
    )

  lazy val controller = instanceOf[EmailController]

  val nino = NINO("AB123456C")
  val bpr = sample(bprGen)

  def mockUuidGenerator(uuid: UUID): CallHandler0[UUID] =
    (mockUuidGenerator.nextId: () => UUID).expects().returning(uuid)

  def mockEmailVerification(expectedEmail: Email, expectedId: UUID)(result: Future[Either[Error, EmailVerificationResponse]]) =
    (mockService.verifyEmail(_: Email, _: UUID)(_: HeaderCarrier))
      .expects(expectedEmail, expectedId, *)
      .returning(result)

  "EmailController" when {

    "handling requests to display the enter email page" must {

      "redirect to the check your details page" when {

        "there is no BPR in session" in {
          inSequence {
            mockAuthWithCl200AndRetrievedNino(nino.value)
            mockGetSession(Future.successful(Right(None)))
          }

          val result = controller.enterEmail()(FakeRequest())
          status(result) shouldBe SEE_OTHER
          redirectLocation(result) shouldBe Some(routes.SubscriptionController.checkYourDetails().url)
        }

      }

      "display the enter email page" when {

        "there is a BPR in session and there is no email to be verified in session" in {
          val session = SessionData.empty.copy(businessPartnerRecord = Some(bpr))

          inSequence {
            mockAuthWithCl200AndRetrievedNino(nino.value)
            mockGetSession(Future.successful(Right(Some(session))))
          }

          val result = controller.enterEmail()(FakeRequest())
          contentAsString(result) should include("Give us your email")
        }

        "there is a BPR in session and there is an email to be verified in session" in {
          val email = Email("email")
          val session = SessionData.empty.copy(businessPartnerRecord = Some(bpr), emailToBeVerified = Some(EmailToBeVerified(email, UUID.randomUUID(), false)))

          inSequence {
            mockAuthWithCl200AndRetrievedNino(nino.value)
            mockGetSession(Future.successful(Right(Some(session))))
          }

          val result = controller.enterEmail()(FakeRequest())
          contentAsString(result) should include("Give us your email")
          contentAsString(result) should include(s"""value="${email.value}"""")
        }

      }

    }

    "handling submitted email addresses" must {

        def requestWithFormData(data: (String, String)*) =
          FakeRequest().withFormUrlEncodedBody(data: _*).withCSRFToken

      val session = SessionData.empty.copy(businessPartnerRecord = Some(bpr))
      val email = Email("test@email.com")
      val id = UUID.randomUUID()
      val emailToBeVerified = EmailToBeVerified(email, id, false)

      "redirect to the check your details page" when {

        "there is no BPR in session" in {
          inSequence {
            mockAuthWithCl200AndRetrievedNino(nino.value)
            mockGetSession(Future.successful(Right(None)))
          }

          val result = controller.enterEmailSubmit()(requestWithFormData("email" -> email.value))
          status(result) shouldBe SEE_OTHER
          redirectLocation(result) shouldBe Some(routes.SubscriptionController.checkYourDetails().url)

        }

      }

      "show a form error" when {

        "the email has no '@' character" in {
          val email = "invalidemail"
          inSequence {
            mockAuthWithCl200AndRetrievedNino(nino.value)
            mockGetSession(Future.successful(Right(Some(session))))
          }

          val result = controller.enterEmailSubmit()(requestWithFormData("email" -> email))
          status(result) shouldBe BAD_REQUEST
          contentAsString(result) should include("Give us your email")
          contentAsString(result) should include(s"""value="$email"""")
          // TODO: check error message is present
        }

      }

      "show an error page" when {

        "the email to be verified cannot be stored in session" in {
          inSequence {
            mockAuthWithCl200AndRetrievedNino(nino.value)
            mockGetSession(Future.successful(Right(Some(session))))
            mockUuidGenerator(id)
            mockStoreSession(session.copy(emailToBeVerified = Some(emailToBeVerified)))(Future.successful(Left(Error(""))))
          }

          val result = controller.enterEmailSubmit()(requestWithFormData("email" -> email.value))
          checkIsTechnicalErrorPage(result)
        }

        "the call to verify the email fails" in {
          inSequence {
            mockAuthWithCl200AndRetrievedNino(nino.value)
            mockGetSession(Future.successful(Right(Some(session))))
            mockUuidGenerator(id)
            mockStoreSession(session.copy(emailToBeVerified = Some(emailToBeVerified)))(Future.successful(Right(())))
            mockEmailVerification(email, id)(Future.successful(Left(Error(""))))
          }

          val result = controller.enterEmailSubmit()(requestWithFormData("email" -> email.value))
          checkIsTechnicalErrorPage(result)
        }

      }

      "redirect to confirm email when the email address has already been verified" in {
        inSequence {
          mockAuthWithCl200AndRetrievedNino(nino.value)
          mockGetSession(Future.successful(Right(Some(session))))
          mockUuidGenerator(id)
          mockStoreSession(session.copy(emailToBeVerified = Some(emailToBeVerified)))(Future.successful(Right(())))
          mockEmailVerification(email, id)(Future.successful(Right(EmailAlreadyVerified)))
        }

        val result = controller.enterEmailSubmit()(requestWithFormData("email" -> email.value))
        status(result) shouldBe SEE_OTHER
        redirectLocation(result) shouldBe Some(routes.EmailController.verifyEmail(id).url)
      }

      "redirect to the check you inbox page when the email address verification request " +
        "has successfully been sent" in {
          inSequence {
            mockAuthWithCl200AndRetrievedNino(nino.value)
            mockGetSession(Future.successful(Right(Some(session))))
            mockUuidGenerator(id)
            mockStoreSession(session.copy(emailToBeVerified = Some(emailToBeVerified)))(Future.successful(Right(())))
            mockEmailVerification(email, id)(Future.successful(Right(EmailVerificationRequested)))
          }

          val result = controller.enterEmailSubmit()(requestWithFormData("email" -> email.value))
          status(result) shouldBe SEE_OTHER
          redirectLocation(result) shouldBe Some(routes.EmailController.checkYourInbox().url)
        }

    }

    "handling requests to display the check your inbox page" must {

      val email = Email("test@email.com")
      val id = UUID.randomUUID()
      val emailToBeVerified = EmailToBeVerified(email, id, false)
      val session = SessionData.empty.copy(businessPartnerRecord = Some(bpr), emailToBeVerified = Some(emailToBeVerified))

      "redirect to the check your details page" when {

        "there is no BPR in session" in {
          inSequence {
            mockAuthWithCl200AndRetrievedNino(nino.value)
            mockGetSession(Future.successful(Right(Some(session.copy(businessPartnerRecord = None)))))
          }

          val result = controller.checkYourInbox()(FakeRequest())
          status(result) shouldBe SEE_OTHER
          redirectLocation(result) shouldBe Some(routes.SubscriptionController.checkYourDetails().url)
        }

        "there is no email to be verified in session" in {
          inSequence {
            mockAuthWithCl200AndRetrievedNino(nino.value)
            mockGetSession(Future.successful(Right(Some(session.copy(emailToBeVerified = None)))))
          }

          val result = controller.checkYourInbox()(FakeRequest())
          status(result) shouldBe SEE_OTHER
          redirectLocation(result) shouldBe Some(routes.SubscriptionController.checkYourDetails().url)

        }

      }

      "show the check your inbox page when there is a BPR in session and there is an " +
        "email to be verified in session" in {
          inSequence {
            mockAuthWithCl200AndRetrievedNino(nino.value)
            mockGetSession(Future.successful(Right(Some(session))))
          }

          val result = controller.checkYourInbox()(FakeRequest())
          status(result) shouldBe OK
          contentAsString(result) should include("Look in your inbox")

        }

    }

    "handling requests to verify an email address" must {

      val email = Email("test@email.com")
      val id = UUID.randomUUID()
      val emailToBeVerified = EmailToBeVerified(email, id, false)
      val session = SessionData.empty.copy(businessPartnerRecord = Some(bpr), emailToBeVerified = Some(emailToBeVerified))

      "redirect to the check your details page" when {

        "there is no BPR in session" in {
          inSequence {
            mockAuthWithCl200AndRetrievedNino(nino.value)
            mockGetSession(Future.successful(Right(Some(session.copy(businessPartnerRecord = None)))))
          }

          val result = controller.verifyEmail(id)(FakeRequest())
          status(result) shouldBe SEE_OTHER
          redirectLocation(result) shouldBe Some(routes.SubscriptionController.checkYourDetails().url)
        }

        "there is no email to be verified in session" in {
          inSequence {
            mockAuthWithCl200AndRetrievedNino(nino.value)
            mockGetSession(Future.successful(Right(Some(session.copy(emailToBeVerified = None)))))
          }

          val result = controller.verifyEmail(id)(FakeRequest())
          status(result) shouldBe SEE_OTHER
          redirectLocation(result) shouldBe Some(routes.SubscriptionController.checkYourDetails().url)
        }
      }

      "show an error page" when {

        "the id in the URL does not match the ID in session" in {
          inSequence {
            mockAuthWithCl200AndRetrievedNino(nino.value)
            mockGetSession(Future.successful(Right(Some(session))))
          }

          val result = controller.verifyEmail(UUID.randomUUID())(FakeRequest())
          checkIsTechnicalErrorPage(result)
        }

        "there is an error updating the session" in {
          inSequence {
            mockAuthWithCl200AndRetrievedNino(nino.value)
            mockGetSession(Future.successful(Right(Some(session))))
            mockStoreSession(session.copy(emailToBeVerified = Some(emailToBeVerified.copy(verified = true))))(Future.successful(Left(Error(""))))
          }

          val result = controller.verifyEmail(id)(FakeRequest())
          checkIsTechnicalErrorPage(result)
        }

      }

      "redirect to email verified" when {

        "the session indicates the email has already been verified" in {
          inSequence {
            mockAuthWithCl200AndRetrievedNino(nino.value)
            mockGetSession(Future.successful(Right(Some(session.copy(emailToBeVerified = Some(emailToBeVerified.copy(verified = true)))))))
          }

          val result = controller.verifyEmail(id)(FakeRequest())
          status(result) shouldBe SEE_OTHER
          redirectLocation(result) shouldBe Some(routes.EmailController.emailVerified().url)
        }

        "the session is updated" in {
          inSequence {
            mockAuthWithCl200AndRetrievedNino(nino.value)
            mockGetSession(Future.successful(Right(Some(session))))
            mockStoreSession(session.copy(emailToBeVerified = Some(emailToBeVerified.copy(verified = true))))(Future.successful(Right(())))
          }

          val result = controller.verifyEmail(id)(FakeRequest())
          status(result) shouldBe SEE_OTHER
          redirectLocation(result) shouldBe Some(routes.EmailController.emailVerified().url)
        }

      }

    }

    "handling verified email addresses" must {

      "" in {

      }
    }

  }

}
