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
import play.api.i18n.MessagesApi
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
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

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  val nino = NINO("AB123456C")
  val bpr = sample(bprGen)

  def mockUuidGenerator(uuid: UUID): CallHandler0[UUID] =
    (mockUuidGenerator.nextId: () => UUID).expects().returning(uuid)

  def mockEmailVerification(expectedEmail: Email, expectedId: UUID, expectedName: String)(result: Future[Either[Error, EmailVerificationResponse]]) =
    (mockService.verifyEmail(_: Email, _: UUID, _: String)(_: HeaderCarrier))
      .expects(expectedEmail, expectedId, expectedName, *)
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
          checkIsRedirect(result, routes.SubscriptionController.checkYourDetails())
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
          contentAsString(result) should include(message("email.title"))
        }

        "there is a BPR in session and there is an email to be verified in session" in {
          val email = Email("email")
          val session = SessionData.empty.copy(businessPartnerRecord = Some(bpr), emailToBeVerified = Some(EmailToBeVerified(email, UUID.randomUUID(), false)))

          inSequence {
            mockAuthWithCl200AndRetrievedNino(nino.value)
            mockGetSession(Future.successful(Right(Some(session))))
          }

          val result = controller.enterEmail()(FakeRequest())
          contentAsString(result) should include(message("email.title"))
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

          val result: Future[Result] = controller.enterEmailSubmit()(requestWithFormData("email" -> email.value))
          checkIsRedirect(result, routes.SubscriptionController.checkYourDetails())

        }

      }

      "show a form error" when {

          def testError(email: String): Unit =
            withClue(s"For email '$email': ") {
              inSequence {
                mockAuthWithCl200AndRetrievedNino(nino.value)
                mockGetSession(Future.successful(Right(Some(session))))
              }

              val result = controller.enterEmailSubmit()(requestWithFormData("email" -> email))
              val content = contentAsString(result)

              status(result) shouldBe BAD_REQUEST

              content should include(message("email.title"))
              content should include(s"""value="$email"""")
              content should include(message("email.invalid"))
            }

        "the email has no '@' character" in {
          testError("invalidemail")
        }

        "the email has no characters before the '@' character" in {
          testError("@domain")
        }

        "the email has no characters after the '@' character" in {
          testError("local@")
        }

        "the email has characters before and after the '@' character but " +
          "there are more than 132 characters in it" in {
            val longString = List.fill(100)("a").mkString("")
            testError(s"$longString@$longString")
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
            mockEmailVerification(email, id, bpr.forename)(Future.successful(Left(Error(""))))
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
          mockEmailVerification(email, id, bpr.forename)(Future.successful(Right(EmailAlreadyVerified)))
        }

        val result: Future[Result] = controller.enterEmailSubmit()(requestWithFormData("email" -> email.value))
        checkIsRedirect(result, routes.EmailController.verifyEmail(id))
      }

      "redirect to the check you inbox page when the email address verification request " +
        "has successfully been sent" in {
          inSequence {
            mockAuthWithCl200AndRetrievedNino(nino.value)
            mockGetSession(Future.successful(Right(Some(session))))
            mockUuidGenerator(id)
            mockStoreSession(session.copy(emailToBeVerified = Some(emailToBeVerified)))(Future.successful(Right(())))
            mockEmailVerification(email, id, bpr.forename)(Future.successful(Right(EmailVerificationRequested)))
          }

          val result: Future[Result] = controller.enterEmailSubmit()(requestWithFormData("email" -> email.value))
          checkIsRedirect(result, routes.EmailController.checkYourInbox())
        }

      "reuse the same id in the continue url if there is an existing email to be verified in session " +
        "and the emails match" in {
          inSequence {
            mockAuthWithCl200AndRetrievedNino(nino.value)
            mockGetSession(Future.successful(Right(Some(session.copy(emailToBeVerified = Some(emailToBeVerified))))))
            mockStoreSession(session.copy(emailToBeVerified = Some(emailToBeVerified)))(Future.successful(Right(())))
            mockEmailVerification(email, id, bpr.forename)(Future.successful(Right(EmailVerificationRequested)))
          }

          val result: Future[Result] = controller.enterEmailSubmit()(requestWithFormData("email" -> email.value))
          checkIsRedirect(result, routes.EmailController.checkYourInbox())
        }

      "strip out spaces in emails" in {
        val emailWithSpaces = " a @ b  "
        val emailWithoutSpaces = "a@b"
        val emailToBeVerified = EmailToBeVerified(Email(emailWithoutSpaces), id, false)

        inSequence {
          mockAuthWithCl200AndRetrievedNino(nino.value)
          mockGetSession(Future.successful(Right(Some(session.copy(emailToBeVerified = Some(emailToBeVerified))))))
          mockStoreSession(session.copy(emailToBeVerified = Some(emailToBeVerified)))(Future.successful(Right(())))
          mockEmailVerification(Email(emailWithoutSpaces), id, bpr.forename)(Future.successful(Right(EmailVerificationRequested)))
        }

        val result: Future[Result] = controller.enterEmailSubmit()(requestWithFormData("email" -> emailWithSpaces))
        checkIsRedirect(result, routes.EmailController.checkYourInbox())
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
          checkIsRedirect(result, routes.SubscriptionController.checkYourDetails())
        }

        "there is no email to be verified in session" in {
          inSequence {
            mockAuthWithCl200AndRetrievedNino(nino.value)
            mockGetSession(Future.successful(Right(Some(session.copy(emailToBeVerified = None)))))
          }

          val result = controller.checkYourInbox()(FakeRequest())
          checkIsRedirect(result, routes.SubscriptionController.checkYourDetails())

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
          contentAsString(result) should include(message("confirmEmail.title"))

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
          checkIsRedirect(result, routes.SubscriptionController.checkYourDetails())
        }

        "there is no email to be verified in session" in {
          inSequence {
            mockAuthWithCl200AndRetrievedNino(nino.value)
            mockGetSession(Future.successful(Right(Some(session.copy(emailToBeVerified = None)))))
          }

          val result = controller.verifyEmail(id)(FakeRequest())
          checkIsRedirect(result, routes.SubscriptionController.checkYourDetails())
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
            mockStoreSession(session.copy(
              emailToBeVerified     = Some(emailToBeVerified.copy(verified = true)),
              businessPartnerRecord = Some(bpr.copy(emailAddress = Some(emailToBeVerified.email.value)))
            ))(Future.successful(Left(Error(""))))
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
          checkIsRedirect(result, routes.EmailController.emailVerified())
        }

        "the session is updated" in {
          inSequence {
            mockAuthWithCl200AndRetrievedNino(nino.value)
            mockGetSession(Future.successful(Right(Some(session))))
            mockStoreSession(session.copy(
              emailToBeVerified     = Some(emailToBeVerified.copy(verified = true)),
              businessPartnerRecord = Some(bpr.copy(emailAddress = Some(emailToBeVerified.email.value)))
            ))(Future.successful(Right(())))
          }

          val result = controller.verifyEmail(id)(FakeRequest())
          checkIsRedirect(result, routes.EmailController.emailVerified())
        }

      }

    }

    "handling verified email addresses" must {

      val emailToBeVerified = EmailToBeVerified(Email("verified@email.com"), UUID.randomUUID(), true)
      val session = SessionData.empty.copy(
        businessPartnerRecord = Some(bpr),
        emailToBeVerified     = Some(emailToBeVerified)
      )

      "show an error page" when {

        "the email has not been verified" in {
          inSequence {
            mockAuthWithCl200AndRetrievedNino(nino.value)
            mockGetSession(Future.successful(Right(Some(session.copy(emailToBeVerified = Some(emailToBeVerified.copy(verified = false)))))))
          }

          checkIsTechnicalErrorPage(controller.emailVerified()(FakeRequest()))
        }

      }

      "redirect to check your details " when {

          def test(session: Option[SessionData]): Unit = {
            inSequence {
              mockAuthWithCl200AndRetrievedNino(nino.value)
              mockGetSession(Future.successful(Right(session)))
            }

            val result = controller.emailVerified()(FakeRequest())
            checkIsRedirect(result, routes.SubscriptionController.checkYourDetails())
          }

        "there is no BPR in session" in {
          test(Some(session.copy(businessPartnerRecord = None)))
        }

        "there is no email to be verified in session" in {
          test(Some(session.copy(emailToBeVerified = None)))
        }

        "there is no session data" in {
          test(None)
        }

      }

      "show the email verified page" when {

        "the email has been verified" in {
          inSequence {
            mockAuthWithCl200AndRetrievedNino(nino.value)
            mockGetSession(Future.successful(Right(Some(session))))
          }

          val result = controller.emailVerified()(FakeRequest())
          status(result) shouldBe OK
          contentAsString(result) should include(message("confirmEmail.verified.title"))
        }

      }

    }

  }

}
