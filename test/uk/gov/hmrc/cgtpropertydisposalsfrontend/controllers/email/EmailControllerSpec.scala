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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.email

import java.util.UUID

import cats.data.EitherT
import cats.instances.future._
import org.scalacheck.ScalacheckShapeless._
import org.scalamock.handlers.CallHandler0
import play.api.i18n.MessagesApi
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.{Call, Result}
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.UUIDGenerator
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.ContactName
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Email, EmailToBeVerified, Error, JourneyStatus, SessionData, SubscribedDetails}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.EmailVerificationService.EmailVerificationResponse
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.EmailVerificationService.EmailVerificationResponse.{EmailAlreadyVerified, EmailVerificationRequested}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.{EmailVerificationService, SubscriptionService}
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

trait EmailControllerSpec[Journey <: JourneyStatus, VerificationCompleteJourney <: JourneyStatus]
    extends ControllerSpec
    with AuthSupport
    with SessionSupport {

  val validJourneyStatus: Journey

  val validVerificationCompleteJourneyStatus: VerificationCompleteJourney

  def updateEmail(journey: Journey, email: Email)(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, VerificationCompleteJourney]

  val controller: EmailController[Journey, VerificationCompleteJourney]

  val isAmendJourney: Boolean

  val mockService = mock[EmailVerificationService]

  val mockUuidGenerator = mock[UUIDGenerator]

  override val overrideBindings =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore),
      bind[EmailVerificationService].toInstance(mockService),
      bind[UUIDGenerator].toInstance(mockUuidGenerator)
    )

  def mockEmailVerification(
    expectedEmail: Email,
    expectedName: ContactName,
    expectedContinue: Call
  )(result: Either[Error, EmailVerificationResponse]) =
    (mockService
      .verifyEmail(_: Email, _: ContactName, _: Call)(_: HeaderCarrier))
      .expects(expectedEmail, expectedName, expectedContinue, *)
      .returning(EitherT.fromEither[Future](result))

  def mockUpdateSubscription(
    subscribedDetails: SubscribedDetails
  )(result: Either[Error, Unit]) =
    (mockSubscriptionService
      .updateSubscribedDetails(_: SubscribedDetails)(_: HeaderCarrier))
      .expects(subscribedDetails, *)
      .returning(EitherT.fromEither[Future](result))

  def mockUuidGenerator(uuid: UUID): CallHandler0[UUID] =
    (mockUuidGenerator.nextId: () => UUID).expects().returning(uuid)

  lazy val sessionDataWithValidJourneyStatus =
    SessionData.empty.copy(journeyStatus = Some(validJourneyStatus))

  def enterEmailPage(performAction: () => Future[Result])(
    implicit messagesApi: MessagesApi
  ): Unit = {
    val titleKey = if (isAmendJourney) "email.amend.title" else "email.title"

    "display the enter email page" when {

      "there is a valid journey in session and there is no email to be verified in session" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(Future.successful(Right(Some(sessionDataWithValidJourneyStatus))))
        }

        contentAsString(performAction()) should include(message(titleKey))
      }

      "there is a BPR in session and there is an email to be verified in session" in {
        val email = Email("email")
        val session = sessionDataWithValidJourneyStatus.copy(
          emailToBeVerified = Some(EmailToBeVerified(email, UUID.randomUUID(), false))
        )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(Future.successful(Right(Some(session))))
        }

        val result = performAction()
        contentAsString(result) should include(message(titleKey))
        contentAsString(result) should include(s"""value="${email.value}"""")
      }

    }
  }

  def enterEmailSubmit(
    performAction: (String, String) => Future[Result],
    expectedName: ContactName,
    verifyEmailCall: UUID => Call,
    checkYourInboxCall: Call
  )(implicit messagesApi: MessagesApi): Unit = {
    val email             = Email("test@email.com")
    val id                = UUID.randomUUID()
    val emailToBeVerified = EmailToBeVerified(email, id, false)
    val titleKey          = if (isAmendJourney) "email.amend.title" else "email.title"

    "show a form error" when {

      def testError(email: String): Unit =
        withClue(s"For email '$email': ") {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionDataWithValidJourneyStatus))))
          }

          val result  = performAction("email", email)
          val content = contentAsString(result)

          status(result) shouldBe BAD_REQUEST

          content should include(message(titleKey))
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
          mockAuthWithNoRetrievals()
          mockGetSession(Future.successful(Right(Some(sessionDataWithValidJourneyStatus))))
          mockUuidGenerator(id)
          mockStoreSession(sessionDataWithValidJourneyStatus.copy(emailToBeVerified = Some(emailToBeVerified)))(
            Future.successful(Left(Error("")))
          )
        }

        val result = performAction("email", email.value)
        checkIsTechnicalErrorPage(result)
      }

      "the call to verify the email fails" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(Future.successful(Right(Some(sessionDataWithValidJourneyStatus))))
          mockUuidGenerator(id)
          mockStoreSession(sessionDataWithValidJourneyStatus.copy(emailToBeVerified = Some(emailToBeVerified)))(
            Future.successful(Right(()))
          )
          mockEmailVerification(email, expectedName, verifyEmailCall(id))(Left(Error("")))
        }

        val result = performAction("email", email.value)
        checkIsTechnicalErrorPage(result)
      }

    }

    "redirect to confirm email when the email address has been verified" in {
      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(Future.successful(Right(Some(sessionDataWithValidJourneyStatus))))
        mockUuidGenerator(id)
        mockStoreSession(sessionDataWithValidJourneyStatus.copy(emailToBeVerified = Some(emailToBeVerified)))(
          Future.successful(Right(()))
        )
        mockEmailVerification(email, expectedName, verifyEmailCall(id))(Right(EmailAlreadyVerified))
      }

      val result: Future[Result] = performAction("email", email.value)
      checkIsRedirect(result, verifyEmailCall(id))
    }

    "redirect to the check you inbox page when the email address verification request " +
      "has successfully been sent" in {
      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(Future.successful(Right(Some(sessionDataWithValidJourneyStatus))))
        mockUuidGenerator(id)
        mockStoreSession(sessionDataWithValidJourneyStatus.copy(emailToBeVerified = Some(emailToBeVerified)))(
          Future.successful(Right(()))
        )
        mockEmailVerification(email, expectedName, verifyEmailCall(id))(Right(EmailVerificationRequested))
      }

      val result: Future[Result] = performAction("email", email.value)
      checkIsRedirect(result, checkYourInboxCall)
    }

    "reuse the same id in the continue url if there is an existing email to be verified in session " +
      "and the emails match" in {
      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(
          Future.successful(
            Right(Some(sessionDataWithValidJourneyStatus.copy(emailToBeVerified = Some(emailToBeVerified))))
          )
        )
        mockEmailVerification(email, expectedName, verifyEmailCall(id))(Right(EmailVerificationRequested))
      }

      val result: Future[Result] = performAction("email", email.value)
      checkIsRedirect(result, checkYourInboxCall)
    }

    "strip out spaces in emails" in {
      val emailWithSpaces    = " a @ b  "
      val emailWithoutSpaces = "a@b"
      val emailToBeVerified  = EmailToBeVerified(Email(emailWithoutSpaces), id, false)

      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(
          Future.successful(
            Right(Some(sessionDataWithValidJourneyStatus.copy(emailToBeVerified = Some(emailToBeVerified))))
          )
        )
        mockEmailVerification(Email(emailWithoutSpaces), expectedName, verifyEmailCall(id))(
          Right(EmailVerificationRequested)
        )
      }

      val result: Future[Result] = performAction("email", emailWithSpaces)
      checkIsRedirect(result, checkYourInboxCall)
    }
  }

  def checkYourInboxPage(performAction: () => Future[Result], enterEmailCall: Call, expectedBackLink: String)(
    implicit messagesApi: MessagesApi
  ): Unit = {
    val email             = Email("test@email.com")
    val id                = UUID.randomUUID()
    val emailToBeVerified = EmailToBeVerified(email, id, false)
    val sessionData = SessionData.empty.copy(
      journeyStatus     = Some(validJourneyStatus),
      emailToBeVerified = Some(emailToBeVerified)
    )

    "redirect to the enter email page" when {

      "there is no email to be verified in session" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(Future.successful(Right(Some(sessionData.copy(emailToBeVerified = None)))))
        }

        checkIsRedirect(performAction(), enterEmailCall)
      }

    }

    "show the check your inbox page when there is a BPR in session and there is an " +
      "email to be verified in session" in {
      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(Future.successful(Right(Some(sessionData))))
      }

      val result         = performAction()
      val resultAsString = contentAsString(result)
      status(result) shouldBe OK
      resultAsString should include(message("confirmEmail.title"))
      resultAsString should include(expectedBackLink)
    }
  }

  def verifyEmail(
    performAction: UUID => Future[Result],
    enterEmailCall: Call,
    emailVerifiedCall: Call
  ): Unit = {
    val email             = Email("test@email.com")
    val id                = UUID.randomUUID()
    val emailToBeVerified = EmailToBeVerified(email, id, false)

    val sessionData = SessionData.empty.copy(
      journeyStatus     = Some(validJourneyStatus),
      emailToBeVerified = Some(emailToBeVerified)
    )

    "redirect to the enter email page" when {

      "there is no email to be verified in session" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(Future.successful(Right(Some(sessionData.copy(emailToBeVerified = None)))))
        }

        val result = performAction(id)
        checkIsRedirect(result, enterEmailCall)
      }
    }

    "show an error page" when {

      "the id in the URL does not match the ID in session" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(Future.successful(Right(Some(sessionData))))
        }

        checkIsTechnicalErrorPage(performAction(UUID.randomUUID()))
      }

      "there is an error updating the session" in {
        implicit val hc: HeaderCarrier = HeaderCarrier()
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(Future.successful(Right(Some(sessionData))))
          for {
            a <- updateEmail(validJourneyStatus, emailToBeVerified.email)
          } yield {

            mockStoreSession(
              sessionData.copy(
                emailToBeVerified = Some(emailToBeVerified.copy(verified = true)),
                journeyStatus     = Some(a)
              )
            )(Future.successful(Left(Error(""))))

          }
        }

        checkIsTechnicalErrorPage(performAction(id))
      }

    }

    "redirect to email verified" when {

      "the session indicates the email has already been verified" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            Future.successful(
              Right(Some(sessionData.copy(emailToBeVerified = Some(emailToBeVerified.copy(verified = true)))))
            )
          )
        }

        checkIsRedirect(performAction(id), emailVerifiedCall)
      }

      "the session is updated" in {
        implicit val hc: HeaderCarrier = HeaderCarrier()
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(Future.successful(Right(Some(sessionData))))
          for {
            journey <- updateEmail(validJourneyStatus, emailToBeVerified.email)
          } yield {
            mockStoreSession(
              sessionData.copy(
                emailToBeVerified = Some(emailToBeVerified.copy(verified = true)),
                journeyStatus = Some(journey)
              )
            )(Future.successful(Right(())))
          }
        }

        checkIsRedirect(performAction(id), emailVerifiedCall)
      }

    }
  }

  def emailVerifiedPage(
    performAction: () => Future[Result],
    expectedContinueCall: Call,
    enterEmailCall: Call
  )(implicit messagesApi: MessagesApi): Unit = {
    val emailToBeVerified = EmailToBeVerified(Email("verified@email.com"), UUID.randomUUID(), true)

    val sessionData = SessionData.empty.copy(
      journeyStatus     = Some(validVerificationCompleteJourneyStatus),
      emailToBeVerified = Some(emailToBeVerified)
    )

    "show an error page" when {

      "the email has not been verified" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            Future.successful(
              Right(Some(sessionData.copy(emailToBeVerified = Some(emailToBeVerified.copy(verified = false)))))
            )
          )
        }

        checkIsTechnicalErrorPage(performAction())
      }

    }

    "redirect to enter email " when {

      "there is no email to be verified in session" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(Future.successful(Right(Some(sessionData.copy(emailToBeVerified = None)))))
        }

        checkIsRedirect(performAction(), enterEmailCall)

      }

    }

    "show the email verified page" when {

      "the email has been verified" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(Future.successful(Right(Some(sessionData))))
        }

        val result = performAction()
        status(result) shouldBe OK
        val content = contentAsString(result)
        content should include(message("confirmEmail.verified.title"))
        content should include(expectedContinueCall.url)
      }

    }

  }

}
