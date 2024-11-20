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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers

import cats.data.EitherT
import cats.instances.future._
import org.scalamock.handlers.CallHandler0
import play.api.i18n.MessagesApi
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.{Call, Result}
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.email.EmailJourneyType
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.http.AcceptLanguage
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.UUIDGenerator
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.ContactName
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.email.{Email, EmailToBeVerified}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, JourneyStatus, SessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.EmailVerificationService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.EmailVerificationService.EmailVerificationResponse
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.EmailVerificationService.EmailVerificationResponse.{EmailAlreadyVerified, EmailVerificationRequested}
import uk.gov.hmrc.http.HeaderCarrier

import java.util.UUID
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

trait EmailControllerSpec[JourneyType <: EmailJourneyType] extends ControllerSpec with AuthSupport with SessionSupport {

  protected def toJourneyStatus(journeyType: JourneyType): JourneyStatus

  protected val validJourneyStatus: JourneyType

  protected val validVerificationCompleteJourneyStatus: JourneyType

  protected def updateEmail(journey: JourneyType, email: Email): JourneyType

  protected val mockUpdateEmail: Option[(JourneyType, JourneyType, Either[Error, Unit]) => Unit]

  protected val controller: EmailController[JourneyType]

  protected implicit val messagesApi: MessagesApi

  protected val mockService: EmailVerificationService = mock[EmailVerificationService]

  protected val mockUuidGenerator: UUIDGenerator = mock[UUIDGenerator]

  protected override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore),
      bind[EmailVerificationService].toInstance(mockService),
      bind[UUIDGenerator].toInstance(mockUuidGenerator)
    )

  private def mockEmailVerification(
    expectedEmail: Email,
    expectedName: ContactName,
    expectedContinue: Call,
    expectedLanguage: AcceptLanguage
  )(result: Either[Error, EmailVerificationResponse]) =
    (mockService
      .verifyEmail(_: Email, _: ContactName, _: Call, _: AcceptLanguage)(_: HeaderCarrier))
      .expects(expectedEmail, expectedName, expectedContinue, expectedLanguage, *)
      .returning(EitherT.fromEither[Future](result))

  private def mockUuidGenerator(uuid: UUID): CallHandler0[UUID] =
    (mockUuidGenerator.nextId _: () => UUID).expects().returning(uuid)

  private lazy val sessionDataWithValidJourneyStatus =
    SessionData.empty
      .copy(journeyStatus = Some(toJourneyStatus(validJourneyStatus)))

  protected def enterEmailPage(performAction: () => Future[Result])(implicit messagesApi: MessagesApi): Unit = {
    val titleKey = "email.title"

    "display the enter email page" when {

      "there is a valid journey in session and there is no email to be verified in session" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionDataWithValidJourneyStatus)
        }

        contentAsString(performAction()) should include(
          messageFromMessageKey(titleKey)
        )
      }

      "there is a BPR in session and there is an email to be verified in session" in {
        val email   = Email("email")
        val session = sessionDataWithValidJourneyStatus.copy(
          emailToBeVerified =
            Some(EmailToBeVerified(email, UUID.randomUUID(), verified = false, hasResentVerificationEmail = false))
        )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        val result = performAction()
        contentAsString(result) should include(messageFromMessageKey(titleKey))
        contentAsString(result) should include(s"""value="${email.value}"""")
      }
    }
  }

  protected def enterEmailSubmit(
    performAction: Seq[(String, String)] => Future[Result],
    expectedName: => ContactName,
    verifyEmailCall: UUID => Call,
    checkYourInboxCall: Call
  )(implicit messagesApi: MessagesApi): Unit = {
    val email                                = Email("test@email.com")
    val id                                   = UUID.randomUUID()
    val titleKey                             = "email.title"
    def emailToBeVerified(isResend: Boolean) =
      EmailToBeVerified(email, id, verified = false, hasResentVerificationEmail = isResend)

    "show a form error" when {

      def testEmailError(email: String): Unit =
        withClue(s"For email '$email': ") {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionDataWithValidJourneyStatus)
          }

          val result  = performAction(
            Seq("email" -> email, "resendVerificationEmail" -> "false")
          )
          val content = contentAsString(result)

          status(result) shouldBe BAD_REQUEST

          content should include(messageFromMessageKey(titleKey))
          content should include(s"""value="$email"""")
          content should include(messageFromMessageKey("email.invalid"))
        }

      "the email has no '@' character" in {
        testEmailError("invalidemail")
      }

      "the email has no characters before the '@' character" in {
        testEmailError("@domain")
      }

      "the email has no characters after the '@' character" in {
        testEmailError("local@")
      }

      "the email has space" in {
        testEmailError("test @email .com")
      }

      "the email has characters before and after the '@' character but " +
        "there are more than 132 characters in it" in {
          val longString = List.fill(100)("a").mkString("")
          testEmailError(s"$longString@$longString")
        }

    }

    "show an error page" when {

      "the email to be verified cannot be stored in session" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionDataWithValidJourneyStatus)
          mockUuidGenerator(id)
          mockStoreSession(
            sessionDataWithValidJourneyStatus
              .copy(emailToBeVerified = Some(emailToBeVerified(false)))
          )(
            Left(Error(""))
          )
        }

        val result = performAction(
          Seq("email" -> email.value, "resendVerificationEmail" -> "false")
        )
        checkIsTechnicalErrorPage(result)
      }

      "the call to verify the email fails" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionDataWithValidJourneyStatus)
          mockUuidGenerator(id)
          mockStoreSession(
            sessionDataWithValidJourneyStatus
              .copy(emailToBeVerified = Some(emailToBeVerified(false)))
          )(
            Right(())
          )
          mockEmailVerification(email, expectedName, verifyEmailCall(id), AcceptLanguage.EN)(
            Left(Error(""))
          )
        }

        val result = performAction(
          Seq("email" -> email.value, "resendVerificationEmail" -> "false")
        )
        checkIsTechnicalErrorPage(result)
      }

    }

    "redirect to confirm email when the email address has been verified" in {
      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(sessionDataWithValidJourneyStatus)
        mockUuidGenerator(id)
        mockStoreSession(
          sessionDataWithValidJourneyStatus
            .copy(emailToBeVerified = Some(emailToBeVerified(false)))
        )(
          Right(())
        )
        mockEmailVerification(email, expectedName, verifyEmailCall(id), AcceptLanguage.EN)(
          Right(EmailAlreadyVerified)
        )
      }

      val result: Future[Result] = performAction(
        Seq("email" -> email.value, "resendVerificationEmail" -> "false")
      )
      checkIsRedirect(result, verifyEmailCall(id))
    }

    "redirect to the check you inbox page when the email address verification request " +
      "has successfully been sent" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionDataWithValidJourneyStatus)
          mockUuidGenerator(id)
          mockStoreSession(
            sessionDataWithValidJourneyStatus
              .copy(emailToBeVerified = Some(emailToBeVerified(false)))
          )(
            Right(())
          )
          mockEmailVerification(email, expectedName, verifyEmailCall(id), AcceptLanguage.EN)(
            Right(EmailVerificationRequested)
          )
        }

        val result: Future[Result] = performAction(
          Seq("email" -> email.value, "resendVerificationEmail" -> "false")
        )
        checkIsRedirect(result, checkYourInboxCall)
      }

    "reuse the same id in the continue url if there is an existing email to be verified in session " +
      "and the emails match" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            sessionDataWithValidJourneyStatus
              .copy(emailToBeVerified = Some(emailToBeVerified(false)))
          )
          mockEmailVerification(email, expectedName, verifyEmailCall(id), AcceptLanguage.EN)(
            Right(EmailVerificationRequested)
          )
        }

        val result: Future[Result] = performAction(
          Seq("email" -> email.value, "resendVerificationEmail" -> "false")
        )
        checkIsRedirect(result, checkYourInboxCall)
      }
  }

  protected def checkYourInboxPage(
    performAction: () => Future[Result],
    enterEmailCall: Call,
    expectedBackLink: String
  )(implicit
    messagesApi: MessagesApi
  ): Unit = {
    val email                 = Email("test@email.com")
    val id                    = UUID.randomUUID()
    val emailToBeVerified     = EmailToBeVerified(email, id, verified = false, hasResentVerificationEmail = false)
    lazy val sessionData      = SessionData.empty.copy(
      journeyStatus = Some(toJourneyStatus(validJourneyStatus)),
      emailToBeVerified = Some(emailToBeVerified)
    )
    lazy val expectedTitleKey = validJourneyStatus match {
      case _: EmailJourneyType.Returns.ChangingRepresenteeEmail =>
        "confirmEmail.representee.title"
      case _                                                    => "confirmEmail.title"
    }

    "redirect to the enter email page" when {

      "there is no email to be verified in session" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionData.copy(emailToBeVerified = None))
        }

        checkIsRedirect(performAction(), enterEmailCall)
      }

    }

    "show the check your inbox page when there is a BPR in session and there is an " +
      "email to be verified in session" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionData)
        }

        val result         = performAction()
        val resultAsString = contentAsString(result)
        status(result) shouldBe OK
        resultAsString   should include(messageFromMessageKey(expectedTitleKey))
        resultAsString   should include(expectedBackLink)
      }
  }

  protected def verifyEmail(
    performAction: UUID => Future[Result],
    enterEmailCall: Call,
    emailVerifiedCall: Call
  ): Unit = {
    val email             = Email("test@email.com")
    val id                = UUID.randomUUID()
    val emailToBeVerified = EmailToBeVerified(email, id, verified = false, hasResentVerificationEmail = false)

    lazy val sessionData = SessionData.empty.copy(
      journeyStatus = Some(toJourneyStatus(validJourneyStatus)),
      emailToBeVerified = Some(emailToBeVerified)
    )

    "redirect to the enter email page" when {

      "there is no email to be verified in session" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionData.copy(emailToBeVerified = None))
        }

        val result = performAction(id)
        checkIsRedirect(result, enterEmailCall)
      }
    }

    "show an error page" when {

      "the id in the URL does not match the ID in session" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionData)
        }

        checkIsTechnicalErrorPage(performAction(UUID.randomUUID()))
      }

      "there is an error updating the session" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionData)
          mockUpdateEmail.foreach { f =>
            f(
              validJourneyStatus,
              updateEmail(validJourneyStatus, emailToBeVerified.email),
              Right(())
            )
          }
          mockStoreSession(
            sessionData.copy(
              emailToBeVerified = Some(emailToBeVerified.copy(verified = true)),
              journeyStatus = Some(
                toJourneyStatus(
                  updateEmail(validJourneyStatus, emailToBeVerified.email)
                )
              )
            )
          )(Left(Error("")))
        }
        checkIsTechnicalErrorPage(performAction(id))
      }

      "there is an error updating the email when it is required" in {
        mockUpdateEmail.foreach { f =>
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionData)
            f(
              validJourneyStatus,
              updateEmail(validJourneyStatus, emailToBeVerified.email),
              Left(Error("Error updating email"))
            )
          }
          checkIsTechnicalErrorPage(performAction(id))
        }
      }
    }

    "redirect to email verified" when {

      "the session indicates the email has already been verified" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            sessionData.copy(emailToBeVerified = Some(emailToBeVerified.copy(verified = true)))
          )
        }
        checkIsRedirect(performAction(id), emailVerifiedCall)
      }

      "the session is updated" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionData)
          mockUpdateEmail.foreach { f =>
            f(
              validJourneyStatus,
              updateEmail(validJourneyStatus, emailToBeVerified.email),
              Right(())
            )
          }
          mockStoreSession(
            sessionData.copy(
              emailToBeVerified = Some(emailToBeVerified.copy(verified = true)),
              journeyStatus = Some(
                toJourneyStatus(
                  updateEmail(validJourneyStatus, emailToBeVerified.email)
                )
              )
            )
          )(Right(()))
        }
        checkIsRedirect(performAction(id), emailVerifiedCall)
      }
    }
  }

  protected def emailVerifiedPage(
    performAction: () => Future[Result],
    expectedContinueCall: Call,
    enterEmailCall: Call
  )(implicit messagesApi: MessagesApi): Unit = {
    val emailToBeVerified = EmailToBeVerified(
      Email("verified@email.com"),
      UUID.randomUUID(),
      verified = true,
      hasResentVerificationEmail = false
    )

    lazy val sessionData = SessionData.empty.copy(
      journeyStatus = Some(toJourneyStatus(validVerificationCompleteJourneyStatus)),
      emailToBeVerified = Some(emailToBeVerified)
    )

    "show an error page" when {

      "the email has not been verified" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            sessionData.copy(emailToBeVerified = Some(emailToBeVerified.copy(verified = false)))
          )
        }

        checkIsTechnicalErrorPage(performAction())
      }

    }

    "redirect to enter email " when {

      "there is no email to be verified in session" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionData.copy(emailToBeVerified = None))
        }

        checkIsRedirect(performAction(), enterEmailCall)

      }

    }

    "show the email verified page" when {

      "the email has been verified" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionData)
        }

        val result = performAction()
        status(result) shouldBe OK
        val content = contentAsString(result)
        content should include(
          messageFromMessageKey("confirmEmail.verified.title")
        )
        content should include(expectedContinueCall.url)
      }

    }

  }

}
