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

import java.time.{Clock, LocalDate}

import play.api.i18n.MessagesApi
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.test.CSRFTokenHelper._
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.auth.core.authorise.EmptyPredicate
import uk.gov.hmrc.auth.core.retrieve.EmptyRetrieval
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Address.UkAddress
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{BusinessPartnerRecord, DateOfBirth, Error, NINO, SessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.DateOfBirth.Ids
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.BusinessPartnerRecordService
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.Future

class BusinessPartnerRecordCheckControllerSpec extends ControllerSpec with AuthSupport with SessionSupport {

  val mockService = mock[BusinessPartnerRecordService]

  override val overrideBindings =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore),
      bind[BusinessPartnerRecordService].toInstance(mockService)
    )

  lazy val controller = instanceOf[BusinessPartnerRecordCheckController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  def mockSuccessfulAuth(): Unit = mockAuth(EmptyPredicate, EmptyRetrieval)(Future.successful(EmptyRetrieval))

  def mockGetBusinessPartnerRecord(nino: NINO, dob: DateOfBirth)(result: Future[Either[Error, BusinessPartnerRecord]]) =
    (mockService.getBusinessPartnerRecord(_: NINO, _: DateOfBirth)(_: HeaderCarrier))
      .expects(nino, dob, *)
      .returning(result)

  val requestWithCSRFToken = FakeRequest().withCSRFToken

  def requestWithFormData(data: (String, String)*) = FakeRequest().withFormUrlEncodedBody(data: _*).withCSRFToken

  val validNINO = NINO("AB123456C")
  val validDOB = DateOfBirth(LocalDate.of(2000, 1, 1))
  val validBpr = BusinessPartnerRecord("forename", "surname", validDOB, Some("email"), UkAddress("line1", None, None, None, "postcode"))

  "The BusinessPartnerRecordCheckController" when {

    "handling requests to display the get NINO page" must {
      "be able to display the get NINO page" in {
        inSequence {
          mockSuccessfulAuth()
          mockGetSession(Future.successful(Right(None)))
        }

        contentAsString(controller.getNino()(requestWithCSRFToken)) should include(message("onboarding.nino.title"))
      }

      "prepopulate the page with the NINO in session if one is found" in {
        inSequence {
          mockSuccessfulAuth()
          mockGetSession(Future.successful(Right(Some(SessionData.empty.copy(nino = Some(validNINO))))))
        }

        val content = contentAsString(controller.getNino()(requestWithCSRFToken))
        content should include(message("onboarding.nino.title"))
        content should include(validNINO.value)
      }

    }

    "handling submitted NINOs" must {

      "redirect to the get DOB page if the NINO submitted is valid" in {
          def test(submittedNINO: String, expectedStoredNINO: String): Unit = {
            withClue(s"For submitted NINO '$submittedNINO': ") {
              inSequence {
                mockSuccessfulAuth()
                mockGetSession(Future.successful(Right(None)))
                mockStoreSession(SessionData.empty.copy(nino = Some(NINO(expectedStoredNINO))))(Future.successful(Right(())))
              }

              val result = controller.getNinoSubmit()(requestWithFormData("nino" -> submittedNINO))
              status(result) shouldBe SEE_OTHER
              redirectLocation(result) shouldBe Some(routes.BusinessPartnerRecordCheckController.getDateOfBirth().url)
            }
          }

        test("AB123456C", "AB123456C")
        test("AB    12  345 6 C", "AB123456C")
      }

      "redirect to the get DOB page if the NINO submitted is valid and overwrite any existing " +
        "NINO in session" in {
          val existingSession = SessionData.empty.copy(nino = Some(validNINO))
          val newNINO = NINO("AB234567C")

          inSequence {
            mockSuccessfulAuth()
            mockGetSession(Future.successful(Right(Some(existingSession))))
            mockStoreSession(SessionData.empty.copy(nino = Some(newNINO)))(Future.successful(Right(())))
          }

          val result = controller.getNinoSubmit()(requestWithFormData("nino" -> newNINO.value))
          status(result) shouldBe SEE_OTHER
          redirectLocation(result) shouldBe Some(routes.BusinessPartnerRecordCheckController.getDateOfBirth().url)
        }

      "show a form error when the NINO submitted is invalid" in {
          def test(nino: String)(expectedErrorMessageKey: String): Unit = {
            withClue(s"For NINO '$nino': ") {
              inSequence {
                mockSuccessfulAuth()
                mockGetSession(Future.successful(Right(None)))
              }

              val result = controller.getNinoSubmit()(requestWithFormData("nino" -> nino))
              status(result) shouldBe BAD_REQUEST
              contentAsString(result) should include(message(expectedErrorMessageKey))
            }
          }

        test("")("nino.invalid")
        test("AB123456")("nino.invalid")
      }

      "display an error page" when {

        "there is an error storing the NINO in store" in {
          val error = Error("Oh no!")
          inSequence {
            mockSuccessfulAuth()
            mockGetSession(Future.successful(Right(None)))
            mockStoreSession(SessionData.empty.copy(nino = Some(validNINO)))(Future.successful(Left(error)))
          }

          val result = controller.getNinoSubmit()(requestWithFormData("nino" -> validNINO.value))
          checkIsTechnicalErrorPage(result)
        }

      }

    }

    "handling requests to get the date of birth page" must {

      "redirect to the NINO page if there is no NINO in the session" in {
          def test(sessionData: Option[SessionData]) = {
            withClue(s"For session data $sessionData: ") {
              inSequence {
                mockSuccessfulAuth()
                mockGetSession(Future.successful(Right(sessionData)))
              }

              val result = controller.getDateOfBirth()(requestWithCSRFToken)
              status(result) shouldBe SEE_OTHER
              redirectLocation(result) shouldBe Some(routes.BusinessPartnerRecordCheckController.getNino().url)
            }
          }

        test(None)
        test(Some(SessionData.empty))
        test(Some(SessionData.empty.copy(dob = Some(validDOB))))
      }

      "display the get DOB page if there is a NINO in session" in {
        inSequence {
          mockSuccessfulAuth()
          mockGetSession(Future.successful(Right(Some(SessionData.empty.copy(nino = Some(validNINO))))))
        }

        contentAsString(controller.getDateOfBirth()(requestWithCSRFToken)) should include(message("onboarding.dob.title"))
      }

      "display the get DOB page if there is a NINO in session and prepopulate the DOB with the " +
        "one in session if one exists" in {
          inSequence {
            mockSuccessfulAuth()
            mockGetSession(Future.successful(Right(Some(SessionData.empty.copy(nino = Some(validNINO), dob = Some(validDOB))))))
          }

          val content = contentAsString(controller.getDateOfBirth()(requestWithCSRFToken))
          content should include(message("onboarding.dob.title"))
          // probably not the best way to check below
          content should include(s"""value="${validDOB.value.getDayOfMonth}"""")
          content should include(s"""value="${validDOB.value.getMonthValue}"""")
          content should include(s"""value="${validDOB.value.getYear}"""")
        }

    }

    "handling submitted dates of birth" must {

        def toFieldList(d: LocalDate): List[(String, String)] =
          List(
            Ids.day -> d.getDayOfMonth.toString,
            Ids.month -> d.getMonthValue.toString,
            Ids.year -> d.getYear.toString
          )

      val today = LocalDate.now(Clock.systemUTC())

      val existingSession = SessionData.empty.copy(nino = Some(validNINO))

      "accept real date of births which are before 16 years in the past" in {
          def testSuccess(d: LocalDate) = {
            withClue(s"For date $d: ") {
              inSequence {
                mockSuccessfulAuth()
                mockGetSession(Future.successful(Right(Some(existingSession))))
                mockStoreSession(existingSession.copy(dob = Some(DateOfBirth(d))))(Future.successful(Right(())))
              }

              val result = controller.getDateOfBirthSubmit()(requestWithFormData(toFieldList(d): _*))
              status(result) shouldBe SEE_OTHER
              redirectLocation(result) shouldBe Some(routes.BusinessPartnerRecordCheckController.displayBusinessPartnerRecord().url)
            }
          }

        (0 to 365).foreach(i => testSuccess(today.minusYears(16L).minusDays(i)))
      }

      "accept real date of births which are before 16 years in the past and overwrite any existing DOB " +
        "in the session" in {
          val existingSession = SessionData(Some(validNINO), Some(validDOB), None)
          val newDOB = DateOfBirth(validDOB.value.plusDays(1L))

          inSequence {
            mockSuccessfulAuth()
            mockGetSession(Future.successful(Right(Some(existingSession))))
            mockStoreSession(existingSession.copy(dob = Some(newDOB)))(Future.successful(Right(())))
          }

          val result = controller.getDateOfBirthSubmit()(requestWithFormData(toFieldList(newDOB.value): _*))
          status(result) shouldBe SEE_OTHER
          redirectLocation(result) shouldBe Some(routes.BusinessPartnerRecordCheckController.displayBusinessPartnerRecord().url)
        }

      "redirect to the get NINO page if there is no NINO in session" in {
          def test(sessionData: Option[SessionData]) = {
            withClue(s"For session data $sessionData: ") {
              inSequence {
                mockSuccessfulAuth()
                mockGetSession(Future.successful(Right(sessionData)))
              }

              val result = controller.getDateOfBirthSubmit()(requestWithCSRFToken)
              status(result) shouldBe SEE_OTHER
              redirectLocation(result) shouldBe Some(routes.BusinessPartnerRecordCheckController.getNino().url)
            }
          }

        test(None)
        test(Some(SessionData.empty))
        test(Some(SessionData.empty.copy(dob = Some(validDOB))))
      }

      "show a form error" when {

          def testErrorWithRawData(formData: (String, String)*)(expectedErrorMessageKey: String) = {
            withClue(s"For form data [$formData]: ") {
              inSequence {
                mockSuccessfulAuth()
                mockGetSession(Future.successful(Right(Some(existingSession))))
              }

              val result = controller.getDateOfBirthSubmit()(requestWithFormData(formData: _*))
              status(result) shouldBe BAD_REQUEST
              contentAsString(result) should include(message(expectedErrorMessageKey))
            }
          }

          def testErrorWithDate(d: LocalDate)(expectedErrorMessageKey: String) =
            testErrorWithRawData(toFieldList(d): _*)(expectedErrorMessageKey)

        "the DOB not more than 16 years in the past" in {
          testErrorWithDate(today.minusYears(16L).plusDays(1L))("dob.invalid")
        }

        "the DOB is in the future" in {
          testErrorWithDate(today.plusDays(1L))("dob.invalid")
        }

        "the date entered is not a real one" in {
          // 31st June doesn't exist
          testErrorWithRawData(Ids.day -> "31", Ids.month -> "6", Ids.year -> "2000")("dob.invalid")
        }

        "one or more of the date fields are blank" in {
          val (validDay, validMonth, validYear) = ("1", "1", "2000")

          val l = List(
            Ids.day -> validDay,
            Ids.month -> validMonth,
            Ids.year -> validYear
          )

          (0 to 2).flatMap(l.combinations).foreach(testErrorWithRawData(_: _*)("dob.invalid"))
        }

      }

      "display an error page" when {

        "there is an error storing the DOB in store" in {
          val error = Error("Oh no!")

          inSequence {
            mockSuccessfulAuth()
            mockGetSession(Future.successful(Right(Some(existingSession))))
            mockStoreSession(existingSession.copy(dob = Some(validDOB)))(Future.successful(Left(error)))
          }

          val result = controller.getDateOfBirthSubmit()(requestWithFormData(toFieldList(validDOB.value): _*))
          checkIsTechnicalErrorPage(result)
        }

      }

    }

    "handling requests to display BPR's" must {

        def performAction = controller.displayBusinessPartnerRecord()(requestWithCSRFToken)

      val existingSession = SessionData.empty.copy(nino = Some(validNINO), dob = Some(validDOB))

      "redirect to the NINO page if there is no NINO in session" in {
        inSequence {
          mockSuccessfulAuth()
          mockGetSession(Future.successful(Right(Some(SessionData.empty))))
        }

        val result = performAction
        status(result) shouldBe SEE_OTHER
        redirectLocation(result) shouldBe Some(routes.BusinessPartnerRecordCheckController.getNino().url)
      }

      "redirect to the DOB page if there is no DOB in session" in {
        inSequence {
          mockSuccessfulAuth()
          mockGetSession(Future.successful(Right(Some(SessionData.empty))))
        }

        val result = performAction
        status(result) shouldBe SEE_OTHER
        redirectLocation(result) shouldBe Some(routes.BusinessPartnerRecordCheckController.getNino().url)
      }

      "display an error page" when {

        "the call to get the BPR fails" in {
          inSequence {
            mockSuccessfulAuth()
            mockGetSession(Future.successful(Right(Some(existingSession))))
            mockGetBusinessPartnerRecord(validNINO, validDOB)(Future.successful(Left(Error("error"))))
          }

          checkIsTechnicalErrorPage(performAction)
        }

        "the call to get BPR succeeds but it cannot be written to session" in {
          val existingSession = SessionData.empty.copy(nino = Some(validNINO), dob = Some(validDOB))
          inSequence {
            mockSuccessfulAuth()
            mockGetSession(Future.successful(Right(Some(existingSession))))
            mockGetBusinessPartnerRecord(validNINO, validDOB)(Future.successful(Right(validBpr)))
            mockStoreSession(existingSession.copy(businessPartnerRecord = Some(validBpr)))(Future.successful(Left(Error("Oh no!"))))
          }

          checkIsTechnicalErrorPage(performAction)
        }

      }

      "display the BPR" when {

        "one doesn't exist in session and it is successfully retrieved" in {
          inSequence {
            mockSuccessfulAuth()
            mockGetSession(Future.successful(Right(Some(existingSession))))
            mockGetBusinessPartnerRecord(validNINO, validDOB)(Future.successful(Right(validBpr)))
            mockStoreSession(existingSession.copy(businessPartnerRecord = Some(validBpr)))(Future.successful(Right(())))
          }

          val result = performAction
          status(result) shouldBe 200
          contentAsString(result) should include(message("onboarding.bpr.title"))
        }

        "one exists in session" in {
          inSequence {
            mockSuccessfulAuth()
            mockGetSession(Future.successful(Right(Some(existingSession.copy(businessPartnerRecord = Some(validBpr))))))
          }

          val result = performAction
          status(result) shouldBe 200
          contentAsString(result) should include(message("onboarding.bpr.title"))
        }

      }

    }

  }

}

