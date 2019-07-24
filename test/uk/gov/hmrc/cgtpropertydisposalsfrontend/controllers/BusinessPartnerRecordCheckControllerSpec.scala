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
import play.api.mvc.RequestHeader
import play.api.test.CSRFTokenHelper._
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.authorise.EmptyPredicate
import uk.gov.hmrc.auth.core.retrieve.EmptyRetrieval
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.DateOfBirth
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.DateOfBirth.Ids
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views

import scala.concurrent.Future

class BusinessPartnerRecordCheckControllerSpec extends ControllerSpec with AuthSupport {

  lazy val controller = new BusinessPartnerRecordCheckController(
    testAuthenticatedAction,
    messagesControllerComponents,
    instanceOf[views.html.bprcheck.nino],
    instanceOf[views.html.bprcheck.date_of_birth]
  )(viewConfig)

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  def mockSuccessfulAuth(): Unit = mockAuth(EmptyPredicate, EmptyRetrieval)(Future.successful(EmptyRetrieval))

  val requestWithCSRFToken = FakeRequest().withCSRFToken

  def requestWithFormData(data: (String, String)*) = FakeRequest().withFormUrlEncodedBody(data: _*).withCSRFToken

  "The BusinessPartnerRecordCheckController" when {

    "handling NINOs" must {

      "be able to display the get NINO page" in {
        mockSuccessfulAuth()

        contentAsString(controller.getNino()(requestWithCSRFToken)) should include(message("onboarding.nino.title"))
      }

      "show errors when the NINO submitted is invalid" in {
          def test(nino: String)(expectedErrorMessageKey: String): Unit = {
            withClue(s"For NINO '$nino': ") {
              mockSuccessfulAuth()

              val result = controller.getNinoSubmit()(requestWithFormData("nino" -> nino))
              status(result) shouldBe BAD_REQUEST
              contentAsString(result) should include(message(expectedErrorMessageKey))
            }
          }

        test("")("nino.invalid")
        test("AB123456")("nino.invalid")
      }

      "redirect to the get DOB page if the NINO submitted is valid" in {
          def test(nino: String): Unit = {
            withClue(s"For NINO '$nino': ") {
              mockSuccessfulAuth()

              val result = controller.getNinoSubmit()(requestWithFormData("nino" -> nino))
              status(result) shouldBe SEE_OTHER
              redirectLocation(result) shouldBe Some(routes.BusinessPartnerRecordCheckController.getDateOfBirth().url)
            }
          }

        test("AB123456C")
        test("AB    12  345 6 C")
      }

    }

    "handling date of births" must {

        def toFieldList(d: LocalDate): List[(String, String)] =
          List(
            Ids.day -> d.getDayOfMonth.toString,
            Ids.month -> d.getMonthValue.toString,
            Ids.year -> d.getYear.toString
          )

      val today = LocalDate.now(Clock.systemUTC())

      "be able to display the get DOB page" in {
        mockSuccessfulAuth()

        contentAsString(controller.getDateOfBirth()(requestWithCSRFToken)) should include(message("onboarding.dob.title"))
      }

      "show an error" when {

          def testErrorWithRawData(formData: (String, String)*)(expectedErrorMessageKey: String) = {
            withClue(s"For form data [$formData]: ") {
              mockSuccessfulAuth()

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

      "accept real date of births which are before 16 years in the past" in {
          def testSuccess(d: LocalDate) = {
            withClue(s"For date $d: ") {
              mockSuccessfulAuth()

              val result = controller.getDateOfBirthSubmit()(requestWithFormData(toFieldList(d): _*))
              status(result) shouldBe OK
              contentAsString(result) shouldBe s"Got ${DateOfBirth(d)}"
            }
          }

        (0 to 365).foreach(i => testSuccess(today.minusYears(16L).minusDays(i)))
      }

    }

  }

}
