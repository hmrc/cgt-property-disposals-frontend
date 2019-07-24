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

import play.api.i18n.MessagesApi
import play.api.test.CSRFTokenHelper._
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.authorise.EmptyPredicate
import uk.gov.hmrc.auth.core.retrieve.EmptyRetrieval
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views

import scala.concurrent.Future

class BusinessPartnerRecordCheckControllerSpec extends ControllerSpec with AuthSupport {

  lazy val controller = new BusinessPartnerRecordCheckController(
    testAuthenticatedAction,
    messagesControllerComponents,
    instanceOf[views.html.bprcheck.nino]
  )(viewConfig)

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  def mockSuccessfulAuth(): Unit = mockAuth(EmptyPredicate, EmptyRetrieval)(Future.successful(EmptyRetrieval))

  "The BusinessPartnerRecordCheckController" when {

    "handling NINOs" must {

      "be able to display the get NINO page" in {
        mockSuccessfulAuth()

        contentAsString(controller.getNino()(FakeRequest().withCSRFToken)) should include(message("onboarding.nino.title"))
      }

      "show errors when the NINO submitted is invalid" in {
          def test(nino: String)(expectedErrorMessage: String): Unit = {
            mockSuccessfulAuth()

            val result = controller.getNinoSubmit()(FakeRequest().withFormUrlEncodedBody("nino" -> nino).withCSRFToken)
            status(result) shouldBe BAD_REQUEST
            contentAsString(result) should include(message(expectedErrorMessage))
          }

        test("")("nino.invalid")
        test("AB123456")("nino.invalid")
      }

      "redirect to the get DOB page if the NINO submitted is valid" in {
        mockSuccessfulAuth()

        val result = controller.getNinoSubmit()(FakeRequest().withFormUrlEncodedBody("nino" -> "AB123456C").withCSRFToken)
        status(result) shouldBe SEE_OTHER
        redirectLocation(result) shouldBe Some(routes.BusinessPartnerRecordCheckController.getDateOfBirth().url)
      }

    }

    "handling date of births" must {

      "display the correct content" in {
        mockSuccessfulAuth()

        contentAsString(controller.getDateOfBirth()(FakeRequest().withCSRFToken)) should include("Give us your date of birth")
      }

    }

  }

}
