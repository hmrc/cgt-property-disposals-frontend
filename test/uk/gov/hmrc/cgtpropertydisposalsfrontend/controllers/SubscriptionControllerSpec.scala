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

import java.time.LocalDate

import play.api.i18n.MessagesApi
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.CSRFTokenHelper._
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Address.UkAddress
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{BusinessPartnerRecord, DateOfBirth, Error, NINO, SessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.BusinessPartnerRecordService
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.Future

class SubscriptionControllerSpec extends ControllerSpec with AuthSupport with SessionSupport {

  val mockService = mock[BusinessPartnerRecordService]

  override val overrideBindings =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore),
      bind[BusinessPartnerRecordService].toInstance(mockService)
    )

  lazy val controller = instanceOf[SubscriptionController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  def mockGetBusinessPartnerRecord(nino: NINO)(result: Future[Either[Error, BusinessPartnerRecord]]) =
    (mockService.getBusinessPartnerRecord(_: NINO)(_: HeaderCarrier))
      .expects(nino, *)
      .returning(result)

  val requestWithCSRFToken = FakeRequest().withCSRFToken

  def requestWithFormData(data: (String, String)*) = FakeRequest().withFormUrlEncodedBody(data: _*).withCSRFToken

  val validNINO = NINO("AB123456C")
  val validDOB = DateOfBirth(LocalDate.of(2000, 1, 1))
  val validBpr = BusinessPartnerRecord("forename", "surname", validDOB, Some("email"), UkAddress("line1", None, None, None, "postcode"))

  "The SubscriptionController" when {

    "handling requests to check subscription details" must {

        def performAction: Future[Result] =
          controller.checkYourDetails()(requestWithCSRFToken)

      val existingSession = SessionData.empty

      "display an error page" when {

        "the call to get the BPR fails" in {
          inSequence {
            mockAuthWithCl200AndRetrievedNino(validNINO.value)
            mockGetSession(Future.successful(Right(Some(existingSession))))
            mockGetBusinessPartnerRecord(validNINO)(Future.successful(Left(Error("error"))))
          }

          checkIsTechnicalErrorPage(performAction)
        }

        "the call to get BPR succeeds but it cannot be written to session" in {

          inSequence {
            mockAuthWithCl200AndRetrievedNino(validNINO.value)
            mockGetSession(Future.successful(Right(Some(existingSession))))
            mockGetBusinessPartnerRecord(validNINO)(Future.successful(Right(validBpr)))
            mockStoreSession(existingSession.copy(businessPartnerRecord = Some(validBpr)))(Future.successful(Left(Error("Oh no!"))))
          }

          checkIsTechnicalErrorPage(performAction)
        }

      }

      "display the subscription details" when {

        "one doesn't exist in session and it is successfully retrieved using the retrieved auth NINO" in {
          List(
            Some(existingSession),
            None
          ).foreach { maybeSession =>
              inSequence {
                mockAuthWithCl200AndRetrievedNino(validNINO.value)
                mockGetSession(Future.successful(Right(maybeSession)))
                mockGetBusinessPartnerRecord(validNINO)(Future.successful(Right(validBpr)))
                mockStoreSession(existingSession.copy(businessPartnerRecord = Some(validBpr)))(Future.successful(Right(())))
              }

              val result = performAction
              status(result) shouldBe 200
              contentAsString(result) should include(message("subscription.title"))
            }
        }

        "one exists in session" in {
          inSequence {
            mockAuthWithCl200AndRetrievedNino(validNINO.value)
            mockGetSession(Future.successful(Right(Some(existingSession.copy(businessPartnerRecord = Some(validBpr))))))
          }

          val result = performAction
          status(result) shouldBe 200
          contentAsString(result) should include(message("subscription.title"))
        }

      }

    }

    "handling submitted confirmation of subscription details" must {

      val existingSession = SessionData.empty.copy(businessPartnerRecord = Some(validBpr))

      "redirect to the display BPR page if there is no BPR in session" in {
        inSequence {
          mockAuthWithCl200AndRetrievedNino(validNINO.value)
          mockGetSession(Future.successful(Right(Some(SessionData.empty))))
        }

        val result = controller.checkYourDetailsSubmit()(requestWithCSRFToken)
        checkIsRedirect(result, routes.SubscriptionController.checkYourDetails())
      }

      "handle the case when the user confirms their details" in {
        inSequence {
          mockAuthWithCl200AndRetrievedNino(validNINO.value)
          mockGetSession(Future.successful(Right(Some(existingSession))))
        }

        val result = controller.checkYourDetailsSubmit()(requestWithCSRFToken)
        status(result) shouldBe OK
        contentAsString(result) shouldBe ("confirmed")
      }

    }

  }

}

