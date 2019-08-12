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

import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Address.UkAddress
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{BusinessPartnerRecord, Error, NINO, SessionData, SubscriptionDetails}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.BusinessPartnerRecordService
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.Future

class StartControllerSpec extends ControllerSpec with AuthSupport with SessionSupport {

  val mockService = mock[BusinessPartnerRecordService]

  override val overrideBindings =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore),
      bind[BusinessPartnerRecordService].toInstance(mockService)
    )

  lazy val controller = instanceOf[StartController]

  def mockGetBusinessPartnerRecord(nino: NINO)(result: Future[Either[Error, BusinessPartnerRecord]]) =
    (mockService.getBusinessPartnerRecord(_: NINO)(_: HeaderCarrier))
      .expects(nino, *)
      .returning(result)

  val nino = NINO("AB123456C")
  val emailAddress = "email"
  val bpr = BusinessPartnerRecord("forename", "surname", Some(emailAddress), UkAddress("line1", None, None, None, "postcode"))
  val subscriptionDetails = SubscriptionDetails(bpr.forename, bpr.surname, emailAddress, bpr.address)

  "The StartController" when {

    "handling requests to start a journey" when {

        def performAction(): Future[Result] =
          controller.start()(FakeRequest())

      "the user is not enrolled and is not subscribed in ETMP" must {

        "redirect to check subscription details" when {

          "there are subscription details in session" in {
            inSequence {
              mockAuthWithCl200AndRetrievedNino(nino.value)
              mockGetSession(Future.successful(Right(Some(SessionData.empty.copy(subscriptionDetails = Some(subscriptionDetails))))))
            }

            checkIsRedirect(performAction(), routes.SubscriptionController.checkYourDetails())
          }

        }

        "display an error page" when {

          "the call to get the BPR fails" in {
            inSequence {
              mockAuthWithCl200AndRetrievedNino(nino.value)
              mockGetSession(Future.successful(Right(Some(SessionData.empty))))
              mockGetBusinessPartnerRecord(nino)(Future.successful(Left(Error("error"))))
            }

            checkIsTechnicalErrorPage(performAction())
          }

          "there is no email in the BPR" in {
            inSequence {
              mockAuthWithCl200AndRetrievedNino(nino.value)
              mockGetSession(Future.successful(Right(Some(SessionData.empty))))
              mockGetBusinessPartnerRecord(nino)(Future.successful(Right(bpr.copy(emailAddress = None))))
            }

            checkIsTechnicalErrorPage(performAction())
          }

          "the call to get BPR succeeds but it cannot be written to session" in {
            inSequence {
              mockAuthWithCl200AndRetrievedNino(nino.value)
              mockGetSession(Future.successful(Right(Some(SessionData.empty))))
              mockGetBusinessPartnerRecord(nino)(Future.successful(Right(bpr)))
              mockStoreSession(SessionData.empty.copy(subscriptionDetails = Some(subscriptionDetails)))(Future.successful(Left(Error("Oh no!"))))
            }

            checkIsTechnicalErrorPage(performAction())
          }

        }

        "redirect to check subscription details" when {

          "one doesn't exist in session and it is successfully retrieved using the retrieved auth NINO" in {
            List(
              Some(SessionData.empty),
              None
            ).foreach { maybeSession =>
                inSequence {
                  mockAuthWithCl200AndRetrievedNino(nino.value)
                  mockGetSession(Future.successful(Right(maybeSession)))
                  mockGetBusinessPartnerRecord(nino)(Future.successful(Right(bpr)))
                  mockStoreSession(SessionData.empty.copy(subscriptionDetails = Some(subscriptionDetails)))(Future.successful(Right(())))
                }

                checkIsRedirect(performAction(), routes.SubscriptionController.checkYourDetails())
              }
          }
        }

      }
    }

  }

}
