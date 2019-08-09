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
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.CSRFTokenHelper._
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Address.UkAddress
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{AddressLookupResult, Error, NINO, Postcode, SessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{bprGen, sample}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.AddressLookupService
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.Future

class AddressControllerSpec extends ControllerSpec with AuthSupport with SessionSupport {

  val mockService = mock[AddressLookupService]

  def mockAddressLookup(expectedPostcode: Postcode)(result: Future[Either[Error, AddressLookupResult]]) =
    (mockService.lookupAddress(_: Postcode)(_: HeaderCarrier))
      .expects(expectedPostcode, *)
      .returning(result)

  override val overrideBindings: List[GuiceableModule] =
    List(
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore),
      bind[AddressLookupService].toInstance(mockService)
    )

  lazy val controller = instanceOf[AddressController]

  lazy implicit val messagesApi: MessagesApi = controller.messagesApi

  val nino = NINO("AB123456C")

  val postcode = Postcode("ABC 123")

  val bpr = sample(bprGen)

  val (addressHead, addresses) = {
      def address(i: Int) = UkAddress(s"$i the Street", Some("The Town"), None, None, postcode.value)

    val head = address(1)
    head -> (head :: (2 to 5).map(address).toList)
  }

  val addressLookupResult = AddressLookupResult(postcode, addresses)

  "AddressController" when {

    "handling requests to display the enter postcode page" must {

        def performAction() = controller.enterPostcode()(FakeRequest())

      "redirect to check your details" when {

        "there is no session data" in {
          inSequence {
            mockAuthWithCl200AndRetrievedNino(nino.value)
            mockGetSession(Future.successful(Right(None)))
          }

          val result = performAction()
          checkIsRedirect(result, routes.SubscriptionController.checkYourDetails())
        }

        "there is no BPR in session" in {
          inSequence {
            mockAuthWithCl200AndRetrievedNino(nino.value)
            mockGetSession(Future.successful(Right(Some(SessionData.empty))))
          }

          val result = performAction()
          checkIsRedirect(result, routes.SubscriptionController.checkYourDetails())
        }

      }

      "display the enter postcode page" when {

        "there is no address lookup result in session" in {
          inSequence {
            mockAuthWithCl200AndRetrievedNino(nino.value)
            mockGetSession(Future.successful(Right(Some(SessionData.empty.copy(businessPartnerRecord = Some(bpr))))))
          }

          contentAsString(performAction()) should include(message("address.postcode.title"))
        }

        "there is an address lookup result in session" in {
          val addressLookupResult = AddressLookupResult(postcode, List.empty)
          val sessionData = SessionData.empty.copy(businessPartnerRecord = Some(bpr), addressLookupResult = Some(addressLookupResult))

          inSequence {
            mockAuthWithCl200AndRetrievedNino(nino.value)
            mockGetSession(Future.successful(Right(Some(sessionData))))
          }

          val content = contentAsString(performAction())
          content should include(message("address.postcode.title"))
          content should include(s"""value="${postcode.value}"""")
        }

      }

    }

    "handling submitted postcodes" must {

        def performAction(formData: (String, String)*): Future[Result] =
          controller.enterPostcodeSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*).withCSRFToken)

      val existingSessionData = SessionData.empty.copy(businessPartnerRecord = Some(bpr))

      "redirect to check your details" when {

        "there is no session data" in {
          inSequence {
            mockAuthWithCl200AndRetrievedNino(nino.value)
            mockGetSession(Future.successful(Right(None)))
          }

          val result = performAction()
          checkIsRedirect(result, routes.SubscriptionController.checkYourDetails())
        }

        "there is no BPR in session" in {
          inSequence {
            mockAuthWithCl200AndRetrievedNino(nino.value)
            mockGetSession(Future.successful(Right(Some(SessionData.empty))))
          }

          val result = performAction()
          checkIsRedirect(result, routes.SubscriptionController.checkYourDetails())
        }

      }

      "show form errors when the postcode isn't valid" in {
        inSequence {
          mockAuthWithCl200AndRetrievedNino(nino.value)
          mockGetSession(Future.successful(Right(Some(existingSessionData))))
        }

        val result = performAction("postcode" -> "invalid.postcode")
        status(result) shouldBe BAD_REQUEST
        contentAsString(result) should include(message("postcode.invalid"))
      }

      "show an error page" when {

        "address lookup fails" in {
          inSequence {
            mockAuthWithCl200AndRetrievedNino(nino.value)
            mockGetSession(Future.successful(Right(Some(existingSessionData))))
            mockAddressLookup(postcode)(Future.successful(Left(Error("Uh oh!"))))
          }

          checkIsTechnicalErrorPage(performAction("postcode" -> postcode.value))
        }

        "the address lookup result cannot be stored in session" in {
          inSequence {
            mockAuthWithCl200AndRetrievedNino(nino.value)
            mockGetSession(Future.successful(Right(Some(existingSessionData))))
            mockAddressLookup(postcode)(Future.successful(Right(addressLookupResult)))
            mockStoreSession(existingSessionData.copy(addressLookupResult = Some(addressLookupResult)))(Future.successful(Left(Error("Uh oh!"))))
          }

          checkIsTechnicalErrorPage(performAction("postcode" -> postcode.value))
        }

      }

      "redirect to select address without doing an address lookup" when {

        "there is an address lookup result already in session for the same postcode" in {
          inSequence {
            mockAuthWithCl200AndRetrievedNino(nino.value)
            mockGetSession(Future.successful(Right(Some(existingSessionData.copy(addressLookupResult = Some(addressLookupResult))))))
          }

          val result = performAction("postcode" -> postcode.value)
          checkIsRedirect(result, routes.AddressController.selectAddress())
        }

      }

      "redirect to select address when the address lookup result has been stored in mongo" in {
        inSequence {
          mockAuthWithCl200AndRetrievedNino(nino.value)
          mockGetSession(Future.successful(Right(Some(existingSessionData))))
          mockAddressLookup(postcode)(Future.successful(Right(addressLookupResult)))
          mockStoreSession(existingSessionData.copy(addressLookupResult = Some(addressLookupResult)))(Future.successful(Right(())))
        }

        val result = performAction("postcode" -> postcode.value)
        checkIsRedirect(result, routes.AddressController.selectAddress())
      }

      "trim leading and trailing spaces in postcodes" in {
        inSequence {
          mockAuthWithCl200AndRetrievedNino(nino.value)
          mockGetSession(Future.successful(Right(Some(existingSessionData))))
          mockAddressLookup(postcode)(Future.successful(Right(addressLookupResult)))
          mockStoreSession(existingSessionData.copy(addressLookupResult = Some(addressLookupResult)))(Future.successful(Right(())))
        }

        val result = performAction("postcode" -> s"  ${postcode.value}  ")
        checkIsRedirect(result, routes.AddressController.selectAddress())
      }

    }

  }

  "handling requests to display the select address page" must {

      def performAction(): Future[Result] = controller.selectAddress()(FakeRequest())

    "redirect to the check your details page" when {

      "there is no data in session" in {
        inSequence {
          mockAuthWithCl200AndRetrievedNino(nino.value)
          mockGetSession(Future.successful(Right(None)))
        }

        val result = performAction()
        checkIsRedirect(result, routes.SubscriptionController.checkYourDetails())
      }

      "there is no BPR in session" in {
        inSequence {
          mockAuthWithCl200AndRetrievedNino(nino.value)
          mockGetSession(Future.successful(Right(Some(SessionData.empty))))
        }

        val result = performAction()
        checkIsRedirect(result, routes.SubscriptionController.checkYourDetails())
      }

      "there is not address lookup result in session" in {
        inSequence {
          mockAuthWithCl200AndRetrievedNino(nino.value)
          mockGetSession(Future.successful(Right(Some(SessionData.empty.copy(businessPartnerRecord = Some(bpr))))))
        }

        val result = performAction()
        checkIsRedirect(result, routes.SubscriptionController.checkYourDetails())
      }

    }

    "display the select address page" when {

      "there is an address lookup result in session" in {
        val session = SessionData.empty.copy(businessPartnerRecord = Some(bpr), addressLookupResult = Some(addressLookupResult))

        inSequence {
          mockAuthWithCl200AndRetrievedNino(nino.value)
          mockGetSession(Future.successful(Right(Some(session))))
        }

        val result = performAction()
        status(result) shouldBe OK
        contentAsString(result) should include(message("address-select.title"))
      }

      "there is an address lookup result in session and an address has already been selected in session" in {
        val session = SessionData.empty.copy(businessPartnerRecord = Some(bpr.copy(address = addressHead)), addressLookupResult = Some(addressLookupResult))

        inSequence {
          mockAuthWithCl200AndRetrievedNino(nino.value)
          mockGetSession(Future.successful(Right(Some(session))))
        }

        val result = performAction()
        status(result) shouldBe OK
        val content = contentAsString(result)
        content should include(message("address-select.title"))
        content should include("""checked="checked"""")
      }

    }

  }

  "handling submitted addresses" must {

      def performAction(formData: (String, String)*): Future[Result] =
        controller.selectAddressSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*).withCSRFToken)

    val session = SessionData.empty.copy(businessPartnerRecord = Some(bpr), addressLookupResult = Some(addressLookupResult))

    "redirect to the check your details page" when {

      "there is no session data" in {
        inSequence {
          mockAuthWithCl200AndRetrievedNino(nino.value)
          mockGetSession(Future.successful(Right(None)))
        }

        val result = performAction()
        checkIsRedirect(result, routes.SubscriptionController.checkYourDetails())
      }

      "there is no bpr is session" in {
        inSequence {
          mockAuthWithCl200AndRetrievedNino(nino.value)
          mockGetSession(Future.successful(Right(Some(SessionData.empty))))
        }

        val result = performAction()
        checkIsRedirect(result, routes.SubscriptionController.checkYourDetails())
      }

      "there is no address lookup result in session" in {
        inSequence {
          mockAuthWithCl200AndRetrievedNino(nino.value)
          mockGetSession(Future.successful(Right(Some(SessionData.empty.copy(businessPartnerRecord = Some(bpr))))))
        }

        val result = performAction()
        checkIsRedirect(result, routes.SubscriptionController.checkYourDetails())
      }

    }

    "show a form error" when {

      "the submitted index is not valid" in {
        List(
          "-1" -> "address-select.invalid",
          addressLookupResult.addresses.size.toString -> "address-select.invalid",
          "a" -> "address-select.error.number"
        ).foreach { case (submitted, errorKey) =>
            withClue(s"For submitted data '$submitted': ") {
              inSequence {
                mockAuthWithCl200AndRetrievedNino(nino.value)
                mockGetSession(Future.successful(Right(Some(session))))
              }

              val result = performAction("address-select" -> submitted)
              status(result) shouldBe BAD_REQUEST
              contentAsString(result) should include(message(errorKey))
            }
          }
      }

    }

    "show an error page" when {

      "the selected address cannot be stored in session" in {
        val updatedSession = session.copy(businessPartnerRecord = Some(bpr.copy(address = addressHead)))

        inSequence {
          mockAuthWithCl200AndRetrievedNino(nino.value)
          mockGetSession(Future.successful(Right(Some(session))))
          mockStoreSession(updatedSession)(Future.successful(Left(Error(""))))
        }

        checkIsTechnicalErrorPage(performAction("address-select" -> "0"))
      }

    }

    "redirect to the check your details page" when {

      "the selected address is stored in session" in {
        val updatedSession = session.copy(businessPartnerRecord = Some(bpr.copy(address = addressHead)))

        inSequence {
          mockAuthWithCl200AndRetrievedNino(nino.value)
          mockGetSession(Future.successful(Right(Some(session))))
          mockStoreSession(updatedSession)(Future.successful(Right(())))
        }

        val result = performAction("address-select" -> "0")
        checkIsRedirect(result, routes.SubscriptionController.checkYourDetails())
      }

    }

  }

}
