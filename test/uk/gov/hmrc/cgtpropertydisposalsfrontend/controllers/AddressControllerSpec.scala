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

import cats.data.EitherT
import cats.instances.future._
import org.scalacheck.ScalacheckShapeless._
import play.api.i18n.MessagesApi
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.CSRFTokenHelper._
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Address.{NonUkAddress, UkAddress}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Country
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{RegistrationStatus, SubscriptionStatus}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.SubscriptionStatus._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{AddressLookupResult, BusinessPartnerRecord, Error, NINO, Name, Postcode, SessionData, SubscriptionDetails, sample}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.UKAddressLookupService
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class AddressControllerSpec extends ControllerSpec with AuthSupport with SessionSupport {

  val mockService = mock[UKAddressLookupService]

  def mockAddressLookup(expectedPostcode: Postcode, filter: Option[String])(result: Either[Error, AddressLookupResult]) =
    (mockService
      .lookupAddress(_: Postcode, _: Option[String])(_: HeaderCarrier))
      .expects(expectedPostcode, filter, *)
      .returning(EitherT.fromEither[Future](result))

  override val overrideBindings: List[GuiceableModule] =
    List(
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore),
      bind[UKAddressLookupService].toInstance(mockService)
    )

  lazy val controller = instanceOf[AddressController]

  lazy implicit val messagesApi: MessagesApi = controller.messagesApi

  val name = Name("Name", "Surname")

  val postcode = Postcode("AB1 2CD")

  def address(i: Int) =
    UkAddress(s"$i the Street", Some("The Town"), None, None, postcode.value)

  val subscriptionDetails: SubscriptionDetails =
    sample[SubscriptionDetails].copy(address = address(1))

  val (addressHead, lastAddress, lastAddressIndex, addresses) = {
    val head = address(1)
    val last = address(5)
    (head, last, 4, head :: ((2 to 4).map(address).toList ::: List(last)))
  }

  val addressLookupResult = AddressLookupResult(postcode, None, addresses)

  def subscriptionDetailsBehavior(performAction: () => Future[Result]): Unit = {
    "redirect to the start endpoint" when {

      "there is no session data" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(Future.successful(Right(None)))
        }

        val result = performAction()
        checkIsRedirect(result, routes.StartController.start())
      }

      "there is no subscription details in session" in {
        val bpr = sample[BusinessPartnerRecord]
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            Future.successful(
              Right(Some(SessionData.empty.copy(journeyStatus = Some(SubscriptionMissingData(bpr, Right(name))))))))
        }

        val result = performAction()
        checkIsRedirect(result, routes.StartController.start())
      }

    }

    "redirect to the do you have a nino page" when {

      "the session data indicates the user does not have sufficient confidence level" in {
        val session = SessionData.empty.copy(journeyStatus = Some(
          SubscriptionStatus.IndividualWithInsufficientConfidenceLevel(None, None, name, None)
        ))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(Future.successful(Right(Some(session))))
        }

        val result = performAction()
        checkIsRedirect(result, routes.InsufficientConfidenceLevelController.doYouHaveNINO())
      }

    }

    "redirect to the register your trust page" when {

      "the session data indicates the user is an organisation without a registered trust associated with it" in {
        val session = SessionData.empty.copy(journeyStatus = Some(SubscriptionStatus.UnregisteredTrust))
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(Future.successful(Right(Some(session))))
        }

        val result = performAction()
        checkIsRedirect(result, routes.RegisterTrustController.registerYourTrust())
      }

    }

    "redirect to the registration start endpoint" when {

      "the session data indicates the user has started a registration journey" in {
        List(
          RegistrationStatus.IndividualWantsToRegisterTrust,
          sample[RegistrationStatus.IndividualSupplyingInformation]
        ).foreach{ registrationStatus =>
          withClue(s"For registration status $registrationStatus: "){
            val session = SessionData.empty.copy(journeyStatus = Some(registrationStatus))

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(Future.successful(Right(Some(session))))
            }

            checkIsRedirect(performAction(), routes.RegistrationController.startRegistration())
          }

        }

      }

    }

  }

  "AddressController" when {

    "handling requests to display the is UK page" must {
      def performAction() = controller.isUk()(FakeRequest())

      behave like subscriptionDetailsBehavior(performAction)

      "show the page" when {
        "the address lookup results have been cleared from session" in {
          val session = SessionData.empty.copy(
            journeyStatus = Some(SubscriptionReady(subscriptionDetails)),
            addressLookupResult = Some(addressLookupResult)
          )
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(session))))
            mockStoreSession(session.copy(
              addressLookupResult = None
            ))(Future(Right(())))
          }

          val result = performAction()
          status(result) shouldBe OK
          contentAsString(result) should include(message("subscription.isUk.title"))
        }
        "there are no address lookup results to clear from session" in {
          val session = SessionData.empty.copy(
            journeyStatus = Some(SubscriptionReady(subscriptionDetails))
          )
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(session))))
          }

          val result = performAction()
          status(result) shouldBe OK
          contentAsString(result) should include (message("subscription.isUk.title"))
        }
      }
      "display an error page" when {
        "there is an error when clearing address lookup results in session" in {
          val session = SessionData.empty.copy(
            journeyStatus = Some(SubscriptionReady(subscriptionDetails)),
            addressLookupResult = Some(addressLookupResult)
          )
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(session))))
            mockStoreSession(session.copy(
              addressLookupResult = None
            ))(Future(Left(Error(""))))
          }

          checkIsTechnicalErrorPage(performAction())

        }
      }
    }

    "handling requests to submit the is UK page" must {
      def performAction(formData: (String, String)*): Future[Result] =
        controller.isUkSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*).withCSRFToken)
      val existingSessionData =
        SessionData.empty.copy(journeyStatus = Some(SubscriptionReady(subscriptionDetails)))

      behave like subscriptionDetailsBehavior(() => performAction())

      "return a form error" when {
        "no selection has been made" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(existingSessionData))))
          }
          val result = performAction()
          status(result) shouldBe BAD_REQUEST
          contentAsString(result) should include (message("isUk.error.required"))
        }
      }

      "redirect to the enter postcode page" when {
        "Uk address has been selected" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(existingSessionData))))
          }
          val result = performAction("isUk" -> "true")
          checkIsRedirect(result, routes.AddressController.enterPostcode())
        }
      }

      "redirect to the enter non UK address page" when {
        "Non Uk address has been selected" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(existingSessionData))))
          }
          val result = performAction("isUk" -> "false")
          checkIsRedirect(result, routes.AddressController.enterNonUkAddress())
        }
      }
    }

    "handling requests to display the enter UK address page" must {

      def performAction() = controller.enterUkAddress()(FakeRequest())

      behave like subscriptionDetailsBehavior(performAction)

      "display the enter UK address page" when {
        "the endpoint is requested" in {
          val session = SessionData.empty.copy(
            journeyStatus = Some(SubscriptionReady(subscriptionDetails)),
            addressLookupResult = Some(addressLookupResult)
          )
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(session))))
          }

          val result = performAction()
          status(result) shouldBe OK
          contentAsString(result) should include (message("address.uk.title"))
        }
      }

    }

    "handling submitted addresses from enter UK address page" must {

      def performAction(formData: (String, String)*): Future[Result] =
        controller.enterUkAddressSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*).withCSRFToken)

      val existingSessionData =
        SessionData.empty.copy(journeyStatus = Some(SubscriptionReady(subscriptionDetails)))

      behave like subscriptionDetailsBehavior(() => performAction())

      "return a form error" when {
        "address line 1 is empty" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(existingSessionData))))
          }
          val result = performAction("postcode" -> "W1A2HV")
          status(result) shouldBe BAD_REQUEST
          contentAsString(result) should include (message("address-line1.error.required"))
        }
        "address postcode is empty" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(existingSessionData))))
          }
          val result = performAction("address-line1" -> "Some street")
          status(result) shouldBe BAD_REQUEST
          contentAsString(result) should include (message("postcode.error.required"))
        }
      }
      "display an error page" when {
        "the address cannot be stored in the session" in {
          val session = SessionData.empty.copy(
            journeyStatus = Some(SubscriptionReady(subscriptionDetails))
          )
          val updatedSession = SessionData.empty.copy(
            journeyStatus = Some(SubscriptionReady(subscriptionDetails.copy(
              address = UkAddress("Test street", None, None, None, "W1A2HR")
            )))
          )
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(session))))
            mockStoreSession(updatedSession)(Future(Left(Error(""))))
          }
          checkIsTechnicalErrorPage(performAction("address-line1" -> "Test street", "postcode" -> "W1A2HR"))
        }
      }
      "redirect to check your details page" when {
        "address has been stored in session" in {
          val session = SessionData.empty.copy(
            journeyStatus = Some(SubscriptionReady(subscriptionDetails))
          )
          val updatedSession = SessionData.empty.copy(
            journeyStatus = Some(SubscriptionReady(subscriptionDetails.copy(
              address = UkAddress("Flat 1", Some("The Street"), Some("The Town"), Some("Countyshire"), "W1A2HR")
            )))
          )
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(session))))
            mockStoreSession(updatedSession)(Future.successful(Right(())))
          }
          val result = performAction(
            "address-line1" -> "Flat 1",
            "address-line2" -> "The Street",
            "address-town" -> "The Town",
            "address-county" -> "Countyshire",
            "postcode" -> "W1A2HR"
          )
          checkIsRedirect(result, routes.SubscriptionController.checkYourDetails())
        }
      }
    }

    "handling requests to display the enter non UK address page" must {

      def performAction() = controller.enterNonUkAddress()(FakeRequest())

      behave like subscriptionDetailsBehavior(performAction)

      "display the enter non UK address page" when {
        "the endpoint is requested" in {
          val session = SessionData.empty.copy(
            journeyStatus = Some(SubscriptionReady(subscriptionDetails)),
            addressLookupResult = None
          )
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(session))))
          }

          val result = performAction()
          status(result) shouldBe OK
          contentAsString(result) should include (message("nonUkAddress.title"))
        }
      }

    }

    "handling requests to submit the enter non UK address page" must {
      def performAction(formData: (String, String)*): Future[Result] =
        controller.enterNonUkAddressSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*).withCSRFToken)

      val existingSessionData =
        SessionData.empty.copy(journeyStatus = Some(SubscriptionReady(subscriptionDetails)))

      behave like subscriptionDetailsBehavior(() => performAction())

      "return a form error" when {
        "address line 1 is empty" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(existingSessionData))))
          }
          val result = performAction("countryCode" -> "NZ")
          status(result) shouldBe BAD_REQUEST
          contentAsString(result) should include(message("nonUkAddress-line1.error.required"))
        }
        "countryCode is empty" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(existingSessionData))))
          }
          val result = performAction("nonUkAddress-line1" -> "10 Some Street")
          status(result) shouldBe BAD_REQUEST
          contentAsString(result) should include(message("countryCode.error.required"))
        }
        "countryCode is invalid" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(existingSessionData))))
          }
          val result = performAction("countryCode" -> "XX")
          status(result) shouldBe BAD_REQUEST
          contentAsString(result) should include(message("countryCode.error.notFound"))
        }
      }

      "redirect to check your details page" when {
        "address has been stored in session" in {
          val session = SessionData.empty.copy(
            journeyStatus = Some(SubscriptionReady(subscriptionDetails))
          )
          val updatedSession = SessionData.empty.copy(
            journeyStatus = Some(SubscriptionReady(subscriptionDetails.copy(
              address = NonUkAddress(
                "Flat 1",
                Some("The Street"),
                Some("The Town"),
                None,
                None,
                Country("NZ", Some("New Zealand"))
              )
            ))
            ))
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(session))))
            mockStoreSession(updatedSession)(Future.successful(Right(())))
          }
          val result = performAction(
            "nonUkAddress-line1" -> "Flat 1",
            "nonUkAddress-line2" -> "The Street",
            "nonUkAddress-line3" -> "The Town",
            "countryCode" -> "NZ"
          )
          checkIsRedirect(result, routes.SubscriptionController.checkYourDetails())
        }
      }
    }

    "handling requests to display the enter postcode page" must {

      def performAction() = controller.enterPostcode()(FakeRequest())

      behave like subscriptionDetailsBehavior(performAction)

      "display the enter postcode page" when {

        "there is no address lookup result in session" in {
          val session = SessionData.empty.copy(journeyStatus = Some(SubscriptionReady(subscriptionDetails)))
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(session))))
          }

          contentAsString(performAction()) should include(message("subscription.postcode.title"))
        }

        "there is an address lookup result in session" in {
          val addressLookupResult = AddressLookupResult(postcode, None, List.empty)
          val session = SessionData.empty.copy(
            journeyStatus  = Some(SubscriptionReady(subscriptionDetails)),
            addressLookupResult = Some(addressLookupResult)
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(session))))
          }

          val content = contentAsString(performAction())
          content should include(message("subscription.postcode.title"))
          content should include(s"""value="${postcode.value}"""")
        }

      }

    }

    "handling submitted postcodes and filters" must {

      def performAction(formData: (String, String)*): Future[Result] =
        controller.enterPostcodeSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*).withCSRFToken)

      val existingSessionData =
        SessionData.empty.copy(journeyStatus = Some(SubscriptionReady(subscriptionDetails)))

      behave like subscriptionDetailsBehavior(() => performAction())

      "show form errors when no results are found for a postcode" in {
        val p = Postcode("NW19AX")
        val addressLookupResult = AddressLookupResult(p, None, List())
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(Future.successful(Right(Some(existingSessionData))))
          mockAddressLookup(p, None)(Right(addressLookupResult))
          mockStoreSession(existingSessionData.copy(addressLookupResult = Some(addressLookupResult)))(
            Future.successful(Right(()))
          )
        }
        val result = performAction("postcode" -> p.value)
        status(result)          shouldBe BAD_REQUEST
        contentAsString(result) should include(message("postcode.error.noResults"))
      }

      "show form errors when no results are found for a filter" in {
        val p = Postcode("NW19AX")
        val filter = "Some filter"
        val addressLookupResult = AddressLookupResult(p, Some(filter), List())
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(Future.successful(Right(Some(existingSessionData))))
          mockAddressLookup(p, Some(filter))(Right(addressLookupResult))
          mockStoreSession(existingSessionData.copy(addressLookupResult = Some(addressLookupResult)))(
            Future.successful(Right(()))
          )
        }
        val result = performAction("postcode" -> p.value, "filter" -> filter)
        status(result)          shouldBe BAD_REQUEST
        contentAsString(result) should include(message("filter.error.noResults"))
      }

      "show form errors when no results are found for a postcode within a stored search" in {
        val p = Postcode("NW19AX")
        val addressLookupResult = AddressLookupResult(p, None, List())
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(Future.successful(Right(Some(existingSessionData.copy(addressLookupResult = Some(addressLookupResult))))))
        }
        val result = performAction("postcode" -> p.value)
        status(result)          shouldBe BAD_REQUEST
        contentAsString(result) should include(message("postcode.error.noResults"))
      }

      "show form errors when no results are found for a filter within a stored search" in {
        val p = Postcode("NW19AX")
        val filter = "Some filter"
        val addressLookupResult = AddressLookupResult(p, Some(filter), List())
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(Future.successful(
            Right(Some(existingSessionData.copy(addressLookupResult = Some(addressLookupResult))))
          ))
        }
        val result = performAction("postcode" -> p.value, "filter" -> filter)
        status(result)          shouldBe BAD_REQUEST
        contentAsString(result) should include(message("filter.error.noResults"))
      }

      "show form errors when the postcode isn't valid" in {
        List(
          "A00A",
          "AA0A0AAA",
          "AA0.0AA",
          "AAA123",
          "BFPO123456",
          "A11AAA"
        ).foreach { invalidPostcode =>
          withClue(s"For postcode '$invalidPostcode'") {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(Future.successful(Right(Some(existingSessionData))))
            }

            val result = performAction("postcode" -> invalidPostcode)
            status(result)          shouldBe BAD_REQUEST
            contentAsString(result) should include(message("postcode.invalid"))
          }
        }

      }

      "show an error page" when {

        "address lookup fails" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(existingSessionData))))
            mockAddressLookup(postcode, None)(Left(Error("Uh oh!")))
          }

          checkIsTechnicalErrorPage(performAction("postcode" -> postcode.value))
        }

        "the address lookup result cannot be stored in session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(existingSessionData))))
            mockAddressLookup(postcode, None)(Right(addressLookupResult))
            mockStoreSession(existingSessionData.copy(addressLookupResult = Some(addressLookupResult)))(
              Future.successful(Left(Error("Uh oh!")))
            )
          }

          checkIsTechnicalErrorPage(performAction("postcode" -> postcode.value))
        }

      }

      "redirect to select address without doing an address lookup" when {

        "there is an address lookup result already in session for the same postcode" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              Future.successful(Right(Some(existingSessionData.copy(addressLookupResult = Some(addressLookupResult)))))
            )
          }

          val result = performAction("postcode" -> postcode.value)
          checkIsRedirect(result, routes.AddressController.selectAddress())
        }

      }

      "redirect to select address when the address lookup result has been stored in mongo " +
        "and the postcode is valid" in {
        List(
          "AA9A 9AA",
          "A9A 9AA",
          "A9 9AA",
          "A99 9AA",
          "AA9 9AA",
          "AA99 9AA",
          "  aA 99 9A a ",
          "BFPO1",
          "BFPO12",
          "BF PO12 3",
          " BfpO1234 ",
          "BFPO  12345"
        ).foreach { postcode =>
          withClue(s"For postcode '$postcode': ") {
            val formattedPostcode = Postcode(postcode.trim)
            val addressLookupResult =
              AddressLookupResult(formattedPostcode, None, List(sample[UkAddress]))

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(Future.successful(Right(Some(existingSessionData))))
              mockAddressLookup(formattedPostcode, None)(Right(addressLookupResult))
              mockStoreSession(existingSessionData.copy(addressLookupResult = Some(addressLookupResult)))(
                Future.successful(Right(()))
              )
            }

            val result = performAction("postcode" -> postcode)
            checkIsRedirect(result, routes.AddressController.selectAddress())
          }

        }

      }

      "trim leading and trailing spaces in postcodes" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(Future.successful(Right(Some(existingSessionData))))
          mockAddressLookup(postcode, None)(Right(addressLookupResult))
          mockStoreSession(existingSessionData.copy(addressLookupResult = Some(addressLookupResult)))(
            Future.successful(Right(()))
          )
        }

        val result = performAction("postcode" -> s"  ${postcode.value}  ")
        checkIsRedirect(result, routes.AddressController.selectAddress())
      }

      "be ok with empty filter field and redirect to select address page" in {
        val filter = ""
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(Future.successful(Right(Some(existingSessionData))))
          mockAddressLookup(postcode, None)(Right(addressLookupResult))
          mockStoreSession(existingSessionData.copy(addressLookupResult = Some(addressLookupResult)))(
            Future.successful(Right(()))
          )
        }
        val result = performAction("filter" -> filter, "postcode" -> postcode.value)
        checkIsRedirect(result, routes.AddressController.selectAddress())
      }

      "redirect to select address page when filter is submitted" in {
        val filter = "1"
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(Future.successful(Right(Some(existingSessionData))))
          mockAddressLookup(postcode, Some(filter))(Right(addressLookupResult))
          mockStoreSession(existingSessionData.copy(addressLookupResult = Some(addressLookupResult)))(
            Future.successful(Right(()))
          )
        }
        val result = performAction("filter" -> filter, "postcode" -> postcode.value)
        checkIsRedirect(result, routes.AddressController.selectAddress())
      }

    }

  }

  "handling requests to display the select address page" must {

    def performAction(): Future[Result] =
      controller.selectAddress()(FakeRequest())

    behave like subscriptionDetailsBehavior(performAction)

    "redirect to the check your details page" when {

      "there is not address lookup result in session" in {
        val session = SessionData.empty.copy(journeyStatus = Some(SubscriptionReady(subscriptionDetails)))
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(Future.successful(Right(Some(session))))
        }

        val result = performAction()
        checkIsRedirect(result, routes.SubscriptionController.checkYourDetails())
      }

    }

    "display the select address page" when {

      "there is an address lookup result in session" in {
        val session = SessionData.empty.copy(
          journeyStatus  = Some(SubscriptionReady(subscriptionDetails)),
          addressLookupResult = Some(addressLookupResult)
        )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(Future.successful(Right(Some(session))))
        }

        val result = performAction()
        status(result)          shouldBe OK
        contentAsString(result) should include(message("address-select.title"))
      }

    }

  }

  "handling submitted addresses" must {

    def performAction(formData: (String, String)*): Future[Result] =
      controller.selectAddressSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*).withCSRFToken)

    val existingSubscriptionReady = SubscriptionReady(subscriptionDetails)

    val existingSession = SessionData.empty.copy(
      journeyStatus  = Some(existingSubscriptionReady),
      addressLookupResult = Some(addressLookupResult)
    )

    behave like subscriptionDetailsBehavior(() => performAction())

    "redirect to the check your details page" when {

      "there is no address lookup result in session" in {
        val session = SessionData.empty.copy(journeyStatus = Some(SubscriptionReady(subscriptionDetails)))
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(Future.successful(Right(Some(session))))
        }

        val result = performAction()
        checkIsRedirect(result, routes.SubscriptionController.checkYourDetails())
      }

    }

    "show a form error" when {

      "the submitted index is not valid" in {
        List(
          "-1"                                        -> "address-select.invalid",
          addressLookupResult.addresses.size.toString -> "address-select.invalid",
          "a"                                         -> "address-select.error.number"
        ).foreach {
          case (submitted, errorKey) =>
            withClue(s"For submitted data '$submitted': ") {
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(Future.successful(Right(Some(existingSession))))
              }

              val result = performAction("address-select" -> submitted)
              status(result)          shouldBe BAD_REQUEST
              contentAsString(result) should include(message(errorKey))
            }
        }
      }

    }

    "show an error page" when {

      "the selected address cannot be stored in session" in {
        val updatedSession = existingSession.copy(
          journeyStatus =
            Some(existingSubscriptionReady.copy(subscriptionDetails = subscriptionDetails.copy(address = lastAddress)))
        )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(Future.successful(Right(Some(existingSession))))
          mockStoreSession(updatedSession)(Future.successful(Left(Error(""))))
        }

        checkIsTechnicalErrorPage(performAction("address-select" -> s"${lastAddressIndex}"))
      }

    }

    "redirect to the check your details page" when {

      "the selected address is stored in session" in {
        val updatedSession = existingSession.copy(
          journeyStatus =
            Some(existingSubscriptionReady.copy(subscriptionDetails = subscriptionDetails.copy(address = lastAddress)))
        )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(Future.successful(Right(Some(existingSession))))
          mockStoreSession(updatedSession)(Future.successful(Right(())))
        }

        val result = performAction("address-select" -> s"$lastAddressIndex")
        checkIsRedirect(result, routes.SubscriptionController.checkYourDetails())
      }

    }

    "not update the session" when {

      "the user selects an address which is already in their subscription details" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(Future.successful(Right(Some(existingSession))))
        }

        val result = performAction("address-select" -> "0")
        checkIsRedirect(result, routes.SubscriptionController.checkYourDetails())
      }


    }




  }

}
