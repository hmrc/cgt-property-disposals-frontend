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
import play.api.mvc.{Call, Result}
import play.api.test.CSRFTokenHelper._
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.BusinessPartnerRecordRequest.IndividualBusinessPartnerRecordRequest
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{RegistrationStatus, SubscriptionStatus}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.SubscriptionStatus._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{BusinessPartnerRecord, BusinessPartnerRecordRequest, Error, NINO, Name, SAUTR, SessionData, SubscriptionDetails, SubscriptionResponse, sample}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.BusinessPartnerRecordService
import uk.gov.hmrc.domain.SaUtrGenerator
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class InsufficientConfidenceLevelControllerSpec
  extends ControllerSpec with IvBehaviourSupport with SessionSupport with AuthSupport with NameFormValidationTests {

  val mockBprService = mock[BusinessPartnerRecordService]

  override val overrideBindings =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore),
      bind[BusinessPartnerRecordService].toInstance(mockBprService)
    )

  override lazy val additionalConfig = ivConfig(useRelativeUrls = false)

  lazy val controller = instanceOf[InsufficientConfidenceLevelController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  def mockGetBusinessPartnerRecord(
        sautr: SAUTR,
        nameMatch: Name)(result: Either[Error,BusinessPartnerRecord]): Unit =
    (mockBprService.getBusinessPartnerRecord(_: BusinessPartnerRecordRequest)(_: HeaderCarrier))
    .expects(IndividualBusinessPartnerRecordRequest(Left(sautr), Some(nameMatch)), *)
    .returning(EitherT.fromEither[Future](result))

  def session(subscriptionStatus: SubscriptionStatus) =
    SessionData.empty.copy(journeyStatus = Some(subscriptionStatus))

  val name = Name("name", "surname")

  def commonBehaviour(performAction: () => Future[Result]) = {
    val bpr = sample[BusinessPartnerRecord]
    val subscriptionDetails = sample[SubscriptionDetails]
    val subscriptionResponse = sample[SubscriptionResponse]

    def test(sessionData: Option[SessionData], expectedRedirectTo: Call): Unit = {
      inSequence{
        mockAuthWithNoRetrievals()
        mockGetSession(Future.successful(Right(sessionData)))
      }

      checkIsRedirect(performAction(), expectedRedirectTo)
    }

    "redirect to the start endpoint" when {

      "there is data missing for subscription" in {
        test(
          Some(session(SubscriptionMissingData(bpr))),
          routes.StartController.start()
        )
      }

      "there is no session data" in {
        test(
         None,
          routes.StartController.start()
        )
      }
    }

    "redirect to check your details" when {

      "the session data indicates that subscription is ready" in {
        test(
          Some(session(SubscriptionReady(subscriptionDetails))),
          routes.SubscriptionController.checkYourDetails()
        )
      }

    }

    "redirect to the subscription confirmation page" when {

      "the session data indicates that the user has successfully subscribed" in {
        test(
          Some(session(SubscriptionComplete(subscriptionDetails, subscriptionResponse))),
          routes.SubscriptionController.subscribed()
        )
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

  "InsufficientConfidenceLevelController" when {

    "handling requests to ask the user if they have a NINO" must {

      def performAction(): Future[Result] =
        controller.doYouHaveNINO()(FakeRequest())

      behave like commonBehaviour(performAction)

      "display the do you have a NINO page" when {

        "the session indicates that the user does not have sufficient confidence level" in {
          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(session(IndividualWithInsufficientConfidenceLevel(None, None, None))))))
          }

          val result = performAction()
          status(result) shouldBe OK
          contentAsString(result) should include(message("haveANino.title"))

        }

        "the session indicates that the user does not have sufficient confidence level and the user " +
          "has previously answered this question" in {
          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(session(IndividualWithInsufficientConfidenceLevel(Some(true), None,  None))))))
          }

          val result = performAction()
          status(result) shouldBe OK
          val content = contentAsString(result)
          content should include(message("haveANino.title"))
          content should include("checked=\"checked\"")
        }

      }

    }

    "handling submitted answers to the have a NINO page" must {

      def performAction(formData: (String,String)*): Future[Result] =
        controller.doYouHaveNINOSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*).withCSRFToken)

      behave like commonBehaviour(() => performAction())

      "show a form error" when {

        "no data has been submitted" in {
          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(session(IndividualWithInsufficientConfidenceLevel(None, None, None))))))
          }

          val result = performAction()
          status(result) shouldBe BAD_REQUEST
          contentAsString(result) should include(message("hasNino.error.required"))
        }

        "the submitted data cannot be parsed" in {
          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(session(IndividualWithInsufficientConfidenceLevel(None, None, None))))))
          }

          val result = performAction("hasNino" -> "blah")
          status(result) shouldBe BAD_REQUEST
          contentAsString(result) should include(message("hasNino.error.boolean"))
        }

      }

      "redirect to the IV journey" when {

        "the user indicates that they do have a NINO" in {
          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(session(IndividualWithInsufficientConfidenceLevel(None, None, None))))))
            mockStoreSession(session(IndividualWithInsufficientConfidenceLevel(Some(true), None, None)))(Future.successful(Right(())))
          }

          val result = performAction("hasNino" -> "true")
          checkIsRedirectToIv(result, false)
        }

        "the user indicates that they do have a NINO and the application " +
          "has been configured to used absolute urls to iv" in new ControllerSpec {

          override val overrideBindings =
            List[GuiceableModule](
              bind[AuthConnector].toInstance(mockAuthConnector),
              bind[SessionStore].toInstance(mockSessionStore)
            )

          override lazy val additionalConfig = ivConfig(useRelativeUrls = true)

          lazy val controller = instanceOf[InsufficientConfidenceLevelController]

          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(session(IndividualWithInsufficientConfidenceLevel(None, None, None))))))
            mockStoreSession(session(IndividualWithInsufficientConfidenceLevel(Some(true), None, None)))(Future.successful(Right(())))
          }

          val result = controller.doYouHaveNINOSubmit()(FakeRequest().withFormUrlEncodedBody("hasNino" -> "true").withCSRFToken)
          checkIsRedirectToIv(result, true)
        }


      }

      "redirect to ask if the user has an SAUTR" when {

        "the user indicates that they do not have a NINO" in {
          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(session(IndividualWithInsufficientConfidenceLevel(None, None, None))))))
            mockStoreSession(session(IndividualWithInsufficientConfidenceLevel(Some(false), None, None)))(Future.successful(Right(())))
          }

          val result = performAction("hasNino" -> "false")
          checkIsRedirect(result, routes.InsufficientConfidenceLevelController.doYouHaveAnSaUtr())
        }

      }

      "not update the session" when {

        "the user has previously indicated that they have a NINO" in {
          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(session(IndividualWithInsufficientConfidenceLevel(Some(true), None, None))))))
          }

          val result = performAction("hasNino" -> "true")
          checkIsRedirectToIv(result, false)
        }

        "the user has previously indicated that they do not have a NINO" in {
          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(session(IndividualWithInsufficientConfidenceLevel(Some(false), None, None))))))
          }

          val result = performAction("hasNino" -> "false")
          checkIsRedirect(result, routes.InsufficientConfidenceLevelController.doYouHaveAnSaUtr())
        }
      }


    }

    "handling requests to ask the user if they have an SAUTR" must {

      def performAction(): Future[Result] =
        controller.doYouHaveAnSaUtr()(FakeRequest())

      behave like commonBehaviour(performAction)

      "redirect to the NINO page" when {

        "the session data indicates the user hasn't selected whether or not they have a NINO" in {
          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(session(IndividualWithInsufficientConfidenceLevel(None, None, None))))))
          }

          checkIsRedirect(performAction(), routes.InsufficientConfidenceLevelController.doYouHaveNINO())
        }

      }

      "display the page" when {

        "the session indicates that the user does not have sufficient confidence level and the user " +
          "has indicated that they do not have a NINO" in {
          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(session(IndividualWithInsufficientConfidenceLevel(Some(false), None, None))))))
          }

          val result = performAction()
          status(result) shouldBe OK
          contentAsString(result) should include(message("haveAnSaUtr.title"))

        }

        "the session indicates that the user does not have sufficient confidence level and the user " +
          "has indicated that they do not have a NINO and the user has previously indicated that they " +
          "have an SAUTR" in {
          val sautr = SAUTR("12345")
          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(session(IndividualWithInsufficientConfidenceLevel(Some(false), Some(true), None))))))
          }

          val result = performAction()
          status(result) shouldBe OK
          val content = contentAsString(result)
          content should include(message("haveAnSaUtr.title"))
          content should include("checked=\"checked\"")
        }

        "the session indicates that the user does not have sufficient confidence level and the user " +
          "has indicated that they do not have a NINO and the user has previously indicated that they " +
          "do not have an SAUTR" in {
          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(session(IndividualWithInsufficientConfidenceLevel(Some(false), Some(false), None))))))
          }

          val result = performAction()
          status(result) shouldBe OK
          val content = contentAsString(result)
          content should include(message("haveAnSaUtr.title"))
          content should include("checked=\"checked\"")
        }

      }

    }

    "handling submitted answers to the have an SAUTR page" must {

      def performAction(formData: (String,String)*): Future[Result] =
        controller.doYouHaveSaUtrSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*).withCSRFToken)

      behave like commonBehaviour(() => performAction())

      "redirect to the NINO page" when {

        "the session data indicates the user hasn't selected whether or not they have a NINO" in {
          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(session(IndividualWithInsufficientConfidenceLevel(None, None, None))))))
          }

          checkIsRedirect(performAction(), routes.InsufficientConfidenceLevelController.doYouHaveNINO())
        }

      }

      "show a form error" when {

        "the user did not select an option" in {
          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(session(IndividualWithInsufficientConfidenceLevel(Some(false), None, None))))))
          }

          val result = performAction()
          status(result) shouldBe BAD_REQUEST
          contentAsString(result) should include(message("hasSaUtr.error.required"))
        }

      }

      "redirect to the enter sautr and name endpoint" when {

        "the user indicates they have an SA UTR and enters in a valid one" in {
          val subscriptionStatus =
            IndividualWithInsufficientConfidenceLevel(Some(false), None, None)

          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(session(subscriptionStatus)))))
            mockStoreSession(session(subscriptionStatus.copy(hasSautr = Some(true))))(Future.successful(Right(())))
          }

          val result = performAction("hasSaUtr" -> "true")
          checkIsRedirect(result, routes.InsufficientConfidenceLevelController.enterSautrAndName())
        }

      }

      "redirect to the registration start journey" when {

        "the user indicates they do not have an SA UTR" in {
          val subscriptionStatus =
            IndividualWithInsufficientConfidenceLevel(Some(false), None, None)

          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(session(subscriptionStatus)))))
            mockStoreSession(session(subscriptionStatus.copy(hasSautr = Some(false))))(Future.successful(Right(())))
          }

          val result = performAction("hasSaUtr" -> "false")
          checkIsRedirect(result, routes.RegistrationController.startRegistration())
        }

      }


    }

    "handling requests to enter an SAUTR and name" must {

      def performAction(): Future[Result] =
        controller.enterSautrAndName()(FakeRequest())

      behave like commonBehaviour(performAction)

      "redirect to the select entity type page" when {

        def test(hasNino: Option[Boolean], hasSautr: Option[Boolean]) = {
          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(session(
              IndividualWithInsufficientConfidenceLevel(hasNino, hasSautr, None)
            )))))
          }

          val result = performAction()
          checkIsRedirect(result, routes.InsufficientConfidenceLevelController.doYouHaveNINO())
        }

        "the user has not answered the do you have a NINO question" in {
          test(None, None)
        }

        "the user has not answered the do you have an SAUTR question" in {
          test(Some(false), None)
        }

        "the user has answered that they have a NINO" in {
          test(Some(true), None)
        }

        "the user has answered that they do not have an SAUTR" in {
          test(Some(false), Some(false))
        }

      }

      "display the enter SA UTR and name page" when {

        "the user has indicated that they have no NINO but they do have an SA UTR" in {
          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(session(
              IndividualWithInsufficientConfidenceLevel(Some(false), Some(true), None)
            )))))
          }

          val result = performAction()
          contentAsString(result) should include(message("enterSaUtr.title"))
        }

      }

    }

    "handling sumitted SAUTRs and names" must {

      val validSautr = SAUTR((new SaUtrGenerator).nextSaUtr.utr)

      val validName = Name("Elaine", "Belaine")

      val bpr = sample[BusinessPartnerRecord]

      val expectedSessionData =
        session(IndividualWithInsufficientConfidenceLevel(Some(false), Some(true), None))


      def performAction(formData: (String,String)*): Future[Result] =
        controller.enterSautrAndNameSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*).withCSRFToken)

      behave like commonBehaviour(() => performAction())

      behave like nameFormValidationTests(
        data => performAction(data :+ ("saUtr" -> validSautr.value): _*),
        () => inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(Future.successful(Right(Some(session(
            IndividualWithInsufficientConfidenceLevel(Some(false), Some(true), None)
          )))))
        }
      )

      "redirect to the select entity type page" when {

        def test(hasNino: Option[Boolean], hasSautr: Option[Boolean]) = {
          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(session(
              IndividualWithInsufficientConfidenceLevel(hasNino, hasSautr, None)
            )))))
          }

          val result = performAction()
          checkIsRedirect(result, routes.InsufficientConfidenceLevelController.doYouHaveNINO())
        }

        "the user has not answered the do you have a NINO question" in {
          test(None, None)
        }

        "the user has not answered the do you have an SAUTR question" in {
          test(Some(false), None)
        }

        "the user has answered that they have a NINO" in {
          test(Some(true), None)
        }

        "the user has answered that they do not have an SAUTR" in {
          test(Some(false), Some(false))
        }

      }

      "show a form error" when {


        "an SA UTR is not submitted" in {
          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(expectedSessionData))))
          }

          val result = performAction("firstName" -> validName.firstName, "lastName" -> validName.lastName)
          status(result) shouldBe BAD_REQUEST
          contentAsString(result) should include(message("saUtr.error.required"))
        }

        "the SA UTR is an empty string" in {
          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(expectedSessionData))))
          }

          val result = performAction(
            "firstName" -> validName.firstName,
            "lastName" -> validName.lastName,
            "saUtr" -> ""
          )
          status(result) shouldBe BAD_REQUEST
          contentAsString(result) should include(message("saUtr.error.required"))
        }

        "the SA UTR is not valid" in {
          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(expectedSessionData))))
          }

          val result = performAction(
            "firstName" -> validName.firstName,
            "lastName" -> validName.lastName,
            "saUtr" -> "not-an-sa-utr"
          )
          status(result) shouldBe BAD_REQUEST
          contentAsString(result) should include(message("saUtr.error.pattern"))
        }

      }

      "display an error page" when {

        "there is an error getting the BPR" in {
          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(expectedSessionData))))
            mockGetBusinessPartnerRecord(validSautr, validName)(Left(Error("")))
          }

          val result = performAction(
            "firstName" -> validName.firstName,
            "lastName" -> validName.lastName,
            "saUtr" -> validSautr.value
          )

          checkIsTechnicalErrorPage(result)
        }

        "there is an error storing a retrieved BPR in mongo" in {
          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(expectedSessionData))))
            mockGetBusinessPartnerRecord(validSautr, validName)(Right(bpr))
            mockStoreSession(session(SubscriptionMissingData(bpr)))(Future.successful(Left(Error(""))))
          }

          val result = performAction(
            "firstName" -> validName.firstName,
            "lastName" -> validName.lastName,
            "saUtr" -> validSautr.value
          )

          checkIsTechnicalErrorPage(result)
        }

      }

      "update the session and redirect to the start endpoint" when {

        "the user submits a valid SA UTR and name and a BPR can be retrieved" in {
          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(expectedSessionData))))
            mockGetBusinessPartnerRecord(validSautr, validName)(Right(bpr))
            mockStoreSession(session(SubscriptionMissingData(bpr)))(Future.successful(Right(())))
          }

          val result = performAction(
            "firstName" -> validName.firstName,
            "lastName" -> validName.lastName,
            "saUtr" -> validSautr.value
          )

          checkIsRedirect(result, routes.StartController.start())
        }

      }

    }

  }



}
