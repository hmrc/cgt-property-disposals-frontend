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

import org.scalacheck.ScalacheckShapeless._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.i18n.MessagesApi
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.CSRFTokenHelper._
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.SubscriptionStatus._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, JourneyStatus, SessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore

import scala.concurrent.Future

class DeterminingIfOrganisationIsTrustControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with ScalaCheckDrivenPropertyChecks
    with RedirectToStartBehaviour {

  override val overrideBindings =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore)
    )

  lazy val controller = instanceOf[DeterminingIfOrganisationIsTrustController]

  lazy implicit val messagesApi: MessagesApi = controller.messagesApi


  def redirectToStartBehaviour(performAction: () => Future[Result]): Unit =
    behave like redirectToStartWhenInvalidJourney(
      performAction, {
        case _: DeterminingIfOrganisationIsTrust => true
        case _                 => false
      }
    )

  def sessionDataWithStatus(journeyStatus: JourneyStatus): SessionData =
    SessionData.empty.copy(journeyStatus = Some(journeyStatus))

  "DeterminingIfOrganisationIsTrustController" when {

    "handling requests to display the 'do you want to report for a trust' page" must {

      def performAction(): Future[Result] =
        controller.doYouWantToReportForATrust()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      "show the page" when {

        "the user has not selected an option before" in {
          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(
              sessionDataWithStatus(DeterminingIfOrganisationIsTrust(None, None))
            ))))
          }

          val result = performAction()
          status(result) shouldBe OK
          contentAsString(result) should include(message("isReportingForATrust.title"))
        }

        "the user has selected an option before" in {
          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(
              sessionDataWithStatus(DeterminingIfOrganisationIsTrust(Some(true), None))
            ))))
          }

          val result = performAction()
          val content = contentAsString(result)

          status(result) shouldBe OK
          content should include(message("isReportingForATrust.title"))
          content should include("checked=\"checked\"")
        }

      }

    }

    "handling submitted answers from the 'do you want to report for a trust' page" must {

      def performAction(formData: (String,String)*): Future[Result] =
        controller.doYouWantToReportForATrustSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*).withCSRFToken)

      behave like redirectToStartBehaviour(() => performAction())

      "show a form error" when {

        "an option has not been selected" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(
              sessionDataWithStatus(DeterminingIfOrganisationIsTrust(None, None))
            ))))
          }

          val result = performAction()
          status(result) shouldBe BAD_REQUEST
          contentAsString(result) should include(message("isReportingForATrust.error.required"))
        }

        "the data submitted cannot be read" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(
              sessionDataWithStatus(DeterminingIfOrganisationIsTrust(None, None))
            ))))
          }

          val result = performAction("isReportingForATrust" -> "123")
          status(result) shouldBe BAD_REQUEST
          contentAsString(result) should include(message("isReportingForATrust.error.boolean"))
        }

      }

      "show an error page" when {

        "the answer cannot be stored in mongo" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(
              sessionDataWithStatus(DeterminingIfOrganisationIsTrust(None, None))
            ))))
            mockStoreSession(
              SessionData.empty.copy(
                journeyStatus = Some(DeterminingIfOrganisationIsTrust(Some(true), None))
              )
            )(Future.successful(Left(Error(""))))
          }

          checkIsTechnicalErrorPage(performAction("isReportingForATrust" -> "true"))
        }

      }

      "redirect to the 'report with corporate tax' page" when {

        "the answer says they are not reporting for a trust" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(
              sessionDataWithStatus(DeterminingIfOrganisationIsTrust(None, None))
            ))))
            mockStoreSession(
              SessionData.empty.copy(
                journeyStatus = Some(DeterminingIfOrganisationIsTrust(Some(false), None))
              )
            )(Future.successful(Right(())))
          }

          checkIsRedirect(
            performAction("isReportingForATrust" -> "false"),
            routes.DeterminingIfOrganisationIsTrustController.reportWithCorporateTax()
          )
        }


      }

      "redirect to the 'do you have a trn' page" when {

        "the answer says they are reporting for a trust" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(
              sessionDataWithStatus(DeterminingIfOrganisationIsTrust(None, None))
            ))))
            mockStoreSession(
              SessionData.empty.copy(
                journeyStatus = Some(DeterminingIfOrganisationIsTrust(Some(true), None))
              )
            )(Future.successful(Right(())))
          }

          checkIsRedirect(
            performAction("isReportingForATrust" -> "true"),
            routes.DeterminingIfOrganisationIsTrustController.doYouHaveATrn()
          )
        }
      }

    }

    "handling requests to display the 'report with corporate tax' page" must {

      def performAction(): Future[Result] =
        controller.reportWithCorporateTax()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      "redirect to the start endpoint" when {

        "the user has not answered whether or not they're reporting for a trust" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(
              sessionDataWithStatus(DeterminingIfOrganisationIsTrust(None, None))
            ))))
          }

          checkIsRedirect(performAction(), routes.StartController.start())
        }

        "the user has said that they're reporting for a trust" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(
              sessionDataWithStatus(DeterminingIfOrganisationIsTrust(Some(true), None))
            ))))
          }

          checkIsRedirect(performAction(), routes.StartController.start())
        }

      }

      "show the page" when {

        "the user has said that they're not reporting for a trust" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(
              sessionDataWithStatus(DeterminingIfOrganisationIsTrust(Some(false), None))
            ))))
          }

          val result = performAction()
          status(result) shouldBe 200
          contentAsString(result) should include("Go and report your CGT tax with corporate tax")
        }

      }

    }

    "handling requests to display the 'do you have a TRN' page" must {

      def performAction(): Future[Result] =
        controller.doYouHaveATrn()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      "redirect to the start endpoint" when {

        "the user has not indicated whether or not they are reporting for a trust" in {
          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(
              sessionDataWithStatus(DeterminingIfOrganisationIsTrust(None, None))
            ))))
          }

          checkIsRedirect(performAction(), routes.StartController.start())
        }

        "the user has indicated that they are not reporting for a trust" in {
          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(
              sessionDataWithStatus(DeterminingIfOrganisationIsTrust(Some(false), None))
            ))))
          }

          checkIsRedirect(performAction(), routes.StartController.start())
        }

      }

      "show the page" when {

        "the user has not selected an option before" in {
          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(
              sessionDataWithStatus(DeterminingIfOrganisationIsTrust(Some(true), None))
            ))))
          }

          val result = performAction()
          status(result) shouldBe OK
          contentAsString(result) should include(message("haveATrn.title"))
        }

        "the user has selected an option before" in {
          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(
              sessionDataWithStatus(DeterminingIfOrganisationIsTrust(Some(true), Some(true)))
            ))))
          }

          val result = performAction()
          val content = contentAsString(result)

          status(result) shouldBe OK
          content should include(message("haveATrn.title"))
          content should include("checked=\"checked\"")
        }

      }

    }

    "handling submitted answers from the 'do you have a TRN' page" must {

      def performAction(formData: (String,String)*): Future[Result] =
        controller.doYouHaveATrnSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*).withCSRFToken)

      behave like redirectToStartBehaviour(() => performAction())

      "redirect to the start endpoint" when {

        "the user has not indicated whether or not they are reporting for a trust" in {
          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(
              sessionDataWithStatus(DeterminingIfOrganisationIsTrust(None, None))
            ))))
          }

          checkIsRedirect(performAction(), routes.StartController.start())
        }

        "the user has indicated that they are not reporting for a trust" in {
          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(
              sessionDataWithStatus(DeterminingIfOrganisationIsTrust(Some(false), None))
            ))))
          }

          checkIsRedirect(performAction(), routes.StartController.start())
        }

      }

      "show a form error" when {

        "an option has not been selected" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(
              sessionDataWithStatus(DeterminingIfOrganisationIsTrust(Some(true), None))
            ))))
          }

          val result = performAction()
          status(result) shouldBe BAD_REQUEST
          contentAsString(result) should include(message("hasTrn.error.required"))
        }

        "the data submitted cannot be read" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(
              sessionDataWithStatus(DeterminingIfOrganisationIsTrust(Some(true), None))
            ))))
          }

          val result = performAction("hasTrn" -> "123")
          status(result) shouldBe BAD_REQUEST
          contentAsString(result) should include(message("hasTrn.error.boolean"))
        }

      }

      "show an error page" when {

        "the answer cannot be stored in mongo" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(
              sessionDataWithStatus(DeterminingIfOrganisationIsTrust(Some(true), None))
            ))))
            mockStoreSession(
              SessionData.empty.copy(
                journeyStatus = Some(DeterminingIfOrganisationIsTrust(Some(true), Some(true)))
              )
            )(Future.successful(Left(Error(""))))
          }

          checkIsTechnicalErrorPage(performAction("hasTrn" -> "true"))
        }

      }

      "redirect to the 'register your trust' page" when {

        "the user does not have a TRN" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(
              sessionDataWithStatus(DeterminingIfOrganisationIsTrust(Some(true), None))
            ))))
            mockStoreSession(
              SessionData.empty.copy(
                journeyStatus = Some(DeterminingIfOrganisationIsTrust(Some(true), Some(false)))
              )
            )(Future.successful(Right(())))
          }

          checkIsRedirect(
            performAction("hasTrn" -> "false"),
            routes.DeterminingIfOrganisationIsTrustController.registerYourTrust()
          )
        }


      }

      "redirect to the 'enter trn' page" when {

        "the user does  have a TRN" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(
              sessionDataWithStatus(DeterminingIfOrganisationIsTrust(Some(true), None))
            ))))
            mockStoreSession(
              SessionData.empty.copy(
                journeyStatus = Some(DeterminingIfOrganisationIsTrust(Some(true), Some(true)))
              )
            )(Future.successful(Right(())))
          }

          checkIsRedirect(
            performAction("hasTrn" -> "true"),
            routes.DeterminingIfOrganisationIsTrustController.enterTrn()
          )
        }
      }

    }

    "handling requests to display the register your trust page" must {

      def performAction(): Future[Result] = controller.registerYourTrust()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      "show the register your trust page" when {

        "the session data indicates the user is an organisation which is not associated with a registered trust" in {
          val sessionData = sessionDataWithStatus(DeterminingIfOrganisationIsTrust(None, None))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionData))))
          }

          val result = performAction()
          status(result)          shouldBe OK
          contentAsString(result) should include(message("registerTrust.title"))
        }

      }
    }

  }

}
