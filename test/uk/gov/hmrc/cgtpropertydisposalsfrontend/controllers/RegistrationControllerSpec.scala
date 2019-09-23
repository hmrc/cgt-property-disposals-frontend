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

import cats.Eq
import cats.syntax.eq._
import org.scalacheck.ScalacheckShapeless._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.i18n.MessagesApi
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.CSRFTokenHelper._
import play.api.test.Helpers._
import shapeless.{Lens, lens}
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.SubscriptionStatus
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.SubscriptionStatus.IndividualWithInsufficientConfidenceLevel
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.RegistrationStatus
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.GGCredId
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, JourneyStatus, Name, SessionData, sample}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore

import scala.concurrent.Future

class RegistrationControllerSpec
  extends ControllerSpec with AuthSupport with SessionSupport with ScalaCheckDrivenPropertyChecks with NameFormValidationTests {

  override val overrideBindings =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore)
    )

  lazy val controller = instanceOf[RegistrationController]

  implicit val messagesApi: MessagesApi = controller.messagesApi

  implicit val subscriptionStatusEq: Eq[SubscriptionStatus] = Eq.fromUniversalEquals

  val name = sample[Name]

  val individualWithInsufficentCLSubscriptionStatus =
    IndividualWithInsufficientConfidenceLevel(Some(false), Some(false), None, GGCredId("id"))

  val journeyStatusLens: Lens[SessionData, Option[JourneyStatus]] = lens[SessionData].journeyStatus

  "RegistrationController" when {

    def redirectToStartBehaviour(performAction: () => Future[Result]): Unit = {

      "redirect to the start endpoint" when {

        "the session data indicates that user should not see page" in {
          def isValidStatus(subscriptionStatus: SubscriptionStatus): Boolean = subscriptionStatus match {
            case IndividualWithInsufficientConfidenceLevel(Some(false), Some(false), _, _) => true
            case _ => false
          }

          forAll { subscriptionStatus: SubscriptionStatus =>
            whenever(!isValidStatus(subscriptionStatus)) {
              val sessionData = SessionData.empty.copy(journeyStatus = Some(subscriptionStatus))

              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(Future.successful(Right(Some(sessionData))))
              }

              checkIsRedirect(performAction(), routes.StartController.start())
            }
          }

        }

      }
    }

    def commonIndividualRegistrationBehaviour(performAction: () => Future[Result]): Unit = {
      val sessionData =
        SessionData.empty.copy(journeyStatus = Some(individualWithInsufficentCLSubscriptionStatus))

      "redirect to the start endpoint" when {

        "there is no registration status in the session" in {
          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionData.copy(journeyStatus = None)))))
          }

          checkIsRedirect(performAction(), routes.StartController.start())
        }

      }

      "redirect to the wrong gg account page" when {

        "the session data indicates that the user wishes to register a trust" in {
          val session = sessionData.copy(journeyStatus = Some(RegistrationStatus.IndividualWantsToRegisterTrust))

          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(session))))
          }

          checkIsRedirect(performAction(), routes.RegistrationController.wrongGGAccountForTrusts())
        }

      }

      "redirect to the registration start page" when {

        "the session data indicates the user has insufficient confidence level and has no NINO " +
          "or SA UTR" in {
          val session = sessionData.copy(journeyStatus = Some(individualWithInsufficentCLSubscriptionStatus))

          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(session))))
          }

          checkIsRedirect(performAction(), routes.RegistrationController.startRegistration())
        }

      }

    }


    "handling requests to show the registration start page" must {

      def performAction(): Future[Result] =
        controller.startRegistration()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      "show the page" when {

        "the session data indicates that the user has no digital footprint and " +
          "they have indicated that they have no NINO or SA UTR" in {
          val sessionData =
            SessionData.empty.copy(journeyStatus = Some(individualWithInsufficentCLSubscriptionStatus))

          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionData))))
          }

          val result = performAction()
          status(result) shouldBe OK
          contentAsString(result) should include(message("registration.start.title"))
        }

      }

    }

    "handling requests to show the select entity type page" must {

      def performAction(): Future[Result] =
        controller.selectEntityType()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      "show the page" when {

        "the session data indicates that the user has no digital footprint and " +
          "the user has opted to start registration" in {
          val sessionData =
            SessionData.empty.copy(journeyStatus = Some(individualWithInsufficentCLSubscriptionStatus))

          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionData))))
          }

          val result = performAction()
          status(result) shouldBe OK
          contentAsString(result) should include(message("entityType.title"))
        }
      }

      "prepopulate the form if the user has previously answered the question" in {
        List(
          RegistrationStatus.IndividualWantsToRegisterTrust,
          RegistrationStatus.IndividualSupplyingInformation(None)
        ).foreach{ journeyStatus =>
          val sessionData =
            SessionData.empty.copy(
              journeyStatus = Some(journeyStatus)
            )

          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionData))))
          }

          val result = performAction()
          status(result) shouldBe OK
          contentAsString(result) should include(message("entityType.title"))
          contentAsString(result) should include("""checked="checked"""")
        }

      }

    }

    "handling requests to submit the selected entity type" must {

      def performAction(formData: (String, String)*): Future[Result] =
        controller.selectEntityTypeSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*).withCSRFToken)

      val sessionData =
        SessionData.empty.copy(journeyStatus = Some(individualWithInsufficentCLSubscriptionStatus))

      behave like redirectToStartBehaviour(() => performAction())

      "show the page with errors" when {
        "the request submits no selection" in {
          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionData))))
          }

          val result = performAction()
          status(result) shouldBe BAD_REQUEST
          contentAsString(result) should include(message("entityType.error.required"))
        }

        "the request submits an invalid value" in {
          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionData))))
          }
          val result = performAction("entityType" -> "2")
          status(result) shouldBe BAD_REQUEST
          contentAsString(result) should include(message("entityType.invalid"))
        }
      }

      "redirect to the wrong gg account page" when {
        "the request selects trust" in {
          val updatedSession =
            sessionData.copy(journeyStatus = Some(RegistrationStatus.IndividualWantsToRegisterTrust))

          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionData))))
            mockStoreSession(updatedSession)(Future.successful(Right(())))
          }
          val result = performAction("entityType" -> "1")
          checkIsRedirect(result, routes.RegistrationController.wrongGGAccountForTrusts())
        }

      "continue the registration journey" when {
        "the request selects individual" in {
          val updatedSession =
            sessionData.copy(journeyStatus = Some(RegistrationStatus.IndividualSupplyingInformation(None)))

          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionData))))
            mockStoreSession(updatedSession)(Future.successful(Right(())))
          }
          val result = performAction("entityType" -> "0")
          checkIsRedirect(result, routes.RegistrationController.enterName())
        }

      }

      "display an error page" when {

        "the session cannot be updated" in {
          List[(String,RegistrationStatus)](
            "0" -> RegistrationStatus.IndividualSupplyingInformation(None),
            "1" -> RegistrationStatus.IndividualWantsToRegisterTrust
          ).foreach{ case (entityType, registrationStatus) =>

            inSequence{
              mockAuthWithNoRetrievals()
              mockGetSession(Future.successful(Right(Some(sessionData))))
              mockStoreSession(sessionData.copy(journeyStatus = Some(registrationStatus)))(Future.successful(Left(Error(""))))
            }

            checkIsTechnicalErrorPage(performAction("entityType" -> entityType))
          }


        }

      }

        "not update the session" when {

          "the user selects trust and has previously indicated that they wish to register a trust" in {
            val session =
              sessionData.copy(journeyStatus = Some(RegistrationStatus.IndividualWantsToRegisterTrust))

            inSequence{
              mockAuthWithNoRetrievals()
              mockGetSession(Future.successful(Right(Some(session))))
            }
            val result = performAction("entityType" -> "1")
            checkIsRedirect(result, routes.RegistrationController.wrongGGAccountForTrusts())

          }

          "the user selects individual and has previously indicated that they wish to register as an individual" in {
            val session =
              sessionData.copy(journeyStatus = Some(RegistrationStatus.IndividualSupplyingInformation(None)))

            inSequence{
              mockAuthWithNoRetrievals()
              mockGetSession(Future.successful(Right(Some(session))))
            }
            val result = performAction("entityType" -> "0")
            checkIsRedirect(result, routes.RegistrationController.enterName())

          }

        }

      }

    }

    "handling requests to view the enter name page" must {
      def performAction(): Future[Result] =
        controller.enterName()(FakeRequest())

      val sessionData =
        SessionData.empty.copy(
          journeyStatus = Some(RegistrationStatus.IndividualSupplyingInformation(None))
        )

      behave like redirectToStartBehaviour(performAction)

      behave like commonIndividualRegistrationBehaviour(performAction)

      "show the page" when {
        "the endpoint is requested" in {
          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionData))))
          }
          val result = performAction()
          status(result) shouldBe OK
          contentAsString(result) should include(message("enterName.title"))
        }

        "the endpoint is requested and the user has previously entered a name" in {
          val name = sample[Name]
          val sessionDataWithName = journeyStatusLens.set(sessionData)(
            Some(RegistrationStatus.IndividualSupplyingInformation(Some(name)))
          )

          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionDataWithName))))
          }
          val result = performAction()
          status(result) shouldBe OK

          val content = contentAsString(result)
          content should include(message("enterName.title"))
          content should include(name.firstName)
          content should include(name.lastName)
        }

      }

    }

    "handling requests to submit the enter name page" must {

      def performAction(formData: (String, String)*): Future[Result] =
        controller.enterNameSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*).withCSRFToken)

      val sessionData =
        SessionData.empty.copy(
          journeyStatus = Some(RegistrationStatus.IndividualSupplyingInformation(None))
        )

      val name = Name("Bob", "Smith")

      val updatedSession =
      journeyStatusLens.set(sessionData)(
          Some(RegistrationStatus.IndividualSupplyingInformation(Some(name)))
      )

      behave like redirectToStartBehaviour(() => performAction())

      behave like commonIndividualRegistrationBehaviour(() => performAction())

      behave like nameFormValidationTests(
        performAction,
        () => inSequence{
          mockAuthWithNoRetrievals()
          mockGetSession(Future.successful(Right(Some(sessionData))))
        }
      )

      "be successful" when {

        "the request submits valid values" in {
          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionData))))
            mockStoreSession(updatedSession)(Future.successful(Right(())))
          }
          val result = performAction("firstName" -> name.firstName, "lastName" -> name.lastName)
          status(result) shouldBe OK
        }
        "request submits valid values with leading and trailing spaces" in {
          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionData))))
            mockStoreSession(updatedSession)(Future.successful(Right(())))
          }

          val result = performAction("firstName" -> s" ${name.firstName}  ", "lastName" -> s" ${name.lastName} ")
          status(result) shouldBe OK
        }
      }

      "not update the session" when {

        "the name submitted is the same in the session" in {
          val session =
            journeyStatusLens.set(sessionData)(
              Some(RegistrationStatus.IndividualSupplyingInformation(Some(name)))
            )

          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(session))))
          }
          val result = performAction("firstName" -> name.firstName, "lastName" -> name.lastName)
          status(result) shouldBe OK
        }

      }

      "display an error page" when {
        "the session cannot be updated" in {
          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionData))))
            mockStoreSession(updatedSession)(Future.successful(Left(Error(""))))
          }

          checkIsTechnicalErrorPage(performAction("firstName" -> name.firstName, "lastName" -> name.lastName))
        }
      }


    }

    "handling requests to view the wrong gg account page" must {

      def performAction(): Future[Result] =
        controller.wrongGGAccountForTrusts()(FakeRequest())

      val sessionData =
        SessionData.empty.copy(
          journeyStatus = Some(RegistrationStatus.IndividualWantsToRegisterTrust)
        )

      behave like redirectToStartBehaviour(performAction)

      "show the page" when {
        "the endpoint is requested" in {
          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionData))))
          }
          val result = performAction()
          status(result) shouldBe OK
          contentAsString(result) should include(message("wrongAccountForTrusts.title"))
        }
      }

      "redirect to the start registration endpoint" when {

        "the session indicates that they want to register as an individual" in {
          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(
              sessionData.copy(journeyStatus = Some(RegistrationStatus.IndividualSupplyingInformation(None))))
            )))
          }
          checkIsRedirect(performAction(), routes.RegistrationController.startRegistration())
        }

      }

    }

  }

}
