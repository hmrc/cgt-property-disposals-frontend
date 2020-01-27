/*
 * Copyright 2020 HM Revenue & Customs
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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.accounts.homepage

import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.Configuration
import play.api.i18n.MessagesApi
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers.{contentAsString, _}
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.accounts.homepage.privatebeta.PrivateBetaHomePageController
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.Subscribed
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{SessionData, UserType}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore

import scala.concurrent.Future

trait HomePageControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with ScalaCheckDrivenPropertyChecks
    with RedirectToStartBehaviour {

  override lazy val additionalConfig: Configuration = Configuration(
    "application.router" -> "prod.Routes"
  )

  override val overrideBindings =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore)
    )

  val subscribed = sample[Subscribed]

  def redirectToStartBehaviour(performAction: () => Future[Result]): Unit =
    redirectToStartWhenInvalidJourney(
      performAction, {
        case _: Subscribed => true
        case _             => false
      }
    )

}

class PublicBetaHomePageControllerSpec extends HomePageControllerSpec {

  override lazy val additionalConfig: Configuration = Configuration(
    "application.router" -> "prod.Routes"
  )

  lazy val controller = instanceOf[HomePageController]

  implicit val messagesApi: MessagesApi = controller.messagesApi

  val subscribedSessionData = SessionData.empty.copy(journeyStatus = Some(subscribed))

  "The HomePage Controller" when {

    "handling requests for account home" must {

      def performAction(): Future[Result] = controller.homepage()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      "display the home page" in {
        forAll { userType: Option[UserType] =>
          whenever(!userType.contains(UserType.Agent)) {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(Future.successful(Right(Some(subscribedSessionData.copy(userType = userType)))))
            }

            val result  = performAction()
            val content = contentAsString(result)

            status(result) shouldBe OK
            content        should include(message("account.home.title"))
            content        should include(message("account.home.button.start-a-new-return"))
            content shouldNot include(
              message(
                "account.home.subtitle.agent",
                subscribed.subscribedDetails.makeAccountName(),
                subscribed.subscribedDetails.cgtReference.value
              )
            )
            content should include(
              message(
                "account.home.subtitle",
                subscribed.subscribedDetails.cgtReference.value
              )
            )
          }
        }
      }

      "display the home page for agents" in {
        val sessionData =
          SessionData.empty.copy(journeyStatus = Some(subscribed), userType = Some(UserType.Agent))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(Future.successful(Right(Some(sessionData))))
        }

        val result  = performAction()
        val content = contentAsString(result)

        status(result) shouldBe OK
        content        should include(message("account.home.title"))
        content should include(
          message(
            "account.home.subtitle.agent",
            subscribed.subscribedDetails.makeAccountName(),
            subscribed.subscribedDetails.cgtReference.value
          )
        )
        content should include(
          message(
            "account.home.subtitle.agent",
            subscribed.subscribedDetails.makeAccountName(),
            subscribed.subscribedDetails.cgtReference.value
          )
        )
      }

    }

    "handling requests to start a new return" must {

      def performAction(): Future[Result] = controller.startNewReturn()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      "show an error page" when {

        "the user type is not valid" in {
          forAll { userType: Option[UserType] =>
            whenever(!userType.contains(UserType.Individual)) {
              withClue(s"For user type '$userType': ") {
                inSequence {
                  mockAuthWithNoRetrievals()
                  mockGetSession(Future.successful(Right(Some(subscribedSessionData.copy(userType = userType)))))
                }

                checkIsTechnicalErrorPage(performAction())
              }
            }
          }
        }
      }

      "redirect to the who is the individual reporting for page" when {

        "the user type is individual" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              Future.successful(Right(Some(subscribedSessionData.copy(userType = Some(UserType.Individual)))))
            )
          }

          checkIsRedirect(
            performAction(),
            controllers.returns.triage.routes.CanTheyUseOurServiceController.whoIsIndividualRepresenting()
          )
        }

      }

    }

  }

}

class PrivateBetaHomePageControllerSpec extends HomePageControllerSpec {

  override lazy val additionalConfig: Configuration = Configuration(
    "application.router" -> "private_beta.Routes"
  )

  lazy val controller = instanceOf[PrivateBetaHomePageController]

  implicit val messagesApi: MessagesApi = controller.messagesApi

  "The HomePage Controller" when {

    "handling requests for account home for private beta" must {

      def performAction(): Future[Result] = controller.privateBetaHomepage()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      "display the home page" in {
        val sessionData =
          SessionData.empty.copy(journeyStatus = Some(subscribed))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(Future.successful(Right(Some(sessionData))))
        }

        val result = performAction()
        status(result)          shouldBe OK
        contentAsString(result) should include(message("account.home.title"))
        contentAsString(result) shouldNot include(message("account.home.button.start-a-new-return"))
      }

    }

  }

}
