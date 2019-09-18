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

import java.util.UUID

import org.scalacheck.ScalacheckShapeless._
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.SubscriptionStatus.IndividualWithInsufficientConfidenceLevel
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, Name, SessionData, sample}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore

import scala.concurrent.Future
class IvControllerSpec extends ControllerSpec with AuthSupport with SessionSupport {

  override val overrideBindings =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore)
    )

  lazy val controller = instanceOf[IvController]

  val name = sample[Name]

  "IvController" when {

    "handling IV success request" must {

      def performAction(): Future[Result] = controller.ivSuccess()(FakeRequest())

      val nonEmptySession =
       SessionData.empty.copy(journeyStatus = Some(IndividualWithInsufficientConfidenceLevel(None, None, name, None)))

      "clear the session and redirect to the start endpoint" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(Future.successful(Right(Some(nonEmptySession))))
          mockStoreSession(SessionData.empty)(Future.successful(Right(())))
        }

        checkIsRedirect(performAction(), routes.StartController.start())
      }

      "show an error page if there is an error clearing the session" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(Future.successful(Right(Some(nonEmptySession))))
          mockStoreSession(SessionData.empty)(Future.successful(Left(Error(""))))
        }

        checkIsTechnicalErrorPage(performAction())
      }

      "not update the session" when {

        "there is no session data" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(None)))
          }

          checkIsRedirect(performAction(), routes.StartController.start())
        }

        "the session data is empty" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(SessionData.empty))))
          }

          checkIsRedirect(performAction(), routes.StartController.start())
        }

      }

    }

    "handling IV failure requests" must {
      "show an error page" in {
        checkIsTechnicalErrorPage(controller.ivFailure(UUID.randomUUID())(FakeRequest()))
      }

    }

  }

}
