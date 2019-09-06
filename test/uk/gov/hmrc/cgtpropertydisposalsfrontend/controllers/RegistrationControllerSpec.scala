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
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.SubscriptionStatus.IndividualWithInsufficientConfidenceLevel
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{HasSAUTR, Name, SessionData, SubscriptionStatus, sample}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore

import scala.concurrent.Future

class RegistrationControllerSpec
  extends ControllerSpec with AuthSupport with SessionSupport with ScalaCheckDrivenPropertyChecks {

  override val overrideBindings =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore)
    )

  lazy val controller = instanceOf[RegistrationController]

  implicit val messagesApi: MessagesApi = controller.messagesApi

  implicit val subscriptionStatusEq: Eq[SubscriptionStatus] = Eq.fromUniversalEquals

  val name = sample[Name]

  "RegistrationController" when {

    "handling requests to show the registration start page" must {

      def performAction(): Future[Result] =
        controller.startRegistration()(FakeRequest())

      "show the page" when {

        "the session data indicates that the user has no digital footprint and " +
          "they have indicated that they have no NINO or SA UTR" in {
          val sessionData =
            SessionData.empty.copy(subscriptionStatus = Some(
              IndividualWithInsufficientConfidenceLevel(Some(false), Some(HasSAUTR(None)), name, None))
              )

          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionData))))
          }

          val result = performAction()
          status(result) shouldBe OK
          contentAsString(result) should include(message("registration.start.title"))
        }


      }

      "redirect to the start endpoint" when {

        "the session data indicates otherwise" in {
          def isValidStatus(subscriptionStatus: SubscriptionStatus): Boolean = subscriptionStatus match {
            case IndividualWithInsufficientConfidenceLevel(Some(false), Some(HasSAUTR(None)), _, _) => true
            case _ => false
          }

          forAll { subscriptionStatus: SubscriptionStatus =>
            whenever(!isValidStatus(subscriptionStatus)) {
              val sessionData = SessionData.empty.copy(subscriptionStatus = Some(subscriptionStatus))

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

  }

}
