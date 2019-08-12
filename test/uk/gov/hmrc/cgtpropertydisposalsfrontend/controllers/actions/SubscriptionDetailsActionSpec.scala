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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions

import play.api.i18n.MessagesApi
import play.api.mvc.Results.Ok
import play.api.mvc.{MessagesRequest, PlayBodyParsers, Result}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.ErrorHandler
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{ControllerSpec, SessionSupport, routes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, NINO, SessionData, sample, sessionGen, subscriptionDetailsGen}

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

class SubscriptionDetailsActionSpec extends ControllerSpec with SessionSupport {

  lazy val action =
    new SubscriptionDetailsAction(mockSessionStore, instanceOf[PlayBodyParsers], instanceOf[ErrorHandler])

  "SubscriptionDetailsAction" must {

    lazy val messagesRequest = new MessagesRequest(FakeRequest(), instanceOf[MessagesApi])
    lazy val authenticatedRequest = AuthenticatedRequest(NINO("nino"), messagesRequest)

    val subscriptionDetails = sample(subscriptionDetailsGen)
    val sessionData = sample(sessionGen).copy(subscriptionDetails = Some(subscriptionDetails))

      def performAction(): Future[Result] =
        action.invokeBlock(authenticatedRequest, { r: RequestWithSubscriptionDetails[_] =>
          r.sessionData shouldBe sessionData
          r.subscriptionDetails shouldBe subscriptionDetails
          Future.successful(Ok)
        })

    "return an error if there is an error getting session data" in {
      mockGetSession(Future.successful(Left(Error(new Exception("Oh no!")))))

      checkIsTechnicalErrorPage(performAction())
    }

    "redirect to the start journey endpoint" when {

      "there is no session data in store" in {
        mockGetSession(Future.successful(Right(None)))

        checkIsRedirect(performAction(), routes.StartController.start())
      }

      "there is no subscription details in the session data" in {
        mockGetSession(Future.successful(Right(Some(SessionData.empty))))

        checkIsRedirect(performAction(), routes.StartController.start())
      }

    }

    "perform the action with the subscription details if it can be retrieved" in {
      mockGetSession(Future.successful(Right(Some(sessionData))))

      status(performAction()) shouldBe OK
    }

  }

}
