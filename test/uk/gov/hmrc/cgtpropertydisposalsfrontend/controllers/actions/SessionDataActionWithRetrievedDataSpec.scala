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
import play.api.mvc.{MessagesRequest, Result}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.ErrorHandler
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{ControllerSpec, SessionSupport}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.email.Email
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.{GGCredId, NINO}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class SessionDataActionWithRetrievedDataSpec extends ControllerSpec with SessionSupport {

  lazy val action: SessionDataActionWithRetrievedData =
    new SessionDataActionWithRetrievedData(mockSessionStore, instanceOf[ErrorHandler])

  "SessionDataActionWithRetrievedData" must {

    lazy val messagesRequest =
      new MessagesRequest(FakeRequest(), instanceOf[MessagesApi])

    lazy val authenticatedRequest =
      AuthenticatedRequestWithRetrievedData(
        JourneyUserType.Individual(
          Right(NINO("nino")),
          Some(Email("email")),
          GGCredId("id")
        ),
        messagesRequest
      )

    val sessionData = sample[SessionData]

    def performAction(): Future[Result] =
      action.invokeBlock(
        authenticatedRequest, { r: RequestWithSessionDataAndRetrievedData[_] =>
          r.messagesApi shouldBe messagesRequest.messagesApi
          r.sessionData shouldBe Some(sessionData)
          Future.successful(Ok)
        }
      )

    "return an error if there is an error getting session data" in {
      mockGetSession(Future.successful(Left(Error(new Exception("Oh no!")))))

      checkIsTechnicalErrorPage(performAction())
    }

    "perform the action with the session data if it can be retrieved" in {
      mockGetSession(Future.successful(Right(Some(sessionData))))

      status(performAction()) shouldBe OK
    }

  }

}
