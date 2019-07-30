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

import java.time.LocalDate

import org.scalamock.scalatest.MockFactory
import play.api.i18n.MessagesApi
import play.api.mvc.{AnyContent, MessagesRequest, PlayBodyParsers, Result}
import play.api.mvc.Results.Ok
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.ErrorHandler
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{DateOfBirth, Error, NINO, SessionData}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class SessionDataActionSpec extends ControllerSpec with SessionSupport with MockFactory {

  lazy val action =
    new SessionDataAction(mockSessionStore, instanceOf[PlayBodyParsers], instanceOf[ErrorHandler])

  "SessionDataAction" must {

    lazy val messagesRequest = new MessagesRequest(FakeRequest(), instanceOf[MessagesApi])

    val sessionData = SessionData(Some(NINO("AB123456C")), Some(DateOfBirth(LocalDate.ofEpochDay(0L))), None)

      def performAction(): Future[Result] =
        action.invokeBlock[AnyContent](messagesRequest, { r =>
          r.sessionData shouldBe Some(sessionData)
          Future.successful(Ok)
        })

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
