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

import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.retrieve.v2.Retrievals
import uk.gov.hmrc.auth.core.{AuthConnector, ConfidenceLevel}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.SessionData
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore

import scala.concurrent.Future

class IvControllerSpec extends ControllerSpec with AuthSupport with SessionSupport {

  override val overrideBindings =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore)
    )

  lazy val controller = instanceOf[IvController]

  "IvController" when {

    "handling IV success request" must {

      "redirect to the IV continue URL if one is found in session" in {
        val ivContinueUrl = "continue"
        inSequence {
          mockAuth(ConfidenceLevel.L200, Retrievals.nino)(Future.successful(Some("nino")))
          mockGetSession(Future.successful(Right(Some(SessionData.empty.copy(ivContinueUrl = Some(ivContinueUrl))))))
        }

        val result = controller.ivSuccess()(FakeRequest())
        checkIsRedirect(result, ivContinueUrl)
      }

      "show an error page if there is no IV continue URL in session" in {
        inSequence {
          mockAuth(ConfidenceLevel.L200, Retrievals.nino)(Future.successful(Some("nino")))
          mockGetSession(Future.successful(Right(Some(SessionData.empty))))
        }

        checkIsTechnicalErrorPage(controller.ivSuccess()(FakeRequest()))
      }

    }

    "handling IV failure requests" must {
      "show an error page" in {
        checkIsTechnicalErrorPage(controller.ivFailure(UUID.randomUUID())(FakeRequest()))
      }

    }

  }

}
