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

import org.joda.time.LocalDate
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.test.FakeRequest
import uk.gov.hmrc.auth.core.retrieve._
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

    val retrievals = Retrievals.nino and Retrievals.itmpName and Retrievals.name and Retrievals.itmpDateOfBirth
    val retrievalsResult = Future successful (
      new ~(
        new ~(
          new ~(Some("nino"), Some(ItmpName(Some("givenName"), Some("middleName"), Some("familyName")))),
          Some(Name(Some("forename"), Some("surname")))
        ),
        Some(new LocalDate(2000, 4, 10))
      )
    )
    "handling IV success request" must {

      "redirect to the IV continue URL if one is found in session" in {
        val ivContinueUrl = "continue"
        inSequence {
          mockAuth(ConfidenceLevel.L200, retrievals)(retrievalsResult)
          mockGetSession(Future.successful(Right(Some(SessionData.empty.copy(ivContinueUrl = Some(ivContinueUrl))))))
        }

        val result = controller.ivSuccess()(FakeRequest())
        checkIsRedirect(result, ivContinueUrl)
      }

      "show an error page if there is no IV continue URL in session" in {
        inSequence {
          mockAuth(ConfidenceLevel.L200, retrievals)(retrievalsResult)
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
