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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns

import org.jsoup.Jsoup
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.i18n.MessagesApi
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.ViewingReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.SessionData
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.MoneyUtils
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore

import scala.concurrent.Future

class ViewReturnControllerSpec
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

  lazy val controller = instanceOf[ViewReturnController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  "ViewReturnController" when {

    "handling requests to display a return" must {

      def performAction(): Future[Result] =
        controller.displayReturn()(FakeRequest())

      val viewingReturn = sample[ViewingReturn]

      behave like redirectToStartWhenInvalidJourney(
        performAction, {
          case _: ViewingReturn => true
          case _                => false
        }
      )

      "display the page" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData.empty.copy(journeyStatus = Some(viewingReturn)))
        }

        val result   = performAction()
        val document = Jsoup.parse(contentAsString(result))

        document
          .select("#content > article > div.govuk-box-highlight.govuk-box-highlight--status > h1")
          .text() shouldBe messageFromMessageKey(
          "viewReturn.title"
        )
        document.select("#heading-reference").text() shouldBe viewingReturn.returnSummary.submissionId
        document.select("#heading-tax-owed").text() shouldBe MoneyUtils.formatAmountOfMoneyWithPoundSign(
          viewingReturn.returnSummary.totalCGTLiability.withFloorZero.inPounds()
        )
      }

    }

  }

}
