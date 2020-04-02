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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers

import play.api.i18n.MessagesApi
import play.api.test.FakeRequest
import play.api.test.Helpers._

class LandingPageControllerSpec extends ControllerSpec {

  lazy val controller: LandingPageController = instanceOf[LandingPageController]

  "The LandingPageController" must {

    "display the landing page" in {
      implicit val messagesApi: MessagesApi = controller.messagesApi
      val result                            = controller.landingPage()(FakeRequest())
      contentAsString(controller.landingPage()(FakeRequest())) should include(
        messageFromMessageKey("landingPage.title")
      )

      checkPageIsDisplayed(
        result,
        messageFromMessageKey("landingPage.title"),
        doc => doc.select(".button").attr("href") shouldBe s"${routes.StartController.start()}"
      )
    }

    "display the agents landing page" in {
      implicit val messagesApi: MessagesApi = controller.messagesApi
      val result                            = controller.agentsLandingPage()(FakeRequest())

      checkPageIsDisplayed(
        result,
        messageFromMessageKey("agentsLandingPage.title"),
        doc => doc.select(".button").attr("href") shouldBe s"${viewConfig.agentsSignInUrl}"
      )
    }

    "display the sign in page" in {
      implicit val messagesApi: MessagesApi = controller.messagesApi
      contentAsString(controller.signInPage()(FakeRequest())) should include(messageFromMessageKey("signInPage.title"))
    }

  }

}
