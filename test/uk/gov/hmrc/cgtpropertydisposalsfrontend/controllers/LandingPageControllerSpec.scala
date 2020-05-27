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

class LandingPageControllerSpec extends ControllerSpec {

  lazy val controller: LandingPageController = instanceOf[LandingPageController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  "The LandingPageController" must {

    "handling requests to display the landing page" must {

      "display the page" in {
        checkPageIsDisplayed(
          controller.landingPage()(FakeRequest()),
          messageFromMessageKey("landingPage.title"),
          doc =>
            doc
              .select(".button")
              .attr("href") shouldBe s"${routes.StartController.start()}"
        )
      }

    }

    "handling requests to display the agents landing page" must {

      "display the page" in {
        checkPageIsDisplayed(
          controller.agentsLandingPage()(FakeRequest()),
          messageFromMessageKey("agentsLandingPage.title"),
          doc => {
            doc
              .select(".button")
              .attr("href")                        shouldBe viewConfig.agentsSignInUrl
            doc
              .select("#nonResidentsRebasingUrl > a")
              .attr("href")                        shouldBe viewConfig.nonResidentsRebasingUrl
            doc
              .select("#createAgentsAccountUrl > a")
              .attr("href")                        shouldBe viewConfig.createAgentsAccountUrl
            doc
              .select("#nrcgtReturn-1 > a")
              .attr("href")                        shouldBe viewConfig.nrcgtReturn
            doc
              .select("#nrcgtReturn-2 > a")
              .attr("href")                        shouldBe viewConfig.nrcgtReturn
            doc
              .select("#contact-hmrc-1 > a")
              .attr("href")                        shouldBe viewConfig.contactHmrc
            doc
              .select("#contact-hmrc-2 > a")
              .attr("href")                        shouldBe viewConfig.contactHmrc
            doc.select("#cgtUrl > a").attr("href") shouldBe viewConfig.cgtUrl
          }
        )
      }
    }

    "handling requests to display the sign in page" must {

      "redirect to the landing page" in {
        checkIsRedirect(
          controller.signInPage()(FakeRequest()),
          routes.LandingPageController.landingPage()
        )
      }

    }

  }

}
