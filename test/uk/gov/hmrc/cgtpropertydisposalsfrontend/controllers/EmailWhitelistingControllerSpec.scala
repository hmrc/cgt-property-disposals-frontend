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

import play.api.i18n.MessagesApi
import play.api.test.FakeRequest
import play.api.test.Helpers._

class EmailWhitelistingControllerSpec extends ControllerSpec {

  lazy val controller = instanceOf[EmailWhitelistingController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  "The EmailWhitelistingController" when {

    "handling requests to display the there's a problem page" must {

      "display the correct content" in {
        val result = controller.thereIsAProblem()(FakeRequest())

        status(result)          shouldBe FORBIDDEN
        contentAsString(result) should include(message("email-whitelisting.problem.title"))
      }

    }

  }

}
