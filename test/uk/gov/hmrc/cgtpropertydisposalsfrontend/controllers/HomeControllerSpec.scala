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

import play.api.test.FakeRequest
import play.api.test.Helpers._

class HomeControllerSpec extends ControllerSpec {

  val controller = instanceOf[HomeController]

  "The ReturnsController" when {

    "handling requests to display the start page" must {

      "display the start page" in {
        val result = controller.start()(FakeRequest())
        status(result)          shouldBe OK
        contentAsString(result) should include("Start your return here")

      }

    }

  }

}
