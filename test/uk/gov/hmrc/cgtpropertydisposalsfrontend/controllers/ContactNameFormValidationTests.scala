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
import play.api.mvc.Result
import play.api.test.Helpers._

import scala.concurrent.Future

trait ContactNameFormValidationTests { this: ControllerSpec =>

  def contactNameFormValidationTests(
    performAction: Seq[(String, String)] => Future[Result],
    mockActions: () => Unit
  )(implicit messagesApi: MessagesApi): Unit =
    "show the page with errors" when {

      "the request submits no selection" in {
        mockActions()

        val result = performAction(Seq.empty)
        status(result) shouldBe BAD_REQUEST
        val resultAsString = contentAsString(result)
        resultAsString should include(messageFromMessageKey("contactName.error.required"))
      }

      "the request submits a contact name that is too long" in {
        mockActions()

        val result = performAction(
          Seq("contactName" -> List.fill(36)("a").mkString(""))
        )
        status(result)          shouldBe BAD_REQUEST
        contentAsString(result) should include(messageFromMessageKey("contactName.error.tooLong"))
      }

      "the request submits a contact name with illegal characters" in {
        mockActions()

        val result = performAction(
          Seq(
            "contactName" -> "C++lin"
          )
        )
        status(result)          shouldBe BAD_REQUEST
        contentAsString(result) should include(messageFromMessageKey("contactName.error.pattern"))
      }

    }

}
