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

trait PostcodeFormValidationTests { this: ControllerSpec =>

  def commonPostcodeFormValidationTests(
    performAction: Seq[(String, String)] => Future[Result],
    mockActions: () => Unit
  )(implicit messagesApi: MessagesApi): Unit =
    "show form errors" when {
      "the postcode is empty" in {
        mockActions()

        val result = performAction(Seq("postcode" -> ""))
        status(result)        shouldBe BAD_REQUEST
        contentAsString(result) should include(
          messageFromMessageKey("postcode.error.required")
        )
      }

      "the postcode is too long" in {
        List(
          "BFPO123456",
          "AA1AB8ABA"
        ).foreach { invalidPostcode =>
          withClue(s"For postcode '$invalidPostcode'") {
            mockActions()

            val result = performAction(Seq("postcode" -> invalidPostcode))
            status(result)        shouldBe BAD_REQUEST
            contentAsString(result) should include(
              messageFromMessageKey("postcode.error.tooLong")
            )
          }
        }
      }

      "the postcode isn't valid" in {
        List(
          "A00A"     -> "postcode.error.pattern",
          "AA0A0AAA" -> "postcode.error.pattern",
          "AA0.0AA"  -> "postcode.error.invalidCharacters",
          "AAA123"   -> "postcode.error.pattern",
          "A11AAA"   -> "postcode.error.pattern"
        ).foreach { case (invalidPostcode, errorMessageKey) =>
          withClue(s"For postcode '$invalidPostcode'") {
            mockActions()

            val result = performAction(Seq("postcode" -> invalidPostcode))
            status(result)        shouldBe BAD_REQUEST
            contentAsString(result) should include(
              messageFromMessageKey(errorMessageKey)
            )
          }
        }
      }
    }

}
