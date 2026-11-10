/*
 * Copyright 2026 HM Revenue & Customs
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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.models.email

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.data.Form
import play.api.data.Forms.single

class EmailSpec extends AnyWordSpec with Matchers {

  private val form: Form[Email] =
    Form(single("email" -> Email.mapping))

  "Email.mapping" should {

    "bind a valid email address" in {
      val email = "test@example.com"

      val result = form.bind(
        Map("email" -> email)
      )

      result.hasErrors shouldBe false
      result.value     shouldBe Some(Email(email))
    }

    "return an error when the email is empty" in {
      val result = form.bind(
        Map("email" -> "")
      )

      result.hasErrors shouldBe true
    }

    "return an error when the email format is invalid" in {
      val result = form.bind(
        Map("email" -> "invalid-email")
      )

      result.hasErrors                    shouldBe true
      result.errors("email").map(_.message) should contain("invalid")
    }

    "bind an email when it is exactly 254 characters" in {
      val email =
        s"${"a" * 64}@${"b" * 63}.${"c" * 63}.${"d" * 61}"

      email.length shouldBe 254

      val result = form.bind(
        Map("email" -> email)
      )

      result.hasErrors shouldBe false
      result.value     shouldBe Some(Email(email))
    }

    "return an error when the email exceeds 254 characters" in {
      val email =
        s"${"a" * 64}@${"b" * 63}.${"c" * 63}.${"d" * 62}"

      email.length shouldBe 255

      val result = form.bind(
        Map("email" -> email)
      )

      result.hasErrors                    shouldBe true
      result.errors("email").map(_.message) should contain("error.maxLength")
    }
  }
}
