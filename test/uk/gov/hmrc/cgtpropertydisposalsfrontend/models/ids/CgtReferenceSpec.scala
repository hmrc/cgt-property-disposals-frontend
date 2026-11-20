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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.data.Form

class CgtReferenceSpec extends AnyWordSpec with Matchers {
  private val form = Form(
    "cgtReference" -> CgtReference.mapping
  )
  "CgtReference Form" should {

    "accept a valid CGT reference" in {
      val result = form.bind(
        Map("cgtReference" -> "XACGTP123456789")
      )

      result.errors shouldBe empty
      result.value  shouldBe defined
    }
    "return required error for an empty reference" in {
      val result = form.bind(
        Map("cgtReference" -> "  ")
      )

      result.errors.map(_.message) should contain("error.required")
    }
    "form successfully bind when inputs contain whitespace variants (Tabs, Newlines)" in {

      val result = form.bind(Map("cgtReference" -> "\tXA\nCGTP\r123 456 789\n"))

      result.errors shouldBe empty
      result.value  shouldBe Some(CgtReference("XACGTP123456789"))
    }
    "return too short error for a short reference" in {
      val result = form.bind(
        Map("cgtReference" -> "XACGTP123")
      )

      result.errors.map(_.message) should contain("error.tooShort")
    }
    "return too long error for a reference longer than 15 characters" in {
      val result = form.bind(
        Map("cgtReference" -> "XACGTP1234567890")
      )

      result.errors.map(_.message) should contain("error.tooLong")
    }

    "return invalid characters error when special characters are supplied" in {
      val result = form.bind(
        Map("cgtReference" -> "XACGTP12345@789")
      )

      result.errors.map(_.message) should contain("error.invalidCharacters")
    }
  }
}
