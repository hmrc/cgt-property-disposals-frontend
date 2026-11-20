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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class CountrySpec extends AnyWordSpec with Matchers {

  "Country formatter" should {

    "bind a valid country code" in {
      val result = Country.formatter.bind(
        "country",
        Map("country" -> "IN")
      )

      result shouldBe Right(Country("IN"))
    }

    "return error.notFound for an invalid country code" in {
      val result = Country.formatter.bind(
        "country",
        Map("country" -> "XYZ")
      )

      result.left.toOption.get.head.message shouldBe "error.notFound"
    }

    "reject input exceeding 10 characters" in {
      val result = Country.formatter.bind(
        "country",
        Map("country" -> "12345678901")
      )

      result.left.toOption.get.head.message shouldBe "error.notFound"
    }

    "return error.required for empty input" in {
      val result = Country.formatter.bind(
        "country",
        Map("country" -> "")
      )

      result.left.toOption.get.head.message shouldBe "error.required"
    }

    "return error.required when country is missing" in {
      val result = Country.formatter.bind(
        "country",
        Map.empty
      )
      result.left.toOption.get.head.message shouldBe "error.required"
    }
  }
}
