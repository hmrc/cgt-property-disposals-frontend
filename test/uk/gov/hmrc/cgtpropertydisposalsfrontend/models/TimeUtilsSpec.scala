/*
 * Copyright 2023 HM Revenue & Customs
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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.models

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.time.LocalDate

class TimeUtilsSpec extends AnyWordSpec with Matchers {

  "TimeUtils" must {

    "have a method which works out the start date of the tax year a given year falls in" when {

      "the given date is strictly before the start of the tax year in calendar year the given date falls in" in {
        TimeUtils.taxYearStart(LocalDate.of(2000, 4, 5)) shouldBe LocalDate.of(1999, 4, 6)
      }

      "the given date is on the start of the tax year in the calendar year the given date falls in" in {
        TimeUtils.taxYearStart(LocalDate.of(2000, 4, 6)) shouldBe LocalDate.of(2000, 4, 6)
      }

      "the given date is strictly after the start of the tax year in the calendar year the given date falls in" in {
        TimeUtils.taxYearStart(LocalDate.of(2000, 4, 7)) shouldBe LocalDate.of(2000, 4, 6)
      }
    }

  }

}
