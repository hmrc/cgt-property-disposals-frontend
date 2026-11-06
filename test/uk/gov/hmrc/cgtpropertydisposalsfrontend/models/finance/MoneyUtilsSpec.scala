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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.data.FormError

class MoneyUtilsSpec extends AnyWordSpec with Matchers {
  "MoneyUtils" must {
    "have a method which validates the amount of money" when {
      val key                               = "amount"
      val isTooSmall: BigDecimal => Boolean = _ < BigDecimal(1)
      val isTooLarge: BigDecimal => Boolean = _.bigDecimal.precision > 30
      "the total characters entered is more than 20 shows invalid error message" in {
        MoneyUtils.validateAmountOfMoney("amount", isTooSmall, isTooLarge)("11".repeat(20)) shouldBe Left(
          FormError(key, "error.invalid")
        )
      }
      "the total characters entered is more less than 20 no invalid error message" in {
        MoneyUtils.validateAmountOfMoney("amount", isTooSmall, isTooLarge)("12345") shouldBe Right(12345)
      }
    }
  }
}
