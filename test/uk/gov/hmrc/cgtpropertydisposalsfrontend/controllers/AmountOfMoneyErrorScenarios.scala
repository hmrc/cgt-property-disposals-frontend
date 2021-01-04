/*
 * Copyright 2021 HM Revenue & Customs
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

import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.MoneyUtils

object AmountOfMoneyErrorScenarios {

  final case class AmountOfMoneyErrorScenario(
    key: String,
    input: Option[String],
    expectedErrorMessageKey: String,
    errorContext: Option[String] = None
  ) {
    val formData: List[(String, String)] =
      List(input).collect { case Some(v) => key -> v }
  }

  def amountOfMoneyErrorScenarios(
    key: String,
    maximumAmountInclusive: BigDecimal = MoneyUtils.maxAmountOfPounds,
    errorContext: Option[String] = None
  ) =
    List(
      AmountOfMoneyErrorScenario(
        key,
        None,
        s"${errorContext.getOrElse(key)}.error.required"
      ),
      AmountOfMoneyErrorScenario(
        key,
        Some(""),
        s"${errorContext.getOrElse(key)}.error.required"
      ),
      AmountOfMoneyErrorScenario(
        key,
        Some("-1"),
        s"${errorContext.getOrElse(key)}.error.tooSmall"
      ),
      AmountOfMoneyErrorScenario(
        key,
        Some((maximumAmountInclusive + 1).toString),
        s"${errorContext.getOrElse(key)}.error.tooLarge"
      ),
      AmountOfMoneyErrorScenario(
        key,
        Some("1.234"),
        s"${errorContext.getOrElse(key)}.error.tooManyDecimals"
      ),
      AmountOfMoneyErrorScenario(
        key,
        Some("abc"),
        s"${errorContext.getOrElse(key)}.error.invalid"
      )
    )

}
