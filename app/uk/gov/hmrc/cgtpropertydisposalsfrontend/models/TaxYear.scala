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

import cats.syntax.order._
import play.api.libs.json.{Json, OFormat}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.TimeUtils.localDateOrder
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.AmountInPence

import java.time.LocalDate

final case class TaxYear(
  startDateInclusive: LocalDate,
  endDateExclusive: LocalDate,
  annualExemptAmountGeneral: AmountInPence,
  annualExemptAmountNonVulnerableTrust: AmountInPence,
  personalAllowance: AmountInPence,
  maxPersonalAllowance: AmountInPence,
  higherIncomePersonalAllowanceThreshold: AmountInPence,
  incomeTaxHigherRateThreshold: AmountInPence,
  cgtRateLowerBandResidential: BigDecimal,
  cgtRateLowerBandNonResidential: BigDecimal,
  cgtRateHigherBandResidential: BigDecimal,
  cgtRateHigherBandNonResidential: BigDecimal,
  maxLettingsReliefAmount: AmountInPence
)

object TaxYear {

  def thisTaxYearStartDate(): LocalDate = {
    val today     = TimeUtils.today()
    val startYear =
      if (today >= LocalDate.of(today.getYear, 4, 6)) today.getYear else today.getYear - 1

    LocalDate.of(startYear, 4, 6)
  }

  val earliestTaxYearStartDate: LocalDate = LocalDate.of(2020, 4, 6)

  implicit val format: OFormat[TaxYear] = Json.format

  implicit class TaxYearOps(private val taxTear: TaxYear) extends AnyVal {

    def isItInLatestTaxYear(futureTaxYearsEnabled: Boolean = false): Boolean = {
      val today =
        if (futureTaxYearsEnabled) TimeUtils.today().plusYears(1L) else TimeUtils.today()
      today < taxTear.endDateExclusive && today >= taxTear.startDateInclusive
    }

    def contains(date: LocalDate): Boolean =
      date >= taxTear.startDateInclusive && date < taxTear.endDateExclusive
  }

}
