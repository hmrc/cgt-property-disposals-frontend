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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns

import cats.Eq
import play.api.libs.json.{Json, OFormat}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.TaxYear

final case class TaxYearExchanged(year: Int) extends Product with Serializable

object TaxYearExchanged {
  val currentTaxYear: Int = TaxYear.thisTaxYearStartDate().getYear

  // Cutoff tax year must be current tax year minus 4 years
  val cutoffTaxYear: Int                         = currentTaxYear - 4
  val taxYearExchangedTooEarly: TaxYearExchanged = TaxYearExchanged(-cutoffTaxYear)

  val differentTaxYears: TaxYearExchanged = TaxYearExchanged(-1)

  implicit val format: OFormat[TaxYearExchanged] = Json.format[TaxYearExchanged]
  implicit val eq: Eq[TaxYearExchanged]          = Eq.fromUniversalEquals
}
