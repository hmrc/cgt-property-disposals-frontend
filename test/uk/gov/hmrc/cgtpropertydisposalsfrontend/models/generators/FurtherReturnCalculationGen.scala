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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators

import org.scalacheck.Gen
import org.scalacheck.ScalacheckShapeless._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{CalculatedGlarBreakdown, TaxableGainOrLossCalculation, TaxableGainOrLossCalculationRequest, YearToDateLiabilityCalculation, YearToDateLiabilityCalculationRequest}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.FurtherReturnCalculationEligibility.{Eligible, Ineligible}

object FurtherReturnCalculationGen extends GenUtils {

  implicit val calculatedGlarBreakdownGen: Gen[CalculatedGlarBreakdown] =
    gen[CalculatedGlarBreakdown]

  implicit val eligibleGen: Gen[Eligible] = gen[Eligible]

  implicit val ineligibleGen: Gen[Ineligible] = gen[Ineligible]

  implicit val taxableGainOrLossCalculationRequestGen: Gen[TaxableGainOrLossCalculationRequest] =
    gen[TaxableGainOrLossCalculationRequest]

  implicit val taxableGainOrLossCalculationGen: Gen[TaxableGainOrLossCalculation] =
    gen[TaxableGainOrLossCalculation]

  implicit val yearToDateLiabilityCalculationRequestGen: Gen[YearToDateLiabilityCalculationRequest] =
    gen[YearToDateLiabilityCalculationRequest]

  implicit val yearToDateLiabilityCalculationGen: Gen[YearToDateLiabilityCalculation] =
    gen[YearToDateLiabilityCalculation]

}
