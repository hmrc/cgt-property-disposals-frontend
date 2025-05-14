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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators

import org.scalacheck.Gen
import io.github.martinhh.derived.scalacheck.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.AddressGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.FurtherReturnCalculationEligibility.{Eligible, Ineligible}

object FurtherReturnCalculationGen extends GenUtils:

  given calculatedGlarBreakdownGen: Gen[CalculatedGlarBreakdown] = gen[CalculatedGlarBreakdown]

  given Gen[FurtherReturnCalculationData] = gen[FurtherReturnCalculationData]

  given eligibleGen: Gen[Eligible] = gen[Eligible]

  given ineligibleGen: Gen[Ineligible] = gen[Ineligible]

  given taxableGainOrLossCalculationRequestGen: Gen[TaxableGainOrLossCalculationRequest] =
    gen[TaxableGainOrLossCalculationRequest]

  given taxableGainOrLossCalculationGen: Gen[TaxableGainOrLossCalculation] =
    gen[TaxableGainOrLossCalculation]

  given yearToDateLiabilityCalculationRequestGen: Gen[YearToDateLiabilityCalculationRequest] =
    gen[YearToDateLiabilityCalculationRequest]

  given yearToDateLiabilityCalculationGen: Gen[YearToDateLiabilityCalculation] =
    gen[YearToDateLiabilityCalculation]
