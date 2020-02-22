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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns

import org.scalatest.{Matchers, WordSpec}
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.AcquisitionDetailsAnswers.CompleteAcquisitionDetailsAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.CalculatedTaxDue.{GainCalculatedTaxDue, NonGainCalculatedTaxDue}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.DisposalDetailsAnswers.CompleteDisposalDetailsAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{AmountInPence, TaxYear}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{AssetType, CalculatedTaxDue, DisposalDate, OtherReliefsOption, TaxableAmountOfMoney}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ExemptionAndLossesAnswers.CompleteExemptionAndLossesAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.TriageAnswers.CompleteTriageAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ReliefDetailsAnswers.CompleteReliefDetailsAnswers

class CgtCalculationServiceImplSpec extends WordSpec with Matchers with ScalaCheckDrivenPropertyChecks {

  val service = new CgtCalculationServiceImpl

  "CgtCalculationServiceImpl" when {

    "calculating tax due" must {
      val completeTriageAnswers = sample[CompleteTriageAnswers]

      val zeroDisposalDetails = sample[CompleteDisposalDetailsAnswers].copy(
        disposalPrice = AmountInPence.zero,
        disposalFees  = AmountInPence.zero
      )
      val zeroAcquisitionDetails = sample[CompleteAcquisitionDetailsAnswers].copy(
        acquisitionPrice        = AmountInPence.zero,
        improvementCosts        = AmountInPence.zero,
        acquisitionFees         = AmountInPence.zero,
        rebasedAcquisitionPrice = None
      )

      val zeroReliefDetails = CompleteReliefDetailsAnswers(
        AmountInPence.zero,
        AmountInPence.zero,
        None
      )

      val zeroExemptionsAndLosses = CompleteExemptionAndLossesAnswers(
        AmountInPence.zero,
        AmountInPence.zero,
        AmountInPence.zero,
        None
      )

      def calculate(
        triageAnswers: CompleteTriageAnswers                  = completeTriageAnswers,
        disposalDetails: CompleteDisposalDetailsAnswers       = zeroDisposalDetails,
        acquisitionDetails: CompleteAcquisitionDetailsAnswers = zeroAcquisitionDetails,
        reliefDetails: CompleteReliefDetailsAnswers           = zeroReliefDetails,
        exemptionAndLosses: CompleteExemptionAndLossesAnswers = zeroExemptionsAndLosses,
        estimatedIncome: AmountInPence                        = sample[AmountInPence],
        personalAllowance: AmountInPence                      = sample[AmountInPence]
      ) =
        service.calculateTaxDue(
          triageAnswers,
          disposalDetails,
          acquisitionDetails,
          reliefDetails,
          exemptionAndLosses,
          estimatedIncome,
          personalAllowance
        )

      "calculate disposal amount less costs correctly" in {
        forAll { (disposalPrice: AmountInPence, disposalFees: AmountInPence) =>
          val result =
            calculate(
              disposalDetails =
                sample[CompleteDisposalDetailsAnswers].copy(disposalPrice = disposalPrice, disposalFees = disposalFees)
            )

          result.disposalAmountLessCosts.value shouldBe (disposalPrice.value - disposalFees.value)
        }
      }

      "calculate acquisition amount plus costs correctly when the user has not rebased" in {
        forAll { (acquisitionPrice: AmountInPence, improvementCosts: AmountInPence, acquisitionFees: AmountInPence) =>
          val result =
            calculate(
              acquisitionDetails = sample[CompleteAcquisitionDetailsAnswers].copy(
                acquisitionPrice        = acquisitionPrice,
                improvementCosts        = improvementCosts,
                acquisitionFees         = acquisitionFees,
                rebasedAcquisitionPrice = None
              )
            )

          result.acquisitionAmountPlusCosts.value shouldBe (acquisitionPrice.value + improvementCosts.value + acquisitionFees.value)
        }
      }

      "calculate acquisition amount plus costs correctly when the user has rebased" in {
        forAll {
          (
            acquisitionPrice: AmountInPence,
            rebasedAcquisitionPrice: AmountInPence,
            improvementCosts: AmountInPence,
            acquisitionFees: AmountInPence
          ) =>
            val result =
              calculate(
                acquisitionDetails = zeroAcquisitionDetails.copy(
                  acquisitionPrice        = acquisitionPrice,
                  improvementCosts        = improvementCosts,
                  acquisitionFees         = acquisitionFees,
                  rebasedAcquisitionPrice = Some(rebasedAcquisitionPrice)
                )
              )

            result.acquisitionAmountPlusCosts.value shouldBe (rebasedAcquisitionPrice.value + improvementCosts.value + acquisitionFees.value)
        }
      }

      "calculate initial gain or loss correctly" in {
        forAll {
          (disposalDetails: CompleteDisposalDetailsAnswers, acquisitionDetails: CompleteAcquisitionDetailsAnswers) =>
            val result = calculate(disposalDetails = disposalDetails, acquisitionDetails = acquisitionDetails)

            result.initialGainOrLoss.value shouldBe (result.disposalAmountLessCosts.value - result.acquisitionAmountPlusCosts.value)
        }
      }

      "calculate total reliefs correctly" when {

        "the user has not had the option to enter other reliefs" in {
          forAll { (privateResidentsRelief: AmountInPence, lettingRelief: AmountInPence) =>
            val result = calculate(
              reliefDetails = CompleteReliefDetailsAnswers(privateResidentsRelief, lettingRelief, None)
            )
            result.totalReliefs.value shouldBe (privateResidentsRelief.value + lettingRelief.value)
          }
        }

        "the user has had the option to enter other reliefs and has chosen not to" in {
          forAll { (privateResidentsRelief: AmountInPence, lettingRelief: AmountInPence) =>
            val result = calculate(
              reliefDetails = CompleteReliefDetailsAnswers(
                privateResidentsRelief,
                lettingRelief,
                Some(OtherReliefsOption.NoOtherReliefs)
              )
            )
            result.totalReliefs.value shouldBe (privateResidentsRelief.value + lettingRelief.value)
          }
        }

        "the user has had the option to enter other reliefs and has chosen to" in {
          forAll {
            (
              privateResidentsRelief: AmountInPence,
              lettingRelief: AmountInPence,
              otherReliefs: OtherReliefsOption.OtherReliefs
            ) =>
              val result = calculate(
                reliefDetails = CompleteReliefDetailsAnswers(privateResidentsRelief, lettingRelief, Some(otherReliefs))
              )
              result.totalReliefs.value shouldBe (privateResidentsRelief.value + lettingRelief.value + otherReliefs.amount.value)
          }
        }

      }

      "calculate gain or loss after reliefs correctly" when {

        "there is an initial gain and" when {

          val disposalDetails = zeroDisposalDetails.copy(
            disposalPrice = AmountInPence(100L)
          )

          "the initial gain is greater than the total reliefs" in {
            val reliefDetails = CompleteReliefDetailsAnswers(
              AmountInPence(1L),
              AmountInPence(1L),
              None
            )
            val result = calculate(
              disposalDetails    = disposalDetails,
              acquisitionDetails = zeroAcquisitionDetails,
              reliefDetails      = reliefDetails
            )

            result.initialGainOrLoss.value      shouldBe 100L
            result.gainOrLossAfterReliefs.value shouldBe 98L
          }

          "the initial gain is less than the total reliefs" in {
            val reliefDetails = CompleteReliefDetailsAnswers(
              AmountInPence(100L),
              AmountInPence(100L),
              None
            )
            val result = calculate(
              disposalDetails    = disposalDetails,
              acquisitionDetails = zeroAcquisitionDetails,
              reliefDetails      = reliefDetails
            )

            result.initialGainOrLoss.value      shouldBe 100L
            result.gainOrLossAfterReliefs.value shouldBe 0L
          }
        }
        "there is an initial loss and" when {

          val acquisitionDetails = zeroAcquisitionDetails.copy(
            acquisitionPrice = AmountInPence(100L)
          )

          "the absolute value of the initial loss is greater than the total reliefs" in {
            val reliefDetails = CompleteReliefDetailsAnswers(
              AmountInPence(1L),
              AmountInPence(1L),
              None
            )
            val result = calculate(
              disposalDetails    = zeroDisposalDetails,
              acquisitionDetails = acquisitionDetails,
              reliefDetails      = reliefDetails
            )

            result.initialGainOrLoss.value      shouldBe -100L
            result.gainOrLossAfterReliefs.value shouldBe -98L
          }

          "the absolute value of the initial loss is less than the total reliefs" in {
            val reliefDetails = CompleteReliefDetailsAnswers(
              AmountInPence(100L),
              AmountInPence(100L),
              None
            )
            val result = calculate(
              disposalDetails    = zeroDisposalDetails,
              acquisitionDetails = acquisitionDetails,
              reliefDetails      = reliefDetails
            )

            result.initialGainOrLoss.value      shouldBe -100L
            result.gainOrLossAfterReliefs.value shouldBe 0L
          }
        }
        "there is neither an initial gain or less" in {
          val reliefDetails = CompleteReliefDetailsAnswers(
            AmountInPence(1L),
            AmountInPence(1L),
            None
          )
          val result = calculate(
            disposalDetails    = zeroDisposalDetails,
            acquisitionDetails = zeroAcquisitionDetails,
            reliefDetails      = reliefDetails
          )

          result.initialGainOrLoss.value      shouldBe 0L
          result.gainOrLossAfterReliefs.value shouldBe 0L
        }

      }

      "calculate total losses correctly" when {

        "the gain or loss after reliefs is negative" in {
          val acquisitionDetails = zeroAcquisitionDetails.copy(
            acquisitionPrice = AmountInPence(100L)
          )

          val result = calculate(
            disposalDetails    = zeroDisposalDetails,
            acquisitionDetails = acquisitionDetails,
            reliefDetails      = zeroReliefDetails,
            exemptionAndLosses = zeroExemptionsAndLosses.copy(
              inYearLosses        = AmountInPence(1L),
              previousYearsLosses = AmountInPence(3L)
            )
          )

          result.gainOrLossAfterReliefs.value shouldBe -100L
          result.totalLosses.value            shouldBe 1L
        }

        "the gain or loss after reliefs is positive" in {
          val disposalDetails = zeroDisposalDetails.copy(
            disposalPrice = AmountInPence(100L)
          )

          val result = calculate(
            disposalDetails    = disposalDetails,
            acquisitionDetails = zeroAcquisitionDetails,
            reliefDetails      = zeroReliefDetails,
            exemptionAndLosses = zeroExemptionsAndLosses.copy(
              inYearLosses        = AmountInPence(1L),
              previousYearsLosses = AmountInPence(3L)
            )
          )

          result.gainOrLossAfterReliefs.value shouldBe 100L
          result.totalLosses.value            shouldBe 4L
        }

        "the gain or loss after reliefs is zero" in {
          val result = calculate(
            disposalDetails    = zeroDisposalDetails,
            acquisitionDetails = zeroAcquisitionDetails,
            reliefDetails      = zeroReliefDetails,
            exemptionAndLosses = zeroExemptionsAndLosses.copy(
              inYearLosses        = AmountInPence(1L),
              previousYearsLosses = AmountInPence(3L)
            )
          )

          result.gainOrLossAfterReliefs.value shouldBe 0L
          // previous years losses can't be applied since gain or loss after relieds
          // is less than in year losses
          result.totalLosses.value shouldBe 1L
        }

        "the gain or loss after reliefs is strictly less than the in year losses" in {
          val disposalDetails = zeroDisposalDetails.copy(
            disposalPrice = AmountInPence(100L)
          )

          val result = calculate(
            disposalDetails    = disposalDetails,
            acquisitionDetails = zeroAcquisitionDetails,
            reliefDetails      = zeroReliefDetails,
            exemptionAndLosses = zeroExemptionsAndLosses.copy(
              inYearLosses        = AmountInPence(200L),
              previousYearsLosses = AmountInPence(3L)
            )
          )

          result.gainOrLossAfterReliefs.value shouldBe 100L
          result.totalLosses.value            shouldBe 200L
        }

        "the gain or loss after reliefs is strictly greater than the in year losses" in {
          val disposalDetails = zeroDisposalDetails.copy(
            disposalPrice = AmountInPence(100L)
          )

          val result = calculate(
            disposalDetails    = disposalDetails,
            acquisitionDetails = zeroAcquisitionDetails,
            reliefDetails      = zeroReliefDetails,
            exemptionAndLosses = zeroExemptionsAndLosses.copy(
              inYearLosses        = AmountInPence(1L),
              previousYearsLosses = AmountInPence(3L)
            )
          )

          result.gainOrLossAfterReliefs.value shouldBe 100L
          result.totalLosses.value            shouldBe 4L
        }

        "the gain or loss after reliefs is equal to the in year losses" in {
          val disposalDetails = zeroDisposalDetails.copy(
            disposalPrice = AmountInPence(100L)
          )

          val result = calculate(
            disposalDetails    = disposalDetails,
            acquisitionDetails = zeroAcquisitionDetails,
            reliefDetails      = zeroReliefDetails,
            exemptionAndLosses = zeroExemptionsAndLosses.copy(
              inYearLosses        = AmountInPence(100L),
              previousYearsLosses = AmountInPence(3L)
            )
          )

          result.gainOrLossAfterReliefs.value shouldBe 100L
          result.totalLosses.value            shouldBe 103L
        }

      }

      "calculate gain or loss after losses correctly" when {

        "the gain or loss after reliefs is positive and is greater than the " +
          "total losses" in {
          val disposalDetails = zeroDisposalDetails.copy(
            disposalPrice = AmountInPence(100L)
          )

          val result = calculate(
            disposalDetails    = disposalDetails,
            acquisitionDetails = zeroAcquisitionDetails,
            reliefDetails      = zeroReliefDetails,
            exemptionAndLosses = zeroExemptionsAndLosses.copy(
              inYearLosses        = AmountInPence(1L),
              previousYearsLosses = AmountInPence(2L)
            )
          )

          result.gainOrLossAfterReliefs.value shouldBe 100L
          result.totalLosses.value            shouldBe 3L
          result.gainOrLossAfterLosses.value  shouldBe 97L
        }

        "the gain or loss after reliefs is positive and is less than the " +
          "total losses" in {
          val disposalDetails = zeroDisposalDetails.copy(
            disposalPrice = AmountInPence(100L)
          )

          val result = calculate(
            disposalDetails    = disposalDetails,
            acquisitionDetails = zeroAcquisitionDetails,
            reliefDetails      = zeroReliefDetails,
            exemptionAndLosses = zeroExemptionsAndLosses.copy(
              inYearLosses        = AmountInPence(1L),
              previousYearsLosses = AmountInPence(200L)
            )
          )

          result.gainOrLossAfterReliefs.value shouldBe 100L
          result.totalLosses.value            shouldBe 201L
          result.gainOrLossAfterLosses.value  shouldBe 0L
        }

        "the gain or loss after reliefs is negative and its absolute value " +
          "is greater than the in year losses" in {
          val acquisitionDetails = zeroAcquisitionDetails.copy(
            acquisitionPrice = AmountInPence(100L)
          )

          val result = calculate(
            disposalDetails    = zeroDisposalDetails,
            acquisitionDetails = acquisitionDetails,
            reliefDetails      = zeroReliefDetails,
            exemptionAndLosses = zeroExemptionsAndLosses.copy(
              inYearLosses        = AmountInPence(1L),
              previousYearsLosses = AmountInPence(2L)
            )
          )

          result.gainOrLossAfterReliefs.value shouldBe -100L
          result.totalLosses.value            shouldBe 1L
          result.gainOrLossAfterLosses.value  shouldBe -99L
        }

        "the gain or loss after reliefs is negative and its absolute value " +
          "is less than the in year losses" in {
          val acquisitionDetails = zeroAcquisitionDetails.copy(
            acquisitionPrice = AmountInPence(100L)
          )

          val result = calculate(
            disposalDetails    = zeroDisposalDetails,
            acquisitionDetails = acquisitionDetails,
            reliefDetails      = zeroReliefDetails,
            exemptionAndLosses = zeroExemptionsAndLosses.copy(
              inYearLosses        = AmountInPence(200L),
              previousYearsLosses = AmountInPence(2L)
            )
          )

          result.gainOrLossAfterReliefs.value shouldBe -100L
          result.totalLosses.value            shouldBe 200L
          result.gainOrLossAfterLosses.value  shouldBe 0L
        }

        "the gain or loss after reliefs is zero" in {
          val result = calculate(
            disposalDetails    = zeroDisposalDetails,
            acquisitionDetails = zeroAcquisitionDetails,
            reliefDetails      = zeroReliefDetails,
            exemptionAndLosses = zeroExemptionsAndLosses.copy(
              inYearLosses        = AmountInPence(1L),
              previousYearsLosses = AmountInPence(2L)
            )
          )

          result.gainOrLossAfterReliefs.value shouldBe 0L
          result.totalLosses.value            shouldBe 1L
          result.gainOrLossAfterLosses.value  shouldBe 0L
        }

      }

      "calculate taxable gain or net loss correctly" when {

        "the gain or loss after losses is negative" in {
          val acquisitionDetails = zeroAcquisitionDetails.copy(
            acquisitionPrice = AmountInPence(100L)
          )

          val result = calculate(
            disposalDetails    = zeroDisposalDetails,
            acquisitionDetails = acquisitionDetails,
            reliefDetails      = zeroReliefDetails,
            exemptionAndLosses = zeroExemptionsAndLosses.copy(
              annualExemptAmount = AmountInPence(1L)
            )
          )

          result                             shouldBe a[NonGainCalculatedTaxDue]
          result.gainOrLossAfterLosses.value shouldBe -100L
          result.taxableGainOrNetLoss.value  shouldBe -100L
        }

        "the gain or loss after losses is zero" in {
          val result = calculate(
            disposalDetails    = zeroDisposalDetails,
            acquisitionDetails = zeroAcquisitionDetails,
            reliefDetails      = zeroReliefDetails,
            exemptionAndLosses = zeroExemptionsAndLosses.copy(
              annualExemptAmount = AmountInPence(1L)
            )
          )

          result                             shouldBe a[NonGainCalculatedTaxDue]
          result.gainOrLossAfterLosses.value shouldBe 0L
          result.taxableGainOrNetLoss.value  shouldBe 0L
        }

        "the gain or loss after losses is positive and is greater than " +
          "the annual exempt amount used" in {
          val disposalDetails = zeroDisposalDetails.copy(
            disposalPrice = AmountInPence(100L)
          )

          val result = calculate(
            disposalDetails    = disposalDetails,
            acquisitionDetails = zeroAcquisitionDetails,
            reliefDetails      = zeroReliefDetails,
            exemptionAndLosses = zeroExemptionsAndLosses.copy(
              annualExemptAmount = AmountInPence(1L)
            )
          )

          result                             shouldBe a[GainCalculatedTaxDue]
          result.gainOrLossAfterLosses.value shouldBe 100L
          result.taxableGainOrNetLoss.value  shouldBe 99L
        }

        "the gain or loss after losses is positive and is less than " +
          "the annual exempt amount used" in {
          val disposalDetails = zeroDisposalDetails.copy(
            disposalPrice = AmountInPence(100L)
          )

          val result = calculate(
            disposalDetails    = disposalDetails,
            acquisitionDetails = zeroAcquisitionDetails,
            reliefDetails      = zeroReliefDetails,
            exemptionAndLosses = zeroExemptionsAndLosses.copy(
              annualExemptAmount = AmountInPence(200L)
            )
          )

          result                             shouldBe a[NonGainCalculatedTaxDue]
          result.gainOrLossAfterLosses.value shouldBe 100L
          result.taxableGainOrNetLoss.value  shouldBe 0L
        }

      }

      "calculate taxable income correctly" when {

        "the estimated income is greater than or equal to the persona allowance used" in {
          forAll { (estimatedIncome: AmountInPence, personalAllowance: AmountInPence) =>
            whenever(estimatedIncome.value - personalAllowance.value >= 0L) {
              val disposalDetails = zeroDisposalDetails.copy(disposalPrice = AmountInPence(100L))
              val result = calculate(
                disposalDetails   = disposalDetails,
                estimatedIncome   = estimatedIncome,
                personalAllowance = personalAllowance
              )

              testOnGainCalculatedTaxDue(result) {
                _.taxableIncome.value shouldBe (estimatedIncome.value - personalAllowance.value)
              }
            }
          }
        }

        "the estimated income is less to the persona allowance used" in {
          forAll { (estimatedIncome: AmountInPence, personalAllowance: AmountInPence) =>
            whenever(estimatedIncome.value - personalAllowance.value < 0L) {
              val disposalDetails = zeroDisposalDetails.copy(disposalPrice = AmountInPence(100L))
              val result = calculate(
                disposalDetails   = disposalDetails,
                estimatedIncome   = estimatedIncome,
                personalAllowance = personalAllowance
              )

              testOnGainCalculatedTaxDue(result) {
                _.taxableIncome.value shouldBe 0L
              }
            }
          }
        }

      }

      "calculate the amount to be taxed at the lower band rate" when {

        def test(assetType: AssetType, expectedLowerBandRate: TaxYear => BigDecimal): Unit = {
          val incomeTaxHigherBandThreshold = AmountInPence(1000L)
          val taxYear                      = sample[TaxYear].copy(incomeTaxHigherRateThreshold = incomeTaxHigherBandThreshold)

          val triageAnswers = sample[CompleteTriageAnswers].copy(
            disposalDate = sample[DisposalDate].copy(taxYear = taxYear),
            assetType    = assetType
          )

          "the taxable gain is less than the income tax higher rate threshold minus " +
            "the taxable income" in {
            val disposalDetails =
              zeroDisposalDetails.copy(disposalPrice = AmountInPence(500L))

            val result = calculate(
              triageAnswers     = triageAnswers,
              disposalDetails   = disposalDetails,
              estimatedIncome   = AmountInPence(100L),
              personalAllowance = AmountInPence.zero
            )

            testOnGainCalculatedTaxDue(result) { calculated =>
              calculated.taxableGainOrNetLoss.value shouldBe 500L
              calculated.taxableIncome.value        shouldBe 100L
              calculated.taxDueAtLowerRate shouldBe TaxableAmountOfMoney(
                expectedLowerBandRate(taxYear),
                AmountInPence(500L)
              )

            }

          }

          "the taxable gain is greater than the income tax higher rate threshold minus " +
            "the taxable income" in {
            val disposalDetails =
              zeroDisposalDetails.copy(disposalPrice = AmountInPence(500L))

            val result = calculate(
              triageAnswers     = triageAnswers,
              disposalDetails   = disposalDetails,
              estimatedIncome   = AmountInPence(600L),
              personalAllowance = AmountInPence.zero
            )

            testOnGainCalculatedTaxDue(result) { calculated =>
              calculated.taxableGainOrNetLoss.value shouldBe 500L
              calculated.taxableIncome.value        shouldBe 600L
              calculated.taxDueAtLowerRate shouldBe TaxableAmountOfMoney(
                expectedLowerBandRate(taxYear),
                AmountInPence(400L)
              )

            }
          }

          "the taxable gain is equal to the income tax higher rate threshold minus " +
            "the taxable income" in {
            val disposalDetails =
              zeroDisposalDetails.copy(disposalPrice = AmountInPence(500L))

            val result = calculate(
              triageAnswers     = triageAnswers,
              disposalDetails   = disposalDetails,
              estimatedIncome   = AmountInPence(500L),
              personalAllowance = AmountInPence.zero
            )

            testOnGainCalculatedTaxDue(result) { calculated =>
              calculated.taxableGainOrNetLoss.value shouldBe 500L
              calculated.taxableIncome.value        shouldBe 500L
              calculated.taxDueAtLowerRate shouldBe TaxableAmountOfMoney(
                expectedLowerBandRate(taxYear),
                AmountInPence(500L)
              )
            }
          }

          "the income tax higher rate threshold minus the taxable income is negative" in {
            val disposalDetails =
              zeroDisposalDetails.copy(disposalPrice = AmountInPence(500L))

            val result = calculate(
              triageAnswers     = triageAnswers,
              disposalDetails   = disposalDetails,
              estimatedIncome   = AmountInPence(2000L),
              personalAllowance = AmountInPence.zero
            )

            testOnGainCalculatedTaxDue(result) { calculated =>
              calculated.taxableGainOrNetLoss.value shouldBe 500L
              calculated.taxableIncome.value        shouldBe 2000L
              calculated.taxDueAtLowerRate shouldBe TaxableAmountOfMoney(
                expectedLowerBandRate(taxYear),
                AmountInPence(0L)
              )
            }
          }

          "the income tax higher rate threshold minus the taxable income is zero" in {
            val disposalDetails =
              zeroDisposalDetails.copy(disposalPrice = AmountInPence(500L))

            val result = calculate(
              triageAnswers     = triageAnswers,
              disposalDetails   = disposalDetails,
              estimatedIncome   = AmountInPence(1000L),
              personalAllowance = AmountInPence.zero
            )

            testOnGainCalculatedTaxDue(result) { calculated =>
              calculated.taxableGainOrNetLoss.value shouldBe 500L
              calculated.taxableIncome.value        shouldBe 1000L
              calculated.taxDueAtLowerRate shouldBe TaxableAmountOfMoney(
                expectedLowerBandRate(taxYear),
                AmountInPence(0L)
              )
            }
          }

        }

        "the asset type is residential and" when {

          behave like test(
            AssetType.Residential,
            _.cgtRateLowerBandResidential
          )

        }

        "the asset type is non-residential and" when {

          behave like test(
            AssetType.NonResidential,
            _.cgtRateLowerBandNonResidential
          )

        }

      }

      "calculate the amount to be taxed at the higher band rate" when {

        def test(assetType: AssetType, expectedHigherRate: TaxYear => BigDecimal): Unit = {
          val incomeTaxHigherRateThreshold = AmountInPence(1000L)
          val taxYear                      = sample[TaxYear].copy(incomeTaxHigherRateThreshold = incomeTaxHigherRateThreshold)
          val triageAnswers = sample[CompleteTriageAnswers].copy(
            disposalDate = sample[DisposalDate].copy(taxYear = taxYear),
            assetType    = assetType
          )

          "the taxable gain is greater than the amount to be taxed at the lower rate" in {
            val disposalDetails = zeroDisposalDetails.copy(disposalPrice = AmountInPence(500L))
            val taxableIncome   = AmountInPence(600L)

            testOnGainCalculatedTaxDue(
              calculate(
                triageAnswers     = triageAnswers,
                disposalDetails   = disposalDetails,
                estimatedIncome   = taxableIncome,
                personalAllowance = AmountInPence.zero
              )
            ) { result =>
              result.taxableGainOrNetLoss.value            shouldBe 500L
              result.taxableIncome.value                   shouldBe 600L
              result.taxDueAtLowerRate.taxableAmount.value shouldBe 400L
              result.taxDueAtHigherRate shouldBe TaxableAmountOfMoney(
                expectedHigherRate(taxYear),
                AmountInPence(100L)
              )
            }

          }

          "the taxable gain is equal than the amount to be taxed at the lower rate" in {
            val disposalDetails = zeroDisposalDetails.copy(disposalPrice = AmountInPence(500L))
            val taxableIncome   = AmountInPence(500L)

            testOnGainCalculatedTaxDue(
              calculate(
                triageAnswers     = triageAnswers,
                disposalDetails   = disposalDetails,
                estimatedIncome   = taxableIncome,
                personalAllowance = AmountInPence.zero
              )
            ) { result =>
              result.taxableGainOrNetLoss.value            shouldBe 500L
              result.taxableIncome.value                   shouldBe 500L
              result.taxDueAtLowerRate.taxableAmount.value shouldBe 500L
              result.taxDueAtHigherRate shouldBe TaxableAmountOfMoney(
                expectedHigherRate(taxYear),
                AmountInPence(0L)
              )
            }
          }
        }

        "the asset type is residential and" when {

          behave like test(AssetType.Residential, _.cgtRateHigherBandResidential)
        }

        "the asset type is non-residential and" when {

          behave like test(AssetType.NonResidential, _.cgtRateHigherBandNonResidential)
        }

      }

      "calculate the amount of tax due correctly" when {

        "a loss has been made" in {
          val acquisitionDetails = zeroAcquisitionDetails.copy(
            acquisitionPrice = AmountInPence(10L)
          )

          val result = calculate(
            acquisitionDetails = acquisitionDetails
          )

          result                            shouldBe a[NonGainCalculatedTaxDue]
          result.taxableGainOrNetLoss.value shouldBe -10L
          result.amountOfTaxDue.value       shouldBe 0L
          result.yearToDateLiability.value  shouldBe 0L
        }

        "the return is a nil-return" in {
          val result = calculate()

          result                            shouldBe a[NonGainCalculatedTaxDue]
          result.taxableGainOrNetLoss.value shouldBe 0L
          result.amountOfTaxDue.value       shouldBe 0L
          result.yearToDateLiability.value  shouldBe 0L
        }

        "a gain has been made" in {
          val incomeTaxHigherRateThreshold = AmountInPence(1000L)
          val lowerBandTaxRate             = BigDecimal("1.23")
          val higherBandTaxRate            = BigDecimal("4.56")

          val taxYear = sample[TaxYear].copy(
            incomeTaxHigherRateThreshold = incomeTaxHigherRateThreshold,
            cgtRateLowerBandResidential  = lowerBandTaxRate,
            cgtRateHigherBandResidential = higherBandTaxRate
          )

          val triageAnswers = sample[CompleteTriageAnswers].copy(
            disposalDate = sample[DisposalDate].copy(taxYear = taxYear),
            assetType    = AssetType.Residential
          )

          val disposalDetails = zeroDisposalDetails.copy(disposalPrice = AmountInPence(987L))

          testOnGainCalculatedTaxDue(
            calculate(
              triageAnswers     = triageAnswers,
              disposalDetails   = disposalDetails,
              estimatedIncome   = AmountInPence(123L),
              personalAllowance = AmountInPence.zero
            )
          ) { result =>
            result.taxableGainOrNetLoss.value shouldBe 987L
            result.taxableIncome.value        shouldBe 123L

            result.taxDueAtLowerRate.taxRate shouldBe lowerBandTaxRate
            // threshold - taxable income (1000-123) = 877 is less  than taxable gain (987)
            result.taxDueAtLowerRate.taxableAmount.value shouldBe 877L
            // 1.23% of 877 = 10.7871 --> rounds down to 10
            result.taxDueAtLowerRate.taxDue().value shouldBe 10L

            result.taxDueAtHigherRate.taxRate             shouldBe higherBandTaxRate
            result.taxDueAtHigherRate.taxableAmount.value shouldBe 110L
            // 4.56% of 110 = 15.016  --> rounds down to 5
            result.taxDueAtHigherRate.taxDue().value shouldBe 5L

            result.amountOfTaxDue.value      shouldBe 15L
            result.yearToDateLiability.value shouldBe 15L
          }
        }

      }

    }
  }
  def testOnGainCalculatedTaxDue(result: CalculatedTaxDue)(f: GainCalculatedTaxDue => Unit): Unit = result match {
    case n: NonGainCalculatedTaxDue => fail(s"Expected a gain but got $n")
    case g: GainCalculatedTaxDue    => f(g)
  }

}
