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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.gainorlossafterreliefs

import com.typesafe.config.ConfigFactory
import org.scalamock.scalatest.MockFactory
import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.Configuration
import play.api.i18n.DefaultMessagesApi
import play.api.mvc.{AnyContentAsEmpty, MessagesRequest}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.SessionSupport
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedRequest, RequestWithSessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.ReturnsServiceSupport
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, PreviousReturnData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address.UkAddress
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.AmountInPence
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.AcquisitionDetailsGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.AddressGen.ukAddressGen
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.CompleteReturnGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.DisposalDetailsGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.DraftReturnGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.JourneyStatusGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.MoneyGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.NameGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.ReliefDetailsGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.ReturnGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.SubscribedDetailsGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.TaxYearGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.TriageQuestionsGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.{IndividualName, TrustName}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.SubscribedDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.AcquisitionDetailsAnswers.{CompleteAcquisitionDetailsAnswers, IncompleteAcquisitionDetailsAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.AssetType.{IndirectDisposal, Residential}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.CompleteReturn.CompleteSingleDisposalReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.DisposalDetailsAnswers.{CompleteDisposalDetailsAnswers, IncompleteDisposalDetailsAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.IndividualUserType.{Capacitor, PersonalRepresentative, PersonalRepresentativeInPeriodOfAdmin, Self}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.OtherReliefsOption.{NoOtherReliefs, OtherReliefs}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ReliefDetailsAnswers.{CompleteReliefDetailsAnswers, IncompleteReliefDetailsAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.{CompleteSingleDisposalTriageAnswers, IncompleteSingleDisposalTriageAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{CompleteReturnWithSummary, Error, SessionData, TaxYear}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.FurtherReturnCalculationEligibility.{Eligible, Ineligible}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.{FurtherReturnCalculationEligibility, FurtherReturnCalculationEligibilityUtil, FurtherReturnCalculationEligibilityUtilImpl}
import uk.gov.hmrc.http.HeaderCarrier

import java.time.LocalDate
import scala.concurrent.ExecutionContext.Implicits.global

class FurtherReturnCalculationEligibilityUtilSpec
    extends AnyWordSpec
    with Matchers
    with MockFactory
    with ReturnsServiceSupport
    with SessionSupport {

  val maxPreviousReturns = 3

  class TestEnvironment(maxPreviousReturns: Int = maxPreviousReturns) {

    private val config = Configuration(
      ConfigFactory.parseString(
        s"""
            |amend-and-further-returns-calculator{
            |  max-previous-returns = $maxPreviousReturns
            |}""".stripMargin
      )
    )

    val service = new FurtherReturnCalculationEligibilityUtilImpl(mockReturnsService, config, mockSessionStore)
  }

  def eligibleFillingOutReturn(
    gainOrLossAfterReliefs: Option[AmountInPence] = None,
    individualUserType: Option[IndividualUserType] = Some(Self),
    assetType: AssetType = Residential,
    previousSentReturns: Option[PreviousReturnData] = Some(
      sample[PreviousReturnData].copy(previousReturnsImplyEligibilityForCalculation = None, calculationData = None)
    ),
    amendReturnData: Option[AmendReturnData] = None,
    otherReliefs: Option[OtherReliefsOption] = Some(NoOtherReliefs),
    withCompleteAcquisitionDetails: Boolean = true,
    withCompleteReliefDetails: Boolean = true,
    withCompleteTriageDetails: Boolean = true,
    withCompleteDisposalDetails: Boolean = true,
    address: Option[UkAddress] = Some(sample[UkAddress]),
    name: Either[TrustName, IndividualName] = Right(sample[IndividualName]),
    disposalDate: Option[DisposalDate] = None
  ): FillingOutReturn = {
    val answers = if (withCompleteTriageDetails) {
      val triageAnswers =
        sample[CompleteSingleDisposalTriageAnswers].copy(individualUserType = individualUserType, assetType = assetType)
      disposalDate.fold(triageAnswers)(date => triageAnswers.copy(disposalDate = date))
    } else {
      sample[IncompleteSingleDisposalTriageAnswers]
    }

    val draftReturn = sample[DraftSingleDisposalReturn]
      .copy(
        triageAnswers = answers,
        propertyAddress = address,
        disposalDetailsAnswers = Some(
          if (withCompleteDisposalDetails) {
            sample[CompleteDisposalDetailsAnswers]
              .copy(disposalPrice = AmountInPence(0), disposalFees = AmountInPence(0))
          } else {
            sample[IncompleteDisposalDetailsAnswers]
          }
        ),
        gainOrLossAfterReliefs = gainOrLossAfterReliefs,
        acquisitionDetailsAnswers = Some(
          if (withCompleteAcquisitionDetails) {
            sample[CompleteAcquisitionDetailsAnswers].copy(
              acquisitionFees = AmountInPence(0),
              acquisitionPrice = AmountInPence(0),
              improvementCosts = AmountInPence(0),
              rebasedAcquisitionPrice = None,
              shouldUseRebase = false
            )
          } else {
            sample[IncompleteAcquisitionDetailsAnswers]
          }
        ),
        reliefDetailsAnswers = Some(
          if (withCompleteReliefDetails) {
            sample[CompleteReliefDetailsAnswers].copy(
              otherReliefs = otherReliefs,
              privateResidentsRelief = AmountInPence(0),
              lettingsRelief = AmountInPence(0)
            )
          } else {
            sample[IncompleteReliefDetailsAnswers]
          }
        )
      )

    sample[FillingOutReturn].copy(
      draftReturn = draftReturn,
      previousSentReturns = previousSentReturns,
      amendReturnData = amendReturnData,
      subscribedDetails = sample[SubscribedDetails].copy(name = name)
    )

  }

  private def genDisplayReturn(
    assetType: AssetType = Residential,
    gainOrLossAfterReliefs: Option[AmountInPence] = Some(sample[AmountInPence]),
    disposalDetailsAnswers: CompleteDisposalDetailsAnswers = sample[CompleteDisposalDetailsAnswers],
    acquisitionDetailsAnswers: CompleteAcquisitionDetailsAnswers = sample[CompleteAcquisitionDetailsAnswers],
    reliefsDetailsAnswers: CompleteReliefDetailsAnswers = sample[CompleteReliefDetailsAnswers].copy(
      otherReliefs = Some(NoOtherReliefs)
    ),
    address: UkAddress = sample[UkAddress]
  ) =
    DisplayReturn(
      sample[CompleteSingleDisposalReturn].copy(
        triageAnswers = sample[CompleteSingleDisposalTriageAnswers].copy(assetType = assetType),
        disposalDetails = disposalDetailsAnswers,
        acquisitionDetails = acquisitionDetailsAnswers,
        reliefDetails = reliefsDetailsAnswers,
        gainOrLossAfterReliefs = gainOrLossAfterReliefs,
        propertyAddress = address
      ),
      ReturnType.FurtherReturn
    )

  implicit val hc: HeaderCarrier = HeaderCarrier()

  private def requestWithSessionData(sessionData: SessionData) =
    RequestWithSessionData(
      Some(sessionData),
      AuthenticatedRequest(new MessagesRequest(FakeRequest(), new DefaultMessagesApi()))
    )

  "FurtherReturnCalculationEligibilityUtilImpl" when {

    "handling requests to check eligibility for further return calculations" must {

      def testWithSession(
        fillingOutReturn: FillingOutReturn,
        sessionData: SessionData,
        expected: FurtherReturnCalculationEligibility
      )(service: FurtherReturnCalculationEligibilityUtil): Assertion = {
        implicit val request: RequestWithSessionData[_] = requestWithSessionData(sessionData)
        await(service.isEligibleForFurtherReturnOrAmendCalculation(fillingOutReturn).value) shouldBe Right(expected)
      }

      def test(
        fillingOutReturn: FillingOutReturn,
        expected: FurtherReturnCalculationEligibility
      )(service: FurtherReturnCalculationEligibilityUtil): Assertion =
        testWithSession(fillingOutReturn, SessionData.empty.copy(journeyStatus = Some(fillingOutReturn)), expected)(
          service
        )

      "return an ineligible response" when {

        "the return is multiple disposal return" in new TestEnvironment() {
          private val previousReturnData = sample[PreviousReturnData].copy(
            previousReturnsImplyEligibilityForCalculation = None,
            calculationData = None
          )

          test(
            sample[FillingOutReturn]
              .copy(draftReturn = sample[DraftMultipleDisposalsReturn], previousSentReturns = Some(previousReturnData)),
            Ineligible(previousReturnData.previousReturnsImplyEligibilityForCalculation)
          )(
            service
          )
        }

        "the return is single mixed use disposal return" in new TestEnvironment() {
          private val previousReturnData = sample[PreviousReturnData].copy(
            previousReturnsImplyEligibilityForCalculation = None,
            calculationData = None
          )

          test(
            sample[FillingOutReturn]
              .copy(
                draftReturn = sample[DraftSingleMixedUseDisposalReturn],
                previousSentReturns = Some(previousReturnData)
              ),
            Ineligible(previousReturnData.previousReturnsImplyEligibilityForCalculation)
          )(
            service
          )
        }

        "the return is mixed use single indirect return" in new TestEnvironment() {
          private val previousReturnData = sample[PreviousReturnData].copy(
            previousReturnsImplyEligibilityForCalculation = None,
            calculationData = None
          )

          test(
            sample[FillingOutReturn]
              .copy(
                draftReturn = sample[DraftSingleIndirectDisposalReturn],
                previousSentReturns = Some(previousReturnData)
              ),
            Ineligible(previousReturnData.previousReturnsImplyEligibilityForCalculation)
          )(
            service
          )
        }

        "the return is mixed use multiple indirect return" in new TestEnvironment() {
          private val previousReturnData = sample[PreviousReturnData].copy(
            previousReturnsImplyEligibilityForCalculation = None,
            calculationData = None
          )

          test(
            sample[FillingOutReturn]
              .copy(
                draftReturn = sample[DraftMultipleIndirectDisposalsReturn],
                previousSentReturns = Some(previousReturnData)
              ),
            Ineligible(previousReturnData.previousReturnsImplyEligibilityForCalculation)
          )(
            service
          )
        }

        "Current return has other reliefs" in new TestEnvironment() {
          test(eligibleFillingOutReturn(otherReliefs = Some(sample[OtherReliefs])), Ineligible(None))(service)
        }

        "there are more than the configured maximum of previous returns" in new TestEnvironment() {
          val taxYear      = "1990"
          val date         = LocalDate.of(taxYear.toInt, 1, 1)
          val disposalDate = DisposalDate(date, sample[TaxYear].copy(startDateInclusive = date))

          private val returns: List[ReturnSummary] =
            List.fill(maxPreviousReturns + 1)(
              sample[ReturnSummary].copy(taxYear = taxYear, lastUpdatedDate = Some(date))
            )
          private val previousReturnData           = sample[PreviousReturnData].copy(
            summaries = returns,
            previousReturnsImplyEligibilityForCalculation = None,
            calculationData = None
          )
          private val tooManyPreviousReturns       =
            eligibleFillingOutReturn(previousSentReturns = Some(previousReturnData), disposalDate = Some(disposalDate))

          test(tooManyPreviousReturns, Ineligible(None))(
            service
          )
        }

        "the current return's user type is Capacitor" in new TestEnvironment() {
          test(eligibleFillingOutReturn(individualUserType = Some(Capacitor)), Ineligible(None))(service)
        }

        "the current return's user type is PersonalRepresentative" in new TestEnvironment() {

          test(eligibleFillingOutReturn(individualUserType = Some(PersonalRepresentative)), Ineligible(None))(service)
        }

        "the current return's user type is PersonalRepresentativeInPeriodOfAdmin" in new TestEnvironment() {
          test(
            eligibleFillingOutReturn(individualUserType = Some(PersonalRepresentativeInPeriodOfAdmin)),
            Ineligible(None)
          )(
            service
          )
        }

        "there is no IndividualUserType in the current return" in new TestEnvironment() {
          test(eligibleFillingOutReturn(individualUserType = None), Ineligible(None))(
            service
          )
        }

        "there is a previous return with an ineligible asset type" in new TestEnvironment() {
          val taxYear      = "1990"
          val date         = LocalDate.of(taxYear.toInt, 1, 1)
          val disposalDate = DisposalDate(date, sample[TaxYear].copy(startDateInclusive = date))

          private val returns: List[ReturnSummary] =
            List.fill(maxPreviousReturns - 1)(
              sample[ReturnSummary].copy(taxYear = taxYear, lastUpdatedDate = Some(date))
            )

          private val previousReturnData =
            sample[PreviousReturnData]
              .copy(summaries = returns, previousReturnsImplyEligibilityForCalculation = None, calculationData = None)
          private val fillingOutReturn   =
            eligibleFillingOutReturn(previousSentReturns = Some(previousReturnData), disposalDate = Some(disposalDate))
          private val sessionData        = SessionData.empty.copy(journeyStatus = Some(fillingOutReturn))

          inSequence {
            returns.foreach { r =>
              mockDisplayReturn(fillingOutReturn.subscribedDetails.cgtReference, r.submissionId)(
                Right(genDisplayReturn(assetType = IndirectDisposal))
              )
            }
            mockStoreSession(
              sessionData.copy(
                journeyStatus = Some(
                  fillingOutReturn.copy(
                    previousSentReturns = Some(
                      previousReturnData.copy(
                        previousYearToDate = None,
                        previousReturnsImplyEligibilityForCalculation = Some(false),
                        calculationData = None
                      )
                    )
                  )
                )
              )
            )(Right(()))
          }

          testWithSession(fillingOutReturn, sessionData, Ineligible(Some(false)))(service)
        }

        "there is previous returns which contains other reliefs" in new TestEnvironment() {
          val taxYear      = "1990"
          val date         = LocalDate.of(taxYear.toInt, 1, 1)
          val disposalDate = DisposalDate(date, sample[TaxYear].copy(startDateInclusive = date))

          private val returns: List[ReturnSummary] =
            List.fill(maxPreviousReturns - 1)(
              sample[ReturnSummary].copy(taxYear = taxYear, lastUpdatedDate = Some(date))
            )
          private val previousReturnData           =
            sample[PreviousReturnData]
              .copy(summaries = returns, previousReturnsImplyEligibilityForCalculation = None, calculationData = None)
          private val fillingOutReturn             =
            eligibleFillingOutReturn(previousSentReturns = Some(previousReturnData), disposalDate = Some(disposalDate))
          private val sessionData                  = SessionData.empty.copy(journeyStatus = Some(fillingOutReturn))

          inSequence {
            returns.map { r =>
              mockDisplayReturn(fillingOutReturn.subscribedDetails.cgtReference, r.submissionId)(
                Right(
                  genDisplayReturn(
                    reliefsDetailsAnswers =
                      sample[CompleteReliefDetailsAnswers].copy(otherReliefs = Some(sample[OtherReliefs]))
                  )
                )
              )
            }
            mockStoreSession(
              sessionData.copy(
                journeyStatus = Some(
                  fillingOutReturn.copy(
                    previousSentReturns = Some(
                      previousReturnData.copy(
                        previousYearToDate = None,
                        previousReturnsImplyEligibilityForCalculation = Some(false),
                        calculationData = None
                      )
                    )
                  )
                )
              )
            )(Right(()))
          }

          testWithSession(fillingOutReturn, sessionData, Ineligible(Some(false)))(service)
        }
      }

      "return an eligible response" when {

        "under limit and displays OK" in new TestEnvironment() {
          private val currentReturnAddress = sample[UkAddress]
          val (return1, return2)           = sample[ReturnSummary] -> sample[ReturnSummary]
          private val previousReturnData   =
            sample[PreviousReturnData]
              .copy(
                summaries = List(return1, return2),
                previousReturnsImplyEligibilityForCalculation = None,
                calculationData = None
              )
          private val fillingOutReturn     =
            eligibleFillingOutReturn(
              previousSentReturns = Some(previousReturnData),
              address = Some(currentReturnAddress)
            )
          private val sessionData          = SessionData.empty.copy(journeyStatus = Some(fillingOutReturn))

          inSequence {

            mockStoreSession(
              sessionData.copy(
                journeyStatus = Some(
                  fillingOutReturn.copy(
                    previousSentReturns = Some(
                      previousReturnData.copy(
                        summaries = List.empty[ReturnSummary],
                        None,
                        Some(true),
                        Some(List.empty[FurtherReturnCalculationData])
                      )
                    )
                  )
                )
              )
            )(Right(()))
          }

          testWithSession(
            fillingOutReturn,
            sessionData,
            Eligible(
              CalculatedGlarBreakdown(
                AmountInPence(0),
                AmountInPence(0),
                AmountInPence(0),
                AmountInPence(0),
                AmountInPence(0),
                AmountInPence(0),
                AmountInPence(0),
                false,
                None
              ),
              List.empty[FurtherReturnCalculationData],
              currentReturnAddress
            )
          )(service)
        }

        "in an amend return journey the original return makes the user ineligible but the " +
          "rest of the returns are eligible" in new TestEnvironment() {
            val (currentReturnAddress, _)     = sample[UkAddress]     -> sample[UkAddress]
            val (originalReturn, otherReturn) = sample[ReturnSummary] -> sample[ReturnSummary]

            private val previousReturnData =
              sample[PreviousReturnData].copy(
                summaries = List(originalReturn, otherReturn),
                previousReturnsImplyEligibilityForCalculation = None,
                calculationData = None
              )
            private val fillingOutReturn   =
              eligibleFillingOutReturn(
                previousSentReturns = Some(previousReturnData),
                amendReturnData = Some(
                  sample[AmendReturnData].copy(
                    originalReturn = sample[CompleteReturnWithSummary].copy(
                      summary = sample[ReturnSummary].copy(
                        submissionId = originalReturn.submissionId
                      )
                    )
                  )
                ),
                address = Some(currentReturnAddress)
              )
            private val sessionData        = SessionData.empty.copy(journeyStatus = Some(fillingOutReturn))

            // display return API will not be called for the original return
            inSequence {
              mockStoreSession(
                sessionData.copy(
                  journeyStatus = Some(
                    fillingOutReturn.copy(
                      previousSentReturns = Some(
                        previousReturnData.copy(
                          summaries = List.empty[ReturnSummary],
                          None,
                          Some(true),
                          Some(List.empty[FurtherReturnCalculationData])
                        )
                      )
                    )
                  )
                )
              )(Right(()))
            }

            testWithSession(
              fillingOutReturn,
              sessionData,
              Eligible(
                CalculatedGlarBreakdown(
                  AmountInPence(0),
                  AmountInPence(0),
                  AmountInPence(0),
                  AmountInPence(0),
                  AmountInPence(0),
                  AmountInPence(0),
                  AmountInPence(0),
                  false,
                  None
                ),
                List.empty[FurtherReturnCalculationData],
                currentReturnAddress
              )
            )(service)

          }

        "there is no individual user type and the user is a trust" in new TestEnvironment() {
          private val currentReturnAddress = sample[UkAddress]
          val (_, previousReturnSummary)   = sample[UkAddress] -> sample[ReturnSummary]
          private val previousReturnData   =
            sample[PreviousReturnData]
              .copy(
                summaries = List(previousReturnSummary),
                previousReturnsImplyEligibilityForCalculation = None,
                calculationData = None
              )
          private val fillingOutReturn     =
            eligibleFillingOutReturn(
              previousSentReturns = Some(previousReturnData),
              address = Some(currentReturnAddress),
              individualUserType = None,
              name = Left(sample[TrustName])
            )
          private val sessionData          = SessionData.empty.copy(journeyStatus = Some(fillingOutReturn))

          inSequence {
            mockStoreSession(
              sessionData.copy(
                journeyStatus = Some(
                  fillingOutReturn.copy(
                    previousSentReturns = Some(
                      previousReturnData.copy(
                        summaries = List.empty[ReturnSummary],
                        None,
                        Some(true),
                        Some(List.empty[FurtherReturnCalculationData])
                      )
                    )
                  )
                )
              )
            )(Right(()))
          }

          testWithSession(
            fillingOutReturn,
            sessionData,
            Eligible(
              CalculatedGlarBreakdown(
                AmountInPence(0),
                AmountInPence(0),
                AmountInPence(0),
                AmountInPence(0),
                AmountInPence(0),
                AmountInPence(0),
                AmountInPence(0),
                false,
                None
              ),
              List.empty[FurtherReturnCalculationData],
              currentReturnAddress
            )
          )(service)
        }

      }

      "return an error" when {

        def testError(
          fillingOutReturn: FillingOutReturn,
          service: FurtherReturnCalculationEligibilityUtilImpl
        ): Unit = {
          val sessionData                                 = SessionData.empty.copy(journeyStatus = Some(fillingOutReturn))
          implicit val request: RequestWithSessionData[_] = requestWithSessionData(sessionData)

          val result = service.isEligibleForFurtherReturnOrAmendCalculation(fillingOutReturn)
          await(result.value) shouldBe a[Left[_, _]]
        }

        "the triage section isn't complete in a DraftSingleDisposalReturn" in new TestEnvironment() {
          testError(
            eligibleFillingOutReturn(
              withCompleteTriageDetails = false,
              previousSentReturns = Some(sample[PreviousReturnData])
            ),
            service
          )
        }

        "the disposal details section is not complete in a DraftSingleDisposalReturn" in new TestEnvironment() {
          testError(
            eligibleFillingOutReturn(
              withCompleteTriageDetails = false,
              previousSentReturns = Some(sample[PreviousReturnData])
            ),
            service
          )
        }

        "the acquisition details section is not complete in a DraftSingleDisposalReturn" in new TestEnvironment() {
          testError(
            eligibleFillingOutReturn(
              withCompleteAcquisitionDetails = false,
              previousSentReturns = Some(sample[PreviousReturnData])
            ),
            service
          )
        }

        "the relief details section is not complete in a DraftSingleDisposalReturn" in new TestEnvironment() {
          testError(
            eligibleFillingOutReturn(
              withCompleteReliefDetails = false,
              previousSentReturns = Some(sample[PreviousReturnData])
            ),
            service
          )
        }

        "there is no property address in the current return" in new TestEnvironment() {
          testError(
            eligibleFillingOutReturn(
              address = None,
              previousSentReturns = Some(sample[PreviousReturnData])
            ),
            service
          )
        }

        "there is an error getting details of a previously sent return" in new TestEnvironment() {
          val taxYear      = "1970"
          val date         = LocalDate.of(taxYear.toInt, 1, 1)
          val disposalDate = DisposalDate(date, sample[TaxYear].copy(startDateInclusive = date))

          private val returns                                                          = List(sample[ReturnSummary].copy(taxYear = taxYear))
          private val previousReturnData                                               =
            sample[PreviousReturnData]
              .copy(summaries = returns, previousReturnsImplyEligibilityForCalculation = None, calculationData = None)
          private val fillingOutReturn                                                 =
            eligibleFillingOutReturn(previousSentReturns = Some(previousReturnData), disposalDate = Some(disposalDate))
          private val sessionData                                                      = SessionData.empty.copy(journeyStatus = Some(fillingOutReturn))
          private implicit val request: RequestWithSessionData[AnyContentAsEmpty.type] =
            requestWithSessionData(sessionData)

          inSequence {
            returns.foreach { r =>
              mockDisplayReturn(fillingOutReturn.subscribedDetails.cgtReference, r.submissionId)(
                Left(Error(""))
              )
            }
          }

          private val result = service.isEligibleForFurtherReturnOrAmendCalculation(fillingOutReturn)
          await(result.value) shouldBe a[Left[_, _]]
        }

        "there is an error updating the session" in new TestEnvironment() {
          private val returns                                                          = List.fill(maxPreviousReturns - 1)(sample[ReturnSummary])
          private val previousReturnData                                               =
            sample[PreviousReturnData]
              .copy(summaries = returns, previousReturnsImplyEligibilityForCalculation = None, calculationData = None)
          private val fillingOutReturn                                                 =
            eligibleFillingOutReturn(previousSentReturns = Some(previousReturnData))
          private val sessionData                                                      = SessionData.empty.copy(journeyStatus = Some(fillingOutReturn))
          private implicit val request: RequestWithSessionData[AnyContentAsEmpty.type] =
            requestWithSessionData(sessionData)

          inSequence {
            mockStoreSession(
              sessionData.copy(
                journeyStatus = Some(
                  fillingOutReturn.copy(
                    previousSentReturns = Some(
                      previousReturnData.copy(
                        summaries = List.empty[ReturnSummary],
                        None,
                        Some(true),
                        Some(List.empty[FurtherReturnCalculationData])
                      )
                    )
                  )
                )
              )
            )(Left(Error("")))

            val result = service.isEligibleForFurtherReturnOrAmendCalculation(fillingOutReturn)
            await(result.value) shouldBe a[Left[_, _]]
          }

        }

      }
    }

  }

}
