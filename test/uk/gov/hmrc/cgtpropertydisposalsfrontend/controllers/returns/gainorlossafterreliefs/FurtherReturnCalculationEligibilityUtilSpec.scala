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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.gainorlossafterreliefs

import com.typesafe.config.ConfigFactory
import org.scalamock.scalatest.MockFactory
import org.scalatest.{Matchers, WordSpec}
import play.api.Configuration
import play.api.i18n.DefaultMessagesApi
import play.api.mvc.MessagesRequest
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.SessionSupport
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedRequest, RequestWithSessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.ReturnsServiceSupport
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{CompleteReturnWithSummary, Error, SessionData}
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
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.FurtherReturnCalculationEligibility.{Eligible, Ineligible}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.{FurtherReturnCalculationEligibility, FurtherReturnCalculationEligibilityUtil, FurtherReturnCalculationEligibilityUtilImpl}
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global

class FurtherReturnCalculationEligibilityUtilSpec
    extends WordSpec
    with Matchers
    with MockFactory
    with ReturnsServiceSupport
    with SessionSupport {

  val maxPreviousReturns = 3

  class TestEnvironment(maxPreviousReturns: Int = maxPreviousReturns) {

    val config = Configuration(
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
    name: Either[TrustName, IndividualName] = Right(sample[IndividualName])
  ): FillingOutReturn = {
    val draftReturn = sample[DraftSingleDisposalReturn]
      .copy(
        triageAnswers = if (withCompleteTriageDetails) {
          sample[CompleteSingleDisposalTriageAnswers]
            .copy(individualUserType = individualUserType, assetType = assetType)
        } else sample[IncompleteSingleDisposalTriageAnswers],
        propertyAddress = address,
        disposalDetailsAnswers = Some(
          if (withCompleteDisposalDetails) {
            sample[CompleteDisposalDetailsAnswers]
              .copy(disposalPrice = AmountInPence(0), disposalFees = AmountInPence(0))
          } else sample[IncompleteDisposalDetailsAnswers]
        ),
        gainOrLossAfterReliefs = gainOrLossAfterReliefs,
        acquisitionDetailsAnswers = Some(
          if (withCompleteAcquisitionDetails) {
            sample[CompleteAcquisitionDetailsAnswers].copy(
              acquisitionFees = AmountInPence(0),
              acquisitionPrice = AmountInPence(0),
              improvementCosts = AmountInPence(0)
            )
          } else sample[IncompleteAcquisitionDetailsAnswers]
        ),
        reliefDetailsAnswers = Some(
          if (withCompleteReliefDetails) {
            sample[CompleteReliefDetailsAnswers].copy(
              otherReliefs = otherReliefs,
              privateResidentsRelief = AmountInPence(0),
              lettingsRelief = AmountInPence(0)
            )
          } else sample[IncompleteReliefDetailsAnswers]
        )
      )

    sample[FillingOutReturn].copy(
      draftReturn = draftReturn,
      previousSentReturns = previousSentReturns,
      amendReturnData = amendReturnData,
      subscribedDetails = sample[SubscribedDetails].copy(name = name)
    )

  }

  def genDisplayReturn(
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

  def requestWithSessionData(sessionData: SessionData) =
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
      )(service: FurtherReturnCalculationEligibilityUtil) = {
        implicit val request: RequestWithSessionData[_] = requestWithSessionData(sessionData)
        await(service.isEligibleForFurtherReturnOrAmendCalculation(fillingOutReturn).value) shouldBe Right(expected)
      }

      def test(
        fillingOutReturn: FillingOutReturn,
        expected: FurtherReturnCalculationEligibility
      )(service: FurtherReturnCalculationEligibilityUtil) =
        testWithSession(fillingOutReturn, SessionData.empty.copy(journeyStatus = Some(fillingOutReturn)), expected)(
          service
        )

      "return an ineligible response" when {

        "the return is multiple disposal return" ignore new TestEnvironment() {
          val previousReturnData = sample[PreviousReturnData].copy(
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

        "the return is single mixed use disposal return" ignore new TestEnvironment() {
          val previousReturnData = sample[PreviousReturnData].copy(
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

        "the return is mixed use single indirect return" ignore new TestEnvironment() {
          val previousReturnData = sample[PreviousReturnData].copy(
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

        "the return is mixed use multiple indirect return" ignore new TestEnvironment() {
          val previousReturnData = sample[PreviousReturnData].copy(
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

        "Current return has other reliefs" ignore new TestEnvironment() {
          test(eligibleFillingOutReturn(otherReliefs = Some(sample[OtherReliefs])), Ineligible(None))(service)
        }

        "there are more than the configured maximum of previous returns" ignore new TestEnvironment() {
          val previousReturnData     = sample[PreviousReturnData].copy(
            summaries = List.fill(maxPreviousReturns + 1)(sample[ReturnSummary]),
            previousReturnsImplyEligibilityForCalculation = None,
            calculationData = None
          )
          val tooManyPreviousReturns = eligibleFillingOutReturn(previousSentReturns = Some(previousReturnData))
          test(tooManyPreviousReturns, Ineligible(None))(
            service
          )
        }

        "the current return's user type is Capacitor" ignore new TestEnvironment() {
          test(eligibleFillingOutReturn(individualUserType = Some(Capacitor)), Ineligible(None))(service)
        }

        "the current return's user type is PersonalRepresentative" ignore new TestEnvironment() {

          test(eligibleFillingOutReturn(individualUserType = Some(PersonalRepresentative)), Ineligible(None))(service)
        }

        "the current return's user type is PersonalRepresentativeInPeriodOfAdmin" ignore new TestEnvironment() {
          test(
            eligibleFillingOutReturn(individualUserType = Some(PersonalRepresentativeInPeriodOfAdmin)),
            Ineligible(None)
          )(
            service
          )
        }

        "there is no IndividualUserType in the current return" ignore new TestEnvironment() {
          test(eligibleFillingOutReturn(individualUserType = None), Ineligible(None))(
            service
          )
        }

        "there is a previous return with an ineligible asset type" ignore new TestEnvironment() {
          val returns            = List.fill(maxPreviousReturns - 1)(sample[ReturnSummary])
          val previousReturnData =
            sample[PreviousReturnData]
              .copy(summaries = returns, previousReturnsImplyEligibilityForCalculation = None, calculationData = None)
          val fillingOutReturn   =
            eligibleFillingOutReturn(previousSentReturns = Some(previousReturnData))
          val sessionData        = SessionData.empty.copy(journeyStatus = Some(fillingOutReturn))

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

        "there is previous returns which contains other reliefs" ignore new TestEnvironment() {
          val returns            = List.fill(maxPreviousReturns - 1)(sample[ReturnSummary])
          val previousReturnData =
            sample[PreviousReturnData]
              .copy(summaries = returns, previousReturnsImplyEligibilityForCalculation = None, calculationData = None)
          val fillingOutReturn   =
            eligibleFillingOutReturn(previousSentReturns = Some(previousReturnData))
          val sessionData        = SessionData.empty.copy(journeyStatus = Some(fillingOutReturn))

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

        "under limit and displays OK" ignore new TestEnvironment() {
          val currentReturnAddress         = sample[UkAddress]
          val (address1, address2)         = sample[UkAddress]     -> sample[UkAddress]
          val (return1, return2)           = sample[ReturnSummary] -> sample[ReturnSummary]
          val previousReturnData           =
            sample[PreviousReturnData]
              .copy(
                summaries = List(return1, return2),
                previousReturnsImplyEligibilityForCalculation = None,
                calculationData = None
              )
          val fillingOutReturn             =
            eligibleFillingOutReturn(
              previousSentReturns = Some(previousReturnData),
              address = Some(currentReturnAddress)
            )
          val sessionData                  = SessionData.empty.copy(journeyStatus = Some(fillingOutReturn))
          val furtherRetrunCalculationData =
            List(
              FurtherReturnCalculationData(
                address2,
                AmountInPence(0L)
              ),
              FurtherReturnCalculationData(
                address1,
                AmountInPence(100L)
              )
            )

          inSequence {
            mockDisplayReturn(fillingOutReturn.subscribedDetails.cgtReference, return1.submissionId)(
              Right(genDisplayReturn(address = address1, gainOrLossAfterReliefs = Some(AmountInPence(100))))
            )

            mockDisplayReturn(fillingOutReturn.subscribedDetails.cgtReference, return2.submissionId)(
              Right(
                genDisplayReturn(
                  address = address2,
                  gainOrLossAfterReliefs = None,
                  disposalDetailsAnswers = sample[CompleteDisposalDetailsAnswers].copy(
                    disposalPrice = AmountInPence(1L),
                    disposalFees = AmountInPence(3L)
                  ),
                  acquisitionDetailsAnswers = sample[CompleteAcquisitionDetailsAnswers].copy(
                    acquisitionPrice = AmountInPence(5L),
                    improvementCosts = AmountInPence(7L),
                    acquisitionFees = AmountInPence(11L)
                  ),
                  reliefsDetailsAnswers = sample[CompleteReliefDetailsAnswers].copy(
                    privateResidentsRelief = AmountInPence(13L),
                    lettingsRelief = AmountInPence(17L),
                    otherReliefs = Some(NoOtherReliefs)
                  )
                )
              )
            )

            mockStoreSession(
              sessionData.copy(
                journeyStatus = Some(
                  fillingOutReturn.copy(
                    previousSentReturns = Some(
                      previousReturnData.copy(
                        previousReturnsImplyEligibilityForCalculation = Some(true),
                        calculationData = Some(furtherRetrunCalculationData)
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
                AmountInPence(0)
              ),
              furtherRetrunCalculationData,
              currentReturnAddress
            )
          )(service)
        }

        "in an amend return journey the original return makes the user ineligible but the " +
          "rest of the returns are eligible" ignore new TestEnvironment() {
            val (currentReturnAddress, previousReturnAddress) = sample[UkAddress]     -> sample[UkAddress]
            val (originalReturn, otherReturn)                 = sample[ReturnSummary] -> sample[ReturnSummary]

            val previousReturnData           =
              sample[PreviousReturnData].copy(
                summaries = List(originalReturn, otherReturn),
                previousReturnsImplyEligibilityForCalculation = None,
                calculationData = None
              )
            val fillingOutReturn             =
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
            val sessionData                  = SessionData.empty.copy(journeyStatus = Some(fillingOutReturn))
            val furtherReturnCalculationData =
              List(
                FurtherReturnCalculationData(
                  previousReturnAddress,
                  AmountInPence(99L)
                )
              )

            // display return API will not be called for the original return
            inSequence {
              mockDisplayReturn(fillingOutReturn.subscribedDetails.cgtReference, otherReturn.submissionId)(
                Right(
                  genDisplayReturn(
                    address = previousReturnAddress,
                    gainOrLossAfterReliefs = Some(AmountInPence(99L))
                  )
                )
              )
              mockStoreSession(
                sessionData.copy(
                  journeyStatus = Some(
                    fillingOutReturn.copy(
                      previousSentReturns = Some(
                        previousReturnData.copy(
                          previousReturnsImplyEligibilityForCalculation = Some(true),
                          calculationData = Some(furtherReturnCalculationData)
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
                  AmountInPence(0)
                ),
                furtherReturnCalculationData,
                currentReturnAddress
              )
            )(service)

          }

        "there is no individual user type and the user is a trust" ignore new TestEnvironment() {
          val currentReturnAddress                           = sample[UkAddress]
          val (previousReturnAddress, previousReturnSummary) = sample[UkAddress] -> sample[ReturnSummary]
          val previousReturnData                             =
            sample[PreviousReturnData]
              .copy(
                summaries = List(previousReturnSummary),
                previousReturnsImplyEligibilityForCalculation = None,
                calculationData = None
              )
          val fillingOutReturn                               =
            eligibleFillingOutReturn(
              previousSentReturns = Some(previousReturnData),
              address = Some(currentReturnAddress),
              individualUserType = None,
              name = Left(sample[TrustName])
            )
          val sessionData                                    = SessionData.empty.copy(journeyStatus = Some(fillingOutReturn))
          val furtherRetrunCalculationData                   =
            List(
              FurtherReturnCalculationData(
                previousReturnAddress,
                AmountInPence(100L)
              )
            )

          inSequence {
            mockDisplayReturn(fillingOutReturn.subscribedDetails.cgtReference, previousReturnSummary.submissionId)(
              Right(
                genDisplayReturn(address = previousReturnAddress, gainOrLossAfterReliefs = Some(AmountInPence(100)))
              )
            )
            mockStoreSession(
              sessionData.copy(
                journeyStatus = Some(
                  fillingOutReturn.copy(
                    previousSentReturns = Some(
                      previousReturnData.copy(
                        previousReturnsImplyEligibilityForCalculation = Some(true),
                        calculationData = Some(furtherRetrunCalculationData)
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
                AmountInPence(0)
              ),
              furtherRetrunCalculationData,
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

        "the triage section isn't complete in a DraftSingleDisposalReturn" ignore new TestEnvironment() {
          testError(
            eligibleFillingOutReturn(
              withCompleteTriageDetails = false,
              previousSentReturns = Some(sample[PreviousReturnData])
            ),
            service
          )
        }

        "the disposal details section is not complete in a DraftSingleDisposalReturn" ignore new TestEnvironment() {
          testError(
            eligibleFillingOutReturn(
              withCompleteTriageDetails = false,
              previousSentReturns = Some(sample[PreviousReturnData])
            ),
            service
          )
        }

        "the acquisition details section is not complete in a DraftSingleDisposalReturn" ignore new TestEnvironment() {
          testError(
            eligibleFillingOutReturn(
              withCompleteAcquisitionDetails = false,
              previousSentReturns = Some(sample[PreviousReturnData])
            ),
            service
          )
        }

        "the relief details section is not complete in a DraftSingleDisposalReturn" ignore new TestEnvironment() {
          testError(
            eligibleFillingOutReturn(
              withCompleteReliefDetails = false,
              previousSentReturns = Some(sample[PreviousReturnData])
            ),
            service
          )
        }

        "there is no property address in the current return" ignore new TestEnvironment() {
          testError(
            eligibleFillingOutReturn(
              address = None,
              previousSentReturns = Some(sample[PreviousReturnData])
            ),
            service
          )
        }

        "there is no previous return data for an individual" ignore new TestEnvironment() {
          testError(
            eligibleFillingOutReturn(previousSentReturns = None),
            service
          )
        }

        "there is an error getting details of a previously sent return" ignore new TestEnvironment() {
          val returns            = List.fill(maxPreviousReturns - 1)(sample[ReturnSummary])
          val previousReturnData =
            sample[PreviousReturnData]
              .copy(summaries = returns, previousReturnsImplyEligibilityForCalculation = None, calculationData = None)
          val fillingOutReturn   =
            eligibleFillingOutReturn(previousSentReturns = Some(previousReturnData))
          val sessionData        = SessionData.empty.copy(journeyStatus = Some(fillingOutReturn))
          implicit val request   = requestWithSessionData(sessionData)

          inSequence {
            returns.foreach { r =>
              mockDisplayReturn(fillingOutReturn.subscribedDetails.cgtReference, r.submissionId)(
                Left(Error(""))
              )
            }
          }

          val result = service.isEligibleForFurtherReturnOrAmendCalculation(fillingOutReturn)
          await(result.value) shouldBe a[Left[_, _]]
        }

        "there is an error updating the session" ignore new TestEnvironment() {
          val returns            = List.fill(maxPreviousReturns - 1)(sample[ReturnSummary])
          val previousReturnData =
            sample[PreviousReturnData]
              .copy(summaries = returns, previousReturnsImplyEligibilityForCalculation = None, calculationData = None)
          val fillingOutReturn   =
            eligibleFillingOutReturn(previousSentReturns = Some(previousReturnData))
          val sessionData        = SessionData.empty.copy(journeyStatus = Some(fillingOutReturn))
          implicit val request   = requestWithSessionData(sessionData)

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
                        previousReturnsImplyEligibilityForCalculation = Some(false),
                        calculationData = None
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
