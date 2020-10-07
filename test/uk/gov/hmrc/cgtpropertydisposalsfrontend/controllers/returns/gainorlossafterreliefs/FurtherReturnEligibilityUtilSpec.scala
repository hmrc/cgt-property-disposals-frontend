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
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.AmountInPence
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.AcquisitionDetailsGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.CompleteReturnGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.DisposalDetailsGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.DraftReturnGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.JourneyStatusGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.ReliefDetailsGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.ReturnGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.TriageQuestionsGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.AcquisitionDetailsAnswers.{CompleteAcquisitionDetailsAnswers, IncompleteAcquisitionDetailsAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.AssetType.{IndirectDisposal, Residential}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.CompleteReturn.CompleteSingleDisposalReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.DisposalDetailsAnswers.{CompleteDisposalDetailsAnswers, IncompleteDisposalDetailsAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.IndividualUserType.{Capacitor, PersonalRepresentative, PersonalRepresentativeInPeriodOfAdmin, Self}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.OtherReliefsOption.{NoOtherReliefs, OtherReliefs}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ReliefDetailsAnswers.{CompleteReliefDetailsAnswers, IncompleteReliefDetailsAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.{CompleteSingleDisposalTriageAnswers, IncompleteSingleDisposalTriageAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.FurtherReturnEligibility.{Eligible, Ineligible}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.{FurtherReturnEligibility, FurtherReturnEligibilityUtil, FurtherReturnEligibilityUtilImpl}
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global

class FurtherReturnEligibilityUtilSpec
    extends WordSpec
    with Matchers
    with MockFactory
    with ReturnsServiceSupport
    with SessionSupport {

  val maxPreviousReturns = 3

  class TestEnvironment(calculationEnabled: Boolean = true, maxPreviousReturns: Int = maxPreviousReturns) {

    val config = Configuration(
      ConfigFactory.parseString(
        s"""
            |amend-and-further-returns-calculator{
            |  enabled = ${calculationEnabled.toString}
            |  max-previous-returns = $maxPreviousReturns
            |}""".stripMargin
      )
    )

    val service = new FurtherReturnEligibilityUtilImpl(mockReturnsService, config, mockSessionStore)
  }

  def eligibleFillingOutReturn(
    gainOrLossAfterReliefs: Option[AmountInPence] = None,
    individualUserType: Option[IndividualUserType] = Some(Self),
    assetType: AssetType = Residential,
    previousSentReturns: Option[PreviousReturnData] = Some(
      sample[PreviousReturnData].copy(previousReturnsImplyEligibilityForCalculation = None)
    ),
    amendReturnData: Option[AmendReturnData] = None,
    otherReliefs: Option[OtherReliefsOption] = Some(NoOtherReliefs),
    withCompleteAcquisitionDetails: Boolean = true,
    withCompleteReliefDetails: Boolean = true,
    withCompleteTriageDetails: Boolean = true,
    withCompleteDisposalDetails: Boolean = true
  ): FillingOutReturn = {
    val draftReturn = sample[DraftSingleDisposalReturn]
      .copy(
        triageAnswers = if (withCompleteTriageDetails) {
          sample[CompleteSingleDisposalTriageAnswers]
            .copy(individualUserType = individualUserType, assetType = assetType)
        } else sample[IncompleteSingleDisposalTriageAnswers],
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
      amendReturnData = amendReturnData
    )

  }

  def genDisplayReturn(
    assetType: AssetType = Residential,
    otherReliefs: Option[OtherReliefsOption] = Some(NoOtherReliefs)
  ) = {
    val completeReturn = sample[CompleteSingleDisposalReturn]
    DisplayReturn(
      completeReturn.copy(
        triageAnswers = completeReturn.triageAnswers.copy(assetType = assetType),
        reliefDetails = completeReturn.reliefDetails.copy(otherReliefs = otherReliefs)
      ),
      ReturnType.FurtherReturn
    )
  }

  implicit val hc: HeaderCarrier = HeaderCarrier()

  def requestWithSessionData(sessionData: SessionData) =
    RequestWithSessionData(
      Some(sessionData),
      AuthenticatedRequest(new MessagesRequest(FakeRequest(), new DefaultMessagesApi()))
    )

  "FurtherReturnEligibilityUtilImpl" when {

    "handling requests to check eligibility for further return calculations" must {

      def testWithSession(
        fillingOutReturn: FillingOutReturn,
        sessionData: SessionData,
        expected: FurtherReturnEligibility
      )(service: FurtherReturnEligibilityUtil) = {
        implicit val request = requestWithSessionData(sessionData)
        await(service.isEligibleForFurtherReturnOrAmendCalculation(fillingOutReturn).value) shouldBe Right(expected)
      }

      def test(
        fillingOutReturn: FillingOutReturn,
        expected: FurtherReturnEligibility
      )(service: FurtherReturnEligibilityUtil) =
        testWithSession(fillingOutReturn, SessionData.empty.copy(journeyStatus = Some(fillingOutReturn)), expected)(
          service
        )

      "return an ineligible response" when {

        "the current return is eligible but the calculation has been disabled in config" in new TestEnvironment(
          calculationEnabled = false
        ) {
          val returns            = List.fill(maxPreviousReturns - 1)(sample[ReturnSummary])
          val previousReturnData = sample[PreviousReturnData].copy(summaries = returns)
          val fillingOutReturn   =
            eligibleFillingOutReturn(previousSentReturns = Some(previousReturnData))
          test(fillingOutReturn, Ineligible(previousReturnData.previousReturnsImplyEligibilityForCalculation))(service)
        }

        "the return is multiple disposal return" in new TestEnvironment() {
          val previousReturnData = sample[PreviousReturnData]

          test(
            sample[FillingOutReturn]
              .copy(draftReturn = sample[DraftMultipleDisposalsReturn], previousSentReturns = Some(previousReturnData)),
            Ineligible(previousReturnData.previousReturnsImplyEligibilityForCalculation)
          )(
            service
          )
        }

        "the return is single mixed use disposal return" in new TestEnvironment() {
          val previousReturnData = sample[PreviousReturnData]

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
          val previousReturnData = sample[PreviousReturnData]

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
          val previousReturnData = sample[PreviousReturnData]

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
          val previousReturnData     = sample[PreviousReturnData].copy(
            summaries = List.fill(maxPreviousReturns + 1)(sample[ReturnSummary]),
            previousReturnsImplyEligibilityForCalculation = None
          )
          val tooManyPreviousReturns = eligibleFillingOutReturn(previousSentReturns = Some(previousReturnData))
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
          val returns            = List.fill(maxPreviousReturns - 1)(sample[ReturnSummary])
          val previousReturnData =
            sample[PreviousReturnData].copy(summaries = returns, previousReturnsImplyEligibilityForCalculation = None)
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
                        previousReturnsImplyEligibilityForCalculation = Some(false)
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
          val returns            = List.fill(maxPreviousReturns - 1)(sample[ReturnSummary])
          val previousReturnData =
            sample[PreviousReturnData].copy(summaries = returns, previousReturnsImplyEligibilityForCalculation = None)
          val fillingOutReturn   =
            eligibleFillingOutReturn(previousSentReturns = Some(previousReturnData))
          val sessionData        = SessionData.empty.copy(journeyStatus = Some(fillingOutReturn))

          inSequence {
            returns.map { r =>
              mockDisplayReturn(fillingOutReturn.subscribedDetails.cgtReference, r.submissionId)(
                Right(genDisplayReturn(otherReliefs = Some(sample[OtherReliefs])))
              )
            }
            mockStoreSession(
              sessionData.copy(
                journeyStatus = Some(
                  fillingOutReturn.copy(
                    previousSentReturns = Some(
                      previousReturnData.copy(
                        previousReturnsImplyEligibilityForCalculation = Some(false)
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
          val returns            = List.fill(maxPreviousReturns - 1)(sample[ReturnSummary])
          val previousReturnData =
            sample[PreviousReturnData].copy(summaries = returns, previousReturnsImplyEligibilityForCalculation = None)
          val fillingOutReturn   =
            eligibleFillingOutReturn(previousSentReturns = Some(previousReturnData))
          val sessionData        = SessionData.empty.copy(journeyStatus = Some(fillingOutReturn))

          inSequence {
            returns.foreach { r =>
              mockDisplayReturn(fillingOutReturn.subscribedDetails.cgtReference, r.submissionId)(
                Right(genDisplayReturn())
              )
            }
            mockStoreSession(
              sessionData.copy(
                journeyStatus = Some(
                  fillingOutReturn.copy(
                    previousSentReturns = Some(
                      previousReturnData.copy(
                        previousReturnsImplyEligibilityForCalculation = Some(true)
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
                Right(Residential),
                previousReturnData
              )
            )
          )(service)
        }

        "in an amend return journey the original return makes the user ineligible but the " +
          "rest of the returns are eligible" in new TestEnvironment() {
            val (originalReturn, otherReturn) = sample[ReturnSummary] -> sample[ReturnSummary]

            val previousReturnData =
              sample[PreviousReturnData].copy(
                summaries = List(originalReturn, otherReturn),
                previousReturnsImplyEligibilityForCalculation = None
              )
            val fillingOutReturn   =
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
                )
              )
            val sessionData        = SessionData.empty.copy(journeyStatus = Some(fillingOutReturn))

            // display return API will not be called for the original return
            inSequence {
              mockDisplayReturn(fillingOutReturn.subscribedDetails.cgtReference, otherReturn.submissionId)(
                Right(genDisplayReturn())
              )
              mockStoreSession(
                sessionData.copy(
                  journeyStatus = Some(
                    fillingOutReturn.copy(
                      previousSentReturns = Some(
                        previousReturnData.copy(
                          previousReturnsImplyEligibilityForCalculation = Some(true)
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
                  Right(Residential),
                  previousReturnData
                )
              )
            )(service)

          }

      }

      "return an error" when {

        def testError(fillingOutReturn: FillingOutReturn, service: FurtherReturnEligibilityUtilImpl): Unit = {
          val sessionData      = SessionData.empty.copy(journeyStatus = Some(fillingOutReturn))
          implicit val request = requestWithSessionData(sessionData)

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

        "there is no previous return data for an individual" in new TestEnvironment() {
          testError(
            eligibleFillingOutReturn(previousSentReturns = None),
            service
          )
        }

        "there is an error getting details of a previously sent return" in new TestEnvironment() {
          val returns            = List.fill(maxPreviousReturns - 1)(sample[ReturnSummary])
          val previousReturnData =
            sample[PreviousReturnData].copy(summaries = returns, previousReturnsImplyEligibilityForCalculation = None)
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

        "there is an error updating the session" in new TestEnvironment() {
          val returns            = List.fill(maxPreviousReturns - 1)(sample[ReturnSummary])
          val previousReturnData =
            sample[PreviousReturnData].copy(summaries = returns, previousReturnsImplyEligibilityForCalculation = None)
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
                        previousReturnsImplyEligibilityForCalculation = Some(false)
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
