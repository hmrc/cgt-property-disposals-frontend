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
import play.api.test.Helpers._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.ReturnsServiceSupport
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, PreviousReturnData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.UserType.Individual
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
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.OtherReliefsOption.OtherReliefs
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ReliefDetailsAnswers.{CompleteReliefDetailsAnswers, IncompleteReliefDetailsAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.{CompleteSingleDisposalTriageAnswers, IncompleteSingleDisposalTriageAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{SessionData, UserType}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.FurtherReturnEligibility.{Eligible, Ineligible}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.{FurtherReturnEligibility, FurtherReturnEligibilityUtil, FurtherReturnEligibilityUtilImpl}
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global

class FurtherReturnEligibilityUtilSpec extends WordSpec with Matchers with MockFactory with ReturnsServiceSupport {

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

    val service = new FurtherReturnEligibilityUtilImpl(mockReturnsService, config)
  }

  def eligibleSession(
    gainOrLossAfterReliefs: Option[AmountInPence] = None,
    individualUserType: Option[IndividualUserType] = Some(Self),
    userType: UserType = Individual,
    assetType: AssetType = Residential,
    previousSentReturns: Option[PreviousReturnData] = None,
    otherReliefs: Option[OtherReliefs] = None,
    withCompleteAcquisitionDetails: Boolean = true,
    withCompleteReliefDetails: Boolean = true,
    withCompleteTriageDetails: Boolean = true,
    withCompleteDisposalDetails: Boolean = true
  ): (SessionData, FillingOutReturn, DraftSingleDisposalReturn) = {
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
    val journey     = sample[FillingOutReturn].copy(
      draftReturn = draftReturn,
      previousSentReturns = previousSentReturns
    )

    (
      SessionData.empty.copy(
        journeyStatus = Some(journey),
        userType = Some(userType)
      ),
      journey,
      draftReturn
    )
  }

  def genDisplayReturn(assetType: AssetType = Residential, otherReliefs: Option[OtherReliefs] = None) = {
    val completeReturn = sample[CompleteSingleDisposalReturn]
    DisplayReturn(
      completeReturn.copy(
        triageAnswers = completeReturn.triageAnswers.copy(assetType = assetType),
        reliefDetails = completeReturn.reliefDetails.copy(otherReliefs = otherReliefs)
      ),
      false
    )
  }

  implicit val hc: HeaderCarrier = HeaderCarrier()

  "FurtherReturnEligibilityUtilImpl" when {

    "handling requests to check eligibility for further return calculations" must {

      def test(
        session: (SessionData, FillingOutReturn, DraftReturn),
        expected: FurtherReturnEligibility
      )(service: FurtherReturnEligibilityUtil) =
        await(service.isEligibleForFurtherReturnOrAmendCalculation(session._2).value) shouldBe Right(expected)

      "return an ineligible response" when {

        "the current return is eligible but the calculation has been disabled in config" in new TestEnvironment(
          calculationEnabled = false
        ) {
          val returns = List.fill(maxPreviousReturns - 1)(sample[ReturnSummary])
          val session =
            eligibleSession(previousSentReturns = Some(sample[PreviousReturnData].copy(summaries = returns)))
          test(session, Ineligible(None))(service)
        }

        "the return is multiple disposal return" in new TestEnvironment() {
          val session             = eligibleSession()
          val multipleDraftReturn = (session._1, session._2, sample[DraftMultipleDisposalsReturn])
          test(multipleDraftReturn, Ineligible(None))(service)
        }

        "the return is single mixed use disposal return" in new TestEnvironment() {
          val session              = eligibleSession()
          val singleMixedUseReturn = (session._1, session._2, sample[DraftSingleMixedUseDisposalReturn])
          test(singleMixedUseReturn, Ineligible(None))(service)
        }

        "the return is mixed use single indirect return" in new TestEnvironment() {
          val session              = eligibleSession()
          val singleIndirectReturn = (session._1, session._2, sample[DraftSingleIndirectDisposalReturn])
          test(singleIndirectReturn, Ineligible(None))(service)
        }

        "the return is mixed use multiple indirect return" in new TestEnvironment() {
          val session                = eligibleSession()
          val multipleIndirectReturn = (session._1, session._2, sample[DraftMultipleIndirectDisposalsReturn])
          test(multipleIndirectReturn, Ineligible(None))(service)
        }

        "the triage section isn't complete" in new TestEnvironment() {
          test(eligibleSession(withCompleteTriageDetails = false), Ineligible(None))(service)
        }

        "the disposal details section is not complete" in new TestEnvironment() {
          test(eligibleSession(withCompleteDisposalDetails = false), Ineligible(None))(service)
        }

        "the acquisition details section is not complete" in new TestEnvironment() {
          test(eligibleSession(withCompleteAcquisitionDetails = false), Ineligible(None))(service)
        }

        "the relief details section is not complete" in new TestEnvironment() {
          test(eligibleSession(withCompleteReliefDetails = false), Ineligible(None))(service)
        }

        "Current return has other reliefs" in new TestEnvironment() {
          test(eligibleSession(otherReliefs = Some(sample[OtherReliefs])), Ineligible(None))(service)
        }

        "there are more than the configured maximum of previous returns" in new TestEnvironment() {
          val previousReturnData     = sample[PreviousReturnData].copy(
            summaries = List.fill(maxPreviousReturns + 1)(sample[ReturnSummary]),
            previousReturnsImplyEligibilityForCalculation = None
          )
          val tooManyPreviousReturns = eligibleSession(previousSentReturns = Some(previousReturnData))
          test(tooManyPreviousReturns, Ineligible(previousReturnData.previousReturnsImplyEligibilityForCalculation))(
            service
          )
        }

        "the current return's user type is Capacitor" in new TestEnvironment() {
          test(eligibleSession(individualUserType = Some(Capacitor)), Ineligible(None))(service)
        }

        "the current return's user type is PersonalRepresentative" in new TestEnvironment() {

          test(eligibleSession(individualUserType = Some(PersonalRepresentative)), Ineligible(None))(service)
        }

        "the current return's user type is PersonalRepresentativeInPeriodOfAdmin" in new TestEnvironment() {
          test(eligibleSession(individualUserType = Some(PersonalRepresentativeInPeriodOfAdmin)), Ineligible(None))(
            service
          )
        }

        "there is no IndividualUserType in the current return" in new TestEnvironment() {
          test(eligibleSession(individualUserType = None), Ineligible(None))(
            service
          )
        }

        "there is a previous return with an ineligible asset type" in new TestEnvironment() {
          val returns            = List.fill(maxPreviousReturns - 1)(sample[ReturnSummary])
          val previousReturnData =
            sample[PreviousReturnData].copy(summaries = returns, previousReturnsImplyEligibilityForCalculation = None)
          val session            =
            eligibleSession(previousSentReturns = Some(previousReturnData))
          inSequence {
            returns.foreach { r =>
              mockDisplayReturn(session._2.subscribedDetails.cgtReference, r.submissionId)(
                Right(genDisplayReturn(assetType = IndirectDisposal))
              )
            }
          }
          test(session, Ineligible(Some(false)))(service)
        }

        "there is previous returns which contains other reliefs" in new TestEnvironment() {
          val returns            = List.fill(maxPreviousReturns - 1)(sample[ReturnSummary])
          val previousReturnData =
            sample[PreviousReturnData].copy(summaries = returns, previousReturnsImplyEligibilityForCalculation = None)
          val session            =
            eligibleSession(previousSentReturns = Some(previousReturnData))
          inSequence {
            returns.map { r =>
              mockDisplayReturn(session._2.subscribedDetails.cgtReference, r.submissionId)(
                Right(genDisplayReturn(otherReliefs = Some(sample[OtherReliefs])))
              )
            }
          }
          test(session, Ineligible(Some(false)))(service)
        }
      }

      "return an eligible response" when {

        "under limit and displays OK" in new TestEnvironment() {
          val returns            = List.fill(maxPreviousReturns - 1)(sample[ReturnSummary])
          val previousReturnData =
            sample[PreviousReturnData].copy(summaries = returns, previousReturnsImplyEligibilityForCalculation = None)
          val session            =
            eligibleSession(previousSentReturns = Some(previousReturnData))
          inSequence {
            returns.foreach { r =>
              mockDisplayReturn(session._2.subscribedDetails.cgtReference, r.submissionId)(Right(genDisplayReturn()))
            }
          }
          test(
            session,
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
    }
  }

}
