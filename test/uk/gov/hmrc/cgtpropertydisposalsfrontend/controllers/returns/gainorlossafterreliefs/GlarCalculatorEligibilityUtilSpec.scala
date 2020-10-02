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

import cats.data.EitherT
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.Configuration
import play.api.i18n.MessagesApi
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.{MessagesRequest, Result}
import play.api.test.FakeRequest
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.{ReturnsServiceSupport, StartingToAmendToFillingOutReturnSpecBehaviour}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, PreviousReturnData, StartingToAmendReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.UserType.Individual
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.AmountInPence
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.AcquisitionDetailsGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.CompleteReturnGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.DisposalDetailsGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.DraftReturnGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.JourneyStatusGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.ReliefDetailsGen._
import play.api.test.Helpers._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedRequest, RequestWithSessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.ReturnGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.TriageQuestionsGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.UUIDGenerator
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.AcquisitionDetailsAnswers.CompleteAcquisitionDetailsAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.AssetType.{IndirectDisposal, Residential}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.CompleteReturn.CompleteSingleDisposalReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.DisposalDetailsAnswers.CompleteDisposalDetailsAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.IndividualUserType.{Capacitor, PersonalRepresentative, PersonalRepresentativeInPeriodOfAdmin, Self}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.OtherReliefsOption.OtherReliefs
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ReliefDetailsAnswers.CompleteReliefDetailsAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.CompleteSingleDisposalTriageAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, SessionData, UserType}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.{FurtherReturnEligibility, GlarCalculatorEligibilityUtil, ReturnsService}
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.Future

class GlarCalculatorEligibilityUtilSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with ReturnsServiceSupport
    with ScalaCheckDrivenPropertyChecks
    with RedirectToStartBehaviour
    with StartingToAmendToFillingOutReturnSpecBehaviour {

  val mockUUIDGenerator = mock[UUIDGenerator]

  def redirectToStartBehaviour(improvementCosts: () => Future[Result]) =
    redirectToStartWhenInvalidJourney(
      improvementCosts,
      {
        case f: FillingOutReturn if f.isFurtherOrAmendReturn.contains(true) => true
        case _: StartingToAmendReturn                                       => true
        case _                                                              => false
      }
    )

  override lazy val additionalConfig = Configuration(
    "glar-calculator.enabled" -> true
  )

  lazy val service                          = instanceOf[GlarCalculatorEligibilityUtil]
  implicit val headerCarrier: HeaderCarrier = mock[HeaderCarrier]
  val message                               = mock[MessagesApi]
  implicit val requestWithSessionData       = RequestWithSessionData(
    None,
    AuthenticatedRequest(
      new MessagesRequest(FakeRequest(), message)
    )
  )

  override val overrideBindings = List[GuiceableModule](
    bind[AuthConnector].toInstance(mockAuthConnector),
    bind[SessionStore].toInstance(mockSessionStore),
    bind[ReturnsService].toInstance(mockReturnsService),
    bind[UUIDGenerator].toInstance(mockUUIDGenerator)
  )

  def eligableSession(
    gainOrLossAfterReliefs: Option[AmountInPence] = None,
    individualUserType: Option[IndividualUserType] = Some(Self),
    userType: UserType = Individual,
    assetType: AssetType = Residential,
    previousSentReturns: Option[PreviousReturnData] = None,
    otherRefliefs: Option[OtherReliefs] = None
  ): (SessionData, FillingOutReturn, DraftSingleDisposalReturn) = {
    val draftReturn = sample[DraftSingleDisposalReturn]
      .copy(
        triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
          .copy(individualUserType = individualUserType, assetType = assetType),
        disposalDetailsAnswers = Some(sample[CompleteDisposalDetailsAnswers]),
        gainOrLossAfterReliefs = gainOrLossAfterReliefs,
        acquisitionDetailsAnswers = Some(sample[CompleteAcquisitionDetailsAnswers]),
        reliefDetailsAnswers = Some(sample[CompleteReliefDetailsAnswers].copy(otherReliefs = otherRefliefs))
      )
    val journey     = sample[FillingOutReturn].copy(
      draftReturn = draftReturn,
      previousSentReturns = previousSentReturns,
      previousReturnsImplyEligilityForFurtherReturnCalculation = None
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

  def performAction(fillingOutReturn: FillingOutReturn): EitherT[Future, Error, FurtherReturnEligibility] =
    service.isEligibleForFurtherReturnOrAmendCalculation(fillingOutReturn)

  "GainOrLossAfterReliefsGlarCalculatorEnabledController" when {

    "handling eligibility request to display the gain or loss after reliefs page" must {

      def test(
        session: (SessionData, FillingOutReturn, DraftSingleDisposalReturn),
        expected: FurtherReturnEligibility
      ) = {
        val result = await(performAction(session._2).value)
        result.isRight shouldBe true
        result match {
          case Left(_)      => fail()
          case Right(value) => value shouldBe expected
        }
      }

      "Checks that do not require return display" when {

        "user not eligible" when {

          "Current return has other reliefs" in {
            test(eligableSession(otherRefliefs = Some(sample[OtherReliefs])), FurtherReturnEligibility(false, None))
          }

          "More than 10 previous returns" in {
            val tooManyPreviousReturns = eligableSession(previousSentReturns =
              Some(sample[PreviousReturnData].copy(summaries = List.fill(11)(sample[ReturnSummary])))
            )
            test(tooManyPreviousReturns, FurtherReturnEligibility(false, None))
          }

          "Current return not self" in {
            val isCapacitor = eligableSession(individualUserType = Some(Capacitor))
            test(isCapacitor, FurtherReturnEligibility(false, None))

            val isPersonalRep = eligableSession(individualUserType = Some(PersonalRepresentative))
            test(isPersonalRep, FurtherReturnEligibility(false, None))

            val isPersonalRepInPeriodOfAdmin =
              eligableSession(individualUserType = Some(PersonalRepresentativeInPeriodOfAdmin))
            test(isPersonalRepInPeriodOfAdmin, FurtherReturnEligibility(false, None))
          }
        }
      }

      "Checks that require return display" when {

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

        "user eligible" when {
          "under limit and displays OK" in {
            val returns = List.fill(9)(sample[ReturnSummary])
            val session =
              eligableSession(previousSentReturns = Some(sample[PreviousReturnData].copy(summaries = returns)))
            inSequence {
              returns.map { r =>
                mockDisplayReturn(session._2.subscribedDetails.cgtReference, r.submissionId)(Right(genDisplayReturn()))
              }
            }
            test(session, FurtherReturnEligibility(true, Some(true)))
          }
        }

        "user not eligible" when {

          "under limit but previous returns asset type incorrect" in {
            val returns = List.fill(9)(sample[ReturnSummary])
            val session =
              eligableSession(previousSentReturns = Some(sample[PreviousReturnData].copy(summaries = returns)))
            inSequence {
              returns.map { r =>
                mockDisplayReturn(session._2.subscribedDetails.cgtReference, r.submissionId)(
                  Right(genDisplayReturn(assetType = IndirectDisposal))
                )
              }
            }
            test(session, FurtherReturnEligibility(true, Some(false)))
          }

          "under limit but previous returns contains other reliefs" in {
            val returns = List.fill(9)(sample[ReturnSummary])
            val session =
              eligableSession(previousSentReturns = Some(sample[PreviousReturnData].copy(summaries = returns)))
            inSequence {
              returns.map { r =>
                mockDisplayReturn(session._2.subscribedDetails.cgtReference, r.submissionId)(
                  Right(genDisplayReturn(otherReliefs = Some(sample[OtherReliefs])))
                )
              }
            }
            test(session, FurtherReturnEligibility(true, Some(false)))
          }
        }
      }
    }
  }
}

class GlarCalculatorEligibilityUtilFlagNotEnabledSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with ReturnsServiceSupport
    with ScalaCheckDrivenPropertyChecks
    with RedirectToStartBehaviour
    with StartingToAmendToFillingOutReturnSpecBehaviour {

  val mockUUIDGenerator = mock[UUIDGenerator]

  def redirectToStartBehaviour(improvementCosts: () => Future[Result]) =
    redirectToStartWhenInvalidJourney(
      improvementCosts,
      {
        case f: FillingOutReturn if f.isFurtherOrAmendReturn.contains(true) => true
        case _: StartingToAmendReturn                                       => true
        case _                                                              => false
      }
    )

  override lazy val additionalConfig = Configuration(
    "amend-and-further-returns-calculator.enabled" -> false
  )

  override val overrideBindings = List[GuiceableModule](
    bind[AuthConnector].toInstance(mockAuthConnector),
    bind[SessionStore].toInstance(mockSessionStore),
    bind[ReturnsService].toInstance(mockReturnsService),
    bind[UUIDGenerator].toInstance(mockUUIDGenerator)
  )

  def eligableSession(
    gainOrLossAfterReliefs: Option[AmountInPence] = None,
    individualUserType: Option[IndividualUserType] = Some(Self),
    userType: UserType = Individual,
    assetType: AssetType = Residential,
    previousSentReturns: Option[PreviousReturnData] = None,
    otherRefliefs: Option[OtherReliefs] = None
  ): (SessionData, FillingOutReturn, DraftSingleDisposalReturn) = {
    val draftReturn = sample[DraftSingleDisposalReturn]
      .copy(
        triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
          .copy(individualUserType = individualUserType, assetType = assetType),
        disposalDetailsAnswers = Some(sample[CompleteDisposalDetailsAnswers]),
        gainOrLossAfterReliefs = gainOrLossAfterReliefs,
        acquisitionDetailsAnswers = Some(sample[CompleteAcquisitionDetailsAnswers]),
        reliefDetailsAnswers = Some(sample[CompleteReliefDetailsAnswers].copy(otherReliefs = otherRefliefs))
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

  lazy val service = instanceOf[GlarCalculatorEligibilityUtil]

  implicit val headerCarrier: HeaderCarrier = mock[HeaderCarrier]
  val message                               = mock[MessagesApi]
  implicit val requestWithSessionData       = RequestWithSessionData(
    None,
    AuthenticatedRequest(
      new MessagesRequest(FakeRequest(), message)
    )
  )

  def performAction(fillingOutReturn: FillingOutReturn): EitherT[Future, Error, FurtherReturnEligibility] =
    service.isEligibleForFurtherReturnOrAmendCalculation(fillingOutReturn)

  "GainOrLossAfterReliefsGlarCalculatorEnabledController" when {

    "handling eligibility request to display the gain or loss after reliefs page" must {

      def test(
        session: (SessionData, FillingOutReturn, DraftSingleDisposalReturn),
        eligibility: FurtherReturnEligibility
      ) = {
        val result = await(performAction(session._2).value)
        result.isRight shouldBe true
        result match {
          case Left(_)      => fail()
          case Right(value) => value shouldBe eligibility
        }
      }

      "Checks that require return display" when {

        "user eligible but flag disabled" when {
          "under limit and displays OK" in {
            val returns = List.fill(9)(sample[ReturnSummary])
            val a       = eligableSession(previousSentReturns = Some(sample[PreviousReturnData].copy(summaries = returns)))
            test(a, FurtherReturnEligibility(false, None))
          }
        }
      }
    }
  }
}
