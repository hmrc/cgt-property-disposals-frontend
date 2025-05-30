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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns

import org.scalamock.scalatest.MockFactory
import play.api.i18n.MessagesApi
import play.api.mvc.request.RequestTarget
import play.api.mvc.{Action, AnyContent}
import play.api.test.FakeRequest
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.amend.routes.{AmendReturnController => amendRoutes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, StartingToAmendReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.CompleteReturnGen.*
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.CompleteReturnGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.JourneyStatusGen.*
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.ReturnGen.*
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.UUIDGenerator
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.CompleteReturn.{CompleteMultipleDisposalsReturn, CompleteMultipleIndirectDisposalReturn, CompleteSingleDisposalReturn, CompleteSingleIndirectDisposalReturn, CompleteSingleMixedUseDisposalReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{CompleteReturnWithSummary, Error, SessionData}

import java.time.{Clock, LocalDate}
import java.util.UUID

trait StartingToAmendToFillingOutReturnSpecBehaviour {
  this: ControllerSpec & SessionSupport & AuthSupport & MockFactory =>

  def amendReturnToFillingOutReturnSpecBehaviour(
    performAction: Action[AnyContent],
    mockUUIDGenerator: UUIDGenerator,
    expectedRedirectToOverride: Option[String] = None
  )(implicit messagesApi: MessagesApi): Unit = {

    "redirect to the same endpoint when converting from a start amend journey to a filling out return journey" should {

      def test(
        completeReturn: CompleteReturn,
        expectedDraftReturn: DraftReturn,
        shouldDisplayGainOrLossAfterReliefs: Boolean
      ): Unit = {
        val startingToAmend = sample[StartingToAmendReturn].copy(
          originalReturn = sample[CompleteReturnWithSummary].copy(completeReturn = completeReturn),
          unmetDependencyFieldUrl = Some("abc")
        )

        val fillingOutReturn = FillingOutReturn(
          startingToAmend.subscribedDetails,
          startingToAmend.ggCredId,
          startingToAmend.agentReferenceNumber,
          expectedDraftReturn,
          startingToAmend.previousSentReturns,
          Some(
            AmendReturnData(
              startingToAmend.originalReturn,
              shouldDisplayGainOrLossAfterReliefs
            )
          )
        )

        val uri     = "/uri"
        val request = FakeRequest().withTarget(RequestTarget(uri, "", Map.empty))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData.empty.copy(journeyStatus = Some(startingToAmend)))
          (() => mockUUIDGenerator.nextId()).expects().returning(expectedDraftReturn.id)
          mockStoreSession(SessionData.empty.copy(journeyStatus = Some(fillingOutReturn)))(Right(()))
        }

        checkIsRedirect(performAction(request), expectedRedirectToOverride.getOrElse(uri))
      }

      "given a CompleteMultipleDisposal return" in {
        val completeReturn = sample[CompleteMultipleDisposalsReturn]
        val draftReturn    = DraftMultipleDisposalsReturn(
          UUID.randomUUID(),
          completeReturn.triageAnswers,
          Some(completeReturn.examplePropertyDetailsAnswers),
          None,
          None,
          None,
          None,
          completeReturn.gainOrLossAfterReliefs,
          LocalDate.now(Clock.systemUTC())
        )

        test(completeReturn, draftReturn, shouldDisplayGainOrLossAfterReliefs = true)
      }

      "given a CompleteSingleDisposal return" in {
        val completeReturn = sample[CompleteSingleDisposalReturn]
        val draftReturn    = DraftSingleDisposalReturn(
          UUID.randomUUID(),
          completeReturn.triageAnswers,
          Some(completeReturn.propertyAddress),
          Some(completeReturn.disposalDetails),
          Some(completeReturn.acquisitionDetails),
          Some(completeReturn.reliefDetails),
          None,
          None,
          None,
          None,
          None,
          completeReturn.gainOrLossAfterReliefs,
          LocalDate.now(Clock.systemUTC())
        )

        test(
          completeReturn,
          draftReturn,
          shouldDisplayGainOrLossAfterReliefs = true
        )
      }

      "given a CompleteSingleIndirectDisposal return" in {
        val completeReturn = sample[CompleteSingleIndirectDisposalReturn]
        val draftReturn    = DraftSingleIndirectDisposalReturn(
          UUID.randomUUID(),
          completeReturn.triageAnswers,
          Some(completeReturn.companyAddress),
          Some(completeReturn.disposalDetails),
          Some(completeReturn.acquisitionDetails),
          None,
          None,
          None,
          None,
          completeReturn.gainOrLossAfterReliefs,
          LocalDate.now(Clock.systemUTC())
        )

        test(completeReturn, draftReturn, shouldDisplayGainOrLossAfterReliefs = true)
      }

      "given a CompleteMultipleIndirectDisposal return" in {
        val completeReturn = sample[CompleteMultipleIndirectDisposalReturn]
        val draftReturn    = DraftMultipleIndirectDisposalsReturn(
          UUID.randomUUID(),
          completeReturn.triageAnswers,
          Some(completeReturn.exampleCompanyDetailsAnswers),
          None,
          None,
          None,
          None,
          completeReturn.gainOrLossAfterReliefs,
          LocalDate.now(Clock.systemUTC())
        )

        test(completeReturn, draftReturn, shouldDisplayGainOrLossAfterReliefs = true)
      }

      "given a CompleteSingleMixedUseDisposal return return" in {
        val completeReturn = sample[CompleteSingleMixedUseDisposalReturn]
        val draftReturn    = DraftSingleMixedUseDisposalReturn(
          UUID.randomUUID(),
          completeReturn.triageAnswers,
          Some(completeReturn.propertyDetailsAnswers),
          None,
          None,
          None,
          None,
          completeReturn.gainOrLossAfterReliefs,
          LocalDate.now(Clock.systemUTC())
        )

        test(completeReturn, draftReturn, shouldDisplayGainOrLossAfterReliefs = true)
      }

    }

    "show an error page" when {

      "there is an error updating the session when converting from a start amend journey to a filling out return journey" in {
        val completeReturn = sample[CompleteMultipleDisposalsReturn]
        val draftReturn    = DraftMultipleDisposalsReturn(
          UUID.randomUUID(),
          completeReturn.triageAnswers,
          Some(completeReturn.examplePropertyDetailsAnswers),
          None,
          None,
          None,
          None,
          completeReturn.gainOrLossAfterReliefs,
          LocalDate.now(Clock.systemUTC())
        )

        val startingToAmend = sample[StartingToAmendReturn].copy(
          originalReturn = sample[CompleteReturnWithSummary].copy(completeReturn = completeReturn),
          unmetDependencyFieldUrl = Some("abc")
        )

        val fillingOutReturn = FillingOutReturn(
          startingToAmend.subscribedDetails,
          startingToAmend.ggCredId,
          startingToAmend.agentReferenceNumber,
          draftReturn,
          startingToAmend.previousSentReturns,
          Some(
            AmendReturnData(
              startingToAmend.originalReturn,
              shouldDisplayGainOrLossAfterReliefs = true
            )
          )
        )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData.empty.copy(journeyStatus = Some(startingToAmend)))
          (() => mockUUIDGenerator.nextId()).expects().returning(draftReturn.id)
          mockStoreSession(SessionData.empty.copy(journeyStatus = Some(fillingOutReturn)))(Left(Error("")))
        }

        checkIsTechnicalErrorPage(performAction(FakeRequest()))
      }
    }

  }

  def markUnmetDependencyBehaviour(
    performAction: Action[AnyContent]
  )(implicit messagesApi: MessagesApi): Unit = {

    val startingToAmend = sample[StartingToAmendReturn]
    val uri             = "/uri"
    val request         = FakeRequest().withTarget(RequestTarget(uri, "", Map.empty))

    "redirect to the unmet dependency page" when {

      "the session has been updated successfully" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            SessionData.empty.copy(
              journeyStatus = Some(startingToAmend)
            )
          )
          mockStoreSession(
            SessionData.empty.copy(
              journeyStatus = Some(startingToAmend.copy(unmetDependencyFieldUrl = Some(uri)))
            )
          )(Right(()))
        }

        checkIsRedirect(performAction(request), amendRoutes.unmetDependency())
      }

    }

    "show an error page" when {

      "there is an error updating the session to mark the unmet dependency" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            SessionData.empty.copy(
              journeyStatus = Some(startingToAmend)
            )
          )
          mockStoreSession(
            SessionData.empty.copy(
              journeyStatus = Some(startingToAmend.copy(unmetDependencyFieldUrl = Some(uri)))
            )
          )(Left(Error("")))
        }

        checkIsTechnicalErrorPage(performAction(request))
      }

    }

  }

}
