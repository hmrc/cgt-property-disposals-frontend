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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.triage

import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.i18n.MessagesApi
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.{Call, Result}
import play.api.test.FakeRequest
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.ReturnsServiceSupport
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators.sample
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{JourneyStatus, SessionData, UserType}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, PreviousReturnData, StartingNewDraftReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.AgentReferenceNumber
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.{IndividualName, TrustName}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.SubscribedDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.IndividualUserType.{Capacitor, PersonalRepresentative, PersonalRepresentativeInPeriodOfAdmin}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.RepresenteeAnswers.CompleteRepresenteeAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{DateOfDeath, MultipleDisposalsTriageAnswers, RepresenteeAnswers, ReturnSummary}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.MultipleDisposalsTriageAnswers.IncompleteMultipleDisposalsTriageAnswers

import scala.concurrent.Future

class FurtherReturnGuidanceControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with ScalaCheckDrivenPropertyChecks
    with RedirectToStartBehaviour
    with ReturnsServiceSupport {

  override val overrideBindings =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore),
      bind[ReturnsService].toInstance(mockReturnsService)
    )

  lazy val controller = instanceOf[FurtherReturnGuidanceController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  def isValidJourney(journeyStatus: JourneyStatus): Boolean =
    journeyStatus match {
      case _: StartingNewDraftReturn | _: FillingOutReturn => true
      case _                                               => false
    }

  def sessionDataWithStartingNewDraftReturn(
    multipleDisposalsAnswers: MultipleDisposalsTriageAnswers,
    name: Either[TrustName, IndividualName] = Right(sample[IndividualName]),
    userType: UserType = UserType.Individual,
    previousSentReturns: Option[List[ReturnSummary]] = None,
    representeeAnswers: Option[RepresenteeAnswers] = None
  ): (SessionData, StartingNewDraftReturn) = {
    val individualUserType     = multipleDisposalsAnswers.fold(_.individualUserType, _.individualUserType)
    val startingNewDraftReturn = sample[StartingNewDraftReturn].copy(
      newReturnTriageAnswers = Left(multipleDisposalsAnswers),
      subscribedDetails = sample[SubscribedDetails].copy(name = name),
      agentReferenceNumber =
        if (userType === UserType.Agent) Some(sample[AgentReferenceNumber])
        else None,
      representeeAnswers = representeeAnswers.orElse {
        if (
          individualUserType.contains(PersonalRepresentative) || individualUserType
            .contains(PersonalRepresentativeInPeriodOfAdmin)
        )
          Some(
            sample[CompleteRepresenteeAnswers]
              .copy(dateOfDeath = Some(sample[DateOfDeath]))
          )
        else if (individualUserType.contains(Capacitor))
          Some(sample[CompleteRepresenteeAnswers].copy(dateOfDeath = None))
        else None
      },
      previousSentReturns = previousSentReturns.map(PreviousReturnData(_, None))
    )
    val sessionData            = SessionData.empty.copy(
      journeyStatus = Some(startingNewDraftReturn),
      userType = Some(userType)
    )
    sessionData -> startingNewDraftReturn
  }

  "FurtherReturnGuidanceController" when {

    "handling requests to display the further return guidance page" must {

      def performAction(): Future[Result] =
        controller.guidance()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(
        performAction,
        isValidJourney
      )

      "display the page" when {

        def test(
          session: SessionData,
          expectedBackLink: Call
        ): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }
          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("furtherReturnGuidance.title"),
            doc => doc.select("#back").attr("href") shouldBe expectedBackLink.url
          )
        }

        "the user has not started a draft return yet and" when {
          "the section is incomplete" in {
            test(
              sessionDataWithStartingNewDraftReturn(
                IncompleteMultipleDisposalsTriageAnswers.empty,
                name = Right(sample[IndividualName])
              )._1,
              controllers.routes.StartController.start()
            )
          }
        }

      }

    }

    "handling submitted answers to the further return guidance page" must {}

  }

}
