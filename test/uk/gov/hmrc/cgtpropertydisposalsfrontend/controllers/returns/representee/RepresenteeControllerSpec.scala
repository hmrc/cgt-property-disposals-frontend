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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.representee

import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.i18n.MessagesApi
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.ReturnsServiceSupport
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, StartingNewDraftReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.SessionData
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.IndividualUserType._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.MultipleDisposalsTriageAnswers.CompleteMultipleDisposalsTriageAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.CompleteSingleDisposalTriageAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{DraftMultipleDisposalsReturn, IndividualUserType, RepresenteeAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService

import scala.concurrent.Future

class RepresenteeControllerSpec
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

  lazy val controller = instanceOf[RepresenteeController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  def sessionWithStartingNewDraftReturn(
    answers: Option[RepresenteeAnswers],
    individualUserType: Option[IndividualUserType]
  ): (SessionData, StartingNewDraftReturn) = {
    val journey = sample[StartingNewDraftReturn].copy(
      representeeAnswers = answers,
      newReturnTriageAnswers =
        Right(sample[CompleteSingleDisposalTriageAnswers].copy(individualUserType = individualUserType))
    )
    val session = SessionData.empty.copy(journeyStatus = Some(journey))
    session -> journey
  }

  def sessionWithStartingNewDraftReturn(
    answers: RepresenteeAnswers,
    representativeType: Either[PersonalRepresentative.type, Capacitor.type]
  ): (SessionData, StartingNewDraftReturn) =
    sessionWithStartingNewDraftReturn(Some(answers), Some(representativeType.merge))

  def sessionWithFillingOutReturn(
    answers: Option[RepresenteeAnswers],
    individualUserType: Option[IndividualUserType]
  ): (SessionData, FillingOutReturn) = {
    val draftReturn = sample[DraftMultipleDisposalsReturn].copy(
      representeeAnswers = answers,
      triageAnswers      = sample[CompleteMultipleDisposalsTriageAnswers].copy(individualUserType = individualUserType)
    )
    val journey = sample[FillingOutReturn].copy(draftReturn = draftReturn)

    val session = SessionData.empty.copy(journeyStatus = Some(journey))
    session -> journey
  }

  def sessionWithFillingOutReturn(
    answers: RepresenteeAnswers,
    representativeType: Either[PersonalRepresentative.type, Capacitor.type]
  ): (SessionData, FillingOutReturn) =
    sessionWithFillingOutReturn(Some(answers), Some(representativeType.merge))

  "RepresenteeController" when {

    "handling requests to display the enter name page" must {

      def performAction(): Future[Result] = controller.enterName()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      behave like nonCapacitorOrPersonalRepBehaviour(performAction)

      "display the page" when {

        "the user is starting a draft return and" when {

          "the user is a capacitor" in {}

          "the user is a personal representative" in {}

          "the section is complete" in {}

        }

        "the user has already started a draft return and" when {

          "the user is a capacitor" in {}

          "the user is a personal representative" in {}

          "the section is complete" in {}

        }

      }

    }

  }

  def redirectToStartBehaviour(performAction: () => Future[Result]): Unit =
    redirectToStartWhenInvalidJourney(performAction, {
      case _: StartingNewDraftReturn | _: FillingOutReturn => true
      case _                                               => false
    })

  def nonCapacitorOrPersonalRepBehaviour(performAction: () => Future[Result]): Unit =
    "redirect to the task list endpoint" when {
      "the user has selected 'self' for the person they are submitting the return for" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            sessionWithStartingNewDraftReturn(None, Some(Self))._1
          )
        }

        checkIsRedirect(performAction(), returns.routes.TaskListController.taskList())
      }

      "the user has not indicated who they are submitting the return for" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            sessionWithStartingNewDraftReturn(None, None)._1
          )
        }

        checkIsRedirect(performAction(), returns.routes.TaskListController.taskList())
      }
    }

}
