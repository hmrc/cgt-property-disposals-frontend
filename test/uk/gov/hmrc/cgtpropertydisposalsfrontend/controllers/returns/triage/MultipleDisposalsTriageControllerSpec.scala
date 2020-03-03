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
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.triage
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.StartingNewDraftReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.IndividualUserType.Self
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{IndividualUserType, _}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.MultipleDisposalsTriageAnswers._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.IncompleteSingleDisposalTriageAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, JourneyStatus, SessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore

import scala.concurrent.Future

class MultipleDisposalsTriageControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with ScalaCheckDrivenPropertyChecks
    with RedirectToStartBehaviour {

  override val overrideBindings =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore)
    )

  lazy val controller = instanceOf[MultipleDisposalsTriageController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  def isValidJourney(journeyStatus: JourneyStatus): Boolean = journeyStatus match {
    case r: StartingNewDraftReturn if (r.newReturnTriageAnswers.isLeft) => true
    case _                                                              => false
  }

  def sessionDataWithStartingNewDraftReturn(
    multipleDisposalsAnswers: MultipleDisposalsTriageAnswers
  ): (SessionData, StartingNewDraftReturn) = {
    val startingNewDraftReturn = sample[StartingNewDraftReturn].copy(
      newReturnTriageAnswers = Left(multipleDisposalsAnswers)
    )

    SessionData.empty.copy(journeyStatus = Some(startingNewDraftReturn)) -> startingNewDraftReturn
  }

  "MultipleDisposalsTriageController" when {

    "handling requests to display the guidance page" must {

      def performAction(): Future[Result] =
        controller.guidance()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(performAction, isValidJourney)

      "display the page" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionDataWithStartingNewDraftReturn(IncompleteMultipleDisposalsAnswers.empty)._1)
        }

        checkPageIsDisplayed(
          performAction,
          messageFromMessageKey("multiple-disposals.guidance.title"), { doc =>
            doc.select("#back").attr("href") shouldBe triage.routes.InitialTriageQuestionsController
              .howManyProperties()
              .url
            doc
              .select("#content > article > form")
              .attr("action") shouldBe routes.MultipleDisposalsTriageController
              .guidanceSubmit()
              .url
          }
        )
      }

    }

    "handling submits on the guidance page" must {

      def performAction(): Future[Result] =
        controller.guidanceSubmit()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(performAction, isValidJourney)

      "redirect to how many disposals page" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionDataWithStartingNewDraftReturn(IncompleteMultipleDisposalsAnswers.empty)._1)
        }
        checkIsRedirect(performAction(), routes.MultipleDisposalsTriageController.howManyDisposals())
      }

    }

    "handling requests to display the how many disposals page" must {

      def performAction(): Future[Result] =
        controller.howManyDisposals()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(performAction, isValidJourney)

      "display the page" in {
        mockAuthWithNoRetrievals()
        mockGetSession(sessionDataWithStartingNewDraftReturn(IncompleteMultipleDisposalsAnswers.empty)._1)

        checkPageIsDisplayed(
          performAction,
          messageFromMessageKey("multiple-disposals.howManyProperties.title"), { doc =>
            doc.select("#back").attr("href") shouldBe triage.routes.MultipleDisposalsTriageController
              .guidance()
              .url
            doc
              .select("#content > article > form")
              .attr("action") shouldBe routes.MultipleDisposalsTriageController
              .howManyDisposalsSubmit()
              .url
          }
        )

      }

    }

    "handling submits on the how many disposals page" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.howManyDisposalsSubmit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      "redirect to disposal method when user enters number of properties as one" in {
        val (session, journey) = sessionDataWithStartingNewDraftReturn(
          IncompleteMultipleDisposalsAnswers.empty.copy(
            individualUserType = Some(Self)
          )
        )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockStoreSession(
            session.copy(journeyStatus = Some(
              journey.copy(
                newReturnTriageAnswers = Right(
                  IncompleteSingleDisposalTriageAnswers.empty.copy(
                    individualUserType         = Some(Self),
                    hasConfirmedSingleDisposal = true
                  )
                )
              )
            )
            )
          )(Right(()))
        }

        checkIsRedirect(
          performAction("numberOfProperties" -> "1"),
          routes.SingleDisposalsTriageController.checkYourAnswers()
        )

      }

      "user has not answered how many disposals section and " +
      "redirect to dummy page when user enters number of properties more than one" in {
        val answers = IncompleteMultipleDisposalsAnswers.empty.copy(
          individualUserType = Some(Self)
        )
        val (session, journey) = sessionDataWithStartingNewDraftReturn(answers)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockStoreSession(
            session.copy(journeyStatus = Some(
              journey.copy(
                newReturnTriageAnswers = Left(answers.copy(numberOfProperties = Some(5)))
              )
            )
            )
          )(Right(()))
        }

        checkIsRedirect(
          performAction("numberOfProperties" -> "5"),
          routes.MultipleDisposalsTriageController.checkYourAnswers()
        )
      }
      "user has not answered how many disposals section and " +
      "redirect to dummy page when user enters number of properties value which is moreThanOne" in {
        val answers = sample[CompleteMultipleDisposalsAnswers]

        val (session, journey) = sessionDataWithStartingNewDraftReturn(answers)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockStoreSession(
            session.copy(journeyStatus = Some(
              journey.copy(
                newReturnTriageAnswers = Left(answers.copy(numberOfProperties = 5))
              )
            )
            )
          )(Right(()))
        }

        checkIsRedirect(
          performAction("numberOfProperties" -> "5"),
          routes.MultipleDisposalsTriageController.checkYourAnswers()
        )
      }
      "user has already answered how many disposals section and " +
      "redirect to dummy page when user re-enters different number of properties value which is moreThanOne" in {
        val answers = sample[CompleteMultipleDisposalsAnswers].copy(numberOfProperties = 9)

        val (session, journey) = sessionDataWithStartingNewDraftReturn(answers)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockStoreSession(
            session.copy(journeyStatus = Some(
              journey.copy(
                newReturnTriageAnswers = Left(answers.copy(numberOfProperties = 3))
              )
            )
            )
          )(Right(()))
        }

        checkIsRedirect(
          performAction("numberOfProperties" -> "3"),
          routes.MultipleDisposalsTriageController.checkYourAnswers()
        )
      }

      "user has  not answered how many disposals section and " +
      "submit request without entering numberOfProperties value" in {
        val answers = IncompleteMultipleDisposalsAnswers.empty.copy(
          individualUserType = Some(Self)
        )
        val (session, journey) = sessionDataWithStartingNewDraftReturn(answers)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        val result = performAction()
        status(result) shouldBe BAD_REQUEST
      }

    }

    "handling requests to display the check your answers page" must {

      def performAction(): Future[Result] =
        controller.checkYourAnswers()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(performAction, isValidJourney)

      "redirect to the how many properties page when no individual user type can be found" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionDataWithStartingNewDraftReturn(IncompleteMultipleDisposalsAnswers.empty)._1)
        }

        checkIsRedirect(performAction(), routes.InitialTriageQuestionsController.howManyProperties())
      }

      "redirect to the multiple disposals guidance page when no individual user type can be found" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            sessionDataWithStartingNewDraftReturn(
              IncompleteMultipleDisposalsAnswers.empty.copy(individualUserType = Some(IndividualUserType.Self))
            )._1
          )
        }

        checkIsRedirect(performAction(), routes.MultipleDisposalsTriageController.guidance())
      }

    }

  }

}
