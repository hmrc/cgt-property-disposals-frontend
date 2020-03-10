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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.initialgainorloss

import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.http.Status.BAD_REQUEST
import play.api.i18n.{Lang, Messages, MessagesApi, MessagesImpl}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers.{contentAsString, status, _}
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.AmountOfMoneyErrorScenarios.amountOfMoneyErrorScenarios
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.ReturnsServiceSupport
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport, returns}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators.{sample, _}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.FillingOutReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.AmountInPence
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.InitialGainOrLossAnswers.CompleteInitialGainOrLossAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{DisposalDate, DraftReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, SessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService

import scala.concurrent.Future

class InitialGainOrLossControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with ReturnsServiceSupport
    with ScalaCheckDrivenPropertyChecks
    with RedirectToStartBehaviour {

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  override val overrideBindings = List[GuiceableModule](
    bind[AuthConnector].toInstance(mockAuthConnector),
    bind[SessionStore].toInstance(mockSessionStore),
    bind[ReturnsService].toInstance(mockReturnsService)
  )

  lazy val controller                  = instanceOf[InitialGainOrLossController]
  implicit lazy val messages: Messages = MessagesImpl(Lang("en"), messagesApi)
  val initialGainOrLosses              = sample[AmountInPence].copy(value = 2)

  def sessionWithState(
    answers: CompleteInitialGainOrLossAnswers,
    disposalDate: DisposalDate
  ): (SessionData, FillingOutReturn) = {
    val journey = sample[FillingOutReturn].copy(
      draftReturn = sample[DraftReturn].copy(
        initialGainOrLossAnswers = Some(answers)
      )
    )

    SessionData.empty.copy(journeyStatus = Some(journey)) -> journey
  }

  val allQuestionsAnswered: CompleteInitialGainOrLossAnswers = CompleteInitialGainOrLossAnswers(
    initialGainOrLosses
  )

  val (session, journey) = sessionWithState(allQuestionsAnswered, sample[DisposalDate])
  val updatedDraftReturn =
    journey.draftReturn.copy(initialGainOrLossAnswers = Some(CompleteInitialGainOrLossAnswers(AmountInPence(0))))
  val updatedSession = session.copy(journeyStatus = Some(journey.copy(draftReturn = updatedDraftReturn)))

  "InitialGainOrLossController" should {
    "show display" when {
      def performAction(): Future[Result] = controller.enterInitialGainOrLoss()(FakeRequest())
      "display a page returning 200" in {

        val fillingOutReturn = sample[FillingOutReturn]

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            SessionData.empty.copy(
              journeyStatus = Some(fillingOutReturn)
            )
          )
        }

        val result: Future[Result] = performAction()
        status(result) shouldBe OK
      }

      "have proper contents" in {
        val fillingOutReturn = sample[FillingOutReturn]

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            SessionData.empty.copy(
              journeyStatus = Some(fillingOutReturn)
            )
          )
        }

        val result: Future[Result] = performAction()

        contentAsString(result) should include("Initial gain or loss")
        contentAsString(result) should include("Did you make an initial gain or loss?")
      }

      "display the page" in {
        val fillingOutReturn = sample[FillingOutReturn]
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            SessionData.empty.copy(
              journeyStatus = Some(fillingOutReturn)
            )
          )
        }
        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey(
            "initialGainOrLoss.title"
          ), { doc =>
            doc.select("#back").attr("href") shouldBe returns.routes.TaskListController.taskList().url
            doc.select("#content > article > form").attr("action") shouldBe routes.InitialGainOrLossController
              .submitInitialGainOrLoss()
              .url
          }
        )
      }

    }
  }

  "show an error page" when {

    def performAction(data: (String, String)*): Future[Result] =
      controller.submitInitialGainOrLoss()(FakeRequest().withFormUrlEncodedBody(data: _*))

    "the user has just answered all the questions and" when {

      "there is an error updating the draft return" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockStoreDraftReturn(updatedDraftReturn)(Left(Error("")))
        }

        checkIsTechnicalErrorPage(performAction("initialGainOrLoss" -> "2", "gain" -> "", "loss" -> ""))
      }

      "there is an error updating the session" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockStoreDraftReturn(updatedDraftReturn)(Right(()))
          mockStoreSession(updatedSession)(Left(Error("")))
        }

        checkIsTechnicalErrorPage(performAction("initialGainOrLoss" -> "2", "gain" -> "", "loss" -> ""))
      }

    }

  }

  "show a form error" when {
    def performAction(data: (String, String)*): Future[Result] =
      controller.submitInitialGainOrLoss()(FakeRequest().withFormUrlEncodedBody(data: _*))
    val currentSession =
      sessionWithState(
        sample[CompleteInitialGainOrLossAnswers].copy(initialGainOrLoss = AmountInPence(5)),
        sample[DisposalDate]
      )._1

    def testFormError(
      data: (String, String)*
    )(expectedErrorMessageKey: String, errorArgs: String*)(pageTitleKey: String, titleArgs: String*)(
      performAction: Seq[(String, String)] => Future[Result],
      currentSession: SessionData = sessionWithState(
        sample[CompleteInitialGainOrLossAnswers],
        sample[DisposalDate]
      )._1
    ): Unit = {
      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(currentSession)
      }

      checkPageIsDisplayed(
        performAction(data),
        messageFromMessageKey(pageTitleKey, titleArgs), { doc =>
          doc.select("#error-summary-display > ul > li > a").text() shouldBe messageFromMessageKey(
            expectedErrorMessageKey,
            errorArgs: _*
          )
        },
        BAD_REQUEST
      )
    }

    def test(data: (String, String)*)(expectedErrorKey: String): Unit =
      testFormError(data: _*)(
        expectedErrorKey
      )("initialGainOrLoss.title")(performAction, currentSession)

    "no option is selected" in {
      test()("initialGainOrLoss.error.required")
    }

    "the amount of gain is invalid" in {
      amountOfMoneyErrorScenarios("gain").foreach { scenario =>
        withClue(s"For $scenario: ") {
          val data = ("initialGainOrLoss" -> "0") :: scenario.formData
          test(data: _*)(scenario.expectedErrorMessageKey)
        }
      }
    }

    "the amount of loss is invalid" in {
      amountOfMoneyErrorScenarios("loss").foreach { scenario =>
        withClue(s"For $scenario: ") {
          val data = ("initialGainOrLoss" -> "1") :: scenario.formData
          test(data: _*)(scenario.expectedErrorMessageKey)
        }
      }
    }

    "the amount of gain is zero" in {
      test(
        "initialGainOrLoss" -> "0",
        "gain"              -> "0"
      )("gain.error.tooSmall")
    }

    "the amount of loss is bad format" in {
      test(
        "initialGainOrLoss" -> "1",
        "loss"              -> "three hundred +10,931"
      )("loss.error.invalid")
    }

    "the amount of loss is too big" in {
      test(
        "initialGainOrLoss" -> "1",
        "loss"              -> "51000000000.980"
      )("loss.error.tooLarge")
    }

    "none selected" in {
      test(
        "initialGainOrLoss" -> "",
        "loss"              -> "",
        "gain"              -> ""
      )("initialGainOrLoss.error.required")
    }
  }
}
