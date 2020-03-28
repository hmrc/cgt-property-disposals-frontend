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

import org.scalacheck.Gen
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.http.Status.BAD_REQUEST
import play.api.i18n.{Lang, Messages, MessagesApi, MessagesImpl}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.AmountOfMoneyErrorScenarios.amountOfMoneyErrorScenarios
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.ReturnsServiceSupport
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport, returns}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.FillingOutReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.AmountInPence
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.DraftSingleDisposalReturn
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

  def redirectToStartBehaviour(performAction: () => Future[Result]) =
    redirectToStartWhenInvalidJourney(
      performAction, {
        case _: FillingOutReturn => true
        case _                   => false
      }
    )

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  override val overrideBindings = List[GuiceableModule](
    bind[AuthConnector].toInstance(mockAuthConnector),
    bind[SessionStore].toInstance(mockSessionStore),
    bind[ReturnsService].toInstance(mockReturnsService)
  )

  lazy val controller                  = instanceOf[InitialGainOrLossController]
  implicit lazy val messages: Messages = MessagesImpl(Lang("en"), messagesApi)

  def sessionWithState(
    initialGainOrLoss: Option[AmountInPence]
  ): (SessionData, FillingOutReturn, DraftSingleDisposalReturn) = {
    val draftReturn = sample[DraftSingleDisposalReturn].copy(initialGainOrLoss = initialGainOrLoss)
    val journey     = sample[FillingOutReturn].copy(draftReturn                = draftReturn)

    (
      SessionData.empty.copy(journeyStatus = Some(journey)),
      journey,
      draftReturn
    )
  }

  "InitialGainOrLossController" when {

    "handling requests to display the initial gain or loss page" must {
      def performAction(): Future[Result] = controller.enterInitialGainOrLoss()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      "display the page with backlink to tasklist" when {

        "initialGainOrLoss is not present in draftReturn" in {
          val draftReturn      = sample[DraftSingleDisposalReturn].copy(initialGainOrLoss = None)
          val fillingOutReturn = sample[FillingOutReturn].copy(draftReturn                = draftReturn)

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
              doc.select(".govuk-caption-xl").html()  should include("Initial gain or loss")
              doc.select("#initialGainOrLoss").html() should include("Did you make an initial gain or loss?")
              doc.select("#back").attr("href")        shouldBe returns.routes.TaskListController.taskList().url
              doc.select("#content > article > form").attr("action") shouldBe routes.InitialGainOrLossController
                .submitInitialGainOrLoss()
                .url
            },
            OK
          )
        }
      }

      "display the page with backlink to check your answers" when {

        "initialGainOrLoss is present in returnDraft" in {
          val draftReturn = sample[DraftSingleDisposalReturn]
            .copy(initialGainOrLoss = Some(AmountInPence(300L)))
          val fillingOutReturn = sample[FillingOutReturn].copy(draftReturn = draftReturn)

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
              doc.select(".govuk-caption-xl").html()                          should include("Initial gain or loss")
              doc.select("#initialGainOrLoss").html()                         should include("Did you make an initial gain or loss?")
              doc.select("#initialGainOrLoss-0-content > div > label").text() should be("Initial gain amount")
              doc.select("#initialGainOrLoss-1-content > div > label").text() should be("Initial loss amount")
              doc.select("#back").attr("href")                                shouldBe routes.InitialGainOrLossController.checkYourAnswers().url
              doc.select("#content > article > form").attr("action") shouldBe routes.InitialGainOrLossController
                .submitInitialGainOrLoss()
                .url
            }
          )
        }
      }

    }

    "submitting initial gain or loss" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.submitInitialGainOrLoss()(FakeRequest().withFormUrlEncodedBody(data: _*))

      def updateDraftReturn(d: DraftSingleDisposalReturn, newAnswer: AmountInPence) =
        d.copy(
          initialGainOrLoss          = Some(newAnswer),
          reliefDetailsAnswers       = d.reliefDetailsAnswers.map(_.unsetPrrAndLettingRelief()),
          yearToDateLiabilityAnswers = d.yearToDateLiabilityAnswers.flatMap(_.unsetAllButIncomeDetails())
        )

      behave like redirectToStartBehaviour(() => performAction())

      "show a technical error page" when {

        "there is an error updating the draft return in return service " in {
          val (session, journey, draftReturn) =
            sessionWithState(Some(AmountInPence(1L)))
          val updatedDraftReturn = updateDraftReturn(draftReturn, AmountInPence(0L))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(
              updatedDraftReturn,
              journey.subscribedDetails.cgtReference,
              journey.agentReferenceNumber
            )(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction("initialGainOrLoss" -> "2"))
        }

        "there is an error updating the session" in {
          val (session, journey, draftReturn) = sessionWithState(None)
          val updatedDraftReturn              = updateDraftReturn(draftReturn, AmountInPence(0L))
          val updatedJourney                  = journey.copy(draftReturn = updatedDraftReturn)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(
              updatedDraftReturn,
              journey.subscribedDetails.cgtReference,
              journey.agentReferenceNumber
            )(Right(()))
            mockStoreSession(session.copy(journeyStatus = Some(updatedJourney)))(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction("initialGainOrLoss" -> "2"))
        }
      }

      "show a form error" when {

        def checkIfValueExistsForKey(expectedErrorMessageKey: String) = {
          (messages(expectedErrorMessageKey) === expectedErrorMessageKey) shouldBe false
          messages(expectedErrorMessageKey).trim().isEmpty                shouldBe false
        }

        def testFormError(
          data: (String, String)*
        )(expectedErrorMessageKey: String, errorArgs: String*)(pageTitleKey: String, titleArgs: String*)(
          performAction: Seq[(String, String)] => Future[Result]
        ): Unit = {

          checkIfValueExistsForKey(expectedErrorMessageKey)

          val session = sessionWithState(None)._1

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
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
          )("initialGainOrLoss.title")(performAction)

        "no option is selected" in {
          test(
            "initialGainOrLoss" -> "",
            "loss"              -> "",
            "gain"              -> ""
          )("initialGainOrLoss.error.required")
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
      }

      "redirect to check your answers" when {

        "initial gain or loss has been entered correctly" in {
          forAll(Gen.choose(0L, 100L).map(AmountInPence(_))) { amountInPence: AmountInPence =>
            val (session, journey, draftReturn) =
              sessionWithState(Some(AmountInPence(amountInPence.value - 1L)))
            val newDraftReturn = updateDraftReturn(draftReturn, amountInPence)
            val updatedJourney = journey.copy(draftReturn = newDraftReturn)

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockStoreDraftReturn(
                newDraftReturn,
                updatedJourney.subscribedDetails.cgtReference,
                updatedJourney.agentReferenceNumber
              )(Right(()))
              mockStoreSession(session.copy(journeyStatus = Some(updatedJourney)))(Right(()))
            }
            checkIsRedirect(
              performAction("initialGainOrLoss" -> "0", "loss" -> "", "gain" -> s"${amountInPence.inPounds()}"),
              routes.InitialGainOrLossController.checkYourAnswers()
            )
          }
        }

      }

      "should not call returnService" when {
        "the same amount of initialGainOrLoss as in the session draftReturn is entered" in {
          val (session, _, _) =
            sessionWithState(Some(AmountInPence(600L)))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }
          checkIsRedirect(
            performAction("initialGainOrLoss" -> "0", "loss" -> "", "gain" -> "6"),
            routes.InitialGainOrLossController.checkYourAnswers()
          )
        }
      }
    }

    "handling requests to display the check you answers page" must {

      def performAction(): Future[Result] = controller.checkYourAnswers()(FakeRequest())

      behave like redirectToStartBehaviour(() => performAction())

      "redirect to the initial gain or loss page" when {

        "the question has not been answered yet" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionWithState(None)._1)
          }

          checkIsRedirect(performAction(), routes.InitialGainOrLossController.enterInitialGainOrLoss())
        }
      }

      "have proper contents" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionWithState(Some(AmountInPence(1L)))._1)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey(
            "initialGainOrLoss.cya.title"
          ), { doc =>
            doc.select("body").html() should include(Messages("initialGainOrLoss.cya.title"))
            doc.select("#content > article > form").attr("action") shouldBe routes.InitialGainOrLossController
              .checkYourAnswersSubmit()
              .url
          }
        )
      }
    }

    "handling submits on the check your answers page" must {
      def performAction(): Future[Result] = controller.checkYourAnswersSubmit()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      "redirect to taskList" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionWithState(Some(AmountInPence(1L)))._1)
        }
        checkIsRedirect(performAction(), controllers.returns.routes.TaskListController.taskList())
      }
    }
  }

}
