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
  val initialGainOrLosses              = sample[AmountInPence].copy(value = 2)

  def sessionWithState(
    initialGainOrLoss: AmountInPence,
    disposalDate: DisposalDate
  ): (SessionData, FillingOutReturn) = {
    val journey = sample[FillingOutReturn].copy(
      draftReturn = sample[DraftReturn].copy(
        initialGainOrLoss = Some(initialGainOrLoss)
      )
    )

    SessionData.empty.copy(journeyStatus = Some(journey)) -> journey
  }

  val allQuestionsAnswered: AmountInPence = initialGainOrLosses

  val (session, journey) = sessionWithState(allQuestionsAnswered, sample[DisposalDate])
  val updatedDraftReturn =
    journey.draftReturn.copy(initialGainOrLoss = Some(AmountInPence(0)))
  val updatedSession         = session.copy(journeyStatus = Some(journey.copy(draftReturn = updatedDraftReturn)))
  val fillingOutReturnSample = sample[FillingOutReturn]

  "Check Your Answers" when {
    def performAction(): Future[Result] = controller.checkYourAnswers()(FakeRequest())
    behave like redirectToStartBehaviour(() => performAction())
    "displaying the page" must {

      "have proper contents" when {
        "loading" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(fillingOutReturnSample)
              )
            )
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
            },
            OK
          )
        }
      }
    }
  }

  "InitialGainOrLossController" when {
    def performAction(): Future[Result] = controller.enterInitialGainOrLoss()(FakeRequest())

    behave like redirectToStartBehaviour(performAction)
    "displaying the form" must {

      val fillingOutReturn =
        fillingOutReturnSample.copy(draftReturn = fillingOutReturnSample.draftReturn.copy(initialGainOrLoss = None))

      "display the page with backlink to tasklist" when {
        "initialGainOrLoss is not present in draftReturn" in {
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
        val fillingOutReturnWithGain =
          fillingOutReturnSample.copy(draftReturn =
            fillingOutReturnSample.draftReturn.copy(initialGainOrLoss = Some(AmountInPence(300)))
          )

        "initialGainOrLoss is present in returnDraft" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(fillingOutReturnWithGain)
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
              doc.select("#back").attr("href")        shouldBe routes.InitialGainOrLossController.checkYourAnswers().url
              doc.select("#content > article > form").attr("action") shouldBe routes.InitialGainOrLossController
                .submitInitialGainOrLoss()
                .url
            },
            OK
          )
        }
      }

      "updating the draft return" must {
        def performAction(data: (String, String)*): Future[Result] =
          controller.submitInitialGainOrLoss()(FakeRequest().withFormUrlEncodedBody(data: _*))

        behave like redirectToStartBehaviour(() => performAction())
        "show a technical error page" when {

          "there is an error updating the draft return in return service " in {
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

      "submitting initial gain or loss" must {
        def performAction(data: (String, String)*): Future[Result] =
          controller.submitInitialGainOrLoss()(FakeRequest().withFormUrlEncodedBody(data: _*))

        behave like redirectToStartBehaviour(() => performAction())
        val currentSession =
          sessionWithState(
            AmountInPence(5),
            sample[DisposalDate]
          )._1

        def testFormError(
          data: (String, String)*
        )(expectedErrorMessageKey: String, errorArgs: String*)(pageTitleKey: String, titleArgs: String*)(
          performAction: Seq[(String, String)] => Future[Result],
          currentSession: SessionData = sessionWithState(
            sample[AmountInPence],
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

        "show required error" when {
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
          val fillingOutReturnSample = sample[FillingOutReturn]

          "initial gain or loss has been entered correctly" in {
            val fillingOutReturn =
              fillingOutReturnSample.copy(draftReturn =
                fillingOutReturnSample.draftReturn.copy(initialGainOrLoss = Some(AmountInPence(500)))
              )
            val sampleDraftReturn = fillingOutReturnSample.draftReturn
            val newDraftReturn    = sampleDraftReturn.copy(initialGainOrLoss = Some(AmountInPence(600)))
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                SessionData.empty.copy(
                  journeyStatus = Some(fillingOutReturn)
                )
              )
              mockStoreDraftReturn(
                newDraftReturn
              )(Right(()))
              mockStoreSession(
                SessionData.empty.copy(
                  journeyStatus = Some(fillingOutReturn.copy(draftReturn = newDraftReturn))
                )
              )(Right(()))
            }
            checkIsRedirect(
              performAction("initialGainOrLoss" -> "0", "loss" -> "", "gain" -> "6"),
              routes.InitialGainOrLossController.checkYourAnswers()
            )
          }

        }

        "should not call returnService" when {
          "the same amount of initialGainOrLoss as in the session draftReturn is entered" in {
            val fillingOutReturn =
              fillingOutReturnSample.copy(draftReturn =
                fillingOutReturnSample.draftReturn.copy(initialGainOrLoss = Some(AmountInPence(600)))
              )

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                SessionData.empty.copy(
                  journeyStatus = Some(fillingOutReturn)
                )
              )
            }
            checkIsRedirect(
              performAction("initialGainOrLoss" -> "0", "loss" -> "", "gain" -> "6"),
              routes.InitialGainOrLossController.checkYourAnswers()
            )
          }
        }
      }
    }
  }

}
