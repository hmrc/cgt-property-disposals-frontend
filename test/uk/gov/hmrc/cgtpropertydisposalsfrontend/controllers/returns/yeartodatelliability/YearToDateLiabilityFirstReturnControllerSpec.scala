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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.yeartodatelliability

import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.http.Status.BAD_REQUEST
import play.api.i18n.{Lang, Messages, MessagesApi, MessagesImpl}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.{Call, Result}
import play.api.test.FakeRequest
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.ReturnsServiceSupport
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AmountOfMoneyErrorScenarios, AuthSupport, ControllerSpec, SessionSupport, returns}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators.{sample, _}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.FillingOutReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.IndividualTriageAnswers.IncompleteIndividualTriageAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.YearToDateLiabilityAnswers.{CompleteYearToDateLiabilityAnswers, IncompleteYearToDateLiabilityAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{DisposalDate, DraftReturn, YearToDateLiabilityAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{AmountInPence, Error, SessionData, TaxYear}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService

import scala.concurrent.Future

class YearToDateLiabilityFirstReturnControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with ReturnsServiceSupport
    with ScalaCheckDrivenPropertyChecks
    with RedirectToStartBehaviour {

  override val overrideBindings =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore),
      bind[ReturnsService].toInstance(mockReturnsService)
    )

  lazy val controller = instanceOf[YearToDateLiabilityFirstReturnController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  implicit lazy val messages: Messages = MessagesImpl(Lang("en"), messagesApi)

  def redirectToStartBehaviour(performAction: () => Future[Result]) =
    redirectToStartWhenInvalidJourney(
      performAction, {
        case _: FillingOutReturn => true
        case _                   => false
      }
    )

  def sessionWithState(
    ytdLiabilityAnswers: Option[YearToDateLiabilityAnswers],
    disposalDate: Option[DisposalDate]
  ): (SessionData, FillingOutReturn) = {
    val journey = sample[FillingOutReturn].copy(
      draftReturn = sample[DraftReturn].copy(
        triageAnswers              = sample[IncompleteIndividualTriageAnswers].copy(disposalDate = disposalDate),
        yearToDateLiabilityAnswers = ytdLiabilityAnswers
      )
    )
    SessionData.empty.copy(journeyStatus = Some(journey)) -> journey
  }

  def sessionWithState(
    ytdLiabilityAnswers: YearToDateLiabilityAnswers,
    disposalDate: DisposalDate
  ): (SessionData, FillingOutReturn) =
    sessionWithState(Some(ytdLiabilityAnswers), Some(disposalDate))

  "ReliefDetailsController" when {

    "handling requests to display the estimated income page" must {

      def performAction(): Future[Result] = controller.estimatedIncome()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      behave like noDisposalDateBehaviour(performAction)

      "display the page" when {

        "the user has not answered the question before" in {

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionWithState(None, Some(sample[DisposalDate]))._1)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("estimatedIncome.title"), { doc =>
              doc.select("#back").attr("href") shouldBe returns.routes.TaskListController.taskList().url
              doc
                .select("#content > article > form")
                .attr("action") shouldBe routes.YearToDateLiabilityFirstReturnController
                .estimatedIncomeSubmit()
                .url
            }
          )

        }

        "the user has answered the question before but has " +
          "not completed the relief detail section" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                IncompleteYearToDateLiabilityAnswers.empty.copy(
                  estimatedIncome = Some(AmountInPence.fromPounds(12.34))
                ),
                sample[DisposalDate]
              )._1
            )
          }

          checkPageIsDisplayed(performAction(), messageFromMessageKey("estimatedIncome.title"), { doc =>
            doc.select("#estimatedIncome").attr("value") shouldBe "12.34"
          })
        }

        "the user has answered the question before but has " +
          "completed the relief detail section" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[CompleteYearToDateLiabilityAnswers].copy(estimatedIncome = AmountInPence.fromPounds(12.34)),
                sample[DisposalDate]
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("estimatedIncome.title"), { doc =>
              doc.select("#estimatedIncome").attr("value") shouldBe "12.34"
              doc.select("#back").attr("href") shouldBe routes.YearToDateLiabilityFirstReturnController
                .checkYourAnswers()
                .url
              doc
                .select("#content > article > form")
                .attr("action") shouldBe routes.YearToDateLiabilityFirstReturnController
                .estimatedIncomeSubmit()
                .url
            }
          )
        }

      }
    }

    "handling submitted answers to the estimated income page" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.estimatedIncomeSubmit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      behave like redirectToStartBehaviour(() => performAction())

      behave like noDisposalDateBehaviour(() => performAction())

      behave like unsuccessfulUpdateBehaviour(
        IncompleteYearToDateLiabilityAnswers.empty,
        IncompleteYearToDateLiabilityAnswers.empty.copy(
          estimatedIncome = Some(AmountInPence(0L))
        ),
        () => performAction("estimatedIncome" -> "0")
      )

      "show a form error" when {

        "the amount of money is invalid" in {
          AmountOfMoneyErrorScenarios.amountOfMoneyErrorScenarios("estimatedIncome").foreach { scenario =>
            testFormError(scenario.formData: _*)(scenario.expectedErrorMessageKey)("estimatedIncome.title")(
              performAction
            )
          }
        }
      }

      "redirect to the check your answers page" when {

        "the answers in this section had not been answered at all" in {
          testSuccessfulUpdatesAfterSubmit(
            performAction("estimatedIncome" -> "1"),
            None,
            IncompleteYearToDateLiabilityAnswers.empty.copy(estimatedIncome = Some(AmountInPence(100L))),
            sample[DisposalDate]
          )
        }

        "the user had started answering questions in this section but had not completed it" in {
          testSuccessfulUpdatesAfterSubmit(
            performAction("estimatedIncome" -> "1"),
            IncompleteYearToDateLiabilityAnswers.empty.copy(estimatedIncome = Some(AmountInPence(1L))),
            IncompleteYearToDateLiabilityAnswers.empty.copy(estimatedIncome = Some(AmountInPence(100L)))
          )
        }

        "the user had already completed the section" in {
          val oldAnswers =
            sample[CompleteYearToDateLiabilityAnswers].copy(estimatedIncome = AmountInPence(1L))
          testSuccessfulUpdatesAfterSubmit(
            performAction("estimatedIncome" -> "1"),
            oldAnswers,
            oldAnswers.copy(estimatedIncome = AmountInPence(100L))
          )
        }

      }

      "remove any personal allowance from the answers" when {

        "the user had entered a non-zero income and has now just submitted a zero income and " +
          "the journey was incomplete" in {
          val oldAnswers =
            sample[IncompleteYearToDateLiabilityAnswers].copy(
              estimatedIncome   = Some(AmountInPence(1L)),
              personalAllowance = Some(AmountInPence(1L))
            )

          testSuccessfulUpdatesAfterSubmit(
            performAction("estimatedIncome" -> "0"),
            oldAnswers,
            oldAnswers.copy(estimatedIncome = Some(AmountInPence(0L)), personalAllowance = None)
          )
        }

        "the user had entered a non-zero income and has now just submitted a zero income and " +
          "the journey was complete" in {
          val oldAnswers =
            sample[CompleteYearToDateLiabilityAnswers].copy(
              estimatedIncome   = AmountInPence(1L),
              personalAllowance = Some(AmountInPence(1L))
            )

          testSuccessfulUpdatesAfterSubmit(
            performAction("estimatedIncome" -> "0"),
            oldAnswers,
            oldAnswers.copy(estimatedIncome = AmountInPence(0L), personalAllowance = None)
          )
        }

        "the user had entered a zero income and has now just submitted a non-zero income and " +
          "the journey was incomplete" in {
          val oldAnswers =
            sample[IncompleteYearToDateLiabilityAnswers].copy(
              estimatedIncome   = Some(AmountInPence(0L)),
              personalAllowance = Some(AmountInPence(1L))
            )

          testSuccessfulUpdatesAfterSubmit(
            performAction("estimatedIncome" -> "1"),
            oldAnswers,
            oldAnswers.copy(estimatedIncome = Some(AmountInPence(100L)), personalAllowance = None)
          )
        }

        "the user had entered a zero income and has now just submitted a non-zero income and " +
          "the journey was complete" in {
          val oldAnswers =
            sample[CompleteYearToDateLiabilityAnswers].copy(
              estimatedIncome   = AmountInPence(0L),
              personalAllowance = Some(AmountInPence(1L))
            )

          val newAnswers = IncompleteYearToDateLiabilityAnswers(
            Some(AmountInPence(100L)),
            None,
            Some(oldAnswers.hasEstimatedDetails)
          )

          testSuccessfulUpdatesAfterSubmit(
            performAction("estimatedIncome" -> "1"),
            oldAnswers,
            newAnswers
          )
        }

      }

    }

    "handling requests to display the personal allowance page" must {

      def performAction(): Future[Result] = controller.personalAllowance()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      behave like noDisposalDateBehaviour(performAction)

      behave like noEstimatedIncomeBehaviour(performAction)

      "redirect to the check you answers page" when {

        "the estimated income is zero" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[CompleteYearToDateLiabilityAnswers].copy(
                  estimatedIncome = AmountInPence(0L)
                ),
                sample[DisposalDate]
              )._1
            )
          }

          checkIsRedirect(performAction(), routes.YearToDateLiabilityFirstReturnController.checkYourAnswers())
        }

      }

      "display the page" when {

        "the estimated income is greater than zero and" when {

          "the section is incomplete" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionWithState(
                  sample[IncompleteYearToDateLiabilityAnswers].copy(
                    estimatedIncome   = Some(AmountInPence.fromPounds(12.34)),
                    personalAllowance = None
                  ),
                  sample[DisposalDate]
                )._1
              )
            }

            checkPageIsDisplayed(
              performAction(),
              messageFromMessageKey("personalAllowance.title"), { doc =>
                doc.select("#back").attr("href") shouldBe routes.YearToDateLiabilityFirstReturnController
                  .estimatedIncome()
                  .url
                doc
                  .select("#content > article > form")
                  .attr("action") shouldBe routes.YearToDateLiabilityFirstReturnController
                  .personalAllowanceSubmit()
                  .url
              }
            )
          }

          "the section is complete" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionWithState(
                  sample[CompleteYearToDateLiabilityAnswers].copy(
                    estimatedIncome   = AmountInPence(1L),
                    personalAllowance = Some(AmountInPence(1234L))
                  ),
                  sample[DisposalDate]
                )._1
              )
            }

            checkPageIsDisplayed(
              performAction(),
              messageFromMessageKey("personalAllowance.title"), { doc =>
                doc.select("#personalAllowance").attr("value") shouldBe "12.34"
                doc.select("#back").attr("href") shouldBe routes.YearToDateLiabilityFirstReturnController
                  .checkYourAnswers()
                  .url
                doc
                  .select("#content > article > form")
                  .attr("action") shouldBe routes.YearToDateLiabilityFirstReturnController
                  .personalAllowanceSubmit()
                  .url
              }
            )
          }

        }

      }

    }

    "handling submitted answers to the personal allowance page" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.personalAllowanceSubmit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      behave like redirectToStartBehaviour(() => performAction())

      behave like noDisposalDateBehaviour(() => performAction())

      behave like noEstimatedIncomeBehaviour(() => performAction())

      {
        val completeAnswers = sample[CompleteYearToDateLiabilityAnswers].copy(
          estimatedIncome   = AmountInPence(1L),
          personalAllowance = Some(AmountInPence(2L))
        )
        behave like unsuccessfulUpdateBehaviour(
          completeAnswers,
          completeAnswers.copy(personalAllowance = Some(AmountInPence(0L))),
          () => performAction("personalAllowance" -> "0")
        )
      }

      "redirect to the check you answers page" when {

        "the estimated income is zero" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[CompleteYearToDateLiabilityAnswers].copy(
                  estimatedIncome = AmountInPence(0L)
                ),
                sample[DisposalDate]
              )._1
            )
          }

          checkIsRedirect(performAction(), routes.YearToDateLiabilityFirstReturnController.checkYourAnswers())
        }

      }

      "show a form error" when {

        "the amount of money is invalid" in {
          val personalAllowance = AmountInPence(100000L)
          val taxYear           = sample[TaxYear].copy(personalAllowance = personalAllowance)
          val disposalDate      = sample[DisposalDate].copy(taxYear = taxYear)
          val session = sessionWithState(
            IncompleteYearToDateLiabilityAnswers.empty.copy(
              estimatedIncome = Some(AmountInPence(1L))
            ),
            disposalDate
          )._1

          AmountOfMoneyErrorScenarios
            .amountOfMoneyErrorScenarios("personalAllowance", personalAllowance.inPounds())
            .foreach { scenario =>
              withClue(s"For $scenario: ") {
                testFormError(scenario.formData: _*)(scenario.expectedErrorMessageKey)("personalAllowance.title")(
                  performAction,
                  session
                )
              }
            }
        }
      }

      "redirect to the check your answers page" when {

        val disposalDate = sample[DisposalDate].copy(
          taxYear = sample[TaxYear].copy(personalAllowance = AmountInPence(1000L))
        )

        "the user had started answering questions in this section but had not completed it" in {
          val oldAnswers =
            IncompleteYearToDateLiabilityAnswers.empty.copy(
              estimatedIncome   = Some(AmountInPence(1L)),
              personalAllowance = None
            )

          val newAnswers = oldAnswers.copy(personalAllowance = Some(AmountInPence(100L)))

          testSuccessfulUpdatesAfterSubmit(
            performAction("personalAllowance" -> "1"),
            oldAnswers,
            newAnswers,
            disposalDate
          )
        }

        "the user had already completed the section" in {
          val oldAnswers =
            sample[CompleteYearToDateLiabilityAnswers].copy(
              estimatedIncome   = AmountInPence(1L),
              personalAllowance = Some(AmountInPence(2L))
            )

          val newAnswers = oldAnswers.copy(personalAllowance = Some(AmountInPence(100L)))

          testSuccessfulUpdatesAfterSubmit(
            performAction("personalAllowance" -> "1"),
            oldAnswers,
            newAnswers,
            disposalDate
          )
        }

      }

    }

    "handling requests to display the has estimated details page" must {

      def performAction(): Future[Result] = controller.hasEstimatedDetails()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      behave like noEstimatedIncomeBehaviour(performAction)

      "redirect to the personal allowance page" when {

        "the estimated income is more than zero and the user has not answered " +
          "the personal allowance question yet" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                IncompleteYearToDateLiabilityAnswers.empty.copy(
                  estimatedIncome = Some(AmountInPence(1L))
                ),
                sample[DisposalDate]
              )._1
            )
          }

          checkIsRedirect(performAction(), routes.YearToDateLiabilityFirstReturnController.personalAllowance())
        }

      }

      "display the page" when {

        def test(
          answers: YearToDateLiabilityAnswers,
          backLink: Call
        ): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                answers,
                sample[DisposalDate]
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("hasEstimatedDetails.title"), { doc =>
              doc.select("#back").attr("href") shouldBe backLink.url
              doc
                .select("#content > article > form")
                .attr("action") shouldBe routes.YearToDateLiabilityFirstReturnController
                .hasEstimatedDetailsSubmit()
                .url
            }
          )
        }

        "the estimated income is greater than zero and" when {

          "the section is incomplete and the estimated income is zero" in {
            test(
              sample[IncompleteYearToDateLiabilityAnswers].copy(
                estimatedIncome = Some(AmountInPence(0L))
              ),
              routes.YearToDateLiabilityFirstReturnController.estimatedIncome()
            )
          }

          "the section is incomplete and the estimated income is non-zero" in {
            test(
              sample[IncompleteYearToDateLiabilityAnswers].copy(
                estimatedIncome   = Some(AmountInPence(100L)),
                personalAllowance = Some(AmountInPence(1L))
              ),
              routes.YearToDateLiabilityFirstReturnController.personalAllowance()
            )
          }

          "the section is complete and the estimated income is zero" in {
            test(
              sample[CompleteYearToDateLiabilityAnswers].copy(
                estimatedIncome   = AmountInPence(0L),
                personalAllowance = None
              ),
              routes.YearToDateLiabilityFirstReturnController.checkYourAnswers()
            )
          }

          "the section is complete and the estimated income is non-zero" in {
            test(
              sample[CompleteYearToDateLiabilityAnswers].copy(
                estimatedIncome   = AmountInPence(100L),
                personalAllowance = Some(AmountInPence(1L))
              ),
              routes.YearToDateLiabilityFirstReturnController.checkYourAnswers()
            )
          }

        }

      }

    }

    "handling submitted answers to the has estimated details page" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.hasEstimatedDetailsSubmit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      behave like redirectToStartBehaviour(() => performAction())

      behave like noEstimatedIncomeBehaviour(() => performAction())

      {
        val answers = IncompleteYearToDateLiabilityAnswers(
          estimatedIncome     = Some(AmountInPence(1L)),
          personalAllowance   = Some(AmountInPence(0L)),
          hasEstimatedDetails = None
        )
        behave like unsuccessfulUpdateBehaviour(
          answers,
          answers.copy(hasEstimatedDetails = Some(true)),
          () => performAction("hasEstimatedDetails" -> "true")
        )
      }

      "redirect to the personal allowance page" when {

        "the estimated income is more than zero and the user has not answered " +
          "the personal allowance question yet" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                IncompleteYearToDateLiabilityAnswers.empty.copy(
                  estimatedIncome = Some(AmountInPence(1L))
                ),
                sample[DisposalDate]
              )._1
            )
          }

          checkIsRedirect(performAction(), routes.YearToDateLiabilityFirstReturnController.personalAllowance())
        }

      }

      "show a form error" when {

        val currentSession = sessionWithState(
          IncompleteYearToDateLiabilityAnswers(
            estimatedIncome     = Some(AmountInPence(1L)),
            personalAllowance   = Some(AmountInPence(0L)),
            hasEstimatedDetails = None
          ),
          sample[DisposalDate]
        )._1

        def test(data: (String, String)*)(expectedErrorMessageKey: String) =
          testFormError(data: _*)(expectedErrorMessageKey)("hasEstimatedDetails.title")(performAction, currentSession)

        "nothing is submitted" in {
          test()("hasEstimatedDetails.error.required")
        }

        "the data submitted is invalid" in {
          test("hasEstimatedDetails" -> "abc")("hasEstimatedDetails.error.boolean")
        }

      }

      "redirect to the check your answers page" when {

        "all updates are successful and" when {

          "the journey was incomplete" in {
            val answers =
              IncompleteYearToDateLiabilityAnswers(
                estimatedIncome     = Some(AmountInPence(1L)),
                personalAllowance   = Some(AmountInPence(0L)),
                hasEstimatedDetails = None
              )
            testSuccessfulUpdatesAfterSubmit(
              performAction("hasEstimatedDetails" -> "true"),
              answers,
              answers.copy(hasEstimatedDetails = Some(true))
            )
          }

          "the journey was complete" in {
            val answers =
              CompleteYearToDateLiabilityAnswers(
                estimatedIncome     = AmountInPence(0L),
                personalAllowance   = None,
                hasEstimatedDetails = true
              )
            testSuccessfulUpdatesAfterSubmit(
              performAction("hasEstimatedDetails" -> "false"),
              answers,
              answers.copy(hasEstimatedDetails = false)
            )
          }

        }

      }

    }

  }

  def noDisposalDateBehaviour(performAction: () => Future[Result]): Unit =
    "redirect to the task list page" when {
      "no disposal date can be found" in {

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionWithState(Some(sample[CompleteYearToDateLiabilityAnswers]), None)._1)
        }

        checkIsRedirect(performAction(), returns.routes.TaskListController.taskList())
      }
    }

  def noEstimatedIncomeBehaviour(performAction: () => Future[Result]): Unit =
    "redirect to the check your answers page" when {
      "no estimated income can be found" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            sessionWithState(
              sample[IncompleteYearToDateLiabilityAnswers].copy(
                estimatedIncome     = None,
                personalAllowance   = Some(sample[AmountInPence]),
                hasEstimatedDetails = Some(false)
              ),
              sample[DisposalDate]
            )._1
          )
        }

        checkIsRedirect(performAction(), routes.YearToDateLiabilityFirstReturnController.checkYourAnswers())
      }

    }

  def unsuccessfulUpdateBehaviour(
    currentAnswers: YearToDateLiabilityAnswers,
    updatedAnswers: YearToDateLiabilityAnswers,
    result: () => Future[Result]
  ): Unit = {
    val (session, journey) = sessionWithState(currentAnswers, sample[DisposalDate])
    val updatedDraftReturn = journey.draftReturn.copy(yearToDateLiabilityAnswers = Some(updatedAnswers))

    val updatedSession = SessionData.empty.copy(journeyStatus = Some(
      journey.copy(draftReturn = updatedDraftReturn)
    )
    )

    "show an error page" when {

      "there is an error updating the draft return" in {

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockStoreDraftReturn(updatedDraftReturn)(Left(Error("")))
        }

        checkIsTechnicalErrorPage(result())
      }

      "there is an error updating the session data" in {

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockStoreDraftReturn(updatedDraftReturn)(Right(()))
          mockStoreSession(updatedSession)(Left(Error("")))
        }

        checkIsTechnicalErrorPage(result())
      }

    }

  }

  def testFormError(
    data: (String, String)*
  )(expectedErrorMessageKey: String, errorArgs: String*)(pageTitleKey: String, titleArgs: String*)(
    performAction: Seq[(String, String)] => Future[Result],
    currentSession: SessionData = sessionWithState(
      sample[CompleteYearToDateLiabilityAnswers],
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

  def testSuccessfulUpdatesAfterSubmit(
    result: => Future[Result],
    oldAnswers: Option[YearToDateLiabilityAnswers],
    newAnswers: YearToDateLiabilityAnswers,
    disposalDate: DisposalDate
  ): Unit = {
    val (session, journey) = sessionWithState(oldAnswers, Some(disposalDate))
    val updatedDraftReturn =
      journey.draftReturn.copy(yearToDateLiabilityAnswers = Some(newAnswers))

    inSequence {
      mockAuthWithNoRetrievals()
      mockGetSession(session)
      mockStoreDraftReturn(updatedDraftReturn)(Right(()))
      mockStoreSession(
        session.copy(
          journeyStatus = Some(journey.copy(draftReturn = updatedDraftReturn))
        )
      )(Right(()))
    }

    checkIsRedirect(result, routes.YearToDateLiabilityFirstReturnController.checkYourAnswers())
  }

  def testSuccessfulUpdatesAfterSubmit(
    result: => Future[Result],
    oldAnswers: YearToDateLiabilityAnswers,
    newAnswers: YearToDateLiabilityAnswers,
    disposalDate: DisposalDate = sample[DisposalDate]
  ): Unit = testSuccessfulUpdatesAfterSubmit(result, Some(oldAnswers), newAnswers, disposalDate)

}
