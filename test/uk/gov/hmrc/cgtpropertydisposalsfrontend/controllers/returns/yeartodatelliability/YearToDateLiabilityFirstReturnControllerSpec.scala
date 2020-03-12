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

import java.util.UUID

import cats.data.EitherT
import cats.instances.future._
import org.jsoup.nodes.Document
import org.scalatest.Matchers
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
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.yeartodatelliability.YearToDateLiabilityFirstReturnControllerSpec.validateYearToDateLiabilityFirstReturnPage
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AmountOfMoneyErrorScenarios, AuthSupport, ControllerSpec, SessionSupport, returns}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators.{sample, _}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.FillingOutReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address.UkAddress
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.MoneyUtils.formatAmountOfMoneyWithPoundSign
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.{AmountInPence, MoneyUtils}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.CgtReference
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.AcquisitionDetailsAnswers.{CompleteAcquisitionDetailsAnswers, IncompleteAcquisitionDetailsAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.CalculatedTaxDue.GainCalculatedTaxDue
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.DisposalDetailsAnswers.{CompleteDisposalDetailsAnswers, IncompleteDisposalDetailsAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ExemptionAndLossesAnswers.{CompleteExemptionAndLossesAnswers, IncompleteExemptionAndLossesAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ReliefDetailsAnswers.{CompleteReliefDetailsAnswers, IncompleteReliefDetailsAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.{CompleteSingleDisposalTriageAnswers, IncompleteSingleDisposalTriageAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.YearToDateLiabilityAnswers.{CompleteYearToDateLiabilityAnswers, IncompleteYearToDateLiabilityAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, LocalDateUtils, SessionData, TaxYear}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.{CgtCalculationService, ReturnsService}
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class YearToDateLiabilityFirstReturnControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with ReturnsServiceSupport
    with ScalaCheckDrivenPropertyChecks
    with RedirectToStartBehaviour {

  val mockCgtCalculationService = mock[CgtCalculationService]

  override val overrideBindings =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore),
      bind[ReturnsService].toInstance(mockReturnsService),
      bind[CgtCalculationService].toInstance(mockCgtCalculationService)
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
        triageAnswers              = sample[IncompleteSingleDisposalTriageAnswers].copy(disposalDate = disposalDate),
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

  def draftReturnWithCompleteJourneys(
    yearToDateLiabilityAnswers: Option[YearToDateLiabilityAnswers],
    disposalDate: DisposalDate
  ) =
    DraftReturn(
      UUID.randomUUID(),
      sample[CgtReference],
      sample[CompleteSingleDisposalTriageAnswers].copy(disposalDate = disposalDate),
      Some(sample[UkAddress]),
      Some(sample[CompleteDisposalDetailsAnswers]),
      Some(sample[CompleteAcquisitionDetailsAnswers]),
      Some(sample[ReliefDetailsAnswers]),
      Some(sample[CompleteExemptionAndLossesAnswers]),
      yearToDateLiabilityAnswers,
      Some(sample[AmountInPence]),
      LocalDateUtils.today()
    )

  def mockCalculationService(
    request: CalculateCgtTaxDueRequest
  )(result: Either[Error, CalculatedTaxDue]) =
    (mockCgtCalculationService
      .calculateTaxDue(_: CalculateCgtTaxDueRequest)(_: HeaderCarrier))
      .expects(request, *)
      .returning(EitherT.fromEither[Future](result))

  def setTaxDue(calculatedTaxDue: CalculatedTaxDue, taxDue: AmountInPence): CalculatedTaxDue =
    calculatedTaxDue match {
      case nonGain: CalculatedTaxDue.NonGainCalculatedTaxDue => nonGain.copy(amountOfTaxDue = taxDue)
      case gain: GainCalculatedTaxDue                        => gain.copy(amountOfTaxDue    = taxDue)
    }

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
          estimatedIncome = Some(AmountInPence.zero)
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
          val answers = sample[IncompleteYearToDateLiabilityAnswers]
          testSuccessfulUpdatesAfterSubmit(
            performAction("estimatedIncome" -> "1"),
            answers.copy(estimatedIncome = Some(AmountInPence(1L))),
            answers.copy(estimatedIncome = Some(AmountInPence(100L)))
          )
        }

        "the user had already completed the section" in {
          val oldAnswers =
            sample[CompleteYearToDateLiabilityAnswers].copy(estimatedIncome = AmountInPence(1L))
          val newAnswers = IncompleteYearToDateLiabilityAnswers.empty.copy(estimatedIncome = Some(AmountInPence(100L)))
          testSuccessfulUpdatesAfterSubmit(
            performAction("estimatedIncome" -> "1"),
            oldAnswers,
            newAnswers
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
            oldAnswers.copy(estimatedIncome = Some(AmountInPence.zero), personalAllowance = None)
          )
        }

        "the user had entered a zero income and has now just submitted a non-zero income and " +
          "the journey was incomplete" in {
          val oldAnswers =
            sample[IncompleteYearToDateLiabilityAnswers].copy(
              estimatedIncome   = Some(AmountInPence.zero),
              personalAllowance = Some(AmountInPence(1L))
            )

          testSuccessfulUpdatesAfterSubmit(
            performAction("estimatedIncome" -> "1"),
            oldAnswers,
            oldAnswers.copy(estimatedIncome = Some(AmountInPence(100L)), personalAllowance = None)
          )
        }
      }

      "not do any updates if the submitted answer is the same as one already stored in session and" when {

        "the section is incomplete" in {
          val answers = sample[IncompleteYearToDateLiabilityAnswers].copy(
            estimatedIncome   = Some(AmountInPence(1L)),
            personalAllowance = Some(AmountInPence(2L))
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionWithState(answers, sample[DisposalDate])._1)
          }

          checkIsRedirect(
            performAction("estimatedIncome" -> "0.01"),
            routes.YearToDateLiabilityFirstReturnController.checkYourAnswers()
          )
        }

        "the section is complete" in {
          val answers = sample[CompleteYearToDateLiabilityAnswers].copy(
            estimatedIncome   = AmountInPence(1L),
            personalAllowance = Some(AmountInPence(2L))
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionWithState(answers, sample[DisposalDate])._1)
          }

          checkIsRedirect(
            performAction("estimatedIncome" -> "0.01"),
            routes.YearToDateLiabilityFirstReturnController.checkYourAnswers()
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
                  estimatedIncome = AmountInPence.zero
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
        val newAnswers = IncompleteYearToDateLiabilityAnswers.empty.copy(
          estimatedIncome   = Some(completeAnswers.estimatedIncome),
          personalAllowance = Some(AmountInPence.zero)
        )
        behave like unsuccessfulUpdateBehaviour(
          completeAnswers,
          newAnswers,
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
                  estimatedIncome = AmountInPence.zero
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
                testFormError(scenario.formData: _*)(
                  scenario.expectedErrorMessageKey,
                  MoneyUtils.formatAmountOfMoneyWithoutPoundSign(taxYear.personalAllowance.inPounds())
                )("personalAllowance.title")(
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
              estimatedIncome     = Some(AmountInPence(1L)),
              personalAllowance   = None,
              hasEstimatedDetails = Some(sample[Boolean])
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

          val newAnswers = IncompleteYearToDateLiabilityAnswers.empty.copy(
            estimatedIncome   = Some(oldAnswers.estimatedIncome),
            personalAllowance = Some(AmountInPence(100L))
          )

          testSuccessfulUpdatesAfterSubmit(
            performAction("personalAllowance" -> "1"),
            oldAnswers,
            newAnswers,
            disposalDate
          )
        }

      }

      "not do any updates if the submitted answer is the same as one already stored in session and" when {

        val disposalDate = sample[DisposalDate].copy(
          taxYear = sample[TaxYear].copy(personalAllowance = AmountInPence(1000L))
        )

        "the section is incomplete" in {
          val answers = sample[IncompleteYearToDateLiabilityAnswers].copy(
            estimatedIncome   = Some(AmountInPence(1L)),
            personalAllowance = Some(AmountInPence(2L))
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionWithState(answers, disposalDate)._1)
          }

          checkIsRedirect(
            performAction("personalAllowance" -> "0.02"),
            routes.YearToDateLiabilityFirstReturnController.checkYourAnswers()
          )
        }

        "the section is complete" in {
          val answers = sample[CompleteYearToDateLiabilityAnswers].copy(
            estimatedIncome   = AmountInPence(1L),
            personalAllowance = Some(AmountInPence(2L))
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionWithState(answers, disposalDate)._1)
          }

          checkIsRedirect(
            performAction("personalAllowance" -> "0.02"),
            routes.YearToDateLiabilityFirstReturnController.checkYourAnswers()
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
                estimatedIncome = Some(AmountInPence.zero)
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
                estimatedIncome   = AmountInPence.zero,
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
        val answers = IncompleteYearToDateLiabilityAnswers.empty.copy(
          estimatedIncome   = Some(AmountInPence(1L)),
          personalAllowance = Some(AmountInPence(2L))
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
        val answers = IncompleteYearToDateLiabilityAnswers.empty.copy(
          estimatedIncome   = Some(AmountInPence(1L)),
          personalAllowance = Some(AmountInPence(2L))
        )

        val currentSession = sessionWithState(answers, sample[DisposalDate])._1

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
            val answers = IncompleteYearToDateLiabilityAnswers.empty.copy(
              estimatedIncome   = Some(AmountInPence(1L)),
              personalAllowance = Some(AmountInPence(2L))
            )

            val updatedAnswers = answers.copy(hasEstimatedDetails = Some(false))

            testSuccessfulUpdatesAfterSubmit(
              performAction("hasEstimatedDetails" -> "false"),
              answers,
              updatedAnswers
            )
          }

          "the journey was complete" in {
            val answers = CompleteYearToDateLiabilityAnswers(
              estimatedIncome     = AmountInPence.zero,
              personalAllowance   = None,
              hasEstimatedDetails = false,
              calculatedTaxDue    = sample[CalculatedTaxDue],
              taxDue              = sample[AmountInPence],
              Some(sample[String])
            )

            val updatedAnswers =
              IncompleteYearToDateLiabilityAnswers(
                estimatedIncome     = Some(AmountInPence.zero),
                personalAllowance   = None,
                hasEstimatedDetails = Some(true),
                None,
                None,
                None
              )

            val draftReturn        = sample[DraftReturn].copy(yearToDateLiabilityAnswers = Some(answers))
            val updatedDraftReturn = draftReturn.copy(yearToDateLiabilityAnswers         = Some(updatedAnswers))

            testSuccessfulUpdatesAfterSubmit(
              performAction("hasEstimatedDetails" -> "true"),
              draftReturn,
              updatedDraftReturn
            )
          }
        }
      }

      "not do any updates if the submitted answer is the same as one already stored in session and" when {

        "the section is incomplete" in {

          val session = sessionWithState(
            IncompleteYearToDateLiabilityAnswers.empty.copy(
              estimatedIncome     = Some(AmountInPence(1L)),
              personalAllowance   = Some(AmountInPence(2L)),
              hasEstimatedDetails = Some(true)
            ),
            sample[DisposalDate]
          )._1

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkIsRedirect(
            performAction("hasEstimatedDetails" -> "true"),
            routes.YearToDateLiabilityFirstReturnController.checkYourAnswers()
          )
        }

        "the section is complete" in {
          val session = sessionWithState(
            CompleteYearToDateLiabilityAnswers(
              AmountInPence(1L),
              Some(AmountInPence(2L)),
              hasEstimatedDetails = false,
              sample[CalculatedTaxDue],
              sample[AmountInPence],
              Some(sample[String])
            ),
            sample[DisposalDate]
          )._1

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkIsRedirect(
            performAction("hasEstimatedDetails" -> "false"),
            routes.YearToDateLiabilityFirstReturnController.checkYourAnswers()
          )
        }

      }

    }

    "handling requests to display the tax due page" must {

      def performAction(): Future[Result] = controller.taxDue()(FakeRequest())

      val disposalDate              = sample[DisposalDate]
      val triageAnswers             = sample[CompleteSingleDisposalTriageAnswers].copy(disposalDate = disposalDate)
      val disposalDetailsAnswers    = sample[CompleteDisposalDetailsAnswers]
      val acquisitionDetailsAnswers = sample[CompleteAcquisitionDetailsAnswers]
      val reliefDetailsAnswers      = sample[CompleteReliefDetailsAnswers]
      val exemptionAndLossesAnswers = sample[CompleteExemptionAndLossesAnswers]

      def draftReturnWithAnswers(yearToDateLiabilityAnswers: YearToDateLiabilityAnswers): DraftReturn =
        sample[DraftReturn].copy(
          triageAnswers              = triageAnswers,
          disposalDetailsAnswers     = Some(disposalDetailsAnswers),
          acquisitionDetailsAnswers  = Some(acquisitionDetailsAnswers),
          reliefDetailsAnswers       = Some(reliefDetailsAnswers),
          exemptionAndLossesAnswers  = Some(exemptionAndLossesAnswers),
          yearToDateLiabilityAnswers = Some(yearToDateLiabilityAnswers)
        )

      def calculateRequest(estimatedIncome: AmountInPence, personalAllowance: AmountInPence) =
        CalculateCgtTaxDueRequest(
          triageAnswers,
          disposalDetailsAnswers,
          acquisitionDetailsAnswers,
          reliefDetailsAnswers,
          exemptionAndLossesAnswers,
          estimatedIncome,
          personalAllowance
        )

      behave like redirectToStartBehaviour(performAction)

      behave like noEstimatedIncomeBehaviour(performAction)

      behave like incompleteOtherJourneysBehaviour(performAction)

      "redirect to the check your answers page" when {

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

          checkIsRedirect(performAction(), routes.YearToDateLiabilityFirstReturnController.checkYourAnswers())
        }

        "the user hasn't verified whether or not any details given in the return were estimates" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  sample[FillingOutReturn].copy(
                    draftReturn = draftReturnWithCompleteJourneys(
                      Some(
                        IncompleteYearToDateLiabilityAnswers.empty.copy(
                          estimatedIncome   = Some(AmountInPence(1L)),
                          personalAllowance = Some(AmountInPence(2L))
                        )
                      ),
                      sample[DisposalDate]
                    )
                  )
                )
              )
            )
          }

          checkIsRedirect(performAction(), routes.YearToDateLiabilityFirstReturnController.checkYourAnswers())
        }

      }

      "show an error page" when {

        "there is an error getting the calculated tax due" in {
          val answers = IncompleteYearToDateLiabilityAnswers.empty.copy(
            estimatedIncome     = Some(AmountInPence(1L)),
            personalAllowance   = Some(AmountInPence(2L)),
            hasEstimatedDetails = Some(false)
          )

          val calculateCgtTaxDueRequest = calculateRequest(AmountInPence(1L), AmountInPence(2L))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  sample[FillingOutReturn].copy(draftReturn = draftReturnWithAnswers(answers))
                )
              )
            )
            mockCalculationService(calculateCgtTaxDueRequest)(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction())
        }

        "there is an error storing the calculated tax due" in {
          val answers = IncompleteYearToDateLiabilityAnswers.empty.copy(
            estimatedIncome     = Some(AmountInPence(1L)),
            personalAllowance   = Some(AmountInPence(2L)),
            hasEstimatedDetails = Some(false)
          )

          val calculateCgtTaxDueRequest = calculateRequest(AmountInPence(1L), AmountInPence(2L))
          val calculatedTaxDue          = sample[CalculatedTaxDue]
          val fillingOutReturn          = sample[FillingOutReturn].copy(draftReturn = draftReturnWithAnswers(answers))
          val updatedFillingOutReturn = fillingOutReturn.copy(draftReturn = fillingOutReturn.draftReturn.copy(
            yearToDateLiabilityAnswers = Some(answers.copy(calculatedTaxDue = Some(calculatedTaxDue)))
          )
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData.empty.copy(journeyStatus = Some(fillingOutReturn)))
            mockCalculationService(calculateCgtTaxDueRequest)(Right(calculatedTaxDue))
            mockStoreSession(SessionData.empty.copy(journeyStatus = Some(updatedFillingOutReturn)))(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction())
        }

      }

      "display the page" when {

        def test(
          answers: YearToDateLiabilityAnswers,
          mockCalculateTaxDue: FillingOutReturn => Unit,
          backLink: Call
        ): Unit = {
          val draftReturn = draftReturnWithAnswers(answers)
          val fillingOutReturn = sample[FillingOutReturn].copy(
            draftReturn = draftReturn
          )

          val session = SessionData.empty.copy(journeyStatus = Some(fillingOutReturn))
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockCalculateTaxDue(fillingOutReturn)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("taxDue.title"), { doc =>
              doc.select("#back").attr("href") shouldBe backLink.url
              doc
                .select("#content > article > form")
                .attr("action") shouldBe routes.YearToDateLiabilityFirstReturnController
                .taxDueSubmit()
                .url
            }
          )
        }

        "the section is incomplete and a calculation hasn't already been done" in {
          val answers = IncompleteYearToDateLiabilityAnswers(
            Some(AmountInPence.zero),
            None,
            Some(true),
            None,
            None,
            None
          )

          val calculatedTaxDue = sample[CalculatedTaxDue]

          test(
            answers, { fillingOutReturn =>
              mockCalculationService(calculateRequest(AmountInPence.zero, AmountInPence.zero))(
                Right(calculatedTaxDue)
              )
              mockStoreSession(
                SessionData.empty.copy(
                  journeyStatus = Some(
                    fillingOutReturn.copy(draftReturn = fillingOutReturn.draftReturn.copy(
                      yearToDateLiabilityAnswers = Some(
                        answers.copy(calculatedTaxDue = Some(calculatedTaxDue))
                      )
                    )
                    )
                  )
                )
              )(Right(()))
            },
            routes.YearToDateLiabilityFirstReturnController.hasEstimatedDetails()
          )
        }

        "the section is incomplete and a calculation has already been done" in {
          val answers = IncompleteYearToDateLiabilityAnswers(
            Some(AmountInPence.zero),
            None,
            Some(true),
            Some(sample[CalculatedTaxDue]),
            None,
            None
          )

          test(
            answers,
            _ => (),
            routes.YearToDateLiabilityFirstReturnController.hasEstimatedDetails()
          )
        }

        "the section is complete" in {
          val answers = sample[CompleteYearToDateLiabilityAnswers]
          test(
            answers,
            _ => (),
            routes.YearToDateLiabilityFirstReturnController.checkYourAnswers()
          )
        }

      }

    }

    "handling submitted answers to the tax due page" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.taxDueSubmit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      behave like redirectToStartBehaviour(() => performAction())

      behave like noEstimatedIncomeBehaviour(() => performAction())

      behave like incompleteOtherJourneysBehaviour(() => performAction())

      {
        val oldAnswers = sample[IncompleteYearToDateLiabilityAnswers].copy(
          estimatedIncome   = Some(AmountInPence(0L)),
          calculatedTaxDue  = Some(sample[CalculatedTaxDue]),
          personalAllowance = None,
          mandatoryEvidence = None
        )
        val draftReturn = draftReturnWithCompleteJourneys(Some(oldAnswers), sample[DisposalDate])
        val newDraftReturn = draftReturn.copy(
          yearToDateLiabilityAnswers = Some(oldAnswers.copy(taxDue = Some(AmountInPence(123L))))
        )

        behave like unsuccessfulUpdateBehaviour(
          draftReturn,
          newDraftReturn,
          () => performAction("taxDue" -> "£1.23")
        )
      }

      "redirect to the check you answers page" when {

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

          checkIsRedirect(performAction(), routes.YearToDateLiabilityFirstReturnController.checkYourAnswers())
        }

        "the user hasn't verified whether or not any details given in the return were estimates" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                IncompleteYearToDateLiabilityAnswers.empty.copy(
                  estimatedIncome   = Some(AmountInPence(1L)),
                  personalAllowance = Some(AmountInPence.zero)
                ),
                sample[DisposalDate]
              )._1
            )
          }

          checkIsRedirect(performAction(), routes.YearToDateLiabilityFirstReturnController.checkYourAnswers())
        }

        "there is no calculated tax due in session" in {
          val answers = sample[IncompleteYearToDateLiabilityAnswers].copy(
            estimatedIncome     = Some(AmountInPence(0L)),
            hasEstimatedDetails = Some(true),
            calculatedTaxDue    = None,
            personalAllowance   = None,
            mandatoryEvidence   = None
          )
          val draftReturn = draftReturnWithCompleteJourneys(Some(answers), sample[DisposalDate])

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(sample[FillingOutReturn].copy(draftReturn = draftReturn))
              )
            )
          }

          checkIsRedirect(performAction(), routes.YearToDateLiabilityFirstReturnController.checkYourAnswers())
        }

      }

      "show a form error" when {
        val draftReturn =
          draftReturnWithCompleteJourneys(Some(sample[CompleteYearToDateLiabilityAnswers]), sample[DisposalDate])

        val currentSession = SessionData.empty.copy(
          journeyStatus = Some(
            sample[FillingOutReturn].copy(
              draftReturn = draftReturn
            )
          )
        )

        "the data submitted is invalid" in {
          AmountOfMoneyErrorScenarios.amountOfMoneyErrorScenarios("taxDue").foreach { scenario =>
            withClue(s"For $scenario: ") {
              testFormError(scenario.formData: _*)(scenario.expectedErrorMessageKey)("taxDue.title")(
                performAction,
                currentSession
              )
            }

          }
        }

      }

      "redirect to the check your answers page" when {

        "all updates are successful and" when {

          "the journey was incomplete" in {
            val disposalDate = sample[DisposalDate]

            val answers = IncompleteYearToDateLiabilityAnswers(
              Some(AmountInPence(1L)),
              Some(AmountInPence(2L)),
              Some(true),
              Some(sample[CalculatedTaxDue]),
              Some(AmountInPence(123L)),
              Some(sample[String])
            )
            val draftReturn = draftReturnWithCompleteJourneys(Some(answers), disposalDate)

            val updatedDraftReturn = draftReturn.copy(
              yearToDateLiabilityAnswers = Some(
                answers.copy(taxDue = Some(AmountInPence(101L)), mandatoryEvidence = None)
              )
            )

            testSuccessfulUpdatesAfterSubmit(
              performAction("taxDue" -> "1.01"),
              draftReturn,
              updatedDraftReturn
            )
          }

          "the journey was complete" in {
            val disposalDate = sample[DisposalDate]

            val answers = CompleteYearToDateLiabilityAnswers(
              AmountInPence(1L),
              Some(AmountInPence(2L)),
              false,
              setTaxDue(sample[CalculatedTaxDue], AmountInPence(100L)),
              AmountInPence(1L),
              Some(sample[String])
            )
            val draftReturn = draftReturnWithCompleteJourneys(Some(answers), disposalDate)

            val updatedDraftReturn = draftReturn.copy(
              yearToDateLiabilityAnswers = Some(
                IncompleteYearToDateLiabilityAnswers(
                  Some(answers.estimatedIncome),
                  answers.personalAllowance,
                  Some(answers.hasEstimatedDetails),
                  Some(answers.calculatedTaxDue),
                  Some(AmountInPence(123456L)),
                  None
                )
              )
            )

            testSuccessfulUpdatesAfterSubmit(
              performAction("taxDue" -> "£1,234.56"),
              draftReturn,
              updatedDraftReturn
            )
          }
        }
      }

    }

    "handling requests to display the check you answers page" must {

      def performAction(): Future[Result] = controller.checkYourAnswers()(FakeRequest())

      val completeAnswers = CompleteYearToDateLiabilityAnswers(
        AmountInPence(1L),
        Some(AmountInPence(2L)),
        sample[Boolean],
        setTaxDue(sample[CalculatedTaxDue], AmountInPence(3L)),
        AmountInPence(4L),
        Some(sample[String])
      )

      val allQuestionAnswered = IncompleteYearToDateLiabilityAnswers(
        Some(completeAnswers.estimatedIncome),
        completeAnswers.personalAllowance,
        Some(completeAnswers.hasEstimatedDetails),
        Some(completeAnswers.calculatedTaxDue),
        Some(completeAnswers.taxDue),
        completeAnswers.mandatoryEvidence
      )

      def testRedirectWhenIncompleteAnswers(
        answers: IncompleteYearToDateLiabilityAnswers,
        expectedRedirect: Call
      ): Unit = {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionWithState(answers, sample[DisposalDate])._1)
        }

        checkIsRedirect(performAction(), expectedRedirect)
      }

      behave like redirectToStartBehaviour(performAction)

      behave like unsuccessfulUpdateBehaviour(
        allQuestionAnswered,
        completeAnswers,
        () => performAction()
      )

      "redirect to the estimated income page" when {

        "that question has not been answered yet" in {
          testRedirectWhenIncompleteAnswers(
            allQuestionAnswered.copy(estimatedIncome = None),
            routes.YearToDateLiabilityFirstReturnController.estimatedIncome()
          )
        }

      }

      "redirect to the personal allowance page" when {

        "that question has not been answered yet and the estimated income is non zero" in {
          testRedirectWhenIncompleteAnswers(
            allQuestionAnswered.copy(
              estimatedIncome   = Some(AmountInPence(1L)),
              personalAllowance = None
            ),
            routes.YearToDateLiabilityFirstReturnController.personalAllowance()
          )
        }

      }

      "redirect to the has estimated details  page" when {

        "that question has not been answered yet" in {
          testRedirectWhenIncompleteAnswers(
            allQuestionAnswered.copy(
              estimatedIncome     = Some(AmountInPence.zero),
              personalAllowance   = None,
              hasEstimatedDetails = None
            ),
            routes.YearToDateLiabilityFirstReturnController.hasEstimatedDetails()
          )
        }

      }

      "redirect to the tax due page" when {

        "that question has not been answered yet" in {
          testRedirectWhenIncompleteAnswers(
            allQuestionAnswered.copy(
              taxDue = None
            ),
            routes.YearToDateLiabilityFirstReturnController.taxDue()
          )
        }

        "there is no calulated tax due" in {
          testRedirectWhenIncompleteAnswers(
            allQuestionAnswered.copy(
              calculatedTaxDue = None
            ),
            routes.YearToDateLiabilityFirstReturnController.taxDue()
          )
        }

      }

      "redirect to the upload mandatory evidence page" when {

        "that question hasn't been completed yet and the calculated tax due is not the same as the submitted tax due" in {
          testRedirectWhenIncompleteAnswers(
            allQuestionAnswered.copy(
              calculatedTaxDue  = Some(setTaxDue(sample[CalculatedTaxDue], AmountInPence(500L))),
              taxDue            = Some(AmountInPence(200L)),
              mandatoryEvidence = None
            ),
            routes.YearToDateLiabilityFirstReturnController.uploadMandatoryEvidence()
          )
        }

      }

      "show the page" when {

        "the section is complete" in {
          forAll { completeAnswers: CompleteYearToDateLiabilityAnswers =>
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(sessionWithState(completeAnswers, sample[DisposalDate])._1)
            }

            checkPageIsDisplayed(
              performAction(),
              messageFromMessageKey("ytdLiability.cya.title"), { doc =>
                validateYearToDateLiabilityFirstReturnPage(completeAnswers, doc)
              }
            )
          }
        }

        "the section has just been completed and all updates are successful" in {
          val (session, journey) = sessionWithState(allQuestionAnswered, sample[DisposalDate])
          val updatedDraftReturn = journey.draftReturn.copy(yearToDateLiabilityAnswers = Some(completeAnswers))
          val updatedSession = session.copy(journeyStatus = Some(
            journey.copy(draftReturn = updatedDraftReturn)
          )
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(updatedDraftReturn)(Right(()))
            mockStoreSession(updatedSession)(Right(()))
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("ytdLiability.cya.title")
          )
        }

      }

    }

    "handling requests to display the upload mandatory evidence page" must {

      def performAction(): Future[Result] = controller.uploadMandatoryEvidence()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      behave like commonUploadMandatoryEvidenceBehaviour(performAction)

      "display the page" when {

        def test(
          answers: YearToDateLiabilityAnswers,
          backLink: Call
        ): Unit = {
          val draftReturn = draftReturnWithCompleteJourneys(Some(answers), sample[DisposalDate])
          val session = SessionData.empty.copy(
            journeyStatus = Some(
              sample[FillingOutReturn].copy(
                draftReturn = draftReturn
              )
            )
          )
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("mandatoryEvidence.title"), { doc =>
              doc.select("#back").attr("href") shouldBe backLink.url
              doc
                .select("#content > article > form")
                .attr("action") shouldBe routes.YearToDateLiabilityFirstReturnController
                .uploadMandatoryEvidenceSubmit()
                .url
            }
          )
        }

        val calculatedTaxDue = sample[GainCalculatedTaxDue].copy(amountOfTaxDue = AmountInPence(100L))

        "the section is incomplete" in {
          test(
            IncompleteYearToDateLiabilityAnswers(
              Some(AmountInPence.zero),
              None,
              Some(true),
              Some(calculatedTaxDue),
              Some(AmountInPence(200L)),
              None
            ),
            routes.YearToDateLiabilityFirstReturnController.taxDue()
          )
        }

        "the section is complete" in {
          test(
            sample[CompleteYearToDateLiabilityAnswers].copy(
              calculatedTaxDue = calculatedTaxDue,
              taxDue           = AmountInPence(200L)
            ),
            routes.YearToDateLiabilityFirstReturnController.checkYourAnswers()
          )
        }

      }

    }

    "handling submitted files from the upload mandatory evidence page" must {

      def performAction(): Future[Result] = controller.uploadMandatoryEvidenceSubmit()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      behave like commonUploadMandatoryEvidenceBehaviour(performAction)

    }

    "handling submits from the check you answers page" must {

      def performAction(): Future[Result] = controller.checkYourAnswersSubmit()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      "redirect to the task list page" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionWithState(sample[CompleteYearToDateLiabilityAnswers], sample[DisposalDate])._1)
        }

        checkIsRedirect(performAction(), returns.routes.TaskListController.taskList())
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
                hasEstimatedDetails = Some(sample[Boolean])
              ),
              sample[DisposalDate]
            )._1
          )
        }

        checkIsRedirect(performAction(), routes.YearToDateLiabilityFirstReturnController.checkYourAnswers())
      }

    }

  def incompleteOtherJourneysBehaviour(performAction: () => Future[Result]): Unit = {
    val draftReturn = draftReturnWithCompleteJourneys(
      Some(
        sample[CompleteYearToDateLiabilityAnswers].copy(estimatedIncome = AmountInPence.zero, personalAllowance = None)
      ),
      sample[DisposalDate]
    )

    "redirect to the task list page" when {

      def test(draftReturn: DraftReturn): Unit = {
        val session = SessionData.empty.copy(
          journeyStatus = Some(
            sample[FillingOutReturn].copy(draftReturn = draftReturn)
          )
        )
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkIsRedirect(performAction(), returns.routes.TaskListController.taskList())
      }

      "the triage section is incomplete" in {
        test(draftReturn.copy(triageAnswers = sample[IncompleteSingleDisposalTriageAnswers]))
      }

      "the disposal details hasn't been started yet" in {
        test(draftReturn.copy(disposalDetailsAnswers = None))
      }

      "the disposal details hasn't been completed yet" in {
        test(
          draftReturn.copy(disposalDetailsAnswers = Some(sample[IncompleteDisposalDetailsAnswers]))
        )
      }

      "the acquisition details hasn't been started yet" in {
        test(draftReturn.copy(acquisitionDetailsAnswers = None))
      }

      "the acquisition details hasn't been completed yet" in {
        test(
          draftReturn
            .copy(acquisitionDetailsAnswers = Some(sample[IncompleteAcquisitionDetailsAnswers]))
        )
      }

      "the relief details hasn't been started yet" in {
        test(draftReturn.copy(reliefDetailsAnswers = None))
      }

      "the relief details hasn't been completed yet" in {
        test(draftReturn.copy(reliefDetailsAnswers = Some(sample[IncompleteReliefDetailsAnswers])))
      }

      "the exemptions and losses section hasn't been started yet" in {
        test(draftReturn.copy(exemptionAndLossesAnswers = None))
      }

      "the exemptions and losses section hasn't been completed yet" in {
        test(
          draftReturn
            .copy(exemptionAndLossesAnswers = Some(sample[IncompleteExemptionAndLossesAnswers]))
        )
      }

    }

  }

  def unsuccessfulUpdateBehaviour(
    currentAnswers: YearToDateLiabilityAnswers,
    updatedAnswers: YearToDateLiabilityAnswers,
    result: () => Future[Result]
  ): Unit = {
    val journey = sessionWithState(
      currentAnswers,
      sample[DisposalDate].copy(taxYear = sample[TaxYear].copy(personalAllowance = AmountInPence(Long.MaxValue)))
    )._2
    unsuccessfulUpdateBehaviour(
      journey.draftReturn,
      journey.draftReturn.copy(yearToDateLiabilityAnswers = Some(updatedAnswers)),
      result
    )
  }

  def unsuccessfulUpdateBehaviour(
    currentDraftReturn: DraftReturn,
    updatedDraftReturn: DraftReturn,
    result: () => Future[Result]
  ): Unit = {
    val journey = sample[FillingOutReturn].copy(draftReturn = currentDraftReturn)
    val session = SessionData.empty.copy(journeyStatus      = Some(journey))

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
        doc.title() should startWith("Error:")
      },
      BAD_REQUEST
    )
  }

  def testSuccessfulUpdatesAfterSubmit(
    result: => Future[Result],
    oldDraftReturn: DraftReturn,
    updatedDraftReturn: DraftReturn
  ): Unit = {
    val journey = sample[FillingOutReturn].copy(draftReturn = oldDraftReturn)
    val session = SessionData.empty.copy(journeyStatus      = Some(journey))

    inSequence {
      mockAuthWithNoRetrievals()
      mockGetSession(session)
      mockStoreDraftReturn(updatedDraftReturn)(Right(()))
      mockStoreSession(
        session
          .copy(journeyStatus = Some(journey.copy(draftReturn = updatedDraftReturn)))
      )(Right(()))
    }

    checkIsRedirect(result, routes.YearToDateLiabilityFirstReturnController.checkYourAnswers())
  }

  def testSuccessfulUpdatesAfterSubmit(
    result: => Future[Result],
    oldAnswers: Option[YearToDateLiabilityAnswers],
    newAnswers: YearToDateLiabilityAnswers,
    disposalDate: DisposalDate
  ): Unit = {
    val draftReturn = draftReturnWithCompleteJourneys(oldAnswers, disposalDate)
    val newDraftReturn = draftReturn.copy(
      yearToDateLiabilityAnswers = Some(newAnswers)
    )
    testSuccessfulUpdatesAfterSubmit(result, draftReturn, newDraftReturn)
  }

  def testSuccessfulUpdatesAfterSubmit(
    result: => Future[Result],
    oldAnswers: YearToDateLiabilityAnswers,
    newAnswers: YearToDateLiabilityAnswers,
    disposalDate: DisposalDate = sample[DisposalDate]
  ): Unit = testSuccessfulUpdatesAfterSubmit(result, Some(oldAnswers), newAnswers, disposalDate)

  def commonUploadMandatoryEvidenceBehaviour(performAction: () => Future[Result]): Unit =
    "redirect to the check your answers page" when {

      val calculatedTaxDue = sample[GainCalculatedTaxDue].copy(amountOfTaxDue = AmountInPence(100L))

      val answers = IncompleteYearToDateLiabilityAnswers.empty.copy(
        estimatedIncome     = Some(AmountInPence(1L)),
        personalAllowance   = Some(AmountInPence.zero),
        hasEstimatedDetails = Some(false)
      )

      "there is no answer to the tax due question" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionWithState(answers, sample[DisposalDate])._1)
        }

        checkIsRedirect(performAction(), routes.YearToDateLiabilityFirstReturnController.checkYourAnswers())
      }
      "there is no answer to the tax due but it is equal to the calculated tax due" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            sessionWithState(answers.copy(taxDue = Some(calculatedTaxDue.amountOfTaxDue)), sample[DisposalDate])._1
          )
        }

        checkIsRedirect(performAction(), routes.YearToDateLiabilityFirstReturnController.checkYourAnswers())
      }

      "the user hasn't verified whether or not any details given in the return were estimates" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            sessionWithState(
              answers.copy(hasEstimatedDetails = None),
              sample[DisposalDate]
            )._1
          )
        }

        checkIsRedirect(performAction(), routes.YearToDateLiabilityFirstReturnController.checkYourAnswers())
      }

    }

}

object YearToDateLiabilityFirstReturnControllerSpec extends Matchers {
  def validateYearToDateLiabilityFirstReturnPage(
    completeYearToDateLiabilityAnswers: CompleteYearToDateLiabilityAnswers,
    doc: Document
  )(implicit messages: MessagesApi, lang: Lang): Unit = {
    doc.select("#estimatedIncome-value-answer").text() shouldBe formatAmountOfMoneyWithPoundSign(
      completeYearToDateLiabilityAnswers.estimatedIncome.inPounds()
    )

    completeYearToDateLiabilityAnswers.personalAllowance.foreach(f =>
      doc.select("#personalAllowance-value-answer").text() shouldBe formatAmountOfMoneyWithPoundSign(f.inPounds())
    )

    if (completeYearToDateLiabilityAnswers.hasEstimatedDetails)
      doc.select("#hasEstimatedDetails-value-answer").text() shouldBe "Yes"
    else
      doc.select("#hasEstimatedDetails-value-answer").text() shouldBe "No"

    doc.select("#taxDue-value-answer").text() shouldBe formatAmountOfMoneyWithPoundSign(
      completeYearToDateLiabilityAnswers.taxDue.inPounds()
    )
  }
}
