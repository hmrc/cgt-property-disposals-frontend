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

import java.time.LocalDate
import java.util.UUID

import cats.data.EitherT
import cats.instances.future._
import cats.syntax.order._
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
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AmountOfMoneyErrorScenarios, AuthSupport, ControllerSpec, SessionSupport, returns}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators.{sample, _}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.FillingOutReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address.UkAddress
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.MoneyUtils.formatAmountOfMoneyWithPoundSign
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.{AmountInPence, MoneyUtils}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.{IndividualName, TrustName}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.SubscribedDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.AcquisitionDetailsAnswers.{CompleteAcquisitionDetailsAnswers, IncompleteAcquisitionDetailsAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.CalculatedTaxDue.GainCalculatedTaxDue
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.DisposalDetailsAnswers.{CompleteDisposalDetailsAnswers, IncompleteDisposalDetailsAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ExemptionAndLossesAnswers.{CompleteExemptionAndLossesAnswers, IncompleteExemptionAndLossesAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ReliefDetailsAnswers.{CompleteReliefDetailsAnswers, IncompleteReliefDetailsAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.{CompleteSingleDisposalTriageAnswers, IncompleteSingleDisposalTriageAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.UploadSupportingDocuments.CompleteUploadSupportingDocuments
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.YearToDateLiabilityAnswers.CalculatedYTDAnswers._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.YearToDateLiabilityAnswers.NonCalculatedYTDAnswers.{CompleteNonCalculatedYTDAnswers, IncompleteNonCalculatedYTDAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, LocalDateUtils, SessionData, TaxYear}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.{CgtCalculationService, ReturnsService}
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class YearToDateLiabilityControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with ReturnsServiceSupport
    with ScalaCheckDrivenPropertyChecks
    with RedirectToStartBehaviour {

  import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.yeartodatelliability.YearToDateLiabilityControllerSpec._

  val mockCgtCalculationService = mock[CgtCalculationService]

  override val overrideBindings =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore),
      bind[ReturnsService].toInstance(mockReturnsService),
      bind[CgtCalculationService].toInstance(mockCgtCalculationService)
    )

  lazy val controller = instanceOf[YearToDateLiabilityController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  implicit lazy val messages: Messages = MessagesImpl(Lang("en"), messagesApi)

  def redirectToStartBehaviour(performAction: () => Future[Result]) =
    redirectToStartWhenInvalidJourney(
      performAction, {
        case _: FillingOutReturn => true
        case _                   => false
      }
    )

  def sessionWithSingleDisposalState(
    ytdLiabilityAnswers: Option[YearToDateLiabilityAnswers],
    disposalDate: Option[DisposalDate],
    reliefDetailsAnswers: Option[ReliefDetailsAnswers] = Some(sample[CompleteReliefDetailsAnswers])
  ): (SessionData, FillingOutReturn, DraftSingleDisposalReturn) = {
    val draftReturn = sample[DraftSingleDisposalReturn].copy(
      triageAnswers              = sample[IncompleteSingleDisposalTriageAnswers].copy(disposalDate = disposalDate),
      reliefDetailsAnswers       = reliefDetailsAnswers,
      yearToDateLiabilityAnswers = ytdLiabilityAnswers
    )
    val journey = sample[FillingOutReturn].copy(draftReturn = draftReturn)
    val fillingOutReturnWithTrust =
      journey.copy(subscribedDetails = journey.subscribedDetails.copy(name = Right(sample[IndividualName])))
    (
      SessionData.empty.copy(journeyStatus = Some(fillingOutReturnWithTrust)),
      fillingOutReturnWithTrust,
      draftReturn
    )
  }

  def sessionWithSingleDisposalState(
    ytdLiabilityAnswers: YearToDateLiabilityAnswers,
    disposalDate: DisposalDate
  ): (SessionData, FillingOutReturn, DraftSingleDisposalReturn) =
    sessionWithSingleDisposalState(Some(ytdLiabilityAnswers), Some(disposalDate))

  def sessionWithMultipleDisposalsState(
    ytdLiabilityAnswers: Option[YearToDateLiabilityAnswers]
  ): (SessionData, FillingOutReturn, DraftMultipleDisposalsReturn) = {
    val draftReturn = sample[DraftMultipleDisposalsReturn].copy(
      yearToDateLiabilityAnswers = ytdLiabilityAnswers
    )
    val journey = sample[FillingOutReturn].copy(
      subscribedDetails = sample[SubscribedDetails].copy(name = Right(sample[IndividualName])),
      draftReturn       = draftReturn
    )
    (
      SessionData.empty.copy(journeyStatus = Some(journey)),
      journey,
      draftReturn
    )
  }

  def sessionWithMultipleDisposalsState(
    ytdLiabilityAnswers: YearToDateLiabilityAnswers
  ): (SessionData, FillingOutReturn, DraftMultipleDisposalsReturn) =
    sessionWithMultipleDisposalsState(Some(ytdLiabilityAnswers))

  def singleDispsaslDraftReturnWithCompleteJourneys(
    yearToDateLiabilityAnswers: Option[YearToDateLiabilityAnswers],
    disposalDate: DisposalDate,
    reliefDetailsAnswers: ReliefDetailsAnswers
  ): DraftSingleDisposalReturn =
    DraftSingleDisposalReturn(
      UUID.randomUUID(),
      sample[CompleteSingleDisposalTriageAnswers].copy(disposalDate = disposalDate),
      Some(sample[UkAddress]),
      Some(sample[CompleteDisposalDetailsAnswers]),
      Some(sample[CompleteAcquisitionDetailsAnswers]),
      Some(reliefDetailsAnswers),
      Some(sample[CompleteExemptionAndLossesAnswers]),
      yearToDateLiabilityAnswers,
      Some(sample[AmountInPence]),
      Some(sample[CompleteUploadSupportingDocuments]),
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

  val completeReliefDetailsAnswersWithNoOtherReliefs = sample[CompleteReliefDetailsAnswers].copy(otherReliefs = None)

  "YearToDateLiabilityController" when {

    "handling requests to display the estimated income page" must {

      def performAction(): Future[Result] = controller.estimatedIncome()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      behave like noDisposalDateBehaviour(performAction)

      behave like redirectWhenNotSingleDisposalCalculatedJourneyBehaviour(performAction)

      "display the page" when {

        "the user has not answered the question before" in {

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithSingleDisposalState(
                None,
                Some(sample[DisposalDate]),
                Some(completeReliefDetailsAnswersWithNoOtherReliefs)
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("estimatedIncome.title"), { doc =>
              doc.select("#back").attr("href") shouldBe returns.routes.TaskListController.taskList().url
              doc
                .select("#content > article > form")
                .attr("action") shouldBe routes.YearToDateLiabilityController
                .estimatedIncomeSubmit()
                .url
            }
          )

        }

        "the user has answered the question before but has " +
          "not completed the section" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithSingleDisposalState(
                IncompleteCalculatedYTDAnswers.empty.copy(
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
          "completed the section" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithSingleDisposalState(
                sample[CompleteCalculatedYTDAnswers]
                  .copy(estimatedIncome = AmountInPence.fromPounds(12.34)),
                sample[DisposalDate]
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("estimatedIncome.title"), { doc =>
              doc.select("#estimatedIncome").attr("value") shouldBe "12.34"
              doc.select("#back").attr("href") shouldBe routes.YearToDateLiabilityController
                .checkYourAnswers()
                .url
              doc
                .select("#content > article > form")
                .attr("action") shouldBe routes.YearToDateLiabilityController
                .estimatedIncomeSubmit()
                .url
            }
          )
        }

      }

      "redirect the page" when {
        "the user is a Trust" in {
          val (session, fillingOutReturn, draftReturn) = sessionWithSingleDisposalState(
            None,
            Some(sample[DisposalDate]),
            Some(completeReliefDetailsAnswersWithNoOtherReliefs)
          )

          val fillingOutReturnWithTrust = fillingOutReturn.copy(subscribedDetails =
            fillingOutReturn.subscribedDetails.copy(name = Left(sample[TrustName]))
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              session.copy(journeyStatus = Some(fillingOutReturnWithTrust))
            )
          }

          checkIsRedirect(
            performAction(),
            routes.YearToDateLiabilityController.checkYourAnswers()
          )
        }
      }
    }

    "handling submitted answers to the estimated income page" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.estimatedIncomeSubmit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      behave like redirectToStartBehaviour(() => performAction())

      behave like noDisposalDateBehaviour(() => performAction())

      behave like redirectWhenNotSingleDisposalCalculatedJourneyBehaviour(() => performAction())

      behave like unsuccessfulUpdateBehaviourForSingleDisposal(
        IncompleteCalculatedYTDAnswers.empty,
        IncompleteCalculatedYTDAnswers.empty.copy(
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
          testSuccessfulUpdatesAfterSubmitWithSingleDisposal(
            performAction("estimatedIncome" -> "1"),
            None,
            IncompleteCalculatedYTDAnswers.empty.copy(estimatedIncome = Some(AmountInPence(100L))),
            completeReliefDetailsAnswersWithNoOtherReliefs,
            sample[DisposalDate]
          )
        }

        "the user had started answering questions in this section but had not completed it" in {
          val answers = sample[IncompleteCalculatedYTDAnswers]
          testSuccessfulUpdatesAfterSubmitWithSingleDisposal(
            performAction("estimatedIncome" -> "1"),
            answers.copy(estimatedIncome                              = Some(AmountInPence(1L))),
            IncompleteCalculatedYTDAnswers.empty.copy(estimatedIncome = Some(AmountInPence(100L)))
          )
        }

        "the user had already completed the section" in {
          val oldAnswers =
            sample[CompleteCalculatedYTDAnswers].copy(estimatedIncome = AmountInPence(1L))
          val newAnswers =
            IncompleteCalculatedYTDAnswers.empty.copy(estimatedIncome = Some(AmountInPence(100L)))
          testSuccessfulUpdatesAfterSubmitWithSingleDisposal(
            performAction("estimatedIncome" -> "1"),
            oldAnswers,
            newAnswers
          )
        }

      }

      "not do any updates if the submitted answer is the same as one already stored in session and" when {

        "the section is incomplete" in {
          val answers = sample[IncompleteCalculatedYTDAnswers].copy(
            estimatedIncome   = Some(AmountInPence(1L)),
            personalAllowance = Some(AmountInPence(2L))
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionWithSingleDisposalState(answers, sample[DisposalDate])._1)
          }

          checkIsRedirect(
            performAction("estimatedIncome" -> "0.01"),
            routes.YearToDateLiabilityController.checkYourAnswers()
          )
        }

        "the section is complete" in {
          val answers = sample[CompleteCalculatedYTDAnswers].copy(
            estimatedIncome   = AmountInPence(1L),
            personalAllowance = Some(AmountInPence(2L))
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionWithSingleDisposalState(answers, sample[DisposalDate])._1)
          }

          checkIsRedirect(
            performAction("estimatedIncome" -> "0.01"),
            routes.YearToDateLiabilityController.checkYourAnswers()
          )
        }

      }

    }

    "handling requests to display the personal allowance page" must {

      def performAction(): Future[Result] = controller.personalAllowance()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      behave like noDisposalDateBehaviour(performAction)

      behave like noEstimatedIncomeBehaviour(performAction)

      behave like redirectWhenNotSingleDisposalCalculatedJourneyBehaviour(performAction)

      "redirect to the check you answers page" when {

        "the estimated income is zero" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithSingleDisposalState(
                sample[CompleteCalculatedYTDAnswers].copy(
                  estimatedIncome = AmountInPence.zero
                ),
                sample[DisposalDate]
              )._1
            )
          }

          checkIsRedirect(performAction(), routes.YearToDateLiabilityController.checkYourAnswers())
        }
        "the user is a Trust" in {
          val (session, fillingOutReturn, draftReturn) = sessionWithSingleDisposalState(
            sample[CompleteCalculatedYTDAnswers].copy(
              estimatedIncome = AmountInPence(100)
            ),
            sample[DisposalDate]
          )

          val fillingOutReturnWithTrust = fillingOutReturn.copy(subscribedDetails =
            fillingOutReturn.subscribedDetails.copy(name = Left(sample[TrustName]))
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              session.copy(journeyStatus = Some(fillingOutReturnWithTrust))
            )
          }

          checkIsRedirect(
            performAction(),
            routes.YearToDateLiabilityController.checkYourAnswers()
          )
        }
      }

      "display the page" when {

        "the estimated income is greater than zero and" when {

          "the section is incomplete" in {
            val taxYear2020 =
              sample[TaxYear].copy(startDateInclusive = LocalDate.of(2020, 4, 6), LocalDate.of(2021, 4, 6))
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionWithSingleDisposalState(
                  sample[IncompleteCalculatedYTDAnswers].copy(
                    estimatedIncome   = Some(AmountInPence.fromPounds(12.34)),
                    personalAllowance = None
                  ),
                  sample[DisposalDate].copy(taxYear = taxYear2020)
                )._1
              )
            }

            checkPageIsDisplayed(
              performAction(),
              messageFromMessageKey(
                "personalAllowance.title",
                taxYear2020.startDateInclusive.getYear.toString,
                taxYear2020.endDateExclusive.getYear.toString
              ), { doc =>
                doc.select("#back").attr("href") shouldBe routes.YearToDateLiabilityController
                  .estimatedIncome()
                  .url
                doc
                  .select("#content > article > form")
                  .attr("action") shouldBe routes.YearToDateLiabilityController
                  .personalAllowanceSubmit()
                  .url
              }
            )
          }

          "the section is complete" in {
            val disposalDate = sample[DisposalDate]
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionWithSingleDisposalState(
                  sample[CompleteCalculatedYTDAnswers].copy(
                    estimatedIncome   = AmountInPence(1L),
                    personalAllowance = Some(AmountInPence(1234L))
                  ),
                  disposalDate
                )._1
              )
            }

            checkPageIsDisplayed(
              performAction(),
              messageFromMessageKey(
                "personalAllowance.title",
                disposalDate.taxYear.startDateInclusive.getYear.toString,
                disposalDate.taxYear.endDateExclusive.getYear.toString
              ), { doc =>
                doc.select("#personalAllowance").attr("value") shouldBe "12.34"
                doc.select("#back").attr("href") shouldBe routes.YearToDateLiabilityController
                  .checkYourAnswers()
                  .url
                doc
                  .select("#content > article > form")
                  .attr("action") shouldBe routes.YearToDateLiabilityController
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

      behave like redirectWhenNotSingleDisposalCalculatedJourneyBehaviour(() => performAction())

      {
        val completeAnswers = sample[CompleteCalculatedYTDAnswers].copy(
          estimatedIncome   = AmountInPence(1L),
          personalAllowance = Some(AmountInPence(2L))
        )
        val newAnswers = IncompleteCalculatedYTDAnswers.empty.copy(
          estimatedIncome   = Some(completeAnswers.estimatedIncome),
          personalAllowance = Some(AmountInPence.zero)
        )
        behave like unsuccessfulUpdateBehaviourForSingleDisposal(
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
              sessionWithSingleDisposalState(
                sample[CompleteCalculatedYTDAnswers].copy(
                  estimatedIncome = AmountInPence.zero
                ),
                sample[DisposalDate]
              )._1
            )
          }

          checkIsRedirect(performAction(), routes.YearToDateLiabilityController.checkYourAnswers())
        }

      }

      "show a form error" when {

        "the amount of money is invalid" in {
          val personalAllowance    = AmountInPence(1250000L)
          val maxPersonalAllowance = AmountInPence(2000000L)
          val taxYear =
            sample[TaxYear].copy(personalAllowance = personalAllowance, maxPersonalAllowance = maxPersonalAllowance)
          val disposalDate = sample[DisposalDate].copy(taxYear = taxYear)
          val session = sessionWithSingleDisposalState(
            IncompleteCalculatedYTDAnswers.empty.copy(
              estimatedIncome = Some(AmountInPence(1L))
            ),
            disposalDate
          )._1
          val args = Map(
            "personalAllowance.error.tooSmall" -> List(
              MoneyUtils.formatAmountOfMoneyWithPoundSign(taxYear.maxPersonalAllowance.inPounds())
            ),
            "personalAllowance.error.tooLarge" -> List(
              MoneyUtils.formatAmountOfMoneyWithPoundSign(taxYear.maxPersonalAllowance.inPounds()),
              disposalDate.taxYear.startDateInclusive.getYear.toString,
              disposalDate.taxYear.endDateExclusive.getYear.toString
            )
          )
          AmountOfMoneyErrorScenarios
            .amountOfMoneyErrorScenarios("personalAllowance", maxPersonalAllowance.inPounds())
            .foreach { scenario =>
              withClue(s"For $scenario: ") {
                testFormError(scenario.formData: _*)(
                  scenario.expectedErrorMessageKey,
                  args.getOrElse(scenario.expectedErrorMessageKey, Nil)
                )(
                  "personalAllowance.title",
                  disposalDate.taxYear.startDateInclusive.getYear.toString,
                  disposalDate.taxYear.endDateExclusive.getYear.toString
                )(
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
            IncompleteCalculatedYTDAnswers.empty.copy(
              estimatedIncome     = Some(AmountInPence(1L)),
              personalAllowance   = None,
              hasEstimatedDetails = Some(sample[Boolean])
            )

          val newAnswers = oldAnswers.copy(personalAllowance = Some(AmountInPence(100L)), hasEstimatedDetails = None)

          testSuccessfulUpdatesAfterSubmitWithSingleDisposal(
            performAction("personalAllowance" -> "1"),
            oldAnswers,
            newAnswers,
            sample[CompleteReliefDetailsAnswers],
            disposalDate
          )
        }

        "the user had already completed the section" in {
          val oldAnswers =
            sample[CompleteCalculatedYTDAnswers].copy(
              estimatedIncome   = AmountInPence(1L),
              personalAllowance = Some(AmountInPence(2L))
            )

          val newAnswers = IncompleteCalculatedYTDAnswers.empty.copy(
            estimatedIncome   = Some(oldAnswers.estimatedIncome),
            personalAllowance = Some(AmountInPence(100L))
          )

          testSuccessfulUpdatesAfterSubmitWithSingleDisposal(
            performAction("personalAllowance" -> "1"),
            oldAnswers,
            newAnswers,
            completeReliefDetailsAnswersWithNoOtherReliefs,
            disposalDate
          )
        }

      }

      "not do any updates if the submitted answer is the same as one already stored in session and" when {

        val disposalDate = sample[DisposalDate].copy(
          taxYear = sample[TaxYear].copy(personalAllowance = AmountInPence(1000L))
        )

        "the section is incomplete" in {
          val answers = sample[IncompleteCalculatedYTDAnswers].copy(
            estimatedIncome   = Some(AmountInPence(1L)),
            personalAllowance = Some(AmountInPence(2L))
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionWithSingleDisposalState(answers, disposalDate)._1)
          }

          checkIsRedirect(
            performAction("personalAllowance" -> "0.02"),
            routes.YearToDateLiabilityController.checkYourAnswers()
          )
        }

        "the section is complete" in {
          val answers = sample[CompleteCalculatedYTDAnswers].copy(
            estimatedIncome   = AmountInPence(1L),
            personalAllowance = Some(AmountInPence(2L))
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionWithSingleDisposalState(answers, disposalDate)._1)
          }

          checkIsRedirect(
            performAction("personalAllowance" -> "0.02"),
            routes.YearToDateLiabilityController.checkYourAnswers()
          )
        }

      }

    }

    "handling requests to display the has estimated details page" when {

      def performAction(): Future[Result] = controller.hasEstimatedDetails()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      behave like noEstimatedIncomeBehaviour(performAction)

      "handling users on a calculated journey" must {

        "redirect to the personal allowance page" when {

          "the estimated income is more than zero and the user has not answered " +
            "the personal allowance question yet" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionWithSingleDisposalState(
                  IncompleteCalculatedYTDAnswers.empty.copy(
                    estimatedIncome = Some(AmountInPence(1L))
                  ),
                  sample[DisposalDate]
                )._1
              )
            }

            checkIsRedirect(performAction(), routes.YearToDateLiabilityController.personalAllowance())
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
                sessionWithSingleDisposalState(
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
                  .attr("action") shouldBe routes.YearToDateLiabilityController
                  .hasEstimatedDetailsSubmit()
                  .url
              }
            )
          }

          "the estimated income is greater than zero and" when {

            "the section is incomplete and the estimated income is zero" in {
              test(
                sample[IncompleteCalculatedYTDAnswers].copy(
                  estimatedIncome = Some(AmountInPence.zero)
                ),
                routes.YearToDateLiabilityController.estimatedIncome()
              )
            }

            "the section is incomplete and the estimated income is non-zero" in {
              test(
                sample[IncompleteCalculatedYTDAnswers].copy(
                  estimatedIncome   = Some(AmountInPence(100L)),
                  personalAllowance = Some(AmountInPence(1L))
                ),
                routes.YearToDateLiabilityController.personalAllowance()
              )
            }

            "the section is complete and the estimated income is zero" in {
              test(
                sample[CompleteCalculatedYTDAnswers].copy(
                  estimatedIncome   = AmountInPence.zero,
                  personalAllowance = None
                ),
                routes.YearToDateLiabilityController.checkYourAnswers()
              )
            }

            "the section is complete and the estimated income is non-zero" in {
              test(
                sample[CompleteCalculatedYTDAnswers].copy(
                  estimatedIncome   = AmountInPence(100L),
                  personalAllowance = Some(AmountInPence(1L))
                ),
                routes.YearToDateLiabilityController.checkYourAnswers()
              )
            }

          }

        }
      }

      "handling user on a non-calculated journey" must {

        "redirect to the taxable gain page" when {

          "that question has not been answered yet" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionWithMultipleDisposalsState(
                  (
                    IncompleteNonCalculatedYTDAnswers.empty
                  )
                )._1
              )
            }

            checkIsRedirect(performAction(), routes.YearToDateLiabilityController.taxableGainOrLoss())
          }

        }

        "display the page" when {

          def test(
            sessionData: SessionData,
            backLink: Call,
            extraChecks: Document => Unit = _ => ()
          ): Unit = {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(sessionData)
            }

            checkPageIsDisplayed(
              performAction(),
              messageFromMessageKey("hasEstimatedDetails.title"), { doc =>
                doc.select("#back").attr("href") shouldBe backLink.url
                doc
                  .select("#content > article > form")
                  .attr("action") shouldBe routes.YearToDateLiabilityController
                  .hasEstimatedDetailsSubmit()
                  .url
                extraChecks(doc)
              }
            )
          }

          "the user has not answered the question yet" in {
            test(
              sessionWithMultipleDisposalsState(
                IncompleteNonCalculatedYTDAnswers.empty.copy(
                  taxableGainOrLoss = Some(AmountInPence.zero)
                )
              )._1,
              routes.YearToDateLiabilityController.taxableGainOrLoss()
            )
          }

          "the user has answered the question before" in {
            val answers = sample[CompleteNonCalculatedYTDAnswers].copy(
              hasEstimatedDetails = true
            )
            test(
              sessionWithMultipleDisposalsState(answers)._1,
              routes.YearToDateLiabilityController.checkYourAnswers(), { doc =>
                doc.select("#hasEstimatedDetails-true").attr("checked") shouldBe "checked"
              }
            )
          }

        }

      }

      "handling a TRUST user on a non-calculated journey" must {

        "display the page" when {

          def testCompletedWithTrust(
            answers: YearToDateLiabilityAnswers,
            backLink: Call,
            extraChecks: Document => Unit = _ => ()
          ): Unit = {
            val (session, fillingOutReturn, _) = sessionWithMultipleDisposalsState(answers)
            val fillingOutReturnWithTrust = fillingOutReturn.copy(subscribedDetails =
              fillingOutReturn.subscribedDetails.copy(name = Left(sample[TrustName]))
            )
            val sessionDataWithTrust = session.copy(journeyStatus = Some(fillingOutReturnWithTrust))

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(sessionDataWithTrust)
            }

            checkPageIsDisplayed(
              performAction(),
              messageFromMessageKey("hasEstimatedDetails.title"), { doc =>
                doc.select("#back").attr("href") shouldBe backLink.url
                doc.select("#hasEstimatedDetails > #yes-content").text shouldBe messageFromMessageKey(
                  "hasEstimatedDetails.trust.yes.content"
                )
                doc
                  .select("#content > article > form")
                  .attr("action") shouldBe routes.YearToDateLiabilityController
                  .hasEstimatedDetailsSubmit()
                  .url
                extraChecks(doc)
              }
            )
          }

          "the user has not answered the question yet" in {
            testCompletedWithTrust(
              IncompleteNonCalculatedYTDAnswers.empty.copy(
                taxableGainOrLoss   = Some(AmountInPence(100)),
                hasEstimatedDetails = None
              ),
              returns.routes.TaskListController.taskList()
            )
          }

          "the user has answered the question before" in {
            val answers = sample[CompleteNonCalculatedYTDAnswers].copy(
              hasEstimatedDetails = true
            )

            testCompletedWithTrust(
              answers,
              routes.YearToDateLiabilityController.checkYourAnswers(), { doc =>
                doc.select("#hasEstimatedDetails-true").attr("checked") shouldBe "checked"
              }
            )
          }

        }

      }

    }

    "handling submitted answers to the has estimated details page" when {

      def performAction(data: (String, String)*): Future[Result] =
        controller.hasEstimatedDetailsSubmit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      "handling users on a calculated journey" must {
        behave like redirectToStartBehaviour(() => performAction())

        behave like noEstimatedIncomeBehaviour(() => performAction())

        {
          val answers = IncompleteCalculatedYTDAnswers.empty.copy(
            estimatedIncome   = Some(AmountInPence(1L)),
            personalAllowance = Some(AmountInPence(2L))
          )

          behave like unsuccessfulUpdateBehaviourForSingleDisposal(
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
                sessionWithSingleDisposalState(
                  IncompleteCalculatedYTDAnswers.empty.copy(
                    estimatedIncome = Some(AmountInPence(1L))
                  ),
                  sample[DisposalDate]
                )._1
              )
            }

            checkIsRedirect(performAction(), routes.YearToDateLiabilityController.personalAllowance())
          }

        }

        "show a form error" when {
          val answers = IncompleteCalculatedYTDAnswers.empty.copy(
            estimatedIncome   = Some(AmountInPence(1L)),
            personalAllowance = Some(AmountInPence(2L))
          )

          val currentSession = sessionWithSingleDisposalState(answers, sample[DisposalDate])._1

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
              val answers = IncompleteCalculatedYTDAnswers.empty.copy(
                estimatedIncome   = Some(AmountInPence(1L)),
                personalAllowance = Some(AmountInPence(2L))
              )

              val updatedAnswers = answers.copy(hasEstimatedDetails = Some(false))

              testSuccessfulUpdatesAfterSubmitWithSingleDisposal(
                performAction("hasEstimatedDetails" -> "false"),
                answers,
                updatedAnswers
              )
            }

            "the journey was complete" in {
              val answers = CompleteCalculatedYTDAnswers(
                estimatedIncome     = AmountInPence.zero,
                personalAllowance   = None,
                hasEstimatedDetails = false,
                calculatedTaxDue    = sample[CalculatedTaxDue],
                taxDue              = sample[AmountInPence],
                Some(sample[String])
              )

              val updatedAnswers =
                IncompleteCalculatedYTDAnswers(
                  estimatedIncome     = Some(AmountInPence.zero),
                  personalAllowance   = None,
                  hasEstimatedDetails = Some(true),
                  None,
                  None,
                  None
                )

              val draftReturn        = sample[DraftSingleDisposalReturn].copy(yearToDateLiabilityAnswers = Some(answers))
              val updatedDraftReturn = draftReturn.copy(yearToDateLiabilityAnswers                       = Some(updatedAnswers))

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

            val session = sessionWithSingleDisposalState(
              IncompleteCalculatedYTDAnswers.empty.copy(
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
              routes.YearToDateLiabilityController.checkYourAnswers()
            )
          }

          "the section is complete" in {
            val session = sessionWithSingleDisposalState(
              CompleteCalculatedYTDAnswers(
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
              routes.YearToDateLiabilityController.checkYourAnswers()
            )
          }

        }
      }

      "handling users on a non-calculated journey" must {

        {
          val answers = IncompleteNonCalculatedYTDAnswers.empty.copy(
            taxableGainOrLoss = Some(AmountInPence.zero)
          )

          val draftReturn = sessionWithMultipleDisposalsState(answers)._3
          val updatedDraftReturn =
            draftReturn.copy(yearToDateLiabilityAnswers = Some(answers.copy(hasEstimatedDetails = Some(false))))

          behave like unsuccessfulUpdateBehaviour(
            draftReturn,
            updatedDraftReturn,
            () => performAction("hasEstimatedDetails" -> "false")
          )
        }

        "show a form error" when {
          val answers = sample[CompleteNonCalculatedYTDAnswers].copy(
            hasEstimatedDetails = false
          )
          val currentSession = sessionWithMultipleDisposalsState(answers)._1

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

            "the user is on a multiple disposal journey" in {
              val answers =
                IncompleteNonCalculatedYTDAnswers.empty.copy(
                  taxableGainOrLoss = Some(AmountInPence(1L))
                )

              val (session, journey, draftReturn) = sessionWithMultipleDisposalsState(answers)
              val updatedDraftReturn =
                draftReturn.copy(yearToDateLiabilityAnswers = Some(answers.copy(hasEstimatedDetails = Some(true))))
              val updatedSession = session.copy(
                journeyStatus = Some(journey.copy(draftReturn = updatedDraftReturn))
              )

              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
                mockStoreDraftReturn(
                  updatedDraftReturn,
                  journey.subscribedDetails.cgtReference,
                  journey.agentReferenceNumber
                )(Right(()))
                mockStoreSession(updatedSession)(Right(()))
              }

              checkIsRedirect(
                performAction("hasEstimatedDetails" -> "true"),
                routes.YearToDateLiabilityController.checkYourAnswers()
              )
            }

            "the user is on a non-calculated single disposal journey" in {
              val answers =
                sample[CompleteNonCalculatedYTDAnswers].copy(
                  hasEstimatedDetails = true
                )

              val newAnswers = IncompleteNonCalculatedYTDAnswers(
                Some(answers.taxableGainOrLoss),
                Some(false),
                None
              )

              val (session, journey, draftReturn) = sessionWithSingleDisposalState(
                Some(answers),
                Some(sample[DisposalDate]),
                Some(
                  sample[CompleteReliefDetailsAnswers].copy(otherReliefs = Some(sample[OtherReliefsOption.OtherReliefs])
                  )
                )
              )
              val updatedDraftReturn =
                draftReturn.copy(yearToDateLiabilityAnswers = Some(newAnswers))
              val updatedSession = session.copy(
                journeyStatus = Some(journey.copy(draftReturn = updatedDraftReturn))
              )

              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
                mockStoreDraftReturn(
                  updatedDraftReturn,
                  journey.subscribedDetails.cgtReference,
                  journey.agentReferenceNumber
                )(Right(()))
                mockStoreSession(updatedSession)(Right(()))
              }

              checkIsRedirect(
                performAction("hasEstimatedDetails" -> "false"),
                routes.YearToDateLiabilityController.checkYourAnswers()
              )

            }

          }
        }

        "not do any updates" when {

          "the answer in the session is the same as the one already stored" in {
            val answers =
              IncompleteNonCalculatedYTDAnswers.empty.copy(
                taxableGainOrLoss   = Some(AmountInPence(1L)),
                hasEstimatedDetails = Some(false)
              )

            val session = sessionWithMultipleDisposalsState(answers)._1

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
            }

            checkIsRedirect(
              performAction("hasEstimatedDetails" -> "false"),
              routes.YearToDateLiabilityController.checkYourAnswers()
            )
          }

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
      val initialGainOrLossAnswers  = sample[AmountInPence]

      def draftReturnWithAnswers(yearToDateLiabilityAnswers: YearToDateLiabilityAnswers): DraftSingleDisposalReturn =
        sample[DraftSingleDisposalReturn].copy(
          triageAnswers              = triageAnswers,
          disposalDetailsAnswers     = Some(disposalDetailsAnswers),
          acquisitionDetailsAnswers  = Some(acquisitionDetailsAnswers),
          reliefDetailsAnswers       = Some(reliefDetailsAnswers),
          exemptionAndLossesAnswers  = Some(exemptionAndLossesAnswers),
          yearToDateLiabilityAnswers = Some(yearToDateLiabilityAnswers),
          initialGainOrLoss          = Some(initialGainOrLossAnswers)
        )

      def calculateRequest(estimatedIncome: AmountInPence, personalAllowance: AmountInPence, isATrust: Boolean) =
        CalculateCgtTaxDueRequest(
          triageAnswers,
          disposalDetailsAnswers,
          acquisitionDetailsAnswers,
          reliefDetailsAnswers,
          exemptionAndLossesAnswers,
          estimatedIncome,
          personalAllowance,
          Some(initialGainOrLossAnswers),
          isATrust
        )

      behave like redirectToStartBehaviour(performAction)

      behave like noEstimatedIncomeBehaviour(performAction)

      behave like incompleteOtherJourneysBehaviour(performAction)

      behave like redirectWhenNotSingleDisposalCalculatedJourneyBehaviour(performAction)

      "redirect to the check your answers page" when {

        "the estimated income is more than zero and the user has not answered " +
          "the personal allowance question yet" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithSingleDisposalState(
                IncompleteCalculatedYTDAnswers.empty.copy(
                  estimatedIncome = Some(AmountInPence(1L))
                ),
                sample[DisposalDate]
              )._1
            )
          }

          checkIsRedirect(performAction(), routes.YearToDateLiabilityController.checkYourAnswers())
        }

        "the user hasn't verified whether or not any details given in the return were estimates" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  sample[FillingOutReturn].copy(
                    subscribedDetails = sample[SubscribedDetails].copy(name = Right(sample[IndividualName])),
                    draftReturn = singleDispsaslDraftReturnWithCompleteJourneys(
                      Some(
                        IncompleteCalculatedYTDAnswers.empty.copy(
                          estimatedIncome   = Some(AmountInPence(1L)),
                          personalAllowance = Some(AmountInPence(2L))
                        )
                      ),
                      sample[DisposalDate],
                      completeReliefDetailsAnswersWithNoOtherReliefs
                    )
                  )
                )
              )
            )
          }

          checkIsRedirect(performAction(), routes.YearToDateLiabilityController.checkYourAnswers())
        }

      }

      "show an error page" when {

        "there is an error getting the calculated tax due" in {
          val subscribedDetails = sample[SubscribedDetails].copy(name = Right(sample[IndividualName]))

          val answers = IncompleteCalculatedYTDAnswers.empty.copy(
            estimatedIncome     = Some(AmountInPence(1L)),
            personalAllowance   = Some(AmountInPence(2L)),
            hasEstimatedDetails = Some(false)
          )

          val calculateCgtTaxDueRequest = calculateRequest(AmountInPence(1L), AmountInPence(2L), isATrust = false)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  sample[FillingOutReturn]
                    .copy(subscribedDetails = subscribedDetails, draftReturn = draftReturnWithAnswers(answers))
                )
              )
            )
            mockCalculationService(calculateCgtTaxDueRequest)(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction())
        }

        "there is an error storing the calculated tax due" in {
          val subscribedDetails = sample[SubscribedDetails].copy(name = Right(sample[IndividualName]))

          val answers = IncompleteCalculatedYTDAnswers.empty.copy(
            estimatedIncome     = Some(AmountInPence(1L)),
            personalAllowance   = Some(AmountInPence(2L)),
            hasEstimatedDetails = Some(false)
          )

          val calculateCgtTaxDueRequest = calculateRequest(AmountInPence(1L), AmountInPence(2L), isATrust = false)
          val calculatedTaxDue          = sample[CalculatedTaxDue]
          val draftReturn               = draftReturnWithAnswers(answers)
          val fillingOutReturn =
            sample[FillingOutReturn].copy(draftReturn = draftReturn, subscribedDetails = subscribedDetails)
          val updatedFillingOutReturn = fillingOutReturn.copy(draftReturn = draftReturn.copy(
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
          subscribedDetails: SubscribedDetails,
          mockCalculateTaxDue: (FillingOutReturn, DraftSingleDisposalReturn) => Unit,
          backLink: Call
        ): Unit = {
          val draftReturn = draftReturnWithAnswers(answers)
          val fillingOutReturn = sample[FillingOutReturn].copy(
            subscribedDetails = subscribedDetails,
            draftReturn       = draftReturn
          )

          val session = SessionData.empty.copy(journeyStatus = Some(fillingOutReturn))
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockCalculateTaxDue(fillingOutReturn, draftReturn)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("taxDue.title"), { doc =>
              doc.select("#back").attr("href") shouldBe backLink.url
              doc
                .select("#content > article > form")
                .attr("action") shouldBe routes.YearToDateLiabilityController
                .taxDueSubmit()
                .url
            }
          )
        }

        "the section is incomplete and a calculation hasn't already been done" in {
          val answers = IncompleteCalculatedYTDAnswers(
            Some(AmountInPence.zero),
            None,
            Some(true),
            None,
            None,
            None
          )

          val calculatedTaxDue = sample[CalculatedTaxDue]

          test(
            answers,
            sample[SubscribedDetails].copy(name = Right(sample[IndividualName])), {
              case (fillingOutReturn, draftReturn) =>
                mockCalculationService(calculateRequest(AmountInPence.zero, AmountInPence.zero, isATrust = false))(
                  Right(calculatedTaxDue)
                )
                mockStoreSession(
                  SessionData.empty.copy(
                    journeyStatus = Some(
                      fillingOutReturn.copy(draftReturn = draftReturn.copy(
                        yearToDateLiabilityAnswers = Some(
                          answers.copy(calculatedTaxDue = Some(calculatedTaxDue))
                        )
                      )
                      )
                    )
                  )
                )(Right(()))
            },
            routes.YearToDateLiabilityController.hasEstimatedDetails()
          )
        }

        "the section is incomplete and a calculation has already been done" in {
          val answers = IncompleteCalculatedYTDAnswers(
            Some(AmountInPence.zero),
            None,
            Some(true),
            Some(sample[CalculatedTaxDue]),
            None,
            None
          )

          test(
            answers,
            sample[SubscribedDetails].copy(name = Left(sample[TrustName])),
            { case (_, _) => () },
            routes.YearToDateLiabilityController.hasEstimatedDetails()
          )
        }

        "the section is complete" in {
          val answers = sample[CompleteCalculatedYTDAnswers]
          test(
            answers,
            sample[SubscribedDetails],
            { case (_, _) => () },
            routes.YearToDateLiabilityController.checkYourAnswers()
          )
        }

      }

    }

    "handling submitted answers to the tax due page for a calculated journey" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.taxDueSubmit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      behave like redirectToStartBehaviour(() => performAction())

      behave like noEstimatedIncomeBehaviour(() => performAction())

      behave like incompleteOtherJourneysBehaviour(() => performAction())

      behave like redirectWhenNotSingleDisposalCalculatedJourneyBehaviour(() => performAction())

      {
        val oldAnswers = sample[IncompleteCalculatedYTDAnswers].copy(
          estimatedIncome     = Some(AmountInPence(0L)),
          calculatedTaxDue    = Some(sample[CalculatedTaxDue]),
          personalAllowance   = None,
          hasEstimatedDetails = Some(true),
          mandatoryEvidence   = None
        )
        val draftReturn = singleDispsaslDraftReturnWithCompleteJourneys(
          Some(oldAnswers),
          sample[DisposalDate],
          completeReliefDetailsAnswersWithNoOtherReliefs
        )
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
              sessionWithSingleDisposalState(
                IncompleteCalculatedYTDAnswers.empty.copy(
                  estimatedIncome = Some(AmountInPence(1L))
                ),
                sample[DisposalDate]
              )._1
            )
          }

          checkIsRedirect(performAction(), routes.YearToDateLiabilityController.checkYourAnswers())
        }

        "the user hasn't verified whether or not any details given in the return were estimates" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithSingleDisposalState(
                IncompleteCalculatedYTDAnswers.empty.copy(
                  estimatedIncome   = Some(AmountInPence(1L)),
                  personalAllowance = Some(AmountInPence.zero)
                ),
                sample[DisposalDate]
              )._1
            )
          }

          checkIsRedirect(performAction(), routes.YearToDateLiabilityController.checkYourAnswers())
        }

        "there is no calculated tax due in session" in {
          val answers = sample[IncompleteCalculatedYTDAnswers].copy(
            estimatedIncome     = Some(AmountInPence(0L)),
            hasEstimatedDetails = Some(true),
            calculatedTaxDue    = None,
            personalAllowance   = None,
            mandatoryEvidence   = None
          )
          val draftReturn = singleDispsaslDraftReturnWithCompleteJourneys(
            Some(answers),
            sample[DisposalDate],
            completeReliefDetailsAnswersWithNoOtherReliefs
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(sample[FillingOutReturn].copy(draftReturn = draftReturn))
              )
            )
          }

          checkIsRedirect(performAction(), routes.YearToDateLiabilityController.checkYourAnswers())
        }

      }

      "show a form error" when {
        val draftReturn =
          singleDispsaslDraftReturnWithCompleteJourneys(
            Some(sample[CompleteCalculatedYTDAnswers]),
            sample[DisposalDate],
            completeReliefDetailsAnswersWithNoOtherReliefs
          )

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

            val answers = IncompleteCalculatedYTDAnswers(
              Some(AmountInPence(1L)),
              Some(AmountInPence(2L)),
              Some(true),
              Some(sample[CalculatedTaxDue]),
              Some(AmountInPence(123L)),
              Some(sample[String])
            )
            val draftReturn = singleDispsaslDraftReturnWithCompleteJourneys(
              Some(answers),
              disposalDate,
              completeReliefDetailsAnswersWithNoOtherReliefs
            )

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

            val answers = CompleteCalculatedYTDAnswers(
              AmountInPence(1L),
              Some(AmountInPence(2L)),
              false,
              setTaxDue(sample[CalculatedTaxDue], AmountInPence(100L)),
              AmountInPence(1L),
              Some(sample[String])
            )
            val draftReturn = singleDispsaslDraftReturnWithCompleteJourneys(
              Some(answers),
              disposalDate,
              completeReliefDetailsAnswersWithNoOtherReliefs
            )

            val updatedDraftReturn = draftReturn.copy(
              yearToDateLiabilityAnswers = Some(
                IncompleteCalculatedYTDAnswers(
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

    "handling requests to display the check you answers page" when {

      def performAction(): Future[Result] = controller.checkYourAnswers()(FakeRequest())

      "the user is on a calculated journey" must {

        val completeAnswers = CompleteCalculatedYTDAnswers(
          AmountInPence(1L),
          Some(AmountInPence(2L)),
          sample[Boolean],
          setTaxDue(sample[CalculatedTaxDue], AmountInPence(3L)),
          AmountInPence(4L),
          Some(sample[String])
        )

        val allQuestionAnswered = IncompleteCalculatedYTDAnswers(
          Some(completeAnswers.estimatedIncome),
          completeAnswers.personalAllowance,
          Some(completeAnswers.hasEstimatedDetails),
          Some(completeAnswers.calculatedTaxDue),
          Some(completeAnswers.taxDue),
          completeAnswers.mandatoryEvidence
        )

        def testRedirectWhenIncompleteAnswers(
          answers: IncompleteCalculatedYTDAnswers,
          expectedRedirect: Call
        ): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionWithSingleDisposalState(answers, sample[DisposalDate])._1)
          }

          checkIsRedirect(performAction(), expectedRedirect)
        }

        def testRedirectWhenIsATrust(
          answers: IncompleteCalculatedYTDAnswers,
          expectedRedirect: Call
        ): Unit = {

          val (sessionData, fillingOutReturn, _) = sessionWithSingleDisposalState(answers, sample[DisposalDate])
          val fillingOutReturnWithTrust = fillingOutReturn.copy(subscribedDetails =
            fillingOutReturn.subscribedDetails.copy(name = Left(sample[TrustName]))
          )
          val sessionDataWithTrust = sessionData.copy(journeyStatus = Some(fillingOutReturnWithTrust))
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionDataWithTrust)
          }

          checkIsRedirect(performAction(), expectedRedirect)
        }

        behave like redirectToStartBehaviour(performAction)

        behave like unsuccessfulUpdateBehaviourForSingleDisposal(
          allQuestionAnswered,
          completeAnswers,
          () => performAction()
        )

        "redirect to the estimated income page" when {

          "that question has not been answered yet" in {
            testRedirectWhenIncompleteAnswers(
              allQuestionAnswered.copy(estimatedIncome = None),
              routes.YearToDateLiabilityController.estimatedIncome()
            )
          }

        }

        "do NOT redirect to the estimated income page" when {
          "the user is a Trust" in {
            testRedirectWhenIsATrust(
              allQuestionAnswered.copy(estimatedIncome = None, hasEstimatedDetails = None),
              routes.YearToDateLiabilityController.hasEstimatedDetails()
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
              routes.YearToDateLiabilityController.personalAllowance()
            )
          }

        }

        "do NOT redirect to the personal allowance page" when {
          "the user is a Trust" in {
            testRedirectWhenIsATrust(
              allQuestionAnswered.copy(personalAllowance = None, hasEstimatedDetails = None),
              routes.YearToDateLiabilityController.hasEstimatedDetails()
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
              routes.YearToDateLiabilityController.hasEstimatedDetails()
            )
          }

        }

        "redirect to the tax due page" when {

          "that question has not been answered yet" in {
            testRedirectWhenIncompleteAnswers(
              allQuestionAnswered.copy(
                taxDue = None
              ),
              routes.YearToDateLiabilityController.taxDue()
            )
          }

          "there is no calulated tax due" in {
            testRedirectWhenIncompleteAnswers(
              allQuestionAnswered.copy(
                calculatedTaxDue = None
              ),
              routes.YearToDateLiabilityController.taxDue()
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
              routes.YearToDateLiabilityController.uploadMandatoryEvidence()
            )
          }
        }

        "show the page" when {

          "the section is complete" in {
            forAll { completeAnswers: CompleteCalculatedYTDAnswers =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(sessionWithSingleDisposalState(completeAnswers, sample[DisposalDate])._1)
              }

              checkPageIsDisplayed(
                performAction(),
                messageFromMessageKey("ytdLiability.cya.title"), { doc =>
                  validateCalculatedYearToDateLiabilityPage(completeAnswers, doc)
                }
              )
            }
          }

          "the section has just been completed and all updates are successful" in {
            val (session, journey, draftReturn) =
              sessionWithSingleDisposalState(allQuestionAnswered, sample[DisposalDate])
            val updatedDraftReturn = draftReturn.copy(yearToDateLiabilityAnswers = Some(completeAnswers))
            val updatedSession = session.copy(journeyStatus = Some(
              journey.copy(draftReturn = updatedDraftReturn)
            )
            )

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockStoreDraftReturn(
                updatedDraftReturn,
                journey.subscribedDetails.cgtReference,
                journey.agentReferenceNumber
              )(Right(()))
              mockStoreSession(updatedSession)(Right(()))
            }

            checkPageIsDisplayed(
              performAction(),
              messageFromMessageKey("ytdLiability.cya.title")
            )
          }

        }

      }

      "the user is on a non-calculated journey" must {

        val completeAnswers = CompleteNonCalculatedYTDAnswers(
          AmountInPence(1L),
          true,
          AmountInPence(2L)
        )

        val allQuestionAnswered = IncompleteNonCalculatedYTDAnswers(
          Some(completeAnswers.taxableGainOrLoss),
          Some(completeAnswers.hasEstimatedDetails),
          Some(completeAnswers.taxDue)
        )

        val (session, journey, draftReturn) = sessionWithMultipleDisposalsState(allQuestionAnswered)
        val updatedDraftReturn              = draftReturn.copy(yearToDateLiabilityAnswers = Some(completeAnswers))
        val updatedJourney                  = journey.copy(draftReturn = updatedDraftReturn)
        val updatedSession                  = session.copy(journeyStatus = Some(updatedJourney))

        def testRedirectWhenIncompleteAnswers(
          answers: IncompleteNonCalculatedYTDAnswers,
          expectedRedirect: Call
        ): Unit = {

          val (sessionData, fillingOutReturn, _) = sessionWithMultipleDisposalsState(answers)
          val fillingOutReturnWithIndividual = fillingOutReturn.copy(subscribedDetails =
            fillingOutReturn.subscribedDetails.copy(name = Right(sample[IndividualName]))
          )
          val sessionDataWithIndividual = sessionData.copy(journeyStatus = Some(fillingOutReturnWithIndividual))
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionDataWithIndividual)
          }

          checkIsRedirect(performAction(), expectedRedirect)
        }

        def testRedirectWhenIsATrust(
          answers: IncompleteNonCalculatedYTDAnswers,
          expectedRedirect: Call
        ): Unit = {

          val (sessionData, fillingOutReturn, _) = sessionWithMultipleDisposalsState(answers)
          val fillingOutReturnWithTrust = fillingOutReturn.copy(subscribedDetails =
            fillingOutReturn.subscribedDetails.copy(name = Left(sample[TrustName]))
          )
          val sessionDataWithTrust = sessionData.copy(journeyStatus = Some(fillingOutReturnWithTrust))
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionDataWithTrust)
          }

          checkIsRedirect(performAction(), expectedRedirect)
        }

        behave like unsuccessfulUpdateBehaviourForSingleDisposal(
          allQuestionAnswered,
          completeAnswers,
          () => performAction()
        )

        "redirect to the taxable income page" when {

          "that question has not been answered yet" in {
            testRedirectWhenIncompleteAnswers(
              allQuestionAnswered.copy(taxableGainOrLoss = None),
              routes.YearToDateLiabilityController.taxableGainOrLoss()
            )

          }

        }

        "redirect to the has estimated details page" when {

          "that question has not been answered yet" in {
            testRedirectWhenIncompleteAnswers(
              allQuestionAnswered.copy(hasEstimatedDetails = None),
              routes.YearToDateLiabilityController.hasEstimatedDetails()
            )

          }
          "the user IS a trust" in {
            testRedirectWhenIsATrust(
              allQuestionAnswered.copy(hasEstimatedDetails = None),
              routes.YearToDateLiabilityController.hasEstimatedDetails()
            )
          }
        }

        "redirect to the non-calculated enter tax due page" when {

          "that question has not been answered yet" in {
            testRedirectWhenIncompleteAnswers(
              allQuestionAnswered.copy(taxDue = None),
              routes.YearToDateLiabilityController.nonCalculatedEnterTaxDue()
            )

          }

        }

        "display the page" when {

          def testPageIsDisplayed(result: Future[Result]): Unit =
            checkPageIsDisplayed(
              result,
              messageFromMessageKey("ytdLiability.cya.title"), { doc =>
                validateNonCalculatedYearToDateLiabilityPage(completeAnswers, doc)
              }
            )

          "the user has just answered all the questions in the section and all updates are successful" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockStoreDraftReturn(
                updatedDraftReturn,
                journey.subscribedDetails.cgtReference,
                journey.agentReferenceNumber
              )(Right(()))
              mockStoreSession(updatedSession)(Right(()))
            }

            testPageIsDisplayed(performAction())
          }

          "the user has already completed the section" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(updatedSession)
            }

            testPageIsDisplayed(performAction())
          }

        }

      }

    }

    "handling requests to display the upload mandatory evidence page" must {

      def performAction(): Future[Result] = controller.uploadMandatoryEvidence()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      behave like commonUploadMandatoryEvidenceBehaviour(performAction)

      behave like redirectWhenNotSingleDisposalCalculatedJourneyBehaviour(performAction)

      "display the page" when {

        def test(
          answers: YearToDateLiabilityAnswers,
          backLink: Call
        ): Unit = {
          val draftReturn = singleDispsaslDraftReturnWithCompleteJourneys(
            Some(answers),
            sample[DisposalDate],
            completeReliefDetailsAnswersWithNoOtherReliefs
          )
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
                .attr("action") shouldBe routes.YearToDateLiabilityController
                .uploadMandatoryEvidenceSubmit()
                .url
            }
          )
        }

        val calculatedTaxDue = sample[GainCalculatedTaxDue].copy(amountOfTaxDue = AmountInPence(100L))

        "the section is incomplete" in {
          test(
            IncompleteCalculatedYTDAnswers(
              Some(AmountInPence.zero),
              None,
              Some(true),
              Some(calculatedTaxDue),
              Some(AmountInPence(200L)),
              None
            ),
            routes.YearToDateLiabilityController.taxDue()
          )
        }

        "the section is complete" in {
          test(
            sample[CompleteCalculatedYTDAnswers].copy(
              calculatedTaxDue = calculatedTaxDue,
              taxDue           = AmountInPence(200L)
            ),
            routes.YearToDateLiabilityController.checkYourAnswers()
          )
        }

      }

    }

    "handling submitted files from the upload mandatory evidence page" must {

      def performAction(): Future[Result] = controller.uploadMandatoryEvidenceSubmit()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      behave like commonUploadMandatoryEvidenceBehaviour(performAction)

      behave like redirectWhenNotSingleDisposalCalculatedJourneyBehaviour(performAction)

    }

    "handling submits from the check you answers page" must {

      def performAction(): Future[Result] = controller.checkYourAnswersSubmit()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      "redirect to the task list page" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            sessionWithSingleDisposalState(sample[CompleteCalculatedYTDAnswers], sample[DisposalDate])._1
          )
        }

        checkIsRedirect(performAction(), returns.routes.TaskListController.taskList())
      }

    }

    "handling requests to display the taxable gain or net loss page" must {

      def performAction(): Future[Result] = controller.taxableGainOrLoss()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      behave like redirectWhenNotNonCalculatedJourneyBehaviour(performAction)

      "display the page" when {

        def test(
          session: SessionData,
          expectedBackLink: Call,
          testPage: Document => Unit = _ => ()
        ): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(
              "taxableGainOrLoss.title"
            ), { doc =>
              doc.select("#back").attr("href") shouldBe expectedBackLink.url
              doc
                .select("#content > article > form")
                .attr("action") shouldBe routes.YearToDateLiabilityController
                .taxableGainOrLossSubmit()
                .url
              testPage(doc)
            }
          )
        }

        "the user has not started this section yet and they are on a multiple disposals journey" in {
          test(
            sessionWithMultipleDisposalsState(None)._1,
            returns.routes.TaskListController.taskList()
          )
        }

        "the user has not started this section yet and they are on a single disposal journey and " +
          "have selected to use other reliefs" in {
          test(
            sessionWithSingleDisposalState(
              None,
              Some(sample[DisposalDate]),
              Some(
                sample[CompleteReliefDetailsAnswers].copy(
                  otherReliefs = Some(sample[OtherReliefsOption.OtherReliefs])
                )
              )
            )._1,
            returns.routes.TaskListController.taskList()
          )
        }

        "the user has already started this uncalculated section but have not completed it yet" in {
          test(
            sessionWithMultipleDisposalsState(
              IncompleteNonCalculatedYTDAnswers.empty.copy(taxableGainOrLoss = Some(
                AmountInPence(-100L)
              )
              )
            )._1,
            returns.routes.TaskListController.taskList(), { doc =>
              doc.select("#taxableGainOrLoss-1").attr("checked") shouldBe "checked"
              doc.select("#netLoss").attr("value")               shouldBe "1"
            }
          )
        }

        "the user has completed this uncalculated section" in {
          test(
            sessionWithMultipleDisposalsState(
              sample[CompleteNonCalculatedYTDAnswers].copy(taxableGainOrLoss = AmountInPence(0L))
            )._1,
            routes.YearToDateLiabilityController.checkYourAnswers(), { doc =>
              doc.select("#taxableGainOrLoss-2").attr("checked") shouldBe "checked"
            }
          )

        }

      }

    }

    "handling submits on the taxable gain or net loss page" must {

      def performAction(formData: (String, String)*): Future[Result] =
        controller.taxableGainOrLossSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*))

      behave like redirectToStartBehaviour(() => performAction())

      behave like redirectWhenNotNonCalculatedJourneyBehaviour(() => performAction())

      {
        val (_, _, draftReturn) = sessionWithMultipleDisposalsState(None)

        behave like unsuccessfulUpdateBehaviour(
          draftReturn,
          draftReturn.copy(yearToDateLiabilityAnswers = Some(
            IncompleteNonCalculatedYTDAnswers.empty.copy(
              taxableGainOrLoss = Some(AmountInPence(101L))
            )
          )
          ),
          () =>
            performAction(
              "taxableGainOrLoss" -> "0",
              "taxableGain"       -> "1.01"
            )
        )
      }

      "show a form error" when {

        val currentSession = sessionWithMultipleDisposalsState(None)._1

        def test(data: (String, String)*)(expectedErrorKey: String): Unit =
          testFormError(data: _*)(
            expectedErrorKey
          )("taxableGainOrLoss.title")(performAction, currentSession)

        "no option is selected" in {
          test()("taxableGainOrLoss.error.required")
        }

        "the amount of gain is invalid" in {
          AmountOfMoneyErrorScenarios.amountOfMoneyErrorScenarios("taxableGain").foreach { scenario =>
            withClue(s"For $scenario: ") {
              val data = ("taxableGainOrLoss" -> "0") :: scenario.formData
              test(data: _*)(scenario.expectedErrorMessageKey)
            }
          }
        }

        "the amount of gain is zero" in {
          test(
            "taxableGainOrLoss" -> "0",
            "taxableGain"       -> "0"
          )("taxableGain.error.tooSmall")
        }

        "the amount of loss is invalid" in {
          AmountOfMoneyErrorScenarios.amountOfMoneyErrorScenarios("netLoss").foreach { scenario =>
            withClue(s"For $scenario: ") {
              val data = ("taxableGainOrLoss" -> "1") :: scenario.formData
              test(data: _*)(scenario.expectedErrorMessageKey)
            }
          }
        }

        "the amount of loss is zero" in {
          test(
            "taxableGainOrLoss" -> "1",
            "netLoss"           -> "0"
          )("netLoss.error.tooSmall")
        }

      }

      "redirect to the check your answers page" when {

        "all updates are successful and" when {

          "the section had not been started yet" in {
            val newAmount = AmountInPence(3000L)

            testSuccessfulUpdatesAfterSubmitWithMultipleDisposals(
              performAction(
                "taxableGainOrLoss" -> "0",
                "taxableGain"       -> "30"
              ),
              None,
              IncompleteNonCalculatedYTDAnswers.empty.copy(taxableGainOrLoss = Some(newAmount))
            )
          }

          "the section had been started but not completed" in {
            val newAmount = AmountInPence(-3000L)

            testSuccessfulUpdatesAfterSubmitWithSingleDisposal(
              performAction(
                "taxableGainOrLoss" -> "1",
                "netLoss"           -> "30"
              ),
              IncompleteNonCalculatedYTDAnswers.empty.copy(taxableGainOrLoss = Some(AmountInPence(2L))),
              IncompleteNonCalculatedYTDAnswers.empty.copy(taxableGainOrLoss = Some(newAmount)),
              sample[CompleteReliefDetailsAnswers].copy(otherReliefs         = Some(sample[OtherReliefsOption.OtherReliefs]))
            )
          }

          "the section was complete" in {
            val newAmount = AmountInPence(0L)
            val answers =
              sample[CompleteNonCalculatedYTDAnswers].copy(taxableGainOrLoss = AmountInPence(1L))
            val newAnswers = IncompleteNonCalculatedYTDAnswers(Some(newAmount), None, None)
            testSuccessfulUpdatesAfterSubmitWithMultipleDisposals(
              performAction(
                "taxableGainOrLoss" -> "2"
              ),
              answers,
              newAnswers
            )
          }

        }

      }

      "not do any updates" when {

        "the answer supplied is the same as one already stored" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithMultipleDisposalsState(
                sample[CompleteNonCalculatedYTDAnswers].copy(taxableGainOrLoss = AmountInPence.zero)
              )._1
            )
          }

          checkIsRedirect(
            performAction("taxableGainOrLoss" -> "2"),
            routes.YearToDateLiabilityController.checkYourAnswers()
          )
        }

      }

    }

    "handling requests to display the non calculated enter tax due page" must {

      def performAction(): Future[Result] = controller.nonCalculatedEnterTaxDue()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      behave like redirectWhenNotNonCalculatedJourneyBehaviour(performAction)

      "redirect to the has estimated details page" when {

        "the question has not been answered yet" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithMultipleDisposalsState(IncompleteNonCalculatedYTDAnswers.empty)._1
            )
          }

          checkIsRedirect(performAction(), routes.YearToDateLiabilityController.hasEstimatedDetails())
        }

      }

      "display the page" when {

        def test(
          session: SessionData,
          expectedBackLink: Call,
          testPage: Document => Unit = _ => ()
        ): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(
              "nonCalculatedTaxDue.title"
            ), { doc =>
              doc.select("#back").attr("href") shouldBe expectedBackLink.url
              doc
                .select("#content > article > form")
                .attr("action") shouldBe routes.YearToDateLiabilityController
                .nonCalculatedEnterTaxDueSubmit()
                .url
              testPage(doc)
            }
          )
        }

        "the user has not answered this question before" in {
          test(
            sessionWithMultipleDisposalsState(
              IncompleteNonCalculatedYTDAnswers.empty.copy(hasEstimatedDetails = Some(true))
            )._1,
            routes.YearToDateLiabilityController.hasEstimatedDetails()
          )
        }

        "the user has answered this question before" in {
          test(
            sessionWithSingleDisposalState(
              Some(sample[CompleteNonCalculatedYTDAnswers]),
              Some(sample[DisposalDate]),
              Some(
                sample[CompleteReliefDetailsAnswers].copy(otherReliefs = Some(sample[OtherReliefsOption.OtherReliefs]))
              )
            )._1,
            routes.YearToDateLiabilityController.checkYourAnswers()
          )
        }
      }
    }

    "handling submits on the non calculated enter tax due page" must {

      def performAction(formData: (String, String)*): Future[Result] =
        controller.nonCalculatedEnterTaxDueSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*))

      behave like redirectToStartBehaviour(() => performAction())

      behave like redirectWhenNotNonCalculatedJourneyBehaviour(() => performAction())

      {
        val answers =
          IncompleteNonCalculatedYTDAnswers.empty.copy(
            taxableGainOrLoss   = Some(AmountInPence.zero),
            hasEstimatedDetails = Some(true)
          )

        val (_, _, draftReturn) = sessionWithMultipleDisposalsState(answers)

        behave like unsuccessfulUpdateBehaviour(
          draftReturn,
          draftReturn.copy(yearToDateLiabilityAnswers = Some(
            answers.copy(taxDue = Some(AmountInPence(100L)))
          )
          ),
          () =>
            performAction(
              "nonCalculatedTaxDue" -> "1"
            )
        )
      }

      "show a form error" when {

        val currentSession = sessionWithMultipleDisposalsState(
          sample[CompleteNonCalculatedYTDAnswers]
        )._1

        def test(data: (String, String)*)(expectedErrorKey: String): Unit =
          testFormError(data: _*)(
            expectedErrorKey
          )("nonCalculatedTaxDue.title")(performAction, currentSession)

        "the value submitted is invalid" in {
          AmountOfMoneyErrorScenarios.amountOfMoneyErrorScenarios("nonCalculatedTaxDue").foreach { scenario =>
            withClue(s"For $scenario: ") {
              test(scenario.formData: _*)(scenario.expectedErrorMessageKey)
            }
          }
        }

      }

      "redirect to the task list page" when {

        "all updates are successful and" when {

          "the section had been started but not completed" in {
            val newAmount = AmountInPence(101L)
            val answers = IncompleteNonCalculatedYTDAnswers.empty.copy(
              taxableGainOrLoss   = Some(AmountInPence(2L)),
              hasEstimatedDetails = Some(true)
            )
            testSuccessfulUpdatesAfterSubmitWithSingleDisposal(
              performAction(
                "nonCalculatedTaxDue" -> "1.01"
              ),
              answers,
              answers.copy(taxDue                                    = Some(newAmount)),
              sample[CompleteReliefDetailsAnswers].copy(otherReliefs = Some(sample[OtherReliefsOption.OtherReliefs]))
            )
          }

          "the section was complete" in {
            val newAmount = AmountInPence(0L)
            val answers =
              sample[CompleteNonCalculatedYTDAnswers].copy(taxDue = AmountInPence(1L))

            testSuccessfulUpdatesAfterSubmitWithMultipleDisposals(
              performAction(
                "nonCalculatedTaxDue" -> "0"
              ),
              answers,
              answers.copy(taxDue = newAmount)
            )
          }

        }

      }

      "not do any updates" when {

        "the answer supplied is the same as one already stored" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithMultipleDisposalsState(
                sample[CompleteNonCalculatedYTDAnswers].copy(taxDue = AmountInPence.zero)
              )._1
            )
          }

          checkIsRedirect(
            performAction("nonCalculatedTaxDue" -> "0"),
            routes.YearToDateLiabilityController.checkYourAnswers()
          )
        }

      }

    }

  }

  def noDisposalDateBehaviour(performAction: () => Future[Result]): Unit =
    "redirect to the task list page" when {
      "no disposal date can be found" in {

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            sessionWithSingleDisposalState(Some(sample[CompleteCalculatedYTDAnswers]), None)._1
          )
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
            sessionWithSingleDisposalState(
              sample[IncompleteCalculatedYTDAnswers].copy(
                estimatedIncome     = None,
                personalAllowance   = Some(sample[AmountInPence]),
                hasEstimatedDetails = Some(sample[Boolean])
              ),
              sample[DisposalDate]
            )._1
          )
        }

        checkIsRedirect(performAction(), routes.YearToDateLiabilityController.checkYourAnswers())
      }

    }

  def incompleteOtherJourneysBehaviour(performAction: () => Future[Result]): Unit = {
    val draftReturn = singleDispsaslDraftReturnWithCompleteJourneys(
      Some(
        sample[CompleteCalculatedYTDAnswers]
          .copy(estimatedIncome = AmountInPence.zero, personalAllowance = None)
      ),
      sample[DisposalDate],
      completeReliefDetailsAnswersWithNoOtherReliefs
    )

    "redirect to the task list page" when {

      def test(draftReturn: DraftSingleDisposalReturn): Unit = {
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

  def redirectWhenNotNonCalculatedJourneyBehaviour(
    performAction: () => Future[Result]
  ): Unit =
    "redirect to the check your answers endpoint" when {

      "the user has already started a calculated year to date liability journey" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            sessionWithSingleDisposalState(
              sample[CompleteCalculatedYTDAnswers],
              sample[DisposalDate]
            )._1
          )
        }

        checkIsRedirect(performAction(), routes.YearToDateLiabilityController.checkYourAnswers())
      }

      "the user has not started this section yet but they are on a single disposal journey and have " +
        "not chosen other reliefs" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            sessionWithSingleDisposalState(
              None,
              Some(sample[DisposalDate]),
              Some(sample[CompleteReliefDetailsAnswers].copy(otherReliefs = Some(OtherReliefsOption.NoOtherReliefs)))
            )._1
          )
        }

        checkIsRedirect(performAction(), routes.YearToDateLiabilityController.checkYourAnswers())
      }

    }

  def redirectWhenNotSingleDisposalCalculatedJourneyBehaviour(
    performAction: () => Future[Result]
  ): Unit =
    "redirect to the check your answers endpoint" when {

      "the user has started this section and is on a single disposal non calculated journey" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            sessionWithSingleDisposalState(
              sample[CompleteNonCalculatedYTDAnswers],
              sample[DisposalDate]
            )._1
          )
        }

        checkIsRedirect(performAction(), routes.YearToDateLiabilityController.checkYourAnswers())
      }

      "the user has not started this section and they are on a single disposal journey and have " +
        "chosen to use other reliefs" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            sessionWithSingleDisposalState(
              Some(sample[CompleteNonCalculatedYTDAnswers]),
              Some(sample[DisposalDate]),
              Some(
                sample[CompleteReliefDetailsAnswers].copy(otherReliefs = Some(sample[OtherReliefsOption.OtherReliefs]))
              )
            )._1
          )
        }

        checkIsRedirect(performAction(), routes.YearToDateLiabilityController.checkYourAnswers())
      }

      "the user is on a multiple disposals journey" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            sessionWithMultipleDisposalsState(None)._1
          )
        }

        checkIsRedirect(performAction(), routes.YearToDateLiabilityController.checkYourAnswers())
      }

    }

  def unsuccessfulUpdateBehaviourForSingleDisposal(
    currentAnswers: YearToDateLiabilityAnswers,
    updatedAnswers: YearToDateLiabilityAnswers,
    result: () => Future[Result]
  ): Unit = {
    val (_, _, draftReturn) = sessionWithSingleDisposalState(
      currentAnswers,
      sample[DisposalDate].copy(taxYear = sample[TaxYear].copy(personalAllowance = AmountInPence(Long.MaxValue)))
    )
    unsuccessfulUpdateBehaviour(
      draftReturn,
      draftReturn.copy(yearToDateLiabilityAnswers = Some(updatedAnswers)),
      result
    )
  }

  def unsuccessfulUpdateBehaviour(
    currentDraftReturn: DraftReturn,
    updatedDraftReturn: DraftReturn,
    result: () => Future[Result]
  ): Unit = {
    val journey = sample[FillingOutReturn].copy(
      subscribedDetails = sample[SubscribedDetails].copy(name = Right(sample[IndividualName])),
      draftReturn       = currentDraftReturn
    )
    val session = SessionData.empty.copy(journeyStatus = Some(journey))

    val updatedSession = SessionData.empty.copy(journeyStatus = Some(
      journey.copy(draftReturn = updatedDraftReturn)
    )
    )

    "show an error page" when {

      "there is an error updating the draft return" in {

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockStoreDraftReturn(
            updatedDraftReturn,
            journey.subscribedDetails.cgtReference,
            journey.agentReferenceNumber
          )(Left(Error("")))
        }

        checkIsTechnicalErrorPage(result())
      }

      "there is an error updating the session data" in {

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockStoreDraftReturn(
            updatedDraftReturn,
            journey.subscribedDetails.cgtReference,
            journey.agentReferenceNumber
          )(Right(()))
          mockStoreSession(updatedSession)(Left(Error("")))
        }

        checkIsTechnicalErrorPage(result())
      }

    }

  }

  def testFormError(
    data: (String, String)*
  )(expectedErrorMessageKey: String, errorArgs: Seq[String] = Nil)(pageTitleKey: String, titleArgs: String*)(
    performAction: Seq[(String, String)] => Future[Result],
    currentSession: SessionData = sessionWithSingleDisposalState(
      sample[CompleteCalculatedYTDAnswers],
      sample[DisposalDate]
    )._1
  ): Unit = {
    inSequence {
      mockAuthWithNoRetrievals()
      mockGetSession(currentSession)
    }
    checkPageIsDisplayed(
      performAction(data),
      messageFromMessageKey(pageTitleKey, titleArgs: _*), { doc =>
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
    val journey = sample[FillingOutReturn].copy(
      draftReturn       = oldDraftReturn,
      subscribedDetails = sample[SubscribedDetails].copy(name = Right(sample[IndividualName]))
    )

    val session = SessionData.empty.copy(journeyStatus = Some(journey))

    inSequence {
      mockAuthWithNoRetrievals()
      mockGetSession(session)
      mockStoreDraftReturn(updatedDraftReturn, journey.subscribedDetails.cgtReference, journey.agentReferenceNumber)(
        Right(())
      )
      mockStoreSession(
        session
          .copy(journeyStatus = Some(journey.copy(draftReturn = updatedDraftReturn)))
      )(Right(()))
    }

    checkIsRedirect(result, routes.YearToDateLiabilityController.checkYourAnswers())
  }

  def testSuccessfulUpdatesAfterSubmitWithSingleDisposal(
    result: => Future[Result],
    oldAnswers: Option[YearToDateLiabilityAnswers],
    newAnswers: YearToDateLiabilityAnswers,
    reliefDetailsAnswers: ReliefDetailsAnswers,
    disposalDate: DisposalDate
  ): Unit = {
    val draftReturn = singleDispsaslDraftReturnWithCompleteJourneys(oldAnswers, disposalDate, reliefDetailsAnswers)
    val newDraftReturn = draftReturn.copy(
      yearToDateLiabilityAnswers = Some(newAnswers),
      reliefDetailsAnswers       = Some(reliefDetailsAnswers)
    )
    testSuccessfulUpdatesAfterSubmit(result, draftReturn, newDraftReturn)
  }

  def testSuccessfulUpdatesAfterSubmitWithMultipleDisposals(
    result: => Future[Result],
    oldAnswers: Option[YearToDateLiabilityAnswers],
    newAnswers: YearToDateLiabilityAnswers
  ): Unit = {
    val draftReturn    = sessionWithMultipleDisposalsState(oldAnswers)._3
    val newDraftReturn = draftReturn.copy(yearToDateLiabilityAnswers = Some(newAnswers))
    testSuccessfulUpdatesAfterSubmit(result, draftReturn, newDraftReturn)
  }

  def testSuccessfulUpdatesAfterSubmitWithMultipleDisposals(
    result: => Future[Result],
    oldAnswers: YearToDateLiabilityAnswers,
    newAnswers: YearToDateLiabilityAnswers
  ): Unit = {
    val draftReturn    = sessionWithMultipleDisposalsState(oldAnswers)._3
    val newDraftReturn = draftReturn.copy(yearToDateLiabilityAnswers = Some(newAnswers))
    testSuccessfulUpdatesAfterSubmit(result, draftReturn, newDraftReturn)
  }

  def testSuccessfulUpdatesAfterSubmitWithSingleDisposal(
    result: => Future[Result],
    oldAnswers: YearToDateLiabilityAnswers,
    newAnswers: YearToDateLiabilityAnswers,
    reliefDetailsAnswers: ReliefDetailsAnswers = sample[CompleteReliefDetailsAnswers],
    disposalDate: DisposalDate                 = sample[DisposalDate]
  ): Unit =
    testSuccessfulUpdatesAfterSubmitWithSingleDisposal(
      result,
      Some(oldAnswers),
      newAnswers,
      reliefDetailsAnswers,
      disposalDate
    )

  def commonUploadMandatoryEvidenceBehaviour(performAction: () => Future[Result]): Unit =
    "redirect to the check your answers page" when {

      val calculatedTaxDue = sample[GainCalculatedTaxDue].copy(amountOfTaxDue = AmountInPence(100L))

      val answers = IncompleteCalculatedYTDAnswers.empty.copy(
        estimatedIncome     = Some(AmountInPence(1L)),
        personalAllowance   = Some(AmountInPence.zero),
        hasEstimatedDetails = Some(false)
      )

      "there is no answer to the tax due question" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionWithSingleDisposalState(answers, sample[DisposalDate])._1)
        }

        checkIsRedirect(performAction(), routes.YearToDateLiabilityController.checkYourAnswers())
      }
      "there is no answer to the tax due but it is equal to the calculated tax due" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            sessionWithSingleDisposalState(
              answers.copy(taxDue = Some(calculatedTaxDue.amountOfTaxDue)),
              sample[DisposalDate]
            )._1
          )
        }

        checkIsRedirect(performAction(), routes.YearToDateLiabilityController.checkYourAnswers())
      }

      "the user hasn't verified whether or not any details given in the return were estimates" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            sessionWithSingleDisposalState(
              answers.copy(hasEstimatedDetails = None),
              sample[DisposalDate]
            )._1
          )
        }

        checkIsRedirect(performAction(), routes.YearToDateLiabilityController.checkYourAnswers())
      }

    }

}

object YearToDateLiabilityControllerSpec extends Matchers {
  def validateCalculatedYearToDateLiabilityPage(
    completeYearToDateLiabilityAnswers: CompleteCalculatedYTDAnswers,
    doc: Document
  ): Unit = {
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

  def validateNonCalculatedYearToDateLiabilityPage(
    answers: CompleteNonCalculatedYTDAnswers,
    doc: Document
  )(implicit messages: MessagesApi, lang: Lang): Unit = {
    if (answers.taxableGainOrLoss < AmountInPence.zero) {
      doc.select("#taxableGainOrLossAnswer-answer").text shouldBe messages("taxableGainOrLoss.loss.label")
      doc.select("#taxableGainOrLossAmount-answer").text shouldBe formatAmountOfMoneyWithPoundSign(
        answers.taxableGainOrLoss.inPounds().abs
      )
    } else if (answers.taxableGainOrLoss > AmountInPence.zero) {
      doc.select("#taxableGainOrLossAnswer-answer").text shouldBe messages("taxableGainOrLoss.gain.label")
      doc.select("#taxableGainOrLossAmount-answer").text shouldBe formatAmountOfMoneyWithPoundSign(
        answers.taxableGainOrLoss.inPounds()
      )
    } else {
      doc.select("#taxableGainOrLossAnswer-answer").text shouldBe messages("taxableGainOrLoss.noLossOrGain.label")
    }

    if (answers.hasEstimatedDetails)
      doc.select("#hasEstimatedDetails-value-answer").text() shouldBe "Yes"
    else
      doc.select("#hasEstimatedDetails-value-answer").text() shouldBe "No"

    doc.select("#nonCalculatedTaxDue-value-answer").text() shouldBe formatAmountOfMoneyWithPoundSign(
      answers.taxDue.inPounds()
    )
  }
}
