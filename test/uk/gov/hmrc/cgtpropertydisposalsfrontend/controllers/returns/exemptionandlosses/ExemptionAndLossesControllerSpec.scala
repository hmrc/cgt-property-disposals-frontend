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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.exemptionandlosses

import org.jsoup.nodes.Document
import org.scalatest.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.http.Status.BAD_REQUEST
import play.api.i18n.{Lang, MessagesApi}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.{Call, Result}
import play.api.test.FakeRequest
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.AmountOfMoneyErrorScenarios.amountOfMoneyErrorScenarios
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.ReturnsServiceSupport
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.exemptionandlosses.ExemptionAndLossesControllerSpec.validateExemptionAndLossesCheckYourAnswersPage
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport, returns}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.FillingOutReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address.UkAddress
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.MoneyUtils.formatAmountOfMoneyWithPoundSign
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.{AmountInPence, MoneyUtils}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.CgtReference
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.{ContactName, IndividualName, TrustName}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.SubscribedDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.email.Email
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ExamplePropertyDetailsAnswers.IncompleteExamplePropertyDetailsAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ExemptionAndLossesAnswers.{CompleteExemptionAndLossesAnswers, IncompleteExemptionAndLossesAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.IncompleteSingleDisposalTriageAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService

import scala.concurrent.Future

class ExemptionAndLossesControllerSpec
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

  lazy val controller    = instanceOf[ExemptionAndLossesController]
  val individualUserType = Right(IndividualName("Hodor", "Hodor"))
  val trustUserType      = Left(TrustName("Littlefinger"))

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  def redirectToStartBehaviour(performAction: () => Future[Result]) =
    redirectToStartWhenInvalidJourney(
      performAction, {
        case _: FillingOutReturn => true
        case _                   => false
      }
    )

  def sessionWithSingleDisposalsState(
    answers: Option[ExemptionAndLossesAnswers],
    disposalDate: Option[DisposalDate],
    userType: Either[TrustName, IndividualName]
  ): (SessionData, FillingOutReturn, DraftSingleDisposalReturn) = {
    val draftReturn =
      sample[DraftSingleDisposalReturn].copy(
        triageAnswers = sample[IncompleteSingleDisposalTriageAnswers].copy(
          disposalDate = disposalDate
        ),
        exemptionAndLossesAnswers = answers
      )

    val subscribe = SubscribedDetails(
      userType,
      sample[Email],
      sample[UkAddress],
      sample[ContactName],
      sample[CgtReference],
      None,
      sample[Boolean]
    )

    val journey = sample[FillingOutReturn].copy(draftReturn = draftReturn, subscribedDetails = subscribe)

    (
      SessionData.empty.copy(journeyStatus = Some(journey)),
      journey,
      draftReturn
    )
  }

  def sessionWithSingleDisposalState(
    answers: ExemptionAndLossesAnswers,
    disposalDate: DisposalDate,
    userType: Either[TrustName, IndividualName]
  ): (SessionData, FillingOutReturn, DraftSingleDisposalReturn) =
    sessionWithSingleDisposalsState(Some(answers), Some(disposalDate), userType)

  def sessionWithMultipleDisposalsState(
    answers: Option[ExemptionAndLossesAnswers],
    disposalDate: Option[DisposalDate],
    userType: Either[TrustName, IndividualName]
  ): (SessionData, FillingOutReturn, DraftMultipleDisposalsReturn) = {
    val draftReturn =
      sample[DraftMultipleDisposalsReturn].copy(
        examplePropertyDetailsAnswers =
          Some(sample[IncompleteExamplePropertyDetailsAnswers].copy(disposalDate = disposalDate)),
        exemptionAndLossesAnswers = answers
      )

    val subscribe = SubscribedDetails(
      userType,
      sample[Email],
      sample[UkAddress],
      sample[ContactName],
      sample[CgtReference],
      None,
      sample[Boolean]
    )
    val journey = sample[FillingOutReturn].copy(draftReturn = draftReturn, subscribedDetails = subscribe)

    (
      SessionData.empty.copy(journeyStatus = Some(journey)),
      journey,
      draftReturn
    )
  }

  def sessionWithMultipleDisposalsState(
    answers: ExemptionAndLossesAnswers,
    disposalDate: DisposalDate,
    userType: Either[TrustName, IndividualName]
  ): (SessionData, FillingOutReturn, DraftMultipleDisposalsReturn) =
    sessionWithMultipleDisposalsState(Some(answers), Some(disposalDate), userType)

  "AcquisitionDetailsController" when {

    "handling requests to display the in year losses page" must {

      def performAction(): Future[Result] = controller.inYearLosses()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      "redirect to the task list page" when {

        "there is no disposal date" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithSingleDisposalsState(
                Some(sample[CompleteExemptionAndLossesAnswers]),
                None,
                individualUserType
              )._1
            )
          }

          checkIsRedirect(performAction(), returns.routes.TaskListController.taskList())
        }

      }

      "display the page" when {

        "the exemption and losses section has not yet been started" in {
          val disposalDate = sample[DisposalDate]
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithSingleDisposalsState(
                None,
                Some(disposalDate),
                individualUserType
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(
              "inYearLosses.title",
              disposalDate.taxYear.startDateInclusive.getYear.toString,
              disposalDate.taxYear.endDateExclusive.getYear.toString
            ), { doc =>
              doc.select("#back").attr("href") shouldBe returns.routes.TaskListController.taskList().url
              doc.select("#content > article > form").attr("action") shouldBe routes.ExemptionAndLossesController
                .inYearLossesSubmit()
                .url
            }
          )
        }

        "the exemption and losses section has been completed" in {
          val disposalDate = sample[DisposalDate]
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithMultipleDisposalsState(
                sample[CompleteExemptionAndLossesAnswers],
                disposalDate,
                individualUserType
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(
              "inYearLosses.title",
              disposalDate.taxYear.startDateInclusive.getYear.toString,
              disposalDate.taxYear.endDateExclusive.getYear.toString
            ), { doc =>
              doc.select("#back").attr("href") shouldBe routes.ExemptionAndLossesController.checkYourAnswers().url
              doc.select("#content > article > form").attr("action") shouldBe routes.ExemptionAndLossesController
                .inYearLossesSubmit()
                .url
            }
          )
        }

        "the amount in the session is zero" in {
          val disposalDate = sample[DisposalDate]
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithSingleDisposalState(
                sample[IncompleteExemptionAndLossesAnswers].copy(
                  inYearLosses = Some(AmountInPence.zero)
                ),
                disposalDate,
                individualUserType
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(
              "inYearLosses.title",
              disposalDate.taxYear.startDateInclusive.getYear.toString,
              disposalDate.taxYear.endDateExclusive.getYear.toString
            ), { doc =>
              doc.select("#inYearLosses-1").attr("checked") shouldBe "checked"
            }
          )
        }

        "the amount in the session is non-zero" in {
          val amountInPence = AmountInPence(1000L)
          val disposalDate  = sample[DisposalDate]
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithMultipleDisposalsState(
                sample[IncompleteExemptionAndLossesAnswers].copy(
                  inYearLosses = Some(amountInPence)
                ),
                disposalDate,
                individualUserType
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(
              "inYearLosses.title",
              disposalDate.taxYear.startDateInclusive.getYear.toString,
              disposalDate.taxYear.endDateExclusive.getYear.toString
            ), { doc =>
              doc.select("#inYearLosses-0").attr("checked")  shouldBe "checked"
              doc.select("#inYearLossesValue").attr("value") shouldBe "10"
            }
          )
        }

      }

    }

    "handling submitted in year losses" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.inYearLossesSubmit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      behave like redirectToStartBehaviour(() => performAction())

      "redirect to the task list page" when {

        "there is no disposal date" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithSingleDisposalsState(
                Some(sample[CompleteExemptionAndLossesAnswers]),
                None,
                individualUserType
              )._1
            )
          }

          checkIsRedirect(performAction(), returns.routes.TaskListController.taskList())
        }

      }

      "show a form error" when {

        val disposalDate = sample[DisposalDate]

        val session = sessionWithMultipleDisposalsState(
          sample[CompleteExemptionAndLossesAnswers],
          disposalDate,
          individualUserType
        )._1

        def test(data: (String, String)*)(expectedErrorKey: String): Unit =
          testFormError(data: _*)(expectedErrorKey)(
            "inYearLosses.title",
            disposalDate.taxYear.startDateInclusive.getYear.toString,
            disposalDate.taxYear.endDateExclusive.getYear.toString
          )(performAction, session)

        "no option has been selected" in {
          test()("inYearLosses.error.required")
        }

        "the option selected is not valid" in {
          test("inYearLosses" -> "2")("inYearLosses.error.invalid")
        }

        "the amount of money is invalid" in {
          amountOfMoneyErrorScenarios("inYearLossesValue").foreach { scenario =>
            withClue(s"For $scenario: ") {
              val data =
                ("inYearLosses" -> "0") :: scenario.formData
              test(data: _*)(scenario.expectedErrorMessageKey)
            }
          }
        }

        "the amount of money is zero" in {
          test("inYearLosses" -> "0", "inYearLossesValue" -> "0")(
            "inYearLossesValue.error.tooSmall"
          )
        }

      }

      "show an error page" when {

        val newAmount = AmountInPence(123L)
        val answers: CompleteExemptionAndLossesAnswers =
          sample[CompleteExemptionAndLossesAnswers].copy(inYearLosses = AmountInPence(newAmount.value + 1L))
        val (session, journey, draftReturn) =
          sessionWithSingleDisposalState(answers, sample[DisposalDate], individualUserType)
        val updatedDraftReturn = draftReturn.copy(exemptionAndLossesAnswers = Some(
          answers.copy(inYearLosses = newAmount)
        )
        )

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

          checkIsTechnicalErrorPage(
            performAction("inYearLosses" -> "0", "inYearLossesValue" -> newAmount.inPounds().toString)
          )
        }

        "there is an error updating the session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(
              updatedDraftReturn,
              journey.subscribedDetails.cgtReference,
              journey.agentReferenceNumber
            )(Right(()))
            mockStoreSession(
              session.copy(
                journeyStatus = Some(journey.copy(draftReturn = updatedDraftReturn))
              )
            )(Left(Error("")))
          }

          checkIsTechnicalErrorPage(
            performAction("inYearLosses" -> "0", "inYearLossesValue" -> newAmount.inPounds().toString)
          )
        }

      }

      "redirect to the task list page" when {

        "all updates are successful and" when {

          "the user selects no and the journey was incomplete" in {
            val newAmount = AmountInPence(3000L)
            val answers =
              sample[IncompleteExemptionAndLossesAnswers].copy(inYearLosses = None)

            testSuccessfulUpdatesAfterSubmit(
              performAction(
                "inYearLosses"      -> "0",
                "inYearLossesValue" -> newAmount.inPounds().toString
              )
            )(answers, answers.copy(inYearLosses = Some(newAmount)))
          }

          "the user selects no and the journey was complete" in {
            val newAmount = AmountInPence(4000L)
            val answers =
              sample[CompleteExemptionAndLossesAnswers].copy(
                inYearLosses = AmountInPence(newAmount.value + 1)
              )

            testSuccessfulUpdatesAfterSubmit(
              performAction(
                "inYearLosses"      -> "0",
                "inYearLossesValue" -> newAmount.inPounds().toString
              )
            )(answers, answers.copy(inYearLosses = newAmount))
          }

          "the user selects yes and submits a valid value and the journey was incomplete" in {
            val answers =
              sample[IncompleteExemptionAndLossesAnswers].copy(inYearLosses = None)

            testSuccessfulUpdatesAfterSubmit(
              performAction(
                "inYearLosses" -> "1"
              )
            )(answers, answers.copy(inYearLosses = Some(AmountInPence.zero)))
          }

          "the user selects yes and submits a valid value and the journey was complete" in {
            val answers =
              sample[CompleteExemptionAndLossesAnswers].copy(inYearLosses = AmountInPence(1L))

            testSuccessfulUpdatesAfterSubmit(
              performAction(
                "inYearLosses" -> "1"
              )
            )(answers, answers.copy(inYearLosses = AmountInPence.zero))
          }

        }

      }

      "not do any updates" when {

        "the value submitted hasn't changed" in {
          val answers =
            sample[CompleteExemptionAndLossesAnswers].copy(inYearLosses = AmountInPence.zero)
          val session = sessionWithSingleDisposalState(answers, sample[DisposalDate], individualUserType)._1

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkIsRedirect(
            performAction("inYearLosses" -> "1"),
            routes.ExemptionAndLossesController.checkYourAnswers()
          )

        }

      }

    }

    "handling requests to display the previous years losses page" must {

      def performAction(): Future[Result] = controller.previousYearsLosses()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      "redirect to the in year losses page" when {

        "that question has not been answered" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithSingleDisposalState(
                sample[IncompleteExemptionAndLossesAnswers].copy(inYearLosses = None),
                sample[DisposalDate],
                individualUserType
              )._1
            )
          }

          checkIsRedirect(performAction(), routes.ExemptionAndLossesController.inYearLosses())
        }

      }

      "display the page" when {

        "the exemption and losses section has been not completed" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithMultipleDisposalsState(
                sample[IncompleteExemptionAndLossesAnswers].copy(inYearLosses = Some(sample[AmountInPence])),
                sample[DisposalDate],
                individualUserType
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(
              "previousYearsLosses.title"
            ), { doc =>
              doc.select("#back").attr("href") shouldBe routes.ExemptionAndLossesController.inYearLosses().url
              doc.select("#content > article > form").attr("action") shouldBe routes.ExemptionAndLossesController
                .previousYearsLossesSubmit()
                .url
            }
          )
        }

        "the exemption and losses section has been completed" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithSingleDisposalState(
                sample[CompleteExemptionAndLossesAnswers],
                sample[DisposalDate],
                individualUserType
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(
              "previousYearsLosses.title"
            ), { doc =>
              doc.select("#back").attr("href") shouldBe routes.ExemptionAndLossesController.checkYourAnswers().url
              doc.select("#content > article > form").attr("action") shouldBe routes.ExemptionAndLossesController
                .previousYearsLossesSubmit()
                .url
            }
          )
        }

        "the amount in the session is zero" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithSingleDisposalState(
                sample[IncompleteExemptionAndLossesAnswers].copy(
                  inYearLosses        = Some(sample[AmountInPence]),
                  previousYearsLosses = Some(AmountInPence.zero)
                ),
                sample[DisposalDate],
                individualUserType
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(
              "previousYearsLosses.title"
            ), { doc =>
              doc.select("#previousYearsLosses-1").attr("checked") shouldBe "checked"
            }
          )
        }

        "the amount in the session is non-zero" in {
          val amountInPence = AmountInPence(1000L)
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithMultipleDisposalsState(
                sample[IncompleteExemptionAndLossesAnswers].copy(
                  inYearLosses        = Some(sample[AmountInPence]),
                  previousYearsLosses = Some(amountInPence)
                ),
                sample[DisposalDate],
                individualUserType
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(
              "previousYearsLosses.title"
            ), { doc =>
              doc.select("#previousYearsLosses-0").attr("checked")  shouldBe "checked"
              doc.select("#previousYearsLossesValue").attr("value") shouldBe "10"
            }
          )
        }

      }

    }

    "handling submitted in previous years losses" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.previousYearsLossesSubmit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      behave like redirectToStartBehaviour(() => performAction())

      "redirect to the in year losses page" when {

        "that question has not been answered" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithMultipleDisposalsState(
                sample[IncompleteExemptionAndLossesAnswers].copy(inYearLosses = None),
                sample[DisposalDate],
                individualUserType
              )._1
            )
          }

          checkIsRedirect(performAction(), routes.ExemptionAndLossesController.inYearLosses())
        }

      }

      "show a form error" when {

        def test(data: (String, String)*)(expectedErrorKey: String): Unit =
          testFormError(data: _*)(expectedErrorKey)("previousYearsLosses.title")(performAction)

        "no option has been selected" in {
          test()("previousYearsLosses.error.required")
        }

        "the option selected is not valid" in {
          test("previousYearsLosses" -> "2")("previousYearsLosses.error.invalid")
        }

        "the amount of money is invalid" in {
          amountOfMoneyErrorScenarios("previousYearsLossesValue").foreach { scenario =>
            withClue(s"For $scenario: ") {
              val data =
                ("previousYearsLosses" -> "0") :: scenario.formData
              test(data: _*)(scenario.expectedErrorMessageKey)
            }
          }
        }

        "the amount of money is zero" in {
          test("previousYearsLosses" -> "0", "previousYearsLossesValue" -> "0")(
            "previousYearsLossesValue.error.tooSmall"
          )
        }

      }

      "show an error page" when {

        val newAmount = AmountInPence(123L)
        val answers: CompleteExemptionAndLossesAnswers =
          sample[CompleteExemptionAndLossesAnswers].copy(previousYearsLosses = AmountInPence(newAmount.value + 1L))
        val (session, journey, draftReturn) =
          sessionWithSingleDisposalState(answers, sample[DisposalDate], individualUserType)
        val updatedDraftReturn = draftReturn.copy(exemptionAndLossesAnswers = Some(
          answers.copy(previousYearsLosses = newAmount)
        )
        )

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

          checkIsTechnicalErrorPage(
            performAction("previousYearsLosses" -> "0", "previousYearsLossesValue" -> newAmount.inPounds().toString)
          )
        }

        "there is an error updating the session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(
              updatedDraftReturn,
              journey.subscribedDetails.cgtReference,
              journey.agentReferenceNumber
            )(Right(()))
            mockStoreSession(
              session.copy(
                journeyStatus = Some(journey.copy(draftReturn = updatedDraftReturn))
              )
            )(Left(Error("")))
          }

          checkIsTechnicalErrorPage(
            performAction("previousYearsLosses" -> "0", "previousYearsLossesValue" -> newAmount.inPounds().toString)
          )
        }

      }

      "redirect to the task list page" when {

        "all updates are successful and" when {

          "the user selects no and the journey was incomplete" in {
            val newAmount = AmountInPence(3000L)
            val answers =
              sample[IncompleteExemptionAndLossesAnswers].copy(
                inYearLosses        = Some(sample[AmountInPence]),
                previousYearsLosses = None
              )

            testSuccessfulUpdatesAfterSubmit(
              performAction(
                "previousYearsLosses"      -> "0",
                "previousYearsLossesValue" -> newAmount.inPounds().toString
              )
            )(answers, answers.copy(previousYearsLosses = Some(newAmount)))
          }

          "the user selects no and the journey was complete" in {
            val newAmount = AmountInPence(4000L)
            val answers =
              sample[CompleteExemptionAndLossesAnswers].copy(
                previousYearsLosses = AmountInPence(newAmount.value + 1)
              )

            testSuccessfulUpdatesAfterSubmit(
              performAction(
                "previousYearsLosses"      -> "0",
                "previousYearsLossesValue" -> newAmount.inPounds().toString
              )
            )(answers, answers.copy(previousYearsLosses = newAmount))
          }

          "the user selects yes and submits a valid value and the journey was incomplete" in {
            val answers =
              sample[IncompleteExemptionAndLossesAnswers].copy(
                inYearLosses        = Some(AmountInPence(1L)),
                previousYearsLosses = None
              )

            testSuccessfulUpdatesAfterSubmit(
              performAction(
                "previousYearsLosses" -> "1"
              )
            )(answers, answers.copy(previousYearsLosses = Some(AmountInPence.zero)))
          }

          "the user selects yes and submits a valid value and the journey was complete" in {
            val answers =
              sample[CompleteExemptionAndLossesAnswers].copy(previousYearsLosses = AmountInPence(1L))

            testSuccessfulUpdatesAfterSubmit(
              performAction(
                "previousYearsLosses" -> "1"
              )
            )(answers, answers.copy(previousYearsLosses = AmountInPence.zero))

          }

        }

      }

      "not do any updates" when {

        "the value submitted hasn't changed" in {
          val answers =
            sample[CompleteExemptionAndLossesAnswers].copy(previousYearsLosses = AmountInPence(1L))
          val session = sessionWithMultipleDisposalsState(answers, sample[DisposalDate], individualUserType)._1

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkIsRedirect(
            performAction(
              "previousYearsLosses"      -> "0",
              "previousYearsLossesValue" -> "0.01"
            ),
            routes.ExemptionAndLossesController.checkYourAnswers()
          )

        }

      }
    }

    "handling requests to display the annual exempt amount page" must {

      def performAction(): Future[Result] = controller.annualExemptAmount()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      "redirect to the task list page" when {

        "there is no disposal date" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithMultipleDisposalsState(
                Some(sample[CompleteExemptionAndLossesAnswers]),
                None,
                individualUserType
              )._1
            )
          }

          checkIsRedirect(performAction(), returns.routes.TaskListController.taskList())
        }

      }

      "redirect to previous years losses page" when {

        "that question has not been answered yet" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithSingleDisposalState(
                sample[IncompleteExemptionAndLossesAnswers]
                  .copy(previousYearsLosses = None),
                sample[DisposalDate],
                individualUserType
              )._1
            )
          }

          checkIsRedirect(performAction(), routes.ExemptionAndLossesController.previousYearsLosses())
        }

      }

      "display the page" when {

        "the exemption and losses section has not yet been completed" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithMultipleDisposalsState(
                sample[IncompleteExemptionAndLossesAnswers]
                  .copy(previousYearsLosses = Some(sample[AmountInPence]), annualExemptAmount = None),
                sample[DisposalDate],
                individualUserType
              )._1
            )
          }
          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(
              "annualExemptAmount.individual.title"
            ), { doc =>
              doc.select("#back").attr("href") shouldBe routes.ExemptionAndLossesController.previousYearsLosses().url
              doc.select("#content > article > form").attr("action") shouldBe routes.ExemptionAndLossesController
                .annualExemptAmountSubmit()
                .url
            }
          )
        }

        "the exemption and losses section has been completed" in {
          val amount = AmountInPence(1000L)
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithSingleDisposalState(
                sample[CompleteExemptionAndLossesAnswers].copy(annualExemptAmount = amount),
                sample[DisposalDate],
                individualUserType
              )._1
            )
          }
          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(
              "annualExemptAmount.individual.title"
            ), { doc =>
              doc.select("#back").attr("href") shouldBe routes.ExemptionAndLossesController.checkYourAnswers().url
              doc.select("#content > article > form").attr("action") shouldBe routes.ExemptionAndLossesController
                .annualExemptAmountSubmit()
                .url
              doc.select("#annualExemptAmount").attr("value") shouldBe "10"

            }
          )
        }

        "the user type is an individual" in {
          val amount = AmountInPence(1000L)
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithSingleDisposalState(
                sample[CompleteExemptionAndLossesAnswers].copy(annualExemptAmount = amount),
                sample[DisposalDate],
                individualUserType
              )._1
            )
          }
          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(
              "annualExemptAmount.individual.title"
            ), { doc =>
              doc.select("#annualExemptAmount-form-hint").text() contains messageFromMessageKey(
                "annualExemptAmount.individual.helpText"
              )
              doc.select("#content > article > form > p > a").text() contains messageFromMessageKey(
                "annualExemptAmount.individual.link"
              )
            }
          )
        }

        "the user type is a trust" in {
          val amount = AmountInPence(1000L)
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithSingleDisposalState(
                sample[CompleteExemptionAndLossesAnswers].copy(annualExemptAmount = amount),
                sample[DisposalDate],
                trustUserType
              )._1
            )
          }
          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(
              "annualExemptAmount.trust.title"
            ), { doc =>
              doc.select("#annualExemptAmount-form-hint").text() contains messageFromMessageKey(
                "annualExemptAmount.trust.helpText"
              )
              doc.select("#content > article > form > p > a").text() contains messageFromMessageKey(
                "annualExemptAmount.trust.link"
              )
              doc.select("#annualExemptAmount-form-hint").text() contains messageFromMessageKey(
                "annualExemptAmount.trust.helpText"
              )
              doc.select("#content > article > form > details:nth-child(3) > summary > span") contains messageFromMessageKey(
                "annualExemptAmount.trust.details.1.header"
              )
              doc.select("#details-content-0") contains messageFromMessageKey("annualExemptAmount.trust.details.1.body")
              doc.select("#content > article > form > details:nth-child(4) > summary > span") contains messageFromMessageKey(
                "annualExemptAmount.trust.details.2.header"
              )
              doc.select("#details-content-1") contains messageFromMessageKey("annualExemptAmount.trust.details.2.body")
            }
          )
        }

      }

    }

  }

  "handling submitted annual exempt amounts" must {

    def performAction(data: (String, String)*): Future[Result] =
      controller.annualExemptAmountSubmit()(FakeRequest().withFormUrlEncodedBody(data: _*))

    val maximumAnnualExemptAmount = AmountInPence(10000L)

    val disposalDate = sample[DisposalDate]
      .copy(taxYear = sample[TaxYear].copy(annualExemptAmountGeneral = maximumAnnualExemptAmount))

    behave like redirectToStartBehaviour(() => performAction())

    "redirect to the task list page" when {

      "there is no disposal date" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            sessionWithSingleDisposalsState(
              Some(sample[CompleteExemptionAndLossesAnswers]),
              None,
              individualUserType
            )._1
          )
        }

        checkIsRedirect(performAction(), returns.routes.TaskListController.taskList())
      }

    }

    "redirect to previous years losses page" when {

      "that question has not been answered yet" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            sessionWithMultipleDisposalsState(
              sample[IncompleteExemptionAndLossesAnswers]
                .copy(previousYearsLosses = None),
              sample[DisposalDate],
              individualUserType
            )._1
          )
        }

        checkIsRedirect(performAction(), routes.ExemptionAndLossesController.previousYearsLosses())
      }

    }

    "show a form error" when {

      val currentSession =
        sessionWithSingleDisposalState(
          sample[CompleteExemptionAndLossesAnswers],
          disposalDate,
          individualUserType
        )._1

      def test(data: (String, String)*)(expectedErrorKey: String): Unit =
        testFormError(data: _*)(
          expectedErrorKey,
          MoneyUtils.formatAmountOfMoneyWithoutPoundSign(disposalDate.taxYear.annualExemptAmountGeneral.inPounds())
        )("annualExemptAmount.individual.title")(performAction, currentSession)

      "the amount of money is invalid" in {
        amountOfMoneyErrorScenarios("annualExemptAmount", maximumAnnualExemptAmount.inPounds()).foreach { scenario =>
          withClue(s"For $scenario: ") {
            test(scenario.formData: _*)(scenario.expectedErrorMessageKey)
          }
        }
      }

    }

    "show an error page" when {

      val newAmount = AmountInPence(123L)
      val answers: CompleteExemptionAndLossesAnswers =
        sample[CompleteExemptionAndLossesAnswers].copy(annualExemptAmount = AmountInPence(newAmount.value + 1L))
      val (session, journey, draftReturn) =
        sessionWithMultipleDisposalsState(answers, disposalDate, individualUserType)
      val updatedDraftReturn = draftReturn.copy(exemptionAndLossesAnswers = Some(
        answers.copy(annualExemptAmount = newAmount)
      )
      )

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

        checkIsTechnicalErrorPage(
          performAction("annualExemptAmount" -> newAmount.inPounds().toString)
        )
      }

      "there is an error updating the session" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockStoreDraftReturn(
            updatedDraftReturn,
            journey.subscribedDetails.cgtReference,
            journey.agentReferenceNumber
          )(Right(()))
          mockStoreSession(
            session.copy(
              journeyStatus = Some(journey.copy(draftReturn = updatedDraftReturn))
            )
          )(Left(Error("")))
        }

        checkIsTechnicalErrorPage(
          performAction("annualExemptAmount" -> newAmount.inPounds().toString)
        )
      }

    }

    "redirect to the task list page" when {

      "all updates are successful and" when {

        "the journey was incomplete" in {
          val newAmount = AmountInPence(3000L)
          val answers =
            sample[IncompleteExemptionAndLossesAnswers].copy(
              previousYearsLosses = Some(sample[AmountInPence]),
              annualExemptAmount  = None
            )

          testSuccessfulUpdatesAfterSubmit(
            performAction(
              "annualExemptAmount" -> newAmount.inPounds().toString
            )
          )(
            answers,
            answers.copy(annualExemptAmount = Some(newAmount)),
            disposalDate = disposalDate
          )
        }

        "the journey was complete" in {
          val newAmount = AmountInPence(6000L)
          val answers =
            sample[CompleteExemptionAndLossesAnswers].copy(
              annualExemptAmount = AmountInPence(newAmount.value + 1)
            )

          testSuccessfulUpdatesAfterSubmit(
            performAction(
              "annualExemptAmount" -> newAmount.inPounds().toString
            )
          )(
            answers,
            answers.copy(annualExemptAmount = newAmount),
            disposalDate = disposalDate
          )
        }

      }

    }

    "not do any updates" when {

      "the value submitted hasn't changed" in {
        val answers =
          sample[CompleteExemptionAndLossesAnswers].copy(annualExemptAmount = AmountInPence(1L))
        val session = sessionWithMultipleDisposalsState(answers, disposalDate, individualUserType)._1

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkIsRedirect(
          performAction(
            "annualExemptAmount" -> "0.01"
          ),
          routes.ExemptionAndLossesController.checkYourAnswers()
        )

      }

    }
  }

  "handling requests to display the cya page" must {

    def performAction(): Future[Result] = controller.checkYourAnswers()(FakeRequest())

    val completeAnswers = sample[CompleteExemptionAndLossesAnswers]

    val allQuestionsAnswered = IncompleteExemptionAndLossesAnswers(
      Some(completeAnswers.inYearLosses),
      Some(completeAnswers.previousYearsLosses),
      Some(completeAnswers.annualExemptAmount)
    )

    val (session, journey, draftReturn) =
      sessionWithSingleDisposalState(allQuestionsAnswered, sample[DisposalDate], individualUserType)
    val updatedDraftReturn = draftReturn.copy(exemptionAndLossesAnswers = Some(completeAnswers))
    val updatedSession     = session.copy(journeyStatus                 = Some(journey.copy(draftReturn = updatedDraftReturn)))

    behave like redirectToStartBehaviour(performAction)

    def testIsRedirectWhenMissingAnswer(
      answers: IncompleteExemptionAndLossesAnswers,
      expectedRedirect: Call
    ): Unit =
      List(
        sessionWithSingleDisposalState(answers, sample[DisposalDate], individualUserType)._1,
        sessionWithMultipleDisposalsState(answers, sample[DisposalDate], individualUserType)._1
      ).foreach { session =>
        withClue(s"For sesssion $session: ") {

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithSingleDisposalState(answers, sample[DisposalDate], individualUserType)._1
            )
          }

          checkIsRedirect(performAction(), expectedRedirect)
        }
      }

    "redirect to the in year losses page" when {

      "that question has not been answered" in {
        testIsRedirectWhenMissingAnswer(
          allQuestionsAnswered.copy(inYearLosses = None),
          routes.ExemptionAndLossesController.inYearLosses()
        )
      }
    }

    "redirect to the previous years losses page" when {

      "that question has not been answered" in {
        testIsRedirectWhenMissingAnswer(
          allQuestionsAnswered.copy(previousYearsLosses = None),
          routes.ExemptionAndLossesController.previousYearsLosses()
        )
      }
    }

    "redirect to the annual exempt amount page" when {

      "that question has not been answered" in {
        testIsRedirectWhenMissingAnswer(
          allQuestionsAnswered.copy(annualExemptAmount = None),
          routes.ExemptionAndLossesController.annualExemptAmount()
        )
      }
    }

    "show an error page" when {

      "the user has just answered all the questions and" when {

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

          checkIsTechnicalErrorPage(performAction())
        }

        "there is an error updating the session" in {
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

          checkIsTechnicalErrorPage(performAction())
        }

      }

    }

    "display the page" when {

      "the user has already answered all the questions" in {
        forAll { completeAnswers: CompleteExemptionAndLossesAnswers =>
          val updatedDraftReturn = draftReturn.copy(exemptionAndLossesAnswers = Some(completeAnswers))
          val updatedSession     = session.copy(journeyStatus                 = Some(journey.copy(draftReturn = updatedDraftReturn)))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(updatedSession)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("exemptionsAndLosses.cya.title"), { doc =>
              validateExemptionAndLossesCheckYourAnswersPage(completeAnswers, doc)
              doc.select("#content > article > form").attr("action") shouldBe routes.ExemptionAndLossesController
                .checkYourAnswersSubmit()
                .url
            }
          )
        }
      }

      "the user has just answered all the questions and all updates are successful" in {
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
          messageFromMessageKey("exemptionsAndLosses.cya.title"), { doc =>
            validateExemptionAndLossesCheckYourAnswersPage(completeAnswers, doc)
            doc.select("#content > article > form").attr("action") shouldBe routes.ExemptionAndLossesController
              .checkYourAnswersSubmit()
              .url
          }
        )
      }

      "the user wishes to use in year losses" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(updatedSession)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("exemptionsAndLosses.cya.title"), { doc =>
            doc.select("#content > article > form").attr("action") shouldBe routes.ExemptionAndLossesController
              .checkYourAnswersSubmit()
              .url
          }
        )
      }

    }

  }

  "handling submits from the cya page" must {

    def performAction(): Future[Result] = controller.checkYourAnswersSubmit()(FakeRequest())

    behave like redirectToStartBehaviour(performAction)

    "redirect to the task list page" in {
      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(
          sessionWithSingleDisposalState(
            sample[CompleteExemptionAndLossesAnswers],
            sample[DisposalDate],
            individualUserType
          )._1
        )
      }

      checkIsRedirect(performAction(), returns.routes.TaskListController.taskList())
    }

  }

  def testFormError(
    data: (String, String)*
  )(expectedErrorMessageKey: String, errorArgs: String*)(pageTitleKey: String, titleArgs: String*)(
    performAction: Seq[(String, String)] => Future[Result],
    currentSession: SessionData = sessionWithSingleDisposalState(
      sample[CompleteExemptionAndLossesAnswers],
      sample[DisposalDate],
      individualUserType
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

  def testSuccessfulUpdatesAfterSubmit(result: => Future[Result])(
    oldAnswers: ExemptionAndLossesAnswers,
    newAnswers: ExemptionAndLossesAnswers,
    disposalDate: DisposalDate = sample[DisposalDate]
  ): Unit =
    List(
      sessionWithSingleDisposalState(oldAnswers, disposalDate, individualUserType),
      sessionWithMultipleDisposalsState(oldAnswers, disposalDate, individualUserType)
    ).foreach {
      case (session, journey, draftReturn) =>
        withClue(s"For initial session $session: ") {
          val updatedDraftReturn = draftReturn.fold(
            _.copy(exemptionAndLossesAnswers = Some(newAnswers)),
            _.copy(exemptionAndLossesAnswers = Some(newAnswers))
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(
              updatedDraftReturn,
              journey.subscribedDetails.cgtReference,
              journey.agentReferenceNumber
            )(
              Right(())
            )
            mockStoreSession(
              session.copy(
                journeyStatus = Some(journey.copy(draftReturn = updatedDraftReturn))
              )
            )(Right(()))
          }

          checkIsRedirect(result, routes.ExemptionAndLossesController.checkYourAnswers())
        }
    }

}

object ExemptionAndLossesControllerSpec extends Matchers {
  def validateExemptionAndLossesCheckYourAnswersPage(
    completeExemptionAndLossesAnswers: CompleteExemptionAndLossesAnswers,
    doc: Document
  )(implicit messages: MessagesApi, lang: Lang): Unit = {

    if (completeExemptionAndLossesAnswers.inYearLosses.isZero) {
      doc.select("#inYearLosses-answer").text shouldBe "No"
    } else {
      doc.select("#inYearLosses-answer").text shouldBe "Yes"
      doc.select("#inYearLossesValue-answer").text shouldBe formatAmountOfMoneyWithPoundSign(
        completeExemptionAndLossesAnswers.inYearLosses.inPounds()
      )
    }

    if (completeExemptionAndLossesAnswers.previousYearsLosses.isZero) {
      doc.select("#previousYearsLosses-answer").text shouldBe "No"
    } else {
      doc.select("#previousYearsLosses-answer").text shouldBe "Yes"
      doc.select("#previousYearsLossesValue-answer").text shouldBe formatAmountOfMoneyWithPoundSign(
        completeExemptionAndLossesAnswers.previousYearsLosses.inPounds()
      )
    }

    doc.select("#annualExemptAmount-answer").text shouldBe formatAmountOfMoneyWithPoundSign(
      completeExemptionAndLossesAnswers.annualExemptAmount.inPounds()
    )
  }
}
