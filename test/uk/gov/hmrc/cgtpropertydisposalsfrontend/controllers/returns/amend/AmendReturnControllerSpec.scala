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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.amend

import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.http.Status.BAD_REQUEST
import play.api.i18n.MessagesApi
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.{Call, Result}
import play.api.test.FakeRequest
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.StartingToAmendToFillingOutReturnSpecBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.exemptionandlosses.routes.{ExemptionAndLossesController => exemptionsAndLossesRoutes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.initialgainorloss.routes.{InitialGainOrLossController => initialGainorLossRoutes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.yeartodatelliability.routes.{YearToDateLiabilityController => ytdRoutes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.StartingToAmendReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.JourneyStatusGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.UUIDGenerator
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{SessionData, UserType}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore

import scala.concurrent.Future

class AmendReturnControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with ScalaCheckDrivenPropertyChecks
    with RedirectToStartBehaviour
    with StartingToAmendToFillingOutReturnSpecBehaviour {

  val mockUUIDGenerator: UUIDGenerator = mock[UUIDGenerator]

  override val overrideBindings =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore),
      bind[UUIDGenerator].toInstance(mockUUIDGenerator)
    )

  lazy val controller = instanceOf[AmendReturnController]

  implicit val messagesApi: MessagesApi = controller.messagesApi

  "AmendReturnController" when {

    def redirectToStartBehaviour(performAction: () => Future[Result]) =
      behave like redirectToStartWhenInvalidJourney(
        performAction,
        {
          case _: StartingToAmendReturn => true
          case _                        => false
        }
      )

    "handling requests to display the 'you must calculate' page" must {

      def performAction(): Future[Result] = controller.youNeedToCalculate()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      "display the page" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData.empty.copy(journeyStatus = Some(sample[StartingToAmendReturn])))
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("youNeedToCalculate.title"),
          { doc =>
            doc.select("#back").attr("href")                      shouldBe controllers.returns.routes.ViewReturnController
              .displayReturn()
              .url
            doc.select("#cancelOrContinue-cancel").attr("href")   shouldBe routes.AmendReturnController
              .confirmCancel(AmendReturnController.ConfirmCancelBackLocations.calculateAmounts)
              .url
            doc.select("#cancelOrContinue-continue").attr("href") shouldBe routes.AmendReturnController
              .checkYourAnswers()
              .url
          }
        )

      }
    }

    "handling requests to display the confirm cancellation page" must {

      def performAction(back: String): Future[Result] =
        controller.confirmCancel(back)(FakeRequest())

      "show an error page" when {

        "the back location is not understood" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData.empty)
          }

          checkIsTechnicalErrorPage(performAction("abc"))
        }

      }

      "display the page" when {

        def test(
          back: String,
          expectedBackLink: Call
        ): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData.empty)
          }

          checkPageIsDisplayed(
            performAction(back),
            messageFromMessageKey("confirmCancelAmendReturn.title"),
            { doc =>
              doc.select("#back").attr("href") shouldBe expectedBackLink.url

              doc
                .select("#content > article > form")
                .attr("action") shouldBe routes.AmendReturnController.confirmCancelSubmit(back).url
            }
          )
        }

        "the user came from the 'you need to calculate page'" in {
          test(
            AmendReturnController.ConfirmCancelBackLocations.calculateAmounts,
            routes.AmendReturnController.youNeedToCalculate()
          )
        }

        "the user came from the amend cya page" in {
          test(
            AmendReturnController.ConfirmCancelBackLocations.checkAnswers,
            routes.AmendReturnController.checkYourAnswers()
          )
        }

        "the user came from the unmet dependency page" in {
          test(
            AmendReturnController.ConfirmCancelBackLocations.unmetDependency,
            routes.AmendReturnController.unmetDependency()
          )
        }

        "the user came from the task list page" in {
          test(
            AmendReturnController.ConfirmCancelBackLocations.taskList,
            controllers.returns.routes.TaskListController.taskList()
          )
        }

        "the user came from the check all answers and send return page" in {
          test(
            AmendReturnController.ConfirmCancelBackLocations.checkAnswersAcceptSend,
            controllers.returns.routes.CheckAllAnswersAndSubmitController.checkAllAnswers()
          )
        }

      }
    }

    "handling submits on the confirm cancellation page" must {

      def performAction(formData: (String, String)*)(back: String): Future[Result] =
        controller.confirmCancelSubmit(back)(FakeRequest().withFormUrlEncodedBody(formData: _*))

      "show an error page" when {

        "the back location is not understood" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData.empty)
          }

          checkIsTechnicalErrorPage(performAction()("abc"))
        }

      }

      "display a form error" when {

        def test(data: (String, String)*)(expectedErrorMessageKey: String): Unit =
          checkPageIsDisplayed(
            performAction(data: _*)(AmendReturnController.ConfirmCancelBackLocations.checkAnswers),
            messageFromMessageKey("confirmCancelAmendReturn.title"),
            { doc =>
              doc.select("#back").attr("href") shouldBe routes.AmendReturnController.checkYourAnswers().url

              doc
                .select("#error-summary-display > ul > li > a")
                .text() shouldBe messageFromMessageKey(
                expectedErrorMessageKey
              )

              doc.title() should startWith("Error:")
            },
            BAD_REQUEST
          )

        "nothing is submitted" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData.empty)
          }

          test()("confirmCancelAmendReturn.error.required")
        }

        "the value submitted is not understood" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData.empty)
          }

          test("confirmCancelAmendReturn" -> "123")("confirmCancelAmendReturn.error.required")
        }

      }

      "redirect to the correct page" when {

        "the user confirms they want to cancel" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData.empty)
          }

          checkIsRedirect(
            performAction("confirmCancelAmendReturn" -> "true")(
              AmendReturnController.ConfirmCancelBackLocations.calculateAmounts
            ),
            controllers.returns.routes.ViewReturnController.displayReturn()
          )
        }

        "the user doesn't want to cancel" in {
          AmendReturnController.confirmCancelBackLinkMappings.foreach {
            case (backKey, backLink) =>
              withClue(s"For back key '$backKey' and back location '${backLink.url}': ") {
                inSequence {
                  mockAuthWithNoRetrievals()
                  mockGetSession(SessionData.empty)
                }

                checkIsRedirect(
                  performAction("confirmCancelAmendReturn" -> "false")(backKey),
                  backLink
                )
              }
          }
        }

      }

    }

    "handling requests to display the check your answers page" must {

      def performAction(): Future[Result] = controller.checkYourAnswers()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      "display the page" when {

        def test(session: SessionData, expectedTitle: String) = {

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              session
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(expectedTitle),
            { doc =>
              doc.select("#back").attr("href") shouldBe routes.AmendReturnController.youNeedToCalculate().url

              doc.select("#cancelOrContinue-cancel").attr("href") shouldBe routes.AmendReturnController
                .confirmCancel(AmendReturnController.ConfirmCancelBackLocations.checkAnswers)
                .url

              doc.select("#cancelOrContinue-continue").isEmpty shouldBe true
              doc.select("#returnSummary").isEmpty             shouldBe false
            }
          )
        }

        "the user is not an agent" in {
          test(
            SessionData.empty.copy(
              journeyStatus = Some(sample[StartingToAmendReturn]),
              userType = Some(UserType.Individual)
            ),
            "amendCya.title"
          )
        }

        "the user is an agent" in {
          test(
            SessionData.empty.copy(
              journeyStatus = Some(sample[StartingToAmendReturn]),
              userType = Some(UserType.Agent)
            ),
            "amendCya.agent.title"
          )
        }

      }

    }

    "handling requests to display the unmet dependency page" must {

      def performAction(): Future[Result] = controller.unmetDependency()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      "redirect to the amend cya page" when {

        "there is no unmet dependency field url in session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  sample[StartingToAmendReturn].copy(
                    unmetDependencyFieldUrl = None
                  )
                )
              )
            )
          }

          checkIsRedirect(performAction(), routes.AmendReturnController.checkYourAnswers())
        }

      }

      "show an error page" when {

        "the unmet dependency field url in session is not understood" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  sample[StartingToAmendReturn].copy(
                    unmetDependencyFieldUrl = Some("abc")
                  )
                )
              )
            )
          }

          checkIsTechnicalErrorPage(performAction())
        }

      }

      "display the page" when {

        def test(
          unmetDependencyFieldUrl: String,
          expectedKey: String
        ): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  sample[StartingToAmendReturn].copy(
                    unmetDependencyFieldUrl = Some(unmetDependencyFieldUrl)
                  )
                )
              )
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(s"${expectedKey + ".title"}"),
            { doc =>
              doc.select("#back").attr("href")         shouldBe routes.AmendReturnController.checkYourAnswers().url
              doc
                .select("#content > article > form")
                .attr("action")                        shouldBe routes.AmendReturnController.unmetDependencySubmit().url
              doc.select("#cancelButton").attr("href") shouldBe routes.AmendReturnController
                .confirmCancelSubmit(AmendReturnController.ConfirmCancelBackLocations.unmetDependency)
                .url
            }
          )
        }

        "the unmet dependency field url in session is understood" in {
          val testCases = List(
            exemptionsAndLossesRoutes.inYearLosses().url         -> "inYearLosses",
            exemptionsAndLossesRoutes.previousYearsLosses().url  -> "previousYearLosses",
            exemptionsAndLossesRoutes.annualExemptAmount().url   -> "annualExemptAmount",
            ytdRoutes.estimatedIncome().url                      -> "income",
            ytdRoutes.personalAllowance().url                    -> "personalAllowance",
            ytdRoutes.taxableGainOrLoss().url                    -> "taxableGainOrLoss",
            ytdRoutes.yearToDateLiability().url                  -> "yearToDateLiability",
            ytdRoutes.taxDue().url                               -> "taxOwed",
            ytdRoutes.nonCalculatedEnterTaxDue().url             -> "taxOwed",
            ytdRoutes.repayment().url                            -> "repayment",
            ytdRoutes.hasEstimatedDetails().url                  -> "hasEstimated",
            initialGainorLossRoutes.enterInitialGainOrLoss().url -> "initialGainOrLoss"
          )

          testCases.foreach {
            case (unmetDependencyFieldUrl, titleKey) =>
              withClue(s"For '$unmetDependencyFieldUrl' and '$titleKey': ") {
                val key =
                  if (
                    titleKey === "initialGainOrLoss" || titleKey === "annualExemptAmount" || titleKey === "income" || titleKey === "personalAllowance"
                  )
                    "unmetDependency.x1"
                  else
                    "unmetDependency.x2"
                test(unmetDependencyFieldUrl, key)
              }

          }

        }

      }

    }

    "handling submits on the unmet dependency page" must {

      def performAction(): Future[Result] = controller.unmetDependencySubmit()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.unmetDependencySubmit(),
        mockUUIDGenerator,
        Some(controllers.returns.routes.TaskListController.taskList().url)
      )

      "redirect to the amend cya page" when {

        "there is no unmet dependency field url in session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  sample[StartingToAmendReturn].copy(
                    unmetDependencyFieldUrl = None
                  )
                )
              )
            )
          }

          checkIsRedirect(performAction(), routes.AmendReturnController.checkYourAnswers())
        }

      }

    }

  }

}
