/*
 * Copyright 2023 HM Revenue & Customs
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

import uk.gov.hmrc.cgtpropertydisposalsfrontend.SampledScalaCheck
import play.api.http.Status.BAD_REQUEST
import play.api.i18n.MessagesApi
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.libs.json.Writes
import play.api.mvc.{Call, Request, Result}
import play.api.test.FakeRequest
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.StartingToAmendToFillingOutReturnSpecBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.exemptionandlosses.routes.{ExemptionAndLossesController => exemptionsAndLossesRoutes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.initialgainorloss.routes.{InitialGainOrLossController => initialGainorLossRoutes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.yeartodatelliability.routes.{YearToDateLiabilityController => ytdRoutes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, StartingToAmendReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.CompleteReturnGen.completeSingleDisposalReturnGen
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.JourneyStatusGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.ReturnGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.SubscribedDetailsGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.TriageQuestionsGen.completeSingleDisposalTriageAnswersGen
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.{AgentReferenceNumber, CgtReference, UUIDGenerator}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.SubscribedDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.CompleteReturn.CompleteSingleDisposalReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.CompleteSingleDisposalTriageAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.audit.CancelAmendReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{AmendReturnData, IndividualUserType, ReturnSummary, TaxYearExchanged}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{CompleteReturnWithSummary, SessionData, UserType}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.AuditService
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

class AmendReturnControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with SampledScalaCheck
    with RedirectToStartBehaviour
    with StartingToAmendToFillingOutReturnSpecBehaviour {

  val mockUUIDGenerator: UUIDGenerator = mock[UUIDGenerator]

  val mockAuditService: AuditService = mock[AuditService]

  protected override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore),
      bind[UUIDGenerator].toInstance(mockUUIDGenerator),
      bind[AuditService].toInstance(mockAuditService)
    )

  private lazy val controller = instanceOf[AmendReturnController]

  implicit val messagesApi: MessagesApi = controller.messagesApi

  def mockAuditCancelAmendReturn(auditEvent: CancelAmendReturn): Unit =
    (mockAuditService
      .sendEvent(_: String, _: CancelAmendReturn, _: String)(using
        _: ExecutionContext,
        _: HeaderCarrier,
        _: Writes[CancelAmendReturn],
        _: Request[?]
      ))
      .expects("CancelAmendReturn", auditEvent, "cancel-amend-return", *, *, *, *)
      .returning(())

  "AmendReturnController" when {

    def redirectToStartBehaviour(performAction: () => Future[Result]): Unit =
      behave like redirectToStartWhenInvalidJourney(
        performAction,
        {
          case _: StartingToAmendReturn => true
          case _                        => false
        }
      )

    "handling requests to display the confirm cancellation page" must {

      def performAction(back: String): Future[Result] =
        controller.confirmCancel(back)(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(
        () => performAction(AmendReturnController.ConfirmCancelBackLocations.checkAnswers),
        {
          case _: StartingToAmendReturn => true
          case f: FillingOutReturn      => f.amendReturnData.isDefined
          case _                        => false
        }
      )

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
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(sample[StartingToAmendReturn])
              )
            )
          }

          checkPageIsDisplayed(
            performAction(back),
            messageFromMessageKey("confirmCancelAmendReturn.title"),
            { doc =>
              doc.select("#back, .govuk-back-link").attr("href") shouldBe expectedBackLink.url

              doc
                .select("#content > article > form, #main-content form")
                .attr("action") shouldBe routes.AmendReturnController.confirmCancelSubmit(back).url
            }
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
        controller.confirmCancelSubmit(back)(FakeRequest().withFormUrlEncodedBody(formData*).withMethod("POST"))

      behave like redirectToStartWhenInvalidJourney(
        () => performAction()(AmendReturnController.ConfirmCancelBackLocations.checkAnswers),
        {
          case _: StartingToAmendReturn => true
          case f: FillingOutReturn      => f.amendReturnData.isDefined
          case _                        => false
        }
      )

      "show an error page" when {

        "the back location is not understood" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(sample[StartingToAmendReturn])
              )
            )
          }

          checkIsTechnicalErrorPage(performAction()("abc"))
        }

      }

      "display a form error" when {

        def test(data: (String, String)*)(expectedErrorMessageKey: String): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(sample[StartingToAmendReturn])
              )
            )
          }

          checkPageIsDisplayed(
            performAction(data*)(AmendReturnController.ConfirmCancelBackLocations.checkAnswers),
            messageFromMessageKey("confirmCancelAmendReturn.title"),
            { doc =>
              doc.select("#back, .govuk-back-link").attr("href") shouldBe routes.AmendReturnController
                .checkYourAnswers()
                .url

              doc
                .select("[data-spec='errorSummaryDisplay'] a")
                .text() shouldBe messageFromMessageKey(
                expectedErrorMessageKey
              )

              doc.title() should startWith("Error:")
            },
            BAD_REQUEST
          )
        }

        "nothing is submitted" in {
          test()("confirmCancelAmendReturn.error.required")
        }

        "the value submitted is not understood" in {
          test("confirmCancelAmendReturn" -> "123")("confirmCancelAmendReturn.error.required")
        }

      }

      "redirect to the correct page" when {

        "the user confirms they want to cancel and" when {

          "they were starting to amend a return" in {
            val auditEvent    = CancelAmendReturn("cgtRef", "submissionId", Some("agentRef"))
            val journeyStatus = sample[StartingToAmendReturn].copy(
              subscribedDetails = sample[SubscribedDetails].copy(cgtReference = CgtReference(auditEvent.cgtReference)),
              originalReturn = sample[CompleteReturnWithSummary].copy(
                summary = sample[ReturnSummary].copy(
                  submissionId = auditEvent.submissionId
                ),
                completeReturn = sample[CompleteSingleDisposalReturn].copy(
                  representeeAnswers = None,
                  triageAnswers = sample[CompleteSingleDisposalTriageAnswers].copy(
                    individualUserType = Some(IndividualUserType.Self)
                  )
                )
              ),
              agentReferenceNumber = auditEvent.agentReferenceNumber.map(AgentReferenceNumber(_))
            )

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(SessionData.empty.copy(journeyStatus = Some(journeyStatus)))
              mockAuditCancelAmendReturn(auditEvent)
            }

            checkIsRedirect(
              performAction("confirmCancelAmendReturn" -> "true")(
                AmendReturnController.ConfirmCancelBackLocations.checkAnswers
              ),
              controllers.returns.routes.ViewReturnController.displayReturn()
            )
          }

          "they were amending a return" in {
            val auditEvent    = CancelAmendReturn("cgtRef", "submissionId", Some("agentRef"))
            val journeyStatus = sample[FillingOutReturn].copy(
              subscribedDetails = sample[SubscribedDetails].copy(cgtReference = CgtReference(auditEvent.cgtReference)),
              amendReturnData = Some(
                sample[AmendReturnData].copy(
                  originalReturn = sample[CompleteReturnWithSummary].copy(
                    summary = sample[ReturnSummary].copy(
                      submissionId = auditEvent.submissionId
                    ),
                    completeReturn = sample[CompleteSingleDisposalReturn].copy(
                      representeeAnswers = None,
                      triageAnswers = sample[CompleteSingleDisposalTriageAnswers].copy(
                        individualUserType = Some(IndividualUserType.Self)
                      )
                    )
                  )
                )
              ),
              agentReferenceNumber = auditEvent.agentReferenceNumber.map(AgentReferenceNumber(_))
            )

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(SessionData.empty.copy(journeyStatus = Some(journeyStatus)))
              mockAuditCancelAmendReturn(auditEvent)
            }

            checkIsRedirect(
              performAction("confirmCancelAmendReturn" -> "true")(
                AmendReturnController.ConfirmCancelBackLocations.checkAnswers
              ),
              controllers.returns.routes.ViewReturnController.displayReturn()
            )
          }

        }

        "the user doesn't want to cancel" in {
          AmendReturnController.confirmCancelBackLinkMappings.foreach { case (backKey, backLink) =>
            withClue(s"For back key '$backKey' and back location '${backLink.url}': ") {
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  SessionData.empty.copy(
                    journeyStatus = Some(sample[StartingToAmendReturn])
                  )
                )
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

      behave like redirectToStartBehaviour(() => performAction())

      "display the page" when {

        def test(session: SessionData, expectedTitle: String): Unit = {

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
              doc
                .select("#back, .govuk-back-link")
                .attr("href") shouldBe controllers.returns.routes.ViewReturnController
                .displayReturn()
                .url

              doc.select("#cancelOrContinue-cancel").attr("href") shouldBe routes.AmendReturnController
                .confirmCancel(AmendReturnController.ConfirmCancelBackLocations.checkAnswers)
                .url

              doc.select("#cancelOrContinue-continue").isEmpty shouldBe true
              doc.select("#returnSummary").isEmpty             shouldBe false
            }
          )
        }

        val completeReturnWithSummary = sample[CompleteReturnWithSummary].copy(
          completeReturn = sample[CompleteSingleDisposalReturn].copy(
            triageAnswers = sample[CompleteSingleDisposalTriageAnswers].copy(
              individualUserType = Some(IndividualUserType.Self)
            )
          ),
          summary = sample[ReturnSummary].copy(taxYear = TaxYearExchanged.currentTaxYear.toString)
        )

        "the user is not an agent" in {
          test(
            SessionData.empty.copy(
              journeyStatus = Some(
                sample[StartingToAmendReturn].copy(
                  originalReturn = completeReturnWithSummary
                )
              ),
              userType = Some(UserType.Individual)
            ),
            "amendCya.title"
          )
        }

        "the user is an agent" in {
          test(
            SessionData.empty.copy(
              journeyStatus = Some(
                sample[StartingToAmendReturn].copy(
                  originalReturn = completeReturnWithSummary
                )
              ),
              userType = Some(UserType.Agent)
            ),
            "amendCya.agent.title"
          )
        }

      }

    }

    "handling requests to display the unmet dependency page" must {

      def performAction(): Future[Result] = controller.unmetDependency()(FakeRequest())

      behave like redirectToStartBehaviour(() => performAction())

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
              doc.select("#back, .govuk-back-link").attr("href") shouldBe routes.AmendReturnController
                .checkYourAnswers()
                .url
              doc
                .select("#content > article > form, #main-content form")
                .attr("action")                                  shouldBe routes.AmendReturnController.unmetDependencySubmit().url
              doc.select("#cancelButton").attr("href")           shouldBe routes.AmendReturnController
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

          testCases.foreach { case (unmetDependencyFieldUrl, titleKey) =>
            withClue(s"For '$unmetDependencyFieldUrl' and '$titleKey': ") {
              val key =
                if (
                  titleKey === "initialGainOrLoss" || titleKey === "annualExemptAmount" || titleKey === "income" || titleKey === "personalAllowance"
                ) {
                  "unmetDependency.x1"
                } else {
                  "unmetDependency.x2"
                }
              test(unmetDependencyFieldUrl, key)
            }

          }

        }

      }

    }

    "handling submits on the unmet dependency page" must {

      def performAction(): Future[Result] = controller.unmetDependencySubmit()(FakeRequest())

      behave like redirectToStartBehaviour(() => performAction())

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
