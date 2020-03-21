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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns

import java.util.UUID

import cats.data.EitherT
import cats.instances.future._
import org.jsoup.nodes.Document
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.i18n.{Lang, MessagesApi}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.{Call, Request, Result}
import play.api.test.FakeRequest
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.accounts.homepage
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.CheckAllAnswersAndSubmitControllerSpec.validateAllCheckYourAnswersSections
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.acquisitiondetails.AcquisitionDetailsControllerSpec.validateAcquisitionDetailsCheckYourAnswersPage
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.acquisitiondetails.RebasingEligibilityUtil
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.disposaldetails.DisposalDetailsControllerSpec._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.exemptionandlosses.ExemptionAndLossesControllerSpec.validateExemptionAndLossesCheckYourAnswersPage
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.reliefdetails.ReliefDetailsControllerSpec.validateReliefDetailsCheckYourAnswersPage
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.triage.SingleDisposalsTriageControllerSpec.validateSingleDisposalTriageCheckYourAnswersPage
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.yeartodatelliability.YearToDateLiabilityControllerSpec._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, JustSubmittedReturn, SubmitReturnFailed, Subscribed}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.{AmountInPence, PaymentsJourney}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.{AgentReferenceNumber, CgtReference}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.AcquisitionDetailsAnswers.IncompleteAcquisitionDetailsAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.DisposalDetailsAnswers.IncompleteDisposalDetailsAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ExemptionAndLossesAnswers.IncompleteExemptionAndLossesAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ReliefDetailsAnswers.IncompleteReliefDetailsAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.IncompleteSingleDisposalTriageAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SubmitReturnResponse.ReturnCharge
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.YearToDateLiabilityAnswers.CalculatedYearToDateLiabilityAnswers.IncompleteCalculatedYearToDateLiabilityAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, JourneyStatus, LocalDateUtils, SessionData, UserType}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.{PaymentsService, ReturnsService}
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class CheckAllAnswersAndSubmitControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with ScalaCheckDrivenPropertyChecks
    with RedirectToStartBehaviour {

  val mockReturnsService = mock[ReturnsService]

  val mockPaymentsService = mock[PaymentsService]

  override val overrideBindings =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore),
      bind[ReturnsService].toInstance(mockReturnsService),
      bind[PaymentsService].toInstance(mockPaymentsService)
    )

  lazy val controller = instanceOf[CheckAllAnswersAndSubmitController]

  implicit val messagesApi: MessagesApi = controller.messagesApi

  def sessionWitJourney(journeyStatus: JourneyStatus): SessionData =
    SessionData.empty.copy(journeyStatus = Some(journeyStatus))

  def mockSubmitReturn(submitReturnRequest: SubmitReturnRequest)(response: Either[Error, SubmitReturnResponse]) =
    (mockReturnsService
      .submitReturn(_: SubmitReturnRequest)(_: HeaderCarrier))
      .expects(submitReturnRequest, *)
      .returning(EitherT.fromEither[Future](response))

  def mockStartPaymentJourney(
    cgtReference: CgtReference,
    chargeReference: Option[String],
    amount: AmountInPence,
    returnUrl: Call,
    backUrl: Call
  )(response: Either[Error, PaymentsJourney]) =
    (mockPaymentsService
      .startPaymentJourney(_: CgtReference, _: Option[String], _: AmountInPence, _: Call, _: Call)(
        _: HeaderCarrier,
        _: Request[_]
      ))
      .expects(cgtReference, chargeReference, amount, returnUrl, backUrl, *, *)
      .returning(EitherT.fromEither[Future](response))

  "CheckAllAnswersAndSubmitController" when {

    val completeReturn = sample[CompleteReturn]

    val completeDraftReturn = SingleDisposalDraftReturn(
      UUID.randomUUID(),
      completeReturn.triageAnswers,
      Some(completeReturn.propertyAddress),
      Some(completeReturn.disposalDetails),
      Some(completeReturn.acquisitionDetails),
      Some(completeReturn.reliefDetails),
      Some(completeReturn.exemptionsAndLossesDetails),
      Some(completeReturn.yearToDateLiabilityAnswers),
      completeReturn.initialGainOrLoss,
      None,
      LocalDateUtils.today()
    )

    val completeFillingOutReturn = sample[FillingOutReturn].copy(draftReturn = completeDraftReturn)

    "handling requests to display the check all answers page" must {

      def performAction(): Future[Result] = controller.checkAllAnswers()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(
        performAction, {
          case _: FillingOutReturn => true
          case _                   => false
        }
      )

      behave like incompleteJourneyBehaviour(performAction, completeDraftReturn)

      "display the page" when {

        "the return is complete" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionWitJourney(completeFillingOutReturn))
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("checkAllAnswers.title"), { doc =>
              validateAllCheckYourAnswersSections(doc, completeReturn, None, false, true)
              doc.select("#back").attr("href") shouldBe routes.TaskListController.taskList().url
              doc.select("#content > article > form").attr("action") shouldBe routes.CheckAllAnswersAndSubmitController
                .checkAllAnswersSubmit()
                .url
            }
          )
        }

        "the return is complete and the user is an agent" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWitJourney(
                completeFillingOutReturn.copy(agentReferenceNumber = Some(sample[AgentReferenceNumber]))
              ).copy(userType = Some(UserType.Agent))
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("checkAllAnswers.title"), { doc =>
              validateAllCheckYourAnswersSections(doc, completeReturn, Some(UserType.Agent), false, true)
              doc.select("#back").attr("href") shouldBe routes.TaskListController.taskList().url
              doc.select("#content > article > form").attr("action") shouldBe routes.CheckAllAnswersAndSubmitController
                .checkAllAnswersSubmit()
                .url
            }
          )
        }

      }

    }

    "handling submits on the check all answers page" must {

      def performAction(): Future[Result] = controller.checkAllAnswersSubmit()(FakeRequest())

      val submitReturnResponse = sample[SubmitReturnResponse]

      val justSubmittedReturn = JustSubmittedReturn(
        completeFillingOutReturn.subscribedDetails,
        completeFillingOutReturn.ggCredId,
        completeFillingOutReturn.agentReferenceNumber,
        completeReturn,
        submitReturnResponse
      )

      val submitReturnRequest = SubmitReturnRequest(
        completeReturn,
        completeFillingOutReturn.draftReturn.id,
        completeFillingOutReturn.subscribedDetails,
        completeFillingOutReturn.agentReferenceNumber
      )

      behave like redirectToStartWhenInvalidJourney(
        performAction, {
          case _: FillingOutReturn => true
          case _                   => false
        }
      )

      behave like incompleteJourneyBehaviour(performAction, completeDraftReturn)

      "show an error page" when {

        "there is an error updating the session after a successful submission" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionWitJourney(completeFillingOutReturn))
            mockSubmitReturn(submitReturnRequest)(Right(submitReturnResponse))
            mockStoreSession(sessionWitJourney(justSubmittedReturn))(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction())
        }

        "there is an error updating the session after a submission failure the return" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionWitJourney(completeFillingOutReturn))
            mockSubmitReturn(submitReturnRequest)(Left(Error("")))
            mockStoreSession(
              sessionWitJourney(
                SubmitReturnFailed(
                  completeFillingOutReturn.subscribedDetails,
                  completeFillingOutReturn.ggCredId,
                  completeFillingOutReturn.agentReferenceNumber
                )
              )
            )(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction())
        }

      }

      "redirect to the submission confirmation page" when {

        "the return has been submitted and the session has been updated" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionWitJourney(completeFillingOutReturn))
            mockSubmitReturn(submitReturnRequest)(Right(submitReturnResponse))
            mockStoreSession(sessionWitJourney(justSubmittedReturn))(Right(()))
          }

          checkIsRedirect(performAction(), routes.CheckAllAnswersAndSubmitController.confirmationOfSubmission())
        }

      }

      "redirect to the submission error page" when {
        "there is an error submitting the return" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionWitJourney(completeFillingOutReturn))
            mockSubmitReturn(submitReturnRequest)(Left(Error("")))
            mockStoreSession(
              sessionWitJourney(
                SubmitReturnFailed(
                  completeFillingOutReturn.subscribedDetails,
                  completeFillingOutReturn.ggCredId,
                  completeFillingOutReturn.agentReferenceNumber
                )
              )
            )(Right(()))
          }

          checkIsRedirect(performAction(), routes.CheckAllAnswersAndSubmitController.submissionError())
        }
      }

    }

    "handling requests to display the return submit failed page" must {

      def performAction(): Future[Result] = controller.submissionError()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(
        performAction, {
          case _: SubmitReturnFailed | _: Subscribed => true
          case _                                     => false
        }
      )

      "display the page" when {

        def test(journey: JourneyStatus): Unit =
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(journey)
              )
            )

            checkPageIsDisplayed(
              performAction(),
              messageFromMessageKey("submitReturnError.title"), { doc =>
                doc
                  .select("#content > article > form")
                  .attr("action") shouldBe routes.CheckAllAnswersAndSubmitController
                  .submissionErrorSubmit()
                  .url
              }
            )
          }

        "the user is in the SubmitReturnFailed state" in {
          test(sample[SubmitReturnFailed])
        }

        "the user is in the subscribed state" in {
          test(sample[Subscribed])
        }

      }

    }

    "handling submits on the return submit failed page" must {

      def performAction(): Future[Result] = controller.submissionErrorSubmit()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(
        performAction, {
          case _: SubmitReturnFailed | _: Subscribed => true
          case _                                     => false
        }
      )

      "redirect to the home page" when {

        def test(journey: JourneyStatus): Unit =
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(journey)
              )
            )

            checkIsRedirect(performAction(), homepage.routes.HomePageController.homepage())
          }

        "the user is in the SubmitReturnFailed state" in {
          test(sample[SubmitReturnFailed])
        }

        "the user is in the subscribed state" in {
          test(sample[Subscribed])
        }

      }

    }

    "handling requests to display the confirmation of submission page" must {

      def performAction(): Future[Result] = controller.confirmationOfSubmission()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(
        performAction, {
          case _: JustSubmittedReturn => true
          case _                      => false
        }
      )

      "display the page" in {
        val justSubmittedReturn = sample[JustSubmittedReturn]

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionWitJourney(justSubmittedReturn))
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("confirmationOfSubmission.title"), { doc =>
            doc.select("#content > article > div > p > a").attr("href") shouldBe viewConfig.selfAssessmentUrl
          }
        )
      }

    }

    "handling requests to pay a return" must {

      def performAction(): Future[Result] = controller.payReturn()(FakeRequest())

      def justSubmittedReturnWithCharge(charge: Option[ReturnCharge]): JustSubmittedReturn =
        sample[JustSubmittedReturn].copy(submissionResponse = sample[SubmitReturnResponse].copy(charge = charge))

      behave like redirectToStartWhenInvalidJourney(
        performAction, {
          case _: JustSubmittedReturn => true
          case _                      => false
        }
      )

      "redirect to the homepage" when {

        "there is no charge" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionWitJourney(justSubmittedReturnWithCharge(None)))
          }

          checkIsRedirect(performAction(), homepage.routes.HomePageController.homepage())
        }

      }

      "show an error page" when {

        "there is an error starting a payments journey" in {
          val charge              = sample[ReturnCharge]
          val justSubmittedReturn = justSubmittedReturnWithCharge(Some(charge))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionWitJourney(justSubmittedReturn))
            mockStartPaymentJourney(
              justSubmittedReturn.subscribedDetails.cgtReference,
              Some(charge.chargeReference),
              charge.amount,
              homepage.routes.HomePageController.homepage(),
              routes.CheckAllAnswersAndSubmitController.confirmationOfSubmission()
            )(Left(Error("")))

            checkIsTechnicalErrorPage(performAction())
          }

        }

      }

      "redirect to the payment journey next url" when {

        "the payments journey has been succesfulyl started" in {
          val paymentJourney      = PaymentsJourney("/next", "id")
          val charge              = sample[ReturnCharge]
          val justSubmittedReturn = justSubmittedReturnWithCharge(Some(charge))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionWitJourney(justSubmittedReturn))
            mockStartPaymentJourney(
              justSubmittedReturn.subscribedDetails.cgtReference,
              Some(charge.chargeReference),
              charge.amount,
              homepage.routes.HomePageController.homepage(),
              routes.CheckAllAnswersAndSubmitController.confirmationOfSubmission()
            )(Right(paymentJourney))

            checkIsRedirect(performAction(), paymentJourney.nextUrl)
          }

        }

      }

    }

  }

  def incompleteJourneyBehaviour(
    performAction: () => Future[Result],
    completeDraftReturn: SingleDisposalDraftReturn
  ) = {
    val makeIncompleteFunctions = List[SingleDisposalDraftReturn => SingleDisposalDraftReturn](
      _.copy(triageAnswers              = sample[IncompleteSingleDisposalTriageAnswers]),
      _.copy(propertyAddress            = None),
      _.copy(disposalDetailsAnswers     = Some(sample[IncompleteDisposalDetailsAnswers])),
      _.copy(disposalDetailsAnswers     = None),
      _.copy(acquisitionDetailsAnswers  = Some(sample[IncompleteAcquisitionDetailsAnswers])),
      _.copy(acquisitionDetailsAnswers  = None),
      _.copy(reliefDetailsAnswers       = Some(sample[IncompleteReliefDetailsAnswers])),
      _.copy(reliefDetailsAnswers       = None),
      _.copy(exemptionAndLossesAnswers  = Some(sample[IncompleteExemptionAndLossesAnswers])),
      _.copy(exemptionAndLossesAnswers  = None),
      _.copy(yearToDateLiabilityAnswers = Some(sample[IncompleteCalculatedYearToDateLiabilityAnswers])),
      _.copy(yearToDateLiabilityAnswers = None)
    )

    "redirect to the task list" when {

      "the return is not complete" in {
        makeIncompleteFunctions.foreach { makeIncomplete =>
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWitJourney(
                sample[FillingOutReturn].copy(
                  draftReturn = makeIncomplete(completeDraftReturn)
                )
              )
            )
          }

          checkIsRedirect(performAction(), routes.TaskListController.taskList())
        }

      }

    }

  }

}

object CheckAllAnswersAndSubmitControllerSpec {
  def validateAllCheckYourAnswersSections(
    doc: Document,
    completeReturn: CompleteReturn,
    userType: Option[UserType],
    isUk: Boolean,
    isRebasing: Boolean
  )(implicit messages: MessagesApi, lang: Lang): Unit = {
    validateSingleDisposalTriageCheckYourAnswersPage(
      completeReturn.triageAnswers,
      userType,
      doc
    )

    validateAcquisitionDetailsCheckYourAnswersPage(
      completeReturn.acquisitionDetails,
      doc,
      isUk,
      isRebasing
    )

    validateDisposalDetailsCheckYourAnswersPage(
      completeReturn.disposalDetails,
      doc
    )
    validateReliefDetailsCheckYourAnswersPage(
      completeReturn.reliefDetails,
      doc
    )
    validateExemptionAndLossesCheckYourAnswersPage(
      completeReturn.exemptionsAndLossesDetails,
      doc
    )
    validateCalculatedYearToDateLiabilityPage(
      completeReturn.yearToDateLiabilityAnswers,
      doc
    )
  }
}
