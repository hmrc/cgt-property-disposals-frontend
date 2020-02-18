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

import cats.data.EitherT
import cats.instances.future._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.i18n.MessagesApi
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.{Call, Result}
import play.api.test.FakeRequest
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.accounts.homepage
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, JustSubmittedReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.CgtReference
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.AcquisitionDetailsAnswers.IncompleteAcquisitionDetailsAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.DisposalDetailsAnswers.IncompleteDisposalDetailsAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ExemptionAndLossesAnswers.IncompleteExemptionAndLossesAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.IndividualTriageAnswers.IncompleteIndividualTriageAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ReliefDetailsAnswers.IncompleteReliefDetailsAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.YearToDateLiabilityAnswers.IncompleteYearToDateLiabilityAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{CompleteReturn, DraftReturn, PaymentsJourney, SubmitReturnResponse}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{AmountInPence, Error, JourneyStatus, SessionData}
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

  def mockSubmitReturn(completeReturn: CompleteReturn)(response: Either[Error, SubmitReturnResponse]) =
    (mockReturnsService
      .submitReturn(_: CompleteReturn)(_: HeaderCarrier))
      .expects(completeReturn, *)
      .returning(EitherT.fromEither[Future](response))

  def mockStartPaymentJourney(
    cgtReference: CgtReference,
    chargeReference: String,
    amount: AmountInPence,
    returnUrl: Call,
    backUrl: Call
  )(response: Either[Error, PaymentsJourney]) =
    (mockPaymentsService
      .startPaymentJourney(_: CgtReference, _: String, _: AmountInPence, _: Call, _: Call)(_: HeaderCarrier))
      .expects(cgtReference, chargeReference, amount, returnUrl, backUrl, *)
      .returning(EitherT.fromEither[Future](response))

  "CheckAllAnswersAndSubmitController" when {

    val completeReturn = sample[CompleteReturn]

    val completeDraftReturn = DraftReturn(
      completeReturn.id,
      completeReturn.cgtReference,
      completeReturn.triageAnswers,
      Some(completeReturn.propertyAddress),
      Some(completeReturn.disposalDetails),
      Some(completeReturn.acquisitionDetails),
      Some(completeReturn.reliefDetails),
      Some(completeReturn.exemptionsAndLossesDetails),
      Some(completeReturn.yearToDateLiabilityAnswers)
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

      behave like redirectToStartWhenInvalidJourney(
        performAction, {
          case _: FillingOutReturn => true
          case _                   => false
        }
      )

      behave like incompleteJourneyBehaviour(performAction, completeDraftReturn)

      "show an error page" when {

        "there is an error submitting the return" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionWitJourney(completeFillingOutReturn))
            mockSubmitReturn(completeReturn)(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction())
        }

        "there is an error updating the session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionWitJourney(completeFillingOutReturn))
            mockSubmitReturn(completeReturn)(Right(submitReturnResponse))
            mockStoreSession(sessionWitJourney(justSubmittedReturn))(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction())
        }

      }

      "redirect to the submission confirmation page" when {

        "the return has been submitted and the session has been updated" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionWitJourney(completeFillingOutReturn))
            mockSubmitReturn(completeReturn)(Right(submitReturnResponse))
            mockStoreSession(sessionWitJourney(justSubmittedReturn))(Right(()))
          }

          checkIsRedirect(performAction(), routes.CheckAllAnswersAndSubmitController.confirmationOfSubmission())
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

        checkPageIsDisplayed(performAction(), messageFromMessageKey("confirmationOfSubmission.title"))
      }

    }

    "handling requests to pay a return" must {

      def performAction(): Future[Result] = controller.payReturn()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(
        performAction, {
          case _: JustSubmittedReturn => true
          case _                      => false
        }
      )

      "show an error page" when {

        "there is an error starting a payments journey" in {
          val justSubmittedReturn = sample[JustSubmittedReturn]

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionWitJourney(justSubmittedReturn))
            mockStartPaymentJourney(
              justSubmittedReturn.completeReturn.cgtReference,
              justSubmittedReturn.submissionResponse.chargeReference,
              justSubmittedReturn.submissionResponse.amount,
              routes.CheckAllAnswersAndSubmitController.confirmationOfSubmission(),
              homepage.routes.HomePageController.homepage()
            )(Left(Error("")))

            checkIsTechnicalErrorPage(performAction())
          }

        }

      }

      "redirect to the payment journey next url" when {

        "the payments journey has been succesfulyl started" in {
          val justSubmittedReturn = sample[JustSubmittedReturn]
          val paymentJourney      = PaymentsJourney("/next", "id")

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionWitJourney(justSubmittedReturn))
            mockStartPaymentJourney(
              justSubmittedReturn.completeReturn.cgtReference,
              justSubmittedReturn.submissionResponse.chargeReference,
              justSubmittedReturn.submissionResponse.amount,
              routes.CheckAllAnswersAndSubmitController.confirmationOfSubmission(),
              homepage.routes.HomePageController.homepage()
            )(Right(paymentJourney))

            checkIsRedirect(performAction(), paymentJourney.nextUrl)
          }

        }

      }

    }

  }

  def incompleteJourneyBehaviour(performAction: () => Future[Result], completeDraftReturn: DraftReturn) = {
    val makeIncompleteFunctions = List[DraftReturn => DraftReturn](
      _.copy(triageAnswers              = sample[IncompleteIndividualTriageAnswers]),
      _.copy(propertyAddress            = None),
      _.copy(disposalDetailsAnswers     = Some(sample[IncompleteDisposalDetailsAnswers])),
      _.copy(disposalDetailsAnswers     = None),
      _.copy(acquisitionDetailsAnswers  = Some(sample[IncompleteAcquisitionDetailsAnswers])),
      _.copy(acquisitionDetailsAnswers  = None),
      _.copy(reliefDetailsAnswers       = Some(sample[IncompleteReliefDetailsAnswers])),
      _.copy(reliefDetailsAnswers       = None),
      _.copy(exemptionAndLossesAnswers  = Some(sample[IncompleteExemptionAndLossesAnswers])),
      _.copy(exemptionAndLossesAnswers  = None),
      _.copy(yearToDateLiabilityAnswers = Some(sample[IncompleteYearToDateLiabilityAnswers])),
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
