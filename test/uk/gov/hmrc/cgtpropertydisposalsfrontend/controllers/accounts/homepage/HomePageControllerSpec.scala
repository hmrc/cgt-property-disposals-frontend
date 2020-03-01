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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.accounts.homepage

import java.util.UUID

import cats.data.EitherT
import cats.instances.future._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.Configuration
import play.api.i18n.MessagesApi
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.libs.json.{JsNumber, JsString, JsValue}
import play.api.mvc.{Call, Result}
import play.api.test.FakeRequest
import play.api.test.Helpers.{contentAsString, _}
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.accounts.homepage.privatebeta.PrivateBetaHomePageController
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, JustSubmittedReturn, StartingNewDraftReturn, Subscribed, ViewingReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.{AmountInPence, PaymentsJourney}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.CgtReference
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.IncompleteSingleDisposalTriageAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{CompleteReturn, DraftReturn, ReturnSummary, SubmitReturnResponse}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, SessionData, UserType}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.{PaymentsService, ReturnsService}
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

trait HomePageControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with ScalaCheckDrivenPropertyChecks
    with RedirectToStartBehaviour {

  override lazy val additionalConfig: Configuration = Configuration(
    "application.router" -> "prod.Routes"
  )

  val mockReturnsService = mock[ReturnsService]

  val mockPaymentsService = mock[PaymentsService]

  override val overrideBindings =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore),
      bind[ReturnsService].toInstance(mockReturnsService),
      bind[PaymentsService].toInstance(mockPaymentsService)
    )

  val subscribed = sample[Subscribed]

  def mockGetDraftReturns(cgtReference: CgtReference)(response: Either[Error, List[DraftReturn]]) =
    (mockReturnsService
      .getDraftReturns(_: CgtReference)(_: HeaderCarrier))
      .expects(cgtReference, *)
      .returning(EitherT.fromEither[Future](response))

  def mockGetReturnsList(cgtReference: CgtReference)(
    response: Either[Error, List[ReturnSummary]]
  ) =
    (mockReturnsService
      .listReturns(_: CgtReference)(_: HeaderCarrier))
      .expects(cgtReference, *)
      .returning(EitherT.fromEither[Future](response))

  def mockDisplayReturn(cgtReference: CgtReference, submissionId: String)(response: Either[Error, JsValue]) =
    (mockReturnsService
      .displayReturn(_: CgtReference, _: String)(_: HeaderCarrier))
      .expects(cgtReference, submissionId, *)
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

}

class PublicBetaHomePageControllerSpec extends HomePageControllerSpec {

  override lazy val additionalConfig: Configuration = Configuration(
    "application.router" -> "prod.Routes"
  )

  lazy val controller = instanceOf[HomePageController]

  implicit val messagesApi: MessagesApi = controller.messagesApi

  val subscribedSessionData = SessionData.empty.copy(journeyStatus = Some(subscribed))

  "The HomePage Controller" when {

    "handling requests for account home" must {

      def performAction(): Future[Result] = controller.homepage()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(
        performAction, {
          case _: Subscribed | _: StartingNewDraftReturn | _: FillingOutReturn | _: JustSubmittedReturn |
              _: ViewingReturn =>
            true
          case _ => false
        }
      )

      "display the home page" in {
        forAll { userType: Option[UserType] =>
          whenever(!userType.contains(UserType.Agent)) {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(subscribedSessionData.copy(userType = userType))
            }

            val result  = performAction()
            val content = contentAsString(result)

            status(result) shouldBe OK
            content        should include(messageFromMessageKey("account.home.title"))
            content        should include(messageFromMessageKey("account.home.button.start-a-new-return"))
            content shouldNot include(
              messageFromMessageKey("account.agent.prefix")
            )
            content should include(
              messageFromMessageKey(
                "account.home.subtitle",
                subscribed.subscribedDetails.cgtReference.value
              )
            )
          }
        }
      }

      "display the home page for agents" in {
        val sessionData =
          SessionData.empty.copy(journeyStatus = Some(subscribed), userType = Some(UserType.Agent))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionData)
        }

        val result  = performAction()
        val content = contentAsString(result)

        status(result) shouldBe OK
        content        should include(messageFromMessageKey("account.home.title"))
        content should include(
          messageFromMessageKey("account.agent.prefix")
        )
        content should include(subscribed.subscribedDetails.makeAccountName())
      }

      val startingNewDraftReturn = StartingNewDraftReturn(
        subscribed.subscribedDetails,
        subscribed.ggCredId,
        subscribed.agentReferenceNumber,
        Right(sample[IncompleteSingleDisposalTriageAnswers])
      )

      val fillingOurReturn = FillingOutReturn(
        subscribed.subscribedDetails,
        subscribed.ggCredId,
        subscribed.agentReferenceNumber,
        sample[DraftReturn]
      )
      val justSubmittedReturn = JustSubmittedReturn(
        subscribed.subscribedDetails,
        subscribed.ggCredId,
        subscribed.agentReferenceNumber,
        sample[CompleteReturn],
        sample[SubmitReturnResponse]
      )

      val viewingReturn = ViewingReturn(
        subscribed.subscribedDetails,
        subscribed.ggCredId,
        subscribed.agentReferenceNumber,
        JsString("")
      )

      List(startingNewDraftReturn, fillingOurReturn, justSubmittedReturn, viewingReturn).foreach { journeyStatus =>
        s"convert a ${journeyStatus.getClass.getSimpleName} to Subscribed journey status" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(journeyStatus),
                userType      = Some(UserType.Individual)
              )
            )
            mockGetDraftReturns(subscribed.subscribedDetails.cgtReference)(Right(subscribed.draftReturns))
            mockGetReturnsList(subscribed.subscribedDetails.cgtReference)(Right(subscribed.sentReturns))
            mockStoreSession(
              SessionData.empty.copy(
                journeyStatus = Some(subscribed),
                userType      = Some(UserType.Individual)
              )
            )(Right(()))
          }

          val result = performAction()
          status(result)          shouldBe OK
          contentAsString(result) should include(messageFromMessageKey("account.home.title"))
        }

        "show an error page" when {

          s"the conversion from ${journeyStatus.getClass.getSimpleName} is successful but " +
            "there is an error updating the session" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                SessionData.empty.copy(
                  journeyStatus = Some(journeyStatus),
                  userType      = Some(UserType.Individual)
                )
              )
              mockGetDraftReturns(subscribed.subscribedDetails.cgtReference)(Right(subscribed.draftReturns))
              mockGetReturnsList(subscribed.subscribedDetails.cgtReference)(Right(subscribed.sentReturns))
              mockStoreSession(
                SessionData.empty.copy(
                  journeyStatus = Some(subscribed),
                  userType      = Some(UserType.Individual)
                )
              )(Left(Error("")))
            }

            checkIsTechnicalErrorPage(performAction())
          }

          s"the conversion from ${journeyStatus.getClass.getSimpleName} is successful but " +
            "there is an error getting the draft returns" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                SessionData.empty.copy(
                  journeyStatus = Some(journeyStatus),
                  userType      = Some(UserType.Individual)
                )
              )
              mockGetDraftReturns(subscribed.subscribedDetails.cgtReference)(Left(Error("")))
            }

            checkIsTechnicalErrorPage(performAction())
          }

          s"the conversion from ${journeyStatus.getClass.getSimpleName} is successful but " +
            "there is an error getting the list of returns" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                SessionData.empty.copy(
                  journeyStatus = Some(journeyStatus),
                  userType      = Some(UserType.Individual)
                )
              )
              mockGetDraftReturns(subscribed.subscribedDetails.cgtReference)(Right(subscribed.draftReturns))
              mockGetReturnsList(subscribed.subscribedDetails.cgtReference)(Left(Error("")))
            }

            checkIsTechnicalErrorPage(performAction())
          }

        }

      }

    }

    "handling requests to start a new return" must {

      def performAction(): Future[Result] = controller.startNewReturn()(FakeRequest())

      redirectToStartWhenInvalidJourney(
        performAction, {
          case _: Subscribed => true
          case _             => false
        }
      )

      "show an error page" when {

        "the user type is not valid" in {
          forAll { userType: Option[UserType] =>
            whenever(!userType.contains(UserType.Individual)) {
              withClue(s"For user type '$userType': ") {
                inSequence {
                  mockAuthWithNoRetrievals()
                  mockGetSession(subscribedSessionData.copy(userType = userType))
                }

                checkIsTechnicalErrorPage(performAction())
              }
            }
          }
        }

        "there is an error updating the session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(subscribedSessionData.copy(userType = Some(UserType.Individual)))
            mockStoreSession(
              subscribedSessionData.copy(
                journeyStatus = Some(
                  StartingNewDraftReturn(
                    subscribed.subscribedDetails,
                    subscribed.ggCredId,
                    subscribed.agentReferenceNumber,
                    Right(IncompleteSingleDisposalTriageAnswers.empty)
                  )
                ),
                userType = Some(UserType.Individual)
              )
            )(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction())
        }
      }

      "redirect to the who is the individual reporting for page" when {

        "the user type is individual" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(subscribedSessionData.copy(userType = Some(UserType.Individual)))
            mockStoreSession(
              subscribedSessionData.copy(
                journeyStatus = Some(
                  StartingNewDraftReturn(
                    subscribed.subscribedDetails,
                    subscribed.ggCredId,
                    subscribed.agentReferenceNumber,
                    Right(IncompleteSingleDisposalTriageAnswers.empty)
                  )
                ),
                userType = Some(UserType.Individual)
              )
            )(Right(()))
          }

          checkIsRedirect(
            performAction(),
            controllers.returns.triage.routes.InitialTriageQuestionsController.whoIsIndividualRepresenting()
          )
        }

      }

    }

    "handling requests to resume a draft return" must {

      def performAction(id: UUID): Future[Result] = controller.resumeDraftReturn(id)(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(() => performAction(UUID.randomUUID()), {
        case _: Subscribed => true
        case _             => false
      })

      val draftReturn = sample[DraftReturn]

      val subscribed = sample[Subscribed].copy(draftReturns = List(draftReturn))

      val sessionWithSubscribed = SessionData.empty.copy(journeyStatus = Some(subscribed))

      val fillingOutReturn = FillingOutReturn(
        subscribed.subscribedDetails,
        subscribed.ggCredId,
        subscribed.agentReferenceNumber,
        draftReturn
      )

      "show an error page" when {

        "no draft return can be found with the given id" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionWithSubscribed)
          }

          checkIsTechnicalErrorPage(performAction(UUID.randomUUID()))
        }

        "there is an error updating the session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionWithSubscribed)
            mockStoreSession(SessionData.empty.copy(journeyStatus = Some(fillingOutReturn)))(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction(draftReturn.id))
        }
      }

      "redirect to the task list page" when {

        "a draft return can be found with the given id and the session is successfully updated" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionWithSubscribed)
            mockStoreSession(SessionData.empty.copy(journeyStatus = Some(fillingOutReturn)))(Right(()))
          }

          checkIsRedirect(performAction(draftReturn.id), controllers.returns.routes.TaskListController.taskList())
        }

      }

    }

    "handling requests to view a return" must {

      def performAction(submissionId: String): Future[Result] =
        controller.viewSentReturn(submissionId)(FakeRequest())

      val sentReturn  = sample[ReturnSummary]
      val subscribed  = sample[Subscribed].copy(sentReturns = List(sentReturn))
      val sessionData = SessionData.empty.copy(journeyStatus = Some(subscribed))

      redirectToStartWhenInvalidJourney(
        () => performAction(""), {
          case _: Subscribed => true
          case _             => false
        }
      )

      "return a bad request" when {

        "the user does not have a return with the given submission id" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionData)
          }

          status(performAction(sentReturn.submissionId + "abc")) shouldBe NOT_FOUND
        }

      }

      "show an error page" when {

        "there is an error getting the return" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionData)
            mockDisplayReturn(subscribed.subscribedDetails.cgtReference, sentReturn.submissionId)(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction(sentReturn.submissionId))
        }

        "there is an error updating the session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionData)
            mockDisplayReturn(subscribed.subscribedDetails.cgtReference, sentReturn.submissionId)(Right(JsNumber(1)))
            mockStoreSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  ViewingReturn(
                    subscribed.subscribedDetails,
                    subscribed.ggCredId,
                    subscribed.agentReferenceNumber,
                    JsNumber(1)
                  )
                )
              )
            )(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction(sentReturn.submissionId))
        }

      }

      "redirect to the view return screen" when {

        "the return is successfully retrieved and the session is updated" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionData)
            mockDisplayReturn(subscribed.subscribedDetails.cgtReference, sentReturn.submissionId)(Right(JsNumber(1)))
            mockStoreSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  ViewingReturn(
                    subscribed.subscribedDetails,
                    subscribed.ggCredId,
                    subscribed.agentReferenceNumber,
                    JsNumber(1)
                  )
                )
              )
            )(Right(()))
          }

          checkIsRedirect(
            performAction(sentReturn.submissionId),
            controllers.returns.routes.ViewReturnController.displayReturn()
          )
        }

      }

    }

    "handling requests to pay a return" must {

      def performAction(): Future[Result] = controller.payTotalAmountLeftToPay()(FakeRequest())

      redirectToStartWhenInvalidJourney(
        performAction, {
          case _: Subscribed => true
          case _             => false
        }
      )

      "show an error page" when {

        "there is an error starting a payments journey" in {
          val subscribed = sample[Subscribed]

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData.empty.copy(journeyStatus = Some(subscribed)))
            mockStartPaymentJourney(
              subscribed.subscribedDetails.cgtReference,
              subscribed.subscribedDetails.cgtReference.value,
              subscribed.totalLeftToPay(),
              routes.HomePageController.homepage(),
              routes.HomePageController.homepage()
            )(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction())
        }

      }

      "redirect to the payment journey" when {

        "the payment journey is successfully started" in {
          val subscribed      = sample[Subscribed]
          val paymentsJourney = sample[PaymentsJourney]

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData.empty.copy(journeyStatus = Some(subscribed)))
            mockStartPaymentJourney(
              subscribed.subscribedDetails.cgtReference,
              subscribed.subscribedDetails.cgtReference.value,
              subscribed.totalLeftToPay(),
              routes.HomePageController.homepage(),
              routes.HomePageController.homepage()
            )(Right(paymentsJourney))
          }

          checkIsRedirect(performAction(), paymentsJourney.nextUrl)
        }

      }

    }

  }

}

class PrivateBetaHomePageControllerSpec extends HomePageControllerSpec {

  override lazy val additionalConfig: Configuration = Configuration(
    "application.router" -> "private_beta.Routes"
  )

  lazy val controller = instanceOf[PrivateBetaHomePageController]

  implicit val messagesApi: MessagesApi = controller.messagesApi

  def redirectToStartBehaviour(performAction: () => Future[Result]): Unit =
    redirectToStartWhenInvalidJourney(
      performAction, {
        case _: Subscribed => true
        case _             => false
      }
    )

  "The HomePage Controller" when {

    "handling requests for account home for private beta" must {

      def performAction(): Future[Result] = controller.privateBetaHomepage()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      "display the home page" in {
        val sessionData =
          SessionData.empty.copy(journeyStatus = Some(subscribed))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionData)
        }

        val result  = performAction()
        val content = contentAsString(result)

        status(result) shouldBe OK
        content shouldNot include(messageFromMessageKey("account.agent.prefix"))
        content should include(messageFromMessageKey("account.home.title"))
        content shouldNot include(messageFromMessageKey("account.home.button.start-a-new-return"))
      }

      "display the home page for agents" in {
        val sessionData =
          SessionData.empty.copy(journeyStatus = Some(subscribed), userType = Some(UserType.Agent))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionData)
        }

        val result  = performAction()
        val content = contentAsString(result)

        status(result) shouldBe OK
        content        should include(messageFromMessageKey("account.home.title"))
        content        should include(messageFromMessageKey("account.agent.prefix"))
        content        should include(subscribed.subscribedDetails.makeAccountName())
      }

    }

  }

}
