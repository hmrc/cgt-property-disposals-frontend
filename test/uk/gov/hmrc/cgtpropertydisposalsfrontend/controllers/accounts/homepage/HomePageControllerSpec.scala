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
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers.{contentAsString, _}
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.accounts.homepage.privatebeta.PrivateBetaHomePageController
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, StartingNewDraftReturn, Subscribed}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.CgtReference
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.DraftReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.IndividualTriageAnswers.IncompleteIndividualTriageAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, SessionData, UserType}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

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

  override val overrideBindings =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore),
      bind[ReturnsService].toInstance(mockReturnsService)
    )

  val subscribed = sample[Subscribed]

  def mockGetDraftReturns(cgtReference: CgtReference)(response: Either[Error, List[DraftReturn]]) =
    (mockReturnsService
      .getDraftReturns(_: CgtReference)(_: HeaderCarrier))
      .expects(cgtReference, *)
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
          case _: Subscribed | _: StartingNewDraftReturn | _: FillingOutReturn => true
          case _                                                               => false
        }
      )

      "display the home page" in {
        forAll { userType: Option[UserType] =>
          whenever(!userType.contains(UserType.Agent)) {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(Future.successful(Right(Some(subscribedSessionData.copy(userType = userType)))))
            }

            val result  = performAction()
            val content = contentAsString(result)

            status(result) shouldBe OK
            content        should include(messageFromMessageKey("account.home.title"))
            content        should include(messageFromMessageKey("account.home.button.start-a-new-return"))
            content shouldNot include(
              messageFromMessageKey(
                "account.home.subtitle.agent",
                subscribed.subscribedDetails.makeAccountName(),
                subscribed.subscribedDetails.cgtReference.value
              )
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
          mockGetSession(Future.successful(Right(Some(sessionData))))
        }

        val result  = performAction()
        val content = contentAsString(result)

        status(result) shouldBe OK
        content        should include(messageFromMessageKey("account.home.title"))
        content should include(
          messageFromMessageKey(
            "account.home.subtitle.agent",
            subscribed.subscribedDetails.makeAccountName(),
            subscribed.subscribedDetails.cgtReference.value
          )
        )
        content should include(
          messageFromMessageKey(
            "account.home.subtitle.agent",
            subscribed.subscribedDetails.makeAccountName(),
            subscribed.subscribedDetails.cgtReference.value
          )
        )
      }

      val startingNewDraftReturn = StartingNewDraftReturn(
        subscribed.subscribedDetails,
        subscribed.ggCredId,
        subscribed.agentReferenceNumber,
        sample[IncompleteIndividualTriageAnswers]
      )

      val fillingOurReturn = FillingOutReturn(
        subscribed.subscribedDetails,
        subscribed.ggCredId,
        subscribed.agentReferenceNumber,
        sample[DraftReturn]
      )

      List(startingNewDraftReturn, fillingOurReturn).foreach { journeyStatus =>
        s"convert a ${journeyStatus.getClass.getSimpleName} to Subscribed journey status" in {

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              Future.successful(
                Right(
                  Some(
                    SessionData.empty.copy(
                      journeyStatus = Some(journeyStatus),
                      userType      = Some(UserType.Individual)
                    )
                  )
                )
              )
            )
            mockGetDraftReturns(subscribed.subscribedDetails.cgtReference)(Right(subscribed.draftReturns))
            mockStoreSession(
              SessionData.empty.copy(
                journeyStatus = Some(subscribed),
                userType      = Some(UserType.Individual)
              )
            )(Future.successful(Right(())))
          }

          val result = performAction()
          status(result)          shouldBe OK
          contentAsString(result) should include(messageFromMessageKey("account.home.title"))
        }

        "show an error page" when {

          s"the conversion from ${journeyStatus.getClass.getSimpleName} is unsuccessful" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                Future.successful(
                  Right(
                    Some(
                      SessionData.empty.copy(
                        journeyStatus = Some(journeyStatus),
                        userType      = Some(UserType.Individual)
                      )
                    )
                  )
                )
              )
              mockGetDraftReturns(subscribed.subscribedDetails.cgtReference)(Right(subscribed.draftReturns))
              mockStoreSession(
                SessionData.empty.copy(
                  journeyStatus = Some(subscribed),
                  userType      = Some(UserType.Individual)
                )
              )(Future.successful(Left(Error(""))))
            }

            checkIsTechnicalErrorPage(performAction())
          }

          s"the conversion from ${journeyStatus.getClass.getSimpleName} is successful but " +
            "there is an error getting the draft returns" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                Future.successful(
                  Right(
                    Some(
                      SessionData.empty.copy(
                        journeyStatus = Some(journeyStatus),
                        userType      = Some(UserType.Individual)
                      )
                    )
                  )
                )
              )
              mockGetDraftReturns(subscribed.subscribedDetails.cgtReference)(Left(Error("")))
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
                  mockGetSession(Future.successful(Right(Some(subscribedSessionData.copy(userType = userType)))))
                }

                checkIsTechnicalErrorPage(performAction())
              }
            }
          }
        }

        "there is an error updating the session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              Future.successful(Right(Some(subscribedSessionData.copy(userType = Some(UserType.Individual)))))
            )
            mockStoreSession(
              subscribedSessionData.copy(
                journeyStatus = Some(
                  StartingNewDraftReturn(
                    subscribed.subscribedDetails,
                    subscribed.ggCredId,
                    subscribed.agentReferenceNumber,
                    IncompleteIndividualTriageAnswers.empty
                  )
                ),
                userType = Some(UserType.Individual)
              )
            )(Future.successful(Left(Error(""))))
          }

          checkIsTechnicalErrorPage(performAction())
        }
      }

      "redirect to the who is the individual reporting for page" when {

        "the user type is individual" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              Future.successful(Right(Some(subscribedSessionData.copy(userType = Some(UserType.Individual)))))
            )
            mockStoreSession(
              subscribedSessionData.copy(
                journeyStatus = Some(
                  StartingNewDraftReturn(
                    subscribed.subscribedDetails,
                    subscribed.ggCredId,
                    subscribed.agentReferenceNumber,
                    IncompleteIndividualTriageAnswers.empty
                  )
                ),
                userType = Some(UserType.Individual)
              )
            )(Future.successful(Right(())))
          }

          checkIsRedirect(
            performAction(),
            controllers.returns.triage.routes.CanTheyUseOurServiceController.whoIsIndividualRepresenting()
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
            mockGetSession(Future.successful(Right(Some(sessionWithSubscribed))))
          }

          checkIsTechnicalErrorPage(performAction(UUID.randomUUID()))
        }

        "there is an error updating the session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionWithSubscribed))))
            mockStoreSession(SessionData.empty.copy(journeyStatus = Some(fillingOutReturn)))(
              Future.successful(Left(Error("")))
            )
          }

          checkIsTechnicalErrorPage(performAction(draftReturn.id))
        }
      }

      "redirect to the task list page" when {

        "a draft return can be found with the given id and the session is successfully updated" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionWithSubscribed))))
            mockStoreSession(SessionData.empty.copy(journeyStatus = Some(fillingOutReturn)))(
              Future.successful(Right(()))
            )
          }

          checkIsRedirect(performAction(draftReturn.id), controllers.returns.routes.TaskListController.taskList())
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
          mockGetSession(Future.successful(Right(Some(sessionData))))
        }

        val result = performAction()
        status(result)          shouldBe OK
        contentAsString(result) should include(messageFromMessageKey("account.home.title"))
        contentAsString(result) shouldNot include(messageFromMessageKey("account.home.button.start-a-new-return"))
      }

    }

  }

}
