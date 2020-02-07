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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding

import java.util.UUID

import cats.data.EitherT
import cats.instances.future._
import play.api.i18n.MessagesApi
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.SubscriptionStatus.TryingToGetIndividualsFootprint
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.GGCredId
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.iv.IvErrorStatus
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.IndividualName
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, SessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.onboarding.IvService
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class IvControllerSpec extends ControllerSpec with AuthSupport with SessionSupport {

  val mockIvService = mock[IvService]

  override val overrideBindings =
    List[GuiceableModule](
      bind[IvService].toInstance(mockIvService),
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore)
    )

  lazy val controller = instanceOf[IvController]

  lazy implicit val messagesApi: MessagesApi = controller.messagesApi

  val name = sample[IndividualName]

  def mockGetFailedJourneyStatus(journeyId: UUID)(result: Either[Error, IvErrorStatus]) =
    (mockIvService
      .getFailedJourneyStatus(_: UUID)(_: HeaderCarrier))
      .expects(journeyId, *)
      .returning(EitherT.fromEither[Future](result))

  "IvController" when {

    "handling IV success request" must {

      def performAction(): Future[Result] = controller.ivSuccessCallback()(FakeRequest())

      val nonEmptySession =
        SessionData.empty.copy(
          journeyStatus = Some(TryingToGetIndividualsFootprint(None, None, None, sample[GGCredId]))
        )

      "clear the session and redirect to the start endpoint" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(Future.successful(Right(Some(nonEmptySession))))
          mockStoreSession(SessionData.empty)(Future.successful(Right(())))
        }

        checkIsRedirect(performAction(), controllers.routes.StartController.start())
      }

      "show an error page if there is an error clearing the session" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(Future.successful(Right(Some(nonEmptySession))))
          mockStoreSession(SessionData.empty)(Future.successful(Left(Error(""))))
        }

        checkIsTechnicalErrorPage(performAction())
      }

      "not update the session" when {

        "there is no session data" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(None)))
          }

          checkIsRedirect(performAction(), controllers.routes.StartController.start())
        }

        "the session data is empty" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(SessionData.empty))))
          }

          checkIsRedirect(performAction(), controllers.routes.StartController.start())
        }

      }

    }

    "handling IV failure requests" must {

      val journeyId = UUID.randomUUID()

      def performAction(): Future[Result] =
        controller.ivFailureCallback(journeyId)(FakeRequest())

      "redirect to the IV error page" when {

        "there is an error querying the journey status" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(None)))
            mockGetFailedJourneyStatus(journeyId)(Left(Error("")))
          }

          checkIsRedirect(performAction(), controllers.onboarding.routes.IvController.getTechnicalIssue())
        }

      }

      List(
        IvErrorStatus.Incomplete     -> (() => controllers.onboarding.routes.IvController.getTechnicalIssue()),
        IvErrorStatus.FailedMatching -> (() => controllers.onboarding.routes.IvController.getFailedMatching()),
        IvErrorStatus.FailedIV       -> (() => controllers.onboarding.routes.IvController.getFailedIV()),
        IvErrorStatus.InsufficientEvidence -> (
          () => controllers.onboarding.routes.IvController.getInsufficientEvidence()
        ),
        IvErrorStatus.LockedOut          -> (() => controllers.onboarding.routes.IvController.getLockedOut()),
        IvErrorStatus.UserAborted        -> (() => controllers.onboarding.routes.IvController.getUserAborted()),
        IvErrorStatus.Timeout            -> (() => controllers.onboarding.routes.IvController.getTimedOut()),
        IvErrorStatus.TechnicalIssue     -> (() => controllers.onboarding.routes.IvController.getTechnicalIssue()),
        IvErrorStatus.PreconditionFailed -> (() => controllers.onboarding.routes.IvController.getPreconditionFailed()),
        IvErrorStatus.Unknown("")        -> (() => controllers.onboarding.routes.IvController.getTechnicalIssue())
      ).foreach {
        case (status, redirectTo) =>
          s"redirect to ${redirectTo().url}" when {

            s"the iv error status is $status" in {
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(Future.successful(Right(None)))
                mockGetFailedJourneyStatus(journeyId)(Right(status))
              }

              checkIsRedirect(performAction(), redirectTo())
            }

          }
      }

    }

    "handling requests to see the failed matching page" must {

      "display the page" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(Future.successful(Right(Some(SessionData.empty))))
        }

        val result = controller.getFailedMatching()(FakeRequest())
        status(result)          shouldBe OK
        contentAsString(result) should include(messagefromMessageKey("iv.failedMatching.title"))
      }

    }

    "handling requests to see the failed iv page" must {

      "display the page" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(Future.successful(Right(Some(SessionData.empty))))
        }

        val result = controller.getFailedIV()(FakeRequest())
        status(result)          shouldBe OK
        contentAsString(result) should include(messagefromMessageKey("iv.failedIv.title"))
      }

    }

    "handling requests to see the insufficient evidence page" must {

      "display the page" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(Future.successful(Right(Some(SessionData.empty))))
        }

        val result = controller.getInsufficientEvidence()(FakeRequest())
        status(result)          shouldBe OK
        contentAsString(result) should include(messagefromMessageKey("iv.insufficientEvidence.title"))
      }

    }

    "handling requests to see the locked out page" must {

      "display the page" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(Future.successful(Right(Some(SessionData.empty))))
        }

        val result = controller.getLockedOut()(FakeRequest())
        status(result)          shouldBe OK
        contentAsString(result) should include(messagefromMessageKey("iv.lockedOut.title"))

      }

    }

    "handling requests to see the user aborted page" must {

      "display the page" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(Future.successful(Right(Some(SessionData.empty))))
        }

        val result = controller.getUserAborted()(FakeRequest())
        status(result)          shouldBe OK
        contentAsString(result) should include(messagefromMessageKey("iv.userAborted.title"))

      }

    }

    "handling requests to see the timed out page" must {

      "display the page" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(Future.successful(Right(Some(SessionData.empty))))
        }

        val result = controller.getTimedOut()(FakeRequest())
        status(result)          shouldBe OK
        contentAsString(result) should include(messagefromMessageKey("iv.timeout.title"))

      }

    }

    "handling requests to see the technical issue page" must {

      "display the page" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(Future.successful(Right(Some(SessionData.empty))))
        }

        val result = controller.getTechnicalIssue()(FakeRequest())
        status(result)          shouldBe OK
        contentAsString(result) should include(messagefromMessageKey("iv.technicalIssue.title"))

      }

    }

    "handling requests to see the precondition failed page" must {

      "display the page" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(Future.successful(Right(Some(SessionData.empty))))
        }

        val result = controller.getPreconditionFailed()(FakeRequest())
        status(result)          shouldBe OK
        contentAsString(result) should include(messagefromMessageKey("iv.preconditionFailed.title"))

      }

    }

  }

}
