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

import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport, accounts, returns}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators.{sample, _}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.FillingOutReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, SessionData, TimeUtils, UserType}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService
import play.api.i18n.{Lang, Messages, MessagesApi, MessagesImpl}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.AgentReferenceNumber
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.{IndividualName, TrustName}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.SubscribedDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.DraftReturn

import scala.concurrent.Future

class DraftReturnSavedControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with ReturnsServiceSupport
    with ScalaCheckDrivenPropertyChecks
    with RedirectToStartBehaviour {

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  override val overrideBindings = List[GuiceableModule](
    bind[AuthConnector].toInstance(mockAuthConnector),
    bind[SessionStore].toInstance(mockSessionStore),
    bind[ReturnsService].toInstance(mockReturnsService)
  )

  lazy val controller                  = instanceOf[DraftReturnSavedController]
  implicit lazy val messages: Messages = MessagesImpl(Lang("en"), messagesApi)

  private def performAction(): Future[Result] = controller.draftReturnSaved()(FakeRequest())

  "DraftReturnSavedController" when {

    "handling requests to display the draft return saved page" must {

      "redirect to the start endpoint" when {

        "there isn't any session data" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty
            )
          }

          checkIsRedirect(performAction(), controllers.routes.StartController.start())
        }

      }

      "display the page" when {

        def test(
          session: SessionData,
          journey: FillingOutReturn,
          updatedDraftReturn: DraftReturn,
          expectedWarningMessageKey: String
        ) = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(
              updatedDraftReturn,
              journey.subscribedDetails.cgtReference,
              journey.agentReferenceNumber
            )(Right(()))
          }

          val formattedDate: String = TimeUtils.govDisplayFormat(TimeUtils.today().plusDays(29))

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("draftReturnSaved.title"), { doc =>
              doc.select("#back").attr("href")   shouldBe returns.routes.TaskListController.taskList().url
              doc.select(".button").attr("href") shouldBe accounts.homepage.routes.HomePageController.homepage().url
              doc.select("#content > article > div > strong").text() shouldBe messageFromMessageKey(
                expectedWarningMessageKey,
                formattedDate
              )

            }
          )
        }

        def fillingOutReturn(name: Either[TrustName, IndividualName]) = sample[FillingOutReturn].copy(
          subscribedDetails = sample[SubscribedDetails].copy(name = name)
        )

        "the user is an individual" in {
          val journey = fillingOutReturn(Right(sample[IndividualName]))

          test(
            SessionData.empty.copy(
              journeyStatus = Some(journey),
              userType      = Some(UserType.Individual)
            ),
            journey,
            journey.draftReturn.fold(
              _.copy(lastUpdatedDate = TimeUtils.today()),
              _.copy(lastUpdatedDate = TimeUtils.today()),
              _.copy(lastUpdatedDate = TimeUtils.today())
            ),
            "draftReturnSaved.warning"
          )
        }

        "the user is a trust" in {
          val journey = fillingOutReturn(Left(sample[TrustName]))

          test(
            SessionData.empty.copy(
              journeyStatus = Some(journey),
              userType      = Some(UserType.Organisation)
            ),
            journey,
            journey.draftReturn.fold(
              _.copy(lastUpdatedDate = TimeUtils.today()),
              _.copy(lastUpdatedDate = TimeUtils.today()),
              _.copy(lastUpdatedDate = TimeUtils.today())
            ),
            "draftReturnSaved.trust.warning"
          )
        }

        "the user is an agent" in {
          val journey =
            fillingOutReturn(Left(sample[TrustName])).copy(agentReferenceNumber = Some(sample[AgentReferenceNumber]))

          test(
            SessionData.empty.copy(
              journeyStatus = Some(journey),
              userType      = Some(UserType.Agent)
            ),
            journey,
            journey.draftReturn.fold(
              _.copy(lastUpdatedDate = TimeUtils.today()),
              _.copy(lastUpdatedDate = TimeUtils.today()),
              _.copy(lastUpdatedDate = TimeUtils.today())
            ),
            "draftReturnSaved.agent.warning"
          )
        }

      }

      "show an error page" when {

        "there is an error updating the draft return" in {
          val journey = sample[FillingOutReturn]
          val updatedDraftReturn = journey.draftReturn.fold(
            _.copy(lastUpdatedDate = TimeUtils.today()),
            _.copy(lastUpdatedDate = TimeUtils.today()),
            _.copy(lastUpdatedDate = TimeUtils.today())
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData.empty.copy(journeyStatus = Some(journey)))
            mockStoreDraftReturn(
              updatedDraftReturn,
              journey.subscribedDetails.cgtReference,
              journey.agentReferenceNumber
            )(
              Left(Error("Some Error"))
            )
          }

          checkIsTechnicalErrorPage(performAction())
        }

      }

    }

  }
}
