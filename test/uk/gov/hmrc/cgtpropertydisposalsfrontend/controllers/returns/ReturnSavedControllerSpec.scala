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
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport, accounts, returns}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators.{sample, _}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.FillingOutReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, LocalDateUtils, SessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService
import play.api.i18n.{Lang, Messages, MessagesApi, MessagesImpl}

import scala.concurrent.Future

class ReturnSavedControllerSpec
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

  lazy val controller                  = instanceOf[ReturnSavedController]
  implicit lazy val messages: Messages = MessagesImpl(Lang("en"), messagesApi)

  private def performAction(): Future[Result] = controller.confirmDraftReturn()(FakeRequest())

  "ReturnSavedController" when {

    "handling requests with empty session data it should redirect to start" in {
      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(
          SessionData.empty
        )
      }

      checkIsRedirect(performAction(), "/capital-gains-tax-uk-property/start")
    }

    "handling requests with session with return should save the data and show proper text" in {

      val fillingOutReturn = sample[FillingOutReturn]
      val updatedDraftReturn = fillingOutReturn.draftReturn.fold(
        _.copy(lastUpdatedDate = LocalDateUtils.today()),
        _.copy(lastUpdatedDate = LocalDateUtils.today())
      )

      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(
          SessionData.empty.copy(
            journeyStatus = Some(fillingOutReturn)
          )
        )
        mockStoreDraftReturn(
          updatedDraftReturn,
          fillingOutReturn.subscribedDetails.cgtReference,
          fillingOutReturn.agentReferenceNumber
        )(Right(()))
      }

      val result: Future[Result] = performAction()
      status(result) shouldBe OK
      val formattedDate: String = LocalDateUtils.govDisplayFormat(LocalDateUtils.today().plusDays(29))
      contentAsString(result) should include(messageFromMessageKey("confirmDraftReturn.warning", formattedDate))
    }

    "handling requests with session with return proper error when StoreDraftReturn service fails" in {

      val fillingOutReturn = sample[FillingOutReturn]
      val updatedDraftReturn = fillingOutReturn.draftReturn.fold(
        _.copy(lastUpdatedDate = LocalDateUtils.today()),
        _.copy(lastUpdatedDate = LocalDateUtils.today())
      )

      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(
          SessionData.empty.copy(
            journeyStatus = Some(fillingOutReturn)
          )
        )
        mockStoreDraftReturn(
          updatedDraftReturn,
          fillingOutReturn.subscribedDetails.cgtReference,
          fillingOutReturn.agentReferenceNumber
        )(
          Left(Error("Some Error"))
        )
      }

      val result: Future[Result] = performAction()
      status(result) shouldBe INTERNAL_SERVER_ERROR
    }

    "handling requests with session with return should save the data and show the page with proper back and accounts home button and right title" in {

      val fillingOutReturn = sample[FillingOutReturn]
      val updatedDraftReturn = fillingOutReturn.draftReturn.fold(
        _.copy(lastUpdatedDate = LocalDateUtils.today()),
        _.copy(lastUpdatedDate = LocalDateUtils.today())
      )

      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(
          SessionData.empty.copy(
            journeyStatus = Some(fillingOutReturn)
          )
        )
        mockStoreDraftReturn(
          updatedDraftReturn,
          fillingOutReturn.subscribedDetails.cgtReference,
          fillingOutReturn.agentReferenceNumber
        )(Right(()))
      }

      checkPageIsDisplayed(
        performAction(),
        messageFromMessageKey("confirmDraftReturn.title"), { doc =>
          doc.select("#back").attr("href")   shouldBe returns.routes.TaskListController.taskList().url
          doc.select(".button").attr("href") shouldBe accounts.homepage.routes.HomePageController.homepage().url
        }
      )

    }

  }
}
