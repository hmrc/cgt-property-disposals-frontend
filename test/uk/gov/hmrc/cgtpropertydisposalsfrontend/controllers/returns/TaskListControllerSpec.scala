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

import org.jsoup.Jsoup.parse
import org.jsoup.nodes.Document
import org.scalatest.Assertion
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.i18n.MessagesApi
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.FillingOutReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address.UkAddress
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.DraftReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.IndividualTriageAnswers.{CompleteIndividualTriageAnswers, IncompleteIndividualTriageAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{JourneyStatus, SessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views.returns.TaskListStatus

import scala.concurrent.Future

class TaskListControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with ScalaCheckDrivenPropertyChecks
    with RedirectToStartBehaviour {

  override val overrideBindings =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore)
    )

  lazy val controller = instanceOf[TaskListController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  "TaskListController" when {

    "handling requests to display the task list page" must {

      def performAction(): Future[Result] = controller.taskList()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(
        performAction, {
          case _: FillingOutReturn => true
          case _                   => false
        }
      )

      "display the page with the proper triage section status" when {
        def testTemplate(fillingOutReturn: JourneyStatus, triageState: TaskListStatus): Assertion = {

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              Future.successful(
                Right(
                  Some(
                    SessionData.empty.copy(
                      journeyStatus = Some(fillingOutReturn)
                    )
                  )
                )
              )
            )
          }

          val result = performAction()
          status(result) shouldBe OK

          val doc: Document = parse(contentAsString(result))
          doc.select("h1").text shouldBe messagefromMessageKey("service.title")
          doc.select("li#canTheyUseOurService > a").text should include(messagefromMessageKey("task-list.triage.link"))
          doc.select("li#canTheyUseOurService > a").attr("href") shouldBe triage.routes.CanTheyUseOurServiceController.checkYourAnswers().url
          doc.select("li#canTheyUseOurService > strong").text shouldBe triageState.toString
        }

        "the session data indicates that they are filling in a return and the triage section is incomplete" in {
          testTemplate(
            sample[FillingOutReturn].copy(draftReturn = sample[DraftReturn].copy(triageAnswers =
              sample[IncompleteIndividualTriageAnswers])),
            TaskListStatus.InProgress
          )
        }

        "the session data indicates that they are filling in a return and the triage section is complete" in {
          testTemplate(
            sample[FillingOutReturn].copy(draftReturn = sample[DraftReturn].copy(triageAnswers =
              sample[CompleteIndividualTriageAnswers])),
            TaskListStatus.Complete
          )
        }
      }

      "display the page with the proper Enter property address section status" when {
        "the session data indicates that they are filling in a return and enter property address is todo" in {
          val fillingOutReturn = sample[FillingOutReturn].copy(draftReturn = sample[DraftReturn]
              .copy(propertyAddress = None))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              Future.successful(
                Right(
                  Some(
                    SessionData.empty.copy(
                      journeyStatus = Some(fillingOutReturn)
                    )
                  )
                )
              )
            )
          }

          val result = performAction()
          status(result) shouldBe OK

          val doc: Document = parse(contentAsString(result))
          doc.select("h1").text shouldBe messagefromMessageKey("service.title")
          doc.select("li#propertyAddress > a").text shouldBe messagefromMessageKey("task-list.enter-property-address.link")
          doc.select("li#propertyAddress > a").attr("href") shouldBe address.routes.PropertyAddressController.enterPostcode().url
          doc.select("li#propertyAddress > strong").text shouldBe TaskListStatus.ToDo.toString
        }

        "the session data indicates that they are filling in a return and enter property address is complete" in {
          val fillingOutReturn = sample[FillingOutReturn].copy(draftReturn = sample[DraftReturn]
            .copy(propertyAddress = Some(sample[UkAddress])))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              Future.successful(
                Right(
                  Some(
                    SessionData.empty.copy(
                      journeyStatus = Some(fillingOutReturn)
                    )
                  )
                )
              )
            )
          }

          val result = performAction()
          status(result) shouldBe OK

          val doc: Document = parse(contentAsString(result))
          doc.select("h1").text shouldBe messagefromMessageKey("service.title")
          doc.select("li#propertyAddress > a").text shouldBe messagefromMessageKey("task-list.enter-property-address.link")
          doc.select("li#propertyAddress > a").attr("href") shouldBe address.routes.PropertyAddressController.checkYourAnswers().url
          doc.select("li#propertyAddress > strong").text shouldBe TaskListStatus.Complete.toString

        }
      }

      "display the page with Save and come back later link" when {
        "the session data indicates that they are filling in a return" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              Future.successful(
                Right(
                  Some(
                    SessionData.empty.copy(
                      journeyStatus = Some(sample[FillingOutReturn])
                    )
                  )
                )
              )
            )
          }

          val result = performAction()
          status(result) shouldBe OK

          val doc: Document = parse(contentAsString(result))
          doc.select("h1").text shouldBe messagefromMessageKey("service.title")
          doc.select("a#saveAndComeBackLater").attr("href") shouldBe uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.accounts.homepage.routes.HomePageController.homepage().url
        }
      }
    }
  }
}
