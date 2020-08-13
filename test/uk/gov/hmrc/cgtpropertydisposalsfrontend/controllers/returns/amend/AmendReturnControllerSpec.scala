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
import play.api.mvc.Result
import play.api.test.FakeRequest
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.StartingToAmendReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.SessionData
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore

import scala.concurrent.Future

class AmendReturnControllerSpec
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

  lazy val controller = instanceOf[AmendReturnController]

  implicit val messagesApi: MessagesApi = controller.messagesApi

  "AmendReturnController" when {

    "handling requests to display the 'you must calculate' page" must {

      def performAction(): Future[Result] = controller.youNeedToCalculate()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(
        performAction,
        {
          case _: StartingToAmendReturn => true
          case _                        => false
        }
      )

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
              .confirmCancel()
              .url
            doc.select("#cancelOrContinue-continue").attr("href") shouldBe "#"
          }
        )

      }
    }

    "handling requests to display the confirm cancellation page" must {

      def performAction(): Future[Result] =
        controller.confirmCancel()(FakeRequest())

      "display the page" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData.empty)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("confirmCancelAmendReturn.title"),
          { doc =>
            doc.select("#back").attr("href") shouldBe routes.AmendReturnController.youNeedToCalculate().url

            doc
              .select("#content > article > form")
              .attr("action") shouldBe routes.AmendReturnController.confirmCancelSubmit().url
          }
        )

      }
    }

    "handling submits on the confirm cancellation page" must {

      def performAction(formData: (String, String)*): Future[Result] =
        controller.confirmCancelSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*))

      "display a form error" when {

        def test(data: (String, String)*)(expectedErrorMessageKey: String): Unit =
          checkPageIsDisplayed(
            performAction(data: _*),
            messageFromMessageKey("confirmCancelAmendReturn.title"),
            { doc =>
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
            performAction("confirmCancelAmendReturn" -> "true"),
            controllers.returns.routes.ViewReturnController.displayReturn()
          )
        }

        "the user doesn't want to cancel" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData.empty)
          }

          checkIsRedirect(
            performAction("confirmCancelAmendReturn" -> "false"),
            routes.AmendReturnController.youNeedToCalculate()
          )
        }

      }

    }

  }

}
