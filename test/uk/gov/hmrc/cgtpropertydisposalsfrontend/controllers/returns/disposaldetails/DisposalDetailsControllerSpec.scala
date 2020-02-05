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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.disposaldetails

import cats.data.EitherT
import cats.instances.future._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.i18n.MessagesApi
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, JourneyStatus, SessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, StartingNewDraftReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{DisposalDetailsAnswers, DraftReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.DisposalDetailsAnswers.{CompleteDisposalDetailsAnswers, IncompleteDisposalDetailsAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

class DisposalDetailsControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with ScalaCheckDrivenPropertyChecks
    with RedirectToStartBehaviour {

  val mockReturnsService = mock[ReturnsService]

  override val overrideBindings =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore),
      bind[ReturnsService].toInstance(mockReturnsService)
    )

  lazy val controller = instanceOf[DisposalDetailsController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  def redirectToStartBehaviour(performAction: () => Future[Result]) =
    redirectToStartWhenInvalidJourney(
      performAction, {
        case _: FillingOutReturn => true
        case _                   => false
      }
    )

  val fillingOutReturn = sample[FillingOutReturn]

  def sessionWithDisposalDetailsAnswers(answers: Option[DisposalDetailsAnswers]): SessionData =
    SessionData.empty.copy(
      journeyStatus = Some(
        fillingOutReturn.copy(
          draftReturn = fillingOutReturn.draftReturn.copy(
            disposalDetailsAnswers = answers
          )
        )
      )
    )

  def sessionWithDisposalDetailsAnswers(answers: DisposalDetailsAnswers): SessionData =
    sessionWithDisposalDetailsAnswers(Some(answers))

  def mockStoreDraftReturn(draftReturn: DraftReturn)(result: Either[Error, Unit]) =
    (mockReturnsService
      .storeDraftReturn(_: DraftReturn)(_: HeaderCarrier))
      .expects(draftReturn, *)
      .returning(EitherT.fromEither[Future](result))

  "DisposalDetailsController" when {

    "handling requests to display the how much did you own page" must {

      def performAction(): Future[Result] = controller.howMuchDidYouOwn()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      "display the page" when {

        "the user has not answered the question before" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionWithDisposalDetailsAnswers(None)))))
          }

          val result = performAction()
          status(result)          shouldBe OK
          contentAsString(result) should include(message("propertyShare.title"))
        }

        "the user has answered the question before but has " +
          "not completed the disposal detail section" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              Future.successful(
                Right(
                  Some(
                    sessionWithDisposalDetailsAnswers(
                      IncompleteDisposalDetailsAnswers.empty.copy(
                        percentageOwned = Some(12.34)
                      )
                    )
                  )
                )
              )
            )
          }

          val result  = performAction()
          val content = contentAsString(result)

          status(result) shouldBe OK
          content        should include(message("propertyShare.title"))
          content        should include("12.34")
        }

        "the user has answered the question before but has " +
          "completed the disposal detail section" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              Future.successful(
                Right(
                  Some(
                    sessionWithDisposalDetailsAnswers(
                      sample[CompleteDisposalDetailsAnswers].copy(
                        percentageOwned = 12.34
                      )
                    )
                  )
                )
              )
            )
          }

          val result  = performAction()
          val content = contentAsString(result)

          status(result) shouldBe OK
          content        should include(message("propertyShare.title"))
          content        should include("12.34")
        }

      }

    }

    "handling submitted answers to the how much did you own page" must {

      def performAction(data: Seq[(String, String)]): Future[Result] =
        controller.howMuchDidYouOwnSubmit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      behave like redirectToStartBehaviour(() => performAction(Seq.empty))

      "show a form error" when {

        def test(data: (String, String)*)(expectedErrorMessageKey: String) = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              Future.successful(Right(Some(sessionWithDisposalDetailsAnswers(sample[CompleteDisposalDetailsAnswers]))))
            )
          }

          val result  = performAction(data)
          val content = contentAsString(result)

          status(result) shouldBe BAD_REQUEST
          content        should include(message("propertyShare.title"))
          content        should include(message(expectedErrorMessageKey))

        }

        "nothing is submitted" in {
          test()("propertySharePercentage.error.required")

        }

        "the number is less than zero" in {
          test("propertySharePercentage" -> "-1")("propertySharePercentage.error.tooSmall")
        }

        "the number is greater than 100" in {
          test("propertySharePercentage" -> "101")("propertySharePercentage.error.tooLarge")

        }

        "the number has more than two decimal places" in {
          test("propertySharePercentage" -> "1.234")("propertySharePercentage.error.tooManyDecimals")

        }

      }

      "show an error page" when {

        val currentAnswers = sample[CompleteDisposalDetailsAnswers].copy(percentageOwned = 1d)
        val newPercentage  = 2d
        val newDraftReturn = fillingOutReturn.draftReturn.copy(
          disposalDetailsAnswers = Some(
            currentAnswers.copy(
              percentageOwned = newPercentage
            )
          )
        )

        "there is an error updating the draft return" in {

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              Future.successful(Right(Some(sessionWithDisposalDetailsAnswers(currentAnswers))))
            )
            mockStoreDraftReturn(newDraftReturn)(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction(Seq("propertySharePercentage" -> newPercentage.toString)))
        }

        "there is an error updating the session data" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              Future.successful(Right(Some(sessionWithDisposalDetailsAnswers(currentAnswers))))
            )
            mockStoreDraftReturn(newDraftReturn)(Right(()))
            mockStoreSession(
              sessionWithDisposalDetailsAnswers(currentAnswers.copy(percentageOwned = newPercentage))
            )(Future.successful(Left(Error(""))))
          }

          checkIsTechnicalErrorPage(performAction(Seq("propertySharePercentage" -> newPercentage.toString)))

        }

      }

      "redirect to the disposal price page" when {

        "the user hasn't ever answered the disposal details question " +
          "and the draft return and session data has been successfully updated" in {
          val newPercentage = 2d
          val updatedAnswers = IncompleteDisposalDetailsAnswers.empty.copy(
            percentageOwned = Some(newPercentage)
          )
          val newDraftReturn = fillingOutReturn.draftReturn.copy(
            disposalDetailsAnswers = Some(updatedAnswers)
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              Future.successful(Right(Some(sessionWithDisposalDetailsAnswers(None))))
            )
            mockStoreDraftReturn(newDraftReturn)(Right(()))
            mockStoreSession(sessionWithDisposalDetailsAnswers(updatedAnswers))(Future.successful(Right(())))
          }

          checkIsRedirect(
            performAction(Seq("propertySharePercentage" -> newPercentage.toString)),
            controllers.returns.disposaldetails.routes.DisposalDetailsController.whatWasDisposalPrice()
          )
        }

        "the user has not answered all of the disposal details questions " +
          "and the draft return and session data has been successfully updated" in {
          val currentAnswers = sample[IncompleteDisposalDetailsAnswers].copy(percentageOwned = None)
          val newPercentage  = 2d
          val updatedAnswers = currentAnswers.copy(
            percentageOwned = Some(newPercentage)
          )
          val newDraftReturn = fillingOutReturn.draftReturn.copy(
            disposalDetailsAnswers = Some(updatedAnswers)
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              Future.successful(Right(Some(sessionWithDisposalDetailsAnswers(currentAnswers))))
            )
            mockStoreDraftReturn(newDraftReturn)(Right(()))
            mockStoreSession(sessionWithDisposalDetailsAnswers(updatedAnswers))(Future.successful(Right(())))
          }

          checkIsRedirect(
            performAction(Seq("propertySharePercentage" -> newPercentage.toString)),
            controllers.returns.disposaldetails.routes.DisposalDetailsController.whatWasDisposalPrice()
          )

        }

      }

      "redirect to the cya page" when {

        "the user has answered all of the disposal details questions " +
          "and the draft return and session data has been successfully updated" in {
          val currentAnswers = sample[CompleteDisposalDetailsAnswers].copy(percentageOwned = 1d)
          val newPercentage  = 2d
          val newDraftReturn = fillingOutReturn.draftReturn.copy(
            disposalDetailsAnswers = Some(
              currentAnswers.copy(
                percentageOwned = newPercentage
              )
            )
          )
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              Future.successful(Right(Some(sessionWithDisposalDetailsAnswers(currentAnswers))))
            )
            mockStoreDraftReturn(newDraftReturn)(Right(()))
            mockStoreSession(
              sessionWithDisposalDetailsAnswers(currentAnswers.copy(percentageOwned = newPercentage))
            )(Future.successful(Right(())))
          }

          checkIsRedirect(
            performAction(Seq("propertySharePercentage" -> newPercentage.toString)),
            controllers.returns.disposaldetails.routes.DisposalDetailsController.checkYourAnswers()
          )

        }

      }

      "not update the draft return or the session data" when {

        "the answer given has not changed from a previous one" in {
          val currentAnswers = sample[CompleteDisposalDetailsAnswers].copy(percentageOwned = 1d)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              Future.successful(Right(Some(sessionWithDisposalDetailsAnswers(currentAnswers))))
            )
          }

          checkIsRedirect(
            performAction(Seq("propertySharePercentage" -> "1")),
            controllers.returns.disposaldetails.routes.DisposalDetailsController.checkYourAnswers()
          )

        }

      }

    }

  }

}
