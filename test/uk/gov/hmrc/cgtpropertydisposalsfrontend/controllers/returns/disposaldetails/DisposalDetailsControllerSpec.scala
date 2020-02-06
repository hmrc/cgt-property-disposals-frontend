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
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{AmountInPence, Error, JourneyStatus, SessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, StartingNewDraftReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{DisposalDetailsAnswers, DraftReturn, ShareOfProperty}
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
                        shareOfProperty = Some(ShareOfProperty.Other(12.34))
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
                        shareOfProperty = ShareOfProperty.Other(12.34)
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

    "handling submitted answers to the how much did you own page" ignore {

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
          test()("shareOfProperty.error.required")

        }

        "other is selected but no percentage is submitted" in {
          test("propertyShare" -> "2")("percentageShare.error.required")

        }

        "the number is less than zero" in {
          test("propertyShare" -> "2", "percentageShare" -> "-1")("percentageShare.error.tooSmall")
        }

        "the number is greater than 100" in {
          test("propertyShare" -> "2", "percentageShare" -> "101")("percentageShare.error.tooLarge")

        }

        "the number has more than two decimal places" in {
          test("propertyShare" -> "2", "percentageShare" -> "1.234")("percentageShare.error.tooManyDecimals")

        }

      }

      "show an error page" when {

        val currentAnswers = sample[CompleteDisposalDetailsAnswers].copy(shareOfProperty = ShareOfProperty.Half)
        val newShare       = ShareOfProperty.Full
        val newDraftReturn = fillingOutReturn.draftReturn.copy(
          disposalDetailsAnswers = Some(
            currentAnswers.copy(
              shareOfProperty = newShare
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

          checkIsTechnicalErrorPage(performAction(Seq("shareOfProperty" -> "0")))
        }

        "there is an error updating the session data" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              Future.successful(Right(Some(sessionWithDisposalDetailsAnswers(currentAnswers))))
            )
            mockStoreDraftReturn(newDraftReturn)(Right(()))
            mockStoreSession(
              sessionWithDisposalDetailsAnswers(currentAnswers.copy(shareOfProperty = newShare))
            )(Future.successful(Left(Error(""))))
          }

          checkIsTechnicalErrorPage(performAction(Seq("shareOfProperty" -> "0")))

        }

      }

      "redirect to the disposal price page" when {

        "the user hasn't ever answered the disposal details question " +
          "and the draft return and session data has been successfully updated" in {
          val (newShare, newShareValue) = ShareOfProperty.Full -> "0"
          val updatedAnswers = IncompleteDisposalDetailsAnswers.empty.copy(
            shareOfProperty = Some(newShare)
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
            performAction(Seq("shareOfProperty" -> newShareValue)),
            controllers.returns.disposaldetails.routes.DisposalDetailsController.whatWasDisposalPrice()
          )
        }

        "the user has not answered all of the disposal details questions " +
          "and the draft return and session data has been successfully updated" in {
          val currentAnswers            = sample[IncompleteDisposalDetailsAnswers].copy(shareOfProperty = None)
          val (newShare, newShareValue) = ShareOfProperty.Half -> "1"
          val updatedAnswers = currentAnswers.copy(
            shareOfProperty = Some(newShare)
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
            performAction(Seq("shareOfProperty" -> newShareValue)),
            controllers.returns.disposaldetails.routes.DisposalDetailsController.whatWasDisposalPrice()
          )

        }

      }

      "redirect to the cya page" when {

        "the user has answered all of the disposal details questions " +
          "and the draft return and session data has been successfully updated" in {
          val currentAnswers            = sample[CompleteDisposalDetailsAnswers].copy(shareOfProperty = ShareOfProperty.Full)
          val (newShare, newShareValue) = ShareOfProperty.Half -> "1"
          val newDraftReturn = fillingOutReturn.draftReturn.copy(
            disposalDetailsAnswers = Some(
              currentAnswers.copy(
                shareOfProperty = newShare
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
              sessionWithDisposalDetailsAnswers(currentAnswers.copy(shareOfProperty = newShare))
            )(Future.successful(Right(())))
          }

          checkIsRedirect(
            performAction(Seq("shareOfProperty" -> newShareValue)),
            controllers.returns.disposaldetails.routes.DisposalDetailsController.checkYourAnswers()
          )

        }

      }

      "not update the draft return or the session data" when {

        "the answer given has not changed from a previous one" in {
          val currentAnswers = sample[CompleteDisposalDetailsAnswers].copy(shareOfProperty = ShareOfProperty.Full)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              Future.successful(Right(Some(sessionWithDisposalDetailsAnswers(currentAnswers))))
            )
          }

          checkIsRedirect(
            performAction(Seq("shareOfProperty" -> "0")),
            controllers.returns.disposaldetails.routes.DisposalDetailsController.checkYourAnswers()
          )

        }

      }

    }

    "handling requests to display the what was disposal price page" must {

      def performAction(): Future[Result] = controller.whatWasDisposalPrice()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      "display the page" when {

        "the user has not answered the question before" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionWithDisposalDetailsAnswers(None)))))
          }

          val result = performAction()
          status(result)          shouldBe OK
          contentAsString(result) should include(message("disposalPrice.title"))
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
                        disposalPrice = Some(AmountInPence.fromPounds(12.34))
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
          content        should include(message("disposalPrice.title"))
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
                        disposalPrice = AmountInPence.fromPounds(12.34)
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
          content        should include(message("disposalPrice.title"))
          content        should include("12.34")
        }

      }

    }

    "handling submitted answers to the what was disposal price page" must {

      def performAction(data: Seq[(String, String)]): Future[Result] =
        controller.whatWasDisposalPrice()(FakeRequest().withFormUrlEncodedBody(data: _*))

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
          content        should include(message("disposalPrice.title"))
          content        should include(message(expectedErrorMessageKey))

        }

        "nothing is submitted" in {
          test()("disposalPrice.error.required")

        }

        "the number is less than zero" in {
          test("disposalPrice" -> "-1")("disposalPrice.error.tooSmall")
        }

        "the number is greater than 100" in {
          test("disposalPrice" -> "101")("disposalPrice.error.tooLarge")

        }

        "the number has more than two decimal places" in {
          test("disposalPrice" -> "1.234")("disposalPrice.error.tooManyDecimals")

        }

      }

      "show an error page" when {

        val currentAnswers   = sample[CompleteDisposalDetailsAnswers].copy(disposalPrice = AmountInPence.fromPounds(1d))
        val newDisposalPrice = AmountInPence.fromPounds(2d)
        val newDraftReturn = fillingOutReturn.draftReturn.copy(
          disposalDetailsAnswers = Some(
            currentAnswers.copy(
              disposalPrice = newDisposalPrice
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

          checkIsTechnicalErrorPage(performAction(Seq("disposalPrice" -> newDisposalPrice.toString)))
        }

        "there is an error updating the session data" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              Future.successful(Right(Some(sessionWithDisposalDetailsAnswers(currentAnswers))))
            )
            mockStoreDraftReturn(newDraftReturn)(Right(()))
            mockStoreSession(
              sessionWithDisposalDetailsAnswers(currentAnswers.copy(disposalPrice = newDisposalPrice))
            )(Future.successful(Left(Error(""))))
          }

          checkIsTechnicalErrorPage(performAction(Seq("disposalPrice" -> newDisposalPrice.toString)))

        }

      }

      "redirect to the disposal fees page" when {

        "the user hasn't ever answered the disposal details question " +
          "and the draft return and session data has been successfully updated" in {
          val newDisposalPrice = 2d
          val updatedAnswers = IncompleteDisposalDetailsAnswers.empty.copy(
            disposalPrice = Some(AmountInPence.fromPounds(newDisposalPrice))
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
            performAction(Seq("disposalPrice" -> newDisposalPrice.toString)),
            controllers.returns.disposaldetails.routes.DisposalDetailsController.whatWasDisposalPrice()
          )
        }

        "the user has not answered all of the disposal details questions " +
          "and the draft return and session data has been successfully updated" in {
          val currentAnswers   = sample[IncompleteDisposalDetailsAnswers].copy(disposalPrice = None)
          val newDisposalPrice = 2d
          val updatedAnswers = currentAnswers.copy(
            disposalPrice = Some(AmountInPence.fromPounds(newDisposalPrice))
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
            performAction(Seq("disposalPrice" -> newDisposalPrice.toString)),
            controllers.returns.disposaldetails.routes.DisposalDetailsController.whatWasDisposalPrice()
          )

        }

      }

      "redirect to the cya page" when {

        "the user has answered all of the disposal details questions " +
          "and the draft return and session data has been successfully updated" in {
          val currentAnswers   = sample[CompleteDisposalDetailsAnswers].copy(disposalPrice = AmountInPence.fromPounds(1d))
          val newDisposalPrice = 2d
          val newDraftReturn = fillingOutReturn.draftReturn.copy(
            disposalDetailsAnswers = Some(
              currentAnswers.copy(
                disposalPrice = AmountInPence.fromPounds(newDisposalPrice)
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
              sessionWithDisposalDetailsAnswers(
                currentAnswers.copy(disposalPrice = AmountInPence.fromPounds(newDisposalPrice))
              )
            )(Future.successful(Right(())))
          }

          checkIsRedirect(
            performAction(Seq("disposalPrice" -> newDisposalPrice.toString)),
            controllers.returns.disposaldetails.routes.DisposalDetailsController.checkYourAnswers()
          )

        }

      }

      "not update the draft return or the session data" when {

        "the answer given has not changed from a previous one" in {
          val currentAnswers = sample[CompleteDisposalDetailsAnswers].copy(disposalPrice = AmountInPence.fromPounds(1d))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              Future.successful(Right(Some(sessionWithDisposalDetailsAnswers(currentAnswers))))
            )
          }

          checkIsRedirect(
            performAction(Seq("disposalPrice" -> "1")),
            controllers.returns.disposaldetails.routes.DisposalDetailsController.checkYourAnswers()
          )

        }

      }

    }
  }

}
