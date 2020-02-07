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
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.FillingOutReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.DisposalDetailsAnswers.{CompleteDisposalDetailsAnswers, IncompleteDisposalDetailsAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.IndividualTriageAnswers.{CompleteIndividualTriageAnswers, IncompleteIndividualTriageAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{DisposalDetailsAnswers, DraftReturn, ShareOfProperty}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{AmountInPence, Error, SessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

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
    .copy(draftReturn = sample[DraftReturn].copy(triageAnswers = sample[CompleteIndividualTriageAnswers]))

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

      behave like noDisposalMethodBehaviour(performAction)

      "display the page" when {

        "the user has not answered the question before" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionWithDisposalDetailsAnswers(None)))))
          }

          val result = performAction()
          status(result)          shouldBe OK
          contentAsString(result) should include(message("shareOfProperty.title"))
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
          content        should include(message("shareOfProperty.title"))
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
          content        should include(message("shareOfProperty.title"))
          content        should include("12.34")
        }

      }

    }

    "handling submitted answers to the how much did you own page" must {

      def performAction(data: Seq[(String, String)]): Future[Result] =
        controller.howMuchDidYouOwnSubmit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      behave like redirectToStartBehaviour(() => performAction(Seq.empty))

      behave like noDisposalMethodBehaviour(() => performAction(Seq.empty))

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
          content        should include(message("shareOfProperty.title"))
          content        should include(message(expectedErrorMessageKey))

        }

        "nothing is submitted" in {
          test()("shareOfProperty.error.required")

        }

        "other is selected but no percentage is submitted" in {
          test("shareOfProperty" -> "2")("percentageShare.error.required")

        }

        "the number is less than zero" in {
          test("shareOfProperty" -> "2", "percentageShare" -> "-1")("percentageShare.error.tooSmall")
        }

        "the number is greater than 100" in {
          test("shareOfProperty" -> "2", "percentageShare" -> "101")("percentageShare.error.tooLarge")

        }

        "the number has more than two decimal places" in {
          test("shareOfProperty" -> "2", "percentageShare" -> "1.234")("percentageShare.error.tooManyDecimals")

        }

        "the submitted value for shareOfProperty is not an integer" in {
          test("shareOfProperty" -> "abc")("shareOfProperty.error.invalid")
        }

        "the submitted value for shareOfProperty is an integer but is not recognised" in {
          test("shareOfProperty" -> "3")("shareOfProperty.error.invalid")
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
          val percentage = 40.23
          val currentAnswers =
            sample[CompleteDisposalDetailsAnswers].copy(shareOfProperty = ShareOfProperty.Full)
          val (newShare, newShareValue) = ShareOfProperty.Other(percentage) -> "2"
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
            performAction(Seq("shareOfProperty" -> newShareValue, "percentageShare" -> percentage.toString)),
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

      "convert a submitted option of 'Other' with value 100 to the 'Full' option" in {
        val updatedAnswers = IncompleteDisposalDetailsAnswers.empty.copy(
          shareOfProperty = Some(ShareOfProperty.Full)
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
          performAction(Seq("shareOfProperty" -> "2", "percentageShare" -> "100")),
          controllers.returns.disposaldetails.routes.DisposalDetailsController.whatWasDisposalPrice()
        )
      }

      "convert a submitted option of 'Other' with value 50 to the 'Half' option" in {
        val updatedAnswers = IncompleteDisposalDetailsAnswers.empty.copy(
          shareOfProperty = Some(ShareOfProperty.Half)
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
          performAction(Seq("shareOfProperty" -> "2", "percentageShare" -> "50")),
          controllers.returns.disposaldetails.routes.DisposalDetailsController.whatWasDisposalPrice()
        )
      }

    }

    "handling requests to display the what was disposal price page" must {

      def performAction(): Future[Result] = controller.whatWasDisposalPrice()(FakeRequest())

      val requiredPreviousAnswers =
        IncompleteDisposalDetailsAnswers.empty.copy(shareOfProperty = Some(ShareOfProperty.Full))

      behave like redirectToStartBehaviour(performAction)

      behave like noDisposalMethodBehaviour(performAction)

      behave like noPropertyShareBehaviour(performAction)

      "display the page" when {

        "the user has not answered the question before" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionWithDisposalDetailsAnswers(requiredPreviousAnswers)))))
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
                      requiredPreviousAnswers.copy(
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
        controller.whatWasDisposalPriceSubmit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      behave like redirectToStartBehaviour(() => performAction(Seq.empty))

      behave like noDisposalMethodBehaviour(() => performAction(Seq.empty))

      behave like noPropertyShareBehaviour(() => performAction(Seq.empty))

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

        "the number is equal to zero" in {
          test("disposalPrice" -> "0")("disposalPrice.error.tooSmall")
        }

        "the number is greater than 100" in {
          test("disposalPrice" -> (5e10 + 1).toString)("disposalPrice.error.tooLarge")
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

          checkIsTechnicalErrorPage(performAction(Seq("disposalPrice" -> newDisposalPrice.inPounds().toString)))
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

          checkIsTechnicalErrorPage(performAction(Seq("disposalPrice" -> newDisposalPrice.inPounds().toString)))

        }

      }

      "redirect to the disposal fees page" when {

        "the user hasn't ever answered the disposal details question " +
          "and the draft return and session data has been successfully updated" in {
          val newDisposalPrice = 2d
          val incompleteDisposalDetailsAnswers =
            IncompleteDisposalDetailsAnswers(Some(ShareOfProperty.Full), None, None)
          val updatedAnswers = incompleteDisposalDetailsAnswers.copy(
            disposalPrice = Some(AmountInPence.fromPounds(newDisposalPrice))
          )
          val newDraftReturn = fillingOutReturn.draftReturn.copy(
            disposalDetailsAnswers = Some(updatedAnswers)
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              Future.successful(
                Right(
                  Some(sessionWithDisposalDetailsAnswers(incompleteDisposalDetailsAnswers))
                )
              )
            )
            mockStoreDraftReturn(newDraftReturn)(Right(()))
            mockStoreSession(sessionWithDisposalDetailsAnswers(updatedAnswers))(Future.successful(Right(())))
          }

          checkIsRedirect(
            performAction(Seq("disposalPrice" -> newDisposalPrice.toString)),
            controllers.returns.disposaldetails.routes.DisposalDetailsController.whatWereDisposalFees()
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
            controllers.returns.disposaldetails.routes.DisposalDetailsController.whatWereDisposalFees()
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

      "accept submitted values with commas" in {
        val currentAnswers =
          sample[CompleteDisposalDetailsAnswers].copy(disposalPrice = AmountInPence.fromPounds(1000d))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            Future.successful(Right(Some(sessionWithDisposalDetailsAnswers(currentAnswers))))
          )
        }

        checkIsRedirect(
          performAction(Seq("disposalPrice" -> "1,000")),
          controllers.returns.disposaldetails.routes.DisposalDetailsController.checkYourAnswers()
        )
      }

      "accept submitted values with pound signs" in {
        val currentAnswers = sample[CompleteDisposalDetailsAnswers].copy(disposalPrice = AmountInPence.fromPounds(1d))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            Future.successful(Right(Some(sessionWithDisposalDetailsAnswers(currentAnswers))))
          )
        }

        checkIsRedirect(
          performAction(Seq("disposalPrice" -> "£1")),
          controllers.returns.disposaldetails.routes.DisposalDetailsController.checkYourAnswers()
        )
      }

    }

    "handling requests to display the what were disposal fees page" must {

      def performAction(): Future[Result] = controller.whatWereDisposalFees()(FakeRequest())

      val requiredPreviousAnswers = IncompleteDisposalDetailsAnswers.empty
        .copy(shareOfProperty = Some(ShareOfProperty.Full), disposalPrice = Some(AmountInPence.fromPounds(2d)))

      behave like redirectToStartBehaviour(performAction)

      behave like noDisposalMethodBehaviour(performAction)

      behave like noPropertyShareBehaviour(performAction)

      "redirect to the disposal price page" when {

        "the user has not answered that question yet" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              Future.successful(
                Right(Some(sessionWithDisposalDetailsAnswers(requiredPreviousAnswers.copy(disposalPrice = None))))
              )
            )
          }

          checkIsRedirect(performAction(), routes.DisposalDetailsController.whatWasDisposalPrice())
        }

      }

      "display the page" when {

        "the user has not answered the question before" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionWithDisposalDetailsAnswers(requiredPreviousAnswers)))))
          }

          val result = performAction()
          status(result)          shouldBe OK
          contentAsString(result) should include(message("disposalFees.title"))
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
                      requiredPreviousAnswers.copy(
                        disposalFees = Some(AmountInPence.fromPounds(12.34))
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
          content        should include(message("disposalFees.title"))
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
                        disposalFees = AmountInPence.fromPounds(12.34)
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
          content        should include(message("disposalFees.title"))
          content        should include("12.34")
        }

      }

    }

    "handling submitted answers to the what was disposal fees page" must {

      def performAction(data: Seq[(String, String)]): Future[Result] =
        controller.whatWereDisposalFeesSubmit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      val requiredPreviousAnswers = IncompleteDisposalDetailsAnswers.empty
        .copy(shareOfProperty = Some(ShareOfProperty.Full), disposalPrice = Some(AmountInPence.fromPounds(2d)))

      behave like redirectToStartBehaviour(() => performAction(Seq.empty))

      behave like noDisposalMethodBehaviour(() => performAction(Seq.empty))

      behave like noPropertyShareBehaviour(() => performAction(Seq.empty))

      "redirect to the disposal price page" when {

        "the user has not answered that question yet" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              Future.successful(
                Right(Some(sessionWithDisposalDetailsAnswers(requiredPreviousAnswers.copy(disposalPrice = None))))
              )
            )
          }

          checkIsRedirect(performAction(Seq.empty), routes.DisposalDetailsController.whatWasDisposalPrice())
        }

      }

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
          content        should include(message("disposalFees.title"))
          content        should include(message(expectedErrorMessageKey))
        }

        "nothing is submitted" in {
          test()("disposalFees.error.required")
        }

        "the number is less than zero" in {
          test("disposalFees" -> "-1")("disposalFees.error.tooSmall")
        }

        "the number is greater than 5e10" in {
          test("disposalFees" -> (5e10 + 1).toString)("disposalFees.error.tooLarge")
        }

        "the number has more than two decimal places" in {
          test("disposalFees" -> "1.234")("disposalFees.error.tooManyDecimals")
        }

      }

      "show an error page" when {

        val currentAnswers  = sample[CompleteDisposalDetailsAnswers].copy(disposalFees = AmountInPence.fromPounds(1d))
        val newDisposalFees = AmountInPence.fromPounds(2d)
        val newDraftReturn = fillingOutReturn.draftReturn.copy(
          disposalDetailsAnswers = Some(
            currentAnswers.copy(
              disposalFees = newDisposalFees
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

          checkIsTechnicalErrorPage(performAction(Seq("disposalFees" -> newDisposalFees.inPounds().toString)))
        }

        "there is an error updating the session data" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              Future.successful(Right(Some(sessionWithDisposalDetailsAnswers(currentAnswers))))
            )
            mockStoreDraftReturn(newDraftReturn)(Right(()))
            mockStoreSession(
              sessionWithDisposalDetailsAnswers(currentAnswers.copy(disposalFees = newDisposalFees))
            )(Future.successful(Left(Error(""))))
          }

          checkIsTechnicalErrorPage(performAction(Seq("disposalFees" -> newDisposalFees.inPounds().toString)))

        }

      }

      "redirect to the check your answers page" when {

        "the user hasn't ever answered the disposal details question " +
          "and the draft return and session data has been successfully updated" in {
          val newDisposalFees = 2d
          val incompleteDisposalDetailsAnswers =
            IncompleteDisposalDetailsAnswers(Some(ShareOfProperty.Full), Some(AmountInPence.fromPounds(1d)), None)

          val updatedAnswers = incompleteDisposalDetailsAnswers.copy(
            disposalFees = Some(AmountInPence.fromPounds(newDisposalFees))
          )
          val newDraftReturn = fillingOutReturn.draftReturn.copy(
            disposalDetailsAnswers = Some(updatedAnswers)
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              Future.successful(
                Right(
                  Some(sessionWithDisposalDetailsAnswers(incompleteDisposalDetailsAnswers))
                )
              )
            )
            mockStoreDraftReturn(newDraftReturn)(Right(()))
            mockStoreSession(sessionWithDisposalDetailsAnswers(updatedAnswers))(Future.successful(Right(())))
          }

          checkIsRedirect(
            performAction(Seq("disposalFees" -> newDisposalFees.toString)),
            controllers.returns.disposaldetails.routes.DisposalDetailsController.checkYourAnswers()
          )
        }

        "the user has not answered all of the disposal details questions " +
          "and the draft return and session data has been successfully updated" in {
          val currentAnswers  = sample[IncompleteDisposalDetailsAnswers].copy(disposalFees = None)
          val newDisposalFees = 2d
          val updatedAnswers = currentAnswers.copy(
            disposalFees = Some(AmountInPence.fromPounds(newDisposalFees))
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
            performAction(Seq("disposalFees" -> newDisposalFees.toString)),
            controllers.returns.disposaldetails.routes.DisposalDetailsController.checkYourAnswers()
          )

        }

      }

      "redirect to the cya page" when {

        "the user has answered all of the disposal details questions " +
          "and the draft return and session data has been successfully updated" in {
          val currentAnswers  = sample[CompleteDisposalDetailsAnswers].copy(disposalFees = AmountInPence.fromPounds(1d))
          val newDisposalFees = 2d
          val newDraftReturn = fillingOutReturn.draftReturn.copy(
            disposalDetailsAnswers = Some(
              currentAnswers.copy(
                disposalFees = AmountInPence.fromPounds(newDisposalFees)
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
                currentAnswers.copy(disposalFees = AmountInPence.fromPounds(newDisposalFees))
              )
            )(Future.successful(Right(())))
          }

          checkIsRedirect(
            performAction(Seq("disposalFees" -> newDisposalFees.toString)),
            controllers.returns.disposaldetails.routes.DisposalDetailsController.checkYourAnswers()
          )

        }

      }

      "not update the draft return or the session data" when {

        "the answer given has not changed from a previous one" in {
          val currentAnswers = sample[CompleteDisposalDetailsAnswers].copy(disposalFees = AmountInPence.fromPounds(1d))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              Future.successful(Right(Some(sessionWithDisposalDetailsAnswers(currentAnswers))))
            )
          }

          checkIsRedirect(
            performAction(Seq("disposalFees" -> "1")),
            controllers.returns.disposaldetails.routes.DisposalDetailsController.checkYourAnswers()
          )

        }

      }

      "accept submitted values with commas" in {
        val currentAnswers =
          sample[CompleteDisposalDetailsAnswers].copy(disposalFees = AmountInPence.fromPounds(1000d))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            Future.successful(Right(Some(sessionWithDisposalDetailsAnswers(currentAnswers))))
          )
        }

        checkIsRedirect(
          performAction(Seq("disposalFees" -> "1,000")),
          controllers.returns.disposaldetails.routes.DisposalDetailsController.checkYourAnswers()
        )
      }

      "accept submitted values with pound signs" in {
        val currentAnswers = sample[CompleteDisposalDetailsAnswers].copy(disposalFees = AmountInPence.fromPounds(1d))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            Future.successful(Right(Some(sessionWithDisposalDetailsAnswers(currentAnswers))))
          )
        }

        checkIsRedirect(
          performAction(Seq("disposalFees" -> "£1")),
          controllers.returns.disposaldetails.routes.DisposalDetailsController.checkYourAnswers()
        )
      }

    }

    "handling requests to display the check your answers page" must {

      def performAction(): Future[Result] = controller.checkYourAnswers()(FakeRequest())

      val completeAnswers = CompleteDisposalDetailsAnswers(
        sample[ShareOfProperty],
        sample[AmountInPence],
        sample[AmountInPence]
      )

      val allQuestionsAnswered = IncompleteDisposalDetailsAnswers(
        Some(completeAnswers.shareOfProperty),
        Some(completeAnswers.disposalPrice),
        Some(completeAnswers.disposalFees)
      )

      behave like redirectToStartBehaviour(performAction)

      "redirect to the how much did you own page" when {

        "there are no disposal details answers in session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionWithDisposalDetailsAnswers(None)))))
          }

          checkIsRedirect(performAction(), routes.DisposalDetailsController.howMuchDidYouOwn())
        }

        "there are disposal details in session but no answer for the property share question" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              Future.successful(
                Right(
                  Some(
                    sessionWithDisposalDetailsAnswers(
                      allQuestionsAnswered.copy(shareOfProperty = None)
                    )
                  )
                )
              )
            )
          }

          checkIsRedirect(performAction(), routes.DisposalDetailsController.howMuchDidYouOwn())

        }

      }

      "redirect to the disposal price page" when {

        "the user has not answered that question" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              Future.successful(
                Right(
                  Some(
                    sessionWithDisposalDetailsAnswers(
                      allQuestionsAnswered.copy(disposalPrice = None)
                    )
                  )
                )
              )
            )
          }

          checkIsRedirect(performAction(), routes.DisposalDetailsController.whatWasDisposalPrice())

        }
      }

      "redirect to the disposal fees page" when {

        "the user has not answered that question" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              Future.successful(
                Right(
                  Some(
                    sessionWithDisposalDetailsAnswers(
                      allQuestionsAnswered.copy(disposalFees = None)
                    )
                  )
                )
              )
            )
          }

          checkIsRedirect(performAction(), routes.DisposalDetailsController.whatWereDisposalFees())

        }
      }

      "show an error page" when {

        "there is an error updating the draft return" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              Future.successful(
                Right(
                  Some(
                    sessionWithDisposalDetailsAnswers(
                      allQuestionsAnswered
                    )
                  )
                )
              )
            )
            mockStoreDraftReturn(
              fillingOutReturn.draftReturn.copy(
                disposalDetailsAnswers = Some(completeAnswers)
              )
            )(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction())
        }

        "there is an error updating the session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              Future.successful(
                Right(
                  Some(
                    sessionWithDisposalDetailsAnswers(
                      allQuestionsAnswered
                    )
                  )
                )
              )
            )
            mockStoreDraftReturn(
              fillingOutReturn.draftReturn.copy(
                disposalDetailsAnswers = Some(completeAnswers)
              )
            )(Right(()))
            mockStoreSession(
              sessionWithDisposalDetailsAnswers(completeAnswers)
            )(Future.successful(Left(Error(""))))
          }

          checkIsTechnicalErrorPage(performAction())
        }

      }

      "redirect to the check your answers page" when {

        "the user has already answered all of the questions" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              Future.successful(
                Right(
                  Some(
                    sessionWithDisposalDetailsAnswers(
                      completeAnswers
                    )
                  )
                )
              )
            )
          }

          val result = performAction()
          status(result)          shouldBe OK
          contentAsString(result) should include(message("returns.disposal-details.cya.title"))

        }

        "the user has just answered all of the questions" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              Future.successful(
                Right(
                  Some(
                    sessionWithDisposalDetailsAnswers(
                      allQuestionsAnswered
                    )
                  )
                )
              )
            )
            mockStoreDraftReturn(
              fillingOutReturn.draftReturn.copy(
                disposalDetailsAnswers = Some(completeAnswers)
              )
            )(Right(()))
            mockStoreSession(
              sessionWithDisposalDetailsAnswers(completeAnswers)
            )(Future.successful(Right(())))
          }

          val result = performAction()
          status(result)          shouldBe OK
          contentAsString(result) should include(message("returns.disposal-details.cya.title"))
        }

      }

    }

    "handling submits on the check your answers page" must {

      def performAction(): Future[Result] = controller.checkYourAnswersSubmit()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      "redirect to the task list page" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            Future.successful(Right(Some(sessionWithDisposalDetailsAnswers(sample[CompleteDisposalDetailsAnswers]))))
          )
        }

        checkIsRedirect(performAction(), controllers.returns.routes.TaskListController.taskList())
      }

    }
  }

  def noDisposalMethodBehaviour(performAction: () => Future[Result]): Unit =
    "redirect to start endpoint" when {

      "there is no disposal method" in {
        val draftReturn = sample[DraftReturn].copy(
          triageAnswers = sample[IncompleteIndividualTriageAnswers].copy(disposalMethod = None)
        )

        val fillingOutReturn = sample[FillingOutReturn].copy(draftReturn = draftReturn)
        val sessionData      = SessionData.empty.copy(journeyStatus      = Some(fillingOutReturn))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(Future.successful(Right(Some(sessionData))))
        }

        checkIsRedirect(performAction(), controllers.routes.StartController.start())
      }
    }

  def noPropertyShareBehaviour(performAction: () => Future[Result]): Unit =
    "redirect to how much did you own endpoint" when {

      "there is no property share" in {
        val draftReturn = sample[DraftReturn].copy(
          triageAnswers = sample[CompleteIndividualTriageAnswers],
          disposalDetailsAnswers = Some(
            IncompleteDisposalDetailsAnswers(
              None,
              Some(sample[AmountInPence]),
              Some(sample[AmountInPence])
            )
          )
        )

        val sessionData = SessionData.empty.copy(journeyStatus = Some(fillingOutReturn.copy(draftReturn = draftReturn)))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(Future.successful(Right(Some(sessionData))))
        }

        checkIsRedirect(
          performAction(),
          controllers.returns.disposaldetails.routes.DisposalDetailsController.howMuchDidYouOwn()
        )
      }
    }
}
