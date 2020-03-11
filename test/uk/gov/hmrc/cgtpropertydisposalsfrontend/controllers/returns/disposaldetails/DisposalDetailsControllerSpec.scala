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

import org.jsoup.nodes.Document
import org.scalatest.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.i18n.{Lang, MessagesApi}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.AmountOfMoneyErrorScenarios._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.ReturnsServiceSupport
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.disposaldetails.DisposalDetailsControllerSpec.{expectedTitles, validateDisposalDetailsCheckYourAnswersPage}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators.{disposalMethod, _}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.FillingOutReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.AmountInPence
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.MoneyUtils.formatAmountOfMoneyWithPoundSign
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.DisposalDetailsAnswers.{CompleteDisposalDetailsAnswers, IncompleteDisposalDetailsAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.{CompleteSingleDisposalTriageAnswers, IncompleteSingleDisposalTriageAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{DisposalDetailsAnswers, DisposalMethod, DraftReturn, ShareOfProperty}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, SessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService

import scala.concurrent.Future

class DisposalDetailsControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with ReturnsServiceSupport
    with ScalaCheckDrivenPropertyChecks
    with RedirectToStartBehaviour {

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

  def fillingOutReturn(disposalMethod: DisposalMethod): FillingOutReturn =
    sample[FillingOutReturn]
      .copy(draftReturn = sample[DraftReturn].copy(triageAnswers = sample[CompleteSingleDisposalTriageAnswers].copy(
        disposalMethod = disposalMethod
      )
      )
      )

  def sessionWithDisposalDetailsAnswers(
    answers: Option[DisposalDetailsAnswers],
    disposalMethod: DisposalMethod
  ): (SessionData, FillingOutReturn) = {
    val journey = fillingOutReturn(disposalMethod)
    SessionData.empty.copy(
      journeyStatus = Some(
        journey.copy(
          draftReturn = journey.draftReturn.copy(
            disposalDetailsAnswers = answers
          )
        )
      )
    ) -> journey
  }

  def sessionWithDisposalDetailsAnswers(
    answers: DisposalDetailsAnswers,
    disposalMethod: DisposalMethod
  ): (SessionData, FillingOutReturn) =
    sessionWithDisposalDetailsAnswers(Some(answers), disposalMethod)

  "DisposalDetailsController" when {

    "handling requests to display the how much did you own page" must {

      def performAction(): Future[Result] = controller.howMuchDidYouOwn()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      "display the page" when {

        "the user has not answered the question before" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionWithDisposalDetailsAnswers(None, DisposalMethod.Sold)._1)
          }

          checkPageIsDisplayed(performAction(), messageFromMessageKey("shareOfProperty.title"))
        }

        "the user has answered the question before but has " +
          "not completed the disposal detail section" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithDisposalDetailsAnswers(
                IncompleteDisposalDetailsAnswers.empty.copy(
                  shareOfProperty = Some(ShareOfProperty.Other(12.34))
                ),
                DisposalMethod.Sold
              )._1
            )
          }

          checkPageIsDisplayed(performAction(), messageFromMessageKey("shareOfProperty.title"), { doc =>
            doc.select("#percentageShare").attr("value") shouldBe "12.34"
          })
        }

        "the user has answered the question before but has " +
          "completed the disposal detail section" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithDisposalDetailsAnswers(
                sample[CompleteDisposalDetailsAnswers].copy(
                  shareOfProperty = ShareOfProperty.Other(12.34)
                ),
                DisposalMethod.Sold
              )._1
            )
          }

          checkPageIsDisplayed(performAction(), messageFromMessageKey("shareOfProperty.title"), { doc =>
            doc.select("#percentageShare").attr("value") shouldBe "12.34"
          })
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
              sessionWithDisposalDetailsAnswers(sample[CompleteDisposalDetailsAnswers], DisposalMethod.Sold)._1
            )
          }

          checkPageIsDisplayed(
            performAction(data),
            messageFromMessageKey("shareOfProperty.title"), { doc =>
              doc.select("#error-summary-display > ul > li > a").text() shouldBe messageFromMessageKey(
                expectedErrorMessageKey
              )
            },
            BAD_REQUEST
          )
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

        val currentAnswers     = sample[CompleteDisposalDetailsAnswers].copy(shareOfProperty = ShareOfProperty.Half)
        val (session, journey) = sessionWithDisposalDetailsAnswers(currentAnswers, DisposalMethod.Sold)
        val newShare           = ShareOfProperty.Full
        val newDraftReturn = journey.draftReturn.copy(
          disposalDetailsAnswers = Some(
            currentAnswers.copy(
              shareOfProperty = newShare
            )
          )
        )

        "there is an error updating the draft return" in {

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(newDraftReturn, journey.agentReferenceNumber)(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction(Seq("shareOfProperty" -> "0")))
        }

        "there is an error updating the session data" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(newDraftReturn, journey.agentReferenceNumber)(Right(()))
            mockStoreSession(
              session.copy(
                journeyStatus = Some(
                  journey.copy(
                    draftReturn = newDraftReturn
                  )
                )
              )
            )(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction(Seq("shareOfProperty" -> "0")))

        }

      }

      "redirect to the disposal price page" when {

        "the user hasn't ever answered the disposal details question " +
          "and the draft return and session data has been successfully updated" in {
          val (session, journey)        = sessionWithDisposalDetailsAnswers(None, DisposalMethod.Sold)
          val (newShare, newShareValue) = ShareOfProperty.Full -> "0"
          val updatedAnswers = IncompleteDisposalDetailsAnswers.empty.copy(
            shareOfProperty = Some(newShare)
          )
          val newDraftReturn = journey.draftReturn.copy(
            disposalDetailsAnswers = Some(updatedAnswers)
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(newDraftReturn, journey.agentReferenceNumber)(Right(()))
            mockStoreSession(
              session.copy(
                journeyStatus = Some(journey.copy(draftReturn = newDraftReturn))
              )
            )(Right(()))
          }

          checkIsRedirect(
            performAction(Seq("shareOfProperty" -> newShareValue)),
            controllers.returns.disposaldetails.routes.DisposalDetailsController.whatWasDisposalPrice()
          )
        }

        "the user has not answered all of the disposal details questions " +
          "and the draft return and session data has been successfully updated" in {
          val currentAnswers            = sample[IncompleteDisposalDetailsAnswers].copy(shareOfProperty = None)
          val (session, journey)        = sessionWithDisposalDetailsAnswers(currentAnswers, DisposalMethod.Sold)
          val (newShare, newShareValue) = ShareOfProperty.Half -> "1"
          val updatedAnswers = currentAnswers.copy(
            shareOfProperty = Some(newShare)
          )
          val newDraftReturn = journey.draftReturn.copy(
            disposalDetailsAnswers = Some(updatedAnswers)
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(newDraftReturn, journey.agentReferenceNumber)(Right(()))
            mockStoreSession(
              session.copy(
                journeyStatus = Some(journey.copy(draftReturn = newDraftReturn))
              )
            )(Right(()))
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
          val (session, journey) = sessionWithDisposalDetailsAnswers(currentAnswers, DisposalMethod.Sold)

          val (newShare, newShareValue) = ShareOfProperty.Other(percentage) -> "2"
          val newDraftReturn = journey.draftReturn.copy(
            disposalDetailsAnswers = Some(
              currentAnswers.copy(
                shareOfProperty = newShare
              )
            )
          )
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(newDraftReturn, journey.agentReferenceNumber)(Right(()))
            mockStoreSession(
              session.copy(
                journeyStatus = Some(journey.copy(draftReturn = newDraftReturn))
              )
            )(Right(()))
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
            mockGetSession(sessionWithDisposalDetailsAnswers(currentAnswers, DisposalMethod.Sold)._1)
          }

          checkIsRedirect(
            performAction(Seq("shareOfProperty" -> "0")),
            controllers.returns.disposaldetails.routes.DisposalDetailsController.checkYourAnswers()
          )

        }

      }

      "convert a submitted option of 'Other' with value 100 to the 'Full' option" in {
        val (session, journey) = sessionWithDisposalDetailsAnswers(None, DisposalMethod.Sold)

        val updatedAnswers = IncompleteDisposalDetailsAnswers.empty.copy(
          shareOfProperty = Some(ShareOfProperty.Full)
        )
        val newDraftReturn = journey.draftReturn.copy(
          disposalDetailsAnswers = Some(updatedAnswers)
        )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockStoreDraftReturn(newDraftReturn, journey.agentReferenceNumber)(Right(()))
          mockStoreSession(
            session.copy(
              journeyStatus = Some(journey.copy(draftReturn = newDraftReturn))
            )
          )(Right(()))
        }

        checkIsRedirect(
          performAction(Seq("shareOfProperty" -> "2", "percentageShare" -> "100")),
          controllers.returns.disposaldetails.routes.DisposalDetailsController.whatWasDisposalPrice()
        )
      }

      "convert a submitted option of 'Other' with value 50 to the 'Half' option" in {
        val (session, journey) = sessionWithDisposalDetailsAnswers(None, DisposalMethod.Sold)

        val updatedAnswers = IncompleteDisposalDetailsAnswers.empty.copy(
          shareOfProperty = Some(ShareOfProperty.Half)
        )
        val newDraftReturn = journey.draftReturn.copy(
          disposalDetailsAnswers = Some(updatedAnswers)
        )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockStoreDraftReturn(newDraftReturn, journey.agentReferenceNumber)(Right(()))
          mockStoreSession(
            session.copy(
              journeyStatus = Some(journey.copy(draftReturn = newDraftReturn))
            )
          )(Right(()))
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

      val disposalPriceTitleScenarios = List(
        (DisposalMethod.Sold, ShareOfProperty.Full, "disposalPrice.Sold.title"),
        (DisposalMethod.Sold, ShareOfProperty.Half, "disposalPrice.Sold.title"),
        (DisposalMethod.Sold, ShareOfProperty.Other(1), "disposalPrice.Sold.title"),
        (DisposalMethod.Gifted, ShareOfProperty.Full, "disposalPrice.Gifted.title"),
        (DisposalMethod.Gifted, ShareOfProperty.Half, "disposalPrice.Gifted.title"),
        (DisposalMethod.Gifted, ShareOfProperty.Other(1), "disposalPrice.Gifted.title"),
        (DisposalMethod.Other, ShareOfProperty.Full, "disposalPrice.Other.title"),
        (DisposalMethod.Other, ShareOfProperty.Half, "disposalPrice.Other.title"),
        (DisposalMethod.Other, ShareOfProperty.Other(1), "disposalPrice.Other.title")
      )

      behave like redirectToStartBehaviour(performAction)

      behave like noDisposalMethodBehaviour(performAction)

      behave like noPropertyShareBehaviour(performAction)

      "display the page" when {

        "the user has not answered the question before" in {
          disposalPriceTitleScenarios.foreach {
            case (disposalMethod, share, expectedTitleKey) =>
              withClue(
                s"For (disposalMethod, shareOfProperty, expectedTitleKey) = " +
                  s"($disposalMethod $share, $expectedTitleKey): "
              ) {
                inSequence {
                  mockAuthWithNoRetrievals()
                  mockGetSession(
                    sessionWithDisposalDetailsAnswers(
                      requiredPreviousAnswers.copy(shareOfProperty = Some(share)),
                      disposalMethod
                    )._1
                  )
                }

                checkPageIsDisplayed(performAction(), messageFromMessageKey(expectedTitleKey))
              }
          }
        }

        "the user has answered the question before but has " +
          "not completed the disposal details section" in {
          disposalPriceTitleScenarios.foreach {
            case (disposalMethod, share, expectedTitleKey) =>
              withClue(
                s"For (disposalMethod, shareOfProperty, expectedTitleKey) = " +
                  s"($disposalMethod $share, $expectedTitleKey): "
              ) {
                inSequence {
                  mockAuthWithNoRetrievals()
                  mockGetSession(
                    sessionWithDisposalDetailsAnswers(
                      requiredPreviousAnswers.copy(
                        shareOfProperty = Some(share),
                        disposalPrice   = Some(AmountInPence.fromPounds(12.34))
                      ),
                      disposalMethod
                    )._1
                  )
                }

                checkPageIsDisplayed(performAction(), messageFromMessageKey(expectedTitleKey), { doc =>
                  doc.select("#disposalPrice").attr("value") shouldBe "12.34"
                })
              }
          }
        }

        "the user has answered the question before but has " +
          "completed the disposal details section" in {
          disposalPriceTitleScenarios.foreach {
            case (disposalMethod, share, expectedTitleKey) =>
              withClue(
                s"For (disposalMethod, shareOfProperty, expectedTitleKey) = " +
                  s"($disposalMethod $share, $expectedTitleKey): "
              ) {
                inSequence {
                  mockAuthWithNoRetrievals()
                  mockGetSession(
                    sessionWithDisposalDetailsAnswers(
                      sample[CompleteDisposalDetailsAnswers].copy(
                        shareOfProperty = share,
                        disposalPrice   = AmountInPence.fromPounds(12.34)
                      ),
                      disposalMethod
                    )._1
                  )
                }

                checkPageIsDisplayed(performAction(), messageFromMessageKey(expectedTitleKey), { doc =>
                  doc.select("#disposalPrice").attr("value") shouldBe "12.34"
                })
              }
          }
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
              sessionWithDisposalDetailsAnswers(
                sample[CompleteDisposalDetailsAnswers].copy(
                  shareOfProperty = ShareOfProperty.Full
                ),
                DisposalMethod.Sold
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(data),
            messageFromMessageKey("disposalPrice.Sold.title"), { doc =>
              doc.select("#error-summary-display > ul > li > a").text() shouldBe messageFromMessageKey(
                expectedErrorMessageKey
              )
            },
            BAD_REQUEST
          )
        }

        "the data is invalid" in {
          amountOfMoneyErrorScenarios("disposalPrice").foreach { scenario =>
            withClue(s"For $scenario: ") {
              test(scenario.formData: _*)(scenario.expectedErrorMessageKey)
            }
          }
        }
      }

      "show an error page" when {

        val currentAnswers     = sample[CompleteDisposalDetailsAnswers].copy(disposalPrice = AmountInPence.fromPounds(1d))
        val (session, journey) = sessionWithDisposalDetailsAnswers(currentAnswers, DisposalMethod.Sold)
        val newDisposalPrice   = AmountInPence.fromPounds(2d)
        val newDraftReturn = journey.draftReturn.copy(
          disposalDetailsAnswers = Some(
            currentAnswers.copy(
              disposalPrice = newDisposalPrice
            )
          )
        )

        "there is an error updating the draft return" in {

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(newDraftReturn, journey.agentReferenceNumber)(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction(Seq("disposalPrice" -> newDisposalPrice.inPounds().toString)))
        }

        "there is an error updating the session data" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(newDraftReturn, journey.agentReferenceNumber)(Right(()))
            mockStoreSession(
              session.copy(journeyStatus = Some(
                journey.copy(
                  draftReturn = newDraftReturn
                )
              )
              )
            )(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction(Seq("disposalPrice" -> newDisposalPrice.inPounds().toString)))

        }

      }

      "redirect to the disposal fees page" when {

        "the user hasn't ever answered the disposal details question " +
          "and the draft return and session data has been successfully updated" in {

          val incompleteDisposalDetailsAnswers =
            IncompleteDisposalDetailsAnswers(Some(ShareOfProperty.Full), None, None)
          val (session, journey) =
            sessionWithDisposalDetailsAnswers(incompleteDisposalDetailsAnswers, DisposalMethod.Sold)

          val newDisposalPrice = 2d
          val updatedAnswers = incompleteDisposalDetailsAnswers.copy(
            disposalPrice = Some(AmountInPence.fromPounds(newDisposalPrice))
          )
          val newDraftReturn = journey.draftReturn.copy(
            disposalDetailsAnswers = Some(updatedAnswers)
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(newDraftReturn, journey.agentReferenceNumber)(Right(()))
            mockStoreSession(
              session.copy(journeyStatus = Some(
                journey.copy(
                  draftReturn = newDraftReturn
                )
              )
              )
            )(Right(()))
          }

          checkIsRedirect(
            performAction(Seq("disposalPrice" -> newDisposalPrice.toString)),
            controllers.returns.disposaldetails.routes.DisposalDetailsController.whatWereDisposalFees()
          )
        }

        "the user has not answered all of the disposal details questions " +
          "and the draft return and session data has been successfully updated" in {
          val currentAnswers = sample[IncompleteDisposalDetailsAnswers]
            .copy(shareOfProperty = Some(sample[ShareOfProperty]), disposalPrice = None)
          val (session, journey) = sessionWithDisposalDetailsAnswers(currentAnswers, DisposalMethod.Sold)

          val newDisposalPrice = 2d
          val updatedAnswers = currentAnswers.copy(
            disposalPrice = Some(AmountInPence.fromPounds(newDisposalPrice))
          )
          val newDraftReturn = journey.draftReturn.copy(
            disposalDetailsAnswers = Some(updatedAnswers)
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(newDraftReturn, journey.agentReferenceNumber)(Right(()))
            mockStoreSession(
              session.copy(journeyStatus = Some(
                journey.copy(
                  draftReturn = newDraftReturn
                )
              )
              )
            )(Right(()))
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
          val currentAnswers     = sample[CompleteDisposalDetailsAnswers].copy(disposalPrice = AmountInPence.fromPounds(1d))
          val (session, journey) = sessionWithDisposalDetailsAnswers(currentAnswers, DisposalMethod.Sold)

          val newDisposalPrice = 2d
          val newDraftReturn = journey.draftReturn.copy(
            disposalDetailsAnswers = Some(
              currentAnswers.copy(
                disposalPrice = AmountInPence.fromPounds(newDisposalPrice)
              )
            )
          )
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(newDraftReturn, journey.agentReferenceNumber)(Right(()))
            mockStoreSession(
              session.copy(journeyStatus = Some(
                journey.copy(
                  draftReturn = newDraftReturn
                )
              )
              )
            )(Right(()))
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
            mockGetSession(sessionWithDisposalDetailsAnswers(currentAnswers, DisposalMethod.Sold)._1)
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
          mockGetSession(sessionWithDisposalDetailsAnswers(currentAnswers, DisposalMethod.Sold)._1)
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
          mockGetSession(sessionWithDisposalDetailsAnswers(currentAnswers, DisposalMethod.Sold)._1)
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

      val disposalFeesTitleScenarios = List(
        (DisposalMethod.Sold, ShareOfProperty.Full, "disposalFees.Sold.title"),
        (DisposalMethod.Sold, ShareOfProperty.Half, "disposalFees.Sold.title"),
        (DisposalMethod.Sold, ShareOfProperty.Other(1), "disposalFees.Sold.title"),
        (DisposalMethod.Gifted, ShareOfProperty.Full, "disposalFees.Gifted.title"),
        (DisposalMethod.Gifted, ShareOfProperty.Half, "disposalFees.Gifted.title"),
        (DisposalMethod.Gifted, ShareOfProperty.Other(1), "disposalFees.Gifted.title"),
        (DisposalMethod.Other, ShareOfProperty.Full, "disposalFees.Other.title"),
        (DisposalMethod.Other, ShareOfProperty.Half, "disposalFees.Other.title"),
        (DisposalMethod.Other, ShareOfProperty.Other(1), "disposalFees.Other.title")
      )

      behave like redirectToStartBehaviour(performAction)

      behave like noDisposalMethodBehaviour(performAction)

      behave like noPropertyShareBehaviour(performAction)

      "redirect to the disposal price page" when {

        "the user has not answered that question yet" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithDisposalDetailsAnswers(
                requiredPreviousAnswers.copy(disposalPrice = None),
                DisposalMethod.Sold
              )._1
            )
          }

          checkIsRedirect(performAction(), routes.DisposalDetailsController.whatWasDisposalPrice())
        }

      }

      "display the page" when {

        "the user has not answered the question before" in {
          disposalFeesTitleScenarios.foreach {
            case (disposalMethod, share, expectedTitleKey) =>
              withClue(
                s"For (disposalMethod, shareOfProperty, expectedTitleKey) = " +
                  s"($disposalMethod $share, $expectedTitleKey): "
              ) {
                inSequence {
                  mockAuthWithNoRetrievals()
                  mockGetSession(
                    sessionWithDisposalDetailsAnswers(
                      requiredPreviousAnswers.copy(
                        shareOfProperty = Some(share)
                      ),
                      disposalMethod
                    )._1
                  )
                }

                checkPageIsDisplayed(performAction(), messageFromMessageKey(expectedTitleKey))
              }
          }
        }

        "the user has answered the question before but has " +
          "not completed the disposal detail section" in {
          disposalFeesTitleScenarios.foreach {
            case (disposalMethod, share, expectedTitleKey) =>
              withClue(
                s"For (disposalMethod, shareOfProperty, expectedTitleKey) = " +
                  s"($disposalMethod $share, $expectedTitleKey): "
              ) {
                inSequence {
                  mockAuthWithNoRetrievals()
                  mockGetSession(
                    sessionWithDisposalDetailsAnswers(
                      requiredPreviousAnswers.copy(
                        shareOfProperty = Some(share),
                        disposalFees    = Some(AmountInPence.fromPounds(12.34))
                      ),
                      disposalMethod
                    )._1
                  )
                }

                checkPageIsDisplayed(performAction(), messageFromMessageKey(expectedTitleKey), { doc =>
                  doc.select("#disposalFees").attr("value") shouldBe "12.34"
                })
              }
          }
        }

        "the user has answered the question before but has " +
          "completed the disposal detail section" in {
          disposalFeesTitleScenarios.foreach {
            case (disposalMethod, share, expectedTitleKey) =>
              withClue(
                s"For (disposalMethod, shareOfProperty, expectedTitleKey) = " +
                  s"($disposalMethod $share, $expectedTitleKey): "
              ) {
                inSequence {
                  mockAuthWithNoRetrievals()
                  mockGetSession(
                    sessionWithDisposalDetailsAnswers(
                      sample[CompleteDisposalDetailsAnswers].copy(
                        shareOfProperty = share,
                        disposalFees    = AmountInPence.fromPounds(12.34)
                      ),
                      disposalMethod
                    )._1
                  )
                }

                checkPageIsDisplayed(performAction(), messageFromMessageKey(expectedTitleKey), { doc =>
                  doc.select("#disposalFees").attr("value") shouldBe "12.34"
                })
              }
          }
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
              sessionWithDisposalDetailsAnswers(
                requiredPreviousAnswers.copy(disposalPrice = None),
                DisposalMethod.Sold
              )._1
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
              sessionWithDisposalDetailsAnswers(
                sample[CompleteDisposalDetailsAnswers].copy(
                  shareOfProperty = ShareOfProperty.Full
                ),
                DisposalMethod.Sold
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(data),
            messageFromMessageKey("disposalFees.Sold.title"), { doc =>
              doc.select("#error-summary-display > ul > li > a").text() shouldBe messageFromMessageKey(
                expectedErrorMessageKey
              )
            },
            BAD_REQUEST
          )
        }

        amountOfMoneyErrorScenarios("disposalFees").foreach { scenario =>
          withClue(s"For $scenario: ") {
            test(scenario.formData: _*)(scenario.expectedErrorMessageKey)
          }
        }

      }

      "show an error page" when {

        val currentAnswers     = sample[CompleteDisposalDetailsAnswers].copy(disposalFees = AmountInPence.fromPounds(1d))
        val (session, journey) = sessionWithDisposalDetailsAnswers(currentAnswers, DisposalMethod.Sold)

        val newDisposalFees = AmountInPence.fromPounds(2d)
        val newDraftReturn = journey.draftReturn.copy(
          disposalDetailsAnswers = Some(
            currentAnswers.copy(
              disposalFees = newDisposalFees
            )
          )
        )

        "there is an error updating the draft return" in {

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(newDraftReturn, journey.agentReferenceNumber)(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction(Seq("disposalFees" -> newDisposalFees.inPounds().toString)))
        }

        "there is an error updating the session data" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(newDraftReturn, journey.agentReferenceNumber)(Right(()))
            mockStoreSession(session.copy(journeyStatus = Some(journey.copy(draftReturn = newDraftReturn))))(
              Left(Error(""))
            )
          }

          checkIsTechnicalErrorPage(performAction(Seq("disposalFees" -> newDisposalFees.inPounds().toString)))

        }

      }

      "redirect to the check your answers page" when {

        "the user hasn't ever answered the disposal details question " +
          "and the draft return and session data has been successfully updated" in {
          val incompleteDisposalDetailsAnswers =
            IncompleteDisposalDetailsAnswers(Some(ShareOfProperty.Full), Some(AmountInPence.fromPounds(1d)), None)

          val (session, journey) =
            sessionWithDisposalDetailsAnswers(incompleteDisposalDetailsAnswers, DisposalMethod.Sold)

          val newDisposalFees = 2d
          val updatedAnswers = incompleteDisposalDetailsAnswers.copy(
            disposalFees = Some(AmountInPence.fromPounds(newDisposalFees))
          )
          val newDraftReturn = journey.draftReturn.copy(
            disposalDetailsAnswers = Some(updatedAnswers)
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(newDraftReturn, journey.agentReferenceNumber)(Right(()))
            mockStoreSession(session.copy(journeyStatus = Some(journey.copy(draftReturn = newDraftReturn))))(Right(()))
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
          val currentAnswers     = sample[CompleteDisposalDetailsAnswers].copy(disposalFees = AmountInPence.fromPounds(1d))
          val (session, journey) = sessionWithDisposalDetailsAnswers(currentAnswers, DisposalMethod.Sold)

          val newDisposalFees = 2d
          val newDraftReturn = journey.draftReturn.copy(
            disposalDetailsAnswers = Some(
              currentAnswers.copy(
                disposalFees = AmountInPence.fromPounds(newDisposalFees)
              )
            )
          )
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(newDraftReturn, journey.agentReferenceNumber)(Right(()))
            mockStoreSession(session.copy(journeyStatus = Some(journey.copy(draftReturn = newDraftReturn))))(Right(()))
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
            mockGetSession(sessionWithDisposalDetailsAnswers(currentAnswers, DisposalMethod.Sold)._1)
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
          mockGetSession(sessionWithDisposalDetailsAnswers(currentAnswers, DisposalMethod.Sold)._1)
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
          mockGetSession(sessionWithDisposalDetailsAnswers(currentAnswers, DisposalMethod.Sold)._1)
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
            mockGetSession(sessionWithDisposalDetailsAnswers(None, DisposalMethod.Sold)._1)
          }

          checkIsRedirect(performAction(), routes.DisposalDetailsController.howMuchDidYouOwn())
        }

        "there are disposal details in session but no answer for the property share question" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithDisposalDetailsAnswers(
                allQuestionsAnswered.copy(shareOfProperty = None),
                DisposalMethod.Sold
              )._1
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
              sessionWithDisposalDetailsAnswers(
                allQuestionsAnswered.copy(disposalPrice = None),
                DisposalMethod.Sold
              )._1
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
              sessionWithDisposalDetailsAnswers(
                allQuestionsAnswered.copy(disposalFees = None),
                DisposalMethod.Sold
              )._1
            )
          }

          checkIsRedirect(performAction(), routes.DisposalDetailsController.whatWereDisposalFees())

        }
      }

      "show an error page" when {

        "there is an error updating the draft return" in {
          val (session, journey) = sessionWithDisposalDetailsAnswers(allQuestionsAnswered, DisposalMethod.Sold)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(
              journey.draftReturn.copy(
                disposalDetailsAnswers = Some(completeAnswers)
              ),
              journey.agentReferenceNumber
            )(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction())
        }

        "there is an error updating the session" in {
          val (session, journey) = sessionWithDisposalDetailsAnswers(allQuestionsAnswered, DisposalMethod.Sold)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(
              journey.draftReturn.copy(
                disposalDetailsAnswers = Some(completeAnswers)
              ),
              journey.agentReferenceNumber
            )(Right(()))
            mockStoreSession(
              session.copy(
                journeyStatus = Some(
                  journey.copy(draftReturn = journey.draftReturn.copy(
                    disposalDetailsAnswers = Some(completeAnswers)
                  )
                  )
                )
              )
            )(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction())
        }

      }

      "show the check your answers page" when {

        def testIsCheckYourAnswers(
          result: Future[Result],
          completeDisposalDetailsAnswers: CompleteDisposalDetailsAnswers,
          expectedTitleKey: String,
          expectedDisposalPriceTitleKey: String,
          expectedDisposalFeesTitleKey: String
        ): Unit =
          checkPageIsDisplayed(
            result,
            messageFromMessageKey("returns.disposal-details.cya.title"), { doc =>
              validateDisposalDetailsCheckYourAnswersPage(completeDisposalDetailsAnswers, doc)
              doc.select("#propertyShare-question").text() shouldBe messageFromMessageKey(
                "shareOfProperty.title"
              )
              doc.select("#disposalPrice-question").text() shouldBe messageFromMessageKey(
                expectedDisposalPriceTitleKey
              )
              doc.select("#disposalFees-question").text() shouldBe messageFromMessageKey(
                expectedDisposalFeesTitleKey
              )
            }
          )

        "the user has already answered all of the questions" in {
          forAll { (disposalMethod: DisposalMethod, completeAnswers: CompleteDisposalDetailsAnswers) =>
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionWithDisposalDetailsAnswers(
                  completeAnswers,
                  disposalMethod
                )._1
              )
            }

            testIsCheckYourAnswers(
              performAction(),
              completeAnswers,
              "returns.disposal-details.cya.title",
              expectedTitles(completeAnswers, disposalMethod)._1,
              expectedTitles(completeAnswers, disposalMethod)._2
            )
          }
        }

        "the user has just answered all of the questions" in {
          forAll { (disposalMethod: DisposalMethod, completeAnswers: CompleteDisposalDetailsAnswers) =>
            val incompleteAnswers = IncompleteDisposalDetailsAnswers(
              Some(completeAnswers.shareOfProperty),
              Some(completeAnswers.disposalPrice),
              Some(completeAnswers.disposalFees)
            )
            val (session, journey) = sessionWithDisposalDetailsAnswers(incompleteAnswers, disposalMethod)

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockStoreDraftReturn(
                journey.draftReturn.copy(
                  disposalDetailsAnswers = Some(completeAnswers)
                ),
                journey.agentReferenceNumber
              )(Right(()))
              mockStoreSession(
                session.copy(
                  journeyStatus = Some(
                    journey.copy(draftReturn = journey.draftReturn.copy(
                      disposalDetailsAnswers = Some(completeAnswers)
                    )
                    )
                  )
                )
              )(Right(()))
            }

            testIsCheckYourAnswers(
              performAction(),
              completeAnswers,
              "returns.disposal-details.cya.title",
              expectedTitles(completeAnswers, disposalMethod)._1,
              expectedTitles(completeAnswers, disposalMethod)._2
            )

          }
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
            sessionWithDisposalDetailsAnswers(sample[CompleteDisposalDetailsAnswers], DisposalMethod.Sold)._1
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
          triageAnswers = sample[IncompleteSingleDisposalTriageAnswers].copy(disposalMethod = None)
        )

        val fillingOutReturn = sample[FillingOutReturn].copy(draftReturn = draftReturn)
        val sessionData      = SessionData.empty.copy(journeyStatus      = Some(fillingOutReturn))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionData)
        }

        checkIsRedirect(performAction(), controllers.routes.StartController.start())
      }
    }

  def noPropertyShareBehaviour(performAction: () => Future[Result]): Unit =
    "redirect to the what was your share page" when {

      "there is no property share" in {
        val draftReturn = sample[DraftReturn].copy(
          triageAnswers = sample[CompleteSingleDisposalTriageAnswers],
          disposalDetailsAnswers = Some(
            IncompleteDisposalDetailsAnswers(
              None,
              Some(sample[AmountInPence]),
              Some(sample[AmountInPence])
            )
          )
        )

        val sessionData = SessionData.empty.copy(journeyStatus = Some(
          fillingOutReturn(DisposalMethod.Sold).copy(
            draftReturn = draftReturn
          )
        )
        )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionData)
        }

        checkIsRedirect(
          performAction(),
          routes.DisposalDetailsController.howMuchDidYouOwn()
        )
      }
    }
}

object DisposalDetailsControllerSpec extends Matchers {
  def validateDisposalDetailsCheckYourAnswersPage(
    disposalDetailsAnswers: CompleteDisposalDetailsAnswers,
    doc: Document
  )(implicit messages: MessagesApi, lang: Lang): Unit = {
    doc
      .select("#propertyShare-answer")
      .text()
      .stripSuffix("%") shouldBe disposalDetailsAnswers.shareOfProperty.percentageValue.toString()
    doc.select("#disposalPrice-answer").text() shouldBe formatAmountOfMoneyWithPoundSign(
      disposalDetailsAnswers.disposalPrice.inPounds()
    )
    doc.select("#disposalFees-answer").text() shouldBe formatAmountOfMoneyWithPoundSign(
      disposalDetailsAnswers.disposalFees.inPounds()
    )
  }

  def expectedTitles(
    completeAnswers: CompleteDisposalDetailsAnswers,
    disposalMethod: DisposalMethod
  ): (String, String) =
    (s"disposalPrice.${disposalMethod.toString}.title", s"disposalFees.${disposalMethod.toString}.title")
}
