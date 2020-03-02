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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.reliefdetails

import org.jsoup.nodes.Document
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.i18n.{Lang, Messages, MessagesApi, MessagesImpl}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers.BAD_REQUEST
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.AmountOfMoneyErrorScenarios.amountOfMoneyErrorScenarios
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.ReturnsServiceSupport
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.FillingOutReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.AmountInPence
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.DisposalDetailsAnswers.CompleteDisposalDetailsAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ExemptionAndLossesAnswers.{CompleteExemptionAndLossesAnswers, IncompleteExemptionAndLossesAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.OtherReliefsOption.{NoOtherReliefs, OtherReliefs}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ReliefDetailsAnswers.{CompleteReliefDetailsAnswers, IncompleteReliefDetailsAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{DraftReturn, ExemptionAndLossesAnswers, OtherReliefsOption, ReliefDetailsAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, SessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService

import scala.concurrent.Future

class ReliefDetailsControllerSpec
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

  lazy val controller = instanceOf[ReliefDetailsController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  implicit lazy val messages: Messages = MessagesImpl(Lang("en"), messagesApi)

  def redirectToStartBehaviour(performAction: () => Future[Result]) =
    redirectToStartWhenInvalidJourney(
      performAction, {
        case _: FillingOutReturn => true
        case _                   => false
      }
    )

  def sessionWithReliefDetailsAnswers(
    reliefDetailsAnswers: Option[ReliefDetailsAnswers],
    exemptionAndLossesAnswers: Option[ExemptionAndLossesAnswers]
  ): (SessionData, FillingOutReturn) = {
    val journey = sample[FillingOutReturn].copy(
      draftReturn = sample[DraftReturn].copy(
        reliefDetailsAnswers      = reliefDetailsAnswers,
        exemptionAndLossesAnswers = exemptionAndLossesAnswers
      )
    )
    SessionData.empty.copy(journeyStatus = Some(journey)) -> journey
  }

  def sessionWithReliefDetailsAnswers(
    reliefDetailsAnswers: ReliefDetailsAnswers,
    exemptionAndLossesAnswers: Option[ExemptionAndLossesAnswers] = None
  ): (SessionData, FillingOutReturn) =
    sessionWithReliefDetailsAnswers(Some(reliefDetailsAnswers), exemptionAndLossesAnswers)

  "ReliefDetailsController" when {

    "handling requests to display the private residents relief page" must {

      def performAction(): Future[Result] = controller.privateResidentsRelief()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      "display the page" when {

        "the user has not answered the question before" in {

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionWithReliefDetailsAnswers(None, None)._1)
          }

          checkPageIsDisplayed(performAction(), messageFromMessageKey("privateResidentsRelief.title"))
        }

        "the user has answered the question before but has " +
          "not completed the relief detail section" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithReliefDetailsAnswers(
                IncompleteReliefDetailsAnswers.empty.copy(
                  privateResidentsRelief = Some(AmountInPence.fromPounds(12.34))
                )
              )._1
            )
          }

          checkPageIsDisplayed(performAction(), messageFromMessageKey("privateResidentsRelief.title"), { doc =>
            doc.select("#privateResidentsReliefValue").attr("value") shouldBe "12.34"
          })
        }

        "the user has answered the question before but has " +
          "completed the relief detail section" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithReliefDetailsAnswers(
                sample[CompleteReliefDetailsAnswers].copy(privateResidentsRelief = AmountInPence.fromPounds(12.34))
              )._1
            )
          }

          checkPageIsDisplayed(performAction(), messageFromMessageKey("privateResidentsRelief.title"), { doc =>
            doc.select("#privateResidentsReliefValue").attr("value") shouldBe "12.34"
          })
        }

      }

    }

    "handling submitted answers to the private residents relief page" must {

      def performAction(data: Seq[(String, String)]): Future[Result] =
        controller.privateResidentsReliefSubmit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      behave like redirectToStartBehaviour(() => performAction(Seq.empty))

      "show a form error" when {

        def test(data: (String, String)*)(expectedErrorMessageKey: String) = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithReliefDetailsAnswers(
                sample[CompleteReliefDetailsAnswers]
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(data),
            messageFromMessageKey("privateResidentsRelief.title"), { doc =>
              doc.select("#error-summary-display > ul > li > a").text() shouldBe messageFromMessageKey(
                expectedErrorMessageKey
              )
            },
            BAD_REQUEST
          )
        }

        "the data is invalid" in {
          amountOfMoneyErrorScenarios("privateResidentsReliefValue").foreach { scenario =>
            withClue(s"For $scenario: ") {
              val data = ("privateResidentsRelief" -> "0") :: scenario.formData
              test(data: _*)(scenario.expectedErrorMessageKey)
            }
          }
        }
      }

      "show an error page" when {

        val currentAnswers =
          sample[CompleteReliefDetailsAnswers].copy(privateResidentsRelief = AmountInPence.fromPounds(1d))
        val (session, journey)        = sessionWithReliefDetailsAnswers(currentAnswers)
        val newprivateResidentsRelief = AmountInPence.fromPounds(10d)
        val newDraftReturn = journey.draftReturn.copy(
          reliefDetailsAnswers = Some(
            currentAnswers.copy(
              privateResidentsRelief = newprivateResidentsRelief
            )
          )
        )

        "there is an error updating the draft return" in {

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(newDraftReturn)(Left(Error("")))
          }

          checkIsTechnicalErrorPage(
            performAction(Seq("privateResidentsRelief" -> "0", "privateResidentsReliefValue" -> "10"))
          )
        }

        "there is an error updating the session data" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(newDraftReturn)(Right(()))
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

          checkIsTechnicalErrorPage(
            performAction(Seq("privateResidentsRelief" -> "0", "privateResidentsReliefValue" -> "10"))
          )

        }

      }

      "redirect to the cya page" when {

        "the user hasn't ever answered the relief details question " +
          "and the draft return and session data has been successfully updated" in {
          val (newPrivateResidentsRelief, newPrivateResidentsReliefValue) = "0" -> 10d
          val oldDraftReturn = sample[DraftReturn].copy(
            reliefDetailsAnswers = None
          )
          val newDraftReturn =
            oldDraftReturn.copy(
              reliefDetailsAnswers = Some(
                IncompleteReliefDetailsAnswers.empty.copy(
                  privateResidentsRelief = Some(AmountInPence.fromPounds(newPrivateResidentsReliefValue))
                )
              )
            )

          testSuccessfulUpdatesAfterSubmit(
            performAction(
              Seq(
                "privateResidentsRelief"      -> newPrivateResidentsRelief,
                "privateResidentsReliefValue" -> newPrivateResidentsReliefValue.toString
              )
            ),
            oldDraftReturn,
            newDraftReturn
          )
        }

        "the user has not answered all of the relief details questions " +
          "and the draft return and session data has been successfully updated" in {
          val (newPrivateResidentsRelief, newPrivateResidentsReliefValue) = "0" -> 1d
          val oldAnswers                                                  = sample[IncompleteReliefDetailsAnswers].copy(privateResidentsRelief = None)

          val oldDraftReturn = sample[DraftReturn].copy(reliefDetailsAnswers = Some(oldAnswers))
          val newDraftReturn =
            oldDraftReturn.copy(
              reliefDetailsAnswers = Some(
                oldAnswers.copy(
                  privateResidentsRelief = Some(AmountInPence.fromPounds(newPrivateResidentsReliefValue))
                )
              )
            )

          testSuccessfulUpdatesAfterSubmit(
            performAction(
              Seq(
                "privateResidentsRelief"      -> newPrivateResidentsRelief,
                "privateResidentsReliefValue" -> newPrivateResidentsReliefValue.toString
              )
            ),
            oldDraftReturn,
            newDraftReturn
          )
        }

        "not update the draft return or the session data" when {

          "the answer given has not changed from a previous one" in {
            val currentAnswers =
              sample[CompleteReliefDetailsAnswers].copy(privateResidentsRelief = AmountInPence.fromPounds(1))

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(sessionWithReliefDetailsAnswers(currentAnswers)._1)
            }

            checkIsRedirect(
              performAction(Seq("privateResidentsRelief" -> "0", "privateResidentsReliefValue" -> "1")),
              controllers.returns.reliefdetails.routes.ReliefDetailsController.checkYourAnswers()
            )

          }

        }

      }
    }

    "handling requests to display the lettings relief page" must {

      def performAction(): Future[Result] = controller.lettingsRelief()(FakeRequest())

      val requiredPreviousAnswers =
        IncompleteReliefDetailsAnswers.empty.copy(privateResidentsRelief = Some(AmountInPence.fromPounds(1)))

      behave like redirectToStartBehaviour(performAction)

      behave like noPrivateResidentsReliefBehaviour(performAction)

      "display the page" when {

        "the user has not answered the question before" in {

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithReliefDetailsAnswers(
                requiredPreviousAnswers.copy(lettingsRelief = Some(AmountInPence.fromPounds(2)))
              )._1
            )
          }

          checkPageIsDisplayed(performAction(), messageFromMessageKey("lettingsRelief.title"))
        }

        "the user has answered the question before but has " +
          "not completed the relief detail section" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithReliefDetailsAnswers(
                IncompleteReliefDetailsAnswers.empty.copy(
                  privateResidentsRelief = Some(AmountInPence.fromPounds(1.34)),
                  lettingsRelief         = Some(AmountInPence.fromPounds(12.34))
                )
              )._1
            )
          }

          checkPageIsDisplayed(performAction(), messageFromMessageKey("lettingsRelief.title"), { doc =>
            doc.select("#lettingsReliefValue").attr("value") shouldBe "12.34"
          })
        }

        "the user has answered the question before but has " +
          "completed the relief detail section" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithReliefDetailsAnswers(
                sample[CompleteReliefDetailsAnswers].copy(
                  privateResidentsRelief = AmountInPence.fromPounds(1.34),
                  lettingsRelief         = AmountInPence.fromPounds(12.34)
                )
              )._1
            )
          }

          checkPageIsDisplayed(performAction(), messageFromMessageKey("lettingsRelief.title"), { doc =>
            doc.select("#lettingsReliefValue").attr("value") shouldBe "12.34"
          })
        }

      }

    }

    "handling submitted answers to the lettings relief page" must {

      def performAction(data: Seq[(String, String)]): Future[Result] =
        controller.lettingsReliefSubmit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      behave like redirectToStartBehaviour(() => performAction(Seq.empty))

      behave like noPrivateResidentsReliefBehaviour(() => performAction(Seq.empty))

      "show a form error" when {

        def test(data: (String, String)*)(expectedErrorMessageKey: String) = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithReliefDetailsAnswers(
                sample[CompleteReliefDetailsAnswers]
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(data),
            messageFromMessageKey("lettingsRelief.title"), { doc =>
              doc.select("#error-summary-display > ul > li > a").text() shouldBe messageFromMessageKey(
                expectedErrorMessageKey
              )
            },
            BAD_REQUEST
          )
        }

        "the data is invalid" in {
          amountOfMoneyErrorScenarios("lettingsReliefValue").foreach { scenario =>
            withClue(s"For $scenario: ") {
              val data = ("lettingsRelief" -> "0") :: scenario.formData
              test(data: _*)(scenario.expectedErrorMessageKey)
            }
          }
        }
      }

      "show an error page" when {

        val currentAnswers     = sample[CompleteReliefDetailsAnswers].copy(lettingsRelief = AmountInPence.fromPounds(1d))
        val (session, journey) = sessionWithReliefDetailsAnswers(currentAnswers)
        val newLettingsRelief  = AmountInPence.fromPounds(2d)
        val newDraftReturn = journey.draftReturn.copy(
          reliefDetailsAnswers = Some(
            currentAnswers.copy(
              lettingsRelief = newLettingsRelief
            )
          )
        )

        "there is an error updating the draft return" in {

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(newDraftReturn)(Left(Error("")))
          }

          checkIsTechnicalErrorPage(
            performAction(Seq("lettingsRelief" -> "0", "lettingsReliefValue" -> newLettingsRelief.inPounds().toString))
          )
        }

        "there is an error updating the session data" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(newDraftReturn)(Right(()))
            mockStoreSession(
              session.copy(journeyStatus = Some(
                journey.copy(
                  draftReturn = newDraftReturn
                )
              )
              )
            )(Left(Error("")))
          }

          checkIsTechnicalErrorPage(
            performAction(Seq("lettingsRelief" -> "0", "lettingsReliefValue" -> newLettingsRelief.inPounds().toString))
          )

        }

      }

      "redirect to the cya page" when {

        "the user has not answered all of the relief details questions " +
          "and the draft return and session data has been successfully updated" in {
          val currentAnswers = sample[IncompleteReliefDetailsAnswers]
            .copy(privateResidentsRelief = Some(sample[AmountInPence]), lettingsRelief = None)
          val newLettingsRelief = 2d

          val oldDraftReturn = sample[DraftReturn].copy(reliefDetailsAnswers = Some(currentAnswers))

          val newDraftReturn =
            oldDraftReturn.copy(
              reliefDetailsAnswers = Some(
                currentAnswers.copy(
                  lettingsRelief = Some(AmountInPence.fromPounds(newLettingsRelief))
                )
              )
            )

          testSuccessfulUpdatesAfterSubmit(
            performAction(Seq("lettingsRelief" -> "0", "lettingsReliefValue" -> newLettingsRelief.toString)),
            oldDraftReturn,
            newDraftReturn
          )
        }

        "the user has answered all of the relief details questions " +
          "and the draft return and session data has been successfully updated" in {
          val currentAnswers    = sample[CompleteReliefDetailsAnswers].copy(lettingsRelief = AmountInPence.fromPounds(1d))
          val newLettingsRelief = 2d
          val oldDraftReturn    = sample[DraftReturn].copy(reliefDetailsAnswers = Some(currentAnswers))

          val newDraftReturn =
            oldDraftReturn.copy(
              reliefDetailsAnswers = Some(
                currentAnswers.copy(
                  lettingsRelief = AmountInPence.fromPounds(newLettingsRelief)
                )
              )
            )

          testSuccessfulUpdatesAfterSubmit(
            performAction(Seq("lettingsRelief" -> "0", "lettingsReliefValue" -> newLettingsRelief.toString)),
            oldDraftReturn,
            newDraftReturn
          )
        }
      }

      "not update the draft return or the session data" when {

        "the answer given has not changed from a previous one" in {
          val currentAnswers = sample[CompleteReliefDetailsAnswers].copy(lettingsRelief = AmountInPence.fromPounds(1d))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionWithReliefDetailsAnswers(currentAnswers)._1)
          }

          checkIsRedirect(
            performAction(Seq("lettingsRelief" -> "0", "lettingsReliefValue" -> "1")),
            controllers.returns.reliefdetails.routes.ReliefDetailsController.checkYourAnswers()
          )

        }

      }

      "accept submitted values with commas" in {
        val currentAnswers =
          sample[CompleteReliefDetailsAnswers].copy(lettingsRelief = AmountInPence.fromPounds(1000d))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionWithReliefDetailsAnswers(currentAnswers)._1)
        }

        checkIsRedirect(
          performAction(Seq("lettingsRelief" -> "0", "lettingsReliefValue" -> "1,000")),
          controllers.returns.reliefdetails.routes.ReliefDetailsController.checkYourAnswers()
        )
      }

      "accept submitted values with pound signs" in {
        val currentAnswers = sample[CompleteReliefDetailsAnswers].copy(lettingsRelief = AmountInPence.fromPounds(1d))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionWithReliefDetailsAnswers(currentAnswers)._1)
        }

        checkIsRedirect(
          performAction(Seq("lettingsRelief" -> "0", "lettingsReliefValue" -> "£1")),
          controllers.returns.reliefdetails.routes.ReliefDetailsController.checkYourAnswers()
        )
      }

    }

    "handling requests to display the other reliefs page" must {

      def performAction(): Future[Result] = controller.otherReliefs()(FakeRequest())

      val requiredPreviousAnswers =
        IncompleteReliefDetailsAnswers.empty.copy(
          privateResidentsRelief = Some(AmountInPence.fromPounds(1)),
          lettingsRelief         = Some(AmountInPence.fromPounds(2))
        )

      behave like redirectToStartBehaviour(performAction)

      behave like noLettingsReliefBehaviour(performAction)

      val otherReliefs = OtherReliefs("ReliefName", AmountInPence.fromPounds(13.34))

      "display the page" when {

        "the user has not answered the question before" in {

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithReliefDetailsAnswers(
                requiredPreviousAnswers.copy(otherReliefs = Some(otherReliefs))
              )._1
            )
          }

          checkPageIsDisplayed(performAction(), messageFromMessageKey("otherReliefs.title"))

        }

        "the user has answered the question before but has " +
          "not completed the relief detail section" in {

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithReliefDetailsAnswers(
                IncompleteReliefDetailsAnswers.empty.copy(
                  privateResidentsRelief = Some(AmountInPence.fromPounds(11.34)),
                  lettingsRelief         = Some(AmountInPence.fromPounds(12.34)),
                  otherReliefs           = Some(otherReliefs)
                )
              )._1
            )
          }

          checkPageIsDisplayed(performAction(), messageFromMessageKey("otherReliefs.title"), { doc =>
            doc.select("#otherReliefsAmount").attr("value") shouldBe "13.34"
          })

        }

        "the user has answered the question before but has " +
          "completed the relief detail section" in {

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithReliefDetailsAnswers(
                sample[CompleteReliefDetailsAnswers].copy(
                  privateResidentsRelief = AmountInPence.fromPounds(1.34),
                  lettingsRelief         = AmountInPence.fromPounds(12.34),
                  otherReliefs           = Some(otherReliefs)
                )
              )._1
            )
          }

          checkPageIsDisplayed(performAction(), messageFromMessageKey("otherReliefs.title"), { doc =>
            doc.select("#otherReliefsAmount").attr("value") shouldBe "13.34"
          })

        }
      }

    }

    "handling submitted answers to the other reliefs page" must {

      def performAction(data: Seq[(String, String)]): Future[Result] =
        controller.otherReliefsSubmit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      behave like redirectToStartBehaviour(() => performAction(Seq.empty))

      behave like noLettingsReliefBehaviour(() => performAction(Seq.empty))

      val otherReliefs = OtherReliefs("ReliefName", AmountInPence.fromPounds(13.34))

      "show a form error for amount" when {

        def test(data: (String, String)*)(expectedErrorMessageKey: String) = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithReliefDetailsAnswers(
                sample[CompleteReliefDetailsAnswers]
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(data),
            messageFromMessageKey("otherReliefs.title"), { doc =>
              doc.select("#error-summary-display > ul > li > a").text() shouldBe messageFromMessageKey(
                expectedErrorMessageKey
              )
            },
            BAD_REQUEST
          )
        }

        "the data is invalid" in {
          amountOfMoneyErrorScenarios("otherReliefsAmount").foreach { scenario =>
            withClue(s"For $scenario: ") {
              val data = ("otherReliefs" -> "0") :: ("otherReliefsName" -> "ReliefsName") :: scenario.formData
              test(data: _*)(scenario.expectedErrorMessageKey)
            }
          }
        }

      }

      "show a form error for name and amount" when {

        def test(data: (String, String)*)(expectedErrorMessageKey: String*) = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithReliefDetailsAnswers(sample[CompleteReliefDetailsAnswers])._1
            )
          }

          checkPageIsDisplayed(
            performAction(data),
            messageFromMessageKey("otherReliefs.title"), { doc =>
              expectedErrorMessageKey.toList match {
                case Nil =>
                case errorKey :: Nil =>
                  doc.select("#error-summary-display > ul > li > a").text() shouldBe messageFromMessageKey(
                    errorKey
                  )
                case errorKeys =>
                  val errors =
                    (1 to errorKeys.length).map(i =>
                      doc.select(s"#error-summary-display > ul > li:nth-child($i) > a").text()
                    )
                  expectedErrorMessageKey
                    .map(messageFromMessageKey(_))
                    .foreach(message => errors should contain(message))
              }
            },
            BAD_REQUEST
          )
        }

        "nothing is submitted" in {
          test("otherReliefs" -> "0")("otherReliefsName.error.required", "otherReliefsAmount.error.required")
        }

        "invalid characters are submitted for name and nothing submitted to amount" in {
          test("otherReliefs" -> "0", "otherReliefsName" -> "£££££")(
            "otherReliefsName.error.invalid",
            "otherReliefsAmount.error.required"
          )
        }

        "empty other reliefs name" in {
          test("otherReliefs" -> "0", "otherReliefsName" -> "    ", "otherReliefsAmount" -> "£1")(
            "otherReliefsName.error.required"
          )
        }

        val otherReliefsNameTooLong =
          "The other reliefs name is too long. The other reliefs name is too long. The other reliefs name is too long."

        "other reliefs name too long " in {
          test("otherReliefs" -> "0", "otherReliefsName" -> otherReliefsNameTooLong, "otherReliefsAmount" -> "£1")(
            "otherReliefsName.error.tooLong"
          )
        }

      }

      "show an error page" when {

        val currentAnswers     = sample[CompleteReliefDetailsAnswers].copy(otherReliefs = Some(otherReliefs))
        val (session, journey) = sessionWithReliefDetailsAnswers(currentAnswers)
        val newOtherReliefs    = OtherReliefs("ReliefName", AmountInPence.fromPounds(2))
        val newDraftReturn = journey.draftReturn.copy(
          reliefDetailsAnswers = Some(
            currentAnswers.copy(
              otherReliefs = Some(newOtherReliefs)
            )
          )
        )

        "there is an error updating the draft return" in {

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(newDraftReturn)(Left(Error("")))
          }

          checkIsTechnicalErrorPage(
            performAction(
              Seq(
                "otherReliefs"       -> "0",
                "otherReliefsName"   -> newOtherReliefs.name,
                "otherReliefsAmount" -> newOtherReliefs.amount.inPounds().toString
              )
            )
          )
        }

        "there is an error updating the session data" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(newDraftReturn)(Right(()))
            mockStoreSession(
              session.copy(journeyStatus = Some(
                journey.copy(
                  draftReturn = newDraftReturn
                )
              )
              )
            )(Left(Error("")))
          }

          checkIsTechnicalErrorPage(
            performAction(
              Seq(
                "otherReliefs"       -> "0",
                "otherReliefsName"   -> newOtherReliefs.name,
                "otherReliefsAmount" -> newOtherReliefs.amount.inPounds().toString
              )
            )
          )

        }

      }

      "redirect to the cya page" when {

        "the user has not answered all of the relief details questions " +
          "and the draft return and session data has been successfully updated" in {
          val currentAnswers = sample[IncompleteReliefDetailsAnswers]
            .copy(
              privateResidentsRelief = Some(sample[AmountInPence]),
              lettingsRelief         = Some(sample[AmountInPence]),
              otherReliefs           = None
            )
          val newOtherReliefs = OtherReliefs("ReliefName", AmountInPence.fromPounds(3d))
          val updatedAnswers = currentAnswers.copy(
            otherReliefs = Some(newOtherReliefs)
          )

          val oldDraftReturn = sample[DraftReturn].copy(
            reliefDetailsAnswers      = Some(currentAnswers),
            exemptionAndLossesAnswers = None
          )
          val newDraftReturn = oldDraftReturn.copy(reliefDetailsAnswers = Some(updatedAnswers))

          testSuccessfulUpdatesAfterSubmit(
            performAction(
              Seq(
                "otherReliefs"       -> "0",
                "otherReliefsName"   -> newOtherReliefs.name,
                "otherReliefsAmount" -> newOtherReliefs.amount.inPounds().toString
              )
            ),
            oldDraftReturn,
            newDraftReturn
          )
        }

        "the user has answered all of the relief details questions " +
          "and the draft return and session data has been successfully updated" in {

          val otherReliefs    = OtherReliefs("ReliefName1", AmountInPence.fromPounds(1d))
          val currentAnswers  = sample[CompleteReliefDetailsAnswers].copy(otherReliefs = Some(otherReliefs))
          val newOtherReliefs = OtherReliefs("ReliefName2", AmountInPence.fromPounds(2d))
          val updatedAnswers =
            currentAnswers.copy(
              otherReliefs = Some(newOtherReliefs)
            )
          val oldDraftReturn = sample[DraftReturn].copy(
            reliefDetailsAnswers      = Some(currentAnswers),
            exemptionAndLossesAnswers = None
          )
          val newDraftReturn = oldDraftReturn.copy(reliefDetailsAnswers = Some(updatedAnswers))

          testSuccessfulUpdatesAfterSubmit(
            performAction(
              Seq(
                "otherReliefs"       -> "0",
                "otherReliefsName"   -> newOtherReliefs.name,
                "otherReliefsAmount" -> newOtherReliefs.amount.inPounds().toString
              )
            ),
            oldDraftReturn,
            newDraftReturn
          )
        }

      }

      "not update the draft return or the session data" when {

        "the answer given has not changed from a previous one" in {
          val otherReliefs   = OtherReliefs("ReliefName1", AmountInPence.fromPounds(1d))
          val currentAnswers = sample[CompleteReliefDetailsAnswers].copy(otherReliefs = Some(otherReliefs))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionWithReliefDetailsAnswers(currentAnswers)._1)
          }

          checkIsRedirect(
            performAction(
              Seq(
                "otherReliefs"       -> "0",
                "otherReliefsName"   -> otherReliefs.name,
                "otherReliefsAmount" -> otherReliefs.amount.inPounds().toString
              )
            ),
            controllers.returns.reliefdetails.routes.ReliefDetailsController.checkYourAnswers()
          )

        }

      }

      "accept submitted values with commas" in {

        val otherReliefs = OtherReliefs("ReliefName1", AmountInPence.fromPounds(1000d))
        val currentAnswers =
          sample[CompleteReliefDetailsAnswers].copy(otherReliefs = Some(otherReliefs))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionWithReliefDetailsAnswers(currentAnswers)._1)
        }

        checkIsRedirect(
          performAction(
            Seq(
              "otherReliefs"       -> "0",
              "otherReliefsName"   -> "ReliefName1",
              "otherReliefsAmount" -> "1,000"
            )
          ),
          controllers.returns.reliefdetails.routes.ReliefDetailsController.checkYourAnswers()
        )

      }

      "accept submitted values with pound signs" in {

        val otherReliefs = OtherReliefs("ReliefName1", AmountInPence.fromPounds(1000d))
        val currentAnswers =
          sample[CompleteReliefDetailsAnswers].copy(otherReliefs = Some(otherReliefs))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionWithReliefDetailsAnswers(currentAnswers)._1)
        }

        checkIsRedirect(
          performAction(
            Seq(
              "otherReliefs"       -> "0",
              "otherReliefsName"   -> "ReliefName1",
              "otherReliefsAmount" -> "£1,000"
            )
          ),
          controllers.returns.reliefdetails.routes.ReliefDetailsController.checkYourAnswers()
        )
      }

      "update the exemption and losses answers correctly" when {

        def test(
          oldReliefAnswers: ReliefDetailsAnswers,
          oldExemptionAndLossesAnswers: ExemptionAndLossesAnswers,
          newReliefAnswers: ReliefDetailsAnswers,
          newExemptionAndLossesAnswers: ExemptionAndLossesAnswers
        )(formData: (String, String)*) = {
          val oldDraftReturn = sample[DraftReturn].copy(
            reliefDetailsAnswers      = Some(oldReliefAnswers),
            exemptionAndLossesAnswers = Some(oldExemptionAndLossesAnswers)
          )
          val newDraftReturn = oldDraftReturn.copy(
            reliefDetailsAnswers      = Some(newReliefAnswers),
            exemptionAndLossesAnswers = Some(newExemptionAndLossesAnswers)
          )

          testSuccessfulUpdatesAfterSubmit(
            performAction(formData),
            oldDraftReturn,
            newDraftReturn
          )
        }

        "the user had entered in other relief details and has now chosen not " +
          "to enter other reliefs and the exemption and losses section was incomplete" in {
          val oldReliefsAnswers = sample[CompleteReliefDetailsAnswers].copy(
            otherReliefs = Some(sample[OtherReliefs])
          )
          val oldExemptionAndLossesAnswers = sample[IncompleteExemptionAndLossesAnswers].copy(
            taxableGainOrLoss = Some(sample[AmountInPence])
          )
          val newReliefAnswers = oldReliefsAnswers.copy(
            otherReliefs = Some(NoOtherReliefs)
          )
          val newExemptionAndLossesAnswers = oldExemptionAndLossesAnswers.copy(
            taxableGainOrLoss = None
          )

          test(oldReliefsAnswers, oldExemptionAndLossesAnswers, newReliefAnswers, newExemptionAndLossesAnswers)(
            "otherReliefs" -> "1"
          )
        }

        "the user had entered in other relief details and has now chosen not " +
          "to enter other reliefs and the exemption and losses section was complete" in {
          val oldReliefsAnswers = sample[CompleteReliefDetailsAnswers].copy(
            otherReliefs = Some(sample[OtherReliefs])
          )
          val oldExemptionAndLossesAnswers = sample[CompleteExemptionAndLossesAnswers].copy(
            taxableGainOrLoss = Some(sample[AmountInPence])
          )
          val newReliefAnswers = oldReliefsAnswers.copy(
            otherReliefs = Some(NoOtherReliefs)
          )
          val newExemptionAndLossesAnswers = oldExemptionAndLossesAnswers.copy(
            taxableGainOrLoss = None
          )
          test(oldReliefsAnswers, oldExemptionAndLossesAnswers, newReliefAnswers, newExemptionAndLossesAnswers)(
            "otherReliefs" -> "1"
          )

        }

        "the user had not entered in other relief details and has now chosen " +
          "to enter other reliefs and the exemption and losses section was incomplete" in {
          val otherRelief = OtherReliefs("name", AmountInPence(1L))

          val oldReliefsAnswers = sample[CompleteReliefDetailsAnswers].copy(
            otherReliefs = Some(NoOtherReliefs)
          )
          val oldExemptionAndLossesAnswers = sample[IncompleteExemptionAndLossesAnswers].copy(
            taxableGainOrLoss = Some(sample[AmountInPence])
          )
          val newReliefAnswers = oldReliefsAnswers.copy(
            otherReliefs = Some(otherRelief)
          )
          val newExemptionAndLossesAnswers = oldExemptionAndLossesAnswers.copy(
            taxableGainOrLoss = None
          )

          test(oldReliefsAnswers, oldExemptionAndLossesAnswers, newReliefAnswers, newExemptionAndLossesAnswers)(
            "otherReliefs"       -> "0",
            "otherReliefsName"   -> otherRelief.name,
            "otherReliefsAmount" -> otherRelief.amount.inPounds().toString
          )
        }

        "the user had not entered in other relief details and has now chosen " +
          "to enter other reliefs and the exemption and losses section was complete" in {
          val otherRelief = OtherReliefs("name", AmountInPence(1L))

          val oldReliefsAnswers = sample[CompleteReliefDetailsAnswers].copy(
            otherReliefs = Some(NoOtherReliefs)
          )
          val oldExemptionAndLossesAnswers = sample[CompleteExemptionAndLossesAnswers].copy(
            taxableGainOrLoss = Some(sample[AmountInPence])
          )
          val newReliefAnswers = oldReliefsAnswers.copy(
            otherReliefs = Some(otherRelief)
          )
          val newExemptionAndLossesAnswers = IncompleteExemptionAndLossesAnswers(
            Some(oldExemptionAndLossesAnswers.inYearLosses),
            Some(oldExemptionAndLossesAnswers.previousYearsLosses),
            Some(oldExemptionAndLossesAnswers.annualExemptAmount),
            None
          )

          test(oldReliefsAnswers, oldExemptionAndLossesAnswers, newReliefAnswers, newExemptionAndLossesAnswers)(
            "otherReliefs"       -> "0",
            "otherReliefsName"   -> otherRelief.name,
            "otherReliefsAmount" -> otherRelief.amount.inPounds().toString
          )
        }

      }

    }

    "handling requests to display the check your answers page" must {

      def performAction(): Future[Result] = controller.checkYourAnswers()(FakeRequest())

      val completeAnswers = CompleteReliefDetailsAnswers(
        sample[AmountInPence],
        sample[AmountInPence],
        Some(sample[OtherReliefs])
      )

      val allQuestionsAnswered = IncompleteReliefDetailsAnswers(
        Some(completeAnswers.privateResidentsRelief),
        Some(completeAnswers.lettingsRelief),
        completeAnswers.otherReliefs
      )

      behave like redirectToStartBehaviour(performAction)

      "redirect to the private residents relief page" when {

        "there are no relief details answers in session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionWithReliefDetailsAnswers(None, None)._1)
          }

          checkIsRedirect(performAction(), routes.ReliefDetailsController.privateResidentsRelief())
        }

        "there are relief details in session but no answer for the private residents relief question" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithReliefDetailsAnswers(
                allQuestionsAnswered.copy(privateResidentsRelief = None)
              )._1
            )
          }

          checkIsRedirect(performAction(), routes.ReliefDetailsController.privateResidentsRelief())

        }

      }

      "redirect to the lettings relief page" when {

        "the user has not answered that question" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithReliefDetailsAnswers(
                allQuestionsAnswered.copy(lettingsRelief = None)
              )._1
            )
          }

          checkIsRedirect(performAction(), routes.ReliefDetailsController.lettingsRelief())

        }

      }

      "redirect to the other reliefs page" when {

        "the user has not answered that question" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithReliefDetailsAnswers(
                allQuestionsAnswered.copy(otherReliefs = None)
              )._1
            )
          }

          checkIsRedirect(performAction(), routes.ReliefDetailsController.otherReliefs())

        }
      }

      "show an error page" when {

        "there is an error updating the draft return" in {
          val (session, journey) = sessionWithReliefDetailsAnswers(allQuestionsAnswered)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(
              journey.draftReturn.copy(
                reliefDetailsAnswers = Some(completeAnswers)
              )
            )(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction())
        }

        "there is an error updating the session" in {
          val (session, journey) = sessionWithReliefDetailsAnswers(allQuestionsAnswered)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(
              journey.draftReturn.copy(
                reliefDetailsAnswers = Some(completeAnswers)
              )
            )(Right(()))
            mockStoreSession(
              session.copy(
                journeyStatus = Some(
                  journey.copy(draftReturn = journey.draftReturn.copy(
                    reliefDetailsAnswers = Some(completeAnswers)
                  )
                  )
                )
              )
            )(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction())
        }

      }

      "show the page" when {

        "the user has just answered all the questions and all updates are successful" in {
          val (session, journey) = sessionWithReliefDetailsAnswers(allQuestionsAnswered)
          val newDraftReturn     = journey.draftReturn.copy(reliefDetailsAnswers = Some(completeAnswers))
          val updatedJourney     = journey.copy(draftReturn = newDraftReturn)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(newDraftReturn)(Right(()))
            mockStoreSession(session.copy(journeyStatus = Some(updatedJourney)))(Right(()))
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("reliefDetails.cya.title"), { doc =>
              doc.select("#content > article > form").attr("action") shouldBe routes.ReliefDetailsController
                .checkYourAnswersSubmit()
                .url
            }
          )
        }

        "the user has already answered all the questions" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionWithReliefDetailsAnswers(completeAnswers)._1)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("reliefDetails.cya.title"), { doc =>
              doc.select("#content > article > form").attr("action") shouldBe routes.ReliefDetailsController
                .checkYourAnswersSubmit()
                .url
            }
          )
        }

      }

    }

    "handling submitted answers to the check your answers page" must {
      def performAction(): Future[Result] = controller.checkYourAnswersSubmit()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      "redirect to the task list page" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            sessionWithReliefDetailsAnswers(sample[CompleteReliefDetailsAnswers])._1
          )
        }

        checkIsRedirect(performAction(), controllers.returns.routes.TaskListController.taskList())
      }

    }
  }

  def noPrivateResidentsReliefBehaviour(performAction: () => Future[Result]): Unit =
    "redirect to the what was your private residents relief page" when {

      "there is no private residents relief " in {
        val sessionData = sessionWithReliefDetailsAnswers(
          IncompleteReliefDetailsAnswers(
            None,
            Some(sample[AmountInPence]),
            Some(sample[OtherReliefsOption])
          )
        )._1

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionData)
        }

        checkIsRedirect(
          performAction(),
          routes.ReliefDetailsController.privateResidentsRelief()
        )
      }
    }

  def noLettingsReliefBehaviour(performAction: () => Future[Result]): Unit =
    "redirect to the what was your lettings relief page" when {

      "there is no lettings relief " in {
        val sessionData = sessionWithReliefDetailsAnswers(
          IncompleteReliefDetailsAnswers(
            Some(sample[AmountInPence]),
            None,
            Some(sample[OtherReliefsOption])
          )
        )._1

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionData)
        }

        checkIsRedirect(
          performAction(),
          routes.ReliefDetailsController.lettingsRelief()
        )
      }
    }

  def testSuccessfulUpdatesAfterSubmit(
    result: => Future[Result],
    oldDraftReturn: DraftReturn,
    newDraftReturn: DraftReturn
  ): Unit = {
    val journey = sample[FillingOutReturn].copy(draftReturn = oldDraftReturn)
    val session = SessionData.empty.copy(journeyStatus      = Some(journey))

    inSequence {
      mockAuthWithNoRetrievals()
      mockGetSession(session)
      mockStoreDraftReturn(newDraftReturn)(Right(()))
      mockStoreSession(
        session.copy(
          journeyStatus = Some(journey.copy(draftReturn = newDraftReturn))
        )
      )(Right(()))
    }

    checkIsRedirect(result, routes.ReliefDetailsController.checkYourAnswers())
  }

}

object ReliefDetailsControllerSpec extends ReliefDetailsControllerSpec {
  def validateReliefDetailsCheckYourAnswersPage(
    reliefDetailsAnswers: CompleteReliefDetailsAnswers,
    doc: Document
  ): Unit = {
    // TODO Implement checks
  }
}
