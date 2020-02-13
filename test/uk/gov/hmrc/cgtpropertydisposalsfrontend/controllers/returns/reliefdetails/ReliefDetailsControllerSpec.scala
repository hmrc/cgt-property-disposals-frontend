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
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.IndividualTriageAnswers.CompleteIndividualTriageAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.OtherReliefsOption.OtherReliefs
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ReliefDetailsAnswers.{CompleteReliefDetailsAnswers, IncompleteReliefDetailsAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{DraftReturn, OtherReliefsOption, ReliefDetailsAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{AmountInPence, Error, SessionData}
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

  def fillingOutReturn(): FillingOutReturn =
    sample[FillingOutReturn]
      .copy(draftReturn = sample[DraftReturn].copy(
        reliefDetailsAnswers = Some(sample[CompleteReliefDetailsAnswers])
      )
      )

  def sessionWithReliefDetailsAnswers(
    answers: Option[ReliefDetailsAnswers]
  ): (SessionData, FillingOutReturn) = {
    val journey = fillingOutReturn()
    SessionData.empty.copy(
      journeyStatus = Some(
        journey.copy(
          draftReturn = journey.draftReturn.copy(reliefDetailsAnswers = answers)
        )
      )
    ) -> journey
  }

  def sessionWithReliefDetailsAnswers(answers: ReliefDetailsAnswers): (SessionData, FillingOutReturn) =
    sessionWithReliefDetailsAnswers(Some(answers))

  "ReliefDetailsController" when {

    "handling requests to display the private residents relief page" must {

      def performAction(): Future[Result] = controller.privateResidentsRelief()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      "display the page" when {

        "the user has not answered the question before" in {

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionWithReliefDetailsAnswers(None)._1)
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
              sessionWithReliefDetailsAnswers(sample[CompleteReliefDetailsAnswers])._1
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

        "nothing is submitted" in {
          test()("privateResidentsRelief.error.required")
        }

        "invalid option is selected" in {
          test("privateResidentsRelief" -> "2")("privateResidentsRelief.error.invalid")
        }

        "yes is selected but no private residents relief is submitted" in {
          test("privateResidentsRelief" -> "0")("privateResidentsReliefValue.error.required")
        }

        "the private residents relief is less than zero" in {
          test("privateResidentsRelief" -> "0", "privateResidentsReliefValue" -> "-1")(
            "privateResidentsReliefValue.error.tooSmall"
          )
        }

        "the private residents relief is greater than 50,000,000,000.00" in {
          test("privateResidentsRelief" -> "0", "privateResidentsReliefValue" -> "50,000,000,001.00")(
            "privateResidentsReliefValue.error.tooLarge"
          )
        }

        "the private residents relief has more than two decimal places" in {
          test("privateResidentsRelief" -> "0", "privateResidentsReliefValue" -> "1.234")(
            "privateResidentsReliefValue.error.tooManyDecimals"
          )
        }

        "the submitted value for private residents relief is not an integer" in {
          test("privateResidentsRelief" -> "abc")("privateResidentsRelief.error.invalid")
        }

        "the submitted value for private residents relief is an integer but is not recognised" in {
          test("privateResidentsRelief" -> "3")("privateResidentsRelief.error.invalid")
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

      "redirect to the  lettings relief page" when {

        "the user hasn't ever answered the relief details question " +
          "and the draft return and session data has been successfully updated" in {

          val (session, journey)                                          = sessionWithReliefDetailsAnswers(None)
          val (newPrivateResidentsRelief, newPrivateResidentsReliefValue) = "0" -> 10d
          val updatedAnswers = IncompleteReliefDetailsAnswers.empty.copy(
            privateResidentsRelief = Some(AmountInPence.fromPounds(newPrivateResidentsReliefValue))
          )
          val newDraftReturn = journey.draftReturn.copy(
            reliefDetailsAnswers = Some(updatedAnswers)
          )

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

          checkIsRedirect(
            performAction(
              Seq(
                "privateResidentsRelief"      -> newPrivateResidentsRelief,
                "privateResidentsReliefValue" -> newPrivateResidentsReliefValue.toString
              )
            ),
            controllers.returns.reliefdetails.routes.ReliefDetailsController.checkYourAnswers()
          )
        }

        "the user has not answered all of the relief details questions " +
          "and the draft return and session data has been successfully updated" in {

          val currentAnswers                                              = sample[IncompleteReliefDetailsAnswers].copy(privateResidentsRelief = None)
          val (session, journey)                                          = sessionWithReliefDetailsAnswers(currentAnswers)
          val (newPrivateResidentsRelief, newPrivateResidentsReliefValue) = "0" -> 1d
          val updatedAnswers = currentAnswers.copy(
            privateResidentsRelief = Some(AmountInPence.fromPounds(newPrivateResidentsReliefValue))
          )
          val newDraftReturn = journey.draftReturn.copy(
            reliefDetailsAnswers = Some(updatedAnswers)
          )

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

          checkIsRedirect(
            performAction(
              Seq(
                "privateResidentsRelief"      -> newPrivateResidentsRelief,
                "privateResidentsReliefValue" -> newPrivateResidentsReliefValue.toString
              )
            ),
            controllers.returns.reliefdetails.routes.ReliefDetailsController.checkYourAnswers()
          )
        }

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

      "redirect to the other reliefs page" when {

        "the user hasn't ever answered the relief details question " +
          "and the draft return and session data has been successfully updated" in {

          val incompleteReliefDetailsAnswers =
            IncompleteReliefDetailsAnswers(Some(AmountInPence.fromPounds(1)), None, None)
          val (session, journey) =
            sessionWithReliefDetailsAnswers(incompleteReliefDetailsAnswers)

          val newLettingsRelief = 2d
          val updatedAnswers = incompleteReliefDetailsAnswers.copy(
            lettingsRelief = Some(AmountInPence.fromPounds(newLettingsRelief))
          )
          val newDraftReturn = journey.draftReturn.copy(
            reliefDetailsAnswers = Some(updatedAnswers)
          )

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
            )(Right(()))
          }

          checkIsRedirect(
            performAction(Seq("lettingsRelief" -> "0", "lettingsReliefValue" -> newLettingsRelief.toString)),
            controllers.returns.reliefdetails.routes.ReliefDetailsController.checkYourAnswers()
          )
        }

        "the user has not answered all of the relief details questions " +
          "and the draft return and session data has been successfully updated" in {
          val currentAnswers = sample[IncompleteReliefDetailsAnswers]
            .copy(privateResidentsRelief = Some(sample[AmountInPence]), lettingsRelief = None)
          val (session, journey) = sessionWithReliefDetailsAnswers(currentAnswers)

          val newLettingsRelief = 2d
          val updatedAnswers = currentAnswers.copy(
            lettingsRelief = Some(AmountInPence.fromPounds(newLettingsRelief))
          )
          val newDraftReturn = journey.draftReturn.copy(
            reliefDetailsAnswers = Some(updatedAnswers)
          )

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
            )(Right(()))
          }

          checkIsRedirect(
            performAction(Seq("lettingsRelief" -> "0", "lettingsReliefValue" -> newLettingsRelief.toString)),
            controllers.returns.reliefdetails.routes.ReliefDetailsController.checkYourAnswers()
          )

        }
      }

      "redirect to the cya page" when {

        "the user has answered all of the relief details questions " +
          "and the draft return and session data has been successfully updated" in {
          val currentAnswers     = sample[CompleteReliefDetailsAnswers].copy(lettingsRelief = AmountInPence.fromPounds(1d))
          val (session, journey) = sessionWithReliefDetailsAnswers(currentAnswers)

          val newLettingsRelief = 2d
          val newDraftReturn = journey.draftReturn.copy(
            reliefDetailsAnswers = Some(
              currentAnswers.copy(
                lettingsRelief = AmountInPence.fromPounds(newLettingsRelief)
              )
            )
          )
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
            )(Right(()))
          }

          checkIsRedirect(
            performAction(Seq("lettingsRelief" -> "0", "lettingsReliefValue" -> newLettingsRelief.toString)),
            controllers.returns.reliefdetails.routes.ReliefDetailsController.checkYourAnswers()
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

      behave like noPrivateResidentsReliefBehaviour(performAction)

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

      behave like noPrivateResidentsReliefBehaviour(() => performAction(Seq.empty))

      behave like noLettingsReliefBehaviour(() => performAction(Seq.empty))

      val otherReliefs = OtherReliefs("ReliefName", AmountInPence.fromPounds(13.34))

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
                "otherReliefsName"   -> "ReliefName",
                "otherReliefsAmount" -> newOtherReliefs.amount.toString
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
                "otherReliefsName"   -> "ReliefName",
                "otherReliefsAmount" -> newOtherReliefs.amount.toString
              )
            )
          )

        }

      }

      "redirect to the other reliefs page" when {}

      "redirect to the cya page" when {}

      "not update the draft return or the session data" when {}

      "accept submitted values with commas" in {}

      "accept submitted values with pound signs" in {}

    }

    "handling requests to display the check your answers page" must {}

    "handling submitted answers to the check your answers page" must {}

    def noPrivateResidentsReliefBehaviour(performAction: () => Future[Result]): Unit =
      "redirect to the what was your private residents relief page" when {

        "there is no private residents relief " in {
          val draftReturn = sample[DraftReturn].copy(
            triageAnswers = sample[CompleteIndividualTriageAnswers],
            reliefDetailsAnswers = Some(
              IncompleteReliefDetailsAnswers(
                None,
                Some(sample[AmountInPence]),
                Some(sample[OtherReliefsOption])
              )
            )
          )

          val sessionData = SessionData.empty.copy(journeyStatus = Some(
            fillingOutReturn().copy(
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
            routes.ReliefDetailsController.privateResidentsRelief()
          )
        }
      }

    def noLettingsReliefBehaviour(performAction: () => Future[Result]): Unit =
      "redirect to the what was your lettings relief page" when {

        "there is no lettings relief " in {
          val draftReturn = sample[DraftReturn].copy(
            triageAnswers = sample[CompleteIndividualTriageAnswers],
            reliefDetailsAnswers = Some(
              IncompleteReliefDetailsAnswers(
                Some(sample[AmountInPence]),
                None,
                Some(sample[OtherReliefsOption])
              )
            )
          )

          val sessionData = SessionData.empty.copy(journeyStatus = Some(
            fillingOutReturn().copy(
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
            routes.ReliefDetailsController.lettingsRelief()
          )
        }
      }

  }

}
