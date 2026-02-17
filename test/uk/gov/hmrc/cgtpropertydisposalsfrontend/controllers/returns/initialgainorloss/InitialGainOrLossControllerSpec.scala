/*
 * Copyright 2023 HM Revenue & Customs
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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.initialgainorloss

import org.scalacheck.Gen
import org.scalatest.Assertion
import uk.gov.hmrc.cgtpropertydisposalsfrontend.SampledScalaCheck
import play.api.http.Status.BAD_REQUEST
import play.api.i18n.{Lang, Messages, MessagesApi, MessagesImpl}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.AmountOfMoneyErrorScenarios.amountOfMoneyErrorScenarios
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.{ReturnsServiceSupport, StartingToAmendToFillingOutReturnSpecBehaviour}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport, returns}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, StartingToAmendReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.AmountInPence
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.DraftReturnGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.JourneyStatusGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.NameGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.SubscribedDetailsGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.TriageQuestionsGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.{IndividualName, TrustName}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.SubscribedDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.DraftSingleDisposalReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.IndividualUserType.{Capacitor, PersonalRepresentative, Self}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.IncompleteSingleDisposalTriageAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, SessionData, UserType}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService

import scala.concurrent.Future

class InitialGainOrLossControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with ReturnsServiceSupport
    with SampledScalaCheck
    with RedirectToStartBehaviour
    with StartingToAmendToFillingOutReturnSpecBehaviour {

  private def redirectToStartBehaviour(performAction: () => Future[Result]): Unit =
    redirectToStartWhenInvalidJourney(
      performAction,
      {
        case _: FillingOutReturn | _: StartingToAmendReturn => true
        case _                                              => false
      }
    )

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  protected override val overrideBindings: List[GuiceableModule] = List[GuiceableModule](
    bind[AuthConnector].toInstance(mockAuthConnector),
    bind[SessionStore].toInstance(mockSessionStore),
    bind[ReturnsService].toInstance(mockReturnsService)
  )

  private lazy val controller          = instanceOf[InitialGainOrLossController]
  implicit lazy val messages: Messages = MessagesImpl(Lang("en"), messagesApi)

  def sessionWithState(
    initialGainOrLoss: Option[AmountInPence]
  ): (SessionData, FillingOutReturn, DraftSingleDisposalReturn) = {
    val draftReturn = sample[DraftSingleDisposalReturn]
      .copy(initialGainOrLoss = initialGainOrLoss)
    val journey     = sample[FillingOutReturn].copy(draftReturn = draftReturn)

    (
      SessionData.empty.copy(journeyStatus = Some(journey)),
      journey,
      draftReturn
    )
  }

  "InitialGainOrLossController" when {

    "handling requests to display the initial gain or loss page" must {
      def performAction(): Future[Result] =
        controller.enterInitialGainOrLoss()(FakeRequest().withFormUrlEncodedBody().withMethod("POST"))

      behave like redirectToStartBehaviour(() => performAction())

      behave like markUnmetDependencyBehaviour(controller.enterInitialGainOrLoss)

      "display the page with backlink to tasklist" when {

        "initialGainOrLoss is not present in draftReturn" in {
          val draftReturn      = sample[DraftSingleDisposalReturn]
            .copy(
              initialGainOrLoss = None,
              triageAnswers = generateTriageAnswersWithSelf()
            )
          val fillingOutReturn = sample[FillingOutReturn]
            .copy(
              draftReturn = draftReturn,
              subscribedDetails = generateIndividualSubscribedDetails()
            )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(fillingOutReturn)
              )
            )
          }
          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(
              "initialGainOrLoss.title"
            ),
            { doc =>
              doc.select(".govuk-caption-xl").text()       should be(
                "Initial gain or loss"
              )
              doc.select(".govuk-fieldset__legend").text() should be(
                messageFromMessageKey("initialGainOrLoss.title")
              )
              doc
                .select(".govuk-back-link")
                .attr("href")                            shouldBe returns.routes.TaskListController
                .taskList()
                .url
              doc
                .select("#main-content form")
                .attr("action")                          shouldBe routes.InitialGainOrLossController
                .submitInitialGainOrLoss()
                .url
            },
            OK
          )
        }
      }

      "display the page with backlink to check your answers" when {

        "initialGainOrLoss is present in returnDraft" in {
          val draftReturn = sample[DraftSingleDisposalReturn]
            .copy(initialGainOrLoss = Some(AmountInPence(300L)))

          val fillingOutReturn =
            sample[FillingOutReturn].copy(draftReturn = draftReturn)

          val trustFillingOutReturn = fillingOutReturn.copy(
            subscribedDetails = generateTrustSubscribedDetails(),
            draftReturn = draftReturn.copy(triageAnswers = generateTriageAnswersForTrust())
          )

          val individualFillingOutReturn = fillingOutReturn.copy(
            subscribedDetails = generateIndividualSubscribedDetails(),
            draftReturn = draftReturn.copy(triageAnswers = generateTriageAnswersWithSelf())
          )

          val personalRepresentativeFillingOutReturn =
            fillingOutReturn.copy(draftReturn =
              draftReturn.copy(triageAnswers = generateTriageAnswersWithPersonalRepresentative())
            )

          val capacitorFillingOutReturn =
            individualFillingOutReturn.copy(draftReturn =
              draftReturn.copy(triageAnswers = generateTriageAnswersWithCapacitor())
            )

          val testData = List(
            ("", individualFillingOutReturn, false),
            (".trust", trustFillingOutReturn, false),
            (".personalRep", personalRepresentativeFillingOutReturn, false),
            (".capacitor", capacitorFillingOutReturn, false),
            (".agent", individualFillingOutReturn, true)
          )
          testData.foreach(keyWithReturn => test(keyWithReturn._1, keyWithReturn._2, keyWithReturn._3))
        }
      }

      def test(
        key: String,
        fillingOutReturn: FillingOutReturn,
        isAgent: Boolean
      ): Unit = {
        val session =
          if (isAgent) {
            SessionData.empty.copy(
              userType = Some(UserType.Agent),
              journeyStatus = Some(fillingOutReturn)
            )
          } else {
            SessionData.empty.copy(
              journeyStatus = Some(fillingOutReturn)
            )
          }

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }
        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey(
            s"initialGainOrLoss$key.title"
          ),
          { doc =>
            doc.select(".govuk-caption-xl").text()           should be(
              "Initial gain or loss"
            )
            doc.select(".govuk-fieldset__legend--xl").text() should be(
              messageFromMessageKey(s"initialGainOrLoss$key.title")
            )
            doc
              .select("label[for='gain']")
              .text()                                        should be("Initial gain amount")
            doc
              .select("label[for='loss']")
              .text()                                        should be("Initial loss amount")
            doc
              .select(".govuk-back-link")
              .attr("href")                                shouldBe routes.InitialGainOrLossController
              .checkYourAnswers()
              .url
            doc
              .select(
                "#main-content details ol > li:nth-child(2)"
              )
              .text()                                        should include(
              messageFromMessageKey(s"initialGainOrLoss$key.details.li2")
            )
            doc
              .select("#main-content details p:first-of-type")
              .text()                                        should startWith(
              messageFromMessageKey(s"initialGainOrLoss$key.details.olTitle")
            )
            doc.select("#initialGainOrLoss-hint").text()     should be(
              messageFromMessageKey(s"initialGainOrLoss$key.helpText")
            )
            doc
              .select("#main-content form")
              .attr("action")                              shouldBe routes.InitialGainOrLossController
              .submitInitialGainOrLoss()
              .url
          }
        )
      }
    }

    "submitting initial gain or loss" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.submitInitialGainOrLoss()(
          FakeRequest().withFormUrlEncodedBody(data*).withMethod("POST")
        )

      def updateDraftReturn(
        d: DraftSingleDisposalReturn,
        newAnswer: AmountInPence
      ): DraftSingleDisposalReturn =
        d.copy(
          initialGainOrLoss = Some(newAnswer),
          reliefDetailsAnswers =
            d.reliefDetailsAnswers.map(_.unsetPrrAndLettingRelief(d.triageAnswers.isPeriodOfAdmin)),
          yearToDateLiabilityAnswers = d.yearToDateLiabilityAnswers.flatMap(_.unsetAllButIncomeDetails())
        )

      behave like redirectToStartBehaviour(() => performAction())

      behave like markUnmetDependencyBehaviour(controller.submitInitialGainOrLoss)

      "show a technical error page" when {

        "there is an error updating the draft return in return service " in {
          val (session, journey, draftReturn) =
            sessionWithState(Some(AmountInPence(1L)))
          val updatedDraftReturn              =
            updateDraftReturn(draftReturn, AmountInPence(0L))
          val updatedJourney                  = journey.copy(draftReturn = updatedDraftReturn)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(updatedJourney)(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction("initialGainOrLoss" -> "2"))
        }

        "there is an error updating the session" in {
          val (session, journey, draftReturn) = sessionWithState(None)
          val updatedDraftReturn              =
            updateDraftReturn(draftReturn, AmountInPence(0L))
          val updatedJourney                  = journey.copy(draftReturn = updatedDraftReturn)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(updatedJourney)(Right(()))
            mockStoreSession(
              session.copy(journeyStatus = Some(updatedJourney))
            )(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction("initialGainOrLoss" -> "2"))
        }
      }

      "show a form error" when {
        val draftReturn = sample[DraftSingleDisposalReturn]
          .copy(initialGainOrLoss = Some(AmountInPence(300L)))

        val fillingOutReturn =
          sample[FillingOutReturn].copy(draftReturn = draftReturn)

        val trustFillingOutReturn = fillingOutReturn.copy(
          subscribedDetails = generateTrustSubscribedDetails(),
          draftReturn = draftReturn.copy(triageAnswers = generateTriageAnswersForTrust())
        )

        val individualFillingOutReturn = fillingOutReturn.copy(
          subscribedDetails = generateIndividualSubscribedDetails(),
          draftReturn = draftReturn.copy(triageAnswers = generateTriageAnswersWithSelf())
        )

        val personalRepresentativeFillingOutReturn =
          fillingOutReturn.copy(draftReturn =
            draftReturn.copy(triageAnswers = generateTriageAnswersWithPersonalRepresentative())
          )

        val capacitorFillingOutReturn =
          individualFillingOutReturn.copy(draftReturn =
            draftReturn.copy(triageAnswers = generateTriageAnswersWithCapacitor())
          )

        val testData = List(
          ("", individualFillingOutReturn, false),
          (".trust", trustFillingOutReturn, false),
          (".personalRep", personalRepresentativeFillingOutReturn, false),
          (".capacitor", capacitorFillingOutReturn, false),
          (".agent", individualFillingOutReturn, true)
        )

        def checkIfValueExistsForKey(expectedErrorMessageKey: String): Assertion = {
          (messages(
            expectedErrorMessageKey
          ) === expectedErrorMessageKey)                   shouldBe false
          messages(expectedErrorMessageKey).trim().isEmpty shouldBe false
        }

        def testFormError(
          fillingOutReturn: FillingOutReturn,
          isAgent: Boolean,
          data: (String, String)*
        )(
          expectedErrorMessageKey: String,
          errorArgs: String*
        )(pageTitleKey: String, titleArgs: String*)(
          performAction: Seq[(String, String)] => Future[Result]
        ): Unit = {

          checkIfValueExistsForKey(expectedErrorMessageKey)

          val session =
            if (isAgent) {
              SessionData.empty.copy(
                userType = Some(UserType.Agent),
                journeyStatus = Some(fillingOutReturn)
              )
            } else {
              SessionData.empty.copy(
                journeyStatus = Some(fillingOutReturn)
              )
            }

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkPageIsDisplayed(
            performAction(data),
            messageFromMessageKey(pageTitleKey, titleArgs),
            doc =>
              doc
                .select("[data-spec='errorSummaryDisplay'] a")
                .text() shouldBe messageFromMessageKey(
                expectedErrorMessageKey,
                errorArgs*
              ),
            BAD_REQUEST
          )
        }

        def test(
          userKey: String,
          fillingOutReturn: FillingOutReturn,
          isAgent: Boolean,
          data: (String, String)*
        )(
          expectedErrorKey: String
        ): Unit =
          testFormError(fillingOutReturn, isAgent, data*)(
            expectedErrorKey
          )(s"initialGainOrLoss$userKey.title")(performAction)

        "no option is selected" in {
          testData.foreach(keyWithReturn =>
            test(
              keyWithReturn._1,
              keyWithReturn._2,
              keyWithReturn._3,
              "initialGainOrLoss" -> "",
              "loss"              -> "",
              "gain"              -> ""
            )(s"initialGainOrLoss${keyWithReturn._1}.error.required")
          )
        }

        "the amount of gain is invalid" in {
          testData.foreach { keyWithReturn =>
            amountOfMoneyErrorScenarios("gain").foreach { scenario =>
              withClue(s"For $scenario: ") {
                val data = ("initialGainOrLoss" -> "0") :: scenario.formData
                test(
                  keyWithReturn._1,
                  keyWithReturn._2,
                  keyWithReturn._3,
                  data*
                )(scenario.expectedErrorMessageKey)
              }
            }
          }
        }

        "the amount of loss is invalid" in {
          testData.foreach { keyWithReturn =>
            amountOfMoneyErrorScenarios("loss").foreach { scenario =>
              withClue(s"For $scenario: ") {
                val data = ("initialGainOrLoss" -> "1") :: scenario.formData
                test(
                  keyWithReturn._1,
                  keyWithReturn._2,
                  keyWithReturn._3,
                  data*
                )(scenario.expectedErrorMessageKey)
              }
            }
          }
        }

        "the amount of gain is zero" in {
          testData.foreach { keyWithReturn =>
            test(
              keyWithReturn._1,
              keyWithReturn._2,
              keyWithReturn._3,
              "initialGainOrLoss" -> "0",
              "gain"              -> "0"
            )(
              "gain.error.tooSmall"
            )
          }
        }
      }

      "redirect to check your answers" when {

        "initial gain or loss has been entered correctly" in {
          forAll(Gen.choose(10L, 100L).map(AmountInPence(_))) { (amountInPence: AmountInPence) =>
            val (session, journey, draftReturn) =
              sessionWithState(Some(AmountInPence(amountInPence.value - 1L)))
            val newDraftReturn                  = updateDraftReturn(draftReturn, amountInPence)
            val updatedJourney                  = journey.copy(draftReturn = newDraftReturn)

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockStoreDraftReturn(updatedJourney)(Right(()))
              mockStoreSession(
                session.copy(journeyStatus = Some(updatedJourney))
              )(Right(()))
            }
            checkIsRedirect(
              performAction(
                "initialGainOrLoss" -> "0",
                "loss"              -> "",
                "gain"              -> s"${amountInPence.inPounds()}"
              ),
              routes.InitialGainOrLossController.checkYourAnswers()
            )
          }
        }

      }

      "should not call returnService" when {
        "the same amount of initialGainOrLoss as in the session draftReturn is entered" in {
          val (session, _, _) =
            sessionWithState(Some(AmountInPence(600L)))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }
          checkIsRedirect(
            performAction(
              "initialGainOrLoss" -> "0",
              "loss"              -> "",
              "gain"              -> "6"
            ),
            routes.InitialGainOrLossController.checkYourAnswers()
          )
        }
      }
    }

    "handling requests to display the check you answers page" must {

      def performAction(): Future[Result] =
        controller.checkYourAnswers()(FakeRequest())

      behave like redirectToStartBehaviour(() => performAction())

      behave like markUnmetDependencyBehaviour(controller.checkYourAnswers())

      "redirect to the initial gain or loss page" when {

        "the question has not been answered yet" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionWithState(None)._1)
          }

          checkIsRedirect(
            performAction(),
            routes.InitialGainOrLossController.enterInitialGainOrLoss()
          )
        }
      }

      "have proper contents" in {
        val draftReturn = sample[DraftSingleDisposalReturn]
          .copy(initialGainOrLoss = Some(AmountInPence(1L)))

        val fillingOutReturn =
          sample[FillingOutReturn].copy(draftReturn = draftReturn)

        val trustFillingOutReturn = fillingOutReturn.copy(
          subscribedDetails = generateTrustSubscribedDetails(),
          draftReturn = draftReturn.copy(triageAnswers = generateTriageAnswersForTrust())
        )

        val individualFillingOutReturn = fillingOutReturn.copy(
          subscribedDetails = generateIndividualSubscribedDetails(),
          draftReturn = draftReturn.copy(triageAnswers = generateTriageAnswersWithSelf())
        )

        val personalRepresentativeFillingOutReturn =
          fillingOutReturn.copy(draftReturn =
            draftReturn.copy(triageAnswers = generateTriageAnswersWithPersonalRepresentative())
          )

        val capacitorFillingOutReturn =
          individualFillingOutReturn.copy(draftReturn =
            draftReturn.copy(triageAnswers = generateTriageAnswersWithCapacitor())
          )

        val testData = List(
          ("", individualFillingOutReturn, false),
          (".trust", trustFillingOutReturn, false),
          (".personalRep", personalRepresentativeFillingOutReturn, false),
          (".capacitor", capacitorFillingOutReturn, false),
          (".agent", individualFillingOutReturn, true)
        )
        testData.foreach(keyWithJourneyStatus =>
          test(
            keyWithJourneyStatus._1,
            keyWithJourneyStatus._2,
            keyWithJourneyStatus._3
          )
        )
      }

      def test(
        userKey: String,
        fillingOutReturn: FillingOutReturn,
        isAgent: Boolean
      ): Unit = {
        val session =
          if (isAgent) {
            SessionData.empty.copy(
              userType = Some(UserType.Agent),
              journeyStatus = Some(fillingOutReturn)
            )
          } else {
            SessionData.empty.copy(
              journeyStatus = Some(fillingOutReturn)
            )
          }
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey(
            "initialGainOrLoss.cya.title"
          ),
          { doc =>
            doc.select("body").html()                        should include(
              Messages("initialGainOrLoss.cya.title")
            )
            doc.select("#initialGainOrLoss-question").text() should include(
              Messages(s"initialGainOrLoss$userKey.title")
            )
            doc
              .select("#content > article > form, #main-content form")
              .attr("action")                              shouldBe routes.InitialGainOrLossController
              .checkYourAnswersSubmit()
              .url
          }
        )
      }
    }

    "handling submits on the check your answers page" must {
      def performAction(): Future[Result] =
        controller.checkYourAnswersSubmit()(FakeRequest())

      behave like redirectToStartBehaviour(() => performAction())

      behave like markUnmetDependencyBehaviour(controller.checkYourAnswersSubmit())

      "redirect to taskList" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionWithState(Some(AmountInPence(1L)))._1)
        }
        checkIsRedirect(
          performAction(),
          controllers.returns.routes.TaskListController.taskList()
        )
      }
    }

  }

  private def generateTrustSubscribedDetails() =
    sample[SubscribedDetails].copy(name = Left(sample[TrustName]))

  private def generateIndividualSubscribedDetails() =
    sample[SubscribedDetails].copy(name = Right(sample[IndividualName]))

  private def generateTriageAnswersWithPersonalRepresentative() =
    sample[IncompleteSingleDisposalTriageAnswers]
      .copy(individualUserType = Some(PersonalRepresentative))

  private def generateTriageAnswersWithCapacitor() =
    sample[IncompleteSingleDisposalTriageAnswers]
      .copy(individualUserType = Some(Capacitor))

  private def generateTriageAnswersWithSelf() =
    sample[IncompleteSingleDisposalTriageAnswers]
      .copy(individualUserType = Some(Self))

  private def generateTriageAnswersForTrust() =
    sample[IncompleteSingleDisposalTriageAnswers]
      .copy(individualUserType = None)

}
