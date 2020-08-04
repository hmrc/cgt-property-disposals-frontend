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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.gainorlossafterreliefs

import cats.syntax.order._
import org.jsoup.nodes.Document
import org.scalatest.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.i18n.{Lang, Messages, MessagesApi, MessagesImpl}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.{Call, Result}
import play.api.test.FakeRequest
import play.api.test.Helpers.BAD_REQUEST
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.AmountOfMoneyErrorScenarios.amountOfMoneyErrorScenarios
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.ReturnsServiceSupport
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport, returns}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, PreviousReturnData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.UserType.{Agent, Individual, Organisation}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.{AmountInPence, MoneyUtils}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.{IndividualName, TrustName}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.SubscribedDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.IndividualUserType.{Capacitor, PersonalRepresentative, PersonalRepresentativeInPeriodOfAdmin, Self}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.MultipleDisposalsTriageAnswers.CompleteMultipleDisposalsTriageAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.RepresenteeAnswers.CompleteRepresenteeAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.CompleteSingleDisposalTriageAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{DraftMultipleDisposalsReturn, DraftSingleDisposalReturn, IndividualUserType, ReturnSummary}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, SessionData, UserType}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService

import scala.concurrent.Future

class GainOrLossAfterReliefsControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with ReturnsServiceSupport
    with ScalaCheckDrivenPropertyChecks
    with RedirectToStartBehaviour {
  def redirectToStartBehaviour(performAction: () => Future[Result]) =
    redirectToStartWhenInvalidJourney(
      performAction,
      {
        case f: FillingOutReturn if f.isFurtherReturn.contains(true) => true
        case _                                                       => false
      }
    )

  lazy val controller = instanceOf[GainOrLossAfterReliefsController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  override val overrideBindings        = List[GuiceableModule](
    bind[AuthConnector].toInstance(mockAuthConnector),
    bind[SessionStore].toInstance(mockSessionStore),
    bind[ReturnsService].toInstance(mockReturnsService)
  )
  implicit lazy val messages: Messages = MessagesImpl(Lang("en"), messagesApi)

  def sessionWithSingleDisposalState(
    gainOrLossAfterReliefs: Option[AmountInPence],
    individualUserType: Option[IndividualUserType] = Some(Self),
    userType: UserType = Individual,
    subscribedDetails: SubscribedDetails = sample[SubscribedDetails].copy(name = Right(sample[IndividualName]))
  ): (SessionData, FillingOutReturn, DraftSingleDisposalReturn) = {
    val draftReturn = sample[DraftSingleDisposalReturn]
      .copy(
        gainOrLossAfterReliefs = gainOrLossAfterReliefs,
        triageAnswers = sample[CompleteSingleDisposalTriageAnswers].copy(
          individualUserType = individualUserType
        ),
        representeeAnswers = Some(sample[CompleteRepresenteeAnswers].copy(isFirstReturn = false))
      )
    val journey     = sample[FillingOutReturn].copy(
      draftReturn = draftReturn,
      subscribedDetails = subscribedDetails,
      previousSentReturns = Some(PreviousReturnData(List(sample[ReturnSummary]), None))
    )

    (
      SessionData.empty.copy(
        journeyStatus = Some(journey),
        userType = Some(userType)
      ),
      journey,
      draftReturn
    )
  }

  def sessionWithMultipleDisposalsState(
    gainOrLossAfterReliefs: Option[AmountInPence],
    individualUserType: Option[IndividualUserType] = Some(Self),
    userType: UserType = Individual,
    subscribedDetails: SubscribedDetails = sample[SubscribedDetails].copy(name = Right(sample[IndividualName]))
  ): (SessionData, FillingOutReturn, DraftMultipleDisposalsReturn) = {
    val draftReturn = sample[DraftMultipleDisposalsReturn]
      .copy(
        gainOrLossAfterReliefs = gainOrLossAfterReliefs,
        triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers].copy(
          individualUserType = individualUserType
        ),
        representeeAnswers = Some(sample[CompleteRepresenteeAnswers].copy(isFirstReturn = false))
      )
    val journey     = sample[FillingOutReturn].copy(
      draftReturn = draftReturn,
      subscribedDetails = subscribedDetails,
      previousSentReturns = Some(PreviousReturnData(List(sample[ReturnSummary]), None))
    )

    (
      SessionData.empty.copy(
        journeyStatus = Some(journey),
        userType = Some(userType)
      ),
      journey,
      draftReturn
    )
  }

  def session(
    gainOrLossAfterReliefs: Option[AmountInPence],
    individualUserType: Option[IndividualUserType] = Some(Self),
    userType: UserType = Individual,
    subscribedDetails: SubscribedDetails = sample[SubscribedDetails].copy(name = Right(sample[IndividualName])),
    isMultipleDisposal: Boolean = false
  ): SessionData =
    if (isMultipleDisposal)
      sessionWithMultipleDisposalsState(gainOrLossAfterReliefs, individualUserType, userType, subscribedDetails)._1
    else sessionWithSingleDisposalState(gainOrLossAfterReliefs, individualUserType, userType, subscribedDetails)._1

  def individualSesssion(gainOrLossAfterReliefs: Option[AmountInPence] = None, isMultipleDisposal: Boolean = false) =
    session(gainOrLossAfterReliefs, isMultipleDisposal = isMultipleDisposal)

  def agentSession(gainOrLossAfterReliefs: Option[AmountInPence] = None, isMultipleDisposal: Boolean = false) =
    session(gainOrLossAfterReliefs, userType = Agent, isMultipleDisposal = isMultipleDisposal)

  def trustSession(gainOrLossAfterReliefs: Option[AmountInPence] = None, isMultipleDisposal: Boolean = false) =
    session(
      gainOrLossAfterReliefs,
      userType = Organisation,
      subscribedDetails = sample[SubscribedDetails].copy(name = Left(sample[TrustName])),
      isMultipleDisposal = isMultipleDisposal
    )

  def capacitorSession(gainOrLossAfterReliefs: Option[AmountInPence] = None, isMultipleDisposal: Boolean = false) =
    session(gainOrLossAfterReliefs, individualUserType = Some(Capacitor), isMultipleDisposal = isMultipleDisposal)

  def personalRepSession(gainOrLossAfterReliefs: Option[AmountInPence] = None, isMultipleDisposal: Boolean = false) =
    session(
      gainOrLossAfterReliefs,
      individualUserType = Some(PersonalRepresentative),
      isMultipleDisposal = isMultipleDisposal
    )

  def periodOfAdminSession(gainOrLossAfterReliefs: Option[AmountInPence] = None, isMultipleDisposal: Boolean = false) =
    session(
      gainOrLossAfterReliefs,
      individualUserType = Some(PersonalRepresentativeInPeriodOfAdmin),
      isMultipleDisposal = isMultipleDisposal
    )

  def agentOfPeriodOfAdminSession(
    gainOrLossAfterReliefs: Option[AmountInPence] = None,
    isMultipleDisposal: Boolean = false
  ) =
    session(
      gainOrLossAfterReliefs,
      userType = Agent,
      individualUserType = Some(PersonalRepresentativeInPeriodOfAdmin),
      isMultipleDisposal = isMultipleDisposal
    )

  val testCasesWithUserKeys = List(
    individualSesssion()          -> "",
    agentSession()                -> ".agent",
    trustSession()                -> ".trust",
    capacitorSession()            -> ".capacitor",
    personalRepSession()          -> ".personalRep",
    periodOfAdminSession()        -> ".personalRepInPeriodOfAdmin",
    agentOfPeriodOfAdminSession() -> ".personalRepInPeriodOfAdmin.agent"
  )

  "GainOrLossAfterReliefsController" when {

    "handling requests to display the gain or loss after reliefs page" must {

      def performAction(): Future[Result] =
        controller.enterGainOrLossAfterReliefs()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      "display the page" when {

        def test(
          sessionData: SessionData,
          expectedTitleKey: String,
          expectedBackLink: Call,
          expectedOuterLabelUserKey: String,
          expectedLinkTextKey: String
        ): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionData)
          }
          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(expectedTitleKey),
            { doc =>
              doc.select("#back").attr("href") shouldBe expectedBackLink.url

              doc.select("#content > article > span").text() shouldBe messageFromMessageKey(
                "gainOrLossAfterReliefs.caption"
              )

              doc.select("#gainOrLossAfterReliefs > div:nth-child(2) > label").text() shouldBe messageFromMessageKey(
                s"gainOrLossAfterReliefs.gain$expectedOuterLabelUserKey.outerLabel"
              )

              doc.select("#gainOrLossAfterReliefs > div:nth-child(4) > label").text() shouldBe messageFromMessageKey(
                s"gainOrLossAfterReliefs.loss$expectedOuterLabelUserKey.outerLabel"
              )

              doc.select("#gainOrLossAfterReliefs > div:nth-child(6) > label").text() shouldBe messageFromMessageKey(
                s"gainOrLossAfterReliefs.noLossOrGain$expectedOuterLabelUserKey.outerLabel"
              )

              doc.select("#howToWorkOut").html() shouldBe messageFromMessageKey(expectedLinkTextKey, "#")

              doc
                .select("#content > article > form")
                .attr("action") shouldBe routes.GainOrLossAfterReliefsController
                .enterGainOrLossAfterReliefsSubmit()
                .url
            }
          )
        }

        "the user is on a single disposal journey and" when {

          "the user is an individual doing the return for themselves" in {
            test(
              individualSesssion(),
              "gainOrLossAfterReliefs.title",
              returns.routes.TaskListController.taskList(),
              "",
              "gainOrLossAfterReliefs.link"
            )
          }

          "the user is an agent for an individual" in {
            test(
              agentSession(),
              "gainOrLossAfterReliefs.agent.title",
              returns.routes.TaskListController.taskList(),
              ".notSelf",
              "gainOrLossAfterReliefs.agent.link"
            )
          }

          "the user is a trust" in {
            test(
              trustSession(Some(AmountInPence(1L))),
              "gainOrLossAfterReliefs.trust.title",
              routes.GainOrLossAfterReliefsController.checkYourAnswers(),
              ".notSelf",
              "gainOrLossAfterReliefs.trust.link"
            )
          }

          "the user is a capacitor" in {
            test(
              capacitorSession(),
              "gainOrLossAfterReliefs.capacitor.title",
              returns.routes.TaskListController.taskList(),
              ".notSelf",
              "gainOrLossAfterReliefs.capacitor.link"
            )
          }

          "the user is a personal rep" in {
            test(
              personalRepSession(),
              "gainOrLossAfterReliefs.personalRep.title",
              returns.routes.TaskListController.taskList(),
              ".notSelf",
              "gainOrLossAfterReliefs.personalRep.link"
            )
          }

          "the user is an estate in a period of admin" in {
            test(
              periodOfAdminSession(Some(AmountInPence(0L))),
              "gainOrLossAfterReliefs.personalRepInPeriodOfAdmin.title",
              routes.GainOrLossAfterReliefsController.checkYourAnswers(),
              ".notSelf",
              "gainOrLossAfterReliefs.personalRepInPeriodOfAdmin.link"
            )
          }

          "the user is an agent of an estate in a period of admin" in {
            test(
              agentOfPeriodOfAdminSession(),
              "gainOrLossAfterReliefs.personalRepInPeriodOfAdmin.agent.title",
              returns.routes.TaskListController.taskList(),
              ".notSelf",
              "gainOrLossAfterReliefs.personalRepInPeriodOfAdmin.agent.link"
            )
          }
        }

        "the user is on a multiple disposals journey and" when {

          "the user is an individual doing the return for themselves" in {
            test(
              individualSesssion(isMultipleDisposal = true),
              "gainOrLossAfterReliefs.multipleDisposals.title",
              returns.routes.TaskListController.taskList(),
              "",
              "gainOrLossAfterReliefs.link"
            )
          }

          "the user is an agent for an individual" in {
            test(
              agentSession(isMultipleDisposal = true),
              "gainOrLossAfterReliefs.agent.multipleDisposals.title",
              returns.routes.TaskListController.taskList(),
              ".notSelf",
              "gainOrLossAfterReliefs.agent.link"
            )
          }

          "the user is a trust" in {
            test(
              trustSession(Some(AmountInPence(1L)), isMultipleDisposal = true),
              "gainOrLossAfterReliefs.trust.multipleDisposals.title",
              routes.GainOrLossAfterReliefsController.checkYourAnswers(),
              ".notSelf",
              "gainOrLossAfterReliefs.trust.link"
            )
          }

          "the user is a capacitor" in {
            test(
              capacitorSession(isMultipleDisposal = true),
              "gainOrLossAfterReliefs.capacitor.multipleDisposals.title",
              returns.routes.TaskListController.taskList(),
              ".notSelf",
              "gainOrLossAfterReliefs.capacitor.link"
            )
          }

          "the user is a personal rep" in {
            test(
              personalRepSession(isMultipleDisposal = true),
              "gainOrLossAfterReliefs.personalRep.multipleDisposals.title",
              returns.routes.TaskListController.taskList(),
              ".notSelf",
              "gainOrLossAfterReliefs.personalRep.link"
            )
          }

          "the user is an estate in a period of admin" in {
            test(
              periodOfAdminSession(Some(AmountInPence(0L)), isMultipleDisposal = true),
              "gainOrLossAfterReliefs.personalRepInPeriodOfAdmin.multipleDisposals.title",
              routes.GainOrLossAfterReliefsController.checkYourAnswers(),
              ".notSelf",
              "gainOrLossAfterReliefs.personalRepInPeriodOfAdmin.link"
            )
          }

          "the user is an agent of an estate in a period of admin" in {
            test(
              agentOfPeriodOfAdminSession(isMultipleDisposal = true),
              "gainOrLossAfterReliefs.personalRepInPeriodOfAdmin.agent.multipleDisposals.title",
              returns.routes.TaskListController.taskList(),
              ".notSelf",
              "gainOrLossAfterReliefs.personalRepInPeriodOfAdmin.agent.link"
            )
          }
        }

      }

    }

    "handling submitted answers to gain or loss after reliefs" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.enterGainOrLossAfterReliefsSubmit()(
          FakeRequest().withFormUrlEncodedBody(data: _*)
        )

      def updateDraftReturn(
        d: DraftSingleDisposalReturn,
        newAnswer: AmountInPence
      ) =
        d.copy(gainOrLossAfterReliefs = Some(newAnswer))

      behave like redirectToStartBehaviour(() => performAction())

      "show a form error" when {

        def testFormError(
          sessionData: SessionData,
          data: (String, String)*
        )(
          pageTitleKey: String,
          expectedErrorMessageKey: String
        ): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionData)
          }

          checkPageIsDisplayed(
            performAction(data: _*),
            messageFromMessageKey(pageTitleKey),
            doc =>
              doc
                .select("#error-summary-display > ul > li > a")
                .text() shouldBe messageFromMessageKey(
                expectedErrorMessageKey
              ),
            BAD_REQUEST
          )
        }

        "no option is selected" in {
          testCasesWithUserKeys.foreach {
            case (session, userKey) =>
              withClue(s"For test case '$userKey': ") {
                testFormError(
                  session
                )(
                  s"gainOrLossAfterReliefs$userKey.title",
                  s"gainOrLossAfterReliefs$userKey.error.required"
                )
              }
          }
        }

        "the amount of gain is invalid" in {
          testCasesWithUserKeys.foreach {
            case (session, userKey) =>
              amountOfMoneyErrorScenarios("gainAfterReliefs")
                .foreach { scenario =>
                  withClue(s"For test case '$userKey' and scenario $scenario: ") {
                    val data = ("gainOrLossAfterReliefs" -> "0") :: scenario.formData

                    testFormError(session, data: _*)(
                      s"gainOrLossAfterReliefs$userKey.title",
                      scenario.expectedErrorMessageKey
                    )
                  }
                }
          }
        }

        "the amount of loss is invalid" in {
          testCasesWithUserKeys.foreach {
            case (session, userKey) =>
              amountOfMoneyErrorScenarios("lossAfterReliefs")
                .foreach { scenario =>
                  withClue(s"For test case '$userKey' and scenario $scenario: ") {
                    val data = ("gainOrLossAfterReliefs" -> "1") :: scenario.formData

                    testFormError(session, data: _*)(
                      s"gainOrLossAfterReliefs$userKey.title",
                      scenario.expectedErrorMessageKey
                    )
                  }
                }
          }
        }

        "the amount of gain is zero" in {
          testCasesWithUserKeys.foreach {
            case (session, userKey) =>
              withClue(s"For test case '$userKey': ") {
                testFormError(
                  session,
                  "gainOrLossAfterReliefs" -> "0",
                  "gainAfterReliefs"       -> "0"
                )(
                  s"gainOrLossAfterReliefs$userKey.title",
                  "gainAfterReliefs.error.tooSmall"
                )
              }
          }
        }

        "the amount of loss is zero" in {
          testCasesWithUserKeys.foreach {
            case (session, userKey) =>
              withClue(s"For test case '$userKey': ") {
                testFormError(
                  session,
                  "gainOrLossAfterReliefs" -> "1",
                  "lossAfterReliefs"       -> "0"
                )(
                  s"gainOrLossAfterReliefs$userKey.title",
                  "lossAfterReliefs.error.tooSmall"
                )
              }
          }
        }
      }

      "show a technical error page" when {

        "there is an error updating the draft return in return service " in {
          val (session, journey, draftReturn) = sessionWithSingleDisposalState(Some(AmountInPence(1L)))
          val updatedDraftReturn              = updateDraftReturn(draftReturn, AmountInPence(0L))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(
              updatedDraftReturn,
              journey.subscribedDetails.cgtReference,
              journey.agentReferenceNumber
            )(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction("gainOrLossAfterReliefs" -> "2"))
        }

        "there is an error updating the session" in {
          val (session, journey, draftReturn) = sessionWithSingleDisposalState(None)
          val updatedDraftReturn              =
            updateDraftReturn(draftReturn, AmountInPence(0L))
          val updatedJourney                  = journey.copy(draftReturn = updatedDraftReturn)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(
              updatedDraftReturn,
              journey.subscribedDetails.cgtReference,
              journey.agentReferenceNumber
            )(Right(()))
            mockStoreSession(
              session.copy(journeyStatus = Some(updatedJourney))
            )(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction("gainOrLossAfterReliefs" -> "2"))
        }
      }

      "redirect to check your answers" when {

        def test(
          newGainOrLossAfterReliefs: AmountInPence,
          formData: (String, String)*
        ) = {
          val (session, journey, draftReturn) = sessionWithSingleDisposalState(None)
          val newDraftReturn                  = updateDraftReturn(draftReturn, newGainOrLossAfterReliefs)
          val updatedJourney                  = journey.copy(draftReturn = newDraftReturn)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(
              newDraftReturn,
              updatedJourney.subscribedDetails.cgtReference,
              updatedJourney.agentReferenceNumber
            )(Right(()))
            mockStoreSession(
              session.copy(journeyStatus = Some(updatedJourney))
            )(Right(()))
          }
          checkIsRedirect(
            performAction(formData: _*),
            routes.GainOrLossAfterReliefsController.checkYourAnswers()
          )
        }

        "a valid gain has been submitted" in {
          test(
            AmountInPence(1000L),
            "gainOrLossAfterReliefs" -> "0",
            "gainAfterReliefs"       -> "10.00"
          )
        }

        "a valid loss has been submitted" in {
          test(
            AmountInPence(-1000L),
            "gainOrLossAfterReliefs" -> "1",
            "lossAfterReliefs"       -> "10.00"
          )
        }

        "no loss and no gain has been submitted" in {
          test(
            AmountInPence(0L),
            "gainOrLossAfterReliefs" -> "2"
          )
        }

      }

      "not perform any updates" when {

        "the same amount of gainOrLossAfterReliefs as in the session draftReturn is entered" in {
          val session = sessionWithSingleDisposalState(Some(AmountInPence(600L)))._1

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }
          checkIsRedirect(
            performAction(
              "gainOrLossAfterReliefs" -> "0",
              "gainAfterReliefs"       -> "6"
            ),
            routes.GainOrLossAfterReliefsController.checkYourAnswers()
          )
        }

      }
    }

    "handling requests to display the check you answers page" must {

      def performAction(): Future[Result] =
        controller.checkYourAnswers()(FakeRequest())

      behave like redirectToStartBehaviour(() => performAction())

      "redirect to the gain or loss after reliefs page" when {

        "the question has not been answered yet" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionWithSingleDisposalState(None)._1)
          }

          checkIsRedirect(
            performAction(),
            routes.GainOrLossAfterReliefsController.enterGainOrLossAfterReliefs()
          )
        }
      }

      "display the page" when {

        def test(
          sessionData: SessionData,
          expectedQuestionKey: String,
          expectedQuestionValueKey: String,
          expectedAmountKey: Option[String],
          expectedAmountValue: Option[String]
        ): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionData)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(
              "gainOrLossAfterReliefs.cya.title"
            ),
            { doc =>
              doc.select("#content > article > h1").text() shouldBe messageFromMessageKey(
                "gainOrLossAfterReliefs.cya.title"
              )

              doc.select("#gainOrLossAfterReliefs-question").text() shouldBe messageFromMessageKey(expectedQuestionKey)
              doc.select("#gainOrLossAfterReliefs-answer").text()   shouldBe messageFromMessageKey(
                expectedQuestionValueKey
              )

              doc.select("#gainOrLossAfterReliefsValue-question").text() shouldBe expectedAmountKey
                .map(messageFromMessageKey(_))
                .getOrElse("")
              doc.select("#gainOrLossAfterReliefsValue-answer").text()   shouldBe expectedAmountValue
                .getOrElse("")

              doc
                .select("#content > article > form")
                .attr("action") shouldBe routes.GainOrLossAfterReliefsController
                .checkYourAnswersSubmit()
                .url
            }
          )
        }

        "the user is an individual doing the return for themselves" in {
          test(
            individualSesssion(Some(AmountInPence(600L)), isMultipleDisposal = true),
            "gainOrLossAfterReliefs.multipleDisposals.title",
            "gainOrLossAfterReliefs.gain.outerLabel",
            Some("gainOrLossAfterReliefs.gain.innerLabel"),
            Some("£6")
          )
        }

        "the user is an agent of an individual" in {
          test(
            agentSession(Some(AmountInPence(-600L))),
            "gainOrLossAfterReliefs.agent.title",
            "gainOrLossAfterReliefs.loss.notSelf.outerLabel",
            Some("gainOrLossAfterReliefs.loss.innerLabel"),
            Some("£6")
          )
        }

        "the user is a trust" in {
          test(
            trustSession(Some(AmountInPence(0L))),
            "gainOrLossAfterReliefs.trust.title",
            "gainOrLossAfterReliefs.noLossOrGain.notSelf.outerLabel",
            None,
            None
          )
        }

        "the user is a capacitor" in {
          test(
            capacitorSession(Some(AmountInPence(0L))),
            "gainOrLossAfterReliefs.capacitor.title",
            "gainOrLossAfterReliefs.noLossOrGain.notSelf.outerLabel",
            None,
            None
          )
        }

        "the user is a personal rep" in {
          test(
            personalRepSession(Some(AmountInPence(0L)), isMultipleDisposal = true),
            "gainOrLossAfterReliefs.personalRep.multipleDisposals.title",
            "gainOrLossAfterReliefs.noLossOrGain.notSelf.outerLabel",
            None,
            None
          )
        }

        "the user is a personal rep in period of admin" in {
          test(
            periodOfAdminSession(Some(AmountInPence(0L))),
            "gainOrLossAfterReliefs.personalRepInPeriodOfAdmin.title",
            "gainOrLossAfterReliefs.noLossOrGain.notSelf.outerLabel",
            None,
            None
          )
        }

        "the user is a agent of a personal rep in periof of admin" in {
          test(
            agentOfPeriodOfAdminSession(Some(AmountInPence(0L))),
            "gainOrLossAfterReliefs.personalRepInPeriodOfAdmin.agent.title",
            "gainOrLossAfterReliefs.noLossOrGain.notSelf.outerLabel",
            None,
            None
          )
        }

      }
    }

    "handling submits on the check your answers page" must {
      def performAction(): Future[Result] =
        controller.checkYourAnswersSubmit()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      "redirect to taskList" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionWithSingleDisposalState(Some(AmountInPence(1L)))._1)
        }
        checkIsRedirect(
          performAction(),
          controllers.returns.routes.TaskListController.taskList()
        )
      }
    }

  }

}

object GainOrLossAfterReliefsControllerSpec extends Matchers {

  def validateGainOrLossOrReliefsCheckYourAnswersPage(
    gainOrLossAfterReliefs: AmountInPence,
    doc: Document,
    isATrust: Boolean,
    isAnAgent: Boolean,
    individualUserType: Option[IndividualUserType],
    isMultipleDisposal: Boolean
  )(implicit messages: MessagesApi, lang: Lang): Unit = {
    val expectedQuestionKey = {
      val userTypeKey          = individualUserType match {
        case Some(PersonalRepresentative)                              => ".personalRep"
        case Some(PersonalRepresentativeInPeriodOfAdmin) if isAnAgent  => ".personalRepInPeriodOfAdmin.agent"
        case Some(PersonalRepresentativeInPeriodOfAdmin) if !isAnAgent => ".personalRepInPeriodOfAdmin"
        case Some(Capacitor)                                           => ".capacitor"
        case _                                                         =>
          if (isAnAgent) ".agent"
          else if (isATrust) ".trust"
          else ""
      }
      val multipleDisposalKeys = if (isMultipleDisposal) ".multipleDisposals" else ""
      s"gainOrLossAfterReliefs$userTypeKey$multipleDisposalKeys.title"
    }

    val (expectedQuestionValueKey, expectedAmountKey, expectedAmountValue) = {
      val userKey = individualUserType match {
        case Some(Self) if !isATrust && !isAnAgent => ""
        case _                                     => ".notSelf"
      }

      if (gainOrLossAfterReliefs > AmountInPence.zero)
        (
          s"gainOrLossAfterReliefs.gain$userKey.outerLabel",
          Some("gainOrLossAfterReliefs.gain.innerLabel"),
          Some(MoneyUtils.formatAmountOfMoneyWithPoundSign(gainOrLossAfterReliefs.inPounds()))
        )
      else if (gainOrLossAfterReliefs < AmountInPence.zero)
        (
          s"gainOrLossAfterReliefs.loss$userKey.outerLabel",
          Some("gainOrLossAfterReliefs.loss.innerLabel"),
          Some(MoneyUtils.formatAmountOfMoneyWithPoundSign(gainOrLossAfterReliefs.inPounds().abs))
        )
      else
        (s"gainOrLossAfterReliefs.noGainOrLoss$userKey.outerLabel", None, None)
    }

    doc.select("#gainOrLossAfterReliefs-question").text() shouldBe messages(expectedQuestionKey)
    doc.select("#gainOrLossAfterReliefs-answer").text()   shouldBe messages(expectedQuestionValueKey)

    doc.select("#gainOrLossAfterReliefsValue-question").text() shouldBe expectedAmountKey
      .map(messages(_))
      .getOrElse("")

    doc.select("#gainOrLossAfterReliefsValue-answer").text() shouldBe expectedAmountValue
      .getOrElse("")
  }

}
