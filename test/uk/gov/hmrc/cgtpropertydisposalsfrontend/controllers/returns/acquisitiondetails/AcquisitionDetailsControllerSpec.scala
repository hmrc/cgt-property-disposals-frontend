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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.acquisitiondetails

import java.time.LocalDate

import org.jsoup.nodes.Document
import org.scalatest.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.http.Status.BAD_REQUEST
import play.api.i18n.{Lang, Messages, MessagesApi, MessagesImpl}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.{Call, Result}
import play.api.test.FakeRequest
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.UserType.{Agent, Individual, Organisation}
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.RebasingCutoffDates
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.acquisitiondetails.AcquisitionDetailsControllerSpec.validateAcquisitionDetailsCheckYourAnswersPage
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.RebasingCutoffDates._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.AmountOfMoneyErrorScenarios.amountOfMoneyErrorScenarios
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.DateErrorScenarios._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.ReturnsServiceSupport
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.FillingOutReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.AmountInPence
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.MoneyUtils.formatAmountOfMoneyWithPoundSign
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.AgentReferenceNumber
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.{IndividualName, TrustName}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.SubscribedDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.AcquisitionDetailsAnswers.{CompleteAcquisitionDetailsAnswers, IncompleteAcquisitionDetailsAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.IncompleteSingleDisposalTriageAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{AcquisitionMethod, _}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService

import scala.concurrent.Future

class AcquisitionDetailsControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with ReturnsServiceSupport
    with ScalaCheckDrivenPropertyChecks
    with RedirectToStartBehaviour {
  lazy val controller  = instanceOf[AcquisitionDetailsController]
  val mockRebasingUtil = new RebasingEligibilityUtil()
  override val overrideBindings =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore),
      bind[ReturnsService].toInstance(mockReturnsService),
      bind[RebasingEligibilityUtil].toInstance(mockRebasingUtil)
    )

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  implicit lazy val messages: Messages = MessagesImpl(Lang("en"), messagesApi)

  def userMessageKey(userType: UserType): String = userType match {
    case UserType.Individual   => ""
    case UserType.Organisation => ".trust"
    case UserType.Agent        => ".agent"
  }

  def userTypeClue(userType: UserType): String = userType match {
    case UserType.Individual   => "an individual"
    case UserType.Organisation => "a trust"
    case UserType.Agent        => "an agent"
  }

  def setAgentReferenceNumber(userType: UserType): Option[AgentReferenceNumber] = userType match {
    case UserType.Agent => Some(sample[AgentReferenceNumber])
    case _              => None
  }

  def setNameForUserType(userType: UserType): Either[TrustName, IndividualName] = userType match {
    case UserType.Organisation => Left(sample[TrustName])
    case _                     => Right(sample[IndividualName])
  }

  def redirectToStartBehaviour(performAction: () => Future[Result]) =
    redirectToStartWhenInvalidJourney(
      performAction, {
        case _: FillingOutReturn => true
        case _                   => false
      }
    )

  def sessionWithState(
    answers: AcquisitionDetailsAnswers,
    assetType: AssetType,
    wasUkResident: Boolean,
    userType: UserType,
    disposalDate: DisposalDate = sample[DisposalDate]
  ): (SessionData, FillingOutReturn, DraftSingleDisposalReturn) =
    sessionWithState(Some(answers), Some(assetType), Some(wasUkResident), userType, Some(disposalDate))

  def sessionWithState(
    answers: Option[AcquisitionDetailsAnswers],
    assetType: Option[AssetType],
    wasUkResident: Option[Boolean],
    userType: UserType,
    disposalDate: Option[DisposalDate]
  ): (SessionData, FillingOutReturn, DraftSingleDisposalReturn) = {
    val draftReturn =
      sample[DraftSingleDisposalReturn].copy(
        triageAnswers = sample[IncompleteSingleDisposalTriageAnswers].copy(
          assetType      = assetType,
          wasAUKResident = wasUkResident,
          disposalDate   = disposalDate
        ),
        acquisitionDetailsAnswers = answers
      )

    val journey = sample[FillingOutReturn].copy(
      draftReturn          = draftReturn,
      agentReferenceNumber = setAgentReferenceNumber(userType),
      subscribedDetails = sample[SubscribedDetails].copy(
        name = setNameForUserType(userType)
      )
    )

    (
      SessionData.empty.copy(
        userType      = Some(userType),
        journeyStatus = Some(journey)
      ),
      journey,
      draftReturn
    )
  }

  def commonUpdateDraftReturn(d: DraftSingleDisposalReturn, newAnswers: AcquisitionDetailsAnswers) =
    d.copy(
      acquisitionDetailsAnswers = Some(newAnswers),
      initialGainOrLoss         = None,
      reliefDetailsAnswers = d.reliefDetailsAnswers.map(
        _.unset(_.privateResidentsRelief).unset(_.lettingsRelief)
      ),
      yearToDateLiabilityAnswers = d.yearToDateLiabilityAnswers.flatMap(_.unsetAllButIncomeDetails())
    )

  "AcquisitionDetailsController" when {

    "handling requests to display the acquisition method page" must {

      def performAction(): Future[Result] = controller.acquisitionMethod()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      "display the page" when {

        "an individual has not completed the acquisition details section of the return" in {
          List(Some(IncompleteAcquisitionDetailsAnswers.empty), None).foreach { answers =>
            withClue(s"For answers $answers: ") {
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithState(answers, None, None, UserType.Individual, Some(sample[DisposalDate]))._1
                )
              }

              checkPageIsDisplayed(
                performAction(),
                messageFromMessageKey("acquisitionMethod.title"), { doc =>
                  doc.select("#back").attr("href") shouldBe controllers.returns.routes.TaskListController.taskList().url
                  doc.select("#content > article > form").attr("action") shouldBe routes.AcquisitionDetailsController
                    .acquisitionMethodSubmit()
                    .url
                }
              )

            }
          }
        }

        "an agent has not completed the acquisition details section of the return" in {
          List(Some(IncompleteAcquisitionDetailsAnswers.empty), None).foreach { answers =>
            withClue(s"For answers $answers: ") {
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(sessionWithState(answers, None, None, UserType.Agent, Some(sample[DisposalDate]))._1)
              }

              checkPageIsDisplayed(
                performAction(),
                messageFromMessageKey("acquisitionMethod.agent.title"), { doc =>
                  doc.select("#back").attr("href") shouldBe controllers.returns.routes.TaskListController.taskList().url
                  doc.select("#content > article > form").attr("action") shouldBe routes.AcquisitionDetailsController
                    .acquisitionMethodSubmit()
                    .url
                }
              )

            }
          }
        }

        "a trust has not completed the acquisition details section of the return" in {
          List(Some(IncompleteAcquisitionDetailsAnswers.empty), None).foreach { answers =>
            withClue(s"For answers $answers: ") {
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithState(answers, None, None, UserType.Organisation, Some(sample[DisposalDate]))._1
                )
              }

              checkPageIsDisplayed(
                performAction(),
                messageFromMessageKey("acquisitionMethod.trust.title"), { doc =>
                  doc.select("#back").attr("href") shouldBe controllers.returns.routes.TaskListController.taskList().url
                  doc.select("#content > article > form").attr("action") shouldBe routes.AcquisitionDetailsController
                    .acquisitionMethodSubmit()
                    .url
                }
              )

            }
          }
        }

        "an individual has already completed the acquisition details section of the return" in {
          List(Some(IncompleteAcquisitionDetailsAnswers.empty), None).foreach { answers =>
            withClue(s"For answers $answers: ") {
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithState(
                    sample[CompleteAcquisitionDetailsAnswers],
                    sample[AssetType],
                    sample[Boolean],
                    UserType.Individual,
                    sample[DisposalDate]
                  )._1
                )
              }

              checkPageIsDisplayed(
                performAction(),
                messageFromMessageKey("acquisitionMethod.title"), { doc =>
                  doc.select("#back").attr("href") shouldBe routes.AcquisitionDetailsController.checkYourAnswers().url
                  doc.select("#content > article > form").attr("action") shouldBe routes.AcquisitionDetailsController
                    .acquisitionMethodSubmit()
                    .url
                }
              )

            }
          }
        }

        "an agent has already completed the acquisition details section of the return" in {
          List(Some(IncompleteAcquisitionDetailsAnswers.empty), None).foreach { answers =>
            withClue(s"For answers $answers: ") {
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithState(
                    sample[CompleteAcquisitionDetailsAnswers],
                    sample[AssetType],
                    sample[Boolean],
                    UserType.Agent
                  )._1
                )
              }

              checkPageIsDisplayed(
                performAction(),
                messageFromMessageKey("acquisitionMethod.agent.title"), { doc =>
                  doc.select("#back").attr("href") shouldBe routes.AcquisitionDetailsController.checkYourAnswers().url
                  doc.select("#content > article > form").attr("action") shouldBe routes.AcquisitionDetailsController
                    .acquisitionMethodSubmit()
                    .url
                }
              )

            }
          }
        }

        "a trust has already completed the acquisition details section of the return" in {
          List(Some(IncompleteAcquisitionDetailsAnswers.empty), None).foreach { answers =>
            withClue(s"For answers $answers: ") {
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithState(
                    sample[CompleteAcquisitionDetailsAnswers],
                    sample[AssetType],
                    sample[Boolean],
                    UserType.Organisation
                  )._1
                )
              }

              checkPageIsDisplayed(
                performAction(),
                messageFromMessageKey("acquisitionMethod.trust.title"), { doc =>
                  doc.select("#back").attr("href") shouldBe routes.AcquisitionDetailsController.checkYourAnswers().url
                  doc.select("#content > article > form").attr("action") shouldBe routes.AcquisitionDetailsController
                    .acquisitionMethodSubmit()
                    .url
                }
              )

            }
          }
        }
      }
    }

    "handling submitted answers to the acquisition method page" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.acquisitionMethodSubmit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      behave like redirectToStartBehaviour(() => performAction())

      "show a form error" when {

        def test(userType: UserType, data: (String, String)*)(expectedErrorMessageKey: String): Unit =
          testFormError(userType, data: _*)(expectedErrorMessageKey)(
            s"acquisitionMethod${userMessageKey(userType)}.title"
          )(
            performAction
          )

        "individual submits nothing" in {
          test(Individual)("acquisitionMethod.error.required")
        }

        "trust submits nothing" in {
          test(Organisation)("acquisitionMethod.trust.error.required")
        }

        "agent submits nothing" in {
          test(Agent)("acquisitionMethod.agent.error.required")
        }

        "individual submits an unknown value" in {
          test(Individual, "acquisitionMethod" -> "4")("acquisitionMethod.error.required")
        }

        "trust submits an unknown value" in {
          test(Organisation, "acquisitionMethod" -> "4")("acquisitionMethod.trust.error.required")
        }

        "agent submits an unknown value" in {
          test(Agent, "acquisitionMethod" -> "4")("acquisitionMethod.agent.error.required")
        }

        "other is selected with a value" that {

          "individual enters acquisition method that doesn't exist" in {
            test(Individual, "acquisitionMethod" -> "3")("otherAcquisitionMethod.error.required")
          }

          "trust enters acquisition method that doesn't exist" in {
            test(Organisation, "acquisitionMethod" -> "3")("otherAcquisitionMethod.trust.error.required")
          }

          "agent enters acquisition method that doesn't exist" in {
            test(Agent, "acquisitionMethod" -> "3")("otherAcquisitionMethod.agent.error.required")
          }

          "individual enters acquisition method that is empty" in {
            test(Individual, "acquisitionMethod" -> "3", "otherAcquisitionMethod" -> "")(
              "otherAcquisitionMethod.error.required"
            )
          }

          "trust enters acquisition method that is empty" in {
            test(Organisation, "acquisitionMethod" -> "3", "otherAcquisitionMethod" -> "")(
              "otherAcquisitionMethod.trust.error.required"
            )
          }

          "agent enters acquisition method that is empty" in {
            test(Agent, "acquisitionMethod" -> "3", "otherAcquisitionMethod" -> "")(
              "otherAcquisitionMethod.agent.error.required"
            )
          }

          "individual enters acquisition method that contains invalid characters" in {
            test(Individual, "acquisitionMethod" -> "3", "otherAcquisitionMethod" -> "1,234")(
              "otherAcquisitionMethod.error.invalid"
            )
          }

          "trust enters acquisition method that contains invalid characters" in {
            test(Organisation, "acquisitionMethod" -> "3", "otherAcquisitionMethod" -> "1,234")(
              "otherAcquisitionMethod.trust.error.invalid"
            )
          }

          "agent enters acquisition method that contains invalid characters" in {
            test(Agent, "acquisitionMethod" -> "3", "otherAcquisitionMethod" -> "1,234")(
              "otherAcquisitionMethod.agent.error.invalid"
            )
          }

          "individual enters acquisition method that is too long" in {
            test(Individual, "acquisitionMethod" -> "3", "otherAcquisitionMethod" -> ("a" * 36))(
              "otherAcquisitionMethod.error.tooLong"
            )
          }

          "trust enters acquisition method that is too long" in {
            test(Organisation, "acquisitionMethod" -> "3", "otherAcquisitionMethod" -> ("a" * 36))(
              "otherAcquisitionMethod.trust.error.tooLong"
            )
          }

          "agent enters acquisition method that is too long" in {
            test(Agent, "acquisitionMethod" -> "3", "otherAcquisitionMethod" -> ("a" * 36))(
              "otherAcquisitionMethod.agent.error.tooLong"
            )
          }

        }

      }

      "show an error page" when {

        val (method, methodValue)           = AcquisitionMethod.Bought -> 0
        val (session, journey, draftReturn) = sessionWithState(None, None, None, UserType.Individual, None)
        val updatedDraftReturn = commonUpdateDraftReturn(
          draftReturn,
          IncompleteAcquisitionDetailsAnswers.empty.copy(acquisitionMethod = Some(method))
        )
        val updatedSession = session.copy(journeyStatus = Some(journey.copy(draftReturn = updatedDraftReturn)))

        "there is an error updating the draft return" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(
              updatedDraftReturn,
              journey.subscribedDetails.cgtReference,
              journey.agentReferenceNumber
            )(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction("acquisitionMethod" -> methodValue.toString))
        }

        "there is an error updating the session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(
              updatedDraftReturn,
              journey.subscribedDetails.cgtReference,
              journey.agentReferenceNumber
            )(Right(()))
            mockStoreSession(updatedSession)(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction("acquisitionMethod" -> methodValue.toString))
        }

      }

      "redirect to the check your answers page" when {

        "the acquisition details journey is incomplete and" when {

          def test(data: (String, String)*)(method: AcquisitionMethod): Unit = {
            val (session, journey, draftReturn) = sessionWithState(None, None, None, UserType.Individual, None)
            val updatedDraftReturn = commonUpdateDraftReturn(
              draftReturn,
              IncompleteAcquisitionDetailsAnswers.empty.copy(acquisitionMethod = Some(method))
            )
            val updatedSession = session.copy(journeyStatus = Some(journey.copy(draftReturn = updatedDraftReturn)))

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockStoreDraftReturn(
                updatedDraftReturn,
                journey.subscribedDetails.cgtReference,
                journey.agentReferenceNumber
              )(Right(()))
              mockStoreSession(updatedSession)(Right(()))
            }

            checkIsRedirect(performAction(data: _*), routes.AcquisitionDetailsController.checkYourAnswers())
          }

          "the user selects bought it" in {
            test("acquisitionMethod" -> "0")(AcquisitionMethod.Bought)
          }

          "the user selects inherited it" in {
            test("acquisitionMethod" -> "1")(AcquisitionMethod.Inherited)
          }

          "the user selects was gifted it" in {
            test("acquisitionMethod" -> "2")(AcquisitionMethod.Gifted)
          }

          "the user selects other" in {
            test("acquisitionMethod" -> "3", "otherAcquisitionMethod" -> "things")(AcquisitionMethod.Other("things"))
          }

        }

        "the acquisition details journey is complete and" when {

          def test(data: (String, String)*)(oldMethod: AcquisitionMethod, method: AcquisitionMethod): Unit = {
            val answers = sample[CompleteAcquisitionDetailsAnswers].copy(acquisitionMethod = oldMethod)
            val (session, journey, draftReturn) =
              sessionWithState(answers, sample[AssetType], sample[Boolean], UserType.Individual)
            val updatedAnswers = IncompleteAcquisitionDetailsAnswers(
              Some(method),
              Some(answers.acquisitionDate),
              None,
              None,
              Some(answers.improvementCosts),
              None,
              None
            )

            val updatedDraftReturn = commonUpdateDraftReturn(draftReturn, updatedAnswers)
            val updatedSession     = session.copy(journeyStatus = Some(journey.copy(draftReturn = updatedDraftReturn)))

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockStoreDraftReturn(
                updatedDraftReturn,
                journey.subscribedDetails.cgtReference,
                journey.agentReferenceNumber
              )(Right(()))
              mockStoreSession(updatedSession)(Right(()))
            }

            checkIsRedirect(performAction(data: _*), routes.AcquisitionDetailsController.checkYourAnswers())
          }

          "the user selects bought it" in {
            test("acquisitionMethod" -> "0")(AcquisitionMethod.Inherited, AcquisitionMethod.Bought)
          }

          "the user selects inherited it" in {
            test("acquisitionMethod" -> "1")(AcquisitionMethod.Bought, AcquisitionMethod.Inherited)
          }

          "the user selects was gifted it" in {
            test("acquisitionMethod" -> "2")(AcquisitionMethod.Bought, AcquisitionMethod.Gifted)
          }

          "the user selects other" in {
            test("acquisitionMethod" -> "3", "otherAcquisitionMethod" -> "things")(
              AcquisitionMethod.Bought,
              AcquisitionMethod.Other("things")
            )
          }
        }
      }

      "not update the draft return or session" when {

        "the user has selected the same as before" in {
          val (incompleteAnswers, completeAnswers) = sample[IncompleteAcquisitionDetailsAnswers] -> sample[
            CompleteAcquisitionDetailsAnswers
          ]
          List(
            Seq("acquisitionMethod" -> "0") -> incompleteAnswers.copy(acquisitionMethod = Some(AcquisitionMethod.Bought)
            ),
            Seq("acquisitionMethod" -> "1") -> incompleteAnswers.copy(acquisitionMethod =
              Some(AcquisitionMethod.Inherited)
            ),
            Seq("acquisitionMethod" -> "2") -> incompleteAnswers.copy(acquisitionMethod = Some(AcquisitionMethod.Gifted)
            ),
            Seq("acquisitionMethod" -> "3", "otherAcquisitionMethod" -> "other things") -> incompleteAnswers
              .copy(acquisitionMethod = Some(AcquisitionMethod.Other("other things"))),
            Seq("acquisitionMethod" -> "0") -> completeAnswers.copy(acquisitionMethod = AcquisitionMethod.Bought),
            Seq("acquisitionMethod" -> "1") -> completeAnswers.copy(acquisitionMethod = AcquisitionMethod.Inherited),
            Seq("acquisitionMethod" -> "2") -> completeAnswers.copy(acquisitionMethod = AcquisitionMethod.Gifted),
            Seq("acquisitionMethod" -> "3", "otherAcquisitionMethod" -> "other things") -> completeAnswers
              .copy(acquisitionMethod = AcquisitionMethod.Other("other things"))
          ).foreach {
            case (data, answers) =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(sessionWithState(answers, sample[AssetType], sample[Boolean], UserType.Individual)._1)
              }

              checkIsRedirect(performAction(data: _*), routes.AcquisitionDetailsController.checkYourAnswers())

          }
        }

      }
    }

    "handling requests to display the acquisition date page" must {

      def performAction(): Future[Result] = controller.acquisitionDate()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      "redirect to the task list page" when {

        "there is no disposal date in the session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                Some(sample[CompleteAcquisitionDetailsAnswers]),
                Some(sample[AssetType]),
                Some(sample[Boolean]),
                UserType.Individual,
                None
              )._1
            )
          }

          checkIsRedirect(performAction(), controllers.returns.routes.TaskListController.taskList())
        }

      }

      "redirect to the acquisition method page" when {

        "that question has not yet been answered" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[IncompleteAcquisitionDetailsAnswers].copy(acquisitionMethod = None),
                sample[AssetType],
                sample[Boolean],
                UserType.Individual
              )._1
            )
          }

          checkIsRedirect(performAction(), routes.AcquisitionDetailsController.acquisitionMethod())
        }

      }

      "display the page" when {

        "an individual has not yet completed the acquisition details section" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[IncompleteAcquisitionDetailsAnswers].copy(
                  acquisitionMethod = Some(AcquisitionMethod.Bought)
                ),
                sample[AssetType],
                sample[Boolean],
                UserType.Individual
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("acquisitionDate.title"), { doc =>
              doc.select("#back").attr("href") shouldBe routes.AcquisitionDetailsController.acquisitionMethod().url
              doc.select("#content > article > form").attr("action") shouldBe routes.AcquisitionDetailsController
                .acquisitionDateSubmit()
                .url
            }
          )
        }

        "a trust has not yet completed the acquisition details section" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[IncompleteAcquisitionDetailsAnswers].copy(
                  acquisitionMethod = Some(AcquisitionMethod.Bought)
                ),
                sample[AssetType],
                sample[Boolean],
                UserType.Organisation
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("acquisitionDate.trust.title"), { doc =>
              doc.select("#back").attr("href") shouldBe routes.AcquisitionDetailsController.acquisitionMethod().url
              doc.select("#content > article > form").attr("action") shouldBe routes.AcquisitionDetailsController
                .acquisitionDateSubmit()
                .url
            }
          )
        }

        "an agent has not yet completed the acquisition details section" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[IncompleteAcquisitionDetailsAnswers].copy(
                  acquisitionMethod = Some(AcquisitionMethod.Bought)
                ),
                sample[AssetType],
                sample[Boolean],
                UserType.Agent
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("acquisitionDate.agent.title"), { doc =>
              doc.select("#back").attr("href") shouldBe routes.AcquisitionDetailsController.acquisitionMethod().url
              doc.select("#content > article > form").attr("action") shouldBe routes.AcquisitionDetailsController
                .acquisitionDateSubmit()
                .url
            }
          )
        }

        "an individual has already completed the acquisition details section" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[CompleteAcquisitionDetailsAnswers],
                sample[AssetType],
                sample[Boolean],
                UserType.Individual
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("acquisitionDate.title"), { doc =>
              doc.select("#back").attr("href") shouldBe routes.AcquisitionDetailsController.checkYourAnswers().url
              doc.select("#content > article > form").attr("action") shouldBe routes.AcquisitionDetailsController
                .acquisitionDateSubmit()
                .url
            }
          )
        }

        "a trust has already completed the acquisition details section" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[CompleteAcquisitionDetailsAnswers],
                sample[AssetType],
                sample[Boolean],
                UserType.Organisation
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("acquisitionDate.trust.title"), { doc =>
              doc.select("#back").attr("href") shouldBe routes.AcquisitionDetailsController.checkYourAnswers().url
              doc.select("#content > article > form").attr("action") shouldBe routes.AcquisitionDetailsController
                .acquisitionDateSubmit()
                .url
            }
          )
        }

        "an agent has already completed the acquisition details section" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[CompleteAcquisitionDetailsAnswers],
                sample[AssetType],
                sample[Boolean],
                UserType.Agent
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("acquisitionDate.agent.title"), { doc =>
              doc.select("#back").attr("href") shouldBe routes.AcquisitionDetailsController.checkYourAnswers().url
              doc.select("#content > article > form").attr("action") shouldBe routes.AcquisitionDetailsController
                .acquisitionDateSubmit()
                .url
            }
          )
        }

      }

    }

    "handling submitted answers to the acquisition date page" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.acquisitionDateSubmit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      def formData(date: LocalDate): List[(String, String)] =
        List(
          "acquisitionDate-day"   -> date.getDayOfMonth.toString,
          "acquisitionDate-month" -> date.getMonthValue.toString,
          "acquisitionDate-year"  -> date.getYear.toString
        )

      val disposalDate = DisposalDate(LocalDate.of(2020, 1, 1), sample[TaxYear])

      behave like redirectToStartBehaviour(() => performAction())

      "redirect to the task list page" when {

        "there is no disposal date in the session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                Some(sample[CompleteAcquisitionDetailsAnswers]),
                Some(sample[AssetType]),
                Some(sample[Boolean]),
                UserType.Individual,
                None
              )._1
            )
          }

          checkIsRedirect(performAction(), controllers.returns.routes.TaskListController.taskList())
        }

      }

      "show a form error" when {

        def test(userType: UserType, data: (String, String)*)(expectedErrorKey: String): Unit =
          testFormError(userType, data: _*)(expectedErrorKey)(s"acquisitionDate${userMessageKey(userType)}.title")(
            performAction,
            sessionWithState(
              sample[CompleteAcquisitionDetailsAnswers],
              sample[AssetType],
              sample[Boolean],
              userType,
              disposalDate
            )._1
          )

        "individual enters date that is invalid" in {
          dateErrorScenarios("acquisitionDate", userMessageKey(Individual)).foreach {
            case d @ DateErrorScenario(dayString, monthString, yearString, expectedErrorKey) =>
              withClue(s"For $d: ") {
                val formData =
                  List(
                    "acquisitionDate-day"   -> dayString,
                    "acquisitionDate-month" -> monthString,
                    "acquisitionDate-year"  -> yearString
                  ).collect { case (id, Some(input)) => id -> input }

                test(Individual, formData: _*)(expectedErrorKey)
              }
          }
        }

        "trust enters date that is invalid" in {
          dateErrorScenarios("acquisitionDate", userMessageKey(Organisation)).foreach {
            case d @ DateErrorScenario(dayString, monthString, yearString, expectedErrorKey) =>
              withClue(s"For $d: ") {
                val formData =
                  List(
                    "acquisitionDate-day"   -> dayString,
                    "acquisitionDate-month" -> monthString,
                    "acquisitionDate-year"  -> yearString
                  ).collect { case (id, Some(input)) => id -> input }

                test(Organisation, formData: _*)(expectedErrorKey)
              }
          }
        }

        "agent enters date that is invalid" in {
          dateErrorScenarios("acquisitionDate", userMessageKey(Agent)).foreach {
            case d @ DateErrorScenario(dayString, monthString, yearString, expectedErrorKey) =>
              withClue(s"For $d: ") {
                val formData =
                  List(
                    "acquisitionDate-day"   -> dayString,
                    "acquisitionDate-month" -> monthString,
                    "acquisitionDate-year"  -> yearString
                  ).collect { case (id, Some(input)) => id -> input }

                test(Agent, formData: _*)(expectedErrorKey)
              }
          }
        }

        "individual enters date that is after the disposal date" in {
          val tomorrow = disposalDate.value.plusDays(1L)

          test(Individual, formData(tomorrow): _*)("acquisitionDate.error.tooFarInFuture")
        }

        "trust enters date that is after the disposal date" in {
          val tomorrow = disposalDate.value.plusDays(1L)

          test(Organisation, formData(tomorrow): _*)("acquisitionDate.trust.error.tooFarInFuture")
        }

        "agent enters date that is after the disposal date" in {
          val tomorrow = disposalDate.value.plusDays(1L)

          test(Agent, formData(tomorrow): _*)("acquisitionDate.agent.error.tooFarInFuture")
        }

      }

      "show an error page" when {

        val acquisitionDate = AcquisitionDate(disposalDate.value)
        val answers = sample[CompleteAcquisitionDetailsAnswers]
          .copy(acquisitionDate = AcquisitionDate(acquisitionDate.value.plusDays(1L)))
        val wasUkResident = sample[Boolean]
        val (session, journey, draftReturn) =
          sessionWithState(answers, sample[AssetType], wasUkResident, UserType.Individual, disposalDate)

        val newAnswers =
          IncompleteAcquisitionDetailsAnswers(
            Some(answers.acquisitionMethod),
            Some(acquisitionDate),
            None,
            None,
            Some(answers.improvementCosts),
            Some(answers.acquisitionFees),
            None
          )

        val updatedDraftReturn = commonUpdateDraftReturn(draftReturn, newAnswers)
        val updatedSession     = session.copy(journeyStatus = Some(journey.copy(draftReturn = updatedDraftReturn)))

        "there is an error updating the draft return" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(
              updatedDraftReturn,
              journey.subscribedDetails.cgtReference,
              journey.agentReferenceNumber
            )(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction(formData(acquisitionDate.value): _*))
        }

        "there is an error updating the session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(
              updatedDraftReturn,
              journey.subscribedDetails.cgtReference,
              journey.agentReferenceNumber
            )(Right(()))
            mockStoreSession(updatedSession)(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction(formData(acquisitionDate.value): _*))
        }
      }

      "update the session correctly and redirect to the cya page" when {
        def test(
          assetType: AssetType,
          wasUkResident: Boolean,
          submittedAcquisitionDate: LocalDate,
          oldAnswers: AcquisitionDetailsAnswers,
          newAnswers: AcquisitionDetailsAnswers
        ): Unit = {
          val (session, journey, draftReturn) =
            sessionWithState(oldAnswers, assetType, wasUkResident, UserType.Individual, disposalDate)
          val updatedDraftReturn = commonUpdateDraftReturn(draftReturn, newAnswers)
          val updatedSession     = session.copy(journeyStatus = Some(journey.copy(draftReturn = updatedDraftReturn)))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(
              updatedDraftReturn,
              journey.subscribedDetails.cgtReference,
              journey.agentReferenceNumber
            )(Right(()))
            mockStoreSession(updatedSession)(Right(()))

          }

          checkIsRedirect(
            performAction(formData(submittedAcquisitionDate): _*),
            routes.AcquisitionDetailsController.checkYourAnswers()
          )
        }

        "the user has not answered this question before" in {
          val date = disposalDate.value
          val oldAnswers = IncompleteAcquisitionDetailsAnswers.empty.copy(
            acquisitionMethod = Some(AcquisitionMethod.Bought)
          )

          test(
            sample[AssetType],
            sample[Boolean],
            date,
            oldAnswers,
            oldAnswers.copy(acquisitionDate = Some(AcquisitionDate(date)))
          )
        }

        "the user has completed this section before" in {
          val date = disposalDate.value
          val oldAnswers = sample[CompleteAcquisitionDetailsAnswers].copy(
            acquisitionDate = AcquisitionDate(date.minusDays(1L))
          )
          val newAnswers = IncompleteAcquisitionDetailsAnswers(
            Some(oldAnswers.acquisitionMethod),
            Some(AcquisitionDate(date)),
            None,
            None,
            Some(oldAnswers.improvementCosts),
            Some(oldAnswers.acquisitionFees),
            None
          )

          test(
            sample[AssetType],
            sample[Boolean],
            date,
            oldAnswers,
            newAnswers
          )
        }

      }

    }

    "handling requests to display the acquisition price page" must {

      def performAction(): Future[Result] = controller.acquisitionPrice()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      behave like missingAcquisitionDateBehaviour(performAction)

      behave like missingAcquisitionMethodBehaviour(performAction)

      "display the page" when {

        def incompleteSection(userType: UserType): Unit = forAll { acquisitionMethod: AcquisitionMethod =>
          val acquisitionDate = sample[AcquisitionDate]
          val answers = sample[IncompleteAcquisitionDetailsAnswers].copy(
            acquisitionMethod = Some(acquisitionMethod),
            acquisitionDate   = Some(acquisitionDate)
          )
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                answers,
                sample[AssetType],
                sample[Boolean],
                userType
              )._1
            )
          }

          val pricePageTitle = acquisitionMethod match {
            case AcquisitionMethod.Bought =>
              messageFromMessageKey(s"acquisitionPriceBought${userMessageKey(userType)}.title")
            case _ =>
              messageFromMessageKey(
                s"acquisitionPriceNotBought${userMessageKey(userType)}.title",
                LocalDateUtils.govDisplayFormat(acquisitionDate.value)
              )
          }

          checkPageIsDisplayed(
            performAction(),
            pricePageTitle, { doc =>
              doc.select("#back").attr("href") shouldBe routes.AcquisitionDetailsController.acquisitionDate().url
              doc.select("#content > article > form").attr("action") shouldBe routes.AcquisitionDetailsController
                .acquisitionPriceSubmit()
                .url
            }
          )
        }

        "an individual has not yet completed the acquisition details section" in {
          incompleteSection(Individual)
        }

        "a trust has not yet completed the acquisition details section" in {
          incompleteSection(Organisation)
        }

        "an agent has not yet completed the acquisition details section" in {
          incompleteSection(Agent)
        }

        def sectionCompleted(userType: UserType): Unit = {
          val answers = sample[CompleteAcquisitionDetailsAnswers]
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                answers,
                sample[AssetType],
                sample[Boolean],
                userType
              )._1
            )
          }

          val pricePageTitle = answers.acquisitionMethod match {
            case AcquisitionMethod.Bought =>
              messageFromMessageKey(s"acquisitionPriceBought${userMessageKey(userType)}.title")
            case _ =>
              messageFromMessageKey(
                s"acquisitionPriceNotBought${userMessageKey(userType)}.title",
                LocalDateUtils.govDisplayFormat(answers.acquisitionDate.value)
              )
          }

          checkPageIsDisplayed(
            performAction(),
            pricePageTitle, { doc =>
              doc.select("#back").attr("href") shouldBe routes.AcquisitionDetailsController.checkYourAnswers().url
              doc.select("#content > article > form").attr("action") shouldBe routes.AcquisitionDetailsController
                .acquisitionPriceSubmit()
                .url
            }
          )
        }

        "an individual has already completed the acquisition details section" in {
          sectionCompleted(Individual)
        }

        "a trust has already completed the acquisition details section" in {
          sectionCompleted(Organisation)
        }

        "an agent has already completed the acquisition details section" in {
          sectionCompleted(Agent)
        }
      }

    }

    "handling submitted acquisition prices" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.acquisitionPriceSubmit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      behave like redirectToStartBehaviour(() => performAction())

      behave like missingAcquisitionDateBehaviour(() => performAction())

      behave like missingAcquisitionMethodBehaviour(() => performAction())

      "show a form error" when {

        def invalidPrice(userType: UserType): Unit =
          forAll { answers: CompleteAcquisitionDetailsAnswers =>
            val scenarioSession = sessionWithState(
              answers,
              sample[AssetType],
              sample[Boolean],
              userType
            )._1

            val contextKey = answers.acquisitionMethod match {
              case AcquisitionMethod.Bought => s"acquisitionPriceBought${userMessageKey(userType)}"
              case _                        => s"acquisitionPriceNotBought${userMessageKey(userType)}"
            }

            amountOfMoneyErrorScenarios(s"acquisitionPrice${userMessageKey(userType)}", errorContext = Some(contextKey))
              .foreach { scenario =>
                withClue(s"For $scenario: ") {
                  testFormError(userType, scenario.formData: _*)(scenario.expectedErrorMessageKey)(
                    s"$contextKey.title",
                    LocalDateUtils.govDisplayFormat(answers.acquisitionDate.value)
                  )(
                    performAction,
                    scenarioSession
                  )
                }
              }
          }

//        Todo broken

        "an individual provides an invalid data" in {
          invalidPrice(Individual)
        }

        "a trust provides an invalid data" in {
          invalidPrice(Organisation)
        }

        "an agent provides an invalid data" in {
          invalidPrice(Agent)
        }

        def amountZero(userType: UserType): Unit =
          forAll { answers: CompleteAcquisitionDetailsAnswers =>
            val scenarioSession = sessionWithState(
              answers,
              sample[AssetType],
              sample[Boolean],
              userType
            )._1

            val contextKey = answers.acquisitionMethod match {
              case AcquisitionMethod.Bought => s"acquisitionPriceBought${userMessageKey(userType)}"
              case _                        => s"acquisitionPriceNotBought${userMessageKey(userType)}"
            }
            testFormError(userType, "acquisitionPrice" -> "0")(
              s"$contextKey.error.tooSmall"
            )(
              s"$contextKey.title",
              LocalDateUtils.govDisplayFormat(answers.acquisitionDate.value)
            )(
              performAction,
              scenarioSession
            )
          }

        "an individual enters zero for amount" in {
          amountZero(Individual)
        }

        "a trust enters zero for amount" in {
          amountZero(Organisation)
        }

        "an agent enters zero for amount" in {
          amountZero(Agent)
        }

      }

      "show an error page" when {

        val price = 1.23d
        val answers = IncompleteAcquisitionDetailsAnswers.empty
          .copy(acquisitionMethod = Some(AcquisitionMethod.Bought), acquisitionDate = Some(sample[AcquisitionDate]))
        val (session, journey, draftReturn) =
          sessionWithState(answers, sample[AssetType], sample[Boolean], UserType.Individual)
        val updatedDraftReturn = commonUpdateDraftReturn(
          draftReturn,
          answers.copy(acquisitionPrice = Some(AmountInPence(123L)))
        )
        val updatedSession = session.copy(journeyStatus = Some(journey.copy(draftReturn = updatedDraftReturn)))

        "there is an error updating the draft return" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(
              updatedDraftReturn,
              journey.subscribedDetails.cgtReference,
              journey.agentReferenceNumber
            )(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction("acquisitionPrice" -> price.toString))
        }

        "there is an error updating the session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(
              updatedDraftReturn,
              journey.subscribedDetails.cgtReference,
              journey.agentReferenceNumber
            )(Right(()))
            mockStoreSession(updatedSession)(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction("acquisitionPrice" -> price.toString))
        }

      }

      "redirect to the check you answers page" when {

        "the price submitted is valid and the journey was incomplete" in {
          val answers = IncompleteAcquisitionDetailsAnswers.empty
            .copy(acquisitionMethod = Some(sample[AcquisitionMethod]), acquisitionDate = Some(sample[AcquisitionDate]))
          val (session, journey, draftReturn) =
            sessionWithState(answers, sample[AssetType], sample[Boolean], UserType.Individual)
          val updatedDraftReturn = commonUpdateDraftReturn(
            draftReturn,
            answers.copy(acquisitionPrice = Some(AmountInPence(123400L)))
          )
          val updatedSession = session.copy(journeyStatus = Some(journey.copy(draftReturn = updatedDraftReturn)))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(
              updatedDraftReturn,
              journey.subscribedDetails.cgtReference,
              journey.agentReferenceNumber
            )(Right(()))
            mockStoreSession(updatedSession)(Right(()))
          }

          checkIsRedirect(
            performAction("acquisitionPrice" -> "£1,234"),
            routes.AcquisitionDetailsController.checkYourAnswers()
          )
        }

        "the price submitted is valid and the journey was complete" in {
          val answers = sample[CompleteAcquisitionDetailsAnswers]
          val (session, journey, draftReturn) =
            sessionWithState(answers, sample[AssetType], sample[Boolean], UserType.Individual)
          val updatedDraftReturn =
            commonUpdateDraftReturn(
              draftReturn,
              answers.copy(acquisitionPrice = AmountInPence(123456L))
            )
          val updatedSession = session.copy(journeyStatus = Some(journey.copy(draftReturn = updatedDraftReturn)))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(
              updatedDraftReturn,
              journey.subscribedDetails.cgtReference,
              journey.agentReferenceNumber
            )(Right(()))
            mockStoreSession(updatedSession)(Right(()))
          }

          checkIsRedirect(
            performAction("acquisitionPrice" -> "£1,234.56"),
            routes.AcquisitionDetailsController.checkYourAnswers()
          )

        }

      }

    }

    "handling requests to display the rebased acquisition price page" must {

      def performAction(): Future[Result] = controller.rebasedAcquisitionPrice()(FakeRequest())

      val acquisitionDate = AcquisitionDate(LocalDate.ofEpochDay(0L))

      behave like redirectToStartBehaviour(performAction)

      behave like missingAssetTypeAndResidentialStatusBehaviour(performAction)

      behave like missingAcquisitionDateBehaviour(performAction)

      "redirect to the acquisition price page" when {

        "that question hasn't been answered for eligible rebase from non uk resident" in {

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[IncompleteAcquisitionDetailsAnswers].copy(
                  acquisitionDate  = Some(AcquisitionDate(nonUkResidentsResidentialProperty.minusDays(2))),
                  acquisitionPrice = None
                ),
                AssetType.Residential,
                false,
                UserType.Individual
              )._1
            )
          }

          checkIsRedirect(
            performAction(),
            routes.AcquisitionDetailsController.acquisitionPrice()
          )
        }

      }

      "that question hasn't been answered for eligible rebase from uk resident" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            sessionWithState(
              sample[IncompleteAcquisitionDetailsAnswers].copy(
                acquisitionDate   = Some(AcquisitionDate(RebasingCutoffDates.ukResidents.minusDays(2))),
                acquisitionMethod = None
              ),
              sample[AssetType],
              true,
              UserType.Individual
            )._1
          )
        }

        checkIsRedirect(
          performAction(),
          routes.AcquisitionDetailsController.acquisitionDate()
        )
      }

      "redirect to the check your answers page" when {

        "the user does not meet the rebasing criteria" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[CompleteAcquisitionDetailsAnswers].copy(
                  acquisitionDate = AcquisitionDate(ukResidents.plusDays(1))
                ),
                AssetType.Residential,
                true,
                UserType.Individual
              )._1
            )
          }
          checkIsRedirect(performAction(), routes.AcquisitionDetailsController.checkYourAnswers())

        }

      }

      "display the page" when {

        "the acquisition details section has not yet been completed for non uk resident" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[IncompleteAcquisitionDetailsAnswers].copy(
                  acquisitionDate =
                    Some(AcquisitionDate(RebasingCutoffDates.nonUkResidentsResidentialProperty.minusDays(1))),
                  acquisitionPrice = None
                ),
                AssetType.Residential,
                false,
                UserType.Individual
              )._1
            )
          }
          checkIsRedirect(performAction(), routes.AcquisitionDetailsController.acquisitionPrice())

        }

        def incompleteSectionUkResident(userType: UserType): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[IncompleteAcquisitionDetailsAnswers].copy(
                  acquisitionDate   = Some(AcquisitionDate(RebasingCutoffDates.ukResidents.minusDays(2))),
                  acquisitionPrice  = Some(sample[AmountInPence]),
                  acquisitionMethod = Some(AcquisitionMethod.Bought)
                ),
                AssetType.Residential,
                true,
                userType
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(
              "rebaseAcquisitionPrice.title",
              LocalDateUtils.govDisplayFormat(ukResidents)
            ), { doc =>
              doc.select("#back").attr("href") shouldBe routes.AcquisitionDetailsController.acquisitionDate().url
              doc.select("#content > article > form").attr("action") shouldBe routes.AcquisitionDetailsController
                .rebasedAcquisitionPriceSubmit()
                .url
              doc.select("#rebaseAcquisitionPrice-form-hint").text() shouldBe messageFromMessageKey(
                s"rebaseAcquisitionPrice${userMessageKey(userType)}.helpText"
              )
            }
          )
        }

        "an individual has not yet completed the rebase acquisition details section for uk resident" in {
          incompleteSectionUkResident(Individual)
        }

        "a trust has not yet completed the rebase acquisition details section for uk resident" in {
          incompleteSectionUkResident(Organisation)
        }

        "an Agent has not yet completed the rebase acquisition details section for uk resident" in {
          incompleteSectionUkResident(Agent)
        }

        def incompleteSectionNonUKResident(userType: UserType): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[IncompleteAcquisitionDetailsAnswers].copy(
                  acquisitionDate  = Some(acquisitionDate),
                  acquisitionPrice = Some(sample[AmountInPence])
                ),
                AssetType.Residential,
                false,
                userType
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(
              "rebaseAcquisitionPrice.title",
              LocalDateUtils.govDisplayFormat(nonUkResidentsResidentialProperty.minusDays(1))
            ), { doc =>
              doc.select("#back").attr("href") shouldBe routes.AcquisitionDetailsController.acquisitionPrice().url
              doc.select("#content > article > form").attr("action") shouldBe routes.AcquisitionDetailsController
                .rebasedAcquisitionPriceSubmit()
                .url
              doc.select("#rebaseAcquisitionPrice-form-hint").text() shouldBe messageFromMessageKey(
                s"rebaseAcquisitionPrice${userMessageKey(userType)}.helpText"
              )
            }
          )
        }

        "an individual has not yet completed the rebase acquisition details section for non-uk resident" in {
          incompleteSectionNonUKResident(Individual)
        }

        "a trust has not yet completed the rebase acquisition details section for non-uk resident" in {
          incompleteSectionNonUKResident(Organisation)
        }

        "an Agent has not yet completed the rebase acquisition details section for non-uk resident" in {
          incompleteSectionNonUKResident(Agent)
        }

        def sectionCompleted(userType: UserType): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[CompleteAcquisitionDetailsAnswers].copy(acquisitionDate = acquisitionDate),
                AssetType.Residential,
                true,
                userType
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(
              "rebaseAcquisitionPrice.title",
              LocalDateUtils.govDisplayFormat(ukResidents)
            ), { doc =>
              doc.select("#back").attr("href") shouldBe routes.AcquisitionDetailsController.checkYourAnswers().url
              doc.select("#content > article > form").attr("action") shouldBe routes.AcquisitionDetailsController
                .rebasedAcquisitionPriceSubmit()
                .url
            }
          )
        }

        "an individual has already completed the acquisition details section" in {
          sectionCompleted(Individual)
        }

        "a trust has already completed the acquisition details section" in {
          sectionCompleted(Organisation)
        }

        "an agent has already completed the acquisition details section" in {
          sectionCompleted(Agent)
        }

        def amountNonZero(userType: UserType): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[CompleteAcquisitionDetailsAnswers].copy(
                  acquisitionDate         = acquisitionDate,
                  rebasedAcquisitionPrice = Some(AmountInPence(1L))
                ),
                AssetType.Residential,
                true,
                userType
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(
              "rebaseAcquisitionPrice.title",
              LocalDateUtils.govDisplayFormat(ukResidents)
            ), { doc =>
              doc.select("#rebaseAcquisitionPrice").attr("value") shouldBe "0.01"
            }
          )
        }

        "the amount in the session is non-zero for individual" in {
          amountNonZero(Individual)
        }

        "the amount in the session is non-zero for trust" in {
          amountNonZero(Organisation)
        }

        "the amount in the session is non-zero for agent" in {
          amountNonZero(Agent)
        }

      }

    }

    "handling submitted rebased acquisition price answers" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.rebasedAcquisitionPriceSubmit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      val acquisitionDate = AcquisitionDate(ukResidents.minusDays(1))

      behave like redirectToStartBehaviour(() => performAction())

      behave like missingAssetTypeAndResidentialStatusBehaviour(() => performAction())

      behave like missingAcquisitionDateBehaviour(() => performAction())

      "redirect to the acquisition price page" when {

        "that question hasn't been answered" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[IncompleteAcquisitionDetailsAnswers].copy(
                  acquisitionDate =
                    Some(AcquisitionDate(RebasingCutoffDates.nonUkResidentsNonResidentialProperty.minusDays(1))),
                  acquisitionPrice = None
                ),
                AssetType.NonResidential,
                false,
                UserType.Individual
              )._1
            )
          }

          checkIsRedirect(
            performAction(),
            routes.AcquisitionDetailsController.acquisitionPrice()
          )
        }

      }

      "show a form error" when {

        def test(data: (String, String)*)(expectedErrorMessageKey: String) = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[CompleteAcquisitionDetailsAnswers].copy(acquisitionDate = acquisitionDate),
                AssetType.Residential,
                true,
                UserType.Individual
              )._1
            )
          }

          val formattedRebaseDate = LocalDateUtils.govDisplayFormat(ukResidents)
          checkPageIsDisplayed(
            performAction(data: _*),
            messageFromMessageKey(
              "rebaseAcquisitionPrice.title",
              formattedRebaseDate
            ), { doc =>
              doc.select("#error-summary-display > ul > li > a").text() shouldBe messageFromMessageKey(
                expectedErrorMessageKey,
                formattedRebaseDate
              )
            },
            BAD_REQUEST
          )
        }

        "the amount of money is zero" in {
          test("rebaseAcquisitionPrice" -> "0")(
            "rebaseAcquisitionPrice.error.tooSmall"
          )
        }

      }

      "show an error page" when {
        val price = 1.23d
        val answers = IncompleteAcquisitionDetailsAnswers.empty.copy(
          acquisitionDate  = Some(AcquisitionDate(ukResidents.minusDays(2))),
          acquisitionPrice = Some(sample[AmountInPence])
        )
        val (session, journey, draftReturn) =
          sessionWithState(answers, AssetType.Residential, true, UserType.Individual)
        val updatedDraftReturn = commonUpdateDraftReturn(
          draftReturn,
          answers.copy(
            rebasedAcquisitionPrice = Some(AmountInPence(123L)),
            acquisitionPrice        = Some(AmountInPence(123L)),
            shouldUseRebase         = Some(true)
          )
        )
        val updatedSession = session.copy(journeyStatus = Some(journey.copy(draftReturn = updatedDraftReturn)))

        "there is an error updating the draft return" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(
              updatedDraftReturn,
              journey.subscribedDetails.cgtReference,
              journey.agentReferenceNumber
            )(Left(Error("")))
          }

          checkIsTechnicalErrorPage(
            performAction(
              "rebaseAcquisitionPrice" -> price.toString
            )
          )
        }

        "there is an error updating the session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(
              updatedDraftReturn,
              journey.subscribedDetails.cgtReference,
              journey.agentReferenceNumber
            )(Right(()))
            mockStoreSession(updatedSession)(Left(Error("")))
          }

          checkIsTechnicalErrorPage(
            performAction(
              "rebaseAcquisitionPrice" -> price.toString
            )
          )
        }

      }

      "redirect to the check you answers page" when {
        val scenarios = List(
          List("rebaseAcquisitionPrice" -> "£1,234") -> AmountInPence(123400L),
          List("rebaseAcquisitionPrice" -> "1")      -> AmountInPence(100L)
        )

        "the price submitted is valid and the journey was incomplete" in {
          scenarios.foreach {
            case (formData, expectedAmountInPence) =>
              withClue(s"For form data $formData and expected amount in pence $expectedAmountInPence: ") {
                val answers = IncompleteAcquisitionDetailsAnswers.empty.copy(
                  acquisitionDate  = Some(acquisitionDate),
                  acquisitionPrice = Some(sample[AmountInPence])
                )
                val (session, journey, draftReturn) =
                  sessionWithState(answers, AssetType.Residential, true, UserType.Individual)
                val updatedDraftReturn = commonUpdateDraftReturn(
                  draftReturn,
                  answers.copy(
                    rebasedAcquisitionPrice = Some(expectedAmountInPence),
                    acquisitionPrice        = Some(expectedAmountInPence),
                    shouldUseRebase         = Some(true)
                  )
                )
                val updatedSession = session.copy(journeyStatus = Some(journey.copy(draftReturn = updatedDraftReturn)))

                inSequence {
                  mockAuthWithNoRetrievals()
                  mockGetSession(session)
                  mockStoreDraftReturn(
                    updatedDraftReturn,
                    journey.subscribedDetails.cgtReference,
                    journey.agentReferenceNumber
                  )(Right(()))
                  mockStoreSession(updatedSession)(Right(()))
                }

                checkIsRedirect(
                  performAction(formData: _*),
                  routes.AcquisitionDetailsController.checkYourAnswers()
                )
              }
          }
        }

        "the price submitted is valid and the journey was complete" in {
          scenarios.foreach {
            case (formData, expectedAmountInPence) =>
              withClue(s"For form data $formData and expected amount in pence $expectedAmountInPence: ") {
                val answers = sample[CompleteAcquisitionDetailsAnswers].copy(
                  acquisitionDate         = acquisitionDate,
                  rebasedAcquisitionPrice = Some(AmountInPence(expectedAmountInPence.value + 1L))
                )
                val (session, journey, draftReturn) =
                  sessionWithState(answers, AssetType.Residential, true, UserType.Individual)
                val updatedDraftReturn = commonUpdateDraftReturn(
                  draftReturn,
                  answers.copy(
                    rebasedAcquisitionPrice = Some(expectedAmountInPence),
                    acquisitionPrice        = expectedAmountInPence,
                    shouldUseRebase         = true
                  )
                )
                val updatedSession = session.copy(journeyStatus = Some(journey.copy(draftReturn = updatedDraftReturn)))

                inSequence {
                  mockAuthWithNoRetrievals()
                  mockGetSession(session)
                  mockStoreDraftReturn(
                    updatedDraftReturn,
                    journey.subscribedDetails.cgtReference,
                    journey.agentReferenceNumber
                  )(Right(()))
                  mockStoreSession(updatedSession)(Right(()))
                }

                checkIsRedirect(
                  performAction(formData: _*),
                  routes.AcquisitionDetailsController.checkYourAnswers()
                )

              }
          }
        }
      }

    }

    "handling requests to display the improvement costs page" must {

      def performAction(): Future[Result] =
        controller.improvementCosts()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      behave like missingAssetTypeAndResidentialStatusBehaviour(performAction)

      behave like missingAcquisitionDateBehaviour(performAction)

      "redirect to the acquisition price page" when {

        "the user does not meet the rebasing criteria and the user has not supplied an acquisition price yet" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[IncompleteAcquisitionDetailsAnswers].copy(
                  acquisitionDate  = Some(AcquisitionDate(ukResidents)),
                  acquisitionPrice = None
                ),
                AssetType.Residential,
                true,
                UserType.Individual
              )._1
            )
          }

          checkIsRedirect(
            performAction(),
            routes.AcquisitionDetailsController.acquisitionPrice()
          )
        }

      }

      "redirect to the rebased acquisition price page" when {

        "the user does meet the rebasing criteria and the user has not supplied an acquisition price yet" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[IncompleteAcquisitionDetailsAnswers].copy(
                  acquisitionDate         = Some(AcquisitionDate(ukResidents.minusDays(1L))),
                  rebasedAcquisitionPrice = None
                ),
                AssetType.Residential,
                true,
                UserType.Individual
              )._1
            )
          }

          checkIsRedirect(
            performAction(),
            routes.AcquisitionDetailsController.rebasedAcquisitionPrice()
          )
        }

      }

      "display the page" when {

        def incompleteSection(userType: UserType): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[IncompleteAcquisitionDetailsAnswers].copy(
                  acquisitionDate         = Some(AcquisitionDate(ukResidents.minusDays(1L))),
                  rebasedAcquisitionPrice = Some(sample[AmountInPence])
                ),
                AssetType.Residential,
                true,
                userType
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(s"improvementCosts${userMessageKey(userType)}.title"), { doc =>
              doc.select("#back").attr("href") shouldBe routes.AcquisitionDetailsController
                .rebasedAcquisitionPrice()
                .url
              doc.select("#content > article > form").attr("action") shouldBe routes.AcquisitionDetailsController
                .improvementCostsSubmit()
                .url
            }
          )
        }

        "an individual meets the rebasing criteria and their acquisition details journey is incomplete" in {
          incompleteSection(Individual)
        }

        "a trust meets the rebasing criteria and their acquisition details journey is incomplete" in {
          incompleteSection(Organisation)
        }

        "an agent meets the rebasing criteria and their acquisition details journey is incomplete" in {
          incompleteSection(Agent)
        }

        "the user does not meet the rebasing criteria and their acquisition details journey is incomplete" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[IncompleteAcquisitionDetailsAnswers].copy(
                  acquisitionDate         = Some(AcquisitionDate(ukResidents)),
                  acquisitionPrice        = Some(sample[AmountInPence]),
                  rebasedAcquisitionPrice = Some(sample[AmountInPence])
                ),
                AssetType.Residential,
                true,
                UserType.Individual
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("improvementCosts.title"), { doc =>
              doc.select("#back").attr("href") shouldBe routes.AcquisitionDetailsController.acquisitionPrice().url
              doc.select("#content > article > form").attr("action") shouldBe routes.AcquisitionDetailsController
                .improvementCostsSubmit()
                .url
            }
          )
        }

        "the user meets the rebasing criteria and their acquisition details journey is complete" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[CompleteAcquisitionDetailsAnswers].copy(
                  acquisitionDate         = AcquisitionDate(ukResidents.minusDays(1L)),
                  rebasedAcquisitionPrice = Some(sample[AmountInPence])
                ),
                AssetType.Residential,
                true,
                UserType.Individual
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("improvementCosts.title"), { doc =>
              doc.select("#back").attr("href") shouldBe routes.AcquisitionDetailsController.checkYourAnswers().url
              doc.select("#content > article > form").attr("action") shouldBe routes.AcquisitionDetailsController
                .improvementCostsSubmit()
                .url
            }
          )
        }

        "the user does not meet the rebasing criteria and their acquisition details journey is complete" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[CompleteAcquisitionDetailsAnswers].copy(
                  acquisitionDate         = AcquisitionDate(ukResidents),
                  rebasedAcquisitionPrice = Some(sample[AmountInPence])
                ),
                AssetType.Residential,
                true,
                UserType.Individual
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("improvementCosts.title"), { doc =>
              doc.select("#back").attr("href") shouldBe routes.AcquisitionDetailsController.checkYourAnswers().url
              doc.select("#content > article > form").attr("action") shouldBe routes.AcquisitionDetailsController
                .improvementCostsSubmit()
                .url
            }
          )
        }

        "the amount in the session is zero" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[CompleteAcquisitionDetailsAnswers].copy(
                  acquisitionDate         = AcquisitionDate(ukResidents),
                  rebasedAcquisitionPrice = Some(sample[AmountInPence]),
                  improvementCosts        = AmountInPence.zero
                ),
                AssetType.Residential,
                true,
                UserType.Individual
              )._1
            )

          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(
              "improvementCosts.title"
            ), { doc =>
              doc.select("#improvementCosts-1").attr("checked") shouldBe "checked"
            }
          )
        }

        "the amount in the session is non-zero" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[CompleteAcquisitionDetailsAnswers].copy(
                  acquisitionDate         = AcquisitionDate(ukResidents),
                  rebasedAcquisitionPrice = Some(sample[AmountInPence]),
                  improvementCosts        = AmountInPence(2L)
                ),
                AssetType.Residential,
                true,
                UserType.Individual
              )._1
            )

          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(
              "improvementCosts.title"
            ), { doc =>
              doc.select("#improvementCosts-0").attr("checked")  shouldBe "checked"
              doc.select("#improvementCostsValue").attr("value") shouldBe "0.02"
            }
          )
        }

      }

    }

    "handling submitted improvement costs answers" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.improvementCostsSubmit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      val acquisitionDate = AcquisitionDate(LocalDate.ofEpochDay(0L))

      behave like redirectToStartBehaviour(() => performAction())

      behave like missingAssetTypeAndResidentialStatusBehaviour(() => performAction())

      behave like missingAcquisitionDateBehaviour(() => performAction())

      "redirect to the acquisition price page" when {

        "the user does not meet the rebasing criteria and the user has not supplied an acquisition price yet" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[IncompleteAcquisitionDetailsAnswers].copy(
                  acquisitionDate  = Some(AcquisitionDate(ukResidents)),
                  acquisitionPrice = None
                ),
                AssetType.Residential,
                true,
                UserType.Individual
              )._1
            )
          }

          checkIsRedirect(
            performAction(),
            routes.AcquisitionDetailsController.acquisitionPrice()
          )
        }

      }

      "redirect to the rebased acquisition price page" when {

        "the user does meet the rebasing criteria and the user has not supplied an acquisition price yet" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[IncompleteAcquisitionDetailsAnswers].copy(
                  acquisitionDate         = Some(AcquisitionDate(ukResidents.minusDays(1L))),
                  rebasedAcquisitionPrice = None
                ),
                AssetType.Residential,
                true,
                UserType.Individual
              )._1
            )
          }

          checkIsRedirect(
            performAction(),
            routes.AcquisitionDetailsController.rebasedAcquisitionPrice()
          )
        }

      }

      "show a form error" when {

        def test(userType: UserType, data: (String, String)*)(expectedErrorKey: String) =
          testFormError(userType, data: _*)(expectedErrorKey)("improvementCosts.title")(performAction)

        "no option has been selected" in {
          test(Individual)("improvementCosts.error.required")
        }

        "the option selected is not valid" in {
          test(Individual, "improvementCosts" -> "2")("improvementCosts.error.invalid")
        }

        "the amount of money is invalid" in {
          amountOfMoneyErrorScenarios("improvementCostsValue").foreach { scenario =>
            withClue(s"For $scenario: ") {
              val data =
                ("improvementCosts" -> "0") :: scenario.formData
              test(Individual, data: _*)(scenario.expectedErrorMessageKey)
            }
          }
        }

        "the amount of money is zero" in {
          test(Individual, "improvementCosts" -> "0", "improvementCostsValue" -> "0")(
            "improvementCostsValue.error.tooSmall"
          )
        }

      }

      "show an error page" when {
        val price = 1.23d
        val answers = IncompleteAcquisitionDetailsAnswers.empty.copy(
          acquisitionDate         = Some(acquisitionDate),
          acquisitionPrice        = Some(sample[AmountInPence]),
          rebasedAcquisitionPrice = Some(sample[AmountInPence])
        )
        val (session, journey, draftReturn) =
          sessionWithState(answers, AssetType.Residential, true, UserType.Individual)
        val updatedDraftReturn = commonUpdateDraftReturn(
          draftReturn,
          answers.copy(improvementCosts = Some(AmountInPence(123L)))
        )
        val updatedSession = session.copy(journeyStatus = Some(journey.copy(draftReturn = updatedDraftReturn)))

        "there is an error updating the draft return" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(
              updatedDraftReturn,
              journey.subscribedDetails.cgtReference,
              journey.agentReferenceNumber
            )(Left(Error("")))
          }

          checkIsTechnicalErrorPage(
            performAction(
              "improvementCosts"      -> "0",
              "improvementCostsValue" -> price.toString
            )
          )
        }

        "there is an error updating the session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(
              updatedDraftReturn,
              journey.subscribedDetails.cgtReference,
              journey.agentReferenceNumber
            )(Right(()))
            mockStoreSession(updatedSession)(Left(Error("")))
          }

          checkIsTechnicalErrorPage(
            performAction(
              "improvementCosts"      -> "0",
              "improvementCostsValue" -> price.toString
            )
          )
        }

      }

      "redirect to the check you answers page" when {
        val scenarios = List(
          List("improvementCosts" -> "0", "improvementCostsValue" -> "£1,234") -> AmountInPence(123400L),
          List("improvementCosts" -> "1") -> AmountInPence.zero
        )

        "the price submitted is valid and the journey was incomplete" in {
          scenarios.foreach {
            case (formData, expectedAmountInPence) =>
              withClue(s"For form data $formData and expected amount in pence $expectedAmountInPence: ") {
                val answers = IncompleteAcquisitionDetailsAnswers.empty.copy(
                  acquisitionDate         = Some(acquisitionDate),
                  acquisitionPrice        = Some(sample[AmountInPence]),
                  rebasedAcquisitionPrice = Some(sample[AmountInPence])
                )
                val (session, journey, draftReturn) =
                  sessionWithState(answers, AssetType.Residential, true, UserType.Individual)
                val updatedDraftReturn = commonUpdateDraftReturn(
                  draftReturn,
                  answers.copy(improvementCosts = Some(expectedAmountInPence))
                )
                val updatedSession = session.copy(journeyStatus = Some(journey.copy(draftReturn = updatedDraftReturn)))

                inSequence {
                  mockAuthWithNoRetrievals()
                  mockGetSession(session)
                  mockStoreDraftReturn(
                    updatedDraftReturn,
                    journey.subscribedDetails.cgtReference,
                    journey.agentReferenceNumber
                  )(Right(()))
                  mockStoreSession(updatedSession)(Right(()))
                }

                checkIsRedirect(
                  performAction(formData: _*),
                  routes.AcquisitionDetailsController.checkYourAnswers()
                )
              }
          }
        }

        "the price submitted is valid and the journey was complete" in {
          scenarios.foreach {
            case (formData, expectedAmountInPence) =>
              withClue(s"For form data $formData and expected amount in pence $expectedAmountInPence: ") {

                val answers = sample[CompleteAcquisitionDetailsAnswers]
                  .copy(
                    acquisitionDate         = acquisitionDate,
                    rebasedAcquisitionPrice = Some(sample[AmountInPence]),
                    improvementCosts        = AmountInPence(expectedAmountInPence.value + 1L)
                  )
                val (session, journey, draftReturn) =
                  sessionWithState(answers, AssetType.Residential, true, UserType.Individual)
                val updatedDraftReturn = commonUpdateDraftReturn(
                  draftReturn,
                  answers.copy(improvementCosts = expectedAmountInPence)
                )
                val updatedSession = session.copy(journeyStatus = Some(journey.copy(draftReturn = updatedDraftReturn)))

                inSequence {
                  mockAuthWithNoRetrievals()
                  mockGetSession(session)
                  mockStoreDraftReturn(
                    updatedDraftReturn,
                    journey.subscribedDetails.cgtReference,
                    journey.agentReferenceNumber
                  )(Right(()))
                  mockStoreSession(updatedSession)(Right(()))
                }

                checkIsRedirect(
                  performAction(formData: _*),
                  routes.AcquisitionDetailsController.checkYourAnswers()
                )
              }
          }
        }
      }

    }

    "handling requests to display the acquisition fees page" must {

      def performAction(): Future[Result] = controller.acquisitionFees()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      "redirect to the improvement costs page" when {

        "that question hasn't been answered" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[IncompleteAcquisitionDetailsAnswers].copy(
                  improvementCosts = None
                ),
                sample[AssetType],
                sample[Boolean],
                UserType.Individual
              )._1
            )
          }

          checkIsRedirect(
            performAction(),
            routes.AcquisitionDetailsController.improvementCosts()
          )
        }

      }

      "display the page" when {

        "the acquisition details section has not yet been completed without rebasing" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[IncompleteAcquisitionDetailsAnswers].copy(
                  acquisitionDate  = Some(AcquisitionDate(nonUkResidentsNonResidentialProperty.plusDays(2L))),
                  shouldUseRebase  = Some(false),
                  improvementCosts = Some(sample[AmountInPence])
                ),
                AssetType.NonResidential,
                false,
                UserType.Individual
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("acquisitionFees.title"), { doc =>
              doc.select("#back").attr("href") shouldBe routes.AcquisitionDetailsController.improvementCosts().url
              doc.select("#content > article > form").attr("action") shouldBe routes.AcquisitionDetailsController
                .acquisitionFeesSubmit()
                .url
            }
          )
        }

        "the acquisition details section has not yet been completed with rebasing" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[IncompleteAcquisitionDetailsAnswers].copy(
                  acquisitionDate  = Some(AcquisitionDate(ukResidents.minusDays(2L))),
                  shouldUseRebase  = Some(true),
                  improvementCosts = Some(sample[AmountInPence])
                ),
                AssetType.Residential,
                true,
                UserType.Individual
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("acquisitionFees.rebased.title"), { doc =>
              doc.select("#back").attr("href") shouldBe routes.AcquisitionDetailsController.improvementCosts().url
              doc.select("#content > article > form").attr("action") shouldBe routes.AcquisitionDetailsController
                .acquisitionFeesSubmit()
                .url
            }
          )
        }

        "the acquisition details section has been completed without rebasing" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[CompleteAcquisitionDetailsAnswers].copy(
                  acquisitionDate = AcquisitionDate(nonUkResidentsResidentialProperty.plusDays(1L)),
                  shouldUseRebase = false
                ),
                AssetType.Residential,
                sample[Boolean],
                UserType.Individual
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("acquisitionFees.title"), { doc =>
              doc.select("#back").attr("href") shouldBe routes.AcquisitionDetailsController.checkYourAnswers().url
              doc.select("#content > article > form").attr("action") shouldBe routes.AcquisitionDetailsController
                .acquisitionFeesSubmit()
                .url
            }
          )
        }

        "the acquisition details section has been completed with rebasing" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[CompleteAcquisitionDetailsAnswers].copy(
                  acquisitionDate = AcquisitionDate(ukResidents.minusDays(1L)),
                  shouldUseRebase = true
                ),
                AssetType.Residential,
                true,
                UserType.Individual
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("acquisitionFees.rebased.title"), { doc =>
              doc.select("#back").attr("href") shouldBe routes.AcquisitionDetailsController.checkYourAnswers().url
              doc.select("#content > article > form").attr("action") shouldBe routes.AcquisitionDetailsController
                .acquisitionFeesSubmit()
                .url
            }
          )
        }

        "the amount in the session is zero" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[CompleteAcquisitionDetailsAnswers].copy(
                  acquisitionDate = AcquisitionDate(nonUkResidentsResidentialProperty.plusDays(2L)),
                  acquisitionFees = AmountInPence.zero,
                  shouldUseRebase = false
                ),
                sample[AssetType],
                sample[Boolean],
                UserType.Individual
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(
              "acquisitionFees.title"
            ), { doc =>
              doc.select("#acquisitionFees-1").attr("checked") shouldBe "checked"
            }
          )
        }

        "the amount in the session is non-zero" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[CompleteAcquisitionDetailsAnswers].copy(
                  acquisitionDate = AcquisitionDate(nonUkResidentsResidentialProperty.plusDays(2L)),
                  acquisitionFees = AmountInPence(3L),
                  shouldUseRebase = false
                ),
                sample[AssetType],
                sample[Boolean],
                UserType.Individual
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(
              "acquisitionFees.title"
            ), { doc =>
              doc.select("#acquisitionFees-0").attr("checked")  shouldBe "checked"
              doc.select("#acquisitionFeesValue").attr("value") shouldBe "0.03"
            }
          )
        }
      }

    }

    "handling submitted acquisition fees" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.acquisitionFeesSubmit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      behave like redirectToStartBehaviour(() => performAction())

      "redirect to the improvement costs page" when {

        "the question has not been answered" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[IncompleteAcquisitionDetailsAnswers].copy(
                  improvementCosts = None
                ),
                sample[AssetType],
                sample[Boolean],
                UserType.Individual
              )._1
            )
          }

          checkIsRedirect(
            performAction(),
            routes.AcquisitionDetailsController.improvementCosts()
          )

        }

      }

      "show a form error" when {

        def test(userType: UserType, data: (String, String)*)(expectedErrorKey: String) =
          testFormError(userType, data: _*)(expectedErrorKey)("acquisitionFees.title")(performAction)

        "no option has been selected" in {
          test(Individual)("acquisitionFees.error.required")
        }

        "the option selected is not valid" in {
          test(Individual, "acquisitionFees" -> "2")("acquisitionFees.error.invalid")
        }

        "the amount of money is invalid" in {
          amountOfMoneyErrorScenarios("acquisitionFeesValue").foreach { scenario =>
            withClue(s"For $scenario: ") {
              val data =
                ("acquisitionFees" -> "0") :: scenario.formData
              test(Individual, data: _*)(scenario.expectedErrorMessageKey)
            }
          }
        }

        "the amount of money is zero" in {
          test(Individual, "acquisitionFees" -> "0", "acquisitionFeesValue" -> "0")(
            "acquisitionFeesValue.error.tooSmall"
          )
        }

      }

      "show an error page" when {
        val price = 1.23d
        val answers = IncompleteAcquisitionDetailsAnswers.empty.copy(
          improvementCosts = Some(sample[AmountInPence])
        )
        val (session, journey, draftReturn) =
          sessionWithState(answers, sample[AssetType], sample[Boolean], UserType.Individual)
        val updatedDraftReturn = commonUpdateDraftReturn(
          draftReturn,
          answers.copy(acquisitionFees = Some(AmountInPence(123L)))
        )
        val updatedSession = session.copy(journeyStatus = Some(journey.copy(draftReturn = updatedDraftReturn)))

        "there is an error updating the draft return" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(
              updatedDraftReturn,
              journey.subscribedDetails.cgtReference,
              journey.agentReferenceNumber
            )(Left(Error("")))
          }

          checkIsTechnicalErrorPage(
            performAction(
              "acquisitionFees"      -> "0",
              "acquisitionFeesValue" -> price.toString
            )
          )
        }

        "there is an error updating the session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(
              updatedDraftReturn,
              journey.subscribedDetails.cgtReference,
              journey.agentReferenceNumber
            )(Right(()))
            mockStoreSession(updatedSession)(Left(Error("")))
          }

          checkIsTechnicalErrorPage(
            performAction(
              "acquisitionFees"      -> "0",
              "acquisitionFeesValue" -> price.toString
            )
          )
        }

      }

      "redirect to the check you answers page" when {

        val scenarios = List(
          List("acquisitionFees" -> "0", "acquisitionFeesValue" -> "£1,234") -> AmountInPence(123400L),
          List("acquisitionFees" -> "1") -> AmountInPence.zero
        )

        "the price submitted is valid and the journey was incomplete" in {
          scenarios.foreach {
            case (formData, expectedAmountInPence) =>
              withClue(s"For form data $formData and expected amount in pence $expectedAmountInPence: ") {
                val answers =
                  IncompleteAcquisitionDetailsAnswers.empty.copy(improvementCosts = Some(sample[AmountInPence]))
                val (session, journey, draftReturn) =
                  sessionWithState(answers, sample[AssetType], sample[Boolean], UserType.Individual)
                val updatedDraftReturn = commonUpdateDraftReturn(
                  draftReturn,
                  answers.copy(acquisitionFees = Some(expectedAmountInPence))
                )
                val updatedSession = session.copy(journeyStatus = Some(journey.copy(draftReturn = updatedDraftReturn)))

                inSequence {
                  mockAuthWithNoRetrievals()
                  mockGetSession(session)
                  mockStoreDraftReturn(
                    updatedDraftReturn,
                    journey.subscribedDetails.cgtReference,
                    journey.agentReferenceNumber
                  )(Right(()))
                  mockStoreSession(updatedSession)(Right(()))
                }

                checkIsRedirect(
                  performAction(formData: _*),
                  routes.AcquisitionDetailsController.checkYourAnswers()
                )
              }
          }
        }

        "the price submitted is valid and the journey was complete" in {
          scenarios.foreach {
            case (formData, expectedAmountInPence) =>
              withClue(s"For form data $formData and expected amount in pence $expectedAmountInPence: ") {
                val answers =
                  sample[CompleteAcquisitionDetailsAnswers]
                    .copy(acquisitionFees = AmountInPence(expectedAmountInPence.value + 1L))
                val (session, journey, draftReturn) =
                  sessionWithState(answers, sample[AssetType], sample[Boolean], UserType.Individual)
                val updatedDraftReturn = commonUpdateDraftReturn(
                  draftReturn,
                  answers.copy(acquisitionFees = expectedAmountInPence)
                )
                val updatedSession = session.copy(journeyStatus = Some(journey.copy(draftReturn = updatedDraftReturn)))

                inSequence {
                  mockAuthWithNoRetrievals()
                  mockGetSession(session)
                  mockStoreDraftReturn(
                    updatedDraftReturn,
                    journey.subscribedDetails.cgtReference,
                    journey.agentReferenceNumber
                  )(Right(()))
                  mockStoreSession(updatedSession)(Right(()))
                }

                checkIsRedirect(
                  performAction(formData: _*),
                  routes.AcquisitionDetailsController.checkYourAnswers()
                )
              }
          }

        }
      }

    }

    "handling requests to display the should use rebase page" must {

      def performAction(): Future[Result] = controller.shouldUseRebase()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      "display the page" when {

        "the user is non uk, residential and acquisition date before rebasing date" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[IncompleteAcquisitionDetailsAnswers].copy(
                  improvementCosts = Some(sample[AmountInPence])
                ),
                AssetType.Residential,
                false,
                UserType.Individual
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(
              "shouldUseRebase.title",
              LocalDateUtils.govDisplayFormat(nonUkResidentsResidentialProperty.minusDays(1))
            )
          )

        }

        "the user is non uk and residential asset type" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[IncompleteAcquisitionDetailsAnswers].copy(
                  improvementCosts = Some(sample[AmountInPence])
                ),
                AssetType.NonResidential,
                false,
                UserType.Individual
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(
              "shouldUseRebase.title",
              LocalDateUtils.govDisplayFormat(nonUkResidentsNonResidentialProperty.minusDays(1))
            )
          )

        }

      }

      "redirect to check your answers" when {

        "the user is uk" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[IncompleteAcquisitionDetailsAnswers].copy(
                  improvementCosts = Some(sample[AmountInPence])
                ),
                AssetType.Residential,
                true,
                UserType.Individual
              )._1
            )
          }

          checkIsRedirect(
            performAction(),
            routes.AcquisitionDetailsController.checkYourAnswers()
          )

        }

      }

    }

    "handling requests to submit to should use rebased" must {

      val disposalDate = DisposalDate(LocalDate.of(1200, 1, 1), sample[TaxYear])
      def performAction(data: (String, String)*): Future[Result] =
        controller.shouldUseRebaseSubmit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      "show a form error for non residential non uk" when {
        val date: String = LocalDateUtils.govDisplayFormat(nonUkResidentsNonResidentialProperty.minusDays(1))

        def test(userType: UserType, data: (String, String)*)(expectedErrorKey: String) =
          testFormError(userType, data: _*)(expectedErrorKey)("shouldUseRebase.title", date)(
            performAction,
            sessionWithState(
              sample[CompleteAcquisitionDetailsAnswers]
                .copy(acquisitionDate = AcquisitionDate(nonUkResidentsNonResidentialProperty.minusDays(1))),
              AssetType.NonResidential,
              false,
              UserType.Individual,
              disposalDate
            )._1
          )

        "no option has been selected" in {
          test(Individual)("shouldUseRebase.error.required")
        }
      }

      "show a form error for residential non uk" when {
        val date: String = LocalDateUtils.govDisplayFormat(nonUkResidentsResidentialProperty.minusDays(1))

        def test(userType: UserType, data: (String, String)*)(expectedErrorKey: String) =
          testFormError(userType, data: _*)(expectedErrorKey)("shouldUseRebase.title", date)(
            performAction,
            sessionWithState(
              sample[CompleteAcquisitionDetailsAnswers]
                .copy(acquisitionDate = AcquisitionDate(nonUkResidentsResidentialProperty.minusDays(1))),
              AssetType.Residential,
              false,
              UserType.Individual,
              disposalDate
            )._1
          )

        "no option has been selected" in {
          test(Individual)("shouldUseRebase.error.required")
        }
      }
    }

    "handling requests to display the check your answers page" must {

      def performAction(): Future[Result] = controller.checkYourAnswers()(FakeRequest())

      val completeAnswers = CompleteAcquisitionDetailsAnswers(
        sample[AcquisitionMethod],
        sample[AcquisitionDate],
        sample[AmountInPence],
        Some(sample[AmountInPence]),
        sample[AmountInPence],
        sample[AmountInPence],
        sample[Boolean]
      )

      val allQuestionsAnswered = IncompleteAcquisitionDetailsAnswers(
        Some(completeAnswers.acquisitionMethod),
        Some(completeAnswers.acquisitionDate),
        Some(completeAnswers.acquisitionPrice),
        completeAnswers.rebasedAcquisitionPrice,
        Some(completeAnswers.improvementCosts),
        Some(completeAnswers.acquisitionFees),
        Some(completeAnswers.shouldUseRebase)
      )

      behave like redirectToStartBehaviour(performAction)

      behave like missingAssetTypeAndResidentialStatusBehaviour(performAction)

      def testRedirectOnMissingData(
        session: SessionData,
        expectedRedirect: Call
      ): Unit = {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkIsRedirect(performAction(), expectedRedirect)
      }

      "redirect to the acquisition method page" when {

        "the question has not been answered" in {
          testRedirectOnMissingData(
            sessionWithState(
              allQuestionsAnswered.copy(acquisitionMethod = None),
              sample[AssetType],
              sample[Boolean],
              UserType.Individual
            )._1,
            routes.AcquisitionDetailsController.acquisitionMethod()
          )
        }

      }

      "redirect to the acquisition date page" when {

        "the question has not been answered" in {
          testRedirectOnMissingData(
            sessionWithState(
              allQuestionsAnswered.copy(acquisitionDate = None),
              sample[AssetType],
              sample[Boolean],
              UserType.Individual
            )._1,
            routes.AcquisitionDetailsController.acquisitionDate()
          )
        }

      }

      "redirect to the acquisition price page" when {

        "the question has not been answered, and eligible for acquisition price" in {
          testRedirectOnMissingData(
            sessionWithState(
              allQuestionsAnswered
                .copy(acquisitionPrice = None, acquisitionDate = Some(AcquisitionDate(LocalDate.now()))),
              sample[AssetType],
              sample[Boolean],
              UserType.Individual
            )._1,
            routes.AcquisitionDetailsController.acquisitionPrice()
          )
        }

      }

      "redirect to the rebased acquisition price page" when {

        "the question has not been answered and the user meets the rebasing criteria" when {

          "the user was a uk resident and is disposing a residential property" in {
            testRedirectOnMissingData(
              sessionWithState(
                allQuestionsAnswered.copy(
                  acquisitionDate         = Some(AcquisitionDate(ukResidents.minusDays(1L))),
                  rebasedAcquisitionPrice = None
                ),
                AssetType.Residential,
                true,
                UserType.Individual
              )._1,
              routes.AcquisitionDetailsController.rebasedAcquisitionPrice()
            )
          }

          "the user was a non-uk resident and is disposing a residential property" in {
            testRedirectOnMissingData(
              sessionWithState(
                allQuestionsAnswered.copy(
                  acquisitionDate =
                    Some(AcquisitionDate(RebasingCutoffDates.nonUkResidentsResidentialProperty.minusDays(1L))),
                  rebasedAcquisitionPrice = None
                ),
                AssetType.Residential,
                false,
                UserType.Individual
              )._1,
              routes.AcquisitionDetailsController.rebasedAcquisitionPrice()
            )
          }

          "the user was a non-uk resident and is disposing a non-residential property" in {
            testRedirectOnMissingData(
              sessionWithState(
                allQuestionsAnswered.copy(
                  acquisitionDate =
                    Some(AcquisitionDate(RebasingCutoffDates.nonUkResidentsNonResidentialProperty.minusDays(1L))),
                  rebasedAcquisitionPrice = None
                ),
                AssetType.NonResidential,
                false,
                UserType.Individual
              )._1,
              routes.AcquisitionDetailsController.rebasedAcquisitionPrice()
            )
          }

        }

      }

      "redirect to the improvement costs page" when {

        "the question has not been answered" in {
          testRedirectOnMissingData(
            sessionWithState(
              allQuestionsAnswered.copy(improvementCosts = None),
              sample[AssetType],
              sample[Boolean],
              UserType.Individual
            )._1,
            routes.AcquisitionDetailsController.improvementCosts()
          )
        }

      }

      "redirect to the acquisition fees page" when {

        "the question has not been answered" in {
          testRedirectOnMissingData(
            sessionWithState(
              allQuestionsAnswered.copy(acquisitionFees = None),
              sample[AssetType],
              sample[Boolean],
              UserType.Individual
            )._1,
            routes.AcquisitionDetailsController.acquisitionFees()
          )
        }

      }

      "show an error page when the user has just answered all of the questions and" when {
        val (session, journey, draftReturn) =
          sessionWithState(allQuestionsAnswered, sample[AssetType], sample[Boolean], UserType.Individual)
        val newDraftReturn = draftReturn.copy(acquisitionDetailsAnswers = Some(completeAnswers))
        val updatedJourney = journey.copy(draftReturn                   = newDraftReturn)
        "there is an error updating the draft return" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(newDraftReturn, journey.subscribedDetails.cgtReference, journey.agentReferenceNumber)(
              Left(Error(""))
            )
          }

          checkIsTechnicalErrorPage(performAction())

        }

        "there is an error updating the session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(newDraftReturn, journey.subscribedDetails.cgtReference, journey.agentReferenceNumber)(
              Right(())
            )
            mockStoreSession(session.copy(journeyStatus = Some(updatedJourney)))(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction())

        }
      }

      "show the page" when {

        "the user has just answered all the questions and all updates are successful" in {
          val (session, journey, draftReturn) =
            sessionWithState(allQuestionsAnswered, sample[AssetType], sample[Boolean], UserType.Individual)
          val newDraftReturn = draftReturn.copy(acquisitionDetailsAnswers = Some(completeAnswers))
          val updatedJourney = journey.copy(draftReturn                   = newDraftReturn)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(newDraftReturn, journey.subscribedDetails.cgtReference, journey.agentReferenceNumber)(
              Right(())
            )
            mockStoreSession(session.copy(journeyStatus = Some(updatedJourney)))(Right(()))
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("acquisitionDetails.cya.title"), { doc =>
              doc.select("#content > article > form").attr("action") shouldBe routes.AcquisitionDetailsController
                .checkYourAnswersSubmit()
                .url
            }
          )
        }

        "the user has already answered all the questions and is non-uk and rebasing" in {
          val nonUkRebasing = CompleteAcquisitionDetailsAnswers(
            sample[AcquisitionMethod],
            AcquisitionDate(nonUkResidentsNonResidentialProperty.minusDays(1)),
            sample[AmountInPence],
            Some(sample[AmountInPence]),
            sample[AmountInPence],
            sample[AmountInPence],
            false
          )
          val assetType = AssetType.NonResidential
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionWithState(nonUkRebasing, assetType, false, UserType.Individual)._1)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("acquisitionDetails.cya.title"), { doc =>
              validateAcquisitionDetailsCheckYourAnswersPage(
                nonUkRebasing,
                doc,
                false,
                true
              )
              doc.select("#content > article > form").attr("action") shouldBe routes.AcquisitionDetailsController
                .checkYourAnswersSubmit()
                .url
            }
          )
        }

        "the user has already answered all the questions and is uk and rebasing" in {
          val nonUkRebasing = CompleteAcquisitionDetailsAnswers(
            sample[AcquisitionMethod],
            AcquisitionDate(ukResidents.minusDays(2)),
            sample[AmountInPence],
            Some(sample[AmountInPence]),
            sample[AmountInPence],
            sample[AmountInPence],
            true
          )
          val assetType = AssetType.NonResidential
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionWithState(nonUkRebasing, assetType, true, UserType.Individual)._1)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("acquisitionDetails.cya.title"), { doc =>
              validateAcquisitionDetailsCheckYourAnswersPage(
                nonUkRebasing,
                doc,
                true,
                true
              )
              doc.select("#content > article > form").attr("action") shouldBe routes.AcquisitionDetailsController
                .checkYourAnswersSubmit()
                .url
            }
          )
        }

        "the user has already answered all the questions and is uk and not rebasing" in {
          val nonUkRebasing = CompleteAcquisitionDetailsAnswers(
            sample[AcquisitionMethod],
            AcquisitionDate(ukResidents.plusDays(1)),
            sample[AmountInPence],
            Some(sample[AmountInPence]),
            sample[AmountInPence],
            sample[AmountInPence],
            true
          )
          val assetType = AssetType.NonResidential
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionWithState(nonUkRebasing, assetType, true, UserType.Individual)._1)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("acquisitionDetails.cya.title"), { doc =>
              validateAcquisitionDetailsCheckYourAnswersPage(
                nonUkRebasing,
                doc,
                true,
                false
              )
              doc.select("#content > article > form").attr("action") shouldBe routes.AcquisitionDetailsController
                .checkYourAnswersSubmit()
                .url
            }
          )
        }

        "the user has already answered all the questions and is not uk and not rebasing" in {
          val nonUkRebasing = CompleteAcquisitionDetailsAnswers(
            sample[AcquisitionMethod],
            AcquisitionDate(nonUkResidentsNonResidentialProperty.plusDays(1)),
            sample[AmountInPence],
            Some(sample[AmountInPence]),
            sample[AmountInPence],
            sample[AmountInPence],
            false
          )
          val assetType = AssetType.NonResidential
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionWithState(nonUkRebasing, assetType, false, UserType.Individual)._1)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("acquisitionDetails.cya.title"), { doc =>
              validateAcquisitionDetailsCheckYourAnswersPage(
                nonUkRebasing,
                doc,
                false,
                false
              )
              doc.select("#content > article > form").attr("action") shouldBe routes.AcquisitionDetailsController
                .checkYourAnswersSubmit()
                .url
            }
          )
        }

        "display correct questions on check your answers" when {

          "The question for the acquisition on the cya is correct for all acquisition methods" in {
            val date = LocalDate.now()
            List(
              (AcquisitionMethod.Bought, messages("acquisitionPriceBought.title")),
              (
                AcquisitionMethod.Inherited,
                messages("acquisitionPriceNotBought.title", LocalDateUtils.govDisplayFormat(date))
              ),
              (
                AcquisitionMethod.Gifted,
                messages("acquisitionPriceNotBought.title", LocalDateUtils.govDisplayFormat(date))
              ),
              (
                AcquisitionMethod.Other("test"),
                messages("acquisitionPriceNotBought.title", LocalDateUtils.govDisplayFormat(date))
              )
            ).foreach {
              case (method, expectedTitle) =>
                withClue(s"For $method and $expectedTitle") {
                  val nonUkRebasing = CompleteAcquisitionDetailsAnswers(
                    method,
                    AcquisitionDate(date),
                    sample[AmountInPence],
                    Some(sample[AmountInPence]),
                    sample[AmountInPence],
                    sample[AmountInPence],
                    false
                  )
                  val assetType = AssetType.NonResidential
                  inSequence {
                    mockAuthWithNoRetrievals()
                    mockGetSession(sessionWithState(nonUkRebasing, assetType, false, UserType.Individual)._1)
                  }

                  checkPageIsDisplayed(
                    performAction(),
                    messageFromMessageKey("acquisitionDetails.cya.title"), { doc =>
                      doc.select("#acquisitionPrice-question").text() shouldBe expectedTitle
                    }
                  )
                }
            }
          }
        }
      }

      "handling submits on the check you answers page" must {

        def performAction(): Future[Result] = controller.checkYourAnswersSubmit()(FakeRequest())

        "redirect to the task list" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[CompleteAcquisitionDetailsAnswers],
                sample[AssetType],
                sample[Boolean],
                UserType.Individual
              )._1
            )
          }

          checkIsRedirect(performAction(), controllers.returns.routes.TaskListController.taskList())
        }
      }

    }

    def missingAssetTypeAndResidentialStatusBehaviour(performAction: () => Future[Result]) =
      "redirect to the task list page" when {

        "there is no asset type" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                Some(sample[CompleteAcquisitionDetailsAnswers]),
                None,
                Some(sample[Boolean]),
                UserType.Individual,
                Some(sample[DisposalDate])
              )._1
            )
          }

          checkIsRedirect(performAction(), controllers.returns.routes.TaskListController.taskList())
        }

        "there is no residential status" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                Some(sample[CompleteAcquisitionDetailsAnswers]),
                Some(sample[AssetType]),
                None,
                UserType.Individual,
                Some(sample[DisposalDate])
              )._1
            )
          }

          checkIsRedirect(performAction(), controllers.returns.routes.TaskListController.taskList())
        }

      }

    def missingAcquisitionDateBehaviour(performAction: () => Future[Result]) =
      "redirect to the check you answers page" when {

        "there is no acquisition date" in {
          val completeAcquisitionDetailsAnswers = sample[CompleteAcquisitionDetailsAnswers]

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                Some(
                  IncompleteAcquisitionDetailsAnswers(
                    Some(completeAcquisitionDetailsAnswers.acquisitionMethod),
                    None,
                    Some(completeAcquisitionDetailsAnswers.acquisitionPrice),
                    completeAcquisitionDetailsAnswers.rebasedAcquisitionPrice,
                    Some(completeAcquisitionDetailsAnswers.improvementCosts),
                    Some(completeAcquisitionDetailsAnswers.acquisitionFees),
                    Some(completeAcquisitionDetailsAnswers.shouldUseRebase)
                  )
                ),
                Some(sample[AssetType]),
                Some(sample[Boolean]),
                UserType.Individual,
                Some(sample[DisposalDate])
              )._1
            )
          }

          checkIsRedirect(performAction(), routes.AcquisitionDetailsController.checkYourAnswers())
        }

      }

    def missingAcquisitionMethodBehaviour(performAction: () => Future[Result]) =
      "redirect to the check you answers page" when {

        "there is no acquisition method" in {
          val completeAcquisitionDetailsAnswers = sample[CompleteAcquisitionDetailsAnswers]

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                Some(
                  IncompleteAcquisitionDetailsAnswers(
                    None,
                    Some(completeAcquisitionDetailsAnswers.acquisitionDate),
                    Some(completeAcquisitionDetailsAnswers.acquisitionPrice),
                    completeAcquisitionDetailsAnswers.rebasedAcquisitionPrice,
                    Some(completeAcquisitionDetailsAnswers.improvementCosts),
                    Some(completeAcquisitionDetailsAnswers.acquisitionFees),
                    Some(completeAcquisitionDetailsAnswers.shouldUseRebase)
                  )
                ),
                Some(sample[AssetType]),
                Some(sample[Boolean]),
                UserType.Individual,
                Some(sample[DisposalDate])
              )._1
            )
          }

          checkIsRedirect(performAction(), routes.AcquisitionDetailsController.checkYourAnswers())
        }
      }
  }

  def testFormError(
    userType: UserType,
    data: (String, String)*
  )(expectedErrorMessageKey: String, errorArgs: String*)(pageTitleKey: String, titleArgs: String*)(
    performAction: Seq[(String, String)] => Future[Result],
    currentSession: SessionData = sessionWithState(
      sample[CompleteAcquisitionDetailsAnswers].copy(
        rebasedAcquisitionPrice = Some(sample[AmountInPence]),
        shouldUseRebase         = false
      ),
      sample[AssetType],
      sample[Boolean],
      userType
    )._1
  ): Unit = {
    inSequence {
      mockAuthWithNoRetrievals()
      mockGetSession(currentSession)
    }

    checkPageIsDisplayed(
      performAction(data),
      messageFromMessageKey(pageTitleKey, titleArgs: _*), { doc =>
        println("Actual: " + doc.select("#error-summary-display > ul > li > a").text())
        println("Expected: " + messageFromMessageKey(expectedErrorMessageKey))
        doc.select("#error-summary-display > ul > li > a").text() shouldBe messageFromMessageKey(
          expectedErrorMessageKey,
          errorArgs: _*
        )
        doc.title() should startWith("Error:")
      },
      BAD_REQUEST
    )
  }
}

object AcquisitionDetailsControllerSpec extends Matchers {

  def validateAcquisitionDetailsCheckYourAnswersPage(
    acquisitionDetailsAnswers: CompleteAcquisitionDetailsAnswers,
    doc: Document,
    isUk: Boolean,
    isRebasing: Boolean
  )(implicit messages: MessagesApi, lang: Lang): Unit = {
    val expectedAcquisitionMethodDisplayName = acquisitionDetailsAnswers.acquisitionMethod match {
      case AcquisitionMethod.Bought       => messages("returns.acquisitionMethod.Bought")
      case AcquisitionMethod.Inherited    => messages("returns.acquisitionMethod.Inherited")
      case AcquisitionMethod.Gifted       => messages("returns.acquisitionMethod.Gifted")
      case AcquisitionMethod.Other(value) => value
    }

    doc.select("#acquisitionMethod-answer").text() shouldBe expectedAcquisitionMethodDisplayName

    if (!isRebasing || !isUk) {
      doc.select("#acquisitionPrice-answer").text() shouldBe formatAmountOfMoneyWithPoundSign(
        acquisitionDetailsAnswers.acquisitionPrice.inPounds()
      )
    } else {
      doc.select("#acquisitionPrice-answer").text() shouldBe ""
    }

    if (acquisitionDetailsAnswers.improvementCosts === AmountInPence.zero) {
      doc.select("#improvementCosts-answer").text shouldBe "No"
    } else {
      doc.select("#improvementCosts-answer").text shouldBe "Yes"
      doc.select("#improvementCosts-value-answer").text shouldBe formatAmountOfMoneyWithPoundSign(
        acquisitionDetailsAnswers.improvementCosts.inPounds()
      )
    }

    if (acquisitionDetailsAnswers.acquisitionFees === AmountInPence.zero) {
      doc.select("#acquisitionFees-answer").text shouldBe "No"
    } else {
      doc.select("#acquisitionFees-answer").text shouldBe "Yes"
      doc.select("#acquisitionFees-value-answer").text shouldBe formatAmountOfMoneyWithPoundSign(
        acquisitionDetailsAnswers.acquisitionFees.inPounds()
      )
    }

    acquisitionDetailsAnswers.rebasedAcquisitionPrice.foreach { rebasedAcquisitionPrice =>
      if (!isUk && isRebasing) {
        if (acquisitionDetailsAnswers.shouldUseRebase) {
          doc.select("#shouldRebase-answer").text shouldBe "Yes"
          doc.select("#rebasedAcquisitionPrice-value-answer").text shouldBe formatAmountOfMoneyWithPoundSign(
            rebasedAcquisitionPrice.inPounds()
          )
        } else {
          doc.select("#shouldRebase-answer").text shouldBe "No"
        }
      }
    }
  }
}
