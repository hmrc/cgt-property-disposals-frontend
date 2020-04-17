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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.representee

import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.Configuration
import play.api.i18n.MessagesApi
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.{Action, AnyContent, Call, Result}
import play.api.test.FakeRequest
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, ControllerSpec, NameFormValidationTests, SessionSupport, returns}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.ReturnsServiceSupport
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, StartingNewDraftReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.IndividualName
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, SessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.IndividualUserType._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.MultipleDisposalsTriageAnswers.CompleteMultipleDisposalsTriageAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.RepresenteeAnswers.{CompleteRepresenteeAnswers, IncompleteRepresenteeAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.CompleteSingleDisposalTriageAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{DraftMultipleDisposalsReturn, IndividualUserType, RepresenteeAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService

import scala.concurrent.Future

class RepresenteeControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with ScalaCheckDrivenPropertyChecks
    with RedirectToStartBehaviour
    with ReturnsServiceSupport
    with NameFormValidationTests {

  override lazy val additionalConfig: Configuration = Configuration(
    "capacitors-and-personal-representatives.enabled" -> "true"
  )

  override val overrideBindings =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore),
      bind[ReturnsService].toInstance(mockReturnsService)
    )

  lazy val controller = instanceOf[RepresenteeController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  def sessionWithStartingNewDraftReturn(
    answers: Option[RepresenteeAnswers],
    individualUserType: Option[IndividualUserType]
  ): (SessionData, StartingNewDraftReturn) = {
    val journey = sample[StartingNewDraftReturn].copy(
      representeeAnswers = answers,
      newReturnTriageAnswers =
        Right(sample[CompleteSingleDisposalTriageAnswers].copy(individualUserType = individualUserType))
    )
    val session = SessionData.empty.copy(journeyStatus = Some(journey))
    session -> journey
  }

  def sessionWithStartingNewDraftReturn(
    answers: RepresenteeAnswers,
    representativeType: Either[PersonalRepresentative.type, Capacitor.type]
  ): (SessionData, StartingNewDraftReturn) =
    sessionWithStartingNewDraftReturn(Some(answers), Some(representativeType.merge))

  def sessionWithFillingOutReturn(
    answers: Option[RepresenteeAnswers],
    individualUserType: Option[IndividualUserType]
  ): (SessionData, FillingOutReturn, DraftMultipleDisposalsReturn) = {
    val draftReturn = sample[DraftMultipleDisposalsReturn].copy(
      representeeAnswers = answers,
      triageAnswers      = sample[CompleteMultipleDisposalsTriageAnswers].copy(individualUserType = individualUserType)
    )
    val journey = sample[FillingOutReturn].copy(draftReturn = draftReturn)

    val session = SessionData.empty.copy(journeyStatus = Some(journey))
    (session, journey, draftReturn)
  }

  def sessionWithFillingOutReturn(
    answers: RepresenteeAnswers,
    representativeType: Either[PersonalRepresentative.type, Capacitor.type]
  ): (SessionData, FillingOutReturn, DraftMultipleDisposalsReturn) =
    sessionWithFillingOutReturn(Some(answers), Some(representativeType.merge))

  "RepresenteeController" when {

    "handling requests to display the enter name page" must {

      def performAction(): Future[Result] = controller.enterName()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      behave like nonCapacitorOrPersonalRepBehaviour(performAction)

      "display the page" when {

        def test(
          sessionData: SessionData,
          expectedTitleKey: String,
          expectedBackLink: Call,
          expectReturnToSummaryLink: Boolean,
          expectedPrepopulatedValue: Option[IndividualName]
        ): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionData)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(expectedTitleKey), { doc =>
              doc.select("#back").attr("href") shouldBe expectedBackLink.url
              doc
                .select("#content > article > form")
                .attr("action") shouldBe routes.RepresenteeController.enterNameSubmit().url
              doc.select("#returnToSummaryLink").text() shouldBe (
                if (expectReturnToSummaryLink) messageFromMessageKey("returns.return-to-summary-link")
                else ""
              )
              expectedPrepopulatedValue.foreach { name =>
                doc.select("#representeeFirstName").attr("value") shouldBe name.firstName
                doc.select("#representeeLastName").attr("value")  shouldBe name.lastName
              }
            }
          )

        }

        "the user is starting a draft return and" when {

          "the user is a capacitor" in {
            test(
              sessionWithStartingNewDraftReturn(IncompleteRepresenteeAnswers.empty, Right(Capacitor))._1,
              "representee.enterName.capacitor.title",
              returns.triage.routes.CommonTriageQuestionsController.whoIsIndividualRepresenting(),
              expectReturnToSummaryLink = false,
              None
            )
          }

          "the user is a personal representative" in {
            val name = sample[IndividualName]
            test(
              sessionWithStartingNewDraftReturn(
                IncompleteRepresenteeAnswers.empty.copy(name = Some(name)),
                Left(PersonalRepresentative)
              )._1,
              "representee.enterName.personalRep.title",
              returns.triage.routes.CommonTriageQuestionsController.whoIsIndividualRepresenting(),
              expectReturnToSummaryLink = false,
              Some(name)
            )

          }

          "the section is complete" in {
            val answers = sample[CompleteRepresenteeAnswers]
            test(
              sessionWithStartingNewDraftReturn(answers, Left(PersonalRepresentative))._1,
              "representee.enterName.personalRep.title",
              routes.RepresenteeController.checkYourAnswers(),
              expectReturnToSummaryLink = false,
              Some(answers.name)
            )
          }

        }

        "the user has already started a draft return and" when {

          "the user is a capacitor" in {
            test(
              sessionWithFillingOutReturn(IncompleteRepresenteeAnswers.empty, Right(Capacitor))._1,
              "representee.enterName.capacitor.title",
              returns.triage.routes.CommonTriageQuestionsController.whoIsIndividualRepresenting(),
              expectReturnToSummaryLink = true,
              None
            )

          }

          "the user is a personal representative" in {
            val name = sample[IndividualName]
            test(
              sessionWithFillingOutReturn(
                IncompleteRepresenteeAnswers.empty.copy(name = Some(name)),
                Left(PersonalRepresentative)
              )._1,
              "representee.enterName.personalRep.title",
              returns.triage.routes.CommonTriageQuestionsController.whoIsIndividualRepresenting(),
              expectReturnToSummaryLink = true,
              Some(name)
            )
          }

          "the section is complete" in {
            val answers = sample[CompleteRepresenteeAnswers]
            test(
              sessionWithFillingOutReturn(answers, Left(PersonalRepresentative))._1,
              "representee.enterName.personalRep.title",
              routes.RepresenteeController.checkYourAnswers(),
              expectReturnToSummaryLink = true,
              Some(answers.name)
            )
          }

        }

      }

    }

    "handling submitted names" must {

      def performAction(formData: Seq[(String, String)]): Future[Result] =
        controller.enterNameSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*))

      val firstNameKey = "representeeFirstName"

      val lastNameKey = "representeeLastName"

      def formData(individualName: IndividualName): Seq[(String, String)] =
        Seq(firstNameKey -> individualName.firstName, lastNameKey -> individualName.lastName)

      behave like redirectToStartBehaviour(() => performAction(Seq.empty))

      behave like nonCapacitorOrPersonalRepBehaviour(() => performAction(Seq.empty))

      behave like nameFormValidationTests(
        performAction,
        () =>
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionWithFillingOutReturn(IncompleteRepresenteeAnswers.empty, Right(Capacitor))._1)
          },
        firstNameKey,
        lastNameKey
      )

      "show an error page" when {

        val newName = IndividualName("Max", "Cax")
        val (session, journey, draftReturn) =
          sessionWithFillingOutReturn(sample[CompleteRepresenteeAnswers], Right(Capacitor))
        val newDraftReturn = draftReturn.copy(
          representeeAnswers = Some(
            IncompleteRepresenteeAnswers.empty.copy(
              name = Some(newName)
            )
          )
        )
        val newJourney = journey.copy(draftReturn = newDraftReturn)

        "there is an error updating the draft return" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(
              newDraftReturn,
              newJourney.subscribedDetails.cgtReference,
              newJourney.agentReferenceNumber
            )(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction(formData(newName)))
        }

        "there is an error updating the session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(
              newDraftReturn,
              newJourney.subscribedDetails.cgtReference,
              newJourney.agentReferenceNumber
            )(Right(()))
            mockStoreSession(session.copy(journeyStatus = Some(newJourney)))(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction(formData(newName)))
        }

      }

      "redirect to the check your answers page" when {

        val newName = IndividualName("Lars", "Fars")

        "the user hasn't started a draft return yet and" when {

          "the section is incomplete" in {
            val (session, journey) =
              sessionWithStartingNewDraftReturn(IncompleteRepresenteeAnswers.empty, Left(PersonalRepresentative))
            val newJourney = journey.copy(
              representeeAnswers = Some(
                IncompleteRepresenteeAnswers.empty.copy(
                  name = Some(newName)
                )
              )
            )

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockStoreSession(session.copy(journeyStatus = Some(newJourney)))(Right(()))
            }

            checkIsRedirect(performAction(formData(newName)), routes.RepresenteeController.checkYourAnswers())
          }

          "the section is complete" in {
            val (session, journey) =
              sessionWithStartingNewDraftReturn(sample[CompleteRepresenteeAnswers], Left(PersonalRepresentative))
            val newJourney = journey.copy(
              representeeAnswers = Some(
                IncompleteRepresenteeAnswers.empty.copy(
                  name = Some(newName)
                )
              )
            )

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockStoreSession(session.copy(journeyStatus = Some(newJourney)))(Right(()))
            }

            checkIsRedirect(performAction(formData(newName)), routes.RepresenteeController.checkYourAnswers())
          }

        }

        "the user has started a draft return and" when {

          "the section is incomplete" in {
            val (session, journey, draftReturn) =
              sessionWithFillingOutReturn(IncompleteRepresenteeAnswers.empty, Right(Capacitor))
            val newDraftReturn = draftReturn.copy(
              representeeAnswers = Some(
                IncompleteRepresenteeAnswers.empty.copy(
                  name = Some(newName)
                )
              )
            )
            val newJourney = journey.copy(draftReturn = newDraftReturn)

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockStoreDraftReturn(
                newDraftReturn,
                newJourney.subscribedDetails.cgtReference,
                newJourney.agentReferenceNumber
              )(Right(()))
              mockStoreSession(session.copy(journeyStatus = Some(newJourney)))(Right(()))
            }

            checkIsRedirect(performAction(formData(newName)), routes.RepresenteeController.checkYourAnswers())
          }

          "the section is complete" in {
            val (session, journey, draftReturn) =
              sessionWithFillingOutReturn(sample[CompleteRepresenteeAnswers], Right(Capacitor))
            val newDraftReturn = draftReturn.copy(
              representeeAnswers = Some(
                IncompleteRepresenteeAnswers.empty.copy(
                  name = Some(newName)
                )
              )
            )
            val newJourney = journey.copy(draftReturn = newDraftReturn)

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockStoreDraftReturn(
                newDraftReturn,
                newJourney.subscribedDetails.cgtReference,
                newJourney.agentReferenceNumber
              )(Right(()))
              mockStoreSession(session.copy(journeyStatus = Some(newJourney)))(Right(()))
            }

            checkIsRedirect(performAction(formData(newName)), routes.RepresenteeController.checkYourAnswers())

          }

        }

      }

      "not perform any updates" when {

        "the name submitted is the same as on already in session" in {
          val name = IndividualName("Lloyd", "Droid")
          val (session, _) =
            sessionWithStartingNewDraftReturn(
              sample[CompleteRepresenteeAnswers].copy(name = name),
              Left(PersonalRepresentative)
            )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkIsRedirect(performAction(formData(name)), routes.RepresenteeController.checkYourAnswers())
        }

      }

    }

    "handling requests to display the check you answers page" must {

      def performAction(): Future[Result] =
        controller.checkYourAnswers()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      behave like nonCapacitorOrPersonalRepBehaviour(performAction)

      val completeAnswers = sample[CompleteRepresenteeAnswers]

      val allQuestionsAnswers = IncompleteRepresenteeAnswers(
        Some(completeAnswers.name)
      )

      "redirect to the enter name page" when {

        "that question hasn't been answered yet" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithFillingOutReturn(allQuestionsAnswers.copy(name = None), Left(PersonalRepresentative))._1
            )
          }

          checkIsRedirect(performAction(), routes.RepresenteeController.enterName())
        }
      }

    }

  }

  def redirectToStartBehaviour(performAction: () => Future[Result]): Unit =
    redirectToStartWhenInvalidJourney(performAction, {
      case _: StartingNewDraftReturn | _: FillingOutReturn => true
      case _                                               => false
    })

  def nonCapacitorOrPersonalRepBehaviour(performAction: () => Future[Result]): Unit =
    "redirect to the task list endpoint" when {
      "the user has selected 'self' for the person they are submitting the return for" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            sessionWithStartingNewDraftReturn(None, Some(Self))._1
          )
        }

        checkIsRedirect(performAction(), returns.routes.TaskListController.taskList())
      }

      "the user has not indicated who they are submitting the return for" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            sessionWithStartingNewDraftReturn(None, None)._1
          )
        }

        checkIsRedirect(performAction(), returns.routes.TaskListController.taskList())
      }
    }

}

class DisabledRepresenteeControllerSpec extends ControllerSpec with AuthSupport with SessionSupport {

  override lazy val additionalConfig: Configuration = Configuration(
    "capacitors-and-personal-representatives.enabled" -> "false"
  )

  override val overrideBindings =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore)
    )

  lazy val controller = instanceOf[RepresenteeController]

  "RepresenteeController" when {

    "the capacitor and personal representative journey are disabled in config" must {

      "redirect to the capacitor and personal representative exit page" when {

        def test(action: Action[AnyContent]): Unit = {
          val journey =
            sample[StartingNewDraftReturn].copy(
              newReturnTriageAnswers = Right(
                sample[CompleteSingleDisposalTriageAnswers].copy(
                  individualUserType = Some(Capacitor)
                )
              )
            )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData.empty.copy(journeyStatus = Some(journey)))
          }

          checkIsRedirect(
            action(FakeRequest()),
            returns.triage.routes.CommonTriageQuestionsController.capacitorsAndPersonalRepresentativesNotHandled()
          )
        }

        "handling requests to display the enter name page" in {
          test(controller.enterName())
        }

        "handling submitted names" in {
          test(controller.enterNameSubmit())
        }

        "handling requests to display the check your answers page" in {
          test(controller.checkYourAnswers())
        }

      }

    }

  }

}
