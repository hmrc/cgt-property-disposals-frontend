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

import java.time.LocalDate

import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.Configuration
import play.api.http.Status.BAD_REQUEST
import play.api.i18n.MessagesApi
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.{Action, AnyContent, Call, Result}
import play.api.test.FakeRequest
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.ReturnsServiceSupport
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, ControllerSpec, DateErrorScenarios, NameFormValidationTests, SessionSupport, returns}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, BusinessPartnerRecordServiceSupport, ControllerSpec, NameFormValidationTests, SessionSupport, returns}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, StartingNewDraftReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.{Address, Postcode}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address.UkAddress
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.{CgtReference, NINO, SAUTR, SapNumber}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.{ContactName, IndividualName}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.SubscribedDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.bpr.BusinessPartnerRecordRequest.IndividualBusinessPartnerRecordRequest
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.bpr.{BusinessPartnerRecord, BusinessPartnerRecordResponse}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.email.Email
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.IndividualUserType._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.MultipleDisposalsTriageAnswers.CompleteMultipleDisposalsTriageAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.RepresenteeAnswers.{CompleteRepresenteeAnswers, IncompleteRepresenteeAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.RepresenteeReferenceId.{NoReferenceId, RepresenteeCgtReference, RepresenteeNino, RepresenteeSautr}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.CompleteSingleDisposalTriageAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{DateOfDeath, DraftMultipleDisposalsReturn, IndividualUserType, RepresenteeAnswers, RepresenteeContactDetails, RepresenteeReferenceId}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, SessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.onboarding.{BusinessPartnerRecordService, SubscriptionService}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService

import scala.concurrent.Future

class RepresenteeControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with ScalaCheckDrivenPropertyChecks
    with RedirectToStartBehaviour
    with ReturnsServiceSupport
    with BusinessPartnerRecordServiceSupport
    with NameFormValidationTests {

  override lazy val additionalConfig: Configuration = Configuration(
    "capacitors-and-personal-representatives.enabled" -> "true"
  )

  override val overrideBindings =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore),
      bind[ReturnsService].toInstance(mockReturnsService),
      bind[BusinessPartnerRecordService].toInstance(mockBusinessPartnerRecordService),
      bind[SubscriptionService].toInstance(mockSubscriptionService)
    )

  lazy val controller = instanceOf[RepresenteeController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  def sessionWithStartingNewDraftReturn(
    answers: Option[RepresenteeAnswers],
    individualUserType: Option[IndividualUserType],
    subscribedDetails: SubscribedDetails
  ): (SessionData, StartingNewDraftReturn) = {
    val journey = sample[StartingNewDraftReturn].copy(
      subscribedDetails  = subscribedDetails,
      representeeAnswers = answers,
      newReturnTriageAnswers =
        Right(sample[CompleteSingleDisposalTriageAnswers].copy(individualUserType = individualUserType))
    )
    val session = SessionData.empty.copy(journeyStatus = Some(journey))
    session -> journey
  }

  def sessionWithStartingNewDraftReturn(
    answers: RepresenteeAnswers,
    representativeType: Either[PersonalRepresentative.type, Capacitor.type],
    subscribedDetails: SubscribedDetails = sample[SubscribedDetails]
  ): (SessionData, StartingNewDraftReturn) =
    sessionWithStartingNewDraftReturn(Some(answers), Some(representativeType.merge), subscribedDetails)

  def sessionWithFillingOutReturn(
    answers: Option[RepresenteeAnswers],
    individualUserType: Option[IndividualUserType],
    subscribedDetails: SubscribedDetails
  ): (SessionData, FillingOutReturn, DraftMultipleDisposalsReturn) = {
    val draftReturn = sample[DraftMultipleDisposalsReturn].copy(
      representeeAnswers = answers,
      triageAnswers      = sample[CompleteMultipleDisposalsTriageAnswers].copy(individualUserType = individualUserType)
    )
    val journey = sample[FillingOutReturn].copy(draftReturn = draftReturn, subscribedDetails = subscribedDetails)

    val session = SessionData.empty.copy(journeyStatus = Some(journey))
    (session, journey, draftReturn)
  }

  def sessionWithFillingOutReturn(
    answers: RepresenteeAnswers,
    representativeType: Either[PersonalRepresentative.type, Capacitor.type],
    subscribedDetails: SubscribedDetails = sample[SubscribedDetails]
  ): (SessionData, FillingOutReturn, DraftMultipleDisposalsReturn) =
    sessionWithFillingOutReturn(Some(answers), Some(representativeType.merge), subscribedDetails)

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

    }

    "handling requests to display the enter date of death page" must {

      def performAction(): Future[Result] = controller.enterDateOfDeath()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      behave like nonCapacitorOrPersonalRepBehaviour(performAction)

      "display the page" when {

        def test(
          sessionData: SessionData,
          expectedTitleKey: String,
          expectedBackLink: Call,
          expectReturnToSummaryLink: Boolean,
          expectedPrepopulatedValue: Option[DateOfDeath]
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
                .attr("action") shouldBe routes.RepresenteeController.enterDateOfDeathSubmit().url
              doc.select("#returnToSummaryLink").text() shouldBe (
                if (expectReturnToSummaryLink) messageFromMessageKey("returns.return-to-summary-link")
                else ""
              )
              expectedPrepopulatedValue.foreach { date =>
                doc.select("#dateOfDeath-day").attr("value")   shouldBe date.value.getDayOfMonth.toString
                doc.select("#dateOfDeath-month").attr("value") shouldBe date.value.getMonthValue.toString
                doc.select("#dateOfDeath-year").attr("value")  shouldBe date.value.getYear.toString
              }
            }
          )

        }

        "the user is starting a draft return and" when {

          "the user is a personal representative" in {
            val date = DateOfDeath(LocalDate.now())
            test(
              sessionWithStartingNewDraftReturn(
                IncompleteRepresenteeAnswers.empty.copy(dateOfDeath = Some(date)),
                Left(PersonalRepresentative)
              )._1,
              "dateOfDeath.title",
              routes.RepresenteeController.enterName(),
              expectReturnToSummaryLink = false,
              Some(date)
            )

          }

          "the section is complete" in {
            val answers = sample[CompleteRepresenteeAnswers]
            test(
              sessionWithStartingNewDraftReturn(answers, Left(PersonalRepresentative))._1,
              "dateOfDeath.title",
              routes.RepresenteeController.checkYourAnswers(),
              expectReturnToSummaryLink = false,
              answers.dateOfDeath
            )
          }

        }

        "the user has already started a draft return and" when {

          "the user is a personal representative" in {
            val date = DateOfDeath(LocalDate.now())
            test(
              sessionWithFillingOutReturn(
                IncompleteRepresenteeAnswers.empty.copy(dateOfDeath = Some(date)),
                Left(PersonalRepresentative)
              )._1,
              "dateOfDeath.title",
              routes.RepresenteeController.enterName(),
              expectReturnToSummaryLink = true,
              Some(date)
            )
          }

          "the section is complete" in {
            val answers = sample[CompleteRepresenteeAnswers]
            test(
              sessionWithFillingOutReturn(answers, Left(PersonalRepresentative))._1,
              "dateOfDeath.title",
              routes.RepresenteeController.checkYourAnswers(),
              expectReturnToSummaryLink = true,
              answers.dateOfDeath
            )
          }

        }

      }

      "redirect to check your answers page" when {
        "the representive type is capacitor" in {
          val answers = sample[CompleteRepresenteeAnswers]
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithFillingOutReturn(answers, Right(Capacitor))._1
            )
          }

          checkIsRedirect(performAction(), routes.RepresenteeController.checkYourAnswers())

        }
      }

    }

    "handling submitted date of death" must {

      def performAction(formData: Seq[(String, String)]): Future[Result] =
        controller.enterDateOfDeathSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*))

      val dayKey   = "dateOfDeath-day"
      val monthKey = "dateOfDeath-month"
      val yearKey  = "dateOfDeath-year"

      def formData(dateOfDeath: DateOfDeath): Seq[(String, String)] =
        Seq(
          dayKey   -> dateOfDeath.value.getDayOfMonth.toString,
          monthKey -> dateOfDeath.value.getMonthValue.toString,
          yearKey  -> dateOfDeath.value.getYear.toString
        )

      behave like redirectToStartBehaviour(() => performAction(Seq.empty))

      behave like nonCapacitorOrPersonalRepBehaviour(() => performAction(Seq.empty))

      "show an error page" when {

        val oldAnswers = sample[CompleteRepresenteeAnswers].copy(dateOfDeath = Some(DateOfDeath(LocalDate.now())))
        val newDate    = DateOfDeath(LocalDate.now().minusDays(1))
        val (session, journey, draftReturn) =
          sessionWithFillingOutReturn(oldAnswers, Left(PersonalRepresentative))
        val newDraftReturn = draftReturn.copy(
          representeeAnswers = Some(oldAnswers.copy(dateOfDeath = Some(newDate)))
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

          checkIsTechnicalErrorPage(performAction(formData(newDate)))
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
          checkIsTechnicalErrorPage(performAction(formData(newDate)))
        }

        "the date entered is invalid" in {

          val session = sessionWithFillingOutReturn(
            IncompleteRepresenteeAnswers.empty.copy(name = Some(sample[IndividualName])),
            Left(PersonalRepresentative)
          )._1

          DateErrorScenarios
            .dateErrorScenarios("dateOfDeath", "")
            .foreach { scenario =>
              withClue(s"For date error scenario $scenario: ") {
                val data = List(
                  "dateOfDeath-day"   -> scenario.dayInput,
                  "dateOfDeath-month" -> scenario.monthInput,
                  "dateOfDeath-year"  -> scenario.yearInput
                ).collect { case (key, Some(value)) => key -> value }

                testFormError(performAction, "dateOfDeath.title", session)(
                  data,
                  scenario.expectedErrorMessageKey
                )
              }
            }
        }

        "the date is in the future" in {
          testFormError(performAction, "dateOfDeath.title", session)(
            formData(DateOfDeath(LocalDate.now().plusDays(1L))),
            "dateOfDeath.error.tooFarInFuture"
          )
        }

      }

      "redirect to the check your answers page" when {

        val dateOfDeath = DateOfDeath(LocalDate.now())

        "the user hasn't started a draft return yet and" when {

          "the section is incomplete" in {
            val (session, journey) =
              sessionWithStartingNewDraftReturn(IncompleteRepresenteeAnswers.empty, Left(PersonalRepresentative))
            val newJourney = journey.copy(
              representeeAnswers = Some(
                IncompleteRepresenteeAnswers.empty.copy(
                  dateOfDeath = Some(dateOfDeath)
                )
              )
            )

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockStoreSession(session.copy(journeyStatus = Some(newJourney)))(Right(()))
            }

            checkIsRedirect(performAction(formData(dateOfDeath)), routes.RepresenteeController.checkYourAnswers())
          }

          "the section is complete" in {
            val data    = sample[CompleteRepresenteeAnswers].copy(dateOfDeath = Some(dateOfDeath))
            val newDate = DateOfDeath(LocalDate.now().minusDays(1))
            val (session, journey) =
              sessionWithStartingNewDraftReturn(data, Left(PersonalRepresentative))
            val newJourney = journey.copy(
              representeeAnswers = Some(data.copy(dateOfDeath = Some(newDate)))
            )

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockStoreSession(session.copy(journeyStatus = Some(newJourney)))(Right(()))
            }

            checkIsRedirect(
              performAction(formData(newDate)),
              routes.RepresenteeController.checkYourAnswers()
            )
          }

        }

        "the user has started a draft return and" when {

          "the section is incomplete" in {
            val (session, journey, draftReturn) =
              sessionWithFillingOutReturn(IncompleteRepresenteeAnswers.empty, Left(PersonalRepresentative))
            val newDraftReturn = draftReturn.copy(
              representeeAnswers = Some(
                IncompleteRepresenteeAnswers.empty.copy(
                  dateOfDeath = Some(dateOfDeath)
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

            checkIsRedirect(performAction(formData(dateOfDeath)), routes.RepresenteeController.checkYourAnswers())
          }

          "the section is complete" in {
            val oldData = sample[IncompleteRepresenteeAnswers]
            val (session, journey, draftReturn) =
              sessionWithFillingOutReturn(oldData, Left(PersonalRepresentative))
            val newDraftReturn = draftReturn.copy(
              representeeAnswers = Some(
                oldData.copy(
                  dateOfDeath = Some(dateOfDeath)
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

            checkIsRedirect(performAction(formData(dateOfDeath)), routes.RepresenteeController.checkYourAnswers())

          }

        }

      }

    }

    "handling requests to display the enter id page" must {

      def performAction(): Future[Result] = controller.enterId()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      behave like nonCapacitorOrPersonalRepBehaviour(performAction)

      "display the page" when {

        def test(
          sessionData: SessionData,
          expectedBackLink: Call,
          expectReturnToSummaryLink: Boolean,
          expectedPrepopulatedValue: Option[RepresenteeReferenceId]
        ): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionData)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("representeeReferenceIdType.title"), { doc =>
              doc.select("#back").attr("href") shouldBe expectedBackLink.url
              doc
                .select("#content > article > form")
                .attr("action") shouldBe routes.RepresenteeController.enterIdSubmit().url
              doc.select("#returnToSummaryLink").text() shouldBe (
                if (expectReturnToSummaryLink) messageFromMessageKey("returns.return-to-summary-link")
                else ""
              )
              expectedPrepopulatedValue.foreach {
                case RepresenteeCgtReference(cgtRef) =>
                  doc.select("#representeeCgtRef").attr("value") shouldBe cgtRef.value
                case RepresenteeNino(nino) =>
                  doc.select("#representeeNino").attr("value") shouldBe nino.value
                case RepresenteeSautr(sautr) =>
                  doc.select("#representeeSautr").attr("value") shouldBe sautr.value
                case NoReferenceId =>
                  doc.select("#representeeReferenceIdType-3").attr("checked") shouldBe "checked"
              }
            }
          )

        }

        "the user is starting a draft return and" when {

          val requiredPreviousAnswers =
            IncompleteRepresenteeAnswers.empty.copy(name = Some(sample[IndividualName]))

          "the user has not selected an option before" in {
            List(
              Left(PersonalRepresentative) -> routes.RepresenteeController.enterDateOfDeath(),
              Right(Capacitor)             -> routes.RepresenteeController.enterName()
            ) foreach {
              case (representativeType, redirect) =>
                test(
                  sessionWithStartingNewDraftReturn(
                    requiredPreviousAnswers,
                    representativeType
                  )._1,
                  redirect,
                  expectReturnToSummaryLink = false,
                  None
                )
            }
          }

          "the user has previously submitted a cgt reference" in {
            List(
              Left(PersonalRepresentative) -> routes.RepresenteeController.enterDateOfDeath(),
              Right(Capacitor)             -> routes.RepresenteeController.enterName()
            ) foreach {
              case (representativeType, redirect) =>
                val cgtReference = sample[RepresenteeCgtReference]
                test(
                  sessionWithStartingNewDraftReturn(
                    requiredPreviousAnswers.copy(id = Some(cgtReference)),
                    representativeType
                  )._1,
                  redirect,
                  expectReturnToSummaryLink = false,
                  Some(cgtReference)
                )
            }
          }

          "the user has previously submitted a nino" in {
            val nino = sample[RepresenteeNino]
            test(
              sessionWithStartingNewDraftReturn(
                requiredPreviousAnswers.copy(id = Some(nino)),
                Left(PersonalRepresentative)
              )._1,
              routes.RepresenteeController.enterDateOfDeath(),
              expectReturnToSummaryLink = false,
              Some(nino)
            )

          }

          "the user has previously submitted an sautr" in {
            val sautr = sample[RepresenteeSautr]
            test(
              sessionWithStartingNewDraftReturn(
                requiredPreviousAnswers.copy(id = Some(sautr)),
                Left(PersonalRepresentative)
              )._1,
              routes.RepresenteeController.enterDateOfDeath(),
              expectReturnToSummaryLink = false,
              Some(sautr)
            )

          }

          "Redirects based on representitive type" in {
            List(
              Left(PersonalRepresentative) -> routes.RepresenteeController.enterDateOfDeath(),
              Right(Capacitor)             -> routes.RepresenteeController.enterName()
            ) foreach {
              case (representativeType, redirect) =>
                val cgtReference = sample[RepresenteeCgtReference]
                test(
                  sessionWithStartingNewDraftReturn(
                    requiredPreviousAnswers.copy(id = Some(cgtReference)),
                    representativeType
                  )._1,
                  redirect,
                  expectReturnToSummaryLink = false,
                  Some(cgtReference)
                )
            }
          }

          "the user has previously submitted no reference" in {
            test(
              sessionWithStartingNewDraftReturn(
                requiredPreviousAnswers.copy(id = Some(NoReferenceId)),
                Right(Capacitor)
              )._1,
              routes.RepresenteeController.enterName(),
              expectReturnToSummaryLink = false,
              Some(NoReferenceId)
            )
          }

          "the section is complete" in {
            val answers = sample[CompleteRepresenteeAnswers]
            test(
              sessionWithStartingNewDraftReturn(answers, Right(Capacitor))._1,
              routes.RepresenteeController.checkYourAnswers(),
              expectReturnToSummaryLink = false,
              Some(answers.id)
            )
          }

        }

        "the user has already started a draft return and" when {

          "the journey is incomplete" in {
            val cgtReference = sample[RepresenteeCgtReference]

            test(
              sessionWithFillingOutReturn(
                IncompleteRepresenteeAnswers.empty
                  .copy(name = Some(sample[IndividualName]))
                  .copy(id = Some(cgtReference)),
                Left(PersonalRepresentative)
              )._1,
              routes.RepresenteeController.enterDateOfDeath(),
              expectReturnToSummaryLink = true,
              Some(cgtReference)
            )
          }

          "the section is complete" in {
            val answers = sample[CompleteRepresenteeAnswers]
            test(
              sessionWithFillingOutReturn(answers, Left(PersonalRepresentative))._1,
              routes.RepresenteeController.checkYourAnswers(),
              expectReturnToSummaryLink = true,
              Some(answers.id)
            )
          }

        }

      }

    }

    "handling requests to display the confirm person page" must {
      def performAction(): Future[Result] = controller.confirmPerson()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      behave like nonCapacitorOrPersonalRepBehaviour(performAction)

      def test(
        sessionData: SessionData,
        expectedBackLink: Call,
        expectedName: IndividualName,
        isCgtRow: Boolean
      ): Unit = {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionData)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("representeeConfirmPerson.title"),
          doc => {
            doc.select("#back").attr("href") shouldBe expectedBackLink.url
            doc
              .select("#name-question")
              .text shouldBe (messageFromMessageKey("representeeConfirmPerson.summaryLine1"))
            doc
              .select("#name-answer")
              .text shouldBe s"${expectedName.firstName} ${expectedName.lastName}"
            if (isCgtRow)
              doc
                .select("#account-question")
                .text shouldBe messageFromMessageKey("representeeConfirmPerson.summaryLine2.cgtReferenceId")
            else ()
            doc
              .select("#content > article > form")
              .attr("action") shouldBe routes.RepresenteeController.confirmPersonSubmit().url
            doc
              .select("#confirmed > legend > h2")
              .text() shouldBe messageFromMessageKey("representeeConfirmPerson.formTitle")
          }
        )
      }

      "display the page" when {

        "the user has name and id " when {
          val name = IndividualName("First", "Last")
          val requiredPreviousAnswers =
            IncompleteRepresenteeAnswers(
              Some(name),
              Some(RepresenteeNino(NINO("AB123456C"))),
              Some(sample[DateOfDeath]),
              None,
              false,
              false
            )
          "show the summary" in {
            test(
              sessionWithStartingNewDraftReturn(
                requiredPreviousAnswers,
                Left(PersonalRepresentative)
              )._1,
              routes.RepresenteeController.enterId(),
              name,
              false
            )
          }
        }
      }

      "redirect the page to cya" when {

        "the user has no id " when {
          val requiredPreviousAnswers =
            IncompleteRepresenteeAnswers(Some(IndividualName("First", "Last")), None, None, None, false, false)
          "redirect to enter id page" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionWithStartingNewDraftReturn(
                  requiredPreviousAnswers,
                  Left(PersonalRepresentative)
                )._1
              )
            }
            checkIsRedirect(performAction(), routes.RepresenteeController.checkYourAnswers())
          }
        }
      }

    }

    "handling requests to submit the confirm person page" must {
      def performAction(formData: Seq[(String, String)]): Future[Result] =
        controller.confirmPersonSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*))

      behave like redirectToStartBehaviour(() => performAction(Seq.empty))

      behave like nonCapacitorOrPersonalRepBehaviour(() => performAction(Seq.empty))

      "show a form error" when {

        val session = sessionWithFillingOutReturn(
          IncompleteRepresenteeAnswers(
            Some(sample[IndividualName]),
            Some(sample[RepresenteeNino]),
            Some(sample[DateOfDeath]),
            None,
            false,
            false
          ),
          Right(Capacitor)
        )._1

        "nothing is selected" in {

          testFormError(performAction, "representeeConfirmPerson.title", session)(
            List(),
            "confirmed.error.required"
          )
        }
      }

      "redirect the page to cya" when {
        "the user has clicked No " in {
          val requiredPreviousAnswers = IncompleteRepresenteeAnswers(
            Some(IndividualName("First", "Last")),
            Some(RepresenteeNino(NINO("AB123456C"))),
            None,
            None,
            false,
            false
          )

          val (session, sndr) = sessionWithStartingNewDraftReturn(
            requiredPreviousAnswers,
            Left(PersonalRepresentative)
          )
          val newSndr =
            sndr.copy(representeeAnswers = Some(IncompleteRepresenteeAnswers(None, None, None, None, false, false)))
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreSession(session.copy(journeyStatus = Some(newSndr)))(Right())
          }
          checkIsRedirect(performAction(List("confirmed" -> "false")), routes.RepresenteeController.checkYourAnswers())
        }

        "the user has clicked Yes " in {
          val requiredPreviousAnswers = IncompleteRepresenteeAnswers(
            Some(IndividualName("First", "Last")),
            Some(RepresenteeNino(NINO("AB123456C"))),
            None,
            None,
            false,
            false
          )

          val (session, sndr) = sessionWithStartingNewDraftReturn(
            requiredPreviousAnswers,
            Left(PersonalRepresentative)
          )

          val newSndr =
            sndr.copy(representeeAnswers = Some(requiredPreviousAnswers.copy(hasConfirmedPerson = true)))
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreSession(session.copy(journeyStatus = Some(newSndr)))(Right())
          }
          checkIsRedirect(performAction(List("confirmed" -> "true")), routes.RepresenteeController.checkYourAnswers())
        }
      }

    }

    "handling submitted ids" must {

      def performAction(formData: Seq[(String, String)]): Future[Result] =
        controller.enterIdSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*))

      val outerKey        = "representeeReferenceIdType"
      val cgtReferenceKey = "representeeCgtRef"
      val ninoKey         = "representeeNino"
      val sautrKey        = "representeeSautr"

      def formData(id: RepresenteeReferenceId): Seq[(String, String)] = id match {
        case RepresenteeCgtReference(cgtRef)      => Seq(outerKey -> "0", cgtReferenceKey -> cgtRef.value)
        case RepresenteeNino(nino)                => Seq(outerKey -> "1", ninoKey -> nino.value)
        case RepresenteeSautr(sautr)              => Seq(outerKey -> "2", sautrKey -> sautr.value)
        case RepresenteeReferenceId.NoReferenceId => Seq(outerKey -> "3")
      }

      behave like redirectToStartBehaviour(() => performAction(Seq.empty))

      behave like nonCapacitorOrPersonalRepBehaviour(() => performAction(Seq.empty))

      "show a form error" when {

        val session = sessionWithFillingOutReturn(
          IncompleteRepresenteeAnswers.empty.copy(name = Some(sample[IndividualName])),
          Right(Capacitor)
        )._1

        def test: (Seq[(String, String)], String) => Unit =
          testFormError(performAction, "representeeReferenceIdType.title", session) _

        "nothing is selected" in {
          test(Seq.empty, "representeeReferenceIdType.error.required")
        }

        "cgt reference is selected and a value is submitted" which {
          "does not belong to the name entered" in {
            val name        = IndividualName("First", "Last")
            val anotherName = IndividualName("Another", "Name")
            val answers     = IncompleteRepresenteeAnswers.empty.copy(name = Some(name))
            val cgtRef      = RepresenteeCgtReference(CgtReference("XYCGTP123456789"))

            val (session, journey, draftReturn) =
              sessionWithFillingOutReturn(answers, Right(Capacitor))
            val newDraftReturn = draftReturn.copy(
              representeeAnswers = Some(answers.copy(id = Some(cgtRef)))
            )
            val newJourney = journey.copy(draftReturn = newDraftReturn)
            val expectedSubscribedDetails = SubscribedDetails(
              Right(anotherName),
              Email("email"),
              UkAddress("Some St.", None, None, None, Postcode("EC1 AB2")),
              ContactName(""),
              CgtReference("cgtReference"),
              None,
              false
            )
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockGetSubscriptionDetails(cgtRef.value, Right(expectedSubscribedDetails))
            }
            checkIsSimpleBadRequest(performAction(formData(cgtRef)), "Name check error")
          }
          "is empty" in {
            test(Seq(outerKey -> "0"), "representeeCgtRef.error.required")
          }

          "is too short" in {
            test(Seq(outerKey -> "0", cgtReferenceKey -> "ABC"), "representeeCgtRef.error.tooShort")
          }

          "is too long" in {
            test(Seq(outerKey -> "0", cgtReferenceKey -> "ABC123453432423426"), "representeeCgtRef.error.tooLong")
          }

          "contains invalid characters" in {
            test(
              Seq(outerKey -> "0", cgtReferenceKey -> "XYCGTP12345678!"),
              "representeeCgtRef.error.invalidCharacters"
            )
          }

          "contains valid characters but is not of the right format" in {
            test(
              Seq(outerKey -> "0", cgtReferenceKey -> "BYCGTP123456789"),
              "representeeCgtRef.error.pattern"
            )
          }

        }

        "nino is selected and a value is submitted" which {
          "does not belong to the name entered" in {
            val individualName = IndividualName("First", "Last")
            val answers = IncompleteRepresenteeAnswers.empty.copy(
              name = Some(individualName),
              id   = Some(sample[RepresenteeCgtReference])
            )
            val ninoValue = NINO("AB123456C")
            val nino      = RepresenteeNino(ninoValue)

            val (session, journey, draftReturn) =
              sessionWithFillingOutReturn(answers, Left(PersonalRepresentative))

            val businessPartnerRecordRequest =
              IndividualBusinessPartnerRecordRequest(Right(ninoValue), Some(individualName))

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockGetBusinessPartnerRecord(
                businessPartnerRecordRequest,
                Right(BusinessPartnerRecordResponse(None, None))
              )
            }
            checkIsSimpleBadRequest(performAction(formData(nino)), "Name check error")
          }

          "is empty" in {
            test(Seq(outerKey -> "1"), "representeeNino.error.required")
          }

          "is too short" in {
            test(Seq(outerKey -> "1", ninoKey -> "AB123"), "representeeNino.error.tooShort")

          }

          "is too long" in {
            test(Seq(outerKey -> "1", ninoKey -> "AB123456789B"), "representeeNino.error.tooLong")
          }

          "contains invalid characters" in {
            test(Seq(outerKey -> "1", ninoKey -> "AB123456$"), "representeeNino.error.invalidCharacters")
          }

          "contains valid characters but is not of the right format" in {
            test(Seq(outerKey -> "1", ninoKey -> "ZZ123456C"), "representeeNino.error.pattern")
          }

        }

        "sautr is selected and a value is submitted" which {

          "is empty" in {
            test(Seq(outerKey -> "2"), "representeeSautr.error.required")
          }

          "is too short" in {
            test(Seq(outerKey -> "2", sautrKey -> "123"), "representeeSautr.error.tooShort")
          }

          "is too long" in {
            test(Seq(outerKey -> "2", sautrKey -> "12345678900"), "representeeSautr.error.tooLong")
          }

          "contains non-numerical characters" in {
            test(Seq(outerKey -> "2", sautrKey -> "123456789A"), "representeeSautr.error.invalid")
          }

        }

      }

      "show an error page" when {

        val answers = IncompleteRepresenteeAnswers.empty.copy(name = Some(IndividualName("First", "Last")))
        val cgtRef  = RepresenteeCgtReference(CgtReference("XYCGTP123456789"))

        val (session, journey, draftReturn) =
          sessionWithFillingOutReturn(answers, Right(Capacitor))
        val newDraftReturn = draftReturn.copy(
          representeeAnswers = Some(answers.copy(id = Some(cgtRef)))
        )
        val newJourney = journey.copy(draftReturn = newDraftReturn)
        val expectedSubscribedDetails = SubscribedDetails(
          Right(IndividualName("First", "Last")),
          Email("email"),
          UkAddress("Some St.", None, None, None, Postcode("EC1 AB2")),
          ContactName(""),
          CgtReference("cgtReference"),
          None,
          false
        )

        "there is an error getting the subscription details" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockGetSubscriptionDetails(cgtRef.value, Left(Error("Some error")))
          }

          checkIsTechnicalErrorPage(performAction(formData(cgtRef)))
        }

        "there is an error updating the draft return" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockGetSubscriptionDetails(cgtRef.value, Right(expectedSubscribedDetails))
            mockStoreDraftReturn(
              newDraftReturn,
              newJourney.subscribedDetails.cgtReference,
              newJourney.agentReferenceNumber
            )(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction(formData(cgtRef)))
        }

        "there is an error updating the session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockGetSubscriptionDetails(cgtRef.value, Right(expectedSubscribedDetails))
            mockStoreDraftReturn(
              newDraftReturn,
              newJourney.subscribedDetails.cgtReference,
              newJourney.agentReferenceNumber
            )(Right(()))
            mockStoreSession(session.copy(journeyStatus = Some(newJourney)))(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction(formData(cgtRef)))
        }

        "there is an error getting the business partner response" in {
          val individualName = IndividualName("First", "Last")
          val answers = IncompleteRepresenteeAnswers.empty.copy(
            name = Some(individualName),
            id   = Some(sample[RepresenteeCgtReference])
          )
          val ninoValue = NINO("AB123456C")
          val nino      = RepresenteeNino(ninoValue)

          val (session, journey, draftReturn) =
            sessionWithFillingOutReturn(answers, Left(PersonalRepresentative))
          val newDraftReturn = draftReturn.copy(
            representeeAnswers = Some(answers.copy(id = Some(nino)))
          )
          val newJourney = journey.copy(draftReturn = newDraftReturn)
          val businessPartnerRecordRequest =
            IndividualBusinessPartnerRecordRequest(Right(ninoValue), Some(individualName))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockGetBusinessPartnerRecord(businessPartnerRecordRequest, Left(Error("Some error")))
          }
          checkIsTechnicalErrorPage(performAction(formData(nino)))
        }

      }

      "redirect to the check your answers page" when {

        "all updates are successful and" when {

          "the user has submitted a valid cgt reference" in {
            val answers = IncompleteRepresenteeAnswers.empty.copy(name = Some(IndividualName("First", "Last")))
            val cgtRef  = RepresenteeCgtReference(CgtReference("XYCGTP123456789"))

            val (session, journey, draftReturn) =
              sessionWithFillingOutReturn(answers, Right(Capacitor))
            val newDraftReturn = draftReturn.copy(
              representeeAnswers = Some(answers.copy(id = Some(cgtRef)))
            )
            val newJourney = journey.copy(draftReturn = newDraftReturn)
            val expectedSubscribedDetails = SubscribedDetails(
              Right(IndividualName("First", "Last")),
              Email("email"),
              UkAddress("Some St.", None, None, None, Postcode("EC1 AB2")),
              ContactName(""),
              CgtReference("cgtReference"),
              None,
              false
            )
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockGetSubscriptionDetails(cgtRef.value, Right(expectedSubscribedDetails))
              mockStoreDraftReturn(
                newDraftReturn,
                newJourney.subscribedDetails.cgtReference,
                newJourney.agentReferenceNumber
              )(Right(()))
              mockStoreSession(session.copy(journeyStatus = Some(newJourney)))(Right(()))
            }

            checkIsRedirect(performAction(formData(cgtRef)), routes.RepresenteeController.checkYourAnswers())
          }

          "the user has submitted a valid nino" in {
            val individualName = IndividualName("First", "Last")
            val answers = IncompleteRepresenteeAnswers.empty.copy(
              name = Some(individualName),
              id   = Some(sample[RepresenteeCgtReference])
            )
            val ninoValue = NINO("AB123456C")
            val nino      = RepresenteeNino(ninoValue)

            val (session, journey, draftReturn) =
              sessionWithFillingOutReturn(answers, Left(PersonalRepresentative))
            val newDraftReturn = draftReturn.copy(
              representeeAnswers = Some(answers.copy(id = Some(nino)))
            )
            val newJourney = journey.copy(draftReturn = newDraftReturn)
            val businessPartnerRecordRequest =
              IndividualBusinessPartnerRecordRequest(Right(ninoValue), Some(individualName))
            val expectedBusinessPartnerRecordResponse = BusinessPartnerRecordResponse(
              Some(
                BusinessPartnerRecord(
                  None,
                  UkAddress("Some St.", None, None, None, Postcode("EC1 AB2")),
                  SapNumber("123"),
                  Right(IndividualName("First", "Last"))
                )
              ),
              None
            )

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockGetBusinessPartnerRecord(businessPartnerRecordRequest, Right(expectedBusinessPartnerRecordResponse))
              mockStoreDraftReturn(
                newDraftReturn,
                newJourney.subscribedDetails.cgtReference,
                newJourney.agentReferenceNumber
              )(Right(()))
              mockStoreSession(session.copy(journeyStatus = Some(newJourney)))(Right(()))
            }

            checkIsRedirect(performAction(formData(nino)), routes.RepresenteeController.checkYourAnswers())

          }

          "the user has submitted a valid sa utr" in {
            val individualName = IndividualName("First", "Last")
            val answers        = IncompleteRepresenteeAnswers.empty.copy(name = Some(individualName))
            val sautrValue     = SAUTR("1234567890")
            val sautr          = RepresenteeSautr(sautrValue)
            val businessPartnerRecordRequest =
              IndividualBusinessPartnerRecordRequest(Left(sautrValue), Some(individualName))
            val expectedBusinessPartnerRecordResponse = BusinessPartnerRecordResponse(
              Some(
                BusinessPartnerRecord(
                  None,
                  UkAddress("Some St.", None, None, None, Postcode("EC1 AB2")),
                  SapNumber("123"),
                  Right(individualName)
                )
              ),
              None
            )
            val (session, journey) =
              sessionWithStartingNewDraftReturn(answers, Right(Capacitor))
            val newAnswers = answers.copy(id                 = Some(sautr))
            val newJourney = journey.copy(representeeAnswers = Some(newAnswers))

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockGetBusinessPartnerRecord(businessPartnerRecordRequest, Right(expectedBusinessPartnerRecordResponse))
              mockStoreSession(session.copy(journeyStatus = Some(newJourney)))(Right(()))
            }

            checkIsRedirect(performAction(formData(sautr)), routes.RepresenteeController.checkYourAnswers())

          }

          "the user has submitted no reference" in {
            val answers = IncompleteRepresenteeAnswers.empty.copy(name = Some(IndividualName("First", "Last")))

            val (session, journey) =
              sessionWithStartingNewDraftReturn(answers, Right(Capacitor))
            val newAnswers = answers.copy(id                 = Some(NoReferenceId))
            val newJourney = journey.copy(representeeAnswers = Some(newAnswers))

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockStoreSession(session.copy(journeyStatus = Some(newJourney)))(Right(()))
            }

            checkIsRedirect(performAction(formData(NoReferenceId)), routes.RepresenteeController.checkYourAnswers())
          }

        }

      }

      "not do any updates" when {

        "the data submitted is the same as that already held in session" in {
          val sautr = RepresenteeSautr(SAUTR("1234567890"))
          val answers = IncompleteRepresenteeAnswers.empty.copy(
            name = Some(sample[IndividualName]),
            id   = Some(sautr)
          )

          val (session, _) =
            sessionWithStartingNewDraftReturn(answers, Right(Capacitor))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkIsRedirect(performAction(formData(sautr)), routes.RepresenteeController.checkYourAnswers())
        }

      }

    }

    "handling requests to display the check contact details page" must {

      def performAction(): Future[Result] =
        controller.checkContactDetails()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      behave like nonCapacitorOrPersonalRepBehaviour(performAction)

      "show an error page" when {

        "the journey is incomplete and there are no existing details in session and" when {

          val contactDetails = sample[RepresenteeContactDetails]
          val answers        = sample[IncompleteRepresenteeAnswers].copy(contactDetails = None)
          val (session, journey, draftReturn) =
            sessionWithFillingOutReturn(
              answers,
              Left(PersonalRepresentative),
              sample[SubscribedDetails].copy(
                contactName  = contactDetails.contactName,
                address      = contactDetails.address,
                emailAddress = contactDetails.emailAddress
              )
            )

          val newAnswers = answers.copy(
            contactDetails             = Some(contactDetails),
            hasConfirmedContactDetails = false
          )
          val newDraftReturn = draftReturn.copy(
            representeeAnswers = Some(newAnswers)
          )
          val newJourney = journey.copy(draftReturn = newDraftReturn)

          "there is an error updating the draft return" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockStoreDraftReturn(
                newDraftReturn,
                journey.subscribedDetails.cgtReference,
                journey.agentReferenceNumber
              )(Left(Error("")))
            }

            checkIsTechnicalErrorPage(performAction())
          }

          "there is an error updating the session" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockStoreDraftReturn(
                newDraftReturn,
                journey.subscribedDetails.cgtReference,
                journey.agentReferenceNumber
              )(Right(()))
              mockStoreSession(session.copy(journeyStatus = Some(newJourney)))(Left(Error("")))
            }

            checkIsTechnicalErrorPage(performAction())
          }

        }

      }

      "display the page" when {

        def checkPage(
          result: Future[Result],
          contactDetails: RepresenteeContactDetails,
          expectReturnToSummaryLink: Boolean
        ) =
          checkPageIsDisplayed(
            result,
            messageFromMessageKey("representeeContactDetails.title"), { doc =>
              val addressLines = {
                val lines = contactDetails.address match {
                  case Address.UkAddress(line1, line2, town, county, postcode) =>
                    List(Some(line1), line2, town, county, Some(postcode.value))
                  case Address.NonUkAddress(line1, line2, line3, line4, postcode, country) =>
                    List[Option[String]](Some(line1), line2, line3, line4, postcode, country.name)
                }
                lines.collect { case Some(s) => s }
              }

              doc.select("#contactName-answer").text()  shouldBe contactDetails.contactName.value
              doc.select("#address-answer").text()      shouldBe addressLines.mkString(" ")
              doc.select("#emailAddress-answer").text() shouldBe contactDetails.emailAddress.value

              doc
                .select("#content > article > form")
                .attr("action") shouldBe routes.RepresenteeController.checkContactDetailsSubmit().url
              doc.select("#returnToSummaryLink").text() shouldBe (
                if (expectReturnToSummaryLink) messageFromMessageKey("returns.return-to-summary-link")
                else ""
              )
            }
          )

        "the journey is complete" in {
          val contactDetails = sample[RepresenteeContactDetails]
          val answers        = sample[CompleteRepresenteeAnswers].copy(contactDetails = contactDetails)
          val session =
            sessionWithStartingNewDraftReturn(
              answers,
              Left(PersonalRepresentative)
            )._1

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkPage(
            performAction(),
            contactDetails,
            expectReturnToSummaryLink = false
          )
        }

        "the journey is incomplete and there are already contact details in session" in {
          val contactDetails = sample[RepresenteeContactDetails]
          val answers        = sample[IncompleteRepresenteeAnswers].copy(contactDetails = Some(contactDetails))
          val session =
            sessionWithFillingOutReturn(
              answers,
              Right(Capacitor)
            )._1

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkPage(
            performAction(),
            contactDetails,
            expectReturnToSummaryLink = true
          )
        }

        "the journey is incomplete and there are no contact details in session and " +
          "all updates are successful and" when {

          "the user has started a draft return" in {
            val contactDetails = sample[RepresenteeContactDetails]
            val answers        = sample[IncompleteRepresenteeAnswers].copy(contactDetails = None)
            val (session, journey, draftReturn) =
              sessionWithFillingOutReturn(
                answers,
                Left(PersonalRepresentative),
                sample[SubscribedDetails].copy(
                  contactName  = contactDetails.contactName,
                  address      = contactDetails.address,
                  emailAddress = contactDetails.emailAddress
                )
              )

            val newAnswers = answers.copy(
              contactDetails             = Some(contactDetails),
              hasConfirmedContactDetails = false
            )
            val newDraftReturn = draftReturn.copy(
              representeeAnswers = Some(newAnswers)
            )
            val newJourney = journey.copy(draftReturn = newDraftReturn)

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockStoreDraftReturn(
                newDraftReturn,
                journey.subscribedDetails.cgtReference,
                journey.agentReferenceNumber
              )(Right(()))
              mockStoreSession(session.copy(journeyStatus = Some(newJourney)))(Right(()))
            }
            checkPage(
              performAction(),
              contactDetails,
              expectReturnToSummaryLink = true
            )
          }

          "the user has not started a draft return" in {
            val contactDetails = sample[RepresenteeContactDetails]
            val answers        = sample[IncompleteRepresenteeAnswers].copy(contactDetails = None)
            val (session, journey) =
              sessionWithStartingNewDraftReturn(
                answers,
                Left(PersonalRepresentative),
                sample[SubscribedDetails].copy(
                  contactName  = contactDetails.contactName,
                  address      = contactDetails.address,
                  emailAddress = contactDetails.emailAddress
                )
              )

            val newAnswers = answers.copy(
              contactDetails             = Some(contactDetails),
              hasConfirmedContactDetails = false
            )
            val newJourney = journey.copy(representeeAnswers = Some(newAnswers))

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockStoreSession(session.copy(journeyStatus = Some(newJourney)))(Right(()))
            }
            checkPage(
              performAction(),
              contactDetails,
              expectReturnToSummaryLink = false
            )
          }

        }

      }

    }

    "handling submits on the check contact details page" must {

      def performAction(): Future[Result] =
        controller.checkContactDetailsSubmit()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      behave like nonCapacitorOrPersonalRepBehaviour(performAction)

      "immediately redirect to the check your answers page" when {

        "the section is incomplete and user has already confirmed the contact details" in {
          val answers = sample[IncompleteRepresenteeAnswers].copy(
            contactDetails             = Some(sample[RepresenteeContactDetails]),
            hasConfirmedContactDetails = true
          )
          val session = sessionWithFillingOutReturn(answers, Right(Capacitor))._1

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkIsRedirect(performAction(), routes.RepresenteeController.checkYourAnswers())
        }

        "the section is complete" in {
          val answers = sample[CompleteRepresenteeAnswers]
          val session = sessionWithFillingOutReturn(answers, Left(PersonalRepresentative))._1

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkIsRedirect(performAction(), routes.RepresenteeController.checkYourAnswers())

        }

      }

      "show an error page" when {

        "the section is incomplete and the user hasn't confirmed the contact details yet and" when {
          val answers = sample[IncompleteRepresenteeAnswers].copy(
            contactDetails             = Some(sample[RepresenteeContactDetails]),
            hasConfirmedContactDetails = false
          )
          val (session, journey, draftReturn) = sessionWithFillingOutReturn(answers, Right(Capacitor))
          val newAnswers                      = answers.copy(hasConfirmedContactDetails = true)
          val newDraftReturn                  = draftReturn.copy(representeeAnswers = Some(newAnswers))
          val newJourney                      = journey.copy(draftReturn = newDraftReturn)

          "there is an error updating the draft return" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockStoreDraftReturn(
                newDraftReturn,
                journey.subscribedDetails.cgtReference,
                journey.agentReferenceNumber
              )(Left(Error("")))
            }

            checkIsTechnicalErrorPage(performAction())
          }

          "there is an error updating the session" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockStoreDraftReturn(
                newDraftReturn,
                journey.subscribedDetails.cgtReference,
                journey.agentReferenceNumber
              )(Right(()))
              mockStoreSession(session.copy(journeyStatus = Some(newJourney)))(Left(Error("")))
            }

            checkIsTechnicalErrorPage(performAction())
          }

        }

      }

      "redirect to the check your answers page" when {

        "the section is incomplete and the user hasn't confirmed the contact details yet and" +
          "all updates are successful" in {
          val answers = sample[IncompleteRepresenteeAnswers].copy(
            contactDetails             = Some(sample[RepresenteeContactDetails]),
            hasConfirmedContactDetails = false
          )
          val (session, journey, draftReturn) = sessionWithFillingOutReturn(answers, Right(Capacitor))
          val newAnswers                      = answers.copy(hasConfirmedContactDetails = true)
          val newDraftReturn                  = draftReturn.copy(representeeAnswers = Some(newAnswers))
          val newJourney                      = journey.copy(draftReturn = newDraftReturn)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(
              newDraftReturn,
              journey.subscribedDetails.cgtReference,
              journey.agentReferenceNumber
            )(Right(()))
            mockStoreSession(session.copy(journeyStatus = Some(newJourney)))(Right(()))
          }

          checkIsRedirect(performAction(), routes.RepresenteeController.checkYourAnswers())
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
        Some(completeAnswers.name),
        Some(completeAnswers.id),
        completeAnswers.dateOfDeath,
        Some(sample[RepresenteeContactDetails]),
        true,
        true
      )

      "redirect to the enter name page" when {
        "redirect the page to confirm-person" when {
          def performAction(): Future[Result] =
            controller.checkYourAnswers()(FakeRequest())

          "the user has not yet confirmed the person " when {
            val requiredPreviousAnswers =
              IncompleteRepresenteeAnswers(
                Some(IndividualName("First", "Last")),
                Some(sample[RepresenteeNino]),
                Some(sample[DateOfDeath]),
                None,
                false,
                false
              )
            "redirect to confirm person page" in {
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithStartingNewDraftReturn(
                    requiredPreviousAnswers,
                    Left(PersonalRepresentative)
                  )._1
                )
              }
              checkIsRedirect(performAction(), routes.RepresenteeController.confirmPerson())
            }
          }
        }

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

        "redirect to the enter id page" when {

          "that question hasn't been answered yet" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionWithFillingOutReturn(allQuestionsAnswers.copy(id = None), Right(Capacitor))._1
              )
            }

            checkIsRedirect(performAction(), routes.RepresenteeController.enterId())
          }
        }

        "redirect to the enter date of death page" when {

          "that question hasn't been answered yet" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionWithFillingOutReturn(allQuestionsAnswers.copy(dateOfDeath = None), Left(PersonalRepresentative))._1
              )
            }

            checkIsRedirect(performAction(), routes.RepresenteeController.enterDateOfDeath())
          }
        }

        "redirect to the check contact details page" when {

          "there are no contact details in session" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionWithFillingOutReturn(
                  allQuestionsAnswers.copy(contactDetails = None),
                  Left(PersonalRepresentative)
                )._1
              )
            }

            checkIsRedirect(
              performAction(),
              routes.RepresenteeController.checkContactDetails()
            )
          }

          "the user has not confirmed the contact details" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionWithFillingOutReturn(
                  allQuestionsAnswers.copy(hasConfirmedContactDetails = false),
                  Right(Capacitor)
                )._1
              )
            }

            checkIsRedirect(
              performAction(),
              routes.RepresenteeController.checkContactDetails()
            )
          }

        }
      }
      "show the page" when {

        "the user has just answered all the questions and all updates are successful" in {
          val updatedAllQuestionsAnswers = allQuestionsAnswers.copy(
            contactDetails = Some(completeAnswers.contactDetails)
          )
          val (session, journey, draftReturn) =
            sessionWithFillingOutReturn(updatedAllQuestionsAnswers, Left(PersonalRepresentative))
          val newDraftReturn = draftReturn.copy(representeeAnswers = Some(completeAnswers))
          val updatedJourney = journey.copy(draftReturn            = newDraftReturn)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(
              newDraftReturn,
              journey.subscribedDetails.cgtReference,
              journey.agentReferenceNumber
            )(Right(()))
            mockStoreSession(session.copy(journeyStatus = Some(updatedJourney)))(Right(()))
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("representee.cya.title"),
            doc => {
              doc.select("#back").attr("href") shouldBe returns.triage.routes.CommonTriageQuestionsController
                .whoIsIndividualRepresenting()
                .url

              doc.select("#content > article > form").attr("action") shouldBe routes.RepresenteeController
                .checkYourAnswersSubmit()
                .url
            }
          )
        }

        "the user has already answered all the questions" in {
          val completeAnswers = sample[CompleteRepresenteeAnswers]
          val (session, _, _) = sessionWithFillingOutReturn(completeAnswers, Left(PersonalRepresentative))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)

          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("representee.cya.title"),
            doc => {
              doc.select("#back").attr("href") shouldBe returns.triage.routes.CommonTriageQuestionsController
                .whoIsIndividualRepresenting()
                .url

              doc.select("#content > article > form").attr("action") shouldBe routes.RepresenteeController
                .checkYourAnswersSubmit()
                .url
            }
          )
        }

      }

      "show an error page" when {

        "there is an error updating the draft return" in {
          val updatedAllQuestionsAnswers = allQuestionsAnswers.copy(
            contactDetails = Some(completeAnswers.contactDetails)
          )
          val (session, journey, draftReturn) =
            sessionWithFillingOutReturn(updatedAllQuestionsAnswers, Left(PersonalRepresentative))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(
              draftReturn.copy(
                representeeAnswers = Some(completeAnswers)
              ),
              journey.subscribedDetails.cgtReference,
              journey.agentReferenceNumber
            )(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction())
        }

        "there is an error updating the session" in {
          val updatedAllQuestionsAnswers = allQuestionsAnswers.copy(
            contactDetails = Some(completeAnswers.contactDetails)
          )
          val (session, journey, draftReturn) =
            sessionWithFillingOutReturn(updatedAllQuestionsAnswers, Left(PersonalRepresentative))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(
              draftReturn.copy(
                representeeAnswers = Some(completeAnswers)
              ),
              journey.subscribedDetails.cgtReference,
              journey.agentReferenceNumber
            )(Right(()))
            mockStoreSession(
              session.copy(
                journeyStatus = Some(
                  journey.copy(draftReturn =
                    draftReturn.copy(
                      representeeAnswers = Some(completeAnswers)
                    )
                  )
                )
              )
            )(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction())
        }

      }

    }

    "handling submits on the cya page" must {

      def performAction(): Future[Result] = controller.checkYourAnswersSubmit()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      val completeAnswers = sample[CompleteRepresenteeAnswers]

      val allQuestionsAnswers = CompleteRepresenteeAnswers(
        completeAnswers.name,
        completeAnswers.id,
        completeAnswers.dateOfDeath,
        sample[RepresenteeContactDetails]
      )

      "redirect to the how many properties page" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            sessionWithFillingOutReturn(allQuestionsAnswers, Right(Capacitor))._1
          )
        }

        checkIsRedirect(
          performAction(),
          returns.triage.routes.CommonTriageQuestionsController.howManyProperties()
        )
      }
    }

    "handling requests for check your answers page" must {
      "redirect the page to confirm-person" when {
        def performAction(): Future[Result] =
          controller.checkYourAnswers()(FakeRequest())
        "the user has not yet confirmed the person " when {
          val requiredPreviousAnswers =
            IncompleteRepresenteeAnswers(
              Some(IndividualName("First", "Last")),
              Some(sample[RepresenteeNino]),
              Some(sample[DateOfDeath]),
              None,
              false,
              false
            )
          "redirect to confirm person page" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionWithStartingNewDraftReturn(
                  requiredPreviousAnswers,
                  Left(PersonalRepresentative)
                )._1
              )
            }
            checkIsRedirect(performAction(), routes.RepresenteeController.confirmPerson())
          }
        }
      }

      "redirect the page to check-contact-details" when {
        def performAction(): Future[Result] =
          controller.checkYourAnswers()(FakeRequest())
        "the user has confirmed the person but contact details has not been entered " when {
          val requiredPreviousAnswers =
            IncompleteRepresenteeAnswers(
              Some(IndividualName("First", "Last")),
              Some(sample[RepresenteeNino]),
              Some(sample[DateOfDeath]),
              None,
              true,
              false
            )
          "redirect to check contact details page" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionWithStartingNewDraftReturn(
                  requiredPreviousAnswers,
                  Left(PersonalRepresentative)
                )._1
              )
            }
            checkIsRedirect(performAction(), routes.RepresenteeController.checkContactDetails())
          }
        }
      }
      "redirect the page to enter-id" when {
        def performAction(): Future[Result] =
          controller.checkYourAnswers()(FakeRequest())
        "the user has no id " when {
          val requiredPreviousAnswers =
            IncompleteRepresenteeAnswers(
              Some(IndividualName("First", "Last")),
              None,
              Some(sample[DateOfDeath]),
              None,
              false,
              false
            )
          "redirect to enter id page" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionWithStartingNewDraftReturn(
                  requiredPreviousAnswers,
                  Left(PersonalRepresentative)
                )._1
              )
            }
            checkIsRedirect(performAction(), routes.RepresenteeController.enterId())
          }
        }
      }
      "redirect the page to enter-name" when {
        def performAction(): Future[Result] =
          controller.checkYourAnswers()(FakeRequest())
        "the user has no name" when {
          val requiredPreviousAnswers =
            IncompleteRepresenteeAnswers(None, None, None, None, false, false)
          "redirect to enter name page" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionWithStartingNewDraftReturn(
                  requiredPreviousAnswers,
                  Left(PersonalRepresentative)
                )._1
              )
            }

            checkIsRedirect(performAction(), routes.RepresenteeController.enterName())

          }

        }
      }
    }
  }

  def testFormError(
    performAction: Seq[(String, String)] => Future[Result],
    pageTitleKey: String,
    currentSession: SessionData
  )(data: Seq[(String, String)], expectedErrorMessageKey: String): Unit = {
    inSequence {
      mockAuthWithNoRetrievals()
      mockGetSession(currentSession)
    }
    checkPageIsDisplayed(
      performAction(data),
      messageFromMessageKey(pageTitleKey), { doc =>
        doc.select("#error-summary-display > ul > li > a").text() shouldBe messageFromMessageKey(
          expectedErrorMessageKey
        )
        doc.title() should startWith("Error:")
      },
      BAD_REQUEST
    )
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
            sessionWithStartingNewDraftReturn(None, Some(Self), sample[SubscribedDetails])._1
          )
        }

        checkIsRedirect(performAction(), returns.routes.TaskListController.taskList())
      }

      "the user has not indicated who they are submitting the return for" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            sessionWithStartingNewDraftReturn(None, None, sample[SubscribedDetails])._1
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

        "handling requests to display the enter id page" in {
          test(controller.enterId())
        }

        "handling submitted ids" in {
          test(controller.enterIdSubmit())
        }

        "handling requests to display the enter date of death page" in {
          test(controller.enterDateOfDeath())
        }

        "handling submitted date of deaths" in {
          test(controller.enterDateOfDeathSubmit())
        }

        "handling requests to display the check contact details page" in {
          test(controller.checkContactDetails())
        }

        "handling submits on the check contact details page" in {
          test(controller.checkContactDetailsSubmit())
        }

        "handling requests to display the check your answers page" in {
          test(controller.checkYourAnswers())
        }

      }

    }

  }

}
