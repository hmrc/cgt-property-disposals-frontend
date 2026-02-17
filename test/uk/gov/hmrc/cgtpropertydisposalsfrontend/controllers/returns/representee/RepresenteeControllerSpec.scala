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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.representee

import cats.data.EitherT
import cats.instances.future._
import org.jsoup.nodes.Document
import org.scalatest.matchers.should.Matchers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.SampledScalaCheck
import play.api.http.Status.BAD_REQUEST
import play.api.i18n.{Lang, MessagesApi, MessagesImpl}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.libs.json.Reads
import play.api.mvc._
import play.api.test.CSRFTokenHelper._
import play.api.test.FakeRequest
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.ReturnsServiceSupport
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, BusinessPartnerRecordServiceSupport, ContactNameFormValidationTests, ControllerSpec, DateErrorScenarios, NameFormValidationTests, SessionSupport, returns}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, StartingNewDraftReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.UnsuccessfulNameMatchAttempts.NameMatchDetails.IndividualRepresenteeNameMatchDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address.UkAddress
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.DraftReturnGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.JourneyStatusGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.NameGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.RepresenteeAnswersGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.ReturnGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.SubscribedDetailsGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.TriageQuestionsGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.{CgtReference, GGCredId, NINO, SAUTR}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.{ContactName, IndividualName}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.SubscribedDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.IndividualUserType._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.MultipleDisposalsTriageAnswers.CompleteMultipleDisposalsTriageAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.RepresenteeAnswers.{CompleteRepresenteeAnswers, IncompleteRepresenteeAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.RepresenteeReferenceId.{NoReferenceId, RepresenteeCgtReference, RepresenteeNino, RepresenteeSautr}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.{CompleteSingleDisposalTriageAnswers, IncompleteSingleDisposalTriageAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, NameMatchServiceError, SessionData, TimeUtils, UnsuccessfulNameMatchAttempts, UserType}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.NameMatchRetryService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService
import uk.gov.hmrc.http.HeaderCarrier

import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.UnsuccessfulNameMatchAttempts._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.UnsuccessfulNameMatchAttempts.NameMatchDetails._

import java.time.LocalDate
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class RepresenteeControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with SampledScalaCheck
    with RedirectToStartBehaviour
    with ReturnsServiceSupport
    with BusinessPartnerRecordServiceSupport
    with NameFormValidationTests
    with ContactNameFormValidationTests {

  val mockNameMatchRetryService: NameMatchRetryService = mock[NameMatchRetryService]

  protected override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore),
      bind[ReturnsService].toInstance(mockReturnsService),
      bind[NameMatchRetryService].toInstance(mockNameMatchRetryService)
    )

  private lazy val controller = instanceOf[RepresenteeController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  def sessionWithStartingNewDraftReturn(
    answers: Option[RepresenteeAnswers],
    individualUserType: Option[IndividualUserType],
    subscribedDetails: SubscribedDetails
  ): (SessionData, StartingNewDraftReturn) = {
    val journey = sample[StartingNewDraftReturn].copy(
      subscribedDetails = subscribedDetails,
      representeeAnswers = answers,
      newReturnTriageAnswers = Right(
        sample[CompleteSingleDisposalTriageAnswers]
          .copy(individualUserType = individualUserType)
      )
    )
    val session = SessionData.empty.copy(journeyStatus = Some(journey))
    session -> journey
  }

  def sessionWithStartingNewDraftReturn(
    answers: RepresenteeAnswers,
    representativeType: RepresentativeType,
    subscribedDetails: SubscribedDetails = sample[SubscribedDetails]
  ): (SessionData, StartingNewDraftReturn) =
    sessionWithStartingNewDraftReturn(
      Some(answers),
      Some(representativeType),
      subscribedDetails
    )

  def sessionWithFillingOutReturn(
    answers: Option[RepresenteeAnswers],
    individualUserType: Option[IndividualUserType],
    subscribedDetails: SubscribedDetails,
    isAmend: Boolean
  ): (SessionData, FillingOutReturn, DraftMultipleDisposalsReturn) = {
    val draftReturn = sample[DraftMultipleDisposalsReturn].copy(
      representeeAnswers = answers,
      triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
        .copy(individualUserType = individualUserType)
    )
    val journey     = sample[FillingOutReturn]
      .copy(
        draftReturn = draftReturn,
        subscribedDetails = subscribedDetails,
        amendReturnData = if (isAmend) Some(sample[AmendReturnData]) else None
      )

    val session = SessionData.empty.copy(journeyStatus = Some(journey))
    (session, journey, draftReturn)
  }

  def sessionWithFillingOutReturn(
    answers: RepresenteeAnswers,
    representativeType: RepresentativeType,
    subscribedDetails: SubscribedDetails = sample[SubscribedDetails],
    isAmend: Boolean = false
  ): (SessionData, FillingOutReturn, DraftMultipleDisposalsReturn) =
    sessionWithFillingOutReturn(
      Some(answers),
      Some(representativeType),
      subscribedDetails,
      isAmend
    )

  private def mockGetPreviousNameMatchAttempts(ggCredId: GGCredId)(
    result: Either[NameMatchServiceError[IndividualRepresenteeNameMatchDetails], Option[
      UnsuccessfulNameMatchAttempts[IndividualRepresenteeNameMatchDetails]
    ]]
  ) =
    (
      mockNameMatchRetryService
        .getNumberOfUnsuccessfulAttempts[IndividualRepresenteeNameMatchDetails](_: GGCredId)(using
          _: Reads[IndividualRepresenteeNameMatchDetails],
          _: HeaderCarrier,
          _: Request[?]
        )
      )
      .expects(ggCredId, *, *, *)
      .returning(EitherT.fromEither[Future](result))

  private def mockNameMatch(
    details: IndividualRepresenteeNameMatchDetails,
    ggCredId: GGCredId,
    previousNameMatchAttempts: Option[UnsuccessfulNameMatchAttempts[IndividualRepresenteeNameMatchDetails]],
    lang: Lang
  )(
    result: Either[NameMatchServiceError[IndividualRepresenteeNameMatchDetails], RepresenteeReferenceId]
  ) =
    (
      mockNameMatchRetryService
        .attemptNameMatch(
          _: IndividualRepresenteeNameMatchDetails,
          _: GGCredId,
          _: Option[UnsuccessfulNameMatchAttempts[IndividualRepresenteeNameMatchDetails]],
          _: Lang
        )(using
          _: HeaderCarrier,
          _: Request[?]
        )
      )
      .expects(
        details,
        ggCredId,
        previousNameMatchAttempts,
        lang,
        *,
        *
      )
      .returning(EitherT.fromEither[Future](result))

  "RepresenteeController" when {

    "handling requests to display the change contact name page" must {

      def performAction(): Future[Result] =
        controller.changeContactName()(FakeRequest())

      behave like nonCapacitorOrPersonalRepBehaviour(() => performAction())

      "redirect to the check your answers page" when {

        "there are no contact details in session" in {
          val session =
            sessionWithStartingNewDraftReturn(
              sample[IncompleteRepresenteeAnswers].copy(contactDetails = None),
              Capacitor
            )._1

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkIsRedirect(
            performAction(),
            routes.RepresenteeController.checkYourAnswers()
          )
        }

      }

      "display the page" when {

        def test(
          sessionData: SessionData,
          expectedTitleKey: String,
          expectedBackLink: Call,
          expectReturnToSummaryLink: Boolean
        ): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionData)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(expectedTitleKey),
            { doc =>
              doc.select("#back, .govuk-back-link").attr("href") shouldBe expectedBackLink.url
              doc
                .select("#content > article > form, #main-content form")
                .attr("action")                                  shouldBe routes.RepresenteeController
                .changeContactName()
                .url
              doc.select("#returnToSummaryLink").text()          shouldBe (
                if (expectReturnToSummaryLink) messageFromMessageKey("returns.return-to-summary-link") else ""
              )
            }
          )
        }
        "the user has started and saved a draft return" in {
          val session =
            sessionWithFillingOutReturn(
              sample[CompleteRepresenteeAnswers],
              Capacitor
            )._1

          test(
            session,
            "representeeContactName.change.title",
            routes.RepresenteeController.checkYourAnswers(),
            expectReturnToSummaryLink = true
          )
        }

        "the user has not saved a draft return yet" in {
          val (session, _) =
            sessionWithStartingNewDraftReturn(
              sample[IncompleteRepresenteeAnswers]
                .copy(contactDetails = Some(sample[RepresenteeContactDetails])),
              Capacitor
            )

          test(
            session,
            "representeeContactName.change.title",
            routes.RepresenteeController.checkContactDetails(),
            expectReturnToSummaryLink = false
          )
        }
      }
    }

    "handling submitting change contact name" must {

      def performAction: Seq[(String, String)] => Future[Result] =
        data =>
          controller.changeContactNameSubmit()(
            FakeRequest().withFormUrlEncodedBody(data*).withCSRFToken.withMethod("POST")
          )

      def formDataForContactName(
        contactName: ContactName
      ): Seq[(String, String)] =
        Seq("contactName" -> contactName.value)

      behave like nonCapacitorOrPersonalRepBehaviour(() => performAction(Seq.empty))

      behave like contactNameFormValidationTests(
        performAction,
        () =>
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithFillingOutReturn(
                sample[CompleteRepresenteeAnswers],
                Capacitor
              )._1
            )
          }
      )

      "redirect to check your answers page" when {

        "the user enters a valid contact name" when {

          "the section is incomplete" in {
            val contactDetails               = sample[RepresenteeContactDetails]
            val incompleteRepresenteeAnswers =
              sample[IncompleteRepresenteeAnswers]
                .copy(contactDetails = Some(contactDetails))

            val (session, journey, draftReturn) =
              sessionWithFillingOutReturn(
                incompleteRepresenteeAnswers,
                Capacitor
              )

            val newContactName            = ContactName("First Last")
            val updatedContactDetails     =
              contactDetails.copy(contactName = newContactName)
            val updatedRepresenteeAnswers = incompleteRepresenteeAnswers
              .copy(
                contactDetails = Some(updatedContactDetails),
                hasConfirmedContactDetails = false
              )
            val updatedDraftReturn        = draftReturn.copy(representeeAnswers = Some(updatedRepresenteeAnswers))
            val updatedJourney            = journey.copy(draftReturn = updatedDraftReturn)
            val updatedSession            = session.copy(journeyStatus = Some(updatedJourney))

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockStoreDraftReturn(updatedJourney)(Right(()))
              mockStoreSession(updatedSession)(Right(()))
            }

            checkIsRedirect(
              performAction(formDataForContactName(newContactName)),
              routes.RepresenteeController.checkYourAnswers()
            )
          }

          "the section is complete" in {
            val answers               = sample[CompleteRepresenteeAnswers]
            val (session, journey)    =
              sessionWithStartingNewDraftReturn(answers, Capacitor)
            val newContactName        = ContactName("First Last")
            val newRepresenteeAnswers =
              IncompleteRepresenteeAnswers(
                Some(answers.name),
                Some(answers.id),
                answers.dateOfDeath,
                Some(answers.contactDetails.copy(contactName = newContactName)),
                hasConfirmedPerson = true,
                hasConfirmedContactDetails = false,
                Some(answers.isFirstReturn)
              )

            val updatedSession =
              session.copy(journeyStatus =
                Some(
                  journey.copy(representeeAnswers = Some(newRepresenteeAnswers))
                )
              )

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockStoreSession(updatedSession)(Right(()))
            }

            checkIsRedirect(
              performAction(formDataForContactName(newContactName)),
              routes.RepresenteeController.checkYourAnswers()
            )
          }

        }
      }
    }

    "handling requests to display the enter name page" must {

      def performAction(): Future[Result] =
        controller.enterName()(FakeRequest())

      behave like redirectToStartBehaviour(() => performAction())

      behave like nonCapacitorOrPersonalRepBehaviour(() => performAction())

      behave like tooManyNameMatchAttemptsBehaviour(() => performAction())

      behave like noIsFirstReturnAnswerBehaviour(() => performAction())

      "display the page" when {

        def test(
          sessionData: SessionData,
          ggCredId: GGCredId,
          expectedTitleKey: String,
          expectedBackLink: Call,
          expectReturnToSummaryLink: Boolean,
          expectedPrepopulatedValue: Option[IndividualName]
        ): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionData)
            mockGetPreviousNameMatchAttempts(ggCredId)(Right(None))
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(expectedTitleKey),
            { doc =>
              doc.select("#back, .govuk-back-link").attr("href") shouldBe expectedBackLink.url
              doc
                .select("#content > article > form, #main-content form")
                .attr("action")                                  shouldBe routes.RepresenteeController
                .enterNameSubmit()
                .url
              doc.select("#returnToSummaryLink").text()          shouldBe (
                if (expectReturnToSummaryLink) messageFromMessageKey("returns.return-to-summary-link") else ""
              )
              expectedPrepopulatedValue.foreach { name =>
                doc
                  .select("#representeeFirstName")
                  .attr("value") shouldBe name.firstName
                doc
                  .select("#representeeLastName")
                  .attr("value") shouldBe name.lastName
              }
            }
          )

        }

        "the user is starting a draft return and" when {

          "the user is a capacitor" in {
            val (session, journey) =
              sessionWithStartingNewDraftReturn(
                IncompleteRepresenteeAnswers.empty.copy(isFirstReturn = Some(true)),
                Capacitor
              )

            test(
              session,
              journey.ggCredId,
              "representee.enterName.capacitor.title",
              routes.RepresenteeController.isFirstReturn(),
              expectReturnToSummaryLink = false,
              None
            )
          }

          "the user is a personal representative" in {
            val name               = sample[IndividualName]
            val (session, journey) =
              sessionWithStartingNewDraftReturn(
                IncompleteRepresenteeAnswers.empty.copy(name = Some(name), isFirstReturn = Some(false)),
                PersonalRepresentative
              )

            test(
              session,
              journey.ggCredId,
              "representee.enterName.personalRep.title",
              routes.RepresenteeController.isFirstReturn(),
              expectReturnToSummaryLink = false,
              Some(name)
            )

          }

          "the section is complete" in {
            val answers            = sample[CompleteRepresenteeAnswers]
            val (session, journey) = sessionWithStartingNewDraftReturn(
              answers,
              PersonalRepresentative
            )
            test(
              session,
              journey.ggCredId,
              "representee.enterName.personalRep.title",
              routes.RepresenteeController.checkYourAnswers(),
              expectReturnToSummaryLink = false,
              Some(answers.name)
            )
          }

        }

        "the user has already started a draft return and" when {

          "the user is a capacitor" in {
            val (session, journey, _) =
              sessionWithFillingOutReturn(
                IncompleteRepresenteeAnswers.empty.copy(isFirstReturn = Some(true)),
                Capacitor
              )

            test(
              session,
              journey.ggCredId,
              "representee.enterName.capacitor.title",
              routes.RepresenteeController.isFirstReturn(),
              expectReturnToSummaryLink = true,
              None
            )

          }

          "the user is a personal representative" in {
            val name                  = sample[IndividualName]
            val (session, journey, _) =
              sessionWithFillingOutReturn(
                IncompleteRepresenteeAnswers.empty.copy(name = Some(name), isFirstReturn = Some(false)),
                PersonalRepresentative
              )

            test(
              session,
              journey.ggCredId,
              "representee.enterName.personalRep.title",
              routes.RepresenteeController.isFirstReturn(),
              expectReturnToSummaryLink = true,
              Some(name)
            )
          }

          "the section is complete" in {
            val answers               = sample[CompleteRepresenteeAnswers]
            val (session, journey, _) =
              sessionWithFillingOutReturn(answers, PersonalRepresentative)

            test(
              session,
              journey.ggCredId,
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
        controller.enterNameSubmit()(
          FakeRequest().withFormUrlEncodedBody(formData*).withMethod("POST")
        )

      val firstNameKey = "representeeFirstName"

      val lastNameKey = "representeeLastName"

      def formData(individualName: IndividualName): Seq[(String, String)] =
        Seq(
          firstNameKey -> individualName.firstName,
          lastNameKey  -> individualName.lastName
        )

      behave like redirectToStartBehaviour(() => performAction(Seq.empty))

      behave like nonCapacitorOrPersonalRepBehaviour(() => performAction(Seq.empty))

      behave like tooManyNameMatchAttemptsBehaviour(() => performAction(Seq.empty))

      behave like noIsFirstReturnAnswerBehaviour(() => performAction(Seq.empty))

      {
        val (session, journey, _) =
          sessionWithFillingOutReturn(
            IncompleteRepresenteeAnswers.empty.copy(isFirstReturn = Some(true)),
            Capacitor
          )

        behave like nameFormValidationTests(
          performAction,
          () =>
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockGetPreviousNameMatchAttempts(journey.ggCredId)(Right(None))
            },
          firstNameKey,
          lastNameKey
        )
      }

      "show an error page" when {

        val newName                         = IndividualName("Max", "Cax")
        val answers                         = sample[CompleteRepresenteeAnswers]
        val (session, journey, draftReturn) =
          sessionWithFillingOutReturn(
            answers,
            Capacitor
          )

        val newDraftReturn =
          DraftSingleDisposalReturn.newDraftReturn(
            draftReturn.id,
            IncompleteSingleDisposalTriageAnswers.empty
              .copy(individualUserType = Some(Capacitor)),
            representeeAnswers = Some(
              IncompleteRepresenteeAnswers.empty.copy(
                name = Some(newName),
                isFirstReturn = Some(answers.isFirstReturn)
              )
            )
          )

        val newJourney = journey.copy(draftReturn = newDraftReturn)

        "there is an error updating the draft return" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockGetPreviousNameMatchAttempts(journey.ggCredId)(Right(None))
            mockStoreDraftReturn(newJourney)(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction(formData(newName)))
        }

        "there is an error updating the session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockGetPreviousNameMatchAttempts(journey.ggCredId)(Right(None))
            mockStoreDraftReturn(newJourney)(Right(()))
            mockStoreSession(session.copy(journeyStatus = Some(newJourney)))(
              Left(Error(""))
            )
          }

          checkIsTechnicalErrorPage(performAction(formData(newName)))
        }

      }

      "redirect to the check your answers page" when {

        val newName = IndividualName("Lars", "Fars")

        "the user hasn't started a draft return yet and" when {

          "the section is incomplete" in {
            val (session, journey) =
              sessionWithStartingNewDraftReturn(
                IncompleteRepresenteeAnswers.empty.copy(isFirstReturn = Some(false)),
                PersonalRepresentative
              )
            val newJourney         = journey.copy(
              representeeAnswers = Some(
                IncompleteRepresenteeAnswers.empty.copy(
                  name = Some(newName),
                  isFirstReturn = Some(false)
                )
              )
            )

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockGetPreviousNameMatchAttempts(journey.ggCredId)(Right(None))
              mockStoreSession(session.copy(journeyStatus = Some(newJourney)))(
                Right(())
              )
            }

            checkIsRedirect(
              performAction(formData(newName)),
              routes.RepresenteeController.checkYourAnswers()
            )
          }

          "the section is complete" in {
            val answers            = sample[CompleteRepresenteeAnswers]
            val (session, journey) =
              sessionWithStartingNewDraftReturn(
                answers,
                PersonalRepresentative
              )
            val newJourney         = journey.copy(
              representeeAnswers = Some(
                IncompleteRepresenteeAnswers.empty.copy(
                  name = Some(newName),
                  isFirstReturn = Some(answers.isFirstReturn)
                )
              )
            )

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockGetPreviousNameMatchAttempts(journey.ggCredId)(Right(None))
              mockStoreSession(session.copy(journeyStatus = Some(newJourney)))(
                Right(())
              )
            }

            checkIsRedirect(
              performAction(formData(newName)),
              routes.RepresenteeController.checkYourAnswers()
            )
          }

        }

        "the user has started a draft return and" when {

          "the section is incomplete" in {
            val (session, journey, draftReturn) =
              sessionWithFillingOutReturn(
                IncompleteRepresenteeAnswers.empty.copy(isFirstReturn = Some(true)),
                Capacitor
              )

            val newDraftReturn =
              DraftSingleDisposalReturn.newDraftReturn(
                draftReturn.id,
                IncompleteSingleDisposalTriageAnswers.empty
                  .copy(individualUserType = Some(Capacitor)),
                representeeAnswers = Some(
                  IncompleteRepresenteeAnswers.empty.copy(
                    name = Some(newName),
                    isFirstReturn = Some(true)
                  )
                )
              )

            val newJourney = journey.copy(draftReturn = newDraftReturn)

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockGetPreviousNameMatchAttempts(journey.ggCredId)(Right(None))
              mockStoreDraftReturn(newJourney)(Right(()))
              mockStoreSession(session.copy(journeyStatus = Some(newJourney)))(
                Right(())
              )
            }

            checkIsRedirect(
              performAction(formData(newName)),
              routes.RepresenteeController.checkYourAnswers()
            )
          }

          "the section is complete" in {
            val answers                         = sample[CompleteRepresenteeAnswers]
            val (session, journey, draftReturn) =
              sessionWithFillingOutReturn(
                answers,
                Capacitor,
                isAmend = true
              )

            val newDraftReturn =
              DraftSingleDisposalReturn.newDraftReturn(
                draftReturn.id,
                IncompleteSingleDisposalTriageAnswers.empty
                  .copy(individualUserType = Some(Capacitor)),
                representeeAnswers = Some(
                  IncompleteRepresenteeAnswers.empty.copy(
                    name = Some(newName),
                    isFirstReturn = Some(answers.isFirstReturn)
                  )
                )
              )

            val newJourney = journey.copy(draftReturn = newDraftReturn).withForceDisplayGainOrLossAfterReliefsForAmends

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockGetPreviousNameMatchAttempts(journey.ggCredId)(Right(None))
              mockStoreDraftReturn(newJourney)(Right(()))
              mockStoreSession(session.copy(journeyStatus = Some(newJourney)))(
                Right(())
              )
            }

            checkIsRedirect(
              performAction(formData(newName)),
              routes.RepresenteeController.checkYourAnswers()
            )

          }

        }

      }

    }

    "handling requests to display the enter date of death page" must {

      def performAction(): Future[Result] =
        controller.enterDateOfDeath()(FakeRequest())

      behave like redirectToStartBehaviour(() => performAction())

      behave like nonCapacitorOrPersonalRepBehaviour(() => performAction())

      behave like noIsFirstReturnAnswerBehaviour(() => performAction())

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
            messageFromMessageKey(expectedTitleKey),
            { doc =>
              doc.select("#back, .govuk-back-link").attr("href") shouldBe expectedBackLink.url
              doc
                .select("#content > article > form, #main-content form")
                .attr("action")                                  shouldBe routes.RepresenteeController
                .enterDateOfDeathSubmit()
                .url
              doc.select("#returnToSummaryLink").text()          shouldBe (
                if (expectReturnToSummaryLink) messageFromMessageKey("returns.return-to-summary-link") else ""
              )
              expectedPrepopulatedValue.foreach { date =>
                doc
                  .select("#dateOfDeath-day")
                  .attr("value") shouldBe date.value.getDayOfMonth.toString
                doc
                  .select("#dateOfDeath-month")
                  .attr("value") shouldBe date.value.getMonthValue.toString
                doc
                  .select("#dateOfDeath-year")
                  .attr("value") shouldBe date.value.getYear.toString
              }
            }
          )

        }

        "the user is starting a draft return and" when {

          "the user is a personal representative" in {
            val date = DateOfDeath(LocalDate.now())
            test(
              sessionWithStartingNewDraftReturn(
                IncompleteRepresenteeAnswers.empty
                  .copy(dateOfDeath = Some(date), isFirstReturn = Some(true)),
                PersonalRepresentative
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
              sessionWithStartingNewDraftReturn(
                answers,
                PersonalRepresentative
              )._1,
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
                IncompleteRepresenteeAnswers.empty
                  .copy(dateOfDeath = Some(date), isFirstReturn = Some(false)),
                PersonalRepresentative
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
              sessionWithFillingOutReturn(
                answers,
                PersonalRepresentative
              )._1,
              "dateOfDeath.title",
              routes.RepresenteeController.checkYourAnswers(),
              expectReturnToSummaryLink = true,
              answers.dateOfDeath
            )
          }

        }

      }

      "redirect to check your answers page" when {

        "the representative type is capacitor" in {
          val answers = sample[CompleteRepresenteeAnswers]
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithFillingOutReturn(answers, Capacitor)._1
            )
          }

          checkIsRedirect(
            performAction(),
            routes.RepresenteeController.checkYourAnswers()
          )

        }

      }

    }

    "handling submitted date of death" must {

      def performAction(formData: Seq[(String, String)]): Future[Result] =
        controller.enterDateOfDeathSubmit()(
          FakeRequest().withFormUrlEncodedBody(formData*).withMethod("POST")
        )

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

      behave like noIsFirstReturnAnswerBehaviour(() => performAction(Seq.empty))

      "show an error page" when {

        val oldAnswers                      = sample[CompleteRepresenteeAnswers]
          .copy(dateOfDeath = Some(DateOfDeath(LocalDate.now())))
        val newDate                         = DateOfDeath(LocalDate.now().minusDays(1))
        val (session, journey, draftReturn) =
          sessionWithFillingOutReturn(oldAnswers, PersonalRepresentative)

        val newDraftReturn =
          DraftSingleDisposalReturn.newDraftReturn(
            draftReturn.id,
            IncompleteSingleDisposalTriageAnswers.empty
              .copy(individualUserType = Some(PersonalRepresentative)),
            representeeAnswers = Some(
              IncompleteRepresenteeAnswers.empty.copy(
                name = Some(oldAnswers.name),
                dateOfDeath = Some(newDate),
                isFirstReturn = Some(oldAnswers.isFirstReturn)
              )
            )
          )

        val newJourney = journey.copy(draftReturn = newDraftReturn)

        "there is an error updating the draft return" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(newJourney)(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction(formData(newDate)))
        }

        "there is an error updating the session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(newJourney)(Right(()))
            mockStoreSession(session.copy(journeyStatus = Some(newJourney)))(
              Left(Error(""))
            )
          }
          checkIsTechnicalErrorPage(performAction(formData(newDate)))
        }

        "the date entered is invalid" in {

          val session = sessionWithFillingOutReturn(
            IncompleteRepresenteeAnswers.empty
              .copy(name = Some(sample[IndividualName]), isFirstReturn = Some(true)),
            PersonalRepresentative
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
            formData(DateOfDeath(LocalDate.now().plusYears(2L).plusDays(1L))),
            "dateOfDeath.error.tooFarInFuture"
          )
        }

      }

      "redirect to the check your answers page" when {

        val dateOfDeath = DateOfDeath(LocalDate.now())

        "the user hasn't started a draft return yet and" when {

          "the section is incomplete" in {
            val name               = sample[IndividualName]
            val (session, journey) =
              sessionWithStartingNewDraftReturn(
                IncompleteRepresenteeAnswers.empty.copy(name = Some(name), isFirstReturn = Some(true)),
                PersonalRepresentative
              )
            val newJourney         = journey.copy(
              representeeAnswers = Some(
                IncompleteRepresenteeAnswers.empty.copy(
                  name = Some(name),
                  dateOfDeath = Some(dateOfDeath),
                  isFirstReturn = Some(true)
                )
              )
            )

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockStoreSession(session.copy(journeyStatus = Some(newJourney)))(
                Right(())
              )
            }

            checkIsRedirect(
              performAction(formData(dateOfDeath)),
              routes.RepresenteeController.checkYourAnswers()
            )
          }

          "the section is complete" in {
            val data               = sample[CompleteRepresenteeAnswers]
              .copy(dateOfDeath = Some(dateOfDeath))
            val newDate            = DateOfDeath(LocalDate.now().minusDays(1))
            val (session, journey) =
              sessionWithStartingNewDraftReturn(
                data,
                PersonalRepresentative
              )
            val newJourney         = journey.copy(
              representeeAnswers = Some(
                IncompleteRepresenteeAnswers.empty.copy(
                  name = Some(data.name),
                  dateOfDeath = Some(newDate),
                  isFirstReturn = Some(data.isFirstReturn)
                )
              )
            )

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockStoreSession(session.copy(journeyStatus = Some(newJourney)))(
                Right(())
              )
            }

            checkIsRedirect(
              performAction(formData(newDate)),
              routes.RepresenteeController.checkYourAnswers()
            )
          }

        }

        "the user has started a draft return and" when {

          "the section is incomplete" in {
            val name                            = sample[IndividualName]
            val (session, journey, draftReturn) =
              sessionWithFillingOutReturn(
                IncompleteRepresenteeAnswers.empty.copy(name = Some(name), isFirstReturn = Some(false)),
                PersonalRepresentative,
                isAmend = true
              )

            val newDraftReturn =
              DraftSingleDisposalReturn.newDraftReturn(
                draftReturn.id,
                IncompleteSingleDisposalTriageAnswers.empty
                  .copy(individualUserType = Some(PersonalRepresentative)),
                representeeAnswers = Some(
                  IncompleteRepresenteeAnswers.empty.copy(
                    name = Some(name),
                    dateOfDeath = Some(dateOfDeath),
                    isFirstReturn = Some(false)
                  )
                )
              )

            val newJourney = journey.copy(draftReturn = newDraftReturn).withForceDisplayGainOrLossAfterReliefsForAmends

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockStoreDraftReturn(newJourney)(Right(()))
              mockStoreSession(session.copy(journeyStatus = Some(newJourney)))(
                Right(())
              )
            }

            checkIsRedirect(
              performAction(formData(dateOfDeath)),
              routes.RepresenteeController.checkYourAnswers()
            )
          }

          "the section is complete" in {
            val oldData                         = sample[CompleteRepresenteeAnswers]
            val (session, journey, draftReturn) =
              sessionWithFillingOutReturn(oldData, PersonalRepresentative)

            val newDraftReturn =
              DraftSingleDisposalReturn.newDraftReturn(
                draftReturn.id,
                IncompleteSingleDisposalTriageAnswers.empty
                  .copy(individualUserType = Some(PersonalRepresentative)),
                representeeAnswers = Some(
                  IncompleteRepresenteeAnswers.empty.copy(
                    name = Some(oldData.name),
                    dateOfDeath = Some(dateOfDeath),
                    isFirstReturn = Some(oldData.isFirstReturn)
                  )
                )
              )
            val newJourney     = journey.copy(draftReturn = newDraftReturn)

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockStoreDraftReturn(newJourney)(Right(()))
              mockStoreSession(session.copy(journeyStatus = Some(newJourney)))(
                Right(())
              )
            }

            checkIsRedirect(
              performAction(formData(dateOfDeath)),
              routes.RepresenteeController.checkYourAnswers()
            )

          }

        }

      }

    }

    "handling requests to display the enter id page" must {

      def performAction(): Future[Result] = controller.enterId()(FakeRequest())

      behave like redirectToStartBehaviour(() => performAction())

      behave like nonCapacitorOrPersonalRepBehaviour(() => performAction())

      behave like tooManyNameMatchAttemptsBehaviour(() => performAction())

      behave like noIsFirstReturnAnswerBehaviour(() => performAction())

      "display the page" when {

        def test(
          sessionData: SessionData,
          ggCredId: GGCredId,
          expectedBackLink: Call,
          expectReturnToSummaryLink: Boolean,
          expectedPrepopulatedValue: Option[RepresenteeReferenceId]
        ): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionData)
            mockGetPreviousNameMatchAttempts(ggCredId)(Right(None))
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("representeeReferenceIdType.title"),
            { doc =>
              doc.select("#back, .govuk-back-link").attr("href") shouldBe expectedBackLink.url
              doc
                .select("#content > article > form, #main-content form")
                .attr("action")                                  shouldBe routes.RepresenteeController
                .enterIdSubmit()
                .url
              doc.select("#returnToSummaryLink").text()          shouldBe (
                if (expectReturnToSummaryLink) messageFromMessageKey("returns.return-to-summary-link") else ""
              )
              expectedPrepopulatedValue.foreach {

                case RepresenteeCgtReference(cgtRef) =>
                  doc
                    .select("#representeeCgtRef")
                    .attr("value") shouldBe cgtRef.value
                case RepresenteeNino(nino)           =>
                  doc
                    .select("#representeeNino")
                    .attr("value") shouldBe nino.value
                case RepresenteeSautr(sautr)         =>
                  doc
                    .select("#representeeSautr")
                    .attr("value") shouldBe sautr.value
                case NoReferenceId                   =>
                  doc
                    .select("#representeeReferenceIdType-3")
                    .hasAttr("checked")
              }
            }
          )

        }

        "the user is starting a draft return and" when {

          val requiredPreviousAnswers =
            IncompleteRepresenteeAnswers.empty
              .copy(name = Some(sample[IndividualName]), isFirstReturn = Some(true))

          "the user has not selected an option before" in {
            List(
              PersonalRepresentative -> routes.RepresenteeController
                .enterDateOfDeath(),
              Capacitor              -> routes.RepresenteeController.enterName()
            ) foreach { case (representativeType, redirect) =>
              val (session, journey) =
                sessionWithStartingNewDraftReturn(
                  requiredPreviousAnswers,
                  representativeType
                )

              test(
                session,
                journey.ggCredId,
                redirect,
                expectReturnToSummaryLink = false,
                None
              )
            }
          }

          "the user has previously submitted a cgt reference" in {
            List(
              PersonalRepresentative -> routes.RepresenteeController
                .enterDateOfDeath(),
              Capacitor              -> routes.RepresenteeController.enterName()
            ) foreach { case (representativeType, redirect) =>
              val cgtReference       = sample[RepresenteeCgtReference]
              val (session, journey) =
                sessionWithStartingNewDraftReturn(
                  requiredPreviousAnswers.copy(id = Some(cgtReference)),
                  representativeType
                )

              test(
                session,
                journey.ggCredId,
                redirect,
                expectReturnToSummaryLink = false,
                Some(cgtReference)
              )
            }
          }

          "the user has previously submitted a nino" in {
            val nino               = sample[RepresenteeNino]
            val (session, journey) = sessionWithStartingNewDraftReturn(
              requiredPreviousAnswers.copy(id = Some(nino)),
              PersonalRepresentative
            )

            test(
              session,
              journey.ggCredId,
              routes.RepresenteeController.enterDateOfDeath(),
              expectReturnToSummaryLink = false,
              Some(nino)
            )

          }

          "the user has previously submitted an sautr" in {
            val sautr              = sample[RepresenteeSautr]
            val (session, journey) = sessionWithStartingNewDraftReturn(
              requiredPreviousAnswers.copy(id = Some(sautr)),
              PersonalRepresentative
            )

            test(
              session,
              journey.ggCredId,
              routes.RepresenteeController.enterDateOfDeath(),
              expectReturnToSummaryLink = false,
              Some(sautr)
            )

          }

          "the user has previously submitted no reference" in {
            val (session, journey) =
              sessionWithStartingNewDraftReturn(
                requiredPreviousAnswers.copy(id = Some(NoReferenceId)),
                Capacitor
              )

            test(
              session,
              journey.ggCredId,
              routes.RepresenteeController.enterName(),
              expectReturnToSummaryLink = false,
              Some(NoReferenceId)
            )
          }

          "the section is complete" in {
            val answers            = sample[CompleteRepresenteeAnswers]
            val (session, journey) =
              sessionWithStartingNewDraftReturn(answers, Capacitor)

            test(
              session,
              journey.ggCredId,
              routes.RepresenteeController.checkYourAnswers(),
              expectReturnToSummaryLink = false,
              Some(answers.id)
            )
          }

        }

        "the user has already started a draft return and" when {

          "the journey is incomplete" in {
            val cgtReference          = sample[RepresenteeCgtReference]
            val (session, journey, _) =
              sessionWithFillingOutReturn(
                IncompleteRepresenteeAnswers.empty
                  .copy(
                    name = Some(sample[IndividualName]),
                    id = Some(cgtReference),
                    isFirstReturn = Some(false)
                  ),
                PersonalRepresentative
              )

            test(
              session,
              journey.ggCredId,
              routes.RepresenteeController.enterDateOfDeath(),
              expectReturnToSummaryLink = true,
              Some(cgtReference)
            )
          }

          "the section is complete" in {
            val answers               = sample[CompleteRepresenteeAnswers]
            val (session, journey, _) =
              sessionWithFillingOutReturn(answers, PersonalRepresentative)

            test(
              session,
              journey.ggCredId,
              routes.RepresenteeController.checkYourAnswers(),
              expectReturnToSummaryLink = true,
              Some(answers.id)
            )
          }

        }

      }

    }

    "handling requests to display the confirm person page" must {
      def performAction(): Future[Result] =
        controller.confirmPerson()(FakeRequest())

      behave like redirectToStartBehaviour(() => performAction())

      behave like nonCapacitorOrPersonalRepBehaviour(() => performAction())

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
            doc.select("#back, .govuk-back-link").attr("href") shouldBe expectedBackLink.url
            doc
              .select("#main-content .govuk-summary-list__key")
              .text                                            shouldBe messageFromMessageKey(
              "representeeConfirmPerson.summaryLine1"
            )
            doc
              .select("#main-content .govuk-summary-list__value")
              .text                                            shouldBe s"${expectedName.firstName} ${expectedName.lastName}"
            if (isCgtRow) {
              doc
                .select("#account-question")
                .text shouldBe messageFromMessageKey(
                "representeeConfirmPerson.summaryLine2.cgtReferenceId"
              )
            } else {
              ()
            }
            doc
              .select("#content > article > form, #main-content form")
              .attr("action")                                  shouldBe routes.RepresenteeController
              .confirmPersonSubmit()
              .url
            doc
              .select("#main-content form legend")
              .text()                                          shouldBe messageFromMessageKey(
              "representeeConfirmPerson.formTitle"
            )
          }
        )
      }

      "display the page" when {

        "the user has name and id " when {
          val name                    = IndividualName("First", "Last")
          val requiredPreviousAnswers =
            IncompleteRepresenteeAnswers(
              Some(name),
              Some(RepresenteeNino(NINO("AB123456C"))),
              Some(sample[DateOfDeath]),
              None,
              hasConfirmedPerson = false,
              hasConfirmedContactDetails = false,
              None
            )
          "show the summary" in {
            test(
              sessionWithStartingNewDraftReturn(
                requiredPreviousAnswers,
                PersonalRepresentative
              )._1,
              routes.RepresenteeController.enterId(),
              name,
              isCgtRow = false
            )
          }
        }
      }

      "redirect the page to cya" when {

        "the user has no id " in {
          val requiredPreviousAnswers =
            IncompleteRepresenteeAnswers(
              Some(IndividualName("First", "Last")),
              None,
              None,
              None,
              hasConfirmedPerson = false,
              hasConfirmedContactDetails = false,
              None
            )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithStartingNewDraftReturn(
                requiredPreviousAnswers,
                PersonalRepresentative
              )._1
            )
          }
          checkIsRedirect(
            performAction(),
            routes.RepresenteeController.checkYourAnswers()
          )
        }

        "the user has no name " in {
          val requiredPreviousAnswers =
            IncompleteRepresenteeAnswers(
              None,
              Some(sample[RepresenteeReferenceId]),
              None,
              None,
              hasConfirmedPerson = true,
              hasConfirmedContactDetails = true,
              None
            )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithStartingNewDraftReturn(
                requiredPreviousAnswers,
                PersonalRepresentative
              )._1
            )
          }
          checkIsRedirect(
            performAction(),
            routes.RepresenteeController.checkYourAnswers()
          )
        }

      }

    }

    "handling requests to submit the confirm person page" must {
      def performAction(formData: Seq[(String, String)]): Future[Result] =
        controller.confirmPersonSubmit()(
          FakeRequest().withFormUrlEncodedBody(formData*).withMethod("POST")
        )

      behave like redirectToStartBehaviour(() => performAction(Seq.empty))

      behave like nonCapacitorOrPersonalRepBehaviour(() => performAction(Seq.empty))

      "show a form error" when {

        val session = sessionWithFillingOutReturn(
          IncompleteRepresenteeAnswers(
            Some(sample[IndividualName]),
            Some(sample[RepresenteeNino]),
            Some(sample[DateOfDeath]),
            None,
            hasConfirmedPerson = false,
            hasConfirmedContactDetails = false,
            Some(false)
          ),
          Capacitor
        )._1

        "nothing is selected" in {

          testFormError(performAction, "representeeConfirmPerson.title", session)(
            List.empty,
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
            hasConfirmedPerson = false,
            hasConfirmedContactDetails = false,
            Some(false)
          )

          val (session, sndr) = sessionWithStartingNewDraftReturn(
            requiredPreviousAnswers,
            PersonalRepresentative
          )
          val newSndr         =
            sndr.copy(representeeAnswers =
              Some(
                IncompleteRepresenteeAnswers(
                  None,
                  None,
                  None,
                  None,
                  hasConfirmedPerson = false,
                  hasConfirmedContactDetails = false,
                  None
                )
              )
            )
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreSession(session.copy(journeyStatus = Some(newSndr)))(
              Right(())
            )
          }
          checkIsRedirect(
            performAction(List("confirmed" -> "false")),
            routes.RepresenteeController.checkYourAnswers()
          )
        }

        "the user has clicked Yes " in {
          val requiredPreviousAnswers = IncompleteRepresenteeAnswers(
            Some(IndividualName("First", "Last")),
            Some(RepresenteeNino(NINO("AB123456C"))),
            None,
            None,
            hasConfirmedPerson = false,
            hasConfirmedContactDetails = false,
            Some(false)
          )

          val (session, fillingOutReturn, draftReturn) = sessionWithFillingOutReturn(
            requiredPreviousAnswers,
            PersonalRepresentative
          )

          val newDraftReturn =
            DraftSingleDisposalReturn.newDraftReturn(
              fillingOutReturn.draftReturn.id,
              IncompleteSingleDisposalTriageAnswers.empty.copy(
                individualUserType = draftReturn.triageAnswers.fold(_.individualUserType, _.individualUserType)
              ),
              Some(
                requiredPreviousAnswers.copy(
                  hasConfirmedPerson = true,
                  hasConfirmedContactDetails = false,
                  contactDetails = None
                )
              )
            )

          val newFillingOutReturn =
            fillingOutReturn.copy(draftReturn = newDraftReturn)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(newFillingOutReturn)(Right(()))
            mockStoreSession(session.copy(journeyStatus = Some(newFillingOutReturn)))(
              Right(())
            )
          }
          checkIsRedirect(
            performAction(List("confirmed" -> "true")),
            routes.RepresenteeController.checkYourAnswers()
          )
        }
      }

    }

    "handling submitted ids" must {

      def performAction(formData: Seq[(String, String)]): Future[Result] =
        controller.enterIdSubmit()(
          FakeRequest().withFormUrlEncodedBody(formData*).withMethod("POST")
        )

      val outerKey        = "representeeReferenceIdType"
      val cgtReferenceKey = "representeeCgtRef"
      val ninoKey         = "representeeNino"
      val sautrKey        = "representeeSautr"

      def formData(id: RepresenteeReferenceId): Seq[(String, String)] =
        id match {
          case RepresenteeCgtReference(cgtRef)      =>
            Seq(outerKey -> "0", cgtReferenceKey -> cgtRef.value)
          case RepresenteeNino(nino)                =>
            Seq(outerKey -> "1", ninoKey -> nino.value)
          case RepresenteeSautr(sautr)              =>
            Seq(outerKey -> "2", sautrKey -> sautr.value)
          case RepresenteeReferenceId.NoReferenceId => Seq(outerKey -> "3")
        }

      behave like redirectToStartBehaviour(() => performAction(Seq.empty))

      behave like nonCapacitorOrPersonalRepBehaviour(() => performAction(Seq.empty))

      behave like noIsFirstReturnAnswerBehaviour(() => performAction(Seq.empty))

      "show a form error" when {

        val (session, journey, _) = sessionWithFillingOutReturn(
          IncompleteRepresenteeAnswers.empty
            .copy(name = Some(sample[IndividualName]), isFirstReturn = Some(true)),
          Capacitor
        )

        def test: (Seq[(String, String)], String) => Unit =
          testFormError(
            performAction,
            "representeeReferenceIdType.title",
            session,
            () => mockGetPreviousNameMatchAttempts(journey.ggCredId)(Right(None))
          )

        "nothing is selected" in {
          test(Seq.empty, "representeeReferenceIdType.error.required")
        }

        "cgt reference is selected and a value is submitted" which {

          "is empty" in {
            test(Seq(outerKey -> "0"), "representeeCgtRef.error.required")
          }

          "is too short" in {
            test(
              Seq(outerKey -> "0", cgtReferenceKey -> "ABC"),
              "representeeCgtRef.error.tooShort"
            )
          }

          "is too long" in {
            test(
              Seq(outerKey -> "0", cgtReferenceKey -> "ABC123453432423426"),
              "representeeCgtRef.error.tooLong"
            )
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

        "sautr is selected and a value is submitted" which {

          "is empty" in {
            test(Seq(outerKey -> "2"), "representeeSautr.error.required")
          }

          "is too short" in {
            test(
              Seq(outerKey -> "2", sautrKey -> "123"),
              "representeeSautr.error.tooShort"
            )
          }

          "is too long" in {
            test(
              Seq(outerKey -> "2", sautrKey -> "12345678900"),
              "representeeSautr.error.tooLong"
            )
          }

          "contains non-numerical characters" in {
            test(
              Seq(outerKey -> "2", sautrKey -> "123456789A"),
              "representeeSautr.error.invalid"
            )
          }

        }

      }

      "show an error page" when {

        val name    = sample[IndividualName]
        val answers = IncompleteRepresenteeAnswers.empty.copy(name = Some(name), isFirstReturn = Some(false))
        val cgtRef  = RepresenteeCgtReference(CgtReference("XYCGTP123456789"))

        val (session, journey, draftReturn) =
          sessionWithFillingOutReturn(answers, Capacitor)
        val newDraftReturn                  =
          DraftSingleDisposalReturn.newDraftReturn(
            draftReturn.id,
            IncompleteSingleDisposalTriageAnswers.empty
              .copy(individualUserType = Some(Capacitor)),
            representeeAnswers = Some(answers.copy(id = Some(cgtRef)))
          )

        val newJourney = journey.copy(draftReturn = newDraftReturn)

        "there is an error getting the previous name match attempts" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockGetPreviousNameMatchAttempts(journey.ggCredId)(
              Left(NameMatchServiceError.BackendError(Error("")))
            )
          }

          checkIsTechnicalErrorPage(performAction(formData(cgtRef)))
        }

        "there is an error performing a name match" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockGetPreviousNameMatchAttempts(journey.ggCredId)(Right(None))
            mockNameMatch(
              IndividualRepresenteeNameMatchDetails(name, cgtRef),
              journey.ggCredId,
              None,
              lang
            )(
              Left(NameMatchServiceError.BackendError(Error("")))
            )
          }

          checkIsTechnicalErrorPage(performAction(formData(cgtRef)))
        }

        "there is an error updating the draft return" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockGetPreviousNameMatchAttempts(journey.ggCredId)(Right(None))
            mockNameMatch(
              IndividualRepresenteeNameMatchDetails(name, cgtRef),
              journey.ggCredId,
              None,
              lang
            )(Right(cgtRef))
            mockStoreDraftReturn(newJourney)(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction(formData(cgtRef)))
        }

        "there is an error updating the session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockGetPreviousNameMatchAttempts(journey.ggCredId)(Right(None))
            mockNameMatch(
              IndividualRepresenteeNameMatchDetails(name, cgtRef),
              journey.ggCredId,
              None,
              lang
            )(Right(cgtRef))
            mockStoreDraftReturn(newJourney)(Right(()))
            mockStoreSession(session.copy(journeyStatus = Some(newJourney)))(
              Left(Error(""))
            )
          }

          checkIsTechnicalErrorPage(performAction(formData(cgtRef)))
        }

      }

      "redirect to the name match error page" when {

        "the name match service indicates that the names do not match" in {

          val name                  = IndividualName("First", "Last")
          val answers               =
            IncompleteRepresenteeAnswers.empty.copy(name = Some(name), isFirstReturn = Some(true))
          val cgtRef                = RepresenteeCgtReference(CgtReference("XYCGTP123456789"))
          val nameMatchDetails      =
            IndividualRepresenteeNameMatchDetails(name, cgtRef)
          val (session, journey, _) =
            sessionWithFillingOutReturn(answers, Capacitor)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockGetPreviousNameMatchAttempts(journey.ggCredId)(Right(None))
            mockNameMatch(nameMatchDetails, journey.ggCredId, None, lang)(
              Left(
                NameMatchServiceError.NameMatchFailed(
                  UnsuccessfulNameMatchAttempts(1, 2, nameMatchDetails)
                )
              )
            )
          }
          checkIsRedirect(
            performAction(formData(cgtRef)),
            routes.RepresenteeController.nameMatchError()
          )
        }

      }

      "redirect to the too many name match attempts page" when {

        val name    = sample[IndividualName]
        val answers = IncompleteRepresenteeAnswers.empty.copy(name = Some(name), isFirstReturn = Some(true))
        val cgtRef  = RepresenteeCgtReference(CgtReference("XYCGTP123456789"))

        val (session, journey, _) =
          sessionWithFillingOutReturn(answers, Capacitor)

        "the name match service indicates that the user has already perform too many unsuccessful attempts" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockGetPreviousNameMatchAttempts(journey.ggCredId)(
              Left(NameMatchServiceError.TooManyUnsuccessfulAttempts())
            )
          }

          checkIsRedirect(
            performAction(Seq.empty),
            routes.RepresenteeController.tooManyNameMatchAttempts()
          )
        }

        "the user has just performed too many unsuccessful attempts" in {
          val previousAttempts = UnsuccessfulNameMatchAttempts(
            1,
            2,
            IndividualRepresenteeNameMatchDetails(
              sample[IndividualName],
              sample[RepresenteeNino]
            )
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockGetPreviousNameMatchAttempts(journey.ggCredId)(
              Right(Some(previousAttempts))
            )
            mockNameMatch(
              IndividualRepresenteeNameMatchDetails(name, cgtRef),
              journey.ggCredId,
              Some(previousAttempts),
              lang
            )(
              Left(NameMatchServiceError.TooManyUnsuccessfulAttempts())
            )
          }

          checkIsRedirect(
            performAction(formData(cgtRef)),
            routes.RepresenteeController.tooManyNameMatchAttempts()
          )
        }

      }

      "redirect to the check your answers page" when {

        "all updates are successful and" when {

          "the user has submitted a valid cgt reference" in {
            val name    = sample[IndividualName]
            val answers =
              IncompleteRepresenteeAnswers.empty.copy(name = Some(name), isFirstReturn = Some(false))
            val cgtRef  =
              RepresenteeCgtReference(CgtReference("XYCGTP123456789"))

            val (session, journey, draftReturn) =
              sessionWithFillingOutReturn(answers, Capacitor)
            val newDraftReturn                  =
              DraftSingleDisposalReturn.newDraftReturn(
                draftReturn.id,
                IncompleteSingleDisposalTriageAnswers.empty
                  .copy(individualUserType = Some(Capacitor)),
                representeeAnswers = Some(answers.copy(id = Some(cgtRef)))
              )

            val newJourney = journey.copy(draftReturn = newDraftReturn)

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockGetPreviousNameMatchAttempts(journey.ggCredId)(Right(None))
              mockNameMatch(
                IndividualRepresenteeNameMatchDetails(name, cgtRef),
                journey.ggCredId,
                None,
                lang
              )(Right(cgtRef))
              mockStoreDraftReturn(newJourney)(Right(()))
              mockStoreSession(session.copy(journeyStatus = Some(newJourney)))(
                Right(())
              )
            }

            checkIsRedirect(
              performAction(formData(cgtRef)),
              routes.RepresenteeController.checkYourAnswers()
            )
          }

          "the user has submitted a valid nino" in {
            val name      = sample[IndividualName]
            val answers   = IncompleteRepresenteeAnswers.empty.copy(
              name = Some(name),
              id = Some(sample[RepresenteeCgtReference]),
              isFirstReturn = Some(false)
            )
            val ninoValue = NINO("AB123456C")
            val nino      = RepresenteeNino(ninoValue)

            val (session, journey, draftReturn) =
              sessionWithFillingOutReturn(answers, PersonalRepresentative)
            val newDraftReturn                  = DraftSingleDisposalReturn.newDraftReturn(
              draftReturn.id,
              IncompleteSingleDisposalTriageAnswers.empty
                .copy(individualUserType = Some(PersonalRepresentative)),
              representeeAnswers = Some(answers.copy(id = Some(nino)))
            )
            val newJourney                      = journey.copy(draftReturn = newDraftReturn)

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockGetPreviousNameMatchAttempts(journey.ggCredId)(Right(None))
              mockNameMatch(
                IndividualRepresenteeNameMatchDetails(name, nino),
                journey.ggCredId,
                None,
                lang
              )(Right(nino))
              mockStoreDraftReturn(newJourney)(Right(()))
              mockStoreSession(session.copy(journeyStatus = Some(newJourney)))(
                Right(())
              )
            }

            checkIsRedirect(
              performAction(formData(nino)),
              routes.RepresenteeController.checkYourAnswers()
            )

          }

          "the user has submitted a valid sa utr" in {
            val name               = sample[IndividualName]
            val answers            =
              IncompleteRepresenteeAnswers.empty.copy(name = Some(name), isFirstReturn = Some(false))
            val sautrValue         = SAUTR("1234567890")
            val sautr              = RepresenteeSautr(sautrValue)
            val (session, journey) =
              sessionWithStartingNewDraftReturn(answers, Capacitor)
            val newAnswers         = answers.copy(id = Some(sautr))
            val newJourney         = journey.copy(representeeAnswers = Some(newAnswers))

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockGetPreviousNameMatchAttempts(journey.ggCredId)(Right(None))
              mockNameMatch(
                IndividualRepresenteeNameMatchDetails(name, sautr),
                journey.ggCredId,
                None,
                lang
              )(Right(sautr))
              mockStoreSession(session.copy(journeyStatus = Some(newJourney)))(
                Right(())
              )
            }

            checkIsRedirect(
              performAction(formData(sautr)),
              routes.RepresenteeController.checkYourAnswers()
            )

          }

          "the user has submitted no reference" in {
            val name    = sample[IndividualName]
            val answers =
              IncompleteRepresenteeAnswers.empty.copy(name = Some(name), isFirstReturn = Some(false))

            val (session, journey) =
              sessionWithStartingNewDraftReturn(answers, Capacitor)
            val newAnswers         = answers.copy(id = Some(NoReferenceId))
            val newJourney         = journey.copy(representeeAnswers = Some(newAnswers))

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockGetPreviousNameMatchAttempts(journey.ggCredId)(Right(None))
              mockNameMatch(
                IndividualRepresenteeNameMatchDetails(name, NoReferenceId),
                journey.ggCredId,
                None,
                lang
              )(
                Right(NoReferenceId)
              )
              mockStoreSession(session.copy(journeyStatus = Some(newJourney)))(
                Right(())
              )
            }

            checkIsRedirect(
              performAction(formData(NoReferenceId)),
              routes.RepresenteeController.checkYourAnswers()
            )
          }

        }

      }

    }

    "handling requests to display the check contact details page" must {

      def performAction(): Future[Result] =
        controller.checkContactDetails()(FakeRequest())

      behave like redirectToStartBehaviour(() => performAction())

      behave like nonCapacitorOrPersonalRepBehaviour(() => performAction())

      "show an error page" when {

        "the journey is incomplete and there are no existing details in session and" when {

          val contactDetails                  = sample[RepresenteeContactDetails]
          val answers                         =
            sample[IncompleteRepresenteeAnswers].copy(contactDetails = None)
          val (session, journey, draftReturn) =
            sessionWithFillingOutReturn(
              answers,
              PersonalRepresentative,
              sample[SubscribedDetails].copy(
                contactName = contactDetails.contactName,
                address = contactDetails.address,
                emailAddress = contactDetails.emailAddress
              )
            )

          val newAnswers     = answers.copy(
            contactDetails = Some(contactDetails),
            hasConfirmedContactDetails = false
          )
          val newDraftReturn =
            DraftSingleDisposalReturn.newDraftReturn(
              draftReturn.id,
              IncompleteSingleDisposalTriageAnswers.empty
                .copy(individualUserType = Some(PersonalRepresentative)),
              Some(newAnswers)
            )

          val newJourney = journey.copy(draftReturn = newDraftReturn)

          "there is an error updating the draft return" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockStoreDraftReturn(newJourney)(Left(Error("")))
            }

            checkIsTechnicalErrorPage(performAction())
          }

          "there is an error updating the session" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockStoreDraftReturn(newJourney)(Right(()))
              mockStoreSession(session.copy(journeyStatus = Some(newJourney)))(
                Left(Error(""))
              )
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
        ): Unit =
          checkPageIsDisplayed(
            result,
            messageFromMessageKey("representeeContactDetails.title"),
            { doc =>
              val addressLines = {
                val lines = contactDetails.address match {
                  case Address
                        .UkAddress(line1, line2, town, county, postcode) =>
                    List(Some(line1), line2, town, county, Some(postcode.value))
                  case Address.NonUkAddress(
                        line1,
                        line2,
                        line3,
                        line4,
                        postcode,
                        country
                      ) =>
                    List[Option[String]](
                      Some(line1),
                      line2,
                      line3,
                      line4,
                      postcode,
                      messagesApi.translate(s"country.${country.code}", Seq.empty)
                    )
                }
                lines.collect { case Some(s) => s }
              }

              doc
                .select("#contactName-answer")
                .text()                            shouldBe contactDetails.contactName.value
              doc.select("#address-answer").text() shouldBe addressLines
                .mkString(" ")
              doc
                .select("#emailAddress-answer")
                .text()                            shouldBe contactDetails.emailAddress.value

              doc
                .select("#content > article > form, #main-content form")
                .attr("action")                         shouldBe routes.RepresenteeController
                .checkContactDetailsSubmit()
                .url
              doc.select("#returnToSummaryLink").text() shouldBe (
                if (expectReturnToSummaryLink) messageFromMessageKey("returns.return-to-summary-link") else ""
              )
            }
          )

        "the journey is complete" in {
          val contactDetails = sample[RepresenteeContactDetails]
          val answers        = sample[CompleteRepresenteeAnswers]
            .copy(contactDetails = contactDetails)
          val session        =
            sessionWithStartingNewDraftReturn(
              answers,
              PersonalRepresentative
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
          val answers        = sample[IncompleteRepresenteeAnswers]
            .copy(contactDetails = Some(contactDetails))
          val session        =
            sessionWithFillingOutReturn(
              answers,
              Capacitor
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
              val contactDetails                  = sample[RepresenteeContactDetails]
              val answers                         =
                sample[IncompleteRepresenteeAnswers].copy(contactDetails = None)
              val (session, journey, draftReturn) =
                sessionWithFillingOutReturn(
                  answers,
                  PersonalRepresentative,
                  sample[SubscribedDetails].copy(
                    contactName = contactDetails.contactName,
                    address = contactDetails.address,
                    emailAddress = contactDetails.emailAddress
                  )
                )

              val newAnswers     = answers.copy(
                contactDetails = Some(contactDetails),
                hasConfirmedContactDetails = false
              )
              val newDraftReturn =
                DraftSingleDisposalReturn.newDraftReturn(
                  draftReturn.id,
                  IncompleteSingleDisposalTriageAnswers.empty
                    .copy(individualUserType = Some(PersonalRepresentative)),
                  Some(newAnswers)
                )

              val newJourney = journey.copy(draftReturn = newDraftReturn)

              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
                mockStoreDraftReturn(newJourney)(Right(()))
                mockStoreSession(session.copy(journeyStatus = Some(newJourney)))(
                  Right(())
                )
              }
              checkPage(
                performAction(),
                contactDetails,
                expectReturnToSummaryLink = true
              )
            }

            "the user has not started a draft return" in {
              val contactDetails     = sample[RepresenteeContactDetails]
              val answers            =
                sample[IncompleteRepresenteeAnswers].copy(contactDetails = None)
              val (session, journey) =
                sessionWithStartingNewDraftReturn(
                  answers,
                  PersonalRepresentative,
                  sample[SubscribedDetails].copy(
                    contactName = contactDetails.contactName,
                    address = contactDetails.address,
                    emailAddress = contactDetails.emailAddress
                  )
                )

              val newAnswers = answers.copy(
                contactDetails = Some(contactDetails),
                hasConfirmedContactDetails = false
              )
              val newJourney = journey.copy(representeeAnswers = Some(newAnswers))

              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
                mockStoreSession(session.copy(journeyStatus = Some(newJourney)))(
                  Right(())
                )
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

      behave like redirectToStartBehaviour(() => performAction())

      behave like nonCapacitorOrPersonalRepBehaviour(() => performAction())

      "immediately redirect to the check your answers page" when {

        "the section is incomplete and user has already confirmed the contact details" in {
          val answers = sample[IncompleteRepresenteeAnswers].copy(
            contactDetails = Some(sample[RepresenteeContactDetails]),
            hasConfirmedContactDetails = true
          )
          val session =
            sessionWithFillingOutReturn(answers, Capacitor)._1

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkIsRedirect(
            performAction(),
            routes.RepresenteeController.checkYourAnswers()
          )
        }

        "the section is complete" in {
          val answers = sample[CompleteRepresenteeAnswers]
          val session = sessionWithFillingOutReturn(
            answers,
            PersonalRepresentative
          )._1

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkIsRedirect(
            performAction(),
            routes.RepresenteeController.checkYourAnswers()
          )

        }

      }

      "show an error page" when {

        "the section is incomplete and the user hasn't confirmed the contact details yet and" when {
          val answers                         = sample[IncompleteRepresenteeAnswers].copy(
            contactDetails = Some(sample[RepresenteeContactDetails]),
            hasConfirmedContactDetails = false
          )
          val (session, journey, draftReturn) =
            sessionWithFillingOutReturn(answers, Capacitor)
          val newAnswers                      = answers.copy(hasConfirmedContactDetails = true)
          val newDraftReturn                  =
            draftReturn.copy(representeeAnswers = Some(newAnswers))
          val newJourney                      = journey.copy(draftReturn = newDraftReturn)

          "there is an error updating the draft return" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockStoreDraftReturn(newJourney)(Left(Error("")))
            }

            checkIsTechnicalErrorPage(performAction())
          }

          "there is an error updating the session" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockStoreDraftReturn(newJourney)(Right(()))
              mockStoreSession(session.copy(journeyStatus = Some(newJourney)))(
                Left(Error(""))
              )
            }

            checkIsTechnicalErrorPage(performAction())
          }

        }

      }

      "redirect to the check your answers page" when {

        "the section is incomplete and the user hasn't confirmed the contact details yet and" +
          "all updates are successful" in {
            val answers                         = sample[IncompleteRepresenteeAnswers].copy(
              contactDetails = Some(sample[RepresenteeContactDetails]),
              hasConfirmedContactDetails = false
            )
            val (session, journey, draftReturn) =
              sessionWithFillingOutReturn(answers, Capacitor)
            val newAnswers                      = answers.copy(hasConfirmedContactDetails = true)
            val newDraftReturn                  =
              draftReturn.copy(representeeAnswers = Some(newAnswers))
            val newJourney                      = journey.copy(draftReturn = newDraftReturn)

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockStoreDraftReturn(newJourney)(Right(()))
              mockStoreSession(session.copy(journeyStatus = Some(newJourney)))(
                Right(())
              )
            }

            checkIsRedirect(
              performAction(),
              routes.RepresenteeController.checkYourAnswers()
            )
          }

      }

    }

    "handling requests to display the check you answers page" must {

      def performAction(): Future[Result] =
        controller.checkYourAnswers()(FakeRequest())

      behave like redirectToStartBehaviour(() => performAction())

      behave like nonCapacitorOrPersonalRepBehaviour(() => performAction())
      val dateOfDeath     = sample[DateOfDeath]
      val completeAnswers = sample[CompleteRepresenteeAnswers].copy(dateOfDeath = Some(dateOfDeath))

      val allQuestionsAnswers = IncompleteRepresenteeAnswers(
        Some(completeAnswers.name),
        Some(completeAnswers.id),
        Some(dateOfDeath),
        Some(sample[RepresenteeContactDetails]),
        hasConfirmedPerson = true,
        hasConfirmedContactDetails = true,
        Some(completeAnswers.isFirstReturn)
      )

      "redirect to the is first return page" when {

        "the user has not yet answered that question" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithStartingNewDraftReturn(
                allQuestionsAnswers.copy(isFirstReturn = None),
                Capacitor
              )._1
            )
          }
          checkIsRedirect(
            performAction(),
            routes.RepresenteeController.isFirstReturn()
          )
        }

      }

      "redirect to the enter name page" when {

        "that question hasn't been answered yet" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithFillingOutReturn(
                allQuestionsAnswers.copy(name = None),
                PersonalRepresentative
              )._1
            )
          }

          checkIsRedirect(
            performAction(),
            routes.RepresenteeController.enterName()
          )
        }
      }

      "redirect to the enter id page" when {

        "that question hasn't been answered yet" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithFillingOutReturn(
                allQuestionsAnswers.copy(id = None),
                Capacitor
              )._1
            )
          }

          checkIsRedirect(
            performAction(),
            routes.RepresenteeController.enterId()
          )
        }
      }

      "redirect to the enter date of death page" when {

        "that question hasn't been answered yet" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithFillingOutReturn(
                allQuestionsAnswers.copy(dateOfDeath = None),
                PersonalRepresentative
              )._1
            )
          }

          checkIsRedirect(
            performAction(),
            routes.RepresenteeController.enterDateOfDeath()
          )
        }
      }

      "redirect the page to confirm-person" when {

        "the user has not yet confirmed the person" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithStartingNewDraftReturn(
                allQuestionsAnswers.copy(hasConfirmedPerson = false),
                PersonalRepresentative
              )._1
            )
          }
          checkIsRedirect(
            performAction(),
            routes.RepresenteeController.confirmPerson()
          )
        }

      }

      "redirect to the check contact details page" when {

        "there are no contact details in session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithFillingOutReturn(
                allQuestionsAnswers.copy(contactDetails = None),
                PersonalRepresentative
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
                Capacitor
              )._1
            )
          }

          checkIsRedirect(
            performAction(),
            routes.RepresenteeController.checkContactDetails()
          )
        }

      }

      "show the page" when {

        "the user has just answered all the questions and all updates are successful" in {
          val updatedAllQuestionsAnswers      = allQuestionsAnswers.copy(
            contactDetails = Some(completeAnswers.contactDetails)
          )
          val (session, journey, draftReturn) =
            sessionWithFillingOutReturn(
              updatedAllQuestionsAnswers,
              PersonalRepresentative
            )
          val newDraftReturn                  =
            draftReturn.copy(representeeAnswers = Some(completeAnswers))
          val updatedJourney                  = journey.copy(draftReturn = newDraftReturn)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(updatedJourney)(Right(()))
            mockStoreSession(
              session.copy(journeyStatus = Some(updatedJourney))
            )(Right(()))
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("representee.cya.title"),
            doc => {
              doc
                .select("#back, .govuk-back-link")
                .attr(
                  "href"
                ) shouldBe returns.triage.routes.CommonTriageQuestionsController
                .whoIsIndividualRepresenting()
                .url

              doc
                .select("#content > article > form, #main-content form")
                .attr("action") shouldBe routes.RepresenteeController
                .checkYourAnswersSubmit()
                .url
            }
          )
        }

        "the user has already answered all the questions" in {
          forAll { (completeAnswers: CompleteRepresenteeAnswers) =>
            val (session, _, _) = sessionWithFillingOutReturn(
              completeAnswers,
              PersonalRepresentative
            )

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)

            }

            checkPageIsDisplayed(
              performAction(),
              messageFromMessageKey("representee.cya.title"),
              { doc =>
                doc
                  .select("#back, .govuk-back-link")
                  .attr("href") shouldBe returns.triage.routes.CommonTriageQuestionsController
                  .whoIsIndividualRepresenting()
                  .url

                doc
                  .select("#content > article > form, #main-content form")
                  .attr("action") shouldBe routes.RepresenteeController
                  .checkYourAnswersSubmit()
                  .url

                RepresenteeControllerSpec
                  .validateRepresenteeCheckYourAnswersPage(completeAnswers, doc)
              }
            )
          }
        }

      }

      "show an error page" when {

        "there is an error updating the draft return" in {
          val updatedAllQuestionsAnswers      = allQuestionsAnswers.copy(
            contactDetails = Some(completeAnswers.contactDetails)
          )
          val (session, journey, draftReturn) =
            sessionWithFillingOutReturn(
              updatedAllQuestionsAnswers,
              PersonalRepresentative
            )

          val updatedJourney = journey.copy(draftReturn =
            draftReturn.copy(
              representeeAnswers = Some(completeAnswers)
            )
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(updatedJourney)(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction())
        }

        "there is an error updating the session" in {
          val updatedAllQuestionsAnswers      = allQuestionsAnswers.copy(
            contactDetails = Some(completeAnswers.contactDetails)
          )
          val (session, journey, draftReturn) =
            sessionWithFillingOutReturn(
              updatedAllQuestionsAnswers,
              PersonalRepresentative
            )
          val updatedJourney                  = journey.copy(draftReturn =
            draftReturn.copy(
              representeeAnswers = Some(completeAnswers)
            )
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(updatedJourney)(Right(()))
            mockStoreSession(
              session.copy(journeyStatus = Some(updatedJourney))
            )(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction())
        }

      }

    }

    "handling submits on the cya page" must {

      def performAction(): Future[Result] =
        controller.checkYourAnswersSubmit()(FakeRequest())

      behave like redirectToStartBehaviour(() => performAction())

      val completeAnswers = sample[CompleteRepresenteeAnswers]

      "redirect to the how many properties page" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            sessionWithFillingOutReturn(
              completeAnswers,
              Capacitor
            )._1
          )
        }

        checkIsRedirect(
          performAction(),
          returns.triage.routes.CommonTriageQuestionsController
            .howManyProperties()
        )
      }
    }

    "handling requests to display the name match error page" must {

      def performAction(): Future[Result] =
        controller.nameMatchError()(FakeRequest())

      behave like redirectToStartBehaviour(() => performAction())

      behave like nonCapacitorOrPersonalRepBehaviour(() => performAction())

      "display the page" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            sessionWithFillingOutReturn(
              sample[RepresenteeAnswers],
              PersonalRepresentative
            )._1
          )
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("representeeNameMatchError.title"),
          doc =>
            doc
              .select("#content > article > form, #main-content form")
              .attr("action") shouldBe routes.RepresenteeController
              .nameMatchErrorSubmit()
              .url
        )

      }

    }

    "handling submits on the name match error page" must {

      def performAction(): Future[Result] =
        controller.nameMatchErrorSubmit()(FakeRequest())

      behave like redirectToStartBehaviour(() => performAction())

      behave like nonCapacitorOrPersonalRepBehaviour(() => performAction())

      "redirect to the enter name page" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            sessionWithStartingNewDraftReturn(
              sample[RepresenteeAnswers],
              Capacitor
            )._1
          )
        }

        checkIsRedirect(
          performAction(),
          routes.RepresenteeController.enterName()
        )
      }

    }

    "handling requests to display the is first return page" must {

      def performAction(): Future[Result] =
        controller.isFirstReturn()(FakeRequest())

      behave like redirectToStartBehaviour(() => performAction())

      behave like nonCapacitorOrPersonalRepBehaviour(() => performAction())

      "display the page" when {

        def test(
          session: SessionData
        )(expectedTitleKey: String, expectedBackLink: Call, expectedPreselectedAnswer: Option[Boolean]): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(expectedTitleKey),
            { doc =>
              doc
                .select("#back, .govuk-back-link")
                .attr("href") shouldBe expectedBackLink.url

              doc
                .select("#content > article > form, #main-content form")
                .attr("action") shouldBe routes.RepresenteeController
                .isFirstReturnSubmit()
                .url

              expectedPreselectedAnswer.foreach { preselected =>
                doc.select(s"#representeeIsFirstReturn-$preselected").hasAttr("checked")
              }
            }
          )
        }

        "the user is a capacitor" in {
          test(
            sessionWithStartingNewDraftReturn(
              IncompleteRepresenteeAnswers(
                Some(sample[IndividualName]),
                Some(sample[RepresenteeReferenceId]),
                None,
                None,
                hasConfirmedPerson = true,
                hasConfirmedContactDetails = false,
                isFirstReturn = None
              ),
              Capacitor
            )._1
          )(
            "representeeIsFirstReturn.capacitor.title",
            returns.triage.routes.CommonTriageQuestionsController.whoIsIndividualRepresenting(),
            None
          )
        }

        "the user is a personal rep" in {
          test(
            sessionWithStartingNewDraftReturn(
              IncompleteRepresenteeAnswers(
                Some(sample[IndividualName]),
                Some(sample[RepresenteeReferenceId]),
                None,
                None,
                hasConfirmedPerson = true,
                hasConfirmedContactDetails = false,
                isFirstReturn = Some(true)
              ),
              PersonalRepresentative
            )._1
          )(
            "representeeIsFirstReturn.personalRep.title",
            returns.triage.routes.CommonTriageQuestionsController.whoIsIndividualRepresenting(),
            Some(true)
          )
        }

        "the user is a personal rep in period of admin" in {
          test(
            sessionWithStartingNewDraftReturn(
              sample[CompleteRepresenteeAnswers].copy(
                isFirstReturn = false
              ),
              PersonalRepresentativeInPeriodOfAdmin
            )._1
          )(
            "representeeIsFirstReturn.personalRepInPeriodOfAdmin.title",
            routes.RepresenteeController.checkYourAnswers(),
            Some(false)
          )

        }

        "the user is an agent of a personal rep in period of admin" in {
          test(
            sessionWithStartingNewDraftReturn(
              sample[CompleteRepresenteeAnswers].copy(
                isFirstReturn = false
              ),
              PersonalRepresentativeInPeriodOfAdmin
            )._1
              .copy(userType = Some(UserType.Agent))
          )(
            "representeeIsFirstReturn.agent.personalRepInPeriodOfAdmin.title",
            routes.RepresenteeController.checkYourAnswers(),
            Some(false)
          )

        }
      }

    }

    "handling submits on the is first return page" must {

      def performAction(formData: Seq[(String, String)]): Future[Result] =
        controller.isFirstReturnSubmit()(FakeRequest().withFormUrlEncodedBody(formData*).withMethod("POST"))

      val key = "representeeIsFirstReturn"

      behave like redirectToStartBehaviour(() => performAction(Seq.empty))

      behave like nonCapacitorOrPersonalRepBehaviour(() => performAction(Seq.empty))

      "show a form error" when {

        val answers = IncompleteRepresenteeAnswers(
          Some(sample[IndividualName]),
          Some(sample[RepresenteeReferenceId]),
          None,
          None,
          hasConfirmedPerson = true,
          hasConfirmedContactDetails = false,
          isFirstReturn = None
        )

        "nothing is submitted for capacitor" in {
          testFormError(
            performAction,
            s"$key.capacitor.title",
            sessionWithStartingNewDraftReturn(
              answers,
              Capacitor
            )._1
          )(
            Seq.empty,
            s"$key.capacitor.error.required"
          )
        }

        "nothing is submitted for personal rep" in {
          testFormError(
            performAction,
            s"$key.personalRep.title",
            sessionWithStartingNewDraftReturn(
              answers,
              PersonalRepresentative
            )._1
          )(
            Seq.empty,
            s"$key.personalRep.error.required"
          )
        }

        "nothing is submitted for personal rep in period of admin" in {
          testFormError(
            performAction,
            s"$key.personalRepInPeriodOfAdmin.title",
            sessionWithStartingNewDraftReturn(
              answers,
              PersonalRepresentativeInPeriodOfAdmin
            )._1
          )(
            Seq.empty,
            s"$key.personalRepInPeriodOfAdmin.error.required"
          )
        }

        "nothing is submitted for agent of personal rep in period of admin" in {
          testFormError(
            performAction,
            s"$key.agent.personalRepInPeriodOfAdmin.title",
            sessionWithStartingNewDraftReturn(
              answers,
              PersonalRepresentativeInPeriodOfAdmin
            )._1
              .copy(userType = Some(UserType.Agent))
          )(
            Seq.empty,
            s"$key.agent.personalRepInPeriodOfAdmin.error.required"
          )
        }

        "the answer submitted is invalid for personal rep" in {
          testFormError(
            performAction,
            s"$key.personalRep.title",
            sessionWithFillingOutReturn(
              answers,
              PersonalRepresentative
            )._1
          )(
            Seq(key -> "123"),
            s"$key.personalRep.error.boolean"
          )
        }

        "the answer submitted is invalid for personal rep in period of admin" in {
          testFormError(
            performAction,
            s"$key.personalRepInPeriodOfAdmin.title",
            sessionWithFillingOutReturn(
              answers,
              PersonalRepresentativeInPeriodOfAdmin
            )._1
          )(
            Seq(key -> "123"),
            s"$key.personalRepInPeriodOfAdmin.error.boolean"
          )
        }

        "the answer submitted is invalid for agent of personal rep in period of admin" in {
          testFormError(
            performAction,
            s"$key.agent.personalRepInPeriodOfAdmin.title",
            sessionWithFillingOutReturn(
              answers,
              PersonalRepresentativeInPeriodOfAdmin
            )._1
              .copy(userType = Some(UserType.Agent))
          )(
            Seq(key -> "123"),
            s"$key.agent.personalRepInPeriodOfAdmin.error.boolean"
          )
        }

      }

      "show an error page" when {

        val answers                                  = IncompleteRepresenteeAnswers(
          Some(sample[IndividualName]),
          Some(sample[RepresenteeReferenceId]),
          None,
          None,
          hasConfirmedPerson = true,
          hasConfirmedContactDetails = false,
          isFirstReturn = Some(false)
        )
        val (session, fillingOutReturn, draftReturn) =
          sessionWithFillingOutReturn(answers, Capacitor)

        val newAnswers     = IncompleteRepresenteeAnswers.empty.copy(isFirstReturn = Some(true))
        val newDraftReturn =
          DraftSingleDisposalReturn.newDraftReturn(
            fillingOutReturn.draftReturn.id,
            IncompleteSingleDisposalTriageAnswers.empty.copy(
              individualUserType = draftReturn.triageAnswers.fold(_.individualUserType, _.individualUserType)
            ),
            Some(newAnswers)
          )
        val newJourney     = fillingOutReturn.copy(draftReturn = newDraftReturn)
        val newSession     = session.copy(journeyStatus = Some(newJourney))

        "there is an error updating the draft return" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(newJourney)(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction(Seq(key -> "true")))
        }

        "there is an error updating the session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(newJourney)(Right(()))
            mockStoreSession(
              newSession
            )(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction(Seq(key -> "true")))
        }

      }

      "redirect to the check your answers page" when {

        "a valid answer is submitted and all updates are successful" in {
          val answers = sample[CompleteRepresenteeAnswers].copy(isFirstReturn = true)

          val (session, fillingOutReturn, draftReturn) =
            sessionWithFillingOutReturn(answers, Capacitor, isAmend = true)

          val newAnswers = IncompleteRepresenteeAnswers.empty.copy(isFirstReturn = Some(false))

          val newDraftReturn =
            DraftSingleDisposalReturn.newDraftReturn(
              fillingOutReturn.draftReturn.id,
              IncompleteSingleDisposalTriageAnswers.empty.copy(
                individualUserType = draftReturn.triageAnswers.fold(_.individualUserType, _.individualUserType)
              ),
              Some(newAnswers)
            )

          val newJourney =
            fillingOutReturn.copy(draftReturn = newDraftReturn).withForceDisplayGainOrLossAfterReliefsForAmends
          val newSession = session.copy(journeyStatus = Some(newJourney))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(newJourney)(Right(()))
            mockStoreSession(
              newSession
            )(Right(()))
          }

          checkIsRedirect(
            performAction(Seq(key -> "false")),
            routes.RepresenteeController.checkYourAnswers()
          )
        }

      }

      "not do any updates" when {

        "the answer submitted is the same as one already in session" in {
          val answers = sample[CompleteRepresenteeAnswers].copy(isFirstReturn = false)

          val session = sessionWithStartingNewDraftReturn(answers, Capacitor)._1

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkIsRedirect(
            performAction(Seq(key -> "false")),
            routes.RepresenteeController.checkYourAnswers()
          )
        }

      }

    }

    "handling requests to display the too many mane match attempts page" must {

      def performAction(): Future[Result] = controller.tooManyNameMatchAttempts()(FakeRequest())

      val (session, journey, _) =
        sessionWithFillingOutReturn(
          sample[IncompleteRepresenteeAnswers].copy(
            name = Some(sample[IndividualName]),
            id = Some(sample[RepresenteeReferenceId]),
            isFirstReturn = Some(true),
            dateOfDeath = Some(sample[DateOfDeath])
          ),
          PersonalRepresentative
        )

      val ggCredId = journey.ggCredId

      "show an error page" when {

        "there is an error getting the number of previous name match attempts" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockGetPreviousNameMatchAttempts(ggCredId)(
              Left(NameMatchServiceError.BackendError(Error("")))
            )
          }

          checkIsTechnicalErrorPage(performAction())
        }

      }

      "redirect to the enter name page if user has not made too many name match attempts" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockGetPreviousNameMatchAttempts(ggCredId)(
            Right(None)
          )
        }

        checkIsRedirect(performAction(), routes.RepresenteeController.enterName())
      }

      "show the page" when {

        "the user has made too many attempts" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockGetPreviousNameMatchAttempts(ggCredId)(
              Left(NameMatchServiceError.TooManyUnsuccessfulAttempts())
            )
          }

          checkPageIsDisplayed(performAction(), messageFromMessageKey("representeeTooManyNameMatchFailures.title"))
        }
      }

    }

  }

  def testFormError(
    performAction: Seq[(String, String)] => Future[Result],
    pageTitleKey: String,
    currentSession: SessionData,
    extraMockActions: () => Unit = () => ()
  )(data: Seq[(String, String)], expectedErrorMessageKey: String): Unit = {
    inSequence {
      mockAuthWithNoRetrievals()
      mockGetSession(currentSession)
      extraMockActions()
    }
    checkPageIsDisplayed(
      performAction(data),
      messageFromMessageKey(pageTitleKey),
      { doc =>
        doc
          .select("[data-spec='errorSummaryDisplay'] a")
          .text() shouldBe messageFromMessageKey(
          expectedErrorMessageKey
        )
        doc.title() should startWith("Error:")
      },
      BAD_REQUEST
    )
  }

  def redirectToStartBehaviour(performAction: () => Future[Result]): Unit =
    redirectToStartWhenInvalidJourney(
      () => performAction(),
      {
        case _: StartingNewDraftReturn | _: FillingOutReturn => true
        case _                                               => false
      }
    )

  def tooManyNameMatchAttemptsBehaviour(
    performAction: () => Future[Result]
  ): Unit = {
    val (session, journey, _) =
      sessionWithFillingOutReturn(
        sample[IncompleteRepresenteeAnswers].copy(
          name = Some(sample[IndividualName]),
          id = Some(sample[RepresenteeReferenceId]),
          isFirstReturn = Some(true),
          dateOfDeath = Some(sample[DateOfDeath])
        ),
        PersonalRepresentative
      )
    val ggCredId              = journey.ggCredId

    "show an error page" when {

      "there is an error getting the number of previous name match attempts" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockGetPreviousNameMatchAttempts(ggCredId)(
            Left(NameMatchServiceError.BackendError(Error("")))
          )
        }

        checkIsTechnicalErrorPage(performAction())
      }

    }

    "redirect to the too many name match attempts page" when {

      "the service indicates that too many attempts have been made" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockGetPreviousNameMatchAttempts(ggCredId)(
            Left(NameMatchServiceError.TooManyUnsuccessfulAttempts())
          )
        }

        checkIsRedirect(
          performAction(),
          routes.RepresenteeController.tooManyNameMatchAttempts()
        )
      }

    }

  }

  def nonCapacitorOrPersonalRepBehaviour(
    performAction: () => Future[Result]
  ): Unit =
    "redirect to the task list endpoint" when {
      "the user has selected 'self' for the person they are submitting the return for" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            sessionWithStartingNewDraftReturn(
              None,
              Some(Self),
              sample[SubscribedDetails]
            )._1
          )
        }

        checkIsRedirect(
          performAction(),
          returns.routes.TaskListController.taskList()
        )
      }

      "the user has not indicated who they are submitting the return for" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            sessionWithStartingNewDraftReturn(
              None,
              None,
              sample[SubscribedDetails]
            )._1
          )
        }

        checkIsRedirect(
          performAction(),
          returns.routes.TaskListController.taskList()
        )
      }
    }

  private def noIsFirstReturnAnswerBehaviour(
    performAction: () => Future[Result]
  ): Unit =
    "redirect to the check your answers endpoint" when {

      "there is no answer to the 'is first return?' question" in {
        val answers = IncompleteRepresenteeAnswers(
          Some(sample[IndividualName]),
          Some(sample[RepresenteeReferenceId]),
          Some(sample[DateOfDeath]),
          Some(sample[RepresenteeContactDetails]),
          hasConfirmedPerson = true,
          hasConfirmedContactDetails = true,
          None
        )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            sessionWithStartingNewDraftReturn(Some(answers), Some(PersonalRepresentative), sample[SubscribedDetails])._1
          )
        }

        checkIsRedirect(performAction(), routes.RepresenteeController.checkYourAnswers())

      }

    }

}

object RepresenteeControllerSpec extends Matchers {

  def validateRepresenteeCheckYourAnswersPage(
    answers: CompleteRepresenteeAnswers,
    doc: Document
  )(implicit messagesApi: MessagesApi, lang: Lang): Unit = {
    val addressLines: List[String] = {
      val lines = answers.contactDetails.address match {
        case UkAddress(line1, line2, town, county, postcode) =>
          List(Some(line1), line2, town, county, Some(postcode.value))
        case Address
              .NonUkAddress(line1, line2, line3, line4, postcode, country) =>
          List(Some(line1), line2, line3, line4, postcode, messagesApi.translate(s"country.${country.code}", Seq.empty))
      }

      lines.collect { case Some(s) => s }
    }

    val expectedIdContent = answers.id match {
      case RepresenteeCgtReference(cgtRef) =>
        Some(
          messagesApi("representee.cyaLabel.cgtReference", cgtRef.value)
            .replace("<br>", " ")
        )
      case _                               =>
        None
    }

    doc
      .select("#isFirstReturn-answer")
      .text() shouldBe messagesApi(
      if (answers.isFirstReturn) "generic.yes" else "generic.no"
    )

    doc
      .select("#personRepresented-answer")
      .text() shouldBe s"${answers.name.makeSingleName}${expectedIdContent.fold("")(s => s" $s")}"

    doc
      .select("#contactName-answer")
      .text() shouldBe answers.contactDetails.contactName.value

    doc
      .select("#address-answer")
      .text()
      .replace(" ", "") shouldBe addressLines.mkString("").replace(" ", "")

    doc
      .select("#email-answer")
      .text() shouldBe answers.contactDetails.emailAddress.value

    answers.dateOfDeath.foreach { date =>
      doc.select("#dateOfDeath-answer").text() shouldBe TimeUtils
        .govShortDisplayFormat(date.value)(using
          MessagesImpl(lang, messagesApi)
        )
    }

  }
}
