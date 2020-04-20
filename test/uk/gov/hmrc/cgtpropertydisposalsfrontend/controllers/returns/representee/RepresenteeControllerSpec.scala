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
import play.api.http.Status.BAD_REQUEST
import play.api.i18n.MessagesApi
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.{Action, AnyContent, Call, Result}
import play.api.test.FakeRequest
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.ReturnsServiceSupport
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, ControllerSpec, NameFormValidationTests, SessionSupport, returns}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, StartingNewDraftReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.{CgtReference, NINO, SAUTR}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.IndividualName
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.IndividualUserType._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.MultipleDisposalsTriageAnswers.CompleteMultipleDisposalsTriageAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.RepresenteeAnswers.{CompleteRepresenteeAnswers, IncompleteRepresenteeAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.RepresenteeReferenceId.{NoReferenceId, RepresenteeCgtReference, RepresenteeNino, RepresenteeSautr}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.CompleteSingleDisposalTriageAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{DraftMultipleDisposalsReturn, IndividualUserType, RepresenteeAnswers, RepresenteeReferenceId}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, SessionData}
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
            test(
              sessionWithStartingNewDraftReturn(
                requiredPreviousAnswers,
                Right(Capacitor)
              )._1,
              routes.RepresenteeController.enterName(),
              expectReturnToSummaryLink = false,
              None
            )
          }

          "the user has previously submitted a cgt reference" in {
            val cgtReference = sample[RepresenteeCgtReference]
            test(
              sessionWithStartingNewDraftReturn(
                requiredPreviousAnswers.copy(id = Some(cgtReference)),
                Left(PersonalRepresentative)
              )._1,
              routes.RepresenteeController.enterName(),
              expectReturnToSummaryLink = false,
              Some(cgtReference)
            )
          }

          "the user has previously submitted a nino" in {
            val nino = sample[RepresenteeNino]
            test(
              sessionWithStartingNewDraftReturn(
                requiredPreviousAnswers.copy(id = Some(nino)),
                Left(PersonalRepresentative)
              )._1,
              routes.RepresenteeController.enterName(),
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
              routes.RepresenteeController.enterName(),
              expectReturnToSummaryLink = false,
              Some(sautr)
            )

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
              routes.RepresenteeController.enterName(),
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

        val answers = IncompleteRepresenteeAnswers.empty.copy(name = Some(sample[IndividualName]))
        val cgtRef  = RepresenteeCgtReference(CgtReference("XYCGTP123456789"))

        val (session, journey, draftReturn) =
          sessionWithFillingOutReturn(answers, Right(Capacitor))
        val newDraftReturn = draftReturn.copy(
          representeeAnswers = Some(answers.copy(id = Some(cgtRef)))
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

          checkIsTechnicalErrorPage(performAction(formData(cgtRef)))
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

          checkIsTechnicalErrorPage(performAction(formData(cgtRef)))
        }

      }

      "redirect to the check your answers page" when {

        "all updates are successful and" when {

          "the user has submitted a valid cgt reference" in {
            val answers = IncompleteRepresenteeAnswers.empty.copy(name = Some(sample[IndividualName]))
            val cgtRef  = RepresenteeCgtReference(CgtReference("XYCGTP123456789"))

            val (session, journey, draftReturn) =
              sessionWithFillingOutReturn(answers, Right(Capacitor))
            val newDraftReturn = draftReturn.copy(
              representeeAnswers = Some(answers.copy(id = Some(cgtRef)))
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

            checkIsRedirect(performAction(formData(cgtRef)), routes.RepresenteeController.checkYourAnswers())
          }

          "the user has submitted a valid nino" in {
            val answers = IncompleteRepresenteeAnswers.empty.copy(
              name = Some(sample[IndividualName]),
              id   = Some(sample[RepresenteeCgtReference])
            )
            val nino = RepresenteeNino(NINO("AB123456C"))

            val (session, journey, draftReturn) =
              sessionWithFillingOutReturn(answers, Left(PersonalRepresentative))
            val newDraftReturn = draftReturn.copy(
              representeeAnswers = Some(answers.copy(id = Some(nino)))
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

            checkIsRedirect(performAction(formData(nino)), routes.RepresenteeController.checkYourAnswers())

          }

          "the user has submitted a valid sa utr" in {
            val answers = IncompleteRepresenteeAnswers.empty.copy(name = Some(sample[IndividualName]))
            val sautr   = RepresenteeSautr(SAUTR("1234567890"))

            val (session, journey) =
              sessionWithStartingNewDraftReturn(answers, Right(Capacitor))
            val newAnswers = answers.copy(id                 = Some(sautr))
            val newJourney = journey.copy(representeeAnswers = Some(newAnswers))

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockStoreSession(session.copy(journeyStatus = Some(newJourney)))(Right(()))
            }

            checkIsRedirect(performAction(formData(sautr)), routes.RepresenteeController.checkYourAnswers())

          }

          "the user has submitted no reference" in {
            val answers = IncompleteRepresenteeAnswers.empty.copy(name = Some(sample[IndividualName]))

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

    "handling requests to display the check you answers page" must {

      def performAction(): Future[Result] =
        controller.checkYourAnswers()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      behave like nonCapacitorOrPersonalRepBehaviour(performAction)

      val completeAnswers = sample[CompleteRepresenteeAnswers]

      val allQuestionsAnswers = IncompleteRepresenteeAnswers(
        Some(completeAnswers.name),
        Some(completeAnswers.id)
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

        "handling requests to display the enter id page" in {
          test(controller.enterId())
        }

        "handling submitted ids" in {
          test(controller.enterIdSubmit())
        }

        "handling requests to display the check your answers page" in {
          test(controller.checkYourAnswers())
        }

      }

    }

  }

}
