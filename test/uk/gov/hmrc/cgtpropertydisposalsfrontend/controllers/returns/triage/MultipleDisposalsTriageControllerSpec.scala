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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.triage

import java.time.{Clock, LocalDate}
import java.util.UUID

import cats.data.EitherT
import cats.instances.future._
import org.jsoup.nodes.Document
import org.scalatest.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.Configuration
import play.api.http.Status.BAD_REQUEST
import play.api.i18n.{Lang, MessagesApi, MessagesImpl}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.{Call, Result}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.triage.MultipleDisposalsTriageControllerSpec.{SelectorAndValue, TagAttributePairAndValue, UserTypeDisplay}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.{ReturnsServiceSupport, representee, triage}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, ControllerSpec, DateErrorScenarios, SessionSupport}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, StartingNewDraftReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Country
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.{AgentReferenceNumber, UUIDGenerator}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.{IndividualName, TrustName}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.SubscribedDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.AssetType.IndirectDisposal
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.IndividualUserType.{Capacitor, PersonalRepresentative, Self}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.MultipleDisposalsTriageAnswers.{IncompleteMultipleDisposalsTriageAnswers, _}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.RepresenteeAnswers.CompleteRepresenteeAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.IncompleteSingleDisposalTriageAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.YearToDateLiabilityAnswers.{CalculatedYTDAnswers, NonCalculatedYTDAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{IndividualUserType, _}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, JourneyStatus, SessionData, TaxYear, TimeUtils, UserType}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.{ReturnsService, TaxYearService}
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class MultipleDisposalsTriageControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with ScalaCheckDrivenPropertyChecks
    with RedirectToStartBehaviour
    with ReturnsServiceSupport {

  val mockTaxYearService = mock[TaxYearService]

  val mockUUIDGenerator = mock[UUIDGenerator]

  val trustDisplay       =
    UserTypeDisplay(UserType.Organisation, None, Left(sample[TrustName]))
  val agentDisplay       =
    UserTypeDisplay(UserType.Agent, None, Right(sample[IndividualName]))
  val individualDisplay  =
    UserTypeDisplay(UserType.Individual, None, Right(sample[IndividualName]))
  val personalRepDisplay =
    UserTypeDisplay(
      UserType.Individual,
      Some(Left(PersonalRepresentative)),
      Right(sample[IndividualName])
    )
  val capacitorDisplay   =
    UserTypeDisplay(
      UserType.Individual,
      Some(Right(Capacitor)),
      Right(sample[IndividualName])
    )

  override val overrideBindings =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore),
      bind[TaxYearService].toInstance(mockTaxYearService),
      bind[UUIDGenerator].toInstance(mockUUIDGenerator),
      bind[ReturnsService].toInstance(mockReturnsService)
    )

  lazy val controller = instanceOf[MultipleDisposalsTriageController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  def mockGetTaxYear(
    date: LocalDate
  )(response: Either[Error, Option[TaxYear]]) =
    (mockTaxYearService
      .taxYear(_: LocalDate)(_: HeaderCarrier))
      .expects(date, *)
      .returning(EitherT.fromEither[Future](response))

  def isValidJourney(journeyStatus: JourneyStatus): Boolean =
    journeyStatus match {
      case r: StartingNewDraftReturn if r.newReturnTriageAnswers.isLeft       => true
      case FillingOutReturn(_, _, _, _: DraftMultipleDisposalsReturn)         => true
      case FillingOutReturn(_, _, _, _: DraftMultipleIndirectDisposalsReturn) => true
      case _                                                                  => false
    }

  def setIndividualUserType(
    displayType: UserTypeDisplay
  ): Option[IndividualUserType] =
    if (displayType.representativeType.exists(_.isLeft))
      Some(PersonalRepresentative)
    else if (displayType.representativeType.exists(_.isRight))
      Some(Capacitor)
    else
      Some(Self)

  def sessionDataWithStartingNewDraftReturn(
    multipleDisposalsAnswers: MultipleDisposalsTriageAnswers,
    name: Either[TrustName, IndividualName] = Right(sample[IndividualName]),
    userType: UserType = UserType.Individual,
    representativeType: Option[
      Either[PersonalRepresentative.type, Capacitor.type]
    ] = None
  ): (SessionData, StartingNewDraftReturn) = {
    val startingNewDraftReturn = sample[StartingNewDraftReturn].copy(
      newReturnTriageAnswers = Left(multipleDisposalsAnswers),
      subscribedDetails = sample[SubscribedDetails].copy(name = name),
      agentReferenceNumber =
        if (userType === UserType.Agent) Some(sample[AgentReferenceNumber])
        else None,
      representeeAnswers =
        if (representativeType.exists(_.isLeft))
          Some(
            sample[CompleteRepresenteeAnswers]
              .copy(dateOfDeath = Some(sample[DateOfDeath]))
          )
        else if (representativeType.exists(_.isRight))
          Some(sample[CompleteRepresenteeAnswers].copy(dateOfDeath = None))
        else None
    )
    SessionData.empty.copy(
      journeyStatus = Some(startingNewDraftReturn),
      userType = Some(userType)
    ) -> startingNewDraftReturn
  }

  def sessionDataWithFillingOutReturn(
    multipleDisposalsAnswers: MultipleDisposalsTriageAnswers,
    name: Either[TrustName, IndividualName] = Right(sample[IndividualName]),
    userType: UserType = UserType.Individual,
    representativeType: Option[
      Either[PersonalRepresentative.type, Capacitor.type]
    ] = None
  ): (SessionData, FillingOutReturn, DraftMultipleDisposalsReturn) = {
    val draftReturn = sample[DraftMultipleDisposalsReturn].copy(
      triageAnswers = multipleDisposalsAnswers,
      representeeAnswers =
        if (representativeType.exists(_.isLeft))
          Some(
            sample[CompleteRepresenteeAnswers]
              .copy(dateOfDeath = Some(sample[DateOfDeath]))
          )
        else if (representativeType.exists(_.isRight))
          Some(sample[CompleteRepresenteeAnswers].copy(dateOfDeath = None))
        else None
    )
    val journey     = sample[FillingOutReturn].copy(
      draftReturn = draftReturn,
      subscribedDetails = sample[SubscribedDetails].copy(name = name),
      agentReferenceNumber =
        if (userType === UserType.Agent) Some(sample[AgentReferenceNumber])
        else None
    )
    val session     = SessionData.empty.copy(
      userType = Some(userType),
      journeyStatus = Some(journey)
    )
    (session, journey, draftReturn)
  }

  def testFormError(
    data: (String, String)*
  )(
    expectedErrorMessageKey: String,
    errorArgs: String*
  )(pageTitleKey: String, titleArgs: String*)(
    performAction: Seq[(String, String)] => Future[Result],
    currentSession: SessionData = sessionDataWithStartingNewDraftReturn(
      sample[IncompleteMultipleDisposalsTriageAnswers]
    )._1
  ): Unit = {
    inSequence {
      mockAuthWithNoRetrievals()
      mockGetSession(currentSession)
    }

    checkPageIsDisplayed(
      performAction(data),
      messageFromMessageKey(pageTitleKey, titleArgs),
      { doc =>
        doc
          .select("#error-summary-display > ul > li > a")
          .text() shouldBe messageFromMessageKey(
          expectedErrorMessageKey,
          errorArgs: _*
        )
        doc.title() should startWith("Error:")
      },
      BAD_REQUEST
    )
  }

  def mockGenerateUUID(uuid: UUID): Unit =
    (mockUUIDGenerator.nextId _)
      .expects()
      .returning(uuid)

  "MultipleDisposalsTriageController" when {

    "handling requests to display the guidance page" must {

      def performAction(): Future[Result] =
        controller.guidance()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(
        performAction,
        isValidJourney
      )

      "display the page" when {

        def test(
          session: SessionData,
          expectedBackLink: Call,
          expectReturnToSummaryLink: Boolean,
          displayType: UserTypeDisplay
        ): Unit =
          testPageIsDisplayed(
            performAction,
            session,
            s"multiple-disposals.guidance${displayType.getSubKey}.title",
            routes.MultipleDisposalsTriageController.guidanceSubmit(),
            expectedBackLink,
            "button.continue",
            expectReturnToSummaryLink
          )

        "the user has not started a draft return yet and" when {
          "the section is incomplete" in {

            test(
              sessionDataWithStartingNewDraftReturn(
                IncompleteMultipleDisposalsTriageAnswers.empty,
                name = Right(sample[IndividualName])
              )._1,
              triage.routes.CommonTriageQuestionsController.howManyProperties(),
              expectReturnToSummaryLink = false,
              individualDisplay
            )
          }

          "the section is complete" in {
            List(
              trustDisplay,
              agentDisplay,
              individualDisplay,
              personalRepDisplay,
              capacitorDisplay
            ).foreach { displayType: UserTypeDisplay =>
              withClue(
                s"For user name ${displayType.name.fold(_ => "Trust name", _ => "Individual name")} ${displayType.getSubKey} "
              ) {

                val answers = sample[CompleteMultipleDisposalsTriageAnswers]
                  .copy(individualUserType = setIndividualUserType(displayType))

                test(
                  sessionDataWithStartingNewDraftReturn(
                    answers,
                    displayType.name,
                    displayType.userType,
                    displayType.representativeType
                  )._1,
                  triage.routes.MultipleDisposalsTriageController
                    .checkYourAnswers(),
                  expectReturnToSummaryLink = false,
                  displayType
                )
              }
            }
          }
        }
        "the user has started a draft return yet and" when {

          "the section is incomplete" in {

            test(
              sessionDataWithFillingOutReturn(
                IncompleteMultipleDisposalsTriageAnswers.empty
                  .copy(individualUserType = Some(Self))
              )._1,
              triage.routes.CommonTriageQuestionsController.howManyProperties(),
              expectReturnToSummaryLink = true,
              individualDisplay
            )
          }

          "the section is complete" in {
            List(
              trustDisplay,
              agentDisplay,
              individualDisplay,
              personalRepDisplay,
              capacitorDisplay
            ).foreach { displayType: UserTypeDisplay =>
              withClue(
                s"For user type ${displayType.getSubKey} "
              ) {
                test(
                  sessionDataWithFillingOutReturn(
                    sample[CompleteMultipleDisposalsTriageAnswers]
                      .copy(individualUserType = setIndividualUserType(displayType)),
                    displayType.name,
                    displayType.userType,
                    displayType.representativeType
                  )._1,
                  triage.routes.MultipleDisposalsTriageController
                    .checkYourAnswers(),
                  expectReturnToSummaryLink = true,
                  displayType
                )
              }
            }
          }
        }
      }
    }

    "handling submits on the guidance page" must {

      def performAction(): Future[Result] =
        controller.guidanceSubmit()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(
        performAction,
        isValidJourney
      )

      "redirect to how many disposals page" when {

        "the user has not completed the section" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithStartingNewDraftReturn(
                IncompleteMultipleDisposalsTriageAnswers.empty
                  .copy(individualUserType = Some(Self)),
                name = Right(sample[IndividualName])
              )._1
            )
          }

          checkIsRedirect(
            performAction(),
            routes.MultipleDisposalsTriageController.howManyDisposals()
          )
        }

      }

      "redirect to check your answers page" when {

        "the user has completed the section" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithStartingNewDraftReturn(
                sample[CompleteMultipleDisposalsTriageAnswers]
              )._1
            )
          }

          checkIsRedirect(
            performAction(),
            routes.MultipleDisposalsTriageController.checkYourAnswers()
          )
        }
      }

    }

    "handling requests to display the how many disposals page" must {

      def performAction(): Future[Result] =
        controller.howManyDisposals()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(
        performAction,
        isValidJourney
      )

      "display the page" when {

        def test(
          session: SessionData,
          expectedBackLink: Call,
          expectedButtonMessageKey: String,
          expectReturnToSummaryLink: Boolean
        ): Unit =
          testPageIsDisplayed(
            performAction,
            session,
            "multipleDisposalsNumberOfProperties.title",
            routes.MultipleDisposalsTriageController.howManyDisposalsSubmit(),
            expectedBackLink,
            expectedButtonMessageKey,
            expectReturnToSummaryLink
          )

        "the user has not started a new draft return and" when {

          "the journey is incomplete" in {
            test(
              sessionDataWithStartingNewDraftReturn(
                IncompleteMultipleDisposalsTriageAnswers.empty
                  .copy(individualUserType = Some(Self)),
                name = Right(sample[IndividualName])
              )._1,
              triage.routes.MultipleDisposalsTriageController.guidance(),
              "button.continue",
              expectReturnToSummaryLink = false
            )
          }

          "the journey is complete" in {
            test(
              sessionDataWithStartingNewDraftReturn(
                sample[CompleteMultipleDisposalsTriageAnswers]
                  .copy(individualUserType = Some(Self)),
                name = Right(sample[IndividualName])
              )._1,
              triage.routes.MultipleDisposalsTriageController
                .checkYourAnswers(),
              "button.continue",
              expectReturnToSummaryLink = false
            )
          }

        }

        "the user has started a new draft return and" when {

          "the journey is incomplete" in {
            test(
              sessionDataWithFillingOutReturn(
                IncompleteMultipleDisposalsTriageAnswers.empty
                  .copy(individualUserType = Some(Self)),
                name = Right(sample[IndividualName])
              )._1,
              triage.routes.MultipleDisposalsTriageController.guidance(),
              "button.saveAndContinue",
              expectReturnToSummaryLink = true
            )
          }

          "the journey is complete" in {
            test(
              sessionDataWithFillingOutReturn(
                sample[CompleteMultipleDisposalsTriageAnswers]
                  .copy(individualUserType = Some(Self)),
                name = Right(sample[IndividualName])
              )._1,
              triage.routes.MultipleDisposalsTriageController
                .checkYourAnswers(),
              "button.saveAndContinue",
              expectReturnToSummaryLink = true
            )
          }
        }

      }

    }

    "handling submits on the how many disposals page" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.howManyDisposalsSubmit()(
          FakeRequest().withFormUrlEncodedBody(data: _*)
        )

      val key = "multipleDisposalsNumberOfProperties"

      "redirect to single disposal cya page" when {

        "user enters number of properties as one" in {
          val (session, journey) = sessionDataWithStartingNewDraftReturn(
            IncompleteMultipleDisposalsTriageAnswers.empty
              .copy(individualUserType = Some(Self)),
            name = Right(sample[IndividualName])
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreSession(
              session.copy(journeyStatus =
                Some(
                  journey.copy(
                    newReturnTriageAnswers = Right(
                      IncompleteSingleDisposalTriageAnswers.empty.copy(
                        individualUserType = Some(Self),
                        hasConfirmedSingleDisposal = true
                      )
                    )
                  )
                )
              )
            )(Right(()))
          }

          checkIsRedirect(
            performAction(key -> "1"),
            routes.SingleDisposalsTriageController.checkYourAnswers()
          )

        }
      }

      "redirect to cya page" when {

        "the user has not started a draft return and" when {

          "they have not answered how many disposals section and " +
            "enters number of properties more than one" in {
            val answers            = IncompleteMultipleDisposalsTriageAnswers.empty
              .copy(individualUserType = Some(Self))
            val (session, journey) = sessionDataWithStartingNewDraftReturn(
              answers,
              name = Right(sample[IndividualName])
            )

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockStoreSession(
                session.copy(journeyStatus =
                  Some(
                    journey.copy(
                      newReturnTriageAnswers = Left(answers.copy(numberOfProperties = Some(5)))
                    )
                  )
                )
              )(Right(()))
            }

            checkIsRedirect(
              performAction(key -> "5"),
              routes.MultipleDisposalsTriageController.checkYourAnswers()
            )
          }

          "they have already completed the section and " +
            "re-enters different number of properties value for more than one" in {
            val answers            = sample[CompleteMultipleDisposalsTriageAnswers]
              .copy(individualUserType = Some(Self), numberOfProperties = 9)
            val newAnswers         =
              IncompleteMultipleDisposalsTriageAnswers.empty.copy(
                individualUserType = Some(Self),
                numberOfProperties = Some(3)
              )
            val (session, journey) = sessionDataWithStartingNewDraftReturn(
              answers,
              name = Right(sample[IndividualName])
            )

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockStoreSession(
                session.copy(journeyStatus =
                  Some(
                    journey.copy(newReturnTriageAnswers = Left(newAnswers.copy(individualUserType = Some(Self))))
                  )
                )
              )(Right(()))
            }

            checkIsRedirect(
              performAction(key -> "3"),
              routes.MultipleDisposalsTriageController.checkYourAnswers()
            )
          }

        }

        "the user has started a draft return and" when {

          "they have not completed the section and they enter a figure which is " +
            "different than one they have already entered" in {
            val answers                         = IncompleteMultipleDisposalsTriageAnswers.empty.copy(
              individualUserType = Some(Self),
              numberOfProperties = Some(2)
            )
            val (session, journey, draftReturn) =
              sessionDataWithFillingOutReturn(answers)

            val updatedDraftReturn = draftReturn.copy(
              triageAnswers = answers.copy(numberOfProperties = Some(5)),
              examplePropertyDetailsAnswers = None,
              exemptionAndLossesAnswers = None,
              yearToDateLiabilityAnswers = None,
              supportingEvidenceAnswers = None,
              lastUpdatedDate = TimeUtils.today()
            )
            val updatedJourney     = journey.copy(draftReturn = updatedDraftReturn)

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockStoreDraftReturn(
                updatedDraftReturn,
                journey.subscribedDetails.cgtReference,
                journey.agentReferenceNumber
              )(
                Right(())
              )
              mockStoreSession(
                session.copy(journeyStatus = Some(updatedJourney))
              )(Right(()))
            }

            checkIsRedirect(
              performAction(key -> "5"),
              routes.MultipleDisposalsTriageController.checkYourAnswers()
            )
          }

        }

      }

      "not update the session" when {

        "user has already answered how many disposals section and " +
          "re-enters same number of properties value for more than one" in {
          val answers = sample[CompleteMultipleDisposalsTriageAnswers].copy(
            individualUserType = Some(Self),
            numberOfProperties = 5
          )

          val (session, _) = sessionDataWithStartingNewDraftReturn(
            answers,
            name = Right(sample[IndividualName])
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkIsRedirect(
            performAction(key -> "5"),
            routes.MultipleDisposalsTriageController.checkYourAnswers()
          )
        }
      }

      "display form error" when {

        def test(data: (String, String)*)(expectedErrorMessageKey: String) =
          testFormError(data: _*)(expectedErrorMessageKey)(s"$key.title")(
            performAction
          )

        "the user submits nothing" in {
          val answers      = IncompleteMultipleDisposalsTriageAnswers.empty.copy(
            individualUserType = Some(Self)
          )
          val (session, _) = sessionDataWithStartingNewDraftReturn(
            answers,
            name = Right(sample[IndividualName])
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          val result = performAction()
          status(result) shouldBe BAD_REQUEST
        }

        "display form error when user enters numberOfProperties value <= 0" in {
          test(key -> "-5")(s"$key.error.tooSmall")

        }

        "display form error when user enters numberOfProperties value > 999" in {
          test(key -> "1000")(s"$key.error.tooLong")
        }

        "display form error when user enters invalid data" in {
          test(key -> "!@£!")(s"$key.error.invalid")
        }

      }

      "show a technical error page" when {

        val answers                         = IncompleteMultipleDisposalsTriageAnswers.empty.copy(
          individualUserType = Some(Self),
          numberOfProperties = None
        )
        val (session, journey, draftReturn) =
          sessionDataWithFillingOutReturn(answers)
        val updatedDraftReturn              = draftReturn.copy(
          triageAnswers = answers.copy(numberOfProperties = Some(5)),
          examplePropertyDetailsAnswers = None,
          exemptionAndLossesAnswers = None,
          yearToDateLiabilityAnswers = None,
          supportingEvidenceAnswers = None,
          lastUpdatedDate = TimeUtils.today()
        )
        val updatedJourney                  = journey.copy(draftReturn = updatedDraftReturn)

        "there is an error storing the draft return" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(
              updatedDraftReturn,
              journey.subscribedDetails.cgtReference,
              journey.agentReferenceNumber
            )(
              Left(Error(""))
            )
          }

          checkIsTechnicalErrorPage(performAction(key -> "5"))
        }

        "there is an error updating the session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(
              updatedDraftReturn,
              journey.subscribedDetails.cgtReference,
              journey.agentReferenceNumber
            )(
              Right(())
            )
            mockStoreSession(
              session.copy(journeyStatus = Some(updatedJourney))
            )(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction(key -> "5"))

        }

      }

    }

    "handling requests to display the were uk resident page" must {

      def performAction(): Future[Result] =
        controller.wereYouAUKResident()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(
        performAction,
        isValidJourney
      )

      "display the page" when {

        def test(
          session: SessionData,
          expectedBackLink: Call,
          expectedButtonMessageKey: String,
          expectReturnToSummaryLink: Boolean,
          displayType: UserTypeDisplay = individualDisplay
        ): Unit =
          testPageIsDisplayed(
            performAction,
            session,
            s"multipleDisposalsWereYouAUKResident${displayType.getSubKey}.title",
            routes.MultipleDisposalsTriageController.wereYouAUKResidentSubmit(),
            expectedBackLink,
            expectedButtonMessageKey,
            expectReturnToSummaryLink,
            List(
              SelectorAndValue(
                "#wrapper > div:eq(1) > article > form > p",
                messageFromMessageKey(
                  s"multipleDisposalsWereYouAUKResident${displayType.getSubKey}.link",
                  viewConfig.workOurYouResidenceStatusUrl
                )
              )
            )
          )

        "the user has not started a new draft return and" when {

          "the journey is incomplete" in {
            List(
              trustDisplay,
              agentDisplay,
              individualDisplay,
              personalRepDisplay,
              capacitorDisplay
            ).foreach { displayType: UserTypeDisplay =>
              withClue(
                s"For user type ${displayType.getSubKey}"
              ) {
                test(
                  sessionDataWithStartingNewDraftReturn(
                    IncompleteMultipleDisposalsTriageAnswers.empty
                      .copy(individualUserType = setIndividualUserType(displayType)),
                    displayType.name,
                    displayType.userType,
                    displayType.representativeType
                  )._1.copy(userType = Some(displayType.userType)),
                  triage.routes.MultipleDisposalsTriageController
                    .howManyDisposals(),
                  "button.continue",
                  expectReturnToSummaryLink = false,
                  displayType
                )
              }
            }
          }
          "the journey is complete" in {
            List(
              trustDisplay,
              agentDisplay,
              individualDisplay,
              personalRepDisplay,
              capacitorDisplay
            ).foreach { displayType: UserTypeDisplay =>
              withClue(
                s"For user type ${displayType.getSubKey}"
              ) {
                test(
                  sessionDataWithStartingNewDraftReturn(
                    sample[CompleteMultipleDisposalsTriageAnswers]
                      .copy(individualUserType = setIndividualUserType(displayType)),
                    displayType.name,
                    displayType.userType,
                    displayType.representativeType
                  )._1.copy(userType = Some(displayType.userType)),
                  triage.routes.MultipleDisposalsTriageController
                    .checkYourAnswers(),
                  "button.continue",
                  expectReturnToSummaryLink = false,
                  displayType
                )
              }
            }
          }

        }

        "the user has started a new draft return and" when {

          "the journey is incomplete" in {
            test(
              sessionDataWithFillingOutReturn(
                IncompleteMultipleDisposalsTriageAnswers.empty
              )._1,
              triage.routes.MultipleDisposalsTriageController
                .howManyDisposals(),
              "button.saveAndContinue",
              expectReturnToSummaryLink = true
            )
          }

          "the journey is complete" in {
            List(
              trustDisplay,
              agentDisplay,
              individualDisplay,
              personalRepDisplay,
              capacitorDisplay
            ).foreach { displayType: UserTypeDisplay =>
              withClue(
                s"For user type ${displayType.getSubKey}"
              ) {
                test(
                  sessionDataWithFillingOutReturn(
                    sample[CompleteMultipleDisposalsTriageAnswers]
                      .copy(individualUserType = setIndividualUserType(displayType)),
                    displayType.name,
                    displayType.userType,
                    displayType.representativeType
                  )._1.copy(userType = Some(displayType.userType)),
                  triage.routes.MultipleDisposalsTriageController
                    .checkYourAnswers(),
                  "button.saveAndContinue",
                  expectReturnToSummaryLink = true,
                  displayType
                )
              }
            }
          }
        }

      }

    }

    "handling submits on the were uk resident page" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.wereYouAUKResidentSubmit()(
          FakeRequest().withFormUrlEncodedBody(data: _*)
        )

      def updateDraftReturn(
        d: DraftMultipleDisposalsReturn,
        newAnswers: MultipleDisposalsTriageAnswers
      ) =
        d.copy(
          triageAnswers = newAnswers,
          examplePropertyDetailsAnswers = d.examplePropertyDetailsAnswers.map(
            _.unset(_.address)
              .unset(_.disposalPrice)
              .unset(_.acquisitionPrice)
          ),
          yearToDateLiabilityAnswers = None,
          supportingEvidenceAnswers = None
        )

      val key = "multipleDisposalsWereYouAUKResident"

      "redirect to redirect to cya page" when {

        val answers = IncompleteMultipleDisposalsTriageAnswers.empty.copy(
          individualUserType = Some(Self),
          numberOfProperties = Some(2)
        )

        "the user has not started a draft return and" when {

          val (session, journey) =
            sessionDataWithStartingNewDraftReturn(answers)

          "they have not answered the were uk resident question and selects true" in {

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockStoreSession(
                session.copy(journeyStatus =
                  Some(
                    journey.copy(
                      newReturnTriageAnswers = Left(answers.copy(wasAUKResident = Some(true)))
                    )
                  )
                )
              )(Right(()))
            }

            checkIsRedirect(
              performAction(key -> "true"),
              routes.MultipleDisposalsTriageController.checkYourAnswers()
            )
          }

          "they have not answered the were uk resident question and selects false" in {

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockStoreSession(
                session.copy(journeyStatus =
                  Some(
                    journey.copy(
                      newReturnTriageAnswers = Left(answers.copy(wasAUKResident = Some(false)))
                    )
                  )
                )
              )(Right(()))
            }

            checkIsRedirect(
              performAction(key -> "false"),
              routes.MultipleDisposalsTriageController.checkYourAnswers()
            )
          }

          "they have already answered were uk resident section and re-selected different option" in {
            val answers = sample[IncompleteMultipleDisposalsTriageAnswers]
              .copy(
                individualUserType = Some(Self),
                wasAUKResident = Some(true),
                countryOfResidence = Some(Country.uk)
              )

            val (session, journey) =
              sessionDataWithStartingNewDraftReturn(answers)

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockStoreSession(
                session.copy(journeyStatus =
                  Some(
                    journey.copy(
                      newReturnTriageAnswers = Left(
                        answers.copy(
                          wasAUKResident = Some(false),
                          countryOfResidence = None,
                          wereAllPropertiesResidential = None,
                          assetTypes = None
                        )
                      )
                    )
                  )
                )
              )(Right(()))
            }

            checkIsRedirect(
              performAction(key -> "false"),
              routes.MultipleDisposalsTriageController.checkYourAnswers()
            )
          }

        }

        "the user has started a draft return and" when {

          "have completed the section and they enter a figure which is " +
            "different than one they have already entered" in {
            forAll { c: CompleteMultipleDisposalsTriageAnswers =>
              val answers                         = c.copy(countryOfResidence = sample[Country])
              val (session, journey, draftReturn) =
                sessionDataWithFillingOutReturn(answers)

              val updatedAnswers     = IncompleteMultipleDisposalsTriageAnswers(
                individualUserType = answers.individualUserType,
                numberOfProperties = Some(answers.numberOfProperties),
                wasAUKResident = Some(true),
                countryOfResidence = None,
                wereAllPropertiesResidential = None,
                assetTypes = None,
                taxYearAfter6April2020 = Some(true),
                taxYear = Some(answers.taxYear),
                completionDate = Some(answers.completionDate)
              )
              val updatedDraftReturn =
                updateDraftReturn(draftReturn, updatedAnswers)
              val updatedJourney     =
                journey.copy(draftReturn = updatedDraftReturn)

              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
                mockStoreDraftReturn(
                  updatedDraftReturn,
                  journey.subscribedDetails.cgtReference,
                  journey.agentReferenceNumber
                )(
                  Right(())
                )
                mockStoreSession(
                  session.copy(journeyStatus = Some(updatedJourney))
                )(Right(()))
              }

              checkIsRedirect(
                performAction(key -> "true"),
                routes.MultipleDisposalsTriageController.checkYourAnswers()
              )
            }
          }
        }

      }

      "show a form error" when {

        "the user submits nothing" in {
          List(
            trustDisplay,
            agentDisplay,
            individualDisplay,
            capacitorDisplay,
            personalRepDisplay
          ).foreach { displayType: UserTypeDisplay =>
            withClue(
              s"For user type ${displayType.getSubKey}"
            ) {
              val answers = IncompleteMultipleDisposalsTriageAnswers.empty.copy(
                individualUserType = setIndividualUserType(displayType),
                numberOfProperties = Some(2)
              )
              val session = sessionDataWithStartingNewDraftReturn(
                answers,
                displayType.name,
                displayType.userType,
                displayType.representativeType
              )._1.copy(userType = Some(displayType.userType))

              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
              }

              checkPageIsDisplayed(
                performAction(),
                messageFromMessageKey(s"$key${displayType.getSubKey}.title"),
                doc =>
                  doc
                    .select("#error-summary-display > ul > li > a")
                    .text() shouldBe messageFromMessageKey(
                    s"$key${displayType.getSubKey}.error.required"
                  ),
                BAD_REQUEST
              )
            }
          }
        }
      }

      "show an error page" when {

        val answers                         = sample[CompleteMultipleDisposalsTriageAnswers]
          .copy(countryOfResidence = sample[Country])
        val (session, journey, draftReturn) =
          sessionDataWithFillingOutReturn(answers)

        val updatedAnswers     = IncompleteMultipleDisposalsTriageAnswers(
          individualUserType = answers.individualUserType,
          numberOfProperties = Some(answers.numberOfProperties),
          wasAUKResident = Some(true),
          countryOfResidence = None,
          wereAllPropertiesResidential = None,
          assetTypes = None,
          taxYearAfter6April2020 = Some(true),
          taxYear = Some(answers.taxYear),
          completionDate = Some(answers.completionDate)
        )
        val updatedDraftReturn = updateDraftReturn(draftReturn, updatedAnswers)
        val updatedJourney     = journey.copy(draftReturn = updatedDraftReturn)

        "there is an error updating the draft return" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(
              updatedDraftReturn,
              journey.subscribedDetails.cgtReference,
              journey.agentReferenceNumber
            )(
              Left(Error(""))
            )
          }

          checkIsTechnicalErrorPage(performAction(key -> "true"))
        }

        "there is an error updating the session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(
              updatedDraftReturn,
              journey.subscribedDetails.cgtReference,
              journey.agentReferenceNumber
            )(
              Right(())
            )
            mockStoreSession(
              session.copy(journeyStatus = Some(updatedJourney))
            )(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction(key -> "true"))
        }

      }

    }

    "handling requests to display the were all properties residential page" must {

      def performAction(): Future[Result] =
        controller.wereAllPropertiesResidential()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(
        performAction,
        isValidJourney
      )

      "display the page" when {

        def test(
          session: SessionData,
          expectedBackLink: Call,
          expectedButtonMessageKey: String,
          expectReturnToSummaryLink: Boolean
        ): Unit =
          testPageIsDisplayed(
            performAction,
            session,
            "multipleDisposalsWereAllPropertiesResidential.title",
            routes.MultipleDisposalsTriageController
              .wereAllPropertiesResidentialSubmit(),
            expectedBackLink,
            expectedButtonMessageKey,
            expectReturnToSummaryLink
          )

        "the user has not started a new draft return and" when {

          "the journey is incomplete" in {
            test(
              sessionDataWithStartingNewDraftReturn(
                IncompleteMultipleDisposalsTriageAnswers.empty
              )._1,
              triage.routes.MultipleDisposalsTriageController
                .wereYouAUKResident(),
              "button.continue",
              expectReturnToSummaryLink = false
            )
          }

          "the journey is complete" in {
            test(
              sessionDataWithStartingNewDraftReturn(
                sample[CompleteMultipleDisposalsTriageAnswers]
              )._1,
              triage.routes.MultipleDisposalsTriageController
                .checkYourAnswers(),
              "button.continue",
              expectReturnToSummaryLink = false
            )
          }

        }

        "the user has started a new draft return and" when {

          "the journey is incomplete" in {
            test(
              sessionDataWithFillingOutReturn(
                IncompleteMultipleDisposalsTriageAnswers.empty
              )._1,
              triage.routes.MultipleDisposalsTriageController
                .wereYouAUKResident(),
              "button.saveAndContinue",
              expectReturnToSummaryLink = true
            )
          }

          "the journey is complete" in {
            test(
              sessionDataWithFillingOutReturn(
                sample[CompleteMultipleDisposalsTriageAnswers]
              )._1,
              triage.routes.MultipleDisposalsTriageController
                .checkYourAnswers(),
              "button.saveAndContinue",
              expectReturnToSummaryLink = true
            )
          }
        }

      }

    }

    "handling submits on the were all properties residential page" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.wereAllPropertiesResidentialSubmit()(
          FakeRequest().withFormUrlEncodedBody(data: _*)
        )

      val key = "multipleDisposalsWereAllPropertiesResidential"

      "redirect to redirect to cya page" when {

        val answers = IncompleteMultipleDisposalsTriageAnswers.empty.copy(
          individualUserType = Some(Self),
          numberOfProperties = Some(2),
          wasAUKResident = Some(true),
          countryOfResidence = Some(Country.uk)
        )

        "the user has not started a draft return and" when {

          val (session, journey) =
            sessionDataWithStartingNewDraftReturn(answers)

          "user has not answered the were all properties residential section and selects true" in {

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockStoreSession(
                session.copy(journeyStatus =
                  Some(
                    journey.copy(
                      newReturnTriageAnswers = Left(
                        answers.copy(
                          wereAllPropertiesResidential = Some(true),
                          assetTypes = Some(List(AssetType.Residential))
                        )
                      )
                    )
                  )
                )
              )(Right(()))
            }

            checkIsRedirect(
              performAction(key -> "true"),
              routes.MultipleDisposalsTriageController.checkYourAnswers()
            )
          }

          "user has not answered the were all properties residential section and selects false" in {

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockStoreSession(
                session.copy(journeyStatus =
                  Some(
                    journey.copy(
                      newReturnTriageAnswers = Left(
                        answers.copy(
                          wereAllPropertiesResidential = Some(false),
                          assetTypes = Some(List(AssetType.NonResidential))
                        )
                      )
                    )
                  )
                )
              )(Right(()))
            }

            checkIsRedirect(
              performAction(key -> "false"),
              routes.MultipleDisposalsTriageController.checkYourAnswers()
            )
          }

          "user has already answered were all properties residential section and re-selected different option" in {
            val answers = sample[IncompleteMultipleDisposalsTriageAnswers]
              .copy(
                wereAllPropertiesResidential = Some(true),
                assetTypes = Some(List(AssetType.Residential))
              )

            val (session, journey) =
              sessionDataWithStartingNewDraftReturn(answers)

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockStoreSession(
                session.copy(journeyStatus =
                  Some(
                    journey.copy(
                      newReturnTriageAnswers = Left(
                        answers.copy(
                          wereAllPropertiesResidential = Some(false),
                          assetTypes = Some(List(AssetType.NonResidential))
                        )
                      )
                    )
                  )
                )
              )(Right(()))
            }

            checkIsRedirect(
              performAction(key -> "false"),
              routes.MultipleDisposalsTriageController.checkYourAnswers()
            )
          }
        }

        "the user has started a draft return and" when {

          "have completed the section and they enter a figure which is " +
            "different than one they have already entered" in {
            val answers                         = sample[CompleteMultipleDisposalsTriageAnswers].copy(
              countryOfResidence = Country.uk,
              assetTypes = List(AssetType.Residential)
            )
            val (session, journey, draftReturn) =
              sessionDataWithFillingOutReturn(answers)

            val updatedAnswers     = IncompleteMultipleDisposalsTriageAnswers(
              individualUserType = answers.individualUserType,
              numberOfProperties = Some(answers.numberOfProperties),
              wasAUKResident = Some(true),
              countryOfResidence = None,
              wereAllPropertiesResidential = Some(false),
              assetTypes = Some(List(AssetType.NonResidential)),
              taxYearAfter6April2020 = Some(true),
              taxYear = Some(answers.taxYear),
              completionDate = Some(answers.completionDate)
            )
            val updatedDraftReturn =
              draftReturn.copy(triageAnswers = updatedAnswers)
            val updatedJourney     = journey.copy(draftReturn = updatedDraftReturn)

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockStoreDraftReturn(
                updatedDraftReturn,
                journey.subscribedDetails.cgtReference,
                journey.agentReferenceNumber
              )(
                Right(())
              )
              mockStoreSession(
                session.copy(journeyStatus = Some(updatedJourney))
              )(Right(()))
            }

            checkIsRedirect(
              performAction(key -> "false"),
              routes.MultipleDisposalsTriageController.checkYourAnswers()
            )
          }

        }

      }

      "not update the session" when {

        "user has already answered were all properties residential section and re-selected same option" in {
          val answers = sample[IncompleteMultipleDisposalsTriageAnswers]
            .copy(
              wereAllPropertiesResidential = Some(true),
              assetTypes = Some(List(AssetType.Residential))
            )

          val (session, _) = sessionDataWithStartingNewDraftReturn(answers)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkIsRedirect(
            performAction(key -> "true"),
            routes.MultipleDisposalsTriageController.checkYourAnswers()
          )
        }

      }

      "show a form error" when {

        "the user submits nothing" in {
          val answers      = IncompleteMultipleDisposalsTriageAnswers.empty.copy(
            individualUserType = Some(Self),
            numberOfProperties = Some(2),
            wasAUKResident = Some(true),
            countryOfResidence = Some(Country.uk)
          )
          val (session, _) = sessionDataWithStartingNewDraftReturn(answers)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(s"$key.title"),
            doc =>
              doc
                .select("#error-summary-display > ul > li > a")
                .text() shouldBe messageFromMessageKey(
                s"$key.error.required"
              ),
            BAD_REQUEST
          )
        }

      }

      "show a error page" when {

        val answers                         = sample[CompleteMultipleDisposalsTriageAnswers].copy(
          countryOfResidence = Country.uk,
          assetTypes = List(AssetType.Residential)
        )
        val (session, journey, draftReturn) =
          sessionDataWithFillingOutReturn(answers)

        val updatedAnswers     = IncompleteMultipleDisposalsTriageAnswers(
          individualUserType = answers.individualUserType,
          numberOfProperties = Some(answers.numberOfProperties),
          wasAUKResident = Some(true),
          countryOfResidence = None,
          wereAllPropertiesResidential = Some(false),
          assetTypes = Some(List(AssetType.NonResidential)),
          taxYearAfter6April2020 = Some(true),
          taxYear = Some(answers.taxYear),
          completionDate = Some(answers.completionDate)
        )
        val updatedDraftReturn =
          draftReturn.copy(triageAnswers = updatedAnswers)
        val updatedJourney     = journey.copy(draftReturn = updatedDraftReturn)

        "there is an error updating the draft return" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(
              updatedDraftReturn,
              journey.subscribedDetails.cgtReference,
              journey.agentReferenceNumber
            )(
              Left(Error(""))
            )
          }

          checkIsTechnicalErrorPage(performAction(key -> "false"))
        }

        "there is an error updating the session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(
              updatedDraftReturn,
              journey.subscribedDetails.cgtReference,
              journey.agentReferenceNumber
            )(
              Right(())
            )
            mockStoreSession(
              session.copy(journeyStatus = Some(updatedJourney))
            )(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction(key -> "false"))
        }

      }

    }

    "handling requests to display the tax year exchanged page" must {

      def performAction(): Future[Result] =
        controller.whenWereContractsExchanged()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(
        performAction,
        isValidJourney
      )

      "display the page" when {

        def test(
          session: SessionData,
          expectedBackLink: Call,
          expectedButtonMessageKey: String,
          expectReturnToSummaryLink: Boolean
        ): Unit =
          testPageIsDisplayed(
            performAction,
            session,
            "multipleDisposalsTaxYear.title",
            routes.MultipleDisposalsTriageController
              .whenWereContractsExchangedSubmit(),
            expectedBackLink,
            expectedButtonMessageKey,
            expectReturnToSummaryLink
          )

        val incompleteAnswers =
          IncompleteMultipleDisposalsTriageAnswers.empty.copy(
            individualUserType = Some(Self),
            numberOfProperties = Some(2),
            wasAUKResident = Some(true),
            countryOfResidence = Some(Country.uk),
            wereAllPropertiesResidential = Some(true),
            assetTypes = Some(List(AssetType.Residential))
          )

        "the user has not started a new draft return and" when {

          "the journey is incomplete" in {
            test(
              sessionDataWithStartingNewDraftReturn(incompleteAnswers)._1,
              triage.routes.MultipleDisposalsTriageController
                .wereAllPropertiesResidential(),
              "button.continue",
              expectReturnToSummaryLink = false
            )
          }

          "the journey is complete" in {
            test(
              sessionDataWithStartingNewDraftReturn(
                sample[CompleteMultipleDisposalsTriageAnswers]
              )._1,
              triage.routes.MultipleDisposalsTriageController
                .checkYourAnswers(),
              "button.continue",
              expectReturnToSummaryLink = false
            )
          }

        }

        "the user has started a new draft return and" when {

          "the journey is incomplete" in {
            test(
              sessionDataWithFillingOutReturn(incompleteAnswers)._1,
              triage.routes.MultipleDisposalsTriageController
                .wereAllPropertiesResidential(),
              "button.saveAndContinue",
              expectReturnToSummaryLink = true
            )
          }

          "the journey is complete" in {
            test(
              sessionDataWithFillingOutReturn(
                sample[CompleteMultipleDisposalsTriageAnswers]
              )._1,
              triage.routes.MultipleDisposalsTriageController
                .checkYourAnswers(),
              "button.saveAndContinue",
              expectReturnToSummaryLink = true
            )
          }
        }

      }
    }

    "handling submits on the tax year exchanged page" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.whenWereContractsExchangedSubmit()(
          FakeRequest().withFormUrlEncodedBody(data: _*)
        )

      val today = LocalDate.now(Clock.systemUTC())
      val key   = "multipleDisposalsTaxYear"

      "redirect to redirect to cya page" when {

        val answers = IncompleteMultipleDisposalsTriageAnswers.empty.copy(
          individualUserType = Some(Self),
          numberOfProperties = Some(2),
          wasAUKResident = Some(true),
          countryOfResidence = Some(Country.uk),
          wereAllPropertiesResidential = Some(true),
          assetTypes = Some(List(AssetType.Residential))
        )

        val taxYear = sample[TaxYear].copy(
          startDateInclusive = LocalDate.of(2019, 4, 6),
          endDateExclusive = LocalDate.of(2020, 4, 6)
        )

        "the user has not started a draft return and" when {

          val (session, journey) =
            sessionDataWithStartingNewDraftReturn(answers)

          "user has not answered the tax year exchanged section and selects after April 06th, 2020" in {

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockGetTaxYear(today)(Right(Some(taxYear)))
              mockStoreSession(
                session.copy(journeyStatus =
                  Some(
                    journey.copy(
                      newReturnTriageAnswers = Left(
                        answers.copy(
                          taxYearAfter6April2020 = Some(true),
                          taxYear = Some(taxYear)
                        )
                      )
                    )
                  )
                )
              )(Right(()))
            }

            checkIsRedirect(
              performAction(key -> "0"),
              routes.MultipleDisposalsTriageController.checkYourAnswers()
            )
          }

          "user has not answered the tax year exchanged section and selects before April 06th, 2020" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockStoreSession(
                session.copy(journeyStatus =
                  Some(
                    journey.copy(
                      newReturnTriageAnswers = Left(
                        answers.copy(
                          taxYearAfter6April2020 = Some(false),
                          taxYear = None
                        )
                      )
                    )
                  )
                )
              )(Right(()))
            }

            checkIsRedirect(
              performAction(key -> "1"),
              routes.MultipleDisposalsTriageController.checkYourAnswers()
            )
          }

          "user has already answered were all properties residential section and re-selected different option" in {
            val answers = sample[IncompleteMultipleDisposalsTriageAnswers]
              .copy(
                individualUserType = Some(Self),
                numberOfProperties = Some(2),
                wasAUKResident = Some(true),
                countryOfResidence = Some(Country.uk),
                wereAllPropertiesResidential = Some(true),
                assetTypes = Some(List(AssetType.Residential)),
                taxYearAfter6April2020 = Some(true),
                taxYear = Some(taxYear),
                completionDate = Some(sample[CompletionDate])
              )

            val (session, journey) =
              sessionDataWithStartingNewDraftReturn(answers)

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockStoreSession(
                session.copy(journeyStatus =
                  Some(
                    journey.copy(
                      newReturnTriageAnswers = Left(
                        answers.copy(
                          taxYearAfter6April2020 = Some(false),
                          taxYear = None,
                          completionDate = None
                        )
                      )
                    )
                  )
                )
              )(Right(()))
            }

            checkIsRedirect(
              performAction(key -> "1"),
              routes.MultipleDisposalsTriageController.checkYourAnswers()
            )
          }
        }

        "the user has started a draft return and" when {

          "have completed the section and they enter a figure which is " +
            "different than one they have already entered" in {
            forAll { c: CompleteMultipleDisposalsTriageAnswers =>
              val answers                         = c.copy(taxYear = taxYear)
              val (session, journey, draftReturn) =
                sessionDataWithFillingOutReturn(answers)

              val updatedAnswers     = IncompleteMultipleDisposalsTriageAnswers
                .fromCompleteAnswers(answers)
                .copy(
                  taxYearAfter6April2020 = Some(false),
                  taxYear = None,
                  completionDate = None
                )
              val updatedDraftReturn = draftReturn.copy(
                triageAnswers = updatedAnswers,
                examplePropertyDetailsAnswers = draftReturn.examplePropertyDetailsAnswers.map(
                  _.unset(_.disposalDate)
                )
              )
              val updatedJourney     =
                journey.copy(draftReturn = updatedDraftReturn)

              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
                mockStoreDraftReturn(
                  updatedDraftReturn,
                  journey.subscribedDetails.cgtReference,
                  journey.agentReferenceNumber
                )(
                  Right(())
                )
                mockStoreSession(
                  session.copy(journeyStatus = Some(updatedJourney))
                )(Right(()))
              }

              checkIsRedirect(
                performAction(key -> "1"),
                routes.MultipleDisposalsTriageController.checkYourAnswers()
              )
            }
          }

        }
      }

      "not update the session" when {

        "user has already answered the tax year exchanged section and re-selected same option" in {
          val taxYear = sample[TaxYear].copy(
            startDateInclusive = LocalDate.of(2019, 4, 6),
            endDateExclusive = LocalDate.of(2020, 4, 6)
          )
          val answers = sample[IncompleteMultipleDisposalsTriageAnswers]
            .copy(
              individualUserType = Some(Self),
              numberOfProperties = Some(2),
              wasAUKResident = Some(true),
              countryOfResidence = Some(Country.uk),
              wereAllPropertiesResidential = Some(true),
              assetTypes = Some(List(AssetType.Residential)),
              taxYearAfter6April2020 = Some(true),
              taxYear = Some(taxYear)
            )

          val (session, _) = sessionDataWithStartingNewDraftReturn(answers)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkIsRedirect(
            performAction(key -> "0"),
            routes.MultipleDisposalsTriageController.checkYourAnswers()
          )
        }

      }

      "show a form error" when {

        "the user submits nothing" in {
          val answers      = IncompleteMultipleDisposalsTriageAnswers.empty.copy(
            individualUserType = Some(Self),
            numberOfProperties = Some(2),
            wasAUKResident = Some(true),
            countryOfResidence = Some(Country.uk),
            wereAllPropertiesResidential = Some(true)
          )
          val (session, _) = sessionDataWithStartingNewDraftReturn(answers)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(s"$key.title"),
            { doc =>
              doc
                .select("#error-summary-display > ul > li > a")
                .text() shouldBe messageFromMessageKey(
                s"$key.error.required"
              )
              doc.title() should startWith("Error:")
            },
            BAD_REQUEST
          )
        }

      }

      "show an error page" when {

        val answers                         = sample[CompleteMultipleDisposalsTriageAnswers].copy(
          assetTypes = List(AssetType.Residential),
          taxYear = sample[TaxYear]
        )
        val (session, journey, draftReturn) =
          sessionDataWithFillingOutReturn(answers)

        val updatedAnswers     = IncompleteMultipleDisposalsTriageAnswers
          .fromCompleteAnswers(answers)
          .copy(
            taxYearAfter6April2020 = Some(false),
            taxYear = None,
            completionDate = None
          )
        val updatedDraftReturn = draftReturn.copy(
          triageAnswers = updatedAnswers,
          examplePropertyDetailsAnswers = draftReturn.examplePropertyDetailsAnswers.map(
            _.unset(_.disposalDate)
          )
        )
        val updatedJourney     = journey.copy(draftReturn = updatedDraftReturn)

        "there is an error updating the draft return" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(
              updatedDraftReturn,
              journey.subscribedDetails.cgtReference,
              journey.agentReferenceNumber
            )(
              Left(Error(""))
            )
          }

          checkIsTechnicalErrorPage(performAction(key -> "1"))
        }

        "there is an error updating the session data" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(
              updatedDraftReturn,
              journey.subscribedDetails.cgtReference,
              journey.agentReferenceNumber
            )(
              Right(())
            )
            mockStoreSession(
              session.copy(journeyStatus = Some(updatedJourney))
            )(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction(key -> "1"))
        }

        "a tax year cannot be found when the user selects after April 2020" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithFillingOutReturn(
                IncompleteMultipleDisposalsTriageAnswers.empty
              )._1
            )
            mockGetTaxYear(TimeUtils.today())(Right(None))
          }

          checkIsTechnicalErrorPage(performAction(key -> "0"))
        }

        "there is an error while getting the tax year when the user selects after April 2020" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithFillingOutReturn(
                IncompleteMultipleDisposalsTriageAnswers.empty
              )._1
            )
            mockGetTaxYear(TimeUtils.today())(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction(key -> "0"))
        }

      }
    }

    "handling requests to display the country of residence page" must {

      def performAction(): Future[Result] =
        controller.countryOfResidence()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(
        performAction,
        isValidJourney
      )

      "display the page" when {

        def test(
          session: SessionData,
          expectedBackLink: Call,
          expectedButtonMessageKey: String,
          expectReturnToSummaryLink: Boolean,
          displayType: UserTypeDisplay
        ): Unit               =
          testPageIsDisplayed(
            performAction,
            session,
            s"multipleDisposalsCountryOfResidence${displayType.getSubKey}.title",
            routes.MultipleDisposalsTriageController.countryOfResidenceSubmit(),
            expectedBackLink,
            expectedButtonMessageKey,
            expectReturnToSummaryLink,
            List(
              SelectorAndValue(
                "#countryCode-form-hint",
                messageFromMessageKey(
                  s"multipleDisposalsCountryOfResidence${displayType.getSubKey}.helpText"
                )
              ),
              SelectorAndValue(
                "#wrapper > div:eq(1) > article > form > p",
                messageFromMessageKey(
                  s"triage.enterCountry${displayType.getSubKey}.link",
                  viewConfig.workOurYouResidenceStatusUrl
                )
              )
            )
          )
        val incompleteAnswers =
          IncompleteMultipleDisposalsTriageAnswers.empty.copy(
            individualUserType = Some(Self),
            numberOfProperties = Some(2),
            wasAUKResident = Some(false)
          )
        "the user has not started a new draft return and" when {

          "the journey is incomplete" in {
            List(
              trustDisplay,
              agentDisplay,
              individualDisplay,
              capacitorDisplay,
              personalRepDisplay
            ).foreach { displayType: UserTypeDisplay =>
              withClue(
                s"For user type ${displayType.getSubKey}"
              ) {
                test(
                  sessionDataWithStartingNewDraftReturn(
                    incompleteAnswers.copy(
                      individualUserType = setIndividualUserType(displayType),
                      numberOfProperties = Some(2),
                      wasAUKResident = Some(false)
                    ),
                    displayType.name,
                    displayType.userType,
                    displayType.representativeType
                  )._1,
                  triage.routes.MultipleDisposalsTriageController
                    .wereYouAUKResident(),
                  "button.continue",
                  expectReturnToSummaryLink = false,
                  displayType
                )
              }
            }
          }

          "the journey is complete" in {
            List(
              trustDisplay,
              agentDisplay,
              individualDisplay,
              personalRepDisplay,
              capacitorDisplay
            ).foreach { displayType: UserTypeDisplay =>
              withClue(
                s"For user type ${displayType.getSubKey}"
              ) {
                test(
                  sessionDataWithStartingNewDraftReturn(
                    sample[CompleteMultipleDisposalsTriageAnswers]
                      .copy(individualUserType = setIndividualUserType(displayType)),
                    displayType.name,
                    displayType.userType,
                    displayType.representativeType
                  )._1,
                  triage.routes.MultipleDisposalsTriageController
                    .checkYourAnswers(),
                  "button.continue",
                  expectReturnToSummaryLink = false,
                  displayType
                )
              }
            }
          }

        }

        "the user has started a new draft return and" when {

          "the journey is incomplete" in {
            test(
              sessionDataWithFillingOutReturn(incompleteAnswers)._1,
              triage.routes.MultipleDisposalsTriageController
                .wereYouAUKResident(),
              "button.saveAndContinue",
              expectReturnToSummaryLink = true,
              individualDisplay
            )
          }

          "the journey is complete" in {
            test(
              sessionDataWithFillingOutReturn(
                sample[CompleteMultipleDisposalsTriageAnswers]
                  .copy(individualUserType = Some(Self))
              )._1,
              triage.routes.MultipleDisposalsTriageController
                .checkYourAnswers(),
              "button.saveAndContinue",
              expectReturnToSummaryLink = true,
              individualDisplay
            )
          }
        }

      }

      "redirect to the cya page" in {
        mockAuthWithNoRetrievals()
        mockGetSession(
          sessionDataWithStartingNewDraftReturn(
            IncompleteMultipleDisposalsTriageAnswers.empty.copy(
              individualUserType = Some(Self),
              numberOfProperties = Some(2),
              wasAUKResident = Some(true)
            )
          )._1
        )

        checkIsRedirect(
          performAction(),
          routes.MultipleDisposalsTriageController.checkYourAnswers()
        )
      }

    }

    "handling submits on the country of residence page" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.countryOfResidenceSubmit()(
          FakeRequest().withFormUrlEncodedBody(data: _*)
        )

      def updateDraftReturn(
        d: DraftMultipleDisposalsReturn,
        newAnswers: MultipleDisposalsTriageAnswers
      ) =
        d.copy(
          triageAnswers = newAnswers,
          yearToDateLiabilityAnswers = d.yearToDateLiabilityAnswers.flatMap {
            case _: CalculatedYTDAnswers    => None
            case n: NonCalculatedYTDAnswers =>
              Some(n.unset(_.hasEstimatedDetails).unset(_.taxDue))
          }
        )

      val key                        = "countryCode"
      val (countryCode, countryName) = "HK" -> "Hong Kong"
      val country                    = Country(countryCode, Some(countryName))

      "redirect to redirect to cya page" when {

        val answers = IncompleteMultipleDisposalsTriageAnswers.empty.copy(
          individualUserType = Some(Self),
          numberOfProperties = Some(2),
          wasAUKResident = Some(false),
          countryOfResidence = None
        )

        "the user has not started a draft return and" when {

          val (session, journey) =
            sessionDataWithStartingNewDraftReturn(answers)

          "user has not answered the country of residence section and " +
            "enters valid country" in {

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockStoreSession(
                session.copy(journeyStatus =
                  Some(
                    journey.copy(
                      newReturnTriageAnswers = Left(
                        answers.copy(
                          countryOfResidence = Some(country)
                        )
                      )
                    )
                  )
                )
              )(Right(()))
            }

            checkIsRedirect(
              performAction(key -> countryCode),
              routes.MultipleDisposalsTriageController.checkYourAnswers()
            )
          }

          "user has already answered the country of residence section and " +
            "re-selected different option" in {
            val answers = sample[IncompleteMultipleDisposalsTriageAnswers]
              .copy(
                individualUserType = Some(Self),
                countryOfResidence = Some(country),
                wereAllPropertiesResidential = None,
                assetTypes = None
              )

            val (session, journey) =
              sessionDataWithStartingNewDraftReturn(answers)

            val (newCountryCode, newCountryName) = "CH" -> "Switzerland"
            val newCountry                       = Country(newCountryCode, Some(newCountryName))

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockStoreSession(
                session.copy(journeyStatus =
                  Some(
                    journey.copy(
                      newReturnTriageAnswers = Left(
                        answers.copy(
                          countryOfResidence = Some(newCountry)
                        )
                      )
                    )
                  )
                )
              )(Right(()))
            }

            checkIsRedirect(
              performAction(key -> newCountryCode),
              routes.MultipleDisposalsTriageController.checkYourAnswers()
            )
          }

        }

        "the user has started a draft return and" when {

          "have completed the section and they enter a figure which is " +
            "different than one they have already entered" in {
            forAll { c: CompleteMultipleDisposalsTriageAnswers =>
              val answers = c.copy(countryOfResidence = Country("FI", None))

              val (session, journey, draftReturn) =
                sessionDataWithFillingOutReturn(answers)

              val updatedAnswers     = answers.copy(countryOfResidence = country)
              val updatedDraftReturn =
                updateDraftReturn(draftReturn, updatedAnswers)
              val updatedJourney     =
                journey.copy(draftReturn = updatedDraftReturn)

              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
                mockStoreDraftReturn(
                  updatedDraftReturn,
                  journey.subscribedDetails.cgtReference,
                  journey.agentReferenceNumber
                )(
                  Right(())
                )
                mockStoreSession(
                  session.copy(journeyStatus = Some(updatedJourney))
                )(Right(()))
              }

              checkIsRedirect(
                performAction(key -> country.code),
                routes.MultipleDisposalsTriageController.checkYourAnswers()
              )
            }

          }

        }

      }

      "not update the session" when {

        "user has already answered country of residence section and" +
          "re-selected same option" in {
          val answers = sample[IncompleteMultipleDisposalsTriageAnswers]
            .copy(
              numberOfProperties = Some(2),
              wasAUKResident = Some(false),
              countryOfResidence = Some(country)
            )

          val (session, _) = sessionDataWithStartingNewDraftReturn(answers)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkIsRedirect(
            performAction(key -> "HK"),
            routes.MultipleDisposalsTriageController.checkYourAnswers()
          )
        }

      }

      "show a form error" when {

        "the user submits nothing" in {
          val answers      = IncompleteMultipleDisposalsTriageAnswers.empty.copy(
            individualUserType = Some(Self),
            numberOfProperties = Some(2),
            wasAUKResident = Some(false),
            countryOfResidence = None
          )
          val (session, _) = sessionDataWithStartingNewDraftReturn(answers)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(s"multipleDisposalsCountryOfResidence.title"),
            { doc =>
              doc
                .select("#error-summary-display > ul > li > a")
                .text() shouldBe messageFromMessageKey(
                s"$key.error.required"
              )
              doc.title() should startWith("Error:")
            },
            BAD_REQUEST
          )
        }

        "the option is not recognised" in {
          val answers = IncompleteMultipleDisposalsTriageAnswers.empty
            .copy(
              numberOfProperties = Some(2),
              wasAUKResident = Some(false),
              countryOfResidence = None
            )

          val (session, _) = sessionDataWithStartingNewDraftReturn(answers)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkPageIsDisplayed(
            performAction(key -> "XX"),
            messageFromMessageKey(s"multipleDisposalsCountryOfResidence.title"),
            { doc =>
              doc
                .select("#error-summary-display > ul > li > a")
                .text() shouldBe messageFromMessageKey(
                s"$key.error.notFound"
              )
              doc.title() should startWith("Error:")
            },
            BAD_REQUEST
          )
        }

      }

      "show an error page" when {

        val answers                         = sample[CompleteMultipleDisposalsTriageAnswers].copy(
          countryOfResidence = sample[Country]
        )
        val (session, journey, draftReturn) =
          sessionDataWithFillingOutReturn(answers)

        val updatedAnswers     = answers.copy(countryOfResidence = country)
        val updatedDraftReturn = updateDraftReturn(draftReturn, updatedAnswers)
        val updatedJourney     = journey.copy(draftReturn = updatedDraftReturn)

        "there is an error updating the draft return" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(
              updatedDraftReturn,
              journey.subscribedDetails.cgtReference,
              journey.agentReferenceNumber
            )(
              Left(Error(""))
            )
          }

          checkIsTechnicalErrorPage(performAction(key -> country.code))
        }

        "there is an error updating the session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(
              updatedDraftReturn,
              journey.subscribedDetails.cgtReference,
              journey.agentReferenceNumber
            )(
              Right(())
            )
            mockStoreSession(
              session.copy(journeyStatus = Some(updatedJourney))
            )(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction(key -> country.code))
        }

      }

    }

    "handling requests to display the asset type for non-uk residents page" must {

      def performAction(): Future[Result] =
        controller.assetTypeForNonUkResidents()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(
        performAction,
        isValidJourney
      )

      "display the page" when {

        def test(
          session: SessionData,
          expectedBackLink: Call,
          expectedButtonMessageKey: String,
          expectReturnToSummaryLink: Boolean,
          displayType: UserTypeDisplay
        ): Unit =
          testPageIsDisplayed(
            performAction,
            session,
            s"multipleDisposalsAssetTypeForNonUkResidents${displayType.getSubKey}.title",
            routes.MultipleDisposalsTriageController
              .assetTypeForNonUkResidentsSubmit(),
            expectedBackLink,
            expectedButtonMessageKey,
            expectReturnToSummaryLink,
            List(
              SelectorAndValue(
                "#content > article > form > div > #multipleDisposalsAssetTypeForNonUkResidents > div:eq(4) > span",
                messageFromMessageKey(
                  s"multipleDisposalsAssetTypeForNonUkResidents.MixedUse${displayType.getSubKey}.helpText"
                )
              )
            ),
            List(
              TagAttributePairAndValue(
                "label",
                "for",
                "multipleDisposalsAssetTypeForNonUkResidents-3",
                s"Residential Non-residential Mixed use ${messageFromMessageKey(
                  s"multipleDisposalsAssetTypeForNonUkResidents${displayType.getSubKey}.IndirectDisposal"
                )}"
              )
            )
          )

        "the user has not started a new draft return and" when {

          "the journey is incomplete" in {
            List(
              trustDisplay,
              agentDisplay,
              individualDisplay,
              capacitorDisplay,
              personalRepDisplay
            ).foreach { displayType: UserTypeDisplay =>
              withClue(
                s"For user type ${displayType.getSubKey}"
              ) {
                test(
                  sessionDataWithStartingNewDraftReturn(
                    IncompleteMultipleDisposalsTriageAnswers.empty
                      .copy(individualUserType = setIndividualUserType(displayType)),
                    displayType.name,
                    displayType.userType,
                    displayType.representativeType
                  )._1.copy(userType = Some(displayType.userType)),
                  triage.routes.MultipleDisposalsTriageController
                    .countryOfResidence(),
                  "button.continue",
                  expectReturnToSummaryLink = false,
                  displayType
                )
              }
            }
          }

          "the journey is complete" in {
            List(
              trustDisplay,
              agentDisplay,
              individualDisplay,
              personalRepDisplay,
              capacitorDisplay
            ).foreach { displayType: UserTypeDisplay =>
              withClue(
                s"For user type ${displayType.getSubKey}"
              ) {
                test(
                  sessionDataWithStartingNewDraftReturn(
                    sample[CompleteMultipleDisposalsTriageAnswers]
                      .copy(individualUserType = setIndividualUserType(displayType)),
                    displayType.name,
                    displayType.userType,
                    displayType.representativeType
                  )._1,
                  triage.routes.MultipleDisposalsTriageController
                    .checkYourAnswers(),
                  "button.continue",
                  expectReturnToSummaryLink = false,
                  displayType
                )
              }
            }
          }

        }

        "the user has started a new draft return and" when {

          "the journey is incomplete" in {
            test(
              sessionDataWithFillingOutReturn(
                IncompleteMultipleDisposalsTriageAnswers.empty
                  .copy(individualUserType = Some(Self))
              )._1,
              triage.routes.MultipleDisposalsTriageController
                .countryOfResidence(),
              "button.saveAndContinue",
              expectReturnToSummaryLink = true,
              individualDisplay
            )
          }

          "the journey is complete" in {
            test(
              sessionDataWithFillingOutReturn(
                sample[CompleteMultipleDisposalsTriageAnswers]
                  .copy(individualUserType = Some(Self))
              )._1,
              triage.routes.MultipleDisposalsTriageController
                .checkYourAnswers(),
              "button.saveAndContinue",
              expectReturnToSummaryLink = true,
              individualDisplay
            )
          }
        }

      }

    }

    "handling submits on the asset type for non-uk residents page" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.assetTypeForNonUkResidentsSubmit()(
          FakeRequest().withFormUrlEncodedBody(data: _*)
        )

      def updateDraftReturn(
        d: DraftMultipleDisposalsReturn,
        newAnswers: MultipleDisposalsTriageAnswers
      ) =
        d.copy(
          triageAnswers = newAnswers,
          examplePropertyDetailsAnswers = None,
          yearToDateLiabilityAnswers = None,
          supportingEvidenceAnswers = None
        )

      val (countryCode, countryName) = "HK" -> "Hong Kong"
      val country                    = Country(countryCode, Some(countryName))

      val key = "multipleDisposalsAssetTypeForNonUkResidents"

      "redirect to redirect to cya page" when {

        "the user has not started a draft return and" when {

          val answers = IncompleteMultipleDisposalsTriageAnswers.empty.copy(
            individualUserType = Some(Self),
            numberOfProperties = Some(2),
            wasAUKResident = Some(false),
            countryOfResidence = Some(country)
          )

          val (session, journey) =
            sessionDataWithStartingNewDraftReturn(answers)

          "user has not answered the asset type for non-uk residents section and " +
            "selects residential checkbox" in {

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockStoreSession(
                session.copy(journeyStatus =
                  Some(
                    journey.copy(
                      newReturnTriageAnswers = Left(
                        answers.copy(
                          assetTypes = Some(List(AssetType.Residential))
                        )
                      )
                    )
                  )
                )
              )(Right(()))
            }

            checkIsRedirect(
              performAction(s"$key[]" -> "0"),
              routes.MultipleDisposalsTriageController.checkYourAnswers()
            )
          }

          "user has not answered the asset type for non-uk residents section and " +
            "selects mixed use checkbox" in {

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockStoreSession(
                session.copy(journeyStatus =
                  Some(
                    journey.copy(
                      newReturnTriageAnswers = Left(
                        answers.copy(
                          assetTypes = Some(List(AssetType.MixedUse))
                        )
                      )
                    )
                  )
                )
              )(Right(()))
            }

            checkIsRedirect(
              performAction(s"$key[]" -> "2"),
              routes.MultipleDisposalsTriageController.checkYourAnswers()
            )
          }

          "user has not answered the asset type for non-uk residents section and " +
            "selects more than one asset type checkboxes" in {

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockStoreSession(
                session.copy(journeyStatus =
                  Some(
                    journey.copy(
                      newReturnTriageAnswers = Left(
                        answers.copy(
                          assetTypes = Some(
                            List(AssetType.Residential, AssetType.MixedUse)
                          )
                        )
                      )
                    )
                  )
                )
              )(Right(()))
            }

            checkIsRedirect(
              performAction(s"$key[]" -> "0", s"$key[]" -> "2"),
              routes.MultipleDisposalsTriageController.checkYourAnswers()
            )
          }

          "user has already answered the asset type for non-uk residents section and " +
            "re-selected different asset type" in {
            val answers = sample[IncompleteMultipleDisposalsTriageAnswers]
              .copy(
                individualUserType = Some(Self),
                numberOfProperties = Some(2),
                wasAUKResident = Some(false),
                assetTypes = Some(List(AssetType.Residential))
              )

            val (session, journey) =
              sessionDataWithStartingNewDraftReturn(answers)

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockStoreSession(
                session.copy(journeyStatus =
                  Some(
                    journey.copy(
                      newReturnTriageAnswers = Left(
                        answers.copy(
                          assetTypes = Some(List(AssetType.NonResidential))
                        )
                      )
                    )
                  )
                )
              )(Right(()))
            }

            checkIsRedirect(
              performAction(s"$key[]" -> "1"),
              routes.MultipleDisposalsTriageController.checkYourAnswers()
            )
          }

        }

        "the user has started a draft return and" when {

          "have completed the section and they enter a figure which is " +
            "different than one they have already entered" in {
            forAll { c: CompleteMultipleDisposalsTriageAnswers =>
              val answers                         = c.copy(
                countryOfResidence = sample[Country],
                assetTypes = List(AssetType.Residential)
              )
              val (session, journey, draftReturn) =
                sessionDataWithFillingOutReturn(answers)

              val updatedAnswers     =
                IncompleteMultipleDisposalsTriageAnswers(
                  answers.individualUserType,
                  Some(answers.numberOfProperties),
                  Some(false),
                  Some(answers.countryOfResidence),
                  None,
                  Some(List(AssetType.NonResidential)),
                  Some(true),
                  Some(answers.taxYear),
                  Some(answers.completionDate)
                )
              val updatedDraftReturn =
                updateDraftReturn(draftReturn, updatedAnswers)
              val updatedJourney     =
                journey.copy(draftReturn = updatedDraftReturn)

              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
                mockStoreDraftReturn(
                  updatedDraftReturn,
                  journey.subscribedDetails.cgtReference,
                  journey.agentReferenceNumber
                )(
                  Right(())
                )
                mockStoreSession(
                  session.copy(journeyStatus = Some(updatedJourney))
                )(Right(()))
              }

              checkIsRedirect(
                performAction(s"$key[]" -> "1"),
                routes.MultipleDisposalsTriageController.checkYourAnswers()
              )
            }
          }

        }

      }

      "not update the session" when {

        "user has already answered the asset type for non-uk residents section and " +
          "re-selected same asset type" in {
          val answers = sample[IncompleteMultipleDisposalsTriageAnswers]
            .copy(
              individualUserType = Some(Self),
              numberOfProperties = Some(2),
              wasAUKResident = Some(false),
              assetTypes = Some(List(AssetType.Residential))
            )

          val (session, _) = sessionDataWithStartingNewDraftReturn(answers)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkIsRedirect(
            performAction(s"$key[]" -> "0"),
            routes.MultipleDisposalsTriageController.checkYourAnswers()
          )
        }

      }

      "show a form error" when {

        "the user submits nothing" in {
          List(
            trustDisplay,
            agentDisplay,
            individualDisplay,
            capacitorDisplay,
            personalRepDisplay
          ).foreach { displayType: UserTypeDisplay =>
            withClue(
              s"For user type ${displayType.getSubKey}"
            ) {
              val answers = IncompleteMultipleDisposalsTriageAnswers.empty.copy(
                individualUserType = setIndividualUserType(displayType),
                numberOfProperties = Some(2),
                wasAUKResident = Some(false),
                countryOfResidence = Some(country),
                assetTypes = None
              )
              val session = sessionDataWithStartingNewDraftReturn(
                answers,
                displayType.name,
                displayType.userType,
                displayType.representativeType
              )._1.copy(userType = Some(displayType.userType))

              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
              }

              checkPageIsDisplayed(
                performAction(),
                messageFromMessageKey(
                  s"multipleDisposalsAssetTypeForNonUkResidents${displayType.getSubKey}.title"
                ),
                { doc =>
                  doc
                    .select("#error-summary-display > ul > li > a")
                    .text() shouldBe messageFromMessageKey(
                    s"$key${displayType.getSubKey}.error.required"
                  )
                  doc.title() should startWith("Error:")
                },
                BAD_REQUEST
              )
            }
          }
        }

      }

      "show an error page" when {

        val answers                         = sample[CompleteMultipleDisposalsTriageAnswers].copy(
          countryOfResidence = sample[Country],
          assetTypes = List(AssetType.Residential)
        )
        val (session, journey, draftReturn) =
          sessionDataWithFillingOutReturn(answers)

        val updatedAnswers     =
          IncompleteMultipleDisposalsTriageAnswers(
            answers.individualUserType,
            Some(answers.numberOfProperties),
            Some(false),
            Some(answers.countryOfResidence),
            None,
            Some(List(AssetType.NonResidential)),
            Some(true),
            Some(answers.taxYear),
            Some(answers.completionDate)
          )
        val updatedDraftReturn = updateDraftReturn(draftReturn, updatedAnswers)
        val updatedJourney     = journey.copy(draftReturn = updatedDraftReturn)

        "there is an error updating the draft return" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(
              updatedDraftReturn,
              journey.subscribedDetails.cgtReference,
              journey.agentReferenceNumber
            )(
              Left(Error(""))
            )
          }

          checkIsTechnicalErrorPage(performAction(s"$key[]" -> "1"))
        }

        "there is an error updating the session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(
              updatedDraftReturn,
              journey.subscribedDetails.cgtReference,
              journey.agentReferenceNumber
            )(
              Right(())
            )
            mockStoreSession(
              session.copy(journeyStatus = Some(updatedJourney))
            )(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction(s"$key[]" -> "1"))
        }

      }

    }

    "handling requests to display the completion date page" must {

      def performAction(): Future[Result] =
        controller.completionDate()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(
        performAction,
        isValidJourney
      )

      "display the page" when {

        def test(
          session: SessionData,
          expectedBackLink: Call,
          expectedButtonMessageKey: String,
          expectReturnToSummaryLink: Boolean
        ): Unit =
          testPageIsDisplayed(
            performAction,
            session,
            "multipleDisposalsCompletionDate.title",
            routes.MultipleDisposalsTriageController.completionDateSubmit(),
            expectedBackLink,
            expectedButtonMessageKey,
            expectReturnToSummaryLink
          )

        "the user has not started a new draft return and" when {

          "the journey is incomplete" in {
            test(
              sessionDataWithStartingNewDraftReturn(
                IncompleteMultipleDisposalsTriageAnswers.empty
              )._1,
              triage.routes.MultipleDisposalsTriageController
                .whenWereContractsExchanged(),
              "button.continue",
              expectReturnToSummaryLink = false
            )
          }

          "the journey is complete" in {
            test(
              sessionDataWithStartingNewDraftReturn(
                sample[CompleteMultipleDisposalsTriageAnswers]
              )._1,
              triage.routes.MultipleDisposalsTriageController
                .checkYourAnswers(),
              "button.continue",
              expectReturnToSummaryLink = false
            )
          }

        }

        "the user has started a new draft return and" when {

          "the journey is incomplete" in {
            test(
              sessionDataWithFillingOutReturn(
                IncompleteMultipleDisposalsTriageAnswers.empty
              )._1,
              triage.routes.MultipleDisposalsTriageController
                .whenWereContractsExchanged(),
              "button.saveAndContinue",
              expectReturnToSummaryLink = true
            )
          }

          "the journey is complete" in {
            test(
              sessionDataWithFillingOutReturn(
                sample[CompleteMultipleDisposalsTriageAnswers]
              )._1,
              triage.routes.MultipleDisposalsTriageController
                .checkYourAnswers(),
              "button.saveAndContinue",
              expectReturnToSummaryLink = true
            )
          }
        }

      }

    }

    "handling submitted completion dates" must {

      def performAction(formData: (String, String)*): Future[Result] =
        controller.completionDateSubmit()(
          FakeRequest().withFormUrlEncodedBody(formData: _*)
        )

      def formData(d: LocalDate): List[(String, String)] =
        List(
          "multipleDisposalsCompletionDate-day"   -> d.getDayOfMonth.toString,
          "multipleDisposalsCompletionDate-month" -> d.getMonthValue.toString,
          "multipleDisposalsCompletionDate-year"  -> d.getYear.toString
        )

      def updateDraftReturn(
        d: DraftMultipleDisposalsReturn,
        newAnswers: MultipleDisposalsTriageAnswers
      ) =
        d.copy(
          triageAnswers = newAnswers,
          examplePropertyDetailsAnswers = d.examplePropertyDetailsAnswers.map(
            _.unset(_.disposalDate)
          ),
          yearToDateLiabilityAnswers = None
        )

      behave like redirectToStartWhenInvalidJourney(
        () => performAction(),
        isValidJourney
      )

      "show a form error" when {

        def testFormError(
          formData: List[(String, String)]
        )(expectedErrorMessageKey: String) = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithStartingNewDraftReturn(
                sample[CompleteMultipleDisposalsTriageAnswers]
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(formData: _*),
            messageFromMessageKey("multipleDisposalsCompletionDate.title"),
            doc =>
              doc
                .select("#error-summary-display > ul > li > a")
                .text() shouldBe messageFromMessageKey(
                expectedErrorMessageKey
              ),
            BAD_REQUEST
          )
        }

        "the date entered is invalid" in {
          DateErrorScenarios
            .dateErrorScenarios(
              "multipleDisposalsCompletionDate",
              ""
            )
            .foreach { scenario =>
              withClue(s"For date error scenario $scenario: ") {
                val data = List(
                  "multipleDisposalsCompletionDate-day"   -> scenario.dayInput,
                  "multipleDisposalsCompletionDate-month" -> scenario.monthInput,
                  "multipleDisposalsCompletionDate-year"  -> scenario.yearInput
                ).collect { case (key, Some(value)) => key -> value }

                testFormError(data)(scenario.expectedErrorMessageKey)
              }
            }
        }

        "the date entered is later than today" in {
          testFormError(formData(TimeUtils.today().plusDays(1L)))(
            "multipleDisposalsCompletionDate.error.tooFarInFuture"
          )
        }

      }

      "show an error page" when {

        "there is an error updating the draft return" in {
          val answers                         = sample[CompleteMultipleDisposalsTriageAnswers].copy(
            completionDate = CompletionDate(TimeUtils.today().minusDays(1L))
          )
          val (session, journey, draftReturn) =
            sessionDataWithFillingOutReturn(answers)

          val updatedAnswers     =
            answers.copy(completionDate = CompletionDate(TimeUtils.today()))
          val updatedDraftReturn =
            updateDraftReturn(draftReturn, updatedAnswers)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(
              updatedDraftReturn,
              journey.subscribedDetails.cgtReference,
              journey.agentReferenceNumber
            )(
              Left(Error(""))
            )
          }

          checkIsTechnicalErrorPage(
            performAction(formData(TimeUtils.today()): _*)
          )
        }

        "there is an error updating the session" in {
          val answers            =
            sample[CompleteMultipleDisposalsTriageAnswers]
              .copy(completionDate = CompletionDate(TimeUtils.today()))
          val (session, journey) =
            sessionDataWithStartingNewDraftReturn(answers)

          val newCompletionDate =
            CompletionDate(answers.completionDate.value.minusDays(1L))
          val updatedJourney    =
            journey.copy(newReturnTriageAnswers = Left(answers.copy(completionDate = newCompletionDate)))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreSession(
              session.copy(journeyStatus = Some(updatedJourney))
            )(Left(Error("")))
          }

          checkIsTechnicalErrorPage(
            performAction(formData(newCompletionDate.value): _*)
          )
        }

      }

      "redirect to the check your answers page" when {

        "the user has not started a draft return and" when {

          "the user has not answered the question before" in {
            val answers            = IncompleteMultipleDisposalsTriageAnswers.empty
            val (session, journey) =
              sessionDataWithStartingNewDraftReturn(answers)

            val newCompletionDate = CompletionDate(TimeUtils.today())
            val updatedJourney    =
              journey.copy(newReturnTriageAnswers = Left(answers.copy(completionDate = Some(newCompletionDate))))

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockStoreSession(
                session.copy(journeyStatus = Some(updatedJourney))
              )(Right(()))
            }

            checkIsRedirect(
              performAction(formData(newCompletionDate.value): _*),
              routes.MultipleDisposalsTriageController.checkYourAnswers()
            )
          }

          "the user has already answered the question" in {
            forAll { c: CompleteMultipleDisposalsTriageAnswers =>
              val answers            =
                c.copy(completionDate = CompletionDate(TimeUtils.today()))
              val (session, journey) =
                sessionDataWithStartingNewDraftReturn(answers)

              val newCompletionDate =
                CompletionDate(answers.completionDate.value.minusDays(1L))
              val updatedJourney    =
                journey.copy(newReturnTriageAnswers = Left(answers.copy(completionDate = newCompletionDate)))

              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
                mockStoreSession(
                  session.copy(journeyStatus = Some(updatedJourney))
                )(Right(()))
              }

              checkIsRedirect(
                performAction(formData(newCompletionDate.value): _*),
                routes.MultipleDisposalsTriageController.checkYourAnswers()
              )
            }
          }
        }

        "the user has started a draft return and" when {

          "have completed the section and they enter a figure which is " +
            "different than one they have already entered" in {
            val answers                         = sample[CompleteMultipleDisposalsTriageAnswers].copy(
              completionDate = CompletionDate(TimeUtils.today().minusDays(1L))
            )
            val (session, journey, draftReturn) =
              sessionDataWithFillingOutReturn(answers)

            val updatedAnswers     =
              answers.copy(completionDate = CompletionDate(TimeUtils.today()))
            val updatedDraftReturn =
              updateDraftReturn(draftReturn, updatedAnswers)
            val updatedJourney     = journey.copy(draftReturn = updatedDraftReturn)

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockStoreDraftReturn(
                updatedDraftReturn,
                journey.subscribedDetails.cgtReference,
                journey.agentReferenceNumber
              )(
                Right(())
              )
              mockStoreSession(
                session.copy(journeyStatus = Some(updatedJourney))
              )(Right(()))
            }

            checkIsRedirect(
              performAction(formData(TimeUtils.today()): _*),
              routes.MultipleDisposalsTriageController.checkYourAnswers()
            )
          }

        }

      }

      "not perform any updates" when {

        "the date submitted is the same as one that already exists in session" in {
          val answers      = sample[CompleteMultipleDisposalsTriageAnswers].copy(
            completionDate = CompletionDate(TimeUtils.today())
          )
          val (session, _) = sessionDataWithStartingNewDraftReturn(answers)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkIsRedirect(
            performAction(formData(answers.completionDate.value): _*),
            routes.MultipleDisposalsTriageController.checkYourAnswers()
          )
        }

      }

    }

    "handling requests to display the share disposal page for non uk residents page" must {

      val requiredPreviousAnswers =
        IncompleteMultipleDisposalsTriageAnswers.empty.copy(
          individualUserType = Some(sample[IndividualUserType]),
          wasAUKResident = Some(false),
          countryOfResidence = Some(sample[Country])
        )

      def performAction(): Future[Result] =
        controller.disposalDateOfShares()(FakeRequest())

      "Page is displayed correctly" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            sessionDataWithFillingOutReturn(
              requiredPreviousAnswers.copy(
                assetTypes = Some(List(AssetType.IndirectDisposal))
              )
            )._1
          )
        }
        checkPageIsDisplayed(
          performAction,
          messageFromMessageKey("sharesDisposalDate.title"),
          doc => {
            doc
              .select("#sharesDisposalDate-form-hint")
              .text()         shouldBe messageFromMessageKey(
              "sharesDisposalDate.helpText"
            )
            doc
              .select("#back")
              .attr("href")   shouldBe routes.MultipleDisposalsTriageController
              .assetTypeForNonUkResidents()
              .url
            doc
              .select("#content > article > form")
              .attr("action") shouldBe routes.MultipleDisposalsTriageController
              .disposalDateOfSharesSubmit()
              .url
          }
        )

      }

    }

    "handling submitted disposal of shares date" must {

      def performAction(formData: (String, String)*): Future[Result] =
        controller.disposalDateOfSharesSubmit()(
          FakeRequest().withFormUrlEncodedBody(formData: _*)
        )

      def formData(d: LocalDate): List[(String, String)] =
        List(
          "sharesDisposalDate-day"   -> d.getDayOfMonth().toString,
          "sharesDisposalDate-month" -> d.getMonthValue().toString,
          "sharesDisposalDate-year"  -> d.getYear().toString
        )

      behave like redirectToStartWhenInvalidJourney(
        () => performAction(),
        isValidJourney
      )

      def updateDraftReturn(
        d: DraftMultipleDisposalsReturn,
        newAnswers: MultipleDisposalsTriageAnswers
      ) =
        d.copy(
          triageAnswers = newAnswers,
          examplePropertyDetailsAnswers = d.examplePropertyDetailsAnswers.map(
            _.unset(_.disposalDate)
          ),
          yearToDateLiabilityAnswers = None
        )

      "show a form error" when {

        def testFormError(
          formData: List[(String, String)]
        )(expectedErrorMessageKey: String) = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithStartingNewDraftReturn(
                sample[CompleteMultipleDisposalsTriageAnswers]
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(formData: _*),
            messageFromMessageKey("sharesDisposalDate.title"),
            doc =>
              doc
                .select("#error-summary-display > ul > li > a")
                .text() shouldBe messageFromMessageKey(
                expectedErrorMessageKey
              ),
            BAD_REQUEST
          )
        }

        "the date entered is invalid" in {
          DateErrorScenarios
            .dateErrorScenarios(
              "sharesDisposalDate",
              ""
            )
            .foreach { scenario =>
              withClue(s"For date error scenario $scenario: ") {
                val data = List(
                  "sharesDisposalDate-day"   -> scenario.dayInput,
                  "sharesDisposalDate-month" -> scenario.monthInput,
                  "sharesDisposalDate-year"  -> scenario.yearInput
                ).collect { case (key, Some(value)) => key -> value }

                testFormError(data)(scenario.expectedErrorMessageKey)
              }
            }
        }

        "the date entered is later than today" in {
          testFormError(formData(TimeUtils.today().plusDays(1L)))(
            "sharesDisposalDate.error.tooFarInFuture"
          )
        }

      }

      "show an error page" when {

        "there is an error updating the session" in {
          val taxYear            = sample[TaxYear]
          val today              = TimeUtils.today()
          val answers            =
            sample[IncompleteMultipleDisposalsTriageAnswers]
              .copy(completionDate = Some(CompletionDate(today)))
          val (session, journey) =
            sessionDataWithStartingNewDraftReturn(answers)

          val newCompletionDate = CompletionDate(today.minusDays(1))
          val updatedJourney    =
            journey.copy(newReturnTriageAnswers =
              Left(
                answers.copy(
                  completionDate = Some(newCompletionDate),
                  taxYear = Some(taxYear),
                  taxYearAfter6April2020 = Some(true)
                )
              )
            )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockGetTaxYear(newCompletionDate.value)(Right(Some(taxYear)))
            mockStoreSession(
              session.copy(journeyStatus = Some(updatedJourney))
            )(Left(Error("")))
          }

          checkIsTechnicalErrorPage(
            performAction(formData(newCompletionDate.value): _*)
          )
        }

      }

      "redirect to the check your answers page" when {

        "redirect to exit page" when {

          "redirect when tax service returns empty" in {
            val tooEarlyDate = LocalDate.of(2020, 1, 1)
            val answers      = sample[IncompleteMultipleDisposalsTriageAnswers]

            val (session, journey, draftReturn) =
              sessionDataWithFillingOutReturn(answers)

            val updatedAnswers     = answers.copy(
              completionDate = Some(CompletionDate(tooEarlyDate)),
              taxYear = None,
              taxYearAfter6April2020 = Some(false)
            )
            val updatedDraftReturn =
              updateDraftReturn(draftReturn, updatedAnswers)
            val updatedJourney     = journey.copy(draftReturn = updatedDraftReturn)

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockGetTaxYear(tooEarlyDate)(Right(None))
              mockStoreDraftReturn(
                updatedDraftReturn,
                journey.subscribedDetails.cgtReference,
                journey.agentReferenceNumber
              )(Right(()))
              mockStoreSession(
                session.copy(journeyStatus = Some(updatedJourney))
              )(Right(()))
            }

            checkIsRedirect(
              performAction(
                formData(tooEarlyDate): _*
              ),
              routes.CommonTriageQuestionsController.disposalsOfSharesTooEarly()
            )
          }
        }
      }

      "not perform any updates" when {

        "the date submitted is the same as one that already exists in session" in {
          val answers      =
            sample[CompleteMultipleDisposalsTriageAnswers]
              .copy(completionDate = CompletionDate(TimeUtils.today()))
          val (session, _) =
            sessionDataWithStartingNewDraftReturn(answers)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkIsRedirect(
            performAction(formData(answers.completionDate.value): _*),
            routes.MultipleDisposalsTriageController.checkYourAnswers()
          )
        }

      }
    }

    "handling requests to display the check your answers page" must {

      def performAction(): Future[Result] =
        controller.checkYourAnswers()(FakeRequest())

      val completeAnswersUk = CompleteMultipleDisposalsTriageAnswers(
        Some(IndividualUserType.Self),
        2,
        Country.uk,
        List(AssetType.Residential),
        sample[TaxYear],
        sample[CompletionDate]
      )

      val allQuestionsAnsweredUk = IncompleteMultipleDisposalsTriageAnswers(
        completeAnswersUk.individualUserType,
        Some(completeAnswersUk.numberOfProperties),
        Some(true),
        None,
        Some(true),
        Some(completeAnswersUk.assetTypes),
        Some(true),
        Some(completeAnswersUk.taxYear),
        Some(completeAnswersUk.completionDate)
      )

      val completeAnswersNonUk = CompleteMultipleDisposalsTriageAnswers(
        Some(IndividualUserType.Self),
        2,
        sample[Country],
        List(AssetType.Residential),
        sample[TaxYear],
        sample[CompletionDate]
      )

      val allQuestionsAnsweredNonUk = IncompleteMultipleDisposalsTriageAnswers(
        completeAnswersNonUk.individualUserType,
        Some(completeAnswersNonUk.numberOfProperties),
        Some(false),
        Some(completeAnswersNonUk.countryOfResidence),
        None,
        Some(completeAnswersNonUk.assetTypes),
        Some(true),
        Some(completeAnswersNonUk.taxYear),
        Some(completeAnswersNonUk.completionDate)
      )

      def testRedirectWhenIncomplete(
        answers: IncompleteMultipleDisposalsTriageAnswers,
        expectedRedirect: Call
      ): Unit = {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            sessionDataWithStartingNewDraftReturn(
              answers,
              Right(sample[IndividualName])
            )._1
          )
        }

        checkIsRedirect(performAction(), expectedRedirect)

      }

      behave like redirectToStartWhenInvalidJourney(
        performAction,
        isValidJourney
      )

      "redirect to the who is individual representing page when no individual user type can be found and the subscribed " +
        "user type is individual" in {
        testRedirectWhenIncomplete(
          IncompleteMultipleDisposalsTriageAnswers.empty,
          routes.CommonTriageQuestionsController.whoIsIndividualRepresenting()
        )
      }

      "redirect to the enter represented person's name page" when {

        "an individual user type of capacitor is found" in {
          testRedirectWhenIncomplete(
            allQuestionsAnsweredUk
              .copy(individualUserType = Some(IndividualUserType.Capacitor)),
            representee.routes.RepresenteeController.enterName()
          )
        }

        "an individual user type of personal representative is found" in {
          testRedirectWhenIncomplete(
            allQuestionsAnsweredUk.copy(individualUserType = Some(IndividualUserType.PersonalRepresentative)),
            representee.routes.RepresenteeController.enterName()
          )

        }

      }

      "redirect to the multiple disposals guidance page when no answer for the number of properties can be found" in {
        testRedirectWhenIncomplete(
          allQuestionsAnsweredUk.copy(numberOfProperties = None),
          routes.MultipleDisposalsTriageController.guidance()
        )
      }

      "redirect the were you a uk resident page" when {

        "the user has not answered that question" in {
          testRedirectWhenIncomplete(
            allQuestionsAnsweredUk.copy(wasAUKResident = None),
            routes.MultipleDisposalsTriageController.wereYouAUKResident()
          )
        }

      }

      "redirect to the country of residence page" when {

        "the user was not a non uk resident and they have not selected a country yet" in {
          testRedirectWhenIncomplete(
            allQuestionsAnsweredNonUk.copy(countryOfResidence = None),
            routes.MultipleDisposalsTriageController.countryOfResidence()
          )
        }

      }

      "redirect to the asset type for non uk residents page" when {

        "the user was not a non uk resident and they have not selected asset types yet" in {
          testRedirectWhenIncomplete(
            allQuestionsAnsweredNonUk.copy(assetTypes = None),
            routes.MultipleDisposalsTriageController
              .assetTypeForNonUkResidents()
          )
        }

      }

      "not redirect to the asset types not implemented page" when {

        "the user selects a valid combination of asset types" in {
          val invalidAssetTypes = List(
            List(AssetType.IndirectDisposal, AssetType.MixedUse),
            List(AssetType.MixedUse, AssetType.IndirectDisposal)
          )

          forAll { assetTypes: List[AssetType] =>
            whenever(
              !invalidAssetTypes
                .contains(assetTypes.distinct) && assetTypes.nonEmpty
            ) {
              val (session, journey, draftReturn) =
                sessionDataWithFillingOutReturn(
                  allQuestionsAnsweredNonUk.copy(assetTypes = Some(assetTypes))
                )
              val updatedDraftReturn              =
                draftReturn.copy(triageAnswers = completeAnswersNonUk.copy(assetTypes = assetTypes))
              val updatedJourney                  =
                journey.copy(draftReturn = updatedDraftReturn)

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
                )(Right(()))
              }

              checkPageIsDisplayed(
                performAction(),
                messageFromMessageKey("multipleDisposals.triage.cya.title")
              )
            }
          }
        }

      }

      "redirect to the were all properties residential page" when {

        "the user was a uk resident and they have not answered the question yet" in {
          testRedirectWhenIncomplete(
            allQuestionsAnsweredUk.copy(wereAllPropertiesResidential = None),
            routes.MultipleDisposalsTriageController
              .wereAllPropertiesResidential()
          )
        }
      }

      "redirect to uk residents can only dispose of residential properties page" when {

        "the user was a uk resident and they said not all properties were residential" in {
          testRedirectWhenIncomplete(
            allQuestionsAnsweredUk
              .copy(wereAllPropertiesResidential = Some(false)),
            routes.CommonTriageQuestionsController
              .ukResidentCanOnlyDisposeResidential()
          )
        }

      }

      "redirect to the tax year page" when {

        "the question has not been answered yet" in {
          testRedirectWhenIncomplete(
            allQuestionsAnsweredUk
              .copy(taxYearAfter6April2020 = None, taxYear = None),
            routes.MultipleDisposalsTriageController
              .whenWereContractsExchanged()
          )
        }

      }

      "redirect to the tax year too early page" when {

        "the user indicated that the tax year was before 6th April 2020" in {
          testRedirectWhenIncomplete(
            allQuestionsAnsweredUk.copy(taxYearAfter6April2020 = Some(false)),
            routes.CommonTriageQuestionsController.disposalDateTooEarly()
          )
        }

      }

      "show an error page" when {

        "no tax year can be found when one is expected" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithStartingNewDraftReturn(
                allQuestionsAnsweredUk
                  .copy(taxYearAfter6April2020 = Some(true), taxYear = None),
                Right(sample[IndividualName])
              )._1
            )
          }

          checkIsTechnicalErrorPage(performAction())
        }

        "there is an error storing a draft return" in {
          val (session, journey, draftReturn) =
            sessionDataWithFillingOutReturn(allQuestionsAnsweredUk)
          val updatedDraftReturn              =
            draftReturn.copy(triageAnswers = completeAnswersUk)
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

          checkIsTechnicalErrorPage(performAction())
        }

      }

      "redirect to the completion date page" when {

        "the question has not been answered yet" in {
          testRedirectWhenIncomplete(
            allQuestionsAnsweredUk.copy(completionDate = None),
            routes.MultipleDisposalsTriageController.completionDate()
          )
        }

      }

      "show an error page" when {

        "there is an error updating the session when converting from inomplete answers to " +
          "complete answers" in {
          val (session, journey) =
            sessionDataWithStartingNewDraftReturn(allQuestionsAnsweredUk)
          val updatedJourney     =
            journey.copy(newReturnTriageAnswers = Left(completeAnswersUk))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreSession(
              session.copy(journeyStatus = Some(updatedJourney))
            )(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction())

        }

      }

      "show the page" when {

        "the user has already completed the section and " when {

          "they were a uk resident" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionDataWithFillingOutReturn(completeAnswersUk)._1
              )
            }

            checkPageIsDisplayed(
              performAction(),
              messageFromMessageKey("multipleDisposals.triage.cya.title"),
              { doc =>
                doc
                  .select("#guidanceLink")
                  .attr(
                    "href"
                  )                              shouldBe routes.MultipleDisposalsTriageController
                  .guidance()
                  .url
                doc.select("#guidanceLink").text shouldBe messageFromMessageKey(
                  "multipleDisposals.triage.cya.guidanceLink"
                )

                MultipleDisposalsTriageControllerSpec
                  .validateMultipleDisposalsTriageCheckYourAnswersPage(
                    completeAnswersUk,
                    None,
                    doc
                  )
              }
            )

          }

          "they were a not a uk resident" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionDataWithStartingNewDraftReturn(completeAnswersNonUk)._1
              )
            }

            checkPageIsDisplayed(
              performAction(),
              messageFromMessageKey("multipleDisposals.triage.cya.title"),
              { doc =>
                doc
                  .select("#guidanceLink")
                  .attr(
                    "href"
                  )                              shouldBe routes.MultipleDisposalsTriageController
                  .guidance()
                  .url
                doc.select("#guidanceLink").text shouldBe messageFromMessageKey(
                  "multipleDisposals.triage.cya.guidanceLink"
                )

                MultipleDisposalsTriageControllerSpec
                  .validateMultipleDisposalsTriageCheckYourAnswersPage(
                    completeAnswersNonUk,
                    None,
                    doc
                  )
              }
            )

          }

          "they user is an agent" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionDataWithStartingNewDraftReturn(
                  completeAnswersNonUk,
                  userType = UserType.Agent
                )._1
              )
            }

            checkPageIsDisplayed(
              performAction(),
              messageFromMessageKey("multipleDisposals.triage.cya.title"),
              { doc =>
                doc
                  .select("#guidanceLink")
                  .attr(
                    "href"
                  )                              shouldBe routes.MultipleDisposalsTriageController
                  .guidance()
                  .url
                doc.select("#guidanceLink").text shouldBe messageFromMessageKey(
                  "multipleDisposals.triage.cya.guidanceLink"
                )

                MultipleDisposalsTriageControllerSpec
                  .validateMultipleDisposalsTriageCheckYourAnswersPage(
                    completeAnswersNonUk,
                    Some(UserType.Agent),
                    doc
                  )
              }
            )

          }

        }

        "non uk resident selects indirect disposal" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithStartingNewDraftReturn(
                completeAnswersNonUk.copy(assetTypes = List(IndirectDisposal)),
                userType = UserType.Agent
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("multipleDisposals.triage.cya.title"),
            { doc =>
              doc
                .select("#guidanceLink")
                .attr("href") shouldBe routes.MultipleDisposalsTriageController
                .guidance()
                .url
              doc
                .select("#completionDate-question")
                .text         shouldBe messageFromMessageKey(
                "sharesDisposalDate.title"
              )

              MultipleDisposalsTriageControllerSpec
                .validateMultipleDisposalsTriageCheckYourAnswersPage(
                  completeAnswersNonUk
                    .copy(assetTypes = List(IndirectDisposal)),
                  Some(UserType.Agent),
                  doc
                )
            }
          )
        }

        "the user has just answered all the question in the section and" when {

          "all updated are successful when the user was a uk resident" in {
            val (session, journey, draftReturn) =
              sessionDataWithFillingOutReturn(allQuestionsAnsweredUk)
            val updatedDraftReturn              =
              draftReturn.copy(triageAnswers = completeAnswersUk)
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
              )(Right(()))
            }

            checkPageIsDisplayed(
              performAction(),
              messageFromMessageKey("multipleDisposals.triage.cya.title"),
              { doc =>
                doc
                  .select("#guidanceLink")
                  .attr(
                    "href"
                  )                              shouldBe routes.MultipleDisposalsTriageController
                  .guidance()
                  .url
                doc.select("#guidanceLink").text shouldBe messageFromMessageKey(
                  "multipleDisposals.triage.cya.guidanceLink"
                )

                MultipleDisposalsTriageControllerSpec
                  .validateMultipleDisposalsTriageCheckYourAnswersPage(
                    completeAnswersUk,
                    None,
                    doc
                  )
              }
            )

          }

          "all updated are successful when the user was a not uk resident" in {
            val (session, journey) =
              sessionDataWithStartingNewDraftReturn(allQuestionsAnsweredNonUk)
            val updatedJourney     =
              journey.copy(newReturnTriageAnswers = Left(completeAnswersNonUk))

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockStoreSession(
                session.copy(journeyStatus = Some(updatedJourney))
              )(Right(()))
            }

            checkPageIsDisplayed(
              performAction(),
              messageFromMessageKey("multipleDisposals.triage.cya.title"),
              { doc =>
                doc
                  .select("#guidanceLink")
                  .attr(
                    "href"
                  )                              shouldBe routes.MultipleDisposalsTriageController
                  .guidance()
                  .url
                doc.select("#guidanceLink").text shouldBe messageFromMessageKey(
                  "multipleDisposals.triage.cya.guidanceLink"
                )

                MultipleDisposalsTriageControllerSpec
                  .validateMultipleDisposalsTriageCheckYourAnswersPage(
                    completeAnswersNonUk,
                    None,
                    doc
                  )
              }
            )

          }
        }

      }
    }

    "handling submits on the check your answers page" when {
      def performAction(): Future[Result] =
        controller.checkYourAnswersSubmit()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(
        performAction,
        isValidJourney
      )

      "the user has not started a new draft return yet" must {

        val completeAnswers = sample[CompleteMultipleDisposalsTriageAnswers].copy(
          countryOfResidence = Country.uk,
          assetTypes = List(AssetType.Residential)
        )

        val (session, journey) =
          sessionDataWithStartingNewDraftReturn(completeAnswers)
        val draftId            = UUID.randomUUID()
        val newDraftReturn     =
          DraftMultipleDisposalsReturn.newDraftReturn(
            draftId,
            completeAnswers,
            journey.representeeAnswers
          )

        val newJourney = FillingOutReturn(
          journey.subscribedDetails,
          journey.ggCredId,
          journey.agentReferenceNumber,
          newDraftReturn
        )

        "show an error page" when {

          "there is an error storing the new draft return" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockGenerateUUID(draftId)
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
              mockGenerateUUID(draftId)
              mockStoreDraftReturn(
                newDraftReturn,
                journey.subscribedDetails.cgtReference,
                journey.agentReferenceNumber
              )(Right(()))
              mockStoreSession(session.copy(journeyStatus = Some(newJourney)))(
                Left(Error(""))
              )
            }

            checkIsTechnicalErrorPage(performAction())
          }

        }

        "redirect to the task list page" when {

          "a new draft return is created and saved" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockGenerateUUID(draftId)
              mockStoreDraftReturn(
                newDraftReturn,
                journey.subscribedDetails.cgtReference,
                journey.agentReferenceNumber
              )(Right(()))
              mockStoreSession(session.copy(journeyStatus = Some(newJourney)))(
                Right(())
              )
            }

            checkIsRedirect(
              performAction(),
              controllers.returns.routes.TaskListController.taskList()
            )
          }

        }

      }

      "the user has already started a new draft return" must {

        "redirect to the task list page" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithFillingOutReturn(
                sample[CompleteMultipleDisposalsTriageAnswers]
              )._1
            )
          }

          checkIsRedirect(
            performAction(),
            controllers.returns.routes.TaskListController.taskList()
          )
        }

      }

    }

  }

  def testPageIsDisplayed(
    performAction: () => Future[Result],
    session: SessionData,
    expectedPageTitleMessageKey: String,
    expectedSubmit: Call,
    expectedBackLink: Call,
    expectedButtonMessageKey: String,
    expectReturnToSummaryLink: Boolean,
    expectedAdditionalIdKeyValues: List[SelectorAndValue] = Nil,
    expectedAdditionalNameAttributeKeyValues: List[TagAttributePairAndValue] = Nil
  ): Unit = {
    inSequence {
      mockAuthWithNoRetrievals()
      mockGetSession(session)
    }

    checkPageIsDisplayed(
      performAction(),
      messageFromMessageKey(expectedPageTitleMessageKey),
      { doc =>
        doc.select("#back").attr("href")          shouldBe expectedBackLink.url
        doc
          .select("#content > article > form")
          .attr("action")                         shouldBe expectedSubmit.url
        doc.select("#submitButton").text()        shouldBe messageFromMessageKey(
          expectedButtonMessageKey
        )
        doc.select("#returnToSummaryLink").text() shouldBe (
          if (expectReturnToSummaryLink)
            messageFromMessageKey("returns.return-to-summary-link")
          else ""
        )
        expectedAdditionalIdKeyValues.map(a => doc.select(a.selector).html() should be(a.value))
        expectedAdditionalNameAttributeKeyValues.map(v =>
          doc
            .select(v.tagName)
            .attr(v.attributeName, v.attributeValue)
            .text() shouldBe (v.value)
        )
      }
    )
  }
}

object MultipleDisposalsTriageControllerSpec extends Matchers {
  final case class SelectorAndValue(selector: String, value: String)
  final case class TagAttributePairAndValue(
    tagName: String,
    attributeName: String,
    attributeValue: String,
    value: String
  )

  final case class UserTypeDisplay(
    userType: UserType,
    representativeType: Option[
      Either[PersonalRepresentative.type, Capacitor.type]
    ],
    name: Either[TrustName, IndividualName]
  ) {
    def getSubKey: String =
      if (representativeType.exists(_.isLeft)) ".personalRep"
      else if (representativeType.exists(_.isRight)) ".capacitor"
      else if (userType === UserType.Agent) ".agent"
      else if (name.isLeft) ".trust"
      else ""
  }

  def validateMultipleDisposalsTriageCheckYourAnswersPage(
    answers: CompleteMultipleDisposalsTriageAnswers,
    userType: Option[UserType],
    doc: Document
  )(implicit messagesApi: MessagesApi, lang: Lang): Unit = {
    implicit val messages = MessagesImpl(lang, messagesApi)

    answers.individualUserType.foreach { individualUserType =>
      doc.select("#individualUserType-answer").text() shouldBe messages(
        if (userType.contains(UserType.Agent))
          s"individualUserType.agent.$individualUserType"
        else s"individualUserType.$individualUserType"
      )
    }

    doc.select("#numberOfProperties-answer").text() shouldBe messages(
      "numberOfProperties.MoreThanOne"
    )

    doc
      .select("#multipleDisposalsNumberOfProperties-answer")
      .text() shouldBe answers.numberOfProperties.toString

    if (answers.countryOfResidence.isUk())
      doc.select("#wereYouAUKResident-answer").text() shouldBe "Yes"
    else
      doc.select("#wereYouAUKResident-answer").text() shouldBe "No"

    if (answers.countryOfResidence.isUk())
      doc.select("#wereAllPropertiesResidential-answer").text() shouldBe "Yes"
    else {
      doc
        .select("#countryOfResidence-answer")
        .text() shouldBe answers.countryOfResidence.name
        .getOrElse(answers.countryOfResidence.code)
      doc
        .select("#assetTypeForNonUkResidents-answer")
        .text() shouldBe answers.assetTypes
        .map(assetType =>
          messages(
            s"multipleDisposalsAssetTypeForNonUkResidents.${assetType.toString}"
          )
        )
        .mkString(", ")
    }

    doc
      .select("#taxYear-answer")
      .text()                                   shouldBe s"${answers.taxYear.startDateInclusive.getYear}/${answers.taxYear.endDateExclusive.getYear}"
    doc.select("#completionDate-answer").text() shouldBe TimeUtils
      .govDisplayFormat(answers.completionDate.value)
  }

}

class DisabledIndirectDisposalMultipleDisposalsTriageControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with ScalaCheckDrivenPropertyChecks
    with RedirectToStartBehaviour
    with ReturnsServiceSupport {

  val mockTaxYearService = mock[TaxYearService]

  val mockUUIDGenerator = mock[UUIDGenerator]

  override val overrideBindings =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore),
      bind[TaxYearService].toInstance(mockTaxYearService),
      bind[UUIDGenerator].toInstance(mockUUIDGenerator),
      bind[ReturnsService].toInstance(mockReturnsService)
    )

  lazy val controller = instanceOf[MultipleDisposalsTriageController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  override lazy val additionalConfig = Configuration(
    "indirect-disposals.enabled" -> false,
    "mixed-use.enabled"          -> false
  )

  def performAction(): Future[Result] =
    controller.checkYourAnswers()(FakeRequest())

  val completeAnswersNonUk = CompleteMultipleDisposalsTriageAnswers(
    Some(IndividualUserType.Self),
    2,
    sample[Country],
    List(AssetType.Residential),
    sample[TaxYear],
    sample[CompletionDate]
  )

  val allQuestionsAnsweredNonUk = IncompleteMultipleDisposalsTriageAnswers(
    completeAnswersNonUk.individualUserType,
    Some(completeAnswersNonUk.numberOfProperties),
    Some(false),
    Some(completeAnswersNonUk.countryOfResidence),
    None,
    Some(completeAnswersNonUk.assetTypes),
    Some(true),
    Some(completeAnswersNonUk.taxYear),
    Some(completeAnswersNonUk.completionDate)
  )

  "redirect to the asset types not implemented page" when {
    "the user was not a non uk resident and they have selected asset types that are not supported" in {
      List(
        List(AssetType.IndirectDisposal, AssetType.MixedUse),
        List(AssetType.MixedUse, AssetType.IndirectDisposal)
      ).foreach { assetTypes =>
        testRedirectWhenIncomplete(
          allQuestionsAnsweredNonUk.copy(assetTypes = Some(assetTypes)),
          routes.CommonTriageQuestionsController.assetTypeNotYetImplemented()
        )
      }
    }

  }

  def testRedirectWhenIncomplete(
    answers: IncompleteMultipleDisposalsTriageAnswers,
    expectedRedirect: Call
  ): Unit = {
    inSequence {
      mockAuthWithNoRetrievals()
      mockGetSession(
        sessionDataWithStartingNewDraftReturn(answers)._1
      )
    }

    checkIsRedirect(performAction(), expectedRedirect)

  }

  def sessionDataWithStartingNewDraftReturn(
    multipleDisposalsAnswers: MultipleDisposalsTriageAnswers
  ): (SessionData, StartingNewDraftReturn) = {
    val startingNewDraftReturn =
      sample[StartingNewDraftReturn].copy(newReturnTriageAnswers = Left(multipleDisposalsAnswers))

    SessionData.empty.copy(journeyStatus = Some(startingNewDraftReturn)) -> startingNewDraftReturn
  }
}
