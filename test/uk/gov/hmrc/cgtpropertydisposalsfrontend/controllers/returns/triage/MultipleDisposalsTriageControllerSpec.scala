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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.triage

import cats.data.EitherT
import cats.instances.future._
import org.jsoup.nodes.Document
import org.scalatest.matchers.should.Matchers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.SampledScalaCheck
import play.api.http.Status.BAD_REQUEST
import play.api.i18n.{Lang, Messages, MessagesApi, MessagesImpl}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.{Call, Result}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.triage.MultipleDisposalsTriageControllerSpec.{SelectorAndValue, TagAttributePairAndValue, UserTypeDisplay}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.{ReturnsServiceSupport, StartingToAmendToFillingOutReturnSpecBehaviour, representee, triage}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, ControllerSpec, DateErrorScenarios, SessionSupport}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, PreviousReturnData, StartingNewDraftReturn, StartingToAmendReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Country
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.AddressGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.CompleteReturnGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.DraftReturnGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.Generators.{arb, sample}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.IdGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.JourneyStatusGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.NameGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.RepresenteeAnswersGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.ReturnGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.SubscribedDetailsGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.TaxYearGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.TriageQuestionsGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.YearToDateLiabilityAnswersGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.{AgentReferenceNumber, UUIDGenerator}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.{IndividualName, TrustName}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.SubscribedDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.AssetType.IndirectDisposal
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.CompleteReturn.CompleteMultipleIndirectDisposalReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.IndividualUserType.{Capacitor, PersonalRepresentative, PersonalRepresentativeInPeriodOfAdmin, Self}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.MultipleDisposalsTriageAnswers._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.RepresenteeAnswers.{CompleteRepresenteeAnswers, IncompleteRepresenteeAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.IncompleteSingleDisposalTriageAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.YearToDateLiabilityAnswers.NonCalculatedYTDAnswers.CompleteNonCalculatedYTDAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.YearToDateLiabilityAnswers.{CalculatedYTDAnswers, NonCalculatedYTDAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{CompleteReturnWithSummary, Error, JourneyStatus, SessionData, TaxYear, TimeUtils, UserType}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.{ReturnsService, TaxYearService}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.TaxYearExchanged.{currentTaxYear, cutoffTaxYear}

import java.time.LocalDate
import java.util.UUID
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.jdk.CollectionConverters._

class MultipleDisposalsTriageControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with SampledScalaCheck
    with RedirectToStartBehaviour
    with ReturnsServiceSupport
    with StartingToAmendToFillingOutReturnSpecBehaviour {
  private val mockTaxYearService = mock[TaxYearService]

  private val mockUUIDGenerator = mock[UUIDGenerator]

  private val trustDisplay =
    UserTypeDisplay(UserType.Organisation, None, Left(sample[TrustName]))

  private val agentDisplay =
    UserTypeDisplay(UserType.Agent, None, Right(sample[IndividualName]))

  private val individualDisplay =
    UserTypeDisplay(UserType.Individual, None, Right(sample[IndividualName]))

  private val personalRepDisplay =
    UserTypeDisplay(
      UserType.Individual,
      Some(PersonalRepresentative),
      Right(sample[IndividualName])
    )

  private val capacitorDisplay =
    UserTypeDisplay(
      UserType.Individual,
      Some(Capacitor),
      Right(sample[IndividualName])
    )

  private val periodOfAdminDisplay =
    UserTypeDisplay(
      UserType.Individual,
      Some(PersonalRepresentativeInPeriodOfAdmin),
      Right(sample[IndividualName])
    )

  protected override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore),
      bind[TaxYearService].toInstance(mockTaxYearService),
      bind[UUIDGenerator].toInstance(mockUUIDGenerator),
      bind[ReturnsService].toInstance(mockReturnsService)
    )

  private lazy val controller = instanceOf[MultipleDisposalsTriageController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  private def mockGetTaxYear(
    date: LocalDate
  )(response: Either[Error, Option[TaxYear]]) =
    (mockTaxYearService
      .taxYear(_: LocalDate)(using _: HeaderCarrier))
      .expects(date, *)
      .returning(EitherT.fromEither[Future](response))

  private def mockAvailableTaxYears()(response: Either[Error, List[Int]]) =
    (mockTaxYearService
      .availableTaxYears()(using _: HeaderCarrier))
      .expects(*)
      .returning(EitherT.fromEither[Future](response))

  private def isValidJourney(journeyStatus: JourneyStatus): Boolean =
    journeyStatus match {
      case r: StartingNewDraftReturn if r.newReturnTriageAnswers.isLeft             => true
      case FillingOutReturn(_, _, _, _: DraftMultipleDisposalsReturn, _, _)         => true
      case FillingOutReturn(_, _, _, _: DraftMultipleIndirectDisposalsReturn, _, _) => true
      case _: StartingToAmendReturn                                                 => true
      case _                                                                        => false
    }

  private def setIndividualUserType(
    displayType: UserTypeDisplay
  ): IndividualUserType =
    displayType.representativeType.getOrElse(Self)

  private def sessionDataWithStartingNewDraftReturn(
    multipleDisposalsAnswers: MultipleDisposalsTriageAnswers,
    name: Either[TrustName, IndividualName] = Right(sample[IndividualName]),
    userType: UserType = UserType.Individual,
    previousSentReturns: Option[List[ReturnSummary]] = None,
    representeeAnswers: Option[RepresenteeAnswers] = None
  ): (SessionData, StartingNewDraftReturn) = {
    val individualUserType     = multipleDisposalsAnswers.fold(_.individualUserType, _.individualUserType)
    val startingNewDraftReturn = sample[StartingNewDraftReturn].copy(
      newReturnTriageAnswers = Left(multipleDisposalsAnswers),
      subscribedDetails = sample[SubscribedDetails].copy(name = name),
      agentReferenceNumber = if (userType === UserType.Agent) Some(sample[AgentReferenceNumber]) else None,
      representeeAnswers = representeeAnswers.orElse {
        if (
          individualUserType.contains(PersonalRepresentative) || individualUserType
            .contains(PersonalRepresentativeInPeriodOfAdmin)
        ) {
          Some(
            sample[CompleteRepresenteeAnswers]
              .copy(dateOfDeath = Some(sample[DateOfDeath]))
          )
        } else if (individualUserType.contains(Capacitor)) {
          Some(sample[CompleteRepresenteeAnswers].copy(dateOfDeath = None))
        } else {
          None
        }
      },
      previousSentReturns = previousSentReturns.map(PreviousReturnData(_, None, None, None))
    )
    SessionData.empty.copy(
      journeyStatus = Some(startingNewDraftReturn),
      userType = Some(userType)
    ) -> startingNewDraftReturn
  }

  private def sessionDataWithFillingOutReturn(
    multipleDisposalsAnswers: MultipleDisposalsTriageAnswers,
    name: Either[TrustName, IndividualName] = Right(sample[IndividualName]),
    userType: UserType = UserType.Individual,
    representeeAnswers: Option[RepresenteeAnswers] = None,
    previousSentReturns: Option[List[ReturnSummary]] = None,
    amendReturnData: Option[AmendReturnData] = None
  ): (SessionData, FillingOutReturn, DraftMultipleDisposalsReturn) = {
    val individualUserType = multipleDisposalsAnswers.fold(_.individualUserType, _.individualUserType)
    val draftReturn        = sample[DraftMultipleDisposalsReturn].copy(
      triageAnswers = multipleDisposalsAnswers,
      representeeAnswers = representeeAnswers.orElse {
        if (
          individualUserType.contains(PersonalRepresentative) || individualUserType
            .contains(PersonalRepresentativeInPeriodOfAdmin)
        ) {
          Some(
            sample[CompleteRepresenteeAnswers]
              .copy(dateOfDeath = Some(sample[DateOfDeath]))
          )
        } else if (individualUserType.contains(Capacitor)) {
          Some(sample[CompleteRepresenteeAnswers].copy(dateOfDeath = None))
        } else {
          None
        }
      }
    )
    val journey            = sample[FillingOutReturn].copy(
      draftReturn = draftReturn,
      subscribedDetails = sample[SubscribedDetails].copy(name = name),
      agentReferenceNumber = if (userType === UserType.Agent) Some(sample[AgentReferenceNumber]) else None,
      previousSentReturns = previousSentReturns.map(PreviousReturnData(_, None, None, None)),
      amendReturnData = amendReturnData
    )
    val session            = SessionData.empty.copy(
      userType = Some(userType),
      journeyStatus = Some(journey)
    )
    (session, journey, draftReturn)
  }

  private def testFormError(
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
          .select("[data-spec='errorSummaryDisplay'] a")
          .text() shouldBe messageFromMessageKey(
          expectedErrorMessageKey,
          errorArgs*
        )
        doc.title() should startWith("Error:")
      },
      BAD_REQUEST
    )
  }

  private def mockGenerateUUID(uuid: UUID): Unit =
    (() => mockUUIDGenerator.nextId())
      .expects()
      .returning(uuid)

  private def getTaxYearExchanged(taxYear: Option[TaxYear]): Option[TaxYearExchanged] = {
    val currentTaxYear = TaxYearExchanged.currentTaxYear
    val validYears     = TaxYearExchanged.cutoffTaxYear to currentTaxYear
    taxYear match {
      case Some(t) if validYears.contains(t.startDateInclusive.getYear) =>
        Some(TaxYearExchanged(t.startDateInclusive.getYear))
      case _                                                            =>
        None
    }
  }

  "MultipleDisposalsTriageController" when {
    "handling requests to display the guidance page" must {
      def performAction(): Future[Result] =
        controller.guidance()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(() => performAction(), isValidJourney)

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.guidance(),
        mockUUIDGenerator
      )

      "display the page" when {
        def test(
          session: SessionData,
          expectedBackLink: Call,
          expectReturnToSummaryLink: Boolean,
          displayType: UserTypeDisplay
        ): Unit =
          testPageIsDisplayed(
            () => performAction(),
            session,
            s"multiple-disposals.guidance${displayType.getSubKey()}.title",
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
              capacitorDisplay,
              periodOfAdminDisplay
            ).foreach { (displayType: UserTypeDisplay) =>
              withClue(
                s"For user name ${displayType.name.fold(_ => "Trust name", _ => "Individual name")} ${displayType.getSubKey()} "
              ) {

                val answers = sample[CompleteMultipleDisposalsTriageAnswers]
                  .copy(individualUserType = Some(setIndividualUserType(displayType)))

                test(
                  sessionDataWithStartingNewDraftReturn(
                    answers,
                    displayType.name,
                    displayType.userType
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
              capacitorDisplay,
              periodOfAdminDisplay
            ).foreach { (displayType: UserTypeDisplay) =>
              withClue(
                s"For user type ${displayType.getSubKey()} "
              ) {
                test(
                  sessionDataWithFillingOutReturn(
                    sample[CompleteMultipleDisposalsTriageAnswers]
                      .copy(individualUserType = Some(setIndividualUserType(displayType))),
                    displayType.name,
                    displayType.userType
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

      behave like redirectToStartWhenInvalidJourney(() => performAction(), isValidJourney)

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.guidanceSubmit(),
        mockUUIDGenerator
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

          checkIsRedirect(performAction(), routes.MultipleDisposalsTriageController.howManyDisposals())
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

      behave like redirectToStartWhenInvalidJourney(() => performAction(), isValidJourney)

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.howManyDisposals(),
        mockUUIDGenerator
      )

      "display the page" when {
        def test(
          session: SessionData,
          expectedBackLink: Call,
          expectedButtonMessageKey: String,
          expectReturnToSummaryLink: Boolean
        ): Unit =
          testPageIsDisplayed(
            () => performAction(),
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
          FakeRequest().withFormUrlEncodedBody(data*).withMethod("POST")
        )

      val key = "multipleDisposalsNumberOfProperties"

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.howManyDisposalsSubmit(),
        mockUUIDGenerator
      )

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
              val amendReturnData                 = sample[AmendReturnData]
              val answers                         = IncompleteMultipleDisposalsTriageAnswers.empty.copy(
                individualUserType = Some(Self),
                numberOfProperties = Some(2)
              )
              val (session, journey, draftReturn) =
                sessionDataWithFillingOutReturn(
                  answers,
                  amendReturnData = Some(amendReturnData.copy(shouldDisplayGainOrLossAfterReliefs = false))
                )

              val updatedDraftReturn = draftReturn.copy(
                triageAnswers = answers.copy(numberOfProperties = Some(5)),
                examplePropertyDetailsAnswers = None,
                exemptionAndLossesAnswers = None,
                yearToDateLiabilityAnswers = None,
                supportingEvidenceAnswers = None,
                gainOrLossAfterReliefs = None,
                lastUpdatedDate = TimeUtils.today()
              )
              val updatedJourney     = journey.copy(
                draftReturn = updatedDraftReturn,
                amendReturnData = Some(amendReturnData.copy(shouldDisplayGainOrLossAfterReliefs = true))
              )

              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
                mockStoreDraftReturn(updatedJourney)(
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
        def test(data: (String, String)*)(expectedErrorMessageKey: String): Unit =
          testFormError(data*)(expectedErrorMessageKey)(s"$key.title")(
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
          gainOrLossAfterReliefs = None,
          lastUpdatedDate = TimeUtils.today()
        )
        val updatedJourney                  = journey.copy(draftReturn = updatedDraftReturn)

        "there is an error storing the draft return" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(updatedJourney)(
              Left(Error(""))
            )
          }

          checkIsTechnicalErrorPage(performAction(key -> "5"))
        }

        "there is an error updating the session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(updatedJourney)(
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

      behave like redirectToStartWhenInvalidJourney(() => performAction(), isValidJourney)

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.wereYouAUKResident(),
        mockUUIDGenerator
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
            () => performAction(),
            session,
            s"multipleDisposalsWereYouAUKResident${displayType.getSubKey(separatePeriodOfAdminKey = true)}.title",
            routes.MultipleDisposalsTriageController.wereYouAUKResidentSubmit(),
            expectedBackLink,
            expectedButtonMessageKey,
            expectReturnToSummaryLink,
            List(
              SelectorAndValue(
                "form > p",
                messageFromMessageKey(
                  s"multipleDisposalsWereYouAUKResident${displayType.getSubKey(separatePeriodOfAdminKey = true)}.link",
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
              capacitorDisplay,
              periodOfAdminDisplay
            ).foreach { (displayType: UserTypeDisplay) =>
              withClue(
                s"For user type ${displayType.getSubKey(separatePeriodOfAdminKey = true)}"
              ) {
                test(
                  sessionDataWithStartingNewDraftReturn(
                    IncompleteMultipleDisposalsTriageAnswers.empty
                      .copy(individualUserType = Some(setIndividualUserType(displayType))),
                    displayType.name,
                    displayType.userType
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
              capacitorDisplay,
              periodOfAdminDisplay
            ).foreach { (displayType: UserTypeDisplay) =>
              withClue(
                s"For user type ${displayType.getSubKey(separatePeriodOfAdminKey = true)}"
              ) {
                test(
                  sessionDataWithStartingNewDraftReturn(
                    sample[CompleteMultipleDisposalsTriageAnswers]
                      .copy(individualUserType = Some(setIndividualUserType(displayType))),
                    displayType.name,
                    displayType.userType
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
              capacitorDisplay,
              periodOfAdminDisplay
            ).foreach { (displayType: UserTypeDisplay) =>
              withClue(
                s"For user type ${displayType.getSubKey(separatePeriodOfAdminKey = true)}"
              ) {
                test(
                  sessionDataWithFillingOutReturn(
                    sample[CompleteMultipleDisposalsTriageAnswers]
                      .copy(individualUserType = Some(setIndividualUserType(displayType))),
                    displayType.name,
                    displayType.userType
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
          FakeRequest().withFormUrlEncodedBody(data*).withMethod("POST")
        )

      def updateDraftReturn(
        d: DraftMultipleDisposalsReturn,
        newAnswers: MultipleDisposalsTriageAnswers
      ): DraftMultipleDisposalsReturn =
        d.copy(
          triageAnswers = newAnswers,
          examplePropertyDetailsAnswers = d.examplePropertyDetailsAnswers.map(
            _.unset(_.address)
              .unset(_.disposalPrice)
              .unset(_.acquisitionPrice)
          ),
          yearToDateLiabilityAnswers = None,
          supportingEvidenceAnswers = None,
          gainOrLossAfterReliefs = None
        )

      val key = "multipleDisposalsWereYouAUKResident"

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.wereYouAUKResidentSubmit(),
        mockUUIDGenerator
      )

      "redirect to cya page" when {
        val taxYear = sample[TaxYear].copy(
          startDateInclusive = LocalDate.of(cutoffTaxYear, 4, 6),
          endDateExclusive = LocalDate.of(cutoffTaxYear + 1, 4, 6)
        )

        val answers = IncompleteMultipleDisposalsTriageAnswers.empty.copy(
          individualUserType = Some(Self),
          numberOfProperties = Some(2),
          taxYearExchanged = getTaxYearExchanged(Some(taxYear)),
          taxYear = Some(taxYear)
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
              forAll { (c: CompleteMultipleDisposalsTriageAnswers) =>
                val answers                         = c.copy(
                  countryOfResidence = sample[Country],
                  taxYearExchanged = getTaxYearExchanged(Some(taxYear)),
                  taxYear = taxYear
                )
                val (session, journey, draftReturn) = sessionDataWithFillingOutReturn(answers)

                val updatedAnswers     = IncompleteMultipleDisposalsTriageAnswers(
                  individualUserType = answers.individualUserType,
                  numberOfProperties = Some(answers.numberOfProperties),
                  wasAUKResident = Some(true),
                  countryOfResidence = None,
                  wereAllPropertiesResidential = None,
                  assetTypes = None,
                  taxYearExchanged = answers.taxYearExchanged,
                  taxYear = Some(answers.taxYear),
                  alreadySentSelfAssessment = answers.alreadySentSelfAssessment,
                  completionDate = Some(answers.completionDate)
                )
                val updatedDraftReturn =
                  updateDraftReturn(draftReturn, updatedAnswers)
                val updatedJourney     =
                  journey.copy(draftReturn = updatedDraftReturn)

                inSequence {
                  mockAuthWithNoRetrievals()
                  mockGetSession(session)
                  mockStoreDraftReturn(updatedJourney)(
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
            personalRepDisplay,
            periodOfAdminDisplay
          ).foreach { (displayType: UserTypeDisplay) =>
            withClue(
              s"For user type ${displayType.getSubKey(separatePeriodOfAdminKey = true)}"
            ) {
              val answers = IncompleteMultipleDisposalsTriageAnswers.empty.copy(
                individualUserType = Some(setIndividualUserType(displayType)),
                numberOfProperties = Some(2),
                alreadySentSelfAssessment = Some(false)
              )
              val session = sessionDataWithStartingNewDraftReturn(
                answers,
                displayType.name,
                displayType.userType
              )._1.copy(userType = Some(displayType.userType))

              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
              }

              checkPageIsDisplayed(
                performAction(),
                messageFromMessageKey(s"$key${displayType.getSubKey(separatePeriodOfAdminKey = true)}.title"),
                doc =>
                  doc
                    .select("[data-spec='errorSummaryDisplay'] a")
                    .text() shouldBe messageFromMessageKey(
                    s"$key${displayType.getSubKey(separatePeriodOfAdminKey = true)}.error.required"
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
        val (session, journey, draftReturn) = sessionDataWithFillingOutReturn(answers)

        val updatedAnswers     = IncompleteMultipleDisposalsTriageAnswers(
          individualUserType = answers.individualUserType,
          numberOfProperties = Some(answers.numberOfProperties),
          wasAUKResident = Some(true),
          countryOfResidence = None,
          wereAllPropertiesResidential = None,
          assetTypes = None,
          taxYearExchanged = answers.taxYearExchanged,
          taxYear = Some(answers.taxYear),
          alreadySentSelfAssessment = answers.alreadySentSelfAssessment,
          completionDate = Some(answers.completionDate)
        )
        val updatedDraftReturn = updateDraftReturn(draftReturn, updatedAnswers)
        val updatedJourney     = journey.copy(draftReturn = updatedDraftReturn)

        "there is an error updating the draft return" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(updatedJourney)(
              Left(Error(""))
            )
          }

          checkIsTechnicalErrorPage(performAction(key -> "true"))
        }

        "there is an error updating the session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(updatedJourney)(
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

      behave like redirectToStartWhenInvalidJourney(() => performAction(), isValidJourney)

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.wereAllPropertiesResidential(),
        mockUUIDGenerator
      )

      "display the page" when {
        def test(
          session: SessionData,
          expectedBackLink: Call,
          expectedButtonMessageKey: String,
          expectReturnToSummaryLink: Boolean
        ): Unit =
          testPageIsDisplayed(
            () => performAction(),
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
          FakeRequest().withFormUrlEncodedBody(data*).withMethod("POST")
        )

      val key = "multipleDisposalsWereAllPropertiesResidential"

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.wereAllPropertiesResidentialSubmit(),
        mockUUIDGenerator
      )

      "redirect to cya page" when {
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
                taxYearExchanged = answers.taxYearExchanged,
                taxYear = Some(answers.taxYear),
                alreadySentSelfAssessment = answers.alreadySentSelfAssessment,
                completionDate = Some(answers.completionDate)
              )
              val updatedDraftReturn =
                draftReturn.copy(triageAnswers = updatedAnswers)
              val updatedJourney     = journey.copy(draftReturn = updatedDraftReturn)

              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
                mockStoreDraftReturn(updatedJourney)(
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

          "the user was on an indirect disposals journey" in {
            val draftReturn      = sample[DraftMultipleIndirectDisposalsReturn].copy(
              triageAnswers = answers
            )
            val fillingOutReturn = sample[FillingOutReturn].copy(draftReturn = draftReturn)

            val updatedDraftReturn = DraftMultipleDisposalsReturn.newDraftReturn(
              draftReturn.id,
              answers.copy(assetTypes = Some(List(AssetType.Residential)), wereAllPropertiesResidential = Some(true)),
              draftReturn.representeeAnswers
            )

            val updatedFillingOutReturn = fillingOutReturn.copy(draftReturn = updatedDraftReturn)

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                SessionData.empty.copy(
                  journeyStatus = Some(fillingOutReturn)
                )
              )
              mockStoreDraftReturn(updatedFillingOutReturn)(Right(()))
              mockStoreSession(
                SessionData.empty.copy(
                  journeyStatus = Some(updatedFillingOutReturn)
                )
              )(Right(()))
            }

            checkIsRedirect(
              performAction(key -> "true"),
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
                .select("[data-spec='errorSummaryDisplay'] a")
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
          taxYearExchanged = answers.taxYearExchanged,
          taxYear = Some(answers.taxYear),
          alreadySentSelfAssessment = answers.alreadySentSelfAssessment,
          completionDate = Some(answers.completionDate)
        )
        val updatedDraftReturn =
          draftReturn.copy(triageAnswers = updatedAnswers)
        val updatedJourney     = journey.copy(draftReturn = updatedDraftReturn)

        "there is an error updating the draft return" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(updatedJourney)(
              Left(Error(""))
            )
          }

          checkIsTechnicalErrorPage(performAction(key -> "false"))
        }

        "there is an error updating the session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(updatedJourney)(
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

      behave like redirectToStartWhenInvalidJourney(() => performAction(), isValidJourney)

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.whenWereContractsExchanged(),
        mockUUIDGenerator
      )

      "redirect to the check your answers endpoint" when {
        "the user is on an indirect disposal journey" in {
          val incompleteAnswers =
            IncompleteMultipleDisposalsTriageAnswers.empty.copy(
              individualUserType = Some(Self),
              numberOfProperties = Some(2),
              wasAUKResident = Some(false),
              countryOfResidence = Some(sample[Country]),
              assetTypes = Some(List(AssetType.IndirectDisposal))
            )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionDataWithFillingOutReturn(incompleteAnswers)._1)
          }

          checkIsRedirect(performAction(), routes.MultipleDisposalsTriageController.checkYourAnswers())
        }
      }

      "display the page" when {
        def test(
          session: SessionData,
          expectedBackLink: Call,
          expectedButtonMessageKey: String,
          expectReturnToSummaryLink: Boolean
        ): Unit =
          testTaxYearExchangedPageIsDisplayed(
            () => performAction(),
            session,
            "multipleDisposalsTaxYear.title",
            routes.MultipleDisposalsTriageController
              .whenWereContractsExchangedSubmit(),
            expectedBackLink,
            expectedButtonMessageKey,
            expectReturnToSummaryLink
          )

        val incompleteAnswers = IncompleteMultipleDisposalsTriageAnswers.empty.copy(
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
                sample[CompleteMultipleDisposalsTriageAnswers].copy(
                  assetTypes = List(AssetType.Residential)
                )
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
                sample[CompleteMultipleDisposalsTriageAnswers].copy(
                  assetTypes = List(AssetType.Residential)
                )
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
          FakeRequest().withFormUrlEncodedBody(data*).withMethod("POST")
        )

      val cutoffTaxYearDate    = LocalDate.of(cutoffTaxYear, 4, 6)
      val currentTaxYearMinus3 = LocalDate.of(currentTaxYear - 3, 4, 6)
      val currentTaxYearMinus2 = LocalDate.of(currentTaxYear - 2, 4, 6)
      val currentTaxYearMinus1 = LocalDate.of(currentTaxYear - 1, 4, 6)
      val currentTaxYearDate   = LocalDate.of(currentTaxYear, 4, 6)

      val key = "multipleDisposalsTaxYear"

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.whenWereContractsExchangedSubmit(),
        mockUUIDGenerator
      )

      "redirect to cya page" when {
        val taxYear = sample[TaxYear].copy(
          startDateInclusive = LocalDate.of(cutoffTaxYear, 4, 6),
          endDateExclusive = LocalDate.of(cutoffTaxYear + 1, 4, 6)
        )

        val answers = IncompleteMultipleDisposalsTriageAnswers.empty.copy(
          individualUserType = Some(Self),
          numberOfProperties = Some(2),
          wasAUKResident = Some(true),
          countryOfResidence = Some(Country.uk),
          wereAllPropertiesResidential = Some(true),
          assetTypes = Some(List(AssetType.Residential)),
          taxYear = None,
          taxYearExchanged = None
        )

        "the user is on an indirect disposal journey" in {
          val incompleteAnswers = IncompleteMultipleDisposalsTriageAnswers.empty.copy(
            individualUserType = Some(Self),
            numberOfProperties = Some(2),
            wasAUKResident = Some(false),
            countryOfResidence = Some(sample[Country]),
            assetTypes = Some(List(AssetType.IndirectDisposal)),
            taxYearExchanged = Some(TaxYearExchanged(cutoffTaxYear))
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionDataWithFillingOutReturn(incompleteAnswers)._1)
          }

          checkIsRedirect(performAction(), routes.MultipleDisposalsTriageController.checkYourAnswers())
        }

        "the user has not started a draft return and" when {
          val (session, journey) = sessionDataWithStartingNewDraftReturn(answers)

          "user has not answered the tax year exchanged section and selects currentTaxYear" in {
            val taxYear = sample[TaxYear].copy(
              startDateInclusive = LocalDate.of(currentTaxYear, 4, 6),
              endDateExclusive = LocalDate.of(currentTaxYear + 1, 4, 6)
            )

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockAvailableTaxYears()(Right(List(currentTaxYear)))
              mockGetTaxYear(currentTaxYearDate)(Right(Some(taxYear)))
              mockStoreSession(
                session.copy(journeyStatus =
                  Some(
                    journey.copy(
                      newReturnTriageAnswers = Left(
                        answers.copy(
                          taxYearExchanged = getTaxYearExchanged(Some(taxYear)),
                          taxYear = Some(taxYear)
                        )
                      )
                    )
                  )
                )
              )(Right(()))
            }

            checkIsRedirect(
              performAction(key -> s"$currentTaxYear"),
              routes.MultipleDisposalsTriageController.checkYourAnswers()
            )
          }

          "user has not answered the tax year exchanged section and selects after currentTaxYear minus 1" in {
            val taxYear = sample[TaxYear].copy(
              startDateInclusive = LocalDate.of(currentTaxYear - 1, 4, 6),
              endDateExclusive = LocalDate.of(currentTaxYear, 4, 6)
            )

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockAvailableTaxYears()(Right(List(currentTaxYear - 1)))
              mockGetTaxYear(currentTaxYearMinus1)(Right(Some(taxYear)))
              mockStoreSession(
                session.copy(journeyStatus =
                  Some(
                    journey.copy(
                      newReturnTriageAnswers = Left(
                        answers.copy(
                          taxYearExchanged = getTaxYearExchanged(Some(taxYear)),
                          taxYear = Some(taxYear)
                        )
                      )
                    )
                  )
                )
              )(Right(()))
            }

            checkIsRedirect(
              performAction(key -> s"${currentTaxYear - 1}"),
              routes.MultipleDisposalsTriageController.checkYourAnswers()
            )
          }

          "user has not answered the tax year exchanged section and selects after currentTaxYear minus 2" in {
            val taxYear = sample[TaxYear].copy(
              startDateInclusive = LocalDate.of(currentTaxYear - 2, 4, 6),
              endDateExclusive = LocalDate.of(currentTaxYear - 1, 4, 6)
            )

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockAvailableTaxYears()(Right(List(currentTaxYear - 2)))
              mockGetTaxYear(currentTaxYearMinus2)(Right(Some(taxYear)))
              mockStoreSession(
                session.copy(journeyStatus =
                  Some(
                    journey.copy(
                      newReturnTriageAnswers = Left(
                        answers.copy(
                          taxYearExchanged = getTaxYearExchanged(Some(taxYear)),
                          taxYear = Some(taxYear)
                        )
                      )
                    )
                  )
                )
              )(Right(()))
            }

            checkIsRedirect(
              performAction(key -> s"${currentTaxYear - 2}"),
              routes.MultipleDisposalsTriageController.checkYourAnswers()
            )
          }

          "user has not answered the tax year exchanged section and selects after currentTaxYear minus 3" in {
            val taxYear = sample[TaxYear].copy(
              startDateInclusive = LocalDate.of(currentTaxYear - 3, 4, 6),
              endDateExclusive = LocalDate.of(currentTaxYear - 2, 4, 6)
            )

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockAvailableTaxYears()(Right(List(currentTaxYear - 3)))
              mockGetTaxYear(currentTaxYearMinus3)(Right(Some(taxYear)))
              mockStoreSession(
                session.copy(journeyStatus =
                  Some(
                    journey.copy(
                      newReturnTriageAnswers = Left(
                        answers.copy(
                          taxYearExchanged = getTaxYearExchanged(Some(taxYear)),
                          taxYear = Some(taxYear)
                        )
                      )
                    )
                  )
                )
              )(Right(()))
            }

            checkIsRedirect(
              performAction(key -> s"${currentTaxYear - 3}"),
              routes.MultipleDisposalsTriageController.checkYourAnswers()
            )
          }

          "user has not answered the tax year exchanged section and selects after cutoffTaxYear" in {
            val taxYear = sample[TaxYear].copy(
              startDateInclusive = LocalDate.of(cutoffTaxYear, 4, 6),
              endDateExclusive = LocalDate.of(cutoffTaxYear + 1, 4, 6)
            )

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockAvailableTaxYears()(Right(List(cutoffTaxYear)))
              mockGetTaxYear(cutoffTaxYearDate)(Right(Some(taxYear)))
              mockStoreSession(
                session.copy(journeyStatus =
                  Some(
                    journey.copy(
                      newReturnTriageAnswers = Left(
                        answers.copy(
                          taxYearExchanged = getTaxYearExchanged(Some(taxYear)),
                          taxYear = Some(taxYear)
                        )
                      )
                    )
                  )
                )
              )(Right(()))
            }

            checkIsRedirect(
              performAction(key -> s"$cutoffTaxYear"),
              routes.MultipleDisposalsTriageController.checkYourAnswers()
            )
          }

          "user has not answered the tax year exchanged section and selects before cutoffTaxYear" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockAvailableTaxYears()(Right(List()))
              mockGetTaxYear(TimeUtils.getTaxYearStartDate(TaxYearExchanged.taxYearExchangedTooEarly.year))(
                Right(None)
              )
              mockStoreSession(
                session.copy(journeyStatus =
                  Some(
                    journey.copy(
                      newReturnTriageAnswers = Left(
                        answers.copy(
                          taxYearExchanged = Some(TaxYearExchanged.taxYearExchangedTooEarly),
                          taxYear = None
                        )
                      )
                    )
                  )
                )
              )(Right(()))
            }

            checkIsRedirect(
              performAction(key -> s"-$cutoffTaxYear"),
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
                taxYearExchanged = Some(TaxYearExchanged(cutoffTaxYear)),
                taxYear = Some(taxYear),
                alreadySentSelfAssessment = None,
                completionDate = Some(sample[CompletionDate])
              )

            val (session, journey) =
              sessionDataWithStartingNewDraftReturn(answers)

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockAvailableTaxYears()(Right(List()))
              mockGetTaxYear(TimeUtils.getTaxYearStartDate(TaxYearExchanged.taxYearExchangedTooEarly.year))(
                Right(None)
              )
              mockStoreSession(
                session.copy(journeyStatus =
                  Some(
                    journey.copy(
                      newReturnTriageAnswers = Left(
                        answers.copy(
                          taxYearExchanged = Some(TaxYearExchanged.taxYearExchangedTooEarly),
                          taxYear = None,
                          completionDate = None,
                          alreadySentSelfAssessment = None
                        )
                      )
                    )
                  )
                )
              )(Right(()))
            }

            checkIsRedirect(
              performAction(key -> s"-$cutoffTaxYear"),
              routes.MultipleDisposalsTriageController.checkYourAnswers()
            )
          }

          "personalRepAdmin user has not answered the tax year exchanged section, date of death is 1-1-2018 " +
            "and selects tax year exchanged as currentTaxYear minus 3" in {
              val taxYear = sample[TaxYear].copy(
                startDateInclusive = LocalDate.of(currentTaxYear - 3, 4, 6),
                endDateExclusive = LocalDate.of(currentTaxYear - 2, 4, 6)
              )
              val answers = sample[IncompleteMultipleDisposalsTriageAnswers].copy(
                individualUserType = Some(PersonalRepresentativeInPeriodOfAdmin),
                numberOfProperties = Some(2),
                wasAUKResident = Some(true),
                countryOfResidence = Some(Country.uk),
                wereAllPropertiesResidential = Some(true),
                assetTypes = Some(List(AssetType.Residential)),
                taxYearExchanged = None,
                taxYear = None,
                alreadySentSelfAssessment = None,
                completionDate = None
              )

              val dateOfDeath        = DateOfDeath(LocalDate.of(2018, 1, 1))
              val representeeAnswers = sample[CompleteRepresenteeAnswers].copy(dateOfDeath = Some(dateOfDeath))

              val (session, journey) = sessionDataWithStartingNewDraftReturn(
                answers,
                representeeAnswers = Some(representeeAnswers)
              )

              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
                mockAvailableTaxYears()(Right(List(currentTaxYear - 3)))
                mockGetTaxYear(currentTaxYearMinus3)(Right(Some(taxYear)))
                mockStoreSession(
                  session.copy(journeyStatus =
                    Some(
                      journey.copy(
                        newReturnTriageAnswers = Left(
                          answers.copy(
                            taxYearExchanged = getTaxYearExchanged(Some(taxYear)),
                            taxYear = Some(taxYear)
                          )
                        )
                      )
                    )
                  )
                )(Right(()))
              }

              checkIsRedirect(
                performAction(key -> s"${currentTaxYear - 3}"),
                routes.MultipleDisposalsTriageController.checkYourAnswers()
              )
            }
        }

        "the user has started a draft return and" when {
          "have completed the section and they enter a figure which is " +
            "different than one they have already entered" in {
              val cutoffTaxYearDateRange = sample[TaxYear].copy(
                startDateInclusive = LocalDate.of(cutoffTaxYear, 4, 6),
                endDateExclusive = LocalDate.of(currentTaxYear - 3, 4, 6)
              )
              val currentTaxYearMinus3   = sample[TaxYear].copy(
                startDateInclusive = LocalDate.of(currentTaxYear - 3, 4, 6),
                endDateExclusive = LocalDate.of(currentTaxYear - 2, 4, 6)
              )
              forAll { (c: CompleteMultipleDisposalsTriageAnswers) =>
                val amendReturnData                 = sample[AmendReturnData]
                val answers                         = c.copy(
                  individualUserType = Some(Self),
                  taxYear = currentTaxYearMinus3,
                  assetTypes = List(AssetType.Residential),
                  taxYearExchanged = Some(TaxYearExchanged(currentTaxYear - 3)),
                  alreadySentSelfAssessment = None
                )
                val (session, journey, draftReturn) =
                  sessionDataWithFillingOutReturn(
                    answers,
                    amendReturnData = Some(amendReturnData.copy(shouldDisplayGainOrLossAfterReliefs = false))
                  )

                val updatedAnswers     = IncompleteMultipleDisposalsTriageAnswers
                  .fromCompleteAnswers(answers)
                  .copy(
                    taxYearExchanged = Some(TaxYearExchanged(cutoffTaxYear)),
                    taxYear = Some(cutoffTaxYearDateRange),
                    alreadySentSelfAssessment = None,
                    completionDate = None
                  )
                val updatedDraftReturn = draftReturn.copy(
                  triageAnswers = updatedAnswers,
                  examplePropertyDetailsAnswers = draftReturn.examplePropertyDetailsAnswers.map(
                    _.unset(_.disposalDate)
                  )
                )
                val updatedJourney     =
                  journey.copy(
                    draftReturn = updatedDraftReturn,
                    amendReturnData = Some(
                      amendReturnData.copy(
                        shouldDisplayGainOrLossAfterReliefs = true
                      )
                    )
                  )

                inSequence {
                  mockAuthWithNoRetrievals()
                  mockGetSession(session)
                  mockAvailableTaxYears()(Right(List(cutoffTaxYear)))
                  mockGetTaxYear(cutoffTaxYearDate)(Right(Some(cutoffTaxYearDateRange)))
                  mockStoreDraftReturn(updatedJourney)(
                    Right(())
                  )
                  mockStoreSession(
                    session.copy(journeyStatus = Some(updatedJourney))
                  )(Right(()))
                }

                checkIsRedirect(
                  performAction(key -> s"$cutoffTaxYear"),
                  routes.MultipleDisposalsTriageController.checkYourAnswers()
                )
              }
            }
        }
      }

      "not update the session" when {
        "user has already answered the tax year exchanged section and re-selected same option" in {
          val cutoffTaxYearDate = sample[TaxYear].copy(
            startDateInclusive = LocalDate.of(cutoffTaxYear, 4, 6),
            endDateExclusive = LocalDate.of(currentTaxYear - 3, 4, 6)
          )
          val answers           = sample[IncompleteMultipleDisposalsTriageAnswers]
            .copy(
              individualUserType = Some(Self),
              numberOfProperties = Some(2),
              wasAUKResident = Some(true),
              countryOfResidence = Some(Country.uk),
              wereAllPropertiesResidential = Some(true),
              assetTypes = Some(List(AssetType.Residential)),
              taxYearExchanged = Some(TaxYearExchanged(cutoffTaxYear)),
              taxYear = Some(cutoffTaxYearDate)
            )

          val (session, _) = sessionDataWithStartingNewDraftReturn(answers)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockAvailableTaxYears()(Right(List(cutoffTaxYear)))
          }

          checkIsRedirect(
            performAction(key -> s"$cutoffTaxYear"),
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
            mockAvailableTaxYears()(Right(List()))
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(s"$key.title"),
            { doc =>
              doc
                .select("[data-spec='errorSummaryDisplay'] a")
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
        val cutoffTaxYearDateRange = sample[TaxYear].copy(
          startDateInclusive = LocalDate.of(cutoffTaxYear, 4, 6),
          endDateExclusive = LocalDate.of(currentTaxYear - 3, 4, 6)
        )

        val answers                         = sample[CompleteMultipleDisposalsTriageAnswers].copy(
          assetTypes = List(AssetType.Residential),
          taxYearExchanged = Some(TaxYearExchanged(cutoffTaxYear)),
          taxYear = cutoffTaxYearDateRange,
          alreadySentSelfAssessment = Some(false)
        )
        val (session, journey, draftReturn) =
          sessionDataWithFillingOutReturn(answers)

        val updatedAnswers     = IncompleteMultipleDisposalsTriageAnswers
          .fromCompleteAnswers(answers)
          .copy(
            taxYear = None,
            completionDate = None,
            taxYearExchanged = Some(TaxYearExchanged.taxYearExchangedTooEarly),
            alreadySentSelfAssessment = None
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
            mockAvailableTaxYears()(Right(List()))
            mockGetTaxYear(TimeUtils.getTaxYearStartDate(TaxYearExchanged.taxYearExchangedTooEarly.year))(
              Right(None)
            )
            mockStoreDraftReturn(updatedJourney)(
              Left(Error(""))
            )
          }
          checkIsTechnicalErrorPage(performAction(key -> s"-$cutoffTaxYear"))
        }

        "there is an error updating the session data" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockAvailableTaxYears()(Right(List()))
            mockGetTaxYear(TimeUtils.getTaxYearStartDate(TaxYearExchanged.taxYearExchangedTooEarly.year))(
              Right(None)
            )
            mockStoreDraftReturn(updatedJourney)(
              Right(())
            )
            mockStoreSession(
              session.copy(journeyStatus = Some(updatedJourney))
            )(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction(key -> s"-$cutoffTaxYear"))
        }

        "a tax year cannot be found when the user selects after cutoffTaxYear" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithFillingOutReturn(
                IncompleteMultipleDisposalsTriageAnswers.empty
              )._1
            )
            mockAvailableTaxYears()(Right(List(cutoffTaxYear)))
            mockGetTaxYear(cutoffTaxYearDate)(Right(None))
          }

          checkIsTechnicalErrorPage(performAction(key -> s"$cutoffTaxYear"))
        }

        "there is an error while getting the tax year when the user selects after cutoffTaxYear" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithFillingOutReturn(
                IncompleteMultipleDisposalsTriageAnswers.empty
              )._1
            )
            mockAvailableTaxYears()(Right(List(cutoffTaxYear)))
            mockGetTaxYear(cutoffTaxYearDate)(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction(key -> s"$cutoffTaxYear"))
        }
      }
    }

    "handling requests to display the country of residence page" must {
      def performAction(): Future[Result] =
        controller.countryOfResidence()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(() => performAction(), isValidJourney)

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.countryOfResidence(),
        mockUUIDGenerator
      )

      "display the page" when {
        def test(
          session: SessionData,
          expectedBackLink: Call,
          expectedButtonMessageKey: String,
          expectReturnToSummaryLink: Boolean,
          displayType: UserTypeDisplay,
          representeeAnswers: Option[RepresenteeAnswers]
        ): Unit = {
          val userKey         = displayType.getSubKey(separatePeriodOfAdminKey = true)
          val isPeriodOfAdmin = displayType.representativeType.contains(PersonalRepresentativeInPeriodOfAdmin)
          val titleArgs       =
            if (isPeriodOfAdmin) {
              representeeAnswers
                .flatMap(_.fold(_.dateOfDeath, _.dateOfDeath))
                .map { dateOfDeath =>
                  val taxYear = TimeUtils.taxYearStart(dateOfDeath.value)
                  List(
                    taxYear.getYear.toString,
                    (taxYear.getYear + 1).toString
                  )
                }
                .getOrElse(sys.error("Could not find date of death for period of admin"))
            } else {
              List.empty
            }

          testPageIsDisplayed(
            () => performAction(),
            session,
            s"multipleDisposalsCountryOfResidence$userKey.title",
            routes.MultipleDisposalsTriageController.countryOfResidenceSubmit(),
            expectedBackLink,
            expectedButtonMessageKey,
            expectReturnToSummaryLink,
            List(
              SelectorAndValue(
                "#countryCode-hint",
                if (isPeriodOfAdmin) {
                  ""
                } else {
                  messageFromMessageKey(s"multipleDisposalsCountryOfResidence$userKey.helpText")
                }
              ),
              SelectorAndValue(
                "form > p:first-of-type",
                messageFromMessageKey(
                  s"triage.enterCountry$userKey.link",
                  viewConfig.workOurYouResidenceStatusUrl
                )
              )
            ),
            titleMessageArgs = titleArgs
          )
        }
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
              personalRepDisplay,
              periodOfAdminDisplay
            ).foreach { (displayType: UserTypeDisplay) =>
              withClue(
                s"For user type ${displayType.getSubKey(separatePeriodOfAdminKey = true)}"
              ) {
                val (session, startingNewDraftReturn) =
                  sessionDataWithStartingNewDraftReturn(
                    incompleteAnswers.copy(
                      individualUserType = Some(setIndividualUserType(displayType)),
                      numberOfProperties = Some(2),
                      wasAUKResident = Some(false)
                    ),
                    displayType.name,
                    displayType.userType
                  )

                test(
                  session,
                  triage.routes.MultipleDisposalsTriageController
                    .wereYouAUKResident(),
                  "button.continue",
                  expectReturnToSummaryLink = false,
                  displayType,
                  startingNewDraftReturn.representeeAnswers
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
              capacitorDisplay,
              periodOfAdminDisplay
            ).foreach { (displayType: UserTypeDisplay) =>
              withClue(
                s"For user type ${displayType.getSubKey(separatePeriodOfAdminKey = true)}"
              ) {
                val (session, startingNewDraftReturn) =
                  sessionDataWithStartingNewDraftReturn(
                    sample[CompleteMultipleDisposalsTriageAnswers]
                      .copy(individualUserType = Some(setIndividualUserType(displayType))),
                    displayType.name,
                    displayType.userType
                  )
                test(
                  session,
                  triage.routes.MultipleDisposalsTriageController
                    .checkYourAnswers(),
                  "button.continue",
                  expectReturnToSummaryLink = false,
                  displayType,
                  startingNewDraftReturn.representeeAnswers
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
              individualDisplay,
              None
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
              individualDisplay,
              None
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
          FakeRequest().withFormUrlEncodedBody(data*).withMethod("POST")
        )

      def updateDraftReturn(
        d: DraftMultipleDisposalsReturn,
        newAnswers: MultipleDisposalsTriageAnswers
      ): DraftMultipleDisposalsReturn =
        d.copy(
          triageAnswers = newAnswers,
          yearToDateLiabilityAnswers = None
        )

      val key     = "countryCode"
      val country = Country("HK")

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.countryOfResidenceSubmit(),
        mockUUIDGenerator
      )

      "redirect to cya page" when {
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
                performAction(key -> country.code),
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

              val newCountry = Country("CH")

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
                performAction(key -> newCountry.code),
                routes.MultipleDisposalsTriageController.checkYourAnswers()
              )
            }
        }

        "the user has started a draft return and" when {
          "have completed the section and they enter a country which is " +
            "different than one they have already entered" in {
              forAll { (c: CompleteMultipleDisposalsTriageAnswers) =>
                val answers = c.copy(countryOfResidence = Country("FI"))

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
                  mockStoreDraftReturn(updatedJourney)(
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

          "the user is on an amend journey where the estimates answer should be preserved" in {
            forAll { (c: CompleteMultipleDisposalsTriageAnswers) =>
              val answers = c.copy(countryOfResidence = Country("FI"))

              val (session, journey, draftReturn) =
                sessionDataWithFillingOutReturn(
                  answers,
                  amendReturnData = Some(
                    sample[AmendReturnData].copy(
                      originalReturn = sample[CompleteReturnWithSummary].copy(
                        completeReturn = sample[CompleteMultipleIndirectDisposalReturn].copy(
                          yearToDateLiabilityAnswers = sample[CompleteNonCalculatedYTDAnswers].copy(
                            hasEstimatedDetails = false
                          )
                        )
                      )
                    )
                  )
                )

              val updatedAnswers     = answers.copy(countryOfResidence = country)
              val updatedDraftReturn =
                updateDraftReturn(draftReturn, updatedAnswers)
              val updatedJourney     =
                journey.copy(draftReturn = updatedDraftReturn)

              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
                mockStoreDraftReturn(updatedJourney)(
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
                .select("[data-spec='errorSummaryDisplay'] a")
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
                .select("[data-spec='errorSummaryDisplay'] a")
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
            mockStoreDraftReturn(updatedJourney)(
              Left(Error(""))
            )
          }

          checkIsTechnicalErrorPage(performAction(key -> country.code))
        }

        "there is an error updating the session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(updatedJourney)(
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

      behave like redirectToStartWhenInvalidJourney(() => performAction(), isValidJourney)

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.assetTypeForNonUkResidents(),
        mockUUIDGenerator
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
            () => performAction(),
            session,
            s"multipleDisposalsAssetTypeForNonUkResidents${displayType.getSubKey(separatePeriodOfAdminKey = true)}.title",
            routes.MultipleDisposalsTriageController
              .assetTypeForNonUkResidentsSubmit(),
            expectedBackLink,
            expectedButtonMessageKey,
            expectReturnToSummaryLink,
            List(
              SelectorAndValue(
                "#multipleDisposalsAssetTypeForNonUkResidents-3-item-hint",
                messageFromMessageKey(
                  s"multipleDisposalsAssetTypeForNonUkResidents.MixedUse${displayType.getSubKey(separatePeriodOfAdminKey = true)}.helpText"
                )
              )
            ),
            List(
              TagAttributePairAndValue(
                "label",
                "for",
                "multipleDisposalsAssetTypeForNonUkResidents-3",
                s"Residential Non-residential Mixed use ${messageFromMessageKey(
                    s"multipleDisposalsAssetTypeForNonUkResidents${displayType.getSubKey(separatePeriodOfAdminKey = true)}.IndirectDisposal"
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
              personalRepDisplay,
              periodOfAdminDisplay
            ).foreach { (displayType: UserTypeDisplay) =>
              withClue(
                s"For user type ${displayType.getSubKey(separatePeriodOfAdminKey = true)}"
              ) {
                test(
                  sessionDataWithStartingNewDraftReturn(
                    IncompleteMultipleDisposalsTriageAnswers.empty
                      .copy(individualUserType = Some(setIndividualUserType(displayType))),
                    displayType.name,
                    displayType.userType
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
              capacitorDisplay,
              periodOfAdminDisplay
            ).foreach { (displayType: UserTypeDisplay) =>
              withClue(
                s"For user type ${displayType.getSubKey(separatePeriodOfAdminKey = true)}"
              ) {
                test(
                  sessionDataWithStartingNewDraftReturn(
                    sample[CompleteMultipleDisposalsTriageAnswers]
                      .copy(individualUserType = Some(setIndividualUserType(displayType))),
                    displayType.name,
                    displayType.userType
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
          FakeRequest().withFormUrlEncodedBody(data*).withMethod("POST")
        )

      def updateDraftReturn(
        d: DraftMultipleDisposalsReturn,
        newAnswers: MultipleDisposalsTriageAnswers,
        isFurtherReturn: Boolean
      ): DraftMultipleDisposalsReturn =
        d.copy(
          triageAnswers = newAnswers,
          examplePropertyDetailsAnswers = None,
          yearToDateLiabilityAnswers = if (isFurtherReturn) {
            d.yearToDateLiabilityAnswers.map {
              case answers: CalculatedYTDAnswers    => answers.unset(_.hasEstimatedDetails)
              case answers: NonCalculatedYTDAnswers => answers.unset(_.hasEstimatedDetails)
            }
          } else {
            None
          },
          supportingEvidenceAnswers = None
        )

      val country = Country("HK")

      val key = "multipleDisposalsAssetTypeForNonUkResidents"

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.assetTypeForNonUkResidentsSubmit(),
        mockUUIDGenerator
      )

      "redirect to cya page" when {
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
              forAll { (c: CompleteMultipleDisposalsTriageAnswers) =>
                val amendReturnData                 = sample[AmendReturnData]
                val answers                         = c.copy(
                  countryOfResidence = sample[Country],
                  assetTypes = List(AssetType.Residential)
                )
                val (session, journey, draftReturn) =
                  sessionDataWithFillingOutReturn(
                    answers,
                    amendReturnData = Some(amendReturnData.copy(shouldDisplayGainOrLossAfterReliefs = false))
                  )

                val updatedAnswers     =
                  IncompleteMultipleDisposalsTriageAnswers(
                    answers.individualUserType,
                    Some(answers.numberOfProperties),
                    Some(false),
                    Some(answers.countryOfResidence),
                    None,
                    Some(List(AssetType.NonResidential)),
                    answers.taxYearExchanged,
                    Some(answers.taxYear),
                    answers.alreadySentSelfAssessment,
                    Some(answers.completionDate)
                  )
                val updatedDraftReturn =
                  updateDraftReturn(draftReturn, updatedAnswers, journey.isFurtherReturn.contains(true))
                val updatedJourney     =
                  journey.copy(
                    draftReturn = updatedDraftReturn,
                    amendReturnData = Some(amendReturnData.copy(shouldDisplayGainOrLossAfterReliefs = true))
                  )

                inSequence {
                  mockAuthWithNoRetrievals()
                  mockGetSession(session)
                  mockStoreDraftReturn(updatedJourney)(
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
            val answers = sample[IncompleteMultipleDisposalsTriageAnswers].copy(
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
            personalRepDisplay,
            periodOfAdminDisplay
          ).foreach { (displayType: UserTypeDisplay) =>
            withClue(
              s"For user type ${displayType.getSubKey(separatePeriodOfAdminKey = true)}"
            ) {
              val answers = IncompleteMultipleDisposalsTriageAnswers.empty.copy(
                individualUserType = Some(setIndividualUserType(displayType)),
                numberOfProperties = Some(2),
                wasAUKResident = Some(false),
                countryOfResidence = Some(country),
                assetTypes = None
              )
              val session = sessionDataWithStartingNewDraftReturn(
                answers,
                displayType.name,
                displayType.userType
              )._1.copy(userType = Some(displayType.userType))

              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
              }

              checkPageIsDisplayed(
                performAction(),
                messageFromMessageKey(
                  s"multipleDisposalsAssetTypeForNonUkResidents${displayType.getSubKey(separatePeriodOfAdminKey = true)}.title"
                ),
                { doc =>
                  doc
                    .select("[data-spec='errorSummaryDisplay'] a")
                    .text() shouldBe messageFromMessageKey(
                    s"$key${displayType.getSubKey(separatePeriodOfAdminKey = true)}.error.required"
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
            answers.taxYearExchanged,
            Some(answers.taxYear),
            answers.alreadySentSelfAssessment,
            Some(answers.completionDate)
          )
        val updatedDraftReturn = updateDraftReturn(draftReturn, updatedAnswers, journey.isFurtherReturn.contains(true))
        val updatedJourney     = journey.copy(draftReturn = updatedDraftReturn)

        "there is an error updating the draft return" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(updatedJourney)(
              Left(Error(""))
            )
          }

          checkIsTechnicalErrorPage(performAction(s"$key[]" -> "1"))
        }

        "there is an error updating the session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(updatedJourney)(
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

      behave like redirectToStartWhenInvalidJourney(() => performAction(), isValidJourney)

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.completionDate(),
        mockUUIDGenerator
      )

      "display the page" when {
        def test(
          session: SessionData,
          expectedBackLink: Call,
          expectedButtonMessageKey: String,
          expectReturnToSummaryLink: Boolean
        ): Unit =
          testPageIsDisplayed(
            () => performAction(),
            session,
            "multipleDisposalsCompletionDate.title",
            routes.MultipleDisposalsTriageController.completionDateSubmit(),
            expectedBackLink,
            expectedButtonMessageKey,
            expectReturnToSummaryLink
          )

        "the user has not started a new draft return and" when {
          "the journey is incomplete with alreadySentSelfAssessment = false" in {
            test(
              sessionDataWithStartingNewDraftReturn(
                IncompleteMultipleDisposalsTriageAnswers.empty.copy(
                  alreadySentSelfAssessment = Some(false)
                )
              )._1,
              triage.routes.CommonTriageQuestionsController.haveYouAlreadySentSelfAssessment(),
              "button.continue",
              expectReturnToSummaryLink = false
            )
          }

          "the journey is incomplete with alreadySentSelfAssessment = None" in {
            test(
              sessionDataWithStartingNewDraftReturn(
                IncompleteMultipleDisposalsTriageAnswers.empty
              )._1,
              triage.routes.MultipleDisposalsTriageController.whenWereContractsExchanged(),
              "button.continue",
              expectReturnToSummaryLink = false
            )
          }

          "the journey is complete" in {
            test(
              sessionDataWithStartingNewDraftReturn(
                sample[CompleteMultipleDisposalsTriageAnswers].copy(
                  individualUserType = Some(Self)
                )
              )._1,
              triage.routes.MultipleDisposalsTriageController
                .checkYourAnswers(),
              "button.continue",
              expectReturnToSummaryLink = false
            )
          }
        }

        "the user has started a new draft return and" when {
          "the journey is incomplete with alreadySentSelfAssessment = false" in {
            test(
              sessionDataWithFillingOutReturn(
                IncompleteMultipleDisposalsTriageAnswers.empty.copy(
                  alreadySentSelfAssessment = Some(false)
                )
              )._1,
              triage.routes.CommonTriageQuestionsController.haveYouAlreadySentSelfAssessment(),
              "button.saveAndContinue",
              expectReturnToSummaryLink = true
            )
          }

          "the journey is incomplete with alreadySentSelfAssessment = None" in {
            test(
              sessionDataWithFillingOutReturn(
                IncompleteMultipleDisposalsTriageAnswers.empty
              )._1,
              triage.routes.MultipleDisposalsTriageController.whenWereContractsExchanged(),
              "button.saveAndContinue",
              expectReturnToSummaryLink = true
            )
          }

          "the journey is complete" in {
            test(
              sessionDataWithFillingOutReturn(
                sample[CompleteMultipleDisposalsTriageAnswers].copy(individualUserType = Some(Self))
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
          FakeRequest().withFormUrlEncodedBody(formData*).withMethod("POST")
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
      ): DraftMultipleDisposalsReturn =
        d.copy(
          triageAnswers = newAnswers,
          examplePropertyDetailsAnswers = d.examplePropertyDetailsAnswers.map(
            _.unset(_.disposalDate).unset(_.acquisitionPrice)
          ),
          yearToDateLiabilityAnswers = None,
          gainOrLossAfterReliefs = None
        )

      val today = TimeUtils.today()

      behave like redirectToStartWhenInvalidJourney(
        () => performAction(),
        isValidJourney
      )

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.completionDateSubmit(),
        mockUUIDGenerator
      )

      "show a form error" when {
        val cutoffTaxYearDateRange = sample[TaxYear].copy(
          startDateInclusive = LocalDate.of(cutoffTaxYear, 4, 6),
          endDateExclusive = LocalDate.of(currentTaxYear - 3, 4, 6)
        )

        def testFormError(
          formData: List[(String, String)]
        )(expectedErrorMessageKey: String, args: Seq[String] = Seq()): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithStartingNewDraftReturn(
                sample[CompleteMultipleDisposalsTriageAnswers].copy(
                  individualUserType = Some(Self),
                  taxYearExchanged = Some(TaxYearExchanged(cutoffTaxYear)),
                  taxYear = cutoffTaxYearDateRange,
                  alreadySentSelfAssessment = Some(false)
                ),
                representeeAnswers = None
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(formData*),
            messageFromMessageKey("multipleDisposalsCompletionDate.title"),
            doc =>
              doc
                .select("[data-spec='errorSummaryDisplay'] a")
                .text() shouldBe messageFromMessageKey(
                expectedErrorMessageKey,
                args
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
          testFormError(formData(today.plusYears(2L).plusDays(1L)))(
            "multipleDisposalsCompletionDate.error.tooFarInFuture"
          )
        }

        "the date entered is before cutoffTaxYear" in {
          testFormError(formData(LocalDate.of(cutoffTaxYear, 1, 5)))(
            "multipleDisposalsCompletionDate.error.tooFarInPast"
          )
        }
      }

      "show an error page" when {
        "there is an error updating the draft return" in {
          val taxYearStart: LocalDate                    = TimeUtils.taxYearStart(today)
          val taxYearExchangedAdjusted: TaxYearExchanged = taxYearStart.getYear match {
            case i if i == cutoffTaxYear - 1 => TaxYearExchanged(cutoffTaxYear - 1)
            case i if i == cutoffTaxYear     => TaxYearExchanged(cutoffTaxYear)
            case _                           => TaxYearExchanged(currentTaxYear - 3)
          }

          val answers                         = sample[CompleteMultipleDisposalsTriageAnswers].copy(
            individualUserType = Some(Self),
            taxYearExchanged = Some(taxYearExchangedAdjusted),
            completionDate = CompletionDate(today.minusDays(1L))
          )
          val (session, journey, draftReturn) =
            sessionDataWithFillingOutReturn(answers)

          val updatedAnswers     =
            IncompleteMultipleDisposalsTriageAnswers
              .fromCompleteAnswers(answers)
              .copy(completionDate = Some(CompletionDate(today)))
          val updatedDraftReturn =
            updateDraftReturn(draftReturn, updatedAnswers)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(journey.copy(draftReturn = updatedDraftReturn))(
              Left(Error(""))
            )
          }

          checkIsTechnicalErrorPage(
            performAction(formData(today)*)
          )
        }

        "there is an error updating the session" in {
          val taxYearStart: LocalDate                    = TimeUtils.taxYearStart(today)
          val taxYearExchangedAdjusted: TaxYearExchanged = taxYearStart.getYear match {
            case i if i == cutoffTaxYear - 1 => TaxYearExchanged(cutoffTaxYear - 1)
            case i if i == cutoffTaxYear     => TaxYearExchanged(cutoffTaxYear)
            case _                           => TaxYearExchanged(currentTaxYear - 3)
          }

          val selfAssessmentFlag = if (taxYearStart.getYear === cutoffTaxYear) true else false
          val answers            =
            sample[CompleteMultipleDisposalsTriageAnswers].copy(
              individualUserType = Some(Self),
              taxYearExchanged = Some(taxYearExchangedAdjusted),
              completionDate = CompletionDate(today),
              alreadySentSelfAssessment = Some(selfAssessmentFlag)
            )
          val (session, journey) =
            sessionDataWithStartingNewDraftReturn(answers)

          val newCompletionDate =
            CompletionDate(answers.completionDate.value.minusDays(1L))
          val updatedAnswers    =
            IncompleteMultipleDisposalsTriageAnswers
              .fromCompleteAnswers(answers)
              .copy(completionDate = Some(newCompletionDate))
          val updatedJourney    =
            journey.copy(newReturnTriageAnswers = Left(updatedAnswers))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreSession(
              session.copy(journeyStatus = Some(updatedJourney))
            )(Left(Error("")))
          }

          checkIsTechnicalErrorPage(
            performAction(formData(newCompletionDate.value)*)
          )
        }
      }

      "redirect to the check your answers page" when {
        val taxYearStart             = TimeUtils.taxYearStart(today)
        val taxYearExchangedAdjusted = taxYearStart.getYear match {
          case i if i == cutoffTaxYear - 1 => TaxYearExchanged(cutoffTaxYear - 1)
          case i if i == cutoffTaxYear     => TaxYearExchanged(cutoffTaxYear)
          case _                           => TaxYearExchanged(currentTaxYear - 3)
        }

        val selfAssessmentFlag = taxYearStart.getYear match {
          case i if i == cutoffTaxYear - 1 => Some(false)
          case i if i == cutoffTaxYear     => Some(false)
          case _                           => None
        }

        val taxYear = sample[TaxYear].copy(
          startDateInclusive = taxYearStart.getYear match {
            case i if i == cutoffTaxYear - 1  => LocalDate.of(cutoffTaxYear - 1, 4, 6)
            case i if i == cutoffTaxYear      => LocalDate.of(cutoffTaxYear, 4, 6)
            case i if i == currentTaxYear - 3 => LocalDate.of(currentTaxYear - 3, 4, 6)
            case i if i == currentTaxYear - 2 => LocalDate.of(currentTaxYear - 2, 4, 6)
            case i if i == currentTaxYear - 1 => LocalDate.of(currentTaxYear - 1, 4, 6)
            case _                            => LocalDate.of(currentTaxYear, 4, 6)
          },
          endDateExclusive = taxYearStart.getYear match {
            case i if i == cutoffTaxYear - 1  => LocalDate.of(cutoffTaxYear, 4, 6)
            case i if i == cutoffTaxYear      => LocalDate.of(currentTaxYear - 3, 4, 6)
            case i if i == currentTaxYear - 3 => LocalDate.of(currentTaxYear - 2, 4, 6)
            case i if i == currentTaxYear - 2 => LocalDate.of(currentTaxYear - 1, 4, 6)
            case _                            => LocalDate.of(currentTaxYear, 4, 6)
          }
        )

        "the user has not started a draft return and" when {
          "the user has not answered the question before" in {
            val answers            = IncompleteMultipleDisposalsTriageAnswers.empty.copy(
              individualUserType = Some(IndividualUserType.Self),
              taxYearExchanged = Some(taxYearExchangedAdjusted),
              taxYear = Some(taxYear),
              alreadySentSelfAssessment = selfAssessmentFlag,
              completionDate = None
            )
            val (session, journey) =
              sessionDataWithStartingNewDraftReturn(answers)

            val newCompletionDate = CompletionDate(today.minusDays(1L))
            val updatedJourney    =
              journey.copy(newReturnTriageAnswers =
                Left(
                  answers
                    .copy(completionDate = Some(newCompletionDate))
                )
              )

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockStoreSession(
                session.copy(journeyStatus = Some(updatedJourney))
              )(Right(()))
            }

            checkIsRedirect(
              performAction(formData(newCompletionDate.value)*),
              routes.MultipleDisposalsTriageController.checkYourAnswers()
            )
          }

          "the user has already answered the question" in {
            forAll { (c: CompleteMultipleDisposalsTriageAnswers) =>
              val answers            = c.copy(
                individualUserType = Some(Self),
                completionDate = CompletionDate(today),
                taxYearExchanged = Some(taxYearExchangedAdjusted),
                taxYear = taxYear,
                alreadySentSelfAssessment = selfAssessmentFlag
              )
              val (session, journey) =
                sessionDataWithStartingNewDraftReturn(answers)

              val newCompletionDate =
                CompletionDate(answers.completionDate.value.minusDays(1L))
              val updatedAnswers    = IncompleteMultipleDisposalsTriageAnswers
                .fromCompleteAnswers(answers)
                .copy(completionDate = Some(newCompletionDate))

              val updatedJourney =
                journey.copy(newReturnTriageAnswers = Left(updatedAnswers))

              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
                mockStoreSession(
                  session.copy(journeyStatus = Some(updatedJourney))
                )(Right(()))
              }

              checkIsRedirect(
                performAction(formData(newCompletionDate.value)*),
                routes.MultipleDisposalsTriageController.checkYourAnswers()
              )
            }
          }
        }

        "the user has started a draft return and" when {
          "have completed the section and they enter a figure which is " +
            "different than one they have already entered" in {

              val currentAnswers  = sample[CompleteMultipleDisposalsTriageAnswers].copy(
                individualUserType = Some(Self),
                taxYearExchanged = Some(taxYearExchangedAdjusted),
                completionDate = CompletionDate(today.minusDays(1L))
              )
              val submittedDate   = today
              val amendReturnData = sample[AmendReturnData]

              val (session, journey, draftReturn) =
                sessionDataWithFillingOutReturn(
                  currentAnswers,
                  representeeAnswers = None,
                  amendReturnData = Some(amendReturnData.copy(shouldDisplayGainOrLossAfterReliefs = false))
                )

              val updatedAnswers     =
                currentAnswers.unset(_.completionDate).copy(completionDate = Some(CompletionDate(submittedDate)))
              val updatedDraftReturn =
                updateDraftReturn(draftReturn, updatedAnswers)
              val updatedJourney     = journey.copy(
                draftReturn = updatedDraftReturn,
                amendReturnData = Some(amendReturnData.copy(shouldDisplayGainOrLossAfterReliefs = true))
              )

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
                  formData(submittedDate)*
                ),
                routes.MultipleDisposalsTriageController.checkYourAnswers()
              )
            }
        }
      }

      "not perform any updates" when {
        "the date submitted is the same as one that already exists in session" in {
          val taxYearStart: LocalDate                    = TimeUtils.taxYearStart(today)
          val taxYearExchangedAdjusted: TaxYearExchanged = taxYearStart.getYear match {
            case i if i == cutoffTaxYear - 1 => TaxYearExchanged(cutoffTaxYear - 1)
            case i if i == cutoffTaxYear     => TaxYearExchanged(cutoffTaxYear)
            case _                           => TaxYearExchanged(currentTaxYear - 3)
          }

          val answers      = sample[CompleteMultipleDisposalsTriageAnswers].copy(
            individualUserType = Some(Self),
            taxYearExchanged = Some(taxYearExchangedAdjusted),
            completionDate = CompletionDate(TimeUtils.today())
          )
          val (session, _) = sessionDataWithStartingNewDraftReturn(answers)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkIsRedirect(
            performAction(formData(answers.completionDate.value)*),
            routes.MultipleDisposalsTriageController.checkYourAnswers()
          )
        }
      }
    }

    "handling requests to display the share disposal page for non uk residents page" must {
      val requiredPreviousAnswers =
        IncompleteMultipleDisposalsTriageAnswers.empty.copy(
          individualUserType = Some(Self),
          wasAUKResident = Some(false),
          countryOfResidence = Some(sample[Country])
        )

      def performAction(): Future[Result] =
        controller.disposalDateOfShares()(FakeRequest())

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.disposalDateOfShares(),
        mockUUIDGenerator
      )

      behave like noDateOfDeathForPersonalRepBehaviour(() => performAction())

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
          performAction(),
          messageFromMessageKey("sharesDisposalDate.title"),
          doc => {
            doc
              .select("#sharesDisposalDate-hint")
              .text()         shouldBe messageFromMessageKey(
              "sharesDisposalDate.helpText"
            )
            doc
              .select("#back, .govuk-back-link")
              .attr("href")   shouldBe routes.MultipleDisposalsTriageController
              .assetTypeForNonUkResidents()
              .url
            doc
              .select("#content > article > form, #main-content form")
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
          FakeRequest().withFormUrlEncodedBody(formData*).withMethod("POST")
        )

      def formData(d: LocalDate): List[(String, String)] =
        List(
          "sharesDisposalDate-day"   -> d.getDayOfMonth.toString,
          "sharesDisposalDate-month" -> d.getMonthValue.toString,
          "sharesDisposalDate-year"  -> d.getYear.toString
        )

      val today = TimeUtils.today()

      behave like redirectToStartWhenInvalidJourney(
        () => performAction(),
        isValidJourney
      )

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.disposalDateOfSharesSubmit(),
        mockUUIDGenerator
      )

      behave like noDateOfDeathForPersonalRepBehaviour(() => performAction())

      def updateDraftReturn(
        d: DraftMultipleDisposalsReturn,
        newAnswers: MultipleDisposalsTriageAnswers
      ): DraftMultipleDisposalsReturn =
        d.copy(
          triageAnswers = newAnswers,
          examplePropertyDetailsAnswers = d.examplePropertyDetailsAnswers.map(
            _.unset(_.disposalDate)
          ),
          yearToDateLiabilityAnswers = None,
          gainOrLossAfterReliefs = None
        )

      "show a form error" when {
        def testFormError(
          individualUserType: Option[IndividualUserType] = Some(Self),
          representeeAnswers: Option[RepresenteeAnswers] = None
        )(
          formData: List[(String, String)]
        )(expectedErrorMessageKey: String): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithStartingNewDraftReturn(
                sample[CompleteMultipleDisposalsTriageAnswers].copy(individualUserType = individualUserType),
                representeeAnswers = representeeAnswers
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(formData*),
            messageFromMessageKey("sharesDisposalDate.title"),
            doc =>
              doc
                .select("[data-spec='errorSummaryDisplay'] a")
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

                testFormError()(data)(scenario.expectedErrorMessageKey)
              }
            }
        }

        "the date entered is later than today" in {
          testFormError()(formData(today.plusYears(2).plusDays(1L)))(
            "sharesDisposalDate.error.tooFarInFuture"
          )
        }

        "the disposal date is strictly after the date of death and the user is a non-period of admin personal rep" in {
          testFormError(
            Some(PersonalRepresentative),
            Some(sample[CompleteRepresenteeAnswers].copy(dateOfDeath = Some(DateOfDeath(today.minusDays(1L)))))
          )(formData(today))(
            "sharesDisposalDate.error.nonPeriodOfAdminDeathAfterDate"
          )
        }

        "the disposal date is before the date of death and the user is a period of admin personal rep" in {
          testFormError(
            Some(PersonalRepresentativeInPeriodOfAdmin),
            Some(sample[CompleteRepresenteeAnswers].copy(dateOfDeath = Some(DateOfDeath(today))))
          )(formData(today.minusDays(1L)))(
            "sharesDisposalDate.error.periodOfAdminDeathNotAfterDate"
          )
        }

        "the disposal date is strictly before the date of death and the user is a period of admin personal rep" in {
          testFormError(
            Some(PersonalRepresentativeInPeriodOfAdmin),
            Some(sample[CompleteRepresenteeAnswers].copy(dateOfDeath = Some(DateOfDeath(today))))
          )(formData(today.minusDays(1L)))(
            "sharesDisposalDate.error.periodOfAdminDeathNotAfterDate"
          )
        }
      }

      "show an error page" when {
        "there is an error updating the session" in {
          val taxYear            = sample[TaxYear].copy(
            startDateInclusive = LocalDate.of(cutoffTaxYear, 4, 6),
            endDateExclusive = LocalDate.of(currentTaxYear - 3, 4, 6)
          )
          val taxYearExchanged   = getTaxYearExchanged(Some(taxYear))
          val answers            = sample[IncompleteMultipleDisposalsTriageAnswers].copy(
            individualUserType = Some(Self),
            completionDate = Some(CompletionDate(today)),
            alreadySentSelfAssessment = None,
            taxYearExchanged = taxYearExchanged
          )
          val (session, journey) = sessionDataWithStartingNewDraftReturn(answers)

          val newCompletionDate = CompletionDate(today.minusDays(1))
          val updatedJourney    = journey.copy(
            newReturnTriageAnswers = Left(
              answers.copy(
                completionDate = Some(newCompletionDate),
                taxYear = Some(taxYear),
                taxYearExchanged = taxYearExchanged
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
            performAction(formData(newCompletionDate.value)*)
          )
        }
      }

      "redirect to the check your answers page" when {
        val taxYearStartDate = TimeUtils.taxYearStart(today)
        val taxYear          = sample[TaxYear].copy(
          startDateInclusive = LocalDate.of(taxYearStartDate.getYear, 4, 6),
          endDateExclusive = LocalDate.of(taxYearStartDate.getYear + 1, 4, 6)
        )

        val saFlagStatus = taxYear.startDateInclusive.getYear match {
          case i if i == cutoffTaxYear - 1 => Some(false)
          case i if i == cutoffTaxYear     => Some(false)
          case _                           => None
        }

        def test(
          currentAnswers: IncompleteMultipleDisposalsTriageAnswers,
          representeeAnswers: Option[RepresenteeAnswers],
          submittedDate: LocalDate,
          taxYear: Option[TaxYear]
        ): Unit = {

          val (session, journey, draftReturn) =
            sessionDataWithFillingOutReturn(
              currentAnswers,
              representeeAnswers = representeeAnswers
            )

          val updatedAnswers     = currentAnswers.copy(
            completionDate = Some(CompletionDate(submittedDate)),
            taxYear = taxYear,
            taxYearExchanged = getTaxYearExchanged(taxYear),
            alreadySentSelfAssessment = saFlagStatus
          )
          val updatedDraftReturn =
            updateDraftReturn(draftReturn, updatedAnswers)
          val updatedJourney     = journey.copy(
            draftReturn = updatedDraftReturn
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockGetTaxYear(submittedDate)(Right(taxYear))
            mockStoreDraftReturn(updatedJourney)(Right(()))
            mockStoreSession(
              session.copy(journeyStatus = Some(updatedJourney))
            )(Right(()))
          }

          checkIsRedirect(
            performAction(
              formData(submittedDate)*
            ),
            routes.MultipleDisposalsTriageController.checkYourAnswers()
          )
        }

        "a valid date is submitted but a tax year cannot be found" in {
          test(
            sample[IncompleteMultipleDisposalsTriageAnswers].copy(individualUserType = Some(Self)),
            None,
            LocalDate.of(cutoffTaxYear - 1, 1, 1),
            None
          )
        }

        "the disposal date is on the date of death when the user is a non-period of admin personal rep" in {
          test(
            sample[IncompleteMultipleDisposalsTriageAnswers].copy(
              individualUserType = Some(PersonalRepresentative),
              alreadySentSelfAssessment = None
            ),
            Some(sample[CompleteRepresenteeAnswers].copy(dateOfDeath = Some(DateOfDeath(today)))),
            today,
            Some(taxYear)
          )
        }

        "the disposal date is strictly before the date of death when the user is a non-period of admin personal rep" in {
          test(
            sample[IncompleteMultipleDisposalsTriageAnswers].copy(
              individualUserType = Some(PersonalRepresentative),
              alreadySentSelfAssessment = None
            ),
            Some(sample[CompleteRepresenteeAnswers].copy(dateOfDeath = Some(DateOfDeath(today)))),
            today.minusDays(1L),
            Some(taxYear)
          )
        }

        "the disposal date is strictly after the date of death when the user is a period of admin personal rep" in {
          test(
            sample[IncompleteMultipleDisposalsTriageAnswers].copy(
              individualUserType = Some(PersonalRepresentativeInPeriodOfAdmin),
              alreadySentSelfAssessment = None
            ),
            Some(sample[CompleteRepresenteeAnswers].copy(dateOfDeath = Some(DateOfDeath(today.minusDays(1L))))),
            today,
            Some(taxYear)
          )
        }
      }

      "redirect to the amend return disposaldate different taxyear page" when {
        val taxYearStartDate = TimeUtils.taxYearStart(today)
        val taxYear          = sample[TaxYear].copy(
          startDateInclusive = LocalDate.of(taxYearStartDate.getYear, 4, 6),
          endDateExclusive = LocalDate.of(taxYearStartDate.getYear + 1, 4, 6)
        )

        val saFlagStatus = taxYear.startDateInclusive.getYear match {
          case i if i == cutoffTaxYear - 1 => Some(false)
          case _                           => None
        }

        def test(
          currentAnswers: IncompleteMultipleDisposalsTriageAnswers,
          representeeAnswers: Option[RepresenteeAnswers],
          submittedDate: LocalDate,
          taxYear: Option[TaxYear]
        ): Unit = {
          val amendReturnData                 = sample[AmendReturnData]
          val (session, journey, draftReturn) =
            sessionDataWithFillingOutReturn(
              currentAnswers,
              representeeAnswers = representeeAnswers,
              amendReturnData = Some(amendReturnData.copy(shouldDisplayGainOrLossAfterReliefs = false))
            )

          val updatedAnswers     = currentAnswers.copy(
            completionDate = Some(CompletionDate(submittedDate)),
            taxYear = taxYear,
            taxYearExchanged = getTaxYearExchanged(taxYear),
            alreadySentSelfAssessment = saFlagStatus
          )
          val updatedDraftReturn =
            updateDraftReturn(draftReturn, updatedAnswers)
          val updatedJourney     = journey.copy(
            draftReturn = updatedDraftReturn,
            amendReturnData = Some(amendReturnData.copy(shouldDisplayGainOrLossAfterReliefs = true))
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockGetTaxYear(submittedDate)(Right(taxYear))
            mockStoreDraftReturn(updatedJourney)(Right(()))
            mockStoreSession(
              session.copy(journeyStatus = Some(updatedJourney))
            )(Right(()))
          }

          checkIsRedirect(
            performAction(
              formData(submittedDate)*
            ),
            routes.CommonTriageQuestionsController.amendReturnDisposalDateDifferentTaxYear()
          )
        }

        "the disposal date has an invalid tax year and" +
          "it is on the date of death when the user is a non-period of admin personal rep" in {
            val disposalDate = today.minusYears(20)
            test(
              sample[IncompleteMultipleDisposalsTriageAnswers].copy(individualUserType = Some(PersonalRepresentative)),
              Some(sample[CompleteRepresenteeAnswers].copy(dateOfDeath = Some(DateOfDeath(today)))),
              disposalDate,
              Some(taxYear)
            )
          }

        "the disposal date is on the date of death when the user is a non-period of admin personal rep" in {
          test(
            sample[IncompleteMultipleDisposalsTriageAnswers].copy(individualUserType = Some(PersonalRepresentative)),
            Some(sample[CompleteRepresenteeAnswers].copy(dateOfDeath = Some(DateOfDeath(today)))),
            today,
            Some(taxYear)
          )
        }

        "the disposal date is strictly before the date of death when the user is a non-period of admin personal rep" in {
          test(
            sample[IncompleteMultipleDisposalsTriageAnswers].copy(individualUserType = Some(PersonalRepresentative)),
            Some(sample[CompleteRepresenteeAnswers].copy(dateOfDeath = Some(DateOfDeath(today)))),
            today.minusDays(1L),
            Some(taxYear)
          )
        }

        "the disposal date is strictly after the date of death when the user is a period of admin personal rep" in {
          test(
            sample[IncompleteMultipleDisposalsTriageAnswers]
              .copy(individualUserType = Some(PersonalRepresentativeInPeriodOfAdmin)),
            Some(sample[CompleteRepresenteeAnswers].copy(dateOfDeath = Some(DateOfDeath(today.minusDays(1L))))),
            today,
            Some(taxYear)
          )
        }
      }

      "not perform any updates" when {
        "the date submitted is the same as one that already exists in session" in {
          val answers      =
            sample[CompleteMultipleDisposalsTriageAnswers]
              .copy(
                individualUserType = Some(Self),
                completionDate = CompletionDate(TimeUtils.today())
              )
          val (session, _) =
            sessionDataWithStartingNewDraftReturn(answers)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkIsRedirect(
            performAction(formData(answers.completionDate.value)*),
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
        Some(TaxYearExchanged(cutoffTaxYear - 1)),
        sample[TaxYear],
        Some(false),
        sample[CompletionDate]
      )

      val allQuestionsAnsweredUk = IncompleteMultipleDisposalsTriageAnswers(
        completeAnswersUk.individualUserType,
        Some(completeAnswersUk.numberOfProperties),
        Some(true),
        None,
        Some(true),
        Some(completeAnswersUk.assetTypes),
        Some(TaxYearExchanged(cutoffTaxYear - 1)),
        Some(completeAnswersUk.taxYear),
        Some(false),
        Some(completeAnswersUk.completionDate)
      )

      val taxYear              = sample[TaxYear].copy(
        startDateInclusive = LocalDate.of(cutoffTaxYear - 1, 4, 6),
        endDateExclusive = LocalDate.of(cutoffTaxYear, 4, 6)
      )
      val completeAnswersNonUk = CompleteMultipleDisposalsTriageAnswers(
        Some(IndividualUserType.Self),
        2,
        sample[Country],
        List(AssetType.Residential),
        Some(TaxYearExchanged(cutoffTaxYear - 1)),
        taxYear,
        Some(false),
        CompletionDate(taxYear.startDateInclusive.plusDays(1L))
      )

      val allQuestionsAnsweredNonUk = IncompleteMultipleDisposalsTriageAnswers(
        completeAnswersNonUk.individualUserType,
        Some(completeAnswersNonUk.numberOfProperties),
        Some(false),
        Some(completeAnswersNonUk.countryOfResidence),
        None,
        Some(completeAnswersNonUk.assetTypes),
        Some(TaxYearExchanged(cutoffTaxYear - 1)),
        Some(completeAnswersNonUk.taxYear),
        Some(false),
        Some(completeAnswersNonUk.completionDate)
      )

      def testRedirectWhenIncomplete(
        answers: IncompleteMultipleDisposalsTriageAnswers,
        expectedRedirect: Call,
        name: Either[TrustName, IndividualName] = Right(sample[IndividualName]),
        userType: UserType = UserType.Individual,
        previousSentReturns: Option[List[ReturnSummary]] = None
      ): Unit = {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            sessionDataWithStartingNewDraftReturn(
              answers,
              name,
              userType,
              previousSentReturns,
              Some(IncompleteRepresenteeAnswers.empty)
            )._1
          )
        }

        checkIsRedirect(performAction(), expectedRedirect)
      }

      behave like redirectToStartWhenInvalidJourney(() => performAction(), isValidJourney)

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.checkYourAnswers(),
        mockUUIDGenerator
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
            representee.routes.RepresenteeController.checkYourAnswers()
          )
        }

        "an individual user type of personal representative is found" in {
          testRedirectWhenIncomplete(
            allQuestionsAnsweredUk.copy(individualUserType = Some(IndividualUserType.PersonalRepresentative)),
            representee.routes.RepresenteeController.checkYourAnswers()
          )
        }

        "an individual user type of personal representative in period admin is found" in {
          testRedirectWhenIncomplete(
            allQuestionsAnsweredUk
              .copy(individualUserType = Some(IndividualUserType.PersonalRepresentativeInPeriodOfAdmin)),
            representee.routes.RepresenteeController.checkYourAnswers()
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

          forAll { (assetTypes: List[AssetType]) =>
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
                mockStoreDraftReturn(updatedJourney)(Right(()))
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
              .copy(taxYearExchanged = None, taxYear = None),
            routes.MultipleDisposalsTriageController
              .whenWereContractsExchanged()
          )
        }
      }

      "redirect to the tax year too early page" when {
        "the user indicated that the tax year was before cutoffTaxYear minus one" in {
          testRedirectWhenIncomplete(
            allQuestionsAnsweredUk.copy(taxYearExchanged = Some(TaxYearExchanged.taxYearExchangedTooEarly)),
            routes.CommonTriageQuestionsController.disposalDateTooEarly()
          )
        }

        "the tax year of a share disposal date was before cutoffTaxYear minus one" in {
          testRedirectWhenIncomplete(
            allQuestionsAnsweredUk
              .copy(
                assetTypes = Some(List(IndirectDisposal)),
                taxYearExchanged = Some(TaxYearExchanged.taxYearExchangedTooEarly)
              ),
            routes.CommonTriageQuestionsController.disposalsOfSharesTooEarly()
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
                  .copy(taxYearExchanged = Some(TaxYearExchanged(cutoffTaxYear - 1)), taxYear = None),
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
            mockStoreDraftReturn(updatedJourney)(Right(()))
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

      "redirect to the previousReturnExistsWithSameCompletionDate exit page" when {
        val previousSentReturns = List(
          sample[ReturnSummary].copy(completionDate = completeAnswersUk.completionDate.value)
        )

        "a completion date is submitted which already exists in a previously sent return" when {
          "the user is a trust" in {
            testRedirectWhenIncomplete(
              allQuestionsAnsweredUk.copy(
                individualUserType = None
              ),
              routes.CommonTriageQuestionsController.previousReturnExistsWithSameCompletionDate(),
              Left(sample[TrustName]),
              UserType.Organisation,
              Some(previousSentReturns)
            )
          }

          "the user is doing the return for themselves" in {
            testRedirectWhenIncomplete(
              allQuestionsAnsweredUk.copy(
                individualUserType = Some(IndividualUserType.Self)
              ),
              routes.CommonTriageQuestionsController.previousReturnExistsWithSameCompletionDate(),
              Right(sample[IndividualName]),
              UserType.Individual,
              Some(previousSentReturns)
            )
          }
        }
      }

      "show an error page" when {
        "there is an error updating the session when converting from incomplete answers to " +
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
                doc.select("#guidanceLink").isEmpty shouldBe true

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
                doc.select("#guidanceLink").isEmpty shouldBe true
                MultipleDisposalsTriageControllerSpec
                  .validateMultipleDisposalsTriageCheckYourAnswersPage(
                    completeAnswersNonUk,
                    Some(UserType.Agent),
                    doc
                  )
              }
            )
          }

          "the user is on an amend journey where the completion date hasn't changed" in {
            val originalReturnSummary =
              sample[ReturnSummary].copy(completionDate = completeAnswersUk.completionDate.value)

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionDataWithFillingOutReturn(
                  completeAnswersUk,
                  previousSentReturns = Some(List(originalReturnSummary)),
                  amendReturnData = Some(
                    sample[AmendReturnData].copy(originalReturn =
                      sample[CompleteReturnWithSummary].copy(
                        summary = originalReturnSummary
                      )
                    )
                  )
                )._1
              )
            }

            checkPageIsDisplayed(
              performAction(),
              messageFromMessageKey("multipleDisposals.triage.cya.title")
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
              doc.select("#guidanceLink").isEmpty shouldBe true
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
              mockStoreDraftReturn(updatedJourney)(Right(()))
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
                doc.select("#guidanceLink").isEmpty shouldBe true
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

        "the user has a completion date which already exists in a previous return and" when {
          def test(representativeType: RepresentativeType): Unit = {
            val triageAnswers = completeAnswersUk.copy(individualUserType = Some(representativeType))
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionDataWithFillingOutReturn(
                  triageAnswers,
                  Right(sample[IndividualName]),
                  UserType.Individual,
                  previousSentReturns = Some(
                    List(sample[ReturnSummary].copy(completionDate = triageAnswers.completionDate.value))
                  )
                )._1
              )
            }

            checkPageIsDisplayed(
              performAction(),
              messageFromMessageKey("multipleDisposals.triage.cya.title")
            )
          }

          "the user is a capacitor" in {
            test(IndividualUserType.Capacitor)
          }

          "the user is a personal rep" in {
            test(IndividualUserType.PersonalRepresentative)
          }

          "the user is a personal rep in period of admin" in {
            test(IndividualUserType.PersonalRepresentativeInPeriodOfAdmin)
          }
        }
      }
    }

    "handling submits on the check your answers page" when {
      def performAction(): Future[Result] =
        controller.checkYourAnswersSubmit()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(() => performAction(), isValidJourney)

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.checkYourAnswersSubmit(),
        mockUUIDGenerator
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
          newDraftReturn,
          journey.previousSentReturns,
          None
        )

        "show an error page" when {
          "there is an error storing the new draft return" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockGenerateUUID(draftId)
              mockStoreDraftReturn(newJourney)(Left(Error("")))
            }

            checkIsTechnicalErrorPage(performAction())
          }

          "there is an error updating the session" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockGenerateUUID(draftId)
              mockStoreDraftReturn(newJourney)(Right(()))
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
              mockStoreDraftReturn(newJourney)(Right(()))
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

  private def testPageIsDisplayed(
    performAction: () => Future[Result],
    session: SessionData,
    expectedPageTitleMessageKey: String,
    expectedSubmit: Call,
    expectedBackLink: Call,
    expectedButtonMessageKey: String,
    expectReturnToSummaryLink: Boolean,
    expectedAdditionalIdKeyValues: List[SelectorAndValue] = Nil,
    expectedAdditionalNameAttributeKeyValues: List[TagAttributePairAndValue] = Nil,
    titleMessageArgs: List[String] = Nil
  ): Unit = {
    inSequence {
      mockAuthWithNoRetrievals()
      mockGetSession(session)
    }

    checkPageIsDisplayed(
      performAction(),
      messageFromMessageKey(expectedPageTitleMessageKey, titleMessageArgs*),
      { doc =>
        doc.select("#back, .govuk-back-link").attr("href") shouldBe expectedBackLink.url
        doc
          .select("#content > article > form, #main-content form")
          .attr("action")                                  shouldBe expectedSubmit.url
        doc.select("#submitButton").text()                 shouldBe messageFromMessageKey(
          expectedButtonMessageKey
        )
        doc.select("#returnToSummaryLink").text()          shouldBe (
          if (expectReturnToSummaryLink) messageFromMessageKey("returns.return-to-summary-link") else ""
        )
        expectedAdditionalIdKeyValues.map(a => doc.select(a.selector).html() should be(a.value))
        expectedAdditionalNameAttributeKeyValues.map(v =>
          doc
            .select(v.tagName)
            .attr(v.attributeName, v.attributeValue)
            .text() shouldBe v.value
        )
      }
    )
  }

  private def testTaxYearExchangedPageIsDisplayed(
    performAction: () => Future[Result],
    session: SessionData,
    expectedPageTitleMessageKey: String,
    expectedSubmit: Call,
    expectedBackLink: Call,
    expectedButtonMessageKey: String,
    expectReturnToSummaryLink: Boolean,
    expectedAdditionalIdKeyValues: List[SelectorAndValue] = Nil,
    expectedAdditionalNameAttributeKeyValues: List[TagAttributePairAndValue] = Nil,
    titleMessageArgs: List[String] = Nil
  ): Unit = {
    inSequence {
      mockAuthWithNoRetrievals()
      mockGetSession(session)
      mockAvailableTaxYears()(Right(List(cutoffTaxYear - 1)))
    }

    checkPageIsDisplayed(
      performAction(),
      messageFromMessageKey(expectedPageTitleMessageKey, titleMessageArgs*),
      { doc =>
        doc.select("#back, .govuk-back-link").attr("href") shouldBe expectedBackLink.url
        val selector = doc.body().select(".govuk-label.govuk-radios__label").asScala.map(_.text()).toList
        doc
          .select("#content > article > form, #main-content form")
          .attr("action")                         shouldBe expectedSubmit.url
        doc.select("#submitButton").text()        shouldBe messageFromMessageKey(
          expectedButtonMessageKey
        )
        doc.select("#returnToSummaryLink").text() shouldBe (
          if (expectReturnToSummaryLink) messageFromMessageKey("returns.return-to-summary-link") else ""
        )
        expectedAdditionalIdKeyValues.map(a => doc.select(a.selector).html() should be(a.value))
        expectedAdditionalNameAttributeKeyValues.map(v =>
          doc
            .select(v.tagName)
            .attr(v.attributeName, v.attributeValue)
            .text() shouldBe v.value
        )
        // Test dynamic content
        selector                                  shouldBe List(
          s"All between 6 April ${cutoffTaxYear - 1} and 5 April $cutoffTaxYear",
          s"All before 6 April ${cutoffTaxYear - 1}",
          "The properties were exchanged in different tax years"
        )
      }
    )
  }

  private def noDateOfDeathForPersonalRepBehaviour(performAction: () => Future[Result]): Unit =
    "show an error page" when {
      def sessionWithNoDateOfDeath(individualUserType: IndividualUserType): SessionData =
        sessionDataWithStartingNewDraftReturn(
          IncompleteMultipleDisposalsTriageAnswers.empty.copy(individualUserType = Some(individualUserType)),
          representeeAnswers = Some(sample[CompleteRepresenteeAnswers].copy(dateOfDeath = None))
        )._1

      "there is no date of death found for a personal rep" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionWithNoDateOfDeath(PersonalRepresentative))
        }

        checkIsTechnicalErrorPage(performAction())
      }

      "there is no date of death found for a personal rep in period of admin" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionWithNoDateOfDeath(PersonalRepresentativeInPeriodOfAdmin))
        }

        checkIsTechnicalErrorPage(performAction())
      }
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
    representativeType: Option[RepresentativeType],
    name: Either[TrustName, IndividualName]
  ) {
    def getSubKey(separatePeriodOfAdminKey: Boolean = false): String =
      representativeType match {
        case Some(PersonalRepresentative)                => ".personalRep"
        case Some(PersonalRepresentativeInPeriodOfAdmin) =>
          if (separatePeriodOfAdminKey) ".personalRepInPeriodOfAdmin" else ".personalRep"
        case Some(Capacitor)                             => ".capacitor"
        case None                                        =>
          if (userType === UserType.Agent) {
            ".agent"
          } else if (name.isLeft) {
            ".trust"
          } else {
            ""
          }
      }
  }

  def validateMultipleDisposalsTriageCheckYourAnswersPage(
    answers: CompleteMultipleDisposalsTriageAnswers,
    userType: Option[UserType],
    doc: Document
  )(implicit messagesApi: MessagesApi, lang: Lang): Unit = {
    implicit val messages: Messages = MessagesImpl(lang, messagesApi)

    if (answers.individualUserType.contains(Self)) {
      doc.select("#individualUserType-answer").text() shouldBe messages(
        if (userType.contains(UserType.Agent)) {
          s"individualUserType.agent.Self"
        } else {
          s"individualUserType.Self"
        }
      )
    }

    doc.select("#numberOfProperties-answer").text() shouldBe messages(
      "numberOfProperties.MoreThanOne"
    )

    doc
      .select("#multipleDisposalsNumberOfProperties-answer")
      .text() shouldBe answers.numberOfProperties.toString

    if (answers.countryOfResidence.isUk) {
      doc.select("#wereYouAUKResident-answer").text() shouldBe "Yes"
    } else {
      doc.select("#wereYouAUKResident-answer").text() shouldBe "No"
    }

    if (answers.countryOfResidence.isUk) {
      doc.select("#wereAllPropertiesResidential-answer").text() shouldBe "Yes"
    } else {
      doc
        .select("#countryOfResidence-answer")
        .text() shouldBe messages
        .translate(s"country.${answers.countryOfResidence.code}", Seq.empty)
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
      .text()                                   shouldBe (
      if (answers.isIndirectDisposal) {
        ""
      } else {
        s"${answers.taxYear.startDateInclusive.getYear}/${answers.taxYear.endDateExclusive.getYear}"
      }
    )
    doc.select("#completionDate-answer").text() shouldBe TimeUtils
      .govDisplayFormat(answers.completionDate.value)
  }
}
