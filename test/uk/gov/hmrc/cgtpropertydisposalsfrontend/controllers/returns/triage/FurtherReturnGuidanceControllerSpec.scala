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

import org.scalacheck.Gen
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.i18n.MessagesApi
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.{Call, Result}
import play.api.test.FakeRequest
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport, returns}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.triage.FurtherReturnGuidanceController.BackLinkLocations
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators.sample
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{CompleteReturnWithSummary, JourneyStatus, SessionData, UserType}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, PreviousReturnData, StartingNewDraftReturn, StartingToAmendReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.AgentReferenceNumber
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.{IndividualName, TrustName}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.SubscribedDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.IndividualUserType.{Capacitor, PersonalRepresentative, PersonalRepresentativeInPeriodOfAdmin, Self}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.RepresenteeAnswers.IncompleteRepresenteeAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{DraftMultipleDisposalsReturn, DraftSingleDisposalReturn, IndividualUserType, MultipleDisposalsTriageAnswers, ReturnSummary, SingleDisposalTriageAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.MultipleDisposalsTriageAnswers.IncompleteMultipleDisposalsTriageAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.IncompleteSingleDisposalTriageAnswers

import scala.concurrent.Future

class FurtherReturnGuidanceControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with ScalaCheckDrivenPropertyChecks
    with RedirectToStartBehaviour {

  override val overrideBindings =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore)
    )

  lazy val controller = instanceOf[FurtherReturnGuidanceController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  def isValidJourney(journeyStatus: JourneyStatus): Boolean =
    journeyStatus match {
      case _: StartingNewDraftReturn | _: FillingOutReturn | _: StartingToAmendReturn => true
      case _                                                                          => false
    }

  def userMessageKey(
    individualUserType: IndividualUserType,
    userType: UserType
  ): String =
    (individualUserType, userType) match {
      case (Capacitor, _)                             => ".capacitor"
      case (PersonalRepresentative, _)                => ".personalRep"
      case (PersonalRepresentativeInPeriodOfAdmin, _) => ".personalRepInPeriodOfAdmin"
      case (_, UserType.Individual)                   => ""
      case (_, UserType.Organisation)                 => ".trust"
      case (_, UserType.Agent)                        => ".agent"
      case other                                      => sys.error(s"User type '$other' not handled")
    }

  def setAgentReferenceNumber(
    userType: UserType
  ): Option[AgentReferenceNumber] =
    userType match {
      case UserType.Agent => Some(sample[AgentReferenceNumber])
      case _              => None
    }

  def setNameForUserType(
    userType: UserType
  ): Either[TrustName, IndividualName] =
    userType match {
      case UserType.Organisation => Left(sample[TrustName])
      case _                     => Right(sample[IndividualName])
    }

  def sessionDataWithStartingNewDraftReturn(
    triageAnswers: Either[
      MultipleDisposalsTriageAnswers,
      SingleDisposalTriageAnswers
    ],
    userType: UserType = UserType.Individual,
    representeeAnswers: Option[IncompleteRepresenteeAnswers] = None,
    previousSentReturns: Option[PreviousReturnData] = None
  ): (SessionData, StartingNewDraftReturn) = {
    val startingNewDraftReturn =
      sample[StartingNewDraftReturn].copy(
        subscribedDetails = sample[SubscribedDetails].copy(
          name = setNameForUserType(userType)
        ),
        newReturnTriageAnswers = triageAnswers,
        agentReferenceNumber = setAgentReferenceNumber(userType),
        representeeAnswers = representeeAnswers,
        previousSentReturns = previousSentReturns
      )

    val sessionData = SessionData.empty.copy(
      journeyStatus = Some(startingNewDraftReturn),
      userType = Some(userType)
    )

    sessionData -> startingNewDraftReturn
  }

  def sessionDataWithFillingOutReturnForSingleDisposals(
    singleDisposalTriageAnswers: SingleDisposalTriageAnswers,
    userType: UserType = UserType.Individual,
    previousReturns: Option[PreviousReturnData] = None
  ): (SessionData, FillingOutReturn, DraftSingleDisposalReturn) = {
    val draftReturn      = sample[DraftSingleDisposalReturn].copy(
      triageAnswers = singleDisposalTriageAnswers
    )
    val fillingOutReturn = sample[FillingOutReturn].copy(
      draftReturn = draftReturn,
      subscribedDetails = sample[SubscribedDetails].copy(
        name = setNameForUserType(userType)
      ),
      previousSentReturns = previousReturns
    )

    val sessionData = SessionData.empty.copy(
      journeyStatus = Some(fillingOutReturn),
      userType = Some(userType)
    )

    (sessionData, fillingOutReturn, draftReturn)
  }

  def sessionDataWithFillingOutReturnForMultipleDisposals(
    multipleDisposalsTriageAnswers: MultipleDisposalsTriageAnswers,
    userType: UserType = UserType.Individual
  ): (SessionData, FillingOutReturn, DraftMultipleDisposalsReturn) = {
    val draftReturn      = sample[DraftMultipleDisposalsReturn].copy(
      triageAnswers = multipleDisposalsTriageAnswers
    )
    val fillingOutReturn = sample[FillingOutReturn].copy(
      draftReturn = draftReturn,
      subscribedDetails = sample[SubscribedDetails].copy(
        name = setNameForUserType(userType)
      )
    )

    val sessionData = SessionData.empty.copy(
      journeyStatus = Some(fillingOutReturn),
      userType = Some(userType)
    )

    (sessionData, fillingOutReturn, draftReturn)
  }

  def sessionDataWithStartingToAmendReturnForMultipleDisposals(
    multipleDisposalsTriageAnswers: MultipleDisposalsTriageAnswers,
    userType: UserType = UserType.Individual
  ): (SessionData, StartingToAmendReturn, DraftMultipleDisposalsReturn) = {
    val draftReturn           = sample[DraftMultipleDisposalsReturn].copy(
      triageAnswers = multipleDisposalsTriageAnswers
    )
    val startingToAmendReturn = sample[StartingToAmendReturn].copy(
      subscribedDetails = sample[SubscribedDetails].copy(
        name = setNameForUserType(userType)
      ),
      isFirstReturn = false,
      agentReferenceNumber = if (userType === UserType.Agent) Some(sample[AgentReferenceNumber]) else None,
      previousSentReturns = Some(
        sample[PreviousReturnData].copy(
          summaries = List(sample[ReturnSummary]),
          previousYearToDate = None
        )
      ),
      originalReturn = sample[CompleteReturnWithSummary].copy(
        isFirstReturn = false
      )
    )

    val sessionData = SessionData.empty.copy(
      journeyStatus = Some(startingToAmendReturn),
      userType = Some(userType)
    )

    (sessionData, startingToAmendReturn, draftReturn)
  }

  val acceptedIndividualUserTypeGen: Gen[IndividualUserType] =
    individualUserTypeGen.filter {
      case Self | Capacitor | PersonalRepresentative | PersonalRepresentativeInPeriodOfAdmin => true
      case _                                                                                 => false
    }

  val acceptedUserTypeGen: Gen[UserType] = userTypeGen.filter {
    case UserType.Agent | UserType.Organisation | UserType.Individual => true
    case _                                                            => false
  }

  "FurtherReturnGuidanceController" when {

    "handling requests to display the further return guidance page for multiple disposals" must {

      def performAction(back: String): Future[Result] =
        controller.guidance(back)(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(
        () => performAction(FurtherReturnGuidanceController.BackLinkLocations.furtherReturnStart),
        isValidJourney
      )

      "the user is starting a new draft return and" when {

        "show an error page" when {

          "the back link parameter is not understood" in {
            forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
              (userType: UserType, individualUserType: IndividualUserType) =>
                inSequence {
                  mockAuthWithNoRetrievals()
                  mockGetSession(
                    sessionDataWithStartingNewDraftReturn(
                      Left(
                        IncompleteMultipleDisposalsTriageAnswers.empty.copy(
                          individualUserType = Some(individualUserType)
                        )
                      ),
                      userType = userType
                    )._1
                  )
                }

                checkIsTechnicalErrorPage(performAction("abc"))
            }
          }

        }

        "display the page" when {

          def test(
            individualUserType: IndividualUserType,
            userType: UserType,
            back: String,
            expectedBackLink: Call
          ): Unit = {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionDataWithStartingNewDraftReturn(
                  Left(
                    IncompleteMultipleDisposalsTriageAnswers.empty.copy(
                      individualUserType = Some(individualUserType)
                    )
                  ),
                  userType = userType
                )._1
              )
            }

            val userKey = userMessageKey(individualUserType, userType)

            checkPageIsDisplayed(
              performAction(back),
              messageFromMessageKey(s"furtherReturnGuidance$userKey.title"),
              doc => doc.select("#back").attr("href") shouldBe expectedBackLink.url
            )
          }

          "the user came from the further returns start page" in {
            forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
              (userType: UserType, individualUserType: IndividualUserType) =>
                test(
                  individualUserType,
                  userType,
                  BackLinkLocations.furtherReturnStart,
                  routes.CommonTriageQuestionsController.furtherReturnHelp()
                )
            }
          }

          "the user came from the in year losses page" in {
            forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
              (userType: UserType, individualUserType: IndividualUserType) =>
                test(
                  individualUserType,
                  userType,
                  BackLinkLocations.inYearLosses,
                  controllers.returns.exemptionandlosses.routes.ExemptionAndLossesController.inYearLosses()
                )
            }
          }

          "the user came from the amend start page" in {
            forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
              (userType: UserType, individualUserType: IndividualUserType) =>
                test(
                  individualUserType,
                  userType,
                  BackLinkLocations.calculateAmounts,
                  controllers.returns.amend.routes.AmendReturnController.youNeedToCalculate()
                )
            }
          }

        }

      }

      "the user is filling out a draft return and" when {

        "show an error page" when {

          "the back link parameter is not understood" in {
            forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
              (userType: UserType, individualUserType: IndividualUserType) =>
                inSequence {
                  mockAuthWithNoRetrievals()
                  mockGetSession(
                    sessionDataWithFillingOutReturnForMultipleDisposals(
                      IncompleteMultipleDisposalsTriageAnswers.empty.copy(
                        individualUserType = Some(individualUserType)
                      ),
                      userType
                    )._1
                  )
                }

                checkIsTechnicalErrorPage(performAction("abc"))
            }
          }

        }

        "display the page" when {

          def test(
            individualUserType: IndividualUserType,
            userType: UserType,
            back: String,
            expectedBackLink: Call
          ): Unit = {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionDataWithFillingOutReturnForMultipleDisposals(
                  IncompleteMultipleDisposalsTriageAnswers.empty.copy(
                    individualUserType = Some(individualUserType)
                  ),
                  userType
                )._1
              )
            }

            val userKey = userMessageKey(individualUserType, userType)

            checkPageIsDisplayed(
              performAction(back),
              messageFromMessageKey(s"furtherReturnGuidance$userKey.title"),
              doc => doc.select("#back").attr("href") shouldBe expectedBackLink.url
            )
          }

          "the user came from the further returns start page" in {
            forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
              (userType: UserType, individualUserType: IndividualUserType) =>
                test(
                  individualUserType,
                  userType,
                  BackLinkLocations.furtherReturnStart,
                  routes.CommonTriageQuestionsController.furtherReturnHelp()
                )
            }
          }

          "the user came from the in year losses page" in {
            forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
              (userType: UserType, individualUserType: IndividualUserType) =>
                test(
                  individualUserType,
                  userType,
                  BackLinkLocations.inYearLosses,
                  controllers.returns.exemptionandlosses.routes.ExemptionAndLossesController.inYearLosses()
                )
            }
          }

          "the user came from the amend start page" in {
            forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
              (userType: UserType, individualUserType: IndividualUserType) =>
                test(
                  individualUserType,
                  userType,
                  BackLinkLocations.calculateAmounts,
                  controllers.returns.amend.routes.AmendReturnController.youNeedToCalculate()
                )
            }
          }

        }

      }

      "the user is starting to amend return and" when {

        "show an error page" when {

          "the back link parameter is not understood" in {
            forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
              (userType: UserType, individualUserType: IndividualUserType) =>
                inSequence {
                  mockAuthWithNoRetrievals()
                  mockGetSession(
                    sessionDataWithStartingToAmendReturnForMultipleDisposals(
                      IncompleteMultipleDisposalsTriageAnswers.empty.copy(
                        individualUserType = Some(individualUserType)
                      ),
                      userType
                    )._1
                  )
                }

                checkIsTechnicalErrorPage(performAction("abc"))
            }
          }

        }

      }

    }

    "handling requests to display the further return guidance page for single disposal" must {

      def performAction(back: String): Future[Result] =
        controller.guidance(back)(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(
        () => performAction(FurtherReturnGuidanceController.BackLinkLocations.furtherReturnStart),
        isValidJourney
      )

      "the user is starting a new draft return and" when {

        "show an error page" when {

          "the back link parameter is not understood" in {
            forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
              (userType: UserType, individualUserType: IndividualUserType) =>
                inSequence {
                  mockAuthWithNoRetrievals()
                  mockGetSession(
                    sessionDataWithStartingNewDraftReturn(
                      Right(
                        IncompleteSingleDisposalTriageAnswers.empty.copy(
                          individualUserType = Some(individualUserType)
                        )
                      ),
                      userType = userType
                    )._1
                  )
                }

                checkIsTechnicalErrorPage(performAction("abc"))
            }
          }

        }

        "display the page" when {

          def test(
            individualUserType: IndividualUserType,
            userType: UserType,
            back: String,
            expectedBackLink: Call
          ): Unit = {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionDataWithStartingNewDraftReturn(
                  Right(
                    IncompleteSingleDisposalTriageAnswers.empty.copy(
                      individualUserType = Some(individualUserType)
                    )
                  ),
                  userType = userType
                )._1
              )
            }

            val userKey = userMessageKey(individualUserType, userType)

            checkPageIsDisplayed(
              performAction(back),
              messageFromMessageKey(s"furtherReturnGuidance$userKey.title"),
              doc => doc.select("#back").attr("href") shouldBe expectedBackLink.url
            )
          }

          "the user came from the further returns start page" in {
            forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
              (userType: UserType, individualUserType: IndividualUserType) =>
                test(
                  individualUserType,
                  userType,
                  BackLinkLocations.furtherReturnStart,
                  routes.CommonTriageQuestionsController.furtherReturnHelp()
                )
            }
          }

          "the user came from the in year losses page" in {
            forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
              (userType: UserType, individualUserType: IndividualUserType) =>
                test(
                  individualUserType,
                  userType,
                  BackLinkLocations.inYearLosses,
                  controllers.returns.exemptionandlosses.routes.ExemptionAndLossesController.inYearLosses()
                )
            }
          }

          "the user came from the amend start page" in {
            forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
              (userType: UserType, individualUserType: IndividualUserType) =>
                test(
                  individualUserType,
                  userType,
                  BackLinkLocations.calculateAmounts,
                  controllers.returns.amend.routes.AmendReturnController.youNeedToCalculate()
                )
            }
          }

        }

      }

      "the user is filling out a draft return and" when {

        "show an error page" when {

          "the back link parameter is not understood" in {
            forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
              (userType: UserType, individualUserType: IndividualUserType) =>
                inSequence {
                  mockAuthWithNoRetrievals()
                  mockGetSession(
                    sessionDataWithFillingOutReturnForSingleDisposals(
                      IncompleteSingleDisposalTriageAnswers.empty.copy(
                        individualUserType = Some(individualUserType)
                      ),
                      userType
                    )._1
                  )
                }

                checkIsTechnicalErrorPage(performAction("abc"))
            }
          }

        }

        "display the page" when {

          def test(
            individualUserType: IndividualUserType,
            userType: UserType,
            back: String,
            expectedBackLink: Call
          ): Unit = {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionDataWithFillingOutReturnForSingleDisposals(
                  IncompleteSingleDisposalTriageAnswers.empty.copy(
                    individualUserType = Some(individualUserType)
                  ),
                  userType
                )._1
              )
            }

            val userKey = userMessageKey(individualUserType, userType)

            checkPageIsDisplayed(
              performAction(back),
              messageFromMessageKey(s"furtherReturnGuidance$userKey.title"),
              doc => doc.select("#back").attr("href") shouldBe expectedBackLink.url
            )
          }

          "the user came from the further returns start page" in {
            forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
              (userType: UserType, individualUserType: IndividualUserType) =>
                test(
                  individualUserType,
                  userType,
                  BackLinkLocations.furtherReturnStart,
                  routes.CommonTriageQuestionsController.furtherReturnHelp()
                )
            }
          }

          "the user came from the in year losses page" in {
            forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
              (userType: UserType, individualUserType: IndividualUserType) =>
                test(
                  individualUserType,
                  userType,
                  BackLinkLocations.inYearLosses,
                  controllers.returns.exemptionandlosses.routes.ExemptionAndLossesController.inYearLosses()
                )
            }
          }

          "the user came from the amend start page" in {
            forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
              (userType: UserType, individualUserType: IndividualUserType) =>
                test(
                  individualUserType,
                  userType,
                  BackLinkLocations.calculateAmounts,
                  controllers.returns.amend.routes.AmendReturnController.youNeedToCalculate()
                )
            }
          }

        }

      }

    }

    "handling requests to display the overall gain guidance page for a single disposal" must {

      def performAction(): Future[Result] =
        controller.taxableGainGuidance()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(
        () => performAction(),
        isValidJourney
      )

      "the user is filling out a draft return and" when {

        "display the page" when {

          def test(
            individualUserType: IndividualUserType,
            userType: UserType
          ): Unit = {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionDataWithFillingOutReturnForSingleDisposals(
                  IncompleteSingleDisposalTriageAnswers.empty.copy(
                    individualUserType = Some(individualUserType)
                  ),
                  userType
                )._1
              )
            }

            val userKey = userMessageKey(individualUserType, userType)

            checkPageIsDisplayed(
              performAction(),
              messageFromMessageKey(s"taxableGainOrLossGuidance$userKey.title"),
              doc =>
                doc
                  .select("#back")
                  .attr("href") shouldBe returns.yeartodatelliability.routes.YearToDateLiabilityController
                  .taxableGainOrLoss()
                  .url
            )
          }

          "the user came from the overall gain page" in {
            forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
              (userType: UserType, individualUserType: IndividualUserType) =>
                test(
                  individualUserType,
                  userType
                )
            }
          }

        }

      }

    }

    "handling requests to display the overall gain guidance page for a multiple disposal" must {

      def performAction(): Future[Result] =
        controller.taxableGainGuidance()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(
        () => performAction(),
        isValidJourney
      )

      "the user is filling out a draft return and" when {

        "display the page" when {

          def test(
            individualUserType: IndividualUserType,
            userType: UserType
          ): Unit = {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionDataWithFillingOutReturnForMultipleDisposals(
                  IncompleteMultipleDisposalsTriageAnswers.empty.copy(
                    individualUserType = Some(individualUserType)
                  ),
                  userType
                )._1
              )
            }

            val userKey = userMessageKey(individualUserType, userType)

            checkPageIsDisplayed(
              performAction(),
              messageFromMessageKey(s"taxableGainOrLossGuidance$userKey.title"),
              doc =>
                doc
                  .select("#back")
                  .attr("href") shouldBe returns.yeartodatelliability.routes.YearToDateLiabilityController
                  .taxableGainOrLoss()
                  .url
            )
          }

          "the user came from the overall gain page" in {
            forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
              (userType: UserType, individualUserType: IndividualUserType) =>
                test(
                  individualUserType,
                  userType
                )
            }
          }

        }

      }

    }

    "handling requests to display the overall liability guidance page for a single disposal" must {

      def performAction(): Future[Result] =
        controller.overallGainGuidance()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(
        () => performAction(),
        isValidJourney
      )

      "the user is filling out a draft return and" when {

        "display the page" when {

          def test(
                    individualUserType: IndividualUserType,
                    userType: UserType
                  ): Unit = {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionDataWithFillingOutReturnForSingleDisposals(
                  IncompleteSingleDisposalTriageAnswers.empty.copy(
                    individualUserType = Some(individualUserType)
                  ),
                  userType
                )._1
              )
            }

            val userKey = userMessageKey(individualUserType, userType)

            checkPageIsDisplayed(
              performAction(),
              messageFromMessageKey(s"yearToDateLiabilityGuidance$userKey.title"),
              doc =>
                doc
                  .select("#back")
                  .attr("href") shouldBe returns.yeartodatelliability.routes.YearToDateLiabilityController
                  .yearToDateLiability()
                  .url
            )
          }

          "the user came from the overall liability page" in {
            forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
              (userType: UserType, individualUserType: IndividualUserType) =>
                test(
                  individualUserType,
                  userType
                )
            }
          }

        }

      }

    }

    "handling requests to display the overall liability guidance page for a multiple disposal" must {

      def performAction(): Future[Result] =
        controller.overallGainGuidance()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(
        () => performAction(),
        isValidJourney
      )

      "the user is filling out a draft return and" when {

        "display the page" when {

          def test(
                    individualUserType: IndividualUserType,
                    userType: UserType
                  ): Unit = {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionDataWithFillingOutReturnForMultipleDisposals(
                  IncompleteMultipleDisposalsTriageAnswers.empty.copy(
                    individualUserType = Some(individualUserType)
                  ),
                  userType
                )._1
              )
            }

            val userKey = userMessageKey(individualUserType, userType)

            checkPageIsDisplayed(
              performAction(),
              messageFromMessageKey(s"yearToDateLiabilityGuidance$userKey.title"),
              doc =>
                doc
                  .select("#back")
                  .attr("href") shouldBe returns.yeartodatelliability.routes.YearToDateLiabilityController
                  .yearToDateLiability()
                  .url
            )
          }

          "the user came from the overall liability page" in {
            forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
              (userType: UserType, individualUserType: IndividualUserType) =>
                test(
                  individualUserType,
                  userType
                )
            }
          }

        }

      }

    }
  }

}
