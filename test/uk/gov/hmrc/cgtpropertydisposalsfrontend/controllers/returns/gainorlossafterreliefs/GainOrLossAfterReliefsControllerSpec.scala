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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.gainorlossafterreliefs

import cats.syntax.order._
import org.jsoup.nodes.Document
import org.scalatest.matchers.should.Matchers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.SampledScalaCheck
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
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.{FurtherReturnCalculationEligibilityUtilSupport, ReturnsServiceSupport, StartingToAmendToFillingOutReturnSpecBehaviour}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport, returns}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, PreviousReturnData, StartingToAmendReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.RetrievedUserType.Trust
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.UserType.{Agent, Individual, Organisation}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.{Address, Postcode}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.{AmountInPence, MoneyUtils}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.AcquisitionDetailsGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.DisposalDetailsGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.DraftReturnGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.JourneyStatusGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.NameGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.ReliefDetailsGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.RepresenteeAnswersGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.ReturnGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.SubscribedDetailsGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.TriageQuestionsGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.UUIDGenerator
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.{IndividualName, TrustName}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.SubscribedDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.AcquisitionDetailsAnswers.CompleteAcquisitionDetailsAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.DisposalDetailsAnswers.CompleteDisposalDetailsAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.IndividualUserType.{Capacitor, PersonalRepresentative, PersonalRepresentativeInPeriodOfAdmin, Self}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.MultipleDisposalsTriageAnswers.CompleteMultipleDisposalsTriageAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ReliefDetailsAnswers.CompleteReliefDetailsAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.RepresenteeAnswers.CompleteRepresenteeAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.CompleteSingleDisposalTriageAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, SessionData, UserType}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.FurtherReturnCalculationEligibility.{Eligible, Ineligible}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.{FurtherReturnCalculationEligibilityUtil, ReturnsService}

import scala.concurrent.Future

class GainOrLossAfterReliefsControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with ReturnsServiceSupport
    with FurtherReturnCalculationEligibilityUtilSupport
    with SampledScalaCheck
    with RedirectToStartBehaviour
    with StartingToAmendToFillingOutReturnSpecBehaviour {

  private val mockUUIDGenerator = mock[UUIDGenerator]

  private def redirectToStartBehaviour(performAction: () => Future[Result]): Unit =
    redirectToStartWhenInvalidJourney(
      performAction,
      {
        case f: FillingOutReturn if f.isFurtherOrAmendReturn.contains(true) => true
        case _: StartingToAmendReturn                                       => true
        case _                                                              => false
      }
    )

  private lazy val controller = instanceOf[GainOrLossAfterReliefsController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

//  implicit val hc: HeaderCarrier = mock[HeaderCarrier]

  protected override val overrideBindings: List[GuiceableModule] = List[GuiceableModule](
    bind[AuthConnector].toInstance(mockAuthConnector),
    bind[SessionStore].toInstance(mockSessionStore),
    bind[ReturnsService].toInstance(mockReturnsService),
    bind[UUIDGenerator].toInstance(mockUUIDGenerator),
    bind[FurtherReturnCalculationEligibilityUtil].toInstance(mockFurtherReturnCalculationEligibilityUtil)
  )

  implicit lazy val messages: Messages = MessagesImpl(Lang("en"), messagesApi)

  val currentReturnAddress: Address.UkAddress = Address.UkAddress("line 1", None, None, None, Postcode("ABC D12"))

  def sessionWithSingleDisposalState(
    gainOrLossAfterReliefs: Option[AmountInPence],
    individualUserType: Option[IndividualUserType] = Some(Self),
    userType: UserType = Individual,
    subscribedDetails: SubscribedDetails = sample[SubscribedDetails].copy(name = Right(sample[IndividualName])),
    previousReturnsImplyEligibilityForFurtherReturnCalculation: Option[Boolean] = Some(false)
  ): (SessionData, FillingOutReturn, DraftSingleDisposalReturn) = {
    val triageAnswers = sample[CompleteSingleDisposalTriageAnswers].copy(
      individualUserType = individualUserType
    )

    val draftReturn = sample[DraftSingleDisposalReturn]
      .copy(
        gainOrLossAfterReliefs = gainOrLossAfterReliefs,
        triageAnswers = triageAnswers,
        representeeAnswers = Some(sample[CompleteRepresenteeAnswers].copy(isFirstReturn = false))
      )

    val taxYearStartYear: String =
      triageAnswers
        .fold(
          _.disposalDate.map(_.taxYear.startDateInclusive.getYear),
          c => Some(c.disposalDate.taxYear.startDateInclusive.getYear)
        )
        .map(_.toString)
        .getOrElse("2020")

    val journey = sample[FillingOutReturn].copy(
      draftReturn = draftReturn,
      subscribedDetails = subscribedDetails,
      previousSentReturns = Some(
        PreviousReturnData(
          List(sample[ReturnSummary].copy(taxYear = taxYearStartYear)),
          None,
          previousReturnsImplyEligibilityForFurtherReturnCalculation,
          None
        )
      )
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
    subscribedDetails: SubscribedDetails = sample[SubscribedDetails].copy(name = Right(sample[IndividualName])),
    previousReturnsImplyEligibilityForFurtherReturnCalculation: Option[Boolean] = Some(false)
  ): (SessionData, FillingOutReturn, DraftMultipleDisposalsReturn) = {
    val triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers].copy(
      individualUserType = individualUserType
    )

    val taxYearStartYear: String =
      triageAnswers
        .fold(
          _.taxYear.map(_.startDateInclusive.getYear),
          c => Some(c.taxYear.startDateInclusive.getYear)
        )
        .map(_.toString)
        .getOrElse("2020")

    val draftReturn = sample[DraftMultipleDisposalsReturn]
      .copy(
        gainOrLossAfterReliefs = gainOrLossAfterReliefs,
        triageAnswers = triageAnswers,
        representeeAnswers = Some(sample[CompleteRepresenteeAnswers].copy(isFirstReturn = false))
      )
    val journey     = sample[FillingOutReturn].copy(
      draftReturn = draftReturn,
      subscribedDetails = subscribedDetails,
      previousSentReturns = Some(
        PreviousReturnData(
          List(sample[ReturnSummary].copy(taxYear = taxYearStartYear)),
          None,
          previousReturnsImplyEligibilityForFurtherReturnCalculation,
          None
        )
      )
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
    isMultipleDisposal: Boolean = false,
    previousReturnsImplyEligibilityForFurtherReturnCalculation: Option[Boolean] = Some(false)
  ): (SessionData, FillingOutReturn, DraftReturn) =
    if (isMultipleDisposal) {
      sessionWithMultipleDisposalsState(
        gainOrLossAfterReliefs,
        individualUserType,
        userType,
        subscribedDetails,
        previousReturnsImplyEligibilityForFurtherReturnCalculation
      )
    } else {
      sessionWithSingleDisposalState(
        gainOrLossAfterReliefs,
        individualUserType,
        userType,
        subscribedDetails,
        previousReturnsImplyEligibilityForFurtherReturnCalculation
      )
    }

  private def individualSession(
    gainOrLossAfterReliefs: Option[AmountInPence] = None,
    isMultipleDisposal: Boolean = false,
    previousReturnsImplyEligibilityForFurtherReturnCalculation: Option[Boolean] = Some(false)
  ) =
    session(
      gainOrLossAfterReliefs,
      isMultipleDisposal = isMultipleDisposal,
      previousReturnsImplyEligibilityForFurtherReturnCalculation =
        previousReturnsImplyEligibilityForFurtherReturnCalculation
    )

  private def agentSession(
    gainOrLossAfterReliefs: Option[AmountInPence] = None,
    isMultipleDisposal: Boolean = false,
    previousReturnsImplyEligibilityForFurtherReturnCalculation: Option[Boolean] = Some(false)
  ) =
    session(
      gainOrLossAfterReliefs,
      userType = Agent,
      isMultipleDisposal = isMultipleDisposal,
      previousReturnsImplyEligibilityForFurtherReturnCalculation =
        previousReturnsImplyEligibilityForFurtherReturnCalculation
    )

  private def trustSession(
    gainOrLossAfterReliefs: Option[AmountInPence] = None,
    isMultipleDisposal: Boolean = false,
    previousReturnsImplyEligibilityForFurtherReturnCalculation: Option[Boolean] = Some(false)
  ) =
    session(
      gainOrLossAfterReliefs,
      userType = Organisation,
      subscribedDetails = sample[SubscribedDetails].copy(name = Left(sample[TrustName])),
      isMultipleDisposal = isMultipleDisposal,
      previousReturnsImplyEligibilityForFurtherReturnCalculation =
        previousReturnsImplyEligibilityForFurtherReturnCalculation
    )

  private def capacitorSession(
    gainOrLossAfterReliefs: Option[AmountInPence] = None,
    isMultipleDisposal: Boolean = false,
    previousReturnsImplyEligibilityForFurtherReturnCalculation: Option[Boolean] = Some(false)
  ) =
    session(
      gainOrLossAfterReliefs,
      individualUserType = Some(Capacitor),
      isMultipleDisposal = isMultipleDisposal,
      previousReturnsImplyEligibilityForFurtherReturnCalculation =
        previousReturnsImplyEligibilityForFurtherReturnCalculation
    )

  private def personalRepSession(
    gainOrLossAfterReliefs: Option[AmountInPence] = None,
    isMultipleDisposal: Boolean = false,
    previousReturnsImplyEligibilityForFurtherReturnCalculation: Option[Boolean] = Some(false)
  ) =
    session(
      gainOrLossAfterReliefs,
      individualUserType = Some(PersonalRepresentative),
      isMultipleDisposal = isMultipleDisposal,
      previousReturnsImplyEligibilityForFurtherReturnCalculation =
        previousReturnsImplyEligibilityForFurtherReturnCalculation
    )

  private def periodOfAdminSession(
    gainOrLossAfterReliefs: Option[AmountInPence] = None,
    isMultipleDisposal: Boolean = false,
    previousReturnsImplyEligibilityForFurtherReturnCalculation: Option[Boolean] = Some(false)
  ) =
    session(
      gainOrLossAfterReliefs,
      individualUserType = Some(PersonalRepresentativeInPeriodOfAdmin),
      isMultipleDisposal = isMultipleDisposal,
      previousReturnsImplyEligibilityForFurtherReturnCalculation =
        previousReturnsImplyEligibilityForFurtherReturnCalculation
    )

  private def agentOfPeriodOfAdminSession(
    gainOrLossAfterReliefs: Option[AmountInPence] = None,
    isMultipleDisposal: Boolean = false,
    previousReturnsImplyEligibilityForFurtherReturnCalculation: Option[Boolean] = Some(false)
  ) =
    session(
      gainOrLossAfterReliefs,
      userType = Agent,
      individualUserType = Some(PersonalRepresentativeInPeriodOfAdmin),
      isMultipleDisposal = isMultipleDisposal,
      previousReturnsImplyEligibilityForFurtherReturnCalculation =
        previousReturnsImplyEligibilityForFurtherReturnCalculation
    )

  private val testCasesWithUserKeys = List(
    individualSession()           -> "",
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

      behave like redirectToStartBehaviour(() => performAction())

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.enterGainOrLossAfterReliefs(),
        mockUUIDGenerator
      )

      "display the page" when {

        "the user is ineligible for a calculation and" when {
          def test(
            sessionData: (SessionData, FillingOutReturn, DraftReturn),
            expectedTitleKey: String,
            expectedBackLink: Call,
            expectedOuterLabelUserKey: String,
            expectedH2Key: String,
            expectedLi1Key: String
          ): Unit = {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(sessionData._1)
              mockFurtherReturnCalculationEligibilityCheck(sessionData._2)(Right(Ineligible(Some(false))))
            }
            checkPageIsDisplayed(
              performAction(),
              messageFromMessageKey(expectedTitleKey),
              { doc =>
                doc.select(".govuk-back-link").attr("href") shouldBe expectedBackLink.url

                doc.select(".govuk-caption-xl").text() shouldBe messageFromMessageKey(
                  "gainOrLossAfterReliefs.caption"
                )

                doc.select("label[for='gainOrLossAfterReliefs']").text() shouldBe messageFromMessageKey(
                  s"gainOrLossAfterReliefs.gain$expectedOuterLabelUserKey.outerLabel"
                )

                doc.select("label[for='gainOrLossAfterReliefs-2']").text() shouldBe messageFromMessageKey(
                  s"gainOrLossAfterReliefs.loss$expectedOuterLabelUserKey.outerLabel"
                )

                doc.select("label[for='gainOrLossAfterReliefs-3']").text() shouldBe messageFromMessageKey(
                  s"gainOrLossAfterReliefs.noLossOrGain$expectedOuterLabelUserKey.outerLabel"
                )

                doc.select("legend").text() shouldBe messageFromMessageKey(expectedH2Key)

                doc
                  .select("#main-content ol.govuk-list--number > li:nth-child(1)")
                  .text() shouldBe messageFromMessageKey(
                  expectedLi1Key
                )

                doc
                  .select("#main-content form")
                  .attr("action") shouldBe routes.GainOrLossAfterReliefsController
                  .enterGainOrLossAfterReliefsSubmit()
                  .url
              }
            )
          }

          "the user is on a single disposal journey and" when {

            "the user is an individual doing the return for themselves" in {
              test(
                individualSession(),
                "gainOrLossAfterReliefs.title",
                returns.routes.TaskListController.taskList(),
                "",
                "gainOrLossAfterReliefs.h2",
                "gainOrLossAfterReliefs.li1"
              )
            }

            "the user is an agent for an individual" in {
              test(
                agentSession(),
                "gainOrLossAfterReliefs.agent.title",
                returns.routes.TaskListController.taskList(),
                ".notSelf",
                "gainOrLossAfterReliefs.agent.h2",
                "gainOrLossAfterReliefs.li1"
              )
            }

            "the user is a trust" in {
              test(
                trustSession(Some(AmountInPence(1L))),
                "gainOrLossAfterReliefs.trust.title",
                routes.GainOrLossAfterReliefsController.checkYourAnswers(),
                ".notSelf",
                "gainOrLossAfterReliefs.trust.h2",
                "gainOrLossAfterReliefs.li1"
              )
            }

            "the user is a capacitor" in {
              test(
                capacitorSession(),
                "gainOrLossAfterReliefs.capacitor.title",
                returns.routes.TaskListController.taskList(),
                ".notSelf",
                "gainOrLossAfterReliefs.capacitor.h2",
                "gainOrLossAfterReliefs.li1"
              )
            }

            "the user is a personal rep" in {
              test(
                personalRepSession(),
                "gainOrLossAfterReliefs.personalRep.title",
                returns.routes.TaskListController.taskList(),
                ".notSelf",
                "gainOrLossAfterReliefs.personalRep.h2",
                "gainOrLossAfterReliefs.li1"
              )
            }

            "the user is an estate in a period of admin" in {
              test(
                periodOfAdminSession(Some(AmountInPence(0L))),
                "gainOrLossAfterReliefs.personalRepInPeriodOfAdmin.title",
                routes.GainOrLossAfterReliefsController.checkYourAnswers(),
                ".notSelf",
                "gainOrLossAfterReliefs.personalRepInPeriodOfAdmin.h2",
                "gainOrLossAfterReliefs.li1"
              )
            }

            "the user is an agent of an estate in a period of admin" in {
              test(
                agentOfPeriodOfAdminSession(),
                "gainOrLossAfterReliefs.personalRepInPeriodOfAdmin.agent.title",
                returns.routes.TaskListController.taskList(),
                ".notSelf",
                "gainOrLossAfterReliefs.personalRepInPeriodOfAdmin.agent.h2",
                "gainOrLossAfterReliefs.li1"
              )
            }
          }

          "the user is on a multiple disposals journey and" when {

            "the user is an individual doing the return for themselves" in {
              test(
                individualSession(isMultipleDisposal = true),
                "gainOrLossAfterReliefs.multipleDisposals.title",
                returns.routes.TaskListController.taskList(),
                "",
                "gainOrLossAfterReliefs.multipleDisposals.h2",
                "gainOrLossAfterReliefs.multipleDisposals.li1"
              )
            }

            "the user is an agent for an individual" in {
              test(
                agentSession(isMultipleDisposal = true),
                "gainOrLossAfterReliefs.agent.multipleDisposals.title",
                returns.routes.TaskListController.taskList(),
                ".notSelf",
                "gainOrLossAfterReliefs.agent.multipleDisposals.h2",
                "gainOrLossAfterReliefs.multipleDisposals.li1"
              )
            }

            "the user is a trust" in {
              test(
                trustSession(Some(AmountInPence(1L)), isMultipleDisposal = true),
                "gainOrLossAfterReliefs.trust.multipleDisposals.title",
                routes.GainOrLossAfterReliefsController.checkYourAnswers(),
                ".notSelf",
                "gainOrLossAfterReliefs.trust.multipleDisposals.h2",
                "gainOrLossAfterReliefs.multipleDisposals.li1"
              )
            }

            "the user is a capacitor" in {
              test(
                capacitorSession(isMultipleDisposal = true),
                "gainOrLossAfterReliefs.capacitor.multipleDisposals.title",
                returns.routes.TaskListController.taskList(),
                ".notSelf",
                "gainOrLossAfterReliefs.capacitor.multipleDisposals.h2",
                "gainOrLossAfterReliefs.multipleDisposals.li1"
              )
            }

            "the user is a personal rep" in {
              test(
                personalRepSession(isMultipleDisposal = true),
                "gainOrLossAfterReliefs.personalRep.multipleDisposals.title",
                returns.routes.TaskListController.taskList(),
                ".notSelf",
                "gainOrLossAfterReliefs.personalRep.multipleDisposals.h2",
                "gainOrLossAfterReliefs.multipleDisposals.li1"
              )
            }

            "the user is an estate in a period of admin" in {
              test(
                periodOfAdminSession(Some(AmountInPence(0L)), isMultipleDisposal = true),
                "gainOrLossAfterReliefs.personalRepInPeriodOfAdmin.multipleDisposals.title",
                routes.GainOrLossAfterReliefsController.checkYourAnswers(),
                ".notSelf",
                "gainOrLossAfterReliefs.personalRepInPeriodOfAdmin.multipleDisposals.h2",
                "gainOrLossAfterReliefs.multipleDisposals.li1"
              )
            }

            "the user is an agent of an estate in a period of admin" in {
              test(
                agentOfPeriodOfAdminSession(isMultipleDisposal = true),
                "gainOrLossAfterReliefs.personalRepInPeriodOfAdmin.agent.multipleDisposals.title",
                returns.routes.TaskListController.taskList(),
                ".notSelf",
                "gainOrLossAfterReliefs.personalRepInPeriodOfAdmin.agent.multipleDisposals.h2",
                "gainOrLossAfterReliefs.multipleDisposals.li1"
              )
            }
          }

          "the draft return and session need updating" in {}
        }

        "the user is eligible for a calculation and" when {
          def test(
            sessionData: (SessionData, FillingOutReturn, DraftReturn, PreviousReturnData),
            expectedTitleKey: String,
            expectedBackLink: Call,
            expectedOuterLabelUserKey: String,
            expectedH2Key: String
          ): Unit = {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(sessionData._1)
              mockFurtherReturnCalculationEligibilityCheck(sessionData._2)(
                Right(
                  Eligible(
                    CalculatedGlarBreakdown(
                      AmountInPence(0),
                      AmountInPence(0),
                      AmountInPence(0),
                      AmountInPence(0),
                      AmountInPence(1L),
                      AmountInPence(0),
                      AmountInPence(0),
                      false,
                      None
                    ),
                    List.empty,
                    currentReturnAddress
                  )
                )
              )
            }
            checkPageIsDisplayed(
              performAction(),
              messageFromMessageKey(expectedTitleKey),
              { doc =>
                doc.select("#back, .govuk-back-link").attr("href") shouldBe expectedBackLink.url

                doc.select(".govuk-caption-xl").text() shouldBe messageFromMessageKey(
                  "gainOrLossAfterReliefs.caption"
                )

                doc.select("label[for='gainOrLossAfterReliefs']").text() shouldBe messageFromMessageKey(
                  s"gainOrLossAfterReliefs.gain$expectedOuterLabelUserKey.outerLabel"
                )

                doc.select("label[for='gainOrLossAfterReliefs-2']").text() shouldBe messageFromMessageKey(
                  s"gainOrLossAfterReliefs.loss$expectedOuterLabelUserKey.outerLabel"
                )

                doc
                  .select(
                    ".govuk-details__text > .govuk-summary-list:nth-child(1) > div.sum-part:nth-child(1) .govuk-summary-list__key"
                  )
                  .text() shouldBe messageFromMessageKey("calculator.disposalPrice")
                doc
                  .select(
                    ".govuk-details__text > .govuk-summary-list:nth-child(1) > div.sum-part:nth-child(2) .govuk-summary-list__key"
                  )
                  .text() shouldBe messageFromMessageKey("calculator.disposalFees")
                doc
                  .select(
                    ".govuk-details__text > .govuk-summary-list:nth-child(1) > div.sum-total > .govuk-summary-list__key"
                  )
                  .text() shouldBe messageFromMessageKey("calculator.total.disposalAmount")

                doc
                  .select(
                    ".govuk-details__text > .govuk-summary-list:nth-child(2) > div.sum-part:nth-child(1) > .govuk-summary-list__key"
                  )
                  .text() shouldBe messageFromMessageKey("calculator.acquisitionPrice")
                doc
                  .select(
                    ".govuk-details__text > .govuk-summary-list:nth-child(2) > div.sum-part:nth-child(2) .govuk-summary-list__key"
                  )
                  .text() shouldBe messageFromMessageKey("calculator.improvementCosts")
                doc
                  .select(
                    ".govuk-details__text > .govuk-summary-list:nth-child(2) > div.sum-part:nth-child(3) > .govuk-summary-list__key"
                  )
                  .text() shouldBe messageFromMessageKey("calculator.acquisitionFees")
                doc
                  .select(
                    ".govuk-details__text > .govuk-summary-list:nth-child(2) > div.sum-total > .govuk-summary-list__key"
                  )
                  .text() shouldBe messageFromMessageKey("calculator.total.acquisitionAmount")

                doc
                  .select(
                    ".govuk-details__text > .govuk-summary-list:nth-child(3) > div.sum-part:nth-child(1) .govuk-summary-list__key"
                  )
                  .text() shouldBe messageFromMessageKey("calculator.total.disposalAmount")
                doc
                  .select(
                    ".govuk-details__text > .govuk-summary-list:nth-child(3) > div.sum-part:nth-child(2) .govuk-summary-list__key"
                  )
                  .text() shouldBe messageFromMessageKey("calculator.total.acquisitionAmount")
                doc
                  .select(
                    ".govuk-details__text > .govuk-summary-list:nth-child(3) > div.sum-total > .govuk-summary-list__key"
                  )
                  .text() shouldBe messageFromMessageKey("calculator.total.initialGainOrLoss")

                doc
                  .select(
                    ".govuk-details__text > .govuk-summary-list:nth-child(4) > div.sum-part:nth-child(1) .govuk-summary-list__key"
                  )
                  .text() shouldBe messageFromMessageKey("calculator.privateResidenceRelief")
                doc
                  .select(
                    ".govuk-details__text > .govuk-summary-list:nth-child(4) > div.sum-part:nth-child(2) .govuk-summary-list__key"
                  )
                  .text() shouldBe messageFromMessageKey("calculator.lettingRelief")
                doc
                  .select(
                    ".govuk-details__text > .govuk-summary-list:nth-child(4) > div.sum-total > .govuk-summary-list__key"
                  )
                  .text() shouldBe messageFromMessageKey("calculator.total.reliefs")

                doc
                  .select(
                    ".govuk-details__text > .govuk-summary-list:nth-child(5) > div.sum-part:nth-child(1) .govuk-summary-list__key"
                  )
                  .text() shouldBe messageFromMessageKey("calculator.total.initialGainOrLoss")
                doc
                  .select(
                    ".govuk-details__text > .govuk-summary-list:nth-child(5) > div.sum-part:nth-child(2) .govuk-summary-list__key"
                  )
                  .text() shouldBe messageFromMessageKey("calculator.total.reliefs")
                doc
                  .select(
                    ".govuk-details__text > .govuk-summary-list:nth-child(5) > div.sum-total > .govuk-summary-list__key"
                  )
                  .text() shouldBe messageFromMessageKey("calculator.total.gainOrLossAfterReliefs")

                doc.select("legend").text() shouldBe messageFromMessageKey(expectedH2Key)

                doc
                  .select("#main-content form")
                  .attr("action") shouldBe routes.GainOrLossAfterReliefsController
                  .enterGainOrLossAfterReliefsSubmit()
                  .url
              }
            )
          }

          "the user is on a single disposal journey and" when {

            def validCalculatorState(
              userType: UserType,
              individualUserType: IndividualUserType,
              previousReturnData: PreviousReturnData
            ): (SessionData, FillingOutReturn, DraftReturn, PreviousReturnData) = {
              val triageAnswers = sample[CompleteSingleDisposalTriageAnswers].copy(
                individualUserType = Some(individualUserType)
              )
              val draftReturn   = sample[DraftSingleDisposalReturn]
                .copy(
                  gainOrLossAfterReliefs = Some(AmountInPence(1000)),
                  triageAnswers = triageAnswers,
                  representeeAnswers = Some(sample[CompleteRepresenteeAnswers].copy(isFirstReturn = false)),
                  acquisitionDetailsAnswers = Some(sample[CompleteAcquisitionDetailsAnswers]),
                  reliefDetailsAnswers = Some(sample[CompleteReliefDetailsAnswers]),
                  disposalDetailsAnswers = Some(sample[CompleteDisposalDetailsAnswers])
                )

              val taxYearStartYear: String =
                triageAnswers
                  .fold(
                    _.disposalDate.map(_.taxYear.startDateInclusive.getYear),
                    c => Some(c.disposalDate.taxYear.startDateInclusive.getYear)
                  )
                  .map(_.toString)
                  .getOrElse("2020")

              val updatedpreviousReturnData =
                previousReturnData.copy(
                  summaries = previousReturnData.summaries.map(_.copy(taxYear = taxYearStartYear))
                )

              val journey = sample[FillingOutReturn].copy(
                draftReturn = draftReturn,
                subscribedDetails = sample[SubscribedDetails]
                  .copy(name = if (userType === Trust) Left(sample[TrustName]) else Right(sample[IndividualName])),
                previousSentReturns = Some(updatedpreviousReturnData)
              )

              (
                SessionData.empty.copy(
                  journeyStatus = Some(journey),
                  userType = Some(userType)
                ),
                journey,
                draftReturn,
                updatedpreviousReturnData
              )
            }

            "the user is an individual doing the return for themselves" in {
              val previousReturnData = PreviousReturnData(List(sample[ReturnSummary]), None, Some(true), None)

              test(
                validCalculatorState(Individual, Self, previousReturnData),
                "gainOrLossAfterReliefs.title",
                routes.GainOrLossAfterReliefsController.checkYourAnswers(),
                "",
                "gainOrLossAfterReliefs.h2"
              )
            }

            "there is no value for previousReturnsImplyEligibilityForFurtherReturnCalculation in session" in {
              val previousReturnData = PreviousReturnData(
                List(sample[ReturnSummary]),
                None,
                None,
                None
              )

              val state = validCalculatorState(Individual, Self, previousReturnData)

              test(
                state,
                "gainOrLossAfterReliefs.title",
                routes.GainOrLossAfterReliefsController.checkYourAnswers(),
                "",
                "gainOrLossAfterReliefs.h2"
              )
            }

            "the value for total reliefs residence relief is zero" in {
              val previousReturnData = PreviousReturnData(List(sample[ReturnSummary]), None, Some(true), None)

              val (session, fillingOutReturn, _, _) =
                validCalculatorState(Individual, Self, previousReturnData)

              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
                mockFurtherReturnCalculationEligibilityCheck(fillingOutReturn)(
                  Right(
                    Eligible(
                      CalculatedGlarBreakdown(
                        AmountInPence(0),
                        AmountInPence(0),
                        AmountInPence(0),
                        AmountInPence(0),
                        AmountInPence(0),
                        AmountInPence(0),
                        AmountInPence(0),
                        false,
                        None
                      ),
                      List.empty,
                      currentReturnAddress
                    )
                  )
                )
              }

              checkPageIsDisplayed(
                performAction(),
                messageFromMessageKey("gainOrLossAfterReliefs.title"),
                { doc =>
                  doc.text() shouldNot include(messageFromMessageKey("calculator.privateResidenceRelief"))
                  doc.text() shouldNot include(messageFromMessageKey("calculator.lettingRelief"))
                  doc
                    .select("label[for='gainOrLossAfterReliefs-3']")
                    .text() shouldBe messageFromMessageKey("gainOrLossAfterReliefs.noLossOrGain.outerLabel")

                  doc
                    .select("#main-content form")
                    .attr("action") shouldBe routes.GainOrLossAfterReliefsController
                    .enterGainOrLossAfterReliefsSubmit()
                    .url
                }
              )

            }

          }

        }

      }

      "Display the correct working" when {

        def validCalculatorState(
          userType: UserType,
          individualUserType: IndividualUserType,
          previousReturnData: PreviousReturnData
        ): (SessionData, FillingOutReturn, DraftReturn, PreviousReturnData) = {
          val triageAnswers = sample[CompleteSingleDisposalTriageAnswers].copy(
            individualUserType = Some(individualUserType)
          )
          val draftReturn   = sample[DraftSingleDisposalReturn]
            .copy(
              gainOrLossAfterReliefs = Some(AmountInPence(1000)),
              triageAnswers = triageAnswers,
              representeeAnswers = Some(sample[CompleteRepresenteeAnswers].copy(isFirstReturn = false)),
              acquisitionDetailsAnswers = Some(sample[CompleteAcquisitionDetailsAnswers]),
              reliefDetailsAnswers = Some(sample[CompleteReliefDetailsAnswers]),
              disposalDetailsAnswers = Some(sample[CompleteDisposalDetailsAnswers])
            )

          val taxYearStartYear: String =
            triageAnswers
              .fold(
                _.disposalDate.map(_.taxYear.startDateInclusive.getYear),
                c => Some(c.disposalDate.taxYear.startDateInclusive.getYear)
              )
              .map(_.toString)
              .getOrElse("2020")

          val updatedpreviousReturnData =
            previousReturnData.copy(
              summaries = previousReturnData.summaries.map(_.copy(taxYear = taxYearStartYear))
            )

          val journey = sample[FillingOutReturn].copy(
            draftReturn = draftReturn,
            subscribedDetails = sample[SubscribedDetails]
              .copy(name = if (userType === Trust) Left(sample[TrustName]) else Right(sample[IndividualName])),
            previousSentReturns = Some(updatedpreviousReturnData)
          )

          (
            SessionData.empty.copy(
              journeyStatus = Some(journey),
              userType = Some(userType)
            ),
            journey,
            draftReturn,
            updatedpreviousReturnData
          )
        }

        "Given a regular data set" in {
          val previousReturnData = PreviousReturnData(List(sample[ReturnSummary]), None, Some(true), None)

          val (session, fillingOutReturn, _, _) =
            validCalculatorState(Individual, Self, previousReturnData)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockFurtherReturnCalculationEligibilityCheck(fillingOutReturn)(
              Right(
                Eligible(
                  CalculatedGlarBreakdown(
                    acquisitionPrice = AmountInPence(100_000_00),
                    acquisitionCosts = AmountInPence(0),
                    disposalPrice = AmountInPence(200_000_00),
                    disposalFees = AmountInPence(0),
                    privateResidentReliefs = AmountInPence(0),
                    lettingRelief = AmountInPence(0),
                    improvementCosts = AmountInPence(0),
                    shouldUseRebase = true,
                    rebasedAcquisitionPrice = Some(AmountInPence(150_000_00))
                  ),
                  List.empty,
                  currentReturnAddress
                )
              )
            )
          }
          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("gainOrLossAfterReliefs.title"),
            { doc =>
              doc.text() should include("Enter your gain or loss after reliefs")
              doc.text() should include("Property disposal amount £200,000")
              doc.text() should include("Disposal costs - £0")
              doc.text() should include("Property disposal amount less costs = £200,000")
              doc.text() should include("Property acquisition amount (rebased) £150,000")
              doc.text() should include("Improvement costs + £0")
              doc.text() should include("Acquisition costs + £0")
              doc.text() should include("Property acquisition amount plus costs = £150,000")
              doc.text() should include("Property acquisition amount plus costs - £150,000")
              doc.text() should include("Initial gain or loss = £50,000 (gain)")
              doc.text() should include("Initial gain or loss £50,000 (gain)")
              doc.text() should include("Total reliefs - £0")
              doc.text() should include("Gain or loss after reliefs = £50,000 (gain)")

              doc
                .select("#main-content form")
                .attr("action") shouldBe routes.GainOrLossAfterReliefsController
                .enterGainOrLossAfterReliefsSubmit()
                .url
            }
          )

        }

      }

      "show an error page" when {

        "there is an error determining eligibility for calculation" in {
          val triageAnswers = sample[CompleteSingleDisposalTriageAnswers].copy(
            individualUserType = Some(Self)
          )

          val taxYearStartYear: String =
            triageAnswers
              .fold(
                _.disposalDate.map(_.taxYear.startDateInclusive.getYear),
                c => Some(c.disposalDate.taxYear.startDateInclusive.getYear)
              )
              .map(_.toString)
              .getOrElse("2020")

          val previousReturnData = PreviousReturnData(
            List(sample[ReturnSummary].copy(taxYear = taxYearStartYear)),
            None,
            None,
            None
          )

          val fillingOutReturn = sample[FillingOutReturn].copy(
            previousSentReturns = Some(previousReturnData),
            draftReturn = sample[DraftSingleDisposalReturn].copy(
              triageAnswers = triageAnswers
            )
          )
          val sessionData      = SessionData.empty.copy(Some(fillingOutReturn))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionData)
            mockFurtherReturnCalculationEligibilityCheck(fillingOutReturn)(Left(Error("Error on eligibility check")))
          }
          checkIsTechnicalErrorPage(performAction())
        }

      }

    }

    "handling submitted answers to gain or loss after reliefs" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.enterGainOrLossAfterReliefsSubmit()(
          FakeRequest().withFormUrlEncodedBody(data*).withMethod("POST")
        )

      def updateDraftReturn(
        d: DraftSingleDisposalReturn,
        newAnswer: AmountInPence
      ): DraftSingleDisposalReturn =
        d.copy(gainOrLossAfterReliefs = Some(newAnswer))

      behave like redirectToStartBehaviour(() => performAction())

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.enterGainOrLossAfterReliefsSubmit(),
        mockUUIDGenerator
      )

      "show a form error" when {

        def testFormError(
          sessionData: SessionData,
          fillingOutReturn: FillingOutReturn,
          data: (String, String)*
        )(
          pageTitleKey: String,
          expectedErrorMessageKey: String
        ): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionData)
            mockFurtherReturnCalculationEligibilityCheck(fillingOutReturn)(Right(Ineligible(Some(false))))
          }

          checkPageIsDisplayed(
            performAction(data*),
            messageFromMessageKey(pageTitleKey),
            doc =>
              doc
                .select("[data-spec='errorSummaryDisplay'] a")
                .text() shouldBe messageFromMessageKey(
                expectedErrorMessageKey
              ),
            BAD_REQUEST
          )
        }

        "no option is selected" in {
          testCasesWithUserKeys.foreach { case (session, userKey) =>
            withClue(s"For test case '$userKey': ") {
              testFormError(
                session._1,
                session._2
              )(
                s"gainOrLossAfterReliefs$userKey.title",
                s"gainOrLossAfterReliefs$userKey.error.required"
              )
            }
          }
        }

        "the amount of gain is invalid" in {
          testCasesWithUserKeys.foreach { case (session, userKey) =>
            amountOfMoneyErrorScenarios("gainAfterReliefs")
              .foreach { scenario =>
                withClue(s"For test case '$userKey' and scenario $scenario: ") {
                  val data = ("gainOrLossAfterReliefs" -> "0") :: scenario.formData

                  testFormError(session._1, session._2, data*)(
                    s"gainOrLossAfterReliefs$userKey.title",
                    scenario.expectedErrorMessageKey
                  )
                }
              }
          }
        }

        "the amount of loss is invalid" in {
          testCasesWithUserKeys.foreach { case (session, userKey) =>
            amountOfMoneyErrorScenarios("lossAfterReliefs")
              .foreach { scenario =>
                withClue(s"For test case '$userKey' and scenario $scenario: ") {
                  val data = ("gainOrLossAfterReliefs" -> "1") :: scenario.formData

                  testFormError(session._1, session._2, data*)(
                    s"gainOrLossAfterReliefs$userKey.title",
                    scenario.expectedErrorMessageKey
                  )
                }
              }
          }
        }

        "the amount of gain is zero" in {
          testCasesWithUserKeys.foreach { case (session, userKey) =>
            withClue(s"For test case '$userKey': ") {
              testFormError(
                session._1,
                session._2,
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
          testCasesWithUserKeys.foreach { case (session, userKey) =>
            withClue(s"For test case '$userKey': ") {
              testFormError(
                session._1,
                session._2,
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
            .copy(exemptionAndLossesAnswers = None, yearToDateLiabilityAnswers = None)
          val updatedJourney                  = journey.copy(draftReturn = updatedDraftReturn)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(updatedJourney)(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction("gainOrLossAfterReliefs" -> "2"))
        }

        "there is an error updating the session" in {
          val (session, journey, draftReturn) = sessionWithSingleDisposalState(None)
          val updatedDraftReturn              = updateDraftReturn(draftReturn, AmountInPence(0L))
            .copy(exemptionAndLossesAnswers = None, yearToDateLiabilityAnswers = None)
          val updatedJourney                  = journey.copy(draftReturn = updatedDraftReturn)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(updatedJourney)(Right(()))
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
        ): Unit = {
          val (session, journey, draftReturn) = sessionWithSingleDisposalState(None)
          val newDraftReturn                  = updateDraftReturn(draftReturn, newGainOrLossAfterReliefs)
            .copy(exemptionAndLossesAnswers = None, yearToDateLiabilityAnswers = None)
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
            performAction(formData*),
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

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.checkYourAnswers(),
        mockUUIDGenerator
      )

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
              doc.select("h1").text() shouldBe messageFromMessageKey(
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
                .select("#main-content form")
                .attr("action") shouldBe routes.GainOrLossAfterReliefsController
                .checkYourAnswersSubmit()
                .url
            }
          )
        }

        "the user is an individual doing the return for themselves" in {
          test(
            individualSession(Some(AmountInPence(600L)), isMultipleDisposal = true)._1,
            "gainOrLossAfterReliefs.multipleDisposals.h2",
            "gainOrLossAfterReliefs.gain.outerLabel",
            Some("gainOrLossAfterReliefs.gain.innerLabel"),
            Some("£6")
          )
        }

        "the user is an agent of an individual" in {
          test(
            agentSession(Some(AmountInPence(-600L)))._1,
            "gainOrLossAfterReliefs.agent.h2",
            "gainOrLossAfterReliefs.loss.notSelf.outerLabel",
            Some("gainOrLossAfterReliefs.loss.innerLabel"),
            Some("£6")
          )
        }

        "the user is a trust" in {
          test(
            trustSession(Some(AmountInPence(0L)))._1,
            "gainOrLossAfterReliefs.trust.h2",
            "gainOrLossAfterReliefs.noLossOrGain.notSelf.outerLabel",
            None,
            None
          )
        }

        "the user is a capacitor" in {
          test(
            capacitorSession(Some(AmountInPence(0L)))._1,
            "gainOrLossAfterReliefs.capacitor.h2",
            "gainOrLossAfterReliefs.noLossOrGain.notSelf.outerLabel",
            None,
            None
          )
        }

        "the user is a personal rep" in {
          test(
            personalRepSession(Some(AmountInPence(0L)), isMultipleDisposal = true)._1,
            "gainOrLossAfterReliefs.personalRep.multipleDisposals.h2",
            "gainOrLossAfterReliefs.noLossOrGain.notSelf.outerLabel",
            None,
            None
          )
        }

        "the user is a personal rep in period of admin" in {
          test(
            periodOfAdminSession(Some(AmountInPence(0L)))._1,
            "gainOrLossAfterReliefs.personalRepInPeriodOfAdmin.h2",
            "gainOrLossAfterReliefs.noLossOrGain.notSelf.outerLabel",
            None,
            None
          )
        }

        "the user is a agent of a personal rep in period of admin" in {
          test(
            agentOfPeriodOfAdminSession(Some(AmountInPence(0L)))._1,
            "gainOrLossAfterReliefs.personalRepInPeriodOfAdmin.agent.h2",
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

      behave like redirectToStartBehaviour(() => performAction())

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.checkYourAnswersSubmit(),
        mockUUIDGenerator
      )

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
          if (isAnAgent) ".agent" else if (isATrust) ".trust" else ""
      }
      val multipleDisposalKeys = if (isMultipleDisposal) ".multipleDisposals" else ""
      s"gainOrLossAfterReliefs$userTypeKey$multipleDisposalKeys.h2"
    }

    val (expectedQuestionValueKey, expectedAmountKey, expectedAmountValue) = {
      val userKey = individualUserType match {
        case Some(Self) if !isATrust && !isAnAgent => ""
        case _                                     => ".notSelf"
      }

      if (gainOrLossAfterReliefs > AmountInPence.zero) {
        (
          s"gainOrLossAfterReliefs.gain$userKey.outerLabel",
          Some("gainOrLossAfterReliefs.gain.innerLabel"),
          Some(MoneyUtils.formatAmountOfMoneyWithPoundSign(gainOrLossAfterReliefs.inPounds()))
        )
      } else if (gainOrLossAfterReliefs < AmountInPence.zero) {
        (
          s"gainOrLossAfterReliefs.loss$userKey.outerLabel",
          Some("gainOrLossAfterReliefs.loss.innerLabel"),
          Some(MoneyUtils.formatAmountOfMoneyWithPoundSign(gainOrLossAfterReliefs.inPounds().abs))
        )
      } else {
        (s"gainOrLossAfterReliefs.noGainOrLoss$userKey.outerLabel", None, None)
      }
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
