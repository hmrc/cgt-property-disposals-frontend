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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.exemptionandlosses

import org.jsoup.nodes.Document
import org.scalacheck.Gen
import org.scalatest.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.http.Status.BAD_REQUEST
import play.api.i18n.MessagesApi
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.{Call, Result}
import play.api.test.FakeRequest
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.AmountOfMoneyErrorScenarios.amountOfMoneyErrorScenarios
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.ReturnsServiceSupport
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.exemptionandlosses.ExemptionAndLossesControllerSpec.validateExemptionAndLossesCheckYourAnswersPage
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport, returns}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.FillingOutReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Country
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.MoneyUtils.formatAmountOfMoneyWithPoundSign
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.{AmountInPence, MoneyUtils}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.AgentReferenceNumber
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.{IndividualName, TrustName}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.SubscribedDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ExamplePropertyDetailsAnswers.IncompleteExamplePropertyDetailsAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ExemptionAndLossesAnswers.{CompleteExemptionAndLossesAnswers, IncompleteExemptionAndLossesAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.IndividualUserType.{Capacitor, PersonalRepresentative, PersonalRepresentativeInPeriodOfAdmin, Self}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.MultipleDisposalsTriageAnswers.IncompleteMultipleDisposalsTriageAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.IncompleteSingleDisposalTriageAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService

import scala.concurrent.Future

class ExemptionAndLossesControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with ReturnsServiceSupport
    with ScalaCheckDrivenPropertyChecks
    with RedirectToStartBehaviour {

  override val overrideBindings =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore),
      bind[ReturnsService].toInstance(mockReturnsService)
    )

  lazy val controller = instanceOf[ExemptionAndLossesController]
  val individualName  = Right(IndividualName("Hodor", "Hodor"))
  val trustName       = Left(TrustName("Littlefinger"))

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  def redirectToStartBehaviour(performAction: () => Future[Result]) =
    redirectToStartWhenInvalidJourney(
      performAction,
      {
        case _: FillingOutReturn => true
        case _                   => false
      }
    )

  def setNameForUserType(
    userType: UserType
  ): Either[TrustName, IndividualName] =
    userType match {
      case UserType.Organisation => Left(sample[TrustName])
      case _                     => Right(sample[IndividualName])
    }

  def setAgentReferenceNumber(
    userType: UserType
  ): Option[AgentReferenceNumber] =
    userType match {
      case UserType.Agent => Some(sample[AgentReferenceNumber])
      case _              => None
    }

  def userMessageKey(
    individualUserType: IndividualUserType,
    userType: UserType
  ): String =
    (individualUserType, userType) match {
      case (Capacitor, _)                                          => ".capacitor"
      case (PersonalRepresentative, _)                             => ".personalRep"
      case (PersonalRepresentativeInPeriodOfAdmin, UserType.Agent) => ".personalRepInPeriodOfAdmin.agent"
      case (PersonalRepresentativeInPeriodOfAdmin, _)              => ".personalRepInPeriodOfAdmin"
      case (_, UserType.Individual)                                => ""
      case (_, UserType.Organisation)                              => ".trust"
      case (_, UserType.Agent)                                     => ".agent"
      case other                                                   => sys.error(s"User type '$other' not handled")
    }

  private def sampleSingleDisposalTriageAnswers(
    disposalDate: Option[DisposalDate],
    individualUserType: Option[IndividualUserType]
  ): IncompleteSingleDisposalTriageAnswers =
    sample[IncompleteSingleDisposalTriageAnswers].copy(
      disposalDate = disposalDate,
      countryOfResidence = Some(Country.uk),
      wasAUKResident = Some(true),
      individualUserType = individualUserType
    )

  private def sampleFillingOutReturn(
    draftReturn: DraftReturn,
    userType: UserType
  ) =
    sample[FillingOutReturn].copy(
      draftReturn = draftReturn,
      subscribedDetails = sample[SubscribedDetails].copy(name = setNameForUserType(userType)),
      agentReferenceNumber = setAgentReferenceNumber(userType)
    )

  def sessionWithSingleDisposalsState(
    answers: Option[ExemptionAndLossesAnswers],
    disposalDate: Option[DisposalDate],
    userType: UserType,
    individualUserType: Option[IndividualUserType]
  ): (SessionData, FillingOutReturn, DraftSingleDisposalReturn) = {
    val draftReturn =
      sample[DraftSingleDisposalReturn].copy(
        triageAnswers = sampleSingleDisposalTriageAnswers(disposalDate, individualUserType),
        exemptionAndLossesAnswers = answers
      )

    val journey = sampleFillingOutReturn(draftReturn, userType)

    val sessionData = SessionData.empty.copy(
      journeyStatus = Some(journey),
      userType = Some(userType)
    )

    (sessionData, journey, draftReturn)
  }

  def sessionWithSingleDisposalState(
    answers: ExemptionAndLossesAnswers,
    disposalDate: DisposalDate,
    userType: UserType,
    individualUserType: Option[IndividualUserType]
  ): (SessionData, FillingOutReturn, DraftSingleDisposalReturn) =
    sessionWithSingleDisposalsState(
      Some(answers),
      Some(disposalDate),
      userType,
      individualUserType
    )

  def sessionWithMultipleDisposalsState(
    answers: Option[ExemptionAndLossesAnswers],
    disposalDate: Option[DisposalDate],
    userType: UserType,
    individualUserType: Option[IndividualUserType]
  ): (SessionData, FillingOutReturn, DraftMultipleDisposalsReturn) = {

    val draftReturn =
      sample[DraftMultipleDisposalsReturn].copy(
        examplePropertyDetailsAnswers = Some(
          sample[IncompleteExamplePropertyDetailsAnswers].copy(
            disposalDate = disposalDate
          )
        ),
        exemptionAndLossesAnswers = answers,
        triageAnswers = sample[IncompleteMultipleDisposalsTriageAnswers].copy(
          countryOfResidence = Some(Country.uk),
          wasAUKResident = Some(true),
          individualUserType = individualUserType
        )
      )

    val journey = sampleFillingOutReturn(draftReturn, userType)

    val sessionData = SessionData.empty.copy(
      journeyStatus = Some(journey),
      userType = Some(userType)
    )

    (sessionData, journey, draftReturn)
  }

  def sessionWithMultipleDisposalsState(
    answers: ExemptionAndLossesAnswers,
    disposalDate: DisposalDate,
    userType: UserType,
    individualUserType: Option[IndividualUserType]
  ): (SessionData, FillingOutReturn, DraftMultipleDisposalsReturn) =
    sessionWithMultipleDisposalsState(
      Some(answers),
      Some(disposalDate),
      userType,
      individualUserType
    )

  def sessionWithSingleIndirectDisposalsState(
    answers: ExemptionAndLossesAnswers,
    disposalDate: DisposalDate,
    userType: UserType,
    individualUserType: Option[IndividualUserType]
  ): (SessionData, FillingOutReturn, DraftSingleIndirectDisposalReturn) = {

    val draftReturn =
      sample[DraftSingleIndirectDisposalReturn].copy(
        exemptionAndLossesAnswers = Some(answers),
        triageAnswers = sampleSingleDisposalTriageAnswers(Some(disposalDate), individualUserType)
      )

    val subscribedDetails = sample[SubscribedDetails].copy(name = setNameForUserType(userType))

    val journey = sample[FillingOutReturn].copy(
      draftReturn = draftReturn,
      subscribedDetails = subscribedDetails,
      agentReferenceNumber = setAgentReferenceNumber(userType)
    )

    val sessionData = SessionData.empty.copy(
      journeyStatus = Some(journey),
      userType = Some(userType)
    )

    (sessionData, journey, draftReturn)
  }

  def sessionWithSingleMixedUseDisposalsState(
    answers: ExemptionAndLossesAnswers,
    disposalDate: DisposalDate,
    userType: UserType,
    individualUserType: Option[IndividualUserType]
  ): (SessionData, FillingOutReturn, DraftSingleMixedUseDisposalReturn) = {

    val draftReturn =
      sample[DraftSingleMixedUseDisposalReturn].copy(
        exemptionAndLossesAnswers = Some(answers),
        triageAnswers = sampleSingleDisposalTriageAnswers(Some(disposalDate), individualUserType)
      )

    val subscribedDetails = sample[SubscribedDetails].copy(name = setNameForUserType(userType))

    val journey = sample[FillingOutReturn].copy(
      draftReturn = draftReturn,
      subscribedDetails = subscribedDetails,
      agentReferenceNumber = setAgentReferenceNumber(userType)
    )

    val sessionData = SessionData.empty.copy(
      journeyStatus = Some(journey),
      userType = Some(userType)
    )

    (sessionData, journey, draftReturn)
  }

  val acceptedUserTypeGen: Gen[UserType] = userTypeGen.filter {
    case UserType.Agent | UserType.Organisation | UserType.Individual => true
    case _                                                            => false
  }

  val acceptedIndividualUserTypeGen: Gen[IndividualUserType] =
    individualUserTypeGen.filter {
      case Self | Capacitor | PersonalRepresentative | PersonalRepresentativeInPeriodOfAdmin => true
      case _                                                                                 => false
    }

  "ExemptionAndLossesController" when {

    "handling requests to display the in year losses page" must {

      def performAction(): Future[Result] =
        controller.inYearLosses()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      val key = "inYearLosses"

      "redirect to the task list page" when {

        "there is no disposal date" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithSingleDisposalsState(
                    Some(sample[CompleteExemptionAndLossesAnswers]),
                    None,
                    userType,
                    Some(individualUserType)
                  )._1
                )
              }

              checkIsRedirect(
                performAction(),
                returns.routes.TaskListController.taskList()
              )
          }
        }

      }

      "display the page" when {

        "the exemption and losses section has not yet been started" in {
          val disposalDate = sample[DisposalDate]

          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithSingleDisposalsState(
                    None,
                    Some(disposalDate),
                    userType,
                    Some(individualUserType)
                  )._1
                )
              }

              val userKey = userMessageKey(individualUserType, userType)

              checkPageIsDisplayed(
                performAction(),
                messageFromMessageKey(
                  s"$key$userKey.title",
                  disposalDate.taxYear.startDateInclusive.getYear.toString,
                  disposalDate.taxYear.endDateExclusive.getYear.toString
                ),
                { doc =>
                  doc
                    .select("#back")
                    .attr("href")   shouldBe returns.routes.TaskListController
                    .taskList()
                    .url
                  doc
                    .select("#content > article > form")
                    .attr("action") shouldBe routes.ExemptionAndLossesController
                    .inYearLossesSubmit()
                    .url
                }
              )
          }

        }

        "the exemption and losses section has been completed" in {
          val disposalDate = sample[DisposalDate]

          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithMultipleDisposalsState(
                    sample[CompleteExemptionAndLossesAnswers],
                    disposalDate,
                    userType,
                    Some(individualUserType)
                  )._1
                )
              }

              val userKey = userMessageKey(individualUserType, userType)

              checkPageIsDisplayed(
                performAction(),
                messageFromMessageKey(
                  s"$key$userKey.title",
                  disposalDate.taxYear.startDateInclusive.getYear.toString,
                  disposalDate.taxYear.endDateExclusive.getYear.toString
                ),
                { doc =>
                  doc
                    .select("#back")
                    .attr("href")   shouldBe routes.ExemptionAndLossesController
                    .checkYourAnswers()
                    .url
                  doc
                    .select("#content > article > form")
                    .attr("action") shouldBe routes.ExemptionAndLossesController
                    .inYearLossesSubmit()
                    .url
                }
              )
          }
        }

        "the amount in the session is zero" in {
          val disposalDate = sample[DisposalDate]
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithSingleIndirectDisposalsState(
                    sample[IncompleteExemptionAndLossesAnswers].copy(
                      inYearLosses = Some(AmountInPence.zero)
                    ),
                    disposalDate,
                    userType,
                    Some(individualUserType)
                  )._1
                )
              }

              val userKey = userMessageKey(individualUserType, userType)

              checkPageIsDisplayed(
                performAction(),
                messageFromMessageKey(
                  s"$key$userKey.title",
                  disposalDate.taxYear.startDateInclusive.getYear.toString,
                  disposalDate.taxYear.endDateExclusive.getYear.toString
                ),
                doc =>
                  doc
                    .select("#inYearLosses-1")
                    .attr("checked") shouldBe "checked"
              )
          }
        }

        "the amount in the session is non-zero" in {
          val amountInPence = AmountInPence(1000L)
          val disposalDate  = sample[DisposalDate]
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithSingleMixedUseDisposalsState(
                    sample[IncompleteExemptionAndLossesAnswers].copy(
                      inYearLosses = Some(amountInPence)
                    ),
                    disposalDate,
                    userType,
                    Some(individualUserType)
                  )._1
                )
              }

              val userKey = userMessageKey(individualUserType, userType)

              checkPageIsDisplayed(
                performAction(),
                messageFromMessageKey(
                  s"$key$userKey.title",
                  disposalDate.taxYear.startDateInclusive.getYear.toString,
                  disposalDate.taxYear.endDateExclusive.getYear.toString
                ),
                { doc =>
                  doc
                    .select("#inYearLosses-0")
                    .attr("checked")                             shouldBe "checked"
                  doc.select("#inYearLossesValue").attr("value") shouldBe "10"
                }
              )
          }
        }

      }

    }

    "handling submitted in year losses page" must {

      val key      = "inYearLosses"
      val valueKey = "inYearLossesValue"

      def performAction(data: (String, String)*): Future[Result] =
        controller.inYearLossesSubmit()(
          FakeRequest().withFormUrlEncodedBody(data: _*)
        )

      behave like redirectToStartBehaviour(() => performAction())

      "redirect to the task list page" when {

        "there is no disposal date" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithSingleDisposalsState(
                    Some(sample[CompleteExemptionAndLossesAnswers]),
                    None,
                    userType,
                    Some(individualUserType)
                  )._1
                )
              }

              checkIsRedirect(
                performAction(),
                returns.routes.TaskListController.taskList()
              )
          }
        }

      }

      "show a form error" when {

        val disposalDate = sample[DisposalDate]

        def test(data: (String, String)*)(
          expectedErrorKey: String
        )(
          userType: UserType,
          individualUserType: IndividualUserType,
          userKey: String
        ): Unit = {

          val session = sessionWithMultipleDisposalsState(
            sample[CompleteExemptionAndLossesAnswers],
            disposalDate,
            userType,
            Some(individualUserType)
          )._1

          testFormError(data: _*)(key, expectedErrorKey)(
            s"$key$userKey.title",
            disposalDate.taxYear.startDateInclusive.getYear.toString,
            disposalDate.taxYear.endDateExclusive.getYear.toString
          )(performAction, session)
        }

        "no option has been selected" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val userKey = userMessageKey(individualUserType, userType)
              test()(s"$key$userKey.error.required")(
                userType,
                individualUserType,
                userKey
              )
          }
        }

        "the option selected is not valid" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val userKey = userMessageKey(individualUserType, userType)
              test(key -> "2")(s"$key$userKey.error.invalid")(
                userType,
                individualUserType,
                userKey
              )
          }
        }

        "the amount of money is invalid" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              amountOfMoneyErrorScenarios(valueKey).foreach { scenario =>
                withClue(s"For $scenario: ") {
                  val data    = (key -> "0") :: scenario.formData
                  val userKey = userMessageKey(individualUserType, userType)
                  test(data: _*)(scenario.expectedErrorMessageKey)(
                    userType,
                    individualUserType,
                    userKey
                  )
                }
              }
          }
        }

      }

      "show an error page" when {

        def getSessionJourneyAndDraftReturn(
          userType: UserType,
          newAmount: AmountInPence,
          individualUserType: IndividualUserType
        ): (SessionData, FillingOutReturn, DraftReturn) = {
          val answers = sample[CompleteExemptionAndLossesAnswers]

          val completeAnswers: CompleteExemptionAndLossesAnswers = answers.copy(
            inYearLosses = AmountInPence(newAmount.value + 1L)
          )

          val (session, journey, draftReturn) = sessionWithSingleIndirectDisposalsState(
            completeAnswers,
            sample[DisposalDate],
            userType,
            Some(individualUserType)
          )

          val updatedDraftReturn = draftReturn.copy(
            exemptionAndLossesAnswers = Some(answers.copy(inYearLosses = newAmount)),
            yearToDateLiabilityAnswers = draftReturn.yearToDateLiabilityAnswers
              .flatMap(_.unsetAllButIncomeDetails())
          )

          (session, journey, updatedDraftReturn)
        }

        "there is an error updating the draft return" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val newAmount                              = AmountInPence(123L)
              val (session, journey, updatedDraftReturn) =
                getSessionJourneyAndDraftReturn(
                  userType,
                  newAmount,
                  individualUserType
                )

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
                  key      -> "0",
                  valueKey -> newAmount.inPounds().toString
                )
              )
          }
        }

        "there is an error updating the session" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val newAmount                              = AmountInPence(123L)
              val (session, journey, updatedDraftReturn) =
                getSessionJourneyAndDraftReturn(
                  userType,
                  newAmount,
                  individualUserType
                )

              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
                mockStoreDraftReturn(
                  updatedDraftReturn,
                  journey.subscribedDetails.cgtReference,
                  journey.agentReferenceNumber
                )(Right(()))
                mockStoreSession(
                  session.copy(
                    journeyStatus = Some(journey.copy(draftReturn = updatedDraftReturn))
                  )
                )(Left(Error("")))
              }

              checkIsTechnicalErrorPage(
                performAction(
                  key      -> "0",
                  valueKey -> newAmount.inPounds().toString
                )
              )
          }
        }

      }

      "redirect to the task list page" when {

        "all updates are successful and" when {

          "the user selects no and the journey was incomplete" in {
            val newAmount = AmountInPence(3000L)
            val answers   = sample[IncompleteExemptionAndLossesAnswers].copy(
              inYearLosses = None
            )
            forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
              (userType: UserType, individualUserType: IndividualUserType) =>
                testSuccessfulUpdatesAfterSubmit(
                  performAction(
                    key      -> "0",
                    valueKey -> newAmount.inPounds().toString
                  )
                )(answers, answers.copy(inYearLosses = Some(newAmount)))(
                  userType,
                  individualUserType
                )
            }
          }

          "the user selects no and the journey was complete" in {
            val newAmount = AmountInPence(4000L)
            val answers   =
              sample[CompleteExemptionAndLossesAnswers].copy(
                inYearLosses = AmountInPence(newAmount.value + 1)
              )
            forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
              (userType: UserType, individualUserType: IndividualUserType) =>
                testSuccessfulUpdatesAfterSubmit(
                  performAction(
                    key      -> "0",
                    valueKey -> newAmount.inPounds().toString
                  )
                )(answers, answers.copy(inYearLosses = newAmount))(
                  userType,
                  individualUserType
                )
            }
          }

          "the user selects yes and submits a valid value and the journey was incomplete" in {
            val answers =
              sample[IncompleteExemptionAndLossesAnswers]
                .copy(inYearLosses = None)
            forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
              (userType: UserType, individualUserType: IndividualUserType) =>
                testSuccessfulUpdatesAfterSubmit(
                  performAction(key -> "1")
                )(
                  answers,
                  answers.copy(inYearLosses = Some(AmountInPence.zero))
                )(userType, individualUserType)
            }
          }

          "the user selects yes and submits a valid value and the journey was complete" in {
            val answers =
              sample[CompleteExemptionAndLossesAnswers]
                .copy(inYearLosses = AmountInPence(1L))

            forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
              (userType: UserType, individualUserType: IndividualUserType) =>
                testSuccessfulUpdatesAfterSubmit(
                  performAction(key -> "1")
                )(answers, answers.copy(inYearLosses = AmountInPence.zero))(
                  userType,
                  individualUserType
                )
            }
          }

        }

      }

      "not do any updates" when {

        "the value submitted hasn't changed" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val answers = sample[CompleteExemptionAndLossesAnswers].copy(
                inYearLosses = AmountInPence.zero
              )

              val session = sessionWithSingleMixedUseDisposalsState(
                answers,
                sample[DisposalDate],
                userType,
                Some(individualUserType)
              )._1

              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
              }

              checkIsRedirect(
                performAction(key -> "1"),
                routes.ExemptionAndLossesController.checkYourAnswers()
              )
          }

        }

      }

    }

    "handling requests to display the previous years losses page" must {

      val key      = "previousYearsLosses"
      val valueKey = "previousYearsLossesValue"

      def performAction(): Future[Result] =
        controller.previousYearsLosses()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      "redirect to the in year losses page" when {

        "that question has not been answered" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithSingleDisposalState(
                    sample[IncompleteExemptionAndLossesAnswers]
                      .copy(inYearLosses = None),
                    sample[DisposalDate],
                    userType,
                    Some(individualUserType)
                  )._1
                )
              }

              checkIsRedirect(
                performAction(),
                routes.ExemptionAndLossesController.inYearLosses()
              )
          }
        }

      }

      "display the page" when {

        "the exemption and losses section has been not completed" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithMultipleDisposalsState(
                    sample[IncompleteExemptionAndLossesAnswers].copy(
                      inYearLosses = Some(sample[AmountInPence])
                    ),
                    sample[DisposalDate],
                    userType,
                    Some(individualUserType)
                  )._1
                )
              }

              val userKey = userMessageKey(individualUserType, userType)

              checkPageIsDisplayed(
                performAction(),
                messageFromMessageKey(
                  s"$key$userKey.title"
                ),
                { doc =>
                  doc
                    .select("#back")
                    .attr("href")   shouldBe routes.ExemptionAndLossesController
                    .inYearLosses()
                    .url
                  doc
                    .select("#content > article > form")
                    .attr("action") shouldBe routes.ExemptionAndLossesController
                    .previousYearsLossesSubmit()
                    .url
                }
              )
          }
        }

        "the exemption and losses section has been completed" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithSingleIndirectDisposalsState(
                    sample[CompleteExemptionAndLossesAnswers],
                    sample[DisposalDate],
                    userType,
                    Some(individualUserType)
                  )._1
                )
              }

              val userKey = userMessageKey(individualUserType, userType)

              checkPageIsDisplayed(
                performAction(),
                messageFromMessageKey(
                  s"$key$userKey.title"
                ),
                { doc =>
                  doc
                    .select("#back")
                    .attr("href")   shouldBe routes.ExemptionAndLossesController
                    .checkYourAnswers()
                    .url
                  doc
                    .select("#content > article > form")
                    .attr("action") shouldBe routes.ExemptionAndLossesController
                    .previousYearsLossesSubmit()
                    .url
                }
              )
          }
        }

        "the amount in the session is zero" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithSingleMixedUseDisposalsState(
                    sample[IncompleteExemptionAndLossesAnswers].copy(
                      inYearLosses = Some(sample[AmountInPence]),
                      previousYearsLosses = Some(AmountInPence.zero)
                    ),
                    sample[DisposalDate],
                    userType,
                    Some(individualUserType)
                  )._1
                )
              }

              val userKey = userMessageKey(individualUserType, userType)

              checkPageIsDisplayed(
                performAction(),
                messageFromMessageKey(
                  s"$key$userKey.title"
                ),
                doc =>
                  doc
                    .select("#previousYearsLosses-1")
                    .attr("checked") shouldBe "checked"
              )
          }
        }

        "the amount in the session is non-zero" in {
          val amountInPence = AmountInPence(1000L)

          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithMultipleDisposalsState(
                    sample[IncompleteExemptionAndLossesAnswers].copy(
                      inYearLosses = Some(sample[AmountInPence]),
                      previousYearsLosses = Some(amountInPence)
                    ),
                    sample[DisposalDate],
                    userType,
                    Some(individualUserType)
                  )._1
                )
              }

              val userKey = userMessageKey(individualUserType, userType)

              checkPageIsDisplayed(
                performAction(),
                messageFromMessageKey(
                  s"$key$userKey.title"
                ),
                { doc =>
                  doc
                    .select("#previousYearsLosses-0")
                    .attr("checked")                      shouldBe "checked"
                  doc.select(s"#$valueKey").attr("value") shouldBe "10"
                }
              )
          }
        }

      }

    }

    "handling submitted in previous years losses page" must {

      val key      = "previousYearsLosses"
      val valueKey = "previousYearsLossesValue"

      def performAction(data: (String, String)*): Future[Result] =
        controller.previousYearsLossesSubmit()(
          FakeRequest().withFormUrlEncodedBody(data: _*)
        )

      behave like redirectToStartBehaviour(() => performAction())

      "redirect to the in year losses page" when {

        "that question has not been answered" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithMultipleDisposalsState(
                    sample[IncompleteExemptionAndLossesAnswers]
                      .copy(inYearLosses = None),
                    sample[DisposalDate],
                    userType,
                    Some(individualUserType)
                  )._1
                )
              }

              checkIsRedirect(
                performAction(),
                routes.ExemptionAndLossesController.inYearLosses()
              )
          }
        }

      }

      "show a form error" when {

        def test(data: (String, String)*)(
          expectedErrorKey: String
        )(
          userType: UserType,
          individualUserType: IndividualUserType,
          userKey: String
        ): Unit = {
          val session: SessionData = sessionWithSingleDisposalState(
            sample[CompleteExemptionAndLossesAnswers],
            sample[DisposalDate],
            userType,
            Some(individualUserType)
          )._1

          testFormError(data: _*)(key, expectedErrorKey)(s"$key$userKey.title")(
            performAction,
            session
          )
        }

        "no option has been selected" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val userKey = userMessageKey(individualUserType, userType)
              test()(s"$key$userKey.error.required")(
                userType,
                individualUserType,
                userKey
              )
          }
        }

        "the option selected is not valid" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val userKey = userMessageKey(individualUserType, userType)
              test(key -> "2")(s"$key$userKey.error.invalid")(
                userType,
                individualUserType,
                userKey
              )
          }
        }

        "the amount of money is invalid" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              amountOfMoneyErrorScenarios(valueKey).foreach { scenario =>
                withClue(s"For $scenario: ") {
                  val data    = (key -> "0") :: scenario.formData
                  val userKey = userMessageKey(individualUserType, userType)
                  test(data: _*)(scenario.expectedErrorMessageKey)(
                    userType,
                    individualUserType,
                    userKey
                  )
                }
              }
          }
        }

      }

      "show an error page" when {

        def getSessionJourneyAndDraftReturn(
          userType: UserType,
          newAmount: AmountInPence,
          individualUserType: IndividualUserType
        ): (SessionData, FillingOutReturn, DraftReturn) = {
          val answers: CompleteExemptionAndLossesAnswers =
            sample[CompleteExemptionAndLossesAnswers].copy(
              previousYearsLosses = AmountInPence(newAmount.value + 1L)
            )

          val (session, journey, draftReturn) = sessionWithSingleIndirectDisposalsState(
            answers,
            sample[DisposalDate],
            userType,
            Some(individualUserType)
          )

          val updatedDraftReturn = draftReturn.copy(
            exemptionAndLossesAnswers = Some(answers.copy(previousYearsLosses = newAmount)),
            yearToDateLiabilityAnswers = draftReturn.yearToDateLiabilityAnswers
              .flatMap(_.unsetAllButIncomeDetails())
          )

          (session, journey, updatedDraftReturn)
        }

        "there is an error updating the draft return" in {

          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val newAmount                              = AmountInPence(123L)
              val (session, journey, updatedDraftReturn) =
                getSessionJourneyAndDraftReturn(
                  userType,
                  newAmount,
                  individualUserType
                )

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
                  key      -> "0",
                  valueKey -> newAmount.inPounds().toString
                )
              )
          }

        }

        "there is an error updating the session" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val newAmount                              = AmountInPence(123L)
              val (session, journey, updatedDraftReturn) =
                getSessionJourneyAndDraftReturn(
                  userType,
                  newAmount,
                  individualUserType
                )

              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
                mockStoreDraftReturn(
                  updatedDraftReturn,
                  journey.subscribedDetails.cgtReference,
                  journey.agentReferenceNumber
                )(Right(()))
                mockStoreSession(
                  session.copy(
                    journeyStatus = Some(journey.copy(draftReturn = updatedDraftReturn))
                  )
                )(Left(Error("")))
              }

              checkIsTechnicalErrorPage(
                performAction(
                  key      -> "0",
                  valueKey -> newAmount.inPounds().toString
                )
              )
          }
        }

      }

      "redirect to the task list page" when {

        "all updates are successful and" when {

          "the user selects no and the journey was incomplete" in {
            val newAmount = AmountInPence(3000L)
            val answers   = sample[IncompleteExemptionAndLossesAnswers].copy(
              inYearLosses = Some(sample[AmountInPence]),
              previousYearsLosses = None
            )
            forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
              (userType: UserType, individualUserType: IndividualUserType) =>
                testSuccessfulUpdatesAfterSubmit(
                  performAction(
                    key      -> "0",
                    valueKey -> newAmount.inPounds().toString
                  )
                )(answers, answers.copy(previousYearsLosses = Some(newAmount)))(
                  userType,
                  individualUserType
                )
            }
          }

          "the user selects no and the journey was complete" in {
            val newAmount = AmountInPence(4000L)
            val answers   =
              sample[CompleteExemptionAndLossesAnswers].copy(
                previousYearsLosses = AmountInPence(newAmount.value + 1)
              )
            forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
              (userType: UserType, individualUserType: IndividualUserType) =>
                testSuccessfulUpdatesAfterSubmit(
                  performAction(
                    key      -> "0",
                    valueKey -> newAmount.inPounds().toString
                  )
                )(answers, answers.copy(previousYearsLosses = newAmount))(
                  userType,
                  individualUserType
                )
            }
          }

          "the user selects yes and submits a valid value and the journey was incomplete" in {
            val answers = sample[IncompleteExemptionAndLossesAnswers].copy(
              inYearLosses = Some(AmountInPence(1L)),
              previousYearsLosses = None
            )

            forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
              (userType: UserType, individualUserType: IndividualUserType) =>
                testSuccessfulUpdatesAfterSubmit(
                  performAction(key -> "1")
                )(
                  answers,
                  answers.copy(previousYearsLosses = Some(AmountInPence.zero))
                )(userType, individualUserType)
            }
          }

          "the user selects yes and submits a valid value and the journey was complete" in {
            val answers = sample[CompleteExemptionAndLossesAnswers].copy(
              previousYearsLosses = AmountInPence(1L)
            )

            forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
              (userType: UserType, individualUserType: IndividualUserType) =>
                testSuccessfulUpdatesAfterSubmit(
                  performAction(key -> "1")
                )(
                  answers,
                  answers.copy(previousYearsLosses = AmountInPence.zero)
                )(userType, individualUserType)
            }

          }

        }

      }

      "not do any updates" when {

        "the value submitted hasn't changed" in {
          val answers = sample[CompleteExemptionAndLossesAnswers].copy(
            previousYearsLosses = AmountInPence(1L)
          )

          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val session =
                sessionWithMultipleDisposalsState(
                  answers,
                  sample[DisposalDate],
                  userType,
                  Some(individualUserType)
                )._1

              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
              }

              checkIsRedirect(
                performAction(key -> "0", valueKey -> "0.01"),
                routes.ExemptionAndLossesController.checkYourAnswers()
              )
          }

        }

      }

    }

    "handling requests to display the annual exempt amount page" must {

      val key = "annualExemptAmount"

      def performAction(): Future[Result] =
        controller.annualExemptAmount()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      "redirect to the task list page" when {

        "there is no disposal date" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithMultipleDisposalsState(
                    Some(sample[CompleteExemptionAndLossesAnswers]),
                    None,
                    userType,
                    Some(individualUserType)
                  )._1
                )
              }

              checkIsRedirect(
                performAction(),
                returns.routes.TaskListController.taskList()
              )
          }
        }

      }

      "redirect to previous years losses page" when {

        "that question has not been answered yet" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithSingleDisposalState(
                    sample[IncompleteExemptionAndLossesAnswers].copy(
                      previousYearsLosses = None
                    ),
                    sample[DisposalDate],
                    userType,
                    Some(individualUserType)
                  )._1
                )
              }

              checkIsRedirect(
                performAction(),
                routes.ExemptionAndLossesController.previousYearsLosses()
              )
          }
        }

      }

      "display the page" when {

        "the exemption and losses section has not yet been completed" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithMultipleDisposalsState(
                    sample[IncompleteExemptionAndLossesAnswers].copy(
                      previousYearsLosses = Some(sample[AmountInPence]),
                      annualExemptAmount = None
                    ),
                    sample[DisposalDate],
                    userType,
                    Some(individualUserType)
                  )._1
                )
              }

              val userKey = userMessageKey(individualUserType, userType)

              checkPageIsDisplayed(
                performAction(),
                messageFromMessageKey(s"$key$userKey.title"),
                { doc =>
                  doc
                    .select("#back")
                    .attr("href")   shouldBe routes.ExemptionAndLossesController
                    .previousYearsLosses()
                    .url
                  doc
                    .select("#content > article > form")
                    .attr("action") shouldBe routes.ExemptionAndLossesController
                    .annualExemptAmountSubmit()
                    .url
                }
              )
          }
        }

        "the exemption and losses section has been completed" in {
          val amount = AmountInPence(1000L)
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithSingleIndirectDisposalsState(
                    sample[CompleteExemptionAndLossesAnswers].copy(
                      annualExemptAmount = amount
                    ),
                    sample[DisposalDate],
                    userType,
                    Some(individualUserType)
                  )._1
                )
              }

              val userKey = userMessageKey(individualUserType, userType)

              checkPageIsDisplayed(
                performAction(),
                messageFromMessageKey(s"$key$userKey.title"),
                { doc =>
                  doc
                    .select("#back")
                    .attr("href")                    shouldBe routes.ExemptionAndLossesController
                    .checkYourAnswers()
                    .url
                  doc
                    .select("#content > article > form")
                    .attr("action")                  shouldBe routes.ExemptionAndLossesController
                    .annualExemptAmountSubmit()
                    .url
                  doc.select(s"#$key").attr("value") shouldBe "10"

                }
              )
          }
        }

        "the user type is an individual or agent" in {
          val amount            = AmountInPence(1000L)
          val acceptedUserTypes =
            List[UserType](UserType.Individual, UserType.Agent)

          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              whenever(acceptedUserTypes.contains(userType)) {
                inSequence {
                  mockAuthWithNoRetrievals()
                  mockGetSession(
                    sessionWithSingleMixedUseDisposalsState(
                      sample[CompleteExemptionAndLossesAnswers]
                        .copy(annualExemptAmount = amount),
                      sample[DisposalDate],
                      userType,
                      Some(individualUserType)
                    )._1
                  )
                }

                val userKey = userMessageKey(individualUserType, userType)

                checkPageIsDisplayed(
                  performAction(),
                  messageFromMessageKey(s"$key$userKey.title"),
                  { doc =>
                    doc
                      .select(s"#$key-form-hint")
                      .text() contains messageFromMessageKey(
                      s"$key$userKey.helpText"
                    )
                    doc
                      .select("#content > article > form > p > a")
                      .text() contains messageFromMessageKey(
                      s"$key.link"
                    )
                  }
                )
              }
          }
        }

        "the user type is a trust" in {
          val amount = AmountInPence(1000L)
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithSingleDisposalState(
                sample[CompleteExemptionAndLossesAnswers]
                  .copy(annualExemptAmount = amount),
                sample[DisposalDate],
                UserType.Organisation,
                Some(IndividualUserType.Self)
              )._1
            )
          }
          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(
              "annualExemptAmount.trust.title"
            ),
            { doc =>
              doc
                .select("#content > article > form > p > a")
                .text() contains messageFromMessageKey(
                "annualExemptAmount.trust.link"
              )
              doc
                .select("#annualExemptAmount-form-hint")
                .text() contains messageFromMessageKey(
                "annualExemptAmount.trust.helpText"
              )
              doc.select(
                "#content > article > form > details:nth-child(3) > summary > span"
              ) contains messageFromMessageKey(
                "annualExemptAmount.trust.details.1.header"
              )
              doc.select("#details-content-0") contains messageFromMessageKey(
                "annualExemptAmount.trust.details.1.body"
              )
              doc.select(
                "#content > article > form > details:nth-child(4) > summary > span"
              ) contains messageFromMessageKey(
                "annualExemptAmount.trust.details.2.header"
              )
              doc.select("#details-content-1") contains messageFromMessageKey(
                "annualExemptAmount.trust.details.2.body"
              )
            }
          )
        }

      }

    }

    "handling submitted annual exempt amounts page" must {

      val key = "annualExemptAmount"

      def performAction(data: (String, String)*): Future[Result] =
        controller.annualExemptAmountSubmit()(
          FakeRequest().withFormUrlEncodedBody(data: _*)
        )

      val maximumAnnualExemptAmount = AmountInPence(10000L)

      val taxYear = sample[TaxYear].copy(
        annualExemptAmountGeneral = maximumAnnualExemptAmount
      )

      val disposalDate = sample[DisposalDate].copy(taxYear = taxYear)

      behave like redirectToStartBehaviour(() => performAction())

      "redirect to the task list page" when {

        "there is no disposal date" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithSingleDisposalsState(
                    Some(sample[CompleteExemptionAndLossesAnswers]),
                    None,
                    userType,
                    Some(individualUserType)
                  )._1
                )
              }

              checkIsRedirect(
                performAction(),
                returns.routes.TaskListController.taskList()
              )
          }
        }

      }

      "redirect to previous years losses page" when {

        "that question has not been answered yet" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithMultipleDisposalsState(
                    sample[IncompleteExemptionAndLossesAnswers].copy(
                      previousYearsLosses = None
                    ),
                    sample[DisposalDate],
                    userType,
                    Some(individualUserType)
                  )._1
                )
              }

              checkIsRedirect(
                performAction(),
                routes.ExemptionAndLossesController.previousYearsLosses()
              )
          }
        }

      }

      "show a form error" when {

        def test(data: (String, String)*)(
          expectedErrorKey: String
        )(
          userType: UserType,
          individualUserType: IndividualUserType,
          userKey: String
        ): Unit = {

          val session = sessionWithSingleDisposalState(
            sample[CompleteExemptionAndLossesAnswers],
            disposalDate,
            userType,
            Some(individualUserType)
          )._1

          testFormError(data: _*)(
            key,
            expectedErrorKey,
            MoneyUtils.formatAmountOfMoneyWithoutPoundSign(
              disposalDate.taxYear.annualExemptAmountGeneral.inPounds()
            )
          )(s"$key$userKey.title")(performAction, session)
        }

        "the amount of money is invalid" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val userKey = userMessageKey(individualUserType, userType)
              amountOfMoneyErrorScenarios(
                key = key,
                errorContext = Some(s"$key$userKey")
              ).foreach { scenario =>
                withClue(s"For $scenario: ") {
                  test(scenario.formData: _*)(scenario.expectedErrorMessageKey)(
                    userType,
                    individualUserType,
                    userKey
                  )
                }
              }
          }
        }

      }

      "show an error page" when {

        def getSessionJourneyAndDraftReturn(
          userType: UserType,
          newAmount: AmountInPence,
          individualUserType: IndividualUserType
        ): (SessionData, FillingOutReturn, DraftReturn) = {
          val answers: CompleteExemptionAndLossesAnswers =
            sample[CompleteExemptionAndLossesAnswers].copy(
              annualExemptAmount = AmountInPence(newAmount.value + 1L)
            )
          val (session, journey, draftReturn)            =
            sessionWithMultipleDisposalsState(
              answers,
              disposalDate,
              userType,
              Some(individualUserType)
            )

          val updatedDraftReturn = draftReturn.copy(
            exemptionAndLossesAnswers = Some(
              answers.copy(annualExemptAmount = newAmount)
            ),
            yearToDateLiabilityAnswers = draftReturn.yearToDateLiabilityAnswers
              .flatMap(_.unsetAllButIncomeDetails())
          )

          (session, journey, updatedDraftReturn)
        }

        "there is an error updating the draft return" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val newAmount                              = AmountInPence(123L)
              val (session, journey, updatedDraftReturn) =
                getSessionJourneyAndDraftReturn(
                  userType,
                  newAmount,
                  individualUserType
                )

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
                performAction(key -> newAmount.inPounds().toString)
              )
          }
        }

        "there is an error updating the session" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val newAmount                              = AmountInPence(123L)
              val (session, journey, updatedDraftReturn) =
                getSessionJourneyAndDraftReturn(
                  userType,
                  newAmount,
                  individualUserType
                )

              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
                mockStoreDraftReturn(
                  updatedDraftReturn,
                  journey.subscribedDetails.cgtReference,
                  journey.agentReferenceNumber
                )(Right(()))
                mockStoreSession(
                  session.copy(
                    journeyStatus = Some(journey.copy(draftReturn = updatedDraftReturn))
                  )
                )(Left(Error("")))
              }

              checkIsTechnicalErrorPage(
                performAction(key -> newAmount.inPounds().toString)
              )
          }
        }

      }

      "redirect to the task list page" when {

        "all updates are successful and" when {

          "the journey was incomplete" in {
            val newAmount = AmountInPence(3000L)
            val answers   = sample[IncompleteExemptionAndLossesAnswers].copy(
              previousYearsLosses = Some(sample[AmountInPence]),
              annualExemptAmount = None
            )

            forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
              (userType: UserType, individualUserType: IndividualUserType) =>
                testSuccessfulUpdatesAfterSubmit(
                  performAction(key -> newAmount.inPounds().toString)
                )(
                  answers,
                  answers.copy(annualExemptAmount = Some(newAmount)),
                  disposalDate = disposalDate
                )(userType, individualUserType)
            }
          }

          "the journey was complete" in {
            val newAmount = AmountInPence(6000L)
            val answers   = sample[CompleteExemptionAndLossesAnswers].copy(
              annualExemptAmount = AmountInPence(newAmount.value + 1)
            )

            forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
              (userType: UserType, individualUserType: IndividualUserType) =>
                testSuccessfulUpdatesAfterSubmit(
                  performAction(key -> newAmount.inPounds().toString)
                )(
                  answers,
                  answers.copy(annualExemptAmount = newAmount),
                  disposalDate = disposalDate
                )(userType, individualUserType)
            }
          }

        }

      }

      "not do any updates" when {

        "the value submitted hasn't changed" in {
          val answers = sample[CompleteExemptionAndLossesAnswers].copy(
            annualExemptAmount = AmountInPence(1L)
          )

          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val session =
                sessionWithMultipleDisposalsState(
                  answers,
                  disposalDate,
                  userType,
                  Some(individualUserType)
                )._1

              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
              }

              checkIsRedirect(
                performAction(key -> "0.01"),
                routes.ExemptionAndLossesController.checkYourAnswers()
              )
          }

        }

      }

    }

    "handling requests to display the cya page" must {

      def performAction(): Future[Result] =
        controller.checkYourAnswers()(FakeRequest())

      val completeAnswers = sample[CompleteExemptionAndLossesAnswers]

      val allQuestionsAnswered = IncompleteExemptionAndLossesAnswers(
        Some(completeAnswers.inYearLosses),
        Some(completeAnswers.previousYearsLosses),
        Some(completeAnswers.annualExemptAmount)
      )

      def getSessionJourneyAndDraftReturn(
        userType: UserType,
        individualUserType: IndividualUserType
      ): (SessionData, SessionData, FillingOutReturn, DraftReturn) = {
        val (session, journey, draftReturn) = sessionWithSingleDisposalState(
          allQuestionsAnswered,
          sample[DisposalDate],
          userType,
          Some(individualUserType)
        )
        val updatedDraftReturn              = draftReturn.copy(
          exemptionAndLossesAnswers = Some(completeAnswers)
        )
        val updatedSession                  = session.copy(
          journeyStatus = Some(journey.copy(draftReturn = updatedDraftReturn))
        )

        (session, updatedSession, journey, updatedDraftReturn)
      }

      behave like redirectToStartBehaviour(performAction)

      def testIsRedirectWhenMissingAnswer(
        answers: IncompleteExemptionAndLossesAnswers,
        expectedRedirect: Call
      )(userType: UserType, individualUserType: IndividualUserType): Unit =
        List(
          sessionWithSingleDisposalState(
            answers,
            sample[DisposalDate],
            userType,
            Some(individualUserType)
          )._1,
          sessionWithMultipleDisposalsState(
            answers,
            sample[DisposalDate],
            userType,
            Some(individualUserType)
          )._1
        ).foreach { session =>
          withClue(s"For session $session: ") {

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionWithSingleDisposalState(
                  answers,
                  sample[DisposalDate],
                  userType,
                  Some(individualUserType)
                )._1
              )
            }

            checkIsRedirect(performAction(), expectedRedirect)
          }
        }

      "redirect to the in year losses page" when {

        "that question has not been answered" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              testIsRedirectWhenMissingAnswer(
                allQuestionsAnswered.copy(inYearLosses = None),
                routes.ExemptionAndLossesController.inYearLosses()
              )(userType, individualUserType)
          }
        }

      }

      "redirect to the previous years losses page" when {

        "that question has not been answered" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              testIsRedirectWhenMissingAnswer(
                allQuestionsAnswered.copy(previousYearsLosses = None),
                routes.ExemptionAndLossesController.previousYearsLosses()
              )(userType, individualUserType)
          }
        }

      }

      "redirect to the annual exempt amount page" when {

        "that question has not been answered" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              testIsRedirectWhenMissingAnswer(
                allQuestionsAnswered.copy(annualExemptAmount = None),
                routes.ExemptionAndLossesController.annualExemptAmount()
              )(userType, individualUserType)
          }
        }

      }

      "show an error page" when {

        "the user has just answered all the questions and" when {

          "there is an error updating the draft return" in {
            forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
              (userType: UserType, individualUserType: IndividualUserType) =>
                val (session, _, journey, updatedDraftReturn) =
                  getSessionJourneyAndDraftReturn(userType, individualUserType)

                inSequence {
                  mockAuthWithNoRetrievals()
                  mockGetSession(session)
                  mockStoreDraftReturn(
                    updatedDraftReturn,
                    journey.subscribedDetails.cgtReference,
                    journey.agentReferenceNumber
                  )(Left(Error("")))
                }

                checkIsTechnicalErrorPage(performAction())
            }
          }

          "there is an error updating the session" in {
            forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
              (userType: UserType, individualUserType: IndividualUserType) =>
                val (session, updatedSession, journey, updatedDraftReturn) =
                  getSessionJourneyAndDraftReturn(userType, individualUserType)

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

                checkIsTechnicalErrorPage(performAction())
            }
          }

        }

      }

      "display the page" when {

        "display the page" when {

          "the user has already answered all the questions" in {
            forAll { (completeAnswers: CompleteExemptionAndLossesAnswers) =>
              forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
                (userType: UserType, individualUserType: IndividualUserType) =>
                  val (session, journey, draftReturn) =
                    sessionWithSingleDisposalState(
                      allQuestionsAnswered,
                      sample[DisposalDate],
                      userType,
                      Some(individualUserType)
                    )

                  val updatedDraftReturn = draftReturn
                    .copy(exemptionAndLossesAnswers = Some(completeAnswers))
                  val updatedSession     =
                    session.copy(journeyStatus = Some(journey.copy(draftReturn = updatedDraftReturn)))

                  inSequence {
                    mockAuthWithNoRetrievals()
                    mockGetSession(updatedSession)
                  }

                  val isAnAgent = userType === UserType.Agent

                  checkPageIsDisplayed(
                    performAction(),
                    messageFromMessageKey("exemptionsAndLosses.cya.title"),
                    { doc =>
                      validateExemptionAndLossesCheckYourAnswersPage(
                        completeAnswers,
                        doc,
                        journey.subscribedDetails.isATrust,
                        isAnAgent,
                        individualUserType
                      )
                      doc
                        .select("#content > article > form")
                        .attr(
                          "action"
                        ) shouldBe routes.ExemptionAndLossesController
                        .checkYourAnswersSubmit()
                        .url
                    }
                  )
              }
            }
          }

          "the user has just answered all the questions and all updates are successful" in {
            forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
              (userType: UserType, individualUserType: IndividualUserType) =>
                val (session, updatedSession, journey, updatedDraftReturn) =
                  getSessionJourneyAndDraftReturn(userType, individualUserType)
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

                val isAnAgent = userType === UserType.Agent

                checkPageIsDisplayed(
                  performAction(),
                  messageFromMessageKey("exemptionsAndLosses.cya.title"),
                  { doc =>
                    validateExemptionAndLossesCheckYourAnswersPage(
                      completeAnswers,
                      doc,
                      journey.subscribedDetails.isATrust,
                      isAnAgent,
                      individualUserType
                    )
                    doc
                      .select("#content > article > form")
                      .attr(
                        "action"
                      ) shouldBe routes.ExemptionAndLossesController
                      .checkYourAnswersSubmit()
                      .url
                  }
                )
            }
          }

          "the user wishes to use in year losses" in {
            forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
              (userType: UserType, individualUserType: IndividualUserType) =>
                val (_, updatedSession, _, _) =
                  getSessionJourneyAndDraftReturn(userType, individualUserType)
                inSequence {
                  mockAuthWithNoRetrievals()
                  mockGetSession(updatedSession)
                }

                checkPageIsDisplayed(
                  performAction(),
                  messageFromMessageKey("exemptionsAndLosses.cya.title"),
                  doc =>
                    doc
                      .select("#content > article > form")
                      .attr(
                        "action"
                      ) shouldBe routes.ExemptionAndLossesController
                      .checkYourAnswersSubmit()
                      .url
                )
            }
          }

        }

      }

    }

    "handling submits from the cya page" must {

      def performAction(): Future[Result] =
        controller.checkYourAnswersSubmit()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      "redirect to the task list page" in {
        forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
          (userType: UserType, individualUserType: IndividualUserType) =>
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionWithSingleDisposalState(
                  sample[CompleteExemptionAndLossesAnswers],
                  sample[DisposalDate],
                  userType,
                  Some(individualUserType)
                )._1
              )
            }

            checkIsRedirect(
              performAction(),
              returns.routes.TaskListController.taskList()
            )
        }
      }

    }

  }

  def testFormError(
    data: (String, String)*
  )(key: String, expectedErrorMessageKey: String, errorArgs: String*)(
    pageTitleKey: String,
    titleArgs: String*
  )(
    performAction: Seq[(String, String)] => Future[Result],
    currentSession: SessionData
  ): Unit = {

    inSequence {
      mockAuthWithNoRetrievals()
      mockGetSession(currentSession)
    }

    checkPageIsDisplayed(
      performAction(data),
      messageFromMessageKey(pageTitleKey, titleArgs: _*),
      { doc =>
        doc
          .select("#error-summary-display > ul > li > a")
          .text() shouldBe messageFromMessageKey(
          expectedErrorMessageKey,
          errorArgs: _*
        )

        doc.title() should startWith("Error:")

        val errorSummary   = doc.select(s"""a[href="#$key"]""").text()
        val inlineErrorMsg =
          doc.select(s"""span[id="$key-inline-error"]""").text()

        val errorSummaryMsg =
          if (errorSummary.nonEmpty) "Error: " + errorSummary else errorSummary

        errorSummaryMsg shouldEqual inlineErrorMsg
      },
      BAD_REQUEST
    )
  }

  def testSuccessfulUpdatesAfterSubmit(result: => Future[Result])(
    oldAnswers: ExemptionAndLossesAnswers,
    newAnswers: ExemptionAndLossesAnswers,
    disposalDate: DisposalDate = sample[DisposalDate]
  )(userType: UserType, individualUserType: IndividualUserType): Unit =
    List(
      sessionWithSingleDisposalState(
        oldAnswers,
        disposalDate,
        userType,
        Some(individualUserType)
      ),
      sessionWithMultipleDisposalsState(
        oldAnswers,
        disposalDate,
        userType,
        Some(individualUserType)
      )
    ).foreach {
      case (session, journey, draftReturn) =>
        withClue(s"For initial session $session: ") {
          val updatedDraftReturn = draftReturn.fold(
            multiple =>
              multiple.copy(
                exemptionAndLossesAnswers = Some(newAnswers),
                yearToDateLiabilityAnswers = multiple.yearToDateLiabilityAnswers
                  .flatMap(_.unsetAllButIncomeDetails())
              ),
            single =>
              single.copy(
                exemptionAndLossesAnswers = Some(newAnswers),
                yearToDateLiabilityAnswers = single.yearToDateLiabilityAnswers
                  .flatMap(_.unsetAllButIncomeDetails())
              ),
            singleIndirect =>
              singleIndirect.copy(
                exemptionAndLossesAnswers = Some(newAnswers),
                yearToDateLiabilityAnswers = singleIndirect.yearToDateLiabilityAnswers
                  .flatMap(_.unsetAllButIncomeDetails())
              ),
            multipleIndirect =>
              multipleIndirect.copy(
                exemptionAndLossesAnswers = Some(newAnswers),
                yearToDateLiabilityAnswers = multipleIndirect.yearToDateLiabilityAnswers
                  .flatMap(_.unsetAllButIncomeDetails())
              ),
            singleMixedUse =>
              singleMixedUse.copy(
                exemptionAndLossesAnswers = Some(newAnswers),
                yearToDateLiabilityAnswers = singleMixedUse.yearToDateLiabilityAnswers
                  .flatMap(_.unsetAllButIncomeDetails())
              )
          )
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
              session.copy(
                journeyStatus = Some(journey.copy(draftReturn = updatedDraftReturn))
              )
            )(Right(()))
          }

          checkIsRedirect(
            result,
            routes.ExemptionAndLossesController.checkYourAnswers()
          )
        }
    }

}

object ExemptionAndLossesControllerSpec extends Matchers {

  def validateExemptionAndLossesCheckYourAnswersPage(
    completeExemptionAndLossesAnswers: CompleteExemptionAndLossesAnswers,
    doc: Document,
    isATrust: Boolean,
    isAnAgent: Boolean,
    individualUserType: IndividualUserType
  ): Unit = {

    if (completeExemptionAndLossesAnswers.inYearLosses.isZero)
      doc.select("#inYearLosses-answer").text shouldBe "No"
    else {
      doc.select("#inYearLosses-answer").text shouldBe "Yes"
      doc
        .select("#inYearLossesValue-answer")
        .text                                 shouldBe formatAmountOfMoneyWithPoundSign(
        completeExemptionAndLossesAnswers.inYearLosses.inPounds()
      )
    }

    if (completeExemptionAndLossesAnswers.previousYearsLosses.isZero)
      doc.select("#previousYearsLosses-answer").text shouldBe "No"
    else {
      doc.select("#previousYearsLosses-answer").text shouldBe "Yes"
      doc
        .select("#previousYearsLossesValue-answer")
        .text                                        shouldBe formatAmountOfMoneyWithPoundSign(
        completeExemptionAndLossesAnswers.previousYearsLosses.inPounds()
      )
    }

    if (individualUserType === IndividualUserType.PersonalRepresentative)
      doc
        .select("#annualExemptAmount-question")
        .text() shouldBe "How much of the person’s Capital Gains Tax Annual Exempt Amount do they want to use?"
    else if (individualUserType === IndividualUserType.PersonalRepresentativeInPeriodOfAdmin)
      doc
        .select("#annualExemptAmount-question")
        .text() shouldBe "How much of the Annual Exempt Amount are you including?"
    else if (individualUserType === IndividualUserType.Capacitor)
      doc
        .select("#annualExemptAmount-question")
        .text() shouldBe "How much of the person’s Capital Gains Tax Annual Exempt Amount do they want to use?"
    else if (individualUserType === PersonalRepresentativeInPeriodOfAdmin)
      doc
        .select("#annualExemptAmount-question")
        .text() shouldBe "How much of the Annual Exempt Amount are you including?"
    else if (isATrust)
      doc
        .select("#annualExemptAmount-question")
        .text() shouldBe "How much of the trust’s Capital Gains Tax Annual Exempt Amount does it want to use?"
    else if (isAnAgent)
      doc
        .select("#annualExemptAmount-question")
        .text() shouldBe "How much of your client’s Capital Gains Tax Annual Exempt Amount do they want to use?"
    else
      doc
        .select("#annualExemptAmount-question")
        .text() shouldBe "How much of your Capital Gains Tax Annual Exempt Amount do you want to use?"

    doc
      .select("#annualExemptAmount-answer")
      .text     shouldBe formatAmountOfMoneyWithPoundSign(
      completeExemptionAndLossesAnswers.annualExemptAmount.inPounds()
    )
  }

}
