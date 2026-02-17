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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.address

import cats.Eq
import uk.gov.hmrc.cgtpropertydisposalsfrontend.SampledScalaCheck
import play.api.i18n.{Messages, MessagesApi, MessagesImpl}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.{Call, Result}
import play.api.test.FakeRequest
import play.api.test.Helpers.BAD_REQUEST
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.AmountOfMoneyErrorScenarios.amountOfMoneyErrorScenarios
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.{ReturnsServiceSupport, StartingToAmendToFillingOutReturnSpecBehaviour}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, PreviousReturnData, StartingToAmendReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.UserType.{Agent, Individual, Organisation}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address.UkAddress
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Postcode
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.AmountInPence
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.AddressGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.DraftReturnGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.IdGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.JourneyStatusGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.MoneyGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.NameGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.RepresenteeAnswersGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.ReturnGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.SingleMixedUseDetailsAnswersGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.SubscribedDetailsGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.TriageQuestionsGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.{AgentReferenceNumber, UUIDGenerator}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.{IndividualName, TrustName}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.SubscribedDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.IndividualUserType.{Capacitor, PersonalRepresentative, PersonalRepresentativeInPeriodOfAdmin, Self}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.MixedUsePropertyDetailsAnswers.{CompleteMixedUsePropertyDetailsAnswers, IncompleteMixedUsePropertyDetailsAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.RepresenteeAnswers.CompleteRepresenteeAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.CompleteSingleDisposalTriageAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, SessionData, TimeUtils, UserType}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService

import java.time.LocalDate
import scala.concurrent.Future

class MixedUsePropertyDetailsControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with ReturnsServiceSupport
    with SampledScalaCheck
    with RedirectToStartBehaviour
    with StartingToAmendToFillingOutReturnSpecBehaviour {

  val mockUUIDGenerator: UUIDGenerator = mock[UUIDGenerator]

  private def redirectToStartBehaviour(performAction: () => Future[Result]): Unit =
    redirectToStartWhenInvalidJourney(
      performAction,
      {
        case FillingOutReturn(_, _, _, _: DraftSingleMixedUseDisposalReturn, _, _) => true
        case _: StartingToAmendReturn                                              => true
        case _                                                                     => false
      }
    )

  protected override val overrideBindings: List[GuiceableModule] = List[GuiceableModule](
    bind[AuthConnector].toInstance(mockAuthConnector),
    bind[SessionStore].toInstance(mockSessionStore),
    bind[ReturnsService].toInstance(mockReturnsService),
    bind[UUIDGenerator].toInstance(mockUUIDGenerator)
  )

  private lazy val controller = instanceOf[MixedUsePropertyDetailsController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  implicit val messages: Messages = MessagesImpl(lang, messagesApi)

  def sessionWithDraftMixedUseDisposal(
    name: Either[TrustName, IndividualName],
    userType: UserType,
    individualUserType: Option[IndividualUserType],
    mixedUsePropertyDetailsAnswers: Option[MixedUsePropertyDetailsAnswers],
    previousReturnData: Option[PreviousReturnData] = None,
    amendReturnData: Option[AmendReturnData] = None
  ): (SessionData, FillingOutReturn, DraftSingleMixedUseDisposalReturn) = {
    val triageAnswers = sample[CompleteSingleDisposalTriageAnswers].copy(
      individualUserType = individualUserType
    )

    val taxYearStartYear: String =
      triageAnswers
        .fold(
          _.disposalDate.map(_.taxYear.startDateInclusive.getYear),
          c => Some(c.disposalDate.taxYear.startDateInclusive.getYear)
        )
        .map(_.toString)
        .getOrElse("2020")

    val updatedPreviousReturnData = previousReturnData match {
      case Some(PreviousReturnData(summaries, l, f, c)) =>
        val updatedSummaries = summaries.map(_.copy(taxYear = taxYearStartYear))
        Some(PreviousReturnData(updatedSummaries, l, f, c))
      case _                                            => previousReturnData
    }

    val draftReturn      = sample[DraftSingleMixedUseDisposalReturn].copy(
      triageAnswers = triageAnswers,
      mixedUsePropertyDetailsAnswers = mixedUsePropertyDetailsAnswers,
      representeeAnswers = individualUserType match {
        case Some(PersonalRepresentative | PersonalRepresentativeInPeriodOfAdmin) =>
          Some(sample[CompleteRepresenteeAnswers].copy(dateOfDeath = Some(sample[DateOfDeath])))
        case Some(Capacitor)                                                      =>
          Some(sample[CompleteRepresenteeAnswers].copy(dateOfDeath = None))
        case _                                                                    =>
          None
      }
    )
    val fillingOutReturn = sample[FillingOutReturn].copy(
      draftReturn = draftReturn,
      subscribedDetails = sample[SubscribedDetails].copy(name = name),
      agentReferenceNumber = if (Eq.eqv(userType, Agent)) Some(sample[AgentReferenceNumber]) else None,
      previousSentReturns = updatedPreviousReturnData,
      amendReturnData = amendReturnData
    )
    val sessionData      = SessionData.empty.copy(
      journeyStatus = Some(fillingOutReturn),
      userType = Some(userType)
    )
    (sessionData, fillingOutReturn, draftReturn)
  }

  def individualState(
    address: Option[UkAddress] = None,
    previousReturnData: Option[PreviousReturnData] = None,
    amendReturnData: Option[AmendReturnData] = None
  ): (SessionData, FillingOutReturn, DraftSingleMixedUseDisposalReturn) =
    sessionWithDraftMixedUseDisposal(
      Right(sample[IndividualName]),
      UserType.Individual,
      Some(Self),
      Some(IncompleteMixedUsePropertyDetailsAnswers(address, None, None)),
      previousReturnData,
      amendReturnData
    )

  def capacitorState(
    answers: Option[MixedUsePropertyDetailsAnswers] = None
  ): (SessionData, FillingOutReturn, DraftSingleMixedUseDisposalReturn) =
    sessionWithDraftMixedUseDisposal(
      Right(sample[IndividualName]),
      UserType.Individual,
      Some(Capacitor),
      answers
    )

  def personalRepState(): (SessionData, FillingOutReturn, DraftSingleMixedUseDisposalReturn) =
    sessionWithDraftMixedUseDisposal(
      Right(sample[IndividualName]),
      UserType.Individual,
      Some(PersonalRepresentative),
      None
    )

  def agentState(): (SessionData, FillingOutReturn, DraftSingleMixedUseDisposalReturn) =
    sessionWithDraftMixedUseDisposal(
      Right(sample[IndividualName]),
      UserType.Agent,
      Some(Self),
      None
    )

  def trustState(): (SessionData, FillingOutReturn, DraftSingleMixedUseDisposalReturn) =
    sessionWithDraftMixedUseDisposal(
      Left(sample[TrustName]),
      UserType.Organisation,
      None,
      None
    )

  def deriveUserKey(isAgent: Boolean, isATrust: Boolean): String =
    if (isAgent) ".agent" else if (isATrust) ".trust" else ""

  def userMessageKey(
    individualUserType: Option[IndividualUserType],
    userType: UserType
  ): String =
    (individualUserType, userType) match {
      case (Some(Capacitor), _)                                          => ".capacitor"
      case (Some(PersonalRepresentative), _)                             => ".personalRep"
      case (Some(PersonalRepresentativeInPeriodOfAdmin), UserType.Agent) => ".personalRepInPeriodOfAdmin.agent"
      case (Some(PersonalRepresentativeInPeriodOfAdmin), _)              => ".personalRepInPeriodOfAdmin"
      case (_, UserType.Individual)                                      => ""
      case (_, UserType.Organisation)                                    => ".trust"
      case (_, UserType.Agent)                                           => ".agent"
      case other                                                         => sys.error(s"User type '$other' not handled")
    }

  "MixedUsePropertyDetailsController" when {

    "handling requests to display the enter postcode page" must {

      def performAction(): Future[Result] =
        controller.enterPostcode()(FakeRequest())

      behave like redirectToStartBehaviour(() => performAction())

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.enterPostcode(),
        mockUUIDGenerator
      )

      "display the page" when {

        def test(result: Future[Result], expectedTitleKey: String, expectedBackLink: Call): Unit =
          checkPageIsDisplayed(
            result,
            messageFromMessageKey(expectedTitleKey),
            { doc =>
              doc
                .select(".govuk-caption-xl")
                .text() shouldBe messageFromMessageKey(
                "singleMixedUse.caption"
              )

              doc.select(".govuk-back-link").attr("href") shouldBe expectedBackLink.url

              doc
                .select("#main-content form")
                .attr("action") shouldBe routes.MixedUsePropertyDetailsController
                .enterPostcodeSubmit()
                .url
            }
          )

        "handling individuals" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(individualState()._1)
          }

          test(
            performAction(),
            "enterPostcode.returns.singleDisposal.title",
            routes.MixedUsePropertyDetailsController.singleMixedUseGuidance()
          )
        }

        "handling capacitors" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(capacitorState(Some(sample[CompleteMixedUsePropertyDetailsAnswers]))._1)
          }

          test(
            performAction(),
            "enterPostcode.returns.capacitor.singleDisposal.title",
            routes.MixedUsePropertyDetailsController.checkYourAnswers()
          )

        }

        "handling personal representatives" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(personalRepState()._1)
          }

          test(
            performAction(),
            "enterPostcode.returns.personalRep.singleDisposal.title",
            routes.MixedUsePropertyDetailsController.singleMixedUseGuidance()
          )
        }

        "handling agents" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(agentState()._1)
          }

          test(
            performAction(),
            "enterPostcode.returns.agent.singleDisposal.title",
            routes.MixedUsePropertyDetailsController.singleMixedUseGuidance()
          )

        }

        "handling trusts" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(trustState()._1)
          }

          test(
            performAction(),
            "enterPostcode.returns.trust.singleDisposal.title",
            routes.MixedUsePropertyDetailsController.singleMixedUseGuidance()
          )
        }

      }

    }

    "handling requests to display the enter address page" must {

      def performAction(): Future[Result] = controller.enterUkAddress()(FakeRequest())

      behave like redirectToStartBehaviour(() => performAction())

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.enterUkAddress(),
        mockUUIDGenerator
      )

      "display the page" when {

        def test(result: Future[Result], expectedTitleKey: String): Unit =
          checkPageIsDisplayed(
            result,
            messageFromMessageKey(expectedTitleKey),
            { doc =>
              doc
                .select(".govuk-caption-xl")
                .text() shouldBe messageFromMessageKey(
                "singleMixedUse.caption"
              )

              doc
                .select("label[for='address-line1']")
                .text() shouldBe messageFromMessageKey(
                "address.uk.line1.label"
              )

              doc
                .select("label[for='address-line2']")
                .text() shouldBe messageFromMessageKey(
                "address.uk.line2.label"
              )

              doc
                .select("#content form, #main-content form")
                .attr("action") shouldBe routes.MixedUsePropertyDetailsController.enterUkAddressSubmit().url
            }
          )

        "handling individuals" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(individualState()._1)
          }

          test(performAction(), "address.uk.returns.singleDisposal.title")
        }

        "handling capacitors" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(capacitorState()._1)
          }

          test(performAction(), "address.uk.returns.capacitor.singleDisposal.title")
        }

        "handling personal representatives" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(personalRepState()._1)
          }

          test(performAction(), "address.uk.returns.personalRep.singleDisposal.title")
        }

        "handling agents" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(agentState()._1)
          }

          test(performAction(), "address.uk.returns.agent.singleDisposal.title")
        }

        "handling trusts" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(trustState()._1)
          }

          test(performAction(), "address.uk.returns.trust.singleDisposal.title")

        }

      }

    }

    "handling submitted uk address" must {

      def performAction(formData: (String, String)*): Future[Result] =
        controller.enterUkAddressSubmit()(
          FakeRequest().withFormUrlEncodedBody(formData*).withMethod("POST")
        )

      behave like redirectToStartBehaviour(() => performAction())

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.enterUkAddressSubmit(),
        mockUUIDGenerator
      )

      "return a form error" when {

        def test(
          formData: (String, String)*
        )(expectedErrorMessageKey: String, expectedTitleKey: String): Unit =
          checkPageIsDisplayed(
            performAction(formData*),
            messageFromMessageKey(expectedTitleKey),
            doc =>
              doc
                .select("[data-spec='errorSummaryDisplay'] a")
                .text() shouldBe messageFromMessageKey(
                expectedErrorMessageKey
              ),
            BAD_REQUEST
          )

        "address line 1 is empty" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(individualState()._1)
          }

          test("postcode" -> "W1A2HV")(
            "address-line1.error.required",
            "address.uk.returns.singleDisposal.title"
          )
        }

        "address line 1 is too long" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(agentState()._1)
          }

          test(
            "address-line1" -> ("a" * 100),
            "postcode"      -> "W1A2HV"
          )(
            "address-line1.error.tooLong",
            "address.uk.returns.agent.singleDisposal.title"
          )
        }

        "address line 1 is invalid" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(trustState()._1)
          }

          test(
            "address-line1" -> "ab%csd",
            "postcode"      -> "W1A2HV"
          )(
            "address-line1.error.pattern",
            "address.uk.returns.trust.singleDisposal.title"
          )
        }

        "address line 2 is too long" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(personalRepState()._1)
          }
          test(
            "address-line1" -> "address line 1",
            "address-line2" -> ("a" * 100),
            "postcode"      -> "W1A2HV"
          )(
            "address-line2.error.tooLong",
            "address.uk.returns.personalRep.singleDisposal.title"
          )
        }

        "address line 2 is invalid" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(capacitorState()._1)
          }

          test(
            "address-line1" -> "address line 1",
            "address-line2" -> "fsdhio*fde@df",
            "postcode"      -> "W1A2HV"
          )(
            "address-line2.error.pattern",
            "address.uk.returns.capacitor.singleDisposal.title"
          )
        }

        "address line 3 is too long" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(personalRepState()._1)
          }
          test(
            "address-line1" -> "address line 1",
            "address-town"  -> ("a" * 100),
            "postcode"      -> "W1A2HV"
          )(
            "address-town.error.tooLong",
            "address.uk.returns.personalRep.singleDisposal.title"
          )
        }

        "address line 3 is invalid" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(capacitorState()._1)
          }

          test(
            "address-line1" -> "address line 1",
            "address-town"  -> "fsdhio*fde@df",
            "postcode"      -> "W1A2HV"
          )(
            "address-town.error.pattern",
            "address.uk.returns.capacitor.singleDisposal.title"
          )
        }

        "address line 4 is too long" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(personalRepState()._1)
          }
          test(
            "address-line1"  -> "address line 1",
            "address-county" -> ("a" * 100),
            "postcode"       -> "W1A2HV"
          )(
            "address-county.error.tooLong",
            "address.uk.returns.personalRep.singleDisposal.title"
          )
        }

        "address line 4 is invalid" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(capacitorState()._1)
          }

          test(
            "address-line1"  -> "address line 1",
            "address-county" -> "fsdhio*fde@df",
            "postcode"       -> "W1A2HV"
          )(
            "address-county.error.pattern",
            "address.uk.returns.capacitor.singleDisposal.title"
          )
        }

        "address postcode is empty" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(individualState()._1)
          }

          test("address-line1" -> "1 the Street")(
            "postcode.error.required",
            "address.uk.returns.singleDisposal.title"
          )
        }

        "the address postcode contains invalid characters" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(individualState()._1)
          }

          test(
            "address-line1" -> "1 the Street",
            "postcode"      -> "W1A,2HV"
          )(
            "postcode.error.pattern",
            "address.uk.returns.singleDisposal.title"
          )
        }

        "the address postcode does not have a valid format" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(individualState()._1)
          }

          test(
            "address-line1" -> "1 the Street",
            "postcode"      -> "ABC123"
          )(
            "postcode.error.pattern",
            "address.uk.returns.singleDisposal.title"
          )
        }

      }

      "show an error page" when {

        val (session, journey, draftReturn) = individualState(
          previousReturnData = None,
          amendReturnData = None
        )
        val address                         =
          UkAddress("address line 1", None, None, None, Postcode("ZZ10ZZ"))

        val newDraftReturn = draftReturn.copy(mixedUsePropertyDetailsAnswers =
          Some(IncompleteMixedUsePropertyDetailsAnswers(Some(address), None, None))
        )
        val newJourney     = journey.copy(draftReturn = newDraftReturn)
        val newSession     = session.copy(journeyStatus = Some(newJourney))

        "there is an error updating the draft return" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(newJourney)(
              Left(Error(""))
            )
          }

          checkIsTechnicalErrorPage(
            performAction(
              "address-line1" -> address.line1,
              "postcode"      -> address.postcode.value
            )
          )
        }

        "there is an error updating the session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(newJourney)(
              Right(())
            )
            mockStoreSession(newSession)(Left(Error("")))
          }

          checkIsTechnicalErrorPage(
            performAction(
              "address-line1" -> address.line1,
              "postcode"      -> address.postcode.value
            )
          )
        }

      }

      "redirect to the cya page" when {

        def test(
          formData: Seq[(String, String)]
        )(session: SessionData, newJourney: FillingOutReturn): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(newJourney)(
              Right(())
            )
            mockStoreSession(session.copy(journeyStatus = Some(newJourney)))(Right(()))
          }

          checkIsRedirect(
            performAction(formData*),
            routes.MixedUsePropertyDetailsController.checkYourAnswers()
          )
        }

        val address  = UkAddress("address line 1", None, None, None, Postcode("ZZ10ZZ"))
        val formData =
          List(
            "address-line1" -> address.line1,
            "postcode"      -> address.postcode.value
          )

        "all updates are successful" in {
          val (session, journey, draftReturn) = agentState()

          val newDraftReturn = draftReturn.copy(mixedUsePropertyDetailsAnswers =
            Some(IncompleteMixedUsePropertyDetailsAnswers(Some(address), None, None))
          )
          val newJourney     = journey.copy(draftReturn = newDraftReturn)

          test(formData)(session, newJourney)
        }

        "the user is on an amend return journey" in {
          val (session, journey, draftReturn) = individualState(
            previousReturnData = None,
            amendReturnData = Some(sample[AmendReturnData])
          )

          val newDraftReturn = draftReturn.copy(
            mixedUsePropertyDetailsAnswers = Some(IncompleteMixedUsePropertyDetailsAnswers(Some(address), None, None)),
            exemptionAndLossesAnswers = None,
            yearToDateLiabilityAnswers = None
          )
          val newJourney     = journey.copy(draftReturn = newDraftReturn)

          test(formData)(session, newJourney)
        }

        "the user is on a further return journey" in {
          val (session, journey, draftReturn) = individualState(
            previousReturnData = Some(sample[PreviousReturnData].copy(summaries = List(sample[ReturnSummary]))),
            amendReturnData = None
          )

          val newDraftReturn = draftReturn.copy(
            mixedUsePropertyDetailsAnswers = Some(IncompleteMixedUsePropertyDetailsAnswers(Some(address), None, None)),
            exemptionAndLossesAnswers = None,
            yearToDateLiabilityAnswers = None
          )
          val newJourney     = journey.copy(draftReturn = newDraftReturn)

          test(formData)(session, newJourney)
        }

      }

    }

    "handling requests to display the disposal price page" must {

      val key = "singleMixedUseDisposalsDisposalPrice"

      def performAction(): Future[Result] =
        controller.enterDisposalValue()(FakeRequest())

      behave like redirectToStartBehaviour(() => performAction())

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.enterDisposalValue(),
        mockUUIDGenerator
      )

      "display the page" when {

        def test(
          draftReturn: DraftSingleMixedUseDisposalReturn,
          expectedBackLink: Call,
          userType: UserType
        ): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                userType = Some(userType),
                journeyStatus = Some(
                  sample[FillingOutReturn].copy(
                    draftReturn = draftReturn,
                    subscribedDetails = sample[SubscribedDetails].copy(
                      name = if (userType === Organisation) Left(sample[TrustName]) else Right(sample[IndividualName])
                    ),
                    agentReferenceNumber = if (userType === Agent) Some(sample[AgentReferenceNumber]) else None
                  )
                )
              )
            )
          }
          val userKey = deriveUserKey(userType === Agent, userType === Organisation)

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(s"$key.title"),
            { doc =>
              doc.select(".govuk-back-link").attr("href") shouldBe expectedBackLink.url
              doc
                .select(".govuk-caption-xl")
                .text()                                   shouldBe messageFromMessageKey(
                "singleMixedUse.caption"
              )
              doc
                .select("#main-content form")
                .attr("action")                           shouldBe routes.MixedUsePropertyDetailsController
                .enterDisposalValueSubmit()
                .url
              doc
                .select(s"#$key-hint")
                .text()                                   shouldBe messageFromMessageKey(s"$key$userKey.helpText")
            }
          )
        }

        "individual user has not started this section before" in {
          test(
            sample[DraftSingleMixedUseDisposalReturn].copy(
              mixedUsePropertyDetailsAnswers = None,
              triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                .copy(individualUserType = None)
            ),
            routes.MixedUsePropertyDetailsController.enterPostcode(),
            Individual
          )
        }

        "trust user has not started this section before" in {
          test(
            sample[DraftSingleMixedUseDisposalReturn].copy(
              mixedUsePropertyDetailsAnswers = None,
              triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                .copy(individualUserType = None)
            ),
            routes.MixedUsePropertyDetailsController.enterPostcode(),
            Organisation
          )
        }

        "agent user has not started this section before" in {
          test(
            sample[DraftSingleMixedUseDisposalReturn].copy(
              mixedUsePropertyDetailsAnswers = None,
              triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                .copy(individualUserType = None)
            ),
            routes.MixedUsePropertyDetailsController.enterPostcode(),
            Agent
          )
        }

        "individual user has started but not completed this section" in {
          test(
            sample[DraftSingleMixedUseDisposalReturn].copy(
              mixedUsePropertyDetailsAnswers = Some(
                sample[IncompleteMixedUsePropertyDetailsAnswers].copy(
                  disposalPrice = None
                )
              ),
              triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                .copy(individualUserType = None)
            ),
            routes.MixedUsePropertyDetailsController.enterPostcode(),
            Individual
          )
        }

        "trust user has started but not completed this section" in {
          test(
            sample[DraftSingleMixedUseDisposalReturn].copy(
              mixedUsePropertyDetailsAnswers = Some(
                sample[IncompleteMixedUsePropertyDetailsAnswers].copy(
                  disposalPrice = None
                )
              ),
              triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                .copy(individualUserType = None)
            ),
            routes.MixedUsePropertyDetailsController.enterPostcode(),
            Organisation
          )
        }

        "agent user has started but not completed this section" in {
          test(
            sample[DraftSingleMixedUseDisposalReturn].copy(
              mixedUsePropertyDetailsAnswers = Some(
                sample[IncompleteMixedUsePropertyDetailsAnswers].copy(
                  disposalPrice = None
                )
              ),
              triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                .copy(individualUserType = None)
            ),
            routes.MixedUsePropertyDetailsController.enterPostcode(),
            Agent
          )
        }

        "individual user has completed this section" in {
          test(
            sample[DraftSingleMixedUseDisposalReturn].copy(
              mixedUsePropertyDetailsAnswers = Some(
                sample[CompleteMixedUsePropertyDetailsAnswers].copy(
                  disposalPrice = sample[AmountInPence]
                )
              ),
              triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                .copy(individualUserType = None)
            ),
            routes.MixedUsePropertyDetailsController.checkYourAnswers(),
            Individual
          )
        }

        "trust user has completed this section" in {
          test(
            sample[DraftSingleMixedUseDisposalReturn].copy(
              mixedUsePropertyDetailsAnswers = Some(
                sample[CompleteMixedUsePropertyDetailsAnswers].copy(
                  disposalPrice = sample[AmountInPence]
                )
              ),
              triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                .copy(individualUserType = None)
            ),
            routes.MixedUsePropertyDetailsController.checkYourAnswers(),
            Organisation
          )
        }

        "agent user has completed this section" in {
          test(
            sample[DraftSingleMixedUseDisposalReturn].copy(
              mixedUsePropertyDetailsAnswers = Some(
                sample[CompleteMixedUsePropertyDetailsAnswers].copy(
                  disposalPrice = sample[AmountInPence]
                )
              ),
              triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                .copy(individualUserType = None)
            ),
            routes.MixedUsePropertyDetailsController.checkYourAnswers(),
            Agent
          )
        }

      }

    }

    "handling submitted answers to the disposal price page" must {

      val key = "singleMixedUseDisposalsDisposalPrice"

      def performAction(formData: (String, String)*): Future[Result] =
        controller.enterDisposalValueSubmit()(
          FakeRequest().withFormUrlEncodedBody(formData*).withMethod("POST")
        )

      behave like redirectToStartBehaviour(() => performAction())

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.enterDisposalValueSubmit(),
        mockUUIDGenerator
      )

      "not update the session" when {

        "the data submitted is the same as one that already exists in session" in {

          val disposalPrice = AmountInPence.fromPounds(1000)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  sample[FillingOutReturn].copy(
                    draftReturn = sample[DraftSingleMixedUseDisposalReturn].copy(
                      mixedUsePropertyDetailsAnswers = Some(
                        sample[IncompleteMixedUsePropertyDetailsAnswers].copy(
                          disposalPrice = Some(disposalPrice)
                        )
                      )
                    ),
                    subscribedDetails = sample[SubscribedDetails].copy(
                      name = Right(sample[IndividualName])
                    )
                  )
                )
              )
            )
          }

          checkIsRedirect(
            performAction(key -> "1000"),
            routes.MixedUsePropertyDetailsController.checkYourAnswers()
          )
        }

      }

      "show a form error for amount" when {

        def test(data: (String, String)*)(expectedErrorMessageKey: String): Unit = {
          val draftReturn = sample[DraftSingleMixedUseDisposalReturn].copy(
            mixedUsePropertyDetailsAnswers = Some(
              sample[CompleteMixedUsePropertyDetailsAnswers]
            )
          )
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  sample[FillingOutReturn].copy(
                    draftReturn = draftReturn,
                    subscribedDetails = sample[SubscribedDetails].copy(
                      name = Right(sample[IndividualName])
                    )
                  )
                )
              )
            )
          }

          val userKey =
            userMessageKey(draftReturn.triageAnswers.fold(_.individualUserType, _.individualUserType), Individual)

          checkPageIsDisplayed(
            performAction(data*),
            messageFromMessageKey(s"$key$userKey.title"),
            doc =>
              doc
                .select("[data-spec='errorSummaryDisplay'] a")
                .text() shouldBe messageFromMessageKey(
                expectedErrorMessageKey
              ),
            BAD_REQUEST
          )
        }

        "the data is invalid" in {
          amountOfMoneyErrorScenarios(key).foreach { scenario =>
            withClue(s"For $scenario: ") {
              val data = scenario.formData
              test(data*)(scenario.expectedErrorMessageKey)
            }
          }
        }

      }

      "redirect to the cya page" when {

        def test(
          result: => Future[Result],
          oldDraftReturn: DraftReturn,
          updatedDraftReturn: DraftReturn,
          isAmend: Boolean
        ): Unit = {
          val amendReturnData = sample[AmendReturnData]

          val journey        = sample[FillingOutReturn].copy(
            draftReturn = oldDraftReturn,
            amendReturnData =
              if (isAmend) Some(amendReturnData.copy(shouldDisplayGainOrLossAfterReliefs = false)) else None
          )
          val session        = SessionData.empty.copy(
            journeyStatus = Some(journey)
          )
          val updatedJourney = journey.copy(
            draftReturn = updatedDraftReturn,
            amendReturnData =
              if (isAmend) Some(amendReturnData.copy(shouldDisplayGainOrLossAfterReliefs = true)) else None
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
            result,
            routes.MixedUsePropertyDetailsController.checkYourAnswers()
          )
        }

        "the user has completed this section" in {

          val answers = sample[CompleteMixedUsePropertyDetailsAnswers].copy(
            disposalPrice = AmountInPence.fromPounds(1)
          )

          val oldDraftReturn = sample[DraftSingleMixedUseDisposalReturn].copy(
            mixedUsePropertyDetailsAnswers = Some(answers)
          )

          val updatedDraftReturn = oldDraftReturn.copy(
            mixedUsePropertyDetailsAnswers = Some(
              answers.copy(
                disposalPrice = AmountInPence.fromPounds(10)
              )
            ),
            yearToDateLiabilityAnswers = None,
            gainOrLossAfterReliefs = None
          )

          test(
            performAction(key -> "10"),
            oldDraftReturn,
            updatedDraftReturn,
            isAmend = true
          )
        }

        "the user hasn't ever answered the disposal price question " +
          "and the draft return and session data has been successfully updated" in {

            val answers = sample[IncompleteMixedUsePropertyDetailsAnswers].copy(
              address = Some(sample[UkAddress]),
              disposalPrice = Some(AmountInPence.fromPounds(1))
            )

            val oldDraftReturn = sample[DraftSingleMixedUseDisposalReturn].copy(
              mixedUsePropertyDetailsAnswers = Some(answers)
            )

            val updatedDraftReturn = oldDraftReturn.copy(
              mixedUsePropertyDetailsAnswers = Some(
                answers.copy(
                  disposalPrice = Some(AmountInPence.fromPounds(10))
                )
              ),
              yearToDateLiabilityAnswers = None,
              gainOrLossAfterReliefs = None
            )

            test(
              performAction(key -> "10"),
              oldDraftReturn,
              updatedDraftReturn,
              isAmend = false
            )
          }

      }

    }

    "handling requests to display the acquisition price page" must {

      val key = "singleMixedUseDisposalsAcquisitionPrice"

      def performAction(): Future[Result] =
        controller.enterAcquisitionValue()(FakeRequest())

      behave like redirectToStartBehaviour(() => performAction())

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.enterAcquisitionValue(),
        mockUUIDGenerator
      )

      "display the page" when {

        def test(
          draftReturn: DraftSingleMixedUseDisposalReturn,
          expectedBackLink: Call,
          userType: UserType,
          titleArgs: Seq[String] = Seq.empty
        ): Unit = {
          val individualUserType = draftReturn.triageAnswers.fold(_.individualUserType, _.individualUserType)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                userType = Some(userType),
                journeyStatus = Some(
                  sample[FillingOutReturn].copy(
                    draftReturn = draftReturn,
                    subscribedDetails = sample[SubscribedDetails].copy(
                      name = if (userType === Organisation) Left(sample[TrustName]) else Right(sample[IndividualName])
                    ),
                    agentReferenceNumber = if (userType === Agent) Some(sample[AgentReferenceNumber]) else None
                  )
                )
              )
            )
          }

          val userKey = userMessageKey(individualUserType, userType)
          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(s"$key$userKey.title", titleArgs*),
            { doc =>
              doc.select(".govuk-back-link").attr("href") shouldBe expectedBackLink.url
              doc
                .select(".govuk-caption-xl")
                .text()                                   shouldBe messageFromMessageKey(
                "singleMixedUse.caption"
              )
              doc
                .select("#main-content form")
                .attr("action")                           shouldBe routes.MixedUsePropertyDetailsController
                .enterAcquisitionValue()
                .url
              doc
                .select(s"#$key-hint")
                .text()                                   shouldBe messageFromMessageKey(s"$key$userKey.helpText")
            }
          )
        }

        "individual user has not started this section before" in {
          test(
            sample[DraftSingleMixedUseDisposalReturn].copy(
              mixedUsePropertyDetailsAnswers = None,
              triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                .copy(individualUserType = Some(Self))
            ),
            routes.MixedUsePropertyDetailsController.enterDisposalValue(),
            Individual
          )
        }

        "trust user has not started this section before" in {
          test(
            sample[DraftSingleMixedUseDisposalReturn].copy(
              mixedUsePropertyDetailsAnswers = None,
              triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                .copy(individualUserType = None)
            ),
            routes.MixedUsePropertyDetailsController.enterDisposalValue(),
            Organisation
          )
        }

        "agent user has not started this section before" in {
          test(
            sample[DraftSingleMixedUseDisposalReturn].copy(
              mixedUsePropertyDetailsAnswers = None,
              triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                .copy(individualUserType = Some(Self))
            ),
            routes.MixedUsePropertyDetailsController.enterDisposalValue(),
            Agent
          )
        }

        "individual user has started but not completed this section" in {
          test(
            sample[DraftSingleMixedUseDisposalReturn].copy(
              triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                .copy(individualUserType = Some(Capacitor)),
              mixedUsePropertyDetailsAnswers = Some(
                sample[IncompleteMixedUsePropertyDetailsAnswers].copy(
                  acquisitionPrice = None
                )
              )
            ),
            routes.MixedUsePropertyDetailsController.enterDisposalValue(),
            Individual
          )
        }

        "trust user has started but not completed this section" in {
          test(
            sample[DraftSingleMixedUseDisposalReturn].copy(
              mixedUsePropertyDetailsAnswers = Some(
                sample[IncompleteMixedUsePropertyDetailsAnswers].copy(
                  acquisitionPrice = None
                )
              ),
              triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                .copy(individualUserType = None)
            ),
            routes.MixedUsePropertyDetailsController.enterDisposalValue(),
            Organisation
          )
        }

        "agent user has started but not completed this section" in {
          val dateOfDeath = LocalDate.ofEpochDay(0L)

          test(
            sample[DraftSingleMixedUseDisposalReturn].copy(
              mixedUsePropertyDetailsAnswers = Some(
                sample[IncompleteMixedUsePropertyDetailsAnswers].copy(
                  acquisitionPrice = None
                )
              ),
              triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                .copy(individualUserType = Some(PersonalRepresentativeInPeriodOfAdmin)),
              representeeAnswers =
                Some(sample[CompleteRepresenteeAnswers].copy(dateOfDeath = Some(DateOfDeath(dateOfDeath))))
            ),
            routes.MixedUsePropertyDetailsController.enterDisposalValue(),
            Agent,
            Seq(TimeUtils.govDisplayFormat(dateOfDeath))
          )
        }

        "individual user has completed this section" in {
          test(
            sample[DraftSingleMixedUseDisposalReturn].copy(
              mixedUsePropertyDetailsAnswers = Some(
                sample[CompleteMixedUsePropertyDetailsAnswers].copy(
                  acquisitionPrice = sample[AmountInPence]
                )
              ),
              triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                .copy(individualUserType = Some(PersonalRepresentative))
            ),
            routes.MixedUsePropertyDetailsController.checkYourAnswers(),
            Individual
          )
        }

        "trust user has completed this section" in {
          test(
            sample[DraftSingleMixedUseDisposalReturn].copy(
              mixedUsePropertyDetailsAnswers = Some(
                sample[CompleteMixedUsePropertyDetailsAnswers].copy(
                  acquisitionPrice = sample[AmountInPence]
                )
              ),
              triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                .copy(individualUserType = None)
            ),
            routes.MixedUsePropertyDetailsController.checkYourAnswers(),
            Organisation
          )
        }

        "agent user has completed this section" in {
          test(
            sample[DraftSingleMixedUseDisposalReturn].copy(
              mixedUsePropertyDetailsAnswers = Some(
                sample[CompleteMixedUsePropertyDetailsAnswers].copy(
                  acquisitionPrice = sample[AmountInPence]
                )
              ),
              triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                .copy(individualUserType = Some(Self))
            ),
            routes.MixedUsePropertyDetailsController.checkYourAnswers(),
            Agent
          )
        }

      }

    }

    "handling submitted answers to the acquisition price page" must {

      val key = "singleMixedUseDisposalsAcquisitionPrice"

      def performAction(formData: (String, String)*): Future[Result] =
        controller.enterAcquisitionValueSubmit()(
          FakeRequest().withFormUrlEncodedBody(formData*).withMethod("POST")
        )

      behave like redirectToStartBehaviour(() => performAction())

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.enterAcquisitionValueSubmit(),
        mockUUIDGenerator
      )

      "not update the session" when {

        "the data submitted is the same as one that already exists in session" in {

          val acquisitionPrice = AmountInPence.fromPounds(1000)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  sample[FillingOutReturn].copy(
                    draftReturn = sample[DraftSingleMixedUseDisposalReturn].copy(
                      mixedUsePropertyDetailsAnswers = Some(
                        sample[IncompleteMixedUsePropertyDetailsAnswers].copy(
                          acquisitionPrice = Some(acquisitionPrice)
                        )
                      )
                    ),
                    subscribedDetails = sample[SubscribedDetails].copy(
                      name = Right(sample[IndividualName])
                    )
                  )
                )
              )
            )
          }

          checkIsRedirect(
            performAction(key -> "1000"),
            routes.MixedUsePropertyDetailsController.checkYourAnswers()
          )
        }

      }

      "show a form error for amount" when {

        def test(
          data: (String, String)*
        )(
          draftReturn: DraftSingleMixedUseDisposalReturn,
          agentReferenceNumber: Option[AgentReferenceNumber],
          expectedErrorMessageKey: String
        ): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  sample[FillingOutReturn].copy(
                    draftReturn = draftReturn,
                    subscribedDetails = sample[SubscribedDetails].copy(
                      name = Right(sample[IndividualName])
                    ),
                    agentReferenceNumber = agentReferenceNumber
                  )
                )
              )
            )
          }
          val userKey     =
            userMessageKey(draftReturn.triageAnswers.fold(_.individualUserType, _.individualUserType), Individual)
          val dateOfDeath = draftReturn.representeeAnswers.flatMap(_.fold(_.dateOfDeath, _.dateOfDeath))
          val titleArg    = dateOfDeath.map(e => TimeUtils.govDisplayFormat(e.value)).getOrElse("")
          checkPageIsDisplayed(
            performAction(data*),
            messageFromMessageKey(s"$key$userKey.title", titleArg),
            doc =>
              doc
                .select("[data-spec='errorSummaryDisplay'] a")
                .text() shouldBe messageFromMessageKey(
                expectedErrorMessageKey
              ),
            BAD_REQUEST
          )
        }

        "the data is invalid" in {
          forAll { (individualUserType: Option[IndividualUserType]) =>
            whenever(!individualUserType.contains(PersonalRepresentativeInPeriodOfAdmin)) {
              amountOfMoneyErrorScenarios(key).foreach { scenario =>
                withClue(s"For $individualUserType and $scenario: ") {
                  val data = scenario.formData
                  test(data*)(
                    sample[DraftSingleMixedUseDisposalReturn].copy(
                      triageAnswers =
                        sample[CompleteSingleDisposalTriageAnswers].copy(individualUserType = individualUserType),
                      mixedUsePropertyDetailsAnswers = Some(
                        sample[CompleteMixedUsePropertyDetailsAnswers]
                      )
                    ),
                    None,
                    scenario.expectedErrorMessageKey
                  )
                }
              }
            }
          }
        }

        "the data is invalid for period of admin" in {
          amountOfMoneyErrorScenarios(key, errorContext = Some(s"$key.personalRepInPeriodOfAdmin")).foreach {
            scenario =>
              withClue(s"For $scenario: ") {
                val data = scenario.formData
                test(data*)(
                  sample[DraftSingleMixedUseDisposalReturn].copy(
                    triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                      .copy(individualUserType = Some(PersonalRepresentativeInPeriodOfAdmin)),
                    mixedUsePropertyDetailsAnswers = Some(
                      sample[CompleteMixedUsePropertyDetailsAnswers]
                    )
                  ),
                  None,
                  scenario.expectedErrorMessageKey
                )
              }

          }
        }

      }

      "redirect to the cya page" when {

        def test(
          result: => Future[Result],
          oldDraftReturn: DraftReturn,
          updatedDraftReturn: DraftReturn,
          isAmend: Boolean
        ): Unit = {
          val amendReturnData = sample[AmendReturnData]
          val journey         =
            sample[FillingOutReturn].copy(
              draftReturn = oldDraftReturn,
              amendReturnData =
                if (isAmend) Some(amendReturnData.copy(shouldDisplayGainOrLossAfterReliefs = false)) else None
            )
          val session         = SessionData.empty.copy(journeyStatus = Some(journey))
          val updatedJourney  = journey.copy(
            draftReturn = updatedDraftReturn,
            amendReturnData =
              if (isAmend) Some(amendReturnData.copy(shouldDisplayGainOrLossAfterReliefs = true)) else None
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(updatedJourney)(
              Right(())
            )
            mockStoreSession(
              session
                .copy(journeyStatus = Some(updatedJourney))
            )(Right(()))
          }

          checkIsRedirect(
            result,
            routes.MixedUsePropertyDetailsController.checkYourAnswers()
          )
        }

        "the user has completed this section" in {
          val answers = sample[CompleteMixedUsePropertyDetailsAnswers].copy(
            acquisitionPrice = AmountInPence.fromPounds(10)
          )

          val oldDraftReturn = sample[DraftSingleMixedUseDisposalReturn].copy(
            mixedUsePropertyDetailsAnswers = Some(answers)
          )

          val newDraftReturn = oldDraftReturn.copy(
            mixedUsePropertyDetailsAnswers = Some(
              answers.copy(
                acquisitionPrice = AmountInPence.fromPounds(100)
              )
            ),
            yearToDateLiabilityAnswers = None,
            gainOrLossAfterReliefs = None
          )

          test(
            performAction(key -> "100"),
            oldDraftReturn,
            newDraftReturn,
            isAmend = true
          )
        }

        "the user hasn't ever answered the acquisition price question " +
          "and the draft return and session data has been successfully updated" in {

            val answers = sample[IncompleteMixedUsePropertyDetailsAnswers].copy(
              acquisitionPrice = Some(AmountInPence.fromPounds(10))
            )

            val oldDraftReturn = sample[DraftSingleMixedUseDisposalReturn].copy(
              mixedUsePropertyDetailsAnswers = Some(answers)
            )

            val newDraftReturn = oldDraftReturn.copy(
              mixedUsePropertyDetailsAnswers = Some(
                answers.copy(
                  acquisitionPrice = Some(AmountInPence.fromPounds(100))
                )
              ),
              yearToDateLiabilityAnswers = None,
              gainOrLossAfterReliefs = None
            )

            test(
              performAction(key -> "100"),
              oldDraftReturn,
              newDraftReturn,
              isAmend = false
            )
          }

      }

    }

    "handling requests to display the cya page" must {

      def performAction(): Future[Result] =
        controller.checkYourAnswers()(FakeRequest())

      behave like redirectToStartBehaviour(() => performAction())

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.checkYourAnswers(),
        mockUUIDGenerator
      )

      "redirect to the guidance page" when {

        "there is no address in the session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(individualState(address = None)._1)
          }

          checkIsRedirect(
            performAction(),
            routes.MixedUsePropertyDetailsController.singleMixedUseGuidance()
          )
        }

      }

    }

    "handling submits on the cya page" must {

      def performAction(): Future[Result] =
        controller.checkYourAnswersSubmit()(FakeRequest())

      behave like redirectToStartBehaviour(() => performAction())

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.checkYourAnswersSubmit(),
        mockUUIDGenerator
      )

      "redirect to the tasklist" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(individualState()._1)
        }

        checkIsRedirect(
          performAction(),
          controllers.returns.routes.TaskListController.taskList()
        )
      }

    }

  }

}
