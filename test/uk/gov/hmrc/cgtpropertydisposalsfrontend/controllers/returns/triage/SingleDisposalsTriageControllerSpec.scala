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
import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.SampledScalaCheck
import play.api.i18n.{Lang, Messages, MessagesApi, MessagesImpl}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.{Call, Result}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.DateErrorScenarios.{DateErrorScenario, dateErrorScenarios}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.triage.SingleDisposalsTriageControllerSpec.validateSingleDisposalTriageCheckYourAnswersPage
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.{ReturnsServiceSupport, StartingToAmendToFillingOutReturnSpecBehaviour, representee, routes => returnsRoutes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, PreviousReturnData, StartingNewDraftReturn, StartingToAmendReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Country
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.AddressGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.CompleteReturnGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.DraftReturnGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.Generators.{arb, booleanGen, sample}
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
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.AssetType.{IndirectDisposal, MixedUse, NonResidential, Residential}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.CompleteReturn.CompleteSingleIndirectDisposalReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.IndividualUserType._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.RepresenteeAnswers.{CompleteRepresenteeAnswers, IncompleteRepresenteeAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.{CompleteSingleDisposalTriageAnswers, IncompleteSingleDisposalTriageAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.YearToDateLiabilityAnswers.NonCalculatedYTDAnswers.CompleteNonCalculatedYTDAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.YearToDateLiabilityAnswers.{CalculatedYTDAnswers, NonCalculatedYTDAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{CompleteReturnWithSummary, Error, JourneyStatus, SessionData, TaxYear, TimeUtils, UserType}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.{ReturnsService, TaxYearService}
import uk.gov.hmrc.http.HeaderCarrier

import java.time.{Clock, LocalDate}
import java.util.UUID
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class SingleDisposalsTriageControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with SampledScalaCheck
    with RedirectToStartBehaviour
    with ReturnsServiceSupport
    with StartingToAmendToFillingOutReturnSpecBehaviour {

  private val mockTaxYearService = mock[TaxYearService]

  private val mockUUIDGenerator = mock[UUIDGenerator]

  protected override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore),
      bind[ReturnsService].toInstance(mockReturnsService),
      bind[UUIDGenerator].toInstance(mockUUIDGenerator),
      bind[TaxYearService].toInstance(mockTaxYearService)
    )

  private val today           = LocalDate.now(Clock.systemUTC())
  private val startDate       = TaxYear.thisTaxYearStartDate()
  private val endDate         = startDate.plusYears(1)
  private val taxYear         = sample[TaxYear].copy(
    startDateInclusive = startDate,
    endDateExclusive = endDate
  )
  private lazy val controller = instanceOf[SingleDisposalsTriageController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  def isValidJourney(journeyStatus: JourneyStatus): Boolean =
    journeyStatus match {
      case r: StartingNewDraftReturn if r.newReturnTriageAnswers.isRight => true
      case _: FillingOutReturn                                           => true
      case _: StartingToAmendReturn                                      => true
      case _                                                             => false
    }

  def sessionDataWithStartingNewDraftReturn(
    singleDisposalTriageAnswers: SingleDisposalTriageAnswers,
    name: Either[TrustName, IndividualName] = Right(sample[IndividualName]),
    representeeAnswers: Option[RepresenteeAnswers] = None,
    previousSentReturns: Option[List[ReturnSummary]] = None
  ): (SessionData, StartingNewDraftReturn) = {
    val startingNewDraftReturn = sample[StartingNewDraftReturn].copy(
      subscribedDetails = sample[SubscribedDetails].copy(name = name),
      newReturnTriageAnswers = Right(singleDisposalTriageAnswers),
      representeeAnswers = representeeAnswers,
      previousSentReturns = previousSentReturns.map(PreviousReturnData(_, None, None, None))
    )

    val sessionData = SessionData.empty.copy(
      journeyStatus = Some(
        startingNewDraftReturn.copy(
          subscribedDetails = startingNewDraftReturn.subscribedDetails.copy(name = name),
          newReturnTriageAnswers = Right(singleDisposalTriageAnswers)
        )
      )
    )

    sessionData -> startingNewDraftReturn
  }

  def sessionDataWithFillingOurReturn(
    draftReturn: DraftSingleDisposalReturn,
    name: Either[TrustName, IndividualName],
    previousSentReturns: Option[List[ReturnSummary]],
    amendReturnData: Option[AmendReturnData] = None
  ): (SessionData, FillingOutReturn) = {
    val fillingOutReturn = sample[FillingOutReturn].copy(
      draftReturn = draftReturn,
      subscribedDetails = sample[SubscribedDetails].copy(name = name),
      previousSentReturns = previousSentReturns.map(PreviousReturnData(_, None, None, None)),
      amendReturnData = amendReturnData
    )

    val sessionData = SessionData.empty.copy(
      journeyStatus = Some(fillingOutReturn)
    )

    sessionData -> fillingOutReturn
  }

  def sessionDataWithFillingOutReturn(
    singleDisposalTriageAnswers: SingleDisposalTriageAnswers,
    name: Either[TrustName, IndividualName] = Right(sample[IndividualName]),
    representeeAnswers: Option[RepresenteeAnswers] = Some(sample[IncompleteRepresenteeAnswers]),
    previousSentReturns: Option[List[ReturnSummary]] = None,
    amendReturnData: Option[AmendReturnData] = None
  ): (SessionData, FillingOutReturn, DraftSingleDisposalReturn) = {
    val draftReturn = sample[DraftSingleDisposalReturn].copy(
      triageAnswers = singleDisposalTriageAnswers,
      representeeAnswers = representeeAnswers
    )

    val (session, journey) = sessionDataWithFillingOurReturn(
      draftReturn,
      name,
      previousSentReturns,
      amendReturnData
    )
    (session, journey, draftReturn)
  }

  private def mockGetNextUUID(uuid: UUID) =
    (() => mockUUIDGenerator.nextId()).expects().returning(uuid)

  private def mockGetTaxYear(
    date: LocalDate
  )(response: Either[Error, Option[TaxYear]]) =
    (mockTaxYearService
      .taxYear(_: LocalDate)(using _: HeaderCarrier))
      .expects(date, *)
      .returning(EitherT.fromEither[Future](response))

  private def expectedSubmitText(isAmend: Boolean) =
    messageFromMessageKey(if (isAmend) "button.continue" else "button.saveAndContinue")

  "The SingleDisposalsTriageController" when {

    "handling requests to display how did you dispose of your property page" must {

      val requiredPreviousAnswers =
        IncompleteSingleDisposalTriageAnswers.empty.copy(
          hasConfirmedSingleDisposal = true,
          individualUserType = Some(Self)
        )

      def performAction(): Future[Result] =
        controller.howDidYouDisposeOfProperty()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(() => performAction(), isValidJourney)

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.howDidYouDisposeOfProperty(),
        mockUUIDGenerator
      )

      behave like redirectWhenNoPreviousAnswerBehaviour[NumberOfProperties](() => performAction())(
        requiredPreviousAnswers,
        routes.CommonTriageQuestionsController.howManyProperties(),
        { case (answers, n) =>
          answers.copy(
            hasConfirmedSingleDisposal = if (n.contains(NumberOfProperties.One)) true else false
          )
        }
      )

      behave like displayIndividualTriagePageBehaviorIncompleteJourney(() => performAction())(
        requiredPreviousAnswers,
        requiredPreviousAnswers.copy(disposalMethod = Some(DisposalMethod.Sold))
      )(
        "disposalMethod.title",
        checkContent(
          _,
          routes.CommonTriageQuestionsController.howManyProperties()
        ),
        _.select("#disposalMethod-0").hasAttr("checked")
      )

      behave like displayIndividualTriagePageBehaviorIncompleteJourney(() => performAction())(
        requiredPreviousAnswers.copy(individualUserType = Some(Capacitor)),
        requiredPreviousAnswers.copy(
          disposalMethod = Some(DisposalMethod.Sold),
          individualUserType = Some(Capacitor)
        )
      )(
        "disposalMethod.capacitor.title",
        checkContent(
          _,
          routes.CommonTriageQuestionsController.howManyProperties()
        ),
        _.select("#disposalMethod-0").hasAttr("checked")
      )

      behave like displayIndividualTriagePageBehaviorIncompleteJourney(() => performAction())(
        requiredPreviousAnswers
          .copy(individualUserType = Some(PersonalRepresentative)),
        requiredPreviousAnswers.copy(
          disposalMethod = Some(DisposalMethod.Sold),
          individualUserType = Some(PersonalRepresentative)
        )
      )(
        "disposalMethod.personalRep.title",
        checkContent(
          _,
          routes.CommonTriageQuestionsController.howManyProperties()
        ),
        _.select("#disposalMethod-0").hasAttr("checked")
      )

      behave like displayIndividualTriagePageBehaviorCompleteJourney(() => performAction())(
        sample[CompleteSingleDisposalTriageAnswers].copy(
          disposalMethod = DisposalMethod.Gifted,
          individualUserType = Some(Self)
        )
      )(
        "disposalMethod.title",
        { doc =>
          checkContent(
            doc,
            routes.SingleDisposalsTriageController.checkYourAnswers()
          )
          doc.select("#disposalMethod-1").hasAttr("checked")
        }
      )

      behave like displayIndividualTriagePageBehaviorCompleteJourney(() => performAction())(
        sample[CompleteSingleDisposalTriageAnswers].copy(
          disposalMethod = DisposalMethod.Gifted,
          individualUserType = Some(Capacitor)
        )
      )(
        "disposalMethod.capacitor.title",
        { doc =>
          checkContent(
            doc,
            routes.SingleDisposalsTriageController.checkYourAnswers()
          )
          doc.select("#disposalMethod-1").hasAttr("checked")
        }
      )

      behave like displayIndividualTriagePageBehaviorCompleteJourney(() => performAction())(
        sample[CompleteSingleDisposalTriageAnswers].copy(
          disposalMethod = DisposalMethod.Gifted,
          individualUserType = Some(PersonalRepresentative)
        )
      )(
        "disposalMethod.personalRep.title",
        { doc =>
          checkContent(
            doc,
            routes.SingleDisposalsTriageController.checkYourAnswers()
          )
          doc.select("#disposalMethod-1").hasAttr("checked")
        }
      )

      behave like displayCustomContentForAgentWithSelfAndTrustType(() => performAction())(
        requiredPreviousAnswers.copy(
          disposalMethod = Some(DisposalMethod.Sold)
        )
      )(
        "disposalMethod.agent.title",
        doc =>
          checkContent(
            doc,
            routes.CommonTriageQuestionsController.howManyProperties()
          )
      )

      behave like displayCustomContentForAgentWithSelfAndTrustType(() => performAction())(
        requiredPreviousAnswers.copy(
          disposalMethod = Some(DisposalMethod.Sold),
          individualUserType = Some(PersonalRepresentative)
        )
      )(
        "disposalMethod.personalRep.title",
        doc =>
          checkContent(
            doc,
            routes.CommonTriageQuestionsController.howManyProperties()
          )
      )

      behave like displayCustomContentForTrusts(() => performAction())(
        requiredPreviousAnswers.copy(disposalMethod = Some(DisposalMethod.Sold))
      )(
        "disposalMethod.trust.title",
        doc =>
          checkContent(
            doc,
            routes.CommonTriageQuestionsController.howManyProperties()
          )
      )

      def checkContent(doc: Document, backLink: Call): Unit = {
        doc.select("#back, .govuk-back-link").attr("href") shouldBe backLink.url
        doc
          .select("#content > article > form, #main-content form")
          .attr("action")                                  shouldBe routes.SingleDisposalsTriageController
          .howDidYouDisposeOfPropertySubmit()
          .url
      }

    }

    "handling submitted answers to the how did you dispose of your property page" must {

      def performAction(formData: (String, String)*): Future[Result] =
        controller.howDidYouDisposeOfPropertySubmit()(
          FakeRequest().withFormUrlEncodedBody(formData*).withMethod("POST")
        )

      def updateDraftReturn(
        d: DraftSingleDisposalReturn,
        newAnswers: SingleDisposalTriageAnswers
      ): DraftSingleDisposalReturn =
        d.copy(
          triageAnswers = newAnswers,
          disposalDetailsAnswers = d.disposalDetailsAnswers.map(
            _.unset(_.disposalPrice)
              .unset(_.disposalFees)
          ),
          initialGainOrLoss = None,
          reliefDetailsAnswers = None,
          exemptionAndLossesAnswers = None,
          yearToDateLiabilityAnswers = None,
          supportingEvidenceAnswers = None,
          gainOrLossAfterReliefs = None
        )

      val requiredPreviousAnswers = IncompleteSingleDisposalTriageAnswers.empty.copy(
        hasConfirmedSingleDisposal = true,
        individualUserType = Some(Self)
      )

      behave like redirectToStartWhenInvalidJourney(() => performAction(), isValidJourney)

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.howDidYouDisposeOfPropertySubmit(),
        mockUUIDGenerator
      )

      behave like redirectWhenNoPreviousAnswerBehaviour[NumberOfProperties](() => performAction())(
        requiredPreviousAnswers,
        routes.CommonTriageQuestionsController.howManyProperties(),
        { case (answers, n) =>
          answers.copy(
            hasConfirmedSingleDisposal = if (n.contains(NumberOfProperties.One)) true else false
          )
        }
      )

      "show a form error with self type" when {

        def test(formData: Seq[(String, String)], expectedErrorKey: String): Unit =
          testFormError(performAction, "disposalMethod.title")(
            formData,
            expectedErrorKey,
            requiredPreviousAnswers
          )

        "nothing is submitted" in {
          test(List.empty, "disposalMethod.error.required")
        }

        "the option is not recognised" in {
          test(List("disposalMethod" -> "3"), "disposalMethod.error.invalid")
        }

      }

      "show a form error with capacitor type" when {

        def test(formData: Seq[(String, String)], expectedErrorKey: String): Unit =
          testFormError(performAction, "disposalMethod.capacitor.title")(
            formData,
            expectedErrorKey,
            requiredPreviousAnswers.copy(
              individualUserType = Some(Capacitor)
            )
          )

        "nothing is submitted" in {
          test(List.empty, "disposalMethod.capacitor.error.required")
        }

        "the option is not recognised" in {
          test(
            List("disposalMethod" -> "3"),
            "disposalMethod.capacitor.error.invalid"
          )
        }

      }

      "show a form error with personal representative" when {

        def test(formData: Seq[(String, String)], expectedErrorKey: String): Unit =
          testFormError(performAction, "disposalMethod.personalRep.title")(
            formData,
            expectedErrorKey,
            requiredPreviousAnswers.copy(
              individualUserType = Some(PersonalRepresentative)
            )
          )

        "nothing is submitted" in {
          test(List.empty, "disposalMethod.personalRep.error.required")
        }

        "the option is not recognised" in {
          test(
            List("disposalMethod" -> "3"),
            "disposalMethod.personalRep.error.invalid"
          )
        }

      }

      behave like unsuccessfulUpdatesBehaviour(
        performAction,
        requiredPreviousAnswers,
        List("disposalMethod" -> "0"),
        requiredPreviousAnswers.copy(
          disposalMethod = Some(DisposalMethod.Sold)
        ),
        updateDraftReturn
      )

      "handle successful updates" when {

        "the user is starting a new draft return and" when {

          "the user has answered some questions but not complete the section" in {
            testSuccessfulUpdateStartingNewDraft(
              performAction,
              requiredPreviousAnswers,
              List("disposalMethod" -> "2"),
              requiredPreviousAnswers
                .copy(disposalMethod = Some(DisposalMethod.Other)),
              checkIsRedirect(
                _,
                routes.SingleDisposalsTriageController.checkYourAnswers()
              )
            )
          }

          "the user has complete the section" in {
            val completeAnswers = sample[CompleteSingleDisposalTriageAnswers]
            testSuccessfulUpdateStartingNewDraft(
              performAction,
              completeAnswers.copy(disposalMethod = DisposalMethod.Sold),
              List("disposalMethod" -> "1"),
              completeAnswers.copy(disposalMethod = DisposalMethod.Gifted),
              checkIsRedirect(
                _,
                routes.SingleDisposalsTriageController.checkYourAnswers()
              )
            )
          }

        }

        "the user is filling out a draft return and" when {

          def updateDraftReturn(
            d: DraftSingleDisposalReturn,
            newAnswers: SingleDisposalTriageAnswers
          ): DraftSingleDisposalReturn =
            d.copy(
              triageAnswers = newAnswers,
              disposalDetailsAnswers = d.disposalDetailsAnswers.map(
                _.unset(_.disposalPrice)
                  .unset(_.disposalFees)
              ),
              initialGainOrLoss = None,
              reliefDetailsAnswers = None,
              exemptionAndLossesAnswers = None,
              yearToDateLiabilityAnswers = None,
              supportingEvidenceAnswers = None,
              gainOrLossAfterReliefs = None
            )

          "the section is complete" in {
            forAll { (completeAnswers: CompleteSingleDisposalTriageAnswers) =>
              testSuccessfulUpdateFillingOutReturn(
                performAction,
                completeAnswers.copy(disposalMethod = DisposalMethod.Sold),
                List("disposalMethod" -> "1"),
                (fillingOutReturn, draftReturn) =>
                  fillingOutReturn
                    .copy(draftReturn =
                      updateDraftReturn(
                        draftReturn,
                        completeAnswers.copy(disposalMethod = DisposalMethod.Gifted)
                      )
                    )
                    .withForceDisplayGainOrLossAfterReliefsForAmends,
                checkIsRedirect(
                  _,
                  routes.SingleDisposalsTriageController.checkYourAnswers()
                ),
                amendReturnData = Some(sample[AmendReturnData])
              )
            }
          }

          "the section is incomplete" in {
            testSuccessfulUpdateFillingOutReturn(
              performAction,
              requiredPreviousAnswers,
              List("disposalMethod" -> "0"),
              (fillingOutReturn, draftReturn) =>
                fillingOutReturn.copy(draftReturn =
                  draftReturn.copy(
                    triageAnswers = requiredPreviousAnswers
                      .copy(disposalMethod = Some(DisposalMethod.Sold)),
                    disposalDetailsAnswers = draftReturn.disposalDetailsAnswers.map(
                      _.unset(_.disposalPrice)
                        .unset(_.disposalFees)
                    ),
                    initialGainOrLoss = None,
                    reliefDetailsAnswers = None,
                    exemptionAndLossesAnswers = None,
                    yearToDateLiabilityAnswers = None,
                    supportingEvidenceAnswers = None,
                    gainOrLossAfterReliefs = None
                  )
                ),
              checkIsRedirect(
                _,
                routes.SingleDisposalsTriageController.checkYourAnswers()
              )
            )
          }
        }
      }

      "not do any updates" when {

        "the answers submitted is the same as the one in session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithFillingOutReturn(
                requiredPreviousAnswers.copy(
                  disposalMethod = Some(DisposalMethod.Sold)
                )
              )._1
            )
          }

          checkIsRedirect(
            performAction("disposalMethod" -> "0"),
            routes.SingleDisposalsTriageController.checkYourAnswers()
          )
        }

      }

    }

    "handling requests to display the were you a uk resident page" must {

      val requiredPreviousAnswers =
        IncompleteSingleDisposalTriageAnswers.empty.copy(
          individualUserType = Some(IndividualUserType.Self),
          disposalMethod = Some(DisposalMethod.Sold)
        )

      def performAction(): Future[Result] =
        controller.wereYouAUKResident()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(() => performAction(), isValidJourney)

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.wereYouAUKResident(),
        mockUUIDGenerator
      )

      behave like redirectWhenNoPreviousAnswerBehaviour[DisposalMethod](() => performAction())(
        requiredPreviousAnswers,
        routes.SingleDisposalsTriageController.howDidYouDisposeOfProperty(),
        { case (answers, m) => answers.copy(disposalMethod = m) }
      )

      behave like displayIndividualTriagePageBehaviorIncompleteJourney(() => performAction())(
        requiredPreviousAnswers,
        requiredPreviousAnswers.copy(wasAUKResident = Some(false))
      )(
        "wereYouAUKResident.title",
        checkContent(
          _,
          routes.SingleDisposalsTriageController.howDidYouDisposeOfProperty()
        ),
        _.select("#wereYouAUKResident-false").hasAttr("checked")
      )

      behave like displayIndividualTriagePageBehaviorIncompleteJourney(() => performAction())(
        requiredPreviousAnswers
          .copy(individualUserType = Some(IndividualUserType.Capacitor)),
        requiredPreviousAnswers
          .copy(
            wasAUKResident = Some(false),
            individualUserType = Some(IndividualUserType.Capacitor)
          )
      )(
        "wereYouAUKResident.capacitor.title",
        checkContent(
          _,
          routes.SingleDisposalsTriageController.howDidYouDisposeOfProperty()
        ),
        _.select("#wereYouAUKResident-false").hasAttr("checked")
      )

      behave like displayIndividualTriagePageBehaviorIncompleteJourney(() => performAction())(
        requiredPreviousAnswers.copy(individualUserType = Some(IndividualUserType.PersonalRepresentative)),
        requiredPreviousAnswers
          .copy(
            wasAUKResident = Some(false),
            individualUserType = Some(IndividualUserType.PersonalRepresentative)
          )
      )(
        "wereYouAUKResident.personalRep.title",
        checkContent(
          _,
          routes.SingleDisposalsTriageController.howDidYouDisposeOfProperty()
        ),
        _.select("#wereYouAUKResident-false").hasAttr("checked")
      )

      behave like displayIndividualTriagePageBehaviorCompleteJourney(() => performAction())(
        sample[CompleteSingleDisposalTriageAnswers]
          .copy(
            countryOfResidence = Country.uk,
            individualUserType = Some(IndividualUserType.Self)
          )
      )(
        "wereYouAUKResident.title",
        { doc =>
          checkContent(
            doc,
            routes.SingleDisposalsTriageController.checkYourAnswers()
          )
          doc
            .select("#wereYouAUKResident-true")
            .hasAttr("checked")
        }
      )

      behave like displayIndividualTriagePageBehaviorCompleteJourney(() => performAction())(
        sample[CompleteSingleDisposalTriageAnswers]
          .copy(
            countryOfResidence = Country.uk,
            individualUserType = Some(IndividualUserType.Capacitor)
          )
      )(
        "wereYouAUKResident.capacitor.title",
        { doc =>
          checkContent(
            doc,
            routes.SingleDisposalsTriageController.checkYourAnswers()
          )
          doc
            .select("#wereYouAUKResident-true")
            .hasAttr("checked")
        }
      )

      behave like displayIndividualTriagePageBehaviorCompleteJourney(() => performAction())(
        sample[CompleteSingleDisposalTriageAnswers]
          .copy(
            countryOfResidence = Country.uk,
            individualUserType = Some(IndividualUserType.PersonalRepresentative)
          )
      )(
        "wereYouAUKResident.personalRep.title",
        { doc =>
          checkContent(
            doc,
            routes.SingleDisposalsTriageController.checkYourAnswers()
          )
          doc
            .select("#wereYouAUKResident-true")
            .hasAttr("checked")
        }
      )

      behave like displayCustomContentForAgentWithSelfAndTrustType(() => performAction())(
        requiredPreviousAnswers.copy(
          wasAUKResident = Some(true),
          countryOfResidence = Some(Country.uk)
        )
      )(
        "wereYouAUKResident.agent.title",
        doc =>
          checkContent(
            doc,
            routes.SingleDisposalsTriageController.howDidYouDisposeOfProperty()
          )
      )

      behave like displayCustomContentForAgentWithSelfAndTrustType(() => performAction())(
        requiredPreviousAnswers.copy(
          wasAUKResident = Some(true),
          countryOfResidence = Some(Country.uk),
          individualUserType = Some(IndividualUserType.PersonalRepresentative)
        )
      )(
        "wereYouAUKResident.personalRep.title",
        doc =>
          checkContent(
            doc,
            routes.SingleDisposalsTriageController.howDidYouDisposeOfProperty()
          )
      )

      behave like displayCustomContentForTrusts(() => performAction())(
        requiredPreviousAnswers.copy(
          wasAUKResident = Some(true),
          countryOfResidence = Some(Country.uk)
        )
      )(
        "wereYouAUKResident.trust.title",
        doc =>
          checkContent(
            doc,
            routes.SingleDisposalsTriageController.howDidYouDisposeOfProperty()
          )
      )

      def checkContent(doc: Document, backLink: Call): Unit = {
        doc.select("#back, .govuk-back-link").attr("href") shouldBe backLink.url
        doc
          .select("#content > article > form, #main-content form")
          .attr("action")                                  shouldBe routes.SingleDisposalsTriageController
          .wereYouAUKResidentSubmit()
          .url
      }

    }

    "handling submitted answers to the were you a uk resident page" must {

      def performAction(formData: (String, String)*): Future[Result] =
        controller.wereYouAUKResidentSubmit()(
          FakeRequest().withFormUrlEncodedBody(formData*).withMethod("POST")
        )

      def updateDraftReturn(
        d: DraftSingleDisposalReturn,
        newAnswers: SingleDisposalTriageAnswers
      ): DraftSingleDisposalReturn =
        d.copy(
          triageAnswers = newAnswers,
          propertyAddress = None,
          disposalDetailsAnswers = None,
          acquisitionDetailsAnswers = None,
          initialGainOrLoss = None,
          gainOrLossAfterReliefs = None,
          reliefDetailsAnswers = d.reliefDetailsAnswers.map(_.unsetPrrAndLettingRelief(newAnswers.isPeriodOfAdmin)),
          exemptionAndLossesAnswers = None,
          yearToDateLiabilityAnswers = None,
          supportingEvidenceAnswers = None
        )

      val requiredPreviousAnswers =
        IncompleteSingleDisposalTriageAnswers.empty.copy(
          disposalMethod = Some(DisposalMethod.Sold),
          individualUserType = Some(IndividualUserType.Self)
        )

      behave like redirectToStartWhenInvalidJourney(() => performAction(), isValidJourney)

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.wereYouAUKResidentSubmit(),
        mockUUIDGenerator
      )

      behave like redirectWhenNoPreviousAnswerBehaviour[DisposalMethod](() => performAction())(
        requiredPreviousAnswers,
        routes.SingleDisposalsTriageController.howDidYouDisposeOfProperty(),
        { case (answers, m) => answers.copy(disposalMethod = m) }
      )

      "show a form error for self type" when {

        def test(formData: Seq[(String, String)], expectedErrorKey: String): Unit =
          testFormError(performAction, "wereYouAUKResident.title")(
            formData,
            expectedErrorKey,
            requiredPreviousAnswers
          )

        "nothing is submitted" in {
          test(List.empty, "wereYouAUKResident.error.required")
        }

        "the option is not recognised" in {
          test(
            List("wereYouAUKResident" -> "3"),
            "wereYouAUKResident.error.boolean"
          )
        }

      }

      "show a form error for personal representative type" when {

        def test(formData: Seq[(String, String)], expectedErrorKey: String): Unit =
          testFormError(performAction, "wereYouAUKResident.personalRep.title")(
            formData,
            expectedErrorKey,
            requiredPreviousAnswers.copy(
              individualUserType = Some(IndividualUserType.PersonalRepresentative)
            )
          )

        "nothing is submitted" in {
          test(List.empty, "wereYouAUKResident.personalRep.error.required")
        }

        "the option is not recognised" in {
          test(
            List("wereYouAUKResident" -> "3"),
            "wereYouAUKResident.personalRep.error.boolean"
          )
        }

      }

      "show a form error for capacitor type" when {

        def test(formData: Seq[(String, String)], expectedErrorKey: String): Unit =
          testFormError(performAction, "wereYouAUKResident.capacitor.title")(
            formData,
            expectedErrorKey,
            requiredPreviousAnswers.copy(
              individualUserType = Some(IndividualUserType.Capacitor)
            )
          )

        "nothing is submitted" in {
          test(List.empty, "wereYouAUKResident.capacitor.error.required")
        }

        "the option is not recognised" in {
          test(
            List("wereYouAUKResident" -> "3"),
            "wereYouAUKResident.capacitor.error.boolean"
          )
        }

      }

      behave like unsuccessfulUpdatesBehaviour(
        performAction,
        requiredPreviousAnswers,
        List("wereYouAUKResident" -> "true"),
        requiredPreviousAnswers.copy(wasAUKResident = Some(true)),
        updateDraftReturn
      )

      "handle successful updates" when {

        "the user is starting a new draft return and" when {

          "the user has answered some questions but not complete the section and has " +
            "changed their answer from not in the uk to was in the uk" in {
              val answers = requiredPreviousAnswers.copy(
                wasAUKResident = Some(false),
                countryOfResidence = Some(Country("AB")),
                assetType = Some(AssetType.Residential),
                disposalMethod = Some(DisposalMethod.Sold)
              )

              testSuccessfulUpdateStartingNewDraft(
                performAction,
                answers,
                List("wereYouAUKResident" -> "true"),
                requiredPreviousAnswers.copy(
                  wasAUKResident = Some(true),
                  countryOfResidence = None,
                  assetType = None
                ),
                checkIsRedirect(
                  _,
                  routes.SingleDisposalsTriageController.checkYourAnswers()
                )
              )
            }

          "the user has answered some questions but not complete the section and has " +
            "changed their answer from was in the uk to not in the uk" in {
              val answers = requiredPreviousAnswers.copy(
                wasAUKResident = Some(true),
                countryOfResidence = None,
                assetType = Some(AssetType.Residential),
                disposalMethod = Some(DisposalMethod.Sold)
              )

              testSuccessfulUpdateStartingNewDraft(
                performAction,
                answers,
                List("wereYouAUKResident" -> "false"),
                requiredPreviousAnswers.copy(
                  wasAUKResident = Some(false),
                  countryOfResidence = None,
                  assetType = None
                ),
                checkIsRedirect(
                  _,
                  routes.SingleDisposalsTriageController.checkYourAnswers()
                )
              )
            }

          "the user has complete the section and has changed their answer from " +
            "not in the uk to was in the uk" in {
              val completeAnswers = sample[CompleteSingleDisposalTriageAnswers].copy(
                disposalMethod = DisposalMethod.Sold
              )
              testSuccessfulUpdateStartingNewDraft(
                performAction,
                completeAnswers.copy(countryOfResidence = Country("AB")),
                List("wereYouAUKResident" -> "true"),
                IncompleteSingleDisposalTriageAnswers(
                  completeAnswers.individualUserType,
                  hasConfirmedSingleDisposal = true,
                  Some(DisposalMethod.Sold),
                  Some(true),
                  None,
                  None,
                  Some(completeAnswers.disposalDate),
                  completeAnswers.alreadySentSelfAssessment,
                  Some(completeAnswers.completionDate),
                  None
                ),
                checkIsRedirect(
                  _,
                  routes.SingleDisposalsTriageController.checkYourAnswers()
                )
              )
            }

          "the user has completed the section and has changed their answer from " +
            "was in the uk to not in the uk" in {
              val completeAnswers = sample[CompleteSingleDisposalTriageAnswers].copy(
                disposalMethod = DisposalMethod.Sold
              )
              testSuccessfulUpdateStartingNewDraft(
                performAction,
                completeAnswers.copy(countryOfResidence = Country.uk),
                List("wereYouAUKResident" -> "false"),
                IncompleteSingleDisposalTriageAnswers(
                  completeAnswers.individualUserType,
                  hasConfirmedSingleDisposal = true,
                  Some(DisposalMethod.Sold),
                  Some(false),
                  None,
                  None,
                  Some(completeAnswers.disposalDate),
                  completeAnswers.alreadySentSelfAssessment,
                  Some(completeAnswers.completionDate),
                  None
                ),
                checkIsRedirect(
                  _,
                  routes.SingleDisposalsTriageController.checkYourAnswers()
                )
              )
            }

        }

        "the user is filling out a draft return and" when {

          "the section is complete" in {
            forAll { (completeAnswers: CompleteSingleDisposalTriageAnswers) =>
              val newAnswers =
                IncompleteSingleDisposalTriageAnswers(
                  completeAnswers.individualUserType,
                  hasConfirmedSingleDisposal = true,
                  Some(DisposalMethod.Sold),
                  Some(true),
                  None,
                  None,
                  Some(completeAnswers.disposalDate),
                  completeAnswers.alreadySentSelfAssessment,
                  Some(completeAnswers.completionDate),
                  None
                )
              testSuccessfulUpdateFillingOutReturn(
                performAction,
                completeAnswers.copy(
                  countryOfResidence = Country("AB"),
                  disposalMethod = DisposalMethod.Sold
                ),
                List("wereYouAUKResident" -> "true"),
                (fillingOutReturn, draftReturn) =>
                  fillingOutReturn.copy(draftReturn = updateDraftReturn(draftReturn, newAnswers)),
                checkIsRedirect(
                  _,
                  routes.SingleDisposalsTriageController.checkYourAnswers()
                )
              )
            }
          }

          "the section is incomplete" in {
            val answers = requiredPreviousAnswers.copy(
              wasAUKResident = Some(true),
              countryOfResidence = None,
              assetType = Some(AssetType.Residential),
              disposalMethod = Some(DisposalMethod.Sold)
            )

            testSuccessfulUpdateFillingOutReturn(
              performAction,
              answers,
              List("wereYouAUKResident" -> "false"),
              (fillingOutReturn, draftReturn) =>
                fillingOutReturn.copy(draftReturn =
                  updateDraftReturn(
                    draftReturn,
                    requiredPreviousAnswers.copy(
                      wasAUKResident = Some(false),
                      countryOfResidence = None,
                      assetType = None
                    )
                  )
                ),
              checkIsRedirect(
                _,
                routes.SingleDisposalsTriageController.checkYourAnswers()
              )
            )
          }

        }

      }

      "not do any updates" when {

        "the answers submitted is the same as the one in session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithFillingOutReturn(
                requiredPreviousAnswers.copy(
                  wasAUKResident = Some(true)
                )
              )._1
            )
          }

          checkIsRedirect(
            performAction("wereYouAUKResident" -> "true"),
            routes.SingleDisposalsTriageController.checkYourAnswers()
          )
        }

      }

    }

    "handling requests to display the did you dispose of a residential property page" must {

      val requiredPreviousAnswers =
        IncompleteSingleDisposalTriageAnswers.empty
          .copy(wasAUKResident = Some(true))

      def performAction(): Future[Result] =
        controller.didYouDisposeOfAResidentialProperty()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(() => performAction(), isValidJourney)

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.didYouDisposeOfAResidentialProperty(),
        mockUUIDGenerator
      )

      behave like redirectWhenNoPreviousAnswerBehaviour[Boolean](() => performAction())(
        requiredPreviousAnswers,
        routes.SingleDisposalsTriageController.wereYouAUKResident(),
        { case (answers, w) => answers.copy(wasAUKResident = w) }
      )

      behave like displayIndividualTriagePageBehaviorIncompleteJourney(() => performAction())(
        requiredPreviousAnswers,
        requiredPreviousAnswers.copy(assetType = Some(AssetType.NonResidential))
      )(
        "didYouDisposeOfResidentialProperty.title",
        checkContent(
          _,
          routes.SingleDisposalsTriageController.wereYouAUKResident()
        ),
        _.select("#didYouDisposeOfResidentialProperty-false")
          .hasAttr("checked")
      )

      behave like displayIndividualTriagePageBehaviorIncompleteJourney(() => performAction())(
        requiredPreviousAnswers.copy(
          individualUserType = Some(IndividualUserType.Capacitor)
        ),
        requiredPreviousAnswers.copy(
          assetType = Some(AssetType.NonResidential),
          individualUserType = Some(IndividualUserType.Capacitor)
        )
      )(
        "didYouDisposeOfResidentialProperty.capacitor.title",
        checkContent(
          _,
          routes.SingleDisposalsTriageController.wereYouAUKResident()
        ),
        _.select("#didYouDisposeOfResidentialProperty-false")
          .hasAttr("checked")
      )

      behave like displayIndividualTriagePageBehaviorIncompleteJourney(() => performAction())(
        requiredPreviousAnswers.copy(
          individualUserType = Some(IndividualUserType.PersonalRepresentative)
        ),
        requiredPreviousAnswers.copy(
          assetType = Some(AssetType.NonResidential),
          individualUserType = Some(IndividualUserType.PersonalRepresentative)
        )
      )(
        "didYouDisposeOfResidentialProperty.personalRep.title",
        checkContent(
          _,
          routes.SingleDisposalsTriageController.wereYouAUKResident()
        ),
        _.select("#didYouDisposeOfResidentialProperty-false")
          .hasAttr("checked")
      )

      behave like displayIndividualTriagePageBehaviorCompleteJourney(() => performAction())(
        sample[CompleteSingleDisposalTriageAnswers].copy(
          individualUserType = Some(IndividualUserType.Self),
          countryOfResidence = Country.uk,
          assetType = AssetType.Residential
        )
      )(
        "didYouDisposeOfResidentialProperty.title",
        { doc =>
          checkContent(
            doc,
            routes.SingleDisposalsTriageController.checkYourAnswers()
          )
          doc
            .select("#didYouDisposeOfResidentialProperty-true")
            .hasAttr("checked")
        }
      )

      behave like displayIndividualTriagePageBehaviorCompleteJourney(() => performAction())(
        sample[CompleteSingleDisposalTriageAnswers].copy(
          individualUserType = Some(IndividualUserType.Capacitor),
          countryOfResidence = Country.uk,
          assetType = AssetType.Residential
        )
      )(
        "didYouDisposeOfResidentialProperty.capacitor.title",
        { doc =>
          checkContent(
            doc,
            routes.SingleDisposalsTriageController.checkYourAnswers()
          )
          doc
            .select("#didYouDisposeOfResidentialProperty-true")
            .hasAttr("checked")
        }
      )

      behave like displayIndividualTriagePageBehaviorCompleteJourney(() => performAction())(
        sample[CompleteSingleDisposalTriageAnswers].copy(
          individualUserType = Some(IndividualUserType.PersonalRepresentative),
          countryOfResidence = Country.uk,
          assetType = AssetType.Residential
        )
      )(
        "didYouDisposeOfResidentialProperty.personalRep.title",
        { doc =>
          checkContent(
            doc,
            routes.SingleDisposalsTriageController.checkYourAnswers()
          )
          doc
            .select("#didYouDisposeOfResidentialProperty-true")
            .hasAttr("checked")
        }
      )

      behave like displayCustomContentForAgentWithSelfAndTrustType(() => performAction())(
        requiredPreviousAnswers.copy(assetType = Some(AssetType.Residential))
      )(
        "didYouDisposeOfResidentialProperty.agent.title",
        doc =>
          checkContent(
            doc,
            routes.SingleDisposalsTriageController.wereYouAUKResident()
          )
      )

      behave like displayCustomContentForAgentWithSelfAndTrustType(() => performAction())(
        requiredPreviousAnswers.copy(
          assetType = Some(AssetType.Residential),
          individualUserType = Some(IndividualUserType.PersonalRepresentative)
        )
      )(
        "didYouDisposeOfResidentialProperty.personalRep.title",
        doc =>
          checkContent(
            doc,
            routes.SingleDisposalsTriageController.wereYouAUKResident()
          )
      )

      behave like displayCustomContentForTrusts(() => performAction())(
        requiredPreviousAnswers.copy(assetType = Some(AssetType.Residential))
      )(
        "didYouDisposeOfResidentialProperty.trust.title",
        doc =>
          checkContent(
            doc,
            routes.SingleDisposalsTriageController.wereYouAUKResident()
          )
      )

      def checkContent(doc: Document, backLink: Call): Unit = {
        doc.select("#back, .govuk-back-link").attr("href") shouldBe backLink.url
        doc
          .select("#content > article > form, #main-content form")
          .attr("action")                                  shouldBe routes.SingleDisposalsTriageController
          .didYouDisposeOfAResidentialPropertySubmit()
          .url

      }

    }

    "handling submitted answers to the did you dispose of a residential property page" must {

      def performAction(formData: (String, String)*): Future[Result] =
        controller.didYouDisposeOfAResidentialPropertySubmit()(
          FakeRequest().withFormUrlEncodedBody(formData*).withMethod("POST")
        )

      val requiredPreviousAnswers =
        IncompleteSingleDisposalTriageAnswers.empty.copy(
          wasAUKResident = Some(true),
          individualUserType = Some(IndividualUserType.Self)
        )

      behave like redirectToStartWhenInvalidJourney(() => performAction(), isValidJourney)

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.didYouDisposeOfAResidentialPropertySubmit(),
        mockUUIDGenerator
      )

      behave like redirectWhenNoPreviousAnswerBehaviour[Boolean](() => performAction())(
        requiredPreviousAnswers,
        routes.SingleDisposalsTriageController.wereYouAUKResident(),
        { case (answers, w) => answers.copy(wasAUKResident = w) }
      )

      "show a form error fro self type" when {

        def test(formData: Seq[(String, String)], expectedErrorKey: String): Unit =
          testFormError(
            performAction,
            "didYouDisposeOfResidentialProperty.title"
          )(
            formData,
            expectedErrorKey,
            requiredPreviousAnswers
          )

        "nothing is submitted" in {
          test(List.empty, "didYouDisposeOfResidentialProperty.error.required")
        }

        "the option is not recognised" in {
          test(
            List("didYouDisposeOfResidentialProperty" -> "3"),
            "didYouDisposeOfResidentialProperty.error.boolean"
          )
        }

      }

      "show a form error fro capacitor type" when {

        def test(formData: Seq[(String, String)], expectedErrorKey: String): Unit =
          testFormError(
            performAction,
            "didYouDisposeOfResidentialProperty.capacitor.title"
          )(
            formData,
            expectedErrorKey,
            requiredPreviousAnswers.copy(
              individualUserType = Some(IndividualUserType.Capacitor)
            )
          )

        "nothing is submitted" in {
          test(
            List.empty,
            "didYouDisposeOfResidentialProperty.capacitor.error.required"
          )
        }

        "the option is not recognised" in {
          test(
            List("didYouDisposeOfResidentialProperty" -> "3"),
            "didYouDisposeOfResidentialProperty.capacitor.error.boolean"
          )
        }

      }

      "show a form error fro personal representative type" when {

        def test(formData: Seq[(String, String)], expectedErrorKey: String): Unit =
          testFormError(
            performAction,
            "didYouDisposeOfResidentialProperty.personalRep.title"
          )(
            formData,
            expectedErrorKey,
            requiredPreviousAnswers.copy(
              individualUserType = Some(IndividualUserType.PersonalRepresentative)
            )
          )

        "nothing is submitted" in {
          test(
            List.empty,
            "didYouDisposeOfResidentialProperty.personalRep.error.required"
          )
        }

        "the option is not recognised" in {
          test(
            List("didYouDisposeOfResidentialProperty" -> "3"),
            "didYouDisposeOfResidentialProperty.personalRep.error.boolean"
          )
        }

      }

      behave like unsuccessfulUpdatesBehaviour(
        performAction,
        requiredPreviousAnswers,
        List("didYouDisposeOfResidentialProperty" -> "true"),
        requiredPreviousAnswers.copy(assetType = Some(AssetType.Residential)),
        { case (draftReturn, answers) =>
          draftReturn.copy(triageAnswers = answers)
        }
      )

      "handle successful updates" when {

        "the user is starting a new draft return and" when {

          "the user has answered some questions but not complete the section" in {
            testSuccessfulUpdateStartingNewDraft(
              performAction,
              requiredPreviousAnswers,
              List("didYouDisposeOfResidentialProperty" -> "true"),
              requiredPreviousAnswers
                .copy(assetType = Some(AssetType.Residential)),
              checkIsRedirect(
                _,
                routes.SingleDisposalsTriageController.checkYourAnswers()
              )
            )
          }

          "the user has complete the section and has changed their answer and they choose asset type non-residential" in {
            val completeAnswers = sample[CompleteSingleDisposalTriageAnswers].copy(
              disposalMethod = DisposalMethod.Sold
            )
            testSuccessfulUpdateStartingNewDraft(
              performAction,
              completeAnswers.copy(assetType = Residential),
              List("didYouDisposeOfResidentialProperty" -> "false"),
              IncompleteSingleDisposalTriageAnswers
                .fromCompleteAnswers(completeAnswers)
                .copy(assetType = Some(NonResidential)),
              checkIsRedirect(
                _,
                routes.SingleDisposalsTriageController.checkYourAnswers()
              )
            )
          }

          "the user has complete the section and has changed their answer and they choose asset type residential" in {
            val completeAnswers = sample[CompleteSingleDisposalTriageAnswers].copy(
              disposalMethod = DisposalMethod.Sold
            )
            testSuccessfulUpdateStartingNewDraft(
              performAction,
              completeAnswers.copy(assetType = NonResidential),
              List("didYouDisposeOfResidentialProperty" -> "true"),
              IncompleteSingleDisposalTriageAnswers
                .fromCompleteAnswers(completeAnswers)
                .copy(assetType = Some(Residential)),
              checkIsRedirect(
                _,
                routes.SingleDisposalsTriageController.checkYourAnswers()
              )
            )
          }

        }

        "the user is filling out a draft return and" when {

          "the user has answered some questions but not complete the section" in {
            testSuccessfulUpdateFillingOutReturn(
              performAction,
              requiredPreviousAnswers,
              List("didYouDisposeOfResidentialProperty" -> "true"),
              (fillingOutReturn, draftReturn) =>
                fillingOutReturn.copy(draftReturn =
                  draftReturn.copy(triageAnswers =
                    requiredPreviousAnswers
                      .copy(assetType = Some(AssetType.Residential))
                  )
                ),
              checkIsRedirect(
                _,
                routes.SingleDisposalsTriageController.checkYourAnswers()
              )
            )
          }

          "the user has complete the section and has changed their answer from not in the uk to was in the uk" in {
            forAll { (completeAnswers: CompleteSingleDisposalTriageAnswers) =>
              testSuccessfulUpdateFillingOutReturn(
                performAction,
                completeAnswers.copy(
                  assetType = Residential,
                  disposalMethod = DisposalMethod.Sold
                ),
                List("didYouDisposeOfResidentialProperty" -> "false"),
                (fillingOutReturn, draftReturn) =>
                  fillingOutReturn.copy(draftReturn =
                    draftReturn.copy(triageAnswers =
                      IncompleteSingleDisposalTriageAnswers
                        .fromCompleteAnswers(completeAnswers)
                        .copy(
                          assetType = Some(NonResidential),
                          disposalMethod = Some(DisposalMethod.Sold)
                        )
                    )
                  ),
                checkIsRedirect(
                  _,
                  routes.SingleDisposalsTriageController.checkYourAnswers()
                )
              )
            }
          }

          "the user was on an indirect disposal journey" in {
            val draftReturn      = sample[DraftSingleIndirectDisposalReturn].copy(
              triageAnswers = requiredPreviousAnswers
            )
            val fillingOutReturn = sample[FillingOutReturn].copy(draftReturn = draftReturn)

            val updatedDraftReturn = DraftSingleDisposalReturn.newDraftReturn(
              draftReturn.id,
              requiredPreviousAnswers.copy(assetType = Some(AssetType.Residential)),
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
              performAction("didYouDisposeOfResidentialProperty" -> "true"),
              routes.SingleDisposalsTriageController.checkYourAnswers()
            )
          }

          "the user was on a mixed use disposal journey" in {
            val draftReturn      = sample[DraftSingleMixedUseDisposalReturn].copy(
              triageAnswers = requiredPreviousAnswers
            )
            val fillingOutReturn = sample[FillingOutReturn].copy(draftReturn = draftReturn)

            val updatedDraftReturn = DraftSingleDisposalReturn.newDraftReturn(
              draftReturn.id,
              requiredPreviousAnswers.copy(assetType = Some(AssetType.Residential)),
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
              performAction("didYouDisposeOfResidentialProperty" -> "true"),
              routes.SingleDisposalsTriageController.checkYourAnswers()
            )
          }

        }

      }

      "not do any updates" when {

        "the answers submitted is the same as the one in session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithFillingOutReturn(
                requiredPreviousAnswers.copy(
                  assetType = Some(Residential)
                )
              )._1
            )
          }

          checkIsRedirect(
            performAction("didYouDisposeOfResidentialProperty" -> "true"),
            routes.SingleDisposalsTriageController.checkYourAnswers()
          )
        }

      }

    }

    "handling requests to display the when was disposal date page" must {

      val requiredPreviousAnswersUkResident =
        IncompleteSingleDisposalTriageAnswers.empty.copy(
          individualUserType = Some(Self),
          hasConfirmedSingleDisposal = true,
          disposalMethod = Some(DisposalMethod.Gifted),
          wasAUKResident = Some(true),
          assetType = Some(AssetType.Residential)
        )

      val disposalDate = DisposalDate(LocalDate.of(2020, 1, 2), sample[TaxYear])

      def performAction(): Future[Result] =
        controller.whenWasDisposalDate()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(() => performAction(), isValidJourney)

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.whenWasDisposalDate(),
        mockUUIDGenerator
      )

      behave like noDateOfDeathForPersonalRepBehaviour(() => performAction())

      behave like redirectWhenNoPreviousAnswerBehaviour[Boolean](() => performAction())(
        requiredPreviousAnswersUkResident,
        routes.SingleDisposalsTriageController
          .didYouDisposeOfAResidentialProperty(),
        { case (answers, w) =>
          answers.copy(assetType = w.map(if (_) AssetType.Residential else AssetType.NonResidential))
        }
      )

      behave like displayIndividualTriagePageBehaviorIncompleteJourney(() => performAction())(
        requiredPreviousAnswersUkResident,
        requiredPreviousAnswersUkResident.copy(
          disposalDate = Some(disposalDate)
        ),
        Some("the user was a uk resident")
      )(
        "disposalDate.title",
        checkContent(
          _,
          routes.SingleDisposalsTriageController
            .didYouDisposeOfAResidentialProperty()
        ),
        checkPrepopulatedContent
      )

      behave like displayIndividualTriagePageBehaviorIncompleteJourney(() => performAction())(
        requiredPreviousAnswersUkResident.copy(
          individualUserType = Some(Capacitor)
        ),
        requiredPreviousAnswersUkResident.copy(
          disposalDate = Some(disposalDate),
          individualUserType = Some(Capacitor)
        ),
        Some("the user was a uk resident")
      )(
        "disposalDate.capacitor.title",
        checkContent(
          _,
          routes.SingleDisposalsTriageController
            .didYouDisposeOfAResidentialProperty()
        ),
        checkPrepopulatedContent
      )

      behave like displayIndividualTriagePageBehaviorIncompleteJourney(() => performAction())(
        requiredPreviousAnswersUkResident.copy(
          individualUserType = Some(PersonalRepresentative)
        ),
        requiredPreviousAnswersUkResident.copy(
          disposalDate = Some(disposalDate),
          individualUserType = Some(PersonalRepresentative)
        ),
        Some("the user was a uk resident"),
        Some(sample[CompleteRepresenteeAnswers].copy(dateOfDeath = Some(sample[DateOfDeath])))
      )(
        "disposalDate.personalRep.title",
        checkContent(
          _,
          routes.SingleDisposalsTriageController
            .didYouDisposeOfAResidentialProperty()
        ),
        checkPrepopulatedContent
      )

      behave like displayIndividualTriagePageBehaviorIncompleteJourney(() => performAction())(
        requiredPreviousAnswersUkResident.copy(
          wasAUKResident = Some(false),
          countryOfResidence = Some(sample[Country]),
          assetType = Some(AssetType.Residential)
        ),
        requiredPreviousAnswersUkResident.copy(
          wasAUKResident = Some(false),
          countryOfResidence = Some(sample[Country]),
          assetType = Some(AssetType.Residential),
          disposalDate = Some(disposalDate)
        ),
        Some("the user was not a uk resident")
      )(
        "disposalDate.title",
        checkContent(
          _,
          routes.SingleDisposalsTriageController.assetTypeForNonUkResidents()
        ),
        checkPrepopulatedContent
      )

      behave like displayIndividualTriagePageBehaviorIncompleteJourney(() => performAction())(
        requiredPreviousAnswersUkResident.copy(
          wasAUKResident = Some(false),
          countryOfResidence = Some(sample[Country]),
          assetType = Some(AssetType.Residential),
          individualUserType = Some(Capacitor)
        ),
        requiredPreviousAnswersUkResident.copy(
          wasAUKResident = Some(false),
          countryOfResidence = Some(sample[Country]),
          assetType = Some(AssetType.Residential),
          disposalDate = Some(disposalDate),
          individualUserType = Some(Capacitor)
        ),
        Some("the user was not a uk resident")
      )(
        "disposalDate.capacitor.title",
        checkContent(
          _,
          routes.SingleDisposalsTriageController.assetTypeForNonUkResidents()
        ),
        checkPrepopulatedContent
      )

      behave like displayIndividualTriagePageBehaviorIncompleteJourney(() => performAction())(
        requiredPreviousAnswersUkResident.copy(
          wasAUKResident = Some(false),
          countryOfResidence = Some(sample[Country]),
          assetType = Some(AssetType.Residential),
          individualUserType = Some(PersonalRepresentative)
        ),
        requiredPreviousAnswersUkResident.copy(
          wasAUKResident = Some(false),
          countryOfResidence = Some(sample[Country]),
          assetType = Some(AssetType.Residential),
          disposalDate = Some(disposalDate),
          individualUserType = Some(PersonalRepresentative)
        ),
        Some("the user was not a uk resident"),
        Some(sample[CompleteRepresenteeAnswers].copy(dateOfDeath = Some(sample[DateOfDeath])))
      )(
        "disposalDate.personalRep.title",
        checkContent(
          _,
          routes.SingleDisposalsTriageController.assetTypeForNonUkResidents()
        ),
        checkPrepopulatedContent
      )

      behave like displayIndividualTriagePageBehaviorIncompleteJourney(() => performAction())(
        requiredPreviousAnswersUkResident,
        requiredPreviousAnswersUkResident
          .copy(tooEarlyDisposalDate = Some(disposalDate.value)),
        Some("the user had not disposed of their property in a valid tax year")
      )(
        "disposalDate.title",
        checkContent(
          _,
          routes.SingleDisposalsTriageController
            .didYouDisposeOfAResidentialProperty()
        ),
        checkPrepopulatedContent
      )

      behave like displayIndividualTriagePageBehaviorIncompleteJourney(() => performAction())(
        requiredPreviousAnswersUkResident.copy(
          individualUserType = Some(Capacitor)
        ),
        requiredPreviousAnswersUkResident.copy(
          tooEarlyDisposalDate = Some(disposalDate.value),
          individualUserType = Some(Capacitor)
        ),
        Some("the user had not disposed of their property in a valid tax year")
      )(
        "disposalDate.capacitor.title",
        checkContent(
          _,
          routes.SingleDisposalsTriageController
            .didYouDisposeOfAResidentialProperty()
        ),
        checkPrepopulatedContent
      )

      behave like displayIndividualTriagePageBehaviorIncompleteJourney(() => performAction())(
        requiredPreviousAnswersUkResident.copy(
          individualUserType = Some(PersonalRepresentative)
        ),
        requiredPreviousAnswersUkResident.copy(
          tooEarlyDisposalDate = Some(disposalDate.value),
          individualUserType = Some(PersonalRepresentative)
        ),
        Some("the user had not disposed of their property in a valid tax year"),
        Some(sample[CompleteRepresenteeAnswers].copy(dateOfDeath = Some(sample[DateOfDeath])))
      )(
        "disposalDate.personalRep.title",
        checkContent(
          _,
          routes.SingleDisposalsTriageController
            .didYouDisposeOfAResidentialProperty()
        ),
        checkPrepopulatedContent
      )

      behave like displayIndividualTriagePageBehaviorCompleteJourney(() => performAction())(
        sample[CompleteSingleDisposalTriageAnswers].copy(
          countryOfResidence = Country.uk,
          assetType = AssetType.Residential,
          disposalDate = disposalDate,
          individualUserType = Some(Self)
        )
      )(
        "disposalDate.title",
        { doc =>
          checkContent(
            doc,
            routes.SingleDisposalsTriageController.checkYourAnswers()
          )
          checkPrepopulatedContent(doc)
        }
      )

      behave like displayIndividualTriagePageBehaviorCompleteJourney(() => performAction())(
        sample[CompleteSingleDisposalTriageAnswers].copy(
          countryOfResidence = Country.uk,
          assetType = AssetType.Residential,
          disposalDate = disposalDate,
          individualUserType = Some(Capacitor)
        )
      )(
        "disposalDate.capacitor.title",
        { doc =>
          checkContent(
            doc,
            routes.SingleDisposalsTriageController.checkYourAnswers()
          )
          checkPrepopulatedContent(doc)
        }
      )

      behave like displayIndividualTriagePageBehaviorCompleteJourney(() => performAction())(
        sample[CompleteSingleDisposalTriageAnswers].copy(
          countryOfResidence = Country.uk,
          assetType = AssetType.Residential,
          disposalDate = disposalDate,
          individualUserType = Some(PersonalRepresentative)
        ),
        Some(sample[CompleteRepresenteeAnswers].copy(dateOfDeath = Some(sample[DateOfDeath])))
      )(
        "disposalDate.personalRep.title",
        { doc =>
          checkContent(
            doc,
            routes.SingleDisposalsTriageController.checkYourAnswers()
          )
          checkPrepopulatedContent(doc)
        }
      )

      behave like displayCustomContentForAgentWithSelfAndTrustType(() => performAction())(
        requiredPreviousAnswersUkResident
          .copy(disposalDate = Some(disposalDate))
      )(
        "disposalDate.agent.title",
        doc =>
          checkContent(
            doc,
            routes.SingleDisposalsTriageController
              .didYouDisposeOfAResidentialProperty()
          )
      )

      behave like displayCustomContentForAgentWithSelfAndTrustType(() => performAction())(
        requiredPreviousAnswersUkResident.copy(
          disposalDate = Some(disposalDate),
          individualUserType = Some(PersonalRepresentative)
        ),
        Some(sample[CompleteRepresenteeAnswers].copy(dateOfDeath = Some(sample[DateOfDeath])))
      )(
        "disposalDate.personalRep.title",
        doc =>
          checkContent(
            doc,
            routes.SingleDisposalsTriageController
              .didYouDisposeOfAResidentialProperty()
          )
      )

      behave like displayCustomContentForTrusts(() => performAction())(
        requiredPreviousAnswersUkResident
          .copy(disposalDate = Some(disposalDate))
      )(
        "disposalDate.trust.title",
        doc =>
          checkContent(
            doc,
            routes.SingleDisposalsTriageController
              .didYouDisposeOfAResidentialProperty()
          )
      )

      def checkContent(doc: Document, backLink: Call): Unit = {
        doc.select("#back, .govuk-back-link").attr("href") shouldBe backLink.url
        doc
          .select("#content > article > form, #main-content form")
          .attr("action")                                  shouldBe routes.SingleDisposalsTriageController
          .whenWasDisposalDateSubmit()
          .url
      }

      def checkPrepopulatedContent(doc: Document): Assertion = {
        doc
          .select("#disposalDate-day")
          .attr("value") shouldBe disposalDate.value.getDayOfMonth.toString
        doc
          .select("#disposalDate-month")
          .attr("value") shouldBe disposalDate.value.getMonthValue.toString
        doc
          .select("#disposalDate-year")
          .attr("value") shouldBe disposalDate.value.getYear.toString
      }

    }

    "handling submitted disposal dates" must {

      def performAction(formData: (String, String)*): Future[Result] =
        controller.whenWasDisposalDateSubmit()(
          FakeRequest().withFormUrlEncodedBody(formData*).withMethod("POST")
        )

      def formData(date: LocalDate): Seq[(String, String)] =
        List(
          "disposalDate-day"   -> date.getDayOfMonth.toString,
          "disposalDate-month" -> date.getMonthValue.toString,
          "disposalDate-year"  -> date.getYear.toString
        )

      def updateDraftReturn(
        d: DraftSingleDisposalReturn,
        newAnswers: SingleDisposalTriageAnswers
      ): DraftSingleDisposalReturn =
        d.copy(
          triageAnswers = newAnswers,
          acquisitionDetailsAnswers = d.acquisitionDetailsAnswers.map(_.unsetAllButAcquisitionMethod(newAnswers)),
          initialGainOrLoss = None,
          reliefDetailsAnswers = d.reliefDetailsAnswers.map(_.unsetPrrAndLettingRelief(newAnswers.isPeriodOfAdmin)),
          yearToDateLiabilityAnswers = d.yearToDateLiabilityAnswers.flatMap {
            case _: NonCalculatedYTDAnswers => None
            case c: CalculatedYTDAnswers    =>
              Some(
                c.unset(_.hasEstimatedDetails)
                  .unset(_.calculatedTaxDue)
                  .unset(_.taxDue)
                  .unset(_.mandatoryEvidence)
                  .unset(_.expiredEvidence)
                  .unset(_.pendingUpscanUpload)
              )
          },
          supportingEvidenceAnswers = None,
          gainOrLossAfterReliefs = None
        )

      val tomorrow = TimeUtils
        .getMaximumDateForDisposalsAndCompletion(
          viewConfig.futureDatesEnabled,
          viewConfig.maxYearForDisposalsAndCompletion
        )
        .plusDays(1L)

      val requiredPreviousAnswers =
        IncompleteSingleDisposalTriageAnswers.empty.copy(
          individualUserType = Some(Self),
          hasConfirmedSingleDisposal = true,
          disposalMethod = Some(DisposalMethod.Gifted),
          wasAUKResident = Some(true),
          assetType = Some(AssetType.Residential)
        )

      behave like redirectToStartWhenInvalidJourney(() => performAction(), isValidJourney)

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.whenWasDisposalDateSubmit(),
        mockUUIDGenerator
      )

      behave like redirectWhenNoPreviousAnswerBehaviour[Boolean](() => performAction())(
        requiredPreviousAnswers,
        routes.SingleDisposalsTriageController
          .didYouDisposeOfAResidentialProperty(),
        { case (answers, w) =>
          answers.copy(assetType = w.map(if (_) AssetType.Residential else AssetType.NonResidential))
        }
      )

      "show an error page" when {

        "there is a problem getting the tax year" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithFillingOutReturn(requiredPreviousAnswers)._1
            )
            mockGetTaxYear(today)(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction(formData(today)*))
        }

      }

      "show a form error with self type" when {

        def test(formData: Seq[(String, String)], expectedErrorKey: String): Unit =
          testFormError(performAction, "disposalDate.title")(
            formData,
            expectedErrorKey,
            requiredPreviousAnswers
          )

        "the date is invalid" in {
          dateErrorScenarios("disposalDate", "").foreach { scenario =>
            withClue(s"For $scenario: ") {
              val formData = List(
                "disposalDate-day"   -> scenario.dayInput,
                "disposalDate-month" -> scenario.monthInput,
                "disposalDate-year"  -> scenario.yearInput
              ).collect { case (id, Some(input)) => id -> input }
              test(formData, scenario.expectedErrorMessageKey)
            }
          }
        }

        "the disposal date is in the future" in {
          test(formData(tomorrow), "disposalDate.error.tooFarInFuture")
        }

        "the disposal date is before 1st Jan 1900" in {
          test(formData(LocalDate.of(1899, 12, 31)), "disposalDate.error.before1900")
        }

      }

      "show a form error with capacitor type" when {

        def test(formData: Seq[(String, String)], expectedErrorKey: String): Unit =
          testFormError(performAction, "disposalDate.capacitor.title")(
            formData,
            expectedErrorKey,
            requiredPreviousAnswers.copy(
              individualUserType = Some(Capacitor)
            )
          )

        "the date is invalid" in {
          dateErrorScenarios("disposalDate", "").foreach { scenario =>
            withClue(s"For $scenario: ") {
              val formData = List(
                "disposalDate-day"   -> scenario.dayInput,
                "disposalDate-month" -> scenario.monthInput,
                "disposalDate-year"  -> scenario.yearInput
              ).collect { case (id, Some(input)) => id -> input }
              test(formData, scenario.expectedErrorMessageKey)
            }
          }
        }

        "the disposal date is in the future" in {
          DateErrorScenario(
            Some(tomorrow.getDayOfMonth.toString),
            Some(tomorrow.getMonthValue.toString),
            Some(tomorrow.getYear.toString),
            "disposalDate.error.tooFarInFuture"
          )

          test(formData(tomorrow), "disposalDate.error.tooFarInFuture")
        }

      }

      "show a form error with personal representative type" when {

        def test(
          personalRepType: Either[PersonalRepresentativeInPeriodOfAdmin.type, PersonalRepresentative.type],
          dateOfDeath: DateOfDeath = DateOfDeath(LocalDate.of(1800, 1, 1))
        )(formData: Seq[(String, String)], expectedErrorKey: String): Unit =
          testFormError(performAction, "disposalDate.personalRep.title")(
            formData,
            expectedErrorKey,
            requiredPreviousAnswers.copy(
              individualUserType = Some(personalRepType.merge)
            ),
            Some(sample[CompleteRepresenteeAnswers].copy(dateOfDeath = Some(dateOfDeath)))
          )

        "the date is invalid" in {
          dateErrorScenarios("disposalDate", "").foreach { scenario =>
            withClue(s"For $scenario: ") {
              val formData = List(
                "disposalDate-day"   -> scenario.dayInput,
                "disposalDate-month" -> scenario.monthInput,
                "disposalDate-year"  -> scenario.yearInput
              ).collect { case (id, Some(input)) => id -> input }
              test(Right(PersonalRepresentative))(formData, scenario.expectedErrorMessageKey)
            }
          }
        }

        "the disposal date is in the future" in {
          DateErrorScenario(
            Some(tomorrow.getDayOfMonth.toString),
            Some(tomorrow.getMonthValue.toString),
            Some(tomorrow.getYear.toString),
            "disposalDate.error.tooFarInFuture"
          )

          test(Left(PersonalRepresentativeInPeriodOfAdmin))(formData(tomorrow), "disposalDate.error.tooFarInFuture")
        }

        "the disposal date is strictly after the date of death and the user is a non-period of admin personal rep" in {
          test(
            Right(PersonalRepresentative),
            dateOfDeath = DateOfDeath(today.minusDays(1L))
          )(
            formData(today),
            "disposalDate.error.nonPeriodOfAdminDeathAfterDate"
          )
        }

        "the disposal date is before the date of death and the user is a period of admin personal rep" in {
          test(
            Left(PersonalRepresentativeInPeriodOfAdmin),
            dateOfDeath = DateOfDeath(today)
          )(
            formData(today.minusDays(1L)),
            "disposalDate.error.periodOfAdminDeathNotAfterDate"
          )
        }

        "the disposal date is strictly before the date of death and the user is a period of admin personal rep" in {
          test(
            Left(PersonalRepresentativeInPeriodOfAdmin),
            dateOfDeath = DateOfDeath(today.minusDays(1L))
          )(
            formData(today.minusDays(2L)),
            "disposalDate.error.periodOfAdminDeathNotAfterDate"
          )
        }

      }

      behave like unsuccessfulUpdatesBehaviour(
        performAction,
        requiredPreviousAnswers,
        formData(today),
        requiredPreviousAnswers
          .copy(disposalDate = Some(DisposalDate(today, taxYear))),
        updateDraftReturn,
        () => mockGetTaxYear(today)(Right(Some(taxYear)))
      )

      "handle valid dates" when {

        "the user is starting in a draft return and" when {

          "no tax year can be found for the given disposal date" in {

            testSuccessfulUpdateStartingNewDraft(
              performAction,
              requiredPreviousAnswers,
              formData(today),
              requiredPreviousAnswers
                .copy(
                  tooEarlyDisposalDate = Some(today),
                  disposalDate = None,
                  alreadySentSelfAssessment = None,
                  completionDate = None
                ),
              checkIsRedirect(
                _,
                routes.CommonTriageQuestionsController.disposalDateTooEarly()
              ),
              () => mockGetTaxYear(today)(Right(None)),
              Some(sample[CompleteRepresenteeAnswers].copy(dateOfDeath = Some(DateOfDeath(LocalDate.of(1800, 1, 1)))))
            )

          }

          "the user has incomplete draft with incorrect disposal tax year" in {
            val disposalDate    = LocalDate.of(2025, 4, 7)
            val taxYear         = sample[TaxYear].copy(
              startDateInclusive = LocalDate.of(2024, 4, 6),
              endDateExclusive = LocalDate.of(2025, 4, 6)
            )
            val expectedTaxYear = taxYear.copy(
              startDateInclusive = LocalDate.of(2025, 4, 6),
              endDateExclusive = LocalDate.of(2026, 4, 6)
            )

            val prevAnswers = requiredPreviousAnswers.copy(
              disposalDate = Some(DisposalDate(disposalDate, taxYear))
            )

            testSuccessfulUpdateStartingNewDraft(
              performAction,
              prevAnswers,
              formData(disposalDate),
              prevAnswers.copy(
                assetType = Some(AssetType.Residential),
                disposalDate = Some(DisposalDate(disposalDate, expectedTaxYear))
              ),
              checkIsRedirect(
                _,
                routes.SingleDisposalsTriageController.checkYourAnswers()
              ),
              () => mockGetTaxYear(disposalDate)(Right(Some(expectedTaxYear))),
              Some(sample[CompleteRepresenteeAnswers].copy(dateOfDeath = Some(DateOfDeath(LocalDate.of(1800, 1, 1)))))
            )
          }

          "a tax year can be found and the journey was complete" in {
            val completeJourney =
              sample[CompleteSingleDisposalTriageAnswers]
                .copy(
                  individualUserType = Some(Self),
                  disposalDate = DisposalDate(today, taxYear),
                  alreadySentSelfAssessment = None
                )
            val date            = today.minusDays(1L)

            testSuccessfulUpdateStartingNewDraft(
              performAction,
              completeJourney,
              formData(date),
              IncompleteSingleDisposalTriageAnswers(
                completeJourney.individualUserType,
                hasConfirmedSingleDisposal = true,
                Some(
                  if (completeJourney.representativeType() === Some(PersonalRepresentativeInPeriodOfAdmin)) {
                    DisposalMethod.Sold
                  } else {
                    completeJourney.disposalMethod
                  }
                ),
                Some(completeJourney.countryOfResidence.isUk),
                if (completeJourney.countryOfResidence.isUk) None else Some(completeJourney.countryOfResidence),
                Some(completeJourney.assetType),
                Some(DisposalDate(date, taxYear)),
                None,
                None,
                None
              ),
              checkIsRedirect(
                _,
                routes.SingleDisposalsTriageController.checkYourAnswers()
              ),
              () => mockGetTaxYear(date)(Right(Some(taxYear)))
            )

          }

        }

        "the user is filling out a draft return and" when {

          "the section is incomplete" in {
            testSuccessfulUpdateFillingOutReturn(
              performAction,
              requiredPreviousAnswers,
              formData(today),
              (fillingOutReturn, draftReturn) =>
                fillingOutReturn
                  .copy(draftReturn =
                    updateDraftReturn(
                      draftReturn,
                      requiredPreviousAnswers
                        .copy(disposalDate = Some(DisposalDate(today, taxYear)))
                    )
                  )
                  .withForceDisplayGainOrLossAfterReliefsForAmends,
              checkIsRedirect(
                _,
                routes.CommonTriageQuestionsController.amendReturnDisposalDateDifferentTaxYear()
              ),
              () => mockGetTaxYear(today)(Right(Some(taxYear))),
              amendReturnData = Some(sample[AmendReturnData])
            )
          }

          "the section is complete" in {
            forAll { (c: CompleteSingleDisposalTriageAnswers) =>
              val completeJourney = c.copy(
                individualUserType = Some(Self),
                disposalDate = DisposalDate(today, taxYear),
                disposalMethod = DisposalMethod.Sold,
                alreadySentSelfAssessment = None
              )
              val date            = today.minusDays(1L)
              val newAnswers      =
                IncompleteSingleDisposalTriageAnswers(
                  completeJourney.individualUserType,
                  hasConfirmedSingleDisposal = true,
                  Some(completeJourney.disposalMethod),
                  Some(completeJourney.countryOfResidence.isUk),
                  if (completeJourney.countryOfResidence.isUk) None else Some(completeJourney.countryOfResidence),
                  Some(completeJourney.assetType),
                  Some(DisposalDate(date, taxYear)),
                  None,
                  None,
                  None
                )

              testSuccessfulUpdateFillingOutReturn(
                performAction,
                completeJourney,
                formData(date),
                (fillingOutReturn, draftReturn) =>
                  fillingOutReturn
                    .copy(
                      draftReturn = updateDraftReturn(draftReturn, newAnswers)
                    )
                    .withForceDisplayGainOrLossAfterReliefsForAmends,
                checkIsRedirect(
                  _,
                  routes.CommonTriageQuestionsController.amendReturnDisposalDateDifferentTaxYear()
                ),
                () => mockGetTaxYear(date)(Right(Some(taxYear))),
                amendReturnData = Some(sample[AmendReturnData])
              )
            }
          }

          "the disposal date is on the date of death when the user is a non-period of admin personal rep" in {
            val answers = requiredPreviousAnswers.copy(individualUserType = Some(PersonalRepresentative))

            testSuccessfulUpdateFillingOutReturn(
              performAction,
              answers,
              formData(today),
              (fillingOutReturn, draftReturn) =>
                fillingOutReturn
                  .copy(draftReturn =
                    updateDraftReturn(
                      draftReturn,
                      answers.copy(disposalDate = Some(DisposalDate(today, taxYear)))
                    )
                  )
                  .withForceDisplayGainOrLossAfterReliefsForAmends,
              checkIsRedirect(
                _,
                routes.CommonTriageQuestionsController.amendReturnDisposalDateDifferentTaxYear()
              ),
              () => mockGetTaxYear(today)(Right(Some(taxYear))),
              Some(sample[CompleteRepresenteeAnswers].copy(dateOfDeath = Some(DateOfDeath(today)))),
              amendReturnData = Some(sample[AmendReturnData])
            )

          }

          "the disposal date is strictly before the date of death when the user is a period of admin personal rep" in {
            val answers         = requiredPreviousAnswers.copy(individualUserType = Some(PersonalRepresentative))
            val newDisposalDate = DisposalDate(today.minusDays(1L), taxYear)

            testSuccessfulUpdateFillingOutReturn(
              performAction,
              answers,
              formData(newDisposalDate.value),
              (fillingOutReturn, draftReturn) =>
                fillingOutReturn
                  .copy(draftReturn =
                    updateDraftReturn(
                      draftReturn,
                      answers.copy(disposalDate = Some(newDisposalDate))
                    )
                  )
                  .withForceDisplayGainOrLossAfterReliefsForAmends,
              checkIsRedirect(
                _,
                routes.CommonTriageQuestionsController.amendReturnDisposalDateDifferentTaxYear()
              ),
              () => mockGetTaxYear(newDisposalDate.value)(Right(Some(taxYear))),
              Some(sample[CompleteRepresenteeAnswers].copy(dateOfDeath = Some(DateOfDeath(today)))),
              amendReturnData = Some(sample[AmendReturnData])
            )
          }

          "the disposal date is strictly after the date of death when the user is a period of admin personal rep" in {
            val answers         = requiredPreviousAnswers.copy(
              individualUserType = Some(PersonalRepresentativeInPeriodOfAdmin)
            )
            val newDisposalDate = DisposalDate(today, taxYear)

            testSuccessfulUpdateFillingOutReturn(
              performAction,
              answers,
              formData(newDisposalDate.value),
              (fillingOutReturn, draftReturn) =>
                fillingOutReturn
                  .copy(draftReturn =
                    updateDraftReturn(
                      draftReturn,
                      answers.copy(disposalDate = Some(newDisposalDate), disposalMethod = Some(DisposalMethod.Sold))
                    )
                  )
                  .withForceDisplayGainOrLossAfterReliefsForAmends,
              checkIsRedirect(
                _,
                routes.CommonTriageQuestionsController.amendReturnDisposalDateDifferentTaxYear()
              ),
              () => mockGetTaxYear(newDisposalDate.value)(Right(Some(taxYear))),
              Some(sample[CompleteRepresenteeAnswers].copy(dateOfDeath = Some(DateOfDeath(today.minusDays(1L))))),
              amendReturnData = Some(sample[AmendReturnData])
            )
          }

        }

      }

      "not do any updates" when {

        "the answers submitted is the same as the one in session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithFillingOutReturn(
                requiredPreviousAnswers.copy(
                  disposalDate = Some(DisposalDate(today, taxYear))
                ),
                amendReturnData = Some(sample[AmendReturnData])
              )._1
            )
          }

          checkIsRedirect(
            performAction(formData(today)*),
            routes.CommonTriageQuestionsController.amendReturnDisposalDateDifferentTaxYear()
          )
        }

      }

    }

    "handling requests to display the when was completion date page" must {

      val disposalDate = DisposalDate(today, sample[TaxYear])

      val requiredPreviousAnswers =
        IncompleteSingleDisposalTriageAnswers.empty.copy(
          individualUserType = Some(Self),
          hasConfirmedSingleDisposal = true,
          disposalMethod = Some(DisposalMethod.Gifted),
          wasAUKResident = Some(true),
          assetType = Some(AssetType.Residential),
          disposalDate = Some(disposalDate)
        )

      def performAction(): Future[Result] =
        controller.whenWasCompletionDate()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(() => performAction(), isValidJourney)

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.whenWasCompletionDate(),
        mockUUIDGenerator
      )

      behave like redirectWhenNoPreviousAnswerBehaviour[DisposalDate](() => performAction())(
        requiredPreviousAnswers,
        routes.SingleDisposalsTriageController.whenWasDisposalDate(),
        { case (answers, d) => answers.copy(disposalDate = d) }
      )

      behave like displayIndividualTriagePageBehaviorIncompleteJourney(() => performAction())(
        requiredPreviousAnswers,
        requiredPreviousAnswers
          .copy(completionDate = Some(CompletionDate(disposalDate.value)))
      )(
        "completionDate.title",
        checkContent(
          _,
          routes.SingleDisposalsTriageController.whenWasDisposalDate()
        ),
        checkPrepopulatedContent(_, disposalDate.value)
      )

      behave like displayIndividualTriagePageBehaviorIncompleteJourney(() => performAction())(
        requiredPreviousAnswers.copy(
          individualUserType = Some(Capacitor)
        ),
        requiredPreviousAnswers.copy(
          completionDate = Some(CompletionDate(disposalDate.value)),
          individualUserType = Some(Capacitor)
        )
      )(
        "completionDate.capacitor.title",
        checkContent(
          _,
          routes.SingleDisposalsTriageController.whenWasDisposalDate()
        ),
        checkPrepopulatedContent(_, disposalDate.value)
      )

      behave like displayIndividualTriagePageBehaviorIncompleteJourney(() => performAction())(
        requiredPreviousAnswers.copy(
          individualUserType = Some(PersonalRepresentative)
        ),
        requiredPreviousAnswers.copy(
          completionDate = Some(CompletionDate(disposalDate.value)),
          individualUserType = Some(PersonalRepresentative)
        )
      )(
        "completionDate.personalRep.title",
        checkContent(
          _,
          routes.SingleDisposalsTriageController.whenWasDisposalDate()
        ),
        checkPrepopulatedContent(_, disposalDate.value)
      )

      behave like displayIndividualTriagePageBehaviorCompleteJourney(() => performAction())(
        sample[CompleteSingleDisposalTriageAnswers].copy(
          assetType = AssetType.Residential,
          completionDate = CompletionDate(disposalDate.value),
          individualUserType = Some(Self)
        )
      )(
        "completionDate.title",
        { doc =>
          checkContent(
            doc,
            routes.SingleDisposalsTriageController.checkYourAnswers()
          )
          checkPrepopulatedContent(doc, disposalDate.value)
        }
      )

      behave like displayIndividualTriagePageBehaviorCompleteJourney(() => performAction())(
        sample[CompleteSingleDisposalTriageAnswers].copy(
          assetType = AssetType.Residential,
          completionDate = CompletionDate(disposalDate.value),
          individualUserType = Some(Capacitor)
        )
      )(
        "completionDate.capacitor.title",
        { doc =>
          checkContent(
            doc,
            routes.SingleDisposalsTriageController.checkYourAnswers()
          )
          checkPrepopulatedContent(doc, disposalDate.value)
        }
      )

      behave like displayIndividualTriagePageBehaviorCompleteJourney(() => performAction())(
        sample[CompleteSingleDisposalTriageAnswers].copy(
          assetType = AssetType.Residential,
          completionDate = CompletionDate(disposalDate.value),
          individualUserType = Some(PersonalRepresentative)
        )
      )(
        "completionDate.personalRep.title",
        { doc =>
          checkContent(
            doc,
            routes.SingleDisposalsTriageController.checkYourAnswers()
          )
          checkPrepopulatedContent(doc, disposalDate.value)
        }
      )

      behave like displayCustomContentForAgentWithSelfAndTrustType(() => performAction())(
        requiredPreviousAnswers
          .copy(completionDate = Some(CompletionDate(disposalDate.value)))
      )(
        "completionDate.agent.title",
        doc =>
          checkContent(
            doc,
            routes.SingleDisposalsTriageController.whenWasDisposalDate()
          )
      )

      behave like displayCustomContentForAgentWithSelfAndTrustType(() => performAction())(
        requiredPreviousAnswers.copy(
          completionDate = Some(CompletionDate(disposalDate.value)),
          individualUserType = Some(PersonalRepresentative)
        )
      )(
        "completionDate.personalRep.title",
        doc =>
          checkContent(
            doc,
            routes.SingleDisposalsTriageController.whenWasDisposalDate()
          )
      )

      behave like displayCustomContentForTrusts(() => performAction())(
        requiredPreviousAnswers
          .copy(completionDate = Some(CompletionDate(disposalDate.value)))
      )(
        "completionDate.trust.title",
        doc =>
          checkContent(
            doc,
            routes.SingleDisposalsTriageController.whenWasDisposalDate()
          )
      )

      def checkContent(doc: Document, backLink: Call): Unit = {
        doc.select("#back, .govuk-back-link").attr("href") shouldBe backLink.url
        doc
          .select("#content > article > form, #main-content form")
          .attr("action")                                  shouldBe routes.SingleDisposalsTriageController
          .whenWasCompletionDateSubmit()
          .url

      }

      def checkPrepopulatedContent(doc: Document, date: LocalDate) = {
        doc
          .select("#completionDate-day")
          .attr("value") shouldBe date.getDayOfMonth.toString
        doc
          .select("#completionDate-month")
          .attr("value") shouldBe date.getMonthValue.toString
        doc
          .select("#completionDate-year")
          .attr("value") shouldBe date.getYear.toString
      }

    }

    "handling submitted completion dates" must {

      def performAction(formData: (String, String)*): Future[Result] =
        controller.whenWasCompletionDateSubmit()(
          FakeRequest().withFormUrlEncodedBody(formData*).withMethod("POST")
        )

      def formData(date: LocalDate) =
        List(
          "completionDate-day"   -> date.getDayOfMonth.toString,
          "completionDate-month" -> date.getMonthValue.toString,
          "completionDate-year"  -> date.getYear.toString
        )

      def updateDraftReturn(
        d: DraftSingleDisposalReturn,
        newAnswers: SingleDisposalTriageAnswers
      ) =
        d.copy(
          triageAnswers = newAnswers,
          acquisitionDetailsAnswers = d.acquisitionDetailsAnswers.map(a =>
            if (d.triageAnswers.representativeType().contains(PersonalRepresentativeInPeriodOfAdmin)) {
              a.unset(_.acquisitionPrice)
            } else {
              a.unset(_.acquisitionDate)
                .unset(_.acquisitionPrice)
                .unset(_.rebasedAcquisitionPrice)
                .unset(_.shouldUseRebase)
            }
          ),
          initialGainOrLoss = None,
          reliefDetailsAnswers = d.reliefDetailsAnswers.map(_.unsetPrrAndLettingRelief(newAnswers.isPeriodOfAdmin)),
          yearToDateLiabilityAnswers = d.yearToDateLiabilityAnswers.flatMap {
            case _: NonCalculatedYTDAnswers => None
            case c: CalculatedYTDAnswers    =>
              Some(
                c.unset(_.hasEstimatedDetails)
                  .unset(_.calculatedTaxDue)
                  .unset(_.taxDue)
                  .unset(_.mandatoryEvidence)
                  .unset(_.expiredEvidence)
                  .unset(_.pendingUpscanUpload)
              )
          }
        )

      val disposalDate = DisposalDate(today.minusDays(5L), sample[TaxYear])

      val tomorrow = TimeUtils
        .getMaximumDateForDisposalsAndCompletion(
          viewConfig.futureDatesEnabled,
          viewConfig.maxYearForDisposalsAndCompletion
        )
        .plusDays(1L)

      val dayBeforeDisposalDate = disposalDate.value.minusDays(1L)

      val requiredPreviousAnswers =
        IncompleteSingleDisposalTriageAnswers.empty.copy(
          individualUserType = Some(Self),
          hasConfirmedSingleDisposal = true,
          disposalMethod = Some(DisposalMethod.Gifted),
          wasAUKResident = Some(true),
          assetType = Some(AssetType.Residential),
          disposalDate = Some(disposalDate)
        )

      behave like redirectToStartWhenInvalidJourney(() => performAction(), isValidJourney)

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.whenWasCompletionDateSubmit(),
        mockUUIDGenerator
      )

      behave like redirectWhenNoPreviousAnswerBehaviour[DisposalDate](() => performAction())(
        requiredPreviousAnswers,
        routes.SingleDisposalsTriageController.whenWasDisposalDate(),
        { case (i, d) => i.copy(disposalDate = d) }
      )

      "show a form error with self" when {

        def test(formData: Seq[(String, String)], expectedErrorKey: String)(
          requiredPreviousTriageAnswers: SingleDisposalTriageAnswers
        ): Unit =
          testFormError(performAction, "completionDate.title")(
            formData,
            expectedErrorKey,
            requiredPreviousTriageAnswers
          )

        def testWithErrorArgs(formData: Seq[(String, String)], expectedErrorKey: String, arg: String)(
          requiredPreviousTriageAnswers: SingleDisposalTriageAnswers
        ): Unit =
          testFormErrorWithErrorArg(performAction, "completionDate.title")(
            formData,
            expectedErrorKey,
            arg,
            requiredPreviousTriageAnswers
          )

        "the date is invalid" in {
          dateErrorScenarios("completionDate", "").foreach { scenario =>
            withClue(s"For $scenario: ") {
              val formData = List(
                "completionDate-day"   -> scenario.dayInput,
                "completionDate-month" -> scenario.monthInput,
                "completionDate-year"  -> scenario.yearInput
              ).collect { case (id, Some(input)) => id -> input }
              test(formData, scenario.expectedErrorMessageKey)(requiredPreviousAnswers)
            }
          }
        }

        "the completion date is in the future" in {
          val futureDate = today.plusYears(2).plusDays(1L)
          test(formData(futureDate), "completionDate.error.tooFarInFuture")(requiredPreviousAnswers)
        }

        "the completion date is before 01-01-1900" in {
          val date = LocalDate.of(1800, 1, 1)

          test(formData(date), "completionDate.error.before1900")(
            requiredPreviousAnswers.copy(
              disposalDate = Some(
                disposalDate.copy(
                  value = LocalDate.of(1800, 1, 1)
                )
              )
            )
          )
        }

        "the completion date is before the disposal date" in {
          val disposalDatInStrFormat =
            s"${disposalDate.value.getDayOfMonth} ${disposalDate.value.getMonth.toString.toLowerCase.capitalize} ${disposalDate.value.getYear}"
          testWithErrorArgs(
            formData(dayBeforeDisposalDate),
            "completionDate.error.tooFarInPast",
            disposalDatInStrFormat
          )(requiredPreviousAnswers)
        }

      }

      "show a form error with capacitor" when {

        def test(formData: Seq[(String, String)], expectedErrorKey: String)(
          requiredPreviousTriageAnswers: IncompleteSingleDisposalTriageAnswers
        ): Unit =
          testFormError(performAction, "completionDate.capacitor.title")(
            formData,
            expectedErrorKey,
            requiredPreviousTriageAnswers.copy(
              individualUserType = Some(Capacitor)
            )
          )

        def testWithErrorArgs(formData: Seq[(String, String)], expectedErrorKey: String, arg: String)(
          requiredPreviousTriageAnswers: SingleDisposalTriageAnswers
        ): Unit =
          testFormErrorWithErrorArg(performAction, "completionDate.title")(
            formData,
            expectedErrorKey,
            arg,
            requiredPreviousTriageAnswers
          )

        "the date is invalid" in {
          dateErrorScenarios("completionDate", "").foreach { scenario =>
            withClue(s"For $scenario: ") {
              val formData = List(
                "completionDate-day"   -> scenario.dayInput,
                "completionDate-month" -> scenario.monthInput,
                "completionDate-year"  -> scenario.yearInput
              ).collect { case (id, Some(input)) => id -> input }
              test(formData, scenario.expectedErrorMessageKey)(requiredPreviousAnswers)
            }
          }
        }

        "the completion date is in the future" in {
          test(formData(tomorrow), "completionDate.error.tooFarInFuture")(requiredPreviousAnswers)
        }

        "the completion date is before 01-01-1900" in {
          val date = LocalDate.of(1800, 1, 1)

          test(formData(date), "completionDate.error.before1900")(
            requiredPreviousAnswers.copy(
              disposalDate = Some(
                disposalDate.copy(
                  value = LocalDate.of(1800, 1, 1)
                )
              )
            )
          )
        }

        "the completion date is before the disposal date" in {
          val disposalDatInStrFormat =
            s"${disposalDate.value.getDayOfMonth} ${disposalDate.value.getMonth.toString.toLowerCase.capitalize} ${disposalDate.value.getYear}"
          testWithErrorArgs(
            formData(dayBeforeDisposalDate),
            "completionDate.error.tooFarInPast",
            disposalDatInStrFormat
          )(requiredPreviousAnswers)
        }

      }

      "show a form error with personal representative" when {

        def test(formData: Seq[(String, String)], expectedErrorKey: String)(
          requiredPreviousTriageAnswers: IncompleteSingleDisposalTriageAnswers
        ): Unit =
          testFormError(performAction, "completionDate.personalRep.title")(
            formData,
            expectedErrorKey,
            requiredPreviousTriageAnswers.copy(
              individualUserType = Some(PersonalRepresentative)
            )
          )

        def testWithErrorArgs(formData: Seq[(String, String)], expectedErrorKey: String, arg: String)(
          requiredPreviousTriageAnswers: SingleDisposalTriageAnswers
        ): Unit =
          testFormErrorWithErrorArg(performAction, "completionDate.title")(
            formData,
            expectedErrorKey,
            arg,
            requiredPreviousTriageAnswers
          )

        "the date is invalid" in {
          dateErrorScenarios("completionDate", "").foreach { scenario =>
            withClue(s"For $scenario: ") {
              val formData = List(
                "completionDate-day"   -> scenario.dayInput,
                "completionDate-month" -> scenario.monthInput,
                "completionDate-year"  -> scenario.yearInput
              ).collect { case (id, Some(input)) => id -> input }
              test(formData, scenario.expectedErrorMessageKey)(requiredPreviousAnswers)
            }
          }
        }

        "the completion date is in the future" in {
          test(formData(tomorrow), "completionDate.error.tooFarInFuture")(requiredPreviousAnswers)
        }

        "the completion date is before 01-01-1900" in {
          val date = LocalDate.of(1800, 1, 1)

          test(formData(date), "completionDate.error.before1900")(
            requiredPreviousAnswers.copy(
              disposalDate = Some(
                disposalDate.copy(
                  value = LocalDate.of(1800, 1, 1)
                )
              )
            )
          )
        }

        "the completion date is before the disposal date" in {
          val disposalDatInStrFormat =
            s"${disposalDate.value.getDayOfMonth} ${disposalDate.value.getMonth.toString.toLowerCase.capitalize} ${disposalDate.value.getYear}"
          testWithErrorArgs(
            formData(dayBeforeDisposalDate),
            "completionDate.error.tooFarInPast",
            disposalDatInStrFormat
          )(requiredPreviousAnswers)
        }

      }

      behave like unsuccessfulUpdatesBehaviour(
        performAction,
        requiredPreviousAnswers,
        formData(disposalDate.value),
        requiredPreviousAnswers
          .copy(completionDate = Some(CompletionDate(disposalDate.value))),
        updateDraftReturn
      )

      "handle valid dates" when {

        "the user is starting in a draft return and" when {

          "the section is incomplete" in {
            val completionDate = CompletionDate(disposalDate.value)
            testSuccessfulUpdateStartingNewDraft(
              performAction,
              requiredPreviousAnswers,
              formData(completionDate.value),
              requiredPreviousAnswers
                .copy(completionDate = Some(completionDate)),
              checkIsRedirect(
                _,
                routes.SingleDisposalsTriageController.checkYourAnswers()
              )
            )
          }

          "the section is complete" in {
            val completionDate = CompletionDate(disposalDate.value.plusDays(1L))

            val completeAnswers = sample[CompleteSingleDisposalTriageAnswers].copy(
              completionDate = CompletionDate(disposalDate.value),
              disposalDate = disposalDate,
              disposalMethod = DisposalMethod.Sold
            )

            testSuccessfulUpdateStartingNewDraft(
              performAction,
              completeAnswers,
              formData(completionDate.value),
              IncompleteSingleDisposalTriageAnswers
                .fromCompleteAnswers(completeAnswers)
                .copy(completionDate = Some(completionDate)),
              checkIsRedirect(
                _,
                routes.SingleDisposalsTriageController.checkYourAnswers()
              )
            )
          }

        }

        "the user is filling out a draft return and" when {

          "the section is incomplete" in {
            val completionDate = CompletionDate(disposalDate.value)
            testSuccessfulUpdateFillingOutReturn(
              performAction,
              requiredPreviousAnswers,
              formData(completionDate.value),
              (fillingOutReturn, draftReturn) =>
                fillingOutReturn.copy(draftReturn =
                  updateDraftReturn(
                    draftReturn,
                    requiredPreviousAnswers
                      .copy(completionDate = Some(completionDate))
                  )
                ),
              checkIsRedirect(
                _,
                routes.SingleDisposalsTriageController.checkYourAnswers()
              )
            )
          }

          "the section is complete" in {
            forAll { (c: CompleteSingleDisposalTriageAnswers) =>
              val completionDate  =
                CompletionDate(disposalDate.value.plusDays(1L))
              val completeAnswers = c.copy(
                completionDate = CompletionDate(disposalDate.value),
                disposalDate = disposalDate,
                disposalMethod = DisposalMethod.Sold
              )

              testSuccessfulUpdateFillingOutReturn(
                performAction,
                completeAnswers,
                formData(completionDate.value),
                (fillingOutReturn, draftReturn) =>
                  fillingOutReturn
                    .copy(draftReturn =
                      updateDraftReturn(
                        draftReturn,
                        IncompleteSingleDisposalTriageAnswers
                          .fromCompleteAnswers(completeAnswers)
                          .copy(completionDate = Some(completionDate))
                      )
                    )
                    .withForceDisplayGainOrLossAfterReliefsForAmends,
                checkIsRedirect(
                  _,
                  routes.SingleDisposalsTriageController.checkYourAnswers()
                ),
                amendReturnData = Some(sample[AmendReturnData])
              )
            }
          }

        }

      }

      "not do any updates" when {

        "the answers submitted is the same as the one in session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithFillingOutReturn(
                requiredPreviousAnswers.copy(
                  completionDate = Some(CompletionDate(disposalDate.value))
                )
              )._1
            )
          }

          checkIsRedirect(
            performAction(formData(disposalDate.value)*),
            routes.SingleDisposalsTriageController.checkYourAnswers()
          )
        }

      }

    }

    "handling requests to display the country of residence page" must {

      val requiredPreviousAnswers =
        IncompleteSingleDisposalTriageAnswers.empty.copy(
          individualUserType = Some(IndividualUserType.Self),
          hasConfirmedSingleDisposal = true,
          disposalMethod = Some(DisposalMethod.Sold),
          wasAUKResident = Some(false)
        )

      val country = Country("HK")

      def performAction(): Future[Result] =
        controller.countryOfResidence()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(() => performAction(), isValidJourney)

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.countryOfResidence(),
        mockUUIDGenerator
      )

      behave like redirectWhenNoPreviousAnswerBehaviour[Boolean](() => performAction())(
        requiredPreviousAnswers,
        routes.SingleDisposalsTriageController.wereYouAUKResident(),
        { case (answers, w) => answers.copy(wasAUKResident = w) }
      )

      "redirect to the were you a uk resident page" when {

        "the user has answered yes to that question and" when {

          "the section is incomplete" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionDataWithStartingNewDraftReturn(
                  requiredPreviousAnswers.copy(wasAUKResident = Some(true))
                )._1
              )
            }

            checkIsRedirect(performAction(), routes.SingleDisposalsTriageController.wereYouAUKResident())
          }

          "the section is complete" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionDataWithStartingNewDraftReturn(
                  sample[CompleteSingleDisposalTriageAnswers].copy(
                    individualUserType = Some(IndividualUserType.Self),
                    countryOfResidence = Country.uk
                  )
                )._1
              )
            }

            checkIsRedirect(performAction(), routes.SingleDisposalsTriageController.wereYouAUKResident())

          }
        }

      }

      behave like displayIndividualTriagePageBehaviorIncompleteJourney(() => performAction())(
        requiredPreviousAnswers,
        requiredPreviousAnswers.copy(countryOfResidence = Some(country))
      )(
        "triage.enterCountry.title",
        checkContent(
          _,
          routes.SingleDisposalsTriageController.wereYouAUKResident()
        ),
        _ => ()
      )

      behave like displayIndividualTriagePageBehaviorCompleteJourney(() => performAction())(
        sample[CompleteSingleDisposalTriageAnswers]
          .copy(
            countryOfResidence = country,
            individualUserType = Some(IndividualUserType.Self)
          )
      )(
        "triage.enterCountry.title",
        doc =>
          checkContent(
            doc,
            routes.SingleDisposalsTriageController.checkYourAnswers()
          )
      )

      behave like displayIndividualTriagePageBehaviorCompleteJourney(() => performAction())(
        sample[CompleteSingleDisposalTriageAnswers]
          .copy(
            countryOfResidence = country,
            individualUserType = Some(IndividualUserType.Capacitor)
          )
      )(
        "triage.enterCountry.capacitor.title",
        doc =>
          checkContent(
            doc,
            routes.SingleDisposalsTriageController.checkYourAnswers()
          )
      )

      behave like displayIndividualTriagePageBehaviorCompleteJourney(() => performAction())(
        sample[CompleteSingleDisposalTriageAnswers].copy(
          countryOfResidence = country,
          individualUserType = Some(IndividualUserType.PersonalRepresentative)
        )
      )(
        "triage.enterCountry.personalRep.title",
        doc =>
          checkContent(
            doc,
            routes.SingleDisposalsTriageController.checkYourAnswers()
          )
      )

      behave like displayCustomContentForAgentWithSelfAndTrustType(() => performAction())(
        requiredPreviousAnswers.copy(countryOfResidence = Some(country))
      )(
        "triage.enterCountry.agent.title",
        doc =>
          checkContent(
            doc,
            routes.SingleDisposalsTriageController.wereYouAUKResident()
          )
      )

      behave like displayCustomContentForAgentWithSelfAndTrustType(() => performAction())(
        requiredPreviousAnswers.copy(
          countryOfResidence = Some(country),
          individualUserType = Some(IndividualUserType.PersonalRepresentative)
        )
      )(
        "triage.enterCountry.personalRep.title",
        doc =>
          checkContent(
            doc,
            routes.SingleDisposalsTriageController.wereYouAUKResident()
          )
      )

      behave like displayCustomContentForTrusts(() => performAction())(
        requiredPreviousAnswers.copy(countryOfResidence = Some(country))
      )(
        "triage.enterCountry.trust.title",
        doc =>
          checkContent(
            doc,
            routes.SingleDisposalsTriageController.wereYouAUKResident()
          )
      )

      def checkContent(doc: Document, backLink: Call): Unit = {
        doc.select("#back, .govuk-back-link").attr("href") shouldBe backLink.url
        doc
          .select("#content > article > form, #main-content form")
          .attr("action")                                  shouldBe routes.SingleDisposalsTriageController
          .countryOfResidenceSubmit()
          .url
      }

    }

    "handling submitted answers to the country of residence page" must {

      def performAction(formData: (String, String)*): Future[Result] =
        controller.countryOfResidenceSubmit()(
          FakeRequest().withFormUrlEncodedBody(formData*).withMethod("POST")
        )

      def updateDraftReturn(
        d: DraftSingleDisposalReturn,
        newAnswers: SingleDisposalTriageAnswers
      ): DraftSingleDisposalReturn =
        d.copy(
          triageAnswers = newAnswers,
          yearToDateLiabilityAnswers = None
        )

      val requiredPreviousAnswers =
        IncompleteSingleDisposalTriageAnswers.empty.copy(
          individualUserType = Some(IndividualUserType.Self),
          hasConfirmedSingleDisposal = true,
          disposalMethod = Some(DisposalMethod.Sold),
          wasAUKResident = Some(false)
        )

      val country = Country("HK")

      behave like redirectToStartWhenInvalidJourney(() => performAction(), isValidJourney)

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.countryOfResidenceSubmit(),
        mockUUIDGenerator
      )

      behave like redirectWhenNoPreviousAnswerBehaviour[Boolean](() => performAction())(
        requiredPreviousAnswers,
        routes.SingleDisposalsTriageController.wereYouAUKResident(),
        { case (answers, w) => answers.copy(wasAUKResident = w) }
      )

      "redirect to the were you a uk resident page" when {

        "the user has answered yes to that question and" when {

          "the section is incomplete" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionDataWithStartingNewDraftReturn(
                  requiredPreviousAnswers.copy(wasAUKResident = Some(true))
                )._1
              )
            }

            checkIsRedirect(performAction(), routes.SingleDisposalsTriageController.wereYouAUKResident())
          }

          "the section is complete" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionDataWithStartingNewDraftReturn(
                  sample[CompleteSingleDisposalTriageAnswers].copy(
                    individualUserType = Some(IndividualUserType.Self),
                    countryOfResidence = Country.uk
                  )
                )._1
              )
            }

            checkIsRedirect(performAction(), routes.SingleDisposalsTriageController.wereYouAUKResident())

          }
        }

      }

      "show a form error for self type" when {

        def test(formData: Seq[(String, String)], expectedErrorKey: String): Unit =
          testFormError(performAction, "triage.enterCountry.title")(
            formData,
            expectedErrorKey,
            requiredPreviousAnswers
          )

        "nothing is submitted" in {
          test(List.empty, "countryCode.error.required")
        }

        "the option is not recognised" in {
          test(List("countryCode" -> "XX"), "countryCode.error.notFound")
        }

      }

      "show a form error for capacitor type" when {

        def test(formData: Seq[(String, String)], expectedErrorKey: String): Unit =
          testFormError(performAction, "triage.enterCountry.capacitor.title")(
            formData,
            expectedErrorKey,
            requiredPreviousAnswers.copy(
              individualUserType = Some(IndividualUserType.Capacitor)
            )
          )

        "nothing is submitted" in {
          test(List.empty, "countryCode.error.required")
        }

        "the option is not recognised" in {
          test(List("countryCode" -> "XX"), "countryCode.error.notFound")
        }

      }

      "show a form error for personal representative type" when {

        def test(formData: Seq[(String, String)], expectedErrorKey: String): Unit =
          testFormError(performAction, "triage.enterCountry.personalRep.title")(
            formData,
            expectedErrorKey,
            requiredPreviousAnswers.copy(
              individualUserType = Some(IndividualUserType.PersonalRepresentative)
            )
          )

        "nothing is submitted" in {
          test(List.empty, "countryCode.error.required")
        }

        "the option is not recognised" in {
          test(List("countryCode" -> "XX"), "countryCode.error.notFound")
        }

      }

      behave like unsuccessfulUpdatesBehaviour(
        performAction,
        requiredPreviousAnswers,
        List("countryCode" -> country.code),
        requiredPreviousAnswers.copy(countryOfResidence = Some(country)),
        updateDraftReturn
      )

      "handle successful updates" when {

        "the user is starting a new draft return and" when {

          "the user has answered some questions but not complete the section" in {
            testSuccessfulUpdateStartingNewDraft(
              performAction,
              requiredPreviousAnswers,
              List("countryCode" -> country.code),
              requiredPreviousAnswers.copy(countryOfResidence = Some(country)),
              checkIsRedirect(
                _,
                routes.SingleDisposalsTriageController.checkYourAnswers()
              )
            )
          }

          "the user has complete the section and has changed their answer and they choose asset type residential" in {
            val completeAnswers = sample[CompleteSingleDisposalTriageAnswers].copy(
              countryOfResidence = Country("CC"),
              disposalMethod = DisposalMethod.Sold
            )
            testSuccessfulUpdateStartingNewDraft(
              performAction,
              completeAnswers,
              List("countryCode" -> country.code),
              completeAnswers.copy(countryOfResidence = country),
              checkIsRedirect(
                _,
                routes.SingleDisposalsTriageController.checkYourAnswers()
              )
            )
          }

        }

        "the user is filling out a draft return and" when {

          "the user has answered some questions but not complete the section" in {
            testSuccessfulUpdateFillingOutReturn(
              performAction,
              requiredPreviousAnswers,
              List("countryCode" -> country.code),
              (fillingOutReturn, draftReturn) =>
                fillingOutReturn.copy(draftReturn =
                  updateDraftReturn(
                    draftReturn,
                    requiredPreviousAnswers.copy(countryOfResidence = Some(country))
                  )
                ),
              checkIsRedirect(
                _,
                routes.SingleDisposalsTriageController.checkYourAnswers()
              )
            )
          }

          "the user has complete the section and has changed their answer from not in the uk to was in the uk" in {
            forAll { (c: CompleteSingleDisposalTriageAnswers) =>
              val completeAnswers = c.copy(
                countryOfResidence = Country("CC"),
                disposalMethod = DisposalMethod.Sold
              )
              testSuccessfulUpdateFillingOutReturn(
                performAction,
                completeAnswers,
                List("countryCode" -> country.code),
                (fillingOutReturn, draftReturn) =>
                  fillingOutReturn.copy(draftReturn =
                    updateDraftReturn(
                      draftReturn,
                      completeAnswers.copy(countryOfResidence = country)
                    )
                  ),
                checkIsRedirect(
                  _,
                  routes.SingleDisposalsTriageController.checkYourAnswers()
                )
              )
            }
          }

          "the user is on an amend journey where the estimates answer should be preserved" in {
            forAll { (c: CompleteSingleDisposalTriageAnswers) =>
              val completeAnswers = c.copy(
                countryOfResidence = Country("CC"),
                disposalMethod = DisposalMethod.Sold
              )
              testSuccessfulUpdateFillingOutReturn(
                performAction,
                completeAnswers,
                List("countryCode" -> country.code),
                (fillingOutReturn, draftReturn) =>
                  fillingOutReturn.copy(draftReturn =
                    updateDraftReturn(
                      draftReturn,
                      completeAnswers.copy(countryOfResidence = country)
                    )
                  ),
                checkIsRedirect(
                  _,
                  routes.SingleDisposalsTriageController.checkYourAnswers()
                ),
                amendReturnData = Some(
                  sample[AmendReturnData].copy(
                    originalReturn = sample[CompleteReturnWithSummary].copy(
                      completeReturn = sample[CompleteSingleIndirectDisposalReturn].copy(
                        yearToDateLiabilityAnswers = sample[CompleteNonCalculatedYTDAnswers].copy(
                          hasEstimatedDetails = false
                        )
                      )
                    )
                  )
                )
              )
            }
          }

        }

      }

      "not do any updates" when {

        "the answers submitted is the same as the one in session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithFillingOutReturn(
                requiredPreviousAnswers.copy(
                  countryOfResidence = Some(country)
                )
              )._1
            )
          }

          val result = performAction("countryCode" -> country.code)
          checkIsRedirect(
            result,
            routes.SingleDisposalsTriageController.checkYourAnswers()
          )
        }

      }

    }

    "handling requests to display the asset type for non uk residents page" must {

      val requiredPreviousAnswers =
        IncompleteSingleDisposalTriageAnswers.empty.copy(
          individualUserType = Some(IndividualUserType.Self),
          hasConfirmedSingleDisposal = true,
          disposalMethod = Some(DisposalMethod.Sold),
          wasAUKResident = Some(false),
          countryOfResidence = Some(sample[Country])
        )

      def performAction(): Future[Result] =
        controller.assetTypeForNonUkResidents()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(() => performAction(), isValidJourney)

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.assetTypeForNonUkResidents(),
        mockUUIDGenerator
      )

      behave like redirectWhenNoPreviousAnswerBehaviour[Country](() => performAction())(
        requiredPreviousAnswers,
        routes.SingleDisposalsTriageController.countryOfResidence(),
        { case (answers, country) =>
          answers.copy(countryOfResidence = country)
        }
      )

      behave like displayIndividualTriagePageBehaviorIncompleteJourney(() => performAction())(
        requiredPreviousAnswers,
        requiredPreviousAnswers.copy(assetType = Some(AssetType.MixedUse))
      )(
        "assetTypeForNonUkResidents.title",
        checkContent(
          _,
          routes.SingleDisposalsTriageController.countryOfResidence()
        ),
        _.select("#assetTypeForNonUkResidents-2").hasAttr("checked")
      )

      behave like displayIndividualTriagePageBehaviorIncompleteJourney(() => performAction())(
        requiredPreviousAnswers
          .copy(individualUserType = Some(IndividualUserType.Capacitor)),
        requiredPreviousAnswers.copy(
          assetType = Some(AssetType.MixedUse),
          individualUserType = Some(IndividualUserType.Capacitor)
        )
      )(
        "assetTypeForNonUkResidents.capacitor.title",
        checkContent(
          _,
          routes.SingleDisposalsTriageController.countryOfResidence()
        ),
        _.select("#assetTypeForNonUkResidents-2").hasAttr("checked")
      )

      behave like displayIndividualTriagePageBehaviorIncompleteJourney(() => performAction())(
        requiredPreviousAnswers.copy(individualUserType = Some(IndividualUserType.PersonalRepresentative)),
        requiredPreviousAnswers.copy(
          assetType = Some(AssetType.MixedUse),
          individualUserType = Some(IndividualUserType.PersonalRepresentative)
        )
      )(
        "assetTypeForNonUkResidents.personalRep.title",
        checkContent(
          _,
          routes.SingleDisposalsTriageController.countryOfResidence()
        ),
        _.select("#assetTypeForNonUkResidents-2")
          .hasAttr("checked")
      )

      behave like displayIndividualTriagePageBehaviorCompleteJourney(() => performAction())(
        sample[CompleteSingleDisposalTriageAnswers]
          .copy(
            countryOfResidence = sample[Country],
            assetType = AssetType.Residential,
            individualUserType = Some(IndividualUserType.Self)
          )
      )(
        "assetTypeForNonUkResidents.title",
        { doc =>
          checkContent(
            doc,
            routes.SingleDisposalsTriageController.checkYourAnswers()
          )
          doc
            .select("#assetTypeForNonUkResidents")
            .hasAttr("checked")
        }
      )

      behave like displayIndividualTriagePageBehaviorCompleteJourney(() => performAction())(
        sample[CompleteSingleDisposalTriageAnswers]
          .copy(
            countryOfResidence = sample[Country],
            assetType = AssetType.Residential,
            individualUserType = Some(IndividualUserType.Capacitor)
          )
      )(
        "assetTypeForNonUkResidents.capacitor.title",
        { doc =>
          checkContent(
            doc,
            routes.SingleDisposalsTriageController.checkYourAnswers()
          )
          doc
            .select("#assetTypeForNonUkResidents-0")
            .hasAttr("checked")
        }
      )

      behave like displayIndividualTriagePageBehaviorCompleteJourney(() => performAction())(
        sample[CompleteSingleDisposalTriageAnswers]
          .copy(
            countryOfResidence = sample[Country],
            assetType = AssetType.Residential,
            individualUserType = Some(IndividualUserType.PersonalRepresentative)
          )
      )(
        "assetTypeForNonUkResidents.personalRep.title",
        { doc =>
          checkContent(
            doc,
            routes.SingleDisposalsTriageController.checkYourAnswers()
          )
          doc
            .select("#assetTypeForNonUkResidents-0")
            .hasAttr("checked")
        }
      )

      behave like displayCustomContentForAgentWithSelfAndTrustType(() => performAction())(
        requiredPreviousAnswers.copy(assetType = Some(AssetType.Residential))
      )(
        "assetTypeForNonUkResidents.agent.title",
        doc =>
          checkContent(
            doc,
            routes.SingleDisposalsTriageController.countryOfResidence()
          )
      )

      behave like displayCustomContentForAgentWithSelfAndTrustType(() => performAction())(
        requiredPreviousAnswers.copy(
          assetType = Some(AssetType.Residential),
          individualUserType = Some(IndividualUserType.PersonalRepresentative)
        )
      )(
        "assetTypeForNonUkResidents.personalRep.title",
        doc =>
          checkContent(
            doc,
            routes.SingleDisposalsTriageController.countryOfResidence()
          )
      )

      behave like displayCustomContentForTrusts(() => performAction())(
        requiredPreviousAnswers.copy(assetType = Some(AssetType.Residential))
      )(
        "assetTypeForNonUkResidents.trust.title",
        doc =>
          checkContent(
            doc,
            routes.SingleDisposalsTriageController.countryOfResidence()
          )
      )

      def checkContent(doc: Document, backLink: Call): Unit = {
        doc.select("#back, .govuk-back-link").attr("href") shouldBe backLink.url
        doc
          .select("#content > article > form, #main-content form")
          .attr("action")                                  shouldBe routes.SingleDisposalsTriageController
          .assetTypeForNonUkResidentsSubmit()
          .url
      }

    }

    "handling submitted answers to the asset type for non uk residents page" must {

      def performAction(formData: (String, String)*): Future[Result] =
        controller.assetTypeForNonUkResidentsSubmit()(
          FakeRequest().withFormUrlEncodedBody(formData*).withMethod("POST")
        )

      def updateDraftReturn(
        d: DraftSingleDisposalReturn,
        newAnswers: SingleDisposalTriageAnswers
      ): DraftSingleDisposalReturn =
        d.copy(
          triageAnswers = newAnswers,
          propertyAddress = None,
          disposalDetailsAnswers = None,
          acquisitionDetailsAnswers = None,
          initialGainOrLoss = None,
          reliefDetailsAnswers = d.reliefDetailsAnswers.map(_.unsetPrrAndLettingRelief(newAnswers.isPeriodOfAdmin)),
          yearToDateLiabilityAnswers = d.yearToDateLiabilityAnswers.flatMap {
            case c: CalculatedYTDAnswers =>
              Some(
                c.unset(_.hasEstimatedDetails)
                  .unset(_.calculatedTaxDue)
                  .unset(_.taxDue)
                  .unset(_.mandatoryEvidence)
                  .unset(_.expiredEvidence)
                  .unset(_.pendingUpscanUpload)
              )

            case _: NonCalculatedYTDAnswers =>
              None
          },
          supportingEvidenceAnswers = None
        )

      val requiredPreviousAnswers =
        IncompleteSingleDisposalTriageAnswers.empty.copy(
          individualUserType = Some(IndividualUserType.Self),
          hasConfirmedSingleDisposal = true,
          disposalMethod = Some(DisposalMethod.Sold),
          wasAUKResident = Some(false),
          countryOfResidence = Some(sample[Country])
        )

      behave like redirectToStartWhenInvalidJourney(() => performAction(), isValidJourney)

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.assetTypeForNonUkResidentsSubmit(),
        mockUUIDGenerator
      )

      behave like redirectWhenNoPreviousAnswerBehaviour[Country](() => performAction())(
        requiredPreviousAnswers,
        routes.SingleDisposalsTriageController.countryOfResidence(),
        { case (answers, country) =>
          answers.copy(countryOfResidence = country)
        }
      )

      "show a form error for self type" when {

        def test(formData: Seq[(String, String)], expectedErrorKey: String): Unit =
          testFormError(performAction, "assetTypeForNonUkResidents.title")(
            formData,
            expectedErrorKey,
            requiredPreviousAnswers
          )

        "nothing is submitted" in {
          test(List.empty, "assetTypeForNonUkResidents.error.required")
        }

        "the option is not recognised" in {
          test(
            List("assetTypeForNonUkResidents" -> "4"),
            "assetTypeForNonUkResidents.error.invalid"
          )
        }

      }

      "show a form error for capacitor type" when {

        def test(formData: Seq[(String, String)], expectedErrorKey: String): Unit =
          testFormError(
            performAction,
            "assetTypeForNonUkResidents.capacitor.title"
          )(
            formData,
            expectedErrorKey,
            requiredPreviousAnswers.copy(
              individualUserType = Some(IndividualUserType.Capacitor)
            )
          )

        "nothing is submitted" in {
          test(
            List.empty,
            "assetTypeForNonUkResidents.capacitor.error.required"
          )
        }

        "the option is not recognised" in {
          test(
            List("assetTypeForNonUkResidents" -> "4"),
            "assetTypeForNonUkResidents.capacitor.error.invalid"
          )
        }

      }

      "show a form error for personal representative type" when {

        def test(formData: Seq[(String, String)], expectedErrorKey: String): Unit =
          testFormError(
            performAction,
            "assetTypeForNonUkResidents.personalRep.title"
          )(
            formData,
            expectedErrorKey,
            requiredPreviousAnswers.copy(
              individualUserType = Some(IndividualUserType.PersonalRepresentative)
            )
          )

        "nothing is submitted" in {
          test(
            List.empty,
            "assetTypeForNonUkResidents.personalRep.error.required"
          )
        }

        "the option is not recognised" in {
          test(
            List("assetTypeForNonUkResidents" -> "4"),
            "assetTypeForNonUkResidents.personalRep.error.invalid"
          )
        }

      }

      behave like unsuccessfulUpdatesBehaviour(
        performAction,
        requiredPreviousAnswers,
        List("assetTypeForNonUkResidents" -> "0"),
        requiredPreviousAnswers.copy(assetType = Some(AssetType.Residential)),
        updateDraftReturn
      )

      "handle successful updates" when {

        "the user is starting a new draft return and" when {

          "the user has answered some questions but not complete the section" in {
            testSuccessfulUpdateStartingNewDraft(
              performAction,
              requiredPreviousAnswers,
              List("assetTypeForNonUkResidents" -> "0"),
              requiredPreviousAnswers
                .copy(assetType = Some(AssetType.Residential)),
              checkIsRedirect(
                _,
                routes.SingleDisposalsTriageController.checkYourAnswers()
              )
            )
          }

          "the user has complete the section" in {
            val completeAnswers = sample[CompleteSingleDisposalTriageAnswers].copy(
              disposalMethod = DisposalMethod.Sold
            )
            testSuccessfulUpdateStartingNewDraft(
              performAction,
              completeAnswers.copy(assetType = AssetType.Residential),
              List("assetTypeForNonUkResidents" -> "1"),
              completeAnswers.copy(assetType = AssetType.NonResidential),
              checkIsRedirect(
                _,
                routes.SingleDisposalsTriageController.checkYourAnswers()
              )
            )
          }

          "the asset type has changed from indirect disposal to not indirect disposal and not mixed use" in {
            testSuccessfulUpdateStartingNewDraft(
              performAction,
              requiredPreviousAnswers.copy(assetType = Some(IndirectDisposal)),
              List("assetTypeForNonUkResidents" -> "0"),
              requiredPreviousAnswers.copy(
                assetType = Some(AssetType.Residential),
                disposalDate = None,
                completionDate = None,
                tooEarlyDisposalDate = None
              ),
              checkIsRedirect(
                _,
                routes.SingleDisposalsTriageController.checkYourAnswers()
              )
            )
          }

          "the asset type has changed from indirect disposal to mixed use" in {
            testSuccessfulUpdateStartingNewDraft(
              performAction,
              requiredPreviousAnswers.copy(assetType = Some(IndirectDisposal)),
              List("assetTypeForNonUkResidents" -> "2"),
              requiredPreviousAnswers.copy(
                assetType = Some(AssetType.MixedUse),
                disposalDate = None,
                completionDate = None,
                tooEarlyDisposalDate = None
              ),
              checkIsRedirect(
                _,
                routes.SingleDisposalsTriageController.checkYourAnswers()
              )
            )
          }

          "the asset type has changed from not indirect disposal and not mixed use to indirect disposal" in {
            val answers    = sample[CompleteSingleDisposalTriageAnswers].copy(
              assetType = NonResidential,
              disposalMethod = DisposalMethod.Sold
            )
            val newAnswers = answers
              .unset(_.disposalDate)
              .unset(_.completionDate)
              .unset(_.tooEarlyDisposalDate)
              .copy(assetType = Some(IndirectDisposal))

            testSuccessfulUpdateStartingNewDraft(
              performAction,
              answers,
              List("assetTypeForNonUkResidents" -> "3"),
              newAnswers,
              checkIsRedirect(
                _,
                routes.SingleDisposalsTriageController.checkYourAnswers()
              )
            )
          }

          "the asset type has changed from not indirect disposal and not mixed use to mixed use" in {
            val answers    = sample[CompleteSingleDisposalTriageAnswers].copy(
              assetType = NonResidential,
              disposalMethod = DisposalMethod.Sold
            )
            val newAnswers = answers
              .unset(_.disposalDate)
              .unset(_.completionDate)
              .unset(_.tooEarlyDisposalDate)
              .copy(assetType = Some(MixedUse))

            testSuccessfulUpdateStartingNewDraft(
              performAction,
              answers,
              List("assetTypeForNonUkResidents" -> "2"),
              newAnswers,
              checkIsRedirect(
                _,
                routes.SingleDisposalsTriageController.checkYourAnswers()
              )
            )
          }

          "the asset type has changed from mixed use to indirect disposal" in {
            val answers    = sample[CompleteSingleDisposalTriageAnswers].copy(
              assetType = MixedUse,
              disposalMethod = DisposalMethod.Sold
            )
            val newAnswers = answers
              .unset(_.disposalDate)
              .unset(_.completionDate)
              .unset(_.tooEarlyDisposalDate)
              .copy(
                assetType = Some(IndirectDisposal),
                disposalMethod = Some(DisposalMethod.Sold)
              )

            testSuccessfulUpdateStartingNewDraft(
              performAction,
              answers,
              List("assetTypeForNonUkResidents" -> "3"),
              newAnswers,
              checkIsRedirect(
                _,
                routes.SingleDisposalsTriageController.checkYourAnswers()
              )
            )
          }

          "the asset type has changed from mixed use to not indirect disposal" in {
            val answers    = sample[CompleteSingleDisposalTriageAnswers].copy(
              assetType = MixedUse,
              disposalMethod = DisposalMethod.Sold
            )
            val newAnswers = answers
              .unset(_.disposalDate)
              .unset(_.completionDate)
              .unset(_.tooEarlyDisposalDate)
              .copy(assetType = Some(Residential))

            testSuccessfulUpdateStartingNewDraft(
              performAction,
              answers,
              List("assetTypeForNonUkResidents" -> "0"),
              newAnswers,
              checkIsRedirect(
                _,
                routes.SingleDisposalsTriageController.checkYourAnswers()
              )
            )
          }

        }

        "the user is filling out a draft return and" when {

          "the section is complete" in {
            forAll { (completeAnswers: CompleteSingleDisposalTriageAnswers) =>
              testSuccessfulUpdateFillingOutReturn(
                performAction,
                completeAnswers.copy(
                  disposalMethod = DisposalMethod.Sold,
                  assetType = AssetType.Residential,
                  countryOfResidence = Country("HK")
                ),
                List("assetTypeForNonUkResidents" -> "1"),
                (fillingOutReturn, draftReturn) =>
                  fillingOutReturn.copy(draftReturn =
                    updateDraftReturn(
                      draftReturn,
                      completeAnswers.copy(
                        disposalMethod = DisposalMethod.Sold,
                        assetType = AssetType.NonResidential,
                        countryOfResidence = Country("HK")
                      )
                    )
                  ),
                checkIsRedirect(
                  _,
                  routes.SingleDisposalsTriageController.checkYourAnswers()
                )
              )
            }
          }

          "the section is incomplete" in {
            testSuccessfulUpdateFillingOutReturn(
              performAction,
              requiredPreviousAnswers,
              List("assetTypeForNonUkResidents" -> "1"),
              (fillingOutReturn, draftReturn) =>
                fillingOutReturn
                  .copy(draftReturn =
                    updateDraftReturn(
                      draftReturn,
                      requiredPreviousAnswers
                        .copy(assetType = Some(AssetType.NonResidential))
                    )
                  )
                  .withForceDisplayGainOrLossAfterReliefsForAmends,
              checkIsRedirect(
                _,
                routes.SingleDisposalsTriageController.checkYourAnswers()
              ),
              amendReturnData = Some(sample[AmendReturnData])
            )
          }

          "the asset type has changed from indirect disposal to not indirect disposal and not mixed use" in {
            val answers = sample[CompleteSingleDisposalTriageAnswers].copy(
              assetType = IndirectDisposal,
              disposalMethod = DisposalMethod.Sold
            )

            val newAnswers = answers
              .unset(_.disposalDate)
              .unset(_.completionDate)
              .unset(_.tooEarlyDisposalDate)
              .copy(assetType = Some(Residential))

            testSuccessfulUpdateFillingOutReturn(
              performAction,
              sample[DraftSingleIndirectDisposalReturn]
                .copy(triageAnswers = answers),
              List("assetTypeForNonUkResidents" -> "0")
            )(
              oldDraftReturn =>
                DraftSingleDisposalReturn
                  .newDraftReturn(
                    oldDraftReturn.id,
                    newAnswers,
                    oldDraftReturn.representeeAnswers
                  ),
              checkIsRedirect(
                _,
                routes.SingleDisposalsTriageController.checkYourAnswers()
              )
            )
          }

          "the asset type has changed from indirect disposal to mixed use" in {
            val answers    = sample[CompleteSingleDisposalTriageAnswers].copy(
              assetType = IndirectDisposal,
              disposalMethod = DisposalMethod.Sold
            )
            val newAnswers = answers
              .unset(_.disposalDate)
              .unset(_.completionDate)
              .unset(_.tooEarlyDisposalDate)
              .copy(assetType = Some(MixedUse))

            testSuccessfulUpdateFillingOutReturn(
              performAction,
              sample[DraftSingleIndirectDisposalReturn]
                .copy(triageAnswers = answers),
              List("assetTypeForNonUkResidents" -> "2")
            )(
              oldDraftReturn =>
                DraftSingleMixedUseDisposalReturn
                  .newDraftReturn(
                    oldDraftReturn.id,
                    newAnswers,
                    oldDraftReturn.representeeAnswers
                  ),
              checkIsRedirect(
                _,
                routes.SingleDisposalsTriageController.checkYourAnswers()
              )
            )
          }

          "the asset type has changed from not indirect disposal and not mixed use to indirect disposal" in {
            val answers    = sample[CompleteSingleDisposalTriageAnswers].copy(
              assetType = NonResidential,
              disposalMethod = DisposalMethod.Sold
            )
            val newAnswers = answers
              .unset(_.disposalDate)
              .unset(_.completionDate)
              .unset(_.tooEarlyDisposalDate)
              .copy(
                assetType = Some(IndirectDisposal),
                disposalMethod = Some(DisposalMethod.Sold)
              )

            testSuccessfulUpdateFillingOutReturn(
              performAction,
              sample[DraftSingleDisposalReturn].copy(triageAnswers = answers),
              List("assetTypeForNonUkResidents" -> "3")
            )(
              oldDraftReturn =>
                DraftSingleIndirectDisposalReturn
                  .newDraftReturn(
                    oldDraftReturn.id,
                    newAnswers,
                    oldDraftReturn.representeeAnswers
                  ),
              checkIsRedirect(
                _,
                routes.SingleDisposalsTriageController.checkYourAnswers()
              )
            )
          }

          "the asset type has changed from not indirect disposal and not mixed use to mixed use" in {
            val answers    = sample[CompleteSingleDisposalTriageAnswers].copy(
              assetType = NonResidential,
              disposalMethod = DisposalMethod.Sold
            )
            val newAnswers = answers
              .unset(_.disposalDate)
              .unset(_.completionDate)
              .unset(_.tooEarlyDisposalDate)
              .copy(assetType = Some(MixedUse))

            testSuccessfulUpdateFillingOutReturn(
              performAction,
              sample[DraftSingleDisposalReturn].copy(triageAnswers = answers),
              List("assetTypeForNonUkResidents" -> "2")
            )(
              oldDraftReturn =>
                DraftSingleMixedUseDisposalReturn
                  .newDraftReturn(
                    oldDraftReturn.id,
                    newAnswers,
                    oldDraftReturn.representeeAnswers
                  ),
              checkIsRedirect(
                _,
                routes.SingleDisposalsTriageController.checkYourAnswers()
              )
            )
          }

          "the asset type has changed from mixed use to indirect disposal" in {
            val answers    = sample[CompleteSingleDisposalTriageAnswers].copy(
              assetType = MixedUse,
              disposalMethod = DisposalMethod.Sold
            )
            val newAnswers = answers
              .unset(_.disposalDate)
              .unset(_.completionDate)
              .unset(_.tooEarlyDisposalDate)
              .copy(assetType = Some(IndirectDisposal))

            testSuccessfulUpdateFillingOutReturn(
              performAction,
              sample[DraftSingleMixedUseDisposalReturn].copy(triageAnswers = answers),
              List("assetTypeForNonUkResidents" -> "3")
            )(
              oldDraftReturn =>
                DraftSingleIndirectDisposalReturn
                  .newDraftReturn(
                    oldDraftReturn.id,
                    newAnswers,
                    oldDraftReturn.representeeAnswers
                  ),
              checkIsRedirect(
                _,
                routes.SingleDisposalsTriageController.checkYourAnswers()
              )
            )
          }

          "the asset type has changed from mixed use to not indirect disposal" in {
            val answers    = sample[CompleteSingleDisposalTriageAnswers].copy(
              assetType = MixedUse,
              disposalMethod = DisposalMethod.Sold
            )
            val newAnswers = answers
              .unset(_.disposalDate)
              .unset(_.completionDate)
              .unset(_.tooEarlyDisposalDate)
              .copy(assetType = Some(Residential))

            testSuccessfulUpdateFillingOutReturn(
              performAction,
              sample[DraftSingleMixedUseDisposalReturn].copy(triageAnswers = answers),
              List("assetTypeForNonUkResidents" -> "0")
            )(
              oldDraftReturn =>
                DraftSingleDisposalReturn
                  .newDraftReturn(
                    oldDraftReturn.id,
                    newAnswers,
                    oldDraftReturn.representeeAnswers
                  ),
              checkIsRedirect(
                _,
                routes.SingleDisposalsTriageController.checkYourAnswers()
              )
            )
          }

        }

      }

      "not do any updates" when {

        "the answers submitted is the same as the one in session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithFillingOutReturn(
                requiredPreviousAnswers.copy(
                  assetType = Some(AssetType.Residential)
                )
              )._1
            )
          }

          checkIsRedirect(
            performAction("assetTypeForNonUkResidents" -> "0"),
            routes.SingleDisposalsTriageController.checkYourAnswers()
          )
        }

      }

    }

    "handling requests to display the share disposal page for non uk residents page" must {

      val requiredPreviousAnswers =
        IncompleteSingleDisposalTriageAnswers.empty.copy(
          individualUserType = Some(Self),
          hasConfirmedSingleDisposal = true,
          disposalMethod = Some(DisposalMethod.Other),
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
        val isAmend = sample[Boolean]
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            sessionDataWithFillingOutReturn(
              requiredPreviousAnswers.copy(
                assetType = Some(AssetType.IndirectDisposal)
              ),
              amendReturnData = if (isAmend) Some(sample[AmendReturnData]) else None
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
              .attr("href")   shouldBe routes.SingleDisposalsTriageController
              .assetTypeForNonUkResidents()
              .url
            doc
              .select("#content > article > form, #main-content form")
              .attr("action") shouldBe routes.SingleDisposalsTriageController
              .disposalDateOfSharesSubmit()
              .url
            doc
              .select("#submitButton")
              .text()         shouldBe expectedSubmitText(isAmend)
          }
        )

      }

    }

    "handling submitted answers to the share disposal date for non uk residents page" must {

      def performAction(formData: (String, String)*): Future[Result] =
        controller.disposalDateOfSharesSubmit()(
          FakeRequest().withFormUrlEncodedBody(formData*).withMethod("POST")
        )

      def formData(date: LocalDate): Seq[(String, String)] =
        List(
          "sharesDisposalDate-day"   -> date.getDayOfMonth.toString,
          "sharesDisposalDate-month" -> date.getMonthValue.toString,
          "sharesDisposalDate-year"  -> date.getYear.toString
        )

      def updateDraftReturn(
        d: DraftSingleDisposalReturn,
        newAnswers: SingleDisposalTriageAnswers
      ): DraftSingleDisposalReturn =
        d.copy(
          triageAnswers = newAnswers,
          acquisitionDetailsAnswers = d.acquisitionDetailsAnswers.map(_.unsetAllButAcquisitionMethod(d.triageAnswers)),
          initialGainOrLoss = None,
          reliefDetailsAnswers = d.reliefDetailsAnswers.map(_.unsetPrrAndLettingRelief(newAnswers.isPeriodOfAdmin)),
          yearToDateLiabilityAnswers = d.yearToDateLiabilityAnswers.flatMap {
            case _: NonCalculatedYTDAnswers => None
            case c: CalculatedYTDAnswers    =>
              Some(
                c.unset(_.hasEstimatedDetails)
                  .unset(_.calculatedTaxDue)
                  .unset(_.taxDue)
                  .unset(_.mandatoryEvidence)
                  .unset(_.expiredEvidence)
                  .unset(_.pendingUpscanUpload)
              )
          },
          supportingEvidenceAnswers = None,
          gainOrLossAfterReliefs = None
        )

      val tomorrow = today.plusDays(1L)

      val requiredPreviousAnswers =
        IncompleteSingleDisposalTriageAnswers.empty.copy(
          individualUserType = Some(Self),
          hasConfirmedSingleDisposal = true,
          disposalMethod = Some(DisposalMethod.Other),
          wasAUKResident = Some(false),
          assetType = Some(AssetType.IndirectDisposal)
        )

      behave like redirectToStartWhenInvalidJourney(() => performAction(), isValidJourney)

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.disposalDateOfSharesSubmit(),
        mockUUIDGenerator
      )

      behave like redirectWhenNoPreviousAnswerBehaviour[Boolean](() => performAction())(
        requiredPreviousAnswers,
        routes.SingleDisposalsTriageController.assetTypeForNonUkResidents(),
        { case (answers, w) =>
          answers.copy(assetType =
            w.map(
              if (_) AssetType.Residential else AssetType.IndirectDisposal
            )
          )
        }
      )

      behave like noDateOfDeathForPersonalRepBehaviour(() => performAction())

      "show an error page" when {

        "there is a problem getting the tax year" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithFillingOutReturn(requiredPreviousAnswers)._1
            )
            mockGetTaxYear(today)(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction(formData(today)*))
        }

      }

      "show a form error" when {

        def test(
          individualUserType: Option[IndividualUserType] = Some(Self),
          representeeAnswers: Option[RepresenteeAnswers] = None
        )(formData: Seq[(String, String)], expectedErrorKey: String): Unit =
          testFormError(performAction, "sharesDisposalDate.title")(
            formData,
            expectedErrorKey,
            requiredPreviousAnswers.copy(individualUserType = individualUserType),
            representeeAnswers
          )

        "the date is invalid" in {
          dateErrorScenarios("sharesDisposalDate", "").foreach { scenario =>
            withClue(s"For $scenario: ") {
              val formData = List(
                "sharesDisposalDate-day"   -> scenario.dayInput,
                "sharesDisposalDate-month" -> scenario.monthInput,
                "sharesDisposalDate-year"  -> scenario.yearInput
              ).collect { case (id, Some(input)) => id -> input }
              test()(formData, scenario.expectedErrorMessageKey)
            }
          }
        }

        "the disposal date is in the future" in {
          DateErrorScenario(
            Some(tomorrow.getDayOfMonth.toString),
            Some(tomorrow.getMonthValue.toString),
            Some(tomorrow.getYear.toString),
            "sharesDisposalDate.error.tooFarInFuture"
          )
          val futureDate = today.plusYears(2).plusDays(1L)
          test()(formData(futureDate), "sharesDisposalDate.error.tooFarInFuture")
        }

        "the disposal date is strictly after the date of death and the user is a non-period of admin personal rep" in {
          test(
            Some(PersonalRepresentative),
            Some(sample[CompleteRepresenteeAnswers].copy(dateOfDeath = Some(DateOfDeath(today.minusDays(1L)))))
          )(
            formData(today),
            "sharesDisposalDate.error.nonPeriodOfAdminDeathAfterDate"
          )
        }

        "the disposal date is before the date of death and the user is a period of admin personal rep" in {
          test(
            Some(PersonalRepresentativeInPeriodOfAdmin),
            Some(sample[CompleteRepresenteeAnswers].copy(dateOfDeath = Some(DateOfDeath(today))))
          )(
            formData(today.minusDays(1L)),
            "sharesDisposalDate.error.periodOfAdminDeathNotAfterDate"
          )
        }

        "the disposal date is strictly before the date of death and the user is a period of admin personal rep" in {
          test(
            Some(PersonalRepresentativeInPeriodOfAdmin),
            Some(sample[CompleteRepresenteeAnswers].copy(dateOfDeath = Some(DateOfDeath(today))))
          )(
            formData(today.minusDays(1L)),
            "sharesDisposalDate.error.periodOfAdminDeathNotAfterDate"
          )
        }

      }

      behave like unsuccessfulUpdatesBehaviour(
        performAction,
        requiredPreviousAnswers.copy(
          disposalMethod = Some(DisposalMethod.Sold)
        ),
        formData(today),
        requiredPreviousAnswers.copy(
          disposalMethod = Some(DisposalMethod.Sold),
          disposalDate = Some(DisposalDate(today, taxYear)),
          completionDate = Some(CompletionDate(today))
        ),
        updateDraftReturn,
        () => mockGetTaxYear(today)(Right(Some(taxYear)))
      )

      "handle valid dates" when {

        "the user is starting in a draft return and" when {

          "no tax year can be found for the given disposal date" in {

            testSuccessfulUpdateStartingNewDraft(
              performAction,
              requiredPreviousAnswers.copy(
                disposalMethod = Some(DisposalMethod.Sold)
              ),
              formData(today),
              requiredPreviousAnswers
                .copy(
                  disposalMethod = Some(DisposalMethod.Sold),
                  tooEarlyDisposalDate = Some(today),
                  disposalDate = None,
                  completionDate = Some(CompletionDate(today))
                ),
              checkIsRedirect(
                _,
                routes.CommonTriageQuestionsController.disposalsOfSharesTooEarly()
              ),
              () => mockGetTaxYear(today)(Right(None))
            )

          }

          "a tax year can be found and the journey was complete" in {

            val completeJourney = sample[CompleteSingleDisposalTriageAnswers].copy(
              individualUserType = Some(Self),
              disposalDate = DisposalDate(today, taxYear),
              completionDate = CompletionDate(today),
              assetType = IndirectDisposal,
              disposalMethod = DisposalMethod.Sold,
              alreadySentSelfAssessment = None
            )

            val date = today.minusDays(1L)

            testSuccessfulUpdateStartingNewDraft(
              performAction,
              completeJourney,
              formData(date),
              IncompleteSingleDisposalTriageAnswers(
                completeJourney.individualUserType,
                hasConfirmedSingleDisposal = true,
                Some(completeJourney.disposalMethod),
                Some(completeJourney.countryOfResidence.isUk),
                if (completeJourney.countryOfResidence.isUk) None else Some(completeJourney.countryOfResidence),
                Some(completeJourney.assetType),
                Some(DisposalDate(date, taxYear)),
                completeJourney.alreadySentSelfAssessment,
                Some(CompletionDate(date)),
                None
              ),
              checkIsRedirect(
                _,
                routes.SingleDisposalsTriageController.checkYourAnswers()
              ),
              () => mockGetTaxYear(date)(Right(Some(taxYear)))
            )

          }

          "invalid tax year was found with disposal date and the journey was complete" in {

            val disposalDate    = LocalDate.of(2025, 4, 7)
            val taxYear         = sample[TaxYear].copy(
              startDateInclusive = LocalDate.of(2024, 4, 6),
              endDateExclusive = LocalDate.of(2025, 4, 6)
            )
            val expectedTaxYear = taxYear.copy(
              startDateInclusive = LocalDate.of(2025, 4, 6),
              endDateExclusive = LocalDate.of(2026, 4, 6)
            )

            val completeJourney = sample[CompleteSingleDisposalTriageAnswers].copy(
              individualUserType = Some(Self),
              disposalDate = DisposalDate(disposalDate, taxYear),
              completionDate = CompletionDate(disposalDate),
              assetType = IndirectDisposal,
              disposalMethod = DisposalMethod.Sold,
              alreadySentSelfAssessment = None
            )

            val date = disposalDate.minusDays(1L)

            testSuccessfulUpdateStartingNewDraft(
              performAction,
              completeJourney,
              formData(date),
              IncompleteSingleDisposalTriageAnswers(
                completeJourney.individualUserType,
                hasConfirmedSingleDisposal = true,
                Some(completeJourney.disposalMethod),
                Some(completeJourney.countryOfResidence.isUk),
                if (completeJourney.countryOfResidence.isUk) None else Some(completeJourney.countryOfResidence),
                Some(completeJourney.assetType),
                Some(DisposalDate(date, expectedTaxYear)),
                completeJourney.alreadySentSelfAssessment,
                Some(CompletionDate(date)),
                None
              ),
              checkIsRedirect(
                _,
                routes.SingleDisposalsTriageController.checkYourAnswers()
              ),
              () => mockGetTaxYear(date)(Right(Some(expectedTaxYear)))
            )

          }

        }

        "the user is filling out a draft return and" when {

          "the section is incomplete" in {
            testSuccessfulUpdateFillingOutReturn(
              performAction,
              requiredPreviousAnswers.copy(
                disposalMethod = Some(DisposalMethod.Sold)
              ),
              formData(today),
              (fillingOutReturn, draftReturn) =>
                fillingOutReturn
                  .copy(draftReturn =
                    updateDraftReturn(
                      draftReturn,
                      requiredPreviousAnswers.copy(
                        disposalDate = Some(DisposalDate(today, taxYear)),
                        completionDate = Some(CompletionDate(today)),
                        disposalMethod = Some(DisposalMethod.Sold)
                      )
                    )
                  )
                  .withForceDisplayGainOrLossAfterReliefsForAmends,
              checkIsRedirect(
                _,
                routes.CommonTriageQuestionsController.amendReturnDisposalDateDifferentTaxYear()
              ),
              () => mockGetTaxYear(today)(Right(Some(taxYear))),
              amendReturnData = Some(sample[AmendReturnData])
            )

          }

          "the section is complete" in {
            forAll { (c: CompleteSingleDisposalTriageAnswers) =>
              val completeJourney = c.copy(
                individualUserType = Some(Self),
                disposalDate = DisposalDate(today, taxYear),
                completionDate = CompletionDate(today),
                assetType = IndirectDisposal,
                disposalMethod = DisposalMethod.Sold,
                alreadySentSelfAssessment = None
              )
              val date            = today.minusDays(1L)
              val newAnswers      =
                IncompleteSingleDisposalTriageAnswers(
                  completeJourney.individualUserType,
                  hasConfirmedSingleDisposal = true,
                  Some(completeJourney.disposalMethod),
                  Some(completeJourney.countryOfResidence.isUk),
                  if (completeJourney.countryOfResidence.isUk) None else Some(completeJourney.countryOfResidence),
                  Some(completeJourney.assetType),
                  Some(DisposalDate(date, taxYear)),
                  completeJourney.alreadySentSelfAssessment,
                  Some(CompletionDate(date)),
                  None
                )

              testSuccessfulUpdateFillingOutReturn(
                performAction,
                completeJourney,
                formData(date),
                (fillingOutReturn, draftReturn) =>
                  fillingOutReturn
                    .copy(
                      draftReturn = updateDraftReturn(draftReturn, newAnswers)
                    )
                    .withForceDisplayGainOrLossAfterReliefsForAmends,
                checkIsRedirect(
                  _,
                  routes.CommonTriageQuestionsController.amendReturnDisposalDateDifferentTaxYear()
                ),
                () => mockGetTaxYear(date)(Right(Some(taxYear))),
                amendReturnData = Some(sample[AmendReturnData])
              )
            }
          }

          "the disposal date is on the date of death when the user is a non-period of admin personal rep" in {
            val answers = requiredPreviousAnswers.copy(individualUserType = Some(PersonalRepresentative))

            testSuccessfulUpdateFillingOutReturn(
              performAction,
              answers,
              formData(today),
              (fillingOutReturn, draftReturn) =>
                fillingOutReturn
                  .copy(draftReturn =
                    updateDraftReturn(
                      draftReturn,
                      answers.copy(
                        disposalDate = Some(DisposalDate(today, taxYear)),
                        completionDate = Some(CompletionDate(today))
                      )
                    )
                  )
                  .withForceDisplayGainOrLossAfterReliefsForAmends,
              checkIsRedirect(
                _,
                routes.CommonTriageQuestionsController.amendReturnDisposalDateDifferentTaxYear()
              ),
              () => mockGetTaxYear(today)(Right(Some(taxYear))),
              Some(sample[CompleteRepresenteeAnswers].copy(dateOfDeath = Some(DateOfDeath(today)))),
              amendReturnData = Some(sample[AmendReturnData])
            )

          }

          "the disposal date is strictly before the date of death when the user is a non-period of admin personal rep" in {
            val answers         = requiredPreviousAnswers.copy(individualUserType = Some(PersonalRepresentative))
            val newDisposalDate = DisposalDate(today.minusDays(1L), taxYear)

            testSuccessfulUpdateFillingOutReturn(
              performAction,
              answers,
              formData(newDisposalDate.value),
              (fillingOutReturn, draftReturn) =>
                fillingOutReturn
                  .copy(draftReturn =
                    updateDraftReturn(
                      draftReturn,
                      answers.copy(
                        disposalDate = Some(newDisposalDate),
                        completionDate = Some(CompletionDate(newDisposalDate.value))
                      )
                    )
                  )
                  .withForceDisplayGainOrLossAfterReliefsForAmends,
              checkIsRedirect(
                _,
                routes.CommonTriageQuestionsController.amendReturnDisposalDateDifferentTaxYear()
              ),
              () => mockGetTaxYear(newDisposalDate.value)(Right(Some(taxYear))),
              Some(sample[CompleteRepresenteeAnswers].copy(dateOfDeath = Some(DateOfDeath(today)))),
              amendReturnData = Some(sample[AmendReturnData])
            )
          }

          "the disposal date is strictly after the date of death when the user is a period of admin personal rep" in {
            val answers         = requiredPreviousAnswers.copy(individualUserType = Some(PersonalRepresentativeInPeriodOfAdmin))
            val newDisposalDate = DisposalDate(today, taxYear)

            testSuccessfulUpdateFillingOutReturn(
              performAction,
              answers,
              formData(newDisposalDate.value),
              (fillingOutReturn, draftReturn) =>
                fillingOutReturn
                  .copy(draftReturn =
                    updateDraftReturn(
                      draftReturn,
                      answers.copy(
                        disposalDate = Some(newDisposalDate),
                        completionDate = Some(CompletionDate(newDisposalDate.value)),
                        disposalMethod = Some(DisposalMethod.Sold)
                      )
                    )
                  )
                  .withForceDisplayGainOrLossAfterReliefsForAmends,
              checkIsRedirect(
                _,
                routes.CommonTriageQuestionsController.amendReturnDisposalDateDifferentTaxYear()
              ),
              () => mockGetTaxYear(newDisposalDate.value)(Right(Some(taxYear))),
              Some(sample[CompleteRepresenteeAnswers].copy(dateOfDeath = Some(DateOfDeath(today.minusDays(1L))))),
              amendReturnData = Some(sample[AmendReturnData])
            )
          }

        }

      }

      "redirect to amend return disposaldate different taxtear page" when {

        val date = today.minusYears(20)

        "the user is filling out a draft return and entered invalid taxyear" when {

          "the section is incomplete" in {
            testSuccessfulUpdateFillingOutReturn(
              performAction,
              requiredPreviousAnswers.copy(
                disposalMethod = Some(DisposalMethod.Sold),
                completionDate = Some(CompletionDate(date)),
                alreadySentSelfAssessment = None
              ),
              formData(date),
              (fillingOutReturn, draftReturn) =>
                fillingOutReturn
                  .copy(draftReturn =
                    updateDraftReturn(
                      draftReturn,
                      requiredPreviousAnswers.copy(
                        disposalDate = None,
                        tooEarlyDisposalDate = Some(date),
                        completionDate = Some(CompletionDate(date)),
                        disposalMethod = Some(DisposalMethod.Sold),
                        alreadySentSelfAssessment = None
                      )
                    )
                  )
                  .withForceDisplayGainOrLossAfterReliefsForAmends,
              checkIsRedirect(
                _,
                routes.CommonTriageQuestionsController.amendReturnDisposalDateDifferentTaxYear()
              ),
              () => mockGetTaxYear(date)(Right(None)),
              amendReturnData = Some(sample[AmendReturnData])
            )

          }

          "the section is complete" in {
            forAll { (c: CompleteSingleDisposalTriageAnswers) =>
              val completeJourney = c.copy(
                individualUserType = Some(Self),
                disposalDate = DisposalDate(today, taxYear),
                completionDate = CompletionDate(today),
                assetType = IndirectDisposal,
                disposalMethod = DisposalMethod.Sold,
                alreadySentSelfAssessment = None
              )
              val newAnswers      =
                IncompleteSingleDisposalTriageAnswers(
                  completeJourney.individualUserType,
                  hasConfirmedSingleDisposal = true,
                  Some(completeJourney.disposalMethod),
                  Some(completeJourney.countryOfResidence.isUk),
                  if (completeJourney.countryOfResidence.isUk) None else Some(completeJourney.countryOfResidence),
                  Some(completeJourney.assetType),
                  None,
                  completeJourney.alreadySentSelfAssessment,
                  Some(CompletionDate(date)),
                  Some(date)
                )

              testSuccessfulUpdateFillingOutReturn(
                performAction,
                completeJourney,
                formData(date),
                (fillingOutReturn, draftReturn) =>
                  fillingOutReturn
                    .copy(
                      draftReturn = updateDraftReturn(draftReturn, newAnswers)
                    )
                    .withForceDisplayGainOrLossAfterReliefsForAmends,
                checkIsRedirect(
                  _,
                  routes.CommonTriageQuestionsController.amendReturnDisposalDateDifferentTaxYear()
                ),
                () => mockGetTaxYear(date)(Right(None)),
                amendReturnData = Some(sample[AmendReturnData])
              )
            }
          }

        }

      }

      "not do any updates" when {

        "the answers submitted is the same as the one in session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithFillingOutReturn(
                requiredPreviousAnswers.copy(
                  disposalDate = Some(DisposalDate(today, taxYear))
                )
              )._1
            )
          }

          checkIsRedirect(
            performAction(formData(today)*),
            routes.SingleDisposalsTriageController.checkYourAnswers()
          )
        }

      }

    }

    "handling requests to display the check your answers page" must {

      def performAction(): Future[Result] =
        controller.checkYourAnswers()(FakeRequest())

      val completeTriageQuestions =
        CompleteSingleDisposalTriageAnswers(
          Some(IndividualUserType.Self),
          DisposalMethod.Sold,
          Country.uk,
          assetType = AssetType.Residential,
          sample[DisposalDate],
          Some(false),
          sample[CompletionDate]
        )

      val allQuestionsAnswered = IncompleteSingleDisposalTriageAnswers(
        completeTriageQuestions.individualUserType,
        hasConfirmedSingleDisposal = true,
        Some(completeTriageQuestions.disposalMethod),
        Some(true),
        None,
        Some(completeTriageQuestions.assetType),
        Some(completeTriageQuestions.disposalDate),
        Some(false),
        Some(completeTriageQuestions.completionDate),
        None
      )

      "redirect to the correct page" when {

        case class Scenario(
          answers: SingleDisposalTriageAnswers,
          name: Either[TrustName, IndividualName],
          expectedRedirect: Call,
          previousReturnSummaries: Option[List[ReturnSummary]]
        )

        def test(
          sessionDataWith: (
            SingleDisposalTriageAnswers,
            Either[TrustName, IndividualName],
            Option[List[ReturnSummary]]
          ) => SessionData
        ): Unit =
          List(
            Scenario(
              allQuestionsAnswered.copy(individualUserType = None),
              Right(sample[IndividualName]),
              routes.CommonTriageQuestionsController
                .whoIsIndividualRepresenting(),
              None
            ),
            Scenario(
              allQuestionsAnswered.copy(
                individualUserType = None,
                hasConfirmedSingleDisposal = false
              ),
              Left(sample[TrustName]),
              routes.CommonTriageQuestionsController.howManyProperties(),
              None
            ),
            Scenario(
              allQuestionsAnswered.copy(hasConfirmedSingleDisposal = false),
              Right(sample[IndividualName]),
              routes.CommonTriageQuestionsController.howManyProperties(),
              None
            ),
            Scenario(
              allQuestionsAnswered.copy(disposalMethod = None),
              Right(sample[IndividualName]),
              routes.SingleDisposalsTriageController
                .howDidYouDisposeOfProperty(),
              None
            ),
            Scenario(
              allQuestionsAnswered.copy(wasAUKResident = None),
              Right(sample[IndividualName]),
              routes.SingleDisposalsTriageController.wereYouAUKResident(),
              None
            ),
            Scenario(
              allQuestionsAnswered.copy(assetType = None),
              Right(sample[IndividualName]),
              routes.SingleDisposalsTriageController
                .didYouDisposeOfAResidentialProperty(),
              None
            ),
            Scenario(
              allQuestionsAnswered.copy(disposalDate = None),
              Right(sample[IndividualName]),
              routes.SingleDisposalsTriageController
                .whenWasDisposalDate(),
              None
            ),
            Scenario(
              allQuestionsAnswered.copy(completionDate = None),
              Right(sample[IndividualName]),
              routes.SingleDisposalsTriageController
                .whenWasCompletionDate(),
              None
            ),
            Scenario(
              allQuestionsAnswered.copy(individualUserType = None),
              Left(sample[TrustName]),
              routes.CommonTriageQuestionsController
                .previousReturnExistsWithSameCompletionDate(),
              Some(List(sample[ReturnSummary].copy(completionDate = completeTriageQuestions.completionDate.value)))
            ),
            Scenario(
              allQuestionsAnswered.copy(individualUserType = Some(Self)),
              Right(sample[IndividualName]),
              routes.CommonTriageQuestionsController
                .previousReturnExistsWithSameCompletionDate(),
              Some(List(sample[ReturnSummary].copy(completionDate = completeTriageQuestions.completionDate.value)))
            ),
            Scenario(
              allQuestionsAnswered
                .copy(wasAUKResident = Some(false), countryOfResidence = None),
              Right(sample[IndividualName]),
              routes.SingleDisposalsTriageController
                .countryOfResidence(),
              None
            ),
            Scenario(
              allQuestionsAnswered
                .copy(
                  wasAUKResident = Some(true),
                  assetType = Some(AssetType.NonResidential)
                ),
              Right(sample[IndividualName]),
              routes.CommonTriageQuestionsController
                .ukResidentCanOnlyDisposeResidential(),
              None
            ),
            Scenario(
              allQuestionsAnswered
                .copy(
                  wasAUKResident = Some(false),
                  countryOfResidence = Some(sample[Country]),
                  assetType = Some(AssetType.IndirectDisposal),
                  completionDate = None,
                  disposalDate = None
                ),
              Right(sample[IndividualName]),
              routes.SingleDisposalsTriageController.disposalDateOfShares(),
              None
            ),
            Scenario(
              allQuestionsAnswered
                .copy(
                  wasAUKResident = Some(false),
                  countryOfResidence = Some(sample[Country]),
                  assetType = Some(AssetType.IndirectDisposal)
                ),
              Right(sample[IndividualName]),
              routes.CommonTriageQuestionsController.previousReturnExistsWithSameCompletionDate(),
              Some(List(sample[ReturnSummary].copy(completionDate = completeTriageQuestions.completionDate.value)))
            ),
            Scenario(
              allQuestionsAnswered.copy(individualUserType = Some(IndividualUserType.Capacitor)),
              Right(sample[IndividualName]),
              representee.routes.RepresenteeController.checkYourAnswers(),
              None
            ),
            Scenario(
              allQuestionsAnswered.copy(individualUserType = Some(IndividualUserType.PersonalRepresentative)),
              Right(sample[IndividualName]),
              representee.routes.RepresenteeController.checkYourAnswers(),
              None
            ),
            Scenario(
              allQuestionsAnswered.copy(individualUserType =
                Some(IndividualUserType.PersonalRepresentativeInPeriodOfAdmin)
              ),
              Right(sample[IndividualName]),
              representee.routes.RepresenteeController.checkYourAnswers(),
              None
            )
          ).foreach { case Scenario(state, name, expectedRedirect, previousSentReturns) =>
            withClue(
              s"For state $state and expected redirect url ${expectedRedirect.url}: "
            ) {
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(sessionDataWith(state, name, previousSentReturns))
              }

              checkIsRedirect(performAction(), expectedRedirect)
            }
          }

        "a question has not yet been answered and a draft return has not been created" in {
          test(
            sessionDataWithStartingNewDraftReturn(
              _,
              _,
              representeeAnswers = Some(sample[IncompleteRepresenteeAnswers].copy(dateOfDeath = None)),
              _
            )._1
          )
        }

        "a question has not yet been answered and a draft return has been created" in {
          test(
            sessionDataWithFillingOutReturn(
              _,
              _,
              representeeAnswers = Some(sample[IncompleteRepresenteeAnswers].copy(dateOfDeath = None)),
              _
            )._1
          )
        }

      }

      "show an error page" when {

        "all the questions have now been answered but the session data cannot be updated" in {
          val (session, journey) = sessionDataWithStartingNewDraftReturn(
            allQuestionsAnswered,
            representeeAnswers = Some(sample[CompleteRepresenteeAnswers].copy(dateOfDeath = None))
          )
          val updatedJourney     = journey.copy(newReturnTriageAnswers = Right(completeTriageQuestions))
          val updatedSession     =
            session.copy(journeyStatus = Some(updatedJourney))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreSession(updatedSession)(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction())
        }
      }

      "display the page" when {

        def testIsCheckYourAnswers(
          result: Future[Result],
          completeSingleDisposalTriageAnswers: CompleteSingleDisposalTriageAnswers,
          expectedTitleKey: String,
          userType: Option[UserType]
        ): Unit =
          checkPageIsDisplayed(
            result,
            messageFromMessageKey(expectedTitleKey),
            doc =>
              validateSingleDisposalTriageCheckYourAnswersPage(
                completeSingleDisposalTriageAnswers,
                userType,
                doc
              )
          )

        "all the questions have now been answered and the session is updated when a draft return has not yet been created" in {
          val (session, journey) = sessionDataWithStartingNewDraftReturn(
            allQuestionsAnswered,
            representeeAnswers = Some(sample[CompleteRepresenteeAnswers].copy(dateOfDeath = None))
          )
          val updatedJourney     = journey.copy(newReturnTriageAnswers = Right(completeTriageQuestions))
          val updatedSession     =
            session.copy(journeyStatus = Some(updatedJourney))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreSession(updatedSession)(Right(()))
          }

          testIsCheckYourAnswers(
            performAction(),
            completeTriageQuestions,
            "triage.check-your-answers.title",
            None
          )
        }

        "all the questions have now been answered and the session is updated when a draft return has been created" in {
          val (session, journey, draftReturn) = sessionDataWithFillingOutReturn(
            allQuestionsAnswered,
            representeeAnswers = Some(sample[CompleteRepresenteeAnswers].copy(dateOfDeath = None))
          )
          val updatedJourney                  =
            journey.copy(draftReturn = draftReturn.copy(triageAnswers = completeTriageQuestions))
          val updatedSession                  =
            session.copy(journeyStatus = Some(updatedJourney))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreSession(updatedSession)(Right(()))
          }

          testIsCheckYourAnswers(
            performAction(),
            completeTriageQuestions,
            "triage.check-your-answers.title",
            None
          )
        }

        "all the questions have already been answered and a draft return has not yet been created" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithStartingNewDraftReturn(
                completeTriageQuestions,
                representeeAnswers = Some(sample[CompleteRepresenteeAnswers].copy(dateOfDeath = None))
              )._1
            )
          }

          testIsCheckYourAnswers(
            performAction(),
            completeTriageQuestions,
            "triage.check-your-answers.title",
            None
          )
        }

        "all the questions have already been answered and a draft return has been created" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithFillingOutReturn(
                completeTriageQuestions,
                representeeAnswers = Some(sample[CompleteRepresenteeAnswers].copy(dateOfDeath = None))
              )._1
            )
          }

          testIsCheckYourAnswers(
            performAction(),
            completeTriageQuestions,
            "triage.check-your-answers.title",
            None
          )
        }

        "display shares disposal date in case of indirect disposal" in {
          val completeTriageQuestionsWithIndirectDisposal = completeTriageQuestions.copy(
            assetType = AssetType.IndirectDisposal,
            countryOfResidence = Country("TR")
          )
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithFillingOutReturn(
                completeTriageQuestionsWithIndirectDisposal,
                representeeAnswers = Some(sample[CompleteRepresenteeAnswers].copy(dateOfDeath = None))
              )._1
            )
          }

          testIsCheckYourAnswers(
            performAction(),
            completeTriageQuestionsWithIndirectDisposal,
            "triage.check-your-answers.title",
            None
          )
        }

        "the user is an agent" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithFillingOutReturn(
                completeTriageQuestions,
                representeeAnswers = Some(sample[CompleteRepresenteeAnswers].copy(dateOfDeath = None))
              )._1.copy(userType = Some(UserType.Agent))
            )
          }

          testIsCheckYourAnswers(
            performAction(),
            completeTriageQuestions,
            "triage.check-your-answers.title",
            Some(UserType.Agent)
          )
        }

        "there is a previous return with the same completion date that has been submitted but" when {

          def test(representativeType: RepresentativeType): Unit = {
            val triageAnswers = completeTriageQuestions.copy(individualUserType = Some(representativeType))
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionDataWithFillingOutReturn(
                  triageAnswers,
                  representeeAnswers = Some(sample[CompleteRepresenteeAnswers].copy(dateOfDeath = None)),
                  previousSentReturns = Some(
                    List(sample[ReturnSummary].copy(completionDate = triageAnswers.completionDate.value))
                  )
                )._1
              )
            }

            testIsCheckYourAnswers(
              performAction(),
              triageAnswers,
              "triage.check-your-answers.title",
              Some(UserType.Individual)
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

          "the user is on an amend return journey where the completion date has not changed" in {
            val originalReturnSummary =
              sample[ReturnSummary].copy(completionDate = completeTriageQuestions.completionDate.value)

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionDataWithFillingOutReturn(
                  completeTriageQuestions,
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
              messageFromMessageKey("triage.check-your-answers.title")
            )
          }

        }

      }

    }

    "handling submit requests from the check your answers page" must {

      def performAction(): Future[Result] =
        controller.checkYourAnswersSubmit()(FakeRequest())

      val completeAnswers = sample[CompleteSingleDisposalTriageAnswers].copy(
        assetType = AssetType.Residential,
        disposalMethod = DisposalMethod.Sold
      )

      val startingNewDraftReturn = sample[StartingNewDraftReturn]
        .copy(newReturnTriageAnswers = Right(completeAnswers))

      val sessionWithCompleteStartingNewDraftReturn =
        SessionData.empty.copy(journeyStatus = Some(startingNewDraftReturn))

      val uuid = UUID.randomUUID()

      val fillingOutReturn = FillingOutReturn(
        startingNewDraftReturn.subscribedDetails,
        startingNewDraftReturn.ggCredId,
        startingNewDraftReturn.agentReferenceNumber,
        DraftSingleDisposalReturn(
          uuid,
          completeAnswers,
          None,
          None,
          None,
          None,
          None,
          None,
          None,
          None,
          startingNewDraftReturn.representeeAnswers,
          None,
          TimeUtils.today()
        ),
        startingNewDraftReturn.previousSentReturns,
        None
      )

      val sessionDataWithFillingOutDraftReturn =
        SessionData.empty.copy(journeyStatus = Some(fillingOutReturn))

      "redirect to the check your answers page" when {

        "the user has not answered all the questions in the triage section" in {
          val incompleteAnswers =
            sample[IncompleteSingleDisposalTriageAnswers].copy(
              individualUserType = Some(IndividualUserType.Self)
            )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithStartingNewDraftReturn(incompleteAnswers)._1
            )
          }

          checkIsRedirect(
            performAction(),
            routes.SingleDisposalsTriageController.checkYourAnswers()
          )
        }

      }

      "show an error page" when {

        "there is a problem storing a draft return" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionWithCompleteStartingNewDraftReturn)
            mockGetNextUUID(uuid)
            mockStoreDraftReturn(fillingOutReturn)(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction())
        }

        "there is a problem updating the session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionWithCompleteStartingNewDraftReturn)
            mockGetNextUUID(uuid)
            mockStoreDraftReturn(fillingOutReturn)(Right(()))
            mockStoreSession(sessionDataWithFillingOutDraftReturn)(
              Left(Error(""))
            )
          }

          checkIsTechnicalErrorPage(performAction())
        }

      }

      "redirect to the task list page" when {

        "the draft return is stored and the session is updated and a draft return had not already been created" when {

          "the asset type is not indirect disposals or mixed use" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(sessionWithCompleteStartingNewDraftReturn)
              mockGetNextUUID(uuid)
              mockStoreDraftReturn(fillingOutReturn)(Right(()))
              mockStoreSession(sessionDataWithFillingOutDraftReturn)(Right(()))
            }

            checkIsRedirect(
              performAction(),
              returnsRoutes.TaskListController.taskList()
            )
          }

          "the asset type is indirect disposal" in {
            val answers                                   = completeAnswers.copy(assetType = IndirectDisposal)
            val sessionWithCompleteStartingNewDraftReturn =
              SessionData.empty
                .copy(journeyStatus = Some(startingNewDraftReturn.copy(newReturnTriageAnswers = Right(answers))))

            val fillingOutReturn                     = FillingOutReturn(
              startingNewDraftReturn.subscribedDetails,
              startingNewDraftReturn.ggCredId,
              startingNewDraftReturn.agentReferenceNumber,
              DraftSingleIndirectDisposalReturn(
                uuid,
                answers,
                None,
                None,
                None,
                None,
                None,
                None,
                startingNewDraftReturn.representeeAnswers,
                None,
                TimeUtils.today()
              ),
              startingNewDraftReturn.previousSentReturns,
              None
            )
            val sessionDataWithFillingOutDraftReturn =
              SessionData.empty.copy(journeyStatus = Some(fillingOutReturn))

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(sessionWithCompleteStartingNewDraftReturn)
              mockGetNextUUID(uuid)
              mockStoreDraftReturn(fillingOutReturn)(Right(()))
              mockStoreSession(sessionDataWithFillingOutDraftReturn)(Right(()))
            }

            checkIsRedirect(
              performAction(),
              returnsRoutes.TaskListController.taskList()
            )
          }

          "the asset type is mixed use" in {
            val answers                                   = completeAnswers.copy(assetType = MixedUse)
            val sessionWithCompleteStartingNewDraftReturn =
              SessionData.empty
                .copy(journeyStatus = Some(startingNewDraftReturn.copy(newReturnTriageAnswers = Right(answers))))

            val fillingOutReturn                     = FillingOutReturn(
              startingNewDraftReturn.subscribedDetails,
              startingNewDraftReturn.ggCredId,
              startingNewDraftReturn.agentReferenceNumber,
              DraftSingleMixedUseDisposalReturn(
                uuid,
                answers,
                None,
                None,
                None,
                None,
                startingNewDraftReturn.representeeAnswers,
                None,
                TimeUtils.today()
              ),
              startingNewDraftReturn.previousSentReturns,
              None
            )
            val sessionDataWithFillingOutDraftReturn =
              SessionData.empty.copy(journeyStatus = Some(fillingOutReturn))

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(sessionWithCompleteStartingNewDraftReturn)
              mockGetNextUUID(uuid)
              mockStoreDraftReturn(fillingOutReturn)(Right(()))
              mockStoreSession(sessionDataWithFillingOutDraftReturn)(Right(()))
            }

            checkIsRedirect(
              performAction(),
              returnsRoutes.TaskListController.taskList()
            )
          }

        }

        "the draft return is stored and the session is updated and a draft return had already been created" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionDataWithFillingOutDraftReturn)
          }

          checkIsRedirect(
            performAction(),
            returnsRoutes.TaskListController.taskList()
          )
        }

      }

    }

  }

  def redirectWhenNoPreviousAnswerBehaviour[A](
    performAction: () => Future[Result]
  )(
    requiredPreviousAnswers: IncompleteSingleDisposalTriageAnswers,
    redirectToPreviousAnswer: => Call,
    setPreviousAnswer: (
      IncompleteSingleDisposalTriageAnswers,
      Option[A]
    ) => IncompleteSingleDisposalTriageAnswers
  ): Unit =
    s"redirect to ${redirectToPreviousAnswer.url}" when {

      "that question has not already answered" in {

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            sessionDataWithStartingNewDraftReturn(
              setPreviousAnswer(requiredPreviousAnswers, None)
            )._1
          )
        }

        checkIsRedirect(performAction(), redirectToPreviousAnswer)
      }
    }

  def testFormError(
    performAction: Seq[(String, String)] => Future[Result],
    pageTitleKey: String
  )(
    formData: Seq[(String, String)],
    expectedErrorMessageKey: String,
    currentAnswers: SingleDisposalTriageAnswers,
    representeeAnswers: Option[RepresenteeAnswers] = None
  ): Unit = {
    inSequence {
      mockAuthWithNoRetrievals()
      mockGetSession(sessionDataWithStartingNewDraftReturn(currentAnswers, representeeAnswers = representeeAnswers)._1)
    }

    val result  = performAction(formData)
    val content = contentAsString(result)

    status(result)        shouldBe BAD_REQUEST
    contentAsString(result) should include(messageFromMessageKey(pageTitleKey))
    content                 should include(messageFromMessageKey(expectedErrorMessageKey))
  }

  def testFormErrorWithErrorArg(
    performAction: Seq[(String, String)] => Future[Result],
    pageTitleKey: String
  )(
    formData: Seq[(String, String)],
    expectedErrorMessageKey: String,
    errorArg: String,
    currentAnswers: SingleDisposalTriageAnswers,
    representeeAnswers: Option[RepresenteeAnswers] = None
  ): Unit = {
    inSequence {
      mockAuthWithNoRetrievals()
      mockGetSession(sessionDataWithStartingNewDraftReturn(currentAnswers, representeeAnswers = representeeAnswers)._1)
    }

    val result  = performAction(formData)
    val content = contentAsString(result)

    status(result)        shouldBe BAD_REQUEST
    contentAsString(result) should include(messageFromMessageKey(pageTitleKey))
    content                 should include(messageFromMessageKey(expectedErrorMessageKey, errorArg))
  }

  def displayIndividualTriagePageBehaviorIncompleteJourney(
    performAction: () => Future[Result]
  )(
    requiredPreviousAnswers: IncompleteSingleDisposalTriageAnswers,
    answersWithCurrentAnswer: IncompleteSingleDisposalTriageAnswers,
    description: Option[String] = None,
    representeeAnswers: Option[RepresenteeAnswers] = None
  )(
    pageTitleKey: String,
    checkContent: Document => Unit,
    checkPrepopulatedContent: Document => Unit
  ): Unit = {
    val scenarioDescription = description.map(_ + " and when ").getOrElse("")

    s"display the page when ${scenarioDescription}no option has been selected before for ${userType(answersWithCurrentAnswer)}" in {
      List(
        sessionDataWithStartingNewDraftReturn(requiredPreviousAnswers, representeeAnswers = representeeAnswers)._1,
        sessionDataWithFillingOutReturn(requiredPreviousAnswers, representeeAnswers = representeeAnswers)._1
      ).foreach { currentSession =>
        withClue(s"For currentSession $currentSession: ") {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(currentSession)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(pageTitleKey),
            checkContent
          )
        }
      }
    }

    s"display the page when ${scenarioDescription}an option has been selected before for ${userType(answersWithCurrentAnswer)}" in {
      List(
        sessionDataWithStartingNewDraftReturn(answersWithCurrentAnswer, representeeAnswers = representeeAnswers)._1,
        sessionDataWithFillingOutReturn(answersWithCurrentAnswer, representeeAnswers = representeeAnswers)._1
      ).foreach { currentSession =>
        withClue(s"For currentSession $currentSession: ") {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(currentSession)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(pageTitleKey),
            { document =>
              checkContent(document)
              checkPrepopulatedContent(document)
            }
          )
        }
      }
    }

  }

  private def userType(answers: SingleDisposalTriageAnswers) =
    answers.representativeType() match {
      case Some(PersonalRepresentative)                => "personal representative"
      case Some(PersonalRepresentativeInPeriodOfAdmin) => "personal representative in period of admin"
      case Some(Capacitor)                             => "capacitor"
      case None                                        => "self"
    }

  def displayIndividualTriagePageBehaviorCompleteJourney(
    performAction: () => Future[Result]
  )(answers: CompleteSingleDisposalTriageAnswers, representeeAnswers: Option[RepresenteeAnswers] = None)(
    pageTitleKey: String,
    checkContent: Document => Unit
  ): Unit =
    s"display the page when the journey has already been completed for ${userType(answers)}" in {
      List(
        sessionDataWithStartingNewDraftReturn(answers, representeeAnswers = representeeAnswers)._1,
        sessionDataWithFillingOutReturn(answers, representeeAnswers = representeeAnswers)._1
      ).foreach { currentSession =>
        withClue(s"For currentSession $currentSession: ") {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(currentSession)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(pageTitleKey),
            checkContent
          )
        }
      }
    }

  def displayCustomContentForAgentWithSelfAndTrustType(
    performAction: () => Future[Result]
  )(answers: IncompleteSingleDisposalTriageAnswers, representeeAnswers: Option[RepresenteeAnswers] = None)(
    pageTitleKey: String,
    checkContent: Document => Unit
  ): Unit =
    s"use the agent page title with ${userType(answers)} type" when {

      "an agent requests the page for an individual client" in {

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            SessionData.empty.copy(
              userType = Some(UserType.Agent),
              journeyStatus = Some(
                sample[FillingOutReturn].copy(
                  agentReferenceNumber = Some(sample[AgentReferenceNumber]),
                  subscribedDetails = sample[SubscribedDetails].copy(
                    name = Right(sample[IndividualName])
                  ),
                  draftReturn = sample[DraftSingleDisposalReturn].copy(
                    triageAnswers = answers,
                    representeeAnswers = representeeAnswers
                  )
                )
              )
            )
          )
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey(pageTitleKey),
          checkContent
        )
      }

      "an agent requests the page for a trust client" in {

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            SessionData.empty.copy(
              userType = Some(UserType.Agent),
              journeyStatus = Some(
                sample[FillingOutReturn].copy(
                  agentReferenceNumber = Some(sample[AgentReferenceNumber]),
                  subscribedDetails = sample[SubscribedDetails].copy(
                    name = Left(sample[TrustName])
                  ),
                  draftReturn = sample[DraftSingleDisposalReturn].copy(
                    triageAnswers = answers,
                    representeeAnswers = representeeAnswers
                  )
                )
              )
            )
          )
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey(pageTitleKey),
          checkContent
        )
      }

    }

  def displayCustomContentForTrusts(
    performAction: () => Future[Result]
  )(answers: IncompleteSingleDisposalTriageAnswers)(
    pageTitleKey: String,
    checkContent: Document => Unit
  ): Unit =
    "use the trust page title" when {

      "a trust requests the page" in {

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            SessionData.empty.copy(
              userType = Some(UserType.Organisation),
              journeyStatus = Some(
                sample[FillingOutReturn].copy(
                  agentReferenceNumber = None,
                  subscribedDetails = sample[SubscribedDetails].copy(
                    name = Left(sample[TrustName])
                  ),
                  draftReturn = sample[DraftSingleDisposalReturn].copy(
                    triageAnswers = answers
                  )
                )
              )
            )
          )
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey(pageTitleKey),
          checkContent
        )
      }
    }

  def unsuccessfulUpdatesBehaviour(
    performAction: Seq[(String, String)] => Future[Result],
    currentAnswers: SingleDisposalTriageAnswers,
    formData: Seq[(String, String)],
    updatedAnswers: SingleDisposalTriageAnswers,
    updateDraftReturn: (
      DraftSingleDisposalReturn,
      SingleDisposalTriageAnswers
    ) => DraftSingleDisposalReturn,
    extraMockActions: () => Unit = () => ()
  ): Unit =
    "show an error page" when {

      "the user is starting a new draft return and" when {

        "there is an error updating the session" in {
          val (session, journey) =
            sessionDataWithStartingNewDraftReturn(currentAnswers)
          val updatedJourney     =
            journey.copy(newReturnTriageAnswers = Right(updatedAnswers))
          val updatedSession     =
            session.copy(journeyStatus = Some(updatedJourney))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            extraMockActions()
            mockStoreSession(updatedSession)(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction(formData))
        }

      }

      "the user is filling in a draft return and" when {
        val draftReturn             =
          sample[DraftSingleDisposalReturn]
            .copy(triageAnswers = currentAnswers, gainOrLossAfterReliefs = None, yearToDateLiabilityAnswers = None)
        val updatedDraftReturn      = updateDraftReturn(draftReturn, updatedAnswers)
        val fillingOutReturn        =
          sample[FillingOutReturn].copy(draftReturn = draftReturn, amendReturnData = None)
        val updatedFillingOutReturn =
          fillingOutReturn.copy(draftReturn = updatedDraftReturn)

        "there is an error updating the draft return" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(journeyStatus = Some(fillingOutReturn))
            )
            extraMockActions()
            mockStoreDraftReturn(updatedFillingOutReturn)(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction(formData))
        }

        "there is an error updating the session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(journeyStatus = Some(fillingOutReturn))
            )
            extraMockActions()
            mockStoreDraftReturn(updatedFillingOutReturn)(Right(()))
            mockStoreSession(
              SessionData.empty
                .copy(journeyStatus = Some(updatedFillingOutReturn))
            )(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction(formData))
        }

      }

    }

  def testSuccessfulUpdateStartingNewDraft(
    performAction: Seq[(String, String)] => Future[Result],
    currentAnswers: SingleDisposalTriageAnswers,
    formData: Seq[(String, String)],
    updatedAnswers: SingleDisposalTriageAnswers,
    checkNextResult: Future[Result] => Unit,
    extraMockActions: () => Unit = () => (),
    representeeAnswers: Option[RepresenteeAnswers] = None
  ): Unit = {
    val (session, journey) = sessionDataWithStartingNewDraftReturn(
      currentAnswers,
      representeeAnswers = representeeAnswers
    )
    val updatedJourney     =
      journey.copy(newReturnTriageAnswers = Right(updatedAnswers))
    val updatedSession     = session.copy(journeyStatus = Some(updatedJourney))

    inSequence {
      mockAuthWithNoRetrievals()
      mockGetSession(session)
      extraMockActions()
      mockStoreSession(updatedSession)(Right(()))
    }

    checkNextResult(performAction(formData))
  }

  def testSuccessfulUpdateFillingOutReturn(
    performAction: Seq[(String, String)] => Future[Result],
    currentAnswers: SingleDisposalTriageAnswers,
    formData: Seq[(String, String)],
    updateJourney: (FillingOutReturn, DraftSingleDisposalReturn) => FillingOutReturn,
    checkNextResult: Future[Result] => Unit,
    extraMockActions: () => Unit = () => (),
    representeeAnswers: Option[RepresenteeAnswers] = None,
    amendReturnData: Option[AmendReturnData] = None
  ): Unit = {
    val draftReturn = sample[DraftSingleDisposalReturn].copy(
      triageAnswers = currentAnswers,
      representeeAnswers = representeeAnswers,
      gainOrLossAfterReliefs = None
    )

    val fillingOutReturn = sample[FillingOutReturn].copy(
      draftReturn = draftReturn,
      amendReturnData = amendReturnData
    )

    val updatedFillingOutReturn =
      updateJourney(fillingOutReturn, draftReturn)

    inSequence {
      mockAuthWithNoRetrievals()
      mockGetSession(
        SessionData.empty.copy(journeyStatus = Some(fillingOutReturn))
      )
      extraMockActions()
      mockStoreDraftReturn(updatedFillingOutReturn)(Right(()))
      mockStoreSession(
        SessionData.empty.copy(journeyStatus = Some(updatedFillingOutReturn))
      )(Right(()))
    }

    checkNextResult(performAction(formData))
  }

  def testSuccessfulUpdateFillingOutReturn[D <: DraftReturn](
    performAction: Seq[(String, String)] => Future[Result],
    currentDraftReturn: D,
    formData: Seq[(String, String)]
  )(
    updateDraftReturn: D => DraftReturn,
    checkNextResult: Future[Result] => Unit
  ): Unit = {
    val updatedDraftReturn = updateDraftReturn(currentDraftReturn)

    val fillingOutReturn        =
      sample[FillingOutReturn].copy(draftReturn = currentDraftReturn, amendReturnData = None)
    val updatedFillingOutReturn =
      fillingOutReturn.copy(draftReturn = updatedDraftReturn)

    inSequence {
      mockAuthWithNoRetrievals()
      mockGetSession(
        SessionData.empty.copy(journeyStatus = Some(fillingOutReturn))
      )
      mockStoreDraftReturn(updatedFillingOutReturn)(Right(()))
      mockStoreSession(
        SessionData.empty.copy(journeyStatus = Some(updatedFillingOutReturn))
      )(Right(()))
    }

    checkNextResult(performAction(formData))
  }

  def noDateOfDeathForPersonalRepBehaviour(performAction: () => Future[Result]): Unit =
    "show an error page" when {

      def sessionWithNoDateOfDeath(individualUserType: IndividualUserType): SessionData =
        sessionDataWithStartingNewDraftReturn(
          IncompleteSingleDisposalTriageAnswers.empty.copy(individualUserType = Some(individualUserType)),
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

object SingleDisposalsTriageControllerSpec extends Matchers {
  def validateSingleDisposalTriageCheckYourAnswersPage(
    completeSingleDisposalTriageAnswers: CompleteSingleDisposalTriageAnswers,
    userType: Option[UserType],
    doc: Document
  )(implicit messagesApi: MessagesApi, lang: Lang): Unit = {

    implicit lazy val messages: Messages = MessagesImpl(lang, messagesApi)

    if (completeSingleDisposalTriageAnswers.individualUserType.contains(Self)) {
      doc.select("#individualUserType-answer").text() shouldBe messages(
        if (userType.contains(UserType.Agent)) s"individualUserType.agent.Self" else s"individualUserType.Self"
      )
    }

    doc.select("#numberOfProperties-answer").text() shouldBe "One"

    if (completeSingleDisposalTriageAnswers.individualUserType.contains(PersonalRepresentativeInPeriodOfAdmin)) {
      doc.select("#disposalMethod-answer").text() shouldBe ""
    } else {
      doc.select("#disposalMethod-answer").text() shouldBe messages(
        s"disposalMethod.${completeSingleDisposalTriageAnswers.disposalMethod}"
      )
    }

    if (completeSingleDisposalTriageAnswers.countryOfResidence.isUk) {
      doc.select("#wereYouAUKResident-answer").text() shouldBe "Yes"
    } else {
      doc.select("#wereYouAUKResident-answer").text() shouldBe "No"
    }

    if (completeSingleDisposalTriageAnswers.countryOfResidence.isUk) {
      completeSingleDisposalTriageAnswers.assetType match {
        case Residential      =>
          doc.select("#propertyType-answer").text() shouldBe "Yes"
        case NonResidential   =>
          doc.select("#propertyType-answer").text() shouldBe "No"
        case IndirectDisposal =>
          doc.select("#propertyType-answer").text() shouldBe ""
        case MixedUse         => doc.select("#propertyType-answer").text() shouldBe ""
      }
    }
    val isIndirectDisposal: Boolean =
      completeSingleDisposalTriageAnswers match {
        case CompleteSingleDisposalTriageAnswers(
              _,
              _,
              _,
              AssetType.IndirectDisposal,
              _,
              _,
              _
            ) =>
          true
        case _ => false
      }
    if (isIndirectDisposal) {
      doc.select("#disposalDateOfShares-answer").text shouldBe TimeUtils
        .govDisplayFormat(
          completeSingleDisposalTriageAnswers.disposalDate.value
        )
    } else {
      doc.select("#disposalDate-answer").text   shouldBe TimeUtils
        .govDisplayFormat(
          completeSingleDisposalTriageAnswers.disposalDate.value
        )
      doc.select("#completionDate-answer").text shouldBe TimeUtils
        .govDisplayFormat(
          completeSingleDisposalTriageAnswers.completionDate.value
        )
    }
  }
}
