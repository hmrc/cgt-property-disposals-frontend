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
import play.api.i18n.{Lang, MessagesApi}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.{Call, Result}
import play.api.test.FakeRequest
import play.api.test.Helpers.{contentAsString, _}
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.DateErrorScenarios.{DateErrorScenario, dateErrorScenarios}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.triage.SingleDisposalsTriageControllerSpec.validateSingleDisposalTriageCheckYourAnswersPage
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.{ReturnsServiceSupport, representee, routes => returnsRoutes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators.{sample, _}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, StartingNewDraftReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Country
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.{AgentReferenceNumber, UUIDGenerator}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.{IndividualName, TrustName}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.SubscribedDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.AssetType.{IndirectDisposal, MixedUse, NonResidential, Residential}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.IndividualUserType._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.RepresenteeAnswers.IncompleteRepresenteeAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.{CompleteSingleDisposalTriageAnswers, IncompleteSingleDisposalTriageAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.YearToDateLiabilityAnswers.{CalculatedYTDAnswers, NonCalculatedYTDAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{SingleDisposalTriageAnswers, _}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, JourneyStatus, SessionData, TaxYear, TimeUtils, UserType}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.{ReturnsService, TaxYearService}
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class SingleDisposalsTriageControllerSpec
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
      bind[ReturnsService].toInstance(mockReturnsService),
      bind[UUIDGenerator].toInstance(mockUUIDGenerator),
      bind[TaxYearService].toInstance(mockTaxYearService)
    )

  val today = LocalDate.now(Clock.systemUTC())

  lazy val controller = instanceOf[SingleDisposalsTriageController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  def isValidJourney(journeyStatus: JourneyStatus): Boolean = journeyStatus match {
    case r: StartingNewDraftReturn if (r.newReturnTriageAnswers.isRight) => true
    case _: FillingOutReturn                                             => true
    case _                                                               => false
  }

  def sessionDataWithStartingNewDraftReturn(
    singleDisposalTriageAnswers: SingleDisposalTriageAnswers,
    name: Either[TrustName, IndividualName] = Right(sample[IndividualName]),
    representeeAnswers: RepresenteeAnswers  = sample[IncompleteRepresenteeAnswers]
  ): (SessionData, StartingNewDraftReturn) = {
    val startingNewDraftReturn = sample[StartingNewDraftReturn].copy(
      subscribedDetails      = sample[SubscribedDetails].copy(name = name),
      newReturnTriageAnswers = Right(singleDisposalTriageAnswers),
      representeeAnswers     = Some(representeeAnswers)
    )

    SessionData.empty
      .copy(journeyStatus =
        Some(
          startingNewDraftReturn.copy(
            subscribedDetails      = startingNewDraftReturn.subscribedDetails.copy(name = name),
            newReturnTriageAnswers = Right(singleDisposalTriageAnswers)
          )
        )
      ) -> startingNewDraftReturn
  }

  def sessionDataWithFillingOurReturn(
    draftReturn: DraftSingleDisposalReturn,
    name: Either[TrustName, IndividualName]
  ): (SessionData, FillingOutReturn) = {
    val fillingOutReturn = sample[FillingOutReturn].copy(
      draftReturn       = draftReturn,
      subscribedDetails = sample[SubscribedDetails].copy(name = name)
    )

    SessionData.empty.copy(journeyStatus = Some(fillingOutReturn)) -> fillingOutReturn
  }

  def sessionDataWithFillingOutReturn(
    singleDisposalTriageAnswers: SingleDisposalTriageAnswers,
    name: Either[TrustName, IndividualName] = Right(sample[IndividualName]),
    representeeAnswers: RepresenteeAnswers  = sample[IncompleteRepresenteeAnswers]
  ): (SessionData, FillingOutReturn, DraftSingleDisposalReturn) = {
    val draftReturn = sample[DraftSingleDisposalReturn].copy(
      triageAnswers      = singleDisposalTriageAnswers,
      representeeAnswers = Some(representeeAnswers)
    )

    val (session, journey) = sessionDataWithFillingOurReturn(
      draftReturn,
      name
    )
    (session, journey, draftReturn)
  }

  def mockGetNextUUID(uuid: UUID) =
    (mockUUIDGenerator.nextId _).expects().returning(uuid)

  def mockGetTaxYear(date: LocalDate)(response: Either[Error, Option[TaxYear]]) =
    (mockTaxYearService
      .taxYear(_: LocalDate)(_: HeaderCarrier))
      .expects(date, *)
      .returning(EitherT.fromEither[Future](response))

  "The SingleDisposalsTriageController" when {

    "handling requests to display how did you dispose of your property page" must {

      val requiredPreviousAnswers =
        IncompleteSingleDisposalTriageAnswers.empty.copy(hasConfirmedSingleDisposal = true)

      def performAction(): Future[Result] = controller.howDidYouDisposeOfProperty()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(performAction, isValidJourney)

      behave like redirectWhenNoPreviousAnswerBehaviour[NumberOfProperties](
        performAction
      )(
        requiredPreviousAnswers,
        routes.CommonTriageQuestionsController.howManyProperties(), {
          case (answers, n) =>
            answers.copy(
              hasConfirmedSingleDisposal = if (n.contains(NumberOfProperties.One)) true else false
            )
        }
      )

      behave like displayIndividualTriagePageBehaviorIncompleteJourney(
        performAction
      )(requiredPreviousAnswers, requiredPreviousAnswers.copy(disposalMethod = Some(DisposalMethod.Sold)))(
        "disposalMethod.title",
        checkContent(_, routes.CommonTriageQuestionsController.howManyProperties()),
        _.select("#disposalMethod-0").attr("checked") shouldBe "checked"
      )

      behave like displayIndividualTriagePageBehaviorCompleteJourney(
        performAction
      )(sample[CompleteSingleDisposalTriageAnswers].copy(disposalMethod = DisposalMethod.Gifted))(
        "disposalMethod.title", { doc =>
          checkContent(doc, routes.SingleDisposalsTriageController.checkYourAnswers())
          doc.select("#disposalMethod-1").attr("checked") shouldBe "checked"
        }
      )

      behave like displayCustomContentForAgentWithSelfAndPersonalRepType(
        performAction
      )(requiredPreviousAnswers.copy(disposalMethod = Some(DisposalMethod.Sold)))(
        "disposalMethod.agent.title",
        doc => checkContent(doc, routes.CommonTriageQuestionsController.howManyProperties())
      )

      behave like displayCustomContentForTrusts(
        performAction
      )(requiredPreviousAnswers.copy(disposalMethod = Some(DisposalMethod.Sold)))(
        "disposalMethod.trust.title",
        doc => checkContent(doc, routes.CommonTriageQuestionsController.howManyProperties())
      )

      def checkContent(doc: Document, backLink: Call): Unit = {
        doc.select("#back").attr("href") shouldBe backLink.url
        doc
          .select("#content > article > form")
          .attr("action") shouldBe routes.SingleDisposalsTriageController
          .howDidYouDisposeOfPropertySubmit()
          .url
      }

    }

    "handling submitted answers to the how did you dispose of your property page" must {

      def performAction(formData: (String, String)*): Future[Result] =
        controller.howDidYouDisposeOfPropertySubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*))

      def updateDraftReturn(d: DraftSingleDisposalReturn, newAnswers: SingleDisposalTriageAnswers) =
        d.copy(
          triageAnswers = newAnswers,
          disposalDetailsAnswers = d.disposalDetailsAnswers.map(
            _.unset(_.disposalPrice)
              .unset(_.disposalFees)
          ),
          initialGainOrLoss          = None,
          reliefDetailsAnswers       = None,
          exemptionAndLossesAnswers  = None,
          yearToDateLiabilityAnswers = None,
          supportingEvidenceAnswers  = None
        )

      val requiredPreviousAnswers =
        IncompleteSingleDisposalTriageAnswers.empty.copy(hasConfirmedSingleDisposal = true)

      behave like redirectToStartWhenInvalidJourney(() => performAction(), isValidJourney)

      behave like redirectWhenNoPreviousAnswerBehaviour[NumberOfProperties](() => performAction())(
        requiredPreviousAnswers,
        routes.CommonTriageQuestionsController.howManyProperties(), {
          case (answers, n) =>
            answers.copy(
              hasConfirmedSingleDisposal = if (n.contains(NumberOfProperties.One)) true else false
            )
        }
      )

      "show a form error" when {

        def test(formData: Seq[(String, String)], expectedErrorKey: String) =
          testFormError(performAction, "disposalMethod.title")(formData, expectedErrorKey, requiredPreviousAnswers)

        "nothing is submitted" in {
          test(List.empty, "disposalMethod.error.required")
        }

        "the option is not recognised" in {
          test(List("disposalMethod" -> "3"), "disposalMethod.error.invalid")
        }

      }

      behave like unsuccessfulUpdatesBehaviour(
        performAction,
        requiredPreviousAnswers,
        List("disposalMethod" -> "0"),
        requiredPreviousAnswers.copy(disposalMethod = Some(DisposalMethod.Sold)),
        updateDraftReturn
      )

      "handle successful updates" when {

        "the user is starting a new draft return and" when {

          "the user has answered some questions but not complete the section" in {
            testSuccessfulUpdateStartingNewDraft(
              performAction,
              requiredPreviousAnswers,
              List("disposalMethod" -> "2"),
              requiredPreviousAnswers.copy(disposalMethod = Some(DisposalMethod.Other)),
              checkIsRedirect(_, routes.SingleDisposalsTriageController.checkYourAnswers())
            )
          }

          "the user has complete the section" in {
            val completeAnswers = sample[CompleteSingleDisposalTriageAnswers]
            testSuccessfulUpdateStartingNewDraft(
              performAction,
              completeAnswers.copy(disposalMethod = DisposalMethod.Sold),
              List("disposalMethod" -> "1"),
              completeAnswers.copy(disposalMethod = DisposalMethod.Gifted),
              checkIsRedirect(_, routes.SingleDisposalsTriageController.checkYourAnswers())
            )
          }

        }

        "the user is filling out a draft return and" when {

          def updateDraftReturn(d: DraftSingleDisposalReturn, newAnswers: SingleDisposalTriageAnswers) =
            d.copy(
              triageAnswers = newAnswers,
              disposalDetailsAnswers = d.disposalDetailsAnswers.map(
                _.unset(_.disposalPrice)
                  .unset(_.disposalFees)
              ),
              initialGainOrLoss          = None,
              reliefDetailsAnswers       = None,
              exemptionAndLossesAnswers  = None,
              yearToDateLiabilityAnswers = None,
              supportingEvidenceAnswers  = None
            )

          "the section is complete" in {
            forAll { completeAnswers: CompleteSingleDisposalTriageAnswers =>
              testSuccessfulUpdateFillingOutReturn(
                performAction,
                completeAnswers.copy(disposalMethod = DisposalMethod.Sold),
                List("disposalMethod" -> "1"),
                updateDraftReturn(_, completeAnswers.copy(disposalMethod = DisposalMethod.Gifted)),
                checkIsRedirect(_, routes.SingleDisposalsTriageController.checkYourAnswers())
              )
            }
          }

          "the section is incomplete" in {
            testSuccessfulUpdateFillingOutReturn(
              performAction,
              requiredPreviousAnswers,
              List("disposalMethod" -> "0"),
              d =>
                d.copy(
                  triageAnswers = requiredPreviousAnswers.copy(disposalMethod = Some(DisposalMethod.Sold)),
                  disposalDetailsAnswers = d.disposalDetailsAnswers.map(
                    _.unset(_.disposalPrice)
                      .unset(_.disposalFees)
                  ),
                  initialGainOrLoss          = None,
                  reliefDetailsAnswers       = None,
                  exemptionAndLossesAnswers  = None,
                  yearToDateLiabilityAnswers = None,
                  supportingEvidenceAnswers  = None
                ),
              checkIsRedirect(_, routes.SingleDisposalsTriageController.checkYourAnswers())
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
          disposalMethod     = Some(DisposalMethod.Sold)
        )

      def performAction(): Future[Result] = controller.wereYouAUKResident()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(performAction, isValidJourney)

      behave like redirectWhenNoPreviousAnswerBehaviour[DisposalMethod](
        performAction
      )(
        requiredPreviousAnswers,
        routes.SingleDisposalsTriageController.howDidYouDisposeOfProperty(),
        { case (answers, m) => answers.copy(disposalMethod = m) }
      )

      behave like displayIndividualTriagePageBehaviorIncompleteJourney(
        performAction
      )(requiredPreviousAnswers, requiredPreviousAnswers.copy(wasAUKResident = Some(false)))(
        "wereYouAUKResident.title",
        checkContent(_, routes.SingleDisposalsTriageController.howDidYouDisposeOfProperty()),
        _.select("#wereYouAUKResident-false").attr("checked") shouldBe "checked"
      )

      behave like displayIndividualTriagePageBehaviorIncompleteJourney(
        performAction
      )(
        requiredPreviousAnswers.copy(individualUserType = Some(IndividualUserType.Capacitor)),
        requiredPreviousAnswers
          .copy(wasAUKResident = Some(false), individualUserType = Some(IndividualUserType.Capacitor))
      )(
        "wereYouAUKResident.capacitor.title",
        checkContent(_, routes.SingleDisposalsTriageController.howDidYouDisposeOfProperty()),
        _.select("#wereYouAUKResident-false").attr("checked") shouldBe "checked"
      )

      behave like displayIndividualTriagePageBehaviorIncompleteJourney(
        performAction
      )(
        requiredPreviousAnswers.copy(individualUserType = Some(IndividualUserType.PersonalRepresentative)),
        requiredPreviousAnswers
          .copy(wasAUKResident = Some(false), individualUserType = Some(IndividualUserType.PersonalRepresentative))
      )(
        "wereYouAUKResident.personalRep.title",
        checkContent(_, routes.SingleDisposalsTriageController.howDidYouDisposeOfProperty()),
        _.select("#wereYouAUKResident-false").attr("checked") shouldBe "checked"
      )

      behave like displayIndividualTriagePageBehaviorCompleteJourney(
        performAction
      )(
        sample[CompleteSingleDisposalTriageAnswers]
          .copy(countryOfResidence = Country.uk, individualUserType = Some(IndividualUserType.Self))
      )(
        "wereYouAUKResident.title", { doc =>
          checkContent(doc, routes.SingleDisposalsTriageController.checkYourAnswers())
          doc.select("#wereYouAUKResident-true").attr("checked") shouldBe "checked"
        }
      )

      behave like displayIndividualTriagePageBehaviorCompleteJourney(
        performAction
      )(
        sample[CompleteSingleDisposalTriageAnswers]
          .copy(countryOfResidence = Country.uk, individualUserType = Some(IndividualUserType.Capacitor))
      )(
        "wereYouAUKResident.capacitor.title", { doc =>
          checkContent(doc, routes.SingleDisposalsTriageController.checkYourAnswers())
          doc.select("#wereYouAUKResident-true").attr("checked") shouldBe "checked"
        }
      )

      behave like displayIndividualTriagePageBehaviorCompleteJourney(
        performAction
      )(
        sample[CompleteSingleDisposalTriageAnswers]
          .copy(countryOfResidence = Country.uk, individualUserType = Some(IndividualUserType.PersonalRepresentative))
      )(
        "wereYouAUKResident.personalRep.title", { doc =>
          checkContent(doc, routes.SingleDisposalsTriageController.checkYourAnswers())
          doc.select("#wereYouAUKResident-true").attr("checked") shouldBe "checked"
        }
      )

      behave like displayCustomContentForAgentWithSelfAndPersonalRepType(
        performAction
      )(requiredPreviousAnswers.copy(wasAUKResident = Some(true), countryOfResidence = Some(Country.uk)))(
        "wereYouAUKResident.agent.title",
        doc => checkContent(doc, routes.SingleDisposalsTriageController.howDidYouDisposeOfProperty())
      )

      behave like displayCustomContentForAgentWithSelfAndPersonalRepType(
        performAction
      )(
        requiredPreviousAnswers.copy(
          wasAUKResident     = Some(true),
          countryOfResidence = Some(Country.uk),
          individualUserType = Some(IndividualUserType.PersonalRepresentative)
        )
      )(
        "wereYouAUKResident.personalRep.title",
        doc => checkContent(doc, routes.SingleDisposalsTriageController.howDidYouDisposeOfProperty())
      )

      behave like displayCustomContentForTrusts(
        performAction
      )(requiredPreviousAnswers.copy(wasAUKResident = Some(true), countryOfResidence = Some(Country.uk)))(
        "wereYouAUKResident.trust.title",
        doc => checkContent(doc, routes.SingleDisposalsTriageController.howDidYouDisposeOfProperty())
      )

      def checkContent(doc: Document, backLink: Call): Unit = {
        doc.select("#back").attr("href") shouldBe backLink.url
        doc
          .select("#content > article > form")
          .attr("action") shouldBe routes.SingleDisposalsTriageController
          .wereYouAUKResidentSubmit()
          .url
      }

    }

    "handling submitted answers to the were you a uk resident page" must {

      def performAction(formData: (String, String)*): Future[Result] =
        controller.wereYouAUKResidentSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*))

      def updateDraftReturn(d: DraftSingleDisposalReturn, newAnswers: SingleDisposalTriageAnswers) =
        d.copy(
          triageAnswers             = newAnswers,
          propertyAddress           = None,
          disposalDetailsAnswers    = None,
          acquisitionDetailsAnswers = None,
          initialGainOrLoss         = None,
          reliefDetailsAnswers = d.reliefDetailsAnswers.map(
            _.unset(_.privateResidentsRelief)
              .unset(_.lettingsRelief)
          ),
          exemptionAndLossesAnswers  = None,
          yearToDateLiabilityAnswers = None,
          supportingEvidenceAnswers  = None
        )

      val requiredPreviousAnswers = IncompleteSingleDisposalTriageAnswers.empty.copy(
        disposalMethod     = Some(DisposalMethod.Sold),
        individualUserType = Some(IndividualUserType.Self)
      )

      behave like redirectToStartWhenInvalidJourney(() => performAction(), isValidJourney)

      behave like redirectWhenNoPreviousAnswerBehaviour[DisposalMethod](() => performAction())(
        requiredPreviousAnswers,
        routes.SingleDisposalsTriageController.howDidYouDisposeOfProperty(),
        { case (answers, m) => answers.copy(disposalMethod = m) }
      )

      "show a form error for self type" when {

        def test(formData: Seq[(String, String)], expectedErrorKey: String) =
          testFormError(performAction, "wereYouAUKResident.title")(formData, expectedErrorKey, requiredPreviousAnswers)

        "nothing is submitted" in {
          test(List.empty, "wereYouAUKResident.error.required")
        }

        "the option is not recognised" in {
          test(List("wereYouAUKResident" -> "3"), "wereYouAUKResident.error.boolean")
        }

      }

      "show a form error for personal representative type" when {

        def test(formData: Seq[(String, String)], expectedErrorKey: String) =
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
          test(List("wereYouAUKResident" -> "3"), "wereYouAUKResident.personalRep.error.boolean")
        }

      }

      "show a form error for capacitor type" when {

        def test(formData: Seq[(String, String)], expectedErrorKey: String) =
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
          test(List("wereYouAUKResident" -> "3"), "wereYouAUKResident.capacitor.error.boolean")
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
              wasAUKResident     = Some(false),
              countryOfResidence = Some(Country("AB", None)),
              assetType          = Some(AssetType.Residential)
            )

            testSuccessfulUpdateStartingNewDraft(
              performAction,
              answers,
              List("wereYouAUKResident" -> "true"),
              requiredPreviousAnswers.copy(wasAUKResident = Some(true), countryOfResidence = None, assetType = None),
              checkIsRedirect(_, routes.SingleDisposalsTriageController.checkYourAnswers())
            )
          }

          "the user has answered some questions but not complete the section and has " +
            "changed their answer from was in the uk to not in the uk" in {
            val answers = requiredPreviousAnswers
              .copy(wasAUKResident = Some(true), countryOfResidence = None, assetType = Some(AssetType.Residential))

            testSuccessfulUpdateStartingNewDraft(
              performAction,
              answers,
              List("wereYouAUKResident" -> "false"),
              requiredPreviousAnswers.copy(wasAUKResident = Some(false), countryOfResidence = None, assetType = None),
              checkIsRedirect(_, routes.SingleDisposalsTriageController.checkYourAnswers())
            )
          }

          "the user has complete the section and has changed their answer from " +
            "not in the uk to was in the uk" in {
            val completeAnswers = sample[CompleteSingleDisposalTriageAnswers]
            testSuccessfulUpdateStartingNewDraft(
              performAction,
              completeAnswers.copy(countryOfResidence = Country("AB", None)),
              List("wereYouAUKResident" -> "true"),
              IncompleteSingleDisposalTriageAnswers(
                completeAnswers.individualUserType,
                true,
                Some(completeAnswers.disposalMethod),
                Some(true),
                None,
                None,
                Some(completeAnswers.disposalDate),
                Some(completeAnswers.completionDate),
                None
              ),
              checkIsRedirect(_, routes.SingleDisposalsTriageController.checkYourAnswers())
            )
          }

          "the user has completed the section and has changed their answer from " +
            "was in the uk to not in the uk" in {
            val completeAnswers = sample[CompleteSingleDisposalTriageAnswers]
            testSuccessfulUpdateStartingNewDraft(
              performAction,
              completeAnswers.copy(countryOfResidence = Country.uk),
              List("wereYouAUKResident" -> "false"),
              IncompleteSingleDisposalTriageAnswers(
                completeAnswers.individualUserType,
                true,
                Some(completeAnswers.disposalMethod),
                Some(false),
                None,
                None,
                Some(completeAnswers.disposalDate),
                Some(completeAnswers.completionDate),
                None
              ),
              checkIsRedirect(_, routes.SingleDisposalsTriageController.checkYourAnswers())
            )
          }

        }

        "the user is filling out a draft return and" when {

          "the section is complete" in {
            forAll { completeAnswers: CompleteSingleDisposalTriageAnswers =>
              val newAnswers =
                IncompleteSingleDisposalTriageAnswers(
                  completeAnswers.individualUserType,
                  true,
                  Some(completeAnswers.disposalMethod),
                  Some(true),
                  None,
                  None,
                  Some(completeAnswers.disposalDate),
                  Some(completeAnswers.completionDate),
                  None
                )
              testSuccessfulUpdateFillingOutReturn(
                performAction,
                completeAnswers.copy(countryOfResidence = Country("AB", None)),
                List("wereYouAUKResident" -> "true"),
                updateDraftReturn(_, newAnswers),
                checkIsRedirect(_, routes.SingleDisposalsTriageController.checkYourAnswers())
              )
            }
          }

          "the section is incomplete" in {
            val answers = requiredPreviousAnswers
              .copy(wasAUKResident = Some(true), countryOfResidence = None, assetType = Some(AssetType.Residential))

            testSuccessfulUpdateFillingOutReturn(
              performAction,
              answers,
              List("wereYouAUKResident" -> "false"),
              updateDraftReturn(
                _,
                requiredPreviousAnswers.copy(wasAUKResident = Some(false), countryOfResidence = None, assetType = None)
              ),
              checkIsRedirect(_, routes.SingleDisposalsTriageController.checkYourAnswers())
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
        IncompleteSingleDisposalTriageAnswers.empty.copy(wasAUKResident = Some(true))

      def performAction(): Future[Result] = controller.didYouDisposeOfAResidentialProperty()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(performAction, isValidJourney)

      behave like redirectWhenNoPreviousAnswerBehaviour[Boolean](
        performAction
      )(
        requiredPreviousAnswers,
        routes.SingleDisposalsTriageController.wereYouAUKResident(),
        { case (answers, w) => answers.copy(wasAUKResident = w) }
      )

      behave like displayIndividualTriagePageBehaviorIncompleteJourney(
        performAction
      )(requiredPreviousAnswers, requiredPreviousAnswers.copy(assetType = Some(AssetType.NonResidential)))(
        "didYouDisposeOfResidentialProperty.title",
        checkContent(_, routes.SingleDisposalsTriageController.wereYouAUKResident()),
        _.select("#didYouDisposeOfResidentialProperty-false").attr("checked") shouldBe "checked"
      )

      behave like displayIndividualTriagePageBehaviorIncompleteJourney(
        performAction
      )(
        requiredPreviousAnswers.copy(
          individualUserType = Some(IndividualUserType.Capacitor)
        ),
        requiredPreviousAnswers.copy(
          assetType          = Some(AssetType.NonResidential),
          individualUserType = Some(IndividualUserType.Capacitor)
        )
      )(
        "didYouDisposeOfResidentialProperty.capacitor.title",
        checkContent(_, routes.SingleDisposalsTriageController.wereYouAUKResident()),
        _.select("#didYouDisposeOfResidentialProperty-false").attr("checked") shouldBe "checked"
      )

      behave like displayIndividualTriagePageBehaviorIncompleteJourney(
        performAction
      )(
        requiredPreviousAnswers.copy(
          individualUserType = Some(IndividualUserType.PersonalRepresentative)
        ),
        requiredPreviousAnswers.copy(
          assetType          = Some(AssetType.NonResidential),
          individualUserType = Some(IndividualUserType.PersonalRepresentative)
        )
      )(
        "didYouDisposeOfResidentialProperty.personalRep.title",
        checkContent(_, routes.SingleDisposalsTriageController.wereYouAUKResident()),
        _.select("#didYouDisposeOfResidentialProperty-false").attr("checked") shouldBe "checked"
      )

      behave like displayIndividualTriagePageBehaviorCompleteJourney(
        performAction
      )(
        sample[CompleteSingleDisposalTriageAnswers].copy(
          individualUserType = Some(IndividualUserType.Self),
          countryOfResidence = Country.uk,
          assetType          = AssetType.Residential
        )
      )(
        "didYouDisposeOfResidentialProperty.title", { doc =>
          checkContent(doc, routes.SingleDisposalsTriageController.checkYourAnswers())
          doc.select("#didYouDisposeOfResidentialProperty-true").attr("checked") shouldBe "checked"
        }
      )

      behave like displayIndividualTriagePageBehaviorCompleteJourney(
        performAction
      )(
        sample[CompleteSingleDisposalTriageAnswers].copy(
          individualUserType = Some(IndividualUserType.Capacitor),
          countryOfResidence = Country.uk,
          assetType          = AssetType.Residential
        )
      )(
        "didYouDisposeOfResidentialProperty.capacitor.title", { doc =>
          checkContent(doc, routes.SingleDisposalsTriageController.checkYourAnswers())
          doc.select("#didYouDisposeOfResidentialProperty-true").attr("checked") shouldBe "checked"
        }
      )

      behave like displayIndividualTriagePageBehaviorCompleteJourney(
        performAction
      )(
        sample[CompleteSingleDisposalTriageAnswers].copy(
          individualUserType = Some(IndividualUserType.PersonalRepresentative),
          countryOfResidence = Country.uk,
          assetType          = AssetType.Residential
        )
      )(
        "didYouDisposeOfResidentialProperty.personalRep.title", { doc =>
          checkContent(doc, routes.SingleDisposalsTriageController.checkYourAnswers())
          doc.select("#didYouDisposeOfResidentialProperty-true").attr("checked") shouldBe "checked"
        }
      )

      behave like displayCustomContentForAgentWithSelfAndPersonalRepType(
        performAction
      )(requiredPreviousAnswers.copy(assetType = Some(AssetType.Residential)))(
        "didYouDisposeOfResidentialProperty.agent.title",
        doc => checkContent(doc, routes.SingleDisposalsTriageController.wereYouAUKResident())
      )

      behave like displayCustomContentForAgentWithSelfAndPersonalRepType(
        performAction
      )(
        requiredPreviousAnswers.copy(
          assetType          = Some(AssetType.Residential),
          individualUserType = Some(IndividualUserType.PersonalRepresentative)
        )
      )(
        "didYouDisposeOfResidentialProperty.personalRep.title",
        doc => checkContent(doc, routes.SingleDisposalsTriageController.wereYouAUKResident())
      )

      behave like displayCustomContentForTrusts(
        performAction
      )(requiredPreviousAnswers.copy(assetType = Some(AssetType.Residential)))(
        "didYouDisposeOfResidentialProperty.trust.title",
        doc => checkContent(doc, routes.SingleDisposalsTriageController.wereYouAUKResident())
      )

      def checkContent(doc: Document, backLink: Call): Unit = {
        doc.select("#back").attr("href") shouldBe backLink.url
        doc
          .select("#content > article > form")
          .attr("action") shouldBe routes.SingleDisposalsTriageController
          .didYouDisposeOfAResidentialPropertySubmit()
          .url

      }

    }

    "handling submitted answers to the did you dispose of a residential property page" must {

      def performAction(formData: (String, String)*): Future[Result] =
        controller.didYouDisposeOfAResidentialPropertySubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*))

      val requiredPreviousAnswers =
        IncompleteSingleDisposalTriageAnswers.empty.copy(
          wasAUKResident     = Some(true),
          individualUserType = Some(IndividualUserType.Self)
        )

      behave like redirectToStartWhenInvalidJourney(() => performAction(), isValidJourney)

      behave like redirectWhenNoPreviousAnswerBehaviour[Boolean](() => performAction())(
        requiredPreviousAnswers,
        routes.SingleDisposalsTriageController.wereYouAUKResident(),
        { case (answers, w) => answers.copy(wasAUKResident = w) }
      )

      "show a form error fro self type" when {

        def test(formData: Seq[(String, String)], expectedErrorKey: String) =
          testFormError(performAction, "didYouDisposeOfResidentialProperty.title")(
            formData,
            expectedErrorKey,
            requiredPreviousAnswers
          )

        "nothing is submitted" in {
          test(List.empty, "didYouDisposeOfResidentialProperty.error.required")
        }

        "the option is not recognised" in {
          test(List("didYouDisposeOfResidentialProperty" -> "3"), "didYouDisposeOfResidentialProperty.error.boolean")
        }

      }

      "show a form error fro capacitor type" when {

        def test(formData: Seq[(String, String)], expectedErrorKey: String) =
          testFormError(performAction, "didYouDisposeOfResidentialProperty.capacitor.title")(
            formData,
            expectedErrorKey,
            requiredPreviousAnswers.copy(
              individualUserType = Some(IndividualUserType.Capacitor)
            )
          )

        "nothing is submitted" in {
          test(List.empty, "didYouDisposeOfResidentialProperty.capacitor.error.required")
        }

        "the option is not recognised" in {
          test(
            List("didYouDisposeOfResidentialProperty" -> "3"),
            "didYouDisposeOfResidentialProperty.capacitor.error.boolean"
          )
        }

      }

      "show a form error fro personal representative type" when {

        def test(formData: Seq[(String, String)], expectedErrorKey: String) =
          testFormError(performAction, "didYouDisposeOfResidentialProperty.personalRep.title")(
            formData,
            expectedErrorKey,
            requiredPreviousAnswers.copy(
              individualUserType = Some(IndividualUserType.PersonalRepresentative)
            )
          )

        "nothing is submitted" in {
          test(List.empty, "didYouDisposeOfResidentialProperty.personalRep.error.required")
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
        { case (draftReturn, answers) => draftReturn.copy(triageAnswers = answers) }
      )

      "handle successful updates" when {

        "the user is starting a new draft return and" when {

          "the user has answered some questions but not complete the section" in {
            testSuccessfulUpdateStartingNewDraft(
              performAction,
              requiredPreviousAnswers,
              List("didYouDisposeOfResidentialProperty" -> "true"),
              requiredPreviousAnswers.copy(assetType = Some(AssetType.Residential)),
              checkIsRedirect(_, routes.SingleDisposalsTriageController.checkYourAnswers())
            )
          }

          "the user has complete the section and has changed their answer and they choose asset type non-residential" in {
            val completeAnswers = sample[CompleteSingleDisposalTriageAnswers]
            testSuccessfulUpdateStartingNewDraft(
              performAction,
              completeAnswers.copy(assetType = Residential),
              List("didYouDisposeOfResidentialProperty" -> "false"),
              IncompleteSingleDisposalTriageAnswers
                .fromCompleteAnswers(completeAnswers)
                .copy(assetType = Some(NonResidential)),
              checkIsRedirect(_, routes.SingleDisposalsTriageController.checkYourAnswers())
            )
          }

          "the user has complete the section and has changed their answer and they choose asset type residential" in {
            val completeAnswers = sample[CompleteSingleDisposalTriageAnswers]
            testSuccessfulUpdateStartingNewDraft(
              performAction,
              completeAnswers.copy(assetType = NonResidential),
              List("didYouDisposeOfResidentialProperty" -> "true"),
              IncompleteSingleDisposalTriageAnswers
                .fromCompleteAnswers(completeAnswers)
                .copy(assetType = Some(Residential)),
              checkIsRedirect(_, routes.SingleDisposalsTriageController.checkYourAnswers())
            )
          }
        }

        "the user is filling out a draft return and" when {

          "the user has answered some questions but not complete the section" in {
            testSuccessfulUpdateFillingOutReturn(
              performAction,
              requiredPreviousAnswers,
              List("didYouDisposeOfResidentialProperty" -> "true"),
              _.copy(triageAnswers = requiredPreviousAnswers.copy(assetType = Some(AssetType.Residential))),
              checkIsRedirect(_, routes.SingleDisposalsTriageController.checkYourAnswers())
            )
          }

          "the user has complete the section and has changed their answer from not in the uk to was in the uk" in {
            forAll { completeAnswers: CompleteSingleDisposalTriageAnswers =>
              testSuccessfulUpdateFillingOutReturn(
                performAction,
                completeAnswers.copy(assetType = Residential),
                List("didYouDisposeOfResidentialProperty" -> "false"),
                _.copy(triageAnswers =
                  IncompleteSingleDisposalTriageAnswers
                    .fromCompleteAnswers(completeAnswers)
                    .copy(assetType = Some(NonResidential))
                ),
                checkIsRedirect(_, routes.SingleDisposalsTriageController.checkYourAnswers())
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
          individualUserType         = Some(sample[IndividualUserType]),
          hasConfirmedSingleDisposal = true,
          disposalMethod             = Some(DisposalMethod.Gifted),
          wasAUKResident             = Some(true),
          assetType                  = Some(AssetType.Residential)
        )

      val disposalDate = DisposalDate(LocalDate.of(2020, 1, 2), sample[TaxYear])

      def performAction(): Future[Result] = controller.whenWasDisposalDate()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(performAction, isValidJourney)

      behave like redirectWhenNoPreviousAnswerBehaviour[Boolean](
        performAction
      )(
        requiredPreviousAnswersUkResident,
        routes.SingleDisposalsTriageController.didYouDisposeOfAResidentialProperty(), {
          case (answers, w) =>
            answers.copy(assetType = w.map(if (_) AssetType.Residential else AssetType.NonResidential))
        }
      )

      behave like displayIndividualTriagePageBehaviorIncompleteJourney(
        performAction
      )(
        requiredPreviousAnswersUkResident,
        requiredPreviousAnswersUkResident.copy(disposalDate = Some(disposalDate)),
        Some("the user was a uk resident")
      )(
        "disposalDate.title",
        checkContent(_, routes.SingleDisposalsTriageController.didYouDisposeOfAResidentialProperty()),
        checkPrepopulatedContent(_, disposalDate)
      )

      {
        val answers = requiredPreviousAnswersUkResident.copy(
          wasAUKResident     = Some(false),
          countryOfResidence = Some(sample[Country]),
          assetType          = Some(AssetType.Residential)
        )
        behave like displayIndividualTriagePageBehaviorIncompleteJourney(
          performAction
        )(answers, answers.copy(disposalDate = Some(disposalDate)), Some("the user was not a uk resident"))(
          "disposalDate.title",
          checkContent(_, routes.SingleDisposalsTriageController.assetTypeForNonUkResidents()),
          checkPrepopulatedContent(_, disposalDate)
        )
      }

      behave like displayIndividualTriagePageBehaviorIncompleteJourney(
        performAction
      )(
        requiredPreviousAnswersUkResident,
        requiredPreviousAnswersUkResident.copy(tooEarlyDisposalDate = Some(disposalDate.value)),
        Some("the user had not disposed of their property in a valid tax year")
      )(
        "disposalDate.title",
        checkContent(_, routes.SingleDisposalsTriageController.didYouDisposeOfAResidentialProperty()),
        checkPrepopulatedContent(_, disposalDate)
      )

      behave like displayIndividualTriagePageBehaviorCompleteJourney(
        performAction
      )(
        sample[CompleteSingleDisposalTriageAnswers]
          .copy(countryOfResidence = Country.uk, assetType = AssetType.Residential, disposalDate = disposalDate)
      )(
        "disposalDate.title", { doc =>
          checkContent(doc, routes.SingleDisposalsTriageController.checkYourAnswers())
          checkPrepopulatedContent(doc, disposalDate)
        }
      )

      behave like displayCustomContentForAgentWithSelfAndPersonalRepType(
        performAction
      )(requiredPreviousAnswersUkResident.copy(disposalDate = Some(disposalDate)))(
        "disposalDate.agent.title",
        doc => checkContent(doc, routes.SingleDisposalsTriageController.didYouDisposeOfAResidentialProperty())
      )

      behave like displayCustomContentForTrusts(
        performAction
      )(requiredPreviousAnswersUkResident.copy(disposalDate = Some(disposalDate)))(
        "disposalDate.trust.title",
        doc => checkContent(doc, routes.SingleDisposalsTriageController.didYouDisposeOfAResidentialProperty())
      )

      def checkContent(doc: Document, backLink: Call): Unit = {
        doc.select("#back").attr("href") shouldBe backLink.url
        doc
          .select("#content > article > form")
          .attr("action") shouldBe routes.SingleDisposalsTriageController
          .whenWasDisposalDateSubmit()
          .url
      }

      def checkPrepopulatedContent(doc: Document, date: DisposalDate) = {
        doc.select("#disposalDate-day").attr("value")   shouldBe disposalDate.value.getDayOfMonth.toString
        doc.select("#disposalDate-month").attr("value") shouldBe disposalDate.value.getMonthValue.toString
        doc.select("#disposalDate-year").attr("value")  shouldBe disposalDate.value.getYear.toString
      }

    }

    "handling submitted disposal dates" must {

      def performAction(formData: (String, String)*): Future[Result] =
        controller.whenWasDisposalDateSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*))

      def formData(date: LocalDate) =
        List(
          "disposalDate-day"   -> date.getDayOfMonth().toString,
          "disposalDate-month" -> date.getMonthValue().toString,
          "disposalDate-year"  -> date.getYear().toString
        )

      def updateDraftReturn(d: DraftSingleDisposalReturn, newAnswers: SingleDisposalTriageAnswers) =
        d.copy(
          triageAnswers = newAnswers,
          acquisitionDetailsAnswers = d.acquisitionDetailsAnswers.map(
            _.unset(_.acquisitionDate)
              .unset(_.acquisitionPrice)
              .unset(_.rebasedAcquisitionPrice)
              .unset(_.shouldUseRebase)
              .unset(_.improvementCosts)
              .unset(_.acquisitionFees)
          ),
          initialGainOrLoss = None,
          reliefDetailsAnswers = d.reliefDetailsAnswers.map(
            _.unset(_.privateResidentsRelief)
              .unset(_.lettingsRelief)
          ),
          yearToDateLiabilityAnswers = d.yearToDateLiabilityAnswers.flatMap {
            case _: NonCalculatedYTDAnswers => None
            case c: CalculatedYTDAnswers =>
              Some(
                c.unset(_.hasEstimatedDetails)
                  .unset(_.calculatedTaxDue)
                  .unset(_.taxDue)
                  .unset(_.mandatoryEvidence)
                  .unset(_.expiredEvidence)
                  .unset(_.pendingUpscanUpload)
              )
          },
          supportingEvidenceAnswers = None
        )

      val tomorrow = today.plusDays(1L)

      val taxYear = sample[TaxYear]

      val requiredPreviousAnswers =
        IncompleteSingleDisposalTriageAnswers.empty.copy(
          individualUserType         = Some(sample[IndividualUserType]),
          hasConfirmedSingleDisposal = true,
          disposalMethod             = Some(DisposalMethod.Gifted),
          wasAUKResident             = Some(true),
          assetType                  = Some(AssetType.Residential)
        )

      behave like redirectToStartWhenInvalidJourney(() => performAction(), isValidJourney)

      behave like redirectWhenNoPreviousAnswerBehaviour[Boolean](() => performAction())(
        requiredPreviousAnswers,
        routes.SingleDisposalsTriageController.didYouDisposeOfAResidentialProperty(), {
          case (answers, w) =>
            answers.copy(assetType = w.map(if (_) AssetType.Residential else AssetType.NonResidential))
        }
      )

      "show an error page" when {

        "there is a problem getting the tax year" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionDataWithFillingOutReturn(requiredPreviousAnswers)._1)
            mockGetTaxYear(today)(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction(formData(today): _*))
        }

      }

      "show a form error" when {

        def test(formData: Seq[(String, String)], expectedErrorKey: String) =
          testFormError(performAction, "disposalDate.title")(formData, expectedErrorKey, requiredPreviousAnswers)

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

      behave like unsuccessfulUpdatesBehaviour(
        performAction,
        requiredPreviousAnswers,
        formData(today),
        requiredPreviousAnswers.copy(disposalDate = Some(DisposalDate(today, taxYear))),
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
                .copy(tooEarlyDisposalDate = Some(today), disposalDate = None, completionDate = None),
              checkIsRedirect(_, routes.CommonTriageQuestionsController.disposalDateTooEarly()),
              () => mockGetTaxYear(today)(Right(None))
            )

          }

          "a tax year can be found and the journey was complete" in {
            val completeJourney =
              sample[CompleteSingleDisposalTriageAnswers]
                .copy(disposalDate = DisposalDate(today, taxYear))
            val date = today.minusDays(1L)

            testSuccessfulUpdateStartingNewDraft(
              performAction,
              completeJourney,
              formData(date),
              IncompleteSingleDisposalTriageAnswers(
                completeJourney.individualUserType,
                true,
                Some(completeJourney.disposalMethod),
                Some(completeJourney.countryOfResidence.isUk()),
                if (completeJourney.countryOfResidence.isUk()) None else Some(completeJourney.countryOfResidence),
                Some(completeJourney.assetType),
                Some(DisposalDate(date, taxYear)),
                None,
                None
              ),
              checkIsRedirect(_, routes.SingleDisposalsTriageController.checkYourAnswers()),
              () => mockGetTaxYear(date)(Right(Some(taxYear)))
            )

          }

        }

        "the user is filling our a draft return and" when {

          "the section is incomplete" in {
            testSuccessfulUpdateFillingOutReturn(
              performAction,
              requiredPreviousAnswers,
              formData(today),
              updateDraftReturn(_, requiredPreviousAnswers.copy(disposalDate = Some(DisposalDate(today, taxYear)))),
              checkIsRedirect(_, routes.SingleDisposalsTriageController.checkYourAnswers()),
              () => mockGetTaxYear(today)(Right(Some(taxYear)))
            )

          }

          "the section is complete" in {
            forAll { c: CompleteSingleDisposalTriageAnswers =>
              val completeJourney = c.copy(disposalDate = DisposalDate(today, taxYear))
              val date            = today.minusDays(1L)
              val newAnswers =
                IncompleteSingleDisposalTriageAnswers(
                  completeJourney.individualUserType,
                  true,
                  Some(completeJourney.disposalMethod),
                  Some(completeJourney.countryOfResidence.isUk()),
                  if (completeJourney.countryOfResidence.isUk()) None else Some(completeJourney.countryOfResidence),
                  Some(completeJourney.assetType),
                  Some(DisposalDate(date, taxYear)),
                  None,
                  None
                )

              testSuccessfulUpdateFillingOutReturn(
                performAction,
                completeJourney,
                formData(date),
                updateDraftReturn(_, newAnswers),
                checkIsRedirect(_, routes.SingleDisposalsTriageController.checkYourAnswers()),
                () => mockGetTaxYear(date)(Right(Some(taxYear)))
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
            performAction(formData(today): _*),
            routes.SingleDisposalsTriageController.checkYourAnswers()
          )
        }

      }

    }

    "handling requests to display the when was completion date page" must {

      val disposalDate = DisposalDate(today, sample[TaxYear])

      val requiredPreviousAnswers =
        IncompleteSingleDisposalTriageAnswers.empty.copy(
          individualUserType         = Some(sample[IndividualUserType]),
          hasConfirmedSingleDisposal = true,
          disposalMethod             = Some(DisposalMethod.Gifted),
          wasAUKResident             = Some(true),
          assetType                  = Some(AssetType.Residential),
          disposalDate               = Some(disposalDate)
        )

      def performAction(): Future[Result] = controller.whenWasCompletionDate()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(performAction, isValidJourney)

      behave like redirectWhenNoPreviousAnswerBehaviour[DisposalDate](
        performAction
      )(
        requiredPreviousAnswers,
        routes.SingleDisposalsTriageController.whenWasDisposalDate(),
        { case (answers, d) => answers.copy(disposalDate = d) }
      )

      behave like displayIndividualTriagePageBehaviorIncompleteJourney(
        performAction
      )(
        requiredPreviousAnswers,
        requiredPreviousAnswers.copy(completionDate = Some(CompletionDate(disposalDate.value)))
      )(
        "completionDate.title",
        checkContent(_, routes.SingleDisposalsTriageController.whenWasDisposalDate()),
        checkPrepopulatedContent(_, disposalDate.value)
      )

      behave like displayIndividualTriagePageBehaviorCompleteJourney(
        performAction
      )(
        sample[CompleteSingleDisposalTriageAnswers].copy(
          assetType      = AssetType.Residential,
          completionDate = CompletionDate(disposalDate.value)
        )
      )(
        "completionDate.title", { doc =>
          checkContent(doc, routes.SingleDisposalsTriageController.checkYourAnswers())
          checkPrepopulatedContent(doc, disposalDate.value)
        }
      )

      behave like displayCustomContentForAgentWithSelfAndPersonalRepType(
        performAction
      )(requiredPreviousAnswers.copy(completionDate = Some(CompletionDate(disposalDate.value))))(
        "completionDate.agent.title",
        doc => checkContent(doc, routes.SingleDisposalsTriageController.whenWasDisposalDate())
      )

      behave like displayCustomContentForTrusts(
        performAction
      )(requiredPreviousAnswers.copy(completionDate = Some(CompletionDate(disposalDate.value))))(
        "completionDate.trust.title",
        doc => checkContent(doc, routes.SingleDisposalsTriageController.whenWasDisposalDate())
      )

      def checkContent(doc: Document, backLink: Call): Unit = {
        doc.select("#back").attr("href") shouldBe backLink.url
        doc
          .select("#content > article > form")
          .attr("action") shouldBe routes.SingleDisposalsTriageController
          .whenWasCompletionDateSubmit()
          .url

      }

      def checkPrepopulatedContent(doc: Document, date: LocalDate) = {
        doc.select("#completionDate-day").attr("value")   shouldBe date.getDayOfMonth.toString
        doc.select("#completionDate-month").attr("value") shouldBe date.getMonthValue.toString
        doc.select("#completionDate-year").attr("value")  shouldBe date.getYear.toString
      }

    }

    "handling submitted completion dates" must {

      def performAction(formData: (String, String)*): Future[Result] =
        controller.whenWasCompletionDateSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*))

      def formData(date: LocalDate) =
        List(
          "completionDate-day"   -> date.getDayOfMonth().toString,
          "completionDate-month" -> date.getMonthValue().toString,
          "completionDate-year"  -> date.getYear().toString
        )

      def updateDraftReturn(d: DraftSingleDisposalReturn, newAnswers: SingleDisposalTriageAnswers) =
        d.copy(
          triageAnswers = newAnswers,
          acquisitionDetailsAnswers = d.acquisitionDetailsAnswers.map(
            _.unset(_.acquisitionDate)
              .unset(_.acquisitionPrice)
              .unset(_.rebasedAcquisitionPrice)
              .unset(_.shouldUseRebase)
          ),
          initialGainOrLoss = None,
          reliefDetailsAnswers = d.reliefDetailsAnswers.map(
            _.unset(_.privateResidentsRelief)
              .unset(_.lettingsRelief)
          ),
          yearToDateLiabilityAnswers = d.yearToDateLiabilityAnswers.flatMap {
            case _: NonCalculatedYTDAnswers => None
            case c: CalculatedYTDAnswers =>
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

      val tomorrow = today.plusDays(1L)

      val dayBeforeDisposalDate = disposalDate.value.minusDays(1L)

      val requiredPreviousAnswers =
        IncompleteSingleDisposalTriageAnswers.empty.copy(
          individualUserType         = Some(sample[IndividualUserType]),
          hasConfirmedSingleDisposal = true,
          disposalMethod             = Some(DisposalMethod.Gifted),
          wasAUKResident             = Some(true),
          assetType                  = Some(AssetType.Residential),
          disposalDate               = Some(disposalDate)
        )

      behave like redirectToStartWhenInvalidJourney(() => performAction(), isValidJourney)

      behave like redirectWhenNoPreviousAnswerBehaviour[DisposalDate](() => performAction())(
        requiredPreviousAnswers,
        routes.SingleDisposalsTriageController.whenWasDisposalDate(),
        { case (i, d) => i.copy(disposalDate = d) }
      )

      "show a form error" when {

        def test(formData: Seq[(String, String)], expectedErrorKey: String) =
          testFormError(performAction, "completionDate.title")(formData, expectedErrorKey, requiredPreviousAnswers)

        "the date is invalid" in {
          dateErrorScenarios("completionDate", "").foreach { scenario =>
            withClue(s"For $scenario: ") {
              val formData = List(
                "completionDate-day"   -> scenario.dayInput,
                "completionDate-month" -> scenario.monthInput,
                "completionDate-year"  -> scenario.yearInput
              ).collect { case (id, Some(input)) => id -> input }
              test(formData, scenario.expectedErrorMessageKey)
            }
          }
        }

        "the completion date is in the future" in {
          test(formData(tomorrow), "completionDate.error.tooFarInFuture")
        }

        "the completion date is before the disposal date" in {
          test(formData(dayBeforeDisposalDate), "completionDate.error.tooFarInPast")
        }

      }

      behave like unsuccessfulUpdatesBehaviour(
        performAction,
        requiredPreviousAnswers,
        formData(disposalDate.value),
        requiredPreviousAnswers.copy(completionDate = Some(CompletionDate(disposalDate.value))),
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
              requiredPreviousAnswers.copy(completionDate = Some(completionDate)),
              checkIsRedirect(_, routes.SingleDisposalsTriageController.checkYourAnswers())
            )
          }

          "the section is complete" in {
            val completionDate = CompletionDate(disposalDate.value.plusDays(1L))
            val completeAnswers =
              sample[CompleteSingleDisposalTriageAnswers]
                .copy(completionDate = CompletionDate(disposalDate.value), disposalDate = disposalDate)

            testSuccessfulUpdateStartingNewDraft(
              performAction,
              completeAnswers,
              formData(completionDate.value),
              completeAnswers.copy(completionDate = completionDate),
              checkIsRedirect(_, routes.SingleDisposalsTriageController.checkYourAnswers())
            )
          }
        }

        "the user is filling our a draft return and" when {

          "the section is incomplete" in {
            val completionDate = CompletionDate(disposalDate.value)
            testSuccessfulUpdateFillingOutReturn(
              performAction,
              requiredPreviousAnswers,
              formData(completionDate.value),
              updateDraftReturn(_, requiredPreviousAnswers.copy(completionDate = Some(completionDate))),
              checkIsRedirect(_, routes.SingleDisposalsTriageController.checkYourAnswers())
            )
          }

          "the section is complete" in {
            forAll { c: CompleteSingleDisposalTriageAnswers =>
              val completionDate = CompletionDate(disposalDate.value.plusDays(1L))
              val completeAnswers =
                c.copy(completionDate = CompletionDate(disposalDate.value), disposalDate = disposalDate)

              testSuccessfulUpdateFillingOutReturn(
                performAction,
                completeAnswers,
                formData(completionDate.value),
                updateDraftReturn(_, completeAnswers.copy(completionDate = completionDate)),
                checkIsRedirect(_, routes.SingleDisposalsTriageController.checkYourAnswers())
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
            performAction(formData(disposalDate.value): _*),
            routes.SingleDisposalsTriageController.checkYourAnswers()
          )
        }

      }

    }

    "handling requests to display the country of residence page" must {

      val requiredPreviousAnswers = IncompleteSingleDisposalTriageAnswers.empty.copy(
        individualUserType         = Some(IndividualUserType.Self),
        hasConfirmedSingleDisposal = true,
        disposalMethod             = Some(DisposalMethod.Sold),
        wasAUKResident             = Some(false)
      )

      val (countryCode, countryName) = "HK" -> "Hong Kong"
      val country                    = Country(countryCode, Some(countryName))

      def performAction(): Future[Result] = controller.countryOfResidence()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(performAction, isValidJourney)

      behave like redirectWhenNoPreviousAnswerBehaviour[Boolean](
        performAction
      )(
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

      behave like displayIndividualTriagePageBehaviorIncompleteJourney(
        performAction
      )(requiredPreviousAnswers, requiredPreviousAnswers.copy(countryOfResidence = Some(country)))(
        "triage.enterCountry.title",
        checkContent(_, routes.SingleDisposalsTriageController.wereYouAUKResident()),
        _ => ()
      )

      behave like displayIndividualTriagePageBehaviorCompleteJourney(
        performAction
      )(
        sample[CompleteSingleDisposalTriageAnswers]
          .copy(countryOfResidence = country, individualUserType = Some(IndividualUserType.Self))
      )(
        "triage.enterCountry.title",
        doc => checkContent(doc, routes.SingleDisposalsTriageController.checkYourAnswers())
      )

      behave like displayIndividualTriagePageBehaviorCompleteJourney(
        performAction
      )(
        sample[CompleteSingleDisposalTriageAnswers]
          .copy(countryOfResidence = country, individualUserType = Some(IndividualUserType.Capacitor))
      )(
        "triage.enterCountry.capacitor.title",
        doc => checkContent(doc, routes.SingleDisposalsTriageController.checkYourAnswers())
      )

      behave like displayIndividualTriagePageBehaviorCompleteJourney(
        performAction
      )(
        sample[CompleteSingleDisposalTriageAnswers].copy(
          countryOfResidence = country,
          individualUserType = Some(IndividualUserType.PersonalRepresentative)
        )
      )(
        "triage.enterCountry.personalRep.title",
        doc => checkContent(doc, routes.SingleDisposalsTriageController.checkYourAnswers())
      )

      behave like displayCustomContentForAgentWithSelfAndPersonalRepType(
        performAction
      )(requiredPreviousAnswers.copy(countryOfResidence = Some(country)))(
        "triage.enterCountry.agent.title",
        doc => checkContent(doc, routes.SingleDisposalsTriageController.wereYouAUKResident())
      )

      behave like displayCustomContentForAgentWithSelfAndPersonalRepType(
        performAction
      )(
        requiredPreviousAnswers.copy(
          countryOfResidence = Some(country),
          individualUserType = Some(IndividualUserType.PersonalRepresentative)
        )
      )(
        "triage.enterCountry.personalRep.title",
        doc => checkContent(doc, routes.SingleDisposalsTriageController.wereYouAUKResident())
      )

      behave like displayCustomContentForTrusts(
        performAction
      )(requiredPreviousAnswers.copy(countryOfResidence = Some(country)))(
        "triage.enterCountry.trust.title",
        doc => checkContent(doc, routes.SingleDisposalsTriageController.wereYouAUKResident())
      )

      def checkContent(doc: Document, backLink: Call): Unit = {
        doc.select("#back").attr("href") shouldBe backLink.url
        doc
          .select("#content > article > form")
          .attr("action") shouldBe routes.SingleDisposalsTriageController
          .countryOfResidenceSubmit()
          .url
      }

    }

    "handling submitted answers to the country of residence page" must {

      def performAction(formData: (String, String)*): Future[Result] =
        controller.countryOfResidenceSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*))

      def updateDraftReturn(d: DraftSingleDisposalReturn, newAnswers: SingleDisposalTriageAnswers) =
        d.copy(
          triageAnswers = newAnswers,
          yearToDateLiabilityAnswers = d.yearToDateLiabilityAnswers.flatMap {
            case _: CalculatedYTDAnswers => None
            case n: NonCalculatedYTDAnswers =>
              Some(
                n.unset(
                  _.hasEstimatedDetails
                )
              )
          }
        )

      val requiredPreviousAnswers =
        IncompleteSingleDisposalTriageAnswers.empty.copy(
          individualUserType         = Some(IndividualUserType.Self),
          hasConfirmedSingleDisposal = true,
          disposalMethod             = Some(DisposalMethod.Sold),
          wasAUKResident             = Some(false)
        )

      val (countryCode, countryName) = "HK" -> "Hong Kong"
      val country                    = Country(countryCode, Some(countryName))

      behave like redirectToStartWhenInvalidJourney(() => performAction(), isValidJourney)

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
                sessionDataWithStartingNewDraftReturn(requiredPreviousAnswers.copy(wasAUKResident = Some(true)))._1
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

        def test(formData: Seq[(String, String)], expectedErrorKey: String) =
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

        def test(formData: Seq[(String, String)], expectedErrorKey: String) =
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

        def test(formData: Seq[(String, String)], expectedErrorKey: String) =
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
        List("countryCode" -> countryCode),
        requiredPreviousAnswers.copy(countryOfResidence = Some(country)),
        updateDraftReturn
      )

      "handle successful updates" when {

        "the user is starting a new draft return and" when {

          "the user has answered some questions but not complete the section" in {
            testSuccessfulUpdateStartingNewDraft(
              performAction,
              requiredPreviousAnswers,
              List("countryCode" -> countryCode),
              requiredPreviousAnswers.copy(countryOfResidence = Some(country)),
              checkIsRedirect(_, routes.SingleDisposalsTriageController.checkYourAnswers())
            )
          }

          "the user has complete the section and has changed their answer and they choose asset type residential" in {
            val completeAnswers =
              sample[CompleteSingleDisposalTriageAnswers].copy(countryOfResidence = Country("CC", None))
            testSuccessfulUpdateStartingNewDraft(
              performAction,
              completeAnswers,
              List("countryCode" -> countryCode),
              completeAnswers.copy(countryOfResidence = country),
              checkIsRedirect(_, routes.SingleDisposalsTriageController.checkYourAnswers())
            )
          }
        }

        "the user is filling out a draft return and" when {

          "the user has answered some questions but not complete the section" in {
            testSuccessfulUpdateFillingOutReturn(
              performAction,
              requiredPreviousAnswers,
              List("countryCode" -> countryCode),
              updateDraftReturn(_, requiredPreviousAnswers.copy(countryOfResidence = Some(country))),
              checkIsRedirect(_, routes.SingleDisposalsTriageController.checkYourAnswers())
            )
          }

          "the user has complete the section and has changed their answer from not in the uk to was in the uk" in {
            forAll { c: CompleteSingleDisposalTriageAnswers =>
              val completeAnswers = c.copy(countryOfResidence = Country("CC", None))
              testSuccessfulUpdateFillingOutReturn(
                performAction,
                completeAnswers,
                List("countryCode" -> countryCode),
                updateDraftReturn(_, completeAnswers.copy(countryOfResidence = country)),
                checkIsRedirect(_, routes.SingleDisposalsTriageController.checkYourAnswers())
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

          val result = performAction("countryCode" -> countryCode)
          checkIsRedirect(result, routes.SingleDisposalsTriageController.checkYourAnswers())
        }

      }

    }

    "handling requests to display the asset type for non uk residents page" must {

      val requiredPreviousAnswers =
        IncompleteSingleDisposalTriageAnswers.empty.copy(
          individualUserType         = Some(IndividualUserType.Self),
          hasConfirmedSingleDisposal = true,
          disposalMethod             = Some(DisposalMethod.Sold),
          wasAUKResident             = Some(false),
          countryOfResidence         = Some(sample[Country])
        )

      def performAction(): Future[Result] = controller.assetTypeForNonUkResidents()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(performAction, isValidJourney)

      behave like redirectWhenNoPreviousAnswerBehaviour[Country](
        performAction
      )(
        requiredPreviousAnswers,
        routes.SingleDisposalsTriageController.countryOfResidence(),
        { case (answers, country) => answers.copy(countryOfResidence = country) }
      )

      behave like displayIndividualTriagePageBehaviorIncompleteJourney(
        performAction
      )(requiredPreviousAnswers, requiredPreviousAnswers.copy(assetType = Some(AssetType.MixedUse)))(
        "assetTypeForNonUkResidents.title",
        checkContent(_, routes.SingleDisposalsTriageController.countryOfResidence()),
        _.select("#assetTypeForNonUkResidents-2").attr("checked") shouldBe "checked"
      )

      behave like displayIndividualTriagePageBehaviorIncompleteJourney(
        performAction
      )(
        requiredPreviousAnswers.copy(individualUserType = Some(IndividualUserType.Capacitor)),
        requiredPreviousAnswers.copy(
          assetType          = Some(AssetType.MixedUse),
          individualUserType = Some(IndividualUserType.Capacitor)
        )
      )(
        "assetTypeForNonUkResidents.capacitor.title",
        checkContent(_, routes.SingleDisposalsTriageController.countryOfResidence()),
        _.select("#assetTypeForNonUkResidents-2").attr("checked") shouldBe "checked"
      )

      behave like displayIndividualTriagePageBehaviorIncompleteJourney(
        performAction
      )(
        requiredPreviousAnswers.copy(individualUserType = Some(IndividualUserType.PersonalRepresentative)),
        requiredPreviousAnswers.copy(
          assetType          = Some(AssetType.MixedUse),
          individualUserType = Some(IndividualUserType.PersonalRepresentative)
        )
      )(
        "assetTypeForNonUkResidents.personalRep.title",
        checkContent(_, routes.SingleDisposalsTriageController.countryOfResidence()),
        _.select("#assetTypeForNonUkResidents-2").attr("checked") shouldBe "checked"
      )

      behave like displayIndividualTriagePageBehaviorCompleteJourney(
        performAction
      )(
        sample[CompleteSingleDisposalTriageAnswers]
          .copy(
            countryOfResidence = sample[Country],
            assetType          = AssetType.Residential,
            individualUserType = Some(IndividualUserType.Self)
          )
      )(
        "assetTypeForNonUkResidents.title", { doc =>
          checkContent(doc, routes.SingleDisposalsTriageController.checkYourAnswers())
          doc.select("#assetTypeForNonUkResidents-0").attr("checked") shouldBe "checked"
        }
      )

      behave like displayIndividualTriagePageBehaviorCompleteJourney(
        performAction
      )(
        sample[CompleteSingleDisposalTriageAnswers]
          .copy(
            countryOfResidence = sample[Country],
            assetType          = AssetType.Residential,
            individualUserType = Some(IndividualUserType.Capacitor)
          )
      )(
        "assetTypeForNonUkResidents.capacitor.title", { doc =>
          checkContent(doc, routes.SingleDisposalsTriageController.checkYourAnswers())
          doc.select("#assetTypeForNonUkResidents-0").attr("checked") shouldBe "checked"
        }
      )

      behave like displayIndividualTriagePageBehaviorCompleteJourney(
        performAction
      )(
        sample[CompleteSingleDisposalTriageAnswers]
          .copy(
            countryOfResidence = sample[Country],
            assetType          = AssetType.Residential,
            individualUserType = Some(IndividualUserType.PersonalRepresentative)
          )
      )(
        "assetTypeForNonUkResidents.personalRep.title", { doc =>
          checkContent(doc, routes.SingleDisposalsTriageController.checkYourAnswers())
          doc.select("#assetTypeForNonUkResidents-0").attr("checked") shouldBe "checked"
        }
      )

      behave like displayCustomContentForAgentWithSelfAndPersonalRepType(
        performAction
      )(requiredPreviousAnswers.copy(assetType = Some(AssetType.Residential)))(
        "assetTypeForNonUkResidents.agent.title",
        doc => checkContent(doc, routes.SingleDisposalsTriageController.countryOfResidence())
      )

      behave like displayCustomContentForAgentWithSelfAndPersonalRepType(
        performAction
      )(
        requiredPreviousAnswers.copy(
          assetType          = Some(AssetType.Residential),
          individualUserType = Some(IndividualUserType.PersonalRepresentative)
        )
      )(
        "assetTypeForNonUkResidents.personalRep.title",
        doc => checkContent(doc, routes.SingleDisposalsTriageController.countryOfResidence())
      )

      behave like displayCustomContentForTrusts(
        performAction
      )(requiredPreviousAnswers.copy(assetType = Some(AssetType.Residential)))(
        "assetTypeForNonUkResidents.trust.title",
        doc => checkContent(doc, routes.SingleDisposalsTriageController.countryOfResidence())
      )

      def checkContent(doc: Document, backLink: Call): Unit = {
        doc.select("#back").attr("href") shouldBe backLink.url
        doc
          .select("#content > article > form")
          .attr("action") shouldBe routes.SingleDisposalsTriageController
          .assetTypeForNonUkResidentsSubmit()
          .url
      }

    }

    "handling submitted answers to the asset type for non uk residents page" must {

      def performAction(formData: (String, String)*): Future[Result] =
        controller.assetTypeForNonUkResidentsSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*))

      def updateDraftReturn(d: DraftSingleDisposalReturn, newAnswers: SingleDisposalTriageAnswers) =
        d.copy(
          triageAnswers             = newAnswers,
          propertyAddress           = None,
          disposalDetailsAnswers    = None,
          acquisitionDetailsAnswers = None,
          initialGainOrLoss         = None,
          reliefDetailsAnswers = d.reliefDetailsAnswers.map(
            _.unset(_.privateResidentsRelief).unset(_.lettingsRelief)
          ),
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
          individualUserType         = Some(IndividualUserType.Self),
          hasConfirmedSingleDisposal = true,
          disposalMethod             = Some(DisposalMethod.Sold),
          wasAUKResident             = Some(false),
          countryOfResidence         = Some(sample[Country])
        )

      behave like redirectToStartWhenInvalidJourney(() => performAction(), isValidJourney)

      behave like redirectWhenNoPreviousAnswerBehaviour[Country](() => performAction())(
        requiredPreviousAnswers,
        routes.SingleDisposalsTriageController.countryOfResidence(),
        { case (answers, country) => answers.copy(countryOfResidence = country) }
      )

      "show a form error for self type" when {

        def test(formData: Seq[(String, String)], expectedErrorKey: String) =
          testFormError(performAction, "assetTypeForNonUkResidents.title")(
            formData,
            expectedErrorKey,
            requiredPreviousAnswers
          )

        "nothing is submitted" in {
          test(List.empty, "assetTypeForNonUkResidents.error.required")
        }

        "the option is not recognised" in {
          test(List("assetTypeForNonUkResidents" -> "4"), "assetTypeForNonUkResidents.error.invalid")
        }

      }

      "show a form error for capacitor type" when {

        def test(formData: Seq[(String, String)], expectedErrorKey: String) =
          testFormError(performAction, "assetTypeForNonUkResidents.capacitor.title")(
            formData,
            expectedErrorKey,
            requiredPreviousAnswers.copy(
              individualUserType = Some(IndividualUserType.Capacitor)
            )
          )

        "nothing is submitted" in {
          test(List.empty, "assetTypeForNonUkResidents.capacitor.error.required")
        }

        "the option is not recognised" in {
          test(
            List("assetTypeForNonUkResidents" -> "4"),
            "assetTypeForNonUkResidents.capacitor.error.invalid"
          )
        }

      }

      "show a form error for personal representative type" when {

        def test(formData: Seq[(String, String)], expectedErrorKey: String) =
          testFormError(performAction, "assetTypeForNonUkResidents.personalRep.title")(
            formData,
            expectedErrorKey,
            requiredPreviousAnswers.copy(
              individualUserType = Some(IndividualUserType.PersonalRepresentative)
            )
          )

        "nothing is submitted" in {
          test(List.empty, "assetTypeForNonUkResidents.personalRep.error.required")
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
              requiredPreviousAnswers.copy(assetType = Some(AssetType.Residential)),
              checkIsRedirect(_, routes.SingleDisposalsTriageController.checkYourAnswers())
            )
          }

          "the user has complete the section" in {
            val completeAnswers = sample[CompleteSingleDisposalTriageAnswers]
            testSuccessfulUpdateStartingNewDraft(
              performAction,
              completeAnswers.copy(assetType = AssetType.Residential),
              List("assetTypeForNonUkResidents" -> "1"),
              completeAnswers.copy(assetType = AssetType.NonResidential),
              checkIsRedirect(_, routes.SingleDisposalsTriageController.checkYourAnswers())
            )
          }

        }

        "the user is filling out a draft return and" when {

          "the section is complete" in {
            forAll { completeAnswers: CompleteSingleDisposalTriageAnswers =>
              testSuccessfulUpdateFillingOutReturn(
                performAction,
                completeAnswers.copy(assetType = AssetType.Residential),
                List("assetTypeForNonUkResidents" -> "2"),
                updateDraftReturn(
                  _,
                  completeAnswers.copy(assetType = AssetType.MixedUse)
                ),
                checkIsRedirect(_, routes.SingleDisposalsTriageController.checkYourAnswers())
              )
            }
          }

          "the section is incomplete" in {
            testSuccessfulUpdateFillingOutReturn(
              performAction,
              requiredPreviousAnswers,
              List("assetTypeForNonUkResidents" -> "3"),
              updateDraftReturn(
                _,
                requiredPreviousAnswers.copy(assetType = Some(AssetType.IndirectDisposal))
              ),
              checkIsRedirect(_, routes.SingleDisposalsTriageController.checkYourAnswers())
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
          individualUserType         = Some(sample[IndividualUserType]),
          hasConfirmedSingleDisposal = true,
          disposalMethod             = Some(DisposalMethod.Other),
          wasAUKResident             = Some(false),
          countryOfResidence         = Some(sample[Country])
        )

      def performAction(): Future[Result] = controller.disposalDateOfShares()(FakeRequest())

      "Page is displayed correctly" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            sessionDataWithFillingOutReturn(
              requiredPreviousAnswers.copy(
                assetType = Some(AssetType.IndirectDisposal)
              )
            )._1
          )
        }
        checkPageIsDisplayed(
          performAction,
          messageFromMessageKey("sharesDisposalDate.title"),
          doc => {
            doc.select("#sharesDisposalDate-form-hint").text() shouldBe messageFromMessageKey(
              "sharesDisposalDate.helpText"
            )
            doc.select("#back").attr("href") shouldBe routes.SingleDisposalsTriageController
              .assetTypeForNonUkResidents()
              .url
            doc
              .select("#content > article > form")
              .attr("action") shouldBe routes.SingleDisposalsTriageController.disposalDateOfSharesSubmit().url
          }
        )

      }

    }

    "handling submitted answers to the shrare disposal date for non uk residents page" must {

      def performAction(formData: (String, String)*): Future[Result] =
        controller.disposalDateOfSharesSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*))

      def formData(date: LocalDate) =
        List(
          "sharesDisposalDate-day"   -> date.getDayOfMonth().toString,
          "sharesDisposalDate-month" -> date.getMonthValue().toString,
          "sharesDisposalDate-year"  -> date.getYear().toString
        )

      def updateDraftReturn(d: DraftSingleDisposalReturn, newAnswers: SingleDisposalTriageAnswers) =
        d.copy(
          triageAnswers = newAnswers,
          acquisitionDetailsAnswers = d.acquisitionDetailsAnswers.map(
            _.unset(_.acquisitionDate)
              .unset(_.acquisitionPrice)
              .unset(_.rebasedAcquisitionPrice)
              .unset(_.shouldUseRebase)
              .unset(_.improvementCosts)
              .unset(_.acquisitionFees)
          ),
          initialGainOrLoss = None,
          reliefDetailsAnswers = d.reliefDetailsAnswers.map(
            _.unset(_.privateResidentsRelief)
              .unset(_.lettingsRelief)
          ),
          yearToDateLiabilityAnswers = d.yearToDateLiabilityAnswers.flatMap {
            case _: NonCalculatedYTDAnswers => None
            case c: CalculatedYTDAnswers =>
              Some(
                c.unset(_.hasEstimatedDetails)
                  .unset(_.calculatedTaxDue)
                  .unset(_.taxDue)
                  .unset(_.mandatoryEvidence)
                  .unset(_.expiredEvidence)
                  .unset(_.pendingUpscanUpload)
              )
          },
          supportingEvidenceAnswers = None
        )

      val tomorrow = today.plusDays(1L)

      val taxYear = sample[TaxYear]

      val requiredPreviousAnswers =
        IncompleteSingleDisposalTriageAnswers.empty.copy(
          individualUserType         = Some(sample[IndividualUserType]),
          hasConfirmedSingleDisposal = true,
          disposalMethod             = Some(DisposalMethod.Other),
          wasAUKResident             = Some(false),
          assetType                  = Some(AssetType.IndirectDisposal)
        )

      behave like redirectToStartWhenInvalidJourney(() => performAction(), isValidJourney)

      behave like redirectWhenNoPreviousAnswerBehaviour[Boolean](() => performAction())(
        requiredPreviousAnswers,
        routes.SingleDisposalsTriageController.assetTypeForNonUkResidents(), {
          case (answers, w) =>
            answers.copy(assetType = w.map(if (_) AssetType.Residential else AssetType.IndirectDisposal))
        }
      )

      "show an error page" when {

        "there is a problem getting the tax year" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionDataWithFillingOutReturn(requiredPreviousAnswers)._1)
            mockGetTaxYear(today)(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction(formData(today): _*))
        }

      }

      "show a form error" when {

        def test(formData: Seq[(String, String)], expectedErrorKey: String) =
          testFormError(performAction, "sharesDisposalDate.title")(formData, expectedErrorKey, requiredPreviousAnswers)

        "the date is invalid" in {
          dateErrorScenarios("sharesDisposalDate", "").foreach { scenario =>
            withClue(s"For $scenario: ") {
              val formData = List(
                "sharesDisposalDate-day"   -> scenario.dayInput,
                "sharesDisposalDate-month" -> scenario.monthInput,
                "sharesDisposalDate-year"  -> scenario.yearInput
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
            "sharesDisposalDate.error.tooFarInFuture"
          )

          test(formData(tomorrow), "sharesDisposalDate.error.tooFarInFuture")
        }

      }

      behave like unsuccessfulUpdatesBehaviour(
        performAction,
        requiredPreviousAnswers,
        formData(today),
        requiredPreviousAnswers
          .copy(disposalDate = Some(DisposalDate(today, taxYear)), completionDate = Some(CompletionDate(today))),
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
                  disposalDate         = None,
                  completionDate       = Some(CompletionDate(today))
                ),
              checkIsRedirect(_, routes.CommonTriageQuestionsController.disposalsOfSharesTooEarly()),
              () => mockGetTaxYear(today)(Right(None))
            )

          }

          "a tax year can be found and the journey was complete" in {
            val completeJourney =
              sample[CompleteSingleDisposalTriageAnswers]
                .copy(
                  disposalDate   = DisposalDate(today, taxYear),
                  completionDate = CompletionDate(today),
                  assetType      = IndirectDisposal
                )
            val date = today.minusDays(1L)

            testSuccessfulUpdateStartingNewDraft(
              performAction,
              completeJourney,
              formData(date),
              IncompleteSingleDisposalTriageAnswers(
                completeJourney.individualUserType,
                true,
                Some(completeJourney.disposalMethod),
                Some(completeJourney.countryOfResidence.isUk()),
                if (completeJourney.countryOfResidence.isUk()) None else Some(completeJourney.countryOfResidence),
                Some(completeJourney.assetType),
                Some(DisposalDate(date, taxYear)),
                Some(CompletionDate(date)),
                None
              ),
              checkIsRedirect(_, routes.SingleDisposalsTriageController.checkYourAnswers()),
              () => mockGetTaxYear(date)(Right(Some(taxYear)))
            )

          }

        }

        "the user is filling our a draft return and" when {

          "the section is incomplete" in {
            testSuccessfulUpdateFillingOutReturn(
              performAction,
              requiredPreviousAnswers,
              formData(today),
              updateDraftReturn(
                _,
                requiredPreviousAnswers
                  .copy(disposalDate = Some(DisposalDate(today, taxYear)), completionDate = Some(CompletionDate(today)))
              ),
              checkIsRedirect(_, routes.SingleDisposalsTriageController.checkYourAnswers()),
              () => mockGetTaxYear(today)(Right(Some(taxYear)))
            )

          }

          "the section is complete" in {
            forAll { c: CompleteSingleDisposalTriageAnswers =>
              val completeJourney = c.copy(
                disposalDate   = DisposalDate(today, taxYear),
                completionDate = CompletionDate(today),
                assetType      = IndirectDisposal
              )
              val date = today.minusDays(1L)
              val newAnswers =
                IncompleteSingleDisposalTriageAnswers(
                  completeJourney.individualUserType,
                  true,
                  Some(completeJourney.disposalMethod),
                  Some(completeJourney.countryOfResidence.isUk()),
                  if (completeJourney.countryOfResidence.isUk()) None else Some(completeJourney.countryOfResidence),
                  Some(completeJourney.assetType),
                  Some(DisposalDate(date, taxYear)),
                  Some(CompletionDate(date)),
                  None
                )

              testSuccessfulUpdateFillingOutReturn(
                performAction,
                completeJourney,
                formData(date),
                updateDraftReturn(_, newAnswers),
                checkIsRedirect(_, routes.SingleDisposalsTriageController.checkYourAnswers()),
                () => mockGetTaxYear(date)(Right(Some(taxYear)))
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
            performAction(formData(today): _*),
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
          sample[CompletionDate]
        )

      val allQuestionsAnswered = IncompleteSingleDisposalTriageAnswers(
        completeTriageQuestions.individualUserType,
        true,
        Some(completeTriageQuestions.disposalMethod),
        Some(true),
        None,
        Some(completeTriageQuestions.assetType),
        Some(completeTriageQuestions.disposalDate),
        Some(completeTriageQuestions.completionDate),
        None
      )

      "redirect to the correct page" when {

        case class Scenario(
          answers: SingleDisposalTriageAnswers,
          name: Either[TrustName, IndividualName],
          expectedRedirect: Call
        )

        def test(
          sessionDataWith: (SingleDisposalTriageAnswers, Either[TrustName, IndividualName]) => SessionData
        ): Unit =
          List(
            Scenario(
              allQuestionsAnswered.copy(individualUserType = None),
              Right(sample[IndividualName]),
              routes.CommonTriageQuestionsController.whoIsIndividualRepresenting()
            ),
            Scenario(
              allQuestionsAnswered.copy(individualUserType = None, hasConfirmedSingleDisposal = false),
              Left(sample[TrustName]),
              routes.CommonTriageQuestionsController.howManyProperties()
            ),
            Scenario(
              allQuestionsAnswered.copy(hasConfirmedSingleDisposal = false),
              Right(sample[IndividualName]),
              routes.CommonTriageQuestionsController.howManyProperties()
            ),
            Scenario(
              allQuestionsAnswered.copy(disposalMethod = None),
              Right(sample[IndividualName]),
              routes.SingleDisposalsTriageController.howDidYouDisposeOfProperty()
            ),
            Scenario(
              allQuestionsAnswered.copy(wasAUKResident = None),
              Right(sample[IndividualName]),
              routes.SingleDisposalsTriageController.wereYouAUKResident()
            ),
            Scenario(
              allQuestionsAnswered.copy(assetType = None),
              Right(sample[IndividualName]),
              routes.SingleDisposalsTriageController
                .didYouDisposeOfAResidentialProperty()
            ),
            Scenario(
              allQuestionsAnswered.copy(disposalDate = None),
              Right(sample[IndividualName]),
              routes.SingleDisposalsTriageController
                .whenWasDisposalDate()
            ),
            Scenario(
              allQuestionsAnswered.copy(completionDate = None),
              Right(sample[IndividualName]),
              routes.SingleDisposalsTriageController
                .whenWasCompletionDate()
            ),
            Scenario(
              allQuestionsAnswered
                .copy(wasAUKResident = Some(false), countryOfResidence = None),
              Right(sample[IndividualName]),
              routes.SingleDisposalsTriageController
                .countryOfResidence()
            ),
            Scenario(
              allQuestionsAnswered
                .copy(wasAUKResident = Some(true), assetType = Some(AssetType.NonResidential)),
              Right(sample[IndividualName]),
              routes.CommonTriageQuestionsController.ukResidentCanOnlyDisposeResidential()
            ),
            Scenario(
              allQuestionsAnswered
                .copy(
                  wasAUKResident     = Some(false),
                  countryOfResidence = Some(sample[Country]),
                  assetType          = Some(AssetType.MixedUse)
                ),
              Right(sample[IndividualName]),
              routes.CommonTriageQuestionsController
                .assetTypeNotYetImplemented()
            ),
            Scenario(
              allQuestionsAnswered
                .copy(
                  wasAUKResident     = Some(false),
                  countryOfResidence = Some(sample[Country]),
                  assetType          = Some(AssetType.IndirectDisposal),
                  completionDate     = None,
                  disposalDate       = None
                ),
              Right(sample[IndividualName]),
              routes.SingleDisposalsTriageController.disposalDateOfShares()
            ),
            Scenario(
              allQuestionsAnswered.copy(individualUserType = Some(IndividualUserType.Capacitor)),
              Right(sample[IndividualName]),
              representee.routes.RepresenteeController.enterName()
            ),
            Scenario(
              allQuestionsAnswered.copy(individualUserType = Some(IndividualUserType.PersonalRepresentative)),
              Right(sample[IndividualName]),
              representee.routes.RepresenteeController.enterName()
            )
          ).foreach {
            case Scenario(state, name, expectedRedirect) =>
              withClue(s"For state $state and expected redirect url ${expectedRedirect.url}: ") {
                inSequence {
                  mockAuthWithNoRetrievals()
                  mockGetSession(sessionDataWith(state, name))
                }

                checkIsRedirect(performAction(), expectedRedirect)
              }
          }

        "a question has not yet been answered and a draft return has not been created" in {
          test(
            sessionDataWithStartingNewDraftReturn(_, _, representeeAnswers = sample[IncompleteRepresenteeAnswers])._1
          )
        }

        "a question has not yet been answered and a draft return has been created" in {
          test(sessionDataWithFillingOutReturn(_, _, representeeAnswers = sample[IncompleteRepresenteeAnswers])._1)
        }

      }

      "show a exit page when a user has selected mixed use" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            sessionDataWithStartingNewDraftReturn(
              allQuestionsAnswered.copy(
                assetType = Some(AssetType.MixedUse)
              )
            )._1
          )
        }

        val result = performAction()
        checkIsRedirect(result, routes.CommonTriageQuestionsController.assetTypeNotYetImplemented())

      }

      "show an error page" when {

        "all the questions have now been answered but the session data cannot be updated" in {
          val (session, journey) = sessionDataWithStartingNewDraftReturn(allQuestionsAnswered)
          val updatedJourney     = journey.copy(newReturnTriageAnswers = Right(completeTriageQuestions))
          val updatedSession     = session.copy(journeyStatus = Some(updatedJourney))

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
            doc => validateSingleDisposalTriageCheckYourAnswersPage(completeSingleDisposalTriageAnswers, userType, doc)
          )

        "all the questions have now been answered and the session is updated when a draft return has not yet been created" in {
          val (session, journey) = sessionDataWithStartingNewDraftReturn(allQuestionsAnswered)
          val updatedJourney     = journey.copy(newReturnTriageAnswers = Right(completeTriageQuestions))
          val updatedSession     = session.copy(journeyStatus = Some(updatedJourney))

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
          val (session, journey, draftReturn) = sessionDataWithFillingOutReturn(allQuestionsAnswered)
          val updatedJourney =
            journey.copy(draftReturn = draftReturn.copy(triageAnswers = completeTriageQuestions))
          val updatedSession = session.copy(journeyStatus = Some(updatedJourney))

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
            mockGetSession(sessionDataWithStartingNewDraftReturn(completeTriageQuestions)._1)
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
            mockGetSession(sessionDataWithFillingOutReturn(completeTriageQuestions)._1)
          }

          testIsCheckYourAnswers(
            performAction(),
            completeTriageQuestions,
            "triage.check-your-answers.title",
            None
          )
        }

        "the user is an agent" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithFillingOutReturn(completeTriageQuestions)._1.copy(userType = Some(UserType.Agent))
            )
          }

          testIsCheckYourAnswers(
            performAction(),
            completeTriageQuestions,
            "triage.check-your-answers.title",
            Some(UserType.Agent)
          )
        }

      }

    }

    "handling submit requests from the check your answers page" must {

      def performAction() =
        controller.checkYourAnswersSubmit()(FakeRequest())

      val completeAnswers = sample[CompleteSingleDisposalTriageAnswers]

      val startingNewDraftReturn = sample[StartingNewDraftReturn].copy(newReturnTriageAnswers = Right(completeAnswers))

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
          TimeUtils.today()
        )
      )

      val sessionDataWithFillingOutDraftReturn = SessionData.empty.copy(journeyStatus = Some(fillingOutReturn))

      "redirect to the check your answers page" when {

        "the user has not answered all the questions in the triage section" in {
          val incompleteAnswers = sample[IncompleteSingleDisposalTriageAnswers].copy(
            individualUserType = Some(IndividualUserType.Self)
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionDataWithStartingNewDraftReturn(incompleteAnswers)._1)
          }

          checkIsRedirect(performAction(), routes.SingleDisposalsTriageController.checkYourAnswers())
        }

      }

      "show an error page" when {

        "there is a problem storing a draft return" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionWithCompleteStartingNewDraftReturn)
            mockGetNextUUID(uuid)
            mockStoreDraftReturn(
              fillingOutReturn.draftReturn,
              fillingOutReturn.subscribedDetails.cgtReference,
              fillingOutReturn.agentReferenceNumber
            )(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction())
        }

        "there is a problem updating the session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionWithCompleteStartingNewDraftReturn)
            mockGetNextUUID(uuid)
            mockStoreDraftReturn(
              fillingOutReturn.draftReturn,
              fillingOutReturn.subscribedDetails.cgtReference,
              fillingOutReturn.agentReferenceNumber
            )(Right(()))
            mockStoreSession(sessionDataWithFillingOutDraftReturn)(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction())
        }

      }

      "redirect to the task list page" when {

        "the draft return is stored and the session is updated and a draft return had not already been created" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionWithCompleteStartingNewDraftReturn)
            mockGetNextUUID(uuid)
            mockStoreDraftReturn(
              fillingOutReturn.draftReturn,
              fillingOutReturn.subscribedDetails.cgtReference,
              fillingOutReturn.agentReferenceNumber
            )(Right(()))
            mockStoreSession(sessionDataWithFillingOutDraftReturn)(Right(()))
          }

          checkIsRedirect(performAction(), returnsRoutes.TaskListController.taskList())
        }

        "the draft return is stored and the session is updated and a draft return had already been created" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionDataWithFillingOutDraftReturn)
          }

          checkIsRedirect(performAction(), returnsRoutes.TaskListController.taskList())
        }

      }

    }

  }

  def redirectWhenNoPreviousAnswerBehaviour[A](performAction: () => Future[Result])(
    requiredPreviousAnswers: IncompleteSingleDisposalTriageAnswers,
    redirectToPreviousAnswer: => Call,
    setPreviousAnswer: (IncompleteSingleDisposalTriageAnswers, Option[A]) => IncompleteSingleDisposalTriageAnswers
  ): Unit =
    s"redirect to ${redirectToPreviousAnswer.url}" when {

      "that question has not already answered" in {

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionDataWithStartingNewDraftReturn(setPreviousAnswer(requiredPreviousAnswers, None))._1)
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
    currentAnswers: SingleDisposalTriageAnswers
  ): Unit = {
    inSequence {
      mockAuthWithNoRetrievals()
      mockGetSession(sessionDataWithStartingNewDraftReturn(currentAnswers)._1)
    }

    val result  = performAction(formData)
    val content = contentAsString(result)

    status(result)          shouldBe BAD_REQUEST
    contentAsString(result) should include(messageFromMessageKey(pageTitleKey))
    content                 should include(messageFromMessageKey(expectedErrorMessageKey))
  }

  def displayIndividualTriagePageBehaviorIncompleteJourney(performAction: () => Future[Result])(
    requiredPreviousAnswers: IncompleteSingleDisposalTriageAnswers,
    answersWithCurrentAnswer: IncompleteSingleDisposalTriageAnswers,
    description: Option[String] = None
  )(
    pageTitleKey: String,
    checkContent: Document => Unit,
    checkPrepopulatedContent: Document => Unit
  ): Unit = {
    val scenarioDescription = description.map(_ + " and when ").getOrElse("")

    s"display the page when ${scenarioDescription}no option has been selected before for ${userType(answersWithCurrentAnswer)}" in {
      List(
        sessionDataWithStartingNewDraftReturn(requiredPreviousAnswers)._1,
        sessionDataWithFillingOutReturn(requiredPreviousAnswers)._1
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
        sessionDataWithStartingNewDraftReturn(answersWithCurrentAnswer)._1,
        sessionDataWithFillingOutReturn(answersWithCurrentAnswer)._1
      ).foreach { currentSession =>
        withClue(s"For currentSession $currentSession: ") {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(currentSession)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(pageTitleKey), { document =>
              checkContent(document)
              checkPrepopulatedContent(document)
            }
          )
        }
      }
    }

  }

  private def userType(answers: SingleDisposalTriageAnswers) = answers.representativeType() match {
    case Some(Left(PersonalRepresentative)) => "personal representative"
    case Some(Right(Capacitor))             => "capacitor"
    case None                               => "self"
  }

  def displayIndividualTriagePageBehaviorCompleteJourney(
    performAction: () => Future[Result]
  )(answers: CompleteSingleDisposalTriageAnswers)(
    pageTitleKey: String,
    checkContent: Document => Unit
  ): Unit =
    s"display the page when the journey has already been completed for ${userType(answers)}" in {
      List(
        sessionDataWithStartingNewDraftReturn(answers)._1,
        sessionDataWithFillingOutReturn(answers)._1
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

  def displayCustomContentForAgentWithSelfAndPersonalRepType(
    performAction: () => Future[Result]
  )(answers: IncompleteSingleDisposalTriageAnswers)(
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
    updateDraftReturn: (DraftSingleDisposalReturn, SingleDisposalTriageAnswers) => DraftSingleDisposalReturn,
    extraMockActions: () => Unit = () => ()
  ): Unit =
    "show an error page" when {

      "the user is starting a new draft return and" when {

        "there is an error updating the session" in {
          val (session, journey) = sessionDataWithStartingNewDraftReturn(currentAnswers)
          val updatedJourney     = journey.copy(newReturnTriageAnswers = Right(updatedAnswers))
          val updatedSession     = session.copy(journeyStatus = Some(updatedJourney))

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
        val draftReturn             = sample[DraftSingleDisposalReturn].copy(triageAnswers = currentAnswers)
        val updatedDraftReturn      = updateDraftReturn(draftReturn, updatedAnswers)
        val fillingOutReturn        = sample[FillingOutReturn].copy(draftReturn = draftReturn)
        val updatedFillingOutReturn = fillingOutReturn.copy(draftReturn = updatedDraftReturn)

        "there is an error updating the draft return" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData.empty.copy(journeyStatus = Some(fillingOutReturn)))
            extraMockActions()
            mockStoreDraftReturn(
              updatedDraftReturn,
              fillingOutReturn.subscribedDetails.cgtReference,
              fillingOutReturn.agentReferenceNumber
            )(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction(formData))
        }

        "there is an error updating the session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData.empty.copy(journeyStatus = Some(fillingOutReturn)))
            extraMockActions()
            mockStoreDraftReturn(
              updatedDraftReturn,
              fillingOutReturn.subscribedDetails.cgtReference,
              fillingOutReturn.agentReferenceNumber
            )(Right(()))
            mockStoreSession(SessionData.empty.copy(journeyStatus = Some(updatedFillingOutReturn)))(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction(formData))
        }

      }

    }

  def testSuccessfulUpdateStartingNewDraft[A](
    performAction: Seq[(String, String)] => Future[Result],
    currentAnswers: SingleDisposalTriageAnswers,
    formData: Seq[(String, String)],
    updatedAnswers: SingleDisposalTriageAnswers,
    checkNextResult: Future[Result] => Unit,
    extraMockActions: () => Unit = () => ()
  ): Unit = {
    val (session, journey) = sessionDataWithStartingNewDraftReturn(currentAnswers)
    val updatedJourney     = journey.copy(newReturnTriageAnswers = Right(updatedAnswers))
    val updatedSession     = session.copy(journeyStatus = Some(updatedJourney))

    inSequence {
      mockAuthWithNoRetrievals()
      mockGetSession(session)
      extraMockActions()
      mockStoreSession(updatedSession)(Right(()))
    }

    checkNextResult(performAction(formData))
  }

  def testSuccessfulUpdateFillingOutReturn[A](
    performAction: Seq[(String, String)] => Future[Result],
    currentAnswers: SingleDisposalTriageAnswers,
    formData: Seq[(String, String)],
    updateDraftReturn: DraftSingleDisposalReturn => DraftSingleDisposalReturn,
    checkNextResult: Future[Result] => Unit,
    extraMockActions: () => Unit = () => ()
  ): Unit = {
    val draftReturn        = sample[DraftSingleDisposalReturn].copy(triageAnswers = currentAnswers)
    val updatedDraftReturn = updateDraftReturn(draftReturn)

    val fillingOutReturn        = sample[FillingOutReturn].copy(draftReturn = draftReturn)
    val updatedFillingOutReturn = fillingOutReturn.copy(draftReturn         = updatedDraftReturn)

    inSequence {
      mockAuthWithNoRetrievals()
      mockGetSession(SessionData.empty.copy(journeyStatus = Some(fillingOutReturn)))
      extraMockActions()
      mockStoreDraftReturn(
        updatedDraftReturn,
        fillingOutReturn.subscribedDetails.cgtReference,
        fillingOutReturn.agentReferenceNumber
      )(Right(()))
      mockStoreSession(SessionData.empty.copy(journeyStatus = Some(updatedFillingOutReturn)))(Right(()))
    }

    checkNextResult(performAction(formData))
  }

}

object SingleDisposalsTriageControllerSpec extends Matchers {
  def validateSingleDisposalTriageCheckYourAnswersPage(
    completeSingleDisposalTriageAnswers: CompleteSingleDisposalTriageAnswers,
    userType: Option[UserType],
    doc: Document
  )(implicit messages: MessagesApi, lang: Lang): Unit = {
    completeSingleDisposalTriageAnswers.individualUserType.foreach { individualUserType =>
      doc.select("#individualUserType-answer").text() shouldBe messages(
        if (userType.contains(UserType.Agent)) s"individualUserType.agent.$individualUserType"
        else s"individualUserType.$individualUserType"
      )
    }

    doc.select("#numberOfProperties-answer").text() shouldBe "One"
    doc.select("#disposalMethod-answer").text() shouldBe messages(
      s"disposalMethod.${completeSingleDisposalTriageAnswers.disposalMethod}"
    )
    if (completeSingleDisposalTriageAnswers.countryOfResidence.isUk())
      doc.select("#wereYouAUKResident-answer").text() shouldBe "Yes"
    else
      doc.select("#wereYouAUKResident-answer").text() shouldBe "No"

    if (completeSingleDisposalTriageAnswers.countryOfResidence.isUk())
      completeSingleDisposalTriageAnswers.assetType match {
        case Residential      => doc.select("#propertyType-answer").text() shouldBe "Yes"
        case NonResidential   => doc.select("#propertyType-answer").text() shouldBe "No"
        case IndirectDisposal => doc.select("#propertyType-answer").text() shouldBe ""
        case MixedUse         => doc.select("#propertyType-answer").text() shouldBe ""
      }
  }
}

class DisabledIndirectDisposalSingleDisposalsTriageControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport {

  override val overrideBindings =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore)
    )

  override lazy val additionalConfig = Configuration(
    "indirect-disposals.enabled" -> false
  )

  lazy val controller = instanceOf[SingleDisposalsTriageController]

  "SingleDisposalsTriageController" when {

    "indirect disposals are disabled" must {

      "redirect to the exit page when a non-uk resident user selects indirect disposals" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            SessionData.empty.copy(
              journeyStatus = Some(
                sample[StartingNewDraftReturn].copy(
                  newReturnTriageAnswers = Right(
                    IncompleteSingleDisposalTriageAnswers(
                      hasConfirmedSingleDisposal = true,
                      individualUserType         = Some(Self),
                      disposalMethod             = Some(sample[DisposalMethod]),
                      wasAUKResident             = Some(false),
                      countryOfResidence         = Some(sample[Country]),
                      assetType                  = Some(AssetType.IndirectDisposal),
                      completionDate             = None,
                      disposalDate               = None,
                      tooEarlyDisposalDate       = None
                    )
                  )
                )
              )
            )
          )
        }

        checkIsRedirect(
          controller.checkYourAnswers()(FakeRequest()),
          routes.CommonTriageQuestionsController.assetTypeNotYetImplemented()
        )
      }

    }

  }

}
