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

import java.time.format.DateTimeFormatter
import java.time.{Clock, LocalDate}
import java.util.UUID

import cats.data.EitherT
import cats.instances.future._
import cats.syntax.order._
import com.typesafe.config.ConfigFactory
import org.jsoup.nodes.Document
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.Configuration
import play.api.i18n.MessagesApi
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.{Call, Result}
import play.api.test.FakeRequest
import play.api.test.Helpers.{contentAsString, _}
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.DateErrorScenarios.{DateErrorScenario, dateErrorScenarios}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.{routes => returnsRoutes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators.{sample, _}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, StartingNewDraftReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.LocalDateUtils.order
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Country
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.UUIDGenerator
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.AssetType.{NonResidential, Residential}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.TriageAnswers.{CompleteTriageAnswers, IncompleteTriageAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{TriageAnswers, _}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, JourneyStatus, SessionData, TaxYear}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class CanTheyUseOurServiceControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with ScalaCheckDrivenPropertyChecks
    with RedirectToStartBehaviour {

  val mockReturnsService = mock[ReturnsService]

  val mockUUIDGenerator = mock[UUIDGenerator]

  override val overrideBindings =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore),
      bind[ReturnsService].toInstance(mockReturnsService),
      bind[UUIDGenerator].toInstance(mockUUIDGenerator)
    )

  val today = LocalDate.now(Clock.systemUTC())

  val earliestDisposalDate = today.minusDays(10L)

  val taxYear = {
    val startYear =
      if (earliestDisposalDate > LocalDate.of(earliestDisposalDate.getYear, 4, 6))
        earliestDisposalDate.getYear
      else
        earliestDisposalDate.getYear - 1

    sample[TaxYear].copy(
      startDateInclusive = LocalDate.of(startYear, 4, 6),
      endDateExclusive   = LocalDate.of(startYear + 1, 4, 6)
    )
  }

  override lazy val additionalConfig: Configuration = Configuration(
    ConfigFactory.parseString(
      s"""
        | returns.earliest-disposal-date-inclusive = ${earliestDisposalDate.format(DateTimeFormatter.ISO_DATE)}
        | tax-years = [
        |  {
        |    start-year = ${taxYear.startDateInclusive.getYear}
        |    annual-exempt-amount {
        |      general              = ${taxYear.annualExemptAmountGeneral.inPounds()}
        |      non-vulnerable-trust = ${taxYear.annualExemptAmountNonVulnerableTrust.inPounds()}
        |    }
        |    personal-allowance = ${taxYear.personalAllowance.inPounds()}
        |    income-tax-higher-rate-threshold = ${taxYear.incomeTaxHigherRateThreshold.inPounds()}
        |    cgt-rates {
        |      lower-band-residential      = ${taxYear.cgtRateLowerBandResidential}
        |      lower-band-non-residential  = ${taxYear.cgtRateLowerBandNonResidential}
        |      higher-band-residential     = ${taxYear.cgtRateHigherBandResidential}
        |      higher-band-non-residential = ${taxYear.cgtRateHigherBandNonResidential}
        |    }
        |  }
        | ]
        |""".stripMargin
    )
  )

  lazy val controller = instanceOf[CanTheyUseOurServiceController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  def isValidJourney(journeyStatus: JourneyStatus): Boolean = journeyStatus match {
    case _: StartingNewDraftReturn | _: FillingOutReturn => true
    case _                                               => false
  }

  val startingNewDraftReturn = sample[StartingNewDraftReturn]

  val fillingOutReturn = sample[FillingOutReturn]
    .copy(
      subscribedDetails = startingNewDraftReturn.subscribedDetails,
      startingNewDraftReturn.ggCredId,
      startingNewDraftReturn.agentReferenceNumber
    )

  val draftReturn = sample[DraftReturn]

  def sessionDataWithStartingNewDraftReturn(individualTriageAnswers: TriageAnswers): SessionData =
    SessionData.empty
      .copy(journeyStatus = Some(startingNewDraftReturn.copy(newReturnTriageAnswers = individualTriageAnswers)))

  def sessionDataWithFillingOurReturn(draftReturn: DraftReturn): SessionData =
    SessionData.empty.copy(journeyStatus = Some(fillingOutReturn.copy(draftReturn = draftReturn)))

  def sessionDataWithFillingOurReturn(individualTriageAnswers: TriageAnswers): SessionData =
    sessionDataWithFillingOurReturn(draftReturn.copy(triageAnswers = individualTriageAnswers))

  def mockGetNextUUID(uuid: UUID) =
    (mockUUIDGenerator.nextId _).expects().returning(uuid)

  def mockStoreDraftReturn(draftReturn: DraftReturn)(result: Either[Error, Unit]) =
    (mockReturnsService
      .storeDraftReturn(_: DraftReturn)(_: HeaderCarrier))
      .expects(draftReturn, *)
      .returning(EitherT.fromEither[Future](result))

  "The CanTheyUseOurServiceController" when {

    "handling requests to display the who is the individual representing page" must {

      def performAction(): Future[Result] = controller.whoIsIndividualRepresenting()(FakeRequest())

      val requiredPreviousAnswers = IncompleteTriageAnswers.empty

      behave like redirectToStartWhenInvalidJourney(performAction, isValidJourney)

      behave like displayIndividualTriagePageBehaviorIncompleteJourney(
        performAction
      )(requiredPreviousAnswers, requiredPreviousAnswers.copy(individualUserType = Some(IndividualUserType.Self)))(
        "who-are-you-reporting-for.title", {
          _.select("#content > article > form")
            .attr("action") shouldBe routes.CanTheyUseOurServiceController
            .whoIsIndividualRepresentingSubmit()
            .url
        }, {
          _.select("#individualUserType-0").attr("checked") shouldBe "checked"
        }
      )

      behave like displayIndividualTriagePageBehaviorCompleteJourney(
        performAction
      )(sample[CompleteTriageAnswers].copy(individualUserType = IndividualUserType.Self))(
        "who-are-you-reporting-for.title", { doc =>
          doc
            .select("#content > article > form")
            .attr("action") shouldBe routes.CanTheyUseOurServiceController
            .whoIsIndividualRepresentingSubmit()
            .url
          doc.select("#individualUserType-0").attr("checked") shouldBe "checked"
        }
      )

    }

    "handling submitted answers to the who is the individual representing page" must {

      def performAction(formData: (String, String)*): Future[Result] =
        controller.whoIsIndividualRepresentingSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*))

      val requiredPreviousAnswers = IncompleteTriageAnswers.empty

      behave like redirectToStartWhenInvalidJourney(() => performAction(), isValidJourney)

      "show a form error" when {

        def test(formData: Seq[(String, String)], expectedErrorKey: String) =
          testFormError(performAction, "who-are-you-reporting-for.title")(
            formData,
            expectedErrorKey,
            requiredPreviousAnswers
          )

        "nothing is submitted" in {
          test(List.empty, "individualUserType.error.required")
        }

        "the option is not recognised" in {
          test(List("individualUserType" -> "3"), "individualUserType.error.invalid")
        }

      }

      behave like unsuccessfulUpdatesStartingNewDraftBehaviour(
        performAction,
        IncompleteTriageAnswers.empty,
        List("individualUserType" -> "0"),
        IncompleteTriageAnswers.empty.copy(individualUserType = Some(IndividualUserType.Self))
      )

      "handle successful updates" when {

        "the user is starting a new draft return and" when {

          "the user has not answered any questions before" in {
            testSuccessfulUpdateStartingNewDraft(
              performAction,
              IncompleteTriageAnswers.empty,
              List("individualUserType" -> "0"),
              IncompleteTriageAnswers.empty.copy(individualUserType = Some(IndividualUserType.Self)),
              checkIsRedirect(_, routes.CanTheyUseOurServiceController.checkYourAnswers())
            )
          }

          "the user has answered some questions but not complete the section" in {
            testSuccessfulUpdateStartingNewDraft(
              performAction,
              IncompleteTriageAnswers.empty.copy(individualUserType = Some(IndividualUserType.Self)),
              List("individualUserType" -> "1"),
              IncompleteTriageAnswers.empty.copy(individualUserType = Some(IndividualUserType.Capacitor)), { result =>
                status(result)          shouldBe OK
                contentAsString(result) shouldBe s"${IndividualUserType.Capacitor} not handled yet"
              }
            )
          }

          "the user has complete the section" in {
            val completeAnswers = sample[CompleteTriageAnswers]
            testSuccessfulUpdateStartingNewDraft(
              performAction,
              completeAnswers.copy(individualUserType = IndividualUserType.Capacitor),
              List("individualUserType" -> "0"),
              completeAnswers.copy(individualUserType = IndividualUserType.Self),
              checkIsRedirect(_, routes.CanTheyUseOurServiceController.checkYourAnswers())
            )
          }

        }

        "the user is filling out a draft return and" when {

          "the section is complete" in {
            val completeAnswers = sample[CompleteTriageAnswers]
            behave like testSuccessfulUpdateFillingOutReturn(
              performAction,
              completeAnswers.copy(individualUserType = IndividualUserType.Capacitor),
              List("individualUserType" -> "0"),
              completeAnswers.copy(individualUserType = IndividualUserType.Self),
              checkIsRedirect(_, routes.CanTheyUseOurServiceController.checkYourAnswers())
            )

          }

          "the section is incomplete" in {
            behave like testSuccessfulUpdateFillingOutReturn(
              performAction,
              IncompleteTriageAnswers.empty,
              List("individualUserType" -> "2"),
              IncompleteTriageAnswers.empty
                .copy(individualUserType = Some(IndividualUserType.PersonalRepresentative)), { result =>
                status(result)          shouldBe OK
                contentAsString(result) shouldBe s"${IndividualUserType.PersonalRepresentative} not handled yet"
              }
            )
          }
        }
      }

      "not do any updates" when {

        "the answers submitted is the same as the one in session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithFillingOurReturn(
                requiredPreviousAnswers.copy(
                  individualUserType = Some(IndividualUserType.Self)
                )
              )
            )
          }

          checkIsRedirect(
            performAction("individualUserType" -> "0"),
            routes.CanTheyUseOurServiceController.checkYourAnswers()
          )
        }

      }

    }

    "handling requests to display the how many properties page" must {

      val requiredPreviousAnswers =
        IncompleteTriageAnswers.empty.copy(individualUserType = Some(sample[IndividualUserType]))

      def performAction(): Future[Result] = controller.howManyProperties()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(performAction, isValidJourney)

      behave like redirectWhenNoPreviousAnswerBehaviour[IndividualUserType](
        performAction
      )(
        requiredPreviousAnswers,
        routes.CanTheyUseOurServiceController.whoIsIndividualRepresenting(),
        { case (answers, t) => answers.copy(individualUserType = t) }
      )

      behave like displayIndividualTriagePageBehaviorIncompleteJourney(
        performAction
      )(
        requiredPreviousAnswers,
        requiredPreviousAnswers.copy(numberOfProperties = Some(NumberOfProperties.MoreThanOne))
      )(
        "numberOfProperties.title",
        checkContent(_, routes.CanTheyUseOurServiceController.whoIsIndividualRepresenting()),
        _.select("#numberOfProperties-1").attr("checked") shouldBe "checked"
      )

      behave like displayIndividualTriagePageBehaviorCompleteJourney(
        performAction
      )(sample[CompleteTriageAnswers].copy(numberOfProperties = NumberOfProperties.One))(
        "numberOfProperties.title", { doc =>
          checkContent(doc, routes.CanTheyUseOurServiceController.checkYourAnswers())
          doc.select("#numberOfProperties-0").attr("checked") shouldBe "checked"
        }
      )

      def checkContent(doc: Document, backLink: Call): Unit = {
        doc.select("#back").attr("href") shouldBe backLink.url
        doc
          .select("#content > article > form")
          .attr("action") shouldBe routes.CanTheyUseOurServiceController
          .howManyPropertiesSubmit()
          .url
      }

    }

    "handling submitted answers to the how many properties page" must {

      def performAction(formData: (String, String)*): Future[Result] =
        controller.howManyPropertiesSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*))

      val requiredPreviousAnswers =
        IncompleteTriageAnswers.empty.copy(individualUserType = Some(sample[IndividualUserType]))

      behave like redirectToStartWhenInvalidJourney(() => performAction(), isValidJourney)

      behave like redirectWhenNoPreviousAnswerBehaviour[IndividualUserType](() => performAction())(
        requiredPreviousAnswers,
        routes.CanTheyUseOurServiceController.whoIsIndividualRepresenting(),
        { case (answers, t) => answers.copy(individualUserType = t) }
      )

      "show a form error" when {

        def test(formData: Seq[(String, String)], expectedErrorKey: String) =
          testFormError(performAction, "numberOfProperties.title")(formData, expectedErrorKey, requiredPreviousAnswers)

        "nothing is submitted" in {
          test(List.empty, "numberOfProperties.error.required")
        }

        "the option is not recognised" in {
          test(List("numberOfProperties" -> "3"), "numberOfProperties.error.invalid")
        }

      }

      behave like unsuccessfulUpdatesStartingNewDraftBehaviour(
        performAction,
        requiredPreviousAnswers,
        List("numberOfProperties" -> "0"),
        requiredPreviousAnswers.copy(numberOfProperties = Some(NumberOfProperties.One))
      )

      "handle successful updates" when {

        "the user is starting a new draft return and" when {

          "the user has answered some questions but not complete the section" in {
            val answers = IncompleteTriageAnswers.empty.copy(individualUserType = Some(IndividualUserType.Self))

            testSuccessfulUpdateStartingNewDraft(
              performAction,
              answers,
              List("numberOfProperties" -> "0"),
              answers.copy(numberOfProperties = Some(NumberOfProperties.One)),
              checkIsRedirect(_, routes.CanTheyUseOurServiceController.checkYourAnswers())
            )
          }

          "the user has complete the section" in {
            val completeAnswers = sample[CompleteTriageAnswers]
            testSuccessfulUpdateStartingNewDraft(
              performAction,
              completeAnswers.copy(numberOfProperties = NumberOfProperties.One),
              List("numberOfProperties" -> "1"),
              completeAnswers.copy(numberOfProperties = NumberOfProperties.MoreThanOne), { result =>
                status(result)          shouldBe OK
                contentAsString(result) shouldBe "multiple disposals not handled yet"

              }
            )
          }

        }

        "the user is filling out a draft return and" when {

          "the section is complete" in {
            val completeAnswers = sample[CompleteTriageAnswers]
            testSuccessfulUpdateFillingOutReturn(
              performAction,
              completeAnswers.copy(numberOfProperties = NumberOfProperties.One),
              List("numberOfProperties" -> "1"),
              completeAnswers.copy(numberOfProperties = NumberOfProperties.MoreThanOne), { result =>
                status(result)          shouldBe OK
                contentAsString(result) shouldBe "multiple disposals not handled yet"

              }
            )

          }

          "the section is incomplete" in {
            val answers = IncompleteTriageAnswers.empty.copy(individualUserType = Some(IndividualUserType.Self))

            testSuccessfulUpdateFillingOutReturn(
              performAction,
              answers,
              List("numberOfProperties" -> "0"),
              answers.copy(numberOfProperties = Some(NumberOfProperties.One)),
              checkIsRedirect(_, routes.CanTheyUseOurServiceController.checkYourAnswers())
            )
          }
        }
      }

      "not do any updates" when {

        "the answers submitted is the same as the one in session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithFillingOurReturn(
                requiredPreviousAnswers.copy(
                  numberOfProperties = Some(NumberOfProperties.One)
                )
              )
            )
          }

          checkIsRedirect(
            performAction("numberOfProperties" -> "0"),
            routes.CanTheyUseOurServiceController.checkYourAnswers()
          )
        }

      }

    }

    "handling requests to display how did you dispose of your property page" must {

      val requiredPreviousAnswers =
        IncompleteTriageAnswers.empty.copy(numberOfProperties = Some(NumberOfProperties.One))

      def performAction(): Future[Result] = controller.howDidYouDisposeOfProperty()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(performAction, isValidJourney)

      behave like redirectWhenNoPreviousAnswerBehaviour[NumberOfProperties](
        performAction
      )(
        requiredPreviousAnswers,
        routes.CanTheyUseOurServiceController.howManyProperties(),
        { case (answers, n) => answers.copy(numberOfProperties = n) }
      )

      behave like displayIndividualTriagePageBehaviorIncompleteJourney(
        performAction
      )(requiredPreviousAnswers, requiredPreviousAnswers.copy(disposalMethod = Some(DisposalMethod.Sold)))(
        "disposalMethod.title",
        checkContent(_, routes.CanTheyUseOurServiceController.howManyProperties()),
        _.select("#disposalMethod-0").attr("checked") shouldBe "checked"
      )

      behave like displayIndividualTriagePageBehaviorCompleteJourney(
        performAction
      )(sample[CompleteTriageAnswers].copy(disposalMethod = DisposalMethod.Gifted))(
        "disposalMethod.title", { doc =>
          checkContent(doc, routes.CanTheyUseOurServiceController.checkYourAnswers())
          doc.select("#disposalMethod-1").attr("checked") shouldBe "checked"
        }
      )

      def checkContent(doc: Document, backLink: Call): Unit = {
        doc.select("#back").attr("href") shouldBe backLink.url
        doc
          .select("#content > article > form")
          .attr("action") shouldBe routes.CanTheyUseOurServiceController
          .howDidYouDisposeOfPropertySubmit()
          .url
      }

    }

    "handling submitted answers to the how did you dispose of your property page" must {

      def performAction(formData: (String, String)*): Future[Result] =
        controller.howDidYouDisposeOfPropertySubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*))

      val requiredPreviousAnswers =
        IncompleteTriageAnswers.empty.copy(numberOfProperties = Some(NumberOfProperties.One))

      behave like redirectToStartWhenInvalidJourney(() => performAction(), isValidJourney)

      behave like redirectWhenNoPreviousAnswerBehaviour[NumberOfProperties](() => performAction())(
        requiredPreviousAnswers,
        routes.CanTheyUseOurServiceController.howManyProperties(),
        { case (answers, n) => answers.copy(numberOfProperties = n) }
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

      behave like unsuccessfulUpdatesStartingNewDraftBehaviour(
        performAction,
        requiredPreviousAnswers,
        List("disposalMethod" -> "0"),
        requiredPreviousAnswers.copy(disposalMethod = Some(DisposalMethod.Sold))
      )

      "handle successful updates" when {

        "the user is starting a new draft return and" when {

          "the user has answered some questions but not complete the section" in {
            testSuccessfulUpdateStartingNewDraft(
              performAction,
              requiredPreviousAnswers,
              List("disposalMethod" -> "0"),
              requiredPreviousAnswers.copy(disposalMethod = Some(DisposalMethod.Sold)),
              checkIsRedirect(_, routes.CanTheyUseOurServiceController.checkYourAnswers())
            )
          }

          "the user has complete the section" in {
            val completeAnswers = sample[CompleteTriageAnswers]
            testSuccessfulUpdateStartingNewDraft(
              performAction,
              completeAnswers.copy(disposalMethod = DisposalMethod.Sold),
              List("disposalMethod" -> "1"),
              completeAnswers.copy(disposalMethod = DisposalMethod.Gifted),
              checkIsRedirect(_, routes.CanTheyUseOurServiceController.checkYourAnswers())
            )
          }

        }

        "the user is filling out a draft return and" when {

          "the section is complete" in {
            val completeAnswers = sample[CompleteTriageAnswers]
            testSuccessfulUpdateFillingOutReturn(
              performAction,
              completeAnswers.copy(disposalMethod = DisposalMethod.Sold),
              List("disposalMethod" -> "1"),
              completeAnswers.copy(disposalMethod = DisposalMethod.Gifted),
              checkIsRedirect(_, routes.CanTheyUseOurServiceController.checkYourAnswers())
            )
          }

          "the section is incomplete" in {
            testSuccessfulUpdateFillingOutReturn(
              performAction,
              requiredPreviousAnswers,
              List("disposalMethod" -> "0"),
              requiredPreviousAnswers.copy(disposalMethod = Some(DisposalMethod.Sold)),
              checkIsRedirect(_, routes.CanTheyUseOurServiceController.checkYourAnswers())
            )
          }
        }
      }

      "not do any updates" when {

        "the answers submitted is the same as the one in session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithFillingOurReturn(
                requiredPreviousAnswers.copy(
                  disposalMethod = Some(DisposalMethod.Sold)
                )
              )
            )
          }

          checkIsRedirect(
            performAction("disposalMethod" -> "0"),
            routes.CanTheyUseOurServiceController.checkYourAnswers()
          )
        }

      }

    }

    "handling requests to display the were you a uk resident page" must {

      val requiredPreviousAnswers =
        IncompleteTriageAnswers.empty.copy(disposalMethod = Some(DisposalMethod.Sold))

      def performAction(): Future[Result] = controller.wereYouAUKResident()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(performAction, isValidJourney)

      behave like redirectWhenNoPreviousAnswerBehaviour[DisposalMethod](
        performAction
      )(
        requiredPreviousAnswers,
        routes.CanTheyUseOurServiceController.howDidYouDisposeOfProperty(),
        { case (answers, m) => answers.copy(disposalMethod = m) }
      )

      behave like displayIndividualTriagePageBehaviorIncompleteJourney(
        performAction
      )(requiredPreviousAnswers, requiredPreviousAnswers.copy(wasAUKResident = Some(false)))(
        "wereYouAUKResident.title",
        checkContent(_, routes.CanTheyUseOurServiceController.howDidYouDisposeOfProperty()),
        _.select("#wereYouAUKResident-false").attr("checked") shouldBe "checked"
      )

      behave like displayIndividualTriagePageBehaviorCompleteJourney(
        performAction
      )(
        sample[CompleteTriageAnswers].copy(countryOfResidence = Country.uk)
      )(
        "wereYouAUKResident.title", { doc =>
          checkContent(doc, routes.CanTheyUseOurServiceController.checkYourAnswers())
          doc.select("#wereYouAUKResident-true").attr("checked") shouldBe "checked"
        }
      )

      def checkContent(doc: Document, backLink: Call): Unit = {
        doc.select("#back").attr("href") shouldBe backLink.url
        doc
          .select("#content > article > form")
          .attr("action") shouldBe routes.CanTheyUseOurServiceController
          .wereYouAUKResidentSubmit()
          .url
      }

    }

    "handling submitted answers to the were you a uk resident page" must {

      def performAction(formData: (String, String)*): Future[Result] =
        controller.wereYouAUKResidentSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*))

      val requiredPreviousAnswers =
        IncompleteTriageAnswers.empty.copy(disposalMethod = Some(DisposalMethod.Sold))

      behave like redirectToStartWhenInvalidJourney(() => performAction(), isValidJourney)

      behave like redirectWhenNoPreviousAnswerBehaviour[DisposalMethod](() => performAction())(
        requiredPreviousAnswers,
        routes.CanTheyUseOurServiceController.howDidYouDisposeOfProperty(),
        { case (answers, m) => answers.copy(disposalMethod = m) }
      )

      "show a form error" when {

        def test(formData: Seq[(String, String)], expectedErrorKey: String) =
          testFormError(performAction, "wereYouAUKResident.title")(formData, expectedErrorKey, requiredPreviousAnswers)

        "nothing is submitted" in {
          test(List.empty, "wereYouAUKResident.error.required")
        }

        "the option is not recognised" in {
          test(List("wereYouAUKResident" -> "3"), "wereYouAUKResident.error.boolean")
        }
      }

      behave like unsuccessfulUpdatesStartingNewDraftBehaviour(
        performAction,
        requiredPreviousAnswers,
        List("wereYouAUKResident" -> "true"),
        requiredPreviousAnswers.copy(wasAUKResident = Some(true))
      )

      "handle successful updates" when {

        "the user is starting a new draft return and" when {

          "the user has answered some questions but not complete the section and has changed their answer from not in the uk to was in the uk" in {
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
              checkIsRedirect(_, routes.CanTheyUseOurServiceController.checkYourAnswers())
            )
          }

          "the user has answered some questions but not complete the section and has changed their answer from was in the uk to not in the uk" in {
            val answers = requiredPreviousAnswers
              .copy(wasAUKResident = Some(true), countryOfResidence = None, assetType = Some(AssetType.Residential))

            testSuccessfulUpdateStartingNewDraft(
              performAction,
              answers,
              List("wereYouAUKResident" -> "false"),
              requiredPreviousAnswers.copy(wasAUKResident = Some(false), countryOfResidence = None, assetType = None),
              checkIsRedirect(_, routes.CanTheyUseOurServiceController.checkYourAnswers())
            )
          }

          "the user has complete the section and has changed their answer from not in the uk to was in the uk" in {
            val completeAnswers = sample[CompleteTriageAnswers]
            testSuccessfulUpdateStartingNewDraft(
              performAction,
              completeAnswers.copy(countryOfResidence = Country("AB", None)),
              List("wereYouAUKResident" -> "true"),
              IncompleteTriageAnswers(
                Some(completeAnswers.individualUserType),
                Some(completeAnswers.numberOfProperties),
                Some(completeAnswers.disposalMethod),
                Some(true),
                None,
                None,
                Some(completeAnswers.disposalDate),
                Some(completeAnswers.completionDate)
              ),
              checkIsRedirect(_, routes.CanTheyUseOurServiceController.checkYourAnswers())
            )
          }

          "the user has completed the section and has changed their answer from was in the uk to not in the uk" in {
            val completeAnswers = sample[CompleteTriageAnswers]
            testSuccessfulUpdateStartingNewDraft(
              performAction,
              completeAnswers.copy(countryOfResidence = Country.uk),
              List("wereYouAUKResident" -> "false"),
              IncompleteTriageAnswers(
                Some(completeAnswers.individualUserType),
                Some(completeAnswers.numberOfProperties),
                Some(completeAnswers.disposalMethod),
                Some(false),
                None,
                None,
                Some(completeAnswers.disposalDate),
                Some(completeAnswers.completionDate)
              ),
              checkIsRedirect(_, routes.CanTheyUseOurServiceController.checkYourAnswers())
            )
          }

        }

        "the user is filling out a draft return and" when {

          "the section is complete" in {
            val completeAnswers = sample[CompleteTriageAnswers]
            testSuccessfulUpdateFillingOutReturn(
              performAction,
              completeAnswers.copy(countryOfResidence = Country("AB", None)),
              List("wereYouAUKResident" -> "true"),
              IncompleteTriageAnswers(
                Some(completeAnswers.individualUserType),
                Some(completeAnswers.numberOfProperties),
                Some(completeAnswers.disposalMethod),
                Some(true),
                None,
                None,
                Some(completeAnswers.disposalDate),
                Some(completeAnswers.completionDate)
              ),
              checkIsRedirect(_, routes.CanTheyUseOurServiceController.checkYourAnswers())
            )
          }

          "the section is incomplete" in {
            val answers = requiredPreviousAnswers
              .copy(wasAUKResident = Some(true), countryOfResidence = None, assetType = Some(AssetType.Residential))

            testSuccessfulUpdateFillingOutReturn(
              performAction,
              answers,
              List("wereYouAUKResident" -> "false"),
              requiredPreviousAnswers.copy(wasAUKResident = Some(false), countryOfResidence = None, assetType = None),
              checkIsRedirect(_, routes.CanTheyUseOurServiceController.checkYourAnswers())
            )
          }
        }
      }

      "not do any updates" when {

        "the answers submitted is the same as the one in session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithFillingOurReturn(
                requiredPreviousAnswers.copy(
                  wasAUKResident = Some(true)
                )
              )
            )
          }

          checkIsRedirect(
            performAction("wereYouAUKResident" -> "true"),
            routes.CanTheyUseOurServiceController.checkYourAnswers()
          )
        }

      }

    }

    "handling requests to display the did you dispose of a residential property page" must {

      val requiredPreviousAnswers =
        IncompleteTriageAnswers.empty.copy(wasAUKResident = Some(true))

      def performAction(): Future[Result] = controller.didYouDisposeOfAResidentialProperty()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(performAction, isValidJourney)

      behave like redirectWhenNoPreviousAnswerBehaviour[Boolean](
        performAction
      )(
        requiredPreviousAnswers,
        routes.CanTheyUseOurServiceController.wereYouAUKResident(),
        { case (answers, w) => answers.copy(wasAUKResident = w) }
      )

      behave like displayIndividualTriagePageBehaviorIncompleteJourney(
        performAction
      )(requiredPreviousAnswers, requiredPreviousAnswers.copy(assetType = Some(AssetType.NonResidential)))(
        "didYouDisposeOfResidentialProperty.title",
        checkContent(_, routes.CanTheyUseOurServiceController.wereYouAUKResident()),
        _.select("#didYouDisposeOfResidentialProperty-false").attr("checked") shouldBe "checked"
      )

      {
        val completeAnswers = sample[CompleteTriageAnswers].copy(
          countryOfResidence = Country.uk,
          assetType          = AssetType.Residential
        )
        behave like displayIndividualTriagePageBehaviorCompleteJourney(
          performAction
        )(completeAnswers)(
          "didYouDisposeOfResidentialProperty.title", { doc =>
            checkContent(doc, routes.CanTheyUseOurServiceController.checkYourAnswers())
            doc.select("#didYouDisposeOfResidentialProperty-true").attr("checked") shouldBe "checked"
          }
        )
      }

      def checkContent(doc: Document, backLink: Call): Unit = {
        doc.select("#back").attr("href") shouldBe backLink.url
        doc
          .select("#content > article > form")
          .attr("action") shouldBe routes.CanTheyUseOurServiceController
          .didYouDisposeOfAResidentialPropertySubmit()
          .url

      }

    }

    "handling submitted answers to the did you dispose of a residential property page" must {

      def performAction(formData: (String, String)*): Future[Result] =
        controller.didYouDisposeOfAResidentialPropertySubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*))

      val requiredPreviousAnswers =
        IncompleteTriageAnswers.empty.copy(wasAUKResident = Some(true))

      behave like redirectToStartWhenInvalidJourney(() => performAction(), isValidJourney)

      behave like redirectWhenNoPreviousAnswerBehaviour[Boolean](() => performAction())(
        requiredPreviousAnswers,
        routes.CanTheyUseOurServiceController.wereYouAUKResident(),
        { case (answers, w) => answers.copy(wasAUKResident = w) }
      )

      "show a form error" when {

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

      behave like unsuccessfulUpdatesStartingNewDraftBehaviour(
        performAction,
        requiredPreviousAnswers,
        List("didYouDisposeOfResidentialProperty" -> "true"),
        requiredPreviousAnswers.copy(assetType = Some(AssetType.Residential))
      )

      "handle successful updates" when {

        "the user is starting a new draft return and" when {

          "the user has answered some questions but not complete the section" in {
            testSuccessfulUpdateStartingNewDraft(
              performAction,
              requiredPreviousAnswers,
              List("didYouDisposeOfResidentialProperty" -> "true"),
              requiredPreviousAnswers.copy(assetType = Some(AssetType.Residential)),
              checkIsRedirect(_, routes.CanTheyUseOurServiceController.checkYourAnswers())
            )
          }

          "the user has complete the section and has changed their answer and they choose asset type non-residential" in {
            val completeAnswers = sample[CompleteTriageAnswers]
            testSuccessfulUpdateStartingNewDraft(
              performAction,
              completeAnswers.copy(assetType = Residential),
              List("didYouDisposeOfResidentialProperty" -> "false"),
              completeAnswers.copy(assetType = NonResidential), { result =>
                status(result)          shouldBe OK
                contentAsString(result) shouldBe "individuals can only report on residential properties"
              }
            )
          }

          "the user has complete the section and has changed their answer and they choose asset type residential" in {
            val completeAnswers = sample[CompleteTriageAnswers]
            testSuccessfulUpdateStartingNewDraft(
              performAction,
              completeAnswers.copy(assetType = NonResidential),
              List("didYouDisposeOfResidentialProperty" -> "true"),
              completeAnswers.copy(assetType = Residential),
              checkIsRedirect(_, routes.CanTheyUseOurServiceController.checkYourAnswers())
            )
          }
        }

        "the user is filling out a draft return and" when {

          "the user has answered some questions but not complete the section" in {
            testSuccessfulUpdateFillingOutReturn(
              performAction,
              requiredPreviousAnswers,
              List("didYouDisposeOfResidentialProperty" -> "true"),
              requiredPreviousAnswers.copy(assetType = Some(AssetType.Residential)),
              checkIsRedirect(_, routes.CanTheyUseOurServiceController.checkYourAnswers())
            )
          }

          "the user has complete the section and has changed their answer from not in the uk to was in the uk" in {
            val completeAnswers = sample[CompleteTriageAnswers]
            testSuccessfulUpdateFillingOutReturn(
              performAction,
              completeAnswers.copy(assetType = Residential),
              List("didYouDisposeOfResidentialProperty" -> "false"),
              completeAnswers.copy(assetType = NonResidential), { result =>
                status(result)          shouldBe OK
                contentAsString(result) shouldBe "individuals can only report on residential properties"
              }
            )
          }
        }
      }

      "not do any updates" when {

        "the answers submitted is the same as the one in session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithFillingOurReturn(
                requiredPreviousAnswers.copy(
                  assetType = Some(Residential)
                )
              )
            )
          }

          checkIsRedirect(
            performAction("didYouDisposeOfResidentialProperty" -> "true"),
            routes.CanTheyUseOurServiceController.checkYourAnswers()
          )
        }

      }

    }

    "handling requests to display the when was disposal date page" must {

      val requiredPreviousAnswersUkResident =
        IncompleteTriageAnswers.empty.copy(
          individualUserType = Some(sample[IndividualUserType]),
          numberOfProperties = Some(NumberOfProperties.One),
          disposalMethod     = Some(DisposalMethod.Gifted),
          wasAUKResident     = Some(true),
          assetType          = Some(AssetType.Residential)
        )

      val disposalDate = DisposalDate(LocalDate.of(2020, 1, 2), taxYear)

      def performAction(): Future[Result] = controller.whenWasDisposalDate()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(performAction, isValidJourney)

      behave like redirectWhenNoPreviousAnswerBehaviour[Boolean](
        performAction
      )(
        requiredPreviousAnswersUkResident,
        routes.CanTheyUseOurServiceController.didYouDisposeOfAResidentialProperty(), {
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
        checkContent(_, routes.CanTheyUseOurServiceController.didYouDisposeOfAResidentialProperty()),
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
          checkContent(_, routes.CanTheyUseOurServiceController.assetTypeForNonUkResidents()),
          checkPrepopulatedContent(_, disposalDate)
        )
      }

      behave like displayIndividualTriagePageBehaviorCompleteJourney(
        performAction
      )(
        sample[CompleteTriageAnswers]
          .copy(countryOfResidence = Country.uk, assetType = AssetType.Residential, disposalDate = disposalDate)
      )(
        "disposalDate.title", { doc =>
          checkContent(doc, routes.CanTheyUseOurServiceController.checkYourAnswers())
          checkPrepopulatedContent(doc, disposalDate)
        }
      )

      def checkContent(doc: Document, backLink: Call): Unit = {
        doc.select("#back").attr("href") shouldBe backLink.url
        doc
          .select("#content > article > form")
          .attr("action") shouldBe routes.CanTheyUseOurServiceController
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

      val tomorrow = today.plusDays(1L)

      val requiredPreviousAnswers =
        IncompleteTriageAnswers.empty.copy(
          individualUserType = Some(sample[IndividualUserType]),
          numberOfProperties = Some(NumberOfProperties.One),
          disposalMethod     = Some(DisposalMethod.Gifted),
          wasAUKResident     = Some(true),
          assetType          = Some(AssetType.Residential)
        )

      behave like redirectToStartWhenInvalidJourney(() => performAction(), isValidJourney)

      behave like redirectWhenNoPreviousAnswerBehaviour[Boolean](() => performAction())(
        requiredPreviousAnswers,
        routes.CanTheyUseOurServiceController.didYouDisposeOfAResidentialProperty(), {
          case (answers, w) =>
            answers.copy(assetType = w.map(if (_) AssetType.Residential else AssetType.NonResidential))
        }
      )

      "show a form error" when {

        def test(formData: Seq[(String, String)], expectedErrorKey: String) =
          testFormError(performAction, "disposalDate.title")(formData, expectedErrorKey, requiredPreviousAnswers)

        "the date is invalid" in {
          dateErrorScenarios("disposalDate").foreach { scenario =>
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

      behave like unsuccessfulUpdatesStartingNewDraftBehaviour(
        performAction,
        requiredPreviousAnswers,
        formData(earliestDisposalDate),
        requiredPreviousAnswers.copy(disposalDate = Some(DisposalDate(earliestDisposalDate, taxYear)))
      )

      "handle valid dates" when {

        "the user is starting in a draft return and" when {

          "the disposal date entered is before the configured earliest disposal date" in {
            val date = earliestDisposalDate.minusDays(1L)
            testSuccessfulUpdateStartingNewDraft(
              performAction,
              requiredPreviousAnswers,
              formData(date),
              requiredPreviousAnswers.copy(disposalDate = Some(DisposalDate(date, taxYear))), { result =>
                status(result)          shouldBe OK
                contentAsString(result) shouldBe s"disposal date was strictly before $earliestDisposalDate"
              }
            )
          }

          "the disposal date is on the configured earliest disposal date and the journey was incomplete" in {
            testSuccessfulUpdateStartingNewDraft(
              performAction,
              requiredPreviousAnswers,
              formData(earliestDisposalDate),
              requiredPreviousAnswers.copy(disposalDate = Some(DisposalDate(earliestDisposalDate, taxYear))),
              checkIsRedirect(_, routes.CanTheyUseOurServiceController.checkYourAnswers())
            )
          }

          "the disposal date is after the configured earliest disposal date and the journey was complete" in {
            val completeJourney =
              sample[CompleteTriageAnswers].copy(disposalDate = DisposalDate(earliestDisposalDate, taxYear))
            val date = earliestDisposalDate.plusDays(1L)

            testSuccessfulUpdateStartingNewDraft(
              performAction,
              completeJourney,
              formData(date),
              IncompleteTriageAnswers(
                Some(completeJourney.individualUserType),
                Some(completeJourney.numberOfProperties),
                Some(completeJourney.disposalMethod),
                Some(completeJourney.countryOfResidence.isUk()),
                if (completeJourney.countryOfResidence.isUk()) None else Some(completeJourney.countryOfResidence),
                Some(completeJourney.assetType),
                Some(DisposalDate(date, taxYear)),
                None
              ),
              checkIsRedirect(_, routes.CanTheyUseOurServiceController.checkYourAnswers())
            )

          }

        }

        "the user is filling our a draft return and" when {

          "the section is incomplete" in {
            testSuccessfulUpdateFillingOutReturn(
              performAction,
              requiredPreviousAnswers,
              formData(earliestDisposalDate),
              requiredPreviousAnswers.copy(disposalDate = Some(DisposalDate(earliestDisposalDate, taxYear))),
              checkIsRedirect(_, routes.CanTheyUseOurServiceController.checkYourAnswers())
            )

          }

          "the section is complete" in {
            val completeJourney =
              sample[CompleteTriageAnswers].copy(disposalDate = DisposalDate(earliestDisposalDate, taxYear))
            val date = earliestDisposalDate.plusDays(1L)

            testSuccessfulUpdateFillingOutReturn(
              performAction,
              completeJourney,
              formData(date),
              IncompleteTriageAnswers(
                Some(completeJourney.individualUserType),
                Some(completeJourney.numberOfProperties),
                Some(completeJourney.disposalMethod),
                Some(completeJourney.countryOfResidence.isUk()),
                if (completeJourney.countryOfResidence.isUk()) None else Some(completeJourney.countryOfResidence),
                Some(completeJourney.assetType),
                Some(DisposalDate(date, taxYear)),
                None
              ),
              checkIsRedirect(_, routes.CanTheyUseOurServiceController.checkYourAnswers())
            )
          }
        }

      }

      "not do any updates" when {

        "the answers submitted is the same as the one in session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithFillingOurReturn(
                requiredPreviousAnswers.copy(
                  disposalDate = Some(DisposalDate(earliestDisposalDate, taxYear))
                )
              )
            )
          }

          checkIsRedirect(
            performAction(formData(earliestDisposalDate): _*),
            routes.CanTheyUseOurServiceController.checkYourAnswers()
          )
        }

      }

    }

    "handling requests to display the when was completion date page" must {

      val disposalDate = DisposalDate(today, taxYear)

      val requiredPreviousAnswers =
        IncompleteTriageAnswers.empty.copy(
          individualUserType = Some(sample[IndividualUserType]),
          numberOfProperties = Some(NumberOfProperties.One),
          disposalMethod     = Some(DisposalMethod.Gifted),
          wasAUKResident     = Some(true),
          assetType          = Some(AssetType.Residential),
          disposalDate       = Some(disposalDate)
        )

      def performAction(): Future[Result] = controller.whenWasCompletionDate()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(performAction, isValidJourney)

      behave like redirectWhenNoPreviousAnswerBehaviour[DisposalDate](
        performAction
      )(
        requiredPreviousAnswers,
        routes.CanTheyUseOurServiceController.whenWasDisposalDate(),
        { case (answers, d) => answers.copy(disposalDate = d) }
      )

      behave like displayIndividualTriagePageBehaviorIncompleteJourney(
        performAction
      )(
        requiredPreviousAnswers,
        requiredPreviousAnswers.copy(completionDate = Some(CompletionDate(disposalDate.value)))
      )(
        "completionDate.title",
        checkContent(_, routes.CanTheyUseOurServiceController.whenWasDisposalDate()),
        checkPrepopulatedContent(_, disposalDate.value)
      )

      behave like displayIndividualTriagePageBehaviorCompleteJourney(
        performAction
      )(
        sample[CompleteTriageAnswers].copy(
          assetType      = AssetType.Residential,
          completionDate = CompletionDate(disposalDate.value)
        )
      )(
        "completionDate.title", { doc =>
          checkContent(doc, routes.CanTheyUseOurServiceController.checkYourAnswers())
          checkPrepopulatedContent(doc, disposalDate.value)
        }
      )

      def checkContent(doc: Document, backLink: Call): Unit = {
        doc.select("#back").attr("href") shouldBe backLink.url
        doc
          .select("#content > article > form")
          .attr("action") shouldBe routes.CanTheyUseOurServiceController
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

      val disposalDate = DisposalDate(today.minusDays(5L), taxYear)

      val tomorrow = today.plusDays(1L)

      val dayBeforeDisposalDate = disposalDate.value.minusDays(1L)

      val requiredPreviousAnswers =
        IncompleteTriageAnswers.empty.copy(
          individualUserType = Some(sample[IndividualUserType]),
          numberOfProperties = Some(NumberOfProperties.One),
          disposalMethod     = Some(DisposalMethod.Gifted),
          wasAUKResident     = Some(true),
          assetType          = Some(AssetType.Residential),
          disposalDate       = Some(disposalDate)
        )

      behave like redirectToStartWhenInvalidJourney(() => performAction(), isValidJourney)

      behave like redirectWhenNoPreviousAnswerBehaviour[DisposalDate](() => performAction())(
        requiredPreviousAnswers,
        routes.CanTheyUseOurServiceController.whenWasDisposalDate(),
        { case (i, d) => i.copy(disposalDate = d) }
      )

      "show a form error" when {

        def test(formData: Seq[(String, String)], expectedErrorKey: String) =
          testFormError(performAction, "completionDate.title")(formData, expectedErrorKey, requiredPreviousAnswers)

        "the date is invalid" in {
          dateErrorScenarios("completionDate").foreach { scenario =>
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

      behave like unsuccessfulUpdatesStartingNewDraftBehaviour(
        performAction,
        requiredPreviousAnswers,
        formData(disposalDate.value),
        requiredPreviousAnswers.copy(completionDate = Some(CompletionDate(disposalDate.value)))
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
              checkIsRedirect(_, routes.CanTheyUseOurServiceController.checkYourAnswers())
            )
          }

          "the section is complete" in {
            val completionDate = CompletionDate(disposalDate.value.plusDays(1L))
            val completeAnswers =
              sample[CompleteTriageAnswers]
                .copy(completionDate = CompletionDate(disposalDate.value), disposalDate = disposalDate)

            testSuccessfulUpdateStartingNewDraft(
              performAction,
              completeAnswers,
              formData(completionDate.value),
              completeAnswers.copy(completionDate = completionDate),
              checkIsRedirect(_, routes.CanTheyUseOurServiceController.checkYourAnswers())
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
              requiredPreviousAnswers.copy(completionDate = Some(completionDate)),
              checkIsRedirect(_, routes.CanTheyUseOurServiceController.checkYourAnswers())
            )
          }

          "the section is complete" in {
            val completionDate = CompletionDate(disposalDate.value.plusDays(1L))
            val completeAnswers =
              sample[CompleteTriageAnswers]
                .copy(completionDate = CompletionDate(disposalDate.value), disposalDate = disposalDate)

            testSuccessfulUpdateFillingOutReturn(
              performAction,
              completeAnswers,
              formData(completionDate.value),
              completeAnswers.copy(completionDate = completionDate),
              checkIsRedirect(_, routes.CanTheyUseOurServiceController.checkYourAnswers())
            )
          }
        }

      }

      "not do any updates" when {

        "the answers submitted is the same as the one in session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithFillingOurReturn(
                requiredPreviousAnswers.copy(
                  completionDate = Some(CompletionDate(disposalDate.value))
                )
              )
            )
          }

          checkIsRedirect(
            performAction(formData(disposalDate.value): _*),
            routes.CanTheyUseOurServiceController.checkYourAnswers()
          )
        }

      }

    }

    "handling requests to display the country of residence page" must {

      val requiredPreviousAnswers =
        IncompleteTriageAnswers.empty.copy(
          individualUserType = Some(IndividualUserType.Self),
          numberOfProperties = Some(NumberOfProperties.One),
          disposalMethod     = Some(DisposalMethod.Sold),
          wasAUKResident     = Some(false)
        )

      val (countryCode, countryName) = "HK" -> "Hong Kong"
      val country                    = Country(countryCode, Some(countryName))

      def performAction(): Future[Result] = controller.countryOfResidence()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(performAction, isValidJourney)

      behave like redirectWhenNoPreviousAnswerBehaviour[Boolean](
        performAction
      )(
        requiredPreviousAnswers,
        routes.CanTheyUseOurServiceController.wereYouAUKResident(),
        { case (answers, w) => answers.copy(wasAUKResident = w) }
      )

      "redirect to the were you a uk resident page" when {

        "the user has answered yes to that question and" when {

          "the section is incomplete" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionDataWithStartingNewDraftReturn(requiredPreviousAnswers.copy(wasAUKResident = Some(true)))
              )
            }

            checkIsRedirect(performAction(), routes.CanTheyUseOurServiceController.wereYouAUKResident())
          }

          "the section is complete" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionDataWithStartingNewDraftReturn(
                  sample[CompleteTriageAnswers].copy(countryOfResidence = Country.uk)
                )
              )
            }

            checkIsRedirect(performAction(), routes.CanTheyUseOurServiceController.wereYouAUKResident())

          }
        }

      }

      behave like displayIndividualTriagePageBehaviorIncompleteJourney(
        performAction
      )(requiredPreviousAnswers, requiredPreviousAnswers.copy(countryOfResidence = Some(country)))(
        "triage.enterCountry.title",
        checkContent(_, routes.CanTheyUseOurServiceController.wereYouAUKResident()),
        _ => ()
      )

      behave like displayIndividualTriagePageBehaviorCompleteJourney(
        performAction
      )(sample[CompleteTriageAnswers].copy(countryOfResidence = country))(
        "triage.enterCountry.title", { doc =>
          checkContent(doc, routes.CanTheyUseOurServiceController.checkYourAnswers())
        }
      )

      def checkContent(doc: Document, backLink: Call): Unit = {
        doc.select("#back").attr("href") shouldBe backLink.url
        doc
          .select("#content > article > form")
          .attr("action") shouldBe routes.CanTheyUseOurServiceController
          .countryOfResidenceSubmit()
          .url
      }

    }

    "handling submitted answers to the country of residence page" must {

      def performAction(formData: (String, String)*): Future[Result] =
        controller.countryOfResidenceSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*))

      val requiredPreviousAnswers =
        IncompleteTriageAnswers.empty.copy(
          individualUserType = Some(IndividualUserType.Self),
          numberOfProperties = Some(NumberOfProperties.One),
          disposalMethod     = Some(DisposalMethod.Sold),
          wasAUKResident     = Some(false)
        )

      val (countryCode, countryName) = "HK" -> "Hong Kong"
      val country                    = Country(countryCode, Some(countryName))

      behave like redirectToStartWhenInvalidJourney(() => performAction(), isValidJourney)

      behave like redirectWhenNoPreviousAnswerBehaviour[Boolean](() => performAction())(
        requiredPreviousAnswers,
        routes.CanTheyUseOurServiceController.wereYouAUKResident(),
        { case (answers, w) => answers.copy(wasAUKResident = w) }
      )

      "redirect to the were you a uk resident page" when {

        "the user has answered yes to that question and" when {

          "the section is incomplete" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionDataWithStartingNewDraftReturn(requiredPreviousAnswers.copy(wasAUKResident = Some(true)))
              )
            }

            checkIsRedirect(performAction(), routes.CanTheyUseOurServiceController.wereYouAUKResident())
          }

          "the section is complete" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionDataWithStartingNewDraftReturn(
                  sample[CompleteTriageAnswers].copy(countryOfResidence = Country.uk)
                )
              )
            }

            checkIsRedirect(performAction(), routes.CanTheyUseOurServiceController.wereYouAUKResident())

          }
        }

      }

      "show a form error" when {

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

      behave like unsuccessfulUpdatesStartingNewDraftBehaviour(
        performAction,
        requiredPreviousAnswers,
        List("countryCode" -> countryCode),
        requiredPreviousAnswers.copy(countryOfResidence = Some(country))
      )

      "handle successful updates" when {

        "the user is starting a new draft return and" when {

          "the user has answered some questions but not complete the section" in {
            testSuccessfulUpdateStartingNewDraft(
              performAction,
              requiredPreviousAnswers,
              List("countryCode" -> countryCode),
              requiredPreviousAnswers.copy(countryOfResidence = Some(country)),
              checkIsRedirect(_, routes.CanTheyUseOurServiceController.checkYourAnswers())
            )
          }

          "the user has complete the section and has changed their answer and they choose asset type residential" in {
            val completeAnswers = sample[CompleteTriageAnswers].copy(countryOfResidence = Country("CC", None))
            testSuccessfulUpdateStartingNewDraft(
              performAction,
              completeAnswers,
              List("countryCode" -> countryCode),
              completeAnswers.copy(countryOfResidence = country),
              checkIsRedirect(_, routes.CanTheyUseOurServiceController.checkYourAnswers())
            )
          }
        }

        "the user is filling out a draft return and" when {

          "the user has answered some questions but not complete the section" in {
            testSuccessfulUpdateFillingOutReturn(
              performAction,
              requiredPreviousAnswers,
              List("countryCode" -> countryCode),
              requiredPreviousAnswers.copy(countryOfResidence = Some(country)),
              checkIsRedirect(_, routes.CanTheyUseOurServiceController.checkYourAnswers())
            )
          }

          "the user has complete the section and has changed their answer from not in the uk to was in the uk" in {
            val completeAnswers = sample[CompleteTriageAnswers].copy(countryOfResidence = Country("CC", None))
            testSuccessfulUpdateFillingOutReturn(
              performAction,
              completeAnswers,
              List("countryCode" -> countryCode),
              completeAnswers.copy(countryOfResidence = country),
              checkIsRedirect(_, routes.CanTheyUseOurServiceController.checkYourAnswers())
            )
          }
        }
      }

      "not do any updates" when {

        "the answers submitted is the same as the one in session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithFillingOurReturn(
                requiredPreviousAnswers.copy(
                  countryOfResidence = Some(country)
                )
              )
            )
          }

          val result = performAction("countryCode" -> countryCode)
          checkIsRedirect(result, routes.CanTheyUseOurServiceController.checkYourAnswers())
        }

      }

    }

    "handling requests to display the asset type for non uk residents page" must {

      val requiredPreviousAnswers =
        IncompleteTriageAnswers.empty.copy(
          individualUserType = Some(sample[IndividualUserType]),
          numberOfProperties = Some(NumberOfProperties.One),
          disposalMethod     = Some(DisposalMethod.Sold),
          wasAUKResident     = Some(false),
          countryOfResidence = Some(sample[Country])
        )

      def performAction(): Future[Result] = controller.assetTypeForNonUkResidents()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(performAction, isValidJourney)

      behave like redirectWhenNoPreviousAnswerBehaviour[Country](
        performAction
      )(
        requiredPreviousAnswers,
        routes.CanTheyUseOurServiceController.countryOfResidence(),
        { case (answers, country) => answers.copy(countryOfResidence = country) }
      )

      behave like displayIndividualTriagePageBehaviorIncompleteJourney(
        performAction
      )(requiredPreviousAnswers, requiredPreviousAnswers.copy(assetType = Some(AssetType.MixedUse)))(
        "assetTypeForNonUkResidents.title",
        checkContent(_, routes.CanTheyUseOurServiceController.countryOfResidence()),
        _.select("#assetTypeForNonUkResidents-2").attr("checked") shouldBe "checked"
      )

      behave like displayIndividualTriagePageBehaviorCompleteJourney(
        performAction
      )(sample[CompleteTriageAnswers].copy(countryOfResidence = sample[Country], assetType = AssetType.Residential))(
        "assetTypeForNonUkResidents.title", { doc =>
          checkContent(doc, routes.CanTheyUseOurServiceController.checkYourAnswers())
          doc.select("#assetTypeForNonUkResidents-0").attr("checked") shouldBe "checked"
        }
      )

      def checkContent(doc: Document, backLink: Call): Unit = {
        doc.select("#back").attr("href") shouldBe backLink.url
        doc
          .select("#content > article > form")
          .attr("action") shouldBe routes.CanTheyUseOurServiceController
          .assetTypeForNonUkResidentsSubmit()
          .url
      }

    }

    "handling submitted answers to the asset type for non uk residents page" must {

      val requiredPreviousAnswers =
        IncompleteTriageAnswers.empty.copy(
          individualUserType = Some(sample[IndividualUserType]),
          numberOfProperties = Some(NumberOfProperties.One),
          disposalMethod     = Some(DisposalMethod.Sold),
          wasAUKResident     = Some(false),
          countryOfResidence = Some(sample[Country])
        )

      def performAction(formData: (String, String)*): Future[Result] =
        controller.assetTypeForNonUkResidentsSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*))

      behave like redirectToStartWhenInvalidJourney(() => performAction(), isValidJourney)

      behave like redirectWhenNoPreviousAnswerBehaviour[Country](() => performAction())(
        requiredPreviousAnswers,
        routes.CanTheyUseOurServiceController.countryOfResidence(),
        { case (answers, country) => answers.copy(countryOfResidence = country) }
      )

      "show a form error" when {

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

      behave like unsuccessfulUpdatesStartingNewDraftBehaviour(
        performAction,
        requiredPreviousAnswers,
        List("assetTypeForNonUkResidents" -> "0"),
        requiredPreviousAnswers.copy(assetType = Some(AssetType.Residential))
      )

      "handle successful updates" when {

        "the user is starting a new draft return and" when {

          "the user has answered some questions but not complete the section" in {
            testSuccessfulUpdateStartingNewDraft(
              performAction,
              requiredPreviousAnswers,
              List("assetTypeForNonUkResidents" -> "0"),
              requiredPreviousAnswers.copy(assetType = Some(AssetType.Residential)),
              checkIsRedirect(_, routes.CanTheyUseOurServiceController.checkYourAnswers())
            )
          }

          "the user has complete the section" in {
            val completeAnswers = sample[CompleteTriageAnswers]
            testSuccessfulUpdateStartingNewDraft(
              performAction,
              completeAnswers.copy(assetType = AssetType.Residential),
              List("assetTypeForNonUkResidents" -> "1"),
              IncompleteTriageAnswers(
                Some(completeAnswers.individualUserType),
                Some(completeAnswers.numberOfProperties),
                Some(completeAnswers.disposalMethod),
                Some(false),
                Some(completeAnswers.countryOfResidence),
                Some(AssetType.NonResidential),
                None,
                None
              ),
              checkIsRedirect(_, routes.CanTheyUseOurServiceController.checkYourAnswers())
            )
          }

        }

        "the user is filling out a draft return and" when {

          "the section is complete" in {
            val completeAnswers = sample[CompleteTriageAnswers]
            testSuccessfulUpdateStartingNewDraft(
              performAction,
              completeAnswers.copy(assetType = AssetType.Residential),
              List("assetTypeForNonUkResidents" -> "2"),
              IncompleteTriageAnswers(
                Some(completeAnswers.individualUserType),
                Some(completeAnswers.numberOfProperties),
                Some(completeAnswers.disposalMethod),
                Some(false),
                Some(completeAnswers.countryOfResidence),
                Some(AssetType.MixedUse),
                None,
                None
              ),
              checkIsRedirect(_, routes.CanTheyUseOurServiceController.checkYourAnswers())
            )

          }

          "the section is incomplete" in {
            testSuccessfulUpdateStartingNewDraft(
              performAction,
              requiredPreviousAnswers
                .copy(disposalDate = Some(sample[DisposalDate]), completionDate = Some(sample[CompletionDate])),
              List("assetTypeForNonUkResidents" -> "3"),
              requiredPreviousAnswers
                .copy(assetType = Some(AssetType.IndirectDisposal), disposalDate = None, completionDate = None),
              checkIsRedirect(_, routes.CanTheyUseOurServiceController.checkYourAnswers())
            )
          }
        }
      }

      "not do any updates" when {

        "the answers submitted is the same as the one in session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithFillingOurReturn(
                requiredPreviousAnswers.copy(
                  assetType = Some(AssetType.Residential)
                )
              )
            )
          }

          checkIsRedirect(
            performAction("assetTypeForNonUkResidents" -> "0"),
            routes.CanTheyUseOurServiceController.checkYourAnswers()
          )
        }

      }

    }

    "handling requests to display the check your answers page" must {

      def performAction(): Future[Result] =
        controller.checkYourAnswers()(FakeRequest())

      val completeTriageQuestions =
        CompleteTriageAnswers(
          IndividualUserType.Self,
          NumberOfProperties.One,
          DisposalMethod.Sold,
          Country.uk,
          assetType = AssetType.Residential,
          sample[DisposalDate],
          sample[CompletionDate]
        )

      val allQuestionsAnswered = IncompleteTriageAnswers(
        Some(completeTriageQuestions.individualUserType),
        Some(completeTriageQuestions.numberOfProperties),
        Some(completeTriageQuestions.disposalMethod),
        Some(true),
        None,
        Some(completeTriageQuestions.assetType),
        Some(completeTriageQuestions.disposalDate),
        Some(completeTriageQuestions.completionDate)
      )

      "redirect to the correct page" when {

        def test(sessionDataWith: TriageAnswers => SessionData): Unit =
          List(
            allQuestionsAnswered.copy(individualUserType = None) -> routes.CanTheyUseOurServiceController
              .whoIsIndividualRepresenting(),
            allQuestionsAnswered.copy(numberOfProperties = None) -> routes.CanTheyUseOurServiceController
              .howManyProperties(),
            allQuestionsAnswered.copy(disposalMethod = None) -> routes.CanTheyUseOurServiceController
              .howDidYouDisposeOfProperty(),
            allQuestionsAnswered.copy(wasAUKResident = None) -> routes.CanTheyUseOurServiceController
              .wereYouAUKResident(),
            allQuestionsAnswered.copy(assetType = None) -> routes.CanTheyUseOurServiceController
              .didYouDisposeOfAResidentialProperty(),
            allQuestionsAnswered.copy(disposalDate = None) -> routes.CanTheyUseOurServiceController
              .whenWasDisposalDate(),
            allQuestionsAnswered.copy(completionDate = None) -> routes.CanTheyUseOurServiceController
              .whenWasCompletionDate(),
            allQuestionsAnswered
              .copy(wasAUKResident = Some(false), countryOfResidence = None) -> routes.CanTheyUseOurServiceController
              .countryOfResidence()
          ).foreach {
            case (state, expectedRedirect) =>
              withClue(s"For state $state and expected redirect url ${expectedRedirect.url}: ") {
                inSequence {
                  mockAuthWithNoRetrievals()
                  mockGetSession(sessionDataWith(state))
                }

                checkIsRedirect(performAction(), expectedRedirect)
              }
          }

        "a question has not yet been answered and a draft return has not been created" in {
          test(sessionDataWithStartingNewDraftReturn)
        }

        "a question has not yet been answered and a draft return has been created" in {
          test(sessionDataWithFillingOurReturn)
        }

      }

      "show a dummy page when a user has selected indirect disposals" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            sessionDataWithStartingNewDraftReturn(
              allQuestionsAnswered.copy(
                assetType = Some(AssetType.IndirectDisposal)
              )
            )
          )
        }

        val result = performAction()
        status(result)          shouldBe OK
        contentAsString(result) shouldBe "Indirect disposals not handled yet"
      }

      "show an error page" when {

        "all the questions have now been answered but the sessino data cannot be updated" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionDataWithStartingNewDraftReturn(allQuestionsAnswered))
            mockStoreSession(sessionDataWithStartingNewDraftReturn(completeTriageQuestions))(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction())
        }
      }

      "display the page" when {

        def testIsCYAPagePage(result: Future[Result]) = {
          status(result)          shouldBe OK
          contentAsString(result) should include(messageFromMessageKey("triage.check-your-answers.title"))
        }

        "all the questions have now been answered and the session is updated when a draft return has not yet been created" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionDataWithStartingNewDraftReturn(allQuestionsAnswered))
            mockStoreSession(sessionDataWithStartingNewDraftReturn(completeTriageQuestions))(Right(()))
          }

          testIsCYAPagePage(performAction())
        }

        "all the questions have now been answered and the session is updated when a draft return has been created" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionDataWithFillingOurReturn(allQuestionsAnswered))
            mockStoreSession(sessionDataWithFillingOurReturn(completeTriageQuestions))(Right(()))
          }

          testIsCYAPagePage(performAction())
        }

        "all the questions have already been answered and a draft return has not yet been created" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionDataWithStartingNewDraftReturn(completeTriageQuestions))
          }

          testIsCYAPagePage(performAction())
        }

        "all the questions have already been answered and a draft return has been created" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionDataWithFillingOurReturn(completeTriageQuestions))
          }

          testIsCYAPagePage(performAction())
        }

      }

    }

    "handling submit requests from the check your answers page" must {

      def performAction() =
        controller.checkYourAnswersSubmit()(FakeRequest())

      val completeAnswers = sample[CompleteTriageAnswers]

      val uuid = UUID.randomUUID()

      val newDraftReturn =
        DraftReturn(
          uuid,
          startingNewDraftReturn.subscribedDetails.cgtReference,
          completeAnswers,
          None,
          None,
          None,
          None,
          None,
          None
        )

      val sessionDataWithNewDraftReturn = SessionData.empty.copy(
        journeyStatus = Some(
          fillingOutReturn.copy(
            draftReturn = newDraftReturn
          )
        )
      )

      "redirect to the check your answers page" when {

        "the user has not answered all the questions in the triage section" in {
          val incompleteAnswers = sample[IncompleteTriageAnswers]

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionDataWithStartingNewDraftReturn(incompleteAnswers))
          }

          checkIsRedirect(performAction(), routes.CanTheyUseOurServiceController.checkYourAnswers())
        }

      }

      "show an error page" when {

        "there is a problem storing a draft return" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionDataWithStartingNewDraftReturn(completeAnswers))
            mockGetNextUUID(uuid)
            mockStoreDraftReturn(newDraftReturn)(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction())
        }

        "there is a problem updating the session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionDataWithStartingNewDraftReturn(completeAnswers))
            mockGetNextUUID(uuid)
            mockStoreDraftReturn(newDraftReturn)(Right(()))
            mockStoreSession(sessionDataWithNewDraftReturn)(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction())
        }

      }

      "redirect to the task list page" when {

        "the draft return is stored and the session is updated and a draft return had not already been created" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionDataWithStartingNewDraftReturn(completeAnswers))
            mockGetNextUUID(uuid)
            mockStoreDraftReturn(newDraftReturn)(Right(()))
            mockStoreSession(sessionDataWithNewDraftReturn)(Right(()))
          }

          checkIsRedirect(performAction(), returnsRoutes.TaskListController.taskList())
        }

        "the draft return is stored and the session is updated and a draft return had already been created" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionDataWithFillingOurReturn(completeAnswers))
          }

          checkIsRedirect(performAction(), returnsRoutes.TaskListController.taskList())
        }

      }

    }

  }

  def redirectWhenNoPreviousAnswerBehaviour[A](performAction: () => Future[Result])(
    requiredPreviousAnswers: IncompleteTriageAnswers,
    redirectToPreviousAnswer: => Call,
    setPreviousAnswer: (IncompleteTriageAnswers, Option[A]) => IncompleteTriageAnswers
  ): Unit =
    s"redirect to ${redirectToPreviousAnswer.url}" when {

      "that question has not already answered" in {
        List(
          sessionDataWithStartingNewDraftReturn(setPreviousAnswer(requiredPreviousAnswers, None)),
          sessionDataWithFillingOurReturn(setPreviousAnswer(requiredPreviousAnswers, None))
        ).foreach { currentSession =>
          withClue(s"For currentSession $currentSession: ") {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(sessionDataWithStartingNewDraftReturn(setPreviousAnswer(requiredPreviousAnswers, None)))
            }

            checkIsRedirect(performAction(), redirectToPreviousAnswer)
          }
        }
      }
    }

  def testFormError(
    performAction: Seq[(String, String)] => Future[Result],
    pageTitleKey: String
  )(formData: Seq[(String, String)], expectedErrorMessageKey: String, currentAnswers: TriageAnswers): Unit = {
    inSequence {
      mockAuthWithNoRetrievals()
      mockGetSession(sessionDataWithStartingNewDraftReturn(currentAnswers))
    }

    val result  = performAction(formData)
    val content = contentAsString(result)

    status(result)          shouldBe BAD_REQUEST
    contentAsString(result) should include(messageFromMessageKey(pageTitleKey))
    content                 should include(messageFromMessageKey(expectedErrorMessageKey))
  }

  def displayIndividualTriagePageBehaviorIncompleteJourney(performAction: () => Future[Result])(
    requiredPreviousAnswers: IncompleteTriageAnswers,
    answersWithCurrentAnswer: IncompleteTriageAnswers,
    description: Option[String] = None
  )(
    pageTitleKey: String,
    checkContent: Document => Unit,
    checkPrepopulatedContent: Document => Unit
  ): Unit = {
    val scenarioDescription = description.map(_ + " and when ").getOrElse("")
    s"display the page when ${scenarioDescription}no option has been selected before" in {
      List(
        sessionDataWithStartingNewDraftReturn(requiredPreviousAnswers),
        sessionDataWithFillingOurReturn(requiredPreviousAnswers)
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

    s"display the page when ${scenarioDescription}an option has been selected before" in {
      List(
        sessionDataWithStartingNewDraftReturn(answersWithCurrentAnswer),
        sessionDataWithFillingOurReturn(answersWithCurrentAnswer)
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

  def displayIndividualTriagePageBehaviorCompleteJourney(
    performAction: () => Future[Result]
  )(answers: CompleteTriageAnswers)(
    pageTitleKey: String,
    checkContent: Document => Unit
  ): Unit =
    "display the page when the journey has already been completed" in {
      List(
        sessionDataWithStartingNewDraftReturn(answers),
        sessionDataWithFillingOurReturn(answers)
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

  def unsuccessfulUpdatesStartingNewDraftBehaviour(
    performAction: Seq[(String, String)] => Future[Result],
    currentAnswers: TriageAnswers,
    formData: Seq[(String, String)],
    updatedAnswers: TriageAnswers
  ): Unit =
    "show an error page" when {

      "the user is starting a new draft return and" when {

        "there is an error updating the session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionDataWithStartingNewDraftReturn(currentAnswers))
            mockStoreSession(sessionDataWithStartingNewDraftReturn(updatedAnswers))(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction(formData))
        }

      }

      "the user is filling in a draft return and" when {
        val draftReturn        = sample[DraftReturn].copy(triageAnswers = currentAnswers)
        val updatedDraftReturn = draftReturn.copy(triageAnswers         = updatedAnswers)

        val fillingOutReturn        = sample[FillingOutReturn].copy(draftReturn = draftReturn)
        val updatedFillingOutReturn = fillingOutReturn.copy(draftReturn         = updatedDraftReturn)

        "there is an error updating the draft return" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData.empty.copy(journeyStatus = Some(fillingOutReturn)))
            mockStoreDraftReturn(updatedDraftReturn)(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction(formData))
        }

        "there is an error updating the session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData.empty.copy(journeyStatus = Some(fillingOutReturn)))
            mockStoreDraftReturn(updatedDraftReturn)(Right(()))
            mockStoreSession(SessionData.empty.copy(journeyStatus = Some(updatedFillingOutReturn)))(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction(formData))
        }

      }

    }

  def testSuccessfulUpdateStartingNewDraft[A](
    performAction: Seq[(String, String)] => Future[Result],
    currentAnswers: TriageAnswers,
    formData: Seq[(String, String)],
    updatedAnswers: TriageAnswers,
    checkNextResult: Future[Result] => Unit
  ): Unit = {
    inSequence {
      mockAuthWithNoRetrievals()
      mockGetSession(sessionDataWithStartingNewDraftReturn(currentAnswers))
      mockStoreSession(sessionDataWithStartingNewDraftReturn(updatedAnswers))(Right(()))
    }

    checkNextResult(performAction(formData))
  }

  def testSuccessfulUpdateFillingOutReturn[A](
    performAction: Seq[(String, String)] => Future[Result],
    currentAnswers: TriageAnswers,
    formData: Seq[(String, String)],
    updatedAnswers: TriageAnswers,
    checkNextResult: Future[Result] => Unit
  ): Unit = {
    val draftReturn        = sample[DraftReturn].copy(triageAnswers = currentAnswers)
    val updatedDraftReturn = draftReturn.copy(triageAnswers         = updatedAnswers)

    val fillingOutReturn        = sample[FillingOutReturn].copy(draftReturn = draftReturn)
    val updatedFillingOutReturn = fillingOutReturn.copy(draftReturn         = updatedDraftReturn)

    inSequence {
      mockAuthWithNoRetrievals()
      mockGetSession(SessionData.empty.copy(journeyStatus = Some(fillingOutReturn)))
      mockStoreDraftReturn(updatedDraftReturn)(Right(()))
      mockStoreSession(SessionData.empty.copy(journeyStatus = Some(updatedFillingOutReturn)))(Right(()))
    }

    checkNextResult(performAction(formData))
  }

}
