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
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.accounts.homepage.{routes => homeRoutes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.{routes => returnsRoutes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators.{sample, _}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, StartingNewDraftReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.LocalDateUtils.order
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.UUIDGenerator
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.IndividualTriageAnswers.{CompleteIndividualTriageAnswers, IncompleteIndividualTriageAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns._
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

  def sessionDataWithStartingNewDraftReturn(individualTriageAnswers: IndividualTriageAnswers): SessionData =
    SessionData.empty
      .copy(journeyStatus = Some(startingNewDraftReturn.copy(newReturnTriageAnswers = individualTriageAnswers)))

  def sessionDataWithFillingOurReturn(draftReturn: DraftReturn): SessionData =
    SessionData.empty.copy(journeyStatus = Some(fillingOutReturn.copy(draftReturn = draftReturn)))

  def sessionDataWithFillingOurReturn(individualTriageAnswers: IndividualTriageAnswers): SessionData =
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

      val requiredPreviousAnswers = IncompleteIndividualTriageAnswers.empty

      behave like redirectToStartWhenInvalidJourney(performAction, isValidJourney)

      behave like displayIndividualTriagePageBehaviorIncompleteJourney[Unit, IndividualUserType](
        performAction
      )(requiredPreviousAnswers, homeRoutes.HomePageController.homepage())(
        { case (i, _) => i },
        { case (i, t) => i.copy(individualUserType = t) }
      )(IndividualUserType.Self)(
        "who-are-you-reporting-for.title",
        _ => List("checked=\"checked\"")
      )

      behave like displayIndividualTriagePageBehaviorCompleteJourney(
        performAction
      )(IndividualUserType.Self) {
        case (i, t) => i.copy(individualUserType = t)
      }(
        "who-are-you-reporting-for.title",
        _ => List("checked=\"checked\"")
      )

    }

    "handling submitted answers to the who is the individual representing page" must {

      def performAction(formData: (String, String)*): Future[Result] =
        controller.whoIsIndividualRepresentingSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*))

      val requiredPreviousAnswers = IncompleteIndividualTriageAnswers.empty

      behave like redirectToStartWhenInvalidJourney(() => performAction(), isValidJourney)

      behave like submitIndividualTriagePageBehavior[Unit, IndividualUserType](
        performAction
      )(
        requiredPreviousAnswers,
        homeRoutes.HomePageController.homepage(),
        sample[CompleteIndividualTriageAnswers]
      )(
        { case (i, _) => i },
        { case (i, t) => i.copy(individualUserType = t) },
        { case (i, t) => i.copy(individualUserType = t) }
      )(
        "who-are-you-reporting-for.title"
      )(
        formErrorScenarios = List(
          List.empty -> "individualUserType.error.required",
          List("individualUserType" -> "3") -> "individualUserType.error.invalid"
        )
      )(
        validValueScenarios = List(
          (List("individualUserType" -> "0"), IndividualUserType.Self, {
            case (result, updatedState) =>
              updatedState.fold(
                _ => checkIsRedirect(result, routes.CanTheyUseOurServiceController.howManyProperties()),
                _ => checkIsRedirect(result, routes.CanTheyUseOurServiceController.checkYourAnswers())
              )
          }),
          (List("individualUserType" -> "1"), IndividualUserType.Capacitor, {
            case (result, _) =>
              status(result)          shouldBe OK
              contentAsString(result) shouldBe s"${IndividualUserType.Capacitor} not handled yet"
          }),
          (List("individualUserType" -> "2"), IndividualUserType.PersonalRepresentative, {
            case (result, _) =>
              status(result)          shouldBe OK
              contentAsString(result) shouldBe s"${IndividualUserType.PersonalRepresentative} not handled yet"
          })
        )
      )
    }

    "handling requests to display the how many properties page" must {

      val requiredPreviousAnswers =
        IncompleteIndividualTriageAnswers.empty.copy(individualUserType = Some(sample[IndividualUserType]))

      def performAction(): Future[Result] = controller.howManyProperties()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(performAction, isValidJourney)

      behave like displayIndividualTriagePageBehaviorIncompleteJourney[IndividualUserType, NumberOfProperties](
        performAction
      )(requiredPreviousAnswers, routes.CanTheyUseOurServiceController.whoIsIndividualRepresenting())(
        { case (i, t) => i.copy(individualUserType = t) },
        { case (i, n) => i.copy(numberOfProperties = n) }
      )(NumberOfProperties.One)(
        "numberOfProperties.title",
        _ => List("checked=\"checked\"")
      )

      behave like displayIndividualTriagePageBehaviorCompleteJourney(
        performAction
      )(NumberOfProperties.One) {
        case (i, n) => i.copy(numberOfProperties = n)
      }(
        "numberOfProperties.title",
        _ => List("checked=\"checked\"")
      )
    }

    "handling submitted answers to the how many properties page" must {

      def performAction(formData: (String, String)*): Future[Result] =
        controller.howManyPropertiesSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*))

      val requiredPreviousAnswers =
        IncompleteIndividualTriageAnswers.empty.copy(individualUserType = Some(sample[IndividualUserType]))

      behave like redirectToStartWhenInvalidJourney(() => performAction(), isValidJourney)

      behave like submitIndividualTriagePageBehavior[IndividualUserType, NumberOfProperties](
        performAction
      )(
        requiredPreviousAnswers,
        routes.CanTheyUseOurServiceController.whoIsIndividualRepresenting(),
        sample[CompleteIndividualTriageAnswers]
      )(
        { case (i, t) => i.copy(individualUserType = t) },
        { case (i, n) => i.copy(numberOfProperties = n) },
        { case (i, n) => i.copy(numberOfProperties = n) }
      )(
        "numberOfProperties.title"
      )(
        formErrorScenarios = List(
          List.empty -> "numberOfProperties.error.required",
          List("numberOfProperties" -> "2") -> "numberOfProperties.error.invalid"
        )
      )(
        validValueScenarios = List(
          (List("numberOfProperties" -> "1"), NumberOfProperties.MoreThanOne, {
            case (result, _) =>
              status(result)          shouldBe OK
              contentAsString(result) shouldBe "multiple disposals not handled yet"
          }),
          (List("numberOfProperties" -> "0"), NumberOfProperties.One, {
            case (result, updatedState) =>
              updatedState.fold(
                _ => checkIsRedirect(result, routes.CanTheyUseOurServiceController.howDidYouDisposeOfProperty()),
                _ => checkIsRedirect(result, routes.CanTheyUseOurServiceController.checkYourAnswers())
              )
          })
        )
      )

    }

    "handling requests to display how did you dispose of your property page" must {

      val requiredPreviousAnswers =
        IncompleteIndividualTriageAnswers.empty.copy(numberOfProperties = Some(NumberOfProperties.One))

      def performAction(): Future[Result] = controller.howDidYouDisposeOfProperty()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(performAction, isValidJourney)

      behave like displayIndividualTriagePageBehaviorIncompleteJourney[NumberOfProperties, DisposalMethod](
        performAction
      )(requiredPreviousAnswers, routes.CanTheyUseOurServiceController.howManyProperties())(
        { case (i, n) => i.copy(numberOfProperties = n) },
        { case (i, m) => i.copy(disposalMethod     = m) }
      )(DisposalMethod.Gifted)(
        "disposalMethod.title",
        _ => List("checked=\"checked\"")
      )

      behave like displayIndividualTriagePageBehaviorCompleteJourney[DisposalMethod](
        performAction
      )(DisposalMethod.Gifted) {
        case (i, m) => i.copy(disposalMethod = m)
      }(
        "disposalMethod.title",
        _ => List("checked=\"checked\"")
      )

    }

    "handling submitted answers to the how did you dispose of your property page" must {

      def performAction(formData: (String, String)*): Future[Result] =
        controller.howDidYouDisposeOfPropertySubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*))

      val requiredPreviousAnswers =
        IncompleteIndividualTriageAnswers.empty.copy(numberOfProperties = Some(NumberOfProperties.One))

      behave like redirectToStartWhenInvalidJourney(() => performAction(), isValidJourney)

      behave like submitIndividualTriagePageBehavior[NumberOfProperties, DisposalMethod](
        performAction
      )(
        requiredPreviousAnswers,
        routes.CanTheyUseOurServiceController.howManyProperties(),
        sample[CompleteIndividualTriageAnswers]
      )(
        { case (i, n) => i.copy(numberOfProperties = n) },
        { case (i, m) => i.copy(disposalMethod     = m) },
        { case (i, m) => i.copy(disposalMethod     = m) }
      )(
        "disposalMethod.title"
      )(
        formErrorScenarios = List(
          List.empty -> "disposalMethod.error.required",
          List("disposalMethod" -> "2") -> "disposalMethod.error.invalid"
        )
      )(
        validValueScenarios = List(
          (List("disposalMethod" -> "0"), DisposalMethod.Sold, {
            case (result, updatedState) =>
              updatedState.fold(
                _ => checkIsRedirect(result, routes.CanTheyUseOurServiceController.wereYouAUKResident()),
                _ => checkIsRedirect(result, routes.CanTheyUseOurServiceController.checkYourAnswers())
              )
          }),
          (List("disposalMethod" -> "1"), DisposalMethod.Gifted, {
            case (result, updatedState) =>
              updatedState.fold(
                _ => checkIsRedirect(result, routes.CanTheyUseOurServiceController.wereYouAUKResident()),
                _ => checkIsRedirect(result, routes.CanTheyUseOurServiceController.checkYourAnswers())
              )
          })
        )
      )

    }

    "handling requests to display the were you a uk resident page" must {

      val requiredPreviousAnswers =
        IncompleteIndividualTriageAnswers.empty.copy(disposalMethod = Some(DisposalMethod.Sold))

      def performAction(): Future[Result] = controller.wereYouAUKResident()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(performAction, isValidJourney)

      behave like displayIndividualTriagePageBehaviorIncompleteJourney[DisposalMethod, Boolean](
        performAction
      )(requiredPreviousAnswers, routes.CanTheyUseOurServiceController.howDidYouDisposeOfProperty())(
        { case (i, m) => i.copy(disposalMethod = m) },
        { case (i, w) => i.copy(wasAUKResident = w) }
      )(true)(
        "wereYouAUKResident.title",
        _ => List("checked=\"checked\"")
      )

      behave like displayIndividualTriagePageBehaviorCompleteJourney(
        performAction
      )(true) {
        case (i, w) => i.copy(wasAUKResident = w)
      }(
        "wereYouAUKResident.title",
        _ => List("checked=\"checked\"")
      )

    }

    "handling submitted answers to the were you a uk resident page" must {

      def performAction(formData: (String, String)*): Future[Result] =
        controller.wereYouAUKResidentSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*))

      val requiredPreviousAnswers =
        IncompleteIndividualTriageAnswers.empty.copy(disposalMethod = Some(DisposalMethod.Sold))

      behave like redirectToStartWhenInvalidJourney(() => performAction(), isValidJourney)

      behave like submitIndividualTriagePageBehavior[DisposalMethod, Boolean](
        performAction
      )(
        requiredPreviousAnswers,
        routes.CanTheyUseOurServiceController.howDidYouDisposeOfProperty(),
        sample[CompleteIndividualTriageAnswers]
      )(
        { case (i, m) => i.copy(disposalMethod = m) },
        { case (i, w) => i.copy(wasAUKResident = w) },
        { case (i, w) => i.copy(wasAUKResident = w) }
      )(
        "wereYouAUKResident.title"
      )(
        formErrorScenarios = List(
          List.empty -> "wereYouAUKResident.error.required",
          List("wereYouAUKResident" -> "2") -> "wereYouAUKResident.error.boolean"
        )
      )(
        validValueScenarios = List(
          (List("wereYouAUKResident" -> "true"), true, {
            case (result, updatedState) =>
              updatedState.fold(
                _ =>
                  checkIsRedirect(result, routes.CanTheyUseOurServiceController.didYouDisposeOfAResidentialProperty()),
                _ => checkIsRedirect(result, routes.CanTheyUseOurServiceController.checkYourAnswers())
              )

          }),
          (List("wereYouAUKResident" -> "false"), false, {
            case (result, updatedState) =>
              status(result)          shouldBe OK
              contentAsString(result) shouldBe "non residents not handled yet"
          })
        )
      )

    }

    "handling requests to display the did you dispose of a residential property page" must {

      val requiredPreviousAnswers =
        IncompleteIndividualTriageAnswers.empty.copy(wasAUKResident = Some(true))

      def performAction(): Future[Result] = controller.didYouDisposeOfAResidentialProperty()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(performAction, isValidJourney)

      behave like displayIndividualTriagePageBehaviorIncompleteJourney[Boolean, Boolean](
        performAction
      )(requiredPreviousAnswers, routes.CanTheyUseOurServiceController.wereYouAUKResident())(
        { case (i, w) => i.copy(wasAUKResident = w) },
        { case (i, w) => i.copy(assetType      = w.map(if (_) AssetType.Residential else AssetType.NonResidential)) }
      )(true)(
        "didYouDisposeOfResidentialProperty.title",
        _ => List("checked=\"checked\"")
      )

      behave like displayIndividualTriagePageBehaviorCompleteJourney(
        performAction
      )(true) {
        case (i, w) => i.copy(assetType = if (w) AssetType.Residential else AssetType.NonResidential)
      }(
        "didYouDisposeOfResidentialProperty.title",
        _ => List("checked=\"checked\"")
      )

    }

    "handling submitted answers to the did you dispose of a residential property page" must {

      def performAction(formData: (String, String)*): Future[Result] =
        controller.didYouDisposeOfAResidentialPropertySubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*))

      val requiredPreviousAnswers =
        IncompleteIndividualTriageAnswers.empty.copy(wasAUKResident = Some(true))

      behave like redirectToStartWhenInvalidJourney(() => performAction(), isValidJourney)

      behave like submitIndividualTriagePageBehavior[Boolean, Boolean](
        performAction
      )(
        requiredPreviousAnswers,
        routes.CanTheyUseOurServiceController.wereYouAUKResident(),
        sample[CompleteIndividualTriageAnswers]
      )(
        { case (i, w) => i.copy(wasAUKResident = w) },
        { case (i, w) => i.copy(assetType      = w.map(if (_) AssetType.Residential else AssetType.NonResidential)) },
        { case (i, w) => i.copy(assetType      = if (w) AssetType.Residential else AssetType.NonResidential) }
      )(
        "didYouDisposeOfResidentialProperty.title"
      )(
        formErrorScenarios = List(
          List.empty -> "didYouDisposeOfResidentialProperty.error.required",
          List("didYouDisposeOfResidentialProperty" -> "2") -> "didYouDisposeOfResidentialProperty.error.boolean"
        )
      )(
        validValueScenarios = List(
          (List("didYouDisposeOfResidentialProperty" -> "true"), true, {
            case (result, updatedState) =>
              updatedState.fold(
                _ => checkIsRedirect(result, routes.CanTheyUseOurServiceController.whenWasDisposalDate()),
                _ => checkIsRedirect(result, routes.CanTheyUseOurServiceController.checkYourAnswers())
              )
          }),
          (List("didYouDisposeOfResidentialProperty" -> "false"), false, {
            case (result, updatedState) =>
              status(result)          shouldBe OK
              contentAsString(result) shouldBe "individuals can only report on residential properties"
          })
        )
      )

    }

    "handling requests to display the when was disposal date page" must {

      val requiredPreviousAnswers =
        IncompleteIndividualTriageAnswers.empty.copy(
          individualUserType = Some(sample[IndividualUserType]),
          numberOfProperties = Some(NumberOfProperties.One),
          disposalMethod     = Some(DisposalMethod.Gifted),
          wasAUKResident     = Some(true),
          assetType          = Some(AssetType.Residential)
        )

      val disposalDate = DisposalDate(LocalDate.of(2020, 1, 2), taxYear)

      def performAction(): Future[Result] = controller.whenWasDisposalDate()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(performAction, isValidJourney)

      behave like displayIndividualTriagePageBehaviorIncompleteJourney[Boolean, DisposalDate](
        performAction
      )(requiredPreviousAnswers, routes.CanTheyUseOurServiceController.didYouDisposeOfAResidentialProperty())(
        { case (i, w) => i.copy(assetType    = w.map(if (_) AssetType.Residential else AssetType.NonResidential)) },
        { case (i, d) => i.copy(disposalDate = d) }
      )(disposalDate)(
        "disposalDate.title",
        d =>
          List(
            s"""value="${d.value.getDayOfMonth()}"""",
            s"""value="${d.value.getMonthValue()}"""",
            s"""value="${d.value.getYear()}""""
          )
      )

      behave like displayIndividualTriagePageBehaviorCompleteJourney(
        performAction
      )(disposalDate) {
        case (i, d) => i.copy(disposalDate = d)
      }(
        "disposalDate.title",
        d =>
          List(
            s"""value="${d.value.getDayOfMonth()}"""",
            s"""value="${d.value.getMonthValue()}"""",
            s"""value="${d.value.getYear()}""""
          )
      )

    }

    "handling submitted disposal dates" must {

      def performAction(formData: (String, String)*): Future[Result] =
        controller.whenWasDisposalDateSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*))

      val tomorrow = today.plusDays(1L)

      val requiredPreviousAnswers =
        IncompleteIndividualTriageAnswers.empty.copy(
          individualUserType = Some(sample[IndividualUserType]),
          numberOfProperties = Some(NumberOfProperties.One),
          disposalMethod     = Some(DisposalMethod.Gifted),
          wasAUKResident     = Some(true),
          assetType          = Some(AssetType.Residential)
        )

      val formErrorScenarios =
        (dateErrorScenarios("disposalDate") ::: List(
          // date in the future
          DateErrorScenario(
            Some(tomorrow.getDayOfMonth.toString),
            Some(tomorrow.getMonthValue.toString),
            Some(tomorrow.getYear.toString),
            "disposalDate.error.tooFarInFuture"
          ),
          // date does not exist
          DateErrorScenario(Some("31"), Some("2"), Some("2019"), "disposalDate.error.invalid")
        )).map {
          case DateErrorScenario(dayString, monthString, yearString, expectedErrorKey) =>
            val formData = List(
              "disposalDate-day"   -> dayString,
              "disposalDate-month" -> monthString,
              "disposalDate-year"  -> yearString
            ).collect { case (id, Some(input)) => id -> input }

            formData -> expectedErrorKey
        }

      def expectedRedirectLocation(updatedState: IndividualTriageAnswers): Call =
        updatedState.fold(
          _ => routes.CanTheyUseOurServiceController.whenWasCompletionDate(),
          _ => routes.CanTheyUseOurServiceController.checkYourAnswers()
        )

      val validValueScenarios =
        List[(LocalDate, (Future[Result], IndividualTriageAnswers) => Unit)](
          earliestDisposalDate.minusDays(1L) -> {
            case (result, _) =>
              status(result)          shouldBe OK
              contentAsString(result) shouldBe s"disposal date was strictly before $earliestDisposalDate"
          },
          earliestDisposalDate -> { (result, updatedState) =>
            checkIsRedirect(result, expectedRedirectLocation(updatedState))
          },
          earliestDisposalDate.plusDays(1L) -> {
            case (result, updatedState) =>
              checkIsRedirect(result, expectedRedirectLocation(updatedState))
          }
        ).map {
          case (date, checks) =>
            val formData = List(
              "disposalDate-day"   -> date.getDayOfMonth().toString,
              "disposalDate-month" -> date.getMonthValue().toString,
              "disposalDate-year"  -> date.getYear().toString
            )
            (formData, DisposalDate(date, taxYear), checks)
        }

      behave like redirectToStartWhenInvalidJourney(() => performAction(), isValidJourney)

      behave like submitIndividualTriagePageBehavior[Boolean, DisposalDate](
        performAction
      )(
        requiredPreviousAnswers,
        routes.CanTheyUseOurServiceController.didYouDisposeOfAResidentialProperty(),
        sample[CompleteIndividualTriageAnswers]
      )(
        { case (i, w) => i.copy(assetType    = w.map(if (_) AssetType.Residential else AssetType.NonResidential)) },
        { case (i, d) => i.copy(disposalDate = d) }, {
          case (i, d) =>
            IncompleteIndividualTriageAnswers(
              Some(i.individualUserType),
              Some(i.numberOfProperties),
              Some(i.disposalMethod),
              Some(i.wasAUKResident),
              Some(i.assetType),
              Some(d),
              None
            )
        }
      )(
        "disposalDate.title"
      )(
        formErrorScenarios
      )(
        validValueScenarios
      )

    }

    "handling requests to display the when was completion date page" must {

      val disposalDate = DisposalDate(today, taxYear)

      val requiredPreviousAnswers =
        IncompleteIndividualTriageAnswers.empty.copy(
          individualUserType = Some(sample[IndividualUserType]),
          numberOfProperties = Some(NumberOfProperties.One),
          disposalMethod     = Some(DisposalMethod.Gifted),
          wasAUKResident     = Some(true),
          assetType          = Some(AssetType.Residential),
          disposalDate       = Some(disposalDate)
        )

      def performAction(): Future[Result] = controller.whenWasCompletionDate()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(performAction, isValidJourney)

      behave like displayIndividualTriagePageBehaviorIncompleteJourney[DisposalDate, CompletionDate](
        performAction
      )(requiredPreviousAnswers, routes.CanTheyUseOurServiceController.whenWasDisposalDate())(
        { case (i, d) => i.copy(disposalDate   = d) },
        { case (i, d) => i.copy(completionDate = d) }
      )(CompletionDate(disposalDate.value))(
        "completionDate.title",
        d =>
          List(
            s"""value="${d.value.getDayOfMonth()}"""",
            s"""value="${d.value.getMonthValue()}"""",
            s"""value="${d.value.getYear()}""""
          )
      )

      behave like displayIndividualTriagePageBehaviorCompleteJourney(
        performAction
      )(CompletionDate(disposalDate.value)) {
        case (i, d) => i.copy(completionDate = d)
      }(
        "completionDate.title",
        d =>
          List(
            s"""value="${d.value.getDayOfMonth()}"""",
            s"""value="${d.value.getMonthValue()}"""",
            s"""value="${d.value.getYear()}""""
          )
      )

    }

    "handling submitted completion dates" must {

      def performAction(formData: (String, String)*): Future[Result] =
        controller.whenWasCompletionDateSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*))

      val disposalDate = DisposalDate(today.minusDays(5L), taxYear)

      val tomorrow = today.plusDays(1L)

      val dayBeforeDisposalDate = disposalDate.value.minusDays(1L)

      val requiredPreviousAnswers =
        IncompleteIndividualTriageAnswers.empty.copy(
          individualUserType = Some(sample[IndividualUserType]),
          numberOfProperties = Some(NumberOfProperties.One),
          disposalMethod     = Some(DisposalMethod.Gifted),
          wasAUKResident     = Some(true),
          assetType          = Some(AssetType.Residential),
          disposalDate       = Some(disposalDate)
        )

      val formErrorScenarios =
        (dateErrorScenarios("completionDate") ::: List(
          // date before disposal date
          DateErrorScenario(
            Some(dayBeforeDisposalDate.getDayOfMonth.toString),
            Some(dayBeforeDisposalDate.getMonthValue.toString),
            Some(dayBeforeDisposalDate.getYear.toString),
            "completionDate.error.tooFarInPast"
          ),
          // date in future
          DateErrorScenario(
            Some(tomorrow.getDayOfMonth.toString),
            Some(tomorrow.getMonthValue.toString),
            Some(tomorrow.getYear.toString),
            "completionDate.error.tooFarInFuture"
          )
        )).map {
          case DateErrorScenario(dayString, monthString, yearString, expectedErrorKey) =>
            val formData = List(
              "completionDate-day"   -> dayString,
              "completionDate-month" -> monthString,
              "completionDate-year"  -> yearString
            ).collect { case (id, Some(input)) => id -> input }

            formData -> expectedErrorKey
        }

      val validValueScenarios =
        List[(LocalDate, (Future[Result], IndividualTriageAnswers) => Unit)](
          disposalDate.value -> {
            case (result, _) =>
              checkIsRedirect(result, routes.CanTheyUseOurServiceController.checkYourAnswers())
          },
          disposalDate.value.plusDays(1L) -> {
            case (result, _) =>
              checkIsRedirect(result, routes.CanTheyUseOurServiceController.checkYourAnswers())
          }
        ).map {
          case (date, checks) =>
            val formData = List(
              "completionDate-day"   -> date.getDayOfMonth().toString,
              "completionDate-month" -> date.getMonthValue().toString,
              "completionDate-year"  -> date.getYear().toString
            )
            (formData, CompletionDate(date), checks)
        }

      behave like redirectToStartWhenInvalidJourney(() => performAction(), isValidJourney)

      behave like submitIndividualTriagePageBehavior[DisposalDate, CompletionDate](
        performAction
      )(
        requiredPreviousAnswers,
        routes.CanTheyUseOurServiceController.whenWasDisposalDate(),
        sample[CompleteIndividualTriageAnswers].copy(disposalDate = disposalDate)
      )(
        { case (i, d) => i.copy(disposalDate   = d) },
        { case (i, d) => i.copy(completionDate = d) },
        { case (i, d) => i.copy(completionDate = d) }
      )(
        "completionDate.title"
      )(
        formErrorScenarios
      )(
        validValueScenarios
      )

    }

    "handling requests to display the check your answers page" must {

      def performAction(): Future[Result] =
        controller.checkYourAnswers()(FakeRequest())

      val completeTriageQuestions =
        CompleteIndividualTriageAnswers(
          IndividualUserType.Self,
          NumberOfProperties.One,
          DisposalMethod.Sold,
          wasAUKResident = true,
          assetType      = AssetType.Residential,
          sample[DisposalDate],
          sample[CompletionDate]
        )

      val allQuestionsAnswered = IncompleteIndividualTriageAnswers(
        Some(completeTriageQuestions.individualUserType),
        Some(completeTriageQuestions.numberOfProperties),
        Some(completeTriageQuestions.disposalMethod),
        Some(completeTriageQuestions.wasAUKResident),
        Some(completeTriageQuestions.assetType),
        Some(completeTriageQuestions.disposalDate),
        Some(completeTriageQuestions.completionDate)
      )

      "redirect to the correct page" when {

        def test(sessionDataWith: IndividualTriageAnswers => SessionData): Unit =
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
              .whenWasCompletionDate()
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

      val completeAnswers = sample[CompleteIndividualTriageAnswers]

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
          val incompleteAnswers = sample[IncompleteIndividualTriageAnswers]

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

  def submitIndividualTriagePageBehavior[A, B](performAction: Seq[(String, String)] => Future[Result])(
    requiredPreviousAnswers: IncompleteIndividualTriageAnswers,
    redirectToIfNotValidJourney: => Call,
    sampleCompleteJourney: CompleteIndividualTriageAnswers
  )(
    setPreviousAnswer: (IncompleteIndividualTriageAnswers, Option[A]) => IncompleteIndividualTriageAnswers,
    updateCurrentIncompleteAnswer: (IncompleteIndividualTriageAnswers, Option[B]) => IncompleteIndividualTriageAnswers,
    updateCurrentCompleteAnswer: (CompleteIndividualTriageAnswers, B) => IndividualTriageAnswers
  )(pageTitleKey: String)(
    formErrorScenarios: Seq[(Seq[(String, String)], String)]
  )(validValueScenarios: Seq[(Seq[(String, String)], B, (Future[Result], IndividualTriageAnswers) => Unit)]): Unit = {

    if (setPreviousAnswer(requiredPreviousAnswers, None) =!= requiredPreviousAnswers) {

      s"redirect to ${redirectToIfNotValidJourney.url}" when {

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

              checkIsRedirect(performAction(Seq.empty), redirectToIfNotValidJourney)
            }
          }
        }

      }
    }

    "show a form error" when {

      "the submitted data is invalid" in {
        formErrorScenarios.foreach {
          case (formData, expectedErrorMessageKey) =>
            withClue(
              s"For form data [${formData.mkString(";")}] and expected error message key '$expectedErrorMessageKey': "
            ) {
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(sessionDataWithStartingNewDraftReturn(requiredPreviousAnswers))
              }

              val result  = performAction(formData)
              val content = contentAsString(result)

              status(result)          shouldBe BAD_REQUEST
              contentAsString(result) should include(messageFromMessageKey(pageTitleKey))
              content                 should include(messageFromMessageKey(expectedErrorMessageKey))
            }
        }
      }
    }

    "show an error page" when {

      "the submitted value is valid but there is an error updating the session" in {

        validValueScenarios.foreach {
          case (formData, validValue, _) =>
            withClue(s"For form data [${formData.mkString(";")}] and value '$validValue': ") {
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(sessionDataWithStartingNewDraftReturn(requiredPreviousAnswers))
                mockStoreSession(
                  sessionDataWithStartingNewDraftReturn(
                    updateCurrentIncompleteAnswer(requiredPreviousAnswers, Some(validValue))
                  )
                )(Left(Error("")))
              }

              val result = performAction(formData)
              checkIsTechnicalErrorPage(result)
            }
        }
      }

      "the submitted value is valid but there is an error storing the draft return" in {
        validValueScenarios.foreach {
          case (formData, validValue, _) =>
            withClue(s"For form data [${formData.mkString(";")}] and value '$validValue': ") {
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(sessionDataWithFillingOurReturn(requiredPreviousAnswers))
                mockStoreDraftReturn(
                  draftReturn.copy(
                    triageAnswers = updateCurrentIncompleteAnswer(requiredPreviousAnswers, Some(validValue))
                  )
                )(Left(Error("")))
              }

              val result = performAction(formData)
              checkIsTechnicalErrorPage(result)
            }
        }
      }

    }

    "handle valid data" when {

      def testDraftReturnJourney(
        formData: Seq[(String, String)],
        validValue: B,
        checkResult: (Future[Result], IndividualTriageAnswers) => Unit,
        initialState: DraftReturn,
        updatedState: DraftReturn
      ): Unit =
        withClue(
          s"For:\n form data [${formData.mkString(";")}]\n value '$validValue'\n initialState\n '$initialState'\n updatedState '$updatedState': "
        ) {
          val (currentSession, updatedSession) =
            sessionDataWithFillingOurReturn(initialState) -> sessionDataWithFillingOurReturn(updatedState)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(currentSession)
            if (updatedState =!= initialState) mockStoreDraftReturn(updatedState)(Right(()))
            if (currentSession =!= updatedSession) mockStoreSession(updatedSession)(Right(()))
          }

          val result = performAction(formData)
          checkResult(result, updatedState.triageAnswers)
        }

      def test(
        formData: Seq[(String, String)],
        validValue: B,
        checkResult: (Future[Result], IndividualTriageAnswers) => Unit,
        initialState: IndividualTriageAnswers,
        updatedState: IndividualTriageAnswers
      ): Unit =
        withClue(
          s"For:\n form data [${formData.mkString(";")}]\n value '$validValue'\n initialState\n '$initialState'\n updatedState '$updatedState': "
        ) {
          val (currentSession, updatedSession) =
            sessionDataWithStartingNewDraftReturn(initialState) -> sessionDataWithStartingNewDraftReturn(updatedState)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(currentSession)
            if (currentSession =!= updatedSession) mockStoreSession(updatedSession)(Right(()))
          }

          val result = performAction(formData)
          checkResult(result, updatedState)
        }

      validValueScenarios.foreach {
        case (formData, validValue, checkResult) =>
          s"submitting [${formData.mkString("; ")}] when the current state is incomplete when the " +
            "user has not yet created a draft return" in {
            test(
              formData,
              validValue,
              checkResult,
              requiredPreviousAnswers,
              updateCurrentIncompleteAnswer(requiredPreviousAnswers, Some(validValue))
            )
          }

          s"submitting [${formData.mkString("; ")}] when the current state is incomplete when the " +
            "user has created a draft return" in {
            testDraftReturnJourney(
              formData,
              validValue,
              checkResult,
              draftReturn.copy(triageAnswers = requiredPreviousAnswers),
              draftReturn.copy(triageAnswers = updateCurrentIncompleteAnswer(requiredPreviousAnswers, Some(validValue)))
            )
          }

          s"submitting [${formData.mkString("; ")}] when the current state is complete when the " +
            "user has not yet created a draft return" in {
            test(
              formData,
              validValue,
              checkResult,
              sampleCompleteJourney,
              updateCurrentCompleteAnswer(sampleCompleteJourney, validValue)
            )
          }

          s"submitting [${formData.mkString("; ")}] when the current state is complete when the " +
            "user has created a draft return" in {
            testDraftReturnJourney(
              formData,
              validValue,
              checkResult,
              draftReturn.copy(triageAnswers = sampleCompleteJourney),
              draftReturn.copy(triageAnswers = updateCurrentCompleteAnswer(sampleCompleteJourney, validValue))
            )
          }
      }
    }

    "not update the session" when {

      "the answer submitted is the same as the one already stored in session" in {
        validValueScenarios.foreach {
          case (formData, validValue, checkResult) =>
            val updatedState = updateCurrentIncompleteAnswer(requiredPreviousAnswers, Some(validValue))

            withClue(s"For form data [${formData.mkString(";")}] and value '$validValue': ") {
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(sessionDataWithStartingNewDraftReturn((updatedState)))
              }

              val result = performAction(formData)
              checkResult(result, updatedState)
            }

        }

      }
    }
  }

  def displayIndividualTriagePageBehaviorIncompleteJourney[A, B](performAction: () => Future[Result])(
    requiredPreviousAnswers: IncompleteIndividualTriageAnswers,
    redirectToIfNotValidJourney: => Call
  )(
    setPreviousAnswer: (IncompleteIndividualTriageAnswers, Option[A]) => IncompleteIndividualTriageAnswers,
    setCurrentAnswer: (IncompleteIndividualTriageAnswers, Option[B]) => IncompleteIndividualTriageAnswers
  )(sampleCurrentAnswer: B)(
    pageTitleKey: String,
    prepopulatedContent: B => List[String]
  ): Unit = {

    if (setPreviousAnswer(requiredPreviousAnswers, None) =!= requiredPreviousAnswers) {
      s"redirect to ${redirectToIfNotValidJourney.url}" when {

        "that question has not already answered" in {
          List(
            sessionDataWithStartingNewDraftReturn(setPreviousAnswer(requiredPreviousAnswers, None)),
            sessionDataWithFillingOurReturn(setPreviousAnswer(requiredPreviousAnswers, None))
          ).foreach { currentSession =>
            withClue(s"For currentSession $currentSession: ") {
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(currentSession)
              }

              checkIsRedirect(performAction(), redirectToIfNotValidJourney)
            }
          }

        }

      }
    }

    "display the page when no option has been selected before" in {
      List(
        sessionDataWithStartingNewDraftReturn(requiredPreviousAnswers),
        sessionDataWithFillingOurReturn(requiredPreviousAnswers)
      ).foreach { currentSession =>
        withClue(s"For currentSession $currentSession: ") {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(currentSession)
          }

          val result = performAction()
          status(result)          shouldBe OK
          contentAsString(result) should include(messageFromMessageKey(pageTitleKey))
        }
      }
    }

    "display the page when an option has been selected before" in {
      List(
        sessionDataWithStartingNewDraftReturn(setCurrentAnswer(requiredPreviousAnswers, Some(sampleCurrentAnswer))),
        sessionDataWithFillingOurReturn(setCurrentAnswer(requiredPreviousAnswers, Some(sampleCurrentAnswer)))
      ).foreach { currentSession =>
        withClue(s"For currentSession $currentSession: ") {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(currentSession)
          }

          val result  = performAction()
          val content = contentAsString(result)
          status(result) shouldBe OK

          content should include(messageFromMessageKey(pageTitleKey))
          prepopulatedContent(sampleCurrentAnswer).foreach(content should include(_))
        }
      }
    }

  }

  def displayIndividualTriagePageBehaviorCompleteJourney[A](
    performAction: () => Future[Result]
  )(sampleCurrentAnswer: A)(setCurrentAnswer: (CompleteIndividualTriageAnswers, A) => CompleteIndividualTriageAnswers)(
    pageTitleKey: String,
    prepopulatedContent: A => List[String]
  ): Unit = {
    val completeIndividualTriageAnswers = sample[CompleteIndividualTriageAnswers]

    "display the page when the journey has already been completed" in {
      List(
        sessionDataWithStartingNewDraftReturn(
          setCurrentAnswer(completeIndividualTriageAnswers, sampleCurrentAnswer)
        ),
        sessionDataWithFillingOurReturn(setCurrentAnswer(completeIndividualTriageAnswers, sampleCurrentAnswer))
      ).foreach { currentSession =>
        withClue(s"For currentSession $currentSession: ") {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(currentSession)
          }

          val result  = performAction()
          val content = contentAsString(result)
          status(result) shouldBe OK

          content should include(messageFromMessageKey(pageTitleKey))
          prepopulatedContent(sampleCurrentAnswer).foreach(content should include(_))
        }
      }
    }
  }

}
