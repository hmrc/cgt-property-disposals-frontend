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
import cats.syntax.eq._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.Configuration
import play.api.i18n.MessagesApi
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.{Call, Result}
import play.api.test.FakeRequest
import play.api.test.Helpers.{contentAsString, _}
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.{routes => returnsRoutes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators.{sample, _}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.Subscribed
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.UUIDGenerator
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.IndividualTriageAnswers.{CompleteIndividualTriageAnswers, IncompleteIndividualTriageAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, JourneyStatus, SessionData}
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

  override lazy val additionalConfig: Configuration = Configuration(
    "returns.earliest-disposal-date-inclusive" -> earliestDisposalDate.format(DateTimeFormatter.ISO_DATE)
  )

  lazy val controller = instanceOf[CanTheyUseOurServiceController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  def isSubscribedJourney(journeyStatus: JourneyStatus): Boolean = journeyStatus match {
    case _: Subscribed => true
    case _             => false
  }

  val subscribed = sample[Subscribed].copy(individualTriageAnswers = None, draftReturn = None)

  val draftReturn = sample[DraftReturn]

  def sessionDataWithIndividualTriageAnswers(individualTriageAnswers: Option[IndividualTriageAnswers]): SessionData =
    SessionData.empty.copy(journeyStatus = Some(subscribed.copy(individualTriageAnswers = individualTriageAnswers)))

  def sessionDataWithIndividualTriageAnswers(individualTriageAnswers: IndividualTriageAnswers): SessionData =
    sessionDataWithIndividualTriageAnswers(Some(individualTriageAnswers))

  def sessionDataWithDraftReturn(individualTriageAnswers: IndividualTriageAnswers): SessionData =
    SessionData.empty.copy(journeyStatus =
      Some(subscribed.copy(draftReturn = Some(draftReturn.copy(triageAnswers = individualTriageAnswers))))
    )

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

      behave like redirectToStartWhenInvalidJourney(performAction, isSubscribedJourney)

      "display the page when no option has been selected before" in {
        List(
          sessionDataWithIndividualTriageAnswers(None),
          sessionDataWithIndividualTriageAnswers(IncompleteIndividualTriageAnswers.empty),
          sessionDataWithDraftReturn(IncompleteIndividualTriageAnswers.empty)
        ).foreach { sessionData =>
          withClue(s"For session data $sessionData: ") {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(Future.successful(Right(Some(sessionData))))
            }

            val result = performAction()
            status(result)          shouldBe OK
            contentAsString(result) should include(message("who-are-you-reporting-for.title"))
            contentAsString(result) should not include ("checked=\"checked\"")
          }
        }
      }

      "display the page when an option has been selected before" in {
        val individualTriageAnswers =
          sample[IncompleteIndividualTriageAnswers].copy(individualUserType = Some(IndividualUserType.Self))

        List(
          sessionDataWithIndividualTriageAnswers(individualTriageAnswers),
          sessionDataWithIndividualTriageAnswers(sample[CompleteIndividualTriageAnswers]),
          sessionDataWithDraftReturn(individualTriageAnswers),
          sessionDataWithDraftReturn(sample[CompleteIndividualTriageAnswers])
        ).foreach { sessionData =>
          withClue(s"For session data $sessionData: ") {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(Future.successful(Right(Some(sessionData))))
            }

            val result = performAction()
            status(result)          shouldBe OK
            contentAsString(result) should include(message("who-are-you-reporting-for.title"))
            contentAsString(result) should include("checked=\"checked\"")
          }
        }
      }

    }

    "handling submitted answers to the who is the individual representing page" must {

      def performAction(formData: (String, String)*): Future[Result] =
        controller.whoIsIndividualRepresentingSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*))

      behave like redirectToStartWhenInvalidJourney(() => performAction(), isSubscribedJourney)

      "show a form error" when {

        def testFormError(submittedData: (String, String)*)(expectedErrorKey: String): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionDataWithIndividualTriageAnswers(None)))))
          }

          val result  = performAction(submittedData: _*)
          val content = contentAsString(result)

          status(result)          shouldBe BAD_REQUEST
          contentAsString(result) should include(message("who-are-you-reporting-for.title"))
          content                 should include(message(expectedErrorKey))
        }

        "no option has been selected" in {
          testFormError()("individualUserType.error.required")
        }

        "the option submitted is not valid" in {
          testFormError("individualUserType" -> "3")("individualUserType.error.invalid")
        }

      }

      "show an error" when {

        "the submitted value is valid but there is an error updating the session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionDataWithIndividualTriageAnswers(None)))))
            mockStoreSession(
              sessionDataWithIndividualTriageAnswers(
                IncompleteIndividualTriageAnswers.empty.copy(
                  individualUserType = Some(IndividualUserType.Self)
                )
              )
            )(Future.successful(Left(Error(""))))
          }

          val result = performAction("individualUserType" -> "0")
          checkIsTechnicalErrorPage(result)
        }

      }

      "show a dummy page" when {

        "the submitted value is not 'self' and the session is updated" in {
          List(
            1 -> IndividualUserType.Capacitor,
            2 -> IndividualUserType.PersonalRepresentative
          ).foreach {
            case (value, userType) =>
              withClue(s"For user type '$userType' and value '$value': ") {
                inSequence {
                  mockAuthWithNoRetrievals()
                  mockGetSession(Future.successful(Right(Some(sessionDataWithIndividualTriageAnswers(None)))))
                  mockStoreSession(
                    sessionDataWithIndividualTriageAnswers(
                      IncompleteIndividualTriageAnswers.empty.copy(
                        individualUserType = Some(userType)
                      )
                    )
                  )(Future.successful(Right(())))
                }

                val result = performAction("individualUserType" -> value.toString)
                status(result)          shouldBe OK
                contentAsString(result) shouldBe s"$userType not handled yet"
              }
          }

        }

      }

      "redirect to the how many properties page" when {

        "the submitted value is self and the session is updated" in {
          val (userType, userTypeValue) = IndividualUserType.Self -> 0
          val updatedAnswers            = IncompleteIndividualTriageAnswers.empty.copy(individualUserType = Some(userType))

          List(
            sessionDataWithIndividualTriageAnswers(None) ->
              sessionDataWithIndividualTriageAnswers(updatedAnswers),
            sessionDataWithIndividualTriageAnswers(IncompleteIndividualTriageAnswers.empty) ->
              sessionDataWithIndividualTriageAnswers(updatedAnswers),
            sessionDataWithDraftReturn(IncompleteIndividualTriageAnswers.empty) ->
              sessionDataWithDraftReturn(updatedAnswers)
          ).foreach {
            case (currentState, updatedState) =>
              withClue(s"With currentState $currentState and updatedState $updatedState: ") {
                inSequence {
                  mockAuthWithNoRetrievals()
                  mockGetSession(Future.successful(Right(Some(currentState))))
                  mockStoreSession(updatedState)(Future.successful(Right(())))
                }

                val result = performAction("individualUserType" -> userTypeValue.toString)
                checkIsRedirect(result, routes.CanTheyUseOurServiceController.howManyProperties())
              }

          }

        }

      }

      "redirect to the check your answers page" when {

        "the user has already complete the section" in {
          val currentCompleteAnswers =
            sample[CompleteIndividualTriageAnswers].copy(individualUserType = IndividualUserType.Capacitor)
          val (userType, userTypeValue) = IndividualUserType.Self -> 0
          val updatedCompleteAnswers    = currentCompleteAnswers.copy(individualUserType = userType)

          List(
            sessionDataWithIndividualTriageAnswers(currentCompleteAnswers) ->
              sessionDataWithIndividualTriageAnswers(updatedCompleteAnswers),
            sessionDataWithDraftReturn(currentCompleteAnswers) ->
              sessionDataWithDraftReturn(updatedCompleteAnswers)
          ).foreach {
            case (currentState, updatedState) =>
              withClue(s"For curentState $currentState and updatedState $updatedState") {
                inSequence {
                  mockAuthWithNoRetrievals()
                  mockGetSession(Future.successful(Right(Some(currentState))))
                  mockStoreSession(updatedState)(Future.successful(Right(())))
                }

                val result = performAction("individualUserType" -> userTypeValue.toString)
                checkIsRedirect(result, routes.CanTheyUseOurServiceController.checkYourAnswers())
              }

          }

        }

      }

      "not update the session" when {

        "the answer submitted is the same as the one already stored in session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              Future.successful(
                Right(
                  Some(
                    sessionDataWithIndividualTriageAnswers(
                      IncompleteIndividualTriageAnswers.empty.copy(
                        individualUserType = Some(IndividualUserType.Self)
                      )
                    )
                  )
                )
              )
            )
          }

          val result = performAction("individualUserType" -> "0")
          checkIsRedirect(result, routes.CanTheyUseOurServiceController.howManyProperties())
        }

      }

    }

    "handling requests to display the how many properties page" must {

      val requiredPreviousAnswers =
        IncompleteIndividualTriageAnswers.empty.copy(individualUserType = Some(sample[IndividualUserType]))

      def performAction(): Future[Result] = controller.howManyProperties()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(performAction, isSubscribedJourney)

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

      behave like redirectToStartWhenInvalidJourney(() => performAction(), isSubscribedJourney)

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
            case (result, updatedState) =>
              updatedState.fold(
                _ => {
                  status(result)          shouldBe OK
                  contentAsString(result) shouldBe "multiple disposals not handled yet"
                },
                _ => checkIsRedirect(result, routes.CanTheyUseOurServiceController.checkYourAnswers())
              )

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

      behave like redirectToStartWhenInvalidJourney(performAction, isSubscribedJourney)

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

      behave like redirectToStartWhenInvalidJourney(() => performAction(), isSubscribedJourney)

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

      behave like redirectToStartWhenInvalidJourney(performAction, isSubscribedJourney)

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

      behave like redirectToStartWhenInvalidJourney(() => performAction(), isSubscribedJourney)

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
              updatedState.fold(
                _ => {
                  status(result)          shouldBe OK
                  contentAsString(result) shouldBe "non residents not handled yet"
                },
                _ => checkIsRedirect(result, routes.CanTheyUseOurServiceController.checkYourAnswers())
              )
          })
        )
      )

    }

    "handling requests to display the did you dispose of a residential property page" must {

      val requiredPreviousAnswers =
        IncompleteIndividualTriageAnswers.empty.copy(wasAUKResident = Some(true))

      def performAction(): Future[Result] = controller.didYouDisposeOfAResidentialProperty()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(performAction, isSubscribedJourney)

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

      behave like redirectToStartWhenInvalidJourney(() => performAction(), isSubscribedJourney)

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
              updatedState.fold(
                _ => {
                  status(result)          shouldBe OK
                  contentAsString(result) shouldBe "individuals can only report on residential properties"
                },
                _ => checkIsRedirect(result, routes.CanTheyUseOurServiceController.checkYourAnswers())
              )

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

      val disposalDate = DisposalDate(LocalDate.of(2020, 1, 2))

      def performAction(): Future[Result] = controller.whenWasDisposalDate()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(performAction, isSubscribedJourney)

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
        (commonDateErrorScenarios("disposalDate") ::: List(
          // date in the future
          (
            Some(tomorrow.getDayOfMonth.toString),
            Some(tomorrow.getMonthValue.toString),
            Some(tomorrow.getYear.toString),
            "disposalDate.error.tooFarInFuture"
          ),
          // date does not exist
          (Some("31"), Some("2"), Some("2019"), "disposalDate.error.invalid")
        )).map {
          case (dayString, monthString, yearString, expectedErrorKey) =>
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
            case (result, updatedState) =>
              updatedState.fold(
                _ => {
                  status(result)          shouldBe OK
                  contentAsString(result) shouldBe s"disposal date was strictly before $earliestDisposalDate"
                },
                _ => checkIsRedirect(result, routes.CanTheyUseOurServiceController.checkYourAnswers())
              )

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
            (formData, DisposalDate(date), checks)
        }

      behave like redirectToStartWhenInvalidJourney(() => performAction(), isSubscribedJourney)

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

      val disposalDate = DisposalDate(today)

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

      behave like redirectToStartWhenInvalidJourney(performAction, isSubscribedJourney)

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

      val disposalDate = DisposalDate(today.minusDays(5L))

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
        (commonDateErrorScenarios("completionDate") ::: List(
          // date before disposal date
          (
            Some(dayBeforeDisposalDate.getDayOfMonth.toString),
            Some(dayBeforeDisposalDate.getMonthValue.toString),
            Some(dayBeforeDisposalDate.getYear.toString),
            "completionDate.error.tooFarInPast"
          ),
          // date in future
          (
            Some(tomorrow.getDayOfMonth.toString),
            Some(tomorrow.getMonthValue.toString),
            Some(tomorrow.getYear.toString),
            "completionDate.error.tooFarInFuture"
          )
        )).map {
          case (dayString, monthString, yearString, expectedErrorKey) =>
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

      behave like redirectToStartWhenInvalidJourney(() => performAction(), isSubscribedJourney)

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
                  mockGetSession(
                    Future.successful(
                      Right(
                        Some(
                          sessionDataWith(state)
                        )
                      )
                    )
                  )
                }

                checkIsRedirect(performAction(), expectedRedirect)
              }
          }

        "a question has not yet been answered and a draft return has not been created" in {
          test(sessionDataWithIndividualTriageAnswers)
        }

        "a question has not yet been answered and a draft return has been created" in {
          test(sessionDataWithDraftReturn)
        }

      }

      "show an error page" when {

        "all the questions have now been answered but the sessino data cannot be updated" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              Future.successful(
                Right(
                  Some(
                    sessionDataWithIndividualTriageAnswers(allQuestionsAnswered)
                  )
                )
              )
            )
            mockStoreSession(
              sessionDataWithIndividualTriageAnswers(completeTriageQuestions)
            )(Future.successful(Left(Error(""))))
          }

          checkIsTechnicalErrorPage(performAction())
        }
      }

      "display the page" when {

        def testIsCYAPagePage(result: Future[Result]) = {
          status(result)          shouldBe OK
          contentAsString(result) should include(message("triage.check-your-answers.title"))
        }

        "all the questions have now been answered and the session is updated when a draft return has not yet been created" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              Future.successful(
                Right(
                  Some(
                    sessionDataWithIndividualTriageAnswers(allQuestionsAnswered)
                  )
                )
              )
            )
            mockStoreSession(
              sessionDataWithIndividualTriageAnswers(completeTriageQuestions)
            )(Future.successful(Right(())))
          }

          testIsCYAPagePage(performAction())
        }

        "all the questions have now been answered and the session is updated when a draft return has been created" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              Future.successful(
                Right(
                  Some(
                    sessionDataWithDraftReturn(allQuestionsAnswered)
                  )
                )
              )
            )
            mockStoreSession(
              sessionDataWithDraftReturn(completeTriageQuestions)
            )(Future.successful(Right(())))
          }

          testIsCYAPagePage(performAction())
        }

        "all the questions have already been answered and a draft return has not yet been created" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              Future.successful(
                Right(
                  Some(
                    sessionDataWithIndividualTriageAnswers(completeTriageQuestions)
                  )
                )
              )
            )
          }

          testIsCYAPagePage(performAction())
        }

        "all the questions have already been answered and a draft return has been created" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              Future.successful(
                Right(
                  Some(
                    sessionDataWithDraftReturn(completeTriageQuestions)
                  )
                )
              )
            )
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

      val newDraftReturn = DraftReturn(uuid, subscribed.subscribedDetails.cgtReference, completeAnswers)

      val sessionDataWithNewDraftReturn = SessionData.empty.copy(
        journeyStatus = Some(
          subscribed.copy(
            individualTriageAnswers = None,
            draftReturn             = Some(newDraftReturn)
          )
        )
      )

      "redirect to the check your answers page" when {

        "the user has not answered all the questions in the triage section" in {
          val incompleteAnswers = sample[IncompleteIndividualTriageAnswers]

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionDataWithIndividualTriageAnswers(incompleteAnswers)))))
          }

          checkIsRedirect(performAction(), routes.CanTheyUseOurServiceController.checkYourAnswers())
        }

      }

      "show an error page" when {

        "there is a problem storing a draft return" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionDataWithIndividualTriageAnswers(completeAnswers)))))
            mockGetNextUUID(uuid)
            mockStoreDraftReturn(newDraftReturn)(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction())
        }

        "there is a problem updating the session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionDataWithIndividualTriageAnswers(completeAnswers)))))
            mockGetNextUUID(uuid)
            mockStoreDraftReturn(newDraftReturn)(Right(()))
            mockStoreSession(sessionDataWithNewDraftReturn)(Future.successful(Left(Error(""))))
          }

          checkIsTechnicalErrorPage(performAction())
        }

      }

      "redirect to the task list page" when {

        "the draft return is stored and the session is updated and a draft return had not already been created" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionDataWithIndividualTriageAnswers(completeAnswers)))))
            mockGetNextUUID(uuid)
            mockStoreDraftReturn(newDraftReturn)(Right(()))
            mockStoreSession(sessionDataWithNewDraftReturn)(Future.successful(Right(())))
          }

          checkIsRedirect(performAction(), returnsRoutes.TaskListController.taskList())
        }

        "the draft return is stored and the session is updated and a draft return had already been created" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionDataWithDraftReturn(completeAnswers)))))
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
    s"redirect to ${redirectToIfNotValidJourney.url}" when {

      "that question has not already answered" in {
        List(
          sessionDataWithIndividualTriageAnswers(setPreviousAnswer(requiredPreviousAnswers, None)),
          sessionDataWithDraftReturn(setPreviousAnswer(requiredPreviousAnswers, None))
        ).foreach { currentSession =>
          withClue(s"For currentSession $currentSession: ") {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                Future.successful(
                  Right(
                    Some(
                      sessionDataWithIndividualTriageAnswers(setPreviousAnswer(requiredPreviousAnswers, None))
                    )
                  )
                )
              )
            }

            checkIsRedirect(performAction(Seq.empty), redirectToIfNotValidJourney)
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
                mockGetSession(
                  Future.successful(Right(Some(sessionDataWithIndividualTriageAnswers(requiredPreviousAnswers))))
                )
              }

              val result  = performAction(formData)
              val content = contentAsString(result)

              status(result)          shouldBe BAD_REQUEST
              contentAsString(result) should include(message(pageTitleKey))
              content                 should include(message(expectedErrorMessageKey))
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
                mockGetSession(
                  Future.successful(Right(Some(sessionDataWithIndividualTriageAnswers(requiredPreviousAnswers))))
                )
                mockStoreSession(
                  sessionDataWithIndividualTriageAnswers(
                    updateCurrentIncompleteAnswer(requiredPreviousAnswers, Some(validValue))
                  )
                )(Future.successful(Left(Error(""))))
              }

              val result = performAction(formData)
              checkIsTechnicalErrorPage(result)
            }
        }
      }
    }

    "handle valid data" when {

      def test(
        formData: Seq[(String, String)],
        validValue: B,
        checkResult: (Future[Result], IndividualTriageAnswers) => Unit,
        setSessionData: IndividualTriageAnswers => SessionData,
        initialState: IndividualTriageAnswers,
        updatedState: IndividualTriageAnswers
      ): Unit =
        withClue(
          s"For:\n form data [${formData.mkString(";")}]\n value '$validValue'\n initialState\n '$initialState'\n updatedState '$updatedState': "
        ) {
          val currentSession = setSessionData(initialState)
          val updatedSession = setSessionData(updatedState)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(currentSession))))
            if (currentSession =!= updatedSession) mockStoreSession(updatedSession)(Future.successful(Right(())))
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
              sessionDataWithIndividualTriageAnswers,
              requiredPreviousAnswers,
              updateCurrentIncompleteAnswer(requiredPreviousAnswers, Some(validValue))
            )
          }

          s"submitting [${formData.mkString("; ")}] when the current state is incomplete when the " +
            "user has created a draft return" in {
            test(
              formData,
              validValue,
              checkResult,
              sessionDataWithDraftReturn,
              requiredPreviousAnswers,
              updateCurrentIncompleteAnswer(requiredPreviousAnswers, Some(validValue))
            )
          }

          s"submitting [${formData.mkString("; ")}] when the current state is complete when the " +
            "user has not yet created a draft return" in {
            test(
              formData,
              validValue,
              checkResult,
              sessionDataWithIndividualTriageAnswers,
              sampleCompleteJourney,
              updateCurrentCompleteAnswer(sampleCompleteJourney, validValue)
            )
          }

          s"submitting [${formData.mkString("; ")}] when the current state is complete when the " +
            "user has created a draft return" in {
            test(
              formData,
              validValue,
              checkResult,
              sessionDataWithDraftReturn,
              sampleCompleteJourney,
              updateCurrentCompleteAnswer(sampleCompleteJourney, validValue)
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
                mockGetSession(
                  Future.successful(
                    Right(
                      Some(
                        sessionDataWithIndividualTriageAnswers((updatedState))
                      )
                    )
                  )
                )
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
    s"redirect to ${redirectToIfNotValidJourney.url}" when {

      "that question has not already answered" in {
        List(
          sessionDataWithIndividualTriageAnswers(setPreviousAnswer(requiredPreviousAnswers, None)),
          sessionDataWithDraftReturn(setPreviousAnswer(requiredPreviousAnswers, None))
        ).foreach { currentSession =>
          withClue(s"For currentSession $currentSession: ") {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(Future.successful(Right(Some(currentSession))))
            }

            checkIsRedirect(performAction(), redirectToIfNotValidJourney)
          }
        }

      }

    }

    "display the page when no option has been selected before" in {
      List(
        sessionDataWithIndividualTriageAnswers(requiredPreviousAnswers),
        sessionDataWithDraftReturn(requiredPreviousAnswers)
      ).foreach { currentSession =>
        withClue(s"For currentSession $currentSession: ") {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(currentSession))))
          }

          val result = performAction()
          status(result)          shouldBe OK
          contentAsString(result) should include(message(pageTitleKey))
        }
      }
    }

    "display the page when an option has been selected before" in {
      List(
        sessionDataWithIndividualTriageAnswers(setCurrentAnswer(requiredPreviousAnswers, Some(sampleCurrentAnswer))),
        sessionDataWithDraftReturn(setCurrentAnswer(requiredPreviousAnswers, Some(sampleCurrentAnswer)))
      ).foreach { currentSession =>
        withClue(s"For currentSession $currentSession: ") {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(currentSession))))
          }

          val result  = performAction()
          val content = contentAsString(result)
          status(result) shouldBe OK

          content should include(message(pageTitleKey))
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
        sessionDataWithIndividualTriageAnswers(
          setCurrentAnswer(completeIndividualTriageAnswers, sampleCurrentAnswer)
        ),
        sessionDataWithDraftReturn(setCurrentAnswer(completeIndividualTriageAnswers, sampleCurrentAnswer))
      ).foreach { currentSession =>
        withClue(s"For currentSession $currentSession: ") {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(currentSession))))
          }

          val result  = performAction()
          val content = contentAsString(result)
          status(result) shouldBe OK

          content should include(message(pageTitleKey))
          prepopulatedContent(sampleCurrentAnswer).foreach(content should include(_))
        }
      }
    }
  }

  def commonDateErrorScenarios(dateKey: String) =
    List(
      (None, None, None, s"$dateKey.error.required"),
      (Some(""), None, None, s"$dateKey.error.required"),
      (None, Some(""), None, s"$dateKey.error.required"),
      (None, None, Some(""), s"$dateKey.error.required"),
      (Some(""), Some(""), None, s"$dateKey.error.required"),
      (Some(""), None, Some(""), s"$dateKey.error.required"),
      (None, Some(""), Some(""), s"$dateKey.error.required"),
      (Some(""), Some(""), Some(""), s"$dateKey.error.required"),
      // single field empty
      (None, Some("12"), Some("2020"), s"$dateKey-day.error.required"),
      (None, Some("100"), Some("-1000"), s"$dateKey-day.error.required"),
      (Some("1"), None, Some("2020"), s"$dateKey-month.error.required"),
      (Some("-1"), None, Some("1.2"), s"$dateKey-month.error.required"),
      (Some("1"), Some("12"), None, s"$dateKey-year.error.required"),
      (Some("0"), Some("-1"), None, s"$dateKey-year.error.required"),
      // two fields mossing
      (Some("1"), None, None, s"$dateKey-month.error.monthAndYearRequired"),
      (Some("0"), None, None, s"$dateKey-month.error.monthAndYearRequired"),
      (None, Some("12"), None, s"$dateKey-day.error.dayAndYearRequired"),
      (None, Some("-1"), None, s"$dateKey-day.error.dayAndYearRequired"),
      (None, None, Some("2020"), s"$dateKey-day.error.dayAndMonthRequired"),
      (None, None, Some("-1"), s"$dateKey-day.error.dayAndMonthRequired"),
      // day invalid and takes precedence over month and year
      (Some("0"), Some("12"), Some("2020"), s"$dateKey-day.error.invalid"),
      (Some("32"), Some("12"), Some("2020"), s"$dateKey-day.error.invalid"),
      (Some("-1"), Some("-1"), Some("-2020"), s"$dateKey-day.error.invalid"),
      (Some("1.2"), Some("3.4"), Some("4.5"), s"$dateKey-day.error.invalid"),
      // month invalid and takes precedence over year
      (Some("1"), Some("13"), Some("2020"), s"$dateKey-month.error.invalid"),
      (Some("1"), Some("0"), Some("0"), s"$dateKey-month.error.invalid"),
      (Some("1"), Some("-1"), Some("-6"), s"$dateKey-month.error.invalid"),
      (Some("1"), Some("1.2"), Some("3.4"), s"$dateKey-month.error.invalid"),
      // year invalid
      (Some("1"), Some("12"), Some("0"), s"$dateKey-year.error.invalid"),
      (Some("1"), Some("12"), Some("-1"), s"$dateKey-year.error.invalid"),
      (Some("1"), Some("12"), Some("1.2"), s"$dateKey-year.error.invalid"),
      // date does not exist
      (Some("31"), Some("2"), Some("2019"), s"$dateKey.error.invalid")
    )

}
