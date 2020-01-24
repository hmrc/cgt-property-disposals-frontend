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

import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.Configuration
import play.api.i18n.MessagesApi
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.{Call, Result}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.Subscribed
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, JourneyStatus, SessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore

import scala.concurrent.Future

class CanTheyUseOurServiceControllerSpec
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

  "The CanTheyUseOurServiceController" when {

    "handling requests to display the who is the individual representing page" must {

      def performAction(): Future[Result] = controller.whoIsIndividualRepresenting()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(performAction, isSubscribedJourney)

      "display the page when no option has been selected before" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            Future.successful(
              Right(
                Some(
                  SessionData.empty.copy(
                    journeyStatus = Some(sample[Subscribed].copy(individualTriageAnswers = None))
                  )
                )
              )
            )
          )
        }

        val result = performAction()
        status(result)          shouldBe OK
        contentAsString(result) should include(message("who-are-you-reporting-for.title"))
        contentAsString(result) should not include ("checked=\"checked\"")
      }

      "display the page when an option has been selected before" in {
        val individualTriageAnswers =
          sample[IndividualTriageAnswers].copy(individualUserType = Some(IndividualUserType.Self))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            Future.successful(
              Right(
                Some(
                  SessionData.empty.copy(
                    journeyStatus =
                      Some(sample[Subscribed].copy(individualTriageAnswers = Some(individualTriageAnswers)))
                  )
                )
              )
            )
          )
        }

        val result = performAction()
        status(result)          shouldBe OK
        contentAsString(result) should include(message("who-are-you-reporting-for.title"))
        contentAsString(result) should include("checked=\"checked\"")
      }

    }

    "handling submitted answers to the who is the individual representing page" must {

      def performAction(formData: (String, String)*): Future[Result] =
        controller.whoIsIndividualRepresentingSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*))

      behave like redirectToStartWhenInvalidJourney(() => performAction(), isSubscribedJourney)

      val subscribedWithNoAnswers  = sample[Subscribed].copy(individualTriageAnswers = None)
      val sessionDataWithNoAnswers = SessionData.empty.copy(journeyStatus            = Some(subscribedWithNoAnswers))

      "show a form error" when {

        def testFormError(submittedData: (String, String)*)(expectedErrorKey: String): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionDataWithNoAnswers))))
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
            mockGetSession(Future.successful(Right(Some(sessionDataWithNoAnswers))))
            mockStoreSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  subscribedWithNoAnswers.copy(
                    individualTriageAnswers = Some(
                      IndividualTriageAnswers.empty.copy(
                        individualUserType = Some(IndividualUserType.Self)
                      )
                    )
                  )
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
                  mockGetSession(Future.successful(Right(Some(sessionDataWithNoAnswers))))
                  mockStoreSession(
                    SessionData.empty.copy(
                      journeyStatus = Some(
                        subscribedWithNoAnswers.copy(
                          individualTriageAnswers = Some(
                            IndividualTriageAnswers.empty.copy(
                              individualUserType = Some(userType)
                            )
                          )
                        )
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

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionDataWithNoAnswers))))
            mockStoreSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  subscribedWithNoAnswers.copy(
                    individualTriageAnswers = Some(
                      IndividualTriageAnswers.empty.copy(
                        individualUserType = Some(userType)
                      )
                    )
                  )
                )
              )
            )(Future.successful(Right(())))
          }

          val result = performAction("individualUserType" -> userTypeValue.toString)
          checkIsRedirect(result, routes.CanTheyUseOurServiceController.howManyProperties())
        }

      }

      "not update the session" when {

        "the answer submitted is the same as the one already stored in session" in {
          val sessionData =
            SessionData.empty.copy(
              journeyStatus = Some(
                subscribedWithNoAnswers.copy(
                  individualTriageAnswers = Some(
                    IndividualTriageAnswers.empty.copy(
                      individualUserType = Some(IndividualUserType.Self)
                    )
                  )
                )
              )
            )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionData))))
          }

          val result = performAction("individualUserType" -> "0")
          checkIsRedirect(result, routes.CanTheyUseOurServiceController.howManyProperties())
        }

      }

    }

    "handling requests to display the how many properties page" must {

      val requiredPreviousAnswers =
        IndividualTriageAnswers.empty.copy(individualUserType = Some(sample[IndividualUserType]))

      def performAction(): Future[Result] = controller.howManyProperties()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(performAction, isSubscribedJourney)

      behave like displayIndividualTriagePageBehavior[IndividualUserType, NumberOfProperties](
        performAction
      )(requiredPreviousAnswers, routes.CanTheyUseOurServiceController.whoIsIndividualRepresenting())(
        { case (i, t) => i.copy(individualUserType = t) },
        { case (i, n) => i.copy(numberOfProperties = n) }
      )(NumberOfProperties.One)(
        "numberOfProperties.title",
        _ => List("checked=\"checked\"")
      )

    }

    "handling submitted answers to the how many properties page" must {

      def performAction(formData: (String, String)*): Future[Result] =
        controller.howManyPropertiesSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*))

      val requiredPreviousAnswers =
        IndividualTriageAnswers.empty.copy(individualUserType = Some(sample[IndividualUserType]))

      behave like redirectToStartWhenInvalidJourney(() => performAction(), isSubscribedJourney)

      behave like submitIndividualTriagePageBehavior[IndividualUserType, NumberOfProperties](
        performAction
      )(
        requiredPreviousAnswers,
        routes.CanTheyUseOurServiceController.whoIsIndividualRepresenting()
      )(
        { case (i, t) => i.copy(individualUserType = t) },
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
          (List("numberOfProperties" -> "1"), NumberOfProperties.MoreThanOne, { result: Future[Result] =>
            status(result)          shouldBe OK
            contentAsString(result) shouldBe "multiple disposals not handled yet"
          }),
          (List("numberOfProperties" -> "0"), NumberOfProperties.One, { result: Future[Result] =>
            checkIsRedirect(result, routes.CanTheyUseOurServiceController.howDidYouDisposeOfProperty())
          })
        )
      )

    }

    "handling requests to display how did you dispose of your property page" must {

      val requiredPreviousAnswers =
        IndividualTriageAnswers.empty.copy(numberOfProperties = Some(NumberOfProperties.One))

      def performAction(): Future[Result] = controller.howDidYouDisposeOfProperty()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(performAction, isSubscribedJourney)

      behave like displayIndividualTriagePageBehavior[NumberOfProperties, DisposalMethod](
        performAction
      )(requiredPreviousAnswers, routes.CanTheyUseOurServiceController.howManyProperties())(
        { case (i, n) => i.copy(numberOfProperties = n) },
        { case (i, m) => i.copy(disposalMethod     = m) }
      )(DisposalMethod.Gifted)(
        "disposalMethod.title",
        _ => List("checked=\"checked\"")
      )

    }

    "handling submitted answers to the how did you dispose of your property page" must {

      def performAction(formData: (String, String)*): Future[Result] =
        controller.howDidYouDisposeOfPropertySubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*))

      val requiredPreviousAnswers =
      IndividualTriageAnswers.empty.copy(numberOfProperties = Some(NumberOfProperties.One))

      behave like redirectToStartWhenInvalidJourney(() => performAction(), isSubscribedJourney)

      behave like submitIndividualTriagePageBehavior[NumberOfProperties, DisposalMethod](
        performAction
      )(
        requiredPreviousAnswers,
        routes.CanTheyUseOurServiceController.howManyProperties()
      )(
        { case (i, n) => i.copy(numberOfProperties = n) },
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
          (List("disposalMethod" -> "0"), DisposalMethod.Sold, { result: Future[Result] =>
            checkIsRedirect(result, routes.CanTheyUseOurServiceController.wereYouAUKResident())
          }),
          (List("disposalMethod" -> "1"), DisposalMethod.Gifted, { result: Future[Result] =>
            checkIsRedirect(result, routes.CanTheyUseOurServiceController.wereYouAUKResident())
          })
        )
      )

    }

    "handling requests to display the were you a uk resident page" must {

      val requiredPreviousAnswers =
        IndividualTriageAnswers.empty.copy(disposalMethod = Some(DisposalMethod.Sold))

      def performAction(): Future[Result] = controller.wereYouAUKResident()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(performAction, isSubscribedJourney)

      behave like displayIndividualTriagePageBehavior[DisposalMethod, Boolean](
        performAction
      )(requiredPreviousAnswers, routes.CanTheyUseOurServiceController.howDidYouDisposeOfProperty())(
        { case (i, m) => i.copy(disposalMethod = m) },
        { case (i, w) => i.copy(wasAUKResident = w) }
      )(true)(
        "wereYouAUKResident.title",
        _ => List("checked=\"checked\"")
      )

    }

    "handling submitted answers to the were you a uk resident page" must {

      def performAction(formData: (String, String)*): Future[Result] =
        controller.wereYouAUKResidentSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*))

      val requiredPreviousAnswers =
        IndividualTriageAnswers.empty.copy(disposalMethod = Some(DisposalMethod.Sold))

      behave like redirectToStartWhenInvalidJourney(() => performAction(), isSubscribedJourney)

      behave like submitIndividualTriagePageBehavior[DisposalMethod, Boolean](
        performAction
      )(
        requiredPreviousAnswers,
        routes.CanTheyUseOurServiceController.howDidYouDisposeOfProperty()
      )(
        { case (i, m) => i.copy(disposalMethod = m) },
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
          (List("wereYouAUKResident" -> "true"), true, { result: Future[Result] =>
            checkIsRedirect(result, routes.CanTheyUseOurServiceController.didYouDisposeOfAResidentialProperty())
          }),
          (List("wereYouAUKResident" -> "false"), false, { result: Future[Result] =>
            status(result)          shouldBe OK
            contentAsString(result) shouldBe "non residents not handled yet"
          })
        )
      )

    }

    "handling requests to display the did you dispose of a residential property page" must {

      val requiredPreviousAnswers =
        IndividualTriageAnswers.empty.copy(wasAUKResident = Some(true))

      def performAction(): Future[Result] = controller.didYouDisposeOfAResidentialProperty()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(performAction, isSubscribedJourney)

      behave like displayIndividualTriagePageBehavior[Boolean, Boolean](
        performAction
      )(requiredPreviousAnswers, routes.CanTheyUseOurServiceController.wereYouAUKResident())(
        { case (i, w) => i.copy(wasAUKResident = w) },
        { case (i, w) => i.copy(wasResidentialProperty = w) }
      )(true)(
        "didYouDisposeOfResidentialProperty.title",
        _ => List("checked=\"checked\"")
      )

    }

    "handling submitted answers to the did you dispose of a residential property page" must {

      def performAction(formData: (String, String)*): Future[Result] =
        controller.didYouDisposeOfAResidentialPropertySubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*))

      val requiredPreviousAnswers =
        IndividualTriageAnswers.empty.copy(wasAUKResident = Some(true))

      behave like redirectToStartWhenInvalidJourney(() => performAction(), isSubscribedJourney)

      behave like submitIndividualTriagePageBehavior[Boolean, Boolean](
        performAction
      )(
        requiredPreviousAnswers,
        routes.CanTheyUseOurServiceController.wereYouAUKResident()
      )(
        { case (i, w) => i.copy(wasAUKResident = w) },
        { case (i, w) => i.copy(wasResidentialProperty = w) }
      )(
        "didYouDisposeOfResidentialProperty.title"
      )(
        formErrorScenarios = List(
          List.empty -> "didYouDisposeOfResidentialProperty.error.required",
          List("didYouDisposeOfResidentialProperty" -> "2") -> "didYouDisposeOfResidentialProperty.error.boolean"
        )
      )(
        validValueScenarios = List(
          (List("didYouDisposeOfResidentialProperty" -> "true"), true, { result: Future[Result] =>
            checkIsRedirect(result, routes.CanTheyUseOurServiceController.whenWasDisposalDate())
          }),
          (List("didYouDisposeOfResidentialProperty" -> "false"), false, { result: Future[Result] =>
            status(result)          shouldBe OK
            contentAsString(result) shouldBe "individuals can only report on residential properties"
          })
        )
      )

    }

    "handling requests to display the when was disposal date page" must {

      val requiredPreviousAnswers =
        IndividualTriageAnswers.empty.copy(
          individualUserType     = Some(sample[IndividualUserType]),
          numberOfProperties     = Some(NumberOfProperties.One),
          disposalMethod         = Some(DisposalMethod.Gifted),
          wasAUKResident         = Some(true),
          wasResidentialProperty = Some(true)
        )

      val disposalDate = DisposalDate(LocalDate.of(2020, 1, 2))

      def performAction(): Future[Result] = controller.whenWasDisposalDate()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(performAction, isSubscribedJourney)

      behave like displayIndividualTriagePageBehavior[Boolean, DisposalDate](
        performAction
      )(requiredPreviousAnswers, routes.CanTheyUseOurServiceController.didYouDisposeOfAResidentialProperty())(
        { case (i, w) => i.copy(wasResidentialProperty = w) },
        { case (i, d) => i.copy(disposalDate           = d) }
      )(disposalDate)(
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
        IndividualTriageAnswers.empty.copy(
          individualUserType     = Some(sample[IndividualUserType]),
          numberOfProperties     = Some(NumberOfProperties.One),
          disposalMethod         = Some(DisposalMethod.Gifted),
          wasAUKResident         = Some(true),
          wasResidentialProperty = Some(true)
        )

      val formErrorScenarios = List(
        // empty data
        (None, None, None, "disposalDate.error.required"),
        (Some(""), None, None, "disposalDate.error.required"),
        (None, Some(""), None, "disposalDate.error.required"),
        (None, None, Some(""), "disposalDate.error.required"),
        (Some(""), Some(""), None, "disposalDate.error.required"),
        (Some(""), None, Some(""), "disposalDate.error.required"),
        (None, Some(""), Some(""), "disposalDate.error.required"),
        (Some(""), Some(""), Some(""), "disposalDate.error.required"),
        // single field empty
        (None, Some("12"), Some("2020"), "disposalDate-day.error.required"),
        (None, Some("100"), Some("-1000"), "disposalDate-day.error.required"),
        (Some("1"), None, Some("2020"), "disposalDate-month.error.required"),
        (Some("-1"), None, Some("1.2"), "disposalDate-month.error.required"),
        (Some("1"), Some("12"), None, "disposalDate-year.error.required"),
        (Some("0"), Some("-1"), None, "disposalDate-year.error.required"),
        // two fields mossing
        (Some("1"), None, None, "disposalDate-month.error.monthAndYearRequired"),
        (Some("0"), None, None, "disposalDate-month.error.monthAndYearRequired"),
        (None, Some("12"), None, "disposalDate-day.error.dayAndYearRequired"),
        (None, Some("-1"), None, "disposalDate-day.error.dayAndYearRequired"),
        (None, None, Some("2020"), "disposalDate-day.error.dayAndMonthRequired"),
        (None, None, Some("-1"), "disposalDate-day.error.dayAndMonthRequired"),
        // day invalid and takes precedence over month and year
        (Some("0"), Some("12"), Some("2020"), "disposalDate-day.error.invalid"),
        (Some("32"), Some("12"), Some("2020"), "disposalDate-day.error.invalid"),
        (Some("-1"), Some("-1"), Some("-2020"), "disposalDate-day.error.invalid"),
        (Some("1.2"), Some("3.4"), Some("4.5"), "disposalDate-day.error.invalid"),
        // month invalid and takes precedence over year
        (Some("1"), Some("13"), Some("2020"), "disposalDate-month.error.invalid"),
        (Some("1"), Some("0"), Some("0"), "disposalDate-month.error.invalid"),
        (Some("1"), Some("-1"), Some("-6"), "disposalDate-month.error.invalid"),
        (Some("1"), Some("1.2"), Some("3.4"), "disposalDate-month.error.invalid"),
        // year invalid
        (Some("1"), Some("12"), Some("0"), "disposalDate-year.error.invalid"),
        (Some("1"), Some("12"), Some("-1"), "disposalDate-year.error.invalid"),
        (Some("1"), Some("12"), Some("1.2"), "disposalDate-year.error.invalid"),
        // date in the future
        (
          Some(tomorrow.getDayOfMonth.toString),
          Some(tomorrow.getMonthValue.toString),
          Some(tomorrow.getYear.toString),
          "disposalDate.error.tooFarInFuture"
        ),
        // date does not exist
        (Some("31"), Some("2"), Some("2019"), "disposalDate.error.invalid")
      ).map {
        case (dayString, monthString, yearString, expectedErrorKey) =>
          val formData = List(
            "disposalDate-day"   -> dayString,
            "disposalDate-month" -> monthString,
            "disposalDate-year"  -> yearString
          ).collect { case (id, Some(input)) => id -> input }

          formData -> expectedErrorKey
      }

      val validValueScenarios =
        List[(LocalDate, Future[Result] => Unit)](
          earliestDisposalDate.minusDays(1L) -> { result =>
            contentAsString(result) shouldBe s"disposal date was strictly before $earliestDisposalDate"
          },
          earliestDisposalDate -> { result =>
            contentAsString(result) shouldBe s"disposal date was on or after $earliestDisposalDate"
          },
          earliestDisposalDate.plusDays(1L) -> { result =>
            contentAsString(result) shouldBe s"disposal date was on or after $earliestDisposalDate"
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
        routes.CanTheyUseOurServiceController.didYouDisposeOfAResidentialProperty()
      )(
        { case (i, w) => i.copy(wasResidentialProperty = w) },
        { case (i, d) => i.copy(disposalDate           = d) }
      )(
        "disposalDate.title"
      )(
        formErrorScenarios
      )(
        validValueScenarios
      )

    }
  }

  def submitIndividualTriagePageBehavior[A, B](performAction: Seq[(String, String)] => Future[Result])(
    requiredPreviousAnswers: IndividualTriageAnswers,
    redirectToIfNotValidJourney: => Call
  )(
    setPreviousAnswer: (IndividualTriageAnswers, Option[A]) => IndividualTriageAnswers,
    setCurrentAnswer: (IndividualTriageAnswers, Option[B]) => IndividualTriageAnswers
  )(pageTitleKey: String)(
    formErrorScenarios: Seq[(Seq[(String, String)], String)]
  )(validValueScenarios: Seq[(Seq[(String, String)], B, Future[Result] => Unit)]): Unit = {
    val subscribed = sample[Subscribed]

    val sessionDataWithRequiredPreviousAnswers = SessionData.empty
      .copy(journeyStatus = Some(subscribed.copy(individualTriageAnswers = Some(requiredPreviousAnswers))))

    s"redirect to ${redirectToIfNotValidJourney.url}" when {

      "that question has not already answered" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            Future.successful(
              Right(
                Some(
                  SessionData.empty.copy(
                    journeyStatus = Some(
                      subscribed.copy(
                        individualTriageAnswers = Some(setPreviousAnswer(requiredPreviousAnswers, None))
                      )
                    )
                  )
                )
              )
            )
          )
        }

        checkIsRedirect(performAction(Seq.empty), redirectToIfNotValidJourney)
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
                mockGetSession(Future.successful(Right(Some(sessionDataWithRequiredPreviousAnswers))))
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
                mockGetSession(Future.successful(Right(Some(sessionDataWithRequiredPreviousAnswers))))
                mockStoreSession(
                  sessionDataWithRequiredPreviousAnswers.copy(
                    journeyStatus = Some(
                      subscribed.copy(
                        individualTriageAnswers = Some(
                          setCurrentAnswer(requiredPreviousAnswers, Some(validValue))
                        )
                      )
                    )
                  )
                )(Future.successful(Left(Error(""))))
              }

              val result = performAction(formData)
              checkIsTechnicalErrorPage(result)
            }
        }
      }
    }

    "handle valid values" in {
      validValueScenarios.foreach {
        case (formData, validValue, checkResult) =>
          withClue(s"For form data [${formData.mkString(";")}] and value '$validValue': ") {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(Future.successful(Right(Some(sessionDataWithRequiredPreviousAnswers))))
              mockStoreSession(
                sessionDataWithRequiredPreviousAnswers.copy(
                  journeyStatus = Some(
                    subscribed.copy(
                      individualTriageAnswers = Some(
                        setCurrentAnswer(requiredPreviousAnswers, Some(validValue))
                      )
                    )
                  )
                )
              )(Future.successful(Right(())))
            }

            val result = performAction(formData)
            checkResult(result)
          }
      }
    }

    "not update the session" when {

      "the answer submitted is the same as the one already stored in session" in {
        validValueScenarios.foreach {
          case (formData, validValue, checkResult) =>
            withClue(s"For form data [${formData.mkString(";")}] and value '$validValue': ") {
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  Future.successful(
                    Right(
                      Some(
                        sessionDataWithRequiredPreviousAnswers.copy(
                          journeyStatus = Some(
                            subscribed.copy(
                              individualTriageAnswers = Some(
                                setCurrentAnswer(requiredPreviousAnswers, Some(validValue))
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              }

              val result = performAction(formData)
              checkResult(result)
            }

        }

      }
    }
  }

  def displayIndividualTriagePageBehavior[A, B](performAction: () => Future[Result])(
    requiredPreviousAnswers: IndividualTriageAnswers,
    redirectToIfNotValidJourney: => Call
  )(
    setPreviousAnswer: (IndividualTriageAnswers, Option[A]) => IndividualTriageAnswers,
    setCurrentAnswer: (IndividualTriageAnswers, Option[B]) => IndividualTriageAnswers
  )(sampleCurrentAnswer: B)(
    pageTitleKey: String,
    prepopulatedContent: B => List[String]
  ): Unit = {
    val subscribed = sample[Subscribed]

    s"redirect to ${redirectToIfNotValidJourney.url}" when {

      "that question has not already answered" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            Future.successful(
              Right(
                Some(
                  SessionData.empty.copy(
                    journeyStatus = Some(
                      subscribed.copy(
                        individualTriageAnswers = Some(setPreviousAnswer(requiredPreviousAnswers, None))
                      )
                    )
                  )
                )
              )
            )
          )
        }

        checkIsRedirect(performAction(), redirectToIfNotValidJourney)
      }

    }

    "display the page when no option has been selected before" in {
      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(
          Future.successful(
            Right(
              Some(
                SessionData.empty
                  .copy(journeyStatus = Some(subscribed.copy(individualTriageAnswers = Some(requiredPreviousAnswers))))
              )
            )
          )
        )
      }

      val result = performAction()
      status(result)          shouldBe OK
      contentAsString(result) should include(message(pageTitleKey))
    }

    "display the page when an option has been selected before" in {
      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(
          Future.successful(
            Right(
              Some(
                SessionData.empty.copy(
                  journeyStatus = Some(
                    sample[Subscribed].copy(individualTriageAnswers =
                      Some(setCurrentAnswer(requiredPreviousAnswers, Some(sampleCurrentAnswer)))
                    )
                  )
                )
              )
            )
          )
        )
      }

      val result  = performAction()
      val content = contentAsString(result)
      status(result) shouldBe OK

      content should include(message(pageTitleKey))
      prepopulatedContent(sampleCurrentAnswer).foreach(content should include(_))
    }

  }

}
