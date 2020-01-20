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
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.Subscribed
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{DisposalDate, IndividualTriageAnswers, IndividualUserType, NumberOfProperties}
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

      val subscribedWithRequiredPreviousAnswers =
        sample[Subscribed].copy(individualTriageAnswers = Some(requiredPreviousAnswers))

      def performAction(): Future[Result] = controller.howManyProperties()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(performAction, isSubscribedJourney)

      "redirect to the who is individual representing page" when {

        "that question has not already answered" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              Future.successful(
                Right(
                  Some(
                    SessionData.empty.copy(
                      journeyStatus =
                        Some(sample[Subscribed].copy(individualTriageAnswers = Some(IndividualTriageAnswers.empty)))
                    )
                  )
                )
              )
            )
          }

          checkIsRedirect(performAction(), routes.CanTheyUseOurServiceController.whoIsIndividualRepresenting())
        }

      }

      "display the page when no option has been selected before" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            Future.successful(
              Right(
                Some(
                  SessionData.empty.copy(
                    journeyStatus = Some(subscribedWithRequiredPreviousAnswers)
                  )
                )
              )
            )
          )
        }

        val result = performAction()
        status(result)          shouldBe OK
        contentAsString(result) should include(message("numberOfProperties.title"))
        contentAsString(result) should not include ("checked=\"checked\"")
      }

      "display the page when an option has been selected before" in {
        val individualTriageAnswers =
          requiredPreviousAnswers.copy(numberOfProperties = Some(NumberOfProperties.One))

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
        contentAsString(result) should include(message("numberOfProperties.title"))
        contentAsString(result) should include("checked=\"checked\"")
      }

    }

    "handling submitted answers to the how many properties page" must {

      def performAction(formData: (String, String)*): Future[Result] =
        controller.howManyPropertiesSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*))

      behave like redirectToStartWhenInvalidJourney(() => performAction(), isSubscribedJourney)

      val requiredPreviousAnswers =
        IndividualTriageAnswers.empty.copy(individualUserType = Some(sample[IndividualUserType]))

      val subscribedWithRequiredPreviousAnswers =
        sample[Subscribed].copy(individualTriageAnswers = Some(requiredPreviousAnswers))

      val sessionDataWithRequiredPreviousAnswers =
        SessionData.empty.copy(journeyStatus = Some(subscribedWithRequiredPreviousAnswers))

      "show a form error" when {

        def testFormError(submittedData: (String, String)*)(expectedErrorKey: String): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionDataWithRequiredPreviousAnswers))))
          }

          val result  = performAction(submittedData: _*)
          val content = contentAsString(result)

          status(result)          shouldBe BAD_REQUEST
          contentAsString(result) should include(message("numberOfProperties.title"))
          content                 should include(message(expectedErrorKey))
        }

        "no option has been selected" in {
          testFormError()("numberOfProperties.error.required")
        }

        "the option submitted is not valid" in {
          testFormError("numberOfProperties" -> "2")("numberOfProperties.error.invalid")
        }

      }

      "show an error" when {

        "the submitted value is valid but there is an error updating the session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionDataWithRequiredPreviousAnswers))))
            mockStoreSession(
              sessionDataWithRequiredPreviousAnswers.copy(
                journeyStatus = Some(
                  subscribedWithRequiredPreviousAnswers.copy(
                    individualTriageAnswers = Some(
                      requiredPreviousAnswers.copy(numberOfProperties = Some(NumberOfProperties.One))
                    )
                  )
                )
              )
            )(Future.successful(Left(Error(""))))
          }

          val result = performAction("numberOfProperties" -> "0")
          checkIsTechnicalErrorPage(result)
        }

      }

      "show a dummy page" when {

        "the submitted value is more than one and the session is updated" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionDataWithRequiredPreviousAnswers))))
            mockStoreSession(
              sessionDataWithRequiredPreviousAnswers.copy(
                journeyStatus = Some(
                  subscribedWithRequiredPreviousAnswers.copy(
                    individualTriageAnswers = Some(
                      requiredPreviousAnswers.copy(numberOfProperties = Some(NumberOfProperties.MoreThanOne))
                    )
                  )
                )
              )
            )(Future.successful(Right(())))
          }

          val result = performAction("numberOfProperties" -> "1")
          status(result)          shouldBe OK
          contentAsString(result) shouldBe "multiple disposals not handled yet"
        }

      }

      "redirect to the disposal date page" when {

        "the submitted value is one and the session is updated" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionDataWithRequiredPreviousAnswers))))
            mockStoreSession(
              sessionDataWithRequiredPreviousAnswers.copy(
                journeyStatus = Some(
                  subscribedWithRequiredPreviousAnswers.copy(
                    individualTriageAnswers = Some(
                      requiredPreviousAnswers.copy(numberOfProperties = Some(NumberOfProperties.One))
                    )
                  )
                )
              )
            )(Future.successful(Right(())))
          }

          val result = performAction("numberOfProperties" -> "0")
          checkIsRedirect(result, routes.CanTheyUseOurServiceController.whenWasDisposalDate())
        }

      }

      "not update the session" when {

        "the answer submitted is the same as the one already stored in session" in {
          val sessionData =
            sessionDataWithRequiredPreviousAnswers.copy(
              journeyStatus = Some(
                subscribedWithRequiredPreviousAnswers.copy(
                  individualTriageAnswers = Some(
                    requiredPreviousAnswers.copy(numberOfProperties = Some(NumberOfProperties.One))
                  )
                )
              )
            )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionDataWithRequiredPreviousAnswers))))
            mockStoreSession(sessionData)(Future.successful(Right(())))
          }

          val result = performAction("numberOfProperties" -> "0")
          checkIsRedirect(result, routes.CanTheyUseOurServiceController.whenWasDisposalDate())
        }

      }

    }

    "handling requests to display the when was disposal date page" must {

      val requiredPreviousAnswers =
        IndividualTriageAnswers.empty.copy(
          individualUserType = Some(sample[IndividualUserType]),
          numberOfProperties = Some(NumberOfProperties.One)
        )

      val subscribedWithRequiredPreviousAnswers =
        sample[Subscribed].copy(individualTriageAnswers = Some(requiredPreviousAnswers))

      def performAction(): Future[Result] = controller.whenWasDisposalDate()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(performAction, isSubscribedJourney)

      "redirect to the number of properties page" when {

        "that question has not already answered" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              Future.successful(
                Right(
                  Some(
                    SessionData.empty.copy(
                      journeyStatus = Some(
                        subscribedWithRequiredPreviousAnswers.copy(
                          individualTriageAnswers = Some(
                            requiredPreviousAnswers.copy(numberOfProperties = None)
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          }

          checkIsRedirect(performAction(), routes.CanTheyUseOurServiceController.howManyProperties())
        }

      }

      "display the page when no option has been selected before" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            Future.successful(
              Right(
                Some(
                  SessionData.empty.copy(
                    journeyStatus = Some(subscribedWithRequiredPreviousAnswers)
                  )
                )
              )
            )
          )
        }

        val result = performAction()
        status(result)          shouldBe OK
        contentAsString(result) should include(message("disposalDate.title"))
      }

      "display the page when an option has been selected before" in {
        val disposalDate = LocalDate.of(2020, 1, 2)
        val individualTriageAnswers =
          requiredPreviousAnswers.copy(disposalDate = Some(DisposalDate(disposalDate)))

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
        contentAsString(result) should include(message("disposalDate.title"))
        contentAsString(result) should include(s"""value="${disposalDate.getDayOfMonth()}"""")
        contentAsString(result) should include(s"""value="${disposalDate.getMonthValue()}"""")
        contentAsString(result) should include(s"""value="${disposalDate.getYear()}"""")
      }

    }

    "handling submitted disposal dates" must {

      def performAction(formData: (String, String)*): Future[Result] =
        controller.whenWasDisposalDateSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*))

      def performActionWithDate(date: LocalDate): Future[Result] =
        performAction(
          "disposalDate-day"   -> date.getDayOfMonth.toString,
          "disposalDate-month" -> date.getMonthValue.toString,
          "disposalDate-year"  -> date.getYear.toString
        )

      behave like redirectToStartWhenInvalidJourney(() => performAction(), isSubscribedJourney)

      val requiredPreviousAnswers =
        IndividualTriageAnswers.empty.copy(
          individualUserType = Some(IndividualUserType.Self),
          numberOfProperties = Some(NumberOfProperties.One)
        )

      val subscribedWithRequiredPreviousAnswers =
        sample[Subscribed].copy(individualTriageAnswers = Some(requiredPreviousAnswers))

      val sessionDataWithRequiredPreviousAnswers =
        SessionData.empty.copy(journeyStatus = Some(subscribedWithRequiredPreviousAnswers))

      val tomorrow = today.plusDays(1L)

      "show a form error" when {

        def testFormError(submittedData: (String, String)*)(expectedErrorKey: String): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionDataWithRequiredPreviousAnswers))))
          }

          val result  = performAction(submittedData: _*)
          val content = contentAsString(result)

          status(result)          shouldBe BAD_REQUEST
          contentAsString(result) should include(message("disposalDate.title"))
          content                 should include(message(expectedErrorKey))
        }

        "the data submitted was not valid" in {
          List(
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
          ).foreach {
            case (dayString, monthString, yearString, expectedErrorKey) =>
              withClue(
                s"For (day, month, year) = ($dayString, $monthString, $yearString) and expectedErrorKey '$expectedErrorKey': "
              ) {
                val formInput =
                  List(
                    "disposalDate-day"   -> dayString,
                    "disposalDate-month" -> monthString,
                    "disposalDate-year"  -> yearString
                  ).collect { case (id, Some(input)) => id -> input }

                testFormError(formInput: _*)(expectedErrorKey)
              }
          }
        }

      }

      "show an error" when {

        "the submitted value is valid but there is an error updating the session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionDataWithRequiredPreviousAnswers))))
            mockStoreSession(
              sessionDataWithRequiredPreviousAnswers.copy(
                journeyStatus = Some(
                  subscribedWithRequiredPreviousAnswers.copy(
                    individualTriageAnswers = Some(
                      requiredPreviousAnswers.copy(disposalDate = Some(DisposalDate(today)))
                    )
                  )
                )
              )
            )(Future.successful(Left(Error(""))))
          }

          val result = performActionWithDate(today)
          checkIsTechnicalErrorPage(result)
        }

      }

      "show a dummy page" when {

        def test(disposalDate: LocalDate, expectedContent: String): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionDataWithRequiredPreviousAnswers))))
            mockStoreSession(
              sessionDataWithRequiredPreviousAnswers.copy(
                journeyStatus = Some(
                  subscribedWithRequiredPreviousAnswers.copy(
                    individualTriageAnswers = Some(
                      requiredPreviousAnswers.copy(disposalDate = Some(DisposalDate(disposalDate)))
                    )
                  )
                )
              )
            )(Future.successful(Right(())))
          }

          val result = performActionWithDate(disposalDate)
          status(result)          shouldBe OK
          contentAsString(result) shouldBe expectedContent
        }

        "the submitted value is strictly before the configured earliest disposal date and the session is updated" in {
          test(
            earliestDisposalDate.minusDays(1L),
            s"disposal date was strictly before $earliestDisposalDate"
          )
        }

        "the submitted value is on the configured earliest disposal date and the session is updated" in {
          test(
            earliestDisposalDate,
            s"disposal date was on or after $earliestDisposalDate"
          )
        }

        "the submitted value is after the configured earliest disposal date and the session is updated" in {
          test(
            earliestDisposalDate.plusDays(1L),
            s"disposal date was on or after $earliestDisposalDate"
          )
        }

      }

      "not update the session" when {

        "the answer submitted is the same as the one already stored in session" in {
          val sessionData =
            sessionDataWithRequiredPreviousAnswers.copy(
              journeyStatus = Some(
                subscribedWithRequiredPreviousAnswers.copy(
                  individualTriageAnswers = Some(
                    requiredPreviousAnswers.copy(disposalDate = Some(DisposalDate(today)))
                  )
                )
              )
            )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionDataWithRequiredPreviousAnswers))))
            mockStoreSession(sessionData)(Future.successful(Right(())))
          }

          val result = performActionWithDate(today)
          status(result)          shouldBe OK
          contentAsString(result) shouldBe s"disposal date was on or after $earliestDisposalDate"
        }

      }

    }

  }

}
