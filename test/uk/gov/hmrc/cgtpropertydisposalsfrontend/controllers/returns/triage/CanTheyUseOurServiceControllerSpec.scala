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

import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
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
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{IndividualTriageAnswers, IndividualUserType, NumberOfProperties}
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
        contentAsString(result) should include(message("triage.who-are-you-reporting-for.title"))
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
        contentAsString(result) should include(message("triage.who-are-you-reporting-for.title"))
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
          contentAsString(result) should include(message("triage.who-are-you-reporting-for.title"))
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
                      IndividualTriageAnswers(
                        Some(IndividualUserType.Self),
                        None
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
                            IndividualTriageAnswers(
                              Some(userType),
                              None
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
                      IndividualTriageAnswers(
                        Some(userType),
                        None
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
                    IndividualTriageAnswers(
                      Some(IndividualUserType.Self),
                      None
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
        contentAsString(result) should include(message("triage.number-of-properties.title"))
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
        contentAsString(result) should include(message("triage.number-of-properties.title"))
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
          contentAsString(result) should include(message("triage.number-of-properties.title"))
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

        "the submitted value is valid and the session is updated" in {
          List(
            0 -> NumberOfProperties.One,
            1 -> NumberOfProperties.MoreThanOne
          ).foreach {
            case (value, numberOfProperties) =>
              withClue(s"For user type '$numberOfProperties' and value '$value': ") {
                inSequence {
                  mockAuthWithNoRetrievals()
                  mockGetSession(Future.successful(Right(Some(sessionDataWithRequiredPreviousAnswers))))
                  mockStoreSession(
                    sessionDataWithRequiredPreviousAnswers.copy(
                      journeyStatus = Some(
                        subscribedWithRequiredPreviousAnswers.copy(
                          individualTriageAnswers = Some(
                            requiredPreviousAnswers.copy(numberOfProperties = Some(numberOfProperties))
                          )
                        )
                      )
                    )
                  )(Future.successful(Right(())))
                }

                val result = performAction("numberOfProperties" -> value.toString)
                status(result) shouldBe OK
                contentAsString(result) shouldBe s"Got number of properties $numberOfProperties"
              }

          }

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
          status(result) shouldBe OK
          contentAsString(result) shouldBe s"Got number of properties ${NumberOfProperties.One}"
        }

      }

    }

  }

}
