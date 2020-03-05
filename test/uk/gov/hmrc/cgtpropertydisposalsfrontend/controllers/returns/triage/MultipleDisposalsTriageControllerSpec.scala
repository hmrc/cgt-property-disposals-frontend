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
import play.api.http.Status.BAD_REQUEST
import play.api.i18n.MessagesApi
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.triage
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, StartingNewDraftReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Country
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.{IndividualName, TrustName}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.SubscribedDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.IndividualUserType.Self
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{IndividualUserType, _}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.MultipleDisposalsTriageAnswers.{IncompleteMultipleDisposalsAnswers, _}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.IncompleteSingleDisposalTriageAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, JourneyStatus, SessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore

import scala.concurrent.Future

class MultipleDisposalsTriageControllerSpec
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

  lazy val controller = instanceOf[MultipleDisposalsTriageController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  def isValidJourney(journeyStatus: JourneyStatus): Boolean = journeyStatus match {
    case r: StartingNewDraftReturn if (r.newReturnTriageAnswers.isLeft) => true
    case _                                                              => false
  }

  def sessionDataWithStartingNewDraftReturn(
    multipleDisposalsAnswers: MultipleDisposalsTriageAnswers,
    name: Either[TrustName, IndividualName] = Right(sample[IndividualName])
  ): (SessionData, StartingNewDraftReturn) = {
    val startingNewDraftReturn = sample[StartingNewDraftReturn].copy(
      newReturnTriageAnswers = Left(multipleDisposalsAnswers),
      subscribedDetails      = sample[SubscribedDetails].copy(name = name)
    )
    SessionData.empty.copy(journeyStatus = Some(startingNewDraftReturn)) -> startingNewDraftReturn
  }

  def sessionWithState(answers: IncompleteMultipleDisposalsAnswers, numberOfProperties: Int): SessionData = {
    val (session, journey) = sessionDataWithStartingNewDraftReturn(answers)
    session.copy(journeyStatus = Some(
      journey.copy(
        newReturnTriageAnswers = Left(answers.copy(numberOfProperties = Some(numberOfProperties)))
      )
    )
    )
  }

  def testFormError(
    data: (String, String)*
  )(
    numberOfProperties: Int
  )(expectedErrorMessageKey: String, errorArgs: String*)(pageTitleKey: String, titleArgs: String*)(
    performAction: Seq[(String, String)] => Future[Result],
    currentSession: SessionData = sessionWithState(
      sample[IncompleteMultipleDisposalsAnswers],
      numberOfProperties
    )
  ): Unit = {
    inSequence {
      mockAuthWithNoRetrievals()
      mockGetSession(currentSession)
    }

    checkPageIsDisplayed(
      performAction(data),
      messageFromMessageKey(pageTitleKey, titleArgs), { doc =>
        doc.select("#error-summary-display > ul > li > a").text() shouldBe messageFromMessageKey(
          expectedErrorMessageKey,
          errorArgs: _*
        )
      },
      BAD_REQUEST
    )
  }

  "MultipleDisposalsTriageController" when {

    "handling requests to display the guidance page" must {

      def performAction(): Future[Result] =
        controller.guidance()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(performAction, isValidJourney)

      "display the page" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionDataWithStartingNewDraftReturn(IncompleteMultipleDisposalsAnswers.empty)._1)
        }

        checkPageIsDisplayed(
          performAction,
          messageFromMessageKey("multiple-disposals.guidance.title"), { doc =>
            doc.select("#back").attr("href") shouldBe triage.routes.InitialTriageQuestionsController
              .howManyProperties()
              .url
            doc
              .select("#content > article > form")
              .attr("action") shouldBe routes.MultipleDisposalsTriageController
              .guidanceSubmit()
              .url
          }
        )
      }

    }

    "handling submits on the guidance page" must {

      def performAction(): Future[Result] =
        controller.guidanceSubmit()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(performAction, isValidJourney)

      "redirect to how many disposals page" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionDataWithStartingNewDraftReturn(IncompleteMultipleDisposalsAnswers.empty)._1)
        }
        checkIsRedirect(performAction(), routes.MultipleDisposalsTriageController.howManyDisposals())
      }

    }

    "handling requests to display the how many disposals page" must {

      def performAction(): Future[Result] =
        controller.howManyDisposals()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(performAction, isValidJourney)

      "display the page" in {
        mockAuthWithNoRetrievals()
        mockGetSession(sessionDataWithStartingNewDraftReturn(IncompleteMultipleDisposalsAnswers.empty)._1)

        checkPageIsDisplayed(
          performAction,
          messageFromMessageKey("multipleDisposalsNumberOfProperties.title"), { doc =>
            doc.select("#back").attr("href") shouldBe triage.routes.MultipleDisposalsTriageController
              .guidance()
              .url
            doc
              .select("#content > article > form")
              .attr("action") shouldBe routes.MultipleDisposalsTriageController
              .howManyDisposalsSubmit()
              .url
          }
        )
      }

    }

    "handling submits on the how many disposals page" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.howManyDisposalsSubmit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      "redirect to disposal method when user enters number of properties as one" in {
        val (session, journey) = sessionDataWithStartingNewDraftReturn(
          IncompleteMultipleDisposalsAnswers.empty.copy(
            individualUserType = Some(Self)
          )
        )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockStoreSession(
            session.copy(journeyStatus = Some(
              journey.copy(
                newReturnTriageAnswers = Right(
                  IncompleteSingleDisposalTriageAnswers.empty.copy(
                    individualUserType         = Some(Self),
                    hasConfirmedSingleDisposal = true
                  )
                )
              )
            )
            )
          )(Right(()))
        }

        checkIsRedirect(
          performAction("multipleDisposalsNumberOfProperties" -> "1"),
          routes.SingleDisposalsTriageController.checkYourAnswers()
        )

      }

      "user has not answered how many disposals section and " +
        "redirect to dummy page when user enters number of properties more than one" in {
        val answers = IncompleteMultipleDisposalsAnswers.empty.copy(
          individualUserType = Some(Self)
        )
        val (session, journey) = sessionDataWithStartingNewDraftReturn(answers)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockStoreSession(
            session.copy(journeyStatus = Some(
              journey.copy(
                newReturnTriageAnswers = Left(answers.copy(numberOfProperties = Some(5)))
              )
            )
            )
          )(Right(()))
        }

        checkIsRedirect(
          performAction("multipleDisposalsNumberOfProperties" -> "5"),
          routes.MultipleDisposalsTriageController.checkYourAnswers()
        )
      }

      "user has not answered how many disposals section and " +
        "redirect to dummy page when user enters number of properties value which is moreThanOne" in {
        val answers = sample[CompleteMultipleDisposalsAnswers]

        val (session, journey) = sessionDataWithStartingNewDraftReturn(answers)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockStoreSession(
            session.copy(journeyStatus = Some(
              journey.copy(
                newReturnTriageAnswers = Left(answers.copy(numberOfProperties = 5))
              )
            )
            )
          )(Right(()))
        }

        checkIsRedirect(
          performAction("multipleDisposalsNumberOfProperties" -> "5"),
          routes.MultipleDisposalsTriageController.checkYourAnswers()
        )
      }

      "user has already answered how many disposals section and " +
        "redirect to dummy page when user re-enters different number of properties value which is moreThanOne" in {
        val answers = sample[CompleteMultipleDisposalsAnswers].copy(numberOfProperties = 9)

        val (session, journey) = sessionDataWithStartingNewDraftReturn(answers)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockStoreSession(
            session.copy(journeyStatus = Some(
              journey.copy(
                newReturnTriageAnswers = Left(answers.copy(numberOfProperties = 3))
              )
            )
            )
          )(Right(()))
        }

        checkIsRedirect(
          performAction("multipleDisposalsNumberOfProperties" -> "3"),
          routes.MultipleDisposalsTriageController.checkYourAnswers()
        )
      }

      "user has not answered how many disposals section and " +
        "submit request without entering numberOfProperties value" in {
        val answers = IncompleteMultipleDisposalsAnswers.empty.copy(
          individualUserType = Some(Self)
        )
        val (session, _) = sessionDataWithStartingNewDraftReturn(answers)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        val result = performAction()
        status(result) shouldBe BAD_REQUEST
      }

      "display form error when user enters numberOfProperties value <= 0" in {

        def test(data: (String, String)*)(expectedErrorMessageKey: String) =
          testFormError(data: _*)(-5)(expectedErrorMessageKey)("multipleDisposalsNumberOfProperties.title")(
            performAction
          )

        test("multipleDisposalsNumberOfProperties" -> "-5")("multipleDisposalsNumberOfProperties.error.tooSmall")

      }

      "display form error when user enters numberOfProperties value > 999" in {

        def test(data: (String, String)*)(expectedErrorMessageKey: String) =
          testFormError(data: _*)(1000)(expectedErrorMessageKey)("multipleDisposalsNumberOfProperties.title")(
            performAction
          )

        test("multipleDisposalsNumberOfProperties" -> "1000")("multipleDisposalsNumberOfProperties.error.tooLong")

      }

    }

    "handling requests to display the were uk resident page" must {

      def performAction(): Future[Result] =
        controller.wereYouAUKResident()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(performAction, isValidJourney)

      "display the page" in {
        mockAuthWithNoRetrievals()
        mockGetSession(sessionDataWithStartingNewDraftReturn(IncompleteMultipleDisposalsAnswers.empty)._1)

        checkPageIsDisplayed(
          performAction,
          messageFromMessageKey("multipleDisposalsWereYouAUKResident.title"), { doc =>
            doc.select("#back").attr("href") shouldBe triage.routes.MultipleDisposalsTriageController
              .howManyDisposals()
              .url
            doc
              .select("#content > article > form")
              .attr("action") shouldBe routes.MultipleDisposalsTriageController
              .wereYouAUKResidentSubmit()
              .url
          }
        )
      }

    }

    "handling submits on the were uk resident page" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.wereYouAUKResidentSubmit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      "redirect to redirect to cya page" when {

        val answers = IncompleteMultipleDisposalsAnswers.empty.copy(
          individualUserType = Some(Self),
          numberOfProperties = Some(2)
        )
        val (session, journey) = sessionDataWithStartingNewDraftReturn(answers)

        "user has not answered all of the were uk resident section and selects yes" in {

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreSession(
              session.copy(journeyStatus = Some(
                journey.copy(
                  newReturnTriageAnswers = Left(answers.copy(wasAUKResident = Some(true)))
                )
              )
              )
            )(Right(()))
          }

          checkIsRedirect(
            performAction("multipleDisposalsWereYouAUKResident" -> "true"),
            routes.MultipleDisposalsTriageController.checkYourAnswers()
          )
        }

        "user has not answered all of the were uk resident section and selects no" in {

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreSession(
              session.copy(journeyStatus = Some(
                journey.copy(
                  newReturnTriageAnswers = Left(answers.copy(wasAUKResident = Some(false)))
                )
              )
              )
            )(Right(()))
          }

          checkIsRedirect(
            performAction("multipleDisposalsWereYouAUKResident" -> "false"),
            routes.MultipleDisposalsTriageController.checkYourAnswers()
          )
        }

        "user has already answered were uk resident section and re-selected different option" in {
          val answers = sample[IncompleteMultipleDisposalsAnswers]
            .copy(wasAUKResident = Some(true), countryOfResidence = Some(Country.uk))

          val (session, journey) = sessionDataWithStartingNewDraftReturn(answers)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreSession(
              session.copy(journeyStatus = Some(
                journey.copy(
                  newReturnTriageAnswers = Left(
                    answers.copy(
                      wasAUKResident     = Some(false),
                      countryOfResidence = None
                    )
                  )
                )
              )
              )
            )(Right(()))
          }

          checkIsRedirect(
            performAction("multipleDisposalsWereYouAUKResident" -> "false"),
            routes.MultipleDisposalsTriageController.checkYourAnswers()
          )
        }

      }

      "show a form error" when {

        "the user submits nothing" in {
          val answers = IncompleteMultipleDisposalsAnswers.empty.copy(
            individualUserType = Some(Self),
            numberOfProperties = Some(2)
          )
          val (session, _) = sessionDataWithStartingNewDraftReturn(answers)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("multipleDisposalsWereYouAUKResident.title"), { doc =>
              doc.select("#error-summary-display > ul > li > a").text() shouldBe messageFromMessageKey(
                "multipleDisposalsWereYouAUKResident.error.required"
              )
            },
            BAD_REQUEST
          )
        }
      }

    }

    "handling requests to display the check your answers page" must {

      def performAction(): Future[Result] =
        controller.checkYourAnswers()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(performAction, isValidJourney)

      "redirect to the who is individual representing page when no individual user type can be found and the subscribed " +
        "user type is individual" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            sessionDataWithStartingNewDraftReturn(
              IncompleteMultipleDisposalsAnswers.empty,
              Right(sample[IndividualName])
            )._1
          )
        }

        checkIsRedirect(performAction(), routes.InitialTriageQuestionsController.whoIsIndividualRepresenting())
      }

      "redirect to the multiple disposals guidance page when no answer for the number of properties can be found" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            sessionDataWithStartingNewDraftReturn(
              IncompleteMultipleDisposalsAnswers.empty.copy(individualUserType = Some(IndividualUserType.Self))
            )._1
          )
        }

        checkIsRedirect(performAction(), routes.MultipleDisposalsTriageController.guidance())
      }

    }

  }

}
