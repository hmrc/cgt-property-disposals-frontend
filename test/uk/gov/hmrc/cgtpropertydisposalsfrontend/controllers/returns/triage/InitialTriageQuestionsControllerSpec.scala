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

import cats.data.EitherT
import cats.instances.future._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.i18n.MessagesApi
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.{Call, Result}
import play.api.test.FakeRequest
import play.api.test.Helpers.BAD_REQUEST
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, StartingNewDraftReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.MultipleDisposalsTriageAnswers.{CompleteMultipleDisposalsAnswers, IncompleteMultipleDisposalsAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.{CompleteSingleDisposalTriageAnswers, IncompleteSingleDisposalTriageAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, JourneyStatus, SessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class InitialTriageQuestionsControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with ScalaCheckDrivenPropertyChecks
    with RedirectToStartBehaviour {

  val mockReturnsService = mock[ReturnsService]

  override val overrideBindings =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore),
      bind[ReturnsService].toInstance(mockReturnsService)
    )

  lazy val controller = instanceOf[InitialTriageQuestionsController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  def isValidJourney(journeyStatus: JourneyStatus): Boolean = journeyStatus match {
    case _: StartingNewDraftReturn | _: FillingOutReturn => true
    case _                                               => false
  }

  def sessionDataWithStartingNewDraftReturn(
    triageAnswers: Either[MultipleDisposalsTriageAnswers, SingleDisposalTriageAnswers]
  ): (SessionData, StartingNewDraftReturn) = {
    val startingNewDraftReturn =
      sample[StartingNewDraftReturn].copy(newReturnTriageAnswers = triageAnswers)
    SessionData.empty.copy(journeyStatus = Some(startingNewDraftReturn)) -> startingNewDraftReturn
  }

  def sessionDataWithFillingOutReturn(
    singleDisposalTriageAnswers: SingleDisposalTriageAnswers
  ): (SessionData, FillingOutReturn) = {
    val fillingOutReturn = sample[FillingOutReturn].copy(
      draftReturn = sample[DraftReturn].copy(
        triageAnswers = singleDisposalTriageAnswers
      )
    )
    SessionData.empty.copy(journeyStatus = Some(fillingOutReturn)) -> fillingOutReturn
  }

  def mockStoreDraftReturn(draftReturn: DraftReturn)(result: Either[Error, Unit]) =
    (mockReturnsService
      .storeDraftReturn(_: DraftReturn)(_: HeaderCarrier))
      .expects(draftReturn, *)
      .returning(EitherT.fromEither[Future](result))

  "InitialTriageQuestionsController" when {

    "handling requests to display the who is individual representing page" must {

      def performAction(): Future[Result] = controller.whoIsIndividualRepresenting()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(performAction, isValidJourney)

      "display the page" when {

        "the user is starting a new draft return and" when {

          "the user has not answered any triage questions yet" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionDataWithStartingNewDraftReturn(Right(IncompleteSingleDisposalTriageAnswers.empty))._1
              )
            }

            checkPageIsDisplayed(
              performAction(),
              messageFromMessageKey("who-are-you-reporting-for.title"), { doc =>
                doc
                  .select("#content > article > form")
                  .attr("action") shouldBe routes.InitialTriageQuestionsController
                  .whoIsIndividualRepresentingSubmit()
                  .url

              }
            )
          }

          "the user is on the single disposal journey and has already answered the question" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionDataWithFillingOutReturn(
                  IncompleteSingleDisposalTriageAnswers.empty.copy(
                    individualUserType = Some(IndividualUserType.Self)
                  )
                )._1
              )
            }

            checkPageIsDisplayed(
              performAction(),
              messageFromMessageKey("who-are-you-reporting-for.title"), { doc =>
                doc
                  .select("#content > article > form")
                  .attr("action") shouldBe routes.InitialTriageQuestionsController
                  .whoIsIndividualRepresentingSubmit()
                  .url
                doc.select("#individualUserType-0").attr("checked") shouldBe "checked"
              }
            )
          }

          "the user is on the multiple disposals journey" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionDataWithStartingNewDraftReturn(
                  Left(
                    IncompleteMultipleDisposalsAnswers.empty.copy(
                      individualUserType = Some(IndividualUserType.Capacitor)
                    )
                  )
                )._1
              )
            }

            checkPageIsDisplayed(
              performAction(),
              messageFromMessageKey("who-are-you-reporting-for.title"), { doc =>
                doc
                  .select("#content > article > form")
                  .attr("action") shouldBe routes.InitialTriageQuestionsController
                  .whoIsIndividualRepresentingSubmit()
                  .url
                doc.select("#individualUserType-1").attr("checked") shouldBe "checked"
              }
            )
          }

        }

      }

    }

    "handling submitted answers to the who is individual representing page" must {

      def performAction(formData: (String, String)*): Future[Result] =
        controller.whoIsIndividualRepresentingSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*))

      behave like redirectToStartWhenInvalidJourney(() => performAction(), isValidJourney)

      "show a form error" when {

        def test(formData: (String, String)*)(expectedErrorKey: String): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithStartingNewDraftReturn(
                Left(
                  IncompleteMultipleDisposalsAnswers.empty.copy(
                    individualUserType = Some(IndividualUserType.Capacitor)
                  )
                )
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(formData: _*),
            messageFromMessageKey("who-are-you-reporting-for.title"), { doc =>
              doc.select("#error-summary-display > ul > li > a").text() shouldBe messageFromMessageKey(expectedErrorKey)
            },
            BAD_REQUEST
          )
        }

        "nothing has been submitted" in {
          test()("individualUserType.error.required")
        }

        "the option submitted has not been recognised" in {
          test("individualUserType" -> "3")("individualUserType.error.invalid")
        }

      }

      "show an error page" when {

        val formData = "individualUserType" -> "0"
        val (session, fillingOutReturn) = sessionDataWithFillingOutReturn(
          IncompleteSingleDisposalTriageAnswers.empty
        )
        val updatedJourney = fillingOutReturn.copy(
          draftReturn = fillingOutReturn.draftReturn.copy(
            triageAnswers = IncompleteSingleDisposalTriageAnswers.empty.copy(
              individualUserType = Some(IndividualUserType.Self)
            )
          )
        )

        "there is an error updating a draft return" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(updatedJourney.draftReturn)(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction(formData))
        }

        "there is an error updating the session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(updatedJourney.draftReturn)(Right(()))
            mockStoreSession(session.copy(journeyStatus = Some(updatedJourney)))(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction(formData))
        }
      }

      "handle valid data" when {

        "the user is starting a new draft return and" when {

          "the user had not answered any questions in the triage section yet" in {
            testSuccessfulUpdateStartingNewDraftReturn(
              performAction("individualUserType" -> "0"),
              Right(IncompleteSingleDisposalTriageAnswers.empty)
            )(
              Right(
                IncompleteSingleDisposalTriageAnswers.empty.copy(individualUserType = Some(IndividualUserType.Self))
              ),
              routes.SingleDisposalsTriageController.checkYourAnswers()
            )
          }

          "the user is on a single disposal journey and they haven't completed the triage section" in {
            testSuccessfulUpdateStartingNewDraftReturn(
              performAction("individualUserType" -> "1"),
              Right(
                IncompleteSingleDisposalTriageAnswers.empty.copy(individualUserType = Some(IndividualUserType.Self))
              )
            )(
              Right(
                IncompleteSingleDisposalTriageAnswers.empty
                  .copy(individualUserType = Some(IndividualUserType.Capacitor))
              ),
              routes.SingleDisposalsTriageController.checkYourAnswers()
            )
          }

          "the user is on a single disposal journey and they have completed the triage section" in {
            val answers = sample[CompleteSingleDisposalTriageAnswers].copy(individualUserType = IndividualUserType.Self)

            testSuccessfulUpdateStartingNewDraftReturn(
              performAction("individualUserType" -> "2"),
              Right(answers)
            )(
              Right(answers.copy(individualUserType = IndividualUserType.PersonalRepresentative)),
              routes.SingleDisposalsTriageController.checkYourAnswers()
            )
          }

          "the user is on a multiple disposals journey and they haven't completed the triage section" in {
            testSuccessfulUpdateStartingNewDraftReturn(
              performAction("individualUserType" -> "1"),
              Left(IncompleteMultipleDisposalsAnswers.empty.copy(individualUserType = Some(IndividualUserType.Self)))
            )(
              Left(
                IncompleteMultipleDisposalsAnswers.empty.copy(individualUserType = Some(IndividualUserType.Capacitor))
              ),
              routes.MultipleDisposalsTriageController.checkYourAnswers()
            )
          }

          "the user is on a multiple disposals journey and they have completed the triage section" in {
            val answers = sample[CompleteMultipleDisposalsAnswers].copy(individualUserType = IndividualUserType.Self)

            testSuccessfulUpdateStartingNewDraftReturn(
              performAction("individualUserType" -> "2"),
              Left(answers)
            )(
              Left(answers.copy(individualUserType = IndividualUserType.PersonalRepresentative)),
              routes.MultipleDisposalsTriageController.checkYourAnswers()
            )

          }
        }

        "the user is filling in a return and" when {

          "the user is on a single disposal journey and they haven't completed the triage section" in {
            testSuccessfulUpdateFillingOutReturn(
              performAction("individualUserType" -> "1"),
              IncompleteSingleDisposalTriageAnswers.empty.copy(individualUserType = Some(IndividualUserType.Self))
            )(
              IncompleteSingleDisposalTriageAnswers.empty.copy(individualUserType = Some(IndividualUserType.Capacitor)),
              routes.SingleDisposalsTriageController.checkYourAnswers()
            )
          }

          "the user is on a single disposal journey and they have completed the triage section" in {
            val answers = sample[CompleteSingleDisposalTriageAnswers].copy(individualUserType = IndividualUserType.Self)

            testSuccessfulUpdateFillingOutReturn(
              performAction("individualUserType" -> "2"),
              answers
            )(
              answers.copy(individualUserType = IndividualUserType.PersonalRepresentative),
              routes.SingleDisposalsTriageController.checkYourAnswers()
            )
          }

        }
      }

      "not do any updates" when {

        "the user has submitted the same answer they have previously entered before" in {
          val answers =
            IncompleteSingleDisposalTriageAnswers.empty.copy(individualUserType = Some(IndividualUserType.Self))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionDataWithStartingNewDraftReturn(Right(answers))._1)
          }

          checkIsRedirect(
            performAction("individualUserType" -> "0"),
            routes.SingleDisposalsTriageController.checkYourAnswers()
          )
        }

      }

    }

    "handling requests to display the number of properties page" must {

      def performAction(): Future[Result] = controller.howManyProperties()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(performAction, isValidJourney)

      "display the page" when {

        "the user has not answered the question yet" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionDataWithStartingNewDraftReturn(Right(IncompleteSingleDisposalTriageAnswers.empty))._1)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("numberOfProperties.title"), { doc =>
              doc.select("#back").attr("href") shouldBe routes.InitialTriageQuestionsController
                .whoIsIndividualRepresenting()
                .url
              doc
                .select("#content > article > form")
                .attr("action") shouldBe routes.InitialTriageQuestionsController
                .howManyPropertiesSubmit()
                .url

            }
          )
        }

        "the user is on the single disposal journey and has already answered the question" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithFillingOutReturn(
                IncompleteSingleDisposalTriageAnswers.empty.copy(hasConfirmedSingleDisposal = true)
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("numberOfProperties.title"), { doc =>
              doc.select("#back").attr("href") shouldBe routes.InitialTriageQuestionsController
                .whoIsIndividualRepresenting()
                .url
              doc
                .select("#content > article > form")
                .attr("action") shouldBe routes.InitialTriageQuestionsController
                .howManyPropertiesSubmit()
                .url
              doc.select("#numberOfProperties-0").attr("checked") shouldBe "checked"
            }
          )
        }

        "the user is on the multiple disposals journey" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithStartingNewDraftReturn(
                Left(
                  IncompleteMultipleDisposalsAnswers.empty.copy(
                    individualUserType = Some(IndividualUserType.Capacitor)
                  )
                )
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("numberOfProperties.title"), { doc =>
              doc.select("#back").attr("href") shouldBe routes.InitialTriageQuestionsController
                .whoIsIndividualRepresenting()
                .url
              doc
                .select("#content > article > form")
                .attr("action") shouldBe routes.InitialTriageQuestionsController
                .howManyPropertiesSubmit()
                .url
              doc.select("#numberOfProperties-1").attr("checked") shouldBe "checked"
            }
          )
        }

        "the user is on the single disposals journey and has completed the triage section " +
          "it has not started a saved a draft return yet" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithStartingNewDraftReturn(
                Right(sample[CompleteSingleDisposalTriageAnswers])
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("numberOfProperties.title"), { doc =>
              doc.select("#back").attr("href") shouldBe routes.SingleDisposalsTriageController
                .checkYourAnswers()
                .url
              doc
                .select("#content > article > form")
                .attr("action") shouldBe routes.InitialTriageQuestionsController
                .howManyPropertiesSubmit()
                .url
              doc.select("#numberOfProperties-0").attr("checked") shouldBe "checked"
            }
          )
        }

        "the user is on the single disposals journey and has completed the triage section " +
          "it hsa not started and has saved a draft return yet" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithFillingOutReturn(
                sample[CompleteSingleDisposalTriageAnswers]
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("numberOfProperties.title"), { doc =>
              doc.select("#back").attr("href") shouldBe routes.SingleDisposalsTriageController
                .checkYourAnswers()
                .url
              doc
                .select("#content > article > form")
                .attr("action") shouldBe routes.InitialTriageQuestionsController
                .howManyPropertiesSubmit()
                .url
              doc.select("#numberOfProperties-0").attr("checked") shouldBe "checked"
            }
          )
        }

      }

    }

    "handling submitted answers to the number of properties page" must {

      def performAction(formData: (String, String)*): Future[Result] =
        controller.howManyPropertiesSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*))

      behave like redirectToStartWhenInvalidJourney(() => performAction(), isValidJourney)

      "show a form error" when {

        def test(formData: (String, String)*)(expectedErrorKey: String): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithStartingNewDraftReturn(
                Left(
                  IncompleteMultipleDisposalsAnswers.empty.copy(
                    individualUserType = Some(IndividualUserType.Capacitor)
                  )
                )
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(formData: _*),
            messageFromMessageKey("numberOfProperties.title"), { doc =>
              doc.select("#error-summary-display > ul > li > a").text() shouldBe messageFromMessageKey(expectedErrorKey)
            },
            BAD_REQUEST
          )
        }

        "nothing has been submitted" in {
          test()("numberOfProperties.error.required")
        }

        "the option submitted has not been recognised" in {
          test("numberOfProperties" -> "2")("numberOfProperties.error.invalid")

        }

      }

      "show an error page" when {

        "there is an error updating a draft return" ignore {
          // TODO: implement when multple disposals triage section can be completed
        }

        "there is an error updating the session" in {
          val formData = "numberOfProperties" -> "0"
          val (session, journey) = sessionDataWithStartingNewDraftReturn(
            Right(
              IncompleteSingleDisposalTriageAnswers.empty.copy(
                individualUserType = Some(IndividualUserType.Self)
              )
            )
          )
          val updatedJourney = journey.copy(
            newReturnTriageAnswers = Right(
              IncompleteSingleDisposalTriageAnswers.empty.copy(
                individualUserType         = Some(IndividualUserType.Self),
                hasConfirmedSingleDisposal = true
              )
            )
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreSession(session.copy(journeyStatus = Some(updatedJourney)))(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction(formData))
        }
      }

      "handle valid data" when {

        "the user is starting a new draft return and" when {

          "the user is on a single disposal journey and hasn't answered the question yet and " +
            "they haven't completed the triage section" in {
            testSuccessfulUpdateStartingNewDraftReturn(
              performAction("numberOfProperties" -> "0"),
              Right(
                IncompleteSingleDisposalTriageAnswers.empty.copy(
                  individualUserType = Some(IndividualUserType.Self)
                )
              )
            )(
              Right(
                IncompleteSingleDisposalTriageAnswers.empty.copy(
                  individualUserType         = Some(IndividualUserType.Self),
                  hasConfirmedSingleDisposal = true
                )
              ),
              routes.SingleDisposalsTriageController.checkYourAnswers()
            )
          }

          "the user is on a single disposal journey and they haven't completed the triage section" in {
            testSuccessfulUpdateStartingNewDraftReturn(
              performAction("numberOfProperties" -> "1"),
              Right(
                IncompleteSingleDisposalTriageAnswers.empty.copy(
                  individualUserType         = Some(IndividualUserType.Self),
                  hasConfirmedSingleDisposal = true
                )
              )
            )(
              Left(
                IncompleteMultipleDisposalsAnswers.empty.copy(
                  individualUserType = Some(IndividualUserType.Self)
                )
              ),
              routes.MultipleDisposalsTriageController.checkYourAnswers()
            )
          }

          "the user is on a single disposal journey and they have completed the triage section" in {
            val answers = sample[CompleteSingleDisposalTriageAnswers]

            testSuccessfulUpdateStartingNewDraftReturn(
              performAction("numberOfProperties" -> "1"),
              Right(answers)
            )(
              Left(
                IncompleteMultipleDisposalsAnswers.empty.copy(
                  individualUserType = Some(answers.individualUserType)
                )
              ),
              routes.MultipleDisposalsTriageController.checkYourAnswers()
            )
          }

          "the user is on a multiple disposals journey and they haven't completed the triage section" in {
            testSuccessfulUpdateStartingNewDraftReturn(
              performAction("numberOfProperties" -> "0"),
              Left(
                IncompleteMultipleDisposalsAnswers.empty.copy(
                  individualUserType = Some(IndividualUserType.Self)
                )
              )
            )(
              Right(
                IncompleteSingleDisposalTriageAnswers.empty.copy(
                  individualUserType         = Some(IndividualUserType.Self),
                  hasConfirmedSingleDisposal = true
                )
              ),
              routes.SingleDisposalsTriageController.checkYourAnswers()
            )
          }

          "the user is on a multiple disposals journey and they have completed the triage section" in {
            val answers = sample[CompleteMultipleDisposalsAnswers].copy(
              individualUserType = IndividualUserType.Self
            )

            testSuccessfulUpdateStartingNewDraftReturn(
              performAction("numberOfProperties" -> "0"),
              Left(answers)
            )(
              Right(
                IncompleteSingleDisposalTriageAnswers.empty.copy(
                  individualUserType         = Some(answers.individualUserType),
                  hasConfirmedSingleDisposal = true
                )
              ),
              routes.SingleDisposalsTriageController.checkYourAnswers()
            )
          }
        }

        "the user is filling in a return and" ignore {
          // TODO: fill in when multiple disposals journey can be completed
        }

      }

      "not do any updates" when {

        "the user has submitted the same answer they have previously entered on the single disposal journey" in {
          val answers =
            IncompleteSingleDisposalTriageAnswers.empty.copy(
              individualUserType         = Some(IndividualUserType.Self),
              hasConfirmedSingleDisposal = true
            )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionDataWithStartingNewDraftReturn(Right(answers))._1)
          }

          checkIsRedirect(
            performAction("numberOfProperties" -> "0"),
            routes.SingleDisposalsTriageController.checkYourAnswers()
          )
        }

        "the user has submitted the same answer they have previously entered on the multiple disposals journey" in {
          val answers =
            IncompleteMultipleDisposalsAnswers.empty.copy(
              individualUserType = Some(IndividualUserType.Self)
            )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionDataWithStartingNewDraftReturn(Left(answers))._1)
          }

          checkIsRedirect(
            performAction("numberOfProperties" -> "1"),
            routes.MultipleDisposalsTriageController.checkYourAnswers()
          )

        }

      }

    }

  }

  def testSuccessfulUpdateStartingNewDraftReturn(
    performAction: => Future[Result],
    answers: Either[MultipleDisposalsTriageAnswers, SingleDisposalTriageAnswers]
  )(
    updatedAnswers: Either[MultipleDisposalsTriageAnswers, SingleDisposalTriageAnswers],
    expectedRedirect: Call
  ): Unit = {
    val (session, journey) = sessionDataWithStartingNewDraftReturn(answers)
    val updatedJourney     = journey.copy(newReturnTriageAnswers = updatedAnswers)

    inSequence {
      mockAuthWithNoRetrievals()
      mockGetSession(session)
      mockStoreSession(session.copy(journeyStatus = Some(updatedJourney)))(Right(()))
    }

    checkIsRedirect(performAction, expectedRedirect)
  }

  def testSuccessfulUpdateFillingOutReturn(performAction: => Future[Result], answers: SingleDisposalTriageAnswers)(
    updatedAnswers: SingleDisposalTriageAnswers,
    expectedRedirect: Call
  ): Unit = {
    val (session, journey) = sessionDataWithFillingOutReturn(answers)
    val updatedJourney     = journey.copy(draftReturn = journey.draftReturn.copy(triageAnswers = updatedAnswers))

    inSequence {
      mockAuthWithNoRetrievals()
      mockGetSession(session)
      mockStoreDraftReturn(updatedJourney.draftReturn)(Right(()))
      mockStoreSession(session.copy(journeyStatus = Some(updatedJourney)))(Right(()))
    }

    checkIsRedirect(performAction, expectedRedirect)

  }

}
