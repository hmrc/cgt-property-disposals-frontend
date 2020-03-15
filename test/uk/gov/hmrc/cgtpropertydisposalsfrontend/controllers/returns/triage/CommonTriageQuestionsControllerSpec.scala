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
import play.api.mvc.{Call, Result}
import play.api.test.FakeRequest
import play.api.test.Helpers.BAD_REQUEST
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.ReturnsServiceSupport
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, StartingNewDraftReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Country
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.AgentReferenceNumber
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.{IndividualName, TrustName}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.SubscribedDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.MultipleDisposalsTriageAnswers.{CompleteMultipleDisposalsAnswers, IncompleteMultipleDisposalsAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.{CompleteSingleDisposalTriageAnswers, IncompleteSingleDisposalTriageAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, JourneyStatus, SessionData, UserType}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService

import scala.concurrent.Future

class CommonTriageQuestionsControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with ScalaCheckDrivenPropertyChecks
    with RedirectToStartBehaviour
    with ReturnsServiceSupport {

  override val overrideBindings =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore),
      bind[ReturnsService].toInstance(mockReturnsService)
    )

  lazy val controller = instanceOf[CommonTriageQuestionsController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  def isValidJourney(journeyStatus: JourneyStatus): Boolean = journeyStatus match {
    case _: StartingNewDraftReturn | _: FillingOutReturn => true
    case _                                               => false
  }

  def sessionDataWithStartingNewDraftReturn(
    triageAnswers: Either[MultipleDisposalsTriageAnswers, SingleDisposalTriageAnswers],
    name: Either[TrustName, IndividualName]
  ): (SessionData, StartingNewDraftReturn) = {
    val startingNewDraftReturn =
      sample[StartingNewDraftReturn].copy(
        subscribedDetails      = sample[SubscribedDetails].copy(name = name),
        newReturnTriageAnswers = triageAnswers
      )
    SessionData.empty.copy(journeyStatus = Some(startingNewDraftReturn)) -> startingNewDraftReturn
  }

  def sessionDataWithFillingOutReturn(
    singleDisposalTriageAnswers: SingleDisposalTriageAnswers,
    name: Either[TrustName, IndividualName] = Right(sample[IndividualName])
  ): (SessionData, FillingOutReturn, SingleDisposalDraftReturn) = {
    val draftReturn = sample[SingleDisposalDraftReturn].copy(
      triageAnswers = singleDisposalTriageAnswers
    )
    val fillingOutReturn = sample[FillingOutReturn].copy(
      draftReturn       = draftReturn,
      subscribedDetails = sample[SubscribedDetails].copy(name = name)
    )
    (
      SessionData.empty.copy(journeyStatus = Some(fillingOutReturn)),
      fillingOutReturn,
      draftReturn
    )
  }

  "CommonTriageQuestionsController" when {

    "handling requests to display the who is individual representing page" must {

      def performAction(): Future[Result] = controller.whoIsIndividualRepresenting()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(performAction, isValidJourney)

      "redirect to the how many properties page" when {

        "the user is a trust" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithStartingNewDraftReturn(
                Right(IncompleteSingleDisposalTriageAnswers.empty),
                Left(sample[TrustName])
              )._1
            )
          }

          checkIsRedirect(performAction(), routes.CommonTriageQuestionsController.howManyProperties())
        }

      }

      "display the page" when {

        "the user is starting a new draft return and" when {

          "the user has not answered any triage questions yet" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionDataWithStartingNewDraftReturn(
                  Right(IncompleteSingleDisposalTriageAnswers.empty),
                  Right(sample[IndividualName])
                )._1
              )
            }

            checkPageIsDisplayed(
              performAction(),
              messageFromMessageKey("who-are-you-reporting-for.title"), { doc =>
                doc
                  .select("#content > article > form")
                  .attr("action") shouldBe routes.CommonTriageQuestionsController
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
                  .attr("action") shouldBe routes.CommonTriageQuestionsController
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
                  ),
                  Right(sample[IndividualName])
                )._1
              )
            }

            checkPageIsDisplayed(
              performAction(),
              messageFromMessageKey("who-are-you-reporting-for.title"), { doc =>
                doc
                  .select("#content > article > form")
                  .attr("action") shouldBe routes.CommonTriageQuestionsController
                  .whoIsIndividualRepresentingSubmit()
                  .url
                doc.select("#individualUserType-1").attr("checked") shouldBe "checked"
              }
            )
          }

          "the user is an agent representing an individual" in {
            List(
              IndividualUserType.Self,
              IndividualUserType.PersonalRepresentative
            ).zipWithIndex.foreach {
              case (value, index) =>
                val (session, journey) =
                  sessionDataWithStartingNewDraftReturn(
                    Right(
                      IncompleteSingleDisposalTriageAnswers.empty
                    ),
                    Right(sample[IndividualName])
                  )

                inSequence {
                  mockAuthWithNoRetrievals()
                  mockGetSession(
                    session.copy(
                      userType = Some(UserType.Agent),
                      journeyStatus = Some(
                        journey.copy(
                          agentReferenceNumber = Some(sample[AgentReferenceNumber]),
                          newReturnTriageAnswers = Right(
                            IncompleteSingleDisposalTriageAnswers.empty.copy(
                              individualUserType = Some(value)
                            )
                          )
                        )
                      )
                    )
                  )
                }

                checkPageIsDisplayed(
                  performAction(),
                  messageFromMessageKey("who-are-you-reporting-for.title"), { doc =>
                    doc
                      .select("#content > article > form")
                      .attr("action") shouldBe routes.CommonTriageQuestionsController
                      .whoIsIndividualRepresentingSubmit()
                      .url

                    doc.select(s"#individualUserType-$index").attr("checked") shouldBe "checked"
                    doc.select("#individualUserType > div:nth-child(2) > label").text() shouldBe messageFromMessageKey(
                      s"individualUserType.agent.${IndividualUserType.Self}"
                    )
                    doc.select("#individualUserType > div:nth-child(3) > label").text() shouldBe messageFromMessageKey(
                      s"individualUserType.agent.${IndividualUserType.PersonalRepresentative}"
                    )
                  }
                )
            }
          }

        }

      }

    }

    "handling submitted answers to the who is individual representing page" must {

      def performAction(formData: (String, String)*): Future[Result] =
        controller.whoIsIndividualRepresentingSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*))

      behave like redirectToStartWhenInvalidJourney(() => performAction(), isValidJourney)

      "redirect to the how many properties page" when {

        "the user is a trust" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithStartingNewDraftReturn(
                Right(IncompleteSingleDisposalTriageAnswers.empty),
                Left(sample[TrustName])
              )._1
            )
          }

          checkIsRedirect(performAction(), routes.CommonTriageQuestionsController.howManyProperties())
        }

      }

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
                ),
                Right(sample[IndividualName])
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(formData: _*),
            messageFromMessageKey("who-are-you-reporting-for.title"), { doc =>
              doc.select("#error-summary-display > ul > li > a").text() shouldBe messageFromMessageKey(expectedErrorKey)
              doc.title()                                               should startWith("Error:")
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
        val (session, fillingOutReturn, draftReturn) = sessionDataWithFillingOutReturn(
          IncompleteSingleDisposalTriageAnswers.empty
        )
        val updatedJourney = fillingOutReturn.copy(
          draftReturn = draftReturn.copy(
            triageAnswers = IncompleteSingleDisposalTriageAnswers.empty.copy(
              individualUserType = Some(IndividualUserType.Self)
            )
          )
        )

        "there is an error updating a draft return" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(updatedJourney.draftReturn, fillingOutReturn.agentReferenceNumber)(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction(formData))
        }

        "there is an error updating the session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(updatedJourney.draftReturn, fillingOutReturn.agentReferenceNumber)(Right(()))
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
              Right(IncompleteSingleDisposalTriageAnswers.empty),
              Right(sample[IndividualName])
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
              ),
              Right(sample[IndividualName])
            )(
              Right(
                IncompleteSingleDisposalTriageAnswers.empty
                  .copy(individualUserType = Some(IndividualUserType.Capacitor))
              ),
              routes.SingleDisposalsTriageController.checkYourAnswers()
            )
          }

          "the user is on a single disposal journey and they have completed the triage section" in {
            val answers =
              sample[CompleteSingleDisposalTriageAnswers].copy(individualUserType = Some(IndividualUserType.Self))

            testSuccessfulUpdateStartingNewDraftReturn(
              performAction("individualUserType" -> "2"),
              Right(answers),
              Right(sample[IndividualName])
            )(
              Right(answers.copy(individualUserType = Some(IndividualUserType.PersonalRepresentative))),
              routes.SingleDisposalsTriageController.checkYourAnswers()
            )
          }

          "the user is on a multiple disposals journey and they haven't completed the triage section" in {
            testSuccessfulUpdateStartingNewDraftReturn(
              performAction("individualUserType" -> "1"),
              Left(IncompleteMultipleDisposalsAnswers.empty.copy(individualUserType = Some(IndividualUserType.Self))),
              Right(sample[IndividualName])
            )(
              Left(
                IncompleteMultipleDisposalsAnswers.empty.copy(individualUserType = Some(IndividualUserType.Capacitor))
              ),
              routes.MultipleDisposalsTriageController.checkYourAnswers()
            )
          }

          "the user is on a multiple disposals journey and they have completed the triage section" in {
            val answers =
              sample[CompleteMultipleDisposalsAnswers].copy(individualUserType = Some(IndividualUserType.Self))

            testSuccessfulUpdateStartingNewDraftReturn(
              performAction("individualUserType" -> "2"),
              Left(answers),
              Right(sample[IndividualName])
            )(
              Left(answers.copy(individualUserType = Some(IndividualUserType.PersonalRepresentative))),
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
            val answers =
              sample[CompleteSingleDisposalTriageAnswers].copy(individualUserType = Some(IndividualUserType.Self))

            testSuccessfulUpdateFillingOutReturn(
              performAction("individualUserType" -> "2"),
              answers
            )(
              answers.copy(individualUserType = Some(IndividualUserType.PersonalRepresentative)),
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
            mockGetSession(sessionDataWithStartingNewDraftReturn(Right(answers), Right(sample[IndividualName]))._1)
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
            mockGetSession(
              sessionDataWithStartingNewDraftReturn(
                Right(IncompleteSingleDisposalTriageAnswers.empty),
                Left(sample[TrustName])
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("numberOfProperties.title"), { doc =>
              doc.select("#back").attr("href") shouldBe ""
              doc
                .select("#content > article > form")
                .attr("action") shouldBe routes.CommonTriageQuestionsController
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
              doc.select("#back").attr("href") shouldBe routes.CommonTriageQuestionsController
                .whoIsIndividualRepresenting()
                .url
              doc
                .select("#content > article > form")
                .attr("action") shouldBe routes.CommonTriageQuestionsController
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
                    individualUserType = None
                  )
                ),
                Left(sample[TrustName])
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("numberOfProperties.title"), { doc =>
              doc.select("#back").attr("href") shouldBe ""
              doc
                .select("#content > article > form")
                .attr("action") shouldBe routes.CommonTriageQuestionsController
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
                Right(sample[CompleteSingleDisposalTriageAnswers]),
                Right(sample[IndividualName])
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
                .attr("action") shouldBe routes.CommonTriageQuestionsController
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
                .attr("action") shouldBe routes.CommonTriageQuestionsController
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
                ),
                Right(sample[IndividualName])
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
            ),
            Right(sample[IndividualName])
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
                IncompleteSingleDisposalTriageAnswers.empty
              ),
              Left(sample[TrustName])
            )(
              Right(
                IncompleteSingleDisposalTriageAnswers.empty.copy(
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
              ),
              Right(sample[IndividualName])
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
              Right(answers),
              Right(sample[IndividualName])
            )(
              Left(
                IncompleteMultipleDisposalsAnswers.empty.copy(
                  individualUserType = answers.individualUserType
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
                  individualUserType = None
                )
              ),
              Left(sample[TrustName])
            )(
              Right(
                IncompleteSingleDisposalTriageAnswers.empty.copy(
                  individualUserType         = None,
                  hasConfirmedSingleDisposal = true
                )
              ),
              routes.SingleDisposalsTriageController.checkYourAnswers()
            )
          }

          "the user is on a multiple disposals journey and they have completed the triage section" in {
            val answers = sample[CompleteMultipleDisposalsAnswers].copy(
              individualUserType = Some(IndividualUserType.Self)
            )

            testSuccessfulUpdateStartingNewDraftReturn(
              performAction("numberOfProperties" -> "0"),
              Left(answers),
              Right(sample[IndividualName])
            )(
              Right(
                IncompleteSingleDisposalTriageAnswers.empty.copy(
                  individualUserType         = answers.individualUserType,
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
              individualUserType         = None,
              hasConfirmedSingleDisposal = true
            )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionDataWithStartingNewDraftReturn(Right(answers), Left(sample[TrustName]))._1)
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
            mockGetSession(sessionDataWithStartingNewDraftReturn(Left(answers), Right(sample[IndividualName]))._1)
          }

          checkIsRedirect(
            performAction("numberOfProperties" -> "1"),
            routes.MultipleDisposalsTriageController.checkYourAnswers()
          )

        }

      }

    }

    "handling requests to display the disposal date too early page" must {

      def performAction(): Future[Result] =
        controller.disposalDateTooEarly()(FakeRequest())

      val singleDisposalRequiredPreviousAnswers = IncompleteSingleDisposalTriageAnswers.empty.copy(
        individualUserType         = Some(IndividualUserType.Self),
        hasConfirmedSingleDisposal = true,
        disposalMethod             = Some(DisposalMethod.Sold),
        wasAUKResident             = Some(true),
        countryOfResidence         = None,
        assetType                  = Some(AssetType.Residential)
      )

      val multipleDisposalsRequiredPreviousAnswers = IncompleteMultipleDisposalsAnswers.empty.copy(
        individualUserType           = Some(IndividualUserType.Self),
        numberOfProperties           = Some(2),
        wasAUKResident               = Some(true),
        countryOfResidence           = None,
        assetTypes                   = Some(List(AssetType.Residential)),
        wereAllPropertiesResidential = Some(true),
        taxYearAfter6April2020       = Some(false)
      )

      "redirect to the relevant check you answers endpoint" when {

        def test(sessionData: SessionData, expectedRedirect: Call): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionData)
          }

          checkIsRedirect(performAction(), expectedRedirect)
        }

        "the section is incomplete and the was a uk resident question has not been answered yet and" when {

          "the user is starting a new draft return and they are on a single disposal journey" in {
            test(
              sessionDataWithStartingNewDraftReturn(
                Right(singleDisposalRequiredPreviousAnswers.copy(wasAUKResident = None)),
                Right(sample[IndividualName])
              )._1,
              routes.SingleDisposalsTriageController.checkYourAnswers()
            )
          }

          "the user is filling in a draft return and they are on a single disposal journey" in {
            test(
              sessionDataWithFillingOutReturn(
                singleDisposalRequiredPreviousAnswers.copy(wasAUKResident = None),
                Right(sample[IndividualName])
              )._1,
              routes.SingleDisposalsTriageController.checkYourAnswers()
            )
          }

          "the user is starting a new draft return and they are on a multiple disposals journey" in {
            test(
              sessionDataWithStartingNewDraftReturn(
                Left(multipleDisposalsRequiredPreviousAnswers.copy(wasAUKResident = None)),
                Right(sample[IndividualName])
              )._1,
              routes.MultipleDisposalsTriageController.checkYourAnswers()
            )
          }

        }

      }

      "display the page" when {

        "the user was a uk resident and" when {

          def test(result: Future[Result], expectedBackLink: Call): Unit =
            checkPageIsDisplayed(
              result,
              messageFromMessageKey("disposalDateTooEarly.uk.title"), { doc =>
                doc.select("#back").attr("href") shouldBe expectedBackLink.url
                doc.select("#content > article > p:nth-child(3)").text() shouldBe messageFromMessageKey(
                  "disposalDateTooEarly.uk.p1"
                )
                doc.select("#content > article > p:nth-child(4)").html() shouldBe messageFromMessageKey(
                  "disposalDateTooEarly.uk.p2",
                  viewConfig.reportingCgtBefore6April2020
                )
              }
            )

          "they are on a single disposal journey" in {

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionDataWithStartingNewDraftReturn(
                  Right(singleDisposalRequiredPreviousAnswers),
                  Right(sample[IndividualName])
                )._1
              )
            }

            test(performAction(), routes.SingleDisposalsTriageController.whenWasDisposalDate())
          }

          "they are on a mutliple disposals journey" in {

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionDataWithStartingNewDraftReturn(
                  Left(multipleDisposalsRequiredPreviousAnswers),
                  Right(sample[IndividualName])
                )._1
              )
            }

            test(performAction(), routes.MultipleDisposalsTriageController.whenWereContractsExchanged())
          }
        }

        "the user was not a uk resident and" when {

          def test(result: Future[Result], expectedBackLink: Call): Unit =
            checkPageIsDisplayed(
              result,
              messageFromMessageKey("disposalDateTooEarly.non-uk.title"), { doc =>
                doc.select("#back").attr("href") shouldBe expectedBackLink.url
                doc.select("#content > article > p:nth-child(3)").text() shouldBe messageFromMessageKey(
                  "disposalDateTooEarly.non-uk.p1"
                )
                doc.select("#content > article > p:nth-child(4)").text() shouldBe messageFromMessageKey(
                  "disposalDateTooEarly.non-uk.p2"
                )
                doc.select("#content > article > p:nth-child(5)").html() shouldBe messageFromMessageKey(
                  "disposalDateTooEarly.non-uk.p3",
                  viewConfig.reportingCgtBefore6April2020
                )
              }
            )

          "they are on a single disposal journey" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionDataWithStartingNewDraftReturn(
                  Right(
                    singleDisposalRequiredPreviousAnswers.copy(
                      wasAUKResident     = Some(false),
                      countryOfResidence = Some(sample[Country])
                    )
                  ),
                  Right(sample[IndividualName])
                )._1
              )
            }

            test(performAction(), routes.SingleDisposalsTriageController.whenWasDisposalDate())
          }

          "they are on a multiple disposals journey" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionDataWithStartingNewDraftReturn(
                  Left(
                    multipleDisposalsRequiredPreviousAnswers.copy(
                      wasAUKResident     = Some(false),
                      countryOfResidence = Some(sample[Country])
                    )
                  ),
                  Right(sample[IndividualName])
                )._1
              )
            }

            test(performAction(), routes.MultipleDisposalsTriageController.whenWereContractsExchanged())
          }

        }
      }

    }

    "handling requests to display the uk residents can only dispose residential properties page" must {

      def performAction(): Future[Result] = controller.ukResidentCanOnlyDisposeResidential()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(performAction, isValidJourney)

      "redirect to the check your answers page" when {

        "the user was not a uk resident and" when {

          "the user is on a single disposal journey" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionDataWithStartingNewDraftReturn(
                  Right(
                    IncompleteSingleDisposalTriageAnswers.empty.copy(
                      individualUserType         = Some(IndividualUserType.Self),
                      hasConfirmedSingleDisposal = true,
                      disposalMethod             = Some(DisposalMethod.Sold),
                      wasAUKResident             = Some(false),
                      countryOfResidence         = Some(sample[Country]),
                      assetType                  = Some(AssetType.NonResidential)
                    )
                  ),
                  Right(sample[IndividualName])
                )._1
              )
            }

            checkIsRedirect(performAction(), routes.SingleDisposalsTriageController.checkYourAnswers())
          }

          "the user is on a multiple disposals journey" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionDataWithStartingNewDraftReturn(
                  Left(
                    IncompleteMultipleDisposalsAnswers.empty.copy(
                      individualUserType = Some(IndividualUserType.Self),
                      numberOfProperties = Some(2),
                      wasAUKResident     = Some(false),
                      countryOfResidence = Some(sample[Country]),
                      assetTypes         = Some(List(AssetType.NonResidential))
                    )
                  ),
                  Right(sample[IndividualName])
                )._1
              )
            }

            checkIsRedirect(performAction(), routes.MultipleDisposalsTriageController.checkYourAnswers())
          }
        }

        "the user was a uk resident and they disposed of a residential and" when {

          "the user is on a single disposal journey" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionDataWithStartingNewDraftReturn(
                  Right(
                    IncompleteSingleDisposalTriageAnswers.empty.copy(
                      individualUserType         = Some(IndividualUserType.Self),
                      hasConfirmedSingleDisposal = true,
                      disposalMethod             = Some(DisposalMethod.Sold),
                      wasAUKResident             = Some(true),
                      assetType                  = Some(AssetType.Residential)
                    )
                  ),
                  Right(sample[IndividualName])
                )._1
              )
            }

            checkIsRedirect(performAction(), routes.SingleDisposalsTriageController.checkYourAnswers())
          }

          "the user is on a multiple disposals journey" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionDataWithStartingNewDraftReturn(
                  Left(
                    IncompleteMultipleDisposalsAnswers.empty.copy(
                      individualUserType = Some(IndividualUserType.Self),
                      numberOfProperties = Some(2),
                      wasAUKResident     = Some(true),
                      assetTypes         = Some(List(AssetType.Residential))
                    )
                  ),
                  Right(sample[IndividualName])
                )._1
              )
            }

            checkIsRedirect(performAction(), routes.MultipleDisposalsTriageController.checkYourAnswers())
          }

        }

      }

      "display the page" when {

        "the user was a uk resident and they disposed of a non-residential property and" when {

          "the user is on a single disposal journey" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionDataWithStartingNewDraftReturn(
                  Right(
                    IncompleteSingleDisposalTriageAnswers.empty.copy(
                      individualUserType         = Some(IndividualUserType.Self),
                      hasConfirmedSingleDisposal = true,
                      disposalMethod             = Some(DisposalMethod.Sold),
                      wasAUKResident             = Some(true),
                      assetType                  = Some(AssetType.NonResidential)
                    )
                  ),
                  Right(sample[IndividualName])
                )._1
              )
            }

            checkPageIsDisplayed(
              performAction(),
              messageFromMessageKey("ukResidentCanOnlyReportResidential.title"), { doc =>
                doc.select("#back").attr("href") shouldBe routes.SingleDisposalsTriageController
                  .didYouDisposeOfAResidentialProperty()
                  .url
              }
            )
          }

          "the user is on a multiple disposals journey" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionDataWithStartingNewDraftReturn(
                  Left(
                    IncompleteMultipleDisposalsAnswers.empty.copy(
                      individualUserType           = Some(IndividualUserType.Self),
                      numberOfProperties           = Some(3),
                      wasAUKResident               = Some(true),
                      assetTypes                   = Some(List(AssetType.NonResidential)),
                      wereAllPropertiesResidential = Some(false)
                    )
                  ),
                  Right(sample[IndividualName])
                )._1
              )
            }

            checkPageIsDisplayed(
              performAction(),
              messageFromMessageKey("ukResidentCanOnlyReportResidential.title"), { doc =>
                doc.select("#back").attr("href") shouldBe routes.MultipleDisposalsTriageController
                  .wereAllPropertiesResidential()
                  .url
              }
            )
          }
        }

      }

    }

    "handling requests to display the asset type not yet implemented page" must {
      def performAction(): Future[Result] =
        controller.assetTypeNotYetImplemented()(FakeRequest())

      "display the page" when {

        "the user is on a single disposal journey and" when {

          def test(assetType: AssetType): Unit = {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionDataWithStartingNewDraftReturn(
                  Right(sample[CompleteSingleDisposalTriageAnswers].copy(assetType = assetType)),
                  Right(sample[IndividualName])
                )._1
              )
            }

            checkPageIsDisplayed(
              performAction(),
              messageFromMessageKey("disposalDateMixedUseOrIndirect.title"), { doc =>
                doc.select("#back").attr("href") shouldBe routes.SingleDisposalsTriageController
                  .assetTypeForNonUkResidents()
                  .url
              }
            )
          }

          "the asset type is mixed use" in {
            test(AssetType.MixedUse)
          }

          "the asset type is indirect disposal" in {
            test(AssetType.IndirectDisposal)
          }

        }

        "the user is on a multiple disposals journey and" when {

          def test(assetType: AssetType): Unit = {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionDataWithStartingNewDraftReturn(
                  Left(sample[CompleteMultipleDisposalsAnswers].copy(assetTypes = List(assetType))),
                  Right(sample[IndividualName])
                )._1
              )
            }

            checkPageIsDisplayed(
              performAction(),
              messageFromMessageKey("disposalDateMixedUseOrIndirect.title"), { doc =>
                doc.select("#back").attr("href") shouldBe routes.MultipleDisposalsTriageController
                  .assetTypeForNonUkResidents()
                  .url
              }
            )
          }

          "the asset type is mixed use" in {
            test(AssetType.MixedUse)
          }

          "the asset type is indirect disposal" in {
            test(AssetType.IndirectDisposal)
          }

        }

      }

      "redirect to the relevant check your answers page" when {

        "the user is on a single disposal journey and" when {

          def test(assetType: AssetType): Unit = {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionDataWithStartingNewDraftReturn(
                  Right(sample[CompleteSingleDisposalTriageAnswers].copy(assetType = assetType)),
                  Right(sample[IndividualName])
                )._1
              )
            }

            checkIsRedirect(performAction(), routes.SingleDisposalsTriageController.checkYourAnswers())
          }

          "the asset type is residential" in {
            test(AssetType.Residential)
          }

          "the result for non-residential" in {
            test(AssetType.NonResidential)
          }

        }

        "the user is on a multiple disposals journey and" when {

          def test(assetType: AssetType): Unit = {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionDataWithStartingNewDraftReturn(
                  Left(sample[CompleteMultipleDisposalsAnswers].copy(assetTypes = List(assetType))),
                  Right(sample[IndividualName])
                )._1
              )
            }

            checkIsRedirect(performAction(), routes.MultipleDisposalsTriageController.checkYourAnswers())
          }

          "the asset type is residential" in {
            test(AssetType.Residential)
          }

          "the result for non-residential" in {
            test(AssetType.NonResidential)
          }

        }

      }

    }

    "handling requests to display the capacitors and personal reps not handled page" must {

      def performAction(): Future[Result] = controller.capacitorsAndPersonalRepresentativesNotHandled()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(performAction, isValidJourney)

      "redirect to the single disposal check your answers endpoint" when {

        "the user is on a single disposal journey and they do not have an individual user type " +
          "of capacitor or personal representative" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithFillingOutReturn(
                sample[CompleteSingleDisposalTriageAnswers].copy(individualUserType = Some(IndividualUserType.Self))
              )._1
            )

            checkIsRedirect(performAction(), routes.SingleDisposalsTriageController.checkYourAnswers())
          }
        }

      }

      "redirect to the multiple disposals check your answers endpoint" when {

        "the user is on a multiple disposals journey and they do not have an individual user type " +
          "of capacitor or personal representative" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithStartingNewDraftReturn(
                Left(IncompleteMultipleDisposalsAnswers.empty),
                Right(sample[IndividualName])
              )._1
            )
          }

          checkIsRedirect(performAction(), routes.MultipleDisposalsTriageController.checkYourAnswers())
        }

      }

      "display the page" when {

        "the user has said they are a capacitor" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithFillingOutReturn(
                sample[CompleteSingleDisposalTriageAnswers].copy(individualUserType = Some(IndividualUserType.Capacitor)
                )
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("capacitorsPersonalRepresentativesNotHandled.title"), { doc =>
              doc.select("#back").attr("href") shouldBe routes.CommonTriageQuestionsController
                .whoIsIndividualRepresenting()
                .url
            }
          )
        }

        "the user has said they are a personal representative" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithStartingNewDraftReturn(
                Left(
                  IncompleteMultipleDisposalsAnswers.empty
                    .copy(individualUserType = Some(IndividualUserType.PersonalRepresentative))
                ),
                Right(sample[IndividualName])
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("capacitorsPersonalRepresentativesNotHandled.title"), { doc =>
              doc.select("#back").attr("href") shouldBe routes.CommonTriageQuestionsController
                .whoIsIndividualRepresenting()
                .url
            }
          )
        }

      }

    }

  }

  def testSuccessfulUpdateStartingNewDraftReturn(
    performAction: => Future[Result],
    answers: Either[MultipleDisposalsTriageAnswers, SingleDisposalTriageAnswers],
    name: Either[TrustName, IndividualName]
  )(
    updatedAnswers: Either[MultipleDisposalsTriageAnswers, SingleDisposalTriageAnswers],
    expectedRedirect: Call
  ): Unit = {
    val (session, journey) = sessionDataWithStartingNewDraftReturn(answers, name)
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
    val (session, journey, draftReturn) = sessionDataWithFillingOutReturn(answers)
    val updatedJourney                  = journey.copy(draftReturn = draftReturn.copy(triageAnswers = updatedAnswers))

    inSequence {
      mockAuthWithNoRetrievals()
      mockGetSession(session)
      mockStoreDraftReturn(updatedJourney.draftReturn, journey.agentReferenceNumber)(Right(()))
      mockStoreSession(session.copy(journeyStatus = Some(updatedJourney)))(Right(()))
    }

    checkIsRedirect(performAction, expectedRedirect)

  }

}
