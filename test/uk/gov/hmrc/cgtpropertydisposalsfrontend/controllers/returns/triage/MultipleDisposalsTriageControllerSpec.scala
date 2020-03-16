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

import cats.data.EitherT
import cats.instances.future._
import cats.instances.list._
import cats.syntax.eq._
import org.jsoup.nodes.Document
import org.scalatest.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.http.Status.BAD_REQUEST
import play.api.i18n.{Lang, MessagesApi, MessagesImpl}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.{Call, Result}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.triage
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, ControllerSpec, DateErrorScenarios, SessionSupport}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.StartingNewDraftReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Country
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.AgentReferenceNumber
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.{IndividualName, TrustName}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.SubscribedDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.IndividualUserType.Self
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.MultipleDisposalsTriageAnswers.{IncompleteMultipleDisposalsAnswers, _}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.IncompleteSingleDisposalTriageAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{IndividualUserType, _}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, JourneyStatus, LocalDateUtils, SessionData, TaxYear, UserType}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.TaxYearService
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class MultipleDisposalsTriageControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with ScalaCheckDrivenPropertyChecks
    with RedirectToStartBehaviour {

  val mockTaxYearService = mock[TaxYearService]

  override val overrideBindings =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore),
      bind[TaxYearService].toInstance(mockTaxYearService)
    )

  lazy val controller = instanceOf[MultipleDisposalsTriageController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  def isValidJourney(journeyStatus: JourneyStatus): Boolean = journeyStatus match {
    case r: StartingNewDraftReturn if (r.newReturnTriageAnswers.isLeft) => true
    case _                                                              => false
  }

  def sessionDataWithStartingNewDraftReturn(
    multipleDisposalsAnswers: MultipleDisposalsTriageAnswers,
    name: Either[TrustName, IndividualName] = Right(sample[IndividualName]),
    isAgent: Boolean                        = false
  ): (SessionData, StartingNewDraftReturn) = {
    val startingNewDraftReturn = sample[StartingNewDraftReturn].copy(
      newReturnTriageAnswers = Left(multipleDisposalsAnswers),
      subscribedDetails      = sample[SubscribedDetails].copy(name = name),
      agentReferenceNumber   = if (isAgent) Some(sample[AgentReferenceNumber]) else None
    )
    SessionData.empty.copy(
      journeyStatus = Some(startingNewDraftReturn),
      userType      = if (isAgent) Some(UserType.Agent) else None
    ) -> startingNewDraftReturn

  }

  def testFormError(
    data: (String, String)*
  )(expectedErrorMessageKey: String, errorArgs: String*)(pageTitleKey: String, titleArgs: String*)(
    performAction: Seq[(String, String)] => Future[Result],
    currentSession: SessionData = sessionDataWithStartingNewDraftReturn(sample[IncompleteMultipleDisposalsAnswers])._1
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
        doc.title() should startWith("Error:")
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
            doc.select("#back").attr("href") shouldBe triage.routes.CommonTriageQuestionsController
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

      "display the page" when {

        "the journey is incomplete" in {
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

        "the journey is complete" in {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionDataWithStartingNewDraftReturn(sample[CompleteMultipleDisposalsAnswers])._1)

          checkPageIsDisplayed(
            performAction,
            messageFromMessageKey("multipleDisposalsNumberOfProperties.title"), { doc =>
              doc.select("#back").attr("href") shouldBe triage.routes.MultipleDisposalsTriageController
                .checkYourAnswers()
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

    }

    "handling submits on the how many disposals page" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.howManyDisposalsSubmit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      val key = "multipleDisposalsNumberOfProperties"

      "redirect to single disposal cya page" when {

        "user enters number of properties as one" in {
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
            performAction(key -> "1"),
            routes.SingleDisposalsTriageController.checkYourAnswers()
          )

        }
      }

      "redirect to cya page" when {

        "user has not answered how many disposals section and " +
          "enters number of properties more than one" in {
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
            performAction(key -> "5"),
            routes.MultipleDisposalsTriageController.checkYourAnswers()
          )
        }

        "user has already answered how many disposals section and " +
          "re-enters different number of properties value for more than one" in {
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
            performAction(key -> "3"),
            routes.MultipleDisposalsTriageController.checkYourAnswers()
          )
        }

      }

      "not update the session" when {

        "user has already answered how many disposals section and " +
          "re-enters same number of properties value for more than one" in {
          val answers = sample[CompleteMultipleDisposalsAnswers].copy(
            numberOfProperties = 5
          )

          val (session, _) = sessionDataWithStartingNewDraftReturn(answers)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkIsRedirect(
            performAction(key -> "5"),
            routes.MultipleDisposalsTriageController.checkYourAnswers()
          )
        }
      }

      "display form error" when {

        def test(data: (String, String)*)(expectedErrorMessageKey: String) =
          testFormError(data: _*)(expectedErrorMessageKey)(s"$key.title")(
            performAction
          )

        "the user submits nothing" in {
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
          test(key -> "-5")(s"$key.error.tooSmall")

        }

        "display form error when user enters numberOfProperties value > 999" in {
          test(key -> "1000")(s"$key.error.tooLong")
        }

        "display form error when user enters invalid data" in {
          test(key -> "!@£!")(s"$key.error.invalid")
        }

      }

    }

    "handling requests to display the were uk resident page" must {

      def performAction(): Future[Result] =
        controller.wereYouAUKResident()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(performAction, isValidJourney)

      "display the page" when {

        "the journey is incomplete " in {
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

        "the journey is complete" in {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionDataWithStartingNewDraftReturn(sample[CompleteMultipleDisposalsAnswers])._1)

          checkPageIsDisplayed(
            performAction,
            messageFromMessageKey("multipleDisposalsWereYouAUKResident.title"), { doc =>
              doc.select("#back").attr("href") shouldBe triage.routes.MultipleDisposalsTriageController
                .checkYourAnswers()
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

    }

    "handling submits on the were uk resident page" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.wereYouAUKResidentSubmit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      val key = "multipleDisposalsWereYouAUKResident"

      "redirect to redirect to cya page" when {

        val answers = IncompleteMultipleDisposalsAnswers.empty.copy(
          individualUserType = Some(Self),
          numberOfProperties = Some(2)
        )

        val (session, journey) = sessionDataWithStartingNewDraftReturn(answers)

        "user has not answered the were uk resident section and selects true" in {

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
            performAction(key -> "true"),
            routes.MultipleDisposalsTriageController.checkYourAnswers()
          )
        }

        "user has not answered the were uk resident section and selects false" in {

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
            performAction(key -> "false"),
            routes.MultipleDisposalsTriageController.checkYourAnswers()
          )
        }

        "user has already answered were uk resident section and re-selected different option" in {
          val answers = sample[IncompleteMultipleDisposalsAnswers]
            .copy(
              wasAUKResident     = Some(true),
              countryOfResidence = Some(Country.uk)
            )

          val (session, journey) = sessionDataWithStartingNewDraftReturn(answers)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreSession(
              session.copy(journeyStatus = Some(
                journey.copy(
                  newReturnTriageAnswers = Left(
                    answers.copy(
                      wasAUKResident               = Some(false),
                      countryOfResidence           = None,
                      wereAllPropertiesResidential = None,
                      assetTypes                   = None
                    )
                  )
                )
              )
              )
            )(Right(()))
          }

          checkIsRedirect(
            performAction(key -> "false"),
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
            messageFromMessageKey(s"$key.title"), { doc =>
              doc.select("#error-summary-display > ul > li > a").text() shouldBe messageFromMessageKey(
                s"$key.error.required"
              )
            },
            BAD_REQUEST
          )
        }
      }

    }

    "handling requests to display the were all properties residential page" must {

      def performAction(): Future[Result] =
        controller.wereAllPropertiesResidential()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(performAction, isValidJourney)

      "display the page" when {

        "the journey is incomplete" in {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionDataWithStartingNewDraftReturn(IncompleteMultipleDisposalsAnswers.empty)._1)

          checkPageIsDisplayed(
            performAction,
            messageFromMessageKey("multipleDisposalsWereAllPropertiesResidential.title"), { doc =>
              doc.select("#back").attr("href") shouldBe triage.routes.MultipleDisposalsTriageController
                .wereYouAUKResident()
                .url
              doc
                .select("#content > article > form")
                .attr("action") shouldBe routes.MultipleDisposalsTriageController
                .wereAllPropertiesResidentialSubmit()
                .url
            }
          )
        }

        "the journey is complete" in {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionDataWithStartingNewDraftReturn(sample[CompleteMultipleDisposalsAnswers])._1)

          checkPageIsDisplayed(
            performAction,
            messageFromMessageKey("multipleDisposalsWereAllPropertiesResidential.title"), { doc =>
              doc.select("#back").attr("href") shouldBe triage.routes.MultipleDisposalsTriageController
                .checkYourAnswers()
                .url
              doc
                .select("#content > article > form")
                .attr("action") shouldBe routes.MultipleDisposalsTriageController
                .wereAllPropertiesResidentialSubmit()
                .url
            }
          )
        }

      }

    }

    "handling submits on the were all properties residential page" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.wereAllPropertiesResidentialSubmit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      val key = "multipleDisposalsWereAllPropertiesResidential"

      "redirect to redirect to cya page" when {

        val answers = IncompleteMultipleDisposalsAnswers.empty.copy(
          individualUserType = Some(Self),
          numberOfProperties = Some(2),
          wasAUKResident     = Some(true),
          countryOfResidence = Some(Country.uk)
        )

        val (session, journey) = sessionDataWithStartingNewDraftReturn(answers)

        "user has not answered the were all properties residential section and selects true" in {

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreSession(
              session.copy(journeyStatus = Some(
                journey.copy(
                  newReturnTriageAnswers = Left(
                    answers.copy(
                      wereAllPropertiesResidential = Some(true),
                      assetTypes                   = Some(List(AssetType.Residential))
                    )
                  )
                )
              )
              )
            )(Right(()))
          }

          checkIsRedirect(
            performAction(key -> "true"),
            routes.MultipleDisposalsTriageController.checkYourAnswers()
          )
        }

        "user has not answered the were all properties residential section and selects false" in {

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreSession(
              session.copy(journeyStatus = Some(
                journey.copy(
                  newReturnTriageAnswers = Left(
                    answers.copy(
                      wereAllPropertiesResidential = Some(false),
                      assetTypes                   = Some(List(AssetType.NonResidential))
                    )
                  )
                )
              )
              )
            )(Right(()))
          }

          checkIsRedirect(
            performAction(key -> "false"),
            routes.MultipleDisposalsTriageController.checkYourAnswers()
          )
        }

        "user has already answered were all properties residential section and re-selected different option" in {
          val answers = sample[IncompleteMultipleDisposalsAnswers]
            .copy(
              wereAllPropertiesResidential = Some(true),
              assetTypes                   = Some(List(AssetType.Residential))
            )

          val (session, journey) = sessionDataWithStartingNewDraftReturn(answers)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreSession(
              session.copy(journeyStatus = Some(
                journey.copy(
                  newReturnTriageAnswers = Left(
                    answers.copy(
                      wereAllPropertiesResidential = Some(false),
                      assetTypes                   = Some(List(AssetType.NonResidential))
                    )
                  )
                )
              )
              )
            )(Right(()))
          }

          checkIsRedirect(
            performAction(key -> "false"),
            routes.MultipleDisposalsTriageController.checkYourAnswers()
          )
        }

      }

      "not update the session" when {

        "user has already answered were all properties residential section and re-selected same option" in {
          val answers = sample[IncompleteMultipleDisposalsAnswers]
            .copy(
              wereAllPropertiesResidential = Some(true),
              assetTypes                   = Some(List(AssetType.Residential))
            )

          val (session, _) = sessionDataWithStartingNewDraftReturn(answers)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkIsRedirect(
            performAction(key -> "true"),
            routes.MultipleDisposalsTriageController.checkYourAnswers()
          )
        }

      }

      "show a form error" when {

        "the user submits nothing" in {
          val answers = IncompleteMultipleDisposalsAnswers.empty.copy(
            individualUserType = Some(Self),
            numberOfProperties = Some(2),
            wasAUKResident     = Some(true),
            countryOfResidence = Some(Country.uk)
          )
          val (session, _) = sessionDataWithStartingNewDraftReturn(answers)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(s"$key.title"), { doc =>
              doc.select("#error-summary-display > ul > li > a").text() shouldBe messageFromMessageKey(
                s"$key.error.required"
              )
            },
            BAD_REQUEST
          )
        }

      }

    }

    "handling requests to display the tax year exchanged page" must {

      def performAction(): Future[Result] =
        controller.whenWereContractsExchanged()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(performAction, isValidJourney)

      "display the page" when {

        "the journeu is incomplete" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithStartingNewDraftReturn(
                IncompleteMultipleDisposalsAnswers.empty.copy(
                  individualUserType           = Some(Self),
                  numberOfProperties           = Some(2),
                  wasAUKResident               = Some(true),
                  countryOfResidence           = Some(Country.uk),
                  wereAllPropertiesResidential = Some(true),
                  assetTypes                   = Some(List(AssetType.Residential))
                )
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction,
            messageFromMessageKey("multipleDisposalsTaxYear.title"), { doc =>
              doc.select("#back").attr("href") shouldBe triage.routes.MultipleDisposalsTriageController
                .wereAllPropertiesResidential()
                .url
              doc
                .select("#content > article > form")
                .attr("action") shouldBe routes.MultipleDisposalsTriageController
                .whenWereContractsExchangedSubmit()
                .url
            }
          )
        }

        "the journey is complete" in {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionDataWithStartingNewDraftReturn(sample[CompleteMultipleDisposalsAnswers])._1)

          checkPageIsDisplayed(
            performAction,
            messageFromMessageKey("multipleDisposalsTaxYear.title"), { doc =>
              doc.select("#back").attr("href") shouldBe triage.routes.MultipleDisposalsTriageController
                .checkYourAnswers()
                .url
              doc
                .select("#content > article > form")
                .attr("action") shouldBe routes.MultipleDisposalsTriageController
                .whenWereContractsExchangedSubmit()
                .url
            }
          )
        }

      }
    }

    "handling submits on the tax year exchanged page" must {

      def mockGetTaxYear(date: LocalDate)(response: Either[Error, Option[TaxYear]]) =
        (mockTaxYearService
          .taxYear(_: LocalDate)(_: HeaderCarrier))
          .expects(date, *)
          .returning(EitherT.fromEither[Future](response))

      def performAction(data: (String, String)*): Future[Result] =
        controller.whenWereContractsExchangedSubmit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      val today = LocalDate.now(Clock.systemUTC())
      val key   = "multipleDisposalsTaxYear"

      "redirect to redirect to cya page" when {

        val answers = IncompleteMultipleDisposalsAnswers.empty.copy(
          individualUserType           = Some(Self),
          numberOfProperties           = Some(2),
          wasAUKResident               = Some(true),
          countryOfResidence           = Some(Country.uk),
          wereAllPropertiesResidential = Some(true),
          assetTypes                   = Some(List(AssetType.Residential))
        )

        val taxYear = sample[TaxYear].copy(
          startDateInclusive = LocalDate.of(2019, 4, 6),
          endDateExclusive   = LocalDate.of(2020, 4, 6)
        )

        val (session, journey) = sessionDataWithStartingNewDraftReturn(answers)

        "user has not answered the tax year exchanged section and selects after April 06th, 2020" in {

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockGetTaxYear(today)(Right(Some(taxYear)))
            mockStoreSession(
              session.copy(journeyStatus = Some(
                journey.copy(
                  newReturnTriageAnswers = Left(
                    answers.copy(
                      taxYearAfter6April2020 = Some(true),
                      taxYear                = Some(taxYear)
                    )
                  )
                )
              )
              )
            )(Right(()))
          }

          checkIsRedirect(
            performAction(key -> "0"),
            routes.MultipleDisposalsTriageController.checkYourAnswers()
          )
        }

        "user has not answered the tax year exchanged section and selects before April 06th, 2020" in {

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreSession(
              session.copy(journeyStatus = Some(
                journey.copy(
                  newReturnTriageAnswers = Left(
                    answers.copy(
                      taxYearAfter6April2020 = Some(false),
                      taxYear                = None
                    )
                  )
                )
              )
              )
            )(Right(()))
          }

          checkIsRedirect(
            performAction(key -> "1"),
            routes.MultipleDisposalsTriageController.checkYourAnswers()
          )
        }

        "user has already answered were all properties residential section and re-selected different option" in {
          val answers = sample[IncompleteMultipleDisposalsAnswers]
            .copy(
              individualUserType           = Some(Self),
              numberOfProperties           = Some(2),
              wasAUKResident               = Some(true),
              countryOfResidence           = Some(Country.uk),
              wereAllPropertiesResidential = Some(true),
              assetTypes                   = Some(List(AssetType.Residential)),
              taxYearAfter6April2020       = Some(true),
              taxYear                      = Some(taxYear)
            )

          val (session, journey) = sessionDataWithStartingNewDraftReturn(answers)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreSession(
              session.copy(journeyStatus = Some(
                journey.copy(
                  newReturnTriageAnswers = Left(
                    answers.copy(
                      taxYearAfter6April2020 = Some(false),
                      taxYear                = None
                    )
                  )
                )
              )
              )
            )(Right(()))
          }

          checkIsRedirect(
            performAction(key -> "1"),
            routes.MultipleDisposalsTriageController.checkYourAnswers()
          )
        }

      }

      "not update the session" when {

        "user has already answered the tax year exchanged section and re-selected same option" in {
          val taxYear = sample[TaxYear].copy(
            startDateInclusive = LocalDate.of(2019, 4, 6),
            endDateExclusive   = LocalDate.of(2020, 4, 6)
          )
          val answers = sample[IncompleteMultipleDisposalsAnswers]
            .copy(
              individualUserType           = Some(Self),
              numberOfProperties           = Some(2),
              wasAUKResident               = Some(true),
              countryOfResidence           = Some(Country.uk),
              wereAllPropertiesResidential = Some(true),
              assetTypes                   = Some(List(AssetType.Residential)),
              taxYearAfter6April2020       = Some(true),
              taxYear                      = Some(taxYear)
            )

          val (session, _) = sessionDataWithStartingNewDraftReturn(answers)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkIsRedirect(
            performAction(key -> "0"),
            routes.MultipleDisposalsTriageController.checkYourAnswers()
          )
        }

      }

      "show a form error" when {

        "the user submits nothing" in {
          val answers = IncompleteMultipleDisposalsAnswers.empty.copy(
            individualUserType           = Some(Self),
            numberOfProperties           = Some(2),
            wasAUKResident               = Some(true),
            countryOfResidence           = Some(Country.uk),
            wereAllPropertiesResidential = Some(true)
          )
          val (session, _) = sessionDataWithStartingNewDraftReturn(answers)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(s"$key.title"), { doc =>
              doc.select("#error-summary-display > ul > li > a").text() shouldBe messageFromMessageKey(
                s"$key.error.required"
              )
              doc.title() should startWith("Error:")
            },
            BAD_REQUEST
          )
        }

      }
    }

    "handling requests to display the country of residence page" must {

      def performAction(): Future[Result] =
        controller.countryOfResidence()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(performAction, isValidJourney)

      "display the page" when {

        "the journey is incomplete" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithStartingNewDraftReturn(
                IncompleteMultipleDisposalsAnswers.empty.copy(
                  individualUserType = Some(Self),
                  numberOfProperties = Some(2),
                  wasAUKResident     = Some(false)
                )
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction,
            messageFromMessageKey("multipleDisposalsCountryOfResidence.title"), { doc =>
              doc.select("#back").attr("href") shouldBe triage.routes.MultipleDisposalsTriageController
                .wereYouAUKResident()
                .url
              doc
                .select("#content > article > form")
                .attr("action") shouldBe routes.MultipleDisposalsTriageController
                .countryOfResidenceSubmit()
                .url
            }
          )
        }

        "the journey is complete" in {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionDataWithStartingNewDraftReturn(sample[CompleteMultipleDisposalsAnswers])._1)

          checkPageIsDisplayed(
            performAction,
            messageFromMessageKey("multipleDisposalsCountryOfResidence.title"), { doc =>
              doc.select("#back").attr("href") shouldBe triage.routes.MultipleDisposalsTriageController
                .checkYourAnswers()
                .url
              doc
                .select("#content > article > form")
                .attr("action") shouldBe routes.MultipleDisposalsTriageController
                .countryOfResidenceSubmit()
                .url
            }
          )
        }

      }

      "redirect to the cya page" in {
        mockAuthWithNoRetrievals()
        mockGetSession(
          sessionDataWithStartingNewDraftReturn(
            IncompleteMultipleDisposalsAnswers.empty.copy(
              individualUserType = Some(Self),
              numberOfProperties = Some(2),
              wasAUKResident     = Some(true)
            )
          )._1
        )

        checkIsRedirect(
          performAction(),
          routes.MultipleDisposalsTriageController.checkYourAnswers()
        )
      }

    }

    "handling submits on the country of residence page" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.countryOfResidenceSubmit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      val key                        = "countryCode"
      val (countryCode, countryName) = "HK" -> "Hong Kong"
      val country                    = Country(countryCode, Some(countryName))

      "redirect to redirect to cya page" when {

        val answers = IncompleteMultipleDisposalsAnswers.empty.copy(
          individualUserType = Some(Self),
          numberOfProperties = Some(2),
          wasAUKResident     = Some(false),
          countryOfResidence = None
        )

        val (session, journey) = sessionDataWithStartingNewDraftReturn(answers)

        "user has not answered the country of residence section and " +
          "enters valid country" in {

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreSession(
              session.copy(journeyStatus = Some(
                journey.copy(
                  newReturnTriageAnswers = Left(
                    answers.copy(
                      countryOfResidence = Some(country)
                    )
                  )
                )
              )
              )
            )(Right(()))
          }

          checkIsRedirect(
            performAction(key -> countryCode),
            routes.MultipleDisposalsTriageController.checkYourAnswers()
          )
        }

        "user has already answered the country of residence section and " +
          "re-selected different option" in {
          val answers = sample[IncompleteMultipleDisposalsAnswers]
            .copy(
              countryOfResidence           = Some(country),
              wereAllPropertiesResidential = None,
              assetTypes                   = None
            )

          val (session, journey) = sessionDataWithStartingNewDraftReturn(answers)

          val (newCountryCode, newCountryName) = "CH" -> "Switzerland"
          val newCountry                       = Country(newCountryCode, Some(newCountryName))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreSession(
              session.copy(journeyStatus = Some(
                journey.copy(
                  newReturnTriageAnswers = Left(
                    answers.copy(
                      countryOfResidence = Some(newCountry)
                    )
                  )
                )
              )
              )
            )(Right(()))
          }

          checkIsRedirect(
            performAction(key -> newCountryCode),
            routes.MultipleDisposalsTriageController.checkYourAnswers()
          )
        }

      }

      "not update the session" when {

        "user has already answered country of residence section and" +
          "re-selected same option" in {
          val answers = sample[IncompleteMultipleDisposalsAnswers]
            .copy(
              numberOfProperties = Some(2),
              wasAUKResident     = Some(false),
              countryOfResidence = Some(country)
            )

          val (session, _) = sessionDataWithStartingNewDraftReturn(answers)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkIsRedirect(
            performAction(key -> "HK"),
            routes.MultipleDisposalsTriageController.checkYourAnswers()
          )
        }

      }

      "show a form error" when {

        "the user submits nothing" in {
          val answers = IncompleteMultipleDisposalsAnswers.empty.copy(
            individualUserType = Some(Self),
            numberOfProperties = Some(2),
            wasAUKResident     = Some(false),
            countryOfResidence = None
          )
          val (session, _) = sessionDataWithStartingNewDraftReturn(answers)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(s"multipleDisposalsCountryOfResidence.title"), { doc =>
              doc.select("#error-summary-display > ul > li > a").text() shouldBe messageFromMessageKey(
                s"$key.error.required"
              )
              doc.title() should startWith("Error:")
            },
            BAD_REQUEST
          )
        }

        "the option is not recognised" in {
          val answers = IncompleteMultipleDisposalsAnswers.empty
            .copy(
              numberOfProperties = Some(2),
              wasAUKResident     = Some(false),
              countryOfResidence = None
            )

          val (session, _) = sessionDataWithStartingNewDraftReturn(answers)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkPageIsDisplayed(
            performAction(key -> "XX"),
            messageFromMessageKey(s"multipleDisposalsCountryOfResidence.title"), { doc =>
              doc.select("#error-summary-display > ul > li > a").text() shouldBe messageFromMessageKey(
                s"$key.error.notFound"
              )
              doc.title() should startWith("Error:")
            },
            BAD_REQUEST
          )
        }

      }

    }

    "handling requests to display the asset type for non-uk residents page" must {

      val (countryCode, countryName) = "HK" -> "Hong Kong"
      val country                    = Country(countryCode, Some(countryName))

      def performAction(): Future[Result] =
        controller.assetTypeForNonUkResidents()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(performAction, isValidJourney)

      "display the page" when {

        "the journey is incomplete" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithStartingNewDraftReturn(
                IncompleteMultipleDisposalsAnswers.empty.copy(
                  individualUserType = Some(Self),
                  numberOfProperties = Some(2),
                  wasAUKResident     = Some(false),
                  countryOfResidence = Some(country)
                )
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction,
            messageFromMessageKey("multipleDisposalsAssetTypeForNonUkResidents.title"), { doc =>
              doc.select("#back").attr("href") shouldBe triage.routes.MultipleDisposalsTriageController
                .countryOfResidence()
                .url
              doc
                .select("#content > article > form")
                .attr("action") shouldBe routes.MultipleDisposalsTriageController
                .assetTypeForNonUkResidentsSubmit()
                .url
            }
          )
        }

        "the journey is complete" in {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionDataWithStartingNewDraftReturn(sample[CompleteMultipleDisposalsAnswers])._1)

          checkPageIsDisplayed(
            performAction,
            messageFromMessageKey("multipleDisposalsAssetTypeForNonUkResidents.title"), { doc =>
              doc.select("#back").attr("href") shouldBe triage.routes.MultipleDisposalsTriageController
                .checkYourAnswers()
                .url
              doc
                .select("#content > article > form")
                .attr("action") shouldBe routes.MultipleDisposalsTriageController
                .assetTypeForNonUkResidentsSubmit()
                .url
            }
          )
        }

      }

    }

    "handling submits on the asset type for non-uk residents page" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.assetTypeForNonUkResidentsSubmit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      val (countryCode, countryName) = "HK" -> "Hong Kong"
      val country                    = Country(countryCode, Some(countryName))

      val key = "multipleDisposalsAssetTypeForNonUkResidents"

      "redirect to redirect to cya page" when {

        val answers = IncompleteMultipleDisposalsAnswers.empty.copy(
          individualUserType = Some(Self),
          numberOfProperties = Some(2),
          wasAUKResident     = Some(false),
          countryOfResidence = Some(country)
        )

        val (session, journey) = sessionDataWithStartingNewDraftReturn(answers)

        "user has not answered the asset type for non-uk residents section and " +
          "selects residential checkbox" in {

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreSession(
              session.copy(journeyStatus = Some(
                journey.copy(
                  newReturnTriageAnswers = Left(
                    answers.copy(
                      assetTypes = Some(List(AssetType.Residential))
                    )
                  )
                )
              )
              )
            )(Right(()))
          }

          checkIsRedirect(
            performAction(s"$key[]" -> "0"),
            routes.MultipleDisposalsTriageController.checkYourAnswers()
          )
        }

        "user has not answered the asset type for non-uk residents section and " +
          "selects mixed use checkbox" in {

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreSession(
              session.copy(journeyStatus = Some(
                journey.copy(
                  newReturnTriageAnswers = Left(
                    answers.copy(
                      assetTypes = Some(List(AssetType.MixedUse))
                    )
                  )
                )
              )
              )
            )(Right(()))
          }

          checkIsRedirect(
            performAction(s"$key[]" -> "2"),
            routes.MultipleDisposalsTriageController.checkYourAnswers()
          )
        }

        "user has not answered the asset type for non-uk residents section and " +
          "selects more than one asset type checkboxes" in {

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreSession(
              session.copy(journeyStatus = Some(
                journey.copy(
                  newReturnTriageAnswers = Left(
                    answers.copy(
                      assetTypes = Some(List(AssetType.Residential, AssetType.MixedUse))
                    )
                  )
                )
              )
              )
            )(Right(()))
          }

          checkIsRedirect(
            performAction(s"$key[]" -> "0", s"$key[]" -> "2"),
            routes.MultipleDisposalsTriageController.checkYourAnswers()
          )
        }

        "user has already answered the asset type for non-uk residents section and " +
          "re-selected different asset type" in {
          val answers = sample[IncompleteMultipleDisposalsAnswers]
            .copy(
              individualUserType = Some(Self),
              numberOfProperties = Some(2),
              wasAUKResident     = Some(false),
              assetTypes         = Some(List(AssetType.Residential))
            )

          val (session, journey) = sessionDataWithStartingNewDraftReturn(answers)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreSession(
              session.copy(journeyStatus = Some(
                journey.copy(
                  newReturnTriageAnswers = Left(
                    answers.copy(
                      assetTypes = Some(List(AssetType.NonResidential))
                    )
                  )
                )
              )
              )
            )(Right(()))
          }

          checkIsRedirect(
            performAction(s"$key[]" -> "1"),
            routes.MultipleDisposalsTriageController.checkYourAnswers()
          )
        }

      }

      "not update the session" when {

        "user has already answered the asset type for non-uk residents section and " +
          "re-selected same asset type" in {
          val answers = sample[IncompleteMultipleDisposalsAnswers]
            .copy(
              individualUserType = Some(Self),
              numberOfProperties = Some(2),
              wasAUKResident     = Some(false),
              assetTypes         = Some(List(AssetType.Residential))
            )

          val (session, _) = sessionDataWithStartingNewDraftReturn(answers)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkIsRedirect(
            performAction(s"$key[]" -> "0"),
            routes.MultipleDisposalsTriageController.checkYourAnswers()
          )
        }

      }

      "show a form error" when {

        "the user submits nothing" in {
          val answers = IncompleteMultipleDisposalsAnswers.empty.copy(
            individualUserType = Some(Self),
            numberOfProperties = Some(2),
            wasAUKResident     = Some(false),
            countryOfResidence = Some(country),
            assetTypes         = None
          )
          val (session, _) = sessionDataWithStartingNewDraftReturn(answers)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(s"multipleDisposalsAssetTypeForNonUkResidents.title"), { doc =>
              doc.select("#error-summary-display > ul > li > a").text() shouldBe messageFromMessageKey(
                s"$key.error.required"
              )
              doc.title() should startWith("Error:")
            },
            BAD_REQUEST
          )
        }

      }

    }

    "handling requests to display the completion date page" must {

      def performAction(): Future[Result] = controller.completionDate()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(performAction, isValidJourney)

      "display the page" when {

        "the user is starting s new draft return and" when {

          "they have not answered the question before" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionDataWithStartingNewDraftReturn(
                  IncompleteMultipleDisposalsAnswers.empty.copy(
                    Some(sample[IndividualUserType]),
                    Some(2),
                    Some(true),
                    None,
                    Some(true),
                    Some(List(AssetType.Residential)),
                    Some(true),
                    Some(sample[TaxYear]),
                    None
                  )
                )._1
              )
            }

            checkPageIsDisplayed(
              performAction(),
              messageFromMessageKey("multipleDisposalsCompletionDate.title"), { doc =>
                doc.select("#back").attr("href") shouldBe routes.MultipleDisposalsTriageController
                  .whenWereContractsExchanged()
                  .url
                doc
                  .select("#content > article > form")
                  .attr("action") shouldBe routes.MultipleDisposalsTriageController
                  .completionDateSubmit()
                  .url
              }
            )
          }

          "they have answered the question before" in {
            val answers = sample[CompleteMultipleDisposalsAnswers]
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(sessionDataWithStartingNewDraftReturn(answers)._1)
            }

            checkPageIsDisplayed(
              performAction(),
              messageFromMessageKey("multipleDisposalsCompletionDate.title"), { doc =>
                doc.select("#back").attr("href") shouldBe routes.MultipleDisposalsTriageController
                  .checkYourAnswers()
                  .url
                doc
                  .select("#content > article > form")
                  .attr("action") shouldBe routes.MultipleDisposalsTriageController
                  .completionDateSubmit()
                  .url
                doc
                  .select("#multipleDisposalsCompletionDate-day")
                  .attr("value") shouldBe s"${answers.completionDate.value.getDayOfMonth()}"
                doc
                  .select("#multipleDisposalsCompletionDate-month")
                  .attr("value") shouldBe s"${answers.completionDate.value.getMonthValue()}"
                doc
                  .select("#multipleDisposalsCompletionDate-year")
                  .attr("value") shouldBe s"${answers.completionDate.value.getYear()}"
              }
            )
          }

        }

      }

    }

    "handling submitted completion dates" must {

      def performAction(formData: (String, String)*): Future[Result] =
        controller.completionDateSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*))

      def formData(d: LocalDate): List[(String, String)] =
        List(
          "multipleDisposalsCompletionDate-day"   -> d.getDayOfMonth.toString,
          "multipleDisposalsCompletionDate-month" -> d.getMonthValue.toString,
          "multipleDisposalsCompletionDate-year"  -> d.getYear.toString
        )

      behave like redirectToStartWhenInvalidJourney(() => performAction(), isValidJourney)

      "show a form error" when {

        def testFormError(formData: List[(String, String)])(expectedErrorMessageKey: String) = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionDataWithStartingNewDraftReturn(sample[CompleteMultipleDisposalsAnswers])._1)
          }

          checkPageIsDisplayed(
            performAction(formData: _*),
            messageFromMessageKey("multipleDisposalsCompletionDate.title"), { doc =>
              doc.select("#error-summary-display > ul > li > a").text() shouldBe messageFromMessageKey(
                expectedErrorMessageKey
              )
            },
            BAD_REQUEST
          )
        }

        "the date entered is invalid" in {
          DateErrorScenarios
            .dateErrorScenarios(
              "multipleDisposalsCompletionDate"
            )
            .foreach { scenario =>
              withClue(s"For date error scenario $scenario: ") {
                val data = List(
                  "multipleDisposalsCompletionDate-day"   -> scenario.dayInput,
                  "multipleDisposalsCompletionDate-month" -> scenario.monthInput,
                  "multipleDisposalsCompletionDate-year"  -> scenario.yearInput
                ).collect { case (key, Some(value)) => key -> value }

                testFormError(data)(scenario.expectedErrorMessageKey)
              }
            }
        }

        "the date entered is later than today" in {
          testFormError(formData(LocalDateUtils.today().plusDays(1L)))(
            "multipleDisposalsCompletionDate.error.tooFarInFuture"
          )
        }
      }

      "show an error page" when {

        "there is an error updating the session" in {
          val answers =
            sample[CompleteMultipleDisposalsAnswers].copy(completionDate = CompletionDate(LocalDateUtils.today()))
          val (session, journey) = sessionDataWithStartingNewDraftReturn(answers)

          val newCompletionDate = CompletionDate(answers.completionDate.value.minusDays(1L))
          val updatedJourney =
            journey.copy(newReturnTriageAnswers = Left(answers.copy(completionDate = newCompletionDate)))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreSession(session.copy(journeyStatus = Some(updatedJourney)))(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction(formData(newCompletionDate.value): _*))
        }

      }

      "redirect to the check your answers page" when {

        "the user submits a valid value and" when {

          "the user has not answered the question before" in {
            val answers            = IncompleteMultipleDisposalsAnswers.empty
            val (session, journey) = sessionDataWithStartingNewDraftReturn(answers)

            val newCompletionDate = CompletionDate(LocalDateUtils.today())
            val updatedJourney =
              journey.copy(newReturnTriageAnswers = Left(answers.copy(completionDate = Some(newCompletionDate))))

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockStoreSession(session.copy(journeyStatus = Some(updatedJourney)))(Right(()))
            }

            checkIsRedirect(
              performAction(formData(newCompletionDate.value): _*),
              routes.MultipleDisposalsTriageController.checkYourAnswers()
            )
          }

          "the user has already answered the question" in {
            val answers =
              sample[CompleteMultipleDisposalsAnswers].copy(completionDate = CompletionDate(LocalDateUtils.today()))
            val (session, journey) = sessionDataWithStartingNewDraftReturn(answers)

            val newCompletionDate = CompletionDate(answers.completionDate.value.minusDays(1L))
            val updatedJourney =
              journey.copy(newReturnTriageAnswers = Left(answers.copy(completionDate = newCompletionDate)))

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockStoreSession(session.copy(journeyStatus = Some(updatedJourney)))(Right(()))
            }

            checkIsRedirect(
              performAction(formData(newCompletionDate.value): _*),
              routes.MultipleDisposalsTriageController.checkYourAnswers()
            )
          }

        }

        "not perform any updates" when {

          "the date submitted is the same as one that already exists in session" in {
            val answers =
              sample[CompleteMultipleDisposalsAnswers].copy(completionDate = CompletionDate(LocalDateUtils.today()))
            val (session, journey) = sessionDataWithStartingNewDraftReturn(answers)

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
            }

            checkIsRedirect(
              performAction(formData(answers.completionDate.value): _*),
              routes.MultipleDisposalsTriageController.checkYourAnswers()
            )
          }

        }

      }

    }

    "handling requests to display the check your answers page" must {

      def performAction(): Future[Result] =
        controller.checkYourAnswers()(FakeRequest())

      val completeAnswersUk = CompleteMultipleDisposalsAnswers(
        Some(IndividualUserType.Self),
        2,
        Country.uk,
        List(AssetType.Residential),
        sample[TaxYear],
        sample[CompletionDate]
      )

      val allQuestionsAnsweredUk = IncompleteMultipleDisposalsAnswers(
        completeAnswersUk.individualUserType,
        Some(completeAnswersUk.numberOfProperties),
        Some(true),
        None,
        Some(true),
        Some(completeAnswersUk.assetTypes),
        Some(true),
        Some(completeAnswersUk.taxYear),
        Some(completeAnswersUk.completionDate)
      )

      val completeAnswersNonUk = CompleteMultipleDisposalsAnswers(
        Some(IndividualUserType.Self),
        2,
        sample[Country],
        List(AssetType.Residential),
        sample[TaxYear],
        sample[CompletionDate]
      )

      val allQuestionsAnsweredNonUk = IncompleteMultipleDisposalsAnswers(
        completeAnswersNonUk.individualUserType,
        Some(completeAnswersNonUk.numberOfProperties),
        Some(false),
        Some(completeAnswersNonUk.countryOfResidence),
        None,
        Some(completeAnswersNonUk.assetTypes),
        Some(true),
        Some(completeAnswersNonUk.taxYear),
        Some(completeAnswersNonUk.completionDate)
      )

      def testRedirectWhenIncomplete(answers: IncompleteMultipleDisposalsAnswers, expectedRedirect: Call): Unit = {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            sessionDataWithStartingNewDraftReturn(
              answers,
              Right(sample[IndividualName])
            )._1
          )
        }

        checkIsRedirect(performAction(), expectedRedirect)

      }

      behave like redirectToStartWhenInvalidJourney(performAction, isValidJourney)

      "redirect to the who is individual representing page when no individual user type can be found and the subscribed " +
        "user type is individual" in {
        testRedirectWhenIncomplete(
          IncompleteMultipleDisposalsAnswers.empty,
          routes.CommonTriageQuestionsController.whoIsIndividualRepresenting()
        )
      }

      "redirect to the capacitors and personal representatives not handled page" when {

        "an individual user type of capacitor is found" in {
          testRedirectWhenIncomplete(
            allQuestionsAnsweredUk.copy(individualUserType = Some(IndividualUserType.Capacitor)),
            routes.CommonTriageQuestionsController.capacitorsAndPersonalRepresentativesNotHandled()
          )
        }

        "an individual user type of personal representative is found" in {
          testRedirectWhenIncomplete(
            allQuestionsAnsweredUk.copy(individualUserType = Some(IndividualUserType.PersonalRepresentative)),
            routes.CommonTriageQuestionsController.capacitorsAndPersonalRepresentativesNotHandled()
          )

        }

      }

      "redirect to the multiple disposals guidance page when no answer for the number of properties can be found" in {
        testRedirectWhenIncomplete(
          allQuestionsAnsweredUk.copy(numberOfProperties = None),
          routes.MultipleDisposalsTriageController.guidance()
        )
      }

      "redirect the were you a uk resident page" when {

        "the user has not answered that question" in {
          testRedirectWhenIncomplete(
            allQuestionsAnsweredUk.copy(wasAUKResident = None),
            routes.MultipleDisposalsTriageController.wereYouAUKResident()
          )
        }

      }

      "redirect to the country of residence page" when {

        "the user was not a non uk resident and they have not selected a country yet" in {
          testRedirectWhenIncomplete(
            allQuestionsAnsweredNonUk.copy(countryOfResidence = None),
            routes.MultipleDisposalsTriageController.countryOfResidence()
          )
        }

      }

      "redirect to the asset type for non uk residents page" when {

        "the user was not a non uk resident and they have not selected asset types yet" in {
          testRedirectWhenIncomplete(
            allQuestionsAnsweredNonUk.copy(assetTypes = None),
            routes.MultipleDisposalsTriageController.assetTypeForNonUkResidents()
          )
        }

      }

      "redirect to the asset types not implemented page" when {

        "the user was not a non uk resident and they have selected asset types that are not supported" in {
          forAll { assetTypes: List[AssetType] =>
            whenever(assetTypes =!= List(AssetType.Residential) && assetTypes =!= List(AssetType.NonResidential)) {

              testRedirectWhenIncomplete(
                allQuestionsAnsweredNonUk.copy(assetTypes = Some(assetTypes)),
                routes.CommonTriageQuestionsController.assetTypeNotYetImplemented()
              )
            }
          }
        }

      }

      "redirect to the were all properties residential page" when {

        "the user was a uk resident and they have not answered the question yet" in {
          testRedirectWhenIncomplete(
            allQuestionsAnsweredUk.copy(wereAllPropertiesResidential = None),
            routes.MultipleDisposalsTriageController.wereAllPropertiesResidential()
          )
        }
      }

      "redirect to uk residents can only dispose of residential properties page" when {

        "the user was a uk resident and they said not all properties were residential" in {
          testRedirectWhenIncomplete(
            allQuestionsAnsweredUk.copy(wereAllPropertiesResidential = Some(false)),
            routes.CommonTriageQuestionsController.ukResidentCanOnlyDisposeResidential()
          )
        }

      }

      "redirect to the tax year page" when {

        "the question has not been answered yet" in {
          testRedirectWhenIncomplete(
            allQuestionsAnsweredUk.copy(taxYearAfter6April2020 = None, taxYear = None),
            routes.MultipleDisposalsTriageController.whenWereContractsExchanged()
          )
        }

      }

      "redirect to the tax year too early page" when {

        "the user indicated that the tax year was before 6th Aprial 2020" in {
          testRedirectWhenIncomplete(
            allQuestionsAnsweredUk.copy(taxYearAfter6April2020 = Some(false)),
            routes.CommonTriageQuestionsController.disposalDateTooEarly()
          )
        }

      }

      "show an error page" when {

        "no tax year can be found when one is expected" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithStartingNewDraftReturn(
                allQuestionsAnsweredUk.copy(taxYearAfter6April2020 = Some(true), taxYear = None),
                Right(sample[IndividualName])
              )._1
            )
          }

          checkIsTechnicalErrorPage(performAction())
        }

      }

      "redirect to the completion date page" when {

        "the question has not been answered yet" in {
          testRedirectWhenIncomplete(
            allQuestionsAnsweredUk.copy(completionDate = None),
            routes.MultipleDisposalsTriageController.completionDate()
          )
        }

      }

      "show an error page" when {

        "there is an error updating the session when converting from inomplete answers to " +
          "complete answers" in {
          val (session, journey) = sessionDataWithStartingNewDraftReturn(allQuestionsAnsweredUk)
          val updatedJourney     = journey.copy(newReturnTriageAnswers = Left(completeAnswersUk))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreSession(session.copy(journeyStatus = Some(updatedJourney)))(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction())

        }

      }

      "show the page" when {

        "the user has already completed the section and " when {

          "they were a uk resident" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(sessionDataWithStartingNewDraftReturn(completeAnswersUk)._1)
            }

            checkPageIsDisplayed(
              performAction(),
              messageFromMessageKey("multipleDisposals.triage.cya.title"), { doc =>
                MultipleDisposalsTriageControllerSpec.validateSingleDisposalTriageCheckYourAnswersPage(
                  completeAnswersUk,
                  None,
                  doc
                )
              }
            )

          }

          "they were a not a uk resident" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(sessionDataWithStartingNewDraftReturn(completeAnswersNonUk)._1)
            }

            checkPageIsDisplayed(
              performAction(),
              messageFromMessageKey("multipleDisposals.triage.cya.title"), { doc =>
                MultipleDisposalsTriageControllerSpec.validateSingleDisposalTriageCheckYourAnswersPage(
                  completeAnswersNonUk,
                  None,
                  doc
                )
              }
            )

          }

          "they user is an agent" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(sessionDataWithStartingNewDraftReturn(completeAnswersNonUk, isAgent = true)._1)
            }

            checkPageIsDisplayed(
              performAction(),
              messageFromMessageKey("multipleDisposals.triage.cya.title"), { doc =>
                MultipleDisposalsTriageControllerSpec.validateSingleDisposalTriageCheckYourAnswersPage(
                  completeAnswersNonUk,
                  Some(UserType.Agent),
                  doc
                )
              }
            )

          }

        }

        "the user has just answered all the question in the section and" when {

          "all updated are successful when the user was a uk resident" in {
            val (session, journey) = sessionDataWithStartingNewDraftReturn(allQuestionsAnsweredUk)
            val updatedJourney     = journey.copy(newReturnTriageAnswers = Left(completeAnswersUk))

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockStoreSession(session.copy(journeyStatus = Some(updatedJourney)))(Right(()))
            }

            checkPageIsDisplayed(
              performAction(),
              messageFromMessageKey("multipleDisposals.triage.cya.title"), { doc =>
                MultipleDisposalsTriageControllerSpec.validateSingleDisposalTriageCheckYourAnswersPage(
                  completeAnswersUk,
                  None,
                  doc
                )
              }
            )

          }

          "all updated are successful when the user was a not uk resident" in {
            val (session, journey) = sessionDataWithStartingNewDraftReturn(allQuestionsAnsweredNonUk)
            val updatedJourney     = journey.copy(newReturnTriageAnswers = Left(completeAnswersNonUk))

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockStoreSession(session.copy(journeyStatus = Some(updatedJourney)))(Right(()))
            }

            checkPageIsDisplayed(
              performAction(),
              messageFromMessageKey("multipleDisposals.triage.cya.title"), { doc =>
                MultipleDisposalsTriageControllerSpec.validateSingleDisposalTriageCheckYourAnswersPage(
                  completeAnswersNonUk,
                  None,
                  doc
                )
              }
            )

          }
        }

      }
    }

  }

}

object MultipleDisposalsTriageControllerSpec extends Matchers {

  def validateSingleDisposalTriageCheckYourAnswersPage(
    answers: CompleteMultipleDisposalsAnswers,
    userType: Option[UserType],
    doc: Document
  )(implicit messagesApi: MessagesApi, lang: Lang): Unit = {
    implicit val messages = MessagesImpl(lang, messagesApi)

    answers.individualUserType.foreach { individualUserType =>
      doc.select("#individualUserType-answer").text() shouldBe messages(
        if (userType.contains(UserType.Agent)) s"individualUserType.agent.$individualUserType"
        else s"individualUserType.$individualUserType"
      )
    }

    doc.select("#numberOfProperties-answer").text() shouldBe messages("numberOfProperties.MoreThanOne")

    doc.select("#multipleDisposalsNumberOfProperties-answer").text() shouldBe answers.numberOfProperties.toString

    if (answers.countryOfResidence.isUk())
      doc.select("#wereYouAUKResident-answer").text() shouldBe "Yes"
    else
      doc.select("#wereYouAUKResident-answer").text() shouldBe "No"

    if (answers.countryOfResidence.isUk())
      doc.select("#wereAllPropertiesResidential-answer").text() shouldBe "Yes"
    else {
      doc.select("#countryOfResidence-answer").text() shouldBe answers.countryOfResidence.name
        .getOrElse(answers.countryOfResidence.code)
      doc.select("#assetTypeForNonUkResidents-answer").text() shouldBe answers.assetTypes
        .map(assetType => messages(s"multipleDisposalsAssetTypeForNonUkResidents.${assetType.toString}"))
        .mkString(", ")
    }

    doc
      .select("#taxYear-answer")
      .text()                                   shouldBe s"${answers.taxYear.startDateInclusive.getYear}/${answers.taxYear.endDateExclusive.getYear}"
    doc.select("#completionDate-answer").text() shouldBe LocalDateUtils.govDisplayFormat(answers.completionDate.value)
  }

}
