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

import cats.instances.future._
import cats.data.EitherT
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
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.StartingNewDraftReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Country
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.{IndividualName, TrustName}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.SubscribedDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.IndividualUserType.Self
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{IndividualUserType, _}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.MultipleDisposalsTriageAnswers.{IncompleteMultipleDisposalsAnswers, _}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.IncompleteSingleDisposalTriageAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, JourneyStatus, SessionData, TaxYear}
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
    name: Either[TrustName, IndividualName] = Right(sample[IndividualName])
  ): (SessionData, StartingNewDraftReturn) = {
    val startingNewDraftReturn = sample[StartingNewDraftReturn].copy(
      newReturnTriageAnswers = Left(multipleDisposalsAnswers),
      subscribedDetails      = sample[SubscribedDetails].copy(name = name)
    )
    SessionData.empty.copy(journeyStatus = Some(startingNewDraftReturn)) -> startingNewDraftReturn
  }

  def sessionWithState(answers: IncompleteMultipleDisposalsAnswers, numberOfProperties: Option[Int]): SessionData = {
    val (session, journey) = sessionDataWithStartingNewDraftReturn(answers)
    session.copy(journeyStatus = Some(
      journey.copy(
        newReturnTriageAnswers = Left(answers.copy(numberOfProperties = numberOfProperties))
      )
    )
    )
  }

  def testFormError(
    data: (String, String)*
  )(expectedErrorMessageKey: String, errorArgs: String*)(pageTitleKey: String, titleArgs: String*)(
    performAction: Seq[(String, String)] => Future[Result],
    currentSession: SessionData = sessionWithState(
      sample[IncompleteMultipleDisposalsAnswers],
      Some(2)
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
                      assetType                    = None
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

      "display the page" in {
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
                      assetType                    = Some(List(AssetType.Residential))
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
                      assetType                    = Some(List(AssetType.NonResidential))
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
              assetType                    = Some(List(AssetType.Residential))
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
                      assetType                    = Some(List(AssetType.NonResidential))
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
              assetType                    = Some(List(AssetType.Residential))
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

      "display the page" in {
        mockAuthWithNoRetrievals()
        mockGetSession(
          sessionDataWithStartingNewDraftReturn(
            IncompleteMultipleDisposalsAnswers.empty.copy(
              individualUserType           = Some(Self),
              numberOfProperties           = Some(2),
              wasAUKResident               = Some(true),
              countryOfResidence           = Some(Country.uk),
              wereAllPropertiesResidential = Some(true),
              assetType                    = Some(List(AssetType.Residential))
            )
          )._1
        )

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
          assetType                    = Some(List(AssetType.Residential))
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
              assetType                    = Some(List(AssetType.Residential)),
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
              assetType                    = Some(List(AssetType.Residential)),
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

      "display the page" in {
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

        "user has not answered the country of residence section and" +
          " enters valid country" in {

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

        "user has already answered the country of residence section and re-selected different option" in {
          val answers = sample[IncompleteMultipleDisposalsAnswers]
            .copy(
              countryOfResidence           = Some(country),
              wereAllPropertiesResidential = None,
              assetType                    = None
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

        "user has already answered country of residence section and re-selected same option" in {
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

        checkIsRedirect(performAction(), routes.CommonTriageQuestionsController.whoIsIndividualRepresenting())
      }

      "redirect to the capacitors and personal representatives not handled page" when {

        "an individual user type of capacitor is found" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                IncompleteMultipleDisposalsAnswers.empty.copy(individualUserType = Some(IndividualUserType.Capacitor)),
                Some(2)
              )
            )
          }

          checkIsRedirect(
            performAction(),
            routes.CommonTriageQuestionsController.capacitorsAndPersonalRepresentativesNotHandled()
          )
        }

        "an individual user type of personal representative is found" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                IncompleteMultipleDisposalsAnswers.empty
                  .copy(individualUserType = Some(IndividualUserType.PersonalRepresentative)),
                Some(2)
              )
            )
          }

          checkIsRedirect(
            performAction(),
            routes.CommonTriageQuestionsController.capacitorsAndPersonalRepresentativesNotHandled()
          )
        }

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
