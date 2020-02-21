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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.acquisitiondetails

import java.time.LocalDate

import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.http.Status.BAD_REQUEST
import play.api.i18n.{Lang, Messages, MessagesApi, MessagesImpl}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.{Call, Result}
import play.api.test.FakeRequest
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.RebasingCutoffDates
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.AmountOfMoneyErrorScenarios.amountOfMoneyErrorScenarios
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.DateErrorScenarios._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.ReturnsServiceSupport
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.FillingOutReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.AcquisitionDetailsAnswers.{CompleteAcquisitionDetailsAnswers, IncompleteAcquisitionDetailsAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.TriageAnswers.IncompleteTriageAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{AmountInPence, Error, LocalDateUtils, SessionData, TaxYear}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService

import scala.concurrent.Future

class AcquisitionDetailsControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with ReturnsServiceSupport
    with ScalaCheckDrivenPropertyChecks
    with RedirectToStartBehaviour {

  override val overrideBindings =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore),
      bind[ReturnsService].toInstance(mockReturnsService)
    )

  lazy val controller = instanceOf[AcquisitionDetailsController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  implicit lazy val messages: Messages = MessagesImpl(Lang("en"), messagesApi)

  def redirectToStartBehaviour(performAction: () => Future[Result]) =
    redirectToStartWhenInvalidJourney(
      performAction, {
        case _: FillingOutReturn => true
        case _                   => false
      }
    )

  def sessionWithState(
    answers: Option[AcquisitionDetailsAnswers],
    assetType: Option[AssetType],
    wasUkResident: Option[Boolean],
    disposalDate: Option[DisposalDate]
  ): (SessionData, FillingOutReturn) = {
    val journey = sample[FillingOutReturn].copy(
      draftReturn = sample[DraftReturn].copy(
        triageAnswers = sample[IncompleteTriageAnswers].copy(
          assetType      = assetType,
          wasAUKResident = wasUkResident,
          disposalDate   = disposalDate
        ),
        acquisitionDetailsAnswers = answers
      )
    )

    SessionData.empty.copy(journeyStatus = Some(journey)) -> journey
  }

  def sessionWithState(
    answers: AcquisitionDetailsAnswers,
    assetType: AssetType,
    wasUkResident: Boolean,
    disposalDate: DisposalDate = sample[DisposalDate]
  ): (SessionData, FillingOutReturn) =
    sessionWithState(Some(answers), Some(assetType), Some(wasUkResident), Some(disposalDate))

  "AcquisitionDetailsController" when {

    "handling requests to display the acquisition method page" must {

      def performAction(): Future[Result] = controller.acquisitionMethod()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      "display the page" when {

        "the user hsa not completed the acquisition details section of the return" in {
          List(Some(IncompleteAcquisitionDetailsAnswers.empty), None).foreach { answers =>
            withClue(s"For answers $answers: ") {
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(sessionWithState(answers, None, None, Some(sample[DisposalDate]))._1)
              }

              checkPageIsDisplayed(
                performAction(),
                messageFromMessageKey("acquisitionMethod.title"), { doc =>
                  doc.select("#back").attr("href") shouldBe controllers.returns.routes.TaskListController.taskList().url
                  doc.select("#content > article > form").attr("action") shouldBe routes.AcquisitionDetailsController
                    .acquisitionMethodSubmit()
                    .url
                }
              )

            }
          }
        }

        "the user hsa already completed the acquisition details section of the return" in {
          List(Some(IncompleteAcquisitionDetailsAnswers.empty), None).foreach { answers =>
            withClue(s"For answers $answers: ") {
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithState(
                    sample[CompleteAcquisitionDetailsAnswers],
                    sample[AssetType],
                    sample[Boolean]
                  )._1
                )
              }

              checkPageIsDisplayed(
                performAction(),
                messageFromMessageKey("acquisitionMethod.title"), { doc =>
                  doc.select("#back").attr("href") shouldBe routes.AcquisitionDetailsController.checkYourAnswers().url
                  doc.select("#content > article > form").attr("action") shouldBe routes.AcquisitionDetailsController
                    .acquisitionMethodSubmit()
                    .url
                }
              )

            }
          }
        }
      }

    }

    "handling submitted answers to the acquisition method page" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.acquisitionMethodSubmit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      behave like redirectToStartBehaviour(() => performAction())

      "show a form error" when {

        def test(data: (String, String)*)(expectedErrorMessageKey: String): Unit =
          testFormError(data: _*)(expectedErrorMessageKey)("acquisitionMethod.title")(
            performAction
          )

        "nothing is submitted" in {
          test()("acquisitionMethod.error.required")
        }

        "an unknown value is submitted" in {
          test("acquisitionMethod" -> "4")("acquisitionMethod.error.required")
        }

        "other is selected with a value" that {

          "that doesn't exist" in {
            test("acquisitionMethod" -> "3")("otherAcquisitionMethod.error.required")
          }

          "that is empty" in {
            test("acquisitionMethod" -> "3", "otherAcquisitionMethod" -> "")("otherAcquisitionMethod.error.required")
          }

          "contains invalid characters" in {
            test("acquisitionMethod" -> "3", "otherAcquisitionMethod" -> "1,234")(
              "otherAcquisitionMethod.error.invalid"
            )
          }

          "is too long" in {
            test("acquisitionMethod" -> "3", "otherAcquisitionMethod" -> ("a" * 36))(
              "otherAcquisitionMethod.error.tooLong"
            )
          }

        }

      }

      "show an error page" when {

        val (method, methodValue) = AcquisitionMethod.Bought -> 0
        val (session, journey)    = sessionWithState(None, None, None, None)
        val updatedDraftReturn = journey.draftReturn.copy(acquisitionDetailsAnswers = Some(
          IncompleteAcquisitionDetailsAnswers.empty.copy(acquisitionMethod = Some(method))
        )
        )
        val updatedSession = session.copy(journeyStatus = Some(journey.copy(draftReturn = updatedDraftReturn)))

        "there is an error updating the draft return" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(updatedDraftReturn)(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction("acquisitionMethod" -> methodValue.toString))
        }

        "there is an error updating the session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(updatedDraftReturn)(Right(()))
            mockStoreSession(updatedSession)(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction("acquisitionMethod" -> methodValue.toString))
        }

      }

      "redirect to the check your answers page" when {

        "the acquisition details journey is incomplete and" when {

          def test(data: (String, String)*)(method: AcquisitionMethod): Unit = {
            val (session, journey) = sessionWithState(None, None, None, None)
            val updatedDraftReturn = journey.draftReturn.copy(acquisitionDetailsAnswers = Some(
              IncompleteAcquisitionDetailsAnswers.empty.copy(acquisitionMethod = Some(method))
            )
            )
            val updatedSession = session.copy(journeyStatus = Some(journey.copy(draftReturn = updatedDraftReturn)))

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockStoreDraftReturn(updatedDraftReturn)(Right(()))
              mockStoreSession(updatedSession)(Right(()))
            }

            checkIsRedirect(performAction(data: _*), routes.AcquisitionDetailsController.checkYourAnswers())
          }

          "the user selects bought it" in {
            test("acquisitionMethod" -> "0")(AcquisitionMethod.Bought)
          }

          "the user selects inherited it" in {
            test("acquisitionMethod" -> "1")(AcquisitionMethod.Inherited)
          }

          "the user selects was gifted it" in {
            test("acquisitionMethod" -> "2")(AcquisitionMethod.Gifted)
          }

          "the user selects other" in {
            test("acquisitionMethod" -> "3", "otherAcquisitionMethod" -> "things")(AcquisitionMethod.Other("things"))
          }

        }

        "the acquisition details journey is complete and" when {

          def test(data: (String, String)*)(oldMethod: AcquisitionMethod, method: AcquisitionMethod): Unit = {
            val answers            = sample[CompleteAcquisitionDetailsAnswers].copy(acquisitionMethod = oldMethod)
            val (session, journey) = sessionWithState(answers, sample[AssetType], sample[Boolean])
            val updatedDraftReturn = journey.draftReturn.copy(acquisitionDetailsAnswers = Some(
              answers.copy(acquisitionMethod = method)
            )
            )
            val updatedSession = session.copy(journeyStatus = Some(journey.copy(draftReturn = updatedDraftReturn)))

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockStoreDraftReturn(updatedDraftReturn)(Right(()))
              mockStoreSession(updatedSession)(Right(()))
            }

            checkIsRedirect(performAction(data: _*), routes.AcquisitionDetailsController.checkYourAnswers())
          }

          "the user selects bought it" in {
            test("acquisitionMethod" -> "0")(AcquisitionMethod.Inherited, AcquisitionMethod.Bought)
          }

          "the user selects inherited it" in {
            test("acquisitionMethod" -> "1")(AcquisitionMethod.Bought, AcquisitionMethod.Inherited)
          }

          "the user selects was gifted it" in {
            test("acquisitionMethod" -> "2")(AcquisitionMethod.Bought, AcquisitionMethod.Gifted)
          }

          "the user selects other" in {
            test("acquisitionMethod" -> "3", "otherAcquisitionMethod" -> "things")(
              AcquisitionMethod.Bought,
              AcquisitionMethod.Other("things")
            )
          }
        }
      }

      "not update the draft return or session" when {

        "the user has selected the same as before" in {
          val (incompleteAnswers, completeAnswers) = sample[IncompleteAcquisitionDetailsAnswers] -> sample[
            CompleteAcquisitionDetailsAnswers
          ]
          List(
            Seq("acquisitionMethod" -> "0") -> incompleteAnswers.copy(acquisitionMethod = Some(AcquisitionMethod.Bought)
            ),
            Seq("acquisitionMethod" -> "1") -> incompleteAnswers.copy(acquisitionMethod =
              Some(AcquisitionMethod.Inherited)
            ),
            Seq("acquisitionMethod" -> "2") -> incompleteAnswers.copy(acquisitionMethod = Some(AcquisitionMethod.Gifted)
            ),
            Seq("acquisitionMethod" -> "3", "otherAcquisitionMethod" -> "other things") -> incompleteAnswers
              .copy(acquisitionMethod = Some(AcquisitionMethod.Other("other things"))),
            Seq("acquisitionMethod" -> "0") -> completeAnswers.copy(acquisitionMethod = AcquisitionMethod.Bought),
            Seq("acquisitionMethod" -> "1") -> completeAnswers.copy(acquisitionMethod = AcquisitionMethod.Inherited),
            Seq("acquisitionMethod" -> "2") -> completeAnswers.copy(acquisitionMethod = AcquisitionMethod.Gifted),
            Seq("acquisitionMethod" -> "3", "otherAcquisitionMethod" -> "other things") -> completeAnswers
              .copy(acquisitionMethod = AcquisitionMethod.Other("other things"))
          ).foreach {
            case (data, answers) =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(sessionWithState(answers, sample[AssetType], sample[Boolean])._1)
              }

              checkIsRedirect(performAction(data: _*), routes.AcquisitionDetailsController.checkYourAnswers())

          }
        }

      }
    }

    "handling requests to display the acquisition date page" must {

      def performAction(): Future[Result] = controller.acquisitionDate()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      "redirect to the task list page" when {

        "there is no disposal date in the session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                Some(sample[CompleteAcquisitionDetailsAnswers]),
                Some(sample[AssetType]),
                Some(sample[Boolean]),
                None
              )._1
            )
          }

          checkIsRedirect(performAction(), controllers.returns.routes.TaskListController.taskList())
        }

      }

      "redirect to the acquisition method page" when {

        "that question has not yet been answered" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[IncompleteAcquisitionDetailsAnswers].copy(acquisitionMethod = None),
                sample[AssetType],
                sample[Boolean]
              )._1
            )
          }

          checkIsRedirect(performAction(), routes.AcquisitionDetailsController.acquisitionMethod())
        }

      }

      "display the page" when {

        "the acquisition details section has not yet been completed" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[IncompleteAcquisitionDetailsAnswers].copy(
                  acquisitionMethod = Some(AcquisitionMethod.Bought)
                ),
                sample[AssetType],
                sample[Boolean]
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("acquisitionDate.title"), { doc =>
              doc.select("#back").attr("href") shouldBe routes.AcquisitionDetailsController.acquisitionMethod().url
              doc.select("#content > article > form").attr("action") shouldBe routes.AcquisitionDetailsController
                .acquisitionDateSubmit()
                .url
            }
          )
        }

        "the acquisition details section has been completed" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(sample[CompleteAcquisitionDetailsAnswers], sample[AssetType], sample[Boolean])._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("acquisitionDate.title"), { doc =>
              doc.select("#back").attr("href") shouldBe routes.AcquisitionDetailsController.checkYourAnswers().url
              doc.select("#content > article > form").attr("action") shouldBe routes.AcquisitionDetailsController
                .acquisitionDateSubmit()
                .url
            }
          )
        }

      }

    }

    "handling submitted answers to the acquisition date page" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.acquisitionDateSubmit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      def formData(date: LocalDate): List[(String, String)] =
        List(
          "acquisitionDate-day"   -> date.getDayOfMonth.toString,
          "acquisitionDate-month" -> date.getMonthValue.toString,
          "acquisitionDate-year"  -> date.getYear.toString
        )

      val disposalDate = DisposalDate(LocalDate.of(2020, 1, 1), sample[TaxYear])

      behave like redirectToStartBehaviour(() => performAction())

      behave like missingAssetTypeAndResidentialStatusBehaviour(() => performAction())

      "redirect to the task list page" when {

        "there is no disposal date in the session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                Some(sample[CompleteAcquisitionDetailsAnswers]),
                Some(sample[AssetType]),
                Some(sample[Boolean]),
                None
              )._1
            )
          }

          checkIsRedirect(performAction(), controllers.returns.routes.TaskListController.taskList())
        }

      }

      "show a form error" when {

        def test(data: (String, String)*)(expectedErrorKey: String): Unit =
          testFormError(data: _*)(expectedErrorKey)("acquisitionDate.title")(
            performAction,
            sessionWithState(
              sample[CompleteAcquisitionDetailsAnswers],
              sample[AssetType],
              sample[Boolean],
              disposalDate
            )._1
          )

        "the date is invalid" in {
          dateErrorScenarios("acquisitionDate").foreach {
            case d @ DateErrorScenario(dayString, monthString, yearString, expectedErrorKey) =>
              withClue(s"For $d: ") {
                val formData =
                  List(
                    "acquisitionDate-day"   -> dayString,
                    "acquisitionDate-month" -> monthString,
                    "acquisitionDate-year"  -> yearString
                  ).collect { case (id, Some(input)) => id -> input }

                test(formData: _*)(expectedErrorKey)
              }
          }
        }

        "the date is after the disposal date" in {
          val tomorrow = disposalDate.value.plusDays(1L)

          test(formData(tomorrow): _*)("acquisitionDate.error.tooFarInFuture")
        }

      }

      "show an error page" when {

        val acquisitionDate = AcquisitionDate(disposalDate.value)
        val answers = sample[CompleteAcquisitionDetailsAnswers]
          .copy(acquisitionDate = AcquisitionDate(acquisitionDate.value.plusDays(1L)))
        val (session, journey) = sessionWithState(answers, sample[AssetType], sample[Boolean], disposalDate)
        val updatedDraftReturn = journey.draftReturn.copy(acquisitionDetailsAnswers = Some(
          answers.copy(acquisitionDate = acquisitionDate)
        )
        )
        val updatedSession = session.copy(journeyStatus = Some(journey.copy(draftReturn = updatedDraftReturn)))

        "there is an error updating the draft return" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(updatedDraftReturn)(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction(formData(acquisitionDate.value): _*))
        }

        "there is an error updating the session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(updatedDraftReturn)(Right(()))
            mockStoreSession(updatedSession)(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction(formData(acquisitionDate.value): _*))
        }
      }

      "update the session correctly and redirect to the cya page" when {
        def test(
          assetType: AssetType,
          wasUkResident: Boolean,
          submittedAcquisitionDate: LocalDate,
          oldAnswers: AcquisitionDetailsAnswers,
          newAnswers: AcquisitionDetailsAnswers
        ): Unit = {
          val (session, journey) = sessionWithState(oldAnswers, assetType, wasUkResident, disposalDate)
          val updatedDraftReturn = journey.draftReturn.copy(acquisitionDetailsAnswers = Some(newAnswers))
          val updatedSession     = session.copy(journeyStatus = Some(journey.copy(draftReturn = updatedDraftReturn)))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(updatedDraftReturn)(Right(()))
            mockStoreSession(updatedSession)(Right(()))

          }

          checkIsRedirect(
            performAction(formData(submittedAcquisitionDate): _*),
            routes.AcquisitionDetailsController.checkYourAnswers()
          )
        }

        "the user has not answered this question before" in {
          val date = disposalDate.value
          val oldAnswers = IncompleteAcquisitionDetailsAnswers.empty.copy(
            acquisitionMethod = Some(AcquisitionMethod.Bought)
          )

          test(
            sample[AssetType],
            sample[Boolean],
            date,
            oldAnswers,
            oldAnswers.copy(acquisitionDate = Some(AcquisitionDate(date)))
          )
        }

        def rebasingCriteriaTests(
          assetType: AssetType,
          wasUkResident: Boolean,
          rebasingCutOffDate: LocalDate
        ): Unit = {
          "the user did satisfy the rebasing criteria and they now do not satisfy it and the acquisition " +
            "details section was incomplete" in {
            val oldAnswers = IncompleteAcquisitionDetailsAnswers.empty.copy(
              acquisitionMethod       = Some(AcquisitionMethod.Bought),
              acquisitionDate         = Some(AcquisitionDate(rebasingCutOffDate.minusDays(1L))),
              rebasedAcquisitionPrice = Some(sample[AmountInPence])
            )

            test(
              assetType,
              wasUkResident,
              rebasingCutOffDate,
              oldAnswers,
              oldAnswers
                .copy(acquisitionDate = Some(AcquisitionDate(rebasingCutOffDate)), rebasedAcquisitionPrice = None)
            )
          }

          "the user did satisfy the rebasing criteria and they now do not satisfy it and the acquisition " +
            "details section was complete" in {
            val oldAnswers = sample[CompleteAcquisitionDetailsAnswers].copy(
              acquisitionDate         = AcquisitionDate(rebasingCutOffDate.minusDays(1L)),
              rebasedAcquisitionPrice = Some(sample[AmountInPence])
            )

            test(
              assetType,
              wasUkResident,
              rebasingCutOffDate,
              oldAnswers,
              oldAnswers.copy(acquisitionDate = AcquisitionDate(rebasingCutOffDate), rebasedAcquisitionPrice = None)
            )

          }

          "the user did not satisfy the rebasing criteria and they satisfy now it and the acquisition " +
            "details section was incomplete" in {
            val oldAnswers = IncompleteAcquisitionDetailsAnswers.empty.copy(
              acquisitionMethod       = Some(AcquisitionMethod.Bought),
              acquisitionDate         = Some(AcquisitionDate(rebasingCutOffDate)),
              acquisitionPrice        = Some(sample[AmountInPence]),
              rebasedAcquisitionPrice = Some(sample[AmountInPence])
            )

            test(
              assetType,
              wasUkResident,
              rebasingCutOffDate.minusDays(1L),
              oldAnswers,
              oldAnswers.copy(
                acquisitionDate         = Some(AcquisitionDate(rebasingCutOffDate.minusDays(1L))),
                acquisitionPrice        = None,
                rebasedAcquisitionPrice = None
              )
            )

          }

          "the user did not satisfy the rebasing criteria and they satisfy now it and the acquisition " +
            "details section was complete" in {
            val oldAnswers = sample[CompleteAcquisitionDetailsAnswers].copy(
              acquisitionDate         = AcquisitionDate(rebasingCutOffDate),
              acquisitionPrice        = sample[AmountInPence],
              rebasedAcquisitionPrice = Some(sample[AmountInPence])
            )

            test(
              assetType,
              wasUkResident,
              rebasingCutOffDate.minusDays(1L),
              oldAnswers,
              IncompleteAcquisitionDetailsAnswers(
                Some(oldAnswers.acquisitionMethod),
                Some(AcquisitionDate(rebasingCutOffDate.minusDays(1L))),
                None,
                None,
                Some(oldAnswers.improvementCosts),
                Some(oldAnswers.acquisitionFees)
              )
            )

          }

          "the user still does not satisfy the rebasing criteria and the acquisition details section " +
            "was incomplete" in {
            val oldAnswers = IncompleteAcquisitionDetailsAnswers.empty.copy(
              acquisitionMethod       = Some(AcquisitionMethod.Bought),
              acquisitionDate         = Some(AcquisitionDate(rebasingCutOffDate)),
              acquisitionPrice        = Some(sample[AmountInPence]),
              rebasedAcquisitionPrice = Some(sample[AmountInPence])
            )

            test(
              assetType,
              wasUkResident,
              rebasingCutOffDate.plusDays(1L),
              oldAnswers,
              oldAnswers.copy(
                acquisitionDate = Some(AcquisitionDate(rebasingCutOffDate.plusDays(1L)))
              )
            )
          }

          "the user still does not satisfy the rebasing criteria and the acquisition details section " +
            "was complete" in {
            val oldAnswers = sample[CompleteAcquisitionDetailsAnswers].copy(
              acquisitionDate         = AcquisitionDate(rebasingCutOffDate),
              acquisitionPrice        = sample[AmountInPence],
              rebasedAcquisitionPrice = Some(sample[AmountInPence])
            )

            test(
              assetType,
              wasUkResident,
              rebasingCutOffDate.plusDays(1L),
              oldAnswers,
              oldAnswers.copy(
                acquisitionDate = AcquisitionDate(rebasingCutOffDate.plusDays(1L))
              )
            )
          }
        }

        "when the user was a uk resident and is disposing of a residential property and " when {

          behave like rebasingCriteriaTests(AssetType.Residential, true, RebasingCutoffDates.ukResidents)

        }

        "when the user was a non uk resident and is disposing of a residential property and " when {

          behave like rebasingCriteriaTests(
            AssetType.Residential,
            false,
            RebasingCutoffDates.nonUkResidentsResidentialProperty
          )

        }

        "when the user was a non uk resident and is disposing of a non-residential property and " when {

          behave like rebasingCriteriaTests(
            AssetType.NonResidential,
            false,
            RebasingCutoffDates.nonUkResidentsNonResidentialProperty
          )

        }

      }

    }

    "handling requests to display the acquisition price page" must {

      def performAction(): Future[Result] = controller.acquisitionPrice()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      "redirect to the acquisition date page" when {

        "that question hasn't been answered" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[IncompleteAcquisitionDetailsAnswers].copy(
                  acquisitionDate = None
                ),
                sample[AssetType],
                sample[Boolean]
              )._1
            )
          }

          checkIsRedirect(
            performAction(),
            routes.AcquisitionDetailsController.acquisitionDate()
          )
        }

      }

      "display the page" when {

        "the acquisition details section has not yet been completed" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[IncompleteAcquisitionDetailsAnswers].copy(
                  acquisitionDate = Some(sample[AcquisitionDate])
                ),
                sample[AssetType],
                sample[Boolean]
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("acquisitionPrice.title"), { doc =>
              doc.select("#back").attr("href") shouldBe routes.AcquisitionDetailsController.acquisitionDate().url
              doc.select("#content > article > form").attr("action") shouldBe routes.AcquisitionDetailsController
                .acquisitionPriceSubmit()
                .url
            }
          )
        }

        "the acquisition details section has been completed" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[CompleteAcquisitionDetailsAnswers],
                sample[AssetType],
                sample[Boolean]
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("acquisitionPrice.title"), { doc =>
              doc.select("#back").attr("href") shouldBe routes.AcquisitionDetailsController.checkYourAnswers().url
              doc.select("#content > article > form").attr("action") shouldBe routes.AcquisitionDetailsController
                .acquisitionPriceSubmit()
                .url
            }
          )
        }
      }

    }

    "handling submitted acquisition prices" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.acquisitionPriceSubmit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      behave like redirectToStartBehaviour(() => performAction())

      "redirect to the acquisition date page" when {

        "that question hasn't been answered" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[IncompleteAcquisitionDetailsAnswers].copy(
                  acquisitionDate = None
                ),
                sample[AssetType],
                sample[Boolean]
              )._1
            )
          }

          checkIsRedirect(
            performAction(),
            routes.AcquisitionDetailsController.acquisitionDate()
          )
        }

      }

      "show a form error" when {

        "the data is invalid" in {
          amountOfMoneyErrorScenarios("acquisitionPrice").foreach { scenario =>
            withClue(s"For $scenario: ") {
              testFormError(scenario.formData: _*)(scenario.expectedErrorMessageKey)(
                "acquisitionPrice.title"
              )(
                performAction
              )
            }
          }
        }

        "the amount of money is zero" in {
          testFormError("acquisitionPrice" -> "0")(
            "acquisitionPrice.error.tooSmall"
          )(
            "acquisitionPrice.title"
          )(
            performAction
          )
        }

      }

      "show an error page" when {

        val price              = 1.23d
        val answers            = IncompleteAcquisitionDetailsAnswers.empty.copy(acquisitionDate = Some(sample[AcquisitionDate]))
        val (session, journey) = sessionWithState(answers, sample[AssetType], sample[Boolean])
        val updatedDraftReturn = journey.draftReturn
          .copy(acquisitionDetailsAnswers = Some(answers.copy(acquisitionPrice = Some(AmountInPence(123L)))))
        val updatedSession = session.copy(journeyStatus = Some(journey.copy(draftReturn = updatedDraftReturn)))

        "there is an error updating the draft return" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(updatedDraftReturn)(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction("acquisitionPrice" -> price.toString))
        }

        "there is an error updating the session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(updatedDraftReturn)(Right(()))
            mockStoreSession(updatedSession)(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction("acquisitionPrice" -> price.toString))
        }

      }

      "redirect to the check you answers page" when {

        "the price submitted is valid and the journey was incomplete" in {
          val answers            = IncompleteAcquisitionDetailsAnswers.empty.copy(acquisitionDate = Some(sample[AcquisitionDate]))
          val (session, journey) = sessionWithState(answers, sample[AssetType], sample[Boolean])
          val updatedDraftReturn = journey.draftReturn
            .copy(acquisitionDetailsAnswers = Some(answers.copy(acquisitionPrice = Some(AmountInPence(123400L)))))
          val updatedSession = session.copy(journeyStatus = Some(journey.copy(draftReturn = updatedDraftReturn)))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(updatedDraftReturn)(Right(()))
            mockStoreSession(updatedSession)(Right(()))
          }

          checkIsRedirect(
            performAction("acquisitionPrice" -> "£1,234"),
            routes.AcquisitionDetailsController.checkYourAnswers()
          )
        }

        "the price submitted is valid and the journey was complete" in {
          val answers            = sample[CompleteAcquisitionDetailsAnswers]
          val (session, journey) = sessionWithState(answers, sample[AssetType], sample[Boolean])
          val updatedDraftReturn = journey.draftReturn
            .copy(acquisitionDetailsAnswers = Some(answers.copy(acquisitionPrice = AmountInPence(123456L))))
          val updatedSession = session.copy(journeyStatus = Some(journey.copy(draftReturn = updatedDraftReturn)))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(updatedDraftReturn)(Right(()))
            mockStoreSession(updatedSession)(Right(()))
          }

          checkIsRedirect(
            performAction("acquisitionPrice" -> "£1,234.56"),
            routes.AcquisitionDetailsController.checkYourAnswers()
          )

        }

      }

    }

    "handling requests to display the rebased acquisition price page" must {

      def performAction(): Future[Result] = controller.rebasedAcquisitionPrice()(FakeRequest())

      val acquisitionDate = AcquisitionDate(LocalDate.ofEpochDay(0L))

      behave like redirectToStartBehaviour(performAction)

      behave like missingAssetTypeAndResidentialStatusBehaviour(performAction)

      "redirect to the acquisition price page" when {

        "that question hasn't been answered" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[IncompleteAcquisitionDetailsAnswers].copy(
                  acquisitionDate  = Some(acquisitionDate),
                  acquisitionPrice = None
                ),
                sample[AssetType],
                sample[Boolean]
              )._1
            )
          }

          checkIsRedirect(
            performAction(),
            routes.AcquisitionDetailsController.acquisitionPrice()
          )
        }

      }

      "redirect to the acquisition date page" when {

        "that question hasn't been answered" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[IncompleteAcquisitionDetailsAnswers].copy(
                  acquisitionDate  = None,
                  acquisitionPrice = Some(sample[AmountInPence])
                ),
                sample[AssetType],
                sample[Boolean]
              )._1
            )
          }

          checkIsRedirect(
            performAction(),
            routes.AcquisitionDetailsController.acquisitionDate()
          )
        }
      }

      "redirect to the check your answers page" when {

        "the user does not meet the rebasing criteria" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[CompleteAcquisitionDetailsAnswers].copy(
                  acquisitionDate = AcquisitionDate(RebasingCutoffDates.ukResidents)
                ),
                AssetType.Residential,
                true
              )._1
            )
          }
          checkIsRedirect(performAction(), routes.AcquisitionDetailsController.checkYourAnswers())

        }

      }

      "display the page" when {

        "the acquisition details section has not yet been completed" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[IncompleteAcquisitionDetailsAnswers].copy(
                  acquisitionDate  = Some(acquisitionDate),
                  acquisitionPrice = Some(sample[AmountInPence])
                ),
                AssetType.Residential,
                true
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(
              "rebaseAcquisitionPrice.title",
              LocalDateUtils.govDisplayFormat(RebasingCutoffDates.ukResidents)
            ), { doc =>
              doc.select("#back").attr("href") shouldBe routes.AcquisitionDetailsController.acquisitionPrice().url
              doc.select("#content > article > form").attr("action") shouldBe routes.AcquisitionDetailsController
                .rebasedAcquisitionPriceSubmit()
                .url
            }
          )
        }

        "the acquisition details section has been completed" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[CompleteAcquisitionDetailsAnswers].copy(acquisitionDate = acquisitionDate),
                AssetType.Residential,
                true
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(
              "rebaseAcquisitionPrice.title",
              LocalDateUtils.govDisplayFormat(RebasingCutoffDates.ukResidents)
            ), { doc =>
              doc.select("#back").attr("href") shouldBe routes.AcquisitionDetailsController.checkYourAnswers().url
              doc.select("#content > article > form").attr("action") shouldBe routes.AcquisitionDetailsController
                .rebasedAcquisitionPriceSubmit()
                .url
            }
          )
        }

        "the amount in the session is zero" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[CompleteAcquisitionDetailsAnswers].copy(
                  acquisitionDate         = acquisitionDate,
                  rebasedAcquisitionPrice = Some(AmountInPence.zero)
                ),
                AssetType.Residential,
                true
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(
              "rebaseAcquisitionPrice.title",
              LocalDateUtils.govDisplayFormat(RebasingCutoffDates.ukResidents)
            ), { doc =>
              doc.select("#rebaseAcquisitionPrice-1").attr("checked") shouldBe "checked"
            }
          )
        }

        "the amount in the session is non-zero" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[CompleteAcquisitionDetailsAnswers].copy(
                  acquisitionDate         = acquisitionDate,
                  rebasedAcquisitionPrice = Some(AmountInPence(1L))
                ),
                AssetType.Residential,
                true
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(
              "rebaseAcquisitionPrice.title",
              LocalDateUtils.govDisplayFormat(RebasingCutoffDates.ukResidents)
            ), { doc =>
              doc.select("#rebaseAcquisitionPrice-0").attr("checked")  shouldBe "checked"
              doc.select("#rebaseAcquisitionPriceValue").attr("value") shouldBe "0.01"
            }
          )
        }

      }

    }

    "handling submitted rebased acquisition price answers" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.rebasedAcquisitionPriceSubmit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      val acquisitionDate = AcquisitionDate(LocalDate.ofEpochDay(0L))

      behave like redirectToStartBehaviour(() => performAction())

      behave like missingAssetTypeAndResidentialStatusBehaviour(() => performAction())

      "redirect to the acquisition price page" when {

        "that question hasn't been answered" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[IncompleteAcquisitionDetailsAnswers].copy(
                  acquisitionDate  = Some(acquisitionDate),
                  acquisitionPrice = None
                ),
                sample[AssetType],
                sample[Boolean]
              )._1
            )
          }

          checkIsRedirect(
            performAction(),
            routes.AcquisitionDetailsController.acquisitionPrice()
          )
        }

      }

      "redirect to the acquisition date page" when {

        "that question hasn't been answered" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[IncompleteAcquisitionDetailsAnswers].copy(
                  acquisitionDate  = None,
                  acquisitionPrice = Some(sample[AmountInPence])
                ),
                sample[AssetType],
                sample[Boolean]
              )._1
            )
          }

          checkIsRedirect(
            performAction(),
            routes.AcquisitionDetailsController.acquisitionDate()
          )
        }
      }

      "show a form error" when {

        def test(data: (String, String)*)(expectedErrorMessageKey: String) = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[CompleteAcquisitionDetailsAnswers].copy(acquisitionDate = acquisitionDate),
                AssetType.Residential,
                true
              )._1
            )
          }

          val formattedRebaseDate = LocalDateUtils.govDisplayFormat(RebasingCutoffDates.ukResidents)
          checkPageIsDisplayed(
            performAction(data: _*),
            messageFromMessageKey(
              "rebaseAcquisitionPrice.title",
              formattedRebaseDate
            ), { doc =>
              doc.select("#error-summary-display > ul > li > a").text() shouldBe messageFromMessageKey(
                expectedErrorMessageKey,
                formattedRebaseDate
              )
            },
            BAD_REQUEST
          )
        }

        "no option has been selected" in {
          test()("rebaseAcquisitionPrice.error.required")
        }

        "the option selected is not valid" in {
          test("rebaseAcquisitionPrice" -> "2")("rebaseAcquisitionPrice.error.invalid")
        }

        "the amount of money is invalid" in {
          amountOfMoneyErrorScenarios("rebaseAcquisitionPriceValue").foreach { scenario =>
            withClue(s"For $scenario: ") {
              val data =
                ("rebaseAcquisitionPrice" -> "0") :: scenario.formData
              test(data: _*)(scenario.expectedErrorMessageKey)
            }
          }
        }

        "the amount of money is zero" in {
          test("rebaseAcquisitionPrice" -> "0", "rebaseAcquisitionPriceValue" -> "0")(
            "rebaseAcquisitionPriceValue.error.tooSmall"
          )
        }

      }

      "show an error page" when {
        val price = 1.23d
        val answers = IncompleteAcquisitionDetailsAnswers.empty.copy(
          acquisitionDate  = Some(acquisitionDate),
          acquisitionPrice = Some(sample[AmountInPence])
        )
        val (session, journey) = sessionWithState(answers, AssetType.Residential, true)
        val updatedDraftReturn = journey.draftReturn
          .copy(acquisitionDetailsAnswers = Some(answers.copy(rebasedAcquisitionPrice = Some(AmountInPence(123L)))))
        val updatedSession = session.copy(journeyStatus = Some(journey.copy(draftReturn = updatedDraftReturn)))

        "there is an error updating the draft return" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(updatedDraftReturn)(Left(Error("")))
          }

          checkIsTechnicalErrorPage(
            performAction(
              "rebaseAcquisitionPrice"      -> "0",
              "rebaseAcquisitionPriceValue" -> price.toString
            )
          )
        }

        "there is an error updating the session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(updatedDraftReturn)(Right(()))
            mockStoreSession(updatedSession)(Left(Error("")))
          }

          checkIsTechnicalErrorPage(
            performAction(
              "rebaseAcquisitionPrice"      -> "0",
              "rebaseAcquisitionPriceValue" -> price.toString
            )
          )
        }

      }

      "redirect to the check you answers page" when {
        val scenarios = List(
          List("rebaseAcquisitionPrice" -> "0", "rebaseAcquisitionPriceValue" -> "£1,234") -> AmountInPence(123400L),
          List("rebaseAcquisitionPrice" -> "1") -> AmountInPence.zero
        )

        "the price submitted is valid and the journey was incomplete" in {
          scenarios.foreach {
            case (formData, expectedAmountInPence) =>
              withClue(s"For form data $formData and expected amount in pence $expectedAmountInPence: ") {
                val answers = IncompleteAcquisitionDetailsAnswers.empty.copy(
                  acquisitionDate  = Some(acquisitionDate),
                  acquisitionPrice = Some(sample[AmountInPence])
                )
                val (session, journey) = sessionWithState(answers, AssetType.Residential, true)
                val updatedDraftReturn = journey.draftReturn
                  .copy(acquisitionDetailsAnswers =
                    Some(answers.copy(rebasedAcquisitionPrice = Some(expectedAmountInPence)))
                  )
                val updatedSession = session.copy(journeyStatus = Some(journey.copy(draftReturn = updatedDraftReturn)))

                inSequence {
                  mockAuthWithNoRetrievals()
                  mockGetSession(session)
                  mockStoreDraftReturn(updatedDraftReturn)(Right(()))
                  mockStoreSession(updatedSession)(Right(()))
                }

                checkIsRedirect(
                  performAction(formData: _*),
                  routes.AcquisitionDetailsController.checkYourAnswers()
                )
              }
          }
        }

        "the price submitted is valid and the journey was complete" in {
          scenarios.foreach {
            case (formData, expectedAmountInPence) =>
              withClue(s"For form data $formData and expected amount in pence $expectedAmountInPence: ") {
                val answers = sample[CompleteAcquisitionDetailsAnswers].copy(
                  acquisitionDate         = acquisitionDate,
                  rebasedAcquisitionPrice = Some(AmountInPence(expectedAmountInPence.value + 1L))
                )
                val (session, journey) = sessionWithState(answers, AssetType.Residential, true)
                val updatedDraftReturn = journey.draftReturn
                  .copy(acquisitionDetailsAnswers =
                    Some(answers.copy(rebasedAcquisitionPrice = Some(expectedAmountInPence)))
                  )
                val updatedSession = session.copy(journeyStatus = Some(journey.copy(draftReturn = updatedDraftReturn)))

                inSequence {
                  mockAuthWithNoRetrievals()
                  mockGetSession(session)
                  mockStoreDraftReturn(updatedDraftReturn)(Right(()))
                  mockStoreSession(updatedSession)(Right(()))
                }

                checkIsRedirect(
                  performAction(formData: _*),
                  routes.AcquisitionDetailsController.checkYourAnswers()
                )

              }
          }
        }
      }

    }

    "handling requests to display the improvement costs page" must {

      def performAction(): Future[Result] =
        controller.improvementCosts()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      behave like missingAssetTypeAndResidentialStatusBehaviour(performAction)

      "redirect to the acquisition date page" when {

        "the question has not been answered" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[IncompleteAcquisitionDetailsAnswers].copy(
                  acquisitionDate = None
                ),
                sample[AssetType],
                sample[Boolean]
              )._1
            )
          }

          checkIsRedirect(
            performAction(),
            routes.AcquisitionDetailsController.acquisitionDate()
          )

        }

      }

      "redirect to the acquisition price page" when {

        "the user does not meet the rebasing criteria and the user has not supplied an acquisition price yet" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[IncompleteAcquisitionDetailsAnswers].copy(
                  acquisitionDate  = Some(AcquisitionDate(RebasingCutoffDates.ukResidents)),
                  acquisitionPrice = None
                ),
                AssetType.Residential,
                true
              )._1
            )
          }

          checkIsRedirect(
            performAction(),
            routes.AcquisitionDetailsController.acquisitionPrice()
          )
        }

      }

      "redirect to the rebased acquisition price page" when {

        "the user does meet the rebasing criteria and the user has not supplied an acquisition price yet" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[IncompleteAcquisitionDetailsAnswers].copy(
                  acquisitionDate         = Some(AcquisitionDate(RebasingCutoffDates.ukResidents.minusDays(1L))),
                  rebasedAcquisitionPrice = None
                ),
                AssetType.Residential,
                true
              )._1
            )
          }

          checkIsRedirect(
            performAction(),
            routes.AcquisitionDetailsController.rebasedAcquisitionPrice()
          )
        }

      }

      "display the page" when {

        "the user meets the rebasing criteria and their acquisition details journey is incomplete" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[IncompleteAcquisitionDetailsAnswers].copy(
                  acquisitionDate         = Some(AcquisitionDate(RebasingCutoffDates.ukResidents.minusDays(1L))),
                  rebasedAcquisitionPrice = Some(sample[AmountInPence])
                ),
                AssetType.Residential,
                true
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("improvementCosts.title"), { doc =>
              doc.select("#back").attr("href") shouldBe routes.AcquisitionDetailsController
                .rebasedAcquisitionPrice()
                .url
              doc.select("#content > article > form").attr("action") shouldBe routes.AcquisitionDetailsController
                .improvementCostsSubmit()
                .url
            }
          )
        }

        "the user does not meet the rebasing criteria and their acquisition details journey is incomplete" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[IncompleteAcquisitionDetailsAnswers].copy(
                  acquisitionDate         = Some(AcquisitionDate(RebasingCutoffDates.ukResidents)),
                  acquisitionPrice        = Some(sample[AmountInPence]),
                  rebasedAcquisitionPrice = Some(sample[AmountInPence])
                ),
                AssetType.Residential,
                true
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("improvementCosts.title"), { doc =>
              doc.select("#back").attr("href") shouldBe routes.AcquisitionDetailsController.acquisitionPrice().url
              doc.select("#content > article > form").attr("action") shouldBe routes.AcquisitionDetailsController
                .improvementCostsSubmit()
                .url
            }
          )
        }

        "the user meets the rebasing criteria and their acquisition details journey is complete" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[CompleteAcquisitionDetailsAnswers].copy(
                  acquisitionDate         = AcquisitionDate(RebasingCutoffDates.ukResidents.minusDays(1L)),
                  rebasedAcquisitionPrice = Some(sample[AmountInPence])
                ),
                AssetType.Residential,
                true
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("improvementCosts.title"), { doc =>
              doc.select("#back").attr("href") shouldBe routes.AcquisitionDetailsController.checkYourAnswers().url
              doc.select("#content > article > form").attr("action") shouldBe routes.AcquisitionDetailsController
                .improvementCostsSubmit()
                .url
            }
          )
        }

        "the user does not meet the rebasing criteria and their acquisition details journey is complete" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[CompleteAcquisitionDetailsAnswers].copy(
                  acquisitionDate         = AcquisitionDate(RebasingCutoffDates.ukResidents),
                  rebasedAcquisitionPrice = Some(sample[AmountInPence])
                ),
                AssetType.Residential,
                true
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("improvementCosts.title"), { doc =>
              doc.select("#back").attr("href") shouldBe routes.AcquisitionDetailsController.checkYourAnswers().url
              doc.select("#content > article > form").attr("action") shouldBe routes.AcquisitionDetailsController
                .improvementCostsSubmit()
                .url
            }
          )
        }

        "the amount in the session is zero" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[CompleteAcquisitionDetailsAnswers].copy(
                  acquisitionDate         = AcquisitionDate(RebasingCutoffDates.ukResidents),
                  rebasedAcquisitionPrice = Some(sample[AmountInPence]),
                  improvementCosts        = AmountInPence.zero
                ),
                AssetType.Residential,
                true
              )._1
            )

          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(
              "improvementCosts.title"
            ), { doc =>
              doc.select("#improvementCosts-1").attr("checked") shouldBe "checked"
            }
          )
        }

        "the amount in the session is non-zero" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[CompleteAcquisitionDetailsAnswers].copy(
                  acquisitionDate         = AcquisitionDate(RebasingCutoffDates.ukResidents),
                  rebasedAcquisitionPrice = Some(sample[AmountInPence]),
                  improvementCosts        = AmountInPence(2L)
                ),
                AssetType.Residential,
                true
              )._1
            )

          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(
              "improvementCosts.title"
            ), { doc =>
              doc.select("#improvementCosts-0").attr("checked")  shouldBe "checked"
              doc.select("#improvementCostsValue").attr("value") shouldBe "0.02"
            }
          )
        }

      }

    }

    "handling submitted improvement costs answers" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.improvementCostsSubmit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      val acquisitionDate = AcquisitionDate(LocalDate.ofEpochDay(0L))

      behave like redirectToStartBehaviour(() => performAction())

      behave like missingAssetTypeAndResidentialStatusBehaviour(() => performAction())

      "redirect to the acquisition date page" when {

        "the question has not been answered" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[IncompleteAcquisitionDetailsAnswers].copy(
                  acquisitionDate = None
                ),
                sample[AssetType],
                sample[Boolean]
              )._1
            )
          }

          checkIsRedirect(
            performAction(),
            routes.AcquisitionDetailsController.acquisitionDate()
          )

        }

      }

      "redirect to the acquisition price page" when {

        "the user does not meet the rebasing criteria and the user has not supplied an acquisition price yet" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[IncompleteAcquisitionDetailsAnswers].copy(
                  acquisitionDate  = Some(AcquisitionDate(RebasingCutoffDates.ukResidents)),
                  acquisitionPrice = None
                ),
                AssetType.Residential,
                true
              )._1
            )
          }

          checkIsRedirect(
            performAction(),
            routes.AcquisitionDetailsController.acquisitionPrice()
          )
        }

      }

      "redirect to the rebased acquisition price page" when {

        "the user does meet the rebasing criteria and the user has not supplied an acquisition price yet" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[IncompleteAcquisitionDetailsAnswers].copy(
                  acquisitionDate         = Some(AcquisitionDate(RebasingCutoffDates.ukResidents.minusDays(1L))),
                  rebasedAcquisitionPrice = None
                ),
                AssetType.Residential,
                true
              )._1
            )
          }

          checkIsRedirect(
            performAction(),
            routes.AcquisitionDetailsController.rebasedAcquisitionPrice()
          )
        }

      }

      "show a form error" when {

        def test(data: (String, String)*)(expectedErrorKey: String) =
          testFormError(data: _*)(expectedErrorKey)("improvementCosts.title")(performAction)

        "no option has been selected" in {
          test()("improvementCosts.error.required")
        }

        "the option selected is not valid" in {
          test("improvementCosts" -> "2")("improvementCosts.error.invalid")
        }

        "the amount of money is invalid" in {
          amountOfMoneyErrorScenarios("improvementCostsValue").foreach { scenario =>
            withClue(s"For $scenario: ") {
              val data =
                ("improvementCosts" -> "0") :: scenario.formData
              test(data: _*)(scenario.expectedErrorMessageKey)
            }
          }
        }

        "the amount of money is zero" in {
          test("improvementCosts" -> "0", "improvementCostsValue" -> "0")(
            "improvementCostsValue.error.tooSmall"
          )
        }

      }

      "show an error page" when {
        val price = 1.23d
        val answers = IncompleteAcquisitionDetailsAnswers.empty.copy(
          acquisitionDate         = Some(acquisitionDate),
          acquisitionPrice        = Some(sample[AmountInPence]),
          rebasedAcquisitionPrice = Some(sample[AmountInPence])
        )
        val (session, journey) = sessionWithState(answers, AssetType.Residential, true)
        val updatedDraftReturn = journey.draftReturn
          .copy(acquisitionDetailsAnswers = Some(answers.copy(improvementCosts = Some(AmountInPence(123L)))))
        val updatedSession = session.copy(journeyStatus = Some(journey.copy(draftReturn = updatedDraftReturn)))

        "there is an error updating the draft return" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(updatedDraftReturn)(Left(Error("")))
          }

          checkIsTechnicalErrorPage(
            performAction(
              "improvementCosts"      -> "0",
              "improvementCostsValue" -> price.toString
            )
          )
        }

        "there is an error updating the session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(updatedDraftReturn)(Right(()))
            mockStoreSession(updatedSession)(Left(Error("")))
          }

          checkIsTechnicalErrorPage(
            performAction(
              "improvementCosts"      -> "0",
              "improvementCostsValue" -> price.toString
            )
          )
        }

      }

      "redirect to the check you answers page" when {
        val scenarios = List(
          List("improvementCosts" -> "0", "improvementCostsValue" -> "£1,234") -> AmountInPence(123400L),
          List("improvementCosts" -> "1") -> AmountInPence.zero
        )

        "the price submitted is valid and the journey was incomplete" in {
          scenarios.foreach {
            case (formData, expectedAmountInPence) =>
              withClue(s"For form data $formData and expected amount in pence $expectedAmountInPence: ") {
                val answers = IncompleteAcquisitionDetailsAnswers.empty.copy(
                  acquisitionDate         = Some(acquisitionDate),
                  acquisitionPrice        = Some(sample[AmountInPence]),
                  rebasedAcquisitionPrice = Some(sample[AmountInPence])
                )
                val (session, journey) = sessionWithState(answers, AssetType.Residential, true)
                val updatedDraftReturn = journey.draftReturn
                  .copy(acquisitionDetailsAnswers = Some(answers.copy(improvementCosts = Some(expectedAmountInPence))))
                val updatedSession = session.copy(journeyStatus = Some(journey.copy(draftReturn = updatedDraftReturn)))

                inSequence {
                  mockAuthWithNoRetrievals()
                  mockGetSession(session)
                  mockStoreDraftReturn(updatedDraftReturn)(Right(()))
                  mockStoreSession(updatedSession)(Right(()))
                }

                checkIsRedirect(
                  performAction(formData: _*),
                  routes.AcquisitionDetailsController.checkYourAnswers()
                )
              }
          }
        }

        "the price submitted is valid and the journey was complete" in {
          scenarios.foreach {
            case (formData, expectedAmountInPence) =>
              withClue(s"For form data $formData and expected amount in pence $expectedAmountInPence: ") {

                val answers = sample[CompleteAcquisitionDetailsAnswers]
                  .copy(
                    acquisitionDate         = acquisitionDate,
                    rebasedAcquisitionPrice = Some(sample[AmountInPence]),
                    improvementCosts        = AmountInPence(expectedAmountInPence.value + 1L)
                  )
                val (session, journey) = sessionWithState(answers, AssetType.Residential, true)
                val updatedDraftReturn = journey.draftReturn
                  .copy(acquisitionDetailsAnswers = Some(answers.copy(improvementCosts = expectedAmountInPence)))
                val updatedSession = session.copy(journeyStatus = Some(journey.copy(draftReturn = updatedDraftReturn)))

                inSequence {
                  mockAuthWithNoRetrievals()
                  mockGetSession(session)
                  mockStoreDraftReturn(updatedDraftReturn)(Right(()))
                  mockStoreSession(updatedSession)(Right(()))
                }

                checkIsRedirect(
                  performAction(formData: _*),
                  routes.AcquisitionDetailsController.checkYourAnswers()
                )
              }
          }
        }
      }

    }

    "handling requests to display the acquisition fees page" must {

      def performAction(): Future[Result] = controller.acquisitionFees()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      "redirect to th improvement costs page" when {

        "that question hasn't been answered" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[IncompleteAcquisitionDetailsAnswers].copy(
                  improvementCosts = None
                ),
                sample[AssetType],
                sample[Boolean]
              )._1
            )
          }

          checkIsRedirect(
            performAction(),
            routes.AcquisitionDetailsController.improvementCosts()
          )
        }

      }

      "display the page" when {

        "the acquisition details section has not yet been completed" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[IncompleteAcquisitionDetailsAnswers].copy(
                  improvementCosts = Some(sample[AmountInPence])
                ),
                sample[AssetType],
                sample[Boolean]
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("acquisitionFees.title"), { doc =>
              doc.select("#back").attr("href") shouldBe routes.AcquisitionDetailsController.improvementCosts().url
              doc.select("#content > article > form").attr("action") shouldBe routes.AcquisitionDetailsController
                .acquisitionFeesSubmit()
                .url
            }
          )
        }

        "the acquisition details section has been completed" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[CompleteAcquisitionDetailsAnswers],
                sample[AssetType],
                sample[Boolean]
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("acquisitionFees.title"), { doc =>
              doc.select("#back").attr("href") shouldBe routes.AcquisitionDetailsController.checkYourAnswers().url
              doc.select("#content > article > form").attr("action") shouldBe routes.AcquisitionDetailsController
                .acquisitionFeesSubmit()
                .url
            }
          )
        }

        "the amount in the session is zero" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[CompleteAcquisitionDetailsAnswers].copy(acquisitionFees = AmountInPence.zero),
                sample[AssetType],
                sample[Boolean]
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(
              "acquisitionFees.title"
            ), { doc =>
              doc.select("#acquisitionFees-1").attr("checked") shouldBe "checked"
            }
          )
        }

        "the amount in the session is non-zero" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[CompleteAcquisitionDetailsAnswers].copy(acquisitionFees = AmountInPence(3L)),
                sample[AssetType],
                sample[Boolean]
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(
              "acquisitionFees.title"
            ), { doc =>
              doc.select("#acquisitionFees-0").attr("checked")  shouldBe "checked"
              doc.select("#acquisitionFeesValue").attr("value") shouldBe "0.03"
            }
          )
        }
      }

    }

    "handling submitted acquisition fees" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.acquisitionFeesSubmit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      behave like redirectToStartBehaviour(() => performAction())

      "redirect to the improvement costs page" when {

        "the question has not been answered" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[IncompleteAcquisitionDetailsAnswers].copy(
                  improvementCosts = None
                ),
                sample[AssetType],
                sample[Boolean]
              )._1
            )
          }

          checkIsRedirect(
            performAction(),
            routes.AcquisitionDetailsController.improvementCosts()
          )

        }

      }

      "show a form error" when {

        def test(data: (String, String)*)(expectedErrorKey: String) =
          testFormError(data: _*)(expectedErrorKey)("acquisitionFees.title")(performAction)

        "no option has been selected" in {
          test()("acquisitionFees.error.required")
        }

        "the option selected is not valid" in {
          test("acquisitionFees" -> "2")("acquisitionFees.error.invalid")
        }

        "the amount of money is invalid" in {
          amountOfMoneyErrorScenarios("acquisitionFeesValue").foreach { scenario =>
            withClue(s"For $scenario: ") {
              val data =
                ("acquisitionFees" -> "0") :: scenario.formData
              test(data: _*)(scenario.expectedErrorMessageKey)
            }
          }
        }

        "the amount of money is zero" in {
          test("acquisitionFees" -> "0", "acquisitionFeesValue" -> "0")(
            "acquisitionFeesValue.error.tooSmall"
          )
        }

      }

      "show an error page" when {
        val price = 1.23d
        val answers = IncompleteAcquisitionDetailsAnswers.empty.copy(
          improvementCosts = Some(sample[AmountInPence])
        )
        val (session, journey) = sessionWithState(answers, sample[AssetType], sample[Boolean])
        val updatedDraftReturn = journey.draftReturn
          .copy(acquisitionDetailsAnswers = Some(answers.copy(acquisitionFees = Some(AmountInPence(123L)))))
        val updatedSession = session.copy(journeyStatus = Some(journey.copy(draftReturn = updatedDraftReturn)))

        "there is an error updating the draft return" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(updatedDraftReturn)(Left(Error("")))
          }

          checkIsTechnicalErrorPage(
            performAction(
              "acquisitionFees"      -> "0",
              "acquisitionFeesValue" -> price.toString
            )
          )
        }

        "there is an error updating the session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(updatedDraftReturn)(Right(()))
            mockStoreSession(updatedSession)(Left(Error("")))
          }

          checkIsTechnicalErrorPage(
            performAction(
              "acquisitionFees"      -> "0",
              "acquisitionFeesValue" -> price.toString
            )
          )
        }

      }

      "redirect to the check you answers page" when {

        val scenarios = List(
          List("acquisitionFees" -> "0", "acquisitionFeesValue" -> "£1,234") -> AmountInPence(123400L),
          List("acquisitionFees" -> "1") -> AmountInPence.zero
        )

        "the price submitted is valid and the journey was incomplete" in {
          scenarios.foreach {
            case (formData, expectedAmountInPence) =>
              withClue(s"For form data $formData and expected amount in pence $expectedAmountInPence: ") {
                val answers =
                  IncompleteAcquisitionDetailsAnswers.empty.copy(improvementCosts = Some(sample[AmountInPence]))
                val (session, journey) = sessionWithState(answers, sample[AssetType], sample[Boolean])
                val updatedDraftReturn = journey.draftReturn
                  .copy(acquisitionDetailsAnswers = Some(answers.copy(acquisitionFees = Some(expectedAmountInPence))))
                val updatedSession = session.copy(journeyStatus = Some(journey.copy(draftReturn = updatedDraftReturn)))

                inSequence {
                  mockAuthWithNoRetrievals()
                  mockGetSession(session)
                  mockStoreDraftReturn(updatedDraftReturn)(Right(()))
                  mockStoreSession(updatedSession)(Right(()))
                }

                checkIsRedirect(
                  performAction(formData: _*),
                  routes.AcquisitionDetailsController.checkYourAnswers()
                )
              }
          }
        }

        "the price submitted is valid and the journey was complete" in {
          scenarios.foreach {
            case (formData, expectedAmountInPence) =>
              withClue(s"For form data $formData and expected amount in pence $expectedAmountInPence: ") {
                val answers =
                  sample[CompleteAcquisitionDetailsAnswers]
                    .copy(acquisitionFees = AmountInPence(expectedAmountInPence.value + 1L))
                val (session, journey) = sessionWithState(answers, sample[AssetType], sample[Boolean])
                val updatedDraftReturn = journey.draftReturn
                  .copy(acquisitionDetailsAnswers = Some(answers.copy(acquisitionFees = expectedAmountInPence)))
                val updatedSession = session.copy(journeyStatus = Some(journey.copy(draftReturn = updatedDraftReturn)))

                inSequence {
                  mockAuthWithNoRetrievals()
                  mockGetSession(session)
                  mockStoreDraftReturn(updatedDraftReturn)(Right(()))
                  mockStoreSession(updatedSession)(Right(()))
                }

                checkIsRedirect(
                  performAction(formData: _*),
                  routes.AcquisitionDetailsController.checkYourAnswers()
                )
              }
          }

        }
      }

    }

    "handling requests to display the check your answers page" must {

      def performAction(): Future[Result] = controller.checkYourAnswers()(FakeRequest())

      val completeAnswers = CompleteAcquisitionDetailsAnswers(
        sample[AcquisitionMethod],
        sample[AcquisitionDate],
        sample[AmountInPence],
        Some(sample[AmountInPence]),
        sample[AmountInPence],
        sample[AmountInPence]
      )

      val allQuestionsAnswered = IncompleteAcquisitionDetailsAnswers(
        Some(completeAnswers.acquisitionMethod),
        Some(completeAnswers.acquisitionDate),
        Some(completeAnswers.acquisitionPrice),
        completeAnswers.rebasedAcquisitionPrice,
        Some(completeAnswers.improvementCosts),
        Some(completeAnswers.acquisitionFees)
      )

      behave like redirectToStartBehaviour(performAction)

      behave like missingAssetTypeAndResidentialStatusBehaviour(performAction)

      def testRedirectOnMissingData(
        session: SessionData,
        expectedRedirect: Call
      ): Unit = {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkIsRedirect(performAction(), expectedRedirect)
      }

      "redirect to the acquisition method page" when {

        "the question has not been answered" in {
          testRedirectOnMissingData(
            sessionWithState(
              allQuestionsAnswered.copy(acquisitionMethod = None),
              sample[AssetType],
              sample[Boolean]
            )._1,
            routes.AcquisitionDetailsController.acquisitionMethod()
          )
        }

      }

      "redirect to the acquisition date page" when {

        "the question has not been answered" in {
          testRedirectOnMissingData(
            sessionWithState(
              allQuestionsAnswered.copy(acquisitionDate = None),
              sample[AssetType],
              sample[Boolean]
            )._1,
            routes.AcquisitionDetailsController.acquisitionDate()
          )
        }

      }

      "redirect to the acquisition price page" when {

        "the question has not been answered" in {
          testRedirectOnMissingData(
            sessionWithState(
              allQuestionsAnswered.copy(acquisitionPrice = None),
              sample[AssetType],
              sample[Boolean]
            )._1,
            routes.AcquisitionDetailsController.acquisitionPrice()
          )
        }

      }

      "redirect to the rebased acquisition price page" when {

        "the question has not been answered and the user meets the rebasing crieria" when {

          "the user was a uk resident and is disposing a residential property" in {
            testRedirectOnMissingData(
              sessionWithState(
                allQuestionsAnswered.copy(
                  acquisitionDate         = Some(AcquisitionDate(RebasingCutoffDates.ukResidents.minusDays(1L))),
                  rebasedAcquisitionPrice = None
                ),
                AssetType.Residential,
                true
              )._1,
              routes.AcquisitionDetailsController.rebasedAcquisitionPrice()
            )
          }

          "the user was a non-uk resident and is disposing a residential property" in {
            testRedirectOnMissingData(
              sessionWithState(
                allQuestionsAnswered.copy(
                  acquisitionDate =
                    Some(AcquisitionDate(RebasingCutoffDates.nonUkResidentsResidentialProperty.minusDays(1L))),
                  rebasedAcquisitionPrice = None
                ),
                AssetType.Residential,
                false
              )._1,
              routes.AcquisitionDetailsController.rebasedAcquisitionPrice()
            )
          }

          "the user was a non-uk resident and is disposing a non-residential property" in {
            testRedirectOnMissingData(
              sessionWithState(
                allQuestionsAnswered.copy(
                  acquisitionDate =
                    Some(AcquisitionDate(RebasingCutoffDates.nonUkResidentsNonResidentialProperty.minusDays(1L))),
                  rebasedAcquisitionPrice = None
                ),
                AssetType.NonResidential,
                false
              )._1,
              routes.AcquisitionDetailsController.rebasedAcquisitionPrice()
            )
          }

        }

      }

      "redirect to the improvement costs page" when {

        "the question has not been answered" in {
          testRedirectOnMissingData(
            sessionWithState(
              allQuestionsAnswered.copy(improvementCosts = None),
              sample[AssetType],
              sample[Boolean]
            )._1,
            routes.AcquisitionDetailsController.improvementCosts()
          )
        }

      }

      "redirect to the acquisition fees page" when {

        "the question has not been answered" in {
          testRedirectOnMissingData(
            sessionWithState(
              allQuestionsAnswered.copy(acquisitionFees = None),
              sample[AssetType],
              sample[Boolean]
            )._1,
            routes.AcquisitionDetailsController.acquisitionFees()
          )
        }

      }

      "show an error page when the user has just answered all of the questions and" when {
        val (session, journey) = sessionWithState(allQuestionsAnswered, sample[AssetType], sample[Boolean])
        val newDraftReturn     = journey.draftReturn.copy(acquisitionDetailsAnswers = Some(completeAnswers))
        val updatedJourney     = journey.copy(draftReturn = newDraftReturn)
        "there is an error updating the draft return" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(newDraftReturn)(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction())

        }

        "there is an error updating the session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(newDraftReturn)(Right(()))
            mockStoreSession(session.copy(journeyStatus = Some(updatedJourney)))(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction())

        }
      }

      "show the page" when {

        "the user has just answered all the questions and all updates are successful" in {
          val (session, journey) = sessionWithState(allQuestionsAnswered, sample[AssetType], sample[Boolean])
          val newDraftReturn     = journey.draftReturn.copy(acquisitionDetailsAnswers = Some(completeAnswers))
          val updatedJourney     = journey.copy(draftReturn = newDraftReturn)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(newDraftReturn)(Right(()))
            mockStoreSession(session.copy(journeyStatus = Some(updatedJourney)))(Right(()))
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("acquisitionDetails.cya.title"), { doc =>
              doc.select("#content > article > form").attr("action") shouldBe routes.AcquisitionDetailsController
                .checkYourAnswersSubmit()
                .url
            }
          )
        }

        "the user has already answered all the questions" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionWithState(completeAnswers, sample[AssetType], sample[Boolean])._1)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("acquisitionDetails.cya.title"), { doc =>
              doc.select("#content > article > form").attr("action") shouldBe routes.AcquisitionDetailsController
                .checkYourAnswersSubmit()
                .url
            }
          )
        }

      }

    }

    "handling submits on the check you answers page" must {

      def performAction(): Future[Result] = controller.checkYourAnswersSubmit()(FakeRequest())

      "redirect to the task list" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            sessionWithState(
              sample[CompleteAcquisitionDetailsAnswers],
              sample[AssetType],
              sample[Boolean]
            )._1
          )
        }

        checkIsRedirect(performAction(), controllers.returns.routes.TaskListController.taskList())
      }

    }

  }

  def missingAssetTypeAndResidentialStatusBehaviour(performAction: () => Future[Result]) =
    "redirect to the task list page" when {

      "there is no asset type" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            sessionWithState(
              Some(sample[CompleteAcquisitionDetailsAnswers]),
              None,
              Some(sample[Boolean]),
              Some(sample[DisposalDate])
            )._1
          )
        }

        checkIsRedirect(performAction(), controllers.returns.routes.TaskListController.taskList())
      }

      "there is no residential status" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            sessionWithState(
              Some(sample[CompleteAcquisitionDetailsAnswers]),
              Some(sample[AssetType]),
              None,
              Some(sample[DisposalDate])
            )._1
          )
        }

        checkIsRedirect(performAction(), controllers.returns.routes.TaskListController.taskList())
      }

    }

  def testFormError(
    data: (String, String)*
  )(expectedErrorMessageKey: String, errorArgs: String*)(pageTitleKey: String, titleArgs: String*)(
    performAction: Seq[(String, String)] => Future[Result],
    currentSession: SessionData = sessionWithState(
      sample[CompleteAcquisitionDetailsAnswers].copy(rebasedAcquisitionPrice = Some(sample[AmountInPence])),
      sample[AssetType],
      sample[Boolean]
    )._1
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

}
