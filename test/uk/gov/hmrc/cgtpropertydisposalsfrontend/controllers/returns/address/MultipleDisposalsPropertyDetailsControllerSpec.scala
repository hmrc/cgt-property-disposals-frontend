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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.address

import java.time.LocalDate

import org.jsoup.nodes.Document
import org.scalatest.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.i18n.{Lang, MessagesApi}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.{Call, Result}
import play.api.test.CSRFTokenHelper._
import play.api.test.FakeRequest
import play.api.test.Helpers.BAD_REQUEST
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.AmountOfMoneyErrorScenarios.amountOfMoneyErrorScenarios
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AddressControllerSpec, DateErrorScenarios}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.ReturnsServiceSupport
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.address.{routes => returnsAddressRoutes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.triage.routes
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.FillingOutReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address.{NonUkAddress, UkAddress}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.AmountInPence
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.GGCredId
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.SubscribedDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{DisposalDate, DraftReturn, MultipleDisposalsDraftReturn, SingleDisposalDraftReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.MultipleDisposalsExamplePropertyDetailsAnswers.{CompleteMultipleDisposalsExamplePropertyDetailsAnswers, IncompleteMultipleDisposalsExamplePropertyDetailsAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, JourneyStatus, LocalDateUtils, SessionData, TaxYear}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService

import scala.concurrent.Future

class MultipleDisposalsPropertyDetailsControllerSpec
    extends AddressControllerSpec[FillingOutReturn]
    with ScalaCheckDrivenPropertyChecks
    with RedirectToStartBehaviour
    with ReturnsServiceSupport {

  val incompleteAnswers =
    IncompleteMultipleDisposalsExamplePropertyDetailsAnswers.empty.copy(address = Some(ukAddress(1)))

  val draftReturn: MultipleDisposalsDraftReturn =
    sample[MultipleDisposalsDraftReturn].copy(examplePropertyDetailsAnswers = Some(incompleteAnswers))

  val validJourneyStatus = FillingOutReturn(sample[SubscribedDetails], sample[GGCredId], None, draftReturn)

  override def overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](bind[ReturnsService].toInstance(mockReturnsService)) ::: super.overrideBindings

  lazy val controller = instanceOf[PropertyDetailsController]

  lazy implicit val messagesApi: MessagesApi = controller.messagesApi

  override def updateAddress(journey: FillingOutReturn, address: Address): FillingOutReturn = address match {
    case a: UkAddress =>
      journey.copy(draftReturn =
        draftReturn.copy(examplePropertyDetailsAnswers = Some(incompleteAnswers.copy(address = Some(a))))
      )
    case _: NonUkAddress => journey
  }

  override val mockUpdateAddress: Option[(FillingOutReturn, Address, Either[Error, Unit]) => Unit] =
    Some {
      case (newDetails: FillingOutReturn, a: UkAddress, r: Either[Error, Unit]) =>
        mockStoreDraftReturn(
          draftReturn.copy(examplePropertyDetailsAnswers = Some(incompleteAnswers.copy(address = Some(a)))),
          newDetails.subscribedDetails.cgtReference,
          newDetails.agentReferenceNumber
        )(r)

      case (_, _: NonUkAddress, _) =>
        sys.error("Non UK addresses not handled in this spec")
    }

  def redirectToStartBehaviour(performAction: () => Future[Result]): Unit =
    redirectToStartWhenInvalidJourney(
      performAction, {
        case _: FillingOutReturn => true
        case _                   => false
      }
    )

  "AddressController" when {

    "handling requests to display the guidance page" must {

      def performAction(): Future[Result] =
        controller.multipleDisposalsGuidance()(FakeRequest())

      "redirect to the check your answers page" when {

        "the user is on a single disposal journey" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  sample[FillingOutReturn].copy(
                    draftReturn = sample[SingleDisposalDraftReturn]
                  )
                )
              )
            )
          }

          checkIsRedirect(performAction(), routes.PropertyDetailsController.checkYourAnswers())
        }

      }

      "display the page" when {

        def test(
          draftReturn: MultipleDisposalsDraftReturn,
          expectedBackLink: Call
        ): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  sample[FillingOutReturn].copy(
                    draftReturn = draftReturn
                  )
                )
              )
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("property-details.multiple-disposals.guidance.title"), { doc =>
              doc.select("#back").attr("href") shouldBe expectedBackLink.url
              doc
                .select("#content > article > form")
                .attr("action") shouldBe routes.PropertyDetailsController
                .multipleDisposalsGuidanceSubmit()
                .url
            }
          )
        }

        "the user has not started this section before" in {
          test(
            sample[MultipleDisposalsDraftReturn].copy(
              examplePropertyDetailsAnswers = None
            ),
            controllers.returns.routes.TaskListController.taskList()
          )
        }

        "the user has started but not completed this section" in {
          test(
            sample[MultipleDisposalsDraftReturn].copy(
              examplePropertyDetailsAnswers = Some(sample[IncompleteMultipleDisposalsExamplePropertyDetailsAnswers])
            ),
            controllers.returns.routes.TaskListController.taskList()
          )
        }

        "the user has completed this section" in {
          test(
            sample[MultipleDisposalsDraftReturn].copy(
              examplePropertyDetailsAnswers = Some(sample[CompleteMultipleDisposalsExamplePropertyDetailsAnswers])
            ),
            routes.PropertyDetailsController.checkYourAnswers()
          )
        }

      }

    }

    "handling submits on the guidance page" must {

      def performAction(): Future[Result] =
        controller.multipleDisposalsGuidanceSubmit()(FakeRequest())

      def test(
        draftReturn: DraftReturn,
        expectedRedirect: Call
      ): Unit = {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            SessionData.empty.copy(
              journeyStatus = Some(
                sample[FillingOutReturn].copy(
                  draftReturn = draftReturn
                )
              )
            )
          )
        }

        checkIsRedirect(performAction(), expectedRedirect)
      }

      "redirect to the check your answers page" when {

        "the user is on a single disposal journey" in {
          test(
            sample[SingleDisposalDraftReturn],
            routes.PropertyDetailsController.checkYourAnswers()
          )
        }

        "the user ins on a multiple disposals journey and has completed this section" in {
          test(
            sample[MultipleDisposalsDraftReturn].copy(
              examplePropertyDetailsAnswers = Some(sample[CompleteMultipleDisposalsExamplePropertyDetailsAnswers])
            ),
            routes.PropertyDetailsController.checkYourAnswers()
          )
        }

      }

      "redirect to the enter postcode page" when {

        "the user has not started this section before" in {
          test(
            sample[MultipleDisposalsDraftReturn].copy(
              examplePropertyDetailsAnswers = None
            ),
            routes.PropertyDetailsController.enterPostcode()
          )
        }

        "the user has started but not completed this section" in {
          test(
            sample[MultipleDisposalsDraftReturn].copy(
              examplePropertyDetailsAnswers = Some(sample[IncompleteMultipleDisposalsExamplePropertyDetailsAnswers])
            ),
            routes.PropertyDetailsController.enterPostcode()
          )
        }

      }
    }

    "handling requests to display the enter UK address page" must {

      def performAction(): Future[Result] = controller.enterUkAddress()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      behave like displayEnterUkAddressPage(performAction)

    }

    "handling submitted addresses from enter UK address page" must {

      def performAction(formData: Seq[(String, String)]): Future[Result] =
        controller.enterUkAddressSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*).withCSRFToken)

      behave like redirectToStartBehaviour(() => performAction(Seq.empty))

      behave like submitEnterUkAddress(
        performAction,
        returnsAddressRoutes.PropertyDetailsController.checkYourAnswers()
      )

    }

    "handling requests to display the enter postcode page" must {

      def performAction(): Future[Result] = controller.enterPostcode()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      behave like enterPostcodePage(performAction)

    }

    "handling submitted postcodes and filters" must {

      def performAction(formData: Seq[(String, String)]): Future[Result] =
        controller.enterPostcodeSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*).withCSRFToken)

      behave like redirectToStartBehaviour(() => performAction(Seq.empty))

      behave like submitEnterPostcode(performAction, returnsAddressRoutes.PropertyDetailsController.selectAddress())

    }

    "handling requests to display the select address page" must {

      def performAction(): Future[Result] =
        controller.selectAddress()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      behave like displaySelectAddress(
        performAction,
        controllers.returns.address.routes.PropertyDetailsController.enterPostcode()
      )

    }

    "handling submitted selected addresses" must {

      def performAction(formData: Seq[(String, String)]): Future[Result] =
        controller.selectAddressSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*).withCSRFToken)

      behave like redirectToStartBehaviour(() => performAction(Seq.empty))

      behave like submitSelectAddress(
        performAction,
        controllers.returns.address.routes.PropertyDetailsController.enterPostcode(),
        returnsAddressRoutes.PropertyDetailsController.checkYourAnswers()
      )

      "not update the session" when {

        "the user selects an address which is already in their session" in {
          val session = sessionWithValidJourneyStatus.copy(addressLookupResult = Some(addressLookupResult))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          val result = performAction(Seq("address-select" -> "0"))
          checkIsRedirect(result, returnsAddressRoutes.PropertyDetailsController.checkYourAnswers())
        }

      }

    }

    "handling requests to display the disposal date page" must {

      def performAction(): Future[Result] =
        controller.disposalDate()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      "redirect to the check your answers page" when {

        "the user is on a single disposal journey" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  sample[FillingOutReturn].copy(
                    draftReturn = sample[SingleDisposalDraftReturn]
                  )
                )
              )
            )
          }

          checkIsRedirect(performAction(), routes.PropertyDetailsController.checkYourAnswers())
        }

      }

      "display the page" when {

        def test(draftReturn: MultipleDisposalsDraftReturn, expectedBackLink: Call): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  sample[FillingOutReturn].copy(
                    draftReturn = draftReturn
                  )
                )
              )
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("multipleDisposalsDisposalDate.title"), { doc =>
              doc.select("#back").attr("href") shouldBe expectedBackLink.url
              doc
                .select("#content > article > form")
                .attr("action") shouldBe routes.PropertyDetailsController
                .disposalDateSubmit()
                .url
            }
          )
        }

        "the user has not started this section before" in {
          test(
            sample[MultipleDisposalsDraftReturn].copy(
              examplePropertyDetailsAnswers = None
            ),
            routes.PropertyDetailsController.enterUkAddress()
          )
        }

        "the user has started but not completed this section" in {
          test(
            sample[MultipleDisposalsDraftReturn].copy(
              examplePropertyDetailsAnswers = Some(
                sample[IncompleteMultipleDisposalsExamplePropertyDetailsAnswers].copy(
                  disposalDate = None
                )
              )
            ),
            routes.PropertyDetailsController.enterUkAddress()
          )
        }

        "the user has completed this section" in {
          test(
            sample[MultipleDisposalsDraftReturn].copy(
              examplePropertyDetailsAnswers = Some(
                sample[CompleteMultipleDisposalsExamplePropertyDetailsAnswers].copy(
                  address      = sample[UkAddress],
                  disposalDate = sample[DisposalDate].copy(value = LocalDateUtils.today())
                )
              )
            ),
            routes.PropertyDetailsController.checkYourAnswers()
          )
        }

      }

    }

    "handling submitted answers to the disposal date page" must {

      val key = "multipleDisposalsDisposalDate"

      def performAction(formData: (String, String)*): Future[Result] =
        controller.disposalDateSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*).withCSRFToken)

      def formData(d: LocalDate): List[(String, String)] =
        List(
          s"$key-day"   -> d.getDayOfMonth.toString,
          s"$key-month" -> d.getMonthValue.toString,
          s"$key-year"  -> d.getYear.toString
        )

      behave like redirectToStartBehaviour(() => performAction())

      "not update the session" when {
        "the date submitted is the same as one that already exists in session" in {

          val disposalDate = sample[DisposalDate].copy(value = LocalDateUtils.today())

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  sample[FillingOutReturn].copy(
                    draftReturn = sample[MultipleDisposalsDraftReturn].copy(
                      examplePropertyDetailsAnswers = Some(
                        sample[IncompleteMultipleDisposalsExamplePropertyDetailsAnswers].copy(
                          disposalDate = Some(disposalDate)
                        )
                      )
                    )
                  )
                )
              )
            )
          }

          checkIsRedirect(
            performAction(formData(disposalDate.value): _*),
            routes.PropertyDetailsController.checkYourAnswers()
          )
        }

      }

      "show a form error" when {

        def testFormError(formData: List[(String, String)])(expectedErrorMessageKey: String) = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithValidJourneyStatus.copy(
                journeyStatus = Some(sample[JourneyStatus])
              )
            )
          }

          checkPageIsDisplayed(
            performAction(formData: _*),
            messageFromMessageKey(s"$key.title"), { doc =>
              doc.select("#error-summary-display > ul > li > a").text() shouldBe messageFromMessageKey(
                expectedErrorMessageKey
              )
            },
            BAD_REQUEST
          )
        }

        "the date entered is invalid" in {
          DateErrorScenarios
            .dateErrorScenarios(key)
            .foreach { scenario =>
              withClue(s"For date error scenario $scenario: ") {
                val data = List(
                  s"$key-day"   -> scenario.dayInput,
                  s"$key-month" -> scenario.monthInput,
                  s"$key-year"  -> scenario.yearInput
                ).collect { case (key, Some(value)) => key -> value }

                testFormError(data)(scenario.expectedErrorMessageKey)
              }
            }
        }

        "the date entered is too far in future" in {
          testFormError(formData(LocalDateUtils.today().plusDays(365L)))(
            s"$key.error.tooFarInFuture"
          )
        }

        "the date entered is too far in past" in {
          testFormError(formData(LocalDateUtils.today().minusDays(365L)))(
            s"$key.error.tooFarInPast"
          )
        }

      }

      "redirect to the check your answers page" when {

        "the user has not started a draft return and" when {

          "the user has not answered the question before" in {}

          "the user has already answered the question" in {}

        }

        "the user has started a draft return and" when {

          "have completed the section and they enter a figure which is " +
            "different than one they have already entered" in {}
        }
      }

    }

    "handling requests to display the disposal price page" must {

      def performAction(): Future[Result] =
        controller.disposalPrice()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      "redirect to the check your answers page" when {

        "the user is on a single disposal journey" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  sample[FillingOutReturn].copy(
                    draftReturn = sample[SingleDisposalDraftReturn]
                  )
                )
              )
            )
          }

          checkIsRedirect(performAction(), routes.PropertyDetailsController.checkYourAnswers())
        }

      }

      "display the page" when {

        def test(draftReturn: MultipleDisposalsDraftReturn, expectedBackLink: Call): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  sample[FillingOutReturn].copy(
                    draftReturn = draftReturn
                  )
                )
              )
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("multipleDisposalsDisposalPrice.title"), { doc =>
              doc.select("#back").attr("href") shouldBe expectedBackLink.url
              doc
                .select("#content > article > form")
                .attr("action") shouldBe routes.PropertyDetailsController
                .disposalPriceSubmit()
                .url
            }
          )
        }

        "the user has not started this section before" in {
          test(
            sample[MultipleDisposalsDraftReturn].copy(
              examplePropertyDetailsAnswers = None
            ),
            routes.PropertyDetailsController.disposalDate()
          )
        }

        "the user has started but not completed this section" in {
          test(
            sample[MultipleDisposalsDraftReturn].copy(
              examplePropertyDetailsAnswers = Some(
                sample[IncompleteMultipleDisposalsExamplePropertyDetailsAnswers].copy(
                  disposalPrice = None
                )
              )
            ),
            routes.PropertyDetailsController.disposalDate()
          )
        }

        "the user has completed this section" in {
          test(
            sample[MultipleDisposalsDraftReturn].copy(
              examplePropertyDetailsAnswers = Some(
                sample[CompleteMultipleDisposalsExamplePropertyDetailsAnswers].copy(
                  disposalPrice = sample[AmountInPence]
                )
              )
            ),
            routes.PropertyDetailsController.checkYourAnswers()
          )
        }

      }

    }

    "handling submitted answers to the disposal price page" must {

      val key = "multipleDisposalsDisposalPrice"

      def performAction(formData: (String, String)*): Future[Result] =
        controller.disposalPriceSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*))

      behave like redirectToStartBehaviour(() => performAction())

      "not update the session" when {

        "the data submitted is the same as one that already exists in session" in {

          val disposalPrice = AmountInPence.fromPounds(1000)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  sample[FillingOutReturn].copy(
                    draftReturn = sample[MultipleDisposalsDraftReturn].copy(
                      examplePropertyDetailsAnswers = Some(
                        sample[IncompleteMultipleDisposalsExamplePropertyDetailsAnswers].copy(
                          disposalPrice = Some(disposalPrice)
                        )
                      )
                    )
                  )
                )
              )
            )
          }

          checkIsRedirect(
            performAction(key -> "1000"),
            routes.PropertyDetailsController.checkYourAnswers()
          )
        }

      }

      "show a form error for amount" when {

        def test(data: (String, String)*)(expectedErrorMessageKey: String) = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  sample[FillingOutReturn].copy(
                    draftReturn = sample[MultipleDisposalsDraftReturn].copy(
                      examplePropertyDetailsAnswers = Some(
                        sample[CompleteMultipleDisposalsExamplePropertyDetailsAnswers]
                      )
                    )
                  )
                )
              )
            )
          }

          checkPageIsDisplayed(
            performAction(data: _*),
            messageFromMessageKey(s"$key.title"), { doc =>
              doc.select("#error-summary-display > ul > li > a").text() shouldBe messageFromMessageKey(
                expectedErrorMessageKey
              )
            },
            BAD_REQUEST
          )
        }

        "the data is invalid" in {
          amountOfMoneyErrorScenarios(key).foreach { scenario =>
            withClue(s"For $scenario: ") {
              val data = scenario.formData
              test(data: _*)(scenario.expectedErrorMessageKey)
            }
          }
        }

      }

      "redirect to the cya page" when {

        def test(
          draftReturn: DraftReturn,
          expectedRedirect: Call
        ): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  sample[FillingOutReturn].copy(
                    draftReturn = draftReturn
                  )
                )
              )
            )
          }

          checkIsRedirect(performAction(), expectedRedirect)
        }

        "the user is on a single disposal journey" in {
          test(
            sample[SingleDisposalDraftReturn],
            routes.PropertyDetailsController.checkYourAnswers()
          )
        }

        "the user is on a multiple disposals journey and has completed this section" in {
          test(
            sample[MultipleDisposalsDraftReturn].copy(
              examplePropertyDetailsAnswers = Some(sample[CompleteMultipleDisposalsExamplePropertyDetailsAnswers])
            ),
            routes.PropertyDetailsController.checkYourAnswers()
          )
        }

        "the user hasn't ever answered the acquisition price question " +
          "and the draft return and session data has been successfully updated" in {
          val taxYear = sample[TaxYear].copy(
            startDateInclusive = LocalDate.of(2019, 4, 6),
            endDateExclusive   = LocalDate.of(2020, 4, 6)
          )
          val disposalDate = DisposalDate(value = LocalDate.of(2020, 3, 20), taxYear = taxYear)
          test(
            sample[MultipleDisposalsDraftReturn].copy(
              examplePropertyDetailsAnswers = Some(
                sample[IncompleteMultipleDisposalsExamplePropertyDetailsAnswers].copy(
                  disposalDate  = Some(disposalDate),
                  disposalPrice = Some(AmountInPence.fromPounds(10))
                )
              )
            ),
            routes.PropertyDetailsController.checkYourAnswers()
          )
        }

      }

    }

    "handling requests to display the acquisition price page" must {

      def performAction(): Future[Result] =
        controller.acquisitionPrice()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      "redirect to the check your answers page" when {

        "the user is on a single disposal journey" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  sample[FillingOutReturn].copy(
                    draftReturn = sample[SingleDisposalDraftReturn]
                  )
                )
              )
            )
          }

          checkIsRedirect(performAction(), routes.PropertyDetailsController.checkYourAnswers())
        }

      }

      "display the page" when {

        def test(draftReturn: MultipleDisposalsDraftReturn, expectedBackLink: Call): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  sample[FillingOutReturn].copy(
                    draftReturn = draftReturn
                  )
                )
              )
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("multipleDisposalsAcquisitionDate.title"), { doc =>
              doc.select("#back").attr("href") shouldBe expectedBackLink.url
              doc
                .select("#content > article > form")
                .attr("action") shouldBe routes.PropertyDetailsController
                .acquisitionPriceSubmit()
                .url
            }
          )
        }

        "the user has not started this section before" in {
          test(
            sample[MultipleDisposalsDraftReturn].copy(
              examplePropertyDetailsAnswers = None
            ),
            routes.PropertyDetailsController.disposalPrice()
          )
        }

        "the user has started but not completed this section" in {
          test(
            sample[MultipleDisposalsDraftReturn].copy(
              examplePropertyDetailsAnswers = Some(
                sample[IncompleteMultipleDisposalsExamplePropertyDetailsAnswers].copy(
                  acquisitionPrice = None
                )
              )
            ),
            routes.PropertyDetailsController.disposalPrice()
          )
        }

        "the user has completed this section" in {
          test(
            sample[MultipleDisposalsDraftReturn].copy(
              examplePropertyDetailsAnswers = Some(
                sample[CompleteMultipleDisposalsExamplePropertyDetailsAnswers].copy(
                  acquisitionPrice = sample[AmountInPence]
                )
              )
            ),
            routes.PropertyDetailsController.checkYourAnswers()
          )
        }

      }

    }

    "handling submitted answers to the acquisition price page" must {

      val key = "multipleDisposalsAcquisitionPrice"

      def performAction(formData: (String, String)*): Future[Result] =
        controller.acquisitionPriceSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*))

      behave like redirectToStartBehaviour(() => performAction())

      "not update the session" when {
        "the data submitted is the same as one that already exists in session" in {

          val acquisitionPrice = AmountInPence.fromPounds(1000)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  sample[FillingOutReturn].copy(
                    draftReturn = sample[MultipleDisposalsDraftReturn].copy(
                      examplePropertyDetailsAnswers = Some(
                        sample[IncompleteMultipleDisposalsExamplePropertyDetailsAnswers].copy(
                          acquisitionPrice = Some(acquisitionPrice)
                        )
                      )
                    )
                  )
                )
              )
            )
          }

          checkIsRedirect(
            performAction(key -> "1000"),
            routes.PropertyDetailsController.checkYourAnswers()
          )
        }

      }

      "show a form error for amount" when {

        def test(data: (String, String)*)(expectedErrorMessageKey: String) = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  sample[FillingOutReturn].copy(
                    draftReturn = sample[MultipleDisposalsDraftReturn].copy(
                      examplePropertyDetailsAnswers = Some(
                        sample[CompleteMultipleDisposalsExamplePropertyDetailsAnswers]
                      )
                    )
                  )
                )
              )
            )
          }

          checkPageIsDisplayed(
            performAction(data: _*),
            messageFromMessageKey(s"$key.title"), { doc =>
              doc.select("#error-summary-display > ul > li > a").text() shouldBe messageFromMessageKey(
                expectedErrorMessageKey
              )
            },
            BAD_REQUEST
          )
        }

        "the data is invalid" in {
          amountOfMoneyErrorScenarios(key).foreach { scenario =>
            withClue(s"For $scenario: ") {
              val data = scenario.formData
              test(data: _*)(scenario.expectedErrorMessageKey)
            }
          }
        }

      }

      "redirect to the cya page" when {

        def test(
          draftReturn: DraftReturn,
          expectedRedirect: Call
        ): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  sample[FillingOutReturn].copy(
                    draftReturn = draftReturn
                  )
                )
              )
            )
          }

          checkIsRedirect(performAction(), expectedRedirect)
        }

        "the user is on a single disposal journey" in {
          test(
            sample[SingleDisposalDraftReturn],
            routes.PropertyDetailsController.checkYourAnswers()
          )
        }

        "the user is on a multiple disposals journey and has completed this section" in {
          test(
            sample[MultipleDisposalsDraftReturn].copy(
              examplePropertyDetailsAnswers = Some(sample[CompleteMultipleDisposalsExamplePropertyDetailsAnswers])
            ),
            routes.PropertyDetailsController.checkYourAnswers()
          )
        }

        "the user hasn't ever answered the acquisition price question " +
          "and the draft return and session data has been successfully updated" in {
          val taxYear = sample[TaxYear].copy(
            startDateInclusive = LocalDate.of(2019, 4, 6),
            endDateExclusive   = LocalDate.of(2020, 4, 6)
          )
          val disposalDate = DisposalDate(value = LocalDateUtils.today(), taxYear = taxYear)
          test(
            sample[MultipleDisposalsDraftReturn].copy(
              examplePropertyDetailsAnswers = Some(
                sample[IncompleteMultipleDisposalsExamplePropertyDetailsAnswers].copy(
                  disposalDate     = Some(disposalDate),
                  disposalPrice    = Some(AmountInPence.fromPounds(10)),
                  acquisitionPrice = Some(AmountInPence.fromPounds(10))
                )
              )
            ),
            routes.PropertyDetailsController.checkYourAnswers()
          )
        }

      }

    }

    "handling requests to display the check your answers page" must {

      def performAction(): Future[Result] =
        controller.checkYourAnswers()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      val address          = sample[UkAddress]
      val disposalDate     = sample[DisposalDate]
      val disposalPrice    = sample[AmountInPence]
      val acquisitionPrice = sample[AmountInPence]

      val completeAnswers =
        CompleteMultipleDisposalsExamplePropertyDetailsAnswers(address, disposalDate, disposalPrice, acquisitionPrice)

      val allQuestionsAnswered = IncompleteMultipleDisposalsExamplePropertyDetailsAnswers(
        Some(completeAnswers.address),
        Some(completeAnswers.disposalDate),
        Some(completeAnswers.disposalPrice),
        Some(completeAnswers.acquisitionPrice)
      )

      val currentDraftReturn =
        sample[MultipleDisposalsDraftReturn].copy(examplePropertyDetailsAnswers = Some(allQuestionsAnswered))
      val currentJourney     = sample[FillingOutReturn].copy(draftReturn             = currentDraftReturn)
      val updatedDraftReturn = currentDraftReturn.copy(examplePropertyDetailsAnswers = Some(completeAnswers))
      val updatedJourney     = currentJourney.copy(draftReturn                       = updatedDraftReturn)

      "redirect to the guidance page" when {

        "the user has not started the section" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(journeyStatus = Some(
                sample[FillingOutReturn].copy(
                  draftReturn = draftReturn.copy(
                    examplePropertyDetailsAnswers = None
                  )
                )
              )
              )
            )
          }

          checkIsRedirect(
            performAction(),
            routes.PropertyDetailsController.multipleDisposalsGuidance()
          )
        }

        "the user has started the section but there is no property address" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(journeyStatus = Some(
                sample[FillingOutReturn].copy(
                  draftReturn = draftReturn.copy(
                    examplePropertyDetailsAnswers = Some(allQuestionsAnswered.copy(address = None))
                  )
                )
              )
              )
            )
          }

          checkIsRedirect(performAction(), routes.PropertyDetailsController.multipleDisposalsGuidance())
        }
      }

      "redirect to the address page" when {

        "the user has started the section but there is no property address" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(journeyStatus = Some(
                sample[FillingOutReturn].copy(
                  draftReturn = draftReturn.copy(
                    examplePropertyDetailsAnswers = Some(
                      sample[IncompleteMultipleDisposalsExamplePropertyDetailsAnswers].copy(
                        address = None
                      )
                    )
                  )
                )
              )
              )
            )
          }

          checkIsRedirect(
            performAction(),
            routes.PropertyDetailsController.enterUkAddress()
          )
        }

      }

      "redirect to the disposal date page" when {

        "the user has started the section but there is no disposal date" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(journeyStatus = Some(
                sample[FillingOutReturn].copy(
                  draftReturn = draftReturn.copy(
                    examplePropertyDetailsAnswers = Some(
                      sample[IncompleteMultipleDisposalsExamplePropertyDetailsAnswers].copy(
                        disposalDate = None
                      )
                    )
                  )
                )
              )
              )
            )
          }

          checkIsRedirect(
            performAction(),
            routes.PropertyDetailsController.disposalDate()
          )
        }

      }

      "redirect to the disposal price page" when {

        "the user has started the section but there is no disposal price" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(journeyStatus = Some(
                sample[FillingOutReturn].copy(
                  draftReturn = draftReturn.copy(
                    examplePropertyDetailsAnswers = Some(
                      sample[IncompleteMultipleDisposalsExamplePropertyDetailsAnswers].copy(
                        disposalPrice = None
                      )
                    )
                  )
                )
              )
              )
            )
          }

          checkIsRedirect(
            performAction(),
            routes.PropertyDetailsController.disposalPrice()
          )
        }

      }

      "redirect to the acquisition price page" when {

        "the user has started the section but there is no acquisition price" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(journeyStatus = Some(
                sample[FillingOutReturn].copy(
                  draftReturn = draftReturn.copy(
                    examplePropertyDetailsAnswers = Some(
                      sample[IncompleteMultipleDisposalsExamplePropertyDetailsAnswers].copy(
                        acquisitionPrice = None
                      )
                    )
                  )
                )
              )
              )
            )
          }

          checkIsRedirect(
            performAction(),
            routes.PropertyDetailsController.acquisitionPrice()
          )
        }

      }

      "display the page" when {

        def testIsCheckYourAnswers(
          result: Future[Result],
          ukAddressDetails: UkAddress,
          expectedTitleKey: String
        ): Unit =
          checkPageIsDisplayed(
            result,
            messageFromMessageKey(expectedTitleKey), { doc =>
              val guidanceLink = doc.select("#guidanceLink")
              guidanceLink.attr("href") shouldBe routes.PropertyDetailsController.multipleDisposalsGuidance().url
              guidanceLink.text() shouldBe messageFromMessageKey(
                "returns.property-details.multiple-disposals.cya.guidanceLink"
              )

              MultipleDisposalsPropertyDetailsControllerSpec.validatePropertyAddressPage(ukAddressDetails, doc)
            }
          )

        "the user has just finished answering the questions and all updates are successful" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData.empty.copy(journeyStatus = Some(currentJourney)))
            mockStoreDraftReturn(
              updatedDraftReturn,
              currentJourney.subscribedDetails.cgtReference,
              currentJourney.agentReferenceNumber
            )(Right(()))
            mockStoreSession(SessionData.empty.copy(journeyStatus = Some(updatedJourney)))(Right(()))
          }

          testIsCheckYourAnswers(
            performAction(),
            address,
            "returns.property-address.cya.title"
          )
        }

        "the user hsa already completed the section" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData.empty.copy(journeyStatus = Some(updatedJourney)))
          }

          testIsCheckYourAnswers(
            performAction(),
            address,
            "returns.property-address.cya.title"
          )
        }

      }

      "show an error page" when {

        "there is an error updating the draft return" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData.empty.copy(journeyStatus = Some(currentJourney)))
            mockStoreDraftReturn(
              updatedDraftReturn,
              currentJourney.subscribedDetails.cgtReference,
              currentJourney.agentReferenceNumber
            )(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction())
        }

        "there is an error updating the session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData.empty.copy(journeyStatus = Some(currentJourney)))
            mockStoreDraftReturn(
              updatedDraftReturn,
              currentJourney.subscribedDetails.cgtReference,
              currentJourney.agentReferenceNumber
            )(Right(()))
            mockStoreSession(SessionData.empty.copy(journeyStatus = Some(updatedJourney)))(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction())
        }

      }

    }

    "handling check your answers submits" must {

      def performAction(): Future[Result] =
        controller.checkYourAnswersSubmit()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      "redirect to the task list" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionWithValidJourneyStatus)
        }

        checkIsRedirect(performAction(), controllers.returns.routes.TaskListController.taskList())
      }

    }

  }
}

object MultipleDisposalsPropertyDetailsControllerSpec extends Matchers {
  def validatePropertyAddressPage(
    ukAddress: UkAddress,
    doc: Document
  )(implicit messages: MessagesApi, lang: Lang): Unit =
    doc.select("#property-address-answer").text() shouldBe
      List(Some(ukAddress.line1), ukAddress.line2, ukAddress.town, ukAddress.county, Some(ukAddress.postcode.value))
        .collect { case Some(s) => s.trim }
        .mkString(" ")
}
