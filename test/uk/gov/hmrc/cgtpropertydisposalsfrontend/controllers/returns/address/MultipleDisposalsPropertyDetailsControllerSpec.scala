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
import play.api.test.Helpers._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.AmountOfMoneyErrorScenarios.amountOfMoneyErrorScenarios
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AddressControllerSpec, DateErrorScenarios}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.ReturnsServiceSupport
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.address.{routes => returnsAddressRoutes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, LocalDateUtils, SessionData, TaxYear}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.FillingOutReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.{Address, Postcode}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address.{NonUkAddress, UkAddress}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.AmountInPence
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.GGCredId
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.SubscribedDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{DisposalDate, DraftReturn, MultipleDisposalsDraftReturn, SingleDisposalDraftReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ExamplePropertyDetailsAnswers.{CompleteExamplePropertyDetailsAnswers, IncompleteExamplePropertyDetailsAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.MultipleDisposalsTriageAnswers.{CompleteMultipleDisposalsTriageAnswers, IncompleteMultipleDisposalsTriageAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.CompleteSingleDisposalTriageAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{AssetType, DraftReturn, MultipleDisposalsDraftReturn, SingleDisposalDraftReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService

import scala.collection.JavaConverters._
import scala.concurrent.Future

class MultipleDisposalsPropertyDetailsControllerSpec
    extends AddressControllerSpec[FillingOutReturn]
    with ScalaCheckDrivenPropertyChecks
    with RedirectToStartBehaviour
    with ReturnsServiceSupport {

  val incompleteAnswers =
    IncompleteExamplePropertyDetailsAnswers.empty.copy(address = Some(ukAddress(1)))

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

  val taxYear = sample[TaxYear].copy(
    startDateInclusive = LocalDate.of(2019, 4, 6),
    endDateExclusive   = LocalDate.of(2020, 4, 6)
  )

  val disposalDate = DisposalDate(value = LocalDate.of(2020, 3, 20), taxYear = taxYear)

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
              examplePropertyDetailsAnswers = Some(sample[IncompleteExamplePropertyDetailsAnswers])
            ),
            controllers.returns.routes.TaskListController.taskList()
          )
        }

        "the user has completed this section" in {
          test(
            sample[MultipleDisposalsDraftReturn].copy(
              examplePropertyDetailsAnswers = Some(sample[CompleteExamplePropertyDetailsAnswers])
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

      behave like redirectToTaskListWhenNoAssetTypeBehaviour(performAction)

      "redirect to the check your answers page" when {

        "the user is on a single disposal journey" in {
          test(
            sample[SingleDisposalDraftReturn].copy(
              triageAnswers = sample[CompleteSingleDisposalTriageAnswers].copy(
                assetType = AssetType.Residential
              )
            ),
            routes.PropertyDetailsController.checkYourAnswers()
          )
        }

        "the user ins on a multiple disposals journey and has completed this section" in {
          test(
            sample[MultipleDisposalsDraftReturn].copy(
              triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers].copy(
                assetTypes = List(AssetType.NonResidential)
              ),
              examplePropertyDetailsAnswers = Some(sample[CompleteExamplePropertyDetailsAnswers])
            ),
            routes.PropertyDetailsController.checkYourAnswers()
          )
        }

      }

      "redirect to the enter postcode page" when {

        "the user did not dispose of a non-residential property and" when {

          "the user has not started this section before and the user did not dispose of a " +
            "non-residential property" in {
            test(
              sample[MultipleDisposalsDraftReturn].copy(
                triageAnswers =
                  sample[CompleteMultipleDisposalsTriageAnswers].copy(assetTypes = List(AssetType.Residential)),
                examplePropertyDetailsAnswers = None
              ),
              routes.PropertyDetailsController.enterPostcode()
            )
          }

          "the user has started but not completed this section" in {
            test(
              sample[MultipleDisposalsDraftReturn].copy(
                triageAnswers =
                  sample[CompleteMultipleDisposalsTriageAnswers].copy(assetTypes = List(AssetType.Residential)),
                examplePropertyDetailsAnswers = Some(sample[IncompleteExamplePropertyDetailsAnswers])
              ),
              routes.PropertyDetailsController.enterPostcode()
            )
          }

        }
      }

      "redirect to the enter has a valid postcode page" when {

        "the user disposed of a non-residential property and" when {

          "the user has not started this section before" in {
            test(
              sample[MultipleDisposalsDraftReturn].copy(
                triageAnswers =
                  sample[CompleteMultipleDisposalsTriageAnswers].copy(assetTypes = List(AssetType.NonResidential)),
                examplePropertyDetailsAnswers = None
              ),
              routes.PropertyDetailsController.singleDisposalHasUkPostcode()
            )
          }

          "the user has started but not completed this section" in {
            test(
              sample[MultipleDisposalsDraftReturn].copy(
                triageAnswers =
                  sample[CompleteMultipleDisposalsTriageAnswers].copy(assetTypes = List(AssetType.NonResidential)),
                examplePropertyDetailsAnswers = Some(sample[IncompleteExamplePropertyDetailsAnswers])
              ),
              routes.PropertyDetailsController.singleDisposalHasUkPostcode()
            )
          }

        }
      }
    }

    "handling requests to display the has a uk postcode page" must {

      def performAction(): Future[Result] = controller.multipleDisposalsHasUkPostcode()(FakeRequest())

      behave like redirectToTaskListWhenNoAssetTypeBehaviour(performAction)

      behave like redirectWhenNotNonResidentialAssetTypeBehaviour(performAction)

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
            messageFromMessageKey("hasValidPostcode.multipleDisposals.title"), { doc =>
              doc.select("#back").attr("href") shouldBe expectedBackLink.url
              doc
                .select("#content > article > form")
                .attr("action") shouldBe routes.PropertyDetailsController
                .multipleDisposalsHasUkPostcodeSubmit()
                .url
            }
          )
        }

        "the user disposed of a non-residential property and the section is not complete" in {
          test(
            sample[MultipleDisposalsDraftReturn].copy(
              triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers].copy(
                assetTypes = List(AssetType.NonResidential)
              ),
              examplePropertyDetailsAnswers = None
            ),
            routes.PropertyDetailsController.multipleDisposalsGuidance()
          )
        }

        "the user disposed of a non-residential property and the section is complete" in {
          test(
            sample[MultipleDisposalsDraftReturn].copy(
              triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers].copy(
                assetTypes = List(AssetType.NonResidential)
              ),
              examplePropertyDetailsAnswers = Some(sample[CompleteExamplePropertyDetailsAnswers])
            ),
            routes.PropertyDetailsController.checkYourAnswers()
          )
        }

      }

    }

    "handling submits on the has a uk postcode page" must {

      def performAction(formData: (String, String)*): Future[Result] =
        controller.singleDisposalHasUkPostcodeSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*))

      behave like redirectToTaskListWhenNoAssetTypeBehaviour(() => performAction())

      behave like redirectWhenNotNonResidentialAssetTypeBehaviour(() => performAction())

      val nonResidentialPropertyDraftReturn = sample[MultipleDisposalsDraftReturn].copy(
        triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers].copy(
          assetTypes = List(AssetType.NonResidential)
        )
      )

      val nonResidentialFillingOutReturn = sample[FillingOutReturn].copy(
        draftReturn = nonResidentialPropertyDraftReturn
      )

      "show a form error" when {

        "the user did not submit anything" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(nonResidentialFillingOutReturn)
              )
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("hasValidPostcode.multipleDisposals.title"), { doc =>
              doc.select("#error-summary-display > ul > li > a").text() shouldBe messageFromMessageKey(
                "hasValidPostcode.multipleDisposals.error.required"
              )
            },
            BAD_REQUEST
          )

        }
      }

      "redirect to the enter postcode page" when {

        "the user selects yes" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(nonResidentialFillingOutReturn)
              )
            )
          }

          checkIsRedirect(
            performAction("hasValidPostcode" -> "true"),
            routes.PropertyDetailsController.enterPostcode()
          )
        }
      }

      "redirect to the enter UPRN page" when {

        "the user selects no" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(nonResidentialFillingOutReturn)
              )
            )
          }

          checkIsRedirect(
            performAction("hasValidPostcode" -> "false"),
            routes.PropertyDetailsController.multipleDisposalsEnterLandUprn()
          )
        }

      }

    }

    "handling requests to display the enter UPRN page" must {

      def performAction(): Future[Result] = controller.multipleDisposalsEnterLandUprn()(FakeRequest())

      behave like redirectToTaskListWhenNoAssetTypeBehaviour(performAction)

      behave like redirectWhenNotNonResidentialAssetTypeBehaviour(performAction)

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
            messageFromMessageKey("enterUPRN.multipleDisposals.title"), { doc =>
              doc.select("#back").attr("href") shouldBe expectedBackLink.url
              doc
                .select("#content > article > form")
                .attr("action") shouldBe routes.PropertyDetailsController
                .multipleDisposalsEnterLandUprnSubmit()
                .url
            }
          )
        }

        "the user disposed of a non-residential property and the section is not complete" in {
          test(
            sample[MultipleDisposalsDraftReturn].copy(
              triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers].copy(
                assetTypes = List(AssetType.NonResidential)
              ),
              examplePropertyDetailsAnswers = None
            ),
            routes.PropertyDetailsController.singleDisposalHasUkPostcode()
          )
        }

        "the user disposed of a non-residential property and the section is complete" in {
          test(
            sample[MultipleDisposalsDraftReturn].copy(
              triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers].copy(
                assetTypes = List(AssetType.NonResidential)
              ),
              examplePropertyDetailsAnswers = Some(sample[CompleteExamplePropertyDetailsAnswers])
            ),
            routes.PropertyDetailsController.singleDisposalHasUkPostcode()
          )
        }

      }

    }

    "handling submits on the enter UPRN page" must {

      def performAction(formData: (String, String)*): Future[Result] =
        controller.multipleDisposalsEnterLandUprnSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*))

      def formData(ukAddress: UkAddress): List[(String, String)] =
        List(
          "enterUPRN-line1" -> Some(ukAddress.line1),
          "address-line2"   -> ukAddress.line2,
          "address-town"    -> ukAddress.town,
          "address-county"  -> ukAddress.county,
          "postcode"        -> Some(ukAddress.postcode.value)
        ).collect { case (key, Some(value)) => key -> value }

      behave like redirectToTaskListWhenNoAssetTypeBehaviour(() => performAction())

      behave like redirectWhenNotNonResidentialAssetTypeBehaviour(() => performAction())

      val answers = IncompleteExamplePropertyDetailsAnswers.empty

      val nonResidentialPropertyDraftReturn = sample[MultipleDisposalsDraftReturn].copy(
        triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers].copy(
          assetTypes = List(AssetType.NonResidential)
        ),
        examplePropertyDetailsAnswers = Some(answers)
      )

      val nonResidentialFillingOutReturn = sample[FillingOutReturn].copy(
        draftReturn = nonResidentialPropertyDraftReturn
      )

      "show a form error" when {

        def test(formData: (String, String)*)(expectedErrorMessageKeys: String*): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(nonResidentialFillingOutReturn)
              )
            )
          }

          checkPageIsDisplayed(
            performAction(formData: _*),
            messageFromMessageKey("enterUPRN.multipleDisposals.title"), { doc =>
              val errors = doc.select("#error-summary-display > ul").first()
              errors.children.eachText().asScala.toList shouldBe expectedErrorMessageKeys.toList
                .map(messageFromMessageKey(_))
            },
            BAD_REQUEST
          )
        }

        "the user did not submit anything" in {
          test()("enterUPRN-line1.error.required", "postcode.error.required")
        }

        "the UPRN contains characters which are not digits" in {
          test(
            "enterUPRN-line1" -> "abc123",
            "postcode"        -> "ZZ00ZZ"
          )(
            "enterUPRN-line1.error.invalid"
          )
        }

        "the UPRN contains more than 13 digits" in {
          test(
            "enterUPRN-line1" -> ("1" * 13),
            "postcode"        -> "ZZ00ZZ"
          )(
            "enterUPRN-line1.error.tooLong"
          )
        }

        "the postcode does not have the correct format" in {
          test(
            "enterUPRN-line1" -> "1",
            "postcode"        -> "12VS34"
          )(
            "postcode.error.pattern"
          )
        }

        "the postcode has invalid characters" in {
          test(
            "enterUPRN-line1" -> "1",
            "postcode"        -> "12$3"
          )(
            "postcode.error.invalidCharacters"
          )
        }

        "the postcode is too long" in {
          test(
            "enterUPRN-line1" -> "1",
            "postcode"        -> ("a" * 20)
          )(
            "postcode.error.tooLong"
          )
        }

        "the address lines are too long" in {
          val tooLong = "a" * 36
          test(
            "enterUPRN-line1" -> "1",
            "postcode"        -> ("ZZ00ZZ"),
            "address-line2"   -> tooLong,
            "address-town"    -> tooLong,
            "address-county"  -> tooLong
          )(
            "address-line2.error.tooLong",
            "address-town.error.tooLong",
            "address-county.error.tooLong"
          )
        }

        "the address lines contain invalid characters" in {
          val invalidCharacters = "^%***"
          test(
            "enterUPRN-line1" -> "1",
            "postcode"        -> ("ZZ00ZZ"),
            "address-line2"   -> invalidCharacters,
            "address-town"    -> invalidCharacters,
            "address-county"  -> invalidCharacters
          )(
            "address-line2.error.pattern",
            "address-town.error.pattern",
            "address-county.error.pattern"
          )
        }

      }

      "redirect to the check your answers page" when {

        "the address submitted is valid and all updates are successful" in {
          val newAddress = UkAddress("1", Some("a"), Some("b"), Some("c"), Postcode("ZZ00ZZ"))
          val newDraftReturn = nonResidentialPropertyDraftReturn.copy(
            examplePropertyDetailsAnswers = Some(
              answers.copy(
                address = Some(newAddress)
              )
            )
          )
          val newJourney = nonResidentialFillingOutReturn.copy(draftReturn = newDraftReturn)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(nonResidentialFillingOutReturn)
              )
            )
            mockStoreDraftReturn(
              newDraftReturn,
              newJourney.subscribedDetails.cgtReference,
              newJourney.agentReferenceNumber
            )(Right(()))
            mockStoreSession(SessionData.empty.copy(journeyStatus = Some(newJourney)))(Right(()))
          }

          checkIsRedirect(performAction(formData(newAddress): _*), routes.PropertyDetailsController.checkYourAnswers())
        }

      }

      "show an error page" when {

        val answers = sample[CompleteExamplePropertyDetailsAnswers]
        val draftReturn = nonResidentialPropertyDraftReturn.copy(
          examplePropertyDetailsAnswers = Some(answers)
        )
        val journey    = nonResidentialFillingOutReturn.copy(draftReturn = draftReturn)
        val newAddress = UkAddress("1", None, None, None, Postcode("ZZ00ZZ"))

        val newDraftReturn = draftReturn.copy(
          examplePropertyDetailsAnswers = Some(answers.copy(address = newAddress))
        )
        val newJourney = journey.copy(draftReturn = newDraftReturn)

        "there is an error updating the draft return" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(journey)
              )
            )
            mockStoreDraftReturn(
              newDraftReturn,
              newJourney.subscribedDetails.cgtReference,
              newJourney.agentReferenceNumber
            )(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction(formData(newAddress): _*))
        }

        "there is an error updating the session" in {

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(journey)
              )
            )
            mockStoreDraftReturn(
              newDraftReturn,
              newJourney.subscribedDetails.cgtReference,
              newJourney.agentReferenceNumber
            )(Right(()))
            mockStoreSession(SessionData.empty.copy(journeyStatus = Some(newJourney)))(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction(formData(newAddress): _*))
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

      "redirect to the task list page" when {

        "no tax year can be found" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  sample[FillingOutReturn].copy(
                    draftReturn = sample[MultipleDisposalsDraftReturn].copy(
                      triageAnswers = sample[IncompleteMultipleDisposalsTriageAnswers].copy(taxYear = None)
                    )
                  )
                )
              )
            )
          }

          checkIsRedirect(performAction(), controllers.returns.routes.TaskListController.taskList())
        }

      }

      "display the page" when {

        val triageAnswersWithTaxYear = sample[CompleteMultipleDisposalsTriageAnswers].copy(taxYear = taxYear)

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
            messageFromMessageKey("multipleDisposalsDisposalDate.title")
          )
        }

        "the user has not started this section before" in {
          test(
            sample[MultipleDisposalsDraftReturn].copy(
              triageAnswers                 = triageAnswersWithTaxYear,
              examplePropertyDetailsAnswers = None
            ),
            routes.PropertyDetailsController.enterUkAddress()
          )
        }

        "the user has started but not completed this section" in {
          test(
            sample[MultipleDisposalsDraftReturn].copy(
              triageAnswers = triageAnswersWithTaxYear,
              examplePropertyDetailsAnswers = Some(
                sample[IncompleteExamplePropertyDetailsAnswers].copy(
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
              triageAnswers = triageAnswersWithTaxYear,
              examplePropertyDetailsAnswers = Some(
                sample[CompleteExamplePropertyDetailsAnswers].copy(
                  disposalDate = disposalDate
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
          "multipleDisposalsDisposalDate-day"   -> d.getDayOfMonth.toString,
          "multipleDisposalsDisposalDate-month" -> d.getMonthValue.toString,
          "multipleDisposalsDisposalDate-year"  -> d.getYear.toString
        )

      behave like redirectToStartBehaviour(() => performAction())

      "redirect to the task list page" when {

        "no tax year can be found" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  sample[FillingOutReturn].copy(
                    draftReturn = sample[MultipleDisposalsDraftReturn].copy(
                      triageAnswers = sample[IncompleteMultipleDisposalsTriageAnswers].copy(taxYear = None)
                    )
                  )
                )
              )
            )
          }

          checkIsRedirect(performAction(), controllers.returns.routes.TaskListController.taskList())
        }

      }

      "not update the session" when {
        "the date submitted is the same as one that already exists in session" in {

          val answers = sample[IncompleteExamplePropertyDetailsAnswers].copy(
            address      = Some(sample[UkAddress]),
            disposalDate = Some(disposalDate)
          )

          val draftReturn = sample[MultipleDisposalsDraftReturn].copy(
            triageAnswers                 = sample[CompleteMultipleDisposalsTriageAnswers].copy(taxYear = taxYear),
            examplePropertyDetailsAnswers = Some(answers)
          )

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

          checkIsRedirect(
            performAction(formData(disposalDate.value): _*),
            routes.PropertyDetailsController.checkYourAnswers()
          )
        }

      }

      "show a form error" when {

        def testFormError(
          formData: List[(String, String)]
        )(expectedErrorMessageKey: String, args: Seq[String] = Seq()) = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithValidJourneyStatus.copy(
                journeyStatus = Some(
                  sample[FillingOutReturn].copy(
                    draftReturn = sample[MultipleDisposalsDraftReturn].copy(
                      triageAnswers                 = sample[CompleteMultipleDisposalsTriageAnswers].copy(taxYear = taxYear),
                      examplePropertyDetailsAnswers = None
                    )
                  )
                )
              )
            )
          }

          checkPageIsDisplayed(
            performAction(formData: _*),
            messageFromMessageKey(s"$key.title"), { doc =>
              doc.select("#error-summary-display > ul > li > a").text() shouldBe messageFromMessageKey(
                expectedErrorMessageKey,
                args: _*
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
          val param1 = taxYear.startDateInclusive.getYear.toString
          val param2 = taxYear.endDateExclusive.getYear.toString
          testFormError(formData(LocalDateUtils.today().minusYears(1L)))(
            s"$key.error.tooFarInPast",
            Seq(param1, param2)
          )
        }

      }

      "redirect to the check your answers page" when {

        def test(
          result: => Future[Result],
          oldDraftReturn: DraftReturn,
          updatedDraftReturn: DraftReturn
        ): Unit = {

          val journey = sample[FillingOutReturn].copy(draftReturn = oldDraftReturn)
          val session = SessionData.empty.copy(journeyStatus      = Some(journey))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(
              updatedDraftReturn,
              journey.subscribedDetails.cgtReference,
              journey.agentReferenceNumber
            )(
              Right(())
            )
            mockStoreSession(
              session
                .copy(journeyStatus = Some(journey.copy(draftReturn = updatedDraftReturn)))
            )(Right(()))
          }

          checkIsRedirect(result, routes.PropertyDetailsController.checkYourAnswers())
        }

        "the user has not started a draft return and" when {

          "the user has not answered the question before" in {
            val answers = sample[IncompleteExamplePropertyDetailsAnswers].copy(
              disposalDate = Some(disposalDate)
            )

            val oldDraftReturn = sample[MultipleDisposalsDraftReturn].copy(
              triageAnswers                 = sample[CompleteMultipleDisposalsTriageAnswers].copy(taxYear = taxYear),
              examplePropertyDetailsAnswers = Some(answers)
            )

            val updatedDisposalDate = disposalDate.copy(
              value = disposalDate.value.plusDays(10)
            )

            val updatedDraftReturn = oldDraftReturn.copy(
              examplePropertyDetailsAnswers = Some(
                answers.copy(
                  disposalDate = Some(updatedDisposalDate)
                )
              )
            )

            test(
              performAction(formData(updatedDisposalDate.value): _*),
              oldDraftReturn,
              updatedDraftReturn
            )

          }

          "the user has already answered the question" in {
            val answers = sample[CompleteExamplePropertyDetailsAnswers].copy(
              disposalDate = disposalDate
            )

            val oldDraftReturn = sample[MultipleDisposalsDraftReturn].copy(
              triageAnswers                 = sample[CompleteMultipleDisposalsTriageAnswers].copy(taxYear = taxYear),
              examplePropertyDetailsAnswers = Some(answers)
            )

            val updatedDisposalDate = disposalDate.copy(
              value = disposalDate.value.plusDays(10)
            )

            val updatedDraftReturn = oldDraftReturn.copy(
              examplePropertyDetailsAnswers = Some(
                answers.copy(
                  disposalDate = updatedDisposalDate
                )
              )
            )

            test(
              performAction(formData(updatedDisposalDate.value): _*),
              oldDraftReturn,
              updatedDraftReturn
            )

          }

        }

        "the user has started a draft return and" when {

          "have completed the section and they enter a figure which is " +
            "different than one they have already entered" in {

            val answers = sample[CompleteExamplePropertyDetailsAnswers].copy(
              disposalDate = disposalDate.copy(
                value = LocalDateUtils.today().minusDays(10L)
              )
            )

            val oldDraftReturn = sample[MultipleDisposalsDraftReturn].copy(
              triageAnswers                 = sample[CompleteMultipleDisposalsTriageAnswers].copy(taxYear = taxYear),
              examplePropertyDetailsAnswers = Some(answers)
            )

            val updatedDraftReturn = oldDraftReturn.copy(
              examplePropertyDetailsAnswers = Some(
                answers.copy(
                  disposalDate = disposalDate
                )
              )
            )

            test(
              performAction(formData(disposalDate.value): _*),
              oldDraftReturn,
              updatedDraftReturn
            )

          }
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
                sample[IncompleteExamplePropertyDetailsAnswers].copy(
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
                sample[CompleteExamplePropertyDetailsAnswers].copy(
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
                        sample[IncompleteExamplePropertyDetailsAnswers].copy(
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
                        sample[CompleteExamplePropertyDetailsAnswers]
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
          result: => Future[Result],
          oldDraftReturn: DraftReturn,
          updatedDraftReturn: DraftReturn
        ): Unit = {

          val journey = sample[FillingOutReturn].copy(draftReturn = oldDraftReturn)
          val session = SessionData.empty.copy(journeyStatus      = Some(journey))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(
              updatedDraftReturn,
              journey.subscribedDetails.cgtReference,
              journey.agentReferenceNumber
            )(
              Right(())
            )
            mockStoreSession(
              session
                .copy(journeyStatus = Some(journey.copy(draftReturn = updatedDraftReturn)))
            )(Right(()))
          }

          checkIsRedirect(result, routes.PropertyDetailsController.checkYourAnswers())
        }

        "the user is on a multiple disposals journey and has completed this section" in {

          val answers = sample[CompleteExamplePropertyDetailsAnswers].copy(
            disposalPrice = AmountInPence.fromPounds(1),
            disposalDate  = disposalDate
          )

          val oldDraftReturn = sample[MultipleDisposalsDraftReturn].copy(
            examplePropertyDetailsAnswers = Some(answers)
          )

          val updatedDraftReturn = oldDraftReturn.copy(
            examplePropertyDetailsAnswers = Some(
              answers.copy(
                disposalPrice = AmountInPence.fromPounds(10)
              )
            )
          )

          test(
            performAction(key -> "10"),
            oldDraftReturn,
            updatedDraftReturn
          )
        }

        "the user hasn't ever answered the disposal price question " +
          "and the draft return and session data has been successfully updated" in {

          val answers = sample[IncompleteExamplePropertyDetailsAnswers].copy(
            address       = Some(sample[UkAddress]),
            disposalDate  = Some(disposalDate),
            disposalPrice = Some(AmountInPence.fromPounds(1))
          )

          val oldDraftReturn = sample[MultipleDisposalsDraftReturn].copy(
            examplePropertyDetailsAnswers = Some(answers)
          )

          val updatedDraftReturn = oldDraftReturn.copy(
            examplePropertyDetailsAnswers = Some(
              answers.copy(
                disposalPrice = Some(AmountInPence.fromPounds(10))
              )
            )
          )

          test(
            performAction(key -> "10"),
            oldDraftReturn,
            updatedDraftReturn
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
            messageFromMessageKey("multipleDisposalsAcquisitionPrice.title"), { doc =>
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
                sample[IncompleteExamplePropertyDetailsAnswers].copy(
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
                sample[CompleteExamplePropertyDetailsAnswers].copy(
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
                        sample[IncompleteExamplePropertyDetailsAnswers].copy(
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
                        sample[CompleteExamplePropertyDetailsAnswers]
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
          result: => Future[Result],
          oldDraftReturn: DraftReturn,
          updatedDraftReturn: DraftReturn
        ): Unit = {

          val journey = sample[FillingOutReturn].copy(draftReturn = oldDraftReturn)
          val session = SessionData.empty.copy(journeyStatus      = Some(journey))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(
              updatedDraftReturn,
              journey.subscribedDetails.cgtReference,
              journey.agentReferenceNumber
            )(
              Right(())
            )
            mockStoreSession(
              session
                .copy(journeyStatus = Some(journey.copy(draftReturn = updatedDraftReturn)))
            )(Right(()))
          }

          checkIsRedirect(result, routes.PropertyDetailsController.checkYourAnswers())
        }

        "the user is on a multiple disposals journey and has completed this section" in {
          val answers = sample[CompleteExamplePropertyDetailsAnswers].copy(
            disposalDate     = disposalDate,
            acquisitionPrice = AmountInPence.fromPounds(10)
          )

          val oldDraftReturn = sample[MultipleDisposalsDraftReturn].copy(
            examplePropertyDetailsAnswers = Some(answers)
          )

          val newDraftReturn = oldDraftReturn.copy(
            examplePropertyDetailsAnswers = Some(
              answers.copy(
                acquisitionPrice = AmountInPence.fromPounds(100)
              )
            )
          )

          test(
            performAction(key -> "100"),
            oldDraftReturn,
            newDraftReturn
          )
        }

        "the user hasn't ever answered the acquisition price question " +
          "and the draft return and session data has been successfully updated" in {

          val answers = sample[IncompleteExamplePropertyDetailsAnswers].copy(
            disposalDate     = Some(disposalDate),
            acquisitionPrice = Some(AmountInPence.fromPounds(10))
          )

          val oldDraftReturn = sample[MultipleDisposalsDraftReturn].copy(
            examplePropertyDetailsAnswers = Some(answers)
          )

          val newDraftReturn = oldDraftReturn.copy(
            examplePropertyDetailsAnswers = Some(
              answers.copy(
                acquisitionPrice = Some(AmountInPence.fromPounds(100))
              )
            )
          )

          test(
            performAction(key -> "100"),
            oldDraftReturn,
            newDraftReturn
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
        CompleteExamplePropertyDetailsAnswers(address, disposalDate, disposalPrice, acquisitionPrice)

      val allQuestionsAnswered = IncompleteExamplePropertyDetailsAnswers(
        Some(completeAnswers.address),
        Some(completeAnswers.disposalDate),
        Some(completeAnswers.disposalPrice),
        Some(completeAnswers.acquisitionPrice)
      )

      val currentDraftReturn =
        sample[MultipleDisposalsDraftReturn].copy(
          triageAnswers                 = sample[CompleteMultipleDisposalsTriageAnswers].copy(assetTypes = List(AssetType.Residential)),
          examplePropertyDetailsAnswers = Some(allQuestionsAnswered)
        )
      val currentJourney     = sample[FillingOutReturn].copy(draftReturn             = currentDraftReturn)
      val updatedDraftReturn = currentDraftReturn.copy(examplePropertyDetailsAnswers = Some(completeAnswers))
      val updatedJourney     = currentJourney.copy(draftReturn                       = updatedDraftReturn)

      behave like redirectToTaskListWhenNoAssetTypeBehaviour(performAction)

      "redirect to the guidance page" when {

        "the user has not started the section" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(journeyStatus = Some(
                sample[FillingOutReturn].copy(
                  draftReturn = currentDraftReturn.copy(
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
                  draftReturn = currentDraftReturn.copy(
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

      "redirect to the disposal date page" when {

        "the user has started the section but there is no disposal date" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(journeyStatus = Some(
                sample[FillingOutReturn].copy(
                  draftReturn = currentDraftReturn.copy(
                    examplePropertyDetailsAnswers = Some(
                      allQuestionsAnswered.copy(disposalDate = None)
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
                  draftReturn = currentDraftReturn.copy(
                    examplePropertyDetailsAnswers = Some(
                      allQuestionsAnswered.copy(disposalPrice = None)
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
                  draftReturn = currentDraftReturn.copy(
                    examplePropertyDetailsAnswers = Some(
                      allQuestionsAnswered.copy(acquisitionPrice = None)
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

  def redirectToTaskListWhenNoAssetTypeBehaviour(performAction: () => Future[Result]): Unit =
    "redirect to the task list" when {

      "no asset type can be found" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            SessionData.empty.copy(
              journeyStatus = Some(
                sample[FillingOutReturn].copy(
                  draftReturn = sample[MultipleDisposalsDraftReturn].copy(
                    triageAnswers = sample[IncompleteMultipleDisposalsTriageAnswers].copy(assetTypes = None)
                  )
                )
              )
            )
          )
        }

        checkIsRedirect(performAction(), controllers.returns.routes.TaskListController.taskList())
      }
    }

  def redirectWhenNotNonResidentialAssetTypeBehaviour(performAction: () => Future[Result]): Unit =
    "redirect to the check your answers page" when {

      "the user did not dispose of a non-residential property" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            SessionData.empty.copy(
              journeyStatus = Some(
                sample[FillingOutReturn].copy(
                  draftReturn = sample[MultipleDisposalsDraftReturn].copy(
                    triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers].copy(
                      assetTypes = List(AssetType.Residential)
                    )
                  )
                )
              )
            )
          )
        }

        checkIsRedirect(performAction(), routes.PropertyDetailsController.checkYourAnswers())
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
