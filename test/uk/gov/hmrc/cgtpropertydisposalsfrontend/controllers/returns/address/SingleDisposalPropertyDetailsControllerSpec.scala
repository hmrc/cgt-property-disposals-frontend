/*
 * Copyright 2023 HM Revenue & Customs
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

import org.jsoup.nodes.Document
import org.scalatest.matchers.should.Matchers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.SampledScalaCheck
import play.api.i18n.MessagesApi
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.{Call, Result}
import play.api.test.CSRFTokenHelper._
import play.api.test.FakeRequest
import play.api.test.Helpers.BAD_REQUEST
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.AddressControllerSpec
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.address.SingleDisposalPropertyDetailsControllerSpec.validatePropertyAddressPage
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.address.{routes => returnsAddressRoutes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.{ReturnsServiceSupport, StartingToAmendToFillingOutReturnSpecBehaviour}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, PreviousReturnData, StartingToAmendReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address.{NonUkAddress, UkAddress}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.{Address, Postcode}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.AddressGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.DraftReturnGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.IdGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.JourneyStatusGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.ReturnGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.SubscribedDetailsGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.TriageQuestionsGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.{GGCredId, UUIDGenerator}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.SubscribedDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.IndividualUserType.Self
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.{CompleteSingleDisposalTriageAnswers, IncompleteSingleDisposalTriageAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{AssetType, DraftSingleDisposalReturn, ReturnSummary}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, SessionData, UserType}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views.address.AddressJourneyType.Returns.FillingOutReturnAddressJourney

import scala.concurrent.Future
import scala.jdk.CollectionConverters._

class SingleDisposalPropertyDetailsControllerSpec
    extends AddressControllerSpec[FillingOutReturnAddressJourney]
    with SampledScalaCheck
    with RedirectToStartBehaviour
    with ReturnsServiceSupport
    with StartingToAmendToFillingOutReturnSpecBehaviour {

  private val mockUUIDGenerator = mock[UUIDGenerator]

  private val draftReturn =
    sample[DraftSingleDisposalReturn].copy(
      triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
        .copy(assetType = AssetType.Residential, individualUserType = None),
      propertyAddress = Some(ukAddress(1))
    )

  protected override val validJourneyStatus: FillingOutReturnAddressJourney = FillingOutReturnAddressJourney(
    FillingOutReturn(
      sample[SubscribedDetails],
      sample[GGCredId],
      None,
      draftReturn,
      None,
      None
    ),
    Right(draftReturn),
    None
  )

  override def overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[ReturnsService].toInstance(mockReturnsService),
      bind[UUIDGenerator].toInstance(mockUUIDGenerator)
    ) ::: super.overrideBindings

  protected override val controller: PropertyDetailsController = instanceOf[PropertyDetailsController]

  lazy implicit val messagesApi: MessagesApi = controller.messagesApi

  override def updateAddress(
    journey: FillingOutReturnAddressJourney,
    address: Address
  ): FillingOutReturn = {
    val isFurtherOrAmendReturn = journey.journey.isFurtherOrAmendReturn.contains(true)

    address match {
      case a: UkAddress    =>
        journey.journey
          .copy(draftReturn =
            draftReturn.copy(
              propertyAddress = Some(a),
              exemptionAndLossesAnswers = if (isFurtherOrAmendReturn) None else draftReturn.exemptionAndLossesAnswers,
              yearToDateLiabilityAnswers = if (isFurtherOrAmendReturn) None else draftReturn.yearToDateLiabilityAnswers
            )
          )
      case _: NonUkAddress => journey.journey
    }
  }

  override val mockUpdateAddress: Option[(FillingOutReturnAddressJourney, Address, Either[Error, Unit]) => Unit] =
    Some {
      case (
            newDetails: FillingOutReturnAddressJourney,
            a: UkAddress,
            r: Either[Error, Unit]
          ) =>
        mockStoreDraftReturn(
          newDetails.journey.copy(draftReturn = draftReturn.copy(propertyAddress = Some(a)))
        )(r)

      case (_, _: NonUkAddress, _) =>
        sys.error("Non UK addresses not handled in this spec")
    }

  def redirectToStartBehaviour(performAction: () => Future[Result]): Unit =
    redirectToStartWhenInvalidJourney(
      () => performAction(),
      {
        case _: FillingOutReturn      => true
        case _: StartingToAmendReturn => true
        case _                        => false
      }
    )

  "AddressController" when {

    "handling requests to display the enter UK address page" must {

      def performAction(): Future[Result] =
        controller.enterUkAddress()(FakeRequest())

      behave like redirectToStartBehaviour(() => performAction())

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.enterUkAddress(),
        mockUUIDGenerator
      )

      behave like displayEnterUkAddressPage(UserType.Individual, None, () => performAction())

      behave like displayEnterUkAddressPage(UserType.Agent, None, () => performAction())

      behave like displayEnterUkAddressPage(UserType.Organisation, None, () => performAction())
    }

    "handling submitted addresses from enter UK address page" must {

      def performAction(formData: Seq[(String, String)]): Future[Result] =
        controller.enterUkAddressSubmit()(
          FakeRequest().withFormUrlEncodedBody(formData*).withCSRFToken.withMethod("POST")
        )

      behave like redirectToStartBehaviour(() => performAction(Seq.empty))

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.enterUkAddressSubmit(),
        mockUUIDGenerator
      )

      behave like submitEnterUkAddress(performAction, returnsAddressRoutes.PropertyDetailsController.checkYourAnswers())

    }

    "handling requests to display the enter postcode page" must {

      def performAction(): Future[Result] =
        controller.enterPostcode()(FakeRequest())

      behave like redirectToStartBehaviour(() => performAction())

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.enterPostcode(),
        mockUUIDGenerator
      )

      behave like enterPostcodePage(UserType.Individual, None, () => performAction())

      behave like enterPostcodePage(UserType.Agent, None, () => performAction())

      behave like enterPostcodePage(UserType.Organisation, None, () => performAction())

    }

    "handling submitted postcodes and filters" must {

      def performAction(formData: Seq[(String, String)]): Future[Result] =
        controller.enterPostcodeSubmit()(
          FakeRequest().withFormUrlEncodedBody(formData*).withCSRFToken.withMethod("POST")
        )

      behave like redirectToStartBehaviour(() => performAction(Seq.empty))

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.enterPostcodeSubmit(),
        mockUUIDGenerator
      )

      behave like submitEnterPostcode(performAction, returnsAddressRoutes.PropertyDetailsController.selectAddress())

    }

    "handling requests to display the select address page" must {

      def performAction(): Future[Result] =
        controller.selectAddress()(FakeRequest())

      behave like redirectToStartBehaviour(() => performAction())

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.selectAddress(),
        mockUUIDGenerator
      )

      behave like displaySelectAddress(
        UserType.Individual,
        None,
        () => performAction(),
        controllers.returns.address.routes.PropertyDetailsController
          .enterPostcode()
      )

      behave like displaySelectAddress(
        UserType.Agent,
        None,
        () => performAction(),
        controllers.returns.address.routes.PropertyDetailsController
          .enterPostcode()
      )

      behave like displaySelectAddress(
        UserType.Organisation,
        None,
        () => performAction(),
        controllers.returns.address.routes.PropertyDetailsController
          .enterPostcode()
      )

    }

    "handling submitted selected addresses" must {

      def performAction(formData: Seq[(String, String)]): Future[Result] =
        controller.selectAddressSubmit()(
          FakeRequest().withFormUrlEncodedBody(formData*).withCSRFToken.withMethod("POST")
        )

      behave like redirectToStartBehaviour(() => performAction(Seq.empty))

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.selectAddressSubmit(),
        mockUUIDGenerator
      )

      behave like submitSelectAddress(
        performAction,
        controllers.returns.address.routes.PropertyDetailsController
          .enterPostcode(),
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
          checkIsRedirect(
            result,
            returnsAddressRoutes.PropertyDetailsController.checkYourAnswers()
          )
        }

      }

    }

    "handling requests to display the check your answers page" must {

      def performAction(): Future[Result] =
        controller.checkYourAnswers()(FakeRequest())

      behave like redirectToStartBehaviour(() => performAction())

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.checkYourAnswers(),
        mockUUIDGenerator
      )

      behave like redirectToTaskListWhenNoAssetTypeBehaviour(() => performAction())

      def testIsCheckYourAnswers(
        result: Future[Result],
        ukAddressDetails: UkAddress,
        expectedTitleKey: String
      ): Unit =
        checkPageIsDisplayed(
          result,
          messageFromMessageKey(expectedTitleKey),
          doc => validatePropertyAddressPage(ukAddressDetails, doc)
        )

      "redirect to the has uk postcode page if there is no address in session and the user " +
        "has a non-residential property type" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(journeyStatus =
                Some(
                  sample[FillingOutReturn].copy(
                    draftReturn = draftReturn.copy(
                      triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                        .copy(assetType = AssetType.NonResidential),
                      propertyAddress = None
                    )
                  )
                )
              )
            )
          }

          checkIsRedirect(
            performAction(),
            routes.PropertyDetailsController.singleDisposalHasUkPostcode()
          )
        }

      "redirect to the enter postcode page if there is no property address in session" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            SessionData.empty.copy(journeyStatus =
              Some(
                sample[FillingOutReturn].copy(
                  draftReturn = draftReturn.copy(
                    triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                      .copy(assetType = AssetType.Residential),
                    propertyAddress = None
                  )
                )
              )
            )
          )
        }

        checkIsRedirect(
          performAction(),
          routes.PropertyDetailsController.enterPostcode()
        )
      }

      "display the page if there is an address in session" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionWithValidJourneyStatus)
        }

        testIsCheckYourAnswers(
          performAction(),
          draftReturn.propertyAddress.getOrElse(sys.error("Error")),
          "returns.property-address.cya.title"
        )
      }

    }

    "handling check your answers submits" must {

      def performAction(): Future[Result] =
        controller.checkYourAnswersSubmit()(FakeRequest())

      behave like redirectToStartBehaviour(() => performAction())

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.checkYourAnswersSubmit(),
        mockUUIDGenerator
      )

      "redirect to the task list" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionWithValidJourneyStatus)
        }

        checkIsRedirect(
          performAction(),
          controllers.returns.routes.TaskListController.taskList()
        )
      }

    }

    "handling requests to display the has a uk postcode page" must {

      def performAction(): Future[Result] =
        controller.singleDisposalHasUkPostcode()(FakeRequest())

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.singleDisposalHasUkPostcode(),
        mockUUIDGenerator
      )

      behave like redirectToTaskListWhenNoAssetTypeBehaviour(() => performAction())

      behave like redirectWhenNotNonResidentialAssetTypeBehaviour(() => performAction())

      "display the page" when {

        def test(
          draftReturn: DraftSingleDisposalReturn,
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
            messageFromMessageKey("hasValidPostcode.singleDisposal.title"),
            { doc =>
              doc.select("#back, .govuk-back-link").attr("href") shouldBe expectedBackLink.url
              doc
                .select("#content > article > form, #main-content form")
                .attr("action")                                  shouldBe routes.PropertyDetailsController
                .singleDisposalHasUkPostcodeSubmit()
                .url
            }
          )
        }

        "the user disposed of a non-residential property and the section is not complete" in {
          test(
            sample[DraftSingleDisposalReturn].copy(
              triageAnswers = sample[CompleteSingleDisposalTriageAnswers].copy(
                assetType = AssetType.NonResidential
              ),
              propertyAddress = None
            ),
            controllers.returns.routes.TaskListController.taskList()
          )
        }

        "the user disposed of a non-residential property and the section is complete" in {
          test(
            sample[DraftSingleDisposalReturn].copy(
              triageAnswers = sample[CompleteSingleDisposalTriageAnswers].copy(
                assetType = AssetType.NonResidential
              ),
              propertyAddress = Some(sample[UkAddress])
            ),
            routes.PropertyDetailsController.checkYourAnswers()
          )
        }

      }

    }

    "handling submits on the has a uk postcode page" must {

      def performAction(formData: (String, String)*): Future[Result] =
        controller.singleDisposalHasUkPostcodeSubmit()(
          FakeRequest().withFormUrlEncodedBody(formData*).withMethod("POST")
        )

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.singleDisposalHasUkPostcodeSubmit(),
        mockUUIDGenerator
      )

      behave like redirectToTaskListWhenNoAssetTypeBehaviour(() => performAction())

      behave like redirectWhenNotNonResidentialAssetTypeBehaviour(() => performAction())

      val nonResidentialPropertyDraftReturn =
        sample[DraftSingleDisposalReturn].copy(
          triageAnswers = sample[CompleteSingleDisposalTriageAnswers].copy(
            assetType = AssetType.NonResidential
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
            messageFromMessageKey("hasValidPostcode.singleDisposal.title"),
            doc =>
              doc
                .select("[data-spec='errorSummaryDisplay'] a")
                .text() shouldBe messageFromMessageKey(
                "hasValidPostcode.singleDisposal.error.required"
              ),
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

      "redirect to the check user has UPRN page" when {

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
            routes.PropertyDetailsController.checkUPRN()
          )
        }

      }

    }

    "handling requests to check if user has UPRN page" must {

      val nonResidentialPropertyDraftReturn =
        sample[DraftSingleDisposalReturn].copy(
          triageAnswers = sample[CompleteSingleDisposalTriageAnswers].copy(
            assetType = AssetType.NonResidential
          )
        )

      val nonResidentialFillingOutReturn = sample[FillingOutReturn].copy(
        draftReturn = nonResidentialPropertyDraftReturn
      )

      def performAction(formData: (String, String)*): Future[Result] =
        controller.checkUPRNSubmit()(
          FakeRequest().withFormUrlEncodedBody(formData*).withMethod("POST")
        )

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.checkUPRN(),
        mockUUIDGenerator
      )

      behave like redirectToTaskListWhenNoAssetTypeBehaviour(() => performAction())

      behave like redirectWhenNotNonResidentialAssetTypeBehaviour(() => performAction())

      val sessionData = SessionData.empty.copy(
        journeyStatus = Some(nonResidentialFillingOutReturn)
      )

      "redirect to enter UPRN details page" when {

        "the user selects yes" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionData)
          }

          checkIsRedirect(
            performAction("doesPropertyHaveUPRN" -> "true"),
            routes.PropertyDetailsController.singleDisposalEnterLandUprn()
          )
        }
      }

      "redirect to enter address details page" when {

        "the user selects no" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionData)
          }

          checkIsRedirect(
            performAction("doesPropertyHaveUPRN" -> "false"),
            routes.PropertyDetailsController.noUPRNEnterAddress()
          )
        }
      }
    }

    "handling requests to display the enter UPRN page" must {

      def performAction(): Future[Result] =
        controller.singleDisposalEnterLandUprn()(FakeRequest())

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.singleDisposalEnterLandUprn(),
        mockUUIDGenerator
      )

      behave like redirectToTaskListWhenNoAssetTypeBehaviour(() => performAction())

      behave like redirectWhenNotNonResidentialAssetTypeBehaviour(() => performAction())

      "display the page" when {

        def test(
          draftReturn: DraftSingleDisposalReturn,
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
            messageFromMessageKey("enterUPRN.title"),
            { doc =>
              doc.select("#back, .govuk-back-link").attr("href") shouldBe expectedBackLink.url
              doc
                .select("#content > article > form, #main-content form")
                .attr("action")                                  shouldBe routes.PropertyDetailsController.singleDisposalEnterLandUprnSubmit().url
            }
          )
        }

        "the user disposed of a non-residential property and the section is not complete" in {
          test(
            sample[DraftSingleDisposalReturn].copy(
              triageAnswers = sample[CompleteSingleDisposalTriageAnswers].copy(
                assetType = AssetType.NonResidential
              ),
              propertyAddress = None
            ),
            routes.PropertyDetailsController.checkUPRN()
          )
        }

        "the user disposed of a non-residential property and the section is complete" in {
          test(
            sample[DraftSingleDisposalReturn].copy(
              triageAnswers = sample[CompleteSingleDisposalTriageAnswers].copy(
                assetType = AssetType.NonResidential
              ),
              propertyAddress = Some(sample[UkAddress])
            ),
            routes.PropertyDetailsController.checkUPRN()
          )
        }
      }

    }

    "handling submits on the enter UPRN page" must {

      def performAction(formData: (String, String)*): Future[Result] =
        controller.singleDisposalEnterLandUprnSubmit()(
          FakeRequest().withFormUrlEncodedBody(formData*).withMethod("POST")
        )

      def formData(ukAddress: UkAddress): List[(String, String)] =
        List(
          "enterUPRN-line1" -> Some(ukAddress.line1),
          "postcode"        -> Some(ukAddress.postcode.value)
        ).collect { case (key, Some(value)) => key -> value }

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.singleDisposalEnterLandUprnSubmit(),
        mockUUIDGenerator
      )

      behave like redirectToTaskListWhenNoAssetTypeBehaviour(() => performAction())

      behave like redirectWhenNotNonResidentialAssetTypeBehaviour(() => performAction())

      val triageAnswers = sample[CompleteSingleDisposalTriageAnswers].copy(
        assetType = AssetType.NonResidential,
        individualUserType = Some(Self)
      )

      val taxYearStartYear: String =
        triageAnswers
          .fold(
            _.disposalDate.map(_.taxYear.startDateInclusive.getYear),
            c => Some(c.disposalDate.taxYear.startDateInclusive.getYear)
          )
          .map(_.toString)
          .getOrElse("2020")

      val nonResidentialPropertyDraftReturn =
        sample[DraftSingleDisposalReturn].copy(
          triageAnswers = triageAnswers,
          representeeAnswers = None,
          propertyAddress = None
        )

      val nonResidentialFillingOutReturn = sample[FillingOutReturn].copy(
        draftReturn = nonResidentialPropertyDraftReturn,
        amendReturnData = None,
        previousSentReturns = None
      )

      "show a form error" when {

        def test(
          formData: (String, String)*
        )(expectedErrorMessageKeys: String*): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(nonResidentialFillingOutReturn)
              )
            )
          }

          checkPageIsDisplayed(
            performAction(formData*),
            messageFromMessageKey("enterUPRN.title"),
            { doc =>
              val errors = doc.select("[data-spec='errorSummaryDisplay'] ul").first()
              errors.children
                .eachText()
                .asScala
                .toList shouldBe expectedErrorMessageKeys.toList
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
            "postcode.error.pattern"
          )
        }

        "the postcode is too long" in {
          test(
            "enterUPRN-line1" -> "1",
            "postcode"        -> ("a" * 20)
          )(
            "postcode.error.pattern"
          )
        }
      }

      "redirect to the check your answers page" when {

        "the address submitted is valid and all updates are successful" in {
          val newAddress     =
            UkAddress("1", None, None, None, Postcode("ZZ00ZZ"))
          val newDraftReturn = nonResidentialPropertyDraftReturn.copy(
            propertyAddress = Some(newAddress)
          )
          val newJourney     =
            nonResidentialFillingOutReturn
              .copy(draftReturn = newDraftReturn, amendReturnData = None, previousSentReturns = None)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(nonResidentialFillingOutReturn)
              )
            )
            mockStoreDraftReturn(newJourney)(Right(()))
            mockStoreSession(
              SessionData.empty.copy(journeyStatus = Some(newJourney))
            )(Right(()))
          }

          checkIsRedirect(
            performAction(formData(newAddress)*),
            routes.PropertyDetailsController.checkYourAnswers()
          )
        }

      }

      "show an error page" when {

        val draftReturn = nonResidentialPropertyDraftReturn.copy(
          propertyAddress = Some(sample[UkAddress])
        )
        val journey     =
          nonResidentialFillingOutReturn
            .copy(
              draftReturn = draftReturn,
              previousSentReturns = Some(
                sample[PreviousReturnData].copy(
                  summaries = List(sample[ReturnSummary].copy(taxYear = taxYearStartYear))
                )
              )
            )
        val newAddress  = UkAddress("1", None, None, None, Postcode("ZZ00ZZ"))

        val newDraftReturn = draftReturn.copy(
          propertyAddress = Some(newAddress),
          exemptionAndLossesAnswers = None,
          yearToDateLiabilityAnswers = None
        )
        val newJourney     = journey.copy(draftReturn = newDraftReturn)

        "there is an error updating the draft return" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(journey)
              )
            )
            mockStoreDraftReturn(newJourney)(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction(formData(newAddress)*))
        }

        "there is an error updating the session" in {

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(journey)
              )
            )
            mockStoreDraftReturn(newJourney)(Right(()))
            mockStoreSession(
              SessionData.empty.copy(journeyStatus = Some(newJourney))
            )(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction(formData(newAddress)*))
        }

      }

    }

    "handling requests to display the enter address page for no UPRN" must {

      def performAction(): Future[Result] = controller.noUPRNEnterAddress()(FakeRequest())

      behave like amendReturnToFillingOutReturnSpecBehaviour(controller.noUPRNEnterAddress(), mockUUIDGenerator)

      behave like redirectToTaskListWhenNoAssetTypeBehaviour(() => performAction())

      behave like redirectWhenNotNonResidentialAssetTypeBehaviour(() => performAction())

      "display the page" when {

        def test(
          draftReturn: DraftSingleDisposalReturn,
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
            messageFromMessageKey("noUPRN.title"),
            { doc =>
              doc.select("#back, .govuk-back-link").attr("href") shouldBe expectedBackLink.url
              doc
                .select("#content > article > form, #main-content form")
                .attr("action")                                  shouldBe routes.PropertyDetailsController.noUPRNEnterAddressSubmit().url
            }
          )
        }

        "the user disposed of a non-residential property and the section is not complete" in {
          test(
            sample[DraftSingleDisposalReturn].copy(
              triageAnswers = sample[CompleteSingleDisposalTriageAnswers].copy(
                assetType = AssetType.NonResidential
              ),
              propertyAddress = None
            ),
            routes.PropertyDetailsController.checkUPRN()
          )
        }

        "the user disposed of a non-residential property and the section is complete" in {
          test(
            sample[DraftSingleDisposalReturn].copy(
              triageAnswers = sample[CompleteSingleDisposalTriageAnswers].copy(
                assetType = AssetType.NonResidential
              ),
              propertyAddress = Some(sample[UkAddress])
            ),
            routes.PropertyDetailsController.checkUPRN()
          )
        }
      }

    }

    "handling submits on the enter address page for no UPRN" must {

      def performAction(formData: (String, String)*): Future[Result] =
        controller.noUPRNEnterAddressSubmit()(
          FakeRequest().withFormUrlEncodedBody(formData*).withMethod("POST")
        )

      def formData(ukAddress: UkAddress): List[(String, String)] =
        List(
          "address-line1"  -> Some(ukAddress.line1),
          "address-line2"  -> ukAddress.line2,
          "address-town"   -> ukAddress.town,
          "address-county" -> ukAddress.county,
          "postcode"       -> Some(ukAddress.postcode.value)
        ).collect { case (key, Some(value)) => key -> value }

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.noUPRNEnterAddressSubmit(),
        mockUUIDGenerator
      )

      behave like redirectToTaskListWhenNoAssetTypeBehaviour(() => performAction())

      behave like redirectWhenNotNonResidentialAssetTypeBehaviour(() => performAction())

      val triageAnswers = sample[CompleteSingleDisposalTriageAnswers].copy(
        assetType = AssetType.NonResidential,
        individualUserType = Some(Self)
      )

      val taxYearStartYear: String =
        triageAnswers
          .fold(
            _.disposalDate.map(_.taxYear.startDateInclusive.getYear),
            c => Some(c.disposalDate.taxYear.startDateInclusive.getYear)
          )
          .map(_.toString)
          .getOrElse("2020")

      val nonResidentialPropertyDraftReturn =
        sample[DraftSingleDisposalReturn].copy(
          triageAnswers = triageAnswers,
          representeeAnswers = None,
          propertyAddress = None
        )

      val nonResidentialFillingOutReturn = sample[FillingOutReturn].copy(
        draftReturn = nonResidentialPropertyDraftReturn,
        amendReturnData = None,
        previousSentReturns = None
      )

      "show a form error" when {

        def test(
          formData: (String, String)*
        )(expectedErrorMessageKeys: String*): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(nonResidentialFillingOutReturn)
              )
            )
          }

          checkPageIsDisplayed(
            performAction(formData*),
            messageFromMessageKey("noUPRN.title"),
            { doc =>
              val errors = doc.select("[data-spec='errorSummaryDisplay'] ul").first()
              errors.children
                .eachText()
                .asScala
                .toList shouldBe expectedErrorMessageKeys.toList
                .map(messageFromMessageKey(_))
            },
            BAD_REQUEST
          )
        }

        "the user did not submit anything" in {
          test()("address-line1.error.required", "postcode.error.required")
        }

        "the address line 1 contains characters which are not digits" in {
          test(
            "address-line1" -> "abc$$123",
            "postcode"      -> "ZZ00ZZ"
          )(
            "address-line1.error.pattern"
          )
        }

        "the address line 1 contains more than 35 digits" in {
          test(
            "address-line1" -> ("1" * 36),
            "postcode"      -> "ZZ00ZZ"
          )(
            "address-line1.error.tooLong"
          )
        }

        "the postcode does not have the correct format" in {
          test(
            "address-line1" -> "1",
            "postcode"      -> "12VS34"
          )(
            "postcode.error.pattern"
          )
        }

        "the postcode has invalid characters" in {
          test(
            "address-line1" -> "1",
            "postcode"      -> "12$3"
          )(
            "postcode.error.pattern"
          )
        }

        "the postcode is too long" in {
          test(
            "address-line1" -> "1",
            "postcode"      -> ("a" * 20)
          )(
            "postcode.error.pattern"
          )
        }
      }

      "redirect to the check your answers page" when {

        "the address submitted is valid and all updates are successful" in {
          val newAddress     = UkAddress("1", Some("a"), Some("b"), Some("c"), Postcode("ZZ00ZZ"))
          val newDraftReturn = nonResidentialPropertyDraftReturn.copy(
            propertyAddress = Some(newAddress)
          )
          val newJourney     =
            nonResidentialFillingOutReturn
              .copy(draftReturn = newDraftReturn, amendReturnData = None, previousSentReturns = None)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(nonResidentialFillingOutReturn)
              )
            )
            mockStoreDraftReturn(newJourney)(Right(()))
            mockStoreSession(
              SessionData.empty.copy(journeyStatus = Some(newJourney))
            )(Right(()))
          }

          checkIsRedirect(
            performAction(formData(newAddress)*),
            routes.PropertyDetailsController.checkYourAnswers()
          )
        }

      }

      "show an error page" when {

        val draftReturn = nonResidentialPropertyDraftReturn.copy(
          propertyAddress = Some(sample[UkAddress])
        )
        val journey     =
          nonResidentialFillingOutReturn
            .copy(
              draftReturn = draftReturn,
              previousSentReturns = Some(
                sample[PreviousReturnData].copy(
                  summaries = List(sample[ReturnSummary].copy(taxYear = taxYearStartYear))
                )
              )
            )
        val newAddress  = UkAddress("1", None, None, None, Postcode("ZZ00ZZ"))

        val newDraftReturn = draftReturn.copy(
          propertyAddress = Some(newAddress),
          exemptionAndLossesAnswers = None,
          yearToDateLiabilityAnswers = None
        )
        val newJourney     = journey.copy(draftReturn = newDraftReturn)

        "there is an error updating the draft return" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(journey)
              )
            )
            mockStoreDraftReturn(newJourney)(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction(formData(newAddress)*))
        }

        "there is an error updating the session" in {

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(journey)
              )
            )
            mockStoreDraftReturn(newJourney)(Right(()))
            mockStoreSession(
              SessionData.empty.copy(journeyStatus = Some(newJourney))
            )(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction(formData(newAddress)*))
        }

      }

    }
  }

  def redirectToTaskListWhenNoAssetTypeBehaviour(
    performAction: () => Future[Result]
  ): Unit =
    "redirect to the task list" when {

      "no asset type can be found" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            SessionData.empty.copy(
              journeyStatus = Some(
                sample[FillingOutReturn].copy(
                  draftReturn = sample[DraftSingleDisposalReturn].copy(
                    triageAnswers = sample[IncompleteSingleDisposalTriageAnswers]
                      .copy(assetType = None)
                  )
                )
              )
            )
          )
        }

        checkIsRedirect(
          performAction(),
          controllers.returns.routes.TaskListController.taskList()
        )
      }
    }

  def redirectWhenNotNonResidentialAssetTypeBehaviour(
    performAction: () => Future[Result]
  ): Unit =
    "redirect to the check your answers page" when {

      "the user did not dispose of a non-residential property" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            SessionData.empty.copy(
              journeyStatus = Some(
                sample[FillingOutReturn].copy(
                  draftReturn = sample[DraftSingleDisposalReturn].copy(
                    triageAnswers = sample[CompleteSingleDisposalTriageAnswers].copy(
                      assetType = AssetType.Residential
                    )
                  )
                )
              )
            )
          )
        }

        checkIsRedirect(
          performAction(),
          routes.PropertyDetailsController.checkYourAnswers()
        )
      }

    }

}

object SingleDisposalPropertyDetailsControllerSpec extends Matchers {
  def validatePropertyAddressPage(
    ukAddress: UkAddress,
    doc: Document
  ): Unit =
    doc.select("#property-address-answer").text() shouldBe
      List(
        Some(ukAddress.line1),
        ukAddress.line2,
        ukAddress.town,
        ukAddress.county,
        Some(ukAddress.postcode.value)
      ).collect { case Some(s) => s.trim }
        .mkString(" ")
}
