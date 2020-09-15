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

import org.jsoup.nodes.Document
import org.scalatest.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
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
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.{ReturnsServiceSupport, StartingToAmendToFillingOutReturnSpecBehaviour}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.address.SingleDisposalPropertyDetailsControllerSpec.validatePropertyAddressPage
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.address.{routes => returnsAddressRoutes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.AddressGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.DraftReturnGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.IdGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.JourneyStatusGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.SubscribedDetailsGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.TriageQuestionsGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, StartingToAmendReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address.{NonUkAddress, UkAddress}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.{Address, Postcode}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.{GGCredId, UUIDGenerator}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.SubscribedDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.{CompleteSingleDisposalTriageAnswers, IncompleteSingleDisposalTriageAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{AssetType, DraftSingleDisposalReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, SessionData, UserType}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views.address.AddressJourneyType.Returns.FillingOutReturnAddressJourney

import scala.collection.JavaConverters._
import scala.concurrent.Future

class SingleDisposalPropertyDetailsControllerSpec
    extends AddressControllerSpec[FillingOutReturnAddressJourney]
    with ScalaCheckDrivenPropertyChecks
    with RedirectToStartBehaviour
    with ReturnsServiceSupport
    with StartingToAmendToFillingOutReturnSpecBehaviour {

  val mockUUIDGenerator: UUIDGenerator = mock[UUIDGenerator]

  val draftReturn: DraftSingleDisposalReturn =
    sample[DraftSingleDisposalReturn].copy(
      triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
        .copy(assetType = AssetType.Residential, individualUserType = None),
      propertyAddress = Some(ukAddress(1))
    )

  val validJourneyStatus = FillingOutReturnAddressJourney(
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

  lazy val controller = instanceOf[PropertyDetailsController]

  lazy implicit val messagesApi: MessagesApi = controller.messagesApi

  override def updateAddress(
    journey: FillingOutReturnAddressJourney,
    address: Address
  ): FillingOutReturn =
    address match {
      case a: UkAddress    =>
        journey.journey
          .copy(draftReturn = draftReturn.copy(propertyAddress = Some(a)))
      case _: NonUkAddress => journey.journey
    }

  override val mockUpdateAddress: Option[
    (FillingOutReturnAddressJourney, Address, Either[Error, Unit]) => Unit
  ] =
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
      performAction,
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

      behave like redirectToStartBehaviour(performAction)

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.enterUkAddress(),
        mockUUIDGenerator
      )

      behave like displayEnterUkAddressPage(
        UserType.Individual,
        None,
        performAction
      )

      behave like displayEnterUkAddressPage(UserType.Agent, None, performAction)

      behave like displayEnterUkAddressPage(
        UserType.Organisation,
        None,
        performAction
      )
    }

    "handling submitted addresses from enter UK address page" must {

      def performAction(formData: Seq[(String, String)]): Future[Result] =
        controller.enterUkAddressSubmit()(
          FakeRequest().withFormUrlEncodedBody(formData: _*).withCSRFToken
        )

      behave like redirectToStartBehaviour(() => performAction(Seq.empty))

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.enterUkAddressSubmit(),
        mockUUIDGenerator
      )

      behave like submitEnterUkAddress(
        performAction,
        returnsAddressRoutes.PropertyDetailsController.checkYourAnswers()
      )

    }

    "handling requests to display the enter postcode page" must {

      def performAction(): Future[Result] =
        controller.enterPostcode()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.enterPostcode(),
        mockUUIDGenerator
      )

      behave like enterPostcodePage(UserType.Individual, None, performAction)

      behave like enterPostcodePage(UserType.Agent, None, performAction)

      behave like enterPostcodePage(UserType.Organisation, None, performAction)

    }

    "handling submitted postcodes and filters" must {

      def performAction(formData: Seq[(String, String)]): Future[Result] =
        controller.enterPostcodeSubmit()(
          FakeRequest().withFormUrlEncodedBody(formData: _*).withCSRFToken
        )

      behave like redirectToStartBehaviour(() => performAction(Seq.empty))

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.enterPostcodeSubmit(),
        mockUUIDGenerator
      )

      behave like submitEnterPostcode(
        performAction,
        returnsAddressRoutes.PropertyDetailsController.selectAddress()
      )

    }

    "handling requests to display the select address page" must {

      def performAction(): Future[Result] =
        controller.selectAddress()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.selectAddress(),
        mockUUIDGenerator
      )

      behave like displaySelectAddress(
        UserType.Individual,
        None,
        performAction,
        controllers.returns.address.routes.PropertyDetailsController
          .enterPostcode()
      )

      behave like displaySelectAddress(
        UserType.Agent,
        None,
        performAction,
        controllers.returns.address.routes.PropertyDetailsController
          .enterPostcode()
      )

      behave like displaySelectAddress(
        UserType.Organisation,
        None,
        performAction,
        controllers.returns.address.routes.PropertyDetailsController
          .enterPostcode()
      )

    }

    "handling submitted selected addresses" must {

      def performAction(formData: Seq[(String, String)]): Future[Result] =
        controller.selectAddressSubmit()(
          FakeRequest().withFormUrlEncodedBody(formData: _*).withCSRFToken
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

      behave like redirectToStartBehaviour(performAction)

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.checkYourAnswers(),
        mockUUIDGenerator
      )

      behave like redirectToTaskListWhenNoAssetTypeBehaviour(performAction)

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

      behave like redirectToStartBehaviour(performAction)

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

      behave like redirectToTaskListWhenNoAssetTypeBehaviour(performAction)

      behave like redirectWhenNotNonResidentialAssetTypeBehaviour(performAction)

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
              doc.select("#back").attr("href") shouldBe expectedBackLink.url
              doc
                .select("#content > article > form")
                .attr("action")                shouldBe routes.PropertyDetailsController
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
          FakeRequest().withFormUrlEncodedBody(formData: _*)
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
                .select("#error-summary-display > ul > li > a")
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
            routes.PropertyDetailsController.singleDisposalEnterLandUprn()
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

      behave like redirectToTaskListWhenNoAssetTypeBehaviour(performAction)

      behave like redirectWhenNotNonResidentialAssetTypeBehaviour(performAction)

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
            messageFromMessageKey("enterUPRN.singleDisposal.title"),
            { doc =>
              doc.select("#back").attr("href") shouldBe expectedBackLink.url
              doc
                .select("#content > article > form")
                .attr("action")                shouldBe routes.PropertyDetailsController
                .singleDisposalEnterLandUprnSubmit()
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
            routes.PropertyDetailsController.singleDisposalHasUkPostcode()
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
            routes.PropertyDetailsController.singleDisposalHasUkPostcode()
          )
        }
      }

    }

    "handling submits on the has a enter UPRN page" must {

      def performAction(formData: (String, String)*): Future[Result] =
        controller.singleDisposalEnterLandUprnSubmit()(
          FakeRequest().withFormUrlEncodedBody(formData: _*)
        )

      def formData(ukAddress: UkAddress): List[(String, String)] =
        List(
          "enterUPRN-line1" -> Some(ukAddress.line1),
          "address-line2"   -> ukAddress.line2,
          "address-town"    -> ukAddress.town,
          "address-county"  -> ukAddress.county,
          "postcode"        -> Some(ukAddress.postcode.value)
        ).collect { case (key, Some(value)) => key -> value }

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.singleDisposalEnterLandUprnSubmit(),
        mockUUIDGenerator
      )

      behave like redirectToTaskListWhenNoAssetTypeBehaviour(() => performAction())

      behave like redirectWhenNotNonResidentialAssetTypeBehaviour(() => performAction())

      val nonResidentialPropertyDraftReturn =
        sample[DraftSingleDisposalReturn].copy(
          triageAnswers = sample[CompleteSingleDisposalTriageAnswers].copy(
            assetType = AssetType.NonResidential
          ),
          propertyAddress = None
        )

      val nonResidentialFillingOutReturn = sample[FillingOutReturn].copy(
        draftReturn = nonResidentialPropertyDraftReturn
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
            performAction(formData: _*),
            messageFromMessageKey("enterUPRN.singleDisposal.title"),
            { doc =>
              val errors = doc.select("#error-summary-display > ul").first()
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
            "postcode"        -> "ZZ00ZZ",
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
            "postcode"        -> "ZZ00ZZ",
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
          val newAddress     =
            UkAddress("1", Some("a"), Some("b"), Some("c"), Postcode("ZZ00ZZ"))
          val newDraftReturn = nonResidentialPropertyDraftReturn.copy(
            propertyAddress = Some(newAddress)
          )
          val newJourney     =
            nonResidentialFillingOutReturn.copy(draftReturn = newDraftReturn)

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
            performAction(formData(newAddress): _*),
            routes.PropertyDetailsController.checkYourAnswers()
          )
        }

      }

      "show an error page" when {

        val draftReturn = nonResidentialPropertyDraftReturn
          .copy(propertyAddress = Some(sample[UkAddress]))
        val journey     =
          nonResidentialFillingOutReturn.copy(draftReturn = draftReturn)
        val newAddress  = UkAddress("1", None, None, None, Postcode("ZZ00ZZ"))

        val newDraftReturn = draftReturn.copy(
          propertyAddress = Some(newAddress)
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
            mockStoreDraftReturn(newJourney)(Right(()))
            mockStoreSession(
              SessionData.empty.copy(journeyStatus = Some(newJourney))
            )(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction(formData(newAddress): _*))
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
