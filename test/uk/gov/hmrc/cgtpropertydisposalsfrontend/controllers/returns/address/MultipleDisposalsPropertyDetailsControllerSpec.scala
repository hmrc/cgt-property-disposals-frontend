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
import play.api.i18n.{Lang, MessagesApi}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.{Call, Result}
import play.api.test.CSRFTokenHelper._
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.AddressControllerSpec
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.ReturnsServiceSupport
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.address.{routes => returnsAddressRoutes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.FillingOutReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.{Address, Postcode}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address.{NonUkAddress, UkAddress}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.GGCredId
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.SubscribedDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.MultipleDisposalsExamplePropertyDetailsAnswers.{CompleteMultipleDisposalsExamplePropertyDetailsAnswers, IncompleteMultipleDisposalsExamplePropertyDetailsAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.MultipleDisposalsTriageAnswers.{CompleteMultipleDisposalsTriageAnswers, IncompleteMultipleDisposalsTriageAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.CompleteSingleDisposalTriageAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{AssetType, DraftReturn, MultipleDisposalsDraftReturn, SingleDisposalDraftReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, SessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService

import scala.collection.JavaConverters._
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
              examplePropertyDetailsAnswers = Some(sample[CompleteMultipleDisposalsExamplePropertyDetailsAnswers])
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
                examplePropertyDetailsAnswers = Some(sample[IncompleteMultipleDisposalsExamplePropertyDetailsAnswers])
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
              routes.PropertyDetailsController.nonResidentialPropertyHasUkPostcode()
            )
          }

          "the user has started but not completed this section" in {
            test(
              sample[MultipleDisposalsDraftReturn].copy(
                triageAnswers =
                  sample[CompleteMultipleDisposalsTriageAnswers].copy(assetTypes = List(AssetType.NonResidential)),
                examplePropertyDetailsAnswers = Some(sample[IncompleteMultipleDisposalsExamplePropertyDetailsAnswers])
              ),
              routes.PropertyDetailsController.nonResidentialPropertyHasUkPostcode()
            )
          }

        }
      }
    }

    "handling requests to display the has a uk postcode page" must {

      def performAction(): Future[Result] = controller.nonResidentialPropertyHasUkPostcode()(FakeRequest())

      behave like redirectToTaskListWhenNoAssetTypeBehaviour(performAction)

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

      "display the page" when {

        def test(
          draftReturn: DraftReturn,
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
            messageFromMessageKey("hasValidPostcode.title"), { doc =>
              doc.select("#back").attr("href") shouldBe expectedBackLink.url
              doc
                .select("#content > article > form")
                .attr("action") shouldBe routes.PropertyDetailsController
                .nonResidentialPropertyHasUkPostcodeSubmit()
                .url
            }
          )
        }

        "the user is on a multiple disposal journey and" when {

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
                examplePropertyDetailsAnswers = Some(sample[CompleteMultipleDisposalsExamplePropertyDetailsAnswers])
              ),
              routes.PropertyDetailsController.checkYourAnswers()
            )
          }
        }

        "the user is on a single disposal journey and" when {

          "the user disposed of a non-residential property and the section is not complete" in {
            test(
              sample[SingleDisposalDraftReturn].copy(
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
              sample[SingleDisposalDraftReturn].copy(
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

    }

    "handling submits on the has a uk postcode page" must {

      def performAction(formData: (String, String)*): Future[Result] =
        controller.nonResidentialPropertyHasUkPostcodeSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*))

      behave like redirectToTaskListWhenNoAssetTypeBehaviour(() => performAction())

      val nonResidentialPropertyDraftReturn = sample[MultipleDisposalsDraftReturn].copy(
        triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers].copy(
          assetTypes = List(AssetType.NonResidential)
        )
      )

      val nonResidentialFillingOutReturn = sample[FillingOutReturn].copy(
        draftReturn = nonResidentialPropertyDraftReturn
      )

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
            messageFromMessageKey("hasValidPostcode.title"), { doc =>
              doc.select("#error-summary-display > ul > li > a").text() shouldBe messageFromMessageKey(
                "hasValidPostcode.error.required"
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
            routes.PropertyDetailsController.nonResidentialPropertyInputLandUPRN()
          )
        }

      }

    }

    "handling requests to display the enter UPRN page" must {

      def performAction(): Future[Result] = controller.nonResidentialPropertyInputLandUPRN()(FakeRequest())

      behave like redirectToTaskListWhenNoAssetTypeBehaviour(performAction)

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

      "display the page" when {

        def test(
          draftReturn: DraftReturn,
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
            messageFromMessageKey("enterUPRN.title"), { doc =>
              doc.select("#back").attr("href") shouldBe expectedBackLink.url
              doc
                .select("#content > article > form")
                .attr("action") shouldBe routes.PropertyDetailsController
                .nonResidentialPropertyInputLandUPRNSubmit()
                .url
            }
          )
        }

        "the user is on a multiple disposal journey and" when {

          "the user disposed of a non-residential property and the section is not complete" in {
            test(
              sample[MultipleDisposalsDraftReturn].copy(
                triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers].copy(
                  assetTypes = List(AssetType.NonResidential)
                ),
                examplePropertyDetailsAnswers = None
              ),
              routes.PropertyDetailsController.nonResidentialPropertyHasUkPostcode()
            )
          }

          "the user disposed of a non-residential property and the section is complete" in {
            test(
              sample[MultipleDisposalsDraftReturn].copy(
                triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers].copy(
                  assetTypes = List(AssetType.NonResidential)
                ),
                examplePropertyDetailsAnswers = Some(sample[CompleteMultipleDisposalsExamplePropertyDetailsAnswers])
              ),
              routes.PropertyDetailsController.nonResidentialPropertyHasUkPostcode()
            )
          }
        }

        "the user is on a single disposal journey and" when {

          "the user disposed of a non-residential property and the section is not complete" in {
            test(
              sample[SingleDisposalDraftReturn].copy(
                triageAnswers = sample[CompleteSingleDisposalTriageAnswers].copy(
                  assetType = AssetType.NonResidential
                ),
                propertyAddress = None
              ),
              routes.PropertyDetailsController.nonResidentialPropertyHasUkPostcode()
            )
          }

          "the user disposed of a non-residential property and the section is complete" in {
            test(
              sample[SingleDisposalDraftReturn].copy(
                triageAnswers = sample[CompleteSingleDisposalTriageAnswers].copy(
                  assetType = AssetType.NonResidential
                ),
                propertyAddress = Some(sample[UkAddress])
              ),
              routes.PropertyDetailsController.nonResidentialPropertyHasUkPostcode()
            )
          }
        }
      }

    }

    "handling submits on the has a enter UPRN page" must {

      def performAction(formData: (String, String)*): Future[Result] =
        controller.nonResidentialPropertyInputLandUPRNSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*))

      def formData(ukAddress: UkAddress): List[(String, String)] =
        List(
          "enterUPRN-line1" -> Some(ukAddress.line1),
          "address-line2"   -> ukAddress.line2,
          "address-town"    -> ukAddress.town,
          "address-county"  -> ukAddress.county,
          "postcode"        -> Some(ukAddress.postcode.value)
        ).collect { case (key, Some(value)) => key -> value }

      behave like redirectToTaskListWhenNoAssetTypeBehaviour(() => performAction())

      val nonResidentialPropertyDraftReturn = sample[MultipleDisposalsDraftReturn].copy(
        triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers].copy(
          assetTypes = List(AssetType.NonResidential)
        )
      )

      val nonResidentialFillingOutReturn = sample[FillingOutReturn].copy(
        draftReturn = nonResidentialPropertyDraftReturn
      )

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
            messageFromMessageKey("enterUPRN.title"), { doc =>
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
              IncompleteMultipleDisposalsExamplePropertyDetailsAnswers.empty.copy(
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

        val answers = sample[CompleteMultipleDisposalsExamplePropertyDetailsAnswers]
        val draftReturn = nonResidentialPropertyDraftReturn.copy(
          examplePropertyDetailsAnswers = Some(answers)
        )
        val journey    = nonResidentialFillingOutReturn.copy(draftReturn = draftReturn)
        val newAddress = UkAddress("1", None, None, None, Postcode("ZZ00ZZ"))

        val newDraftReturn = draftReturn.copy(
          examplePropertyDetailsAnswers = Some(
            IncompleteMultipleDisposalsExamplePropertyDetailsAnswers(Some(newAddress))
          )
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

    "handling requests to display the check your answers page" must {

      def performAction(): Future[Result] =
        controller.checkYourAnswers()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      val address = sample[UkAddress]

      val completeAnswers      = CompleteMultipleDisposalsExamplePropertyDetailsAnswers(address)
      val allQuestionsAnswered = IncompleteMultipleDisposalsExamplePropertyDetailsAnswers(Some(completeAnswers.address))
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

          checkIsRedirect(performAction(), routes.PropertyDetailsController.multipleDisposalsGuidance())
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
