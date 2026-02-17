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
import play.api.i18n.{Messages, MessagesApi, MessagesImpl}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.{Call, Result}
import play.api.test.CSRFTokenHelper._
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.AmountOfMoneyErrorScenarios.amountOfMoneyErrorScenarios
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.address.{routes => returnsAddressRoutes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.{ReturnsServiceSupport, StartingToAmendToFillingOutReturnSpecBehaviour}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AddressControllerSpec, DateErrorScenarios}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, StartingToAmendReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.UserType.{Agent, Individual, Organisation}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address.{NonUkAddress, UkAddress}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.{Address, Postcode}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.AmountInPence
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.AddressGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.DraftReturnGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.ExamplePropertyDetailsAnswersGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.ExemptionsAndLossesAnswersGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.IdGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.JourneyStatusGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.MoneyGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.NameGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.RepresenteeAnswersGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.ReturnGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.SubscribedDetailsGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.TaxYearGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.TriageQuestionsGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.YearToDateLiabilityAnswersGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.{AgentReferenceNumber, GGCredId, UUIDGenerator}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.{IndividualName, TrustName}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.SubscribedDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ExamplePropertyDetailsAnswers.{CompleteExamplePropertyDetailsAnswers, IncompleteExamplePropertyDetailsAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ExemptionAndLossesAnswers.CompleteExemptionAndLossesAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.IndividualUserType.{Capacitor, PersonalRepresentative, PersonalRepresentativeInPeriodOfAdmin, Self}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.MultipleDisposalsTriageAnswers.{CompleteMultipleDisposalsTriageAnswers, IncompleteMultipleDisposalsTriageAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.RepresenteeAnswers.CompleteRepresenteeAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.CompleteSingleDisposalTriageAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.YearToDateLiabilityAnswers.NonCalculatedYTDAnswers.CompleteNonCalculatedYTDAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, SessionData, TaxYear, TimeUtils, UserType}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views.address.AddressJourneyType.Returns.FillingOutReturnAddressJourney

import java.time.LocalDate
import scala.concurrent.Future
import scala.jdk.CollectionConverters._

class MultipleDisposalsPropertyDetailsControllerSpec
    extends AddressControllerSpec[FillingOutReturnAddressJourney]
    with SampledScalaCheck
    with RedirectToStartBehaviour
    with ReturnsServiceSupport
    with StartingToAmendToFillingOutReturnSpecBehaviour {

  private val mockUUIDGenerator = mock[UUIDGenerator]

  private val emptyAnswers = IncompleteExamplePropertyDetailsAnswers.empty

  private val incompleteAnswers =
    emptyAnswers.copy(
      address = Some(ukAddress(1)),
      disposalDate = Some(sample[DisposalDate])
    )

  private val draftReturn =
    sample[DraftMultipleDisposalsReturn].copy(
      examplePropertyDetailsAnswers = Some(incompleteAnswers),
      triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
        .copy(individualUserType = Some(Self))
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
    Left(draftReturn),
    Some(Self)
  )

  override def overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[ReturnsService].toInstance(mockReturnsService),
      bind[UUIDGenerator].toInstance(mockUUIDGenerator)
    ) ::: super.overrideBindings

  protected override val controller: PropertyDetailsController = instanceOf[PropertyDetailsController]

  private lazy implicit val messagesApi: MessagesApi = controller.messagesApi

  private implicit val messages: Messages = MessagesImpl(lang, messagesApi)

  private def messageKey(
    userType: UserType,
    individualUserType: Option[IndividualUserType]
  ) =
    (userType, individualUserType) match {
      case (_, Some(Capacitor))                             => ".capacitor"
      case (_, Some(PersonalRepresentative))                => ".personalRep"
      case (_, Some(PersonalRepresentativeInPeriodOfAdmin)) => ".personalRepInPeriodOfAdmin"
      case (Individual, _)                                  => ""
      case (Organisation, _)                                => ".trust"
      case (Agent, _)                                       => ".agent"
      case other                                            => sys.error(s"User type '$other' not handled")
    }

  override def updateAddress(
    journey: FillingOutReturnAddressJourney,
    address: Address
  ): FillingOutReturn = {
    val isFurtherOrAmendReturn = journey.journey.isFurtherOrAmendReturn.contains(true)

    address match {
      case a: UkAddress    =>
        journey.journey.copy(draftReturn =
          draftReturn.copy(
            examplePropertyDetailsAnswers = Some(
              incompleteAnswers.copy(address = Some(a), disposalDate = None)
            ),
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
        val newAnswers =
          incompleteAnswers.copy(address = Some(a), disposalDate = None)

        mockStoreDraftReturn(
          newDetails.journey.copy(draftReturn = draftReturn.copy(examplePropertyDetailsAnswers = Some(newAnswers)))
        )(r)

      case (_, _: NonUkAddress, _) =>
        sys.error("Non UK addresses not handled in this spec")
    }

  private def redirectToStartBehaviour(performAction: () => Future[Result]): Unit =
    redirectToStartWhenInvalidJourney(
      performAction,
      {
        case _: FillingOutReturn      => true
        case _: StartingToAmendReturn => true
        case _                        => false
      }
    )

  private val taxYear = sample[TaxYear].copy(
    startDateInclusive = LocalDate.of(2019, 4, 6),
    endDateExclusive = LocalDate.of(2020, 4, 6)
  )

  private val disposalDate =
    DisposalDate(value = LocalDate.of(2020, 3, 10), taxYear = taxYear)

  private def expectedSubmitText(isAmend: Boolean) =
    messageFromMessageKey(if (isAmend) "button.continue" else "button.saveAndContinue")

  "AddressController" when {
    "handling requests to display the guidance page" must {
      def performAction(): Future[Result] =
        controller.multipleDisposalsGuidance()(FakeRequest())

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.multipleDisposalsGuidance(),
        mockUUIDGenerator
      )

      "redirect to the check your answers page" when {
        "the user is on a single disposal journey" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  sample[FillingOutReturn].copy(
                    draftReturn = sample[DraftSingleDisposalReturn]
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

      "display the page" when {
        def test(
          draftReturn: DraftMultipleDisposalsReturn,
          expectedBackLink: Call,
          userType: UserType
        ): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                userType = Some(userType),
                journeyStatus = Some(
                  sample[FillingOutReturn].copy(
                    subscribedDetails = sample[SubscribedDetails].copy(
                      name = if (userType === Organisation) Left(sample[TrustName]) else Right(sample[IndividualName])
                    ),
                    agentReferenceNumber = if (userType === Agent) Some(sample[AgentReferenceNumber]) else None,
                    draftReturn = draftReturn
                  )
                )
              )
            )
          }

          val individualUserType = draftReturn.triageAnswers
            .fold(_.individualUserType, _.individualUserType)

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(
              "property-details.multiple-disposals.guidance.title"
            ),
            { doc =>
              doc.select("#back, .govuk-back-link").attr("href") shouldBe expectedBackLink.url
              doc
                .select("#content > article form, #main-content form")
                .attr("action")                                  shouldBe routes.PropertyDetailsController
                .multipleDisposalsGuidanceSubmit()
                .url
              doc
                .select("#main-content > div > div > div > p:nth-child(3)")
                .text()                                          shouldBe messageFromMessageKey(
                s"property-details.multiple-disposals.guidance.p0"
              )
              doc
                .select("#main-content > div > div > div > p:nth-child(4)")
                .text()                                          shouldBe messageFromMessageKey(
                s"property-details.multiple-disposals.guidance${messageKey(userType, individualUserType)}.p1"
              )
              doc
                .select("#main-content > div > div > div > p:nth-child(7)")
                .text()                                          shouldBe messageFromMessageKey(
                s"property-details.multiple-disposals.guidance.p3"
              )
            }
          )
        }

        "individual user has not started this section before" in {
          test(
            sample[DraftMultipleDisposalsReturn].copy(
              examplePropertyDetailsAnswers = Some(emptyAnswers.copy(address = Some(sample[UkAddress]))),
              triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                .copy(individualUserType = None)
            ),
            controllers.returns.routes.TaskListController.taskList(),
            Individual
          )
        }

        "trust user has not started this section before" in {
          test(
            sample[DraftMultipleDisposalsReturn].copy(
              examplePropertyDetailsAnswers = None,
              triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                .copy(individualUserType = None)
            ),
            controllers.returns.routes.TaskListController.taskList(),
            Organisation
          )
        }

        "agent user has not started this section before" in {
          test(
            sample[DraftMultipleDisposalsReturn].copy(
              examplePropertyDetailsAnswers = None,
              triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                .copy(individualUserType = None)
            ),
            controllers.returns.routes.TaskListController.taskList(),
            Agent
          )
        }

        "individual user has started but not completed this section" in {
          test(
            sample[DraftMultipleDisposalsReturn].copy(
              examplePropertyDetailsAnswers = Some(sample[IncompleteExamplePropertyDetailsAnswers]),
              triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                .copy(individualUserType = None)
            ),
            controllers.returns.routes.TaskListController.taskList(),
            Individual
          )
        }

        "trust user has started but not completed this section" in {
          test(
            sample[DraftMultipleDisposalsReturn].copy(
              examplePropertyDetailsAnswers = Some(sample[IncompleteExamplePropertyDetailsAnswers]),
              triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                .copy(individualUserType = None)
            ),
            controllers.returns.routes.TaskListController.taskList(),
            Organisation
          )
        }

        "agent user has started but not completed this section" in {
          test(
            sample[DraftMultipleDisposalsReturn].copy(
              examplePropertyDetailsAnswers = Some(sample[IncompleteExamplePropertyDetailsAnswers]),
              triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                .copy(individualUserType = None)
            ),
            controllers.returns.routes.TaskListController.taskList(),
            Agent
          )
        }

        "individual user has completed this section" in {
          test(
            sample[DraftMultipleDisposalsReturn].copy(
              examplePropertyDetailsAnswers = Some(sample[CompleteExamplePropertyDetailsAnswers]),
              triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                .copy(individualUserType = None)
            ),
            routes.PropertyDetailsController.checkYourAnswers(),
            Individual
          )
        }

        "trust user has completed this section" in {
          test(
            sample[DraftMultipleDisposalsReturn].copy(
              examplePropertyDetailsAnswers = Some(sample[CompleteExamplePropertyDetailsAnswers]),
              triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                .copy(individualUserType = None)
            ),
            routes.PropertyDetailsController.checkYourAnswers(),
            Organisation
          )
        }

        "agent user has completed this section" in {
          test(
            sample[DraftMultipleDisposalsReturn].copy(
              examplePropertyDetailsAnswers = Some(sample[CompleteExamplePropertyDetailsAnswers]),
              triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                .copy(individualUserType = None)
            ),
            routes.PropertyDetailsController.checkYourAnswers(),
            Agent
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

      behave like redirectToTaskListWhenNoAssetTypeBehaviour(() => performAction())

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.multipleDisposalsGuidanceSubmit(),
        mockUUIDGenerator
      )

      "redirect to the check your answers page" when {
        "the user is on a single disposal journey" in {
          test(
            sample[DraftSingleDisposalReturn].copy(
              triageAnswers = sample[CompleteSingleDisposalTriageAnswers].copy(
                assetType = AssetType.Residential
              )
            ),
            routes.PropertyDetailsController.checkYourAnswers()
          )
        }

        "the user ins on a multiple disposals journey and has completed this section" in {
          test(
            sample[DraftMultipleDisposalsReturn].copy(
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
          "the user has not started this section before" in {
            forAll { (assetTypes: List[AssetType]) =>
              whenever(
                assetTypes.contains(AssetType.Residential) ||
                  assetTypes.toSet === Set(
                    AssetType.MixedUse,
                    AssetType.NonResidential
                  ) ||
                  assetTypes.toSet === Set(
                    AssetType.IndirectDisposal,
                    AssetType.MixedUse
                  )
              ) {
                test(
                  sample[DraftMultipleDisposalsReturn].copy(
                    triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                      .copy(assetTypes = assetTypes),
                    examplePropertyDetailsAnswers = None
                  ),
                  routes.PropertyDetailsController.enterPostcode()
                )
              }
            }
          }

          "the user has started but not completed this section" in {
            forAll { (assetTypes: List[AssetType]) =>
              whenever(
                assetTypes.contains(AssetType.Residential) ||
                  assetTypes.toSet === Set(
                    AssetType.MixedUse,
                    AssetType.NonResidential
                  ) ||
                  assetTypes.toSet === Set(
                    AssetType.IndirectDisposal,
                    AssetType.MixedUse
                  )
              ) {
                test(
                  sample[DraftMultipleDisposalsReturn].copy(
                    triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                      .copy(assetTypes = assetTypes),
                    examplePropertyDetailsAnswers = Some(sample[IncompleteExamplePropertyDetailsAnswers])
                  ),
                  routes.PropertyDetailsController.enterPostcode()
                )
              }
            }
          }
        }
      }

      "redirect to the enter has a valid postcode page" when {
        "the user disposed of a non-residential property and" when {
          "the user has not started this section before" in {
            test(
              sample[DraftMultipleDisposalsReturn].copy(
                triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                  .copy(assetTypes = List(AssetType.NonResidential)),
                examplePropertyDetailsAnswers = None
              ),
              routes.PropertyDetailsController.singleDisposalHasUkPostcode()
            )
          }

          "the user has started but not completed this section" in {
            test(
              sample[DraftMultipleDisposalsReturn].copy(
                triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                  .copy(assetTypes = List(AssetType.NonResidential, AssetType.IndirectDisposal)),
                examplePropertyDetailsAnswers = Some(sample[IncompleteExamplePropertyDetailsAnswers])
              ),
              routes.PropertyDetailsController.singleDisposalHasUkPostcode()
            )
          }
        }
      }
    }

    "handling requests to display the has a uk postcode page" must {
      def performAction(): Future[Result] =
        controller.multipleDisposalsHasUkPostcode()(FakeRequest())

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.multipleDisposalsHasUkPostcode(),
        mockUUIDGenerator
      )

      behave like redirectToTaskListWhenNoAssetTypeBehaviour(() => performAction())

      behave like redirectWhenShouldNotAskIfPostcodeExistsBehaviour(() => performAction())

      "display the page" when {
        def test(
          draftReturn: DraftMultipleDisposalsReturn,
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
            messageFromMessageKey("hasValidPostcode.multipleDisposals.title"),
            { doc =>
              doc.select("#back, .govuk-back-link").attr("href") shouldBe expectedBackLink.url
              doc
                .select("#main-content form")
                .attr("action")                                  shouldBe routes.PropertyDetailsController
                .multipleDisposalsHasUkPostcodeSubmit()
                .url
            }
          )
        }

        "the user disposed of a non-residential property and the section is not complete" in {
          test(
            sample[DraftMultipleDisposalsReturn].copy(
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
            sample[DraftMultipleDisposalsReturn].copy(
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
        controller.multipleDisposalsHasUkPostcodeSubmit()(
          FakeRequest().withFormUrlEncodedBody(formData*).withMethod("POST")
        )

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.multipleDisposalsHasUkPostcodeSubmit(),
        mockUUIDGenerator
      )

      behave like redirectToTaskListWhenNoAssetTypeBehaviour(() => performAction())

      behave like redirectWhenShouldNotAskIfPostcodeExistsBehaviour(() => performAction())

      val nonResidentialPropertyDraftReturn =
        sample[DraftMultipleDisposalsReturn].copy(
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
            messageFromMessageKey("hasValidPostcode.multipleDisposals.title"),
            doc =>
              doc
                .select("[data-spec='errorSummaryDisplay'] a")
                .text() shouldBe messageFromMessageKey(
                "hasValidPostcode.multipleDisposals.error.required"
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
            routes.PropertyDetailsController.checkUPRN()
          )
        }
      }
    }

    "handling requests to check if user has UPRN page" must {

      val nonResidentialPropertyDraftReturn =
        sample[DraftMultipleDisposalsReturn].copy(
          triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers].copy(
            assetTypes = List(AssetType.NonResidential)
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

      behave like redirectWhenShouldNotAskIfPostcodeExistsBehaviour(() => performAction())

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
            routes.PropertyDetailsController.multipleDisposalsEnterLandUprn()
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
        controller.multipleDisposalsEnterLandUprn()(FakeRequest())

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.multipleDisposalsEnterLandUprn(),
        mockUUIDGenerator
      )

      behave like redirectToTaskListWhenNoAssetTypeBehaviour(() => performAction())

      behave like redirectWhenShouldNotAskIfPostcodeExistsBehaviour(() => performAction())

      "display the page" when {
        def test(
          draftReturn: DraftMultipleDisposalsReturn,
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
              doc.select(".govuk-back-link").attr("href") shouldBe expectedBackLink.url
              doc
                .select("#main-content form")
                .attr("action")                           shouldBe routes.PropertyDetailsController
                .multipleDisposalsEnterLandUprnSubmit()
                .url
            }
          )
        }

        "the user disposed of a non-residential property and the section is not complete" in {
          test(
            sample[DraftMultipleDisposalsReturn].copy(
              triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers].copy(
                assetTypes = List(AssetType.NonResidential)
              ),
              examplePropertyDetailsAnswers = None
            ),
            routes.PropertyDetailsController.checkUPRN()
          )
        }

        "the user disposed of a non-residential property and the section is complete" in {
          test(
            sample[DraftMultipleDisposalsReturn].copy(
              triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers].copy(
                assetTypes = List(AssetType.NonResidential)
              ),
              examplePropertyDetailsAnswers = Some(sample[CompleteExamplePropertyDetailsAnswers])
            ),
            routes.PropertyDetailsController.checkUPRN()
          )
        }
      }
    }

    "handling submits on the enter UPRN page" must {
      def performAction(formData: (String, String)*): Future[Result] =
        controller.multipleDisposalsEnterLandUprnSubmit()(
          FakeRequest().withFormUrlEncodedBody(formData*).withMethod("POST")
        )

      def formData(ukAddress: UkAddress): List[(String, String)] =
        List(
          "enterUPRN-line1" -> Some(ukAddress.line1),
          "postcode"        -> Some(ukAddress.postcode.value)
        ).collect { case (key, Some(value)) => key -> value }

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.multipleDisposalsEnterLandUprnSubmit(),
        mockUUIDGenerator
      )

      behave like redirectToTaskListWhenNoAssetTypeBehaviour(() => performAction())

      behave like redirectWhenShouldNotAskIfPostcodeExistsBehaviour(() => performAction())

      val nonResidentialPropertyDraftReturn =
        sample[DraftMultipleDisposalsReturn].copy(
          triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers].copy(
            assetTypes = List(AssetType.NonResidential),
            individualUserType = Some(Self)
          ),
          representeeAnswers = None,
          examplePropertyDetailsAnswers = Some(emptyAnswers)
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

      "redirect to the check your emptyAnswers page" when {
        "the address submitted is valid and all updates are successful" in {
          val newAddress     = UkAddress("1", None, None, None, Postcode("ZZ00ZZ"))
          val newDraftReturn = nonResidentialPropertyDraftReturn.copy(
            examplePropertyDetailsAnswers = Some(
              emptyAnswers.copy(
                address = Some(newAddress)
              )
            )
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
            performAction(formData(newAddress)*),
            routes.PropertyDetailsController.checkYourAnswers()
          )
        }
      }

      "show an error page" when {
        val answers     = sample[CompleteExamplePropertyDetailsAnswers]
        val draftReturn = nonResidentialPropertyDraftReturn.copy(
          examplePropertyDetailsAnswers = Some(answers)
        )
        val journey     =
          nonResidentialFillingOutReturn
            .copy(draftReturn = draftReturn, amendReturnData = Some(sample[AmendReturnData]))
        val newAddress  = UkAddress("1", None, None, None, Postcode("ZZ00ZZ"))

        val newDraftReturn = draftReturn.copy(
          examplePropertyDetailsAnswers = Some(
            IncompleteExamplePropertyDetailsAnswers(
              Some(newAddress),
              None,
              Some(answers.disposalPrice),
              Some(answers.acquisitionPrice)
            )
          ),
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

    "handling requests to display the enter UK address page" must {
      def performAction(): Future[Result] =
        controller.enterUkAddress()(FakeRequest())

      behave like redirectToStartBehaviour(() => performAction())

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.enterUkAddress(),
        mockUUIDGenerator
      )

      behave like displayEnterUkAddressPage(UserType.Individual, None, () => performAction())
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

      List(Some(Capacitor), Some(PersonalRepresentative), None).foreach { individualUserType =>
        behave like enterPostcodePage(UserType.Individual, individualUserType, () => performAction())
        behave like enterPostcodePage(UserType.Agent, individualUserType, () => performAction())
        behave like enterPostcodePage(UserType.Organisation, individualUserType, () => performAction())
      }
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

      List(Some(Capacitor), Some(PersonalRepresentative), None).foreach { individualUserType =>
        behave like displaySelectAddress(
          UserType.Individual,
          individualUserType,
          () => performAction(),
          controllers.returns.address.routes.PropertyDetailsController
            .enterPostcode()
        )

        behave like displaySelectAddress(
          UserType.Agent,
          individualUserType,
          () => performAction(),
          controllers.returns.address.routes.PropertyDetailsController
            .enterPostcode()
        )

        behave like displaySelectAddress(
          UserType.Organisation,
          individualUserType,
          () => performAction(),
          controllers.returns.address.routes.PropertyDetailsController
            .enterPostcode()
        )
      }
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

    "handling requests to display the disposal date page" must {
      def performAction(): Future[Result] =
        controller.disposalDate()(FakeRequest())

      behave like redirectToStartBehaviour(() => performAction())

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.disposalDate(),
        mockUUIDGenerator
      )

      behave like noDateOfDeathForPersonalRepBehaviour(() => performAction())

      "redirect to the check your answers page" when {
        "the user is on a single disposal journey" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  sample[FillingOutReturn].copy(
                    draftReturn = sample[DraftSingleDisposalReturn]
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

      "redirect to the technical error page" when {
        "no address can be found" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  sample[FillingOutReturn].copy(
                    draftReturn = sample[DraftMultipleDisposalsReturn].copy(
                      triageAnswers = sample[IncompleteMultipleDisposalsTriageAnswers].copy(
                        taxYear = Some(sample[TaxYear]),
                        completionDate = Some(sample[CompletionDate])
                      ),
                      examplePropertyDetailsAnswers =
                        Some(sample[IncompleteExamplePropertyDetailsAnswers].copy(address = None))
                    )
                  )
                )
              )
            )
          }

          checkIsTechnicalErrorPage(
            performAction()
          )
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
                    draftReturn = sample[DraftMultipleDisposalsReturn].copy(
                      triageAnswers = sample[IncompleteMultipleDisposalsTriageAnswers]
                        .copy(
                          completionDate = Some(sample[CompletionDate]),
                          taxYear = None
                        )
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

        "no completion date can be found" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  sample[FillingOutReturn].copy(
                    draftReturn = sample[DraftMultipleDisposalsReturn].copy(
                      triageAnswers = sample[IncompleteMultipleDisposalsTriageAnswers]
                        .copy(
                          taxYear = Some(sample[TaxYear]),
                          completionDate = None
                        )
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

      "display the page" when {
        val triageAnswersWithTaxYear = sample[CompleteMultipleDisposalsTriageAnswers].copy(
          taxYear = taxYear,
          individualUserType = None
        )

        def test(
          draftReturn: DraftMultipleDisposalsReturn,
          userType: UserType
        ): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                userType = Some(userType),
                journeyStatus = Some(
                  sample[FillingOutReturn].copy(
                    draftReturn = draftReturn,
                    subscribedDetails = sample[SubscribedDetails].copy(
                      name = if (userType === Organisation) Left(sample[TrustName]) else Right(sample[IndividualName])
                    ),
                    agentReferenceNumber = if (userType === Agent) Some(sample[AgentReferenceNumber]) else None
                  )
                )
              )
            )
          }

          val individualUserType = draftReturn.triageAnswers
            .fold(_.individualUserType, _.individualUserType)

          val addressLine1 = draftReturn.examplePropertyDetailsAnswers
            .getOrElse(fail("Cannot find address"))
            .fold(
              i =>
                i.address match {
                  case Some(address) => address.line1
                  case None          => fail("Cannot find address")
                },
              c => c.address.line1
            )

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(
              s"multipleDisposalsDisposalDate${messageKey(userType, individualUserType)}.title"
            ),
            doc => {
              doc
                .select("#multipleDisposalsDisposalDate-hint")
                .text()                                shouldBe messageFromMessageKey(
                s"multipleDisposalsDisposalDate${messageKey(userType, individualUserType)}.helpText"
              )
              doc.select("span.govuk-caption-xl").text shouldBe addressLine1
            }
          )
        }

        "individual user has not started this section before" in {
          test(
            sample[DraftMultipleDisposalsReturn].copy(
              triageAnswers = triageAnswersWithTaxYear,
              examplePropertyDetailsAnswers = Some(
                emptyAnswers.copy(
                  address = Some(sample[UkAddress])
                )
              )
            ),
            Individual
          )
        }

        "trust user has not started this section before" in {
          test(
            sample[DraftMultipleDisposalsReturn].copy(
              triageAnswers = triageAnswersWithTaxYear,
              examplePropertyDetailsAnswers = Some(
                emptyAnswers.copy(
                  address = Some(sample[UkAddress])
                )
              )
            ),
            Organisation
          )
        }

        "agent user has not started this section before" in {
          test(
            sample[DraftMultipleDisposalsReturn].copy(
              triageAnswers = triageAnswersWithTaxYear,
              examplePropertyDetailsAnswers = Some(
                emptyAnswers.copy(
                  address = Some(sample[UkAddress])
                )
              )
            ),
            Agent
          )
        }

        "individual user has started but not completed this section" in {
          test(
            sample[DraftMultipleDisposalsReturn].copy(
              triageAnswers = triageAnswersWithTaxYear,
              examplePropertyDetailsAnswers = Some(
                sample[IncompleteExamplePropertyDetailsAnswers].copy(
                  disposalDate = None,
                  address = Some(sample[UkAddress])
                )
              )
            ),
            Individual
          )
        }

        "trust user has started but not completed this section" in {
          test(
            sample[DraftMultipleDisposalsReturn].copy(
              triageAnswers = triageAnswersWithTaxYear,
              examplePropertyDetailsAnswers = Some(
                sample[IncompleteExamplePropertyDetailsAnswers].copy(
                  disposalDate = None,
                  address = Some(sample[UkAddress])
                )
              )
            ),
            Organisation
          )
        }

        "agent user has started but not completed this section" in {
          test(
            sample[DraftMultipleDisposalsReturn].copy(
              triageAnswers = triageAnswersWithTaxYear,
              examplePropertyDetailsAnswers = Some(
                sample[IncompleteExamplePropertyDetailsAnswers].copy(
                  disposalDate = None,
                  address = Some(sample[UkAddress])
                )
              )
            ),
            Agent
          )
        }

        "individual user has completed this section" in {
          test(
            sample[DraftMultipleDisposalsReturn].copy(
              triageAnswers = triageAnswersWithTaxYear,
              examplePropertyDetailsAnswers = Some(
                sample[CompleteExamplePropertyDetailsAnswers].copy(
                  disposalDate = disposalDate
                )
              )
            ),
            Individual
          )
        }

        "trust user has completed this section" in {
          test(
            sample[DraftMultipleDisposalsReturn].copy(
              triageAnswers = triageAnswersWithTaxYear,
              examplePropertyDetailsAnswers = Some(
                sample[CompleteExamplePropertyDetailsAnswers].copy(
                  disposalDate = disposalDate
                )
              )
            ),
            Organisation
          )
        }

        "agent user has completed this section" in {
          test(
            sample[DraftMultipleDisposalsReturn].copy(
              triageAnswers = triageAnswersWithTaxYear,
              examplePropertyDetailsAnswers = Some(
                sample[CompleteExamplePropertyDetailsAnswers].copy(
                  disposalDate = disposalDate
                )
              )
            ),
            Agent
          )
        }
      }
    }

    "handling submitted answers to the disposal date page" must {
      val key = "multipleDisposalsDisposalDate"

      def performAction(formData: (String, String)*): Future[Result] =
        controller.disposalDateSubmit()(
          FakeRequest().withFormUrlEncodedBody(formData*).withCSRFToken.withMethod("POST")
        )

      def formData(d: LocalDate): List[(String, String)] =
        List(
          "multipleDisposalsDisposalDate-day"   -> d.getDayOfMonth.toString,
          "multipleDisposalsDisposalDate-month" -> d.getMonthValue.toString,
          "multipleDisposalsDisposalDate-year"  -> d.getYear.toString
        )

      val today = TimeUtils.today()

      behave like redirectToStartBehaviour(() => performAction())

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.disposalDateSubmit(),
        mockUUIDGenerator
      )

      behave like noDateOfDeathForPersonalRepBehaviour(() => performAction())

      "redirect to the task list page" when {
        "no tax year can be found" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  sample[FillingOutReturn].copy(
                    draftReturn = sample[DraftMultipleDisposalsReturn].copy(
                      triageAnswers = sample[IncompleteMultipleDisposalsTriageAnswers]
                        .copy(taxYear = None)
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

        "no completion date can be found" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  sample[FillingOutReturn].copy(
                    draftReturn = sample[DraftMultipleDisposalsReturn].copy(
                      triageAnswers = sample[IncompleteMultipleDisposalsTriageAnswers]
                        .copy(completionDate = None)
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

      "not update the session" when {
        "the date submitted is the same as one that already exists in session" in {
          val answers = sample[IncompleteExamplePropertyDetailsAnswers].copy(
            address = Some(sample[UkAddress]),
            disposalDate = Some(disposalDate)
          )

          val draftReturn = sample[DraftMultipleDisposalsReturn].copy(
            triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
              .copy(
                taxYear = taxYear,
                completionDate = CompletionDate(taxYear.endDateExclusive),
                individualUserType = Some(Self)
              ),
            examplePropertyDetailsAnswers = Some(answers)
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  sample[FillingOutReturn].copy(
                    draftReturn = draftReturn,
                    subscribedDetails = sample[SubscribedDetails].copy(
                      name = Right(sample[IndividualName])
                    )
                  )
                )
              )
            )
          }

          checkIsRedirect(
            performAction(formData(disposalDate.value)*),
            routes.PropertyDetailsController.checkYourAnswers()
          )
        }
      }

      "show a form error" when {
        val completionDate = CompletionDate(
          taxYear.endDateExclusive.minusDays(10L)
        )

        def testFormError(
          individualUserType: Option[IndividualUserType] = Some(Self),
          representeeAnswers: Option[RepresenteeAnswers] = None
        )(
          formData: List[(String, String)]
        )(
          expectedErrorMessageKey: String,
          args: Seq[String] = Seq(),
          messageRegexPrefix: String = "not_found_message"
        ): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithValidJourneyStatus.copy(
                journeyStatus = Some(
                  sample[FillingOutReturn].copy(
                    draftReturn = sample[DraftMultipleDisposalsReturn].copy(
                      triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers].copy(
                        taxYear = taxYear,
                        completionDate = completionDate,
                        individualUserType = individualUserType
                      ),
                      representeeAnswers = representeeAnswers,
                      examplePropertyDetailsAnswers = Some(
                        emptyAnswers.copy(
                          address = Some(sample[UkAddress])
                        )
                      )
                    ),
                    subscribedDetails = sample[SubscribedDetails].copy(
                      name = Right(sample[IndividualName])
                    )
                  )
                )
              )
            )
          }

          checkPageIsDisplayed(
            performAction(formData*),
            messageFromMessageKey(s"$key.title"),
            doc =>
              doc
                .select("[data-spec='errorSummaryDisplay'] a")
                .text() shouldBe messageFromMessageKey(
                expectedErrorMessageKey,
                args*
              ),
            BAD_REQUEST,
            messageRegexPrefix
          )
        }

        "the date entered is invalid" in {
          DateErrorScenarios
            .dateErrorScenarios(key, "")
            .foreach { scenario =>
              withClue(s"For date error scenario $scenario: ") {
                val data = List(
                  s"$key-day"   -> scenario.dayInput,
                  s"$key-month" -> scenario.monthInput,
                  s"$key-year"  -> scenario.yearInput
                ).collect { case (key, Some(value)) => key -> value }

                testFormError()(data)(scenario.expectedErrorMessageKey)
              }
            }
        }

        "the date entered is too far in future" in {
          val param1 = TimeUtils.govDisplayFormat(completionDate.value)
          val param2 = taxYear.startDateInclusive.getYear.toString
          val param3 = taxYear.endDateExclusive.getYear.toString
          testFormError()(formData(today.plusYears(2L)))(
            s"$key.error.tooFarInFuture",
            Seq(param1, param2, param3)
          )
        }

        "the date entered is too far in past" in {
          val param1 = taxYear.startDateInclusive.getYear.toString
          val param2 = taxYear.endDateExclusive.getYear.toString
          testFormError()(formData(taxYear.startDateInclusive.minusYears(2L)))(
            s"$key.error.tooFarInPast",
            Seq(param1, param2),
            "ignore_not_found_message"
          )
        }

        "the disposal date is strictly after the date of death and the user is a non-period of admin personal rep" in {
          testFormError(
            Some(PersonalRepresentative),
            Some(
              sample[CompleteRepresenteeAnswers]
                .copy(dateOfDeath = Some(DateOfDeath(completionDate.value.minusDays(2L))))
            )
          )(formData(completionDate.value.minusDays(1L)))(
            s"$key.error.nonPeriodOfAdminDeathAfterDate"
          )
        }

        "the disposal date is before the date of death and the user is a period of admin personal rep" in {
          testFormError(
            Some(PersonalRepresentativeInPeriodOfAdmin),
            Some(sample[CompleteRepresenteeAnswers].copy(dateOfDeath = Some(DateOfDeath(completionDate.value))))
          )(formData(completionDate.value.minusDays(1L)))(
            s"$key.error.periodOfAdminDeathNotAfterDate"
          )
        }

        "the disposal date is strictly before the date of death and the user is a period of admin personal rep" in {
          testFormError(
            Some(PersonalRepresentativeInPeriodOfAdmin),
            Some(
              sample[CompleteRepresenteeAnswers]
                .copy(dateOfDeath = Some(DateOfDeath(completionDate.value.minusDays(1L))))
            )
          )(formData(completionDate.value.minusDays(2L)))(
            s"$key.error.periodOfAdminDeathNotAfterDate"
          )
        }
      }

      "redirect to the check your answers page" when {
        def test(
          result: => Future[Result],
          oldDraftReturn: DraftReturn,
          updatedDraftReturn: DraftReturn,
          isAmend: Boolean
        ): Unit = {
          val amendReturnData = sample[AmendReturnData]
          val journey         =
            sample[FillingOutReturn].copy(
              draftReturn = oldDraftReturn,
              amendReturnData =
                if (isAmend) Some(amendReturnData.copy(shouldDisplayGainOrLossAfterReliefs = false)) else None,
              previousSentReturns = None
            )
          val session         = SessionData.empty.copy(journeyStatus = Some(journey))
          val updatedJourney  = journey.copy(
            draftReturn = updatedDraftReturn,
            amendReturnData =
              if (isAmend) Some(amendReturnData.copy(shouldDisplayGainOrLossAfterReliefs = true)) else None
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(updatedJourney)(Right(()))
            mockStoreSession(session.copy(journeyStatus = Some(updatedJourney)))(Right(()))
          }

          checkIsRedirect(
            result,
            routes.PropertyDetailsController.checkYourAnswers()
          )
        }

        "the user has not started a draft return and" when {
          "the user has not answered the question before" in {
            val answers = sample[IncompleteExamplePropertyDetailsAnswers].copy(
              disposalDate = Some(disposalDate),
              address = Some(sample[UkAddress])
            )

            val oldDraftReturn = sample[DraftMultipleDisposalsReturn].copy(
              triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                .copy(
                  taxYear = taxYear,
                  completionDate = CompletionDate(taxYear.endDateExclusive),
                  individualUserType = Some(Self)
                ),
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
              performAction(formData(updatedDisposalDate.value)*),
              oldDraftReturn,
              updatedDraftReturn,
              isAmend = false
            )
          }

          "the user has already answered the question" in {
            val answers = sample[CompleteExamplePropertyDetailsAnswers].copy(
              disposalDate = disposalDate
            )

            val oldDraftReturn = sample[DraftMultipleDisposalsReturn].copy(
              triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                .copy(
                  taxYear = taxYear,
                  completionDate = CompletionDate(taxYear.endDateExclusive),
                  individualUserType = Some(Self)
                ),
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
              performAction(formData(updatedDisposalDate.value)*),
              oldDraftReturn,
              updatedDraftReturn,
              isAmend = false
            )
          }
        }

        "the user has started a draft return and" when {
          "have completed the section and they enter a figure which is " +
            "different than one they have already entered" in {
              val answers = sample[CompleteExamplePropertyDetailsAnswers].copy(
                disposalDate = disposalDate.copy(
                  value = today.minusDays(10L)
                )
              )

              val oldDraftReturn     = sample[DraftMultipleDisposalsReturn].copy(
                triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                  .copy(
                    taxYear = taxYear,
                    completionDate = CompletionDate(taxYear.endDateExclusive),
                    individualUserType = Some(Self)
                  ),
                examplePropertyDetailsAnswers = Some(answers),
                exemptionAndLossesAnswers = Some(sample[CompleteExemptionAndLossesAnswers]),
                yearToDateLiabilityAnswers = Some(sample[CompleteNonCalculatedYTDAnswers]),
                gainOrLossAfterReliefs = Some(sample[AmountInPence])
              )
              val updatedDraftReturn = oldDraftReturn.copy(
                examplePropertyDetailsAnswers = Some(
                  answers.copy(
                    disposalDate = disposalDate
                  )
                ),
                exemptionAndLossesAnswers = None,
                yearToDateLiabilityAnswers = None,
                gainOrLossAfterReliefs = None
              )

              test(
                performAction(formData(disposalDate.value)*),
                oldDraftReturn,
                updatedDraftReturn,
                isAmend = true
              )
            }
        }
      }
    }

    "handling requests to display the disposal price page" must {
      def performAction(): Future[Result] =
        controller.disposalPrice()(FakeRequest())

      behave like redirectToStartBehaviour(() => performAction())

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.disposalPrice(),
        mockUUIDGenerator
      )

      "redirect to the technical error page" when {
        "no address can be found" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  sample[FillingOutReturn].copy(
                    draftReturn = sample[DraftMultipleDisposalsReturn].copy(
                      triageAnswers = sample[IncompleteMultipleDisposalsTriageAnswers].copy(
                        taxYear = Some(sample[TaxYear]),
                        completionDate = Some(sample[CompletionDate])
                      ),
                      examplePropertyDetailsAnswers =
                        Some(sample[IncompleteExamplePropertyDetailsAnswers].copy(address = None))
                    )
                  )
                )
              )
            )
          }

          checkIsTechnicalErrorPage(
            performAction()
          )
        }
      }

      "redirect to the check your answers page" when {
        "the user is on a single disposal journey" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  sample[FillingOutReturn].copy(
                    draftReturn = sample[DraftSingleDisposalReturn],
                    subscribedDetails = sample[SubscribedDetails].copy(
                      name = Right(sample[IndividualName])
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

      "display the page" when {
        def test(
          draftReturn: DraftMultipleDisposalsReturn,
          expectedBackLink: Call,
          userType: UserType
        ): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                userType = Some(userType),
                journeyStatus = Some(
                  sample[FillingOutReturn].copy(
                    draftReturn = draftReturn,
                    subscribedDetails = sample[SubscribedDetails].copy(
                      name = if (userType === Organisation) Left(sample[TrustName]) else Right(sample[IndividualName])
                    ),
                    agentReferenceNumber = if (userType === Agent) Some(sample[AgentReferenceNumber]) else None
                  )
                )
              )
            )
          }

          val individualUserType = draftReturn.triageAnswers
            .fold(_.individualUserType, _.individualUserType)

          val addressLine1 = draftReturn.examplePropertyDetailsAnswers
            .getOrElse(fail("Cannot find address"))
            .fold(
              i =>
                i.address match {
                  case Some(address) => address.line1
                  case None          => fail("Cannot find address")
                },
              c => c.address.line1
            )

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(s"multipleDisposalsDisposalPrice.title"),
            { doc =>
              doc.select("#back, .govuk-back-link").attr("href") shouldBe expectedBackLink.url
              doc
                .select("#main-content form")
                .attr("action")                                  shouldBe routes.PropertyDetailsController
                .disposalPriceSubmit()
                .url
              doc
                .select("#multipleDisposalsDisposalPrice-hint")
                .text()                                          shouldBe messageFromMessageKey(
                s"multipleDisposalsDisposalPrice${messageKey(userType, individualUserType)}.helpText"
              )
              doc.select("span.govuk-caption-xl").text           shouldBe addressLine1
            }
          )
        }

        "individual user has not started this section before" in {
          test(
            sample[DraftMultipleDisposalsReturn].copy(
              examplePropertyDetailsAnswers = Some(emptyAnswers.copy(address = Some(sample[UkAddress]))),
              triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                .copy(individualUserType = None)
            ),
            routes.PropertyDetailsController.disposalDate(),
            Individual
          )
        }

        "trust user has not started this section before" in {
          test(
            sample[DraftMultipleDisposalsReturn].copy(
              examplePropertyDetailsAnswers = Some(emptyAnswers.copy(address = Some(sample[UkAddress]))),
              triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                .copy(individualUserType = None)
            ),
            routes.PropertyDetailsController.disposalDate(),
            Organisation
          )
        }

        "agent user has not started this section before" in {
          test(
            sample[DraftMultipleDisposalsReturn].copy(
              examplePropertyDetailsAnswers = Some(emptyAnswers.copy(address = Some(sample[UkAddress]))),
              triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                .copy(individualUserType = None)
            ),
            routes.PropertyDetailsController.disposalDate(),
            Agent
          )
        }

        "individual user has started but not completed this section" in {
          test(
            sample[DraftMultipleDisposalsReturn].copy(
              examplePropertyDetailsAnswers = Some(
                sample[IncompleteExamplePropertyDetailsAnswers].copy(
                  disposalPrice = None,
                  address = Some(sample[UkAddress])
                )
              ),
              triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                .copy(individualUserType = None)
            ),
            routes.PropertyDetailsController.disposalDate(),
            Individual
          )
        }

        "trust user has started but not completed this section" in {
          test(
            sample[DraftMultipleDisposalsReturn].copy(
              examplePropertyDetailsAnswers = Some(
                sample[IncompleteExamplePropertyDetailsAnswers].copy(
                  disposalPrice = None,
                  address = Some(sample[UkAddress])
                )
              ),
              triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                .copy(individualUserType = None)
            ),
            routes.PropertyDetailsController.disposalDate(),
            Organisation
          )
        }

        "agent user has started but not completed this section" in {
          test(
            sample[DraftMultipleDisposalsReturn].copy(
              examplePropertyDetailsAnswers = Some(
                sample[IncompleteExamplePropertyDetailsAnswers].copy(
                  disposalPrice = None,
                  address = Some(sample[UkAddress])
                )
              ),
              triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                .copy(individualUserType = None)
            ),
            routes.PropertyDetailsController.disposalDate(),
            Agent
          )
        }

        "individual user has completed this section" in {
          test(
            sample[DraftMultipleDisposalsReturn].copy(
              examplePropertyDetailsAnswers = Some(
                sample[CompleteExamplePropertyDetailsAnswers].copy(
                  disposalPrice = sample[AmountInPence]
                )
              ),
              triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                .copy(individualUserType = None)
            ),
            routes.PropertyDetailsController.checkYourAnswers(),
            Individual
          )
        }

        "trust user has completed this section" in {
          test(
            sample[DraftMultipleDisposalsReturn].copy(
              examplePropertyDetailsAnswers = Some(
                sample[CompleteExamplePropertyDetailsAnswers].copy(
                  disposalPrice = sample[AmountInPence]
                )
              ),
              triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                .copy(individualUserType = None)
            ),
            routes.PropertyDetailsController.checkYourAnswers(),
            Organisation
          )
        }

        "agent user has completed this section" in {
          test(
            sample[DraftMultipleDisposalsReturn].copy(
              examplePropertyDetailsAnswers = Some(
                sample[CompleteExamplePropertyDetailsAnswers].copy(
                  disposalPrice = sample[AmountInPence]
                )
              ),
              triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                .copy(individualUserType = None)
            ),
            routes.PropertyDetailsController.checkYourAnswers(),
            Agent
          )
        }
      }
    }

    "handling submitted answers to the disposal price page" must {
      val key = "multipleDisposalsDisposalPrice"

      def performAction(formData: (String, String)*): Future[Result] =
        controller.disposalPriceSubmit()(
          FakeRequest().withFormUrlEncodedBody(formData*).withMethod("POST")
        )

      behave like redirectToStartBehaviour(() => performAction())

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.disposalPriceSubmit(),
        mockUUIDGenerator
      )

      "not update the session" when {
        "the data submitted is the same as one that already exists in session" in {
          val disposalPrice = AmountInPence.fromPounds(1000)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  sample[FillingOutReturn].copy(
                    draftReturn = sample[DraftMultipleDisposalsReturn].copy(
                      examplePropertyDetailsAnswers = Some(
                        sample[IncompleteExamplePropertyDetailsAnswers].copy(
                          disposalPrice = Some(disposalPrice),
                          address = Some(sample[UkAddress])
                        )
                      )
                    ),
                    subscribedDetails = sample[SubscribedDetails].copy(
                      name = Right(sample[IndividualName])
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
        def test(data: (String, String)*)(expectedErrorMessageKey: String): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  sample[FillingOutReturn].copy(
                    draftReturn = sample[DraftMultipleDisposalsReturn].copy(
                      examplePropertyDetailsAnswers = Some(
                        sample[CompleteExamplePropertyDetailsAnswers]
                      ),
                      triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                        .copy(individualUserType = None)
                    ),
                    subscribedDetails = sample[SubscribedDetails].copy(
                      name = Right(sample[IndividualName])
                    )
                  )
                )
              )
            )
          }

          checkPageIsDisplayed(
            performAction(data*),
            messageFromMessageKey(s"$key.title"),
            doc =>
              doc
                .select("[data-spec='errorSummaryDisplay'] a")
                .text() shouldBe messageFromMessageKey(
                expectedErrorMessageKey
              ),
            BAD_REQUEST
          )
        }

        "the data is invalid" in {
          amountOfMoneyErrorScenarios(key).foreach { scenario =>
            withClue(s"For $scenario: ") {
              val data = scenario.formData
              test(data*)(scenario.expectedErrorMessageKey)
            }
          }
        }
      }

      "redirect to the cya page" when {
        def test(
          result: => Future[Result],
          oldDraftReturn: DraftReturn,
          updatedDraftReturn: DraftReturn,
          isAmend: Boolean
        ): Unit = {
          val amendReturnData = sample[AmendReturnData]

          val journey        =
            sample[FillingOutReturn].copy(
              draftReturn = oldDraftReturn,
              amendReturnData =
                if (isAmend) Some(amendReturnData.copy(shouldDisplayGainOrLossAfterReliefs = false)) else None
            )
          val session        = SessionData.empty.copy(journeyStatus = Some(journey))
          val updatedJourney = journey.copy(
            draftReturn = updatedDraftReturn,
            amendReturnData =
              if (isAmend) Some(amendReturnData.copy(shouldDisplayGainOrLossAfterReliefs = true)) else None
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(updatedJourney)(Right(()))
            mockStoreSession(session.copy(journeyStatus = Some(updatedJourney)))(Right(()))
          }

          checkIsRedirect(
            result,
            routes.PropertyDetailsController.checkYourAnswers()
          )
        }

        "the user is on a multiple disposals journey and has completed this section" in {
          val answers = sample[CompleteExamplePropertyDetailsAnswers].copy(
            disposalPrice = AmountInPence.fromPounds(1),
            disposalDate = disposalDate
          )

          val oldDraftReturn = sample[DraftMultipleDisposalsReturn].copy(
            examplePropertyDetailsAnswers = Some(answers)
          )

          val updatedDraftReturn = oldDraftReturn.copy(
            examplePropertyDetailsAnswers = Some(
              answers.copy(
                disposalPrice = AmountInPence.fromPounds(10)
              )
            ),
            yearToDateLiabilityAnswers = None,
            gainOrLossAfterReliefs = None
          )

          test(
            performAction(key -> "10"),
            oldDraftReturn,
            updatedDraftReturn,
            isAmend = false
          )
        }

        "the user hasn't ever answered the disposal price question " +
          "and the draft return and session data has been successfully updated" in {
            val answers = sample[IncompleteExamplePropertyDetailsAnswers].copy(
              address = Some(sample[UkAddress]),
              disposalDate = Some(disposalDate),
              disposalPrice = Some(AmountInPence.fromPounds(1))
            )

            val oldDraftReturn = sample[DraftMultipleDisposalsReturn].copy(
              examplePropertyDetailsAnswers = Some(answers)
            )

            val updatedDraftReturn = oldDraftReturn.copy(
              examplePropertyDetailsAnswers = Some(
                answers.copy(
                  disposalPrice = Some(AmountInPence.fromPounds(10))
                )
              ),
              yearToDateLiabilityAnswers = None,
              gainOrLossAfterReliefs = None
            )

            test(
              performAction(key -> "10"),
              oldDraftReturn,
              updatedDraftReturn,
              isAmend = false
            )
          }
      }
    }

    "handling requests to display the acquisition price page" must {
      def performAction(): Future[Result] =
        controller.acquisitionPrice()(FakeRequest())

      behave like redirectToStartBehaviour(() => performAction())

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.acquisitionPrice(),
        mockUUIDGenerator
      )

      "redirect to the technical error page" when {
        "no address can be found" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  sample[FillingOutReturn].copy(
                    draftReturn = sample[DraftMultipleDisposalsReturn].copy(
                      triageAnswers = sample[IncompleteMultipleDisposalsTriageAnswers].copy(
                        taxYear = Some(sample[TaxYear]),
                        completionDate = Some(sample[CompletionDate])
                      ),
                      examplePropertyDetailsAnswers =
                        Some(sample[IncompleteExamplePropertyDetailsAnswers].copy(address = None))
                    )
                  )
                )
              )
            )
          }

          checkIsTechnicalErrorPage(
            performAction()
          )
        }
      }

      "redirect to the check your answers page" when {
        "the user is on a single disposal journey" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  sample[FillingOutReturn].copy(
                    draftReturn = sample[DraftSingleDisposalReturn],
                    subscribedDetails = sample[SubscribedDetails].copy(
                      name = Right(sample[IndividualName])
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

      "display the page" when {
        def test(
          draftReturn: DraftMultipleDisposalsReturn,
          expectedBackLink: Call,
          userType: UserType
        ): Unit = {
          val dateOfDeath = LocalDate.of(2020, 1, 1)
          val isAmend     = sample[Boolean]
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                userType = Some(userType),
                journeyStatus = Some(
                  sample[FillingOutReturn].copy(
                    draftReturn = draftReturn.copy(
                      representeeAnswers =
                        Some(sample[CompleteRepresenteeAnswers].copy(dateOfDeath = Some(DateOfDeath(dateOfDeath))))
                    ),
                    subscribedDetails = sample[SubscribedDetails].copy(
                      name = if (userType === Organisation) Left(sample[TrustName]) else Right(sample[IndividualName])
                    ),
                    agentReferenceNumber = if (userType === Agent) Some(sample[AgentReferenceNumber]) else None,
                    amendReturnData = if (isAmend) Some(sample[AmendReturnData]) else None
                  )
                )
              )
            )
          }

          val individualUserType = draftReturn.triageAnswers
            .fold(_.individualUserType, _.individualUserType)

          val addressLine1 = draftReturn.examplePropertyDetailsAnswers
            .getOrElse(fail("Cannot find address"))
            .fold(
              i =>
                i.address match {
                  case Some(address) => address.line1
                  case None          => fail("Cannot find address")
                },
              c => c.address.line1
            )

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("multipleDisposalsAcquisitionPrice.title"),
            { doc =>
              doc.select("#back, .govuk-back-link").attr("href") shouldBe expectedBackLink.url
              doc
                .select("#content > article > form, #main-content form")
                .attr("action")                                  shouldBe routes.PropertyDetailsController
                .acquisitionPriceSubmit()
                .url
              doc
                .select("#multipleDisposalsAcquisitionPrice-hint")
                .text()                                          shouldBe messageFromMessageKey(
                s"multipleDisposalsAcquisitionPrice${messageKey(userType, individualUserType)}.helpText"
              )
              doc
                .select("#submitButton")
                .text()                                          shouldBe expectedSubmitText(isAmend)
              doc.select("span.govuk-caption-xl").text           shouldBe addressLine1
            }
          )
        }

        "individual user has not started this section before" in {
          test(
            sample[DraftMultipleDisposalsReturn].copy(
              examplePropertyDetailsAnswers = Some(emptyAnswers.copy(address = Some(sample[UkAddress]))),
              triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                .copy(individualUserType = Some(Self))
            ),
            routes.PropertyDetailsController.disposalPrice(),
            Individual
          )
        }

        "trust user has not started this section before" in {
          test(
            sample[DraftMultipleDisposalsReturn].copy(
              examplePropertyDetailsAnswers = Some(emptyAnswers.copy(address = Some(sample[UkAddress]))),
              triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                .copy(individualUserType = None)
            ),
            routes.PropertyDetailsController.disposalPrice(),
            Organisation
          )
        }

        "agent user has not started this section before" in {
          test(
            sample[DraftMultipleDisposalsReturn].copy(
              examplePropertyDetailsAnswers = Some(emptyAnswers.copy(address = Some(sample[UkAddress]))),
              triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                .copy(individualUserType = None)
            ),
            routes.PropertyDetailsController.disposalPrice(),
            Agent
          )
        }

        "capacitor user has not started this section before" in {
          test(
            sample[DraftMultipleDisposalsReturn].copy(
              examplePropertyDetailsAnswers = Some(emptyAnswers.copy(address = Some(sample[UkAddress]))),
              triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                .copy(individualUserType = Some(Capacitor))
            ),
            routes.PropertyDetailsController.disposalPrice(),
            Individual
          )
        }

        "personal representative user has not started this section before" in {
          test(
            sample[DraftMultipleDisposalsReturn].copy(
              examplePropertyDetailsAnswers = Some(emptyAnswers.copy(address = Some(sample[UkAddress]))),
              triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                .copy(individualUserType = Some(PersonalRepresentative))
            ),
            routes.PropertyDetailsController.disposalPrice(),
            Individual
          )
        }

        "individual user has started but not completed this section" in {
          test(
            sample[DraftMultipleDisposalsReturn].copy(
              triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers].copy(
                individualUserType = Some(Self)
              ),
              examplePropertyDetailsAnswers = Some(
                sample[IncompleteExamplePropertyDetailsAnswers].copy(
                  acquisitionPrice = None,
                  address = Some(sample[UkAddress])
                )
              )
            ),
            routes.PropertyDetailsController.disposalPrice(),
            Individual
          )
        }

        "trust user has started but not completed this section" in {
          test(
            sample[DraftMultipleDisposalsReturn].copy(
              examplePropertyDetailsAnswers = Some(
                sample[IncompleteExamplePropertyDetailsAnswers].copy(
                  acquisitionPrice = None,
                  address = Some(sample[UkAddress])
                )
              ),
              triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                .copy(individualUserType = None)
            ),
            routes.PropertyDetailsController.disposalPrice(),
            Organisation
          )
        }

        "agent user has started but not completed this section" in {
          test(
            sample[DraftMultipleDisposalsReturn].copy(
              examplePropertyDetailsAnswers = Some(
                sample[IncompleteExamplePropertyDetailsAnswers].copy(
                  acquisitionPrice = None,
                  address = Some(sample[UkAddress])
                )
              ),
              triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                .copy(individualUserType = Some(Self))
            ),
            routes.PropertyDetailsController.disposalPrice(),
            Agent
          )
        }

        "capacitor user has started but not completed this section" in {
          test(
            sample[DraftMultipleDisposalsReturn].copy(
              examplePropertyDetailsAnswers = Some(
                sample[IncompleteExamplePropertyDetailsAnswers].copy(
                  acquisitionPrice = None,
                  address = Some(sample[UkAddress])
                )
              ),
              triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                .copy(individualUserType = Some(Capacitor))
            ),
            routes.PropertyDetailsController.disposalPrice(),
            Individual
          )
        }

        "period of admin personal rep has started but not completed this section" in {
          test(
            sample[DraftMultipleDisposalsReturn].copy(
              examplePropertyDetailsAnswers = Some(
                sample[IncompleteExamplePropertyDetailsAnswers].copy(
                  acquisitionPrice = None,
                  address = Some(sample[UkAddress])
                )
              ),
              triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                .copy(individualUserType = Some(PersonalRepresentativeInPeriodOfAdmin))
            ),
            routes.PropertyDetailsController.disposalPrice(),
            Individual
          )
        }

        "agent of estate has started but not completed this section" in {
          test(
            sample[DraftMultipleDisposalsReturn].copy(
              examplePropertyDetailsAnswers = Some(
                sample[IncompleteExamplePropertyDetailsAnswers].copy(
                  acquisitionPrice = None,
                  address = Some(sample[UkAddress])
                )
              ),
              triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                .copy(individualUserType = Some(PersonalRepresentativeInPeriodOfAdmin))
            ),
            routes.PropertyDetailsController.disposalPrice(),
            Agent
          )
        }

        "personal representative user has started but not completed this section" in {
          test(
            sample[DraftMultipleDisposalsReturn].copy(
              examplePropertyDetailsAnswers = Some(
                sample[IncompleteExamplePropertyDetailsAnswers].copy(
                  acquisitionPrice = None,
                  address = Some(sample[UkAddress])
                )
              ),
              triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                .copy(individualUserType = Some(PersonalRepresentative))
            ),
            routes.PropertyDetailsController.disposalPrice(),
            Individual
          )
        }

        "individual user has completed this section" in {
          test(
            sample[DraftMultipleDisposalsReturn].copy(
              examplePropertyDetailsAnswers = Some(
                sample[CompleteExamplePropertyDetailsAnswers].copy(
                  acquisitionPrice = sample[AmountInPence]
                )
              ),
              triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                .copy(individualUserType = None)
            ),
            routes.PropertyDetailsController.checkYourAnswers(),
            Individual
          )
        }

        "trust user has completed this section" in {
          test(
            sample[DraftMultipleDisposalsReturn].copy(
              examplePropertyDetailsAnswers = Some(
                sample[CompleteExamplePropertyDetailsAnswers].copy(
                  acquisitionPrice = sample[AmountInPence]
                )
              ),
              triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                .copy(individualUserType = None)
            ),
            routes.PropertyDetailsController.checkYourAnswers(),
            Organisation
          )
        }

        "agent user has completed this section" in {
          test(
            sample[DraftMultipleDisposalsReturn].copy(
              examplePropertyDetailsAnswers = Some(
                sample[CompleteExamplePropertyDetailsAnswers].copy(
                  acquisitionPrice = sample[AmountInPence]
                )
              ),
              triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                .copy(individualUserType = None)
            ),
            routes.PropertyDetailsController.checkYourAnswers(),
            Agent
          )
        }

        "capacitor user has completed this section" in {
          test(
            sample[DraftMultipleDisposalsReturn].copy(
              examplePropertyDetailsAnswers = Some(
                sample[CompleteExamplePropertyDetailsAnswers].copy(
                  acquisitionPrice = sample[AmountInPence]
                )
              ),
              triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                .copy(individualUserType = Some(Capacitor))
            ),
            routes.PropertyDetailsController.checkYourAnswers(),
            Individual
          )
        }

        "personal representative user has completed this section" in {
          test(
            sample[DraftMultipleDisposalsReturn].copy(
              examplePropertyDetailsAnswers = Some(
                sample[CompleteExamplePropertyDetailsAnswers].copy(
                  acquisitionPrice = sample[AmountInPence]
                )
              ),
              triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                .copy(individualUserType = Some(PersonalRepresentative))
            ),
            routes.PropertyDetailsController.checkYourAnswers(),
            Individual
          )
        }

        "period of admin personal representative user has completed this section" in {
          test(
            sample[DraftMultipleDisposalsReturn].copy(
              examplePropertyDetailsAnswers = Some(
                sample[CompleteExamplePropertyDetailsAnswers].copy(
                  acquisitionPrice = sample[AmountInPence]
                )
              ),
              triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                .copy(individualUserType = Some(PersonalRepresentativeInPeriodOfAdmin))
            ),
            routes.PropertyDetailsController.checkYourAnswers(),
            Individual
          )
        }
      }
    }

    "handling submitted answers to the acquisition price page" must {
      val key = "multipleDisposalsAcquisitionPrice"

      def performAction(formData: (String, String)*): Future[Result] =
        controller.acquisitionPriceSubmit()(
          FakeRequest().withFormUrlEncodedBody(formData*).withMethod("POST")
        )

      behave like redirectToStartBehaviour(() => performAction())

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.acquisitionPriceSubmit(),
        mockUUIDGenerator
      )

      "not update the session" when {
        "the data submitted is the same as one that already exists in session" in {
          val acquisitionPrice = AmountInPence.fromPounds(1000)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  sample[FillingOutReturn].copy(
                    draftReturn = sample[DraftMultipleDisposalsReturn].copy(
                      examplePropertyDetailsAnswers = Some(
                        sample[IncompleteExamplePropertyDetailsAnswers].copy(
                          acquisitionPrice = Some(acquisitionPrice),
                          address = Some(sample[UkAddress])
                        )
                      )
                    ),
                    subscribedDetails = sample[SubscribedDetails].copy(
                      name = Right(sample[IndividualName])
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
        def test(
          data: (String, String)*
        )(draftReturn: DraftMultipleDisposalsReturn, expectedTitle: String, expectedErrorMessage: String): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  sample[FillingOutReturn].copy(
                    draftReturn = draftReturn,
                    subscribedDetails = sample[SubscribedDetails].copy(
                      name = Right(sample[IndividualName])
                    )
                  )
                )
              )
            )
          }

          checkPageIsDisplayed(
            performAction(data*),
            expectedTitle,
            doc =>
              doc
                .select("[data-spec='errorSummaryDisplay'] a")
                .text() shouldBe expectedErrorMessage,
            BAD_REQUEST
          )
        }

        "the data is invalid" in {
          forAll { (individualUserType: Option[IndividualUserType]) =>
            whenever(!individualUserType.contains(PersonalRepresentativeInPeriodOfAdmin)) {
              amountOfMoneyErrorScenarios(key).foreach { scenario =>
                withClue(s"For $scenario: ") {
                  test(scenario.formData*)(
                    sample[DraftMultipleDisposalsReturn].copy(
                      triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers].copy(individualUserType = None),
                      examplePropertyDetailsAnswers = Some(
                        sample[CompleteExamplePropertyDetailsAnswers]
                      )
                    ),
                    messageFromMessageKey(s"$key.title"),
                    messageFromMessageKey(scenario.expectedErrorMessageKey)
                  )
                }
              }
            }
          }
        }

        "the data is invalid for a period of admin personal rep" in {
          val dateOfDeath = LocalDate.ofEpochDay(0L)

          amountOfMoneyErrorScenarios(key, errorContext = Some(s"$key.personalRepInPeriodOfAdmin")).foreach {
            scenario =>
              withClue(s"For $scenario: ") {
                val data = scenario.formData
                test(data*)(
                  sample[DraftMultipleDisposalsReturn].copy(
                    triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                      .copy(individualUserType = Some(PersonalRepresentativeInPeriodOfAdmin)),
                    representeeAnswers =
                      Some(sample[CompleteRepresenteeAnswers].copy(dateOfDeath = Some(DateOfDeath(dateOfDeath)))),
                    examplePropertyDetailsAnswers = Some(
                      sample[CompleteExamplePropertyDetailsAnswers]
                    )
                  ),
                  messageFromMessageKey(
                    s"$key.title",
                    TimeUtils.govDisplayFormat(dateOfDeath)
                  ),
                  messageFromMessageKey(scenario.expectedErrorMessageKey)
                )
              }
          }
        }
      }

      "redirect to the cya page" when {
        def test(
          result: => Future[Result],
          oldDraftReturn: DraftReturn,
          updatedDraftReturn: DraftReturn,
          isAmend: Boolean
        ): Unit = {
          val amendReturnData = sample[AmendReturnData]
          val journey         =
            sample[FillingOutReturn].copy(
              draftReturn = oldDraftReturn,
              amendReturnData =
                if (isAmend) Some(amendReturnData.copy(shouldDisplayGainOrLossAfterReliefs = false)) else None
            )
          val session         = SessionData.empty.copy(journeyStatus = Some(journey))
          val updatedJourney  = journey.copy(
            draftReturn = updatedDraftReturn,
            amendReturnData =
              if (isAmend) Some(amendReturnData.copy(shouldDisplayGainOrLossAfterReliefs = true)) else None
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(updatedJourney)(
              Right(())
            )
            mockStoreSession(
              session
                .copy(journeyStatus = Some(updatedJourney))
            )(Right(()))
          }

          checkIsRedirect(
            result,
            routes.PropertyDetailsController.checkYourAnswers()
          )
        }

        "the user is on a multiple disposals journey and has completed this section" in {
          val answers = sample[CompleteExamplePropertyDetailsAnswers].copy(
            disposalDate = disposalDate,
            acquisitionPrice = AmountInPence.fromPounds(10)
          )

          val oldDraftReturn = sample[DraftMultipleDisposalsReturn].copy(
            examplePropertyDetailsAnswers = Some(answers)
          )

          val newDraftReturn = oldDraftReturn.copy(
            examplePropertyDetailsAnswers = Some(
              answers.copy(
                acquisitionPrice = AmountInPence.fromPounds(100)
              )
            ),
            yearToDateLiabilityAnswers = None,
            gainOrLossAfterReliefs = None
          )

          test(
            performAction(key -> "100"),
            oldDraftReturn,
            newDraftReturn,
            isAmend = true
          )
        }

        "the user hasn't ever answered the acquisition price question " +
          "and the draft return and session data has been successfully updated" in {
            val answers = sample[IncompleteExamplePropertyDetailsAnswers].copy(
              address = Some(sample[UkAddress]),
              disposalDate = Some(disposalDate),
              acquisitionPrice = Some(AmountInPence.fromPounds(10))
            )

            val oldDraftReturn = sample[DraftMultipleDisposalsReturn].copy(
              examplePropertyDetailsAnswers = Some(answers)
            )

            val newDraftReturn = oldDraftReturn.copy(
              examplePropertyDetailsAnswers = Some(
                answers.copy(
                  acquisitionPrice = Some(AmountInPence.fromPounds(100))
                )
              ),
              yearToDateLiabilityAnswers = None,
              gainOrLossAfterReliefs = None
            )

            test(
              performAction(key -> "100"),
              oldDraftReturn,
              newDraftReturn,
              isAmend = true
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

      val address          = sample[UkAddress]
      val disposalDate     = sample[DisposalDate]
      val disposalPrice    = sample[AmountInPence]
      val acquisitionPrice = sample[AmountInPence]

      val completeAnswers =
        CompleteExamplePropertyDetailsAnswers(
          address,
          disposalDate,
          disposalPrice,
          acquisitionPrice
        )

      val allQuestionsAnswered = IncompleteExamplePropertyDetailsAnswers(
        Some(completeAnswers.address),
        Some(completeAnswers.disposalDate),
        Some(completeAnswers.disposalPrice),
        Some(completeAnswers.acquisitionPrice)
      )

      val currentDraftReturn =
        sample[DraftMultipleDisposalsReturn].copy(
          triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
            .copy(
              assetTypes = List(AssetType.Residential),
              individualUserType = Some(PersonalRepresentativeInPeriodOfAdmin)
            ),
          examplePropertyDetailsAnswers = Some(allQuestionsAnswered),
          representeeAnswers = None
        )
      val currentJourney     =
        sample[FillingOutReturn].copy(draftReturn = currentDraftReturn)
      val updatedDraftReturn = currentDraftReturn
        .copy(examplePropertyDetailsAnswers = Some(completeAnswers))
      val updatedJourney     = currentJourney.copy(draftReturn = updatedDraftReturn)

      behave like redirectToTaskListWhenNoAssetTypeBehaviour(() => performAction())

      "redirect to the guidance page" when {
        "the user has not started the section" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(journeyStatus =
                Some(
                  sample[FillingOutReturn].copy(
                    draftReturn = currentDraftReturn.copy(
                      examplePropertyDetailsAnswers = None
                    ),
                    subscribedDetails = sample[SubscribedDetails].copy(
                      name = Right(sample[IndividualName])
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
              SessionData.empty.copy(journeyStatus =
                Some(
                  sample[FillingOutReturn].copy(
                    draftReturn = currentDraftReturn.copy(
                      examplePropertyDetailsAnswers = Some(allQuestionsAnswered.copy(address = None))
                    ),
                    subscribedDetails = sample[SubscribedDetails].copy(
                      name = Right(sample[IndividualName])
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
      }

      "redirect to the disposal date page" when {
        "the user has started the section but there is no disposal date" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(journeyStatus =
                Some(
                  sample[FillingOutReturn].copy(
                    draftReturn = currentDraftReturn.copy(
                      examplePropertyDetailsAnswers = Some(
                        allQuestionsAnswered.copy(disposalDate = None)
                      )
                    ),
                    subscribedDetails = sample[SubscribedDetails].copy(
                      name = Right(sample[IndividualName])
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
              SessionData.empty.copy(journeyStatus =
                Some(
                  sample[FillingOutReturn].copy(
                    draftReturn = currentDraftReturn.copy(
                      examplePropertyDetailsAnswers = Some(
                        allQuestionsAnswered.copy(disposalPrice = None)
                      )
                    ),
                    subscribedDetails = sample[SubscribedDetails].copy(
                      name = Right(sample[IndividualName])
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
              SessionData.empty.copy(journeyStatus =
                Some(
                  sample[FillingOutReturn].copy(
                    draftReturn = currentDraftReturn.copy(
                      examplePropertyDetailsAnswers = Some(
                        allQuestionsAnswered.copy(acquisitionPrice = None)
                      )
                    ),
                    subscribedDetails = sample[SubscribedDetails].copy(
                      name = Right(sample[IndividualName])
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
          answers: CompleteExamplePropertyDetailsAnswers,
          expectedTitleKey: String,
          hasGuidanceLink: Boolean
        ): Unit =
          checkPageIsDisplayed(
            result,
            messageFromMessageKey(expectedTitleKey),
            { doc =>
              val guidanceLink = doc.select("#guidanceLink")

              if (hasGuidanceLink) {
                guidanceLink.attr(
                  "href"
                )                   shouldBe routes.PropertyDetailsController
                  .multipleDisposalsGuidance()
                  .url
                guidanceLink.text() shouldBe messageFromMessageKey(
                  "returns.property-details.multiple-disposals.cya.guidanceLink"
                )
              } else {
                guidanceLink.isEmpty shouldBe true
              }

              MultipleDisposalsPropertyDetailsControllerSpec
                .validateExamplePropertyDetailsSummary(answers, doc)

              doc.select("span.govuk-caption-xl").text shouldBe answers.address.line1
            }
          )

        "the user has just finished answering the questions and all updates are successful" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(journeyStatus = Some(currentJourney))
            )
            mockStoreDraftReturn(updatedJourney)(Right(()))
            mockStoreSession(
              SessionData.empty.copy(journeyStatus = Some(updatedJourney))
            )(Right(()))
          }

          testIsCheckYourAnswers(
            performAction(),
            completeAnswers,
            "returns.property-address.cya.title",
            hasGuidanceLink = false
          )
        }

        "the user hsa already completed the section" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(journeyStatus = Some(updatedJourney))
            )
          }

          testIsCheckYourAnswers(
            performAction(),
            completeAnswers,
            "returns.property-address.cya.title",
            hasGuidanceLink = true
          )
        }
      }

      "show an error page" when {
        "there is an error updating the draft return" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(journeyStatus = Some(currentJourney))
            )
            mockStoreDraftReturn(updatedJourney)(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction())
        }

        "there is an error updating the session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(journeyStatus = Some(currentJourney))
            )
            mockStoreDraftReturn(updatedJourney)(Right(()))
            mockStoreSession(
              SessionData.empty.copy(journeyStatus = Some(updatedJourney))
            )(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction())
        }
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
  }

  private def redirectToTaskListWhenNoAssetTypeBehaviour(
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
                  draftReturn = sample[DraftMultipleDisposalsReturn].copy(
                    triageAnswers = sample[IncompleteMultipleDisposalsTriageAnswers]
                      .copy(assetTypes = None)
                  ),
                  subscribedDetails = sample[SubscribedDetails].copy(
                    name = Right(sample[IndividualName])
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

  private def redirectWhenShouldNotAskIfPostcodeExistsBehaviour(
    performAction: () => Future[Result]
  ): Unit =
    "redirect to the check your answers page" when {
      "the asset types being disposed of do not require us to ask if a postcode exists" in {
        forAll { (assetTypes: List[AssetType]) =>
          whenever(
            assetTypes.contains(AssetType.Residential) ||
              assetTypes.toSet === Set(
                AssetType.MixedUse,
                AssetType.NonResidential
              ) ||
              assetTypes.toSet === Set(
                AssetType.IndirectDisposal,
                AssetType.MixedUse
              )
          ) {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                SessionData.empty.copy(
                  journeyStatus = Some(
                    sample[FillingOutReturn].copy(
                      draftReturn = sample[DraftMultipleDisposalsReturn].copy(
                        triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers].copy(
                          assetTypes = List(AssetType.Residential)
                        )
                      ),
                      subscribedDetails = sample[SubscribedDetails].copy(
                        name = Right(sample[IndividualName])
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
    }

  private def noDateOfDeathForPersonalRepBehaviour(performAction: () => Future[Result]): Unit =
    "show an error page" when {
      def sessionWithNoDateOfDeath(individualUserType: IndividualUserType): SessionData =
        SessionData.empty.copy(
          journeyStatus = Some(
            sample[FillingOutReturn].copy(
              draftReturn = sample[DraftMultipleDisposalsReturn].copy(
                triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers].copy(
                  individualUserType = Some(individualUserType)
                ),
                representeeAnswers = Some(sample[CompleteRepresenteeAnswers].copy(dateOfDeath = None))
              )
            )
          )
        )

      "there is no date of death found for a personal rep" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionWithNoDateOfDeath(PersonalRepresentative))
        }

        checkIsTechnicalErrorPage(performAction())
      }

      "there is no date of death found for a personal rep in period of admin" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionWithNoDateOfDeath(PersonalRepresentativeInPeriodOfAdmin))
        }

        checkIsTechnicalErrorPage(performAction())
      }
    }
}

object MultipleDisposalsPropertyDetailsControllerSpec extends Matchers {
  def validateExamplePropertyDetailsSummary(
    examplePropertyDetailsAnswers: CompleteExamplePropertyDetailsAnswers,
    doc: Document
  ): Unit = {
    val ukAddress = examplePropertyDetailsAnswers.address

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
}
