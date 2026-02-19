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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns

import org.jsoup.Jsoup.parse
import org.jsoup.nodes.Document
import uk.gov.hmrc.cgtpropertydisposalsfrontend.SampledScalaCheck
import play.api.i18n.MessagesApi
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.{Call, Result}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, PreviousReturnData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.SessionData
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address.UkAddress
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.{Address, Country}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.AmountInPence
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.AcquisitionDetailsGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.AddressGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.DisposalDetailsGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.DraftReturnGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.ExampleCompanyDetailsAnswersGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.ExamplePropertyDetailsAnswersGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.ExemptionsAndLossesAnswersGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.FileUploadGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.JourneyStatusGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.MoneyGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.ReliefDetailsGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.RepresenteeAnswersGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.ReturnGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.SingleMixedUseDetailsAnswersGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.TriageQuestionsGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.YearToDateLiabilityAnswersGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.AcquisitionDetailsAnswers.{CompleteAcquisitionDetailsAnswers, IncompleteAcquisitionDetailsAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.DisposalDetailsAnswers.{CompleteDisposalDetailsAnswers, IncompleteDisposalDetailsAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ExampleCompanyDetailsAnswers.{CompleteExampleCompanyDetailsAnswers, IncompleteExampleCompanyDetailsAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ExamplePropertyDetailsAnswers.{CompleteExamplePropertyDetailsAnswers, IncompleteExamplePropertyDetailsAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ExemptionAndLossesAnswers.{CompleteExemptionAndLossesAnswers, IncompleteExemptionAndLossesAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.IndividualUserType.{PersonalRepresentative, PersonalRepresentativeInPeriodOfAdmin, Self}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.MixedUsePropertyDetailsAnswers.{CompleteMixedUsePropertyDetailsAnswers, IncompleteMixedUsePropertyDetailsAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.MultipleDisposalsTriageAnswers.{CompleteMultipleDisposalsTriageAnswers, IncompleteMultipleDisposalsTriageAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ReliefDetailsAnswers.{CompleteReliefDetailsAnswers, IncompleteReliefDetailsAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.RepresenteeAnswers.{CompleteRepresenteeAnswers, IncompleteRepresenteeAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.{CompleteSingleDisposalTriageAnswers, IncompleteSingleDisposalTriageAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SupportingEvidenceAnswers.CompleteSupportingEvidenceAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.YearToDateLiabilityAnswers.CalculatedYTDAnswers._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.YearToDateLiabilityAnswers.{CalculatedYTDAnswers, NonCalculatedYTDAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views.returns.TaskListStatus

import java.time.LocalDate
import scala.concurrent.Future

class TaskListControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with SampledScalaCheck
    with RedirectToStartBehaviour {

  protected override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore)
    )

  private lazy val controller = instanceOf[TaskListController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  def removeEvidence(u: SupportingEvidenceAnswers): SupportingEvidenceAnswers =
    u.fold(
      _.copy(evidences = List.empty),
      _.copy(evidences = List.empty)
    )

  def removeEvidence(
    y: YearToDateLiabilityAnswers
  ): YearToDateLiabilityAnswers =
    y match {
      case c: CalculatedYTDAnswers    =>
        c.fold(
          _.copy(mandatoryEvidence = None),
          _.copy(mandatoryEvidence = None)
        )
      case n: NonCalculatedYTDAnswers =>
        n.fold(
          _.copy(mandatoryEvidence = None),
          _.copy(mandatoryEvidence = None)
        )
    }

  def removeEvidence(d: DraftReturn): DraftReturn =
    d.fold(
      multiple =>
        multiple.copy(
          supportingEvidenceAnswers = multiple.supportingEvidenceAnswers.map(removeEvidence),
          yearToDateLiabilityAnswers = multiple.yearToDateLiabilityAnswers.map(removeEvidence)
        ),
      single =>
        single.copy(
          supportingEvidenceAnswers = single.supportingEvidenceAnswers.map(removeEvidence),
          yearToDateLiabilityAnswers = single.yearToDateLiabilityAnswers.map(removeEvidence)
        ),
      singleIndirect =>
        singleIndirect.copy(
          supportingEvidenceAnswers = singleIndirect.supportingEvidenceAnswers.map(removeEvidence),
          yearToDateLiabilityAnswers = singleIndirect.yearToDateLiabilityAnswers.map(removeEvidence)
        ),
      multipleIndirect =>
        multipleIndirect.copy(
          supportingEvidenceAnswers = multipleIndirect.supportingEvidenceAnswers.map(removeEvidence),
          yearToDateLiabilityAnswers = multipleIndirect.yearToDateLiabilityAnswers.map(removeEvidence)
        ),
      singleMixedUse =>
        singleMixedUse.copy(
          supportingEvidenceAnswers = singleMixedUse.supportingEvidenceAnswers.map(removeEvidence),
          yearToDateLiabilityAnswers = singleMixedUse.yearToDateLiabilityAnswers.map(removeEvidence)
        )
    )

  "TaskListController" when {

    "handling requests to display the task list page" when {

      def performAction(): Future[Result] = controller.taskList()(FakeRequest())

      def testStateOfSection(draftReturn: DraftReturn)(
        sectionLinkId: String,
        sectionLinkText: String,
        sectionLinkHref: Call,
        sectionsStatus: TaskListStatus,
        extraChecks: Document => Unit = _ => (),
        previousSentReturns: Option[List[ReturnSummary]] = Some(List(sample[ReturnSummary])),
        amendReturnData: Option[AmendReturnData] = None
      ): Unit =
        withClue(s"For draft return $draftReturn: ") {
          val fillingOutReturn = sample[FillingOutReturn].copy(
            draftReturn = removeEvidence(draftReturn),
            previousSentReturns =
              previousSentReturns.map(PreviousReturnData(_, Some(sample[AmountInPence]), None, None)),
            amendReturnData = amendReturnData
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(fillingOutReturn)
              )
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("service.title"),
            doc =>
              try {
                sectionsStatus match {
                  case TaskListStatus.CannotStart =>
                    doc
                      .select(s"li#$sectionLinkId > span")
                      .text shouldBe sectionLinkText

                  case _ =>
                    doc
                      .select(s"li#$sectionLinkId > a")
                      .text         shouldBe sectionLinkText
                    doc
                      .select(s"li#$sectionLinkId > a")
                      .attr("href") shouldBe sectionLinkHref.url
                }

                doc
                  .select(s"li#$sectionLinkId > strong")
                  .text shouldBe messageFromMessageKey(s"task-list.$sectionsStatus").toUpperCase
                extraChecks(doc)
              } catch {
                case t: Throwable =>
                  println(s"Failed with html: ${doc.select("#content").html()}")
                  sys.error(t.getMessage)
              }
          )
        }

      def testSectionNonExistent(
        draftReturn: DraftReturn,
        previousSentReturns: Option[List[ReturnSummary]] = None,
        amendReturnData: Option[AmendReturnData] = None
      )(
        sectionLinkId: String
      ): Unit = {
        val fillingOutReturn = sample[FillingOutReturn].copy(
          draftReturn = removeEvidence(draftReturn),
          previousSentReturns = previousSentReturns.map(PreviousReturnData(_, Some(sample[AmountInPence]), None, None)),
          amendReturnData = amendReturnData
        )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            SessionData.empty.copy(
              journeyStatus = Some(fillingOutReturn)
            )
          )
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("service.title"),
          doc => doc.select(s"li#$sectionLinkId > span").isEmpty shouldBe true
        )
      }

      behave like redirectToStartWhenInvalidJourney(
        () => performAction(),
        {
          case _: FillingOutReturn => true
          case _                   => false
        }
      )

      "handling requests to display the single disposal task list page" must {

        "display the page with the proper person represented section status" when {

          "the individual user type is personal representative or capacitor and" when {

            "the section has not been started" in {
              val draftReturn =
                sample[DraftSingleDisposalReturn].copy(
                  triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                    .copy(individualUserType = Some(PersonalRepresentative)),
                  representeeAnswers = None
                )

              testStateOfSection(
                draftReturn
              )(
                "representee",
                messageFromMessageKey("task-list.representee.link"),
                representee.routes.RepresenteeController.checkYourAnswers(),
                TaskListStatus.ToDo,
                _.select("div.notice")
                  .contains(messageFromMessageKey("task-list.incompleteTriage"))
              )
            }

            "the section is incomplete" in {
              val draftReturn =
                sample[DraftSingleDisposalReturn].copy(
                  triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                    .copy(individualUserType = Some(PersonalRepresentative)),
                  representeeAnswers = Some(sample[IncompleteRepresenteeAnswers])
                )

              testStateOfSection(
                draftReturn
              )(
                "representee",
                messageFromMessageKey("task-list.representee.link"),
                representee.routes.RepresenteeController.checkYourAnswers(),
                TaskListStatus.InProgress,
                _.select("div.notice")
                  .contains(messageFromMessageKey("task-list.incompleteTriage"))
              )
            }

            "the section is complete" in {
              val draftReturn =
                sample[DraftSingleDisposalReturn].copy(
                  triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                    .copy(individualUserType = Some(PersonalRepresentative)),
                  representeeAnswers = Some(sample[CompleteRepresenteeAnswers])
                )

              testStateOfSection(
                draftReturn
              )(
                "representee",
                messageFromMessageKey("task-list.representee.link"),
                representee.routes.RepresenteeController.checkYourAnswers(),
                TaskListStatus.Complete
              )
            }

          }

        }

        "display the page with the proper single disposal triage section status" when {

          "the session data indicates that they are filling in a return and the triage section is incomplete" in {
            val incompleteTriage =
              sample[DraftSingleDisposalReturn].copy(
                triageAnswers = sample[IncompleteSingleDisposalTriageAnswers]
                  .copy(individualUserType = Some(Self))
              )
            testStateOfSection(
              incompleteTriage
            )(
              "canTheyUseOurService",
              messageFromMessageKey("task-list.triage.link"),
              triage.routes.SingleDisposalsTriageController.checkYourAnswers(),
              TaskListStatus.InProgress,
              _.select("div.notice")
                .contains(messageFromMessageKey("task-list.incompleteTriage"))
            )

          }

          "the session data indicates that they are filling in a return and the triage section is complete" in {
            testStateOfSection(
              sample[DraftSingleDisposalReturn]
                .copy(triageAnswers =
                  sample[CompleteSingleDisposalTriageAnswers]
                    .copy(individualUserType = Some(Self))
                )
            )(
              "canTheyUseOurService",
              messageFromMessageKey("task-list.triage.link"),
              triage.routes.SingleDisposalsTriageController.checkYourAnswers(),
              TaskListStatus.Complete
            )
          }

        }

        "display the page with the proper Enter property address section status" when {

          "the session data indicates that they are filling in a return and enter property address is todo" in {
            testStateOfSection(
              sample[DraftSingleDisposalReturn]
                .copy(
                  triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                    .copy(individualUserType = Some(Self)),
                  propertyAddress = None
                )
            )(
              "propertyAddress",
              messageFromMessageKey("task-list.enter-property-address.link"),
              address.routes.PropertyDetailsController.checkYourAnswers(),
              TaskListStatus.ToDo
            )
          }

          "the session data indicates that they are filling in a return and enter property address is complete" in {
            testStateOfSection(
              sample[DraftSingleDisposalReturn].copy(
                triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                  .copy(individualUserType = Some(Self)),
                propertyAddress = Some(sample[UkAddress])
              )
            )(
              "propertyAddress",
              messageFromMessageKey("task-list.enter-property-address.link"),
              address.routes.PropertyDetailsController.checkYourAnswers(),
              TaskListStatus.Complete
            )
          }

        }

        "display the page with the proper disposal details section status" when {

          "the session data indicates that they are filling in a return and the section has not been started yet is todo" in {
            testStateOfSection(
              sample[DraftSingleDisposalReturn]
                .copy(
                  triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                    .copy(individualUserType = Some(Self)),
                  disposalDetailsAnswers = None
                )
            )(
              "disposalDetails",
              messageFromMessageKey("task-list.disposals-details.link"),
              disposaldetails.routes.DisposalDetailsController.checkYourAnswers(),
              TaskListStatus.ToDo
            )
          }

          "the session data indicates that they are filling in a return and they have started the section but not complete it yet" in {
            testStateOfSection(
              sample[DraftSingleDisposalReturn]
                .copy(
                  triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                    .copy(individualUserType = Some(Self)),
                  disposalDetailsAnswers = Some(sample[IncompleteDisposalDetailsAnswers])
                )
            )(
              "disposalDetails",
              messageFromMessageKey("task-list.disposals-details.link"),
              disposaldetails.routes.DisposalDetailsController.checkYourAnswers(),
              TaskListStatus.InProgress
            )
          }

          "the session data indicates that they are filling in a return and they have completed the section" in {
            testStateOfSection(
              sample[DraftSingleDisposalReturn].copy(
                triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                  .copy(individualUserType = Some(Self)),
                disposalDetailsAnswers = Some(sample[CompleteDisposalDetailsAnswers])
              )
            )(
              "disposalDetails",
              messageFromMessageKey("task-list.disposals-details.link"),
              disposaldetails.routes.DisposalDetailsController.checkYourAnswers(),
              TaskListStatus.Complete
            )
          }

        }

        "display the page with the proper acquisition details section status" when {

          "the session data indicates that they are filling in a return and the section has not been started yet is todo" in {
            testStateOfSection(
              sample[DraftSingleDisposalReturn]
                .copy(
                  triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                    .copy(individualUserType = Some(Self)),
                  acquisitionDetailsAnswers = None
                )
            )(
              "acquisitionDetails",
              messageFromMessageKey("task-list.acquisition-details.link"),
              acquisitiondetails.routes.AcquisitionDetailsController
                .checkYourAnswers(),
              TaskListStatus.ToDo
            )
          }

          "the session data indicates that they are filling in a return and they have started the section but not complete it yet" in {
            testStateOfSection(
              sample[DraftSingleDisposalReturn]
                .copy(
                  triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                    .copy(individualUserType = Some(Self)),
                  acquisitionDetailsAnswers = Some(sample[IncompleteAcquisitionDetailsAnswers])
                )
            )(
              "acquisitionDetails",
              messageFromMessageKey("task-list.acquisition-details.link"),
              acquisitiondetails.routes.AcquisitionDetailsController
                .checkYourAnswers(),
              TaskListStatus.InProgress
            )
          }

          "the session data indicates that they are filling in a return and they have completed the section" in {
            testStateOfSection(
              sample[DraftSingleDisposalReturn]
                .copy(
                  triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                    .copy(individualUserType = Some(Self)),
                  acquisitionDetailsAnswers = Some(sample[CompleteAcquisitionDetailsAnswers])
                )
            )(
              "acquisitionDetails",
              messageFromMessageKey("task-list.acquisition-details.link"),
              acquisitiondetails.routes.AcquisitionDetailsController
                .checkYourAnswers(),
              TaskListStatus.Complete
            )
          }

        }

        "display the page with the proper reliefs details section status" when {

          def test(
            draftReturn: DraftSingleDisposalReturn,
            expectedStatus: TaskListStatus
          ): Unit =
            testStateOfSection(draftReturn)(
              "reliefDetails",
              messageFromMessageKey("task-list.relief-details.link"),
              reliefdetails.routes.ReliefDetailsController.checkYourAnswers(),
              expectedStatus
            )

          "the session data indicates that the property address has not been entered in" in {
            test(
              sample[DraftSingleDisposalReturn].copy(
                triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                  .copy(individualUserType = Some(Self), countryOfResidence = Country.uk),
                propertyAddress = None,
                disposalDetailsAnswers = Some(sample[CompleteDisposalDetailsAnswers]),
                acquisitionDetailsAnswers = Some(sample[CompleteAcquisitionDetailsAnswers]),
                reliefDetailsAnswers = None,
                exemptionAndLossesAnswers = None,
                initialGainOrLoss = Some(sample[AmountInPence])
              ),
              TaskListStatus.ToDo
            )
          }

          "the session data indicates that the disposal details section is not yet complete" in {
            List(None, Some(sample[IncompleteDisposalDetailsAnswers])).foreach { disposalDetailsState =>
              test(
                sample[DraftSingleDisposalReturn].copy(
                  triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                    .copy(individualUserType = Some(Self), countryOfResidence = Country.uk),
                  propertyAddress = Some(sample[UkAddress]),
                  disposalDetailsAnswers = disposalDetailsState,
                  acquisitionDetailsAnswers = Some(sample[CompleteAcquisitionDetailsAnswers]),
                  reliefDetailsAnswers = None,
                  exemptionAndLossesAnswers = None,
                  initialGainOrLoss = None
                ),
                TaskListStatus.CannotStart
              )
            }
          }

          "the session data indicates that the acquisition details section is not yet complete" in {
            List(None, Some(sample[IncompleteAcquisitionDetailsAnswers]))
              .foreach { acquisitionDetailsAnswers =>
                test(
                  sample[DraftSingleDisposalReturn].copy(
                    triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                      .copy(individualUserType = Some(Self), countryOfResidence = Country.uk),
                    propertyAddress = Some(sample[UkAddress]),
                    disposalDetailsAnswers = Some(sample[CompleteDisposalDetailsAnswers]),
                    acquisitionDetailsAnswers = acquisitionDetailsAnswers,
                    reliefDetailsAnswers = None,
                    exemptionAndLossesAnswers = None,
                    initialGainOrLoss = None
                  ),
                  TaskListStatus.CannotStart
                )
              }
          }

          "the property address, disposal details and acquisition details section have all " +
            "been completed and the reliefs section has not been started yet" in {
              test(
                sample[DraftSingleDisposalReturn].copy(
                  triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                    .copy(individualUserType = Some(Self), countryOfResidence = Country.uk),
                  propertyAddress = Some(sample[UkAddress]),
                  disposalDetailsAnswers = Some(sample[CompleteDisposalDetailsAnswers]),
                  acquisitionDetailsAnswers = Some(sample[CompleteAcquisitionDetailsAnswers]),
                  reliefDetailsAnswers = None,
                  exemptionAndLossesAnswers = None,
                  initialGainOrLoss = Some(sample[AmountInPence])
                ),
                TaskListStatus.ToDo
              )
            }

          "the property address, disposal details and acquisition details section have all " +
            "been completed and the reliefs section has been started but not completed yet" in {
              test(
                sample[DraftSingleDisposalReturn].copy(
                  triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                    .copy(individualUserType = Some(Self), countryOfResidence = Country.uk),
                  propertyAddress = Some(sample[UkAddress]),
                  disposalDetailsAnswers = Some(sample[CompleteDisposalDetailsAnswers]),
                  acquisitionDetailsAnswers = Some(sample[CompleteAcquisitionDetailsAnswers]),
                  reliefDetailsAnswers = Some(sample[IncompleteReliefDetailsAnswers]),
                  exemptionAndLossesAnswers = None,
                  initialGainOrLoss = Some(sample[AmountInPence])
                ),
                TaskListStatus.InProgress
              )
            }

          "the session data indicates that they are filling in a return and they have completed the section" in {
            test(
              sample[DraftSingleDisposalReturn].copy(
                triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                  .copy(individualUserType = Some(Self), countryOfResidence = Country.uk),
                propertyAddress = Some(sample[UkAddress]),
                disposalDetailsAnswers = Some(sample[CompleteDisposalDetailsAnswers]),
                acquisitionDetailsAnswers = Some(sample[CompleteAcquisitionDetailsAnswers]),
                reliefDetailsAnswers = Some(sample[CompleteReliefDetailsAnswers]),
                exemptionAndLossesAnswers = None,
                initialGainOrLoss = Some(sample[AmountInPence])
              ),
              TaskListStatus.Complete
            )
          }

          "the initial gain or loss section is not completed, " +
            " the property address, disposal details & acquisition details sections have been completed" in {
              val country = Country("HK")

              test(
                sample[DraftSingleDisposalReturn].copy(
                  triageAnswers = sample[CompleteSingleDisposalTriageAnswers].copy(
                    individualUserType = Some(Self),
                    countryOfResidence = country,
                    assetType = AssetType.Residential
                  ),
                  propertyAddress = Some(sample[UkAddress]),
                  disposalDetailsAnswers = Some(sample[CompleteDisposalDetailsAnswers]),
                  acquisitionDetailsAnswers = Some(
                    sample[CompleteAcquisitionDetailsAnswers].copy(
                      acquisitionDate = AcquisitionDate(LocalDate.of(1200, 1, 1))
                    )
                  ),
                  reliefDetailsAnswers = None,
                  exemptionAndLossesAnswers = None,
                  initialGainOrLoss = None
                ),
                TaskListStatus.CannotStart
              )
            }

        }

        "display the page with the proper gain or loss after reliefs section status" when {

          def test(
            draftReturn: DraftSingleDisposalReturn,
            previousSentReturns: Option[List[ReturnSummary]],
            amendReturnData: Option[AmendReturnData],
            expectedStatus: TaskListStatus
          ): Unit =
            testStateOfSection(draftReturn)(
              "gainOrLossAfterReliefs",
              messageFromMessageKey("task-list.gain-or-loss-after-reliefs.link"),
              gainorlossafterreliefs.routes.GainOrLossAfterReliefsController.checkYourAnswers(),
              expectedStatus,
              previousSentReturns = previousSentReturns,
              amendReturnData = amendReturnData
            )

          val triageAnswers = sample[CompleteSingleDisposalTriageAnswers].copy(
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

          val prerequisiteDraftReturn =
            sample[DraftSingleDisposalReturn].copy(
              triageAnswers = triageAnswers,
              propertyAddress = Some(sample[UkAddress]),
              disposalDetailsAnswers = Some(sample[CompleteDisposalDetailsAnswers]),
              acquisitionDetailsAnswers = Some(sample[CompleteAcquisitionDetailsAnswers]),
              reliefDetailsAnswers = Some(sample[CompleteReliefDetailsAnswers]),
              exemptionAndLossesAnswers = None,
              initialGainOrLoss = None,
              gainOrLossAfterReliefs = None
            )

          "the return is a further return and" when {

            "the property address section is not been started" in {
              test(
                prerequisiteDraftReturn.copy(propertyAddress = None),
                Some(List(sample[ReturnSummary].copy(taxYear = taxYearStartYear))),
                None,
                TaskListStatus.CannotStart
              )
            }

            "the disposal details section is not been started" in {
              test(
                prerequisiteDraftReturn.copy(disposalDetailsAnswers = None),
                Some(List(sample[ReturnSummary].copy(taxYear = taxYearStartYear))),
                None,
                TaskListStatus.CannotStart
              )
            }

            "the disposal details section is not complete" in {
              test(
                prerequisiteDraftReturn.copy(disposalDetailsAnswers = Some(sample[IncompleteDisposalDetailsAnswers])),
                Some(List(sample[ReturnSummary].copy(taxYear = taxYearStartYear))),
                None,
                TaskListStatus.CannotStart
              )
            }

            "the acquisition details section is not been started" in {
              test(
                prerequisiteDraftReturn.copy(acquisitionDetailsAnswers = None),
                Some(List(sample[ReturnSummary].copy(taxYear = taxYearStartYear))),
                None,
                TaskListStatus.CannotStart
              )
            }

            "the acquisition details section is not complete" in {
              test(
                prerequisiteDraftReturn
                  .copy(acquisitionDetailsAnswers = Some(sample[IncompleteAcquisitionDetailsAnswers])),
                Some(List(sample[ReturnSummary].copy(taxYear = taxYearStartYear))),
                None,
                TaskListStatus.CannotStart
              )
            }

            "the reliefs section is not been started" in {
              test(
                prerequisiteDraftReturn.copy(reliefDetailsAnswers = None),
                Some(List(sample[ReturnSummary].copy(taxYear = taxYearStartYear))),
                None,
                TaskListStatus.CannotStart
              )
            }

            "the reliefs section is not complete" in {
              test(
                prerequisiteDraftReturn.copy(reliefDetailsAnswers = Some(sample[IncompleteReliefDetailsAnswers])),
                Some(List(sample[ReturnSummary].copy(taxYear = taxYearStartYear))),
                None,
                TaskListStatus.CannotStart
              )
            }

            "the required sections are now complete but the GLAR section has not been started yet" in {
              test(
                prerequisiteDraftReturn.copy(gainOrLossAfterReliefs = None),
                Some(List(sample[ReturnSummary].copy(taxYear = taxYearStartYear))),
                None,
                TaskListStatus.ToDo
              )
            }

            "the required sections are now complete but the GLAR section has not been completed" in {
              test(
                prerequisiteDraftReturn.copy(gainOrLossAfterReliefs = Some(sample[AmountInPence])),
                Some(List(sample[ReturnSummary].copy(taxYear = taxYearStartYear))),
                None,
                TaskListStatus.Complete
              )
            }

          }

          "the return is not a further return or an amend return" in {
            testSectionNonExistent(prerequisiteDraftReturn.copy(gainOrLossAfterReliefs = None), None, None)(
              "gainOrLossAfterReliefs"
            )
          }

          "the return is an amend return where gain or loss after reliefs shouldn't be shown" in {
            testSectionNonExistent(
              prerequisiteDraftReturn.copy(gainOrLossAfterReliefs = None),
              Some(List(sample[ReturnSummary])),
              Some(
                sample[AmendReturnData].copy(
                  shouldDisplayGainOrLossAfterReliefs = false
                )
              )
            )(
              "gainOrLossAfterReliefs"
            )
          }

          "the return is an amend return where gain or loss after reliefs should be shown" in {
            test(
              prerequisiteDraftReturn.copy(gainOrLossAfterReliefs = None),
              Some(List(sample[ReturnSummary])),
              Some(
                sample[AmendReturnData].copy(
                  shouldDisplayGainOrLossAfterReliefs = true
                )
              ),
              TaskListStatus.ToDo
            )
          }

        }

        "display the page with the proper exemptions and losses section status" when {

          def test(
            draftReturn: DraftSingleDisposalReturn,
            expectedStatus: TaskListStatus,
            previousSentReturns: Option[List[ReturnSummary]] = None
          ): Unit =
            testStateOfSection(draftReturn)(
              "exemptionsAndLosses",
              messageFromMessageKey("task-list.exemptions-and-losses.link"),
              exemptionandlosses.routes.ExemptionAndLossesController
                .checkYourAnswers(),
              expectedStatus,
              previousSentReturns = previousSentReturns
            )

          "the session data indicates that the reliefs section is has not yet been started" in {
            test(
              sample[DraftSingleDisposalReturn].copy(
                triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                  .copy(individualUserType = Some(Self), countryOfResidence = Country.uk),
                propertyAddress = Some(sample[UkAddress]),
                disposalDetailsAnswers = Some(sample[CompleteDisposalDetailsAnswers]),
                acquisitionDetailsAnswers = Some(sample[CompleteAcquisitionDetailsAnswers]),
                reliefDetailsAnswers = None,
                exemptionAndLossesAnswers = None
              ),
              TaskListStatus.CannotStart
            )
          }

          "the session data indicates that the reliefs section is has not yet been completed" in {
            test(
              sample[DraftSingleDisposalReturn].copy(
                triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                  .copy(individualUserType = Some(Self), countryOfResidence = Country.uk),
                propertyAddress = Some(sample[UkAddress]),
                disposalDetailsAnswers = Some(sample[CompleteDisposalDetailsAnswers]),
                acquisitionDetailsAnswers = Some(sample[CompleteAcquisitionDetailsAnswers]),
                reliefDetailsAnswers = Some(sample[IncompleteReliefDetailsAnswers]),
                exemptionAndLossesAnswers = None
              ),
              TaskListStatus.CannotStart
            )
          }

          "the reliefs section has been completed and the section has not been started yet" in {
            test(
              sample[DraftSingleDisposalReturn].copy(
                triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                  .copy(individualUserType = Some(Self), countryOfResidence = Country.uk),
                propertyAddress = Some(sample[UkAddress]),
                disposalDetailsAnswers = Some(sample[CompleteDisposalDetailsAnswers]),
                acquisitionDetailsAnswers = Some(sample[CompleteAcquisitionDetailsAnswers]),
                reliefDetailsAnswers = Some(sample[CompleteReliefDetailsAnswers]),
                exemptionAndLossesAnswers = None
              ),
              TaskListStatus.ToDo
            )
          }

          "the session data indicates that they are filling in a return and they have started the section but not complete it yet" in {
            test(
              sample[DraftSingleDisposalReturn].copy(
                triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                  .copy(individualUserType = Some(Self), countryOfResidence = Country.uk),
                propertyAddress = None,
                disposalDetailsAnswers = Some(sample[CompleteDisposalDetailsAnswers]),
                acquisitionDetailsAnswers = Some(sample[CompleteAcquisitionDetailsAnswers]),
                reliefDetailsAnswers = Some(sample[CompleteReliefDetailsAnswers]),
                exemptionAndLossesAnswers = Some(sample[IncompleteExemptionAndLossesAnswers])
              ),
              TaskListStatus.InProgress
            )
          }

          "the session data indicates that they are filling in a return and they have completed the section" in {
            test(
              sample[DraftSingleDisposalReturn].copy(
                triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                  .copy(
                    individualUserType = Some(Self),
                    countryOfResidence = Country.uk
                  ),
                propertyAddress = Some(sample[UkAddress]),
                disposalDetailsAnswers = Some(sample[CompleteDisposalDetailsAnswers]),
                acquisitionDetailsAnswers = Some(sample[CompleteAcquisitionDetailsAnswers]),
                reliefDetailsAnswers = Some(sample[CompleteReliefDetailsAnswers]),
                exemptionAndLossesAnswers = Some(sample[CompleteExemptionAndLossesAnswers])
              ),
              TaskListStatus.Complete
            )
          }

          val triageAnswers = sample[CompleteSingleDisposalTriageAnswers].copy(
            individualUserType = Some(Self),
            countryOfResidence = Country.uk
          )

          val taxYearStartYear: String =
            triageAnswers
              .fold(
                _.disposalDate.map(_.taxYear.startDateInclusive.getYear),
                c => Some(c.disposalDate.taxYear.startDateInclusive.getYear)
              )
              .map(_.toString)
              .getOrElse("2020")

          "the reliefs section is complete but the gain or loss after reliefs section is not complete for a further return" in {
            test(
              sample[DraftSingleDisposalReturn].copy(
                triageAnswers = triageAnswers,
                propertyAddress = Some(sample[UkAddress]),
                disposalDetailsAnswers = Some(sample[CompleteDisposalDetailsAnswers]),
                acquisitionDetailsAnswers = Some(sample[CompleteAcquisitionDetailsAnswers]),
                reliefDetailsAnswers = Some(sample[CompleteReliefDetailsAnswers]),
                gainOrLossAfterReliefs = None,
                exemptionAndLossesAnswers = None
              ),
              TaskListStatus.CannotStart,
              Some(List(sample[ReturnSummary].copy(taxYear = taxYearStartYear)))
            )
          }

          "the reliefs section is complete and the gain or loss after reliefs section is complete for a further return" in {
            test(
              sample[DraftSingleDisposalReturn].copy(
                triageAnswers = triageAnswers,
                propertyAddress = Some(sample[UkAddress]),
                disposalDetailsAnswers = Some(sample[CompleteDisposalDetailsAnswers]),
                acquisitionDetailsAnswers = Some(sample[CompleteAcquisitionDetailsAnswers]),
                reliefDetailsAnswers = Some(sample[CompleteReliefDetailsAnswers]),
                gainOrLossAfterReliefs = Some(sample[AmountInPence]),
                exemptionAndLossesAnswers = None
              ),
              TaskListStatus.ToDo,
              Some(List(sample[ReturnSummary].copy(taxYear = taxYearStartYear)))
            )
          }

        }

        "display the page with the proper year to date liability section status" when {

          def test(
            draftReturn: DraftSingleDisposalReturn,
            expectedStatus: TaskListStatus
          ): Unit =
            testStateOfSection(draftReturn)(
              "enterCgtLiability",
              messageFromMessageKey("task-list.enter-cgt-liability.link"),
              yeartodatelliability.routes.YearToDateLiabilityController
                .checkYourAnswers(),
              expectedStatus
            )

          "the session data indicates that the exemptions and losses section has not yet been started" in {
            test(
              sample[DraftSingleDisposalReturn].copy(
                triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                  .copy(individualUserType = Some(Self), countryOfResidence = Country.uk),
                propertyAddress = Some(sample[UkAddress]),
                disposalDetailsAnswers = Some(sample[CompleteDisposalDetailsAnswers]),
                acquisitionDetailsAnswers = Some(sample[CompleteAcquisitionDetailsAnswers]),
                reliefDetailsAnswers = Some(sample[CompleteReliefDetailsAnswers]),
                exemptionAndLossesAnswers = None,
                yearToDateLiabilityAnswers = None
              ),
              TaskListStatus.CannotStart
            )
          }

          "the session data indicates that the reliefs section is has not yet been completed" in {
            test(
              sample[DraftSingleDisposalReturn].copy(
                triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                  .copy(individualUserType = Some(Self), countryOfResidence = Country.uk),
                propertyAddress = Some(sample[UkAddress]),
                disposalDetailsAnswers = Some(sample[CompleteDisposalDetailsAnswers]),
                acquisitionDetailsAnswers = Some(sample[CompleteAcquisitionDetailsAnswers]),
                reliefDetailsAnswers = Some(sample[CompleteReliefDetailsAnswers]),
                exemptionAndLossesAnswers = Some(sample[IncompleteExemptionAndLossesAnswers]),
                yearToDateLiabilityAnswers = None
              ),
              TaskListStatus.CannotStart
            )
          }

          "the reliefs section has been completed and the section has not been started yet" in {
            test(
              sample[DraftSingleDisposalReturn].copy(
                triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                  .copy(individualUserType = Some(Self), countryOfResidence = Country.uk),
                propertyAddress = Some(sample[UkAddress]),
                disposalDetailsAnswers = Some(sample[CompleteDisposalDetailsAnswers]),
                acquisitionDetailsAnswers = Some(sample[CompleteAcquisitionDetailsAnswers]),
                reliefDetailsAnswers = Some(sample[CompleteReliefDetailsAnswers]),
                exemptionAndLossesAnswers = Some(sample[CompleteExemptionAndLossesAnswers]),
                yearToDateLiabilityAnswers = None
              ),
              TaskListStatus.ToDo
            )
          }

          "the session data indicates that they are filling in a return and they have started the section but not complete it yet" in {
            test(
              sample[DraftSingleDisposalReturn].copy(
                triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                  .copy(individualUserType = Some(Self), countryOfResidence = Country.uk),
                propertyAddress = Some(sample[UkAddress]),
                disposalDetailsAnswers = Some(sample[CompleteDisposalDetailsAnswers]),
                acquisitionDetailsAnswers = Some(sample[CompleteAcquisitionDetailsAnswers]),
                reliefDetailsAnswers = Some(sample[CompleteReliefDetailsAnswers]),
                exemptionAndLossesAnswers = Some(sample[CompleteExemptionAndLossesAnswers]),
                yearToDateLiabilityAnswers = Some(sample[IncompleteCalculatedYTDAnswers])
              ),
              TaskListStatus.InProgress
            )
          }

          "the session data indicates that they are filling in a return and they have completed the section" in {
            test(
              sample[DraftSingleDisposalReturn].copy(
                triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                  .copy(individualUserType = Some(Self), countryOfResidence = Country.uk),
                propertyAddress = None,
                disposalDetailsAnswers = Some(sample[CompleteDisposalDetailsAnswers]),
                acquisitionDetailsAnswers = Some(sample[CompleteAcquisitionDetailsAnswers]),
                reliefDetailsAnswers = Some(sample[CompleteReliefDetailsAnswers]),
                exemptionAndLossesAnswers = Some(sample[CompleteExemptionAndLossesAnswers]),
                yearToDateLiabilityAnswers = Some(sample[CompleteCalculatedYTDAnswers])
              ),
              TaskListStatus.Complete
            )
          }
        }

        "display the page with the PROPER initial gains and losses section STATUS" when {

          def test(
            draftReturn: DraftSingleDisposalReturn,
            expectedStatus: TaskListStatus
          ): Unit =
            testStateOfSection(draftReturn)(
              "initialGainOrLoss",
              messageFromMessageKey("task-list.enter-initial-gain-or-loss.link"),
              initialgainorloss.routes.InitialGainOrLossController
                .enterInitialGainOrLoss(),
              expectedStatus
            )

          "the session data indicates that the country of residence is United Kingdom" in {

            testSectionNonExistent(
              sample[DraftSingleDisposalReturn].copy(
                triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                  .copy(
                    assetType = AssetType.Residential,
                    countryOfResidence = Country.uk,
                    individualUserType = Some(Self)
                  ),
                reliefDetailsAnswers = Some(sample[IncompleteReliefDetailsAnswers]),
                acquisitionDetailsAnswers = Some(sample[CompleteAcquisitionDetailsAnswers]).map(answers =>
                  answers.copy(acquisitionDate = AcquisitionDate(LocalDate.of(2014, 10, 1)))
                )
              )
            )(
              "initialGainOrLoss"
            )
          }

          "the session data indicates that the country of residence is NOT United Kingdom and RESIDENTIAL property was bought BEFORE 01/04/2015 " +
            "and the user has not completed the section yet" in {
              test(
                sample[DraftSingleDisposalReturn].copy(
                  triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                    .copy(
                      assetType = AssetType.Residential,
                      countryOfResidence = Country("TR"),
                      individualUserType = Some(Self)
                    ),
                  disposalDetailsAnswers = Some(sample[CompleteDisposalDetailsAnswers]),
                  acquisitionDetailsAnswers = Some(sample[CompleteAcquisitionDetailsAnswers]).map(answers =>
                    answers.copy(acquisitionDate = AcquisitionDate(LocalDate.of(2014, 10, 1)))
                  ),
                  initialGainOrLoss = None
                ),
                TaskListStatus.ToDo
              )
            }

          "the session data indicates that the country of residence is NOT United Kingdom and RESIDENTIAL property was bought BEFORE 01/04/2015 " +
            "and the user has completed the section" in {
              test(
                sample[DraftSingleDisposalReturn].copy(
                  triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                    .copy(
                      assetType = AssetType.Residential,
                      countryOfResidence = Country("TR"),
                      individualUserType = Some(Self)
                    ),
                  disposalDetailsAnswers = Some(sample[CompleteDisposalDetailsAnswers]),
                  acquisitionDetailsAnswers = Some(sample[CompleteAcquisitionDetailsAnswers]).map(answers =>
                    answers.copy(acquisitionDate = AcquisitionDate(LocalDate.of(2014, 10, 1)))
                  ),
                  initialGainOrLoss = Some(sample[AmountInPence])
                ),
                TaskListStatus.Complete
              )
            }

          "the session data indicates that the country of residence is NOT United Kingdom and RESIDENTIAL property was bought BEFORE 01/04/2015 " +
            "and it is a further return" in {
              testSectionNonExistent(
                sample[DraftSingleDisposalReturn].copy(
                  triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                    .copy(
                      assetType = AssetType.Residential,
                      countryOfResidence = Country("TR"),
                      individualUserType = Some(Self)
                    ),
                  disposalDetailsAnswers = Some(sample[CompleteDisposalDetailsAnswers]),
                  acquisitionDetailsAnswers = Some(sample[CompleteAcquisitionDetailsAnswers]).map(answers =>
                    answers.copy(acquisitionDate = AcquisitionDate(LocalDate.of(2014, 10, 1)))
                  ),
                  initialGainOrLoss = None
                ),
                Some(List(sample[ReturnSummary]))
              )(
                "initialGainOrLoss"
              )
            }

          "the session data indicates that the country of residence is NOT United Kingdom and RESIDENTIAL property was bought BEFORE 01/04/2015 and " +
            "the user is on a period of admin journey" in {
              val dateOfDeath = LocalDate.of(2014, 10, 1)
              testSectionNonExistent(
                sample[DraftSingleDisposalReturn].copy(
                  triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                    .copy(
                      assetType = AssetType.Residential,
                      countryOfResidence = Country("TR"),
                      individualUserType = Some(PersonalRepresentativeInPeriodOfAdmin)
                    ),
                  representeeAnswers =
                    Some(sample[CompleteRepresenteeAnswers].copy(dateOfDeath = Some(DateOfDeath(dateOfDeath)))),
                  disposalDetailsAnswers = Some(sample[CompleteDisposalDetailsAnswers]),
                  acquisitionDetailsAnswers = Some(sample[CompleteAcquisitionDetailsAnswers]).map(answers =>
                    answers.copy(acquisitionDate = AcquisitionDate(dateOfDeath))
                  ),
                  initialGainOrLoss = None
                )
              )("initialGainOrLoss")
            }

          "the session data indicates that the country of residence is NOT United Kingdom and NON-RESIDENTIAL property was bought" in {
            testSectionNonExistent(
              sample[DraftSingleDisposalReturn].copy(
                triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                  .copy(
                    assetType = AssetType.NonResidential,
                    countryOfResidence = Country("TR"),
                    individualUserType = Some(Self)
                  ),
                disposalDetailsAnswers = Some(sample[CompleteDisposalDetailsAnswers]),
                acquisitionDetailsAnswers = Some(sample[CompleteAcquisitionDetailsAnswers]).map(answers =>
                  answers.copy(acquisitionDate = AcquisitionDate(LocalDate.of(2014, 10, 1)))
                ),
                initialGainOrLoss = None
              )
            )("initialGainOrLoss")
          }

          "the session data indicates that the country of residence is NOT United Kingdom but acquisition date is AFTER 01/04/2015 for RESIDENTIAL property" in {
            testSectionNonExistent(
              sample[DraftSingleDisposalReturn].copy(
                triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                  .copy(
                    assetType = AssetType.Residential,
                    countryOfResidence = Country("TR"),
                    individualUserType = Some(Self)
                  ),
                disposalDetailsAnswers = Some(sample[CompleteDisposalDetailsAnswers]),
                acquisitionDetailsAnswers = Some(sample[CompleteAcquisitionDetailsAnswers]).map(answers =>
                  answers.copy(acquisitionDate = AcquisitionDate(LocalDate.of(2020, 10, 1)))
                )
              )
            )("initialGainOrLoss")
          }
        }

        "display the page with a Save and come back later link" when {

          "the session data indicates that they are filling in a return" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                SessionData.empty.copy(
                  journeyStatus = Some(
                    sample[FillingOutReturn].copy(
                      draftReturn = sample[DraftSingleDisposalReturn].copy(
                        supportingEvidenceAnswers = None,
                        yearToDateLiabilityAnswers = None
                      ),
                      amendReturnData = None
                    )
                  )
                )
              )
            }

            val result = performAction()
            status(result) shouldBe OK

            val doc: Document = parse(contentAsString(result))
            doc.select("h1").text shouldBe messageFromMessageKey("service.title")
            doc
              .select("a#saveAndComeBackLater")
              .attr("href")       shouldBe routes.DraftReturnSavedController
              .draftReturnSaved()
              .url
          }

        }

        "display the page without a Save and come back later link" when {

          "the session data indicates that they are amending a return" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                SessionData.empty.copy(
                  journeyStatus = Some(
                    sample[FillingOutReturn].copy(
                      draftReturn = sample[DraftSingleDisposalReturn].copy(
                        supportingEvidenceAnswers = None,
                        yearToDateLiabilityAnswers = None
                      ),
                      amendReturnData = Some(sample[AmendReturnData])
                    )
                  )
                )
              )
            }

            val result = performAction()
            status(result) shouldBe OK

            val doc: Document = parse(contentAsString(result))
            doc.select("h1").text                        shouldBe messageFromMessageKey("service.title")
            doc.select("a#saveAndComeBackLater").isEmpty shouldBe true
          }

        }

      }

      "handling requests to display the multiple disposal task list page" must {

        "display the page with the proper person represented section status" when {

          "the individual user type is personal representative or capacitor and" when {

            "the section has not been started" in {
              val draftReturn =
                sample[DraftMultipleDisposalsReturn].copy(
                  triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                    .copy(individualUserType = Some(PersonalRepresentative)),
                  representeeAnswers = None
                )

              testStateOfSection(
                draftReturn
              )(
                "representee",
                messageFromMessageKey("task-list.representee.link"),
                representee.routes.RepresenteeController.checkYourAnswers(),
                TaskListStatus.ToDo,
                _.select("div.notice")
                  .contains(messageFromMessageKey("task-list.incompleteTriage"))
              )
            }

            "the section is incomplete" in {
              val draftReturn =
                sample[DraftMultipleDisposalsReturn].copy(
                  triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                    .copy(individualUserType = Some(PersonalRepresentative)),
                  representeeAnswers = Some(sample[IncompleteRepresenteeAnswers])
                )

              testStateOfSection(
                draftReturn
              )(
                "representee",
                messageFromMessageKey("task-list.representee.link"),
                representee.routes.RepresenteeController.checkYourAnswers(),
                TaskListStatus.InProgress,
                _.select("div.notice")
                  .contains(messageFromMessageKey("task-list.incompleteTriage"))
              )
            }

            "the section is complete" in {
              val draftReturn =
                sample[DraftMultipleDisposalsReturn].copy(
                  triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                    .copy(individualUserType = Some(PersonalRepresentative)),
                  representeeAnswers = Some(sample[CompleteRepresenteeAnswers])
                )

              testStateOfSection(
                draftReturn
              )(
                "representee",
                messageFromMessageKey("task-list.representee.link"),
                representee.routes.RepresenteeController.checkYourAnswers(),
                TaskListStatus.Complete
              )
            }

          }

        }

        "display the page with the proper multiple disposal triage section status" when {

          "the session data indicates that they are filling in a return and the triage section is incomplete" in {
            val incompleteTriage = sample[DraftMultipleDisposalsReturn].copy(
              triageAnswers = sample[IncompleteMultipleDisposalsTriageAnswers]
                .copy(individualUserType = Some(Self))
            )

            testStateOfSection(
              incompleteTriage
            )(
              "canTheyUseOurService",
              messageFromMessageKey("task-list.triage.link"),
              triage.routes.MultipleDisposalsTriageController.checkYourAnswers(),
              TaskListStatus.InProgress,
              _.select("div.notice")
                .contains(messageFromMessageKey("task-list.incompleteTriage"))
            )

          }

          "the session data indicates that they are filling in a return and the triage section is complete" in {
            val completeTriage = sample[DraftMultipleDisposalsReturn].copy(
              triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                .copy(individualUserType = Some(Self))
            )

            testStateOfSection(
              completeTriage
            )(
              "canTheyUseOurService",
              messageFromMessageKey("task-list.triage.link"),
              triage.routes.MultipleDisposalsTriageController.checkYourAnswers(),
              TaskListStatus.Complete
            )
          }

        }

        "display the page with the enter details of one property section status" when {

          "the session data indicates that they are filling in a return and the enter details of one property section is incomplete" in {

            val incompletePropertyDetails = sample[DraftMultipleDisposalsReturn].copy(
              triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers].copy(
                assetTypes = List(AssetType.Residential),
                individualUserType = Some(Self)
              ),
              examplePropertyDetailsAnswers = Some(sample[IncompleteExamplePropertyDetailsAnswers])
            )

            testStateOfSection(
              incompletePropertyDetails
            )(
              "examplePropertyDetails",
              messageFromMessageKey("task-list.enter-example-property-address.link"),
              address.routes.PropertyDetailsController.checkYourAnswers(),
              TaskListStatus.InProgress,
              _.select("div.notice").contains(messageFromMessageKey("task-list.incompleteTriage"))
            )

          }

          "the session data indicates that they are filling in a return and the enter details of one property section is complete" in {

            val completePropertyDetails = sample[DraftMultipleDisposalsReturn].copy(
              triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers].copy(
                assetTypes = List(AssetType.Residential),
                individualUserType = Some(Self)
              ),
              examplePropertyDetailsAnswers = Some(sample[CompleteExamplePropertyDetailsAnswers])
            )

            testStateOfSection(
              completePropertyDetails
            )(
              "examplePropertyDetails",
              messageFromMessageKey("task-list.enter-example-property-address.link"),
              address.routes.PropertyDetailsController.checkYourAnswers(),
              TaskListStatus.Complete
            )

          }

        }

        "display the page with the proper gain or loss after reliefs section status" when {

          def test(
            draftReturn: DraftMultipleDisposalsReturn,
            previousSentReturns: Option[List[ReturnSummary]],
            amendReturnData: Option[AmendReturnData],
            expectedStatus: TaskListStatus
          ): Unit =
            testStateOfSection(draftReturn)(
              "gainOrLossAfterReliefs",
              messageFromMessageKey("task-list.gain-or-loss-after-reliefs.link"),
              gainorlossafterreliefs.routes.GainOrLossAfterReliefsController.checkYourAnswers(),
              expectedStatus,
              previousSentReturns = previousSentReturns,
              amendReturnData = amendReturnData
            )

          val triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers].copy(
            individualUserType = Some(Self)
          )

          val taxYearStartYear: String =
            triageAnswers
              .fold(
                _.taxYear.map(_.startDateInclusive.getYear),
                c => Some(c.taxYear.startDateInclusive.getYear)
              )
              .map(_.toString)
              .getOrElse("2020")

          val prerequisiteDraftReturn =
            sample[DraftMultipleDisposalsReturn].copy(
              triageAnswers = triageAnswers,
              examplePropertyDetailsAnswers = Some(sample[CompleteExamplePropertyDetailsAnswers]),
              exemptionAndLossesAnswers = None,
              gainOrLossAfterReliefs = None
            )

          "the return is a further return and" when {

            "the example property address section is not been started" in {
              test(
                prerequisiteDraftReturn.copy(examplePropertyDetailsAnswers = None),
                Some(List(sample[ReturnSummary].copy(taxYear = taxYearStartYear))),
                None,
                TaskListStatus.CannotStart
              )
            }

            "the example property address section is not complete" in {
              test(
                prerequisiteDraftReturn.copy(examplePropertyDetailsAnswers = None),
                Some(List(sample[ReturnSummary].copy(taxYear = taxYearStartYear))),
                None,
                TaskListStatus.CannotStart
              )
            }

            "the required sections are now complete but the GLAR section has not been started yet" in {
              test(
                prerequisiteDraftReturn.copy(gainOrLossAfterReliefs = None),
                Some(List(sample[ReturnSummary].copy(taxYear = taxYearStartYear))),
                None,
                TaskListStatus.ToDo
              )
            }

            "the required sections are now complete but the GLAR section has not been completed" in {
              test(
                prerequisiteDraftReturn.copy(gainOrLossAfterReliefs = Some(sample[AmountInPence])),
                Some(List(sample[ReturnSummary].copy(taxYear = taxYearStartYear))),
                None,
                TaskListStatus.Complete
              )
            }

          }

          "the return is not a further return or an amend return" in {
            testSectionNonExistent(prerequisiteDraftReturn.copy(gainOrLossAfterReliefs = None), None, None)(
              "gainOrLossAfterReliefs"
            )
          }

          "the return is an amend return where gain or loss after reliefs shouldn't be shown" in {
            testSectionNonExistent(
              prerequisiteDraftReturn.copy(gainOrLossAfterReliefs = None),
              None,
              Some(
                sample[AmendReturnData].copy(
                  shouldDisplayGainOrLossAfterReliefs = false
                )
              )
            )(
              "gainOrLossAfterReliefs"
            )
          }

          "the return is an amend return where gain or loss after reliefs should be shown" in {
            test(
              prerequisiteDraftReturn.copy(gainOrLossAfterReliefs = None),
              None,
              Some(
                sample[AmendReturnData].copy(
                  shouldDisplayGainOrLossAfterReliefs = true
                )
              ),
              TaskListStatus.ToDo
            )
          }

        }

        "display the page with the enter losses and exemptions section status" when {

          "the session data indicates that they are filling in a return and the enter losses and exemptions section is incomplete" in {
            val incompleteExemptionAndLossesAnswers =
              sample[DraftMultipleDisposalsReturn].copy(
                triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                  .copy(individualUserType = Some(Self)),
                examplePropertyDetailsAnswers = Some(sample[CompleteExamplePropertyDetailsAnswers]),
                exemptionAndLossesAnswers = Some(sample[IncompleteExemptionAndLossesAnswers])
              )

            testStateOfSection(
              incompleteExemptionAndLossesAnswers
            )(
              "exemptionsAndLosses",
              messageFromMessageKey("task-list.exemptions-and-losses.link"),
              exemptionandlosses.routes.ExemptionAndLossesController
                .checkYourAnswers(),
              TaskListStatus.InProgress,
              _.select("div.notice")
                .contains(messageFromMessageKey("task-list.incompleteTriage"))
            )

          }

          "the session data indicates that they are filling in a return and the enter losses and exemptions section is complete" in {
            val completeExemptionAndLossesAnswers =
              sample[DraftMultipleDisposalsReturn].copy(
                triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                  .copy(individualUserType = Some(Self)),
                examplePropertyDetailsAnswers = Some(sample[CompleteExamplePropertyDetailsAnswers]),
                exemptionAndLossesAnswers = Some(sample[CompleteExemptionAndLossesAnswers])
              )

            testStateOfSection(
              completeExemptionAndLossesAnswers
            )(
              "exemptionsAndLosses",
              messageFromMessageKey("task-list.exemptions-and-losses.link"),
              exemptionandlosses.routes.ExemptionAndLossesController
                .checkYourAnswers(),
              TaskListStatus.Complete
            )

          }

          val triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
            .copy(individualUserType = Some(Self), countryOfResidence = Country.uk)

          val taxYearStartYear: String =
            triageAnswers
              .fold(
                _.taxYear.map(_.startDateInclusive.getYear),
                c => Some(c.taxYear.startDateInclusive.getYear)
              )
              .map(_.toString)
              .getOrElse("2020")

          "the property details section is complete but the gain or loss after reliefs section is not complete for a further return" in {
            testStateOfSection(
              sample[DraftMultipleDisposalsReturn].copy(
                triageAnswers = triageAnswers,
                examplePropertyDetailsAnswers = Some(sample[CompleteExamplePropertyDetailsAnswers]),
                gainOrLossAfterReliefs = None,
                exemptionAndLossesAnswers = None
              )
            )(
              "exemptionsAndLosses",
              messageFromMessageKey("task-list.exemptions-and-losses.link"),
              exemptionandlosses.routes.ExemptionAndLossesController
                .checkYourAnswers(),
              TaskListStatus.CannotStart,
              previousSentReturns = Some(List(sample[ReturnSummary].copy(taxYear = taxYearStartYear)))
            )
          }

          "the reliefs section is complete and the gain or loss after reliefs section is complete for a further return" in {
            testStateOfSection(
              sample[DraftMultipleDisposalsReturn].copy(
                triageAnswers = triageAnswers,
                examplePropertyDetailsAnswers = Some(sample[CompleteExamplePropertyDetailsAnswers]),
                gainOrLossAfterReliefs = Some(sample[AmountInPence]),
                exemptionAndLossesAnswers = None
              )
            )(
              "exemptionsAndLosses",
              messageFromMessageKey("task-list.exemptions-and-losses.link"),
              exemptionandlosses.routes.ExemptionAndLossesController
                .checkYourAnswers(),
              TaskListStatus.ToDo,
              previousSentReturns = Some(List(sample[ReturnSummary].copy(taxYear = taxYearStartYear)))
            )
          }

        }

        "display the page with the enter capital gains tax liability so far this tax year section status" when {

          "the session data indicates that they are filling in a return and" +
            " the enter capital gains tax liability so far this tax year section is incomplete" in {
              val incompleteNonCalculatedYTDAnswers =
                sample[DraftMultipleDisposalsReturn].copy(
                  triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                    .copy(individualUserType = Some(Self)),
                  examplePropertyDetailsAnswers = Some(sample[CompleteExamplePropertyDetailsAnswers]),
                  exemptionAndLossesAnswers = Some(sample[CompleteExemptionAndLossesAnswers]),
                  yearToDateLiabilityAnswers = Some(sample[NonCalculatedYTDAnswers])
                )

              testStateOfSection(
                incompleteNonCalculatedYTDAnswers
              )(
                "enterCgtLiability",
                messageFromMessageKey("task-list.enter-cgt-liability.link"),
                yeartodatelliability.routes.YearToDateLiabilityController
                  .checkYourAnswers(),
                TaskListStatus.Complete
              )

            }

        }

        "display the page with the check and send return, pay any tax due section status" when {

          "the session data indicates that they are filling in a return and" +
            " the check and send return, pay any tax due section is incomplete" in {
              val checkAllAnswersAndSubmitAnswers =
                sample[DraftMultipleDisposalsReturn].copy(
                  triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                    .copy(individualUserType = Some(Self)),
                  examplePropertyDetailsAnswers = Some(sample[CompleteExamplePropertyDetailsAnswers]),
                  exemptionAndLossesAnswers = Some(sample[CompleteExemptionAndLossesAnswers]),
                  yearToDateLiabilityAnswers = Some(sample[CompleteCalculatedYTDAnswers]),
                  supportingEvidenceAnswers = Some(sample[CompleteSupportingEvidenceAnswers])
                )

              testStateOfSection(
                checkAllAnswersAndSubmitAnswers
              )(
                "checkAndSendReturn",
                messageFromMessageKey("task-list.check-and-send-return.link"),
                routes.CheckAllAnswersAndSubmitController.checkAllAnswers(),
                TaskListStatus.ToDo
              )

            }

        }

      }

      "handling requests to display the single indirect disposal task list page" must {

        "display the page with the proper person represented section status" when {

          "the individual user type is personal representative or capacitor and" when {

            "the section has not been started" in {
              val draftReturn =
                sample[DraftSingleIndirectDisposalReturn].copy(
                  triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                    .copy(individualUserType = Some(PersonalRepresentative)),
                  representeeAnswers = None
                )

              testStateOfSection(
                draftReturn
              )(
                "representee",
                messageFromMessageKey("task-list.representee.link"),
                representee.routes.RepresenteeController.checkYourAnswers(),
                TaskListStatus.ToDo,
                _.select("div.notice")
                  .contains(messageFromMessageKey("task-list.incompleteTriage"))
              )
            }

            "the section is incomplete" in {
              val draftReturn =
                sample[DraftSingleIndirectDisposalReturn].copy(
                  triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                    .copy(individualUserType = Some(PersonalRepresentative)),
                  representeeAnswers = Some(sample[IncompleteRepresenteeAnswers])
                )

              testStateOfSection(
                draftReturn
              )(
                "representee",
                messageFromMessageKey("task-list.representee.link"),
                representee.routes.RepresenteeController.checkYourAnswers(),
                TaskListStatus.InProgress,
                _.select("div.notice")
                  .contains(messageFromMessageKey("task-list.incompleteTriage"))
              )
            }

            "the section is complete" in {
              val draftReturn =
                sample[DraftSingleIndirectDisposalReturn].copy(
                  triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                    .copy(individualUserType = Some(PersonalRepresentative)),
                  representeeAnswers = Some(sample[CompleteRepresenteeAnswers])
                )

              testStateOfSection(
                draftReturn
              )(
                "representee",
                messageFromMessageKey("task-list.representee.link"),
                representee.routes.RepresenteeController.checkYourAnswers(),
                TaskListStatus.Complete
              )
            }

          }

        }

        "display the page with the proper single disposal triage section status" when {

          "the session data indicates that they are filling in a return and the triage section is incomplete" in {
            val incompleteTriage =
              sample[DraftSingleIndirectDisposalReturn].copy(
                triageAnswers = sample[IncompleteSingleDisposalTriageAnswers]
                  .copy(individualUserType = Some(Self))
              )
            testStateOfSection(
              incompleteTriage
            )(
              "canTheyUseOurService",
              messageFromMessageKey("task-list.triage.link"),
              triage.routes.SingleDisposalsTriageController.checkYourAnswers(),
              TaskListStatus.InProgress,
              _.select("div.notice")
                .contains(messageFromMessageKey("task-list.incompleteTriage"))
            )

          }

          "the session data indicates that they are filling in a return and the triage section is complete" in {
            testStateOfSection(
              sample[DraftSingleIndirectDisposalReturn]
                .copy(triageAnswers =
                  sample[CompleteSingleDisposalTriageAnswers]
                    .copy(individualUserType = Some(Self))
                )
            )(
              "canTheyUseOurService",
              messageFromMessageKey("task-list.triage.link"),
              triage.routes.SingleDisposalsTriageController.checkYourAnswers(),
              TaskListStatus.Complete
            )
          }

        }

        "display the page with the proper Enter company details section status" when {

          "the session data indicates that they are filling in a return and enter company details is todo" in {
            testStateOfSection(
              sample[DraftSingleIndirectDisposalReturn]
                .copy(
                  triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                    .copy(individualUserType = Some(Self)),
                  companyAddress = None
                )
            )(
              "companyDetails",
              messageFromMessageKey("task-list.enter-company-details.link"),
              address.routes.CompanyDetailsController.checkYourAnswers(),
              TaskListStatus.ToDo
            )
          }

          "the session data indicates that they are filling in a return and enter property address is complete" in {
            testStateOfSection(
              sample[DraftSingleIndirectDisposalReturn].copy(
                triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                  .copy(individualUserType = Some(Self)),
                companyAddress = Some(sample[Address])
              )
            )(
              "companyDetails",
              messageFromMessageKey("task-list.enter-company-details.link"),
              address.routes.CompanyDetailsController.checkYourAnswers(),
              TaskListStatus.Complete
            )
          }

        }

        "display the page with the proper disposal details section status" when {

          "the session data indicates that they are filling in a return and the section has not been started yet is todo" in {
            testStateOfSection(
              sample[DraftSingleIndirectDisposalReturn]
                .copy(
                  triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                    .copy(individualUserType = Some(Self)),
                  disposalDetailsAnswers = None
                )
            )(
              "disposalDetails",
              messageFromMessageKey("task-list.disposals-details.link"),
              disposaldetails.routes.DisposalDetailsController.checkYourAnswers(),
              TaskListStatus.ToDo
            )
          }

          "the session data indicates that they are filling in a return and they have started the section but not complete it yet" in {
            testStateOfSection(
              sample[DraftSingleIndirectDisposalReturn]
                .copy(
                  triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                    .copy(individualUserType = Some(Self)),
                  disposalDetailsAnswers = Some(sample[IncompleteDisposalDetailsAnswers])
                )
            )(
              "disposalDetails",
              messageFromMessageKey("task-list.disposals-details.link"),
              disposaldetails.routes.DisposalDetailsController.checkYourAnswers(),
              TaskListStatus.InProgress
            )
          }

          "the session data indicates that they are filling in a return and they have completed the section" in {
            testStateOfSection(
              sample[DraftSingleIndirectDisposalReturn].copy(
                triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                  .copy(individualUserType = Some(Self)),
                disposalDetailsAnswers = Some(sample[CompleteDisposalDetailsAnswers])
              )
            )(
              "disposalDetails",
              messageFromMessageKey("task-list.disposals-details.link"),
              disposaldetails.routes.DisposalDetailsController.checkYourAnswers(),
              TaskListStatus.Complete
            )
          }

        }

        "display the page with the proper acquisition details section status" when {

          "the session data indicates that they are filling in a return and the section has not been started yet is todo" in {
            testStateOfSection(
              sample[DraftSingleIndirectDisposalReturn]
                .copy(
                  triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                    .copy(individualUserType = Some(Self)),
                  acquisitionDetailsAnswers = None
                )
            )(
              "acquisitionDetails",
              messageFromMessageKey("task-list.acquisition-details.link"),
              acquisitiondetails.routes.AcquisitionDetailsController
                .checkYourAnswers(),
              TaskListStatus.ToDo
            )
          }

          "the session data indicates that they are filling in a return and they have started the section but not complete it yet" in {
            testStateOfSection(
              sample[DraftSingleIndirectDisposalReturn]
                .copy(
                  triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                    .copy(individualUserType = Some(Self)),
                  acquisitionDetailsAnswers = Some(sample[IncompleteAcquisitionDetailsAnswers])
                )
            )(
              "acquisitionDetails",
              messageFromMessageKey("task-list.acquisition-details.link"),
              acquisitiondetails.routes.AcquisitionDetailsController
                .checkYourAnswers(),
              TaskListStatus.InProgress
            )
          }

          "the session data indicates that they are filling in a return and they have completed the section" in {
            testStateOfSection(
              sample[DraftSingleIndirectDisposalReturn]
                .copy(
                  triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                    .copy(individualUserType = Some(Self)),
                  acquisitionDetailsAnswers = Some(sample[CompleteAcquisitionDetailsAnswers])
                )
            )(
              "acquisitionDetails",
              messageFromMessageKey("task-list.acquisition-details.link"),
              acquisitiondetails.routes.AcquisitionDetailsController
                .checkYourAnswers(),
              TaskListStatus.Complete
            )
          }

        }

        "display the page with the proper gain or loss after reliefs section status" when {

          def test(
            draftReturn: DraftSingleIndirectDisposalReturn,
            previousSentReturns: Option[List[ReturnSummary]],
            amendReturnData: Option[AmendReturnData],
            expectedStatus: TaskListStatus
          ): Unit =
            testStateOfSection(draftReturn)(
              "gainOrLossAfterReliefs",
              messageFromMessageKey("task-list.gain-or-loss-after-reliefs.link"),
              gainorlossafterreliefs.routes.GainOrLossAfterReliefsController.checkYourAnswers(),
              expectedStatus,
              previousSentReturns = previousSentReturns,
              amendReturnData = amendReturnData
            )

          val triageAnswers = sample[CompleteSingleDisposalTriageAnswers].copy(
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

          val prerequisiteDraftReturn =
            sample[DraftSingleIndirectDisposalReturn].copy(
              triageAnswers = triageAnswers,
              companyAddress = Some(sample[UkAddress]),
              disposalDetailsAnswers = Some(sample[CompleteDisposalDetailsAnswers]),
              acquisitionDetailsAnswers = Some(sample[CompleteAcquisitionDetailsAnswers]),
              exemptionAndLossesAnswers = None,
              gainOrLossAfterReliefs = None
            )

          "the return is a further return and" when {

            "the company address section is not been started" in {
              test(
                prerequisiteDraftReturn.copy(companyAddress = None),
                Some(List(sample[ReturnSummary].copy(taxYear = taxYearStartYear))),
                None,
                TaskListStatus.CannotStart
              )
            }

            "the disposal details section is not been started" in {
              test(
                prerequisiteDraftReturn.copy(disposalDetailsAnswers = None),
                Some(List(sample[ReturnSummary].copy(taxYear = taxYearStartYear))),
                None,
                TaskListStatus.CannotStart
              )
            }

            "the disposal details section is not complete" in {
              test(
                prerequisiteDraftReturn.copy(disposalDetailsAnswers = Some(sample[IncompleteDisposalDetailsAnswers])),
                Some(List(sample[ReturnSummary].copy(taxYear = taxYearStartYear))),
                None,
                TaskListStatus.CannotStart
              )
            }

            "the acquisition details section is not been started" in {
              test(
                prerequisiteDraftReturn.copy(acquisitionDetailsAnswers = None),
                Some(List(sample[ReturnSummary].copy(taxYear = taxYearStartYear))),
                None,
                TaskListStatus.CannotStart
              )
            }

            "the acquisition details section is not complete" in {
              test(
                prerequisiteDraftReturn
                  .copy(acquisitionDetailsAnswers = Some(sample[IncompleteAcquisitionDetailsAnswers])),
                Some(List(sample[ReturnSummary].copy(taxYear = taxYearStartYear))),
                None,
                TaskListStatus.CannotStart
              )
            }

            "the required sections are now complete but the GLAR section has not been started yet" in {
              test(
                prerequisiteDraftReturn.copy(gainOrLossAfterReliefs = None),
                Some(List(sample[ReturnSummary].copy(taxYear = taxYearStartYear))),
                None,
                TaskListStatus.ToDo
              )
            }

            "the required sections are now complete but the GLAR section has not been completed" in {
              test(
                prerequisiteDraftReturn.copy(gainOrLossAfterReliefs = Some(sample[AmountInPence])),
                Some(List(sample[ReturnSummary].copy(taxYear = taxYearStartYear))),
                None,
                TaskListStatus.Complete
              )
            }

          }

          "the return is not a further return or an amend return" in {
            testSectionNonExistent(prerequisiteDraftReturn.copy(gainOrLossAfterReliefs = None), None, None)(
              "gainOrLossAfterReliefs"
            )
          }

          "the return is an amend return where gain or loss after reliefs shouldn't be shown" in {
            testSectionNonExistent(
              prerequisiteDraftReturn.copy(gainOrLossAfterReliefs = None),
              Some(List(sample[ReturnSummary])),
              Some(
                sample[AmendReturnData].copy(
                  shouldDisplayGainOrLossAfterReliefs = false
                )
              )
            )(
              "gainOrLossAfterReliefs"
            )
          }

          "the return is an amend return where gain or loss after reliefs should be shown" in {
            test(
              prerequisiteDraftReturn.copy(gainOrLossAfterReliefs = None),
              Some(List(sample[ReturnSummary])),
              Some(
                sample[AmendReturnData].copy(
                  shouldDisplayGainOrLossAfterReliefs = true
                )
              ),
              TaskListStatus.ToDo
            )
          }

        }

        "display the page with the proper exemptions and losses section status" when {

          def test(
            draftReturn: DraftSingleIndirectDisposalReturn,
            expectedStatus: TaskListStatus,
            previousSentReturns: Option[List[ReturnSummary]] = None
          ): Unit =
            testStateOfSection(draftReturn)(
              "exemptionsAndLosses",
              messageFromMessageKey("task-list.exemptions-and-losses.link"),
              exemptionandlosses.routes.ExemptionAndLossesController
                .checkYourAnswers(),
              expectedStatus,
              previousSentReturns = previousSentReturns
            )

          "the session data indicates that the company address details, disposal details and acquisition details have not yet been completed" in {
            List(
              None                                           -> None,
              Some(sample[IncompleteDisposalDetailsAnswers]) -> None,
              None                                           -> Some(sample[IncompleteAcquisitionDetailsAnswers]),
              Some(sample[IncompleteDisposalDetailsAnswers]) -> Some(
                sample[IncompleteAcquisitionDetailsAnswers]
              ),
              Some(sample[IncompleteDisposalDetailsAnswers]) -> Some(
                sample[CompleteAcquisitionDetailsAnswers]
              ),
              Some(sample[CompleteDisposalDetailsAnswers])   -> Some(
                sample[IncompleteAcquisitionDetailsAnswers]
              )
            ).foreach { case (disposalDetailsAnswers, acquisitionDetailsAnswers) =>
              withClue(
                s"For $disposalDetailsAnswers and $acquisitionDetailsAnswers:"
              ) {
                test(
                  sample[DraftSingleIndirectDisposalReturn].copy(
                    triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                      .copy(individualUserType = Some(Self)),
                    companyAddress = Some(sample[Address]),
                    disposalDetailsAnswers = disposalDetailsAnswers,
                    acquisitionDetailsAnswers = acquisitionDetailsAnswers,
                    exemptionAndLossesAnswers = None
                  ),
                  TaskListStatus.CannotStart
                )
              }
            }
          }

          "the company address details, disposal details and acquisition details sections have been completed and the section has not been started yet" in {
            test(
              sample[DraftSingleIndirectDisposalReturn].copy(
                triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                  .copy(individualUserType = Some(Self)),
                companyAddress = Some(sample[Address]),
                disposalDetailsAnswers = Some(sample[CompleteDisposalDetailsAnswers]),
                acquisitionDetailsAnswers = Some(sample[CompleteAcquisitionDetailsAnswers]),
                exemptionAndLossesAnswers = None
              ),
              TaskListStatus.ToDo
            )
          }

          "the session data indicates that they are filling in a return and they have started the section but not complete it yet" in {
            test(
              sample[DraftSingleIndirectDisposalReturn].copy(
                triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                  .copy(individualUserType = Some(Self)),
                companyAddress = Some(sample[Address]),
                disposalDetailsAnswers = Some(sample[CompleteDisposalDetailsAnswers]),
                acquisitionDetailsAnswers = Some(sample[CompleteAcquisitionDetailsAnswers]),
                exemptionAndLossesAnswers = Some(sample[IncompleteExemptionAndLossesAnswers])
              ),
              TaskListStatus.InProgress
            )
          }

          "the session data indicates that they are filling in a return and they have completed the section" in {
            test(
              sample[DraftSingleIndirectDisposalReturn].copy(
                triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                  .copy(individualUserType = Some(Self)),
                companyAddress = Some(sample[Address]),
                disposalDetailsAnswers = Some(sample[CompleteDisposalDetailsAnswers]),
                acquisitionDetailsAnswers = Some(sample[CompleteAcquisitionDetailsAnswers]),
                exemptionAndLossesAnswers = Some(sample[CompleteExemptionAndLossesAnswers])
              ),
              TaskListStatus.Complete
            )
          }

          val triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
            .copy(individualUserType = Some(Self), countryOfResidence = Country.uk)

          val taxYearStartYear: String =
            triageAnswers
              .fold(
                _.disposalDate.map(_.taxYear.startDateInclusive.getYear),
                c => Some(c.disposalDate.taxYear.startDateInclusive.getYear)
              )
              .map(_.toString)
              .getOrElse("2020")

          "the company address, disposal details and acquisition details section are complete " +
            "but the gain or loss after reliefs section is not complete for a further return" in {
              test(
                sample[DraftSingleIndirectDisposalReturn].copy(
                  triageAnswers = triageAnswers,
                  companyAddress = Some(sample[UkAddress]),
                  disposalDetailsAnswers = Some(sample[CompleteDisposalDetailsAnswers]),
                  acquisitionDetailsAnswers = Some(sample[CompleteAcquisitionDetailsAnswers]),
                  gainOrLossAfterReliefs = None,
                  exemptionAndLossesAnswers = None
                ),
                TaskListStatus.CannotStart,
                Some(List(sample[ReturnSummary].copy(taxYear = taxYearStartYear)))
              )
            }

          "the company address, disposal details and acquisition details section arecomplete " +
            "and the gain or loss after reliefs section is complete for a further return" in {
              test(
                sample[DraftSingleIndirectDisposalReturn].copy(
                  triageAnswers = triageAnswers,
                  companyAddress = Some(sample[UkAddress]),
                  disposalDetailsAnswers = Some(sample[CompleteDisposalDetailsAnswers]),
                  acquisitionDetailsAnswers = Some(sample[CompleteAcquisitionDetailsAnswers]),
                  gainOrLossAfterReliefs = Some(sample[AmountInPence]),
                  exemptionAndLossesAnswers = None
                ),
                TaskListStatus.ToDo,
                Some(List(sample[ReturnSummary].copy(taxYear = taxYearStartYear)))
              )
            }

        }

        "display the page with the proper year to date liability section status" when {

          def test(
            draftReturn: DraftSingleIndirectDisposalReturn,
            expectedStatus: TaskListStatus
          ): Unit =
            testStateOfSection(draftReturn)(
              "enterCgtLiability",
              messageFromMessageKey("task-list.enter-cgt-liability.link"),
              yeartodatelliability.routes.YearToDateLiabilityController
                .checkYourAnswers(),
              expectedStatus
            )

          "the session data indicates that the exemptions and losses section has not yet been started" in {
            test(
              sample[DraftSingleIndirectDisposalReturn].copy(
                triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                  .copy(individualUserType = Some(Self)),
                companyAddress = Some(sample[Address]),
                disposalDetailsAnswers = Some(sample[CompleteDisposalDetailsAnswers]),
                acquisitionDetailsAnswers = Some(sample[CompleteAcquisitionDetailsAnswers]),
                exemptionAndLossesAnswers = None,
                yearToDateLiabilityAnswers = None
              ),
              TaskListStatus.CannotStart
            )
          }

          "the session data indicates that the exemptions and losses section has not yet been completed" in {
            test(
              sample[DraftSingleIndirectDisposalReturn].copy(
                triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                  .copy(individualUserType = Some(Self)),
                companyAddress = Some(sample[Address]),
                disposalDetailsAnswers = Some(sample[CompleteDisposalDetailsAnswers]),
                acquisitionDetailsAnswers = Some(sample[CompleteAcquisitionDetailsAnswers]),
                exemptionAndLossesAnswers = Some(sample[IncompleteExemptionAndLossesAnswers]),
                yearToDateLiabilityAnswers = None
              ),
              TaskListStatus.CannotStart
            )
          }

          "the losses and exemption section has not started yet so we cannot start the section" in {
            test(
              sample[DraftSingleIndirectDisposalReturn].copy(
                triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                  .copy(individualUserType = Some(Self)),
                companyAddress = Some(sample[Address]),
                disposalDetailsAnswers = Some(sample[CompleteDisposalDetailsAnswers]),
                acquisitionDetailsAnswers = Some(sample[CompleteAcquisitionDetailsAnswers]),
                exemptionAndLossesAnswers = None,
                yearToDateLiabilityAnswers = None
              ),
              TaskListStatus.CannotStart
            )
          }

          "the losses and exemption section has been completed and the section has not been started yet" in {
            test(
              sample[DraftSingleIndirectDisposalReturn].copy(
                triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                  .copy(individualUserType = Some(Self)),
                companyAddress = Some(sample[Address]),
                disposalDetailsAnswers = Some(sample[CompleteDisposalDetailsAnswers]),
                acquisitionDetailsAnswers = Some(sample[CompleteAcquisitionDetailsAnswers]),
                exemptionAndLossesAnswers = Some(sample[CompleteExemptionAndLossesAnswers]),
                yearToDateLiabilityAnswers = None
              ),
              TaskListStatus.ToDo
            )
          }

          "the session data indicates that they are filling in a return and they have started the section but not complete it yet" in {
            test(
              sample[DraftSingleIndirectDisposalReturn].copy(
                triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                  .copy(individualUserType = Some(Self)),
                companyAddress = Some(sample[Address]),
                disposalDetailsAnswers = Some(sample[CompleteDisposalDetailsAnswers]),
                acquisitionDetailsAnswers = Some(sample[CompleteAcquisitionDetailsAnswers]),
                exemptionAndLossesAnswers = Some(sample[CompleteExemptionAndLossesAnswers]),
                yearToDateLiabilityAnswers = Some(sample[IncompleteCalculatedYTDAnswers])
              ),
              TaskListStatus.InProgress
            )
          }

          "the session data indicates that they are filling in a return and they have completed the section" in {
            test(
              sample[DraftSingleIndirectDisposalReturn].copy(
                triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                  .copy(individualUserType = Some(Self)),
                companyAddress = Some(sample[Address]),
                disposalDetailsAnswers = Some(sample[CompleteDisposalDetailsAnswers]),
                acquisitionDetailsAnswers = Some(sample[CompleteAcquisitionDetailsAnswers]),
                exemptionAndLossesAnswers = Some(sample[CompleteExemptionAndLossesAnswers]),
                yearToDateLiabilityAnswers = Some(sample[CompleteCalculatedYTDAnswers])
              ),
              TaskListStatus.Complete
            )
          }

        }

        "display the page with a Save and come back later link" when {

          "the session data indicates that they are filling in a return" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                SessionData.empty.copy(
                  journeyStatus = Some(
                    sample[FillingOutReturn].copy(
                      draftReturn = sample[DraftSingleIndirectDisposalReturn].copy(
                        supportingEvidenceAnswers = None,
                        yearToDateLiabilityAnswers = None
                      ),
                      amendReturnData = None
                    )
                  )
                )
              )
            }

            val result = performAction()
            status(result) shouldBe OK

            val doc: Document = parse(contentAsString(result))
            doc.select("h1").text shouldBe messageFromMessageKey("service.title")
            doc
              .select("a#saveAndComeBackLater")
              .attr("href")       shouldBe routes.DraftReturnSavedController
              .draftReturnSaved()
              .url
          }

        }

        "display the page without a Save and come back later link" when {

          "the session data indicates that they are amending a return" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                SessionData.empty.copy(
                  journeyStatus = Some(
                    sample[FillingOutReturn].copy(
                      draftReturn = sample[DraftSingleIndirectDisposalReturn].copy(
                        supportingEvidenceAnswers = None,
                        yearToDateLiabilityAnswers = None
                      ),
                      amendReturnData = Some(sample[AmendReturnData])
                    )
                  )
                )
              )
            }

            val result = performAction()
            status(result) shouldBe OK

            val doc: Document = parse(contentAsString(result))
            doc.select("h1").text                        shouldBe messageFromMessageKey("service.title")
            doc.select("a#saveAndComeBackLater").isEmpty shouldBe true
          }

        }

      }

      "handling requests to display the multiple indirect disposal task list page" must {

        "display the page with the proper person represented section status" when {

          "the individual user type is personal representative or capacitor and" when {

            "the section has not been started" in {
              val draftReturn =
                sample[DraftMultipleIndirectDisposalsReturn].copy(
                  triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                    .copy(individualUserType = Some(PersonalRepresentative)),
                  representeeAnswers = None
                )

              testStateOfSection(
                draftReturn
              )(
                "representee",
                messageFromMessageKey("task-list.representee.link"),
                representee.routes.RepresenteeController.checkYourAnswers(),
                TaskListStatus.ToDo,
                _.select("div.notice")
                  .contains(messageFromMessageKey("task-list.incompleteTriage"))
              )
            }

            "the section is incomplete" in {
              val draftReturn =
                sample[DraftMultipleIndirectDisposalsReturn].copy(
                  triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                    .copy(individualUserType = Some(PersonalRepresentative)),
                  representeeAnswers = Some(sample[IncompleteRepresenteeAnswers])
                )

              testStateOfSection(
                draftReturn
              )(
                "representee",
                messageFromMessageKey("task-list.representee.link"),
                representee.routes.RepresenteeController.checkYourAnswers(),
                TaskListStatus.InProgress,
                _.select("div.notice")
                  .contains(messageFromMessageKey("task-list.incompleteTriage"))
              )
            }

            "the section is complete" in {
              val draftReturn =
                sample[DraftMultipleIndirectDisposalsReturn].copy(
                  triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                    .copy(individualUserType = Some(PersonalRepresentative)),
                  representeeAnswers = Some(sample[CompleteRepresenteeAnswers])
                )

              testStateOfSection(
                draftReturn
              )(
                "representee",
                messageFromMessageKey("task-list.representee.link"),
                representee.routes.RepresenteeController.checkYourAnswers(),
                TaskListStatus.Complete
              )
            }

          }

        }

        "display the page with the proper multiple disposal triage section status" when {

          "the session data indicates that they are filling in a return and the triage section is incomplete" in {
            val incompleteTriage = sample[DraftMultipleIndirectDisposalsReturn].copy(
              triageAnswers = sample[IncompleteMultipleDisposalsTriageAnswers]
                .copy(individualUserType = Some(Self))
            )

            testStateOfSection(
              incompleteTriage
            )(
              "canTheyUseOurService",
              messageFromMessageKey("task-list.triage.link"),
              triage.routes.MultipleDisposalsTriageController.checkYourAnswers(),
              TaskListStatus.InProgress,
              _.select("div.notice")
                .contains(messageFromMessageKey("task-list.incompleteTriage"))
            )

          }

          "the session data indicates that they are filling in a return and the triage section is complete" in {
            val completeTriage = sample[DraftMultipleIndirectDisposalsReturn].copy(
              triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                .copy(individualUserType = Some(Self))
            )

            testStateOfSection(
              completeTriage
            )(
              "canTheyUseOurService",
              messageFromMessageKey("task-list.triage.link"),
              triage.routes.MultipleDisposalsTriageController.checkYourAnswers(),
              TaskListStatus.Complete
            )
          }

        }

        "display the page with the enter details of one company section status" when {

          val country = Country("ZA")

          "the session data indicates that they are filling in a return and" +
            " the enter details of one company section is incomplete" in {
              val multipleIndirectDisposalsReturn = sample[DraftMultipleIndirectDisposalsReturn].copy(
                triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers].copy(
                  numberOfProperties = 2,
                  countryOfResidence = country,
                  assetTypes = List(AssetType.IndirectDisposal),
                  individualUserType = Some(Self)
                ),
                exampleCompanyDetailsAnswers = Some(sample[IncompleteExampleCompanyDetailsAnswers])
              )

              testStateOfSection(
                multipleIndirectDisposalsReturn
              )(
                "exampleCompanyDetails",
                messageFromMessageKey("task-list.indirect.enter-example-company-address.link"),
                address.routes.CompanyDetailsController.checkYourAnswers(),
                TaskListStatus.InProgress,
                _.select("div.notice").contains(messageFromMessageKey("task-list.incompleteTriage"))
              )

            }

          "the session data indicates that they are filling in a return and" +
            " the enter details of one company section is complete" in {
              val multipleIndirectDisposalsReturn = sample[DraftMultipleIndirectDisposalsReturn].copy(
                triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers].copy(
                  numberOfProperties = 2,
                  countryOfResidence = country,
                  assetTypes = List(AssetType.IndirectDisposal),
                  individualUserType = Some(Self)
                ),
                exampleCompanyDetailsAnswers = Some(sample[CompleteExampleCompanyDetailsAnswers])
              )

              testStateOfSection(
                multipleIndirectDisposalsReturn
              )(
                "exampleCompanyDetails",
                messageFromMessageKey("task-list.indirect.enter-example-company-address.link"),
                address.routes.CompanyDetailsController.checkYourAnswers(),
                TaskListStatus.Complete
              )

            }

        }

        "display the page with the proper gain or loss after reliefs section status" when {

          def test(
            draftReturn: DraftMultipleIndirectDisposalsReturn,
            previousSentReturns: Option[List[ReturnSummary]],
            amendReturnData: Option[AmendReturnData],
            expectedStatus: TaskListStatus
          ): Unit =
            testStateOfSection(draftReturn)(
              "gainOrLossAfterReliefs",
              messageFromMessageKey("task-list.gain-or-loss-after-reliefs.link"),
              gainorlossafterreliefs.routes.GainOrLossAfterReliefsController.checkYourAnswers(),
              expectedStatus,
              previousSentReturns = previousSentReturns,
              amendReturnData = amendReturnData
            )

          val prerequisiteDraftReturn =
            sample[DraftMultipleIndirectDisposalsReturn].copy(
              triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers].copy(
                individualUserType = Some(Self)
              ),
              exampleCompanyDetailsAnswers = Some(sample[CompleteExampleCompanyDetailsAnswers]),
              exemptionAndLossesAnswers = None,
              gainOrLossAfterReliefs = None
            )

          val taxYearStartYear: String = prerequisiteDraftReturn.triageAnswers
            .fold(
              _.taxYear.map(_.startDateInclusive.getYear),
              c => Some(c.taxYear.startDateInclusive.getYear)
            )
            .map(_.toString)
            .getOrElse("2020")

          "the return is a further return and" when {

            "the example company details section has not been started" in {
              test(
                prerequisiteDraftReturn.copy(exampleCompanyDetailsAnswers = None),
                Some(List(sample[ReturnSummary].copy(taxYear = taxYearStartYear))),
                None,
                TaskListStatus.CannotStart
              )
            }

            "the example company details section is not complete" in {
              test(
                prerequisiteDraftReturn
                  .copy(exampleCompanyDetailsAnswers = Some(sample[IncompleteExampleCompanyDetailsAnswers])),
                Some(List(sample[ReturnSummary].copy(taxYear = taxYearStartYear))),
                None,
                TaskListStatus.CannotStart
              )
            }

            "the required section is now complete but the GLAR section has not been started yet" in {
              test(
                prerequisiteDraftReturn.copy(gainOrLossAfterReliefs = None),
                Some(List(sample[ReturnSummary].copy(taxYear = taxYearStartYear))),
                None,
                TaskListStatus.ToDo
              )
            }

            "the required section is now complete but the GLAR section has not been completed" in {
              test(
                prerequisiteDraftReturn.copy(gainOrLossAfterReliefs = Some(sample[AmountInPence])),
                Some(List(sample[ReturnSummary].copy(taxYear = taxYearStartYear))),
                None,
                TaskListStatus.Complete
              )
            }

          }

          "the return is not a further return or an amend return" in {
            testSectionNonExistent(prerequisiteDraftReturn.copy(gainOrLossAfterReliefs = None), None, None)(
              "gainOrLossAfterReliefs"
            )
          }

          "the return is an amend return where gain or loss after reliefs shouldn't be shown" in {
            testSectionNonExistent(
              prerequisiteDraftReturn.copy(gainOrLossAfterReliefs = None),
              None,
              Some(
                sample[AmendReturnData].copy(
                  shouldDisplayGainOrLossAfterReliefs = false
                )
              )
            )(
              "gainOrLossAfterReliefs"
            )
          }

          "the return is an amend return where gain or loss after reliefs should be shown" in {
            test(
              prerequisiteDraftReturn.copy(gainOrLossAfterReliefs = None),
              None,
              Some(
                sample[AmendReturnData].copy(
                  shouldDisplayGainOrLossAfterReliefs = true
                )
              ),
              TaskListStatus.ToDo
            )
          }

        }

        "display the page with the enter losses and exemptions section status" when {

          "the session data indicates that they are filling in a return and the enter losses and exemptions section is incomplete" in {
            val incompleteExemptionAndLossesAnswers =
              sample[DraftMultipleIndirectDisposalsReturn].copy(
                triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                  .copy(individualUserType = Some(Self)),
                exampleCompanyDetailsAnswers = Some(sample[CompleteExampleCompanyDetailsAnswers]),
                exemptionAndLossesAnswers = Some(sample[IncompleteExemptionAndLossesAnswers])
              )

            testStateOfSection(
              incompleteExemptionAndLossesAnswers
            )(
              "exemptionsAndLosses",
              messageFromMessageKey("task-list.exemptions-and-losses.link"),
              exemptionandlosses.routes.ExemptionAndLossesController
                .checkYourAnswers(),
              TaskListStatus.InProgress,
              _.select("div.notice")
                .contains(messageFromMessageKey("task-list.incompleteTriage"))
            )

          }

          "the session data indicates that they are filling in a return and the enter losses and exemptions section is complete" in {
            val completeExemptionAndLossesAnswers =
              sample[DraftMultipleIndirectDisposalsReturn].copy(
                triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                  .copy(individualUserType = Some(Self)),
                exampleCompanyDetailsAnswers = Some(sample[CompleteExampleCompanyDetailsAnswers]),
                exemptionAndLossesAnswers = Some(sample[CompleteExemptionAndLossesAnswers])
              )

            testStateOfSection(
              completeExemptionAndLossesAnswers
            )(
              "exemptionsAndLosses",
              messageFromMessageKey("task-list.exemptions-and-losses.link"),
              exemptionandlosses.routes.ExemptionAndLossesController
                .checkYourAnswers(),
              TaskListStatus.Complete
            )
          }

          val triageAnswers =
            sample[CompleteMultipleDisposalsTriageAnswers]
              .copy(individualUserType = Some(Self), countryOfResidence = Country.uk)

          val taxYearStartYear: String =
            triageAnswers
              .fold(
                _.taxYear.map(_.startDateInclusive.getYear),
                c => Some(c.taxYear.startDateInclusive.getYear)
              )
              .map(_.toString)
              .getOrElse("2020")

          "the company details sections is complete but the gain or loss after reliefs section is not complete for a further return" in {

            testStateOfSection(
              sample[DraftMultipleIndirectDisposalsReturn].copy(
                triageAnswers = triageAnswers,
                exampleCompanyDetailsAnswers = Some(sample[CompleteExampleCompanyDetailsAnswers]),
                gainOrLossAfterReliefs = None,
                exemptionAndLossesAnswers = None
              )
            )(
              "exemptionsAndLosses",
              messageFromMessageKey("task-list.exemptions-and-losses.link"),
              exemptionandlosses.routes.ExemptionAndLossesController
                .checkYourAnswers(),
              TaskListStatus.CannotStart,
              previousSentReturns = Some(List(sample[ReturnSummary].copy(taxYear = taxYearStartYear)))
            )
          }

          "the company details sections is complete and the gain or loss after reliefs section is complete for a further return" in {
            testStateOfSection(
              sample[DraftMultipleIndirectDisposalsReturn].copy(
                triageAnswers = triageAnswers,
                exampleCompanyDetailsAnswers = Some(sample[CompleteExampleCompanyDetailsAnswers]),
                gainOrLossAfterReliefs = Some(sample[AmountInPence]),
                exemptionAndLossesAnswers = None
              )
            )(
              "exemptionsAndLosses",
              messageFromMessageKey("task-list.exemptions-and-losses.link"),
              exemptionandlosses.routes.ExemptionAndLossesController
                .checkYourAnswers(),
              TaskListStatus.ToDo,
              previousSentReturns = Some(List(sample[ReturnSummary].copy(taxYear = taxYearStartYear)))
            )
          }

        }

        "display the page with the enter capital gains tax liability so far this tax year section status" when {

          "the session data indicates that they are filling in a return and" +
            " the enter capital gains tax liability so far this tax year section is incomplete" in {
              val incompleteNonCalculatedYTDAnswers =
                sample[DraftMultipleIndirectDisposalsReturn].copy(
                  triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                    .copy(individualUserType = Some(Self)),
                  exampleCompanyDetailsAnswers = Some(sample[CompleteExampleCompanyDetailsAnswers]),
                  exemptionAndLossesAnswers = Some(sample[CompleteExemptionAndLossesAnswers]),
                  yearToDateLiabilityAnswers = Some(sample[NonCalculatedYTDAnswers])
                )

              testStateOfSection(
                incompleteNonCalculatedYTDAnswers
              )(
                "enterCgtLiability",
                messageFromMessageKey("task-list.enter-cgt-liability.link"),
                yeartodatelliability.routes.YearToDateLiabilityController
                  .checkYourAnswers(),
                TaskListStatus.Complete
              )

            }

        }

        "display the page with the check and send return, pay any tax due section status" when {

          "the session data indicates that they are filling in a return and" +
            " the check and send return, pay any tax due section is incomplete" in {
              val checkAllAnswersAndSubmitAnswers =
                sample[DraftMultipleIndirectDisposalsReturn].copy(
                  triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                    .copy(individualUserType = Some(Self)),
                  exampleCompanyDetailsAnswers = Some(sample[CompleteExampleCompanyDetailsAnswers]),
                  exemptionAndLossesAnswers = Some(sample[CompleteExemptionAndLossesAnswers]),
                  yearToDateLiabilityAnswers = Some(sample[CompleteCalculatedYTDAnswers]),
                  supportingEvidenceAnswers = Some(sample[CompleteSupportingEvidenceAnswers])
                )

              testStateOfSection(
                checkAllAnswersAndSubmitAnswers
              )(
                "checkAndSendReturn",
                messageFromMessageKey("task-list.check-and-send-return.link"),
                routes.CheckAllAnswersAndSubmitController.checkAllAnswers(),
                TaskListStatus.ToDo
              )

            }

        }

      }

      "handling requests to display the single mixed use task list page" must {

        "display the page with the proper person represented section status" when {

          "the individual user type is personal representative or capacitor and" when {

            "the section has not been started" in {
              val draftReturn =
                sample[DraftSingleMixedUseDisposalReturn].copy(
                  triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                    .copy(individualUserType = Some(PersonalRepresentative)),
                  representeeAnswers = None
                )

              testStateOfSection(
                draftReturn
              )(
                "representee",
                messageFromMessageKey("task-list.representee.link"),
                representee.routes.RepresenteeController.checkYourAnswers(),
                TaskListStatus.ToDo,
                _.select("div.notice")
                  .contains(messageFromMessageKey("task-list.incompleteTriage"))
              )
            }

            "the section is incomplete" in {
              val draftReturn =
                sample[DraftSingleMixedUseDisposalReturn].copy(
                  triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                    .copy(individualUserType = Some(PersonalRepresentative)),
                  representeeAnswers = Some(sample[IncompleteRepresenteeAnswers])
                )

              testStateOfSection(
                draftReturn
              )(
                "representee",
                messageFromMessageKey("task-list.representee.link"),
                representee.routes.RepresenteeController.checkYourAnswers(),
                TaskListStatus.InProgress,
                _.select("div.notice")
                  .contains(messageFromMessageKey("task-list.incompleteTriage"))
              )
            }

            "the section is complete" in {
              val draftReturn =
                sample[DraftSingleMixedUseDisposalReturn].copy(
                  triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                    .copy(individualUserType = Some(PersonalRepresentative)),
                  representeeAnswers = Some(sample[CompleteRepresenteeAnswers])
                )

              testStateOfSection(
                draftReturn
              )(
                "representee",
                messageFromMessageKey("task-list.representee.link"),
                representee.routes.RepresenteeController.checkYourAnswers(),
                TaskListStatus.Complete
              )
            }

          }

        }

        "display the page with the proper triage section status" when {

          "the session data indicates that they are filling in a return and the triage section is incomplete" in {
            val incompleteTriage = sample[DraftSingleMixedUseDisposalReturn].copy(
              triageAnswers = sample[IncompleteSingleDisposalTriageAnswers]
                .copy(individualUserType = Some(Self))
            )

            testStateOfSection(
              incompleteTriage
            )(
              "canTheyUseOurService",
              messageFromMessageKey("task-list.triage.link"),
              triage.routes.SingleDisposalsTriageController.checkYourAnswers(),
              TaskListStatus.InProgress,
              _.select("div.notice")
                .contains(messageFromMessageKey("task-list.incompleteTriage"))
            )

          }

          "the session data indicates that they are filling in a return and the triage section is complete" in {
            val completeTriage = sample[DraftSingleMixedUseDisposalReturn].copy(
              triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                .copy(individualUserType = Some(Self))
            )

            testStateOfSection(
              completeTriage
            )(
              "canTheyUseOurService",
              messageFromMessageKey("task-list.triage.link"),
              triage.routes.SingleDisposalsTriageController.checkYourAnswers(),
              TaskListStatus.Complete
            )
          }

        }

        "display the page with the enter property details section status" when {

          "the session data indicates that they are filling in a return and the enter details of one property section is incomplete" in {
            val incompletePropertyDetails = sample[DraftSingleMixedUseDisposalReturn].copy(
              triageAnswers = sample[CompleteSingleDisposalTriageAnswers].copy(
                assetType = AssetType.MixedUse,
                individualUserType = Some(Self)
              ),
              mixedUsePropertyDetailsAnswers = Some(sample[IncompleteMixedUsePropertyDetailsAnswers])
            )

            testStateOfSection(
              incompletePropertyDetails
            )(
              "propertyDetails",
              messageFromMessageKey("task-list.enter-property-details-single-mixed-use.link"),
              address.routes.MixedUsePropertyDetailsController.checkYourAnswers(),
              TaskListStatus.InProgress,
              _.select("div.notice").contains(messageFromMessageKey("task-list.incompleteTriage"))
            )

          }

          "the session data indicates that they are filling in a return and the enter details of one property section is complete" in {
            val completePropertyDetails = sample[DraftSingleMixedUseDisposalReturn].copy(
              triageAnswers = sample[CompleteSingleDisposalTriageAnswers].copy(
                assetType = AssetType.MixedUse,
                individualUserType = Some(Self)
              ),
              mixedUsePropertyDetailsAnswers = Some(sample[CompleteMixedUsePropertyDetailsAnswers])
            )

            testStateOfSection(
              completePropertyDetails
            )(
              "propertyDetails",
              messageFromMessageKey("task-list.enter-property-details-single-mixed-use.link"),
              address.routes.MixedUsePropertyDetailsController.checkYourAnswers(),
              TaskListStatus.Complete
            )

          }

        }

        "display the page with the proper gain or loss after reliefs section status" when {

          def test(
            draftReturn: DraftSingleMixedUseDisposalReturn,
            previousSentReturns: Option[List[ReturnSummary]],
            amendReturnData: Option[AmendReturnData],
            expectedStatus: TaskListStatus
          ): Unit =
            testStateOfSection(draftReturn)(
              "gainOrLossAfterReliefs",
              messageFromMessageKey("task-list.gain-or-loss-after-reliefs.link"),
              gainorlossafterreliefs.routes.GainOrLossAfterReliefsController.checkYourAnswers(),
              expectedStatus,
              previousSentReturns = previousSentReturns,
              amendReturnData = amendReturnData
            )

          val prerequisiteDraftReturn =
            sample[DraftSingleMixedUseDisposalReturn].copy(
              triageAnswers = sample[CompleteSingleDisposalTriageAnswers].copy(
                individualUserType = Some(Self)
              ),
              mixedUsePropertyDetailsAnswers = Some(sample[CompleteMixedUsePropertyDetailsAnswers]),
              exemptionAndLossesAnswers = None,
              gainOrLossAfterReliefs = None
            )

          val taxYearStartYear: String =
            prerequisiteDraftReturn.triageAnswers
              .fold(
                _.disposalDate.map(_.taxYear.startDateInclusive.getYear),
                c => Some(c.disposalDate.taxYear.startDateInclusive.getYear)
              )
              .map(_.toString)
              .getOrElse("2020")

          "the return is a further return and" when {

            "the property details section is not been started" in {
              test(
                prerequisiteDraftReturn.copy(mixedUsePropertyDetailsAnswers = None),
                Some(List(sample[ReturnSummary].copy(taxYear = taxYearStartYear))),
                None,
                TaskListStatus.CannotStart
              )
            }

            "the property details section is not complete" in {
              test(
                prerequisiteDraftReturn
                  .copy(mixedUsePropertyDetailsAnswers = Some(sample[IncompleteMixedUsePropertyDetailsAnswers])),
                Some(List(sample[ReturnSummary].copy(taxYear = taxYearStartYear))),
                None,
                TaskListStatus.CannotStart
              )
            }

            "the required section is now complete but the GLAR section has not been started yet" in {
              test(
                prerequisiteDraftReturn.copy(gainOrLossAfterReliefs = None),
                Some(List(sample[ReturnSummary].copy(taxYear = taxYearStartYear))),
                None,
                TaskListStatus.ToDo
              )
            }

            "the required sections are now complete but the GLAR section has not been completed" in {
              test(
                prerequisiteDraftReturn.copy(gainOrLossAfterReliefs = Some(sample[AmountInPence])),
                Some(List(sample[ReturnSummary].copy(taxYear = taxYearStartYear))),
                None,
                TaskListStatus.Complete
              )
            }

          }

          "the return is not a further return or an amend return" in {
            testSectionNonExistent(prerequisiteDraftReturn.copy(gainOrLossAfterReliefs = None), None, None)(
              "gainOrLossAfterReliefs"
            )
          }

          "the return is an amend return where gain or loss after reliefs shouldn't be shown" in {
            testSectionNonExistent(
              prerequisiteDraftReturn.copy(gainOrLossAfterReliefs = None),
              Some(List(sample[ReturnSummary])),
              Some(
                sample[AmendReturnData].copy(
                  shouldDisplayGainOrLossAfterReliefs = false
                )
              )
            )(
              "gainOrLossAfterReliefs"
            )
          }

          "the return is an amend return where gain or loss after reliefs should be shown" in {
            test(
              prerequisiteDraftReturn.copy(gainOrLossAfterReliefs = None),
              Some(List(sample[ReturnSummary])),
              Some(
                sample[AmendReturnData].copy(
                  shouldDisplayGainOrLossAfterReliefs = true
                )
              ),
              TaskListStatus.ToDo
            )
          }

        }

        "display the page with the enter losses and exemptions section status" when {

          "the session data indicates that they are filling in a return and the enter losses and exemptions section is incomplete" in {
            val incompleteExemptionAndLossesAnswers =
              sample[DraftSingleMixedUseDisposalReturn].copy(
                triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                  .copy(individualUserType = Some(Self)),
                exemptionAndLossesAnswers = Some(sample[IncompleteExemptionAndLossesAnswers]),
                mixedUsePropertyDetailsAnswers = Some(sample[CompleteMixedUsePropertyDetailsAnswers])
              )

            testStateOfSection(
              incompleteExemptionAndLossesAnswers
            )(
              "exemptionsAndLosses",
              messageFromMessageKey("task-list.exemptions-and-losses.link"),
              exemptionandlosses.routes.ExemptionAndLossesController
                .checkYourAnswers(),
              TaskListStatus.InProgress,
              _.select("div.notice")
                .contains(messageFromMessageKey("task-list.incompleteTriage"))
            )

          }

          "the session data indicates that they are filling in a return and the enter losses and exemptions section is complete" in {
            val completeExemptionAndLossesAnswers =
              sample[DraftSingleMixedUseDisposalReturn].copy(
                triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                  .copy(individualUserType = Some(Self)),
                mixedUsePropertyDetailsAnswers = Some(sample[CompleteMixedUsePropertyDetailsAnswers]),
                exemptionAndLossesAnswers = Some(sample[CompleteExemptionAndLossesAnswers])
              )

            testStateOfSection(
              completeExemptionAndLossesAnswers
            )(
              "exemptionsAndLosses",
              messageFromMessageKey("task-list.exemptions-and-losses.link"),
              exemptionandlosses.routes.ExemptionAndLossesController
                .checkYourAnswers(),
              TaskListStatus.Complete
            )
          }

          "the reliefs section is complete but the gain or loss after reliefs section is not complete for a further return" in {

            val triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
              .copy(
                individualUserType = Some(Self),
                countryOfResidence = Country.uk,
                disposalDate = sample[DisposalDate]
              )

            val taxYearStartYear = triageAnswers
              .fold(
                _.disposalDate.map(_.taxYear.startDateInclusive.getYear.toString),
                c => Some(c.disposalDate.taxYear.startDateInclusive.getYear.toString)
              )
              .getOrElse("2020")

            testStateOfSection(
              sample[DraftSingleMixedUseDisposalReturn].copy(
                triageAnswers = triageAnswers,
                mixedUsePropertyDetailsAnswers = Some(sample[CompleteMixedUsePropertyDetailsAnswers]),
                gainOrLossAfterReliefs = None,
                exemptionAndLossesAnswers = None
              )
            )(
              "exemptionsAndLosses",
              messageFromMessageKey("task-list.exemptions-and-losses.link"),
              exemptionandlosses.routes.ExemptionAndLossesController.checkYourAnswers(),
              TaskListStatus.CannotStart,
              previousSentReturns = Some(
                List(
                  sample[ReturnSummary].copy(taxYear = taxYearStartYear)
                )
              )
            )
          }

          "the reliefs section is complete and the gain or loss after reliefs section is complete for a further return" in {
            testStateOfSection(
              sample[DraftSingleMixedUseDisposalReturn].copy(
                triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                  .copy(individualUserType = Some(Self), countryOfResidence = Country.uk),
                mixedUsePropertyDetailsAnswers = Some(sample[CompleteMixedUsePropertyDetailsAnswers]),
                gainOrLossAfterReliefs = Some(sample[AmountInPence]),
                exemptionAndLossesAnswers = None
              )
            )(
              "exemptionsAndLosses",
              messageFromMessageKey("task-list.exemptions-and-losses.link"),
              exemptionandlosses.routes.ExemptionAndLossesController
                .checkYourAnswers(),
              TaskListStatus.ToDo,
              previousSentReturns = Some(List(sample[ReturnSummary]))
            )
          }

        }

        "display the page with the enter capital gains tax liability so far this tax year section status" when {

          "the session data indicates that they are filling in a return and" +
            " the enter capital gains tax liability so far this tax year section is incomplete" in {
              val incompleteNonCalculatedYTDAnswers =
                sample[DraftSingleMixedUseDisposalReturn].copy(
                  triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                    .copy(individualUserType = Some(Self)),
                  mixedUsePropertyDetailsAnswers = Some(sample[CompleteMixedUsePropertyDetailsAnswers]),
                  exemptionAndLossesAnswers = Some(sample[CompleteExemptionAndLossesAnswers]),
                  yearToDateLiabilityAnswers = Some(sample[NonCalculatedYTDAnswers])
                )

              testStateOfSection(
                incompleteNonCalculatedYTDAnswers
              )(
                "enterCgtLiability",
                messageFromMessageKey("task-list.enter-cgt-liability.link"),
                yeartodatelliability.routes.YearToDateLiabilityController
                  .checkYourAnswers(),
                TaskListStatus.Complete
              )

            }

        }

        "display the page with the check and send return, pay any tax due section status" when {

          "the session data indicates that they are filling in a return and" +
            " the check and send return, pay any tax due section is incomplete" in {
              val checkAllAnswersAndSubmitAnswers =
                sample[DraftSingleMixedUseDisposalReturn].copy(
                  triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                    .copy(individualUserType = Some(Self)),
                  exemptionAndLossesAnswers = Some(sample[CompleteExemptionAndLossesAnswers]),
                  yearToDateLiabilityAnswers = Some(sample[CompleteCalculatedYTDAnswers]),
                  supportingEvidenceAnswers = Some(sample[CompleteSupportingEvidenceAnswers]),
                  mixedUsePropertyDetailsAnswers = Some(sample[CompleteMixedUsePropertyDetailsAnswers])
                )

              testStateOfSection(
                checkAllAnswersAndSubmitAnswers
              )(
                "checkAndSendReturn",
                messageFromMessageKey("task-list.check-and-send-return.link"),
                routes.CheckAllAnswersAndSubmitController.checkAllAnswers(),
                TaskListStatus.ToDo
              )

            }

        }

      }

    }

  }

}
