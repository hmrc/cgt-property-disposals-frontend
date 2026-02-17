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

import cats.data.EitherT
import cats.instances.future.*
import org.jsoup.nodes.Document
import uk.gov.hmrc.cgtpropertydisposalsfrontend.SampledScalaCheck
import play.api.i18n.{Lang, Messages, MessagesApi, MessagesImpl}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.{Call, MessagesRequest, Request, Result}
import play.api.test.FakeRequest
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.accounts.homepage
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedRequest, RequestWithSessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.CheckAllAnswersAndSubmitControllerSpec.*
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.acquisitiondetails.AcquisitionDetailsControllerSpec.validateAcquisitionDetailsCheckYourAnswersPage
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.acquisitiondetails.RebasingEligibilityUtil
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.address.MultipleDisposalsPropertyDetailsControllerSpec.validateExamplePropertyDetailsSummary
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.amend.AmendReturnController
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.disposaldetails.DisposalDetailsControllerSpec.*
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.exemptionandlosses.ExemptionAndLossesControllerSpec.validateExemptionAndLossesCheckYourAnswersPage
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.gainorlossafterreliefs.GainOrLossAfterReliefsControllerSpec.validateGainOrLossOrReliefsCheckYourAnswersPage
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.reliefdetails.ReliefDetailsControllerSpec.validateReliefDetailsCheckYourAnswersPage
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.representee.RepresenteeControllerSpec
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.triage.MultipleDisposalsTriageControllerSpec.validateMultipleDisposalsTriageCheckYourAnswersPage
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.triage.SingleDisposalsTriageControllerSpec.validateSingleDisposalTriageCheckYourAnswersPage
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.yeartodatelliability.YearToDateLiabilityControllerSpec.{validateCalculatedYearToDateLiabilityPage, validateNonCalculatedYearToDateLiabilityPage}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, JustSubmittedReturn, PreviousReturnData, SubmitReturnFailed, SubmittingReturn, Subscribed}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address.UkAddress
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Postcode
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.{AmountInPence, MoneyUtils, PaymentsJourney}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.AcquisitionDetailsGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.AddressGen.*
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.CompleteReturnGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.DisposalDetailsGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.ExampleCompanyDetailsAnswersGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.ExamplePropertyDetailsAnswersGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.ExemptionsAndLossesAnswersGen.*
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.FurtherReturnCalculationGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.IdGen.*
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.JourneyStatusGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.MoneyGen.*
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.NameGen.*
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.ReliefDetailsGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.RepresenteeAnswersGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.ReturnAPIGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.ReturnGen.*
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.SingleMixedUseDetailsAnswersGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.SubscribedDetailsGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.TriageQuestionsGen.*
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.YearToDateLiabilityAnswersGen.*
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.{AgentReferenceNumber, CgtReference}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.{IndividualName, TrustName}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.SubscribedDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.*
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.AcquisitionDetailsAnswers.IncompleteAcquisitionDetailsAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.AssetType.IndirectDisposal
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.CompleteReturn.{CompleteMultipleDisposalsReturn, CompleteMultipleIndirectDisposalReturn, CompleteSingleDisposalReturn, CompleteSingleIndirectDisposalReturn, CompleteSingleMixedUseDisposalReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.DisposalDetailsAnswers.IncompleteDisposalDetailsAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ExampleCompanyDetailsAnswers.{CompleteExampleCompanyDetailsAnswers, IncompleteExampleCompanyDetailsAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ExamplePropertyDetailsAnswers.IncompleteExamplePropertyDetailsAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ExemptionAndLossesAnswers.IncompleteExemptionAndLossesAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.IndividualUserType.{Capacitor, PersonalRepresentative, PersonalRepresentativeInPeriodOfAdmin, Self}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.MixedUsePropertyDetailsAnswers.IncompleteMixedUsePropertyDetailsAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.MultipleDisposalsTriageAnswers.{CompleteMultipleDisposalsTriageAnswers, IncompleteMultipleDisposalsTriageAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ReliefDetailsAnswers.IncompleteReliefDetailsAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.RepresenteeAnswers.{CompleteRepresenteeAnswers, IncompleteRepresenteeAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.RepresenteeReferenceId.{NoReferenceId, RepresenteeCgtReference}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.{CompleteSingleDisposalTriageAnswers, IncompleteSingleDisposalTriageAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SubmitReturnResponse.{DeltaCharge, ReturnCharge}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.YearToDateLiabilityAnswers.CalculatedYTDAnswers.{CompleteCalculatedYTDAnswers, IncompleteCalculatedYTDAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.YearToDateLiabilityAnswers.NonCalculatedYTDAnswers.{CompleteNonCalculatedYTDAnswers, IncompleteNonCalculatedYTDAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{B64Html, CompleteReturnWithSummary, Error, JourneyStatus, SessionData, TimeUtils, UserType}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.onboarding.SubscriptionService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.FurtherReturnCalculationEligibility.{Eligible, Ineligible}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.{FurtherReturnCalculationEligibility, FurtherReturnCalculationEligibilityUtil, PaymentsService, ReturnsService}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.{controllers, views}
import uk.gov.hmrc.http.HeaderCarrier

import java.time.{LocalDate, LocalDateTime, LocalTime}
import java.util.{Base64, UUID}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class CheckAllAnswersAndSubmitControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with SampledScalaCheck
    with RedirectToStartBehaviour
    with FurtherReturnCalculationEligibilityUtilSupport {

  private val mockReturnsService = mock[ReturnsService]

  private val mockPaymentsService = mock[PaymentsService]

  protected override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore),
      bind[ReturnsService].toInstance(mockReturnsService),
      bind[PaymentsService].toInstance(mockPaymentsService),
      bind[SubscriptionService].toInstance(mockSubscriptionService),
      bind[FurtherReturnCalculationEligibilityUtil].toInstance(mockFurtherReturnCalculationEligibilityUtil)
    )

  private lazy val controller = instanceOf[CheckAllAnswersAndSubmitController]

  private implicit val messagesApi: MessagesApi = controller.messagesApi

  private implicit val messages: MessagesImpl = MessagesImpl(lang, messagesApi)

  private val rebasingEligibilityUtil = new RebasingEligibilityUtil()

  private def setNameForUserType(
    userType: UserType
  ) =
    userType match {
      case UserType.Organisation => Left(sample[TrustName])
      case _                     => Right(sample[IndividualName])
    }

  private def setAgentReferenceNumber(
    userType: UserType
  ) =
    userType match {
      case UserType.Agent => Some(sample[AgentReferenceNumber])
      case _              => None
    }

  private def sessionWithJourney(
    journeyStatus: JourneyStatus,
    userType: UserType
  ) =
    SessionData.empty.copy(
      journeyStatus = Some(journeyStatus),
      userType = Some(userType)
    )

  private def sessionWithJourney(journeyStatus: JourneyStatus) =
    SessionData.empty.copy(journeyStatus = Some(journeyStatus))

  private def mockSubmitReturn(
    submitReturnRequest: SubmitReturnRequest,
    lang: Lang
  )(response: Either[Error, SubmitReturnResponse]) =
    (mockReturnsService
      .submitReturn(_: SubmitReturnRequest, _: Lang)(using _: HeaderCarrier))
      .expects(submitReturnRequest, lang, *)
      .returning(EitherT.fromEither[Future](response))

  private def mockStartPaymentJourney(
    cgtReference: CgtReference,
    chargeReference: Option[String],
    amount: AmountInPence,
    dueDate: Option[LocalDate],
    returnUrl: Call,
    backUrl: Call
  )(response: Either[Error, PaymentsJourney]) =
    (
      mockPaymentsService
        .startPaymentJourney(
          _: CgtReference,
          _: Option[String],
          _: AmountInPence,
          _: Option[LocalDate],
          _: Call,
          _: Call
        )(using
          _: HeaderCarrier,
          _: Request[?]
        )
      )
      .expects(cgtReference, chargeReference, amount, dueDate, returnUrl, backUrl, *, *)
      .returning(EitherT.fromEither[Future](response))

  private def mockCgtRegistrationService(
    response: Either[Error, RepresenteeCgtReference],
    completeRepresenteeAnswers: CompleteRepresenteeAnswers,
    lang: Lang
  ) =
    (mockSubscriptionService
      .registerWithoutIdAndSubscribe(
        _: CompleteRepresenteeAnswers,
        _: Lang
      )(using
        _: HeaderCarrier
      ))
      .expects(completeRepresenteeAnswers, lang, *)
      .returning(EitherT.fromEither(response))

  private def userMessageKey(
    individualUserType: Option[IndividualUserType],
    userType: UserType
  ): String =
    (individualUserType, userType) match {
      case (Some(Capacitor), _)                                          => ".capacitor"
      case (Some(PersonalRepresentative), _)                             => ".personalRep"
      case (Some(PersonalRepresentativeInPeriodOfAdmin), UserType.Agent) => ".personalRepInPeriodOfAdmin.agent"
      case (Some(PersonalRepresentativeInPeriodOfAdmin), _)              => ".personalRepInPeriodOfAdmin"
      case (_, UserType.Individual)                                      => ""
      case (_, UserType.Organisation)                                    => ".trust"
      case (_, UserType.Agent)                                           => ".agent"
      case other                                                         => sys.error(s"User type '$other' not handled")
    }

  "CheckAllAnswersAndSubmitController" when {
    "handling requests to display the check all answers page" when {
      def performAction(): Future[Result] =
        controller.checkAllAnswers()(FakeRequest())

      "the user is on a single disposal journey" must {
        val representeeAnswers = sample[CompleteRepresenteeAnswers].copy(isFirstReturn = true)

        val completeReturn = {
          val complete = sample[CompleteSingleDisposalReturn].copy(
            triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
              .copy(individualUserType = Some(PersonalRepresentative)),
            representeeAnswers = Some(representeeAnswers),
            gainOrLossAfterReliefs = None
          )
          complete.copy(hasAttachments =
            complete.supportingDocumentAnswers.evidences.nonEmpty || complete.yearToDateLiabilityAnswers
              .fold(_.mandatoryEvidence, _.mandatoryEvidence)
              .isDefined
          )
        }

        val completeDraftReturn = DraftSingleDisposalReturn(
          UUID.randomUUID(),
          completeReturn.triageAnswers,
          Some(completeReturn.propertyAddress),
          Some(completeReturn.disposalDetails),
          Some(completeReturn.acquisitionDetails),
          Some(completeReturn.reliefDetails),
          Some(completeReturn.exemptionsAndLossesDetails),
          Some(completeReturn.yearToDateLiabilityAnswers.merge),
          completeReturn.initialGainOrLoss,
          Some(completeReturn.supportingDocumentAnswers),
          completeReturn.representeeAnswers,
          None,
          TimeUtils.today()
        )

        val completeFillingOutReturn =
          sample[FillingOutReturn]
            .copy(draftReturn = completeDraftReturn, previousSentReturns = None, amendReturnData = None)

        behave like redirectToStartWhenInvalidJourney(
          () => performAction(),
          {
            case _: FillingOutReturn => true
            case _                   => false
          }
        )

        behave like incompleteSingleDisposalJourneyBehaviour(() => performAction(), completeDraftReturn)

        "display the page" when {
          def test(
            sessionData: SessionData,
            fillingOutReturn: FillingOutReturn,
            completeReturn: CompleteSingleDisposalReturn,
            expectedTitleKey: String,
            userType: Option[UserType],
            isATrust: Boolean,
            hideEstimatesQuestion: Boolean,
            furtherReturnCalculationEligibility: Option[FurtherReturnCalculationEligibility],
            extraChecks: Document => Unit = _ => ()
          ): Unit = {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(sessionData)
              furtherReturnCalculationEligibility.foreach(eligibility =>
                mockFurtherReturnCalculationEligibilityCheck(fillingOutReturn)(Right(eligibility))
              )
            }

            checkPageIsDisplayed(
              performAction(),
              messageFromMessageKey(expectedTitleKey),
              { doc =>
                validateSingleDisposalCheckAllYourAnswersSections(
                  doc,
                  completeReturn,
                  userType,
                  rebasingEligibilityUtil.isUk(completeReturn),
                  rebasingEligibilityUtil.isEligibleForRebase(completeReturn),
                  isATrust,
                  completeReturn.triageAnswers.assetType,
                  furtherReturnCalculationEligibility.isDefined,
                  hideEstimatesQuestion,
                  furtherReturnCalculationEligibility.forall(_.isEligible)
                )
                doc
                  .select("#back, .govuk-back-link")
                  .attr("href") shouldBe routes.TaskListController
                  .taskList()
                  .url
                doc
                  .select("#content > article > form, #main-content form")
                  .attr(
                    "action"
                  )             shouldBe routes.CheckAllAnswersAndSubmitController
                  .checkAllAnswersSubmit()
                  .url
                extraChecks(doc)
              }
            )
          }

          "the return is complete" in {
            test(
              sessionWithJourney(completeFillingOutReturn),
              completeFillingOutReturn,
              completeReturn,
              "checkAllAnswers.title",
              None,
              completeFillingOutReturn.subscribedDetails.isATrust,
              hideEstimatesQuestion = false,
              furtherReturnCalculationEligibility = None,
              extraChecks = { doc =>
                doc.select("#cancelButton").isEmpty shouldBe true
                doc
                  .select("#saveAndComeBackLater")
                  .attr("href")                     shouldBe controllers.returns.routes.DraftReturnSavedController.draftReturnSaved().url
              }
            )
          }

          "the user is on a further return journey" in {
            val gainOrLossAfterReliefs = sample[AmountInPence]
            val representeeAnswers     = sample[CompleteRepresenteeAnswers].copy(
              isFirstReturn = false
            )

            val triageAnswers = sample[CompleteSingleDisposalTriageAnswers].copy(
              individualUserType = Some(PersonalRepresentative)
            )

            val completeReturn = {
              val complete = sample[CompleteSingleDisposalReturn].copy(
                triageAnswers = triageAnswers,
                representeeAnswers = Some(representeeAnswers),
                gainOrLossAfterReliefs = Some(gainOrLossAfterReliefs)
              )
              complete.copy(hasAttachments =
                complete.supportingDocumentAnswers.evidences.nonEmpty || complete.yearToDateLiabilityAnswers
                  .fold(_.mandatoryEvidence, _.mandatoryEvidence)
                  .isDefined
              )
            }

            val completeDraftReturn = DraftSingleDisposalReturn(
              UUID.randomUUID(),
              completeReturn.triageAnswers,
              Some(completeReturn.propertyAddress),
              Some(completeReturn.disposalDetails),
              Some(completeReturn.acquisitionDetails),
              Some(completeReturn.reliefDetails),
              Some(completeReturn.exemptionsAndLossesDetails),
              Some(completeReturn.yearToDateLiabilityAnswers.merge),
              completeReturn.initialGainOrLoss,
              Some(completeReturn.supportingDocumentAnswers),
              completeReturn.representeeAnswers,
              completeReturn.gainOrLossAfterReliefs,
              TimeUtils.today()
            )

            val taxYearStartYear: String =
              triageAnswers
                .fold(
                  _.disposalDate.map(_.taxYear.startDateInclusive.getYear),
                  c => Some(c.disposalDate.taxYear.startDateInclusive.getYear)
                )
                .map(_.toString)
                .getOrElse("2020")

            val completeFillingOutReturn = sample[FillingOutReturn].copy(
              draftReturn = completeDraftReturn,
              previousSentReturns = Some(
                PreviousReturnData(
                  List(sample[ReturnSummary].copy(taxYear = taxYearStartYear)),
                  Some(sample[AmountInPence]),
                  None,
                  None
                )
              ),
              amendReturnData = None
            )

            test(
              sessionWithJourney(completeFillingOutReturn),
              completeFillingOutReturn,
              completeReturn,
              "checkAllAnswers.title",
              None,
              completeFillingOutReturn.subscribedDetails.isATrust,
              hideEstimatesQuestion = false,
              furtherReturnCalculationEligibility = Some(sample[Eligible])
            )
          }

          "the user is on an amend return journey where the estimates question should be hidden" in {
            val gainOrLossAfterReliefs = sample[AmountInPence]
            val representeeAnswers     = sample[CompleteRepresenteeAnswers].copy(
              isFirstReturn = false
            )

            val completeReturn = {
              val complete = sample[CompleteSingleDisposalReturn].copy(
                triageAnswers = sample[CompleteSingleDisposalTriageAnswers].copy(
                  individualUserType = Some(PersonalRepresentative)
                ),
                representeeAnswers = Some(representeeAnswers),
                gainOrLossAfterReliefs = Some(gainOrLossAfterReliefs)
              )
              complete.copy(hasAttachments =
                complete.supportingDocumentAnswers.evidences.nonEmpty || complete.yearToDateLiabilityAnswers
                  .fold(_.mandatoryEvidence, _.mandatoryEvidence)
                  .isDefined
              )
            }

            val completeDraftReturn = DraftSingleDisposalReturn(
              UUID.randomUUID(),
              completeReturn.triageAnswers,
              Some(completeReturn.propertyAddress),
              Some(completeReturn.disposalDetails),
              Some(completeReturn.acquisitionDetails),
              Some(completeReturn.reliefDetails),
              Some(completeReturn.exemptionsAndLossesDetails),
              Some(completeReturn.yearToDateLiabilityAnswers.merge),
              completeReturn.initialGainOrLoss,
              Some(completeReturn.supportingDocumentAnswers),
              completeReturn.representeeAnswers,
              completeReturn.gainOrLossAfterReliefs,
              TimeUtils.today()
            )

            val completeFillingOutReturn = sample[FillingOutReturn].copy(
              draftReturn = completeDraftReturn,
              previousSentReturns =
                Some(PreviousReturnData(List(sample[ReturnSummary]), Some(sample[AmountInPence]), None, None)),
              amendReturnData = Some(
                sample[AmendReturnData].copy(
                  originalReturn = sample[CompleteReturnWithSummary].copy(
                    completeReturn = sample[CompleteSingleDisposalReturn].copy(
                      yearToDateLiabilityAnswers = Left(
                        sample[CompleteNonCalculatedYTDAnswers].copy(
                          hasEstimatedDetails = false
                        )
                      )
                    )
                  )
                )
              )
            )

            test(
              sessionWithJourney(completeFillingOutReturn),
              completeFillingOutReturn,
              completeReturn,
              "checkAllAnswers.amend.title",
              None,
              completeFillingOutReturn.subscribedDetails.isATrust,
              hideEstimatesQuestion = true,
              furtherReturnCalculationEligibility = Some(sample[Ineligible]),
              extraChecks = { doc =>
                doc.select("#cancelButton").attr("href")    shouldBe controllers.returns.amend.routes.AmendReturnController
                  .confirmCancel(
                    AmendReturnController.ConfirmCancelBackLocations.checkAnswersAcceptSend
                  )
                  .url
                doc.select("#saveAndComeBackLater").isEmpty shouldBe true
              }
            )
          }

          "the return is complete and the user is an agent" in {
            val userType          = UserType.Agent
            val subscribedDetails = sample[SubscribedDetails].copy(
              name = setNameForUserType(userType)
            )
            val fillingOutReturn  = completeFillingOutReturn.copy(
              agentReferenceNumber = setAgentReferenceNumber(userType),
              subscribedDetails = subscribedDetails,
              amendReturnData = None,
              previousSentReturns = None
            )

            test(
              sessionWithJourney(
                fillingOutReturn,
                userType = userType
              ).copy(userType = Some(userType)),
              fillingOutReturn,
              completeReturn,
              "checkAllAnswers.title",
              Some(userType),
              subscribedDetails.isATrust,
              hideEstimatesQuestion = false,
              furtherReturnCalculationEligibility = None
            )
          }
        }

        "redirect to the task list page" when {
          "the user has chosen a user type of capacitor or personal rep and" when {
            "there are no representee answers" in {
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithJourney(
                    completeFillingOutReturn.copy(
                      draftReturn = completeDraftReturn.copy(
                        triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                          .copy(individualUserType = Some(Capacitor)),
                        representeeAnswers = None
                      )
                    )
                  )
                )
              }

              checkIsRedirect(performAction(), routes.TaskListController.taskList())
            }

            "the representee answers are incomplete" in {
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithJourney(
                    completeFillingOutReturn.copy(
                      draftReturn = completeDraftReturn.copy(
                        triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                          .copy(individualUserType = Some(PersonalRepresentative)),
                        representeeAnswers = Some(sample[IncompleteRepresenteeAnswers])
                      )
                    )
                  )
                )
              }

              checkIsRedirect(performAction(), routes.TaskListController.taskList())
            }
          }
        }
      }

      "the user is on a multiple disposals journey" must {
        val completeReturn = {
          val complete = sample[CompleteMultipleDisposalsReturn]
            .copy(
              triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                .copy(individualUserType = Some(PersonalRepresentativeInPeriodOfAdmin)),
              representeeAnswers = Some(
                sample[CompleteRepresenteeAnswers]
                  .copy(dateOfDeath = Some(DateOfDeath(LocalDate.now)), isFirstReturn = true)
              )
            )

          complete.copy(
            hasAttachments =
              complete.supportingDocumentAnswers.evidences.nonEmpty || complete.yearToDateLiabilityAnswers.mandatoryEvidence.isDefined
          )
        }

        val completeDraftReturn = DraftMultipleDisposalsReturn(
          UUID.randomUUID(),
          completeReturn.triageAnswers,
          Some(completeReturn.examplePropertyDetailsAnswers),
          Some(completeReturn.exemptionAndLossesAnswers),
          Some(completeReturn.yearToDateLiabilityAnswers),
          Some(completeReturn.supportingDocumentAnswers),
          completeReturn.representeeAnswers,
          None,
          TimeUtils.today()
        )

        val completeFillingOutReturn =
          sample[FillingOutReturn]
            .copy(draftReturn = completeDraftReturn, previousSentReturns = None, amendReturnData = None)

        behave like redirectToStartWhenInvalidJourney(
          () => performAction(),
          {
            case _: FillingOutReturn => true
            case _                   => false
          }
        )

        behave like incompleteMultipleDisposalsJourneyBehaviour(() => performAction(), completeDraftReturn)

        "display the page" when {
          def test(
            sessionData: SessionData,
            expectedTitleKey: String,
            userType: Option[UserType],
            isATrust: Boolean,
            isFurtherOrAmendReturn: Boolean,
            hideEstimatesQuestion: Boolean,
            showAnnualExemptAmount: Boolean
          ): Unit = {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(sessionData)
            }

            checkPageIsDisplayed(
              performAction(),
              messageFromMessageKey(expectedTitleKey),
              { doc =>
                validateMultipleDisposalsCheckAllYourAnswersSections(
                  doc,
                  completeReturn,
                  userType,
                  isATrust,
                  isFurtherOrAmendReturn,
                  hideEstimatesQuestion,
                  showAnnualExemptAmount
                )
                doc
                  .select("#back, .govuk-back-link")
                  .attr("href") shouldBe routes.TaskListController
                  .taskList()
                  .url
                doc
                  .select("#content > article > form, #main-content form")
                  .attr(
                    "action"
                  )             shouldBe routes.CheckAllAnswersAndSubmitController
                  .checkAllAnswersSubmit()
                  .url
              }
            )
          }

          "the return is complete" in {
            test(
              sessionWithJourney(completeFillingOutReturn),
              "checkAllAnswers.title",
              None,
              completeFillingOutReturn.subscribedDetails.isATrust,
              isFurtherOrAmendReturn = false,
              hideEstimatesQuestion = false,
              showAnnualExemptAmount = true
            )
          }

          "the return is complete and the user is an agent" in {
            val userType          = UserType.Agent
            val subscribedDetails = sample[SubscribedDetails].copy(
              name = setNameForUserType(userType)
            )

            test(
              sessionWithJourney(
                completeFillingOutReturn.copy(
                  agentReferenceNumber = setAgentReferenceNumber(userType),
                  subscribedDetails = subscribedDetails
                ),
                userType = userType
              ).copy(userType = Some(userType)),
              "checkAllAnswers.title",
              Some(userType),
              subscribedDetails.isATrust,
              isFurtherOrAmendReturn = false,
              hideEstimatesQuestion = false,
              showAnnualExemptAmount = true
            )
          }
        }

        "redirect to the task list page" when {
          "the user has chosen a user type of capacitor or personal rep and" when {
            "there are no representee answers" in {
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithJourney(
                    completeFillingOutReturn.copy(
                      draftReturn = completeDraftReturn.copy(
                        triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                          .copy(individualUserType = Some(Capacitor)),
                        representeeAnswers = None
                      )
                    )
                  )
                )
              }

              checkIsRedirect(performAction(), routes.TaskListController.taskList())
            }

            "the representee answers are incomplete" in {
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithJourney(
                    completeFillingOutReturn.copy(
                      draftReturn = completeDraftReturn.copy(
                        triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                          .copy(individualUserType = Some(PersonalRepresentative)),
                        representeeAnswers = Some(sample[IncompleteRepresenteeAnswers])
                      )
                    )
                  )
                )
              }

              checkIsRedirect(performAction(), routes.TaskListController.taskList())
            }
          }
        }
      }

      "the user is on a single indirect disposal journey" must {
        val completeReturn = {
          val complete = sample[CompleteSingleIndirectDisposalReturn].copy(
            triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
              .copy(individualUserType = Some(PersonalRepresentative)),
            representeeAnswers = Some(sample[CompleteRepresenteeAnswers].copy(isFirstReturn = true)),
            yearToDateLiabilityAnswers = sample[CompleteNonCalculatedYTDAnswers]
          )

          complete.copy(
            hasAttachments =
              complete.supportingDocumentAnswers.evidences.nonEmpty || complete.yearToDateLiabilityAnswers.mandatoryEvidence.isDefined
          )
        }

        val completeDraftReturn = DraftSingleIndirectDisposalReturn(
          UUID.randomUUID(),
          completeReturn.triageAnswers,
          Some(completeReturn.companyAddress),
          Some(completeReturn.disposalDetails),
          Some(completeReturn.acquisitionDetails),
          Some(completeReturn.exemptionsAndLossesDetails),
          Some(completeReturn.yearToDateLiabilityAnswers),
          Some(completeReturn.supportingDocumentAnswers),
          completeReturn.representeeAnswers,
          None,
          TimeUtils.today()
        )

        val completeFillingOutReturn =
          sample[FillingOutReturn]
            .copy(draftReturn = completeDraftReturn, previousSentReturns = None, amendReturnData = None)

        behave like redirectToStartWhenInvalidJourney(
          () => performAction(),
          {
            case _: FillingOutReturn => true
            case _                   => false
          }
        )

        behave like incompleteSingleIndirectDisposalJourneyBehaviour(() => performAction(), completeDraftReturn)

        "display the page" when {
          def test(
            sessionData: SessionData,
            expectedTitleKey: String,
            userType: Option[UserType],
            isATrust: Boolean,
            isFurtherOrAmendReturn: Boolean,
            hideEstimatesQuestion: Boolean,
            showAnnualExemptAmount: Boolean
          ): Unit = {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(sessionData)
            }

            checkPageIsDisplayed(
              performAction(),
              messageFromMessageKey(expectedTitleKey),
              { doc =>
                validateSingleIndirectDisposalCheckAllYourAnswersSections(
                  doc,
                  completeReturn,
                  userType,
                  rebasingEligibilityUtil.isEligibleForRebase(
                    wasAUkResident = false,
                    IndirectDisposal,
                    completeReturn.acquisitionDetails.acquisitionDate,
                    completeReturn.representativeType
                  ),
                  isATrust,
                  IndirectDisposal,
                  isFurtherOrAmendReturn,
                  hideEstimatesQuestion,
                  showAnnualExemptAmount
                )
                doc
                  .select("#back, .govuk-back-link")
                  .attr("href") shouldBe routes.TaskListController
                  .taskList()
                  .url
                doc
                  .select("#content > article > form, #main-content form")
                  .attr(
                    "action"
                  )             shouldBe routes.CheckAllAnswersAndSubmitController
                  .checkAllAnswersSubmit()
                  .url
              }
            )
          }

          "the return is complete" in {
            test(
              sessionWithJourney(completeFillingOutReturn),
              "checkAllAnswers.title",
              None,
              completeFillingOutReturn.subscribedDetails.isATrust,
              isFurtherOrAmendReturn = false,
              hideEstimatesQuestion = false,
              showAnnualExemptAmount = true
            )
          }

          "the return is complete and the user is an agent" in {
            val userType          = UserType.Agent
            val subscribedDetails = sample[SubscribedDetails].copy(
              name = setNameForUserType(userType)
            )

            test(
              sessionWithJourney(
                completeFillingOutReturn.copy(
                  agentReferenceNumber = setAgentReferenceNumber(userType),
                  subscribedDetails = subscribedDetails
                ),
                userType = userType
              ).copy(userType = Some(userType)),
              "checkAllAnswers.title",
              Some(userType),
              subscribedDetails.isATrust,
              isFurtherOrAmendReturn = false,
              hideEstimatesQuestion = false,
              showAnnualExemptAmount = true
            )
          }
        }

        "redirect to the task list page" when {
          "the user has chosen a user type of capacitor or personal rep and" when {
            "there are no representee answers" in {
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithJourney(
                    completeFillingOutReturn.copy(
                      draftReturn = completeDraftReturn.copy(
                        triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                          .copy(individualUserType = Some(Capacitor)),
                        representeeAnswers = None
                      )
                    )
                  )
                )
              }

              checkIsRedirect(performAction(), routes.TaskListController.taskList())
            }

            "the representee answers are incomplete" in {
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithJourney(
                    completeFillingOutReturn.copy(
                      draftReturn = completeDraftReturn.copy(
                        triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                          .copy(individualUserType = Some(PersonalRepresentative)),
                        representeeAnswers = Some(sample[IncompleteRepresenteeAnswers])
                      )
                    )
                  )
                )
              }

              checkIsRedirect(performAction(), routes.TaskListController.taskList())
            }
          }
        }
      }

      "the user is on a multiple indirect disposals journey" must {
        val completeReturn = {
          val complete = sample[CompleteMultipleIndirectDisposalReturn]
            .copy(
              triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                .copy(individualUserType = Some(PersonalRepresentative)),
              representeeAnswers = Some(sample[CompleteRepresenteeAnswers].copy(isFirstReturn = true))
            )

          complete.copy(
            hasAttachments =
              complete.supportingDocumentAnswers.evidences.nonEmpty || complete.yearToDateLiabilityAnswers.mandatoryEvidence.isDefined
          )
        }

        val completeDraftReturn = DraftMultipleIndirectDisposalsReturn(
          UUID.randomUUID(),
          completeReturn.triageAnswers,
          Some(completeReturn.exampleCompanyDetailsAnswers),
          Some(completeReturn.exemptionsAndLossesDetails),
          Some(completeReturn.yearToDateLiabilityAnswers),
          Some(completeReturn.supportingDocumentAnswers),
          completeReturn.representeeAnswers,
          None,
          TimeUtils.today()
        )

        val completeFillingOutReturn =
          sample[FillingOutReturn]
            .copy(draftReturn = completeDraftReturn, previousSentReturns = None, amendReturnData = None)

        behave like redirectToStartWhenInvalidJourney(
          () => performAction(),
          {
            case _: FillingOutReturn => true
            case _                   => false
          }
        )

        behave like incompleteMultipleIndirectDisposalsJourneyBehaviour(() => performAction(), completeDraftReturn)

        "display the page" when {
          def test(
            sessionData: SessionData,
            expectedTitleKey: String,
            userType: Option[UserType],
            isATrust: Boolean,
            isFurtherOrAmendReturn: Boolean,
            hideEstimatesQuestion: Boolean,
            showAnnualExemptAmount: Boolean
          ): Unit = {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(sessionData)
            }

            checkPageIsDisplayed(
              performAction(),
              messageFromMessageKey(expectedTitleKey),
              { doc =>
                validateMultipleIndirectDisposalsCheckAllYourAnswersSections(
                  doc,
                  completeReturn,
                  userType,
                  isATrust,
                  isFurtherOrAmendReturn,
                  hideEstimatesQuestion,
                  showAnnualExemptAmount
                )
                doc
                  .select("#back, .govuk-back-link")
                  .attr("href") shouldBe routes.TaskListController
                  .taskList()
                  .url
                doc
                  .select("#content > article > form, #main-content form")
                  .attr(
                    "action"
                  )             shouldBe routes.CheckAllAnswersAndSubmitController
                  .checkAllAnswersSubmit()
                  .url
              }
            )
          }

          "the return is complete" in {
            test(
              sessionWithJourney(completeFillingOutReturn),
              "checkAllAnswers.title",
              None,
              completeFillingOutReturn.subscribedDetails.isATrust,
              isFurtherOrAmendReturn = false,
              hideEstimatesQuestion = false,
              showAnnualExemptAmount = true
            )
          }

          "the return is complete and the user is an agent" in {
            val userType          = UserType.Agent
            val subscribedDetails = sample[SubscribedDetails].copy(
              name = setNameForUserType(userType)
            )

            test(
              sessionWithJourney(
                completeFillingOutReturn.copy(
                  agentReferenceNumber = setAgentReferenceNumber(userType),
                  subscribedDetails = subscribedDetails
                ),
                userType = userType
              ).copy(userType = Some(userType)),
              "checkAllAnswers.title",
              Some(userType),
              subscribedDetails.isATrust,
              isFurtherOrAmendReturn = false,
              hideEstimatesQuestion = false,
              showAnnualExemptAmount = true
            )
          }
        }

        "redirect to the task list page" when {
          "the user has chosen a user type of capacitor or personal rep and" when {
            "there are no representee answers" in {
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithJourney(
                    completeFillingOutReturn.copy(
                      draftReturn = completeDraftReturn.copy(
                        triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                          .copy(individualUserType = Some(Capacitor)),
                        representeeAnswers = None
                      )
                    )
                  )
                )
              }

              checkIsRedirect(performAction(), routes.TaskListController.taskList())
            }

            "the representee answers are incomplete" in {
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithJourney(
                    completeFillingOutReturn.copy(
                      draftReturn = completeDraftReturn.copy(
                        triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                          .copy(individualUserType = Some(PersonalRepresentative)),
                        representeeAnswers = Some(sample[IncompleteRepresenteeAnswers])
                      )
                    )
                  )
                )
              }

              checkIsRedirect(performAction(), routes.TaskListController.taskList())
            }
          }
        }
      }

      "the user is on a single mixed use disposals journey" must {
        val completeReturn = {
          val complete = sample[CompleteSingleMixedUseDisposalReturn]
            .copy(
              triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                .copy(individualUserType = Some(PersonalRepresentative)),
              representeeAnswers = Some(sample[CompleteRepresenteeAnswers].copy(isFirstReturn = true))
            )

          complete.copy(
            hasAttachments =
              complete.supportingDocumentAnswers.evidences.nonEmpty || complete.yearToDateLiabilityAnswers.mandatoryEvidence.isDefined
          )
        }

        val completeDraftReturn = DraftSingleMixedUseDisposalReturn(
          UUID.randomUUID(),
          completeReturn.triageAnswers,
          Some(completeReturn.propertyDetailsAnswers),
          Some(completeReturn.exemptionsAndLossesDetails),
          Some(completeReturn.yearToDateLiabilityAnswers),
          Some(completeReturn.supportingDocumentAnswers),
          completeReturn.representeeAnswers,
          None,
          TimeUtils.today()
        )

        val completeFillingOutReturn =
          sample[FillingOutReturn]
            .copy(draftReturn = completeDraftReturn, previousSentReturns = None, amendReturnData = None)

        behave like redirectToStartWhenInvalidJourney(
          () => performAction(),
          {
            case _: FillingOutReturn => true
            case _                   => false
          }
        )

        behave like incompleteSingleMixedUseDisposalsJourneyBehaviour(() => performAction(), completeDraftReturn)

        "display the page" when {
          def test(
            sessionData: SessionData,
            expectedTitleKey: String,
            userType: Option[UserType],
            isATrust: Boolean,
            isFurtherOrAmendReturn: Boolean,
            hideEstimatesQuestion: Boolean,
            showAnnualExemptAmount: Boolean
          ): Unit = {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(sessionData)
            }

            checkPageIsDisplayed(
              performAction(),
              messageFromMessageKey(expectedTitleKey),
              { doc =>
                validateSingleMixedUseDisposalsCheckAllYourAnswersSections(
                  doc,
                  completeReturn,
                  userType,
                  isATrust,
                  isFurtherOrAmendReturn,
                  hideEstimatesQuestion,
                  showAnnualExemptAmount
                )
                doc
                  .select("#back, .govuk-back-link")
                  .attr("href") shouldBe routes.TaskListController
                  .taskList()
                  .url
                doc
                  .select("#content > article > form, #main-content form")
                  .attr(
                    "action"
                  )             shouldBe routes.CheckAllAnswersAndSubmitController
                  .checkAllAnswersSubmit()
                  .url
              }
            )
          }

          "the return is complete" in {
            test(
              sessionWithJourney(completeFillingOutReturn),
              "checkAllAnswers.title",
              None,
              completeFillingOutReturn.subscribedDetails.isATrust,
              isFurtherOrAmendReturn = false,
              hideEstimatesQuestion = false,
              showAnnualExemptAmount = true
            )
          }

          "the return is complete and the user is an agent" in {
            val userType          = UserType.Agent
            val subscribedDetails = sample[SubscribedDetails].copy(
              name = setNameForUserType(userType)
            )

            test(
              sessionWithJourney(
                completeFillingOutReturn.copy(
                  agentReferenceNumber = setAgentReferenceNumber(userType),
                  subscribedDetails = subscribedDetails
                ),
                userType = userType
              ).copy(userType = Some(userType)),
              "checkAllAnswers.title",
              Some(userType),
              subscribedDetails.isATrust,
              isFurtherOrAmendReturn = false,
              hideEstimatesQuestion = false,
              showAnnualExemptAmount = true
            )
          }
        }

        "redirect to the task list page" when {
          "the user has chosen a user type of capacitor or personal rep and" when {
            "there are no representee answers" in {
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithJourney(
                    completeFillingOutReturn.copy(
                      draftReturn = completeDraftReturn.copy(
                        triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                          .copy(individualUserType = Some(Capacitor)),
                        representeeAnswers = None
                      )
                    )
                  )
                )
              }

              checkIsRedirect(performAction(), routes.TaskListController.taskList())
            }

            "the representee answers are incomplete" in {
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithJourney(
                    completeFillingOutReturn.copy(
                      draftReturn = completeDraftReturn.copy(
                        triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                          .copy(individualUserType = Some(PersonalRepresentative)),
                        representeeAnswers = Some(sample[IncompleteRepresenteeAnswers])
                      )
                    )
                  )
                )
              }

              checkIsRedirect(performAction(), routes.TaskListController.taskList())
            }
          }
        }
      }
    }

    "handling submits on the check all answers page" must {
      def performAction(): Future[Result] =
        controller.checkAllAnswersSubmit()(FakeRequest())

      val (completeReturn, hasAttachments) = {
        val r              = sample[CompleteSingleDisposalReturn].copy(
          triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
            .copy(individualUserType = None),
          representeeAnswers = None
        )
        val hasAttachments =
          r.supportingDocumentAnswers.evidences.nonEmpty || r.yearToDateLiabilityAnswers
            .fold(_.mandatoryEvidence, _.mandatoryEvidence)
            .isDefined

        r.copy(hasAttachments = hasAttachments) -> hasAttachments
      }

      val completeDraftReturnNoRepresentee = DraftSingleDisposalReturn(
        UUID.randomUUID(),
        completeReturn.triageAnswers,
        Some(completeReturn.propertyAddress),
        Some(completeReturn.disposalDetails),
        Some(completeReturn.acquisitionDetails),
        Some(completeReturn.reliefDetails),
        Some(completeReturn.exemptionsAndLossesDetails),
        Some(completeReturn.yearToDateLiabilityAnswers.merge),
        completeReturn.initialGainOrLoss,
        Some(completeReturn.supportingDocumentAnswers),
        None,
        completeReturn.gainOrLossAfterReliefs,
        TimeUtils.today()
      )

      val completeDraftReturnRepresenteWithNoReference =
        completeDraftReturnNoRepresentee.copy(
          representeeAnswers = Some(sample[CompleteRepresenteeAnswers].copy(id = NoReferenceId, isFirstReturn = true)),
          triageAnswers = completeReturn.triageAnswers.copy(individualUserType = Some(PersonalRepresentative))
        )

      val completeFillingOutReturnWithRepresenteeWithNoReference =
        sample[FillingOutReturn]
          .copy(
            draftReturn = completeDraftReturnRepresenteWithNoReference,
            amendReturnData = None,
            previousSentReturns = None
          )

      val completeFillingOutReturnNoRepresentee =
        completeFillingOutReturnWithRepresenteeWithNoReference.copy(draftReturn = completeDraftReturnNoRepresentee)

      val submitReturnResponse = sample[SubmitReturnResponse]

      val justSubmittedReturn = JustSubmittedReturn(
        completeFillingOutReturnNoRepresentee.subscribedDetails,
        completeFillingOutReturnNoRepresentee.ggCredId,
        completeFillingOutReturnNoRepresentee.agentReferenceNumber,
        completeReturn.copy(hasAttachments = hasAttachments),
        submitReturnResponse,
        completeFillingOutReturnNoRepresentee.amendReturnData
      )
      val submittingReturn    = SubmittingReturn(
        completeFillingOutReturnNoRepresentee.subscribedDetails,
        completeFillingOutReturnNoRepresentee.ggCredId,
        completeFillingOutReturnNoRepresentee.agentReferenceNumber
      )

      lazy val submitReturnRequest = {
        val cyaPge                                                     = instanceOf[views.html.returns.check_all_answers]
        implicit val requestWithSessionData: RequestWithSessionData[?] =
          RequestWithSessionData(
            None,
            AuthenticatedRequest(
              new MessagesRequest(FakeRequest(), messagesApi)
            )
          )
        implicit val messages: MessagesImpl                            = MessagesImpl(Lang.apply("en"), messagesApi)

        val cyaPageHtml =
          cyaPge(
            completeReturn,
            instanceOf[RebasingEligibilityUtil],
            completeFillingOutReturnNoRepresentee,
            showSubmissionDetails = true,
            hideEstimatesQuestion = false,
            None
          ).toString

        SubmitReturnRequest(
          completeReturn.copy(hasAttachments = hasAttachments),
          completeFillingOutReturnNoRepresentee.draftReturn.id,
          completeFillingOutReturnNoRepresentee.subscribedDetails,
          completeFillingOutReturnNoRepresentee.agentReferenceNumber,
          completeFillingOutReturnNoRepresentee.isFurtherOrAmendReturn.contains(true),
          B64Html(new String(Base64.getEncoder.encode(cyaPageHtml.getBytes()))),
          completeFillingOutReturnNoRepresentee.amendReturnData
        )
      }

      def submitReturnRequestForOverriddenReferenceId(
        representeeCgtReference: RepresenteeCgtReference,
        hideEstimatesQuestion: Boolean
      ): SubmitReturnRequest = {
        val cyaPge                                                     = instanceOf[views.html.returns.check_all_answers]
        implicit val requestWithSessionData: RequestWithSessionData[?] =
          RequestWithSessionData(
            None,
            AuthenticatedRequest(
              new MessagesRequest(FakeRequest(), messagesApi)
            )
          )

        implicit val messages: Messages = MessagesImpl(Lang.apply("en"), messagesApi)
        val mockedCompleteReturn        = CompleteSingleDisposalReturn
          .fromDraftReturn(
            completeDraftReturnRepresenteWithNoReference
              .copy(representeeAnswers =
                completeDraftReturnRepresenteWithNoReference.representeeAnswers
                  .map(e => e.fold(_.copy(id = Some(representeeCgtReference)), _.copy(id = representeeCgtReference)))
              )
          )
          .getOrElse(sample[CompleteSingleDisposalReturn])

        val cyaPageHtml =
          cyaPge(
            mockedCompleteReturn,
            instanceOf[RebasingEligibilityUtil],
            completeFillingOutReturnWithRepresenteeWithNoReference,
            showSubmissionDetails = true,
            hideEstimatesQuestion = hideEstimatesQuestion,
            None
          ).toString

        SubmitReturnRequest(
          mockedCompleteReturn,
          completeFillingOutReturnWithRepresenteeWithNoReference.draftReturn.id,
          completeFillingOutReturnWithRepresenteeWithNoReference.subscribedDetails,
          completeFillingOutReturnWithRepresenteeWithNoReference.agentReferenceNumber,
          completeFillingOutReturnWithRepresenteeWithNoReference.isFurtherOrAmendReturn.contains(true),
          B64Html(new String(Base64.getEncoder.encode(cyaPageHtml.getBytes()))),
          completeFillingOutReturnWithRepresenteeWithNoReference.amendReturnData
        )
      }

      behave like redirectToStartWhenInvalidJourney(
        () => performAction(),
        {
          case _: FillingOutReturn => true
          case _                   => false
        }
      )

      behave like incompleteSingleDisposalJourneyBehaviour(() => performAction(), completeDraftReturnNoRepresentee)

      "show an error page" when {
        "there is an error updating the session after a successful submission" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionWithJourney(completeFillingOutReturnNoRepresentee))
            mockStoreSession(sessionWithJourney(submittingReturn))(
              Right(())
            )
            mockSubmitReturn(submitReturnRequest, lang)(Right(submitReturnResponse))
            mockStoreSession(sessionWithJourney(justSubmittedReturn))(
              Left(Error(""))
            )
          }

          checkIsTechnicalErrorPage(performAction())
        }

        "there is an error updating the session after a submission failure the return" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionWithJourney(completeFillingOutReturnNoRepresentee))
            mockStoreSession(sessionWithJourney(submittingReturn))(
              Right(())
            )
            mockSubmitReturn(submitReturnRequest, lang)(Left(Error("")))
            mockStoreSession(
              sessionWithJourney(
                SubmitReturnFailed(
                  completeFillingOutReturnNoRepresentee.subscribedDetails,
                  completeFillingOutReturnNoRepresentee.ggCredId,
                  completeFillingOutReturnNoRepresentee.agentReferenceNumber
                )
              )
            )(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction())
        }
      }

      "redirect to the submission confirmation page" when {
        "the return has been submitted and the session has been updated" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionWithJourney(completeFillingOutReturnNoRepresentee))
            mockStoreSession(sessionWithJourney(submittingReturn))(
              Right(())
            )
            mockSubmitReturn(submitReturnRequest, lang)(Right(submitReturnResponse))
            mockStoreSession(sessionWithJourney(justSubmittedReturn))(Right(()))
          }

          checkIsRedirect(performAction(), routes.CheckAllAnswersAndSubmitController.confirmationOfSubmission())
        }

        "representees are handled correctly" in {
          val mockRepresenteeCgtReference = sample[RepresenteeCgtReference]

          val representeeAnswersNoReferenceId: CompleteRepresenteeAnswers =
            completeDraftReturnRepresenteWithNoReference.representeeAnswers
              .map(e => e.fold(_ => sample[CompleteRepresenteeAnswers], _.copy(id = NoReferenceId)))
              .getOrElse(sample[CompleteRepresenteeAnswers])

          val mockedCompleteReturn = CompleteSingleDisposalReturn
            .fromDraftReturn(
              completeDraftReturnRepresenteWithNoReference
                .copy(representeeAnswers = Some(representeeAnswersNoReferenceId.copy(id = mockRepresenteeCgtReference)))
            )
            .getOrElse(sample[CompleteSingleDisposalReturn])

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionWithJourney(completeFillingOutReturnWithRepresenteeWithNoReference))
            mockCgtRegistrationService(Right(mockRepresenteeCgtReference), representeeAnswersNoReferenceId, lang)
            mockStoreSession(sessionWithJourney(submittingReturn))(
              Right(())
            )
            mockSubmitReturn(
              submitReturnRequestForOverriddenReferenceId(mockRepresenteeCgtReference, hideEstimatesQuestion = false),
              lang
            )(
              Right(submitReturnResponse)
            )
            mockStoreSession(
              sessionWithJourney(
                justSubmittedReturn.copy(
                  subscribedDetails = completeFillingOutReturnWithRepresenteeWithNoReference.subscribedDetails,
                  completeReturn = mockedCompleteReturn
                )
              )
            )(Right(()))
          }

          checkIsRedirect(performAction(), routes.CheckAllAnswersAndSubmitController.confirmationOfSubmission())
        }

        "representees returns error" in {
          val representeeAnswersNoReferenceId: CompleteRepresenteeAnswers =
            completeDraftReturnRepresenteWithNoReference.representeeAnswers
              .map(e => e.fold(_ => sample[CompleteRepresenteeAnswers], _.copy(id = NoReferenceId)))
              .getOrElse(sample[CompleteRepresenteeAnswers])

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionWithJourney(completeFillingOutReturnWithRepresenteeWithNoReference))
            mockCgtRegistrationService(Left(Error("error thrown")), representeeAnswersNoReferenceId, lang)
          }

          checkIsTechnicalErrorPage(performAction())
        }
      }

      "redirect to the submission error page" when {
        "there is an error submitting the return" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionWithJourney(completeFillingOutReturnNoRepresentee))
            mockStoreSession(sessionWithJourney(submittingReturn))(
              Right(())
            )
            mockSubmitReturn(submitReturnRequest, lang)(Left(Error("")))
            mockStoreSession(
              sessionWithJourney(
                SubmitReturnFailed(
                  completeFillingOutReturnNoRepresentee.subscribedDetails,
                  completeFillingOutReturnNoRepresentee.ggCredId,
                  completeFillingOutReturnNoRepresentee.agentReferenceNumber
                )
              )
            )(Right(()))
          }

          checkIsRedirect(performAction(), routes.CheckAllAnswersAndSubmitController.submissionError())
        }
      }
    }

    "handling requests to display the return submit failed page" must {
      def performAction(): Future[Result] =
        controller.submissionError()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(
        () => performAction(),
        {
          case _: SubmitReturnFailed | _: Subscribed => true
          case _                                     => false
        }
      )

      "display the page" when {
        def test(journey: JourneyStatus): Unit =
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(journey)
              )
            )

            checkPageIsDisplayed(
              performAction(),
              messageFromMessageKey("submitReturnError.title"),
              { doc =>
                doc
                  .select("#content > article > form, #main-content form")
                  .attr(
                    "action"
                  ) shouldBe routes.CheckAllAnswersAndSubmitController
                  .submissionErrorSubmit()
                  .url

                doc.select(".govuk-body").text shouldBe
                  s"""${messageFromMessageKey("submitReturnError.p1")} ${messageFromMessageKey(
                      "submitReturnError.p2"
                    )} ${messageFromMessageKey("submitReturnError.p3")} ${messageFromMessageKey(
                      "submitReturnError.p4"
                    )}"""

                doc
                  .select("#main-content ol.govuk-list--number > li:nth-child(1)")
                  .text() shouldBe messageFromMessageKey(
                  "submitReturnError.li1"
                )

                doc
                  .select("#main-content ol.govuk-list--number > li:nth-child(2)")
                  .text() shouldBe messageFromMessageKey(
                  "submitReturnError.li2"
                )

                doc
                  .select("#main-content ol.govuk-list--number > li:nth-child(3)")
                  .text() shouldBe messageFromMessageKey(
                  "submitReturnError.li3"
                )
              }
            )
          }

        "the user is in the SubmitReturnFailed state" in {
          test(sample[SubmitReturnFailed])
        }

        "the user is in the subscribed state" in {
          test(sample[Subscribed])
        }
      }
    }

    "handling submits on the return submit failed page" must {
      def performAction(): Future[Result] =
        controller.submissionErrorSubmit()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(
        () => performAction(),
        {
          case _: SubmitReturnFailed | _: Subscribed => true
          case _                                     => false
        }
      )

      "redirect to the home page" when {
        def test(journey: JourneyStatus): Unit =
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(journey)
              )
            )

            checkIsRedirect(performAction(), homepage.routes.HomePageController.homepage())
          }

        "the user is in the SubmitReturnFailed state" in {
          test(sample[SubmitReturnFailed])
        }

        "the user is in the subscribed state" in {
          test(sample[Subscribed])
        }
      }
    }

    "handling requests to display the confirmation of submission page" must {
      def performAction(): Future[Result] =
        controller.confirmationOfSubmission()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(
        () => performAction(),
        {
          case _: JustSubmittedReturn => true
          case _                      => false
        }
      )

      val key             = "confirmationOfSubmission"
      val noTaxDueRefLine = s"""${messageFromMessageKey(s"$key.returnReference")} form bundle id"""
      val taxDueRefLine   = s"""${messageFromMessageKey(s"$key.paymentReference")} charge ref"""
      val submissionLine  = (
        "sent-date-table",
        messageFromMessageKey(s"$key.sentToHmrc"),
        "2 February 2020"
      )
      val addressLine     = (
        "property-address-table",
        messageFromMessageKey(s"$key.propertyAddress"),
        "123 fake street, abc123"
      )

      val companyLine = (
        "property-address-table",
        messageFromMessageKey(s"$key.companyAddress"),
        "Fake Company Name, 123 Fake Street, abc123"
      )

      val returnReferenceWithBundleId = (
        "return-reference-table",
        messageFromMessageKey(s"$key.returnReference"),
        "form bundle id"
      )
      val taxDueDateLine              = (
        "tax-due-date-table",
        messageFromMessageKey(s"$key.taxDue"),
        "1 January 2020"
      )

      val individualNameLine = (
        "account-name-table",
        messageFromMessageKey(s"$key.nameLabel"),
        "John Doe"
      )

      val agentNameLine = (
        "account-name-table",
        messageFromMessageKey(s"$key.agent.nameLabel"),
        "John Doe"
      )

      val trustNameLine = (
        "account-name-table",
        messageFromMessageKey(s"$key.trust.nameLabel"),
        "trust"
      )

      val personNameLine = (
        "account-name-table",
        messageFromMessageKey(s"$key.person.nameLabel"),
        "John Doe"
      )

      "confirmation of submission is correct" in {
        sealed case class TestScenario(
          description: String,
          userType: UserType,
          taxOwed: AmountInPence,
          submissionLine: String,
          tableLines: List[(String, String, String)],
          name: Either[TrustName, IndividualName],
          individualUserType: Option[IndividualUserType]
        )

        val scenarios = Seq(
          TestScenario(
            "user with no tax due",
            UserType.Individual,
            AmountInPence(0),
            noTaxDueRefLine,
            List(individualNameLine, submissionLine, addressLine),
            Right(IndividualName("John", "Doe")),
            Some(Self)
          ),
          TestScenario(
            "user with tax due",
            UserType.Individual,
            AmountInPence(10000),
            taxDueRefLine,
            List(
              individualNameLine,
              submissionLine,
              returnReferenceWithBundleId,
              addressLine,
              taxDueDateLine
            ),
            Right(IndividualName("John", "Doe")),
            Some(Self)
          ),
          TestScenario(
            "agent for individual with no tax due",
            UserType.Agent,
            AmountInPence(0),
            noTaxDueRefLine,
            List(agentNameLine, submissionLine, addressLine),
            Right(IndividualName("John", "Doe")),
            Some(Self)
          ),
          TestScenario(
            "agent for individual with tax due",
            UserType.Agent,
            AmountInPence(10000),
            taxDueRefLine,
            List(
              agentNameLine,
              submissionLine,
              returnReferenceWithBundleId,
              addressLine,
              taxDueDateLine
            ),
            Right(IndividualName("John", "Doe")),
            Some(Self)
          ),
          TestScenario(
            "organisation with no tax due",
            UserType.Organisation,
            AmountInPence(0),
            noTaxDueRefLine,
            List(trustNameLine, submissionLine, addressLine),
            Left(TrustName("trust")),
            None
          ),
          TestScenario(
            "organisation with tax due",
            UserType.Organisation,
            AmountInPence(10000),
            taxDueRefLine,
            List(
              trustNameLine,
              submissionLine,
              returnReferenceWithBundleId,
              addressLine,
              taxDueDateLine
            ),
            Left(TrustName("trust")),
            None
          ),
          TestScenario(
            "capacitor with no tax due",
            UserType.Individual,
            AmountInPence(0),
            noTaxDueRefLine,
            List(personNameLine, submissionLine, addressLine),
            Right(IndividualName("John", "Doe")),
            Some(Capacitor)
          ),
          TestScenario(
            "capacitor with tax due",
            UserType.Individual,
            AmountInPence(10000),
            taxDueRefLine,
            List(
              personNameLine,
              submissionLine,
              returnReferenceWithBundleId,
              addressLine,
              taxDueDateLine
            ),
            Right(IndividualName("John", "Doe")),
            Some(Capacitor)
          ),
          TestScenario(
            "personal representative with no tax due",
            UserType.Individual,
            AmountInPence(0),
            noTaxDueRefLine,
            List(
              personNameLine,
              submissionLine,
              addressLine
            ),
            Right(IndividualName("John", "Doe")),
            Some(PersonalRepresentative)
          ),
          TestScenario(
            "personal representative with tax due",
            UserType.Individual,
            AmountInPence(10000),
            taxDueRefLine,
            List(
              personNameLine,
              submissionLine,
              returnReferenceWithBundleId,
              addressLine,
              taxDueDateLine
            ),
            Right(IndividualName("John", "Doe")),
            Some(PersonalRepresentative)
          ),
          TestScenario(
            "Estate in period of administration with tax due",
            UserType.Individual,
            AmountInPence(10000),
            noTaxDueRefLine,
            List(
              personNameLine,
              submissionLine,
              returnReferenceWithBundleId,
              addressLine
            ),
            Right(IndividualName("John", "Doe")),
            Some(PersonalRepresentativeInPeriodOfAdmin)
          ),
          TestScenario(
            "Agent of estate in period of administration with tax due",
            UserType.Agent,
            AmountInPence(10000),
            noTaxDueRefLine,
            List(
              personNameLine,
              submissionLine,
              returnReferenceWithBundleId,
              addressLine
            ),
            Right(IndividualName("John", "Doe")),
            Some(PersonalRepresentativeInPeriodOfAdmin)
          )
        )

        scenarios.foreach {
          case TestScenario(
                description,
                userType,
                taxOwed,
                reference,
                expectedTable,
                name,
                individualUserType
              ) =>
            withClue(description) {
              val address            = sample[UkAddress].copy(
                line1 = "123 fake street",
                line2 = None,
                town = None,
                county = None,
                postcode = Postcode("abc123")
              )
              val processingDate     = LocalDate.of(2020, 2, 2)
              val dueDate            = LocalDate.of(2020, 1, 1)
              val chargeReference    = "charge ref"
              val returnResponse     = sample[SubmitReturnResponse].copy(
                formBundleId = "form bundle id",
                processingDate = LocalDateTime.of(processingDate, LocalTime.of(1, 1)),
                charge = Some(
                  sample[ReturnCharge].copy(
                    amount = taxOwed,
                    chargeReference = chargeReference,
                    dueDate = dueDate
                  )
                ),
                deltaCharge = None
              )
              val representeeAnswers = individualUserType match {
                case Some(_: RepresentativeType) =>
                  Some(
                    sample[CompleteRepresenteeAnswers]
                      .copy(
                        dateOfDeath = Some(sample[DateOfDeath]),
                        name = IndividualName("John", "Doe")
                      )
                  )
                case _                           => None
              }
              val triageAnswers      =
                sample[CompleteSingleDisposalTriageAnswers]
                  .copy(individualUserType = individualUserType)

              val justSubmittedReturn = sample[JustSubmittedReturn].copy(
                submissionResponse = returnResponse,
                subscribedDetails = sample[SubscribedDetails].copy(name = name),
                completeReturn = sample[CompleteSingleDisposalReturn].copy(
                  propertyAddress = address,
                  triageAnswers = triageAnswers,
                  representeeAnswers = representeeAnswers
                ),
                agentReferenceNumber = if (userType === UserType.Agent) Some(sample[AgentReferenceNumber]) else None
              )

              val userKey = userMessageKey(individualUserType, userType)

              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithJourney(justSubmittedReturn)
                    .copy(userType = Some(userType))
                )
              }

              checkPageIsDisplayed(
                performAction(),
                messageFromMessageKey("confirmationOfSubmission.title"),
                { doc =>
                  doc.select("#ref-id").text() shouldBe reference

                  expectedTable.map { tableDetails =>
                    doc
                      .select(s"#${tableDetails._1}-question")
                      .text() shouldBe tableDetails._2
                    doc
                      .select(s"#${tableDetails._1}-answer")
                      .text() shouldBe tableDetails._3
                  }

                  doc
                    .select("#printPage")
                    .html() shouldBe messageFromMessageKey(
                    if (
                      individualUserType
                        .contains(Capacitor) && representeeAnswers
                        .exists(
                          _.fold(_.id, c => Some(c.id)).contains(NoReferenceId)
                        )
                    ) {
                      s"confirmationOfSubmission.capacitor.noId.printPage"
                    } else {
                      s"confirmationOfSubmission$userKey.printPage"
                    },
                    "#print-dialogue"
                  )

                  doc.select("#howToPay").html() shouldBe messageFromMessageKey(
                    s"confirmationOfSubmission$userKey.howToPay.p1",
                    chargeReference
                  )

                  if (!userKey.contains("personalRepInPeriodOfAdmin")) {
                    doc
                      .select("#ifSaHeading")
                      .text() shouldBe messageFromMessageKey(
                      s"confirmationOfSubmission$userKey.ifSa"
                    )
                  }
                }
              )
            }
        }
      }

      "confirmation of submission is correct for multiple indirect disposal" in {
        sealed case class TestScenario(
          description: String,
          userType: UserType,
          taxOwed: AmountInPence,
          submissionLine: String,
          tableLines: List[(String, String, String)],
          name: Either[TrustName, IndividualName],
          individualUserType: Option[IndividualUserType]
        )

        val scenarios = Seq(
          TestScenario(
            "user with no tax due",
            UserType.Individual,
            AmountInPence(0),
            noTaxDueRefLine,
            List(individualNameLine, submissionLine, companyLine),
            Right(IndividualName("John", "Doe")),
            Some(Self)
          ),
          TestScenario(
            "user with tax due",
            UserType.Individual,
            AmountInPence(10000),
            taxDueRefLine,
            List(
              individualNameLine,
              submissionLine,
              returnReferenceWithBundleId,
              companyLine,
              taxDueDateLine
            ),
            Right(IndividualName("John", "Doe")),
            Some(Self)
          ),
          TestScenario(
            "agent for individual with no tax due",
            UserType.Agent,
            AmountInPence(0),
            noTaxDueRefLine,
            List(agentNameLine, submissionLine, companyLine),
            Right(IndividualName("John", "Doe")),
            Some(Self)
          ),
          TestScenario(
            "agent for individual with tax due",
            UserType.Agent,
            AmountInPence(10000),
            taxDueRefLine,
            List(
              agentNameLine,
              submissionLine,
              returnReferenceWithBundleId,
              companyLine,
              taxDueDateLine
            ),
            Right(IndividualName("John", "Doe")),
            Some(Self)
          ),
          TestScenario(
            "organisation with no tax due",
            UserType.Organisation,
            AmountInPence(0),
            noTaxDueRefLine,
            List(trustNameLine, submissionLine, companyLine),
            Left(TrustName("trust")),
            None
          ),
          TestScenario(
            "organisation with tax due",
            UserType.Organisation,
            AmountInPence(10000),
            taxDueRefLine,
            List(
              trustNameLine,
              submissionLine,
              returnReferenceWithBundleId,
              companyLine,
              taxDueDateLine
            ),
            Left(TrustName("trust")),
            None
          ),
          TestScenario(
            "capacitor with no tax due",
            UserType.Individual,
            AmountInPence(0),
            noTaxDueRefLine,
            List(personNameLine, submissionLine, companyLine),
            Right(IndividualName("John", "Doe")),
            Some(Capacitor)
          ),
          TestScenario(
            "capacitor with tax due",
            UserType.Individual,
            AmountInPence(10000),
            taxDueRefLine,
            List(
              personNameLine,
              submissionLine,
              returnReferenceWithBundleId,
              companyLine,
              taxDueDateLine
            ),
            Right(IndividualName("John", "Doe")),
            Some(Capacitor)
          ),
          TestScenario(
            "personal representative with no tax due",
            UserType.Individual,
            AmountInPence(0),
            noTaxDueRefLine,
            List(personNameLine, submissionLine, companyLine),
            Right(IndividualName("John", "Doe")),
            Some(PersonalRepresentative)
          ),
          TestScenario(
            "personal representative with tax due",
            UserType.Individual,
            AmountInPence(10000),
            taxDueRefLine,
            List(
              personNameLine,
              submissionLine,
              returnReferenceWithBundleId,
              companyLine,
              taxDueDateLine
            ),
            Right(IndividualName("John", "Doe")),
            Some(PersonalRepresentative)
          )
        )

        scenarios.foreach {
          case TestScenario(
                description,
                userType,
                taxOwed,
                reference,
                expectedTable,
                name,
                individualUserType
              ) =>
            withClue(description) {
              val address = sample[UkAddress].copy(
                line1 = "Fake Company Name",
                line2 = Some("123 Fake Street"),
                town = None,
                county = None,
                postcode = Postcode("abc123")
              )

              val exampleCompanyDetailsAnswers = sample[CompleteExampleCompanyDetailsAnswers].copy(
                address = address
              )
              val processingDate               = LocalDate.of(2020, 2, 2)
              val dueDate                      = LocalDate.of(2020, 1, 1)
              val chargeReference              = "charge ref"
              val returnResponse               = sample[SubmitReturnResponse].copy(
                formBundleId = "form bundle id",
                processingDate = LocalDateTime.of(processingDate, LocalTime.of(1, 1)),
                charge = Some(
                  sample[ReturnCharge].copy(
                    amount = taxOwed,
                    chargeReference = chargeReference,
                    dueDate = dueDate
                  )
                ),
                deltaCharge = None
              )
              val representeeAnswers           = individualUserType match {
                case Some(PersonalRepresentative) =>
                  Some(
                    sample[CompleteRepresenteeAnswers]
                      .copy(
                        dateOfDeath = Some(sample[DateOfDeath]),
                        name = IndividualName("John", "Doe")
                      )
                  )
                case Some(Capacitor)              =>
                  Some(
                    sample[CompleteRepresenteeAnswers].copy(
                      dateOfDeath = None,
                      name = IndividualName("John", "Doe")
                    )
                  )

                case _ => None
              }
              val triageAnswers                =
                sample[CompleteMultipleDisposalsTriageAnswers]
                  .copy(individualUserType = individualUserType)

              val justSubmittedReturn = sample[JustSubmittedReturn].copy(
                submissionResponse = returnResponse,
                subscribedDetails = sample[SubscribedDetails].copy(name = name),
                completeReturn = sample[CompleteMultipleIndirectDisposalReturn].copy(
                  exampleCompanyDetailsAnswers = exampleCompanyDetailsAnswers,
                  triageAnswers = triageAnswers,
                  representeeAnswers = representeeAnswers
                ),
                agentReferenceNumber = if (userType === UserType.Agent) Some(sample[AgentReferenceNumber]) else None
              )

              val userKey = userMessageKey(individualUserType, userType)

              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithJourney(justSubmittedReturn)
                    .copy(userType = Some(userType))
                )
              }

              checkPageIsDisplayed(
                performAction(),
                messageFromMessageKey("confirmationOfSubmission.title"),
                { doc =>
                  doc.select("#ref-id").text() shouldBe reference

                  expectedTable.map { tableDetails =>
                    doc
                      .select(s"#${tableDetails._1}-question")
                      .text() shouldBe tableDetails._2
                    doc
                      .select(s"#${tableDetails._1}-answer")
                      .text() shouldBe tableDetails._3
                  }

                  doc
                    .select("#printPage")
                    .html() shouldBe messageFromMessageKey(
                    if (
                      individualUserType
                        .contains(Capacitor) && representeeAnswers
                        .exists(
                          _.fold(_.id, c => Some(c.id)).contains(NoReferenceId)
                        )
                    ) {
                      s"confirmationOfSubmission.capacitor.noId.printPage"
                    } else {
                      s"confirmationOfSubmission$userKey.printPage"
                    },
                    "#print-dialogue"
                  )

                  doc.select("#howToPay").html() shouldBe messageFromMessageKey(
                    s"confirmationOfSubmission$userKey.howToPay.p1",
                    chargeReference
                  )

                  doc
                    .select("#ifSaHeading")
                    .text() shouldBe messageFromMessageKey(
                    s"confirmationOfSubmission$userKey.ifSa"
                  )
                }
              )
            }
        }
      }

      "display the 30 days late filing penalty warning for an individual if the completion date is before 27-OCT-2021" in {
        val completionDate      = CompletionDate(LocalDate.of(2021, 9, 10))
        val justSubmittedReturn = sample[JustSubmittedReturn].copy(
          subscribedDetails = sample[SubscribedDetails].copy(
            name = Right(sample[IndividualName])
          ),
          completeReturn = sample[CompleteSingleDisposalReturn].copy(
            triageAnswers = sample[CompleteSingleDisposalTriageAnswers].copy(
              individualUserType = Some(Self),
              completionDate = completionDate
            ),
            representeeAnswers = None
          ),
          submissionResponse = sample[SubmitReturnResponse].copy(
            charge = Some(sample[ReturnCharge]),
            deltaCharge = None
          ),
          amendReturnData = None
        )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            sessionWithJourney(justSubmittedReturn)
              .copy(userType = Some(UserType.Individual))
          )
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("confirmationOfSubmission.title"),
          _.select("#lfpWarning").text shouldBe messageFromMessageKey("confirmationOfSubmission.penalty.warning")
        )
      }

      "display the 30 days late filing penalty warning for a trust if the completion date is before 27-OCT-2021" in {
        val completionDate      = CompletionDate(LocalDate.of(2021, 9, 10))
        val justSubmittedReturn = sample[JustSubmittedReturn].copy(
          subscribedDetails = sample[SubscribedDetails].copy(
            name = Left(sample[TrustName])
          ),
          completeReturn = sample[CompleteSingleDisposalReturn].copy(
            triageAnswers = sample[CompleteSingleDisposalTriageAnswers].copy(
              individualUserType = Some(Self),
              completionDate = completionDate
            ),
            representeeAnswers = None
          ),
          submissionResponse = sample[SubmitReturnResponse].copy(
            charge = Some(sample[ReturnCharge]),
            deltaCharge = None
          ),
          amendReturnData = None
        )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            sessionWithJourney(justSubmittedReturn)
              .copy(userType = Some(UserType.Organisation))
          )
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("confirmationOfSubmission.title"),
          _.select("#lfpWarning").text shouldBe messageFromMessageKey("confirmationOfSubmission.trust.penalty.warning")
        )
      }

      "display the 60 days late filing penalty warning for an individual if the completion date is after 27-OCT-2021" in {
        val completionDate      = CompletionDate(LocalDate.now().minusDays(61L))
        val justSubmittedReturn = sample[JustSubmittedReturn].copy(
          subscribedDetails = sample[SubscribedDetails].copy(
            name = Right(sample[IndividualName])
          ),
          completeReturn = sample[CompleteSingleDisposalReturn].copy(
            triageAnswers = sample[CompleteSingleDisposalTriageAnswers].copy(
              individualUserType = Some(Self),
              completionDate = completionDate
            ),
            representeeAnswers = None
          ),
          submissionResponse = sample[SubmitReturnResponse].copy(
            charge = Some(sample[ReturnCharge]),
            deltaCharge = None
          ),
          amendReturnData = None
        )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            sessionWithJourney(justSubmittedReturn)
              .copy(userType = Some(UserType.Individual))
          )
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("confirmationOfSubmission.title"),
          _.select("#lfpWarning").text shouldBe messageFromMessageKey("confirmationOfSubmission.60days.penalty.warning")
        )
      }

      "display the 60 days late filing penalty warning for a trust if the completion date is after 27-OCT-2021" in {
        val completionDate      = CompletionDate(LocalDate.now().minusDays(61L))
        val justSubmittedReturn = sample[JustSubmittedReturn].copy(
          subscribedDetails = sample[SubscribedDetails].copy(
            name = Left(sample[TrustName])
          ),
          completeReturn = sample[CompleteSingleDisposalReturn].copy(
            triageAnswers = sample[CompleteSingleDisposalTriageAnswers].copy(
              individualUserType = Some(Self),
              completionDate = completionDate
            ),
            representeeAnswers = None
          ),
          submissionResponse = sample[SubmitReturnResponse].copy(
            charge = Some(sample[ReturnCharge]),
            deltaCharge = None
          ),
          amendReturnData = None
        )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            sessionWithJourney(justSubmittedReturn)
              .copy(userType = Some(UserType.Organisation))
          )
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("confirmationOfSubmission.title"),
          _.select("#lfpWarning").text shouldBe messageFromMessageKey(
            "confirmationOfSubmission.trust.60days.penalty.warning"
          )
        )
      }

      "not display the 60 days late filing penalty warning for an individual if the completion date is after 27-OCT-2021 but before 60 days" in {
        val completionDate      = CompletionDate(LocalDate.now().minusDays(59L))
        val justSubmittedReturn = sample[JustSubmittedReturn].copy(
          subscribedDetails = sample[SubscribedDetails].copy(
            name = Right(sample[IndividualName])
          ),
          completeReturn = sample[CompleteSingleDisposalReturn].copy(
            triageAnswers = sample[CompleteSingleDisposalTriageAnswers].copy(
              individualUserType = Some(Self),
              completionDate = completionDate
            ),
            representeeAnswers = None
          ),
          submissionResponse = sample[SubmitReturnResponse].copy(
            charge = Some(sample[ReturnCharge]),
            deltaCharge = None
          ),
          amendReturnData = None
        )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            sessionWithJourney(justSubmittedReturn)
              .copy(userType = Some(UserType.Individual))
          )
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("confirmationOfSubmission.title"),
          _.select("#lfpWarning").text shouldBe ""
        )
      }

      "not display the 60 days late filing penalty warning for a trust if the completion date is after 27-OCT-2021 but before 60 days" in {
        val completionDate      = CompletionDate(LocalDate.now().minusDays(59L))
        val justSubmittedReturn = sample[JustSubmittedReturn].copy(
          subscribedDetails = sample[SubscribedDetails].copy(
            name = Left(sample[TrustName])
          ),
          completeReturn = sample[CompleteSingleDisposalReturn].copy(
            triageAnswers = sample[CompleteSingleDisposalTriageAnswers].copy(
              individualUserType = Some(Self),
              completionDate = completionDate
            ),
            representeeAnswers = None
          ),
          submissionResponse = sample[SubmitReturnResponse].copy(
            charge = Some(sample[ReturnCharge]),
            deltaCharge = None
          ),
          amendReturnData = None
        )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            sessionWithJourney(justSubmittedReturn)
              .copy(userType = Some(UserType.Organisation))
          )
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("confirmationOfSubmission.title"),
          _.select("#lfpWarning").text shouldBe ""
        )
      }

      "not display the late filing penalty warning" when {
        "if the completion date is not more than 30 days before today" in {
          val completionDate      = CompletionDate(LocalDate.now().minusDays(30L))
          val justSubmittedReturn = sample[JustSubmittedReturn].copy(
            completeReturn = sample[CompleteSingleDisposalReturn].copy(
              triageAnswers = sample[CompleteSingleDisposalTriageAnswers].copy(
                individualUserType = Some(Self),
                completionDate = completionDate
              ),
              representeeAnswers = None
            ),
            submissionResponse = sample[SubmitReturnResponse].copy(
              charge = Some(sample[ReturnCharge]),
              deltaCharge = None
            ),
            amendReturnData = None
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithJourney(justSubmittedReturn)
                .copy(userType = Some(UserType.Individual))
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("confirmationOfSubmission.title"),
            _.select("#lfpWarning").text shouldBe ""
          )
        }

        "the return has been amended but the completion date has not been changed" in {
          val completionDate      = CompletionDate(LocalDate.now().minusDays(31L))
          val justSubmittedReturn = sample[JustSubmittedReturn].copy(
            completeReturn = sample[CompleteSingleDisposalReturn].copy(
              triageAnswers = sample[CompleteSingleDisposalTriageAnswers].copy(
                individualUserType = Some(Self),
                completionDate = completionDate
              ),
              representeeAnswers = None
            ),
            submissionResponse = sample[SubmitReturnResponse].copy(
              charge = Some(sample[ReturnCharge]),
              deltaCharge = None
            ),
            amendReturnData = Some(
              sample[AmendReturnData].copy(
                originalReturn = sample[CompleteReturnWithSummary].copy(
                  completeReturn = sample[CompleteSingleDisposalReturn]
                    .copy(
                      triageAnswers = sample[CompleteSingleDisposalTriageAnswers].copy(
                        completionDate = completionDate
                      )
                    )
                )
              )
            )
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithJourney(justSubmittedReturn)
                .copy(userType = Some(UserType.Individual))
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("confirmationOfSubmission.title"),
            _.select("#lfpWarning").text shouldBe ""
          )
        }
      }

      "display the delta charge table instead of summary due date" when {
        "an amended return has a delta charge" in {
          val completionDate      = CompletionDate(LocalDate.now().minusDays(30L))
          val justSubmittedReturn = sample[JustSubmittedReturn].copy(
            completeReturn = sample[CompleteSingleDisposalReturn].copy(
              triageAnswers = sample[CompleteSingleDisposalTriageAnswers].copy(
                individualUserType = Some(Self),
                completionDate = completionDate
              ),
              representeeAnswers = None
            ),
            submissionResponse = sample[SubmitReturnResponse].copy(
              charge = Some(sample[ReturnCharge]),
              deltaCharge = Some(
                DeltaCharge(
                  originalCharge = sample[ReturnCharge],
                  deltaCharge = sample[ReturnCharge]
                )
              )
            ),
            amendReturnData = None
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithJourney(justSubmittedReturn)
                .copy(userType = Some(UserType.Individual))
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("confirmationOfSubmission.title"),
            { doc =>
              doc.select("#delta-charge-due-date").text shouldBe
                justSubmittedReturn.submissionResponse.deltaCharge
                  .map(d => TimeUtils.govDisplayFormat(d.deltaCharge.dueDate))
                  .getOrElse("")

              doc.select("#tax-due-date-table").text shouldBe ""
            }
          )
        }
      }
    }

    "handling requests to pay a return" must {
      def performAction(): Future[Result] =
        controller.payReturn()(FakeRequest())

      def justSubmittedReturnWithCharge(
        charge: Option[ReturnCharge]
      ): JustSubmittedReturn =
        sample[JustSubmittedReturn].copy(submissionResponse =
          sample[SubmitReturnResponse].copy(
            charge = charge,
            deltaCharge = None
          )
        )

      behave like redirectToStartWhenInvalidJourney(
        () => performAction(),
        {
          case _: JustSubmittedReturn => true
          case _                      => false
        }
      )

      "redirect to the homepage" when {
        "there is no charge" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithJourney(justSubmittedReturnWithCharge(None))
            )
          }

          checkIsRedirect(performAction(), homepage.routes.HomePageController.homepage())
        }
      }

      "show an error page" when {
        "there is an error starting a payments journey" in {
          val charge              = sample[ReturnCharge]
          val justSubmittedReturn = justSubmittedReturnWithCharge(Some(charge))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionWithJourney(justSubmittedReturn))
            mockStartPaymentJourney(
              justSubmittedReturn.subscribedDetails.cgtReference,
              Some(charge.chargeReference),
              charge.amount,
              Some(charge.dueDate),
              homepage.routes.HomePageController.homepage(),
              routes.CheckAllAnswersAndSubmitController
                .confirmationOfSubmission()
            )(Left(Error("")))

            checkIsTechnicalErrorPage(performAction())
          }
        }
      }

      "redirect to the payment journey next url" when {
        "the payments journey has been successfully started" in {
          val paymentJourney      = PaymentsJourney("/next", "id")
          val charge              = sample[ReturnCharge]
          val justSubmittedReturn = justSubmittedReturnWithCharge(Some(charge))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionWithJourney(justSubmittedReturn))
            mockStartPaymentJourney(
              justSubmittedReturn.subscribedDetails.cgtReference,
              Some(charge.chargeReference),
              charge.amount,
              Some(charge.dueDate),
              homepage.routes.HomePageController.homepage(),
              routes.CheckAllAnswersAndSubmitController
                .confirmationOfSubmission()
            )(Right(paymentJourney))

            checkIsRedirect(performAction(), paymentJourney.nextUrl)
          }
        }
      }
    }
  }

  private def incompleteSingleDisposalJourneyBehaviour(
    performAction: () => Future[Result],
    completeDraftReturn: DraftSingleDisposalReturn
  ): Unit = {
    val makeIncompleteFunctions =
      List[DraftSingleDisposalReturn => DraftSingleDisposalReturn](
        _.copy(triageAnswers = sample[IncompleteSingleDisposalTriageAnswers]),
        _.copy(propertyAddress = None),
        _.copy(disposalDetailsAnswers = Some(sample[IncompleteDisposalDetailsAnswers])),
        _.copy(disposalDetailsAnswers = None),
        _.copy(acquisitionDetailsAnswers = Some(sample[IncompleteAcquisitionDetailsAnswers])),
        _.copy(acquisitionDetailsAnswers = None),
        _.copy(reliefDetailsAnswers = Some(sample[IncompleteReliefDetailsAnswers])),
        _.copy(reliefDetailsAnswers = None),
        _.copy(exemptionAndLossesAnswers = Some(sample[IncompleteExemptionAndLossesAnswers])),
        _.copy(exemptionAndLossesAnswers = None),
        _.copy(yearToDateLiabilityAnswers = Some(sample[IncompleteCalculatedYTDAnswers])),
        _.copy(yearToDateLiabilityAnswers = None)
      )

    "redirect to the task list" when {
      "the return is not complete" in {
        makeIncompleteFunctions.foreach { makeIncomplete =>
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithJourney(
                sample[FillingOutReturn].copy(
                  draftReturn = makeIncomplete(completeDraftReturn),
                  amendReturnData = None
                )
              )
            )
          }

          checkIsRedirect(performAction(), routes.TaskListController.taskList())
        }
      }
    }
  }

  private def incompleteMultipleDisposalsJourneyBehaviour(
    performAction: () => Future[Result],
    completeDraftReturn: DraftMultipleDisposalsReturn
  ): Unit = {
    val makeIncompleteFunctions =
      List[DraftMultipleDisposalsReturn => DraftMultipleDisposalsReturn](
        _.copy(triageAnswers = sample[IncompleteMultipleDisposalsTriageAnswers]),
        _.copy(examplePropertyDetailsAnswers = None),
        _.copy(examplePropertyDetailsAnswers = Some(sample[IncompleteExamplePropertyDetailsAnswers])),
        _.copy(exemptionAndLossesAnswers = Some(sample[IncompleteExemptionAndLossesAnswers])),
        _.copy(exemptionAndLossesAnswers = None),
        _.copy(yearToDateLiabilityAnswers = Some(sample[IncompleteCalculatedYTDAnswers])),
        _.copy(yearToDateLiabilityAnswers = None)
      )

    "redirect to the task list" when {
      "the return is not complete" in {
        makeIncompleteFunctions.foreach { makeIncomplete =>
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithJourney(
                sample[FillingOutReturn].copy(
                  draftReturn = makeIncomplete(completeDraftReturn),
                  amendReturnData = None
                )
              )
            )
          }

          checkIsRedirect(performAction(), routes.TaskListController.taskList())
        }
      }
    }
  }

  private def incompleteSingleIndirectDisposalJourneyBehaviour(
    performAction: () => Future[Result],
    completeDraftReturn: DraftSingleIndirectDisposalReturn
  ): Unit = {
    val makeIncompleteFunctions =
      List[DraftSingleIndirectDisposalReturn => DraftSingleIndirectDisposalReturn](
        _.copy(triageAnswers = sample[IncompleteSingleDisposalTriageAnswers]),
        _.copy(companyAddress = None),
        _.copy(disposalDetailsAnswers = Some(sample[IncompleteDisposalDetailsAnswers])),
        _.copy(disposalDetailsAnswers = None),
        _.copy(acquisitionDetailsAnswers = Some(sample[IncompleteAcquisitionDetailsAnswers])),
        _.copy(acquisitionDetailsAnswers = None),
        _.copy(exemptionAndLossesAnswers = Some(sample[IncompleteExemptionAndLossesAnswers])),
        _.copy(exemptionAndLossesAnswers = None),
        _.copy(yearToDateLiabilityAnswers = Some(sample[IncompleteNonCalculatedYTDAnswers])),
        _.copy(yearToDateLiabilityAnswers = None)
      )

    "redirect to the task list" when {
      "the return is not complete" in {
        makeIncompleteFunctions.foreach { makeIncomplete =>
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithJourney(
                sample[FillingOutReturn].copy(
                  draftReturn = makeIncomplete(completeDraftReturn),
                  amendReturnData = None
                )
              )
            )
          }

          checkIsRedirect(performAction(), routes.TaskListController.taskList())
        }
      }

      "the return contains calculated answers" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            sessionWithJourney(
              sample[FillingOutReturn].copy(
                draftReturn =
                  completeDraftReturn.copy(yearToDateLiabilityAnswers = Some(sample[CompleteCalculatedYTDAnswers])),
                amendReturnData = None
              )
            )
          )
        }

        checkIsRedirect(performAction(), routes.TaskListController.taskList())

      }
    }
  }

  private def incompleteMultipleIndirectDisposalsJourneyBehaviour(
    performAction: () => Future[Result],
    completeDraftReturn: DraftMultipleIndirectDisposalsReturn
  ): Unit = {
    val makeIncompleteFunctions =
      List[DraftMultipleIndirectDisposalsReturn => DraftMultipleIndirectDisposalsReturn](
        _.copy(triageAnswers = sample[IncompleteMultipleDisposalsTriageAnswers]),
        _.copy(exampleCompanyDetailsAnswers = None),
        _.copy(exampleCompanyDetailsAnswers = Some(sample[IncompleteExampleCompanyDetailsAnswers])),
        _.copy(exemptionAndLossesAnswers = Some(sample[IncompleteExemptionAndLossesAnswers])),
        _.copy(exemptionAndLossesAnswers = None),
        _.copy(yearToDateLiabilityAnswers = Some(sample[IncompleteCalculatedYTDAnswers])),
        _.copy(yearToDateLiabilityAnswers = None)
      )

    "redirect to the task list" when {
      "the return is not complete" in {
        makeIncompleteFunctions.foreach { makeIncomplete =>
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithJourney(
                sample[FillingOutReturn].copy(
                  draftReturn = makeIncomplete(completeDraftReturn),
                  amendReturnData = None
                )
              )
            )
          }

          checkIsRedirect(performAction(), routes.TaskListController.taskList())
        }
      }
    }
  }

  private def incompleteSingleMixedUseDisposalsJourneyBehaviour(
    performAction: () => Future[Result],
    completeDraftReturn: DraftSingleMixedUseDisposalReturn
  ): Unit = {
    val makeIncompleteFunctions =
      List[DraftSingleMixedUseDisposalReturn => DraftSingleMixedUseDisposalReturn](
        _.copy(triageAnswers = sample[IncompleteSingleDisposalTriageAnswers]),
        _.copy(mixedUsePropertyDetailsAnswers = None),
        _.copy(mixedUsePropertyDetailsAnswers = Some(sample[IncompleteMixedUsePropertyDetailsAnswers])),
        _.copy(exemptionAndLossesAnswers = Some(sample[IncompleteExemptionAndLossesAnswers])),
        _.copy(exemptionAndLossesAnswers = None),
        _.copy(yearToDateLiabilityAnswers = Some(sample[IncompleteCalculatedYTDAnswers])),
        _.copy(yearToDateLiabilityAnswers = None)
      )

    "redirect to the task list" when {
      "the return is not complete" in {
        makeIncompleteFunctions.foreach { makeIncomplete =>
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithJourney(
                sample[FillingOutReturn].copy(
                  draftReturn = makeIncomplete(completeDraftReturn),
                  amendReturnData = None
                )
              )
            )
          }

          checkIsRedirect(performAction(), routes.TaskListController.taskList())
        }
      }
    }
  }
}

object CheckAllAnswersAndSubmitControllerSpec {
  def validateSingleDisposalCheckAllYourAnswersSections(
    doc: Document,
    completeReturn: CompleteSingleDisposalReturn,
    userType: Option[UserType],
    isUk: Boolean,
    isRebasing: Boolean,
    isATrust: Boolean,
    assetType: AssetType,
    isFurtherOrAmendReturn: Boolean,
    hideEstimatesQuestion: Boolean,
    showAnnualExemptAmount: Boolean
  )(implicit messages: MessagesApi, lang: Lang): Unit = {

    completeReturn.representeeAnswers.foreach(
      RepresenteeControllerSpec.validateRepresenteeCheckYourAnswersPage(_, doc)
    )

    validateSingleDisposalTriageCheckYourAnswersPage(
      completeReturn.triageAnswers,
      userType,
      doc
    )

    validateAcquisitionDetailsCheckYourAnswersPage(
      completeReturn.acquisitionDetails,
      doc,
      isUk,
      isRebasing,
      assetType
    )

    validateDisposalDetailsCheckYourAnswersPage(
      completeReturn.disposalDetails,
      doc,
      completeReturn.isIndirectDisposal
    )

    validateReliefDetailsCheckYourAnswersPage(
      completeReturn.reliefDetails,
      doc
    )

    completeReturn.gainOrLossAfterReliefs.foreach { gainOrLossAfterReliefs =>
      validateGainOrLossOrReliefsCheckYourAnswersPage(
        gainOrLossAfterReliefs,
        doc,
        isATrust,
        userType.contains(UserType.Agent),
        completeReturn.triageAnswers.individualUserType,
        isMultipleDisposal = false
      )

      completeReturn.initialGainOrLoss.foreach { initialGainOrLoss =>
        doc.select("#initialGainOrLoss-answer").text() should endWith(
          MoneyUtils.formatAmountOfMoneyWithPoundSign(initialGainOrLoss.inPounds().abs)
        )
      }
    }

    completeReturn.triageAnswers.individualUserType.foreach { individualUserType =>
      validateExemptionAndLossesCheckYourAnswersPage(
        completeReturn.exemptionsAndLossesDetails,
        doc,
        isATrust,
        userType.contains(UserType.Agent),
        individualUserType,
        showAnnualExemptAmount
      )
      completeReturn.yearToDateLiabilityAnswers.fold(
        validateNonCalculatedYearToDateLiabilityPage(
          _,
          doc,
          userType,
          Some(individualUserType),
          isFurtherOrAmendReturn,
          hideEstimatesQuestion
        ),
        validateCalculatedYearToDateLiabilityPage(_, isATrust, hideEstimatesQuestion, doc)
      )
    }
  }

  def validateMultipleDisposalsCheckAllYourAnswersSections(
    doc: Document,
    completeReturn: CompleteMultipleDisposalsReturn,
    userType: Option[UserType],
    isATrust: Boolean,
    isFurtherOrAmendReturn: Boolean,
    hideEstimatesQuestion: Boolean,
    showAnnualExemptAmount: Boolean
  )(implicit messages: MessagesApi, lang: Lang): Unit = {
    completeReturn.representeeAnswers.foreach(
      RepresenteeControllerSpec.validateRepresenteeCheckYourAnswersPage(_, doc)
    )

    validateMultipleDisposalsTriageCheckYourAnswersPage(
      completeReturn.triageAnswers,
      userType,
      doc
    )

    validateExamplePropertyDetailsSummary(
      completeReturn.examplePropertyDetailsAnswers,
      doc
    )

    completeReturn.triageAnswers.individualUserType.foreach { individualUserType =>
      validateExemptionAndLossesCheckYourAnswersPage(
        completeReturn.exemptionAndLossesAnswers,
        doc,
        isATrust,
        userType.contains(UserType.Agent),
        individualUserType,
        showAnnualExemptAmount
      )
      validateNonCalculatedYearToDateLiabilityPage(
        completeReturn.yearToDateLiabilityAnswers,
        doc,
        userType,
        Some(individualUserType),
        isFurtherOrAmendReturn,
        hideEstimatesQuestion
      )
    }
  }

  private def validateSingleIndirectDisposalCheckAllYourAnswersSections(
    doc: Document,
    completeReturn: CompleteSingleIndirectDisposalReturn,
    userType: Option[UserType],
    isRebasing: Boolean,
    isATrust: Boolean,
    assetType: AssetType,
    isFurtherOrAmendReturn: Boolean,
    hideEstimatesQuestion: Boolean,
    showAnnualExemptAmount: Boolean
  )(implicit messages: MessagesApi, lang: Lang): Unit = {

    completeReturn.representeeAnswers.foreach(
      RepresenteeControllerSpec.validateRepresenteeCheckYourAnswersPage(_, doc)
    )

    validateSingleDisposalTriageCheckYourAnswersPage(
      completeReturn.triageAnswers,
      userType,
      doc
    )

    validateAcquisitionDetailsCheckYourAnswersPage(
      completeReturn.acquisitionDetails,
      doc,
      isUk = false,
      isRebasing,
      assetType
    )

    validateDisposalDetailsCheckYourAnswersPage(
      completeReturn.disposalDetails,
      doc,
      completeReturn.isIndirectDisposal
    )

    completeReturn.triageAnswers.individualUserType.foreach { individualUserType =>
      validateExemptionAndLossesCheckYourAnswersPage(
        completeReturn.exemptionsAndLossesDetails,
        doc,
        isATrust,
        userType.contains(UserType.Agent),
        individualUserType,
        showAnnualExemptAmount
      )

      validateNonCalculatedYearToDateLiabilityPage(
        completeReturn.yearToDateLiabilityAnswers,
        doc,
        userType,
        Some(individualUserType),
        isFurtherOrAmendReturn,
        hideEstimatesQuestion
      )
    }
  }

  def validateMultipleIndirectDisposalsCheckAllYourAnswersSections(
    doc: Document,
    completeReturn: CompleteMultipleIndirectDisposalReturn,
    userType: Option[UserType],
    isATrust: Boolean,
    isFurtherOrAmendReturn: Boolean,
    hideEstimatesQuestion: Boolean,
    showAnnualExemptAmount: Boolean
  )(implicit messages: MessagesApi, lang: Lang): Unit = {
    completeReturn.representeeAnswers.foreach(
      RepresenteeControllerSpec.validateRepresenteeCheckYourAnswersPage(_, doc)
    )

    validateMultipleDisposalsTriageCheckYourAnswersPage(
      completeReturn.triageAnswers,
      userType,
      doc
    )

    completeReturn.triageAnswers.individualUserType.foreach { individualUserType =>
      validateExemptionAndLossesCheckYourAnswersPage(
        completeReturn.exemptionsAndLossesDetails,
        doc,
        isATrust,
        userType.contains(UserType.Agent),
        individualUserType,
        showAnnualExemptAmount
      )
      validateNonCalculatedYearToDateLiabilityPage(
        completeReturn.yearToDateLiabilityAnswers,
        doc,
        userType,
        Some(individualUserType),
        isFurtherOrAmendReturn,
        hideEstimatesQuestion
      )
    }
  }

  private def validateSingleMixedUseDisposalsCheckAllYourAnswersSections(
    doc: Document,
    completeReturn: CompleteSingleMixedUseDisposalReturn,
    userType: Option[UserType],
    isATrust: Boolean,
    isFurtherOrAmendReturn: Boolean,
    hideEstimatesQuestion: Boolean,
    showAnnualExemptAmount: Boolean
  )(implicit messages: MessagesApi, lang: Lang): Unit = {
    completeReturn.representeeAnswers.foreach(
      RepresenteeControllerSpec.validateRepresenteeCheckYourAnswersPage(_, doc)
    )

    validateSingleDisposalTriageCheckYourAnswersPage(
      completeReturn.triageAnswers,
      userType,
      doc
    )

    completeReturn.triageAnswers.individualUserType.foreach { individualUserType =>
      validateExemptionAndLossesCheckYourAnswersPage(
        completeReturn.exemptionsAndLossesDetails,
        doc,
        isATrust,
        userType.contains(UserType.Agent),
        individualUserType,
        showAnnualExemptAmount
      )
    }
    validateNonCalculatedYearToDateLiabilityPage(
      completeReturn.yearToDateLiabilityAnswers,
      doc,
      userType,
      completeReturn.triageAnswers.individualUserType,
      isFurtherOrAmendReturn,
      hideEstimatesQuestion
    )
  }
}
