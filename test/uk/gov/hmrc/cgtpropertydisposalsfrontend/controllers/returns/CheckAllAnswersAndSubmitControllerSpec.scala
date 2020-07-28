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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns

import java.time.{LocalDate, LocalDateTime, LocalTime}
import java.util.{Base64, UUID}

import cats.data.EitherT
import cats.instances.future._
import org.jsoup.nodes.Document
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.i18n.{Lang, MessagesApi, MessagesImpl}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.{Call, MessagesRequest, Request, Result}
import play.api.test.FakeRequest
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.accounts.homepage
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedRequest, RequestWithSessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.CheckAllAnswersAndSubmitControllerSpec._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.acquisitiondetails.AcquisitionDetailsControllerSpec.validateAcquisitionDetailsCheckYourAnswersPage
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.acquisitiondetails.RebasingEligibilityUtil
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.address.MultipleDisposalsPropertyDetailsControllerSpec.validateExamplePropertyDetailsSummary
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.disposaldetails.DisposalDetailsControllerSpec._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.exemptionandlosses.ExemptionAndLossesControllerSpec.validateExemptionAndLossesCheckYourAnswersPage
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.reliefdetails.ReliefDetailsControllerSpec.validateReliefDetailsCheckYourAnswersPage
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.representee.RepresenteeControllerSpec
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.triage.MultipleDisposalsTriageControllerSpec.validateMultipleDisposalsTriageCheckYourAnswersPage
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.triage.SingleDisposalsTriageControllerSpec.validateSingleDisposalTriageCheckYourAnswersPage
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.yeartodatelliability.YearToDateLiabilityControllerSpec._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, JustSubmittedReturn, SubmitReturnFailed, Subscribed}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address.UkAddress
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Postcode
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.{AmountInPence, PaymentsJourney}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.{AgentReferenceNumber, CgtReference}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.{IndividualName, TrustName}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.SubscribedDetails
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
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SubmitReturnResponse.ReturnCharge
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.YearToDateLiabilityAnswers.CalculatedYTDAnswers.{CompleteCalculatedYTDAnswers, IncompleteCalculatedYTDAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.YearToDateLiabilityAnswers.NonCalculatedYTDAnswers.{CompleteNonCalculatedYTDAnswers, IncompleteNonCalculatedYTDAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{B64Html, Error, JourneyStatus, SessionData, TimeUtils, UserType}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.onboarding.SubscriptionService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.{PaymentsService, ReturnsService}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class CheckAllAnswersAndSubmitControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with ScalaCheckDrivenPropertyChecks
    with RedirectToStartBehaviour {

  val mockReturnsService = mock[ReturnsService]

  val mockPaymentsService = mock[PaymentsService]

  override val overrideBindings =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore),
      bind[ReturnsService].toInstance(mockReturnsService),
      bind[PaymentsService].toInstance(mockPaymentsService),
      bind[SubscriptionService].toInstance(mockSubscriptionService)
    )

  lazy val controller = instanceOf[CheckAllAnswersAndSubmitController]

  implicit val messagesApi: MessagesApi = controller.messagesApi

  val rebasingEligibilityUtil = new RebasingEligibilityUtil()

  def setNameForUserType(
    userType: UserType
  ): Either[TrustName, IndividualName] =
    userType match {
      case UserType.Organisation => Left(sample[TrustName])
      case _                     => Right(sample[IndividualName])
    }

  def setAgentReferenceNumber(
    userType: UserType
  ): Option[AgentReferenceNumber] =
    userType match {
      case UserType.Agent => Some(sample[AgentReferenceNumber])
      case _              => None
    }

  def sessionWithJourney(
    journeyStatus: JourneyStatus,
    userType: UserType
  ): SessionData =
    SessionData.empty.copy(
      journeyStatus = Some(journeyStatus),
      userType = Some(userType)
    )

  def sessionWithJourney(journeyStatus: JourneyStatus): SessionData =
    SessionData.empty.copy(journeyStatus = Some(journeyStatus))

  def mockSubmitReturn(
    submitReturnRequest: SubmitReturnRequest
  )(response: Either[Error, SubmitReturnResponse]) =
    (mockReturnsService
      .submitReturn(_: SubmitReturnRequest)(_: HeaderCarrier))
      .expects(submitReturnRequest, *)
      .returning(EitherT.fromEither[Future](response))

  def mockStartPaymentJourney(
    cgtReference: CgtReference,
    chargeReference: Option[String],
    amount: AmountInPence,
    returnUrl: Call,
    backUrl: Call
  )(response: Either[Error, PaymentsJourney]) =
    (mockPaymentsService
      .startPaymentJourney(
        _: CgtReference,
        _: Option[String],
        _: AmountInPence,
        _: Call,
        _: Call
      )(
        _: HeaderCarrier,
        _: Request[_]
      ))
      .expects(cgtReference, chargeReference, amount, returnUrl, backUrl, *, *)
      .returning(EitherT.fromEither[Future](response))

  def mockCgtRegistrationService(
    response: Either[Error, RepresenteeCgtReference],
    completeRepresenteeAnswers: CompleteRepresenteeAnswers
  ) =
    (mockSubscriptionService
      .registerWithoutIdAndSubscribe(
        _: CompleteRepresenteeAnswers
      )(
        _: HeaderCarrier
      ))
      .expects(completeRepresenteeAnswers, *)
      .returning(EitherT.fromEither(response))

  def userMessageKey(
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
            representeeAnswers = Some(representeeAnswers)
          )
          complete.copy(hasAttachments =
            complete.supportingDocumentAnswers.evidences.nonEmpty || complete.yearToDateLiabilityAnswers.isLeft
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
          sample[FillingOutReturn].copy(draftReturn = completeDraftReturn, previousSentReturns = None)

        behave like redirectToStartWhenInvalidJourney(
          performAction,
          {
            case _: FillingOutReturn => true
            case _                   => false
          }
        )

        behave like incompleteSingleDisposalJourneyBehaviour(
          performAction,
          completeDraftReturn
        )

        "display the page" when {

          def test(
            sessionData: SessionData,
            completeReturn: CompleteSingleDisposalReturn,
            expectedTitleKey: String,
            userType: Option[UserType],
            isATrust: Boolean,
            isFurtherReturn: Boolean
          ): Unit = {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(sessionData)
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
                  isFurtherReturn
                )
                doc
                  .select("#back")
                  .attr("href") shouldBe routes.TaskListController
                  .taskList()
                  .url
                doc
                  .select("#content > article > form")
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
              completeReturn,
              "checkAllAnswers.title",
              None,
              completeFillingOutReturn.subscribedDetails.isATrust,
              isFurtherReturn = false
            )
          }

          "the user is on a further return journey" in {
            val representeeAnswers = sample[CompleteRepresenteeAnswers].copy(
              isFirstReturn = false
            )

            val completeReturn = {
              val complete = sample[CompleteSingleDisposalReturn].copy(
                triageAnswers = sample[CompleteSingleDisposalTriageAnswers].copy(
                  individualUserType = Some(PersonalRepresentative)
                ),
                representeeAnswers = Some(representeeAnswers)
              )
              complete.copy(hasAttachments =
                complete.supportingDocumentAnswers.evidences.nonEmpty || complete.yearToDateLiabilityAnswers.isLeft
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

            val completeFillingOutReturn = sample[FillingOutReturn].copy(
              draftReturn = completeDraftReturn,
              previousSentReturns = Some(List(sample[ReturnSummary]))
            )

            test(
              sessionWithJourney(completeFillingOutReturn),
              completeReturn,
              "checkAllAnswers.title",
              None,
              completeFillingOutReturn.subscribedDetails.isATrust,
              isFurtherReturn = true
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
              completeReturn,
              "checkAllAnswers.title",
              Some(userType),
              subscribedDetails.isATrust,
              isFurtherReturn = false
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

              checkIsRedirect(
                performAction(),
                routes.TaskListController.taskList()
              )
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

              checkIsRedirect(
                performAction(),
                routes.TaskListController.taskList()
              )
            }

          }

        }

      }

      "the user is on a multiple disposals journey" must {

        val completeReturn = sample[CompleteMultipleDisposalsReturn]
          .copy(
            triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
              .copy(individualUserType = Some(PersonalRepresentativeInPeriodOfAdmin)),
            representeeAnswers = Some(
              sample[CompleteRepresenteeAnswers]
                .copy(dateOfDeath = Some(DateOfDeath(LocalDate.now)), isFirstReturn = true)
            ),
            hasAttachments = true
          )

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
          sample[FillingOutReturn].copy(draftReturn = completeDraftReturn, previousSentReturns = None)

        behave like redirectToStartWhenInvalidJourney(
          performAction,
          {
            case _: FillingOutReturn => true
            case _                   => false
          }
        )

        behave like incompleteMultipleDisposalsJourneyBehaviour(
          performAction,
          completeDraftReturn
        )

        "display the page" when {

          def test(
            sessionData: SessionData,
            expectedTitleKey: String,
            userType: Option[UserType],
            isATrust: Boolean,
            isFurtherReturn: Boolean
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
                  isFurtherReturn
                )
                doc
                  .select("#back")
                  .attr("href") shouldBe routes.TaskListController
                  .taskList()
                  .url
                doc
                  .select("#content > article > form")
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
              isFurtherReturn = false
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
              isFurtherReturn = false
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

              checkIsRedirect(
                performAction(),
                routes.TaskListController.taskList()
              )
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

              checkIsRedirect(
                performAction(),
                routes.TaskListController.taskList()
              )
            }

          }

        }

      }

      "the user is on a single indirect disposal journey" must {

        val completeReturn = sample[CompleteSingleIndirectDisposalReturn].copy(
          triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
            .copy(individualUserType = Some(PersonalRepresentative)),
          representeeAnswers = Some(sample[CompleteRepresenteeAnswers].copy(isFirstReturn = true)),
          yearToDateLiabilityAnswers = sample[CompleteNonCalculatedYTDAnswers],
          hasAttachments = true
        )

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
          sample[FillingOutReturn].copy(draftReturn = completeDraftReturn, previousSentReturns = None)

        behave like redirectToStartWhenInvalidJourney(
          performAction,
          {
            case _: FillingOutReturn => true
            case _                   => false
          }
        )

        behave like incompleteSingleIndirectDisposalJourneyBehaviour(
          performAction,
          completeDraftReturn
        )

        "display the page" when {

          def test(
            sessionData: SessionData,
            expectedTitleKey: String,
            userType: Option[UserType],
            isATrust: Boolean,
            isFurtherReturn: Boolean
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
                    completeReturn.representativeType()
                  ),
                  isATrust,
                  IndirectDisposal,
                  isFurtherReturn
                )
                doc
                  .select("#back")
                  .attr("href") shouldBe routes.TaskListController
                  .taskList()
                  .url
                doc
                  .select("#content > article > form")
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
              isFurtherReturn = false
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
              isFurtherReturn = false
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

              checkIsRedirect(
                performAction(),
                routes.TaskListController.taskList()
              )
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

              checkIsRedirect(
                performAction(),
                routes.TaskListController.taskList()
              )
            }

          }

        }

      }

      "the user is on a multiple indirect disposals journey" must {

        val completeReturn = sample[CompleteMultipleIndirectDisposalReturn]
          .copy(
            triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
              .copy(individualUserType = Some(PersonalRepresentative)),
            representeeAnswers = Some(sample[CompleteRepresenteeAnswers].copy(isFirstReturn = true)),
            hasAttachments = true
          )

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
          sample[FillingOutReturn].copy(draftReturn = completeDraftReturn, previousSentReturns = None)

        behave like redirectToStartWhenInvalidJourney(
          performAction,
          {
            case _: FillingOutReturn => true
            case _                   => false
          }
        )

        behave like incompleteMultipleIndirectDisposalsJourneyBehaviour(
          performAction,
          completeDraftReturn
        )

        "display the page" when {

          def test(
            sessionData: SessionData,
            expectedTitleKey: String,
            userType: Option[UserType],
            isATrust: Boolean,
            isFurtherReturn: Boolean
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
                  isFurtherReturn
                )
                doc
                  .select("#back")
                  .attr("href") shouldBe routes.TaskListController
                  .taskList()
                  .url
                doc
                  .select("#content > article > form")
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
              isFurtherReturn = false
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
              isFurtherReturn = false
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

              checkIsRedirect(
                performAction(),
                routes.TaskListController.taskList()
              )
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

              checkIsRedirect(
                performAction(),
                routes.TaskListController.taskList()
              )
            }

          }

        }

      }

      "the user is on a single mixed use disposals journey" must {

        val completeReturn = sample[CompleteSingleMixedUseDisposalReturn]
          .copy(
            triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
              .copy(individualUserType = Some(PersonalRepresentative)),
            representeeAnswers = Some(sample[CompleteRepresenteeAnswers].copy(isFirstReturn = true)),
            hasAttachments = true
          )

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
          sample[FillingOutReturn].copy(draftReturn = completeDraftReturn, previousSentReturns = None)

        behave like redirectToStartWhenInvalidJourney(
          performAction,
          {
            case _: FillingOutReturn => true
            case _                   => false
          }
        )

        behave like incompleteSingleMixedUseDisposalsJourneyBehaviour(
          performAction,
          completeDraftReturn
        )

        "display the page" when {

          def test(
            sessionData: SessionData,
            expectedTitleKey: String,
            userType: Option[UserType],
            isATrust: Boolean,
            isFurtherReturn: Boolean
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
                  isFurtherReturn
                )
                doc
                  .select("#back")
                  .attr("href") shouldBe routes.TaskListController
                  .taskList()
                  .url
                doc
                  .select("#content > article > form")
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
              isFurtherReturn = false
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
              isFurtherReturn = false
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

              checkIsRedirect(
                performAction(),
                routes.TaskListController.taskList()
              )
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

              checkIsRedirect(
                performAction(),
                routes.TaskListController.taskList()
              )
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
            .fold(_ => true, _.mandatoryEvidence.isDefined)

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
        None,
        TimeUtils.today()
      )

      val completeDraftReturnRepresenteWithNoReference =
        completeDraftReturnNoRepresentee.copy(
          representeeAnswers = Some(sample[CompleteRepresenteeAnswers].copy(id = NoReferenceId)),
          triageAnswers = completeReturn.triageAnswers.copy(individualUserType = Some(PersonalRepresentative))
        )

      val completeFillingOutReturnWithRepresenteeWithNoReference =
        sample[FillingOutReturn].copy(draftReturn = completeDraftReturnRepresenteWithNoReference)

      val completeFillingOutReturnNoRepresentee =
        completeFillingOutReturnWithRepresenteeWithNoReference.copy(draftReturn = completeDraftReturnNoRepresentee)

      val submitReturnResponse = sample[SubmitReturnResponse]

      val justSubmittedReturn = JustSubmittedReturn(
        completeFillingOutReturnNoRepresentee.subscribedDetails,
        completeFillingOutReturnNoRepresentee.ggCredId,
        completeFillingOutReturnNoRepresentee.agentReferenceNumber,
        completeReturn.copy(hasAttachments = hasAttachments),
        submitReturnResponse
      )

      lazy val submitReturnRequest = {
        val cyaPge                          = instanceOf[views.html.returns.check_all_answers]
        implicit val requestWithSessionData =
          RequestWithSessionData(
            None,
            AuthenticatedRequest(
              new MessagesRequest(FakeRequest(), messagesApi)
            )
          )
        implicit val config                 = viewConfig
        implicit val messages               = MessagesImpl(Lang.apply("en"), messagesApi)

        val cyaPageHtml =
          cyaPge(
            completeReturn,
            instanceOf[RebasingEligibilityUtil],
            completeFillingOutReturnNoRepresentee.subscribedDetails,
            completeReturn.representativeType(),
            completeReturn.isIndirectDisposal(),
            completeFillingOutReturnNoRepresentee.agentReferenceNumber,
            completeFillingOutReturnNoRepresentee.isFurtherReturn,
            true
          ).toString

        SubmitReturnRequest(
          completeReturn.copy(hasAttachments = hasAttachments),
          completeFillingOutReturnNoRepresentee.draftReturn.id,
          completeFillingOutReturnNoRepresentee.subscribedDetails,
          completeFillingOutReturnNoRepresentee.agentReferenceNumber,
          B64Html(new String(Base64.getEncoder.encode(cyaPageHtml.getBytes())))
        )
      }

      def submitReturnRequestForOverriddenReferenceId(representeeCgtReference: RepresenteeCgtReference) = {
        val cyaPge                          = instanceOf[views.html.returns.check_all_answers]
        implicit val requestWithSessionData =
          RequestWithSessionData(
            None,
            AuthenticatedRequest(
              new MessagesRequest(FakeRequest(), messagesApi)
            )
          )
        implicit val config                 = viewConfig
        implicit val messages               = MessagesImpl(Lang.apply("en"), messagesApi)
        val mockedCompleteReturn            = CompleteSingleDisposalReturn
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
            completeFillingOutReturnWithRepresenteeWithNoReference.subscribedDetails,
            mockedCompleteReturn.representativeType(),
            mockedCompleteReturn.isIndirectDisposal(),
            completeFillingOutReturnWithRepresenteeWithNoReference.agentReferenceNumber,
            completeFillingOutReturnWithRepresenteeWithNoReference.isFurtherReturn,
            true
          ).toString

        SubmitReturnRequest(
          mockedCompleteReturn,
          completeFillingOutReturnWithRepresenteeWithNoReference.draftReturn.id,
          completeFillingOutReturnWithRepresenteeWithNoReference.subscribedDetails,
          completeFillingOutReturnWithRepresenteeWithNoReference.agentReferenceNumber,
          B64Html(new String(Base64.getEncoder.encode(cyaPageHtml.getBytes())))
        )
      }

      behave like redirectToStartWhenInvalidJourney(
        performAction,
        {
          case _: FillingOutReturn => true
          case _                   => false
        }
      )

      behave like incompleteSingleDisposalJourneyBehaviour(
        performAction,
        completeDraftReturnNoRepresentee
      )

      "show an error page" when {

        "there is an error updating the session after a successful submission" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionWithJourney(completeFillingOutReturnNoRepresentee))
            mockSubmitReturn(submitReturnRequest)(Right(submitReturnResponse))
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
            mockSubmitReturn(submitReturnRequest)(Left(Error("")))
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
            mockSubmitReturn(submitReturnRequest)(Right(submitReturnResponse))
            mockStoreSession(sessionWithJourney(justSubmittedReturn))(Right(()))
          }

          checkIsRedirect(
            performAction(),
            routes.CheckAllAnswersAndSubmitController.confirmationOfSubmission()
          )
        }

        "representees are handled correctly" in {
          val mockRepresenteeCgtReference = sample[RepresenteeCgtReference]
          val mockedCompleteReturn        = CompleteSingleDisposalReturn
            .fromDraftReturn(
              completeDraftReturnRepresenteWithNoReference
                .copy(representeeAnswers =
                  completeDraftReturnRepresenteWithNoReference.representeeAnswers.map(e =>
                    e.fold(_.copy(id = Some(mockRepresenteeCgtReference)), _.copy(id = mockRepresenteeCgtReference))
                  )
                )
            )
            .getOrElse(sample[CompleteSingleDisposalReturn])
          val completeRepresenteeAnswers  = mockedCompleteReturn.representeeAnswers
            .getOrElse(sample[CompleteRepresenteeAnswers])
            .copy(id = NoReferenceId)
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionWithJourney(completeFillingOutReturnWithRepresenteeWithNoReference))
            mockCgtRegistrationService(Right(mockRepresenteeCgtReference), completeRepresenteeAnswers)
            mockSubmitReturn(submitReturnRequestForOverriddenReferenceId(mockRepresenteeCgtReference))(
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

          checkIsRedirect(
            performAction(),
            routes.CheckAllAnswersAndSubmitController.confirmationOfSubmission()
          )
        }

        "representees returns error" in {
          val mockRepresenteeCgtReference = sample[RepresenteeCgtReference]
          val mockedCompleteReturn        = CompleteSingleDisposalReturn
            .fromDraftReturn(
              completeDraftReturnRepresenteWithNoReference
                .copy(representeeAnswers =
                  completeDraftReturnRepresenteWithNoReference.representeeAnswers.map(e =>
                    e.fold(_.copy(id = Some(mockRepresenteeCgtReference)), _.copy(id = mockRepresenteeCgtReference))
                  )
                )
            )
            .getOrElse(sample[CompleteSingleDisposalReturn])
          val completeRepresenteeAnswers  = mockedCompleteReturn.representeeAnswers
            .getOrElse(sample[CompleteRepresenteeAnswers])
            .copy(id = NoReferenceId)
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionWithJourney(completeFillingOutReturnWithRepresenteeWithNoReference))
            mockCgtRegistrationService(Left(Error("error thrown")), completeRepresenteeAnswers)
          }

          checkIsTechnicalErrorPage(
            performAction()
          )
        }
      }

      "redirect to the submission error page" when {

        "there is an error submitting the return" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionWithJourney(completeFillingOutReturnNoRepresentee))
            mockSubmitReturn(submitReturnRequest)(Left(Error("")))
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

          checkIsRedirect(
            performAction(),
            routes.CheckAllAnswersAndSubmitController.submissionError()
          )
        }
      }

    }

    "handling requests to display the return submit failed page" must {

      def performAction(): Future[Result] =
        controller.submissionError()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(
        performAction,
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
              doc =>
                doc
                  .select("#content > article > form")
                  .attr(
                    "action"
                  ) shouldBe routes.CheckAllAnswersAndSubmitController
                  .submissionErrorSubmit()
                  .url
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
        performAction,
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

            checkIsRedirect(
              performAction(),
              homepage.routes.HomePageController.homepage()
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

    "handling requests to display the confirmation of submission page" must {

      def performAction(): Future[Result] =
        controller.confirmationOfSubmission()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(
        performAction,
        {
          case _: JustSubmittedReturn => true
          case _                      => false
        }
      )

      "confirmation of submission is correct" in {

        val noTaxDueRefLine             = "Return reference number form bundle id"
        val taxDueRefLine               = "Payment reference number charge ref"
        val submissionLine              =
          ("sent-date-table", "Return sent to HMRC", "2 February 2020")
        val addressLine                 = (
          "property-address-table",
          "Property address",
          "123 fake street, abc123"
        )
        val returnReferenceWithBundleId = (
          "return-reference-table",
          "Return reference number",
          "form bundle id"
        )
        val taxDueDateLine              =
          ("tax-due-date-table", "Tax due by", "1 January 2020")

        sealed case class TestScenario(
          description: String,
          userType: UserType,
          taxOwed: AmountInPence,
          prefix: String,
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
            "",
            noTaxDueRefLine,
            List(submissionLine, addressLine),
            Right(IndividualName("John", "Doe")),
            Some(Self)
          ),
          TestScenario(
            "user with tax due",
            UserType.Individual,
            AmountInPence(10000),
            "",
            taxDueRefLine,
            List(
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
            "Client: ",
            noTaxDueRefLine,
            List(submissionLine, addressLine),
            Right(IndividualName("John", "Doe")),
            Some(Self)
          ),
          TestScenario(
            "agent for individual with tax due",
            UserType.Agent,
            AmountInPence(10000),
            "Client: ",
            taxDueRefLine,
            List(
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
            "Trust: ",
            noTaxDueRefLine,
            List(submissionLine, addressLine),
            Left(TrustName("trust")),
            None
          ),
          TestScenario(
            "organisation with tax due",
            UserType.Organisation,
            AmountInPence(10000),
            "Trust: ",
            taxDueRefLine,
            List(
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
            "",
            noTaxDueRefLine,
            List(submissionLine, addressLine),
            Right(IndividualName("John", "Doe")),
            Some(Capacitor)
          ),
          TestScenario(
            "capacitor with tax due",
            UserType.Individual,
            AmountInPence(10000),
            "",
            taxDueRefLine,
            List(
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
            "",
            noTaxDueRefLine,
            List(submissionLine, addressLine),
            Right(IndividualName("John", "Doe")),
            Some(PersonalRepresentative)
          ),
          TestScenario(
            "personal representative with tax due",
            UserType.Individual,
            AmountInPence(10000),
            "",
            taxDueRefLine,
            List(
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
            "Estate: ",
            taxDueRefLine,
            List(
              submissionLine,
              returnReferenceWithBundleId,
              addressLine,
              taxDueDateLine
            ),
            Right(IndividualName("John", "Doe")),
            Some(PersonalRepresentativeInPeriodOfAdmin)
          ),
          TestScenario(
            "Agent of estate in period of administration with tax due",
            UserType.Agent,
            AmountInPence(10000),
            "Client: ",
            taxDueRefLine,
            List(
              submissionLine,
              returnReferenceWithBundleId,
              addressLine,
              taxDueDateLine
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
                namePrefix,
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
                )
              )
              val representeeAnswers = individualUserType match {
                case Some(_: RepresentativeType) =>
                  Some(
                    sample[CompleteRepresenteeAnswers]
                      .copy(dateOfDeath = Some(sample[DateOfDeath]))
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
                agentReferenceNumber =
                  if (userType === UserType.Agent)
                    Some(sample[AgentReferenceNumber])
                  else None
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
                  val expectedName =
                    namePrefix + representeeAnswers
                      .map(_.name.makeSingleName())
                      .getOrElse(
                        name.fold(_.value, _.makeSingleName())
                      )

                  doc.select("#user-details-name").text() shouldBe expectedName
                  doc.select("#ref-id").text()            shouldBe reference

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
                    )
                      s"confirmationOfSubmission.capacitor.noId.printPage"
                    else
                      s"confirmationOfSubmission$userKey.printPage",
                    "JavaScript: window.print();"
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

      "confirmation of submission is correct for multiple indirect disposal" in {

        val noTaxDueRefLine             = "Return reference number form bundle id"
        val taxDueRefLine               = "Payment reference number charge ref"
        val submissionLine              =
          ("sent-date-table", "Return sent to HMRC", "2 February 2020")
        val addressLine                 = (
          "property-address-table",
          "Company address",
          "123 fake street, abc123"
        )
        val returnReferenceWithBundleId = (
          "return-reference-table",
          "Return reference number",
          "form bundle id"
        )
        val taxDueDateLine              =
          ("tax-due-date-table", "Tax due by", "1 January 2020")

        sealed case class TestScenario(
          description: String,
          userType: UserType,
          taxOwed: AmountInPence,
          prefix: String,
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
            "",
            noTaxDueRefLine,
            List(submissionLine, addressLine),
            Right(IndividualName("John", "Doe")),
            Some(Self)
          ),
          TestScenario(
            "user with tax due",
            UserType.Individual,
            AmountInPence(10000),
            "",
            taxDueRefLine,
            List(
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
            "Client: ",
            noTaxDueRefLine,
            List(submissionLine, addressLine),
            Right(IndividualName("John", "Doe")),
            Some(Self)
          ),
          TestScenario(
            "agent for individual with tax due",
            UserType.Agent,
            AmountInPence(10000),
            "Client: ",
            taxDueRefLine,
            List(
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
            "Trust: ",
            noTaxDueRefLine,
            List(submissionLine, addressLine),
            Left(TrustName("trust")),
            None
          ),
          TestScenario(
            "organisation with tax due",
            UserType.Organisation,
            AmountInPence(10000),
            "Trust: ",
            taxDueRefLine,
            List(
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
            "",
            noTaxDueRefLine,
            List(submissionLine, addressLine),
            Right(IndividualName("John", "Doe")),
            Some(Capacitor)
          ),
          TestScenario(
            "capacitor with tax due",
            UserType.Individual,
            AmountInPence(10000),
            "",
            taxDueRefLine,
            List(
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
            "",
            noTaxDueRefLine,
            List(submissionLine, addressLine),
            Right(IndividualName("John", "Doe")),
            Some(PersonalRepresentative)
          ),
          TestScenario(
            "personal representative with tax due",
            UserType.Individual,
            AmountInPence(10000),
            "",
            taxDueRefLine,
            List(
              submissionLine,
              returnReferenceWithBundleId,
              addressLine,
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
                namePrefix,
                reference,
                expectedTable,
                name,
                individualUserType
              ) =>
            withClue(description) {
              val address = sample[UkAddress].copy(
                line1 = "123 fake street",
                line2 = None,
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
                )
              )
              val representeeAnswers           = individualUserType match {
                case Some(PersonalRepresentative) =>
                  Some(
                    sample[CompleteRepresenteeAnswers]
                      .copy(dateOfDeath = Some(sample[DateOfDeath]))
                  )
                case Some(Capacitor)              =>
                  Some(
                    sample[CompleteRepresenteeAnswers].copy(dateOfDeath = None)
                  )

                case _                            => None
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
                agentReferenceNumber =
                  if (userType === UserType.Agent)
                    Some(sample[AgentReferenceNumber])
                  else None
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
                  val expectedName =
                    representeeAnswers
                      .map(_.name.makeSingleName())
                      .getOrElse(
                        namePrefix + name.fold(_.value, _.makeSingleName())
                      )

                  doc.select("#user-details-name").text() shouldBe expectedName
                  doc.select("#ref-id").text()            shouldBe reference

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
                    )
                      s"confirmationOfSubmission.capacitor.noId.printPage"
                    else
                      s"confirmationOfSubmission$userKey.printPage",
                    "JavaScript: window.print();"
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

    }

    "handling requests to pay a return" must {

      def performAction(): Future[Result] =
        controller.payReturn()(FakeRequest())

      def justSubmittedReturnWithCharge(
        charge: Option[ReturnCharge]
      ): JustSubmittedReturn =
        sample[JustSubmittedReturn].copy(submissionResponse = sample[SubmitReturnResponse].copy(charge = charge))

      behave like redirectToStartWhenInvalidJourney(
        performAction,
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

          checkIsRedirect(
            performAction(),
            homepage.routes.HomePageController.homepage()
          )
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
              homepage.routes.HomePageController.homepage(),
              routes.CheckAllAnswersAndSubmitController
                .confirmationOfSubmission()
            )(Left(Error("")))

            checkIsTechnicalErrorPage(performAction())
          }

        }

      }

      "redirect to the payment journey next url" when {

        "the payments journey has been succesfulyl started" in {
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

  def incompleteSingleDisposalJourneyBehaviour(
    performAction: () => Future[Result],
    completeDraftReturn: DraftSingleDisposalReturn
  ) = {
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
                  draftReturn = makeIncomplete(completeDraftReturn)
                )
              )
            )
          }

          checkIsRedirect(performAction(), routes.TaskListController.taskList())
        }

      }

    }

  }

  def incompleteMultipleDisposalsJourneyBehaviour(
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
                  draftReturn = makeIncomplete(completeDraftReturn)
                )
              )
            )
          }

          checkIsRedirect(performAction(), routes.TaskListController.taskList())
        }

      }

    }

  }

  def incompleteSingleIndirectDisposalJourneyBehaviour(
    performAction: () => Future[Result],
    completeDraftReturn: DraftSingleIndirectDisposalReturn
  ) = {
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
                  draftReturn = makeIncomplete(completeDraftReturn)
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
                  completeDraftReturn.copy(yearToDateLiabilityAnswers = Some(sample[CompleteCalculatedYTDAnswers]))
              )
            )
          )
        }

        checkIsRedirect(performAction(), routes.TaskListController.taskList())

      }

    }

  }

  def incompleteMultipleIndirectDisposalsJourneyBehaviour(
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
                  draftReturn = makeIncomplete(completeDraftReturn)
                )
              )
            )
          }

          checkIsRedirect(performAction(), routes.TaskListController.taskList())
        }

      }

    }

  }

  def incompleteSingleMixedUseDisposalsJourneyBehaviour(
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
                  draftReturn = makeIncomplete(completeDraftReturn)
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
    isFurtherReturn: Boolean
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
      completeReturn.isIndirectDisposal()
    )

    validateReliefDetailsCheckYourAnswersPage(
      completeReturn.reliefDetails,
      doc
    )

    completeReturn.triageAnswers.individualUserType.foreach { individualUserType =>
      validateExemptionAndLossesCheckYourAnswersPage(
        completeReturn.exemptionsAndLossesDetails,
        doc,
        isATrust,
        userType.contains(UserType.Agent),
        individualUserType,
        isFurtherReturn
      )
      completeReturn.yearToDateLiabilityAnswers.fold(
        validateNonCalculatedYearToDateLiabilityPage(
          _,
          doc,
          userType,
          Some(individualUserType),
          isFurtherReturn
        ),
        validateCalculatedYearToDateLiabilityPage(_, isATrust, doc)
      )
    }
  }

  def validateMultipleDisposalsCheckAllYourAnswersSections(
    doc: Document,
    completeReturn: CompleteMultipleDisposalsReturn,
    userType: Option[UserType],
    isATrust: Boolean,
    isFurtherReturn: Boolean
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
        individualUserType
      )
      validateNonCalculatedYearToDateLiabilityPage(
        completeReturn.yearToDateLiabilityAnswers,
        doc,
        userType,
        Some(individualUserType),
        isFurtherReturn
      )
    }

  }

  def validateSingleIndirectDisposalCheckAllYourAnswersSections(
    doc: Document,
    completeReturn: CompleteSingleIndirectDisposalReturn,
    userType: Option[UserType],
    isRebasing: Boolean,
    isATrust: Boolean,
    assetType: AssetType,
    isFurtherReturn: Boolean
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
      completeReturn.isIndirectDisposal()
    )

    completeReturn.triageAnswers.individualUserType.foreach { individualUserType =>
      validateExemptionAndLossesCheckYourAnswersPage(
        completeReturn.exemptionsAndLossesDetails,
        doc,
        isATrust,
        userType.contains(UserType.Agent),
        individualUserType,
        isFurtherReturn
      )

      validateNonCalculatedYearToDateLiabilityPage(
        completeReturn.yearToDateLiabilityAnswers,
        doc,
        userType,
        Some(individualUserType),
        isFurtherReturn
      )
    }
  }

  def validateMultipleIndirectDisposalsCheckAllYourAnswersSections(
    doc: Document,
    completeReturn: CompleteMultipleIndirectDisposalReturn,
    userType: Option[UserType],
    isATrust: Boolean,
    isFurtherReturn: Boolean
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
        individualUserType
      )
      validateNonCalculatedYearToDateLiabilityPage(
        completeReturn.yearToDateLiabilityAnswers,
        doc,
        userType,
        Some(individualUserType),
        isFurtherReturn
      )
    }

  }

  def validateSingleMixedUseDisposalsCheckAllYourAnswersSections(
    doc: Document,
    completeReturn: CompleteSingleMixedUseDisposalReturn,
    userType: Option[UserType],
    isATrust: Boolean,
    isFurtherReturn: Boolean
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
        individualUserType
      )
    }
    validateNonCalculatedYearToDateLiabilityPage(
      completeReturn.yearToDateLiabilityAnswers,
      doc,
      userType,
      completeReturn.triageAnswers.individualUserType,
      isFurtherReturn
    )

  }

}
