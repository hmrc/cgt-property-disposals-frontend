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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.yeartodatelliability

import java.time.LocalDate
import java.util.UUID

import cats.data.EitherT
import cats.instances.future._
import cats.syntax.order._
import org.jsoup.nodes.Document
import org.scalatest.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.http.Status.BAD_REQUEST
import play.api.i18n.{Lang, Messages, MessagesApi, MessagesImpl}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.{Call, Result}
import play.api.test.FakeRequest
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.{ReturnsServiceSupport, StartingToAmendToFillingOutReturnSpecBehaviour}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AmountOfMoneyErrorScenarios, AuthSupport, ControllerSpec, SessionSupport, returns}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.AddressGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.AcquisitionDetailsGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.CompleteReturnGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.DraftReturnGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.DisposalMethodGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.DisposalDetailsGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.ExemptionsAndLossesAnswersGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.FileUploadGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.IdGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.JourneyStatusGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.MoneyGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.NameGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.ReliefDetailsGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.RepresenteeAnswersGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.ReturnGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.SubscribedDetailsGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.TaxYearGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.UserTypeGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.TriageQuestionsGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.YearToDateLiabilityAnswersGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, PreviousReturnData, StartingToAmendReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address.UkAddress
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.{Address, Country}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.MoneyUtils.formatAmountOfMoneyWithPoundSign
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.{AmountInPence, MoneyUtils}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.CompleteReturnGen.completeMultipleDisposalsReturnGen
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.AgentReferenceNumber
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.{IndividualName, TrustName}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.SubscribedDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.AcquisitionDetailsAnswers.{CompleteAcquisitionDetailsAnswers, IncompleteAcquisitionDetailsAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.CalculatedTaxDue.GainCalculatedTaxDue
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.CompleteReturn.{CompleteMultipleDisposalsReturn, CompleteSingleDisposalReturn, CompleteSingleMixedUseDisposalReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.DisposalDetailsAnswers.{CompleteDisposalDetailsAnswers, IncompleteDisposalDetailsAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ExemptionAndLossesAnswers.{CompleteExemptionAndLossesAnswers, IncompleteExemptionAndLossesAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.IndividualUserType.{Capacitor, PersonalRepresentative, PersonalRepresentativeInPeriodOfAdmin, Self}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.MultipleDisposalsTriageAnswers.CompleteMultipleDisposalsTriageAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ReliefDetailsAnswers.{CompleteReliefDetailsAnswers, IncompleteReliefDetailsAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.RepresenteeAnswers.CompleteRepresenteeAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.{CompleteSingleDisposalTriageAnswers, IncompleteSingleDisposalTriageAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SupportingEvidenceAnswers.CompleteSupportingEvidenceAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.YearToDateLiabilityAnswers.CalculatedYTDAnswers._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.YearToDateLiabilityAnswers.NonCalculatedYTDAnswers.{CompleteNonCalculatedYTDAnswers, IncompleteNonCalculatedYTDAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.YearToDateLiabilityAnswers.{CalculatedYTDAnswers, NonCalculatedYTDAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.upscan.UpscanCallBack.{UpscanFailure, UpscanSuccess}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.upscan.{UploadReference, UpscanUpload}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{CompleteReturnWithSummary, Error, SessionData, TaxYear, TimeUtils, UserType}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.{CgtCalculationService, ReturnsService}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.upscan.UpscanService
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class YearToDateLiabilityControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with ReturnsServiceSupport
    with ScalaCheckDrivenPropertyChecks
    with RedirectToStartBehaviour
    with StartingToAmendToFillingOutReturnSpecBehaviour {

  import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.yeartodatelliability.YearToDateLiabilityControllerSpec._

  val mockCgtCalculationService = mock[CgtCalculationService]

  val mockUpscanService = mock[UpscanService]

  override val overrideBindings =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore),
      bind[ReturnsService].toInstance(mockReturnsService),
      bind[CgtCalculationService].toInstance(mockCgtCalculationService),
      bind[UpscanService].toInstance(mockUpscanService)
    )

  lazy val controller = instanceOf[YearToDateLiabilityController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  implicit lazy val messages: Messages = MessagesImpl(Lang("en"), messagesApi)

  def userMessageKey(
    userType: UserType,
    individualUserType: Option[IndividualUserType] = None
  ): String =
    individualUserType match {
      case Some(PersonalRepresentative | PersonalRepresentativeInPeriodOfAdmin) => ".personalRep"
      case Some(Capacitor)                                                      => ".capacitor"
      case _                                                                    =>
        userType match {
          case UserType.Individual   => ""
          case UserType.Organisation => ".trust"
          case UserType.Agent        => ".agent"
          case other                 => sys.error(s"User type '$other' not handled")
        }
    }

  def multipleMessageKey(isMultiple: Boolean): String =
    if (isMultiple) ".multiple" else ""

  def setAgentReferenceNumber(
    userType: UserType
  ): Option[AgentReferenceNumber] =
    userType match {
      case UserType.Agent => Some(sample[AgentReferenceNumber])
      case _              => None
    }

  def setNameForUserType(
    userType: UserType
  ): Either[TrustName, IndividualName] =
    userType match {
      case UserType.Organisation => Left(sample[TrustName])
      case _                     => Right(sample[IndividualName])
    }

  def setIndividualUserType(
    representativeType: Option[RepresentativeType]
  ): IndividualUserType =
    representativeType.getOrElse(Self)

  def redirectToStartBehaviour(performAction: () => Future[Result]) =
    redirectToStartWhenInvalidJourney(
      performAction,
      {
        case _: FillingOutReturn      => true
        case _: StartingToAmendReturn => true
        case _                        => false
      }
    )

  def singleDisposalTriageAnswers(
    disposalDate: Option[DisposalDate],
    wasUkResident: Boolean,
    individualUserType: Option[IndividualUserType]
  ) =
    sample[IncompleteSingleDisposalTriageAnswers].copy(
      individualUserType = individualUserType,
      hasConfirmedSingleDisposal = true,
      disposalMethod = Some(sample[DisposalMethod]),
      assetType = Some(AssetType.Residential),
      wasAUKResident = Some(wasUkResident),
      countryOfResidence = if (wasUkResident) Some(Country.uk) else Some(sample[Country]),
      disposalDate = disposalDate
    )

  def representeeAnswers(individualUserType: Option[IndividualUserType], isFurtherReturn: Boolean) =
    individualUserType match {
      case Some(PersonalRepresentative | PersonalRepresentativeInPeriodOfAdmin) =>
        Some(
          sample[CompleteRepresenteeAnswers].copy(
            dateOfDeath = Some(sample[DateOfDeath]),
            isFirstReturn = !isFurtherReturn
          )
        )
      case Some(Capacitor)                                                      =>
        Some(
          sample[CompleteRepresenteeAnswers].copy(
            dateOfDeath = None,
            isFirstReturn = !isFurtherReturn
          )
        )
      case _                                                                    => None
    }

  def fillingOutReturn(draftReturn: DraftReturn, userType: UserType) =
    sample[FillingOutReturn].copy(
      agentReferenceNumber = setAgentReferenceNumber(userType),
      subscribedDetails = sample[SubscribedDetails].copy(
        name = setNameForUserType(userType)
      ),
      draftReturn = draftReturn,
      amendReturnData = None
    )

  def sessionWithSingleIndirectDisposalState(
    ytdLiabilityAnswers: Option[YearToDateLiabilityAnswers],
    disposalDate: Option[DisposalDate] = Some(sample[DisposalDate]),
    userType: UserType,
    wasUkResident: Boolean,
    address: Option[Address] = Some(sample[Address]),
    individualUserType: Option[IndividualUserType] = Some(
      IndividualUserType.Self
    ),
    isFurtherReturn: Boolean = false
  ): (SessionData, FillingOutReturn, DraftSingleIndirectDisposalReturn) = {
    val draftReturn = sample[DraftSingleIndirectDisposalReturn].copy(
      triageAnswers = singleDisposalTriageAnswers(disposalDate, wasUkResident, individualUserType),
      representeeAnswers = representeeAnswers(individualUserType, isFurtherReturn),
      companyAddress = address,
      yearToDateLiabilityAnswers = ytdLiabilityAnswers
    )
    val journey     = sample[FillingOutReturn].copy(
      agentReferenceNumber = setAgentReferenceNumber(userType),
      subscribedDetails = sample[SubscribedDetails].copy(
        name = setNameForUserType(userType)
      ),
      draftReturn = draftReturn,
      previousSentReturns = if (isFurtherReturn) Some(PreviousReturnData(List(sample[ReturnSummary]), None)) else None,
      amendReturnData = None
    )
    (
      SessionData.empty.copy(
        userType = Some(userType),
        journeyStatus = Some(journey)
      ),
      journey,
      draftReturn
    )
  }

  def sessionWithSingleIndirectDisposalState(
    ytdLiabilityAnswers: YearToDateLiabilityAnswers,
    disposalDate: DisposalDate,
    userType: UserType,
    wasUkResident: Boolean
  ): (SessionData, FillingOutReturn, DraftSingleIndirectDisposalReturn) =
    sessionWithSingleIndirectDisposalState(
      Some(ytdLiabilityAnswers),
      Some(disposalDate),
      userType,
      wasUkResident
    )

  def sessionWithSingleMixedUseDisposalState(
    ytdLiabilityAnswers: Option[YearToDateLiabilityAnswers],
    disposalDate: Option[DisposalDate] = Some(sample[DisposalDate]),
    userType: UserType,
    wasUkResident: Boolean,
    individualUserType: Option[IndividualUserType] = Some(
      IndividualUserType.Self
    ),
    isFurtherReturn: Boolean = false
  ): (SessionData, FillingOutReturn, DraftSingleMixedUseDisposalReturn) = {
    val draftReturn = sample[DraftSingleMixedUseDisposalReturn].copy(
      triageAnswers = singleDisposalTriageAnswers(disposalDate, wasUkResident, individualUserType),
      representeeAnswers = representeeAnswers(individualUserType, isFurtherReturn),
      yearToDateLiabilityAnswers = ytdLiabilityAnswers
    )
    val journey     = sample[FillingOutReturn].copy(
      agentReferenceNumber = setAgentReferenceNumber(userType),
      subscribedDetails = sample[SubscribedDetails].copy(
        name = setNameForUserType(userType)
      ),
      draftReturn = draftReturn,
      previousSentReturns = if (isFurtherReturn) Some(PreviousReturnData(List(sample[ReturnSummary]), None)) else None,
      amendReturnData = None
    )
    (
      SessionData.empty.copy(
        userType = Some(userType),
        journeyStatus = Some(journey)
      ),
      journey,
      draftReturn
    )
  }

  def sessionWithSingleMixedUseDisposalState(
    ytdLiabilityAnswers: YearToDateLiabilityAnswers,
    disposalDate: DisposalDate,
    userType: UserType,
    wasUkResident: Boolean
  ): (SessionData, FillingOutReturn, DraftSingleMixedUseDisposalReturn) =
    sessionWithSingleMixedUseDisposalState(
      Some(ytdLiabilityAnswers),
      Some(disposalDate),
      userType,
      wasUkResident
    )

  def sessionWithSingleDisposalState(
    ytdLiabilityAnswers: Option[YearToDateLiabilityAnswers],
    disposalDate: Option[DisposalDate] = Some(sample[DisposalDate]),
    userType: UserType,
    wasUkResident: Boolean,
    reliefDetailsAnswers: Option[ReliefDetailsAnswers] = Some(
      sample[CompleteReliefDetailsAnswers].copy(otherReliefs = None)
    ),
    individualUserType: Option[IndividualUserType] = Some(
      IndividualUserType.Self
    ),
    isFurtherReturn: Boolean = false,
    amendReturnData: Option[AmendReturnData] = None
  ): (SessionData, FillingOutReturn, DraftSingleDisposalReturn) = {
    val draftReturn = sample[DraftSingleDisposalReturn].copy(
      triageAnswers = singleDisposalTriageAnswers(disposalDate, wasUkResident, individualUserType),
      representeeAnswers = representeeAnswers(individualUserType, isFurtherReturn),
      reliefDetailsAnswers = reliefDetailsAnswers,
      yearToDateLiabilityAnswers = ytdLiabilityAnswers
    )
    val journey     = sample[FillingOutReturn].copy(
      agentReferenceNumber = setAgentReferenceNumber(userType),
      subscribedDetails = sample[SubscribedDetails].copy(
        name = setNameForUserType(userType)
      ),
      draftReturn = draftReturn,
      previousSentReturns =
        if (isFurtherReturn) Some(PreviousReturnData(List(sample[ReturnSummary]), Some(sample[AmountInPence])))
        else None,
      amendReturnData = amendReturnData
    )
    (
      SessionData.empty.copy(
        userType = Some(userType),
        journeyStatus = Some(journey)
      ),
      journey,
      draftReturn
    )
  }

  def sessionWithSingleDisposalState(
    ytdLiabilityAnswers: YearToDateLiabilityAnswers,
    disposalDate: DisposalDate,
    userType: UserType,
    wasUkResident: Boolean
  ): (SessionData, FillingOutReturn, DraftSingleDisposalReturn) =
    sessionWithSingleDisposalState(
      Some(ytdLiabilityAnswers),
      Some(disposalDate),
      userType,
      wasUkResident
    )

  def sessionWithMultipleDisposalsState(
    ytdLiabilityAnswers: Option[YearToDateLiabilityAnswers],
    userType: UserType,
    wasUkResident: Boolean,
    individualUserType: Option[IndividualUserType] = Some(Self),
    isFurtherReturn: Boolean = false,
    taxYear: TaxYear = sample[TaxYear],
    amendReturnData: Option[AmendReturnData] = None
  ): (SessionData, FillingOutReturn, DraftMultipleDisposalsReturn) = {
    val draftReturn = sample[DraftMultipleDisposalsReturn].copy(
      triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers].copy(
        individualUserType = individualUserType,
        countryOfResidence = if (wasUkResident) Country.uk else sample[Country],
        taxYear = taxYear
      ),
      yearToDateLiabilityAnswers = ytdLiabilityAnswers,
      representeeAnswers = representeeAnswers(individualUserType, isFurtherReturn)
    )
    val journey     = sample[FillingOutReturn].copy(
      agentReferenceNumber = setAgentReferenceNumber(userType),
      subscribedDetails = sample[SubscribedDetails].copy(name = setNameForUserType(userType)),
      draftReturn = draftReturn,
      previousSentReturns = if (isFurtherReturn) Some(PreviousReturnData(List(sample[ReturnSummary]), None)) else None,
      amendReturnData = amendReturnData
    )
    (
      SessionData.empty.copy(
        userType = Some(userType),
        journeyStatus = Some(journey)
      ),
      journey,
      draftReturn
    )
  }

  def sessionWithMultipleDisposalsState(
    ytdLiabilityAnswers: YearToDateLiabilityAnswers,
    userType: UserType,
    wasUkResident: Boolean,
    isFurtherReturn: Boolean,
    amendReturnData: Option[AmendReturnData]
  ): (SessionData, FillingOutReturn, DraftMultipleDisposalsReturn) =
    sessionWithMultipleDisposalsState(
      Some(ytdLiabilityAnswers),
      userType,
      wasUkResident,
      isFurtherReturn = isFurtherReturn,
      amendReturnData = amendReturnData
    )

  def singleDisposalDraftReturnWithCompleteJourneys(
    yearToDateLiabilityAnswers: Option[YearToDateLiabilityAnswers],
    disposalDate: DisposalDate,
    reliefDetailsAnswers: ReliefDetailsAnswers,
    individualUserType: Option[IndividualUserType] = None
  ): DraftSingleDisposalReturn =
    DraftSingleDisposalReturn(
      UUID.randomUUID(),
      sample[CompleteSingleDisposalTriageAnswers]
        .copy(disposalDate = disposalDate, individualUserType = individualUserType),
      Some(sample[UkAddress]),
      Some(sample[CompleteDisposalDetailsAnswers]),
      Some(sample[CompleteAcquisitionDetailsAnswers]),
      Some(reliefDetailsAnswers),
      Some(sample[CompleteExemptionAndLossesAnswers]),
      yearToDateLiabilityAnswers,
      Some(sample[AmountInPence]),
      Some(sample[CompleteSupportingEvidenceAnswers]),
      None,
      None,
      TimeUtils.today()
    )

  def mockCalculationService(
    request: CalculateCgtTaxDueRequest
  )(result: Either[Error, CalculatedTaxDue]) =
    (mockCgtCalculationService
      .calculateTaxDue(_: CalculateCgtTaxDueRequest)(_: HeaderCarrier))
      .expects(request, *)
      .returning(EitherT.fromEither[Future](result))

  def mockUpscanInitiate(
    errorRedirectCall: Call,
    successRedirectCall: UploadReference => Call
  )(
    result: Either[Error, UpscanUpload]
  ) =
    (mockUpscanService
      .initiate(_: Call, _: UploadReference => Call)(_: HeaderCarrier))
      .expects(
        where {
          (
            actualErrorRedirectCall: Call,
            actualSuccessRedirectCall: UploadReference => Call,
            _: HeaderCarrier
          ) =>
            val uploadReference = sample[UploadReference]

            actualErrorRedirectCall shouldBe errorRedirectCall
            actualSuccessRedirectCall(
              uploadReference
            )                       shouldBe successRedirectCall(uploadReference)
            true
        }
      )
      .returning(EitherT.fromEither(result))

  def mockGetUpscanUpload(
    uploadReference: UploadReference
  )(result: Either[Error, UpscanUpload]) =
    (mockUpscanService
      .getUpscanUpload(_: UploadReference)(_: HeaderCarrier))
      .expects(uploadReference, *)
      .returning(EitherT.fromEither(result))

  def setTaxDue(
    calculatedTaxDue: CalculatedTaxDue,
    taxDue: AmountInPence
  ): CalculatedTaxDue =
    calculatedTaxDue match {
      case nonGain: CalculatedTaxDue.NonGainCalculatedTaxDue =>
        nonGain.copy(amountOfTaxDue = taxDue)
      case gain: GainCalculatedTaxDue                        => gain.copy(amountOfTaxDue = taxDue)
    }

  val completeReliefDetailsAnswersWithNoOtherReliefs =
    sample[CompleteReliefDetailsAnswers].copy(otherReliefs = None)

  "YearToDateLiabilityController" when {

    "handling requests to display the estimated income page" must {

      def performAction(): Future[Result] =
        controller.estimatedIncome()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      behave like markUnmetDependencyBehaviour(controller.estimatedIncome())

      behave like noDisposalDateBehaviour(performAction)

      behave like redirectWhenNotSingleDisposalCalculatedJourneyBehaviour(
        performAction
      )

      "display the page" when {

        "an individual user has not answered the question before" in {

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithSingleDisposalState(
                None,
                Some(sample[DisposalDate]),
                UserType.Individual,
                wasUkResident = true,
                Some(completeReliefDetailsAnswersWithNoOtherReliefs),
                None
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("estimatedIncome.title"),
            { doc =>
              doc
                .select("#back")
                .attr("href")   shouldBe returns.routes.TaskListController
                .taskList()
                .url
              doc
                .select("#content > article > form")
                .attr("action") shouldBe routes.YearToDateLiabilityController
                .estimatedIncomeSubmit()
                .url
            }
          )

        }

        "an agent user has not answered the question before" in {

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithSingleDisposalState(
                None,
                Some(sample[DisposalDate]),
                UserType.Agent,
                wasUkResident = true,
                Some(completeReliefDetailsAnswersWithNoOtherReliefs),
                None
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("estimatedIncome.agent.title"),
            { doc =>
              doc
                .select("#back")
                .attr("href")   shouldBe returns.routes.TaskListController
                .taskList()
                .url
              doc
                .select("#content > article > form")
                .attr("action") shouldBe routes.YearToDateLiabilityController
                .estimatedIncomeSubmit()
                .url
            }
          )

        }

        "a capacitor user has not answered the question before" in {

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithSingleDisposalState(
                None,
                Some(sample[DisposalDate]),
                UserType.Individual,
                wasUkResident = true,
                Some(completeReliefDetailsAnswersWithNoOtherReliefs),
                Some(Capacitor)
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("estimatedIncome.capacitor.title"),
            { doc =>
              doc
                .select("#back")
                .attr("href")   shouldBe returns.routes.TaskListController
                .taskList()
                .url
              doc
                .select("#content > article > form")
                .attr("action") shouldBe routes.YearToDateLiabilityController
                .estimatedIncomeSubmit()
                .url
            }
          )

        }

        "a personal representative user has not answered the question before" in {

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithSingleDisposalState(
                None,
                Some(sample[DisposalDate]),
                UserType.Individual,
                wasUkResident = true,
                Some(completeReliefDetailsAnswersWithNoOtherReliefs),
                Some(PersonalRepresentative)
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("estimatedIncome.personalRep.title"),
            { doc =>
              doc
                .select("#back")
                .attr("href")   shouldBe returns.routes.TaskListController
                .taskList()
                .url
              doc
                .select("#content > article > form")
                .attr("action") shouldBe routes.YearToDateLiabilityController
                .estimatedIncomeSubmit()
                .url
            }
          )

        }

        "an individual has answered the question before but has " +
          "not completed the section" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithSingleDisposalState(
                IncompleteCalculatedYTDAnswers.empty.copy(
                  estimatedIncome = Some(AmountInPence.fromPounds(12.34))
                ),
                sample[DisposalDate],
                UserType.Individual,
                wasUkResident = true
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("estimatedIncome.title"),
            doc => doc.select("#estimatedIncome").attr("value") shouldBe "12.34"
          )
        }

        "the user has answered the question before but has " +
          "completed the section" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithSingleDisposalState(
                sample[CompleteCalculatedYTDAnswers]
                  .copy(estimatedIncome = AmountInPence.fromPounds(12.34)),
                sample[DisposalDate],
                UserType.Individual,
                wasUkResident = true
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("estimatedIncome.title"),
            { doc =>
              doc.select("#estimatedIncome").attr("value") shouldBe "12.34"
              doc
                .select("#back")
                .attr("href")                              shouldBe routes.YearToDateLiabilityController
                .checkYourAnswers()
                .url
              doc
                .select("#content > article > form")
                .attr("action")                            shouldBe routes.YearToDateLiabilityController
                .estimatedIncomeSubmit()
                .url
            }
          )
        }

      }

      "redirect the page" when {

        "the user is a Trust" in {
          val (session, _, _) = sessionWithSingleDisposalState(
            None,
            Some(sample[DisposalDate]),
            UserType.Organisation,
            wasUkResident = true,
            Some(completeReliefDetailsAnswersWithNoOtherReliefs)
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              session
            )
          }

          checkIsRedirect(
            performAction(),
            routes.YearToDateLiabilityController.checkYourAnswers()
          )
        }

        "the user is a personal rep in period of admin" in {
          val (session, _, _) = sessionWithSingleDisposalState(
            None,
            Some(sample[DisposalDate]),
            UserType.Individual,
            wasUkResident = true,
            Some(completeReliefDetailsAnswersWithNoOtherReliefs),
            individualUserType = Some(PersonalRepresentativeInPeriodOfAdmin)
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              session
            )
          }

          checkIsRedirect(
            performAction(),
            routes.YearToDateLiabilityController.checkYourAnswers()
          )
        }

      }

    }

    "handling submitted answers to the estimated income page" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.estimatedIncomeSubmit()(
          FakeRequest().withFormUrlEncodedBody(data: _*)
        )

      behave like redirectToStartBehaviour(() => performAction())

      behave like markUnmetDependencyBehaviour(controller.estimatedIncomeSubmit())

      behave like noDisposalDateBehaviour(() => performAction())

      behave like redirectWhenNotSingleDisposalCalculatedJourneyBehaviour(() => performAction())

      behave like unsuccessfulUpdateBehaviourForSingleDisposal(
        IncompleteCalculatedYTDAnswers.empty,
        IncompleteCalculatedYTDAnswers.empty.copy(
          estimatedIncome = Some(AmountInPence.zero)
        ),
        () => performAction("estimatedIncome" -> "0")
      )

      "show a form error" when {

        "the amount of money is invalid" in {
          AmountOfMoneyErrorScenarios
            .amountOfMoneyErrorScenarios("estimatedIncome")
            .foreach { scenario =>
              testFormError(scenario.formData: _*)(
                scenario.expectedErrorMessageKey
              )("estimatedIncome.title")(
                performAction
              )
            }
        }
      }

      "redirect to the check your answers page" when {

        "the answers in this section had not been answered at all" in {
          testSuccessfulUpdatesAfterSubmitWithSingleDisposal(
            performAction("estimatedIncome" -> "1"),
            None,
            IncompleteCalculatedYTDAnswers.empty
              .copy(estimatedIncome = Some(AmountInPence(100L))),
            completeReliefDetailsAnswersWithNoOtherReliefs,
            sample[DisposalDate],
            None,
            None
          )
        }

        "the user had started answering questions in this section but had not completed it" in {
          val answers = sample[IncompleteCalculatedYTDAnswers]
          testSuccessfulUpdatesAfterSubmitWithSingleDisposal(
            performAction("estimatedIncome" -> "1"),
            answers.copy(estimatedIncome = Some(AmountInPence(1L))),
            IncompleteCalculatedYTDAnswers.empty
              .copy(estimatedIncome = Some(AmountInPence(100L)))
          )
        }

        "the user had already completed the section" in {
          val oldAnswers =
            sample[CompleteCalculatedYTDAnswers]
              .copy(estimatedIncome = AmountInPence(1L))
          val newAnswers =
            IncompleteCalculatedYTDAnswers.empty
              .copy(estimatedIncome = Some(AmountInPence(100L)))
          testSuccessfulUpdatesAfterSubmitWithSingleDisposal(
            performAction("estimatedIncome" -> "1"),
            oldAnswers,
            newAnswers
          )
        }

      }

      "not do any updates if the submitted answer is the same as one already stored in session and" when {

        "the section is incomplete" in {
          val answers = sample[IncompleteCalculatedYTDAnswers].copy(
            estimatedIncome = Some(AmountInPence(1L)),
            personalAllowance = Some(AmountInPence(2L))
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithSingleDisposalState(
                answers,
                sample[DisposalDate],
                UserType.Individual,
                wasUkResident = true
              )._1
            )
          }

          checkIsRedirect(
            performAction("estimatedIncome" -> "0.01"),
            routes.YearToDateLiabilityController.checkYourAnswers()
          )
        }

        "the section is complete" in {
          val answers = sample[CompleteCalculatedYTDAnswers].copy(
            estimatedIncome = AmountInPence(1L),
            personalAllowance = Some(AmountInPence(2L))
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithSingleDisposalState(
                answers,
                sample[DisposalDate],
                UserType.Individual,
                wasUkResident = true
              )._1
            )
          }

          checkIsRedirect(
            performAction("estimatedIncome" -> "0.01"),
            routes.YearToDateLiabilityController.checkYourAnswers()
          )
        }

      }

    }

    "handling requests to display the personal allowance page" must {

      def performAction(): Future[Result] =
        controller.personalAllowance()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      behave like markUnmetDependencyBehaviour(controller.personalAllowance())

      behave like noDisposalDateBehaviour(performAction)

      behave like noEstimatedIncomeBehaviour(performAction)

      behave like redirectWhenNotSingleDisposalCalculatedJourneyBehaviour(
        performAction
      )

      "redirect to the check you answers page" when {

        "the estimated income is zero" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithSingleDisposalState(
                sample[CompleteCalculatedYTDAnswers].copy(
                  estimatedIncome = AmountInPence.zero
                ),
                sample[DisposalDate],
                UserType.Individual,
                wasUkResident = true
              )._1
            )
          }

          checkIsRedirect(
            performAction(),
            routes.YearToDateLiabilityController.checkYourAnswers()
          )
        }
        "the user is a Trust" in {
          val (session, _, _) = sessionWithSingleDisposalState(
            sample[CompleteCalculatedYTDAnswers].copy(
              estimatedIncome = AmountInPence(100)
            ),
            sample[DisposalDate],
            UserType.Organisation,
            wasUkResident = true
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkIsRedirect(
            performAction(),
            routes.YearToDateLiabilityController.checkYourAnswers()
          )
        }

        "the user is a personal rep in period of admin" in {
          val (session, _, _) = sessionWithSingleDisposalState(
            None,
            Some(sample[DisposalDate]),
            UserType.Individual,
            wasUkResident = true,
            Some(completeReliefDetailsAnswersWithNoOtherReliefs),
            individualUserType = Some(PersonalRepresentativeInPeriodOfAdmin)
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              session
            )
          }

          checkIsRedirect(
            performAction(),
            routes.YearToDateLiabilityController.checkYourAnswers()
          )
        }
      }

      "display the page" when {

        "the estimated income is greater than zero and" when {

          "the section is incomplete" in {
            val taxYear2020 =
              sample[TaxYear].copy(
                startDateInclusive = LocalDate.of(2020, 4, 6),
                LocalDate.of(2021, 4, 6)
              )
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionWithSingleDisposalState(
                  sample[IncompleteCalculatedYTDAnswers].copy(
                    estimatedIncome = Some(AmountInPence.fromPounds(12.34)),
                    personalAllowance = None
                  ),
                  sample[DisposalDate].copy(taxYear = taxYear2020),
                  UserType.Individual,
                  wasUkResident = true
                )._1
              )
            }

            checkPageIsDisplayed(
              performAction(),
              messageFromMessageKey(
                "personalAllowance.title",
                taxYear2020.startDateInclusive.getYear.toString,
                taxYear2020.endDateExclusive.getYear.toString
              ),
              { doc =>
                doc
                  .select("#back")
                  .attr("href")   shouldBe routes.YearToDateLiabilityController
                  .estimatedIncome()
                  .url
                doc
                  .select("#content > article > form")
                  .attr("action") shouldBe routes.YearToDateLiabilityController
                  .personalAllowanceSubmit()
                  .url
              }
            )
          }

          "the section is complete" in {
            val disposalDate = sample[DisposalDate]
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionWithSingleDisposalState(
                  sample[CompleteCalculatedYTDAnswers].copy(
                    estimatedIncome = AmountInPence(1L),
                    personalAllowance = Some(AmountInPence(1234L))
                  ),
                  disposalDate,
                  UserType.Individual,
                  wasUkResident = true
                )._1
              )
            }

            checkPageIsDisplayed(
              performAction(),
              messageFromMessageKey(
                "personalAllowance.title",
                disposalDate.taxYear.startDateInclusive.getYear.toString,
                disposalDate.taxYear.endDateExclusive.getYear.toString
              ),
              { doc =>
                doc.select("#personalAllowance").attr("value") shouldBe "12.34"
                doc
                  .select("#back")
                  .attr("href")                                shouldBe routes.YearToDateLiabilityController
                  .checkYourAnswers()
                  .url
                doc
                  .select("#content > article > form")
                  .attr("action")                              shouldBe routes.YearToDateLiabilityController
                  .personalAllowanceSubmit()
                  .url
              }
            )
          }

        }

      }

    }

    "handling submitted answers to the personal allowance page" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.personalAllowanceSubmit()(
          FakeRequest().withFormUrlEncodedBody(data: _*)
        )

      behave like redirectToStartBehaviour(() => performAction())

      behave like markUnmetDependencyBehaviour(controller.personalAllowanceSubmit())

      behave like noDisposalDateBehaviour(() => performAction())

      behave like noEstimatedIncomeBehaviour(() => performAction())

      behave like redirectWhenNotSingleDisposalCalculatedJourneyBehaviour(() => performAction())

      {
        val completeAnswers = sample[CompleteCalculatedYTDAnswers].copy(
          estimatedIncome = AmountInPence(1L),
          personalAllowance = Some(AmountInPence(2L))
        )
        val newAnswers      = IncompleteCalculatedYTDAnswers.empty.copy(
          estimatedIncome = Some(completeAnswers.estimatedIncome),
          personalAllowance = Some(AmountInPence.zero)
        )
        behave like unsuccessfulUpdateBehaviourForSingleDisposal(
          completeAnswers,
          newAnswers,
          () => performAction("personalAllowance" -> "0")
        )
      }

      "redirect to the check you answers page" when {

        "the estimated income is zero" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithSingleDisposalState(
                sample[CompleteCalculatedYTDAnswers].copy(
                  estimatedIncome = AmountInPence.zero
                ),
                sample[DisposalDate],
                UserType.Individual,
                wasUkResident = true
              )._1
            )
          }

          checkIsRedirect(
            performAction(),
            routes.YearToDateLiabilityController.checkYourAnswers()
          )
        }

      }

      "show a form error" when {

        "the amount of money is invalid" in {
          val personalAllowance    = AmountInPence(1250000L)
          val maxPersonalAllowance = AmountInPence(2000000L)
          val taxYear              =
            sample[TaxYear].copy(
              personalAllowance = personalAllowance,
              maxPersonalAllowance = maxPersonalAllowance
            )
          val disposalDate         = sample[DisposalDate].copy(taxYear = taxYear)
          val session              = sessionWithSingleDisposalState(
            IncompleteCalculatedYTDAnswers.empty.copy(
              estimatedIncome = Some(AmountInPence(1L))
            ),
            disposalDate,
            UserType.Individual,
            wasUkResident = true
          )._1
          val args                 = Map(
            "personalAllowance.error.tooSmall" -> List(
              MoneyUtils.formatAmountOfMoneyWithPoundSign(
                taxYear.maxPersonalAllowance.inPounds()
              )
            ),
            "personalAllowance.error.tooLarge" -> List(
              MoneyUtils.formatAmountOfMoneyWithPoundSign(
                taxYear.maxPersonalAllowance.inPounds()
              ),
              disposalDate.taxYear.startDateInclusive.getYear.toString,
              disposalDate.taxYear.endDateExclusive.getYear.toString
            )
          )
          AmountOfMoneyErrorScenarios
            .amountOfMoneyErrorScenarios(
              "personalAllowance",
              maxPersonalAllowance.inPounds()
            )
            .foreach { scenario =>
              withClue(s"For $scenario: ") {
                testFormError(scenario.formData: _*)(
                  scenario.expectedErrorMessageKey,
                  args.getOrElse(scenario.expectedErrorMessageKey, Nil)
                )(
                  "personalAllowance.title",
                  disposalDate.taxYear.startDateInclusive.getYear.toString,
                  disposalDate.taxYear.endDateExclusive.getYear.toString
                )(
                  performAction,
                  session
                )
              }
            }
        }
      }

      "redirect to the check your answers page" when {

        val disposalDate = sample[DisposalDate].copy(
          taxYear = sample[TaxYear].copy(
            personalAllowance = AmountInPence(1000L),
            maxPersonalAllowance = AmountInPence(1000L)
          )
        )

        "the user had started answering questions in this section but had not completed it" in {
          val oldAnswers =
            IncompleteCalculatedYTDAnswers.empty.copy(
              estimatedIncome = Some(AmountInPence(1L)),
              personalAllowance = None,
              hasEstimatedDetails = Some(sample[Boolean])
            )

          val newAnswers = oldAnswers.copy(
            personalAllowance = Some(AmountInPence(100L)),
            hasEstimatedDetails = None
          )

          testSuccessfulUpdatesAfterSubmitWithSingleDisposal(
            performAction("personalAllowance" -> "1"),
            oldAnswers,
            newAnswers,
            sample[CompleteReliefDetailsAnswers],
            disposalDate
          )
        }

        "the user had already completed the section" in {
          val oldAnswers =
            sample[CompleteCalculatedYTDAnswers].copy(
              estimatedIncome = AmountInPence(1L),
              personalAllowance = Some(AmountInPence(2L))
            )

          val newAnswers = IncompleteCalculatedYTDAnswers.empty.copy(
            estimatedIncome = Some(oldAnswers.estimatedIncome),
            personalAllowance = Some(AmountInPence(100L))
          )

          testSuccessfulUpdatesAfterSubmitWithSingleDisposal(
            performAction("personalAllowance" -> "1"),
            oldAnswers,
            newAnswers,
            completeReliefDetailsAnswersWithNoOtherReliefs,
            disposalDate
          )
        }

      }

      "not do any updates if the submitted answer is the same as one already stored in session and" when {

        val disposalDate = sample[DisposalDate].copy(
          taxYear = sample[TaxYear].copy(
            personalAllowance = AmountInPence(1000L),
            maxPersonalAllowance = AmountInPence(1000L)
          )
        )

        "the section is incomplete" in {
          val answers = sample[IncompleteCalculatedYTDAnswers].copy(
            estimatedIncome = Some(AmountInPence(1L)),
            personalAllowance = Some(AmountInPence(2L))
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithSingleDisposalState(
                answers,
                disposalDate,
                UserType.Individual,
                wasUkResident = true
              )._1
            )
          }

          checkIsRedirect(
            performAction("personalAllowance" -> "0.02"),
            routes.YearToDateLiabilityController.checkYourAnswers()
          )
        }

        "the section is complete" in {
          val answers = sample[CompleteCalculatedYTDAnswers].copy(
            estimatedIncome = AmountInPence(1L),
            personalAllowance = Some(AmountInPence(2L))
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithSingleDisposalState(
                answers,
                disposalDate,
                UserType.Individual,
                wasUkResident = true
              )._1
            )
          }

          checkIsRedirect(
            performAction("personalAllowance" -> "0.02"),
            routes.YearToDateLiabilityController.checkYourAnswers()
          )
        }

      }

    }

    "handling requests to display the has estimated details page" when {

      def performAction(): Future[Result] =
        controller.hasEstimatedDetails()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      behave like markUnmetDependencyBehaviour(controller.hasEstimatedDetails())

      behave like noEstimatedIncomeBehaviour(performAction)

      "handling users on a calculated journey" must {

        "redirect to the personal allowance page" when {

          "the estimated income is more than zero and the user has not answered " +
            "the personal allowance question yet" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionWithSingleDisposalState(
                  IncompleteCalculatedYTDAnswers.empty.copy(
                    estimatedIncome = Some(AmountInPence(1L))
                  ),
                  sample[DisposalDate],
                  UserType.Individual,
                  wasUkResident = true
                )._1
              )
            }

            checkIsRedirect(
              performAction(),
              routes.YearToDateLiabilityController.personalAllowance()
            )
          }

        }

        "display the page" when {

          def test(
            answers: YearToDateLiabilityAnswers,
            backLink: Call
          ): Unit = {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionWithSingleDisposalState(
                  answers,
                  sample[DisposalDate],
                  UserType.Individual,
                  wasUkResident = true
                )._1
              )
            }

            checkPageIsDisplayed(
              performAction(),
              messageFromMessageKey("hasEstimatedDetails.title"),
              { doc =>
                doc.select("#back").attr("href") shouldBe backLink.url
                doc
                  .select("#content > article > form")
                  .attr("action")                shouldBe routes.YearToDateLiabilityController
                  .hasEstimatedDetailsSubmit()
                  .url
              }
            )
          }

          "the estimated income is greater than zero and" when {

            "the section is incomplete and the estimated income is zero" in {
              test(
                sample[IncompleteCalculatedYTDAnswers].copy(
                  estimatedIncome = Some(AmountInPence.zero)
                ),
                routes.YearToDateLiabilityController.estimatedIncome()
              )
            }

            "the section is incomplete and the estimated income is non-zero" in {
              test(
                sample[IncompleteCalculatedYTDAnswers].copy(
                  estimatedIncome = Some(AmountInPence(100L)),
                  personalAllowance = Some(AmountInPence(1L))
                ),
                routes.YearToDateLiabilityController.personalAllowance()
              )
            }

            "the section is complete and the estimated income is zero" in {
              test(
                sample[CompleteCalculatedYTDAnswers].copy(
                  estimatedIncome = AmountInPence.zero,
                  personalAllowance = None
                ),
                routes.YearToDateLiabilityController.checkYourAnswers()
              )
            }

            "the section is complete and the estimated income is non-zero" in {
              test(
                sample[CompleteCalculatedYTDAnswers].copy(
                  estimatedIncome = AmountInPence(100L),
                  personalAllowance = Some(AmountInPence(1L))
                ),
                routes.YearToDateLiabilityController.checkYourAnswers()
              )
            }

          }

        }
      }

      "handling user on a non-calculated journey" must {

        "redirect to the taxable gain page" when {

          "that question has not been answered yet" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionWithMultipleDisposalsState(
                  IncompleteNonCalculatedYTDAnswers.empty,
                  UserType.Individual,
                  wasUkResident = true,
                  isFurtherReturn = false,
                  None
                )._1
              )
            }

            checkIsRedirect(
              performAction(),
              routes.YearToDateLiabilityController.taxableGainOrLoss()
            )
          }

        }

        "display the page" when {

          def test(
            sessionData: SessionData,
            backLink: Call,
            extraChecks: Document => Unit = _ => ()
          ): Unit = {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(sessionData)
            }

            checkPageIsDisplayed(
              performAction(),
              messageFromMessageKey("hasEstimatedDetails.title"),
              { doc =>
                doc.select("#back").attr("href") shouldBe backLink.url
                doc
                  .select("#content > article > form")
                  .attr("action")                shouldBe routes.YearToDateLiabilityController
                  .hasEstimatedDetailsSubmit()
                  .url
                extraChecks(doc)
              }
            )
          }

          "the user has not answered the question yet" in {
            test(
              sessionWithMultipleDisposalsState(
                IncompleteNonCalculatedYTDAnswers.empty.copy(
                  taxableGainOrLoss = Some(AmountInPence.zero)
                ),
                UserType.Individual,
                wasUkResident = true,
                isFurtherReturn = false,
                None
              )._1,
              routes.YearToDateLiabilityController.taxableGainOrLoss()
            )
          }

          "the user has answered the question before" in {
            val answers = sample[CompleteNonCalculatedYTDAnswers].copy(
              hasEstimatedDetails = true
            )
            test(
              sessionWithMultipleDisposalsState(
                answers,
                UserType.Individual,
                wasUkResident = true,
                isFurtherReturn = false,
                None
              )._1,
              routes.YearToDateLiabilityController.checkYourAnswers(),
              doc =>
                doc
                  .select("#hasEstimatedDetails-true")
                  .attr("checked") shouldBe "checked"
            )
          }

        }

      }

      "handling a TRUST user on a non-calculated journey" must {

        "display the page" when {

          def testCompletedWithTrust(
            answers: YearToDateLiabilityAnswers,
            backLink: Call,
            extraChecks: Document => Unit = _ => ()
          ): Unit = {
            val (session, fillingOutReturn, _) =
              sessionWithMultipleDisposalsState(
                answers,
                UserType.Organisation,
                wasUkResident = true,
                isFurtherReturn = false,
                None
              )
            val fillingOutReturnWithTrust      =
              fillingOutReturn.copy(subscribedDetails =
                fillingOutReturn.subscribedDetails
                  .copy(name = Left(sample[TrustName]))
              )
            val sessionDataWithTrust           =
              session.copy(journeyStatus = Some(fillingOutReturnWithTrust))

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(sessionDataWithTrust)
            }

            checkPageIsDisplayed(
              performAction(),
              messageFromMessageKey("hasEstimatedDetails.title"),
              { doc =>
                doc.select("#back").attr("href") shouldBe backLink.url
                doc
                  .select("#hasEstimatedDetails > #yes-content")
                  .text                          shouldBe messageFromMessageKey(
                  "hasEstimatedDetails.trust.yes.content"
                )
                doc
                  .select("#content > article > form")
                  .attr("action")                shouldBe routes.YearToDateLiabilityController
                  .hasEstimatedDetailsSubmit()
                  .url
                extraChecks(doc)
              }
            )
          }

          "the user has not answered the question yet" in {
            testCompletedWithTrust(
              IncompleteNonCalculatedYTDAnswers.empty.copy(
                taxableGainOrLoss = Some(AmountInPence(100)),
                hasEstimatedDetails = None
              ),
              routes.YearToDateLiabilityController.taxableGainOrLoss()
            )
          }

          "the user has answered the question before" in {
            val answers = sample[CompleteNonCalculatedYTDAnswers].copy(
              hasEstimatedDetails = true
            )

            testCompletedWithTrust(
              answers,
              routes.YearToDateLiabilityController.checkYourAnswers(),
              doc =>
                doc
                  .select("#hasEstimatedDetails-true")
                  .attr("checked") shouldBe "checked"
            )
          }

        }

      }

      "redirect to the check your answers endpoint" when {

        "it is an amend journey and the original return did not contain any estimates" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithMultipleDisposalsState(
                IncompleteNonCalculatedYTDAnswers.empty,
                UserType.Individual,
                wasUkResident = true,
                isFurtherReturn = false,
                Some(
                  sample[AmendReturnData].copy(
                    originalReturn = sample[CompleteReturnWithSummary].copy(
                      completeReturn = sample[CompleteMultipleDisposalsReturn].copy(
                        yearToDateLiabilityAnswers = sample[CompleteNonCalculatedYTDAnswers].copy(
                          hasEstimatedDetails = false
                        )
                      )
                    )
                  )
                )
              )._1
            )
          }

          checkIsRedirect(
            performAction(),
            routes.YearToDateLiabilityController.checkYourAnswers()
          )
        }

      }

    }

    "handling submitted answers to the has estimated details page" when {

      def performAction(data: (String, String)*): Future[Result] =
        controller.hasEstimatedDetailsSubmit()(
          FakeRequest().withFormUrlEncodedBody(data: _*)
        )

      behave like markUnmetDependencyBehaviour(controller.hasEstimatedDetailsSubmit())

      "handling users on a calculated journey" must {
        behave like redirectToStartBehaviour(() => performAction())

        behave like noEstimatedIncomeBehaviour(() => performAction())

        {
          val answers = IncompleteCalculatedYTDAnswers.empty.copy(
            estimatedIncome = Some(AmountInPence(1L)),
            personalAllowance = Some(AmountInPence(2L))
          )

          behave like unsuccessfulUpdateBehaviourForSingleDisposal(
            answers,
            answers.copy(hasEstimatedDetails = Some(true)),
            () => performAction("hasEstimatedDetails" -> "true")
          )
        }

        "redirect to the personal allowance page" when {

          "the estimated income is more than zero and the user has not answered " +
            "the personal allowance question yet" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionWithSingleDisposalState(
                  IncompleteCalculatedYTDAnswers.empty.copy(
                    estimatedIncome = Some(AmountInPence(1L))
                  ),
                  sample[DisposalDate],
                  UserType.Individual,
                  wasUkResident = true
                )._1
              )
            }

            checkIsRedirect(
              performAction(),
              routes.YearToDateLiabilityController.personalAllowance()
            )
          }

        }

        "show a form error" when {
          val answers = IncompleteCalculatedYTDAnswers.empty.copy(
            estimatedIncome = Some(AmountInPence(1L)),
            personalAllowance = Some(AmountInPence(2L))
          )

          val currentSession = sessionWithSingleDisposalState(
            answers,
            sample[DisposalDate],
            UserType.Individual,
            wasUkResident = true
          )._1

          def test(data: (String, String)*)(expectedErrorMessageKey: String) =
            testFormError(data: _*)(expectedErrorMessageKey)(
              "hasEstimatedDetails.title"
            )(performAction, currentSession)

          "nothing is submitted" in {
            test()("hasEstimatedDetails.error.required")
          }

          "the data submitted is invalid" in {
            test("hasEstimatedDetails" -> "abc")(
              "hasEstimatedDetails.error.boolean"
            )
          }

        }

        "redirect to the check your answers page" when {

          "all updates are successful and" when {

            "the journey was incomplete" in {
              val answers = IncompleteCalculatedYTDAnswers.empty.copy(
                estimatedIncome = Some(AmountInPence(1L)),
                personalAllowance = Some(AmountInPence(2L))
              )

              val updatedAnswers =
                answers.copy(hasEstimatedDetails = Some(false))

              testSuccessfulUpdatesAfterSubmitWithSingleDisposal(
                performAction("hasEstimatedDetails" -> "false"),
                answers,
                updatedAnswers
              )
            }

            "the journey was complete" in {
              val taxDue = sample[AmountInPence]

              val answers = CompleteCalculatedYTDAnswers(
                estimatedIncome = AmountInPence.zero,
                personalAllowance = None,
                hasEstimatedDetails = false,
                calculatedTaxDue = sample[CalculatedTaxDue],
                taxDue = taxDue,
                Some(sample[MandatoryEvidence])
              )

              val updatedAnswers =
                IncompleteCalculatedYTDAnswers(
                  estimatedIncome = Some(AmountInPence.zero),
                  personalAllowance = None,
                  hasEstimatedDetails = Some(true),
                  None,
                  taxDue = Some(taxDue),
                  None,
                  None,
                  None
                )

              val draftReturn        = sample[DraftSingleDisposalReturn]
                .copy(yearToDateLiabilityAnswers = Some(answers))
              val updatedDraftReturn = draftReturn
                .copy(yearToDateLiabilityAnswers = Some(updatedAnswers))

              testSuccessfulUpdatesAfterSubmit(
                performAction("hasEstimatedDetails" -> "true"),
                draftReturn,
                updatedDraftReturn
              )
            }
          }
        }

        "not do any updates if the submitted answer is the same as one already stored in session and" when {

          "the section is incomplete" in {

            val session = sessionWithSingleDisposalState(
              IncompleteCalculatedYTDAnswers.empty.copy(
                estimatedIncome = Some(AmountInPence(1L)),
                personalAllowance = Some(AmountInPence(2L)),
                hasEstimatedDetails = Some(true)
              ),
              sample[DisposalDate],
              UserType.Individual,
              wasUkResident = true
            )._1

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
            }

            checkIsRedirect(
              performAction("hasEstimatedDetails" -> "true"),
              routes.YearToDateLiabilityController.checkYourAnswers()
            )
          }

          "the section is complete" in {
            val session = sessionWithSingleDisposalState(
              CompleteCalculatedYTDAnswers(
                AmountInPence(1L),
                Some(AmountInPence(2L)),
                hasEstimatedDetails = false,
                sample[CalculatedTaxDue],
                sample[AmountInPence],
                Some(sample[MandatoryEvidence])
              ),
              sample[DisposalDate],
              UserType.Individual,
              wasUkResident = true
            )._1

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
            }

            checkIsRedirect(
              performAction("hasEstimatedDetails" -> "false"),
              routes.YearToDateLiabilityController.checkYourAnswers()
            )
          }

        }
      }

      "handling users on a non-calculated journey" must {

        {
          val answers = IncompleteNonCalculatedYTDAnswers.empty.copy(
            taxableGainOrLoss = Some(AmountInPence.zero)
          )

          val (_, journey, draftReturn) = sessionWithMultipleDisposalsState(
            answers,
            UserType.Individual,
            wasUkResident = true,
            isFurtherReturn = false,
            None
          )
          val updatedJourney            =
            journey.copy(draftReturn =
              draftReturn.copy(yearToDateLiabilityAnswers = Some(answers.copy(hasEstimatedDetails = Some(false))))
            )

          behave like unsuccessfulUpdateBehaviour(
            journey,
            updatedJourney,
            () => performAction("hasEstimatedDetails" -> "false")
          )
        }

        "show a form error" when {
          val answers        = sample[CompleteNonCalculatedYTDAnswers].copy(
            hasEstimatedDetails = false
          )
          val currentSession = sessionWithMultipleDisposalsState(
            answers,
            UserType.Individual,
            wasUkResident = true,
            isFurtherReturn = false,
            None
          )._1

          def test(data: (String, String)*)(expectedErrorMessageKey: String) =
            testFormError(data: _*)(expectedErrorMessageKey)(
              "hasEstimatedDetails.title"
            )(performAction, currentSession)

          "nothing is submitted" in {
            test()("hasEstimatedDetails.error.required")
          }

          "the data submitted is invalid" in {
            test("hasEstimatedDetails" -> "abc")(
              "hasEstimatedDetails.error.boolean"
            )
          }

        }

        "redirect to the check your answers page" when {

          "all updates are successful and" when {

            "the user is on a multiple disposal journey" in {
              val answers =
                IncompleteNonCalculatedYTDAnswers.empty.copy(
                  taxableGainOrLoss = Some(AmountInPence(1L))
                )

              val (session, journey, draftReturn) =
                sessionWithMultipleDisposalsState(
                  answers,
                  UserType.Individual,
                  wasUkResident = true,
                  isFurtherReturn = false,
                  None
                )
              val updatedDraftReturn              =
                draftReturn.copy(yearToDateLiabilityAnswers = Some(answers.copy(hasEstimatedDetails = Some(true))))

              val updatedJourney = journey.copy(draftReturn = updatedDraftReturn)
              val updatedSession = session.copy(journeyStatus = Some(updatedJourney))

              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
                mockStoreDraftReturn(updatedJourney)(Right(()))
                mockStoreSession(updatedSession)(Right(()))
              }

              checkIsRedirect(
                performAction("hasEstimatedDetails" -> "true"),
                routes.YearToDateLiabilityController.checkYourAnswers()
              )
            }

            "the user is on a non-calculated single disposal journey" in {
              val answers =
                sample[CompleteNonCalculatedYTDAnswers].copy(
                  hasEstimatedDetails = true
                )

              val newAnswers = IncompleteNonCalculatedYTDAnswers(
                Some(answers.taxableGainOrLoss),
                Some(false),
                None,
                None,
                None,
                None,
                answers.yearToDateLiability,
                answers.checkForRepayment
              )

              val (session, journey, draftReturn) =
                sessionWithSingleDisposalState(
                  Some(answers),
                  Some(sample[DisposalDate]),
                  UserType.Individual,
                  wasUkResident = true,
                  Some(
                    sample[CompleteReliefDetailsAnswers]
                      .copy(otherReliefs = Some(sample[OtherReliefsOption.OtherReliefs]))
                  )
                )
              val updatedDraftReturn              =
                draftReturn.copy(yearToDateLiabilityAnswers = Some(newAnswers))
              val updatedJourney                  = journey.copy(draftReturn = updatedDraftReturn)
              val updatedSession                  = session.copy(journeyStatus = Some(updatedJourney))

              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
                mockStoreDraftReturn(updatedJourney)(Right(()))
                mockStoreSession(updatedSession)(Right(()))
              }

              checkIsRedirect(
                performAction("hasEstimatedDetails" -> "false"),
                routes.YearToDateLiabilityController.checkYourAnswers()
              )

            }

          }
        }

        "not do any updates" when {

          "the answer in the session is the same as the one already stored" in {
            val answers =
              IncompleteNonCalculatedYTDAnswers.empty.copy(
                taxableGainOrLoss = Some(AmountInPence(1L)),
                hasEstimatedDetails = Some(false)
              )

            val session = sessionWithMultipleDisposalsState(
              answers,
              UserType.Individual,
              wasUkResident = true,
              isFurtherReturn = false,
              None
            )._1

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
            }

            checkIsRedirect(
              performAction("hasEstimatedDetails" -> "false"),
              routes.YearToDateLiabilityController.checkYourAnswers()
            )
          }

        }

      }

      "redirect to the check your answers endpoint" when {

        "it is an amend journey and the original return did not contain any estimates" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithMultipleDisposalsState(
                IncompleteNonCalculatedYTDAnswers.empty,
                UserType.Individual,
                wasUkResident = true,
                isFurtherReturn = false,
                Some(
                  sample[AmendReturnData].copy(
                    originalReturn = sample[CompleteReturnWithSummary].copy(
                      completeReturn = sample[CompleteMultipleDisposalsReturn].copy(
                        yearToDateLiabilityAnswers = sample[CompleteNonCalculatedYTDAnswers].copy(
                          hasEstimatedDetails = false
                        )
                      )
                    )
                  )
                )
              )._1
            )
          }

          checkIsRedirect(
            performAction(),
            routes.YearToDateLiabilityController.checkYourAnswers()
          )
        }

      }

    }

    "handling requests to display the tax due page" must {

      def performAction(): Future[Result] = controller.taxDue()(FakeRequest())

      val disposalDate              = sample[DisposalDate]
      val disposalDetailsAnswers    = sample[CompleteDisposalDetailsAnswers]
      val acquisitionDetailsAnswers = sample[CompleteAcquisitionDetailsAnswers]
      val reliefDetailsAnswers      = sample[CompleteReliefDetailsAnswers]
      val exemptionAndLossesAnswers = sample[CompleteExemptionAndLossesAnswers]
      val initialGainOrLossAnswers  = sample[AmountInPence]

      def draftReturnWithAnswers(
        yearToDateLiabilityAnswers: YearToDateLiabilityAnswers,
        individualUserType: Option[IndividualUserType],
        disposalDate: DisposalDate = disposalDate
      ): (DraftSingleDisposalReturn, CompleteSingleDisposalTriageAnswers) = {
        val triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
          .copy(individualUserType = individualUserType, disposalDate = disposalDate)
        val draftReturn   = sample[DraftSingleDisposalReturn].copy(
          triageAnswers = triageAnswers,
          disposalDetailsAnswers = Some(disposalDetailsAnswers),
          acquisitionDetailsAnswers = Some(acquisitionDetailsAnswers),
          reliefDetailsAnswers = Some(reliefDetailsAnswers),
          exemptionAndLossesAnswers = Some(exemptionAndLossesAnswers),
          yearToDateLiabilityAnswers = Some(yearToDateLiabilityAnswers),
          initialGainOrLoss = Some(initialGainOrLossAnswers)
        )
        draftReturn -> triageAnswers
      }

      def calculateRequest(
        estimatedIncome: AmountInPence,
        personalAllowance: AmountInPence,
        isATrust: Boolean,
        triageAnswers: CompleteSingleDisposalTriageAnswers
      ) =
        CalculateCgtTaxDueRequest(
          triageAnswers,
          disposalDetailsAnswers,
          acquisitionDetailsAnswers,
          reliefDetailsAnswers,
          exemptionAndLossesAnswers,
          estimatedIncome,
          personalAllowance,
          Some(initialGainOrLossAnswers),
          isATrust
        )

      behave like redirectToStartBehaviour(performAction)

      behave like markUnmetDependencyBehaviour(controller.taxDue())

      behave like noEstimatedIncomeBehaviour(performAction)

      behave like incompleteOtherJourneysBehaviour(performAction)

      behave like redirectWhenNotSingleDisposalCalculatedJourneyBehaviour(
        performAction
      )

      "redirect to the check your answers page" when {

        "the estimated income is more than zero and the user has not answered " +
          "the personal allowance question yet" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithSingleDisposalState(
                IncompleteCalculatedYTDAnswers.empty.copy(
                  estimatedIncome = Some(AmountInPence(1L))
                ),
                sample[DisposalDate],
                UserType.Individual,
                wasUkResident = true
              )._1
            )
          }

          checkIsRedirect(
            performAction(),
            routes.YearToDateLiabilityController.checkYourAnswers()
          )
        }

        "the user hasn't verified whether or not any details given in the return were estimates" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  sample[FillingOutReturn].copy(
                    subscribedDetails = sample[SubscribedDetails]
                      .copy(name = Right(sample[IndividualName])),
                    draftReturn = singleDisposalDraftReturnWithCompleteJourneys(
                      Some(
                        IncompleteCalculatedYTDAnswers.empty.copy(
                          estimatedIncome = Some(AmountInPence(1L)),
                          personalAllowance = Some(AmountInPence(2L))
                        )
                      ),
                      sample[DisposalDate],
                      completeReliefDetailsAnswersWithNoOtherReliefs
                    ),
                    amendReturnData = None
                  )
                )
              )
            )
          }

          checkIsRedirect(
            performAction(),
            routes.YearToDateLiabilityController.checkYourAnswers()
          )
        }

      }

      "show an error page" when {

        "there is an error getting the calculated tax due" in {
          val subscribedDetails =
            sample[SubscribedDetails].copy(name = Right(sample[IndividualName]))

          val answers = IncompleteCalculatedYTDAnswers.empty.copy(
            estimatedIncome = Some(AmountInPence(1L)),
            personalAllowance = Some(AmountInPence(2L)),
            hasEstimatedDetails = Some(false)
          )

          val (draftReturn, triageAnswers) = draftReturnWithAnswers(answers, None)

          val calculateCgtTaxDueRequest = calculateRequest(
            AmountInPence(1L),
            AmountInPence(2L),
            isATrust = false,
            triageAnswers
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  sample[FillingOutReturn]
                    .copy(
                      subscribedDetails = subscribedDetails,
                      draftReturn = draftReturn,
                      amendReturnData = None
                    )
                )
              )
            )
            mockCalculationService(calculateCgtTaxDueRequest)(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction())
        }

        "there is an error storing the calculated tax due" in {
          val subscribedDetails =
            sample[SubscribedDetails].copy(name = Right(sample[IndividualName]))

          val calculatedTaxDue             = sample[CalculatedTaxDue]
          val answers                      = IncompleteCalculatedYTDAnswers.empty.copy(
            estimatedIncome = Some(AmountInPence(1L)),
            personalAllowance = Some(AmountInPence(2L)),
            hasEstimatedDetails = Some(false)
          )
          val (draftReturn, triageAnswers) = draftReturnWithAnswers(answers, individualUserType = None)
          val fillingOutReturn             =
            sample[FillingOutReturn].copy(
              draftReturn = draftReturn,
              subscribedDetails = subscribedDetails,
              amendReturnData = None
            )
          val updatedFillingOutReturn      = fillingOutReturn.copy(draftReturn =
            draftReturn.copy(
              yearToDateLiabilityAnswers = Some(answers.copy(calculatedTaxDue = Some(calculatedTaxDue)))
            )
          )
          val calculateCgtTaxDueRequest    = calculateRequest(
            AmountInPence(1L),
            AmountInPence(2L),
            isATrust = false,
            triageAnswers
          )
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(journeyStatus = Some(fillingOutReturn))
            )
            mockCalculationService(calculateCgtTaxDueRequest)(
              Right(calculatedTaxDue)
            )
            mockStoreSession(
              SessionData.empty
                .copy(journeyStatus = Some(updatedFillingOutReturn))
            )(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction())
        }

      }

      "display the page" when {

        def test(
          answers: YearToDateLiabilityAnswers,
          subscribedDetails: SubscribedDetails,
          individualUserType: Option[IndividualUserType],
          mockCalculateTaxDue: (
            FillingOutReturn,
            DraftSingleDisposalReturn,
            CompleteSingleDisposalTriageAnswers
          ) => Unit,
          backLink: Call
        ): Unit = {
          val (draftReturn, triageAnswers) = draftReturnWithAnswers(answers, individualUserType)
          val fillingOutReturn             = sample[FillingOutReturn].copy(
            subscribedDetails = subscribedDetails,
            draftReturn = draftReturn,
            amendReturnData = None
          )

          val session =
            SessionData.empty.copy(journeyStatus = Some(fillingOutReturn))
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockCalculateTaxDue(fillingOutReturn, draftReturn, triageAnswers)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("taxDue.title"),
            { doc =>
              doc.select("#back").attr("href") shouldBe backLink.url
              doc
                .select("#content > article > form")
                .attr("action")                shouldBe routes.YearToDateLiabilityController
                .taxDueSubmit()
                .url
            }
          )
        }

        "the section is incomplete and a calculation hasn't already been done" when {
          val answers = IncompleteCalculatedYTDAnswers(
            Some(AmountInPence.zero),
            None,
            Some(true),
            None,
            None,
            None,
            None,
            None
          )

          val calculatedTaxDue = sample[CalculatedTaxDue]

          "the user is an individual" in {
            test(
              answers,
              sample[SubscribedDetails]
                .copy(name = Right(sample[IndividualName])),
              None,
              {
                case (fillingOutReturn, draftReturn, triageAnswers) =>
                  mockCalculationService(
                    calculateRequest(
                      AmountInPence.zero,
                      AmountInPence.zero,
                      isATrust = false,
                      triageAnswers
                    )
                  )(
                    Right(calculatedTaxDue)
                  )
                  mockStoreSession(
                    SessionData.empty.copy(
                      journeyStatus = Some(
                        fillingOutReturn.copy(draftReturn =
                          draftReturn.copy(
                            yearToDateLiabilityAnswers = Some(
                              answers.copy(calculatedTaxDue = Some(calculatedTaxDue))
                            )
                          )
                        )
                      )
                    )
                  )(Right(()))
              },
              routes.YearToDateLiabilityController.hasEstimatedDetails()
            )
          }

          "the user is a trust" in {
            val answers = IncompleteCalculatedYTDAnswers(
              Some(AmountInPence.zero),
              None,
              Some(true),
              None,
              None,
              None,
              None,
              None
            )

            val calculatedTaxDue = sample[CalculatedTaxDue]

            test(
              answers,
              sample[SubscribedDetails]
                .copy(name = Left(sample[TrustName])),
              None,
              {
                case (fillingOutReturn, draftReturn, triageAnswers) =>
                  mockCalculationService(
                    calculateRequest(
                      AmountInPence.zero,
                      AmountInPence.zero,
                      isATrust = true,
                      triageAnswers
                    )
                  )(
                    Right(calculatedTaxDue)
                  )
                  mockStoreSession(
                    SessionData.empty.copy(
                      journeyStatus = Some(
                        fillingOutReturn.copy(draftReturn =
                          draftReturn.copy(
                            yearToDateLiabilityAnswers = Some(
                              answers.copy(calculatedTaxDue = Some(calculatedTaxDue))
                            )
                          )
                        )
                      )
                    )
                  )(Right(()))
              },
              routes.YearToDateLiabilityController.hasEstimatedDetails()
            )
          }

          "the user is a personal rep in period of admin" in {
            val answers = IncompleteCalculatedYTDAnswers(
              Some(AmountInPence.zero),
              None,
              Some(true),
              None,
              None,
              None,
              None,
              None
            )

            val calculatedTaxDue = sample[CalculatedTaxDue]

            test(
              answers,
              sample[SubscribedDetails]
                .copy(name = Right(sample[IndividualName])),
              Some(PersonalRepresentativeInPeriodOfAdmin),
              {
                case (fillingOutReturn, draftReturn, triageAnswers) =>
                  mockCalculationService(
                    calculateRequest(
                      AmountInPence.zero,
                      AmountInPence.zero,
                      isATrust = true,
                      triageAnswers
                    )
                  )(
                    Right(calculatedTaxDue)
                  )
                  mockStoreSession(
                    SessionData.empty.copy(
                      journeyStatus = Some(
                        fillingOutReturn.copy(draftReturn =
                          draftReturn.copy(
                            yearToDateLiabilityAnswers = Some(
                              answers.copy(calculatedTaxDue = Some(calculatedTaxDue))
                            )
                          )
                        )
                      )
                    )
                  )(Right(()))
              },
              routes.YearToDateLiabilityController.hasEstimatedDetails()
            )
          }

        }

        "the section is incomplete and a calculation has already been done" in {
          val answers = IncompleteCalculatedYTDAnswers(
            Some(AmountInPence.zero),
            None,
            Some(true),
            Some(sample[CalculatedTaxDue]),
            None,
            None,
            None,
            None
          )

          test(
            answers,
            sample[SubscribedDetails].copy(name = Left(sample[TrustName])),
            None,
            { case (_, _, _) => () },
            routes.YearToDateLiabilityController.hasEstimatedDetails()
          )
        }

        "the section is complete" in {
          val answers = sample[CompleteCalculatedYTDAnswers]
          test(
            answers,
            sample[SubscribedDetails],
            None,
            { case (_, _, _) => () },
            routes.YearToDateLiabilityController.checkYourAnswers()
          )
        }

      }

    }

    "handling submitted answers to the tax due page for a calculated journey" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.taxDueSubmit()(
          FakeRequest().withFormUrlEncodedBody(data: _*)
        )

      behave like redirectToStartBehaviour(() => performAction())

      behave like markUnmetDependencyBehaviour(controller.taxDueSubmit())

      behave like noEstimatedIncomeBehaviour(() => performAction())

      behave like incompleteOtherJourneysBehaviour(() => performAction())

      behave like redirectWhenNotSingleDisposalCalculatedJourneyBehaviour(() => performAction())

      {
        val oldAnswers  = sample[IncompleteCalculatedYTDAnswers].copy(
          estimatedIncome = Some(AmountInPence(0L)),
          calculatedTaxDue = Some(sample[CalculatedTaxDue]),
          personalAllowance = None,
          hasEstimatedDetails = Some(true),
          mandatoryEvidence = None,
          expiredEvidence = None,
          pendingUpscanUpload = None
        )
        val draftReturn = singleDisposalDraftReturnWithCompleteJourneys(
          Some(oldAnswers),
          sample[DisposalDate],
          completeReliefDetailsAnswersWithNoOtherReliefs
        )
        val journey     = sample[FillingOutReturn].copy(
          draftReturn = draftReturn,
          previousSentReturns = None,
          amendReturnData = None
        )
        val newJourney  = journey.copy(draftReturn =
          draftReturn.copy(
            yearToDateLiabilityAnswers = Some(oldAnswers.copy(taxDue = Some(AmountInPence(123L))))
          )
        )

        behave like unsuccessfulUpdateBehaviour(
          journey,
          newJourney,
          () => performAction("taxDue" -> "£1.23")
        )
      }

      "redirect to the check you answers page" when {

        "the estimated income is more than zero and the user has not answered " +
          "the personal allowance question yet" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithSingleDisposalState(
                IncompleteCalculatedYTDAnswers.empty.copy(
                  estimatedIncome = Some(AmountInPence(1L))
                ),
                sample[DisposalDate],
                UserType.Individual,
                wasUkResident = true
              )._1
            )
          }

          checkIsRedirect(
            performAction(),
            routes.YearToDateLiabilityController.checkYourAnswers()
          )
        }

        "the user hasn't verified whether or not any details given in the return were estimates" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithSingleDisposalState(
                IncompleteCalculatedYTDAnswers.empty.copy(
                  estimatedIncome = Some(AmountInPence(1L)),
                  personalAllowance = Some(AmountInPence.zero)
                ),
                sample[DisposalDate],
                UserType.Individual,
                wasUkResident = true
              )._1
            )
          }

          checkIsRedirect(
            performAction(),
            routes.YearToDateLiabilityController.checkYourAnswers()
          )
        }

        "there is no calculated tax due in session" in {
          val answers     = sample[IncompleteCalculatedYTDAnswers].copy(
            estimatedIncome = Some(AmountInPence(0L)),
            hasEstimatedDetails = Some(true),
            calculatedTaxDue = None,
            personalAllowance = None,
            mandatoryEvidence = None,
            expiredEvidence = None
          )
          val draftReturn = singleDisposalDraftReturnWithCompleteJourneys(
            Some(answers),
            sample[DisposalDate],
            completeReliefDetailsAnswersWithNoOtherReliefs
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(sample[FillingOutReturn].copy(draftReturn = draftReturn))
              )
            )
          }

          checkIsRedirect(
            performAction(),
            routes.YearToDateLiabilityController.checkYourAnswers()
          )
        }

      }

      "show a form error" when {
        val draftReturn =
          singleDisposalDraftReturnWithCompleteJourneys(
            Some(sample[CompleteCalculatedYTDAnswers]),
            sample[DisposalDate],
            completeReliefDetailsAnswersWithNoOtherReliefs
          )

        val currentSession = SessionData.empty.copy(
          journeyStatus = Some(
            sample[FillingOutReturn].copy(
              draftReturn = draftReturn
            )
          )
        )

        "the data submitted is invalid" in {
          AmountOfMoneyErrorScenarios
            .amountOfMoneyErrorScenarios("taxDue")
            .foreach { scenario =>
              withClue(s"For $scenario: ") {
                testFormError(scenario.formData: _*)(
                  scenario.expectedErrorMessageKey
                )("taxDue.title")(
                  performAction,
                  currentSession
                )
              }

            }
        }

      }

      "redirect to the check your answers page" when {

        "all updates are successful and" when {

          "the journey was incomplete" in {
            val disposalDate = sample[DisposalDate]

            val answers     = IncompleteCalculatedYTDAnswers(
              Some(AmountInPence(1L)),
              Some(AmountInPence(2L)),
              Some(true),
              Some(sample[CalculatedTaxDue]),
              Some(AmountInPence(123L)),
              Some(sample[MandatoryEvidence]),
              None,
              None
            )
            val draftReturn = singleDisposalDraftReturnWithCompleteJourneys(
              Some(answers),
              disposalDate,
              completeReliefDetailsAnswersWithNoOtherReliefs
            )

            val updatedDraftReturn = draftReturn.copy(
              yearToDateLiabilityAnswers = Some(
                answers.copy(
                  taxDue = Some(AmountInPence(101L)),
                  mandatoryEvidence = None
                )
              )
            )

            testSuccessfulUpdatesAfterSubmit(
              performAction("taxDue" -> "1.01"),
              draftReturn,
              updatedDraftReturn
            )
          }

          "the journey was complete" in {
            val disposalDate = sample[DisposalDate]

            val answers     = CompleteCalculatedYTDAnswers(
              AmountInPence(1L),
              Some(AmountInPence(2L)),
              false,
              setTaxDue(sample[CalculatedTaxDue], AmountInPence(100L)),
              AmountInPence(1L),
              Some(sample[MandatoryEvidence])
            )
            val draftReturn = singleDisposalDraftReturnWithCompleteJourneys(
              Some(answers),
              disposalDate,
              completeReliefDetailsAnswersWithNoOtherReliefs
            )

            val updatedDraftReturn = draftReturn.copy(
              yearToDateLiabilityAnswers = Some(
                IncompleteCalculatedYTDAnswers(
                  Some(answers.estimatedIncome),
                  answers.personalAllowance,
                  Some(answers.hasEstimatedDetails),
                  Some(answers.calculatedTaxDue),
                  Some(AmountInPence(123456L)),
                  None,
                  None,
                  None
                )
              )
            )

            testSuccessfulUpdatesAfterSubmit(
              performAction("taxDue" -> "£1,234.56"),
              draftReturn,
              updatedDraftReturn
            )
          }
        }
      }

    }

    "handling requests to display the check you answers page" when {

      def performAction(): Future[Result] =
        controller.checkYourAnswers()(FakeRequest())

      behave like markUnmetDependencyBehaviour(controller.checkYourAnswers())

      "the user is on a calculated journey" must {

        val completeAnswers = CompleteCalculatedYTDAnswers(
          AmountInPence(1L),
          Some(AmountInPence(2L)),
          sample[Boolean],
          setTaxDue(sample[CalculatedTaxDue], AmountInPence(3L)),
          AmountInPence(4L),
          Some(sample[MandatoryEvidence])
        )

        val allQuestionAnswered = IncompleteCalculatedYTDAnswers(
          Some(completeAnswers.estimatedIncome),
          completeAnswers.personalAllowance,
          Some(completeAnswers.hasEstimatedDetails),
          Some(completeAnswers.calculatedTaxDue),
          Some(completeAnswers.taxDue),
          completeAnswers.mandatoryEvidence,
          None,
          None
        )

        def testRedirectWhenIncompleteAnswers(
          answers: IncompleteCalculatedYTDAnswers,
          expectedRedirect: Call
        ): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithSingleDisposalState(
                answers,
                sample[DisposalDate],
                UserType.Individual,
                wasUkResident = true
              )._1
            )
          }

          checkIsRedirect(performAction(), expectedRedirect)
        }

        def testRedirectWhenIsATrust(
          answers: IncompleteCalculatedYTDAnswers,
          expectedRedirect: Call
        ): Unit = {

          val (sessionData, _, _) =
            sessionWithSingleDisposalState(
              answers,
              sample[DisposalDate],
              UserType.Organisation,
              wasUkResident = true
            )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionData)
          }

          checkIsRedirect(performAction(), expectedRedirect)
        }

        def testRedirectWhenIsPeriodOfAdmin(
          answers: IncompleteCalculatedYTDAnswers,
          expectedRedirect: Call
        ): Unit = {

          val (sessionData, _, _) =
            sessionWithSingleDisposalState(
              Some(answers),
              Some(sample[DisposalDate]),
              UserType.Individual,
              wasUkResident = true,
              individualUserType = Some(PersonalRepresentativeInPeriodOfAdmin)
            )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionData)
          }

          checkIsRedirect(performAction(), expectedRedirect)
        }

        behave like redirectToStartBehaviour(performAction)

        behave like unsuccessfulUpdateBehaviourForSingleDisposal(
          allQuestionAnswered,
          completeAnswers,
          () => performAction()
        )

        "redirect to the estimated income page" when {

          "that question has not been answered yet" in {
            testRedirectWhenIncompleteAnswers(
              allQuestionAnswered.copy(estimatedIncome = None),
              routes.YearToDateLiabilityController.estimatedIncome()
            )
          }

        }

        "do NOT redirect to the estimated income page" when {
          "the user is a Trust" in {
            testRedirectWhenIsATrust(
              allQuestionAnswered
                .copy(estimatedIncome = None, hasEstimatedDetails = None),
              routes.YearToDateLiabilityController.hasEstimatedDetails()
            )
          }

          "the user is a personal rep in period of admin" in {
            testRedirectWhenIsPeriodOfAdmin(
              allQuestionAnswered
                .copy(estimatedIncome = None, hasEstimatedDetails = None),
              routes.YearToDateLiabilityController.hasEstimatedDetails()
            )
          }
        }

        "redirect to the personal allowance page" when {

          "that question has not been answered yet and the estimated income is non zero" in {
            testRedirectWhenIncompleteAnswers(
              allQuestionAnswered.copy(
                estimatedIncome = Some(AmountInPence(1L)),
                personalAllowance = None
              ),
              routes.YearToDateLiabilityController.personalAllowance()
            )
          }

        }

        "do NOT redirect to the personal allowance page" when {
          "the user is a Trust" in {
            testRedirectWhenIsATrust(
              allQuestionAnswered
                .copy(personalAllowance = None, hasEstimatedDetails = None),
              routes.YearToDateLiabilityController.hasEstimatedDetails()
            )
          }

          "the user is a personal rep in period of admin" in {
            testRedirectWhenIsPeriodOfAdmin(
              allQuestionAnswered
                .copy(personalAllowance = None, hasEstimatedDetails = None),
              routes.YearToDateLiabilityController.hasEstimatedDetails()
            )
          }
        }

        "redirect to the has estimated details  page" when {

          "that question has not been answered yet" in {
            testRedirectWhenIncompleteAnswers(
              allQuestionAnswered.copy(
                estimatedIncome = Some(AmountInPence.zero),
                personalAllowance = None,
                hasEstimatedDetails = None
              ),
              routes.YearToDateLiabilityController.hasEstimatedDetails()
            )
          }

        }

        "redirect to the tax due page" when {

          "that question has not been answered yet" in {
            testRedirectWhenIncompleteAnswers(
              allQuestionAnswered.copy(
                taxDue = None
              ),
              routes.YearToDateLiabilityController.taxDue()
            )
          }

          "there is no calulated tax due" in {
            testRedirectWhenIncompleteAnswers(
              allQuestionAnswered.copy(
                calculatedTaxDue = None
              ),
              routes.YearToDateLiabilityController.taxDue()
            )
          }

        }

        "redirect to the upload mandatory evidence page" when {

          "that question hasn't been completed yet and the calculated tax due is not the same as the submitted tax due" in {
            testRedirectWhenIncompleteAnswers(
              allQuestionAnswered.copy(
                calculatedTaxDue = Some(
                  setTaxDue(sample[CalculatedTaxDue], AmountInPence(500L))
                ),
                taxDue = Some(AmountInPence(200L)),
                mandatoryEvidence = None
              ),
              routes.YearToDateLiabilityController.uploadMandatoryEvidence()
            )
          }

          "there is a pending upscan upload in session and it is successfully removed" in {
            val (session, journey, draftReturn) =
              sessionWithSingleDisposalState(
                allQuestionAnswered
                  .copy(pendingUpscanUpload = Some(sample[UpscanUpload])),
                sample[DisposalDate],
                sample[UserType],
                wasUkResident = sample[Boolean]
              )

            val newDraftReturn = draftReturn.copy(
              yearToDateLiabilityAnswers = Some(allQuestionAnswered.copy(pendingUpscanUpload = None))
            )
            val newJourney     = journey.copy(draftReturn = newDraftReturn)
            val newSession     = session.copy(journeyStatus = Some(newJourney))

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockStoreDraftReturn(newJourney)(Right(()))
              mockStoreSession(newSession)(Right(()))
            }

            checkIsRedirect(
              performAction(),
              routes.YearToDateLiabilityController.uploadMandatoryEvidence()
            )
          }

        }

        "redirect to the file expired page" when {

          "there is an expired file" in {
            testRedirectWhenIncompleteAnswers(
              allQuestionAnswered.copy(
                expiredEvidence = Some(sample[MandatoryEvidence])
              ),
              routes.YearToDateLiabilityController.mandatoryEvidenceExpired()
            )

          }

        }

        "show an error page" when {

          "there is pending evidence in session and" when {

            val (session, journey, draftReturn) =
              sessionWithSingleDisposalState(
                allQuestionAnswered
                  .copy(pendingUpscanUpload = Some(sample[UpscanUpload])),
                sample[DisposalDate],
                sample[UserType],
                wasUkResident = sample[Boolean]
              )

            val newDraftReturn = draftReturn.copy(
              yearToDateLiabilityAnswers = Some(allQuestionAnswered.copy(pendingUpscanUpload = None))
            )
            val newJourney     = journey.copy(draftReturn = newDraftReturn)

            val newSession = session.copy(journeyStatus = Some(newJourney))

            "there is an error updating the draft return" in {
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
                mockStoreDraftReturn(newJourney)(Left(Error("")))
              }

              checkIsTechnicalErrorPage(performAction())
            }

            "there is an error updating the session" in {
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
                mockStoreDraftReturn(newJourney)(Right(()))
                mockStoreSession(newSession)(Left(Error("")))
              }

              checkIsTechnicalErrorPage(performAction())
            }

          }

        }

        "show the page" when {

          "the section is complete" in {
            forAll { completeAnswers: CompleteCalculatedYTDAnswers =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithSingleDisposalState(
                    completeAnswers,
                    sample[DisposalDate],
                    UserType.Individual,
                    wasUkResident = true
                  )._1
                )
              }

              checkPageIsDisplayed(
                performAction(),
                messageFromMessageKey("ytdLiability.cya.title"),
                doc =>
                  validateCalculatedYearToDateLiabilityPage(
                    completeAnswers,
                    isATrust = false,
                    hideEstimatesQuestion = false,
                    doc
                  )
              )
            }
          }

          "the user is on an amend journey where the estimates question should be hidden" in {
            forAll { completeAnswers: CompleteCalculatedYTDAnswers =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithSingleDisposalState(
                    Some(completeAnswers),
                    Some(sample[DisposalDate]),
                    UserType.Individual,
                    wasUkResident = true,
                    amendReturnData = Some(
                      sample[AmendReturnData].copy(
                        originalReturn = sample[CompleteReturnWithSummary].copy(
                          completeReturn = sample[CompleteSingleDisposalReturn].copy(
                            yearToDateLiabilityAnswers = Right(
                              sample[CompleteCalculatedYTDAnswers].copy(
                                hasEstimatedDetails = false
                              )
                            )
                          )
                        )
                      )
                    )
                  )._1
                )
              }

              checkPageIsDisplayed(
                performAction(),
                messageFromMessageKey("ytdLiability.cya.title"),
                doc =>
                  validateCalculatedYearToDateLiabilityPage(
                    completeAnswers,
                    isATrust = false,
                    hideEstimatesQuestion = true,
                    doc
                  )
              )
            }
          }

          "the section has just been completed and all updates are successful" in {
            val (session, journey, draftReturn) =
              sessionWithSingleDisposalState(
                allQuestionAnswered,
                sample[DisposalDate],
                UserType.Individual,
                wasUkResident = true
              )
            val updatedDraftReturn              = draftReturn
              .copy(yearToDateLiabilityAnswers = Some(completeAnswers))
            val updatedJourney                  = journey.copy(draftReturn = updatedDraftReturn)
            val updatedSession                  = session.copy(journeyStatus = Some(updatedJourney))

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockStoreDraftReturn(updatedJourney)(Right(()))
              mockStoreSession(updatedSession)(Right(()))
            }

            checkPageIsDisplayed(
              performAction(),
              messageFromMessageKey("ytdLiability.cya.title")
            )
          }

        }

      }

      "the user is on a non-calculated journey" must {

        val completeAnswers = CompleteNonCalculatedYTDAnswers(
          AmountInPence(1L),
          true,
          AmountInPence(2L),
          sample[MandatoryEvidence],
          None,
          None
        )

        val allQuestionAnswered = IncompleteNonCalculatedYTDAnswers(
          Some(completeAnswers.taxableGainOrLoss),
          Some(completeAnswers.hasEstimatedDetails),
          Some(completeAnswers.taxDue),
          Some(completeAnswers.mandatoryEvidence),
          None,
          None,
          None,
          None
        )

        val (session, journey, draftReturn) = sessionWithMultipleDisposalsState(
          allQuestionAnswered,
          UserType.Individual,
          wasUkResident = true,
          isFurtherReturn = false,
          None
        )
        val updatedDraftReturn              =
          draftReturn.copy(yearToDateLiabilityAnswers = Some(completeAnswers))
        val updatedJourney                  = journey.copy(draftReturn = updatedDraftReturn)
        val updatedSession                  = session.copy(journeyStatus = Some(updatedJourney))

        def testRedirectWhenIncompleteAnswers(
          answers: IncompleteNonCalculatedYTDAnswers,
          expectedRedirect: Call,
          isFurtherReturn: Boolean = false
        ): Unit = {

          val (sessionData, fillingOutReturn, _) =
            sessionWithMultipleDisposalsState(
              answers,
              UserType.Individual,
              wasUkResident = true,
              isFurtherReturn = isFurtherReturn,
              None
            )
          val fillingOutReturnWithIndividual     =
            fillingOutReturn.copy(subscribedDetails =
              fillingOutReturn.subscribedDetails
                .copy(name = Right(sample[IndividualName]))
            )
          val sessionDataWithIndividual          = sessionData.copy(journeyStatus = Some(fillingOutReturnWithIndividual))
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionDataWithIndividual)
          }

          checkIsRedirect(performAction(), expectedRedirect)
        }

        def testRedirectWhenIsATrust(
          answers: IncompleteNonCalculatedYTDAnswers,
          expectedRedirect: Call
        ): Unit = {

          val (sessionData, _, _) = sessionWithMultipleDisposalsState(
            answers,
            UserType.Organisation,
            wasUkResident = true,
            isFurtherReturn = false,
            None
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionData)
          }

          checkIsRedirect(performAction(), expectedRedirect)
        }

        behave like unsuccessfulUpdateBehaviourForSingleDisposal(
          allQuestionAnswered,
          completeAnswers,
          () => performAction()
        )

        "redirect to the taxable income page" when {

          "that question has not been answered yet" in {
            testRedirectWhenIncompleteAnswers(
              allQuestionAnswered.copy(taxableGainOrLoss = None),
              routes.YearToDateLiabilityController.taxableGainOrLoss()
            )

          }

        }

        "redirect to the has estimated details page" when {

          "that question has not been answered yet" in {
            testRedirectWhenIncompleteAnswers(
              allQuestionAnswered.copy(hasEstimatedDetails = None),
              routes.YearToDateLiabilityController.hasEstimatedDetails()
            )

          }
          "the user IS a trust" in {
            testRedirectWhenIsATrust(
              allQuestionAnswered.copy(hasEstimatedDetails = None),
              routes.YearToDateLiabilityController.hasEstimatedDetails()
            )
          }
        }

        "redirect to the non-calculated enter tax due page" when {

          "that question has not been answered yet" in {
            testRedirectWhenIncompleteAnswers(
              allQuestionAnswered.copy(taxDue = None),
              routes.YearToDateLiabilityController.nonCalculatedEnterTaxDue()
            )

          }

        }

        "redirect to the mandatory evidence page" when {

          "that question has not been answered yet" in {
            testRedirectWhenIncompleteAnswers(
              allQuestionAnswered.copy(mandatoryEvidence = None),
              routes.YearToDateLiabilityController.uploadMandatoryEvidence()
            )

          }

          "there is a pending upscan upload in session and it is successfully removed" in {
            val (session, journey, draftReturn) =
              sessionWithMultipleDisposalsState(
                allQuestionAnswered
                  .copy(pendingUpscanUpload = Some(sample[UpscanUpload])),
                sample[UserType],
                wasUkResident = sample[Boolean],
                isFurtherReturn = false,
                None
              )

            val newDraftReturn = draftReturn.copy(
              yearToDateLiabilityAnswers = Some(allQuestionAnswered.copy(pendingUpscanUpload = None))
            )
            val newJourney     = journey.copy(draftReturn = newDraftReturn)

            val newSession = session.copy(journeyStatus = Some(newJourney))

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockStoreDraftReturn(newJourney)(Right(()))
              mockStoreSession(newSession)(Right(()))
            }

            checkIsRedirect(
              performAction(),
              routes.YearToDateLiabilityController.uploadMandatoryEvidence()
            )
          }

        }

        "redirect to the file expired page" when {

          "there is an expired file" in {
            testRedirectWhenIncompleteAnswers(
              allQuestionAnswered.copy(
                expiredEvidence = Some(sample[MandatoryEvidence])
              ),
              routes.YearToDateLiabilityController.mandatoryEvidenceExpired()
            )

          }

        }

        "redirect to the year to date liability page" when {

          "the return is a further return and that question hasn't been answered yet" in {
            testRedirectWhenIncompleteAnswers(
              allQuestionAnswered.copy(
                yearToDateLiability = None
              ),
              routes.YearToDateLiabilityController.yearToDateLiability(),
              isFurtherReturn = true
            )

          }

        }

        "redirect to the repayment page" when {

          "the return is a further return and that question hasn't been answered yet" in {
            testRedirectWhenIncompleteAnswers(
              allQuestionAnswered.copy(
                yearToDateLiability = Some(sample[AmountInPence])
              ),
              routes.YearToDateLiabilityController.repayment(),
              isFurtherReturn = true
            )

          }

        }

        "show an error page" when {

          "there is pending evidence in session and" when {

            val (session, journey, draftReturn) =
              sessionWithMultipleDisposalsState(
                allQuestionAnswered
                  .copy(pendingUpscanUpload = Some(sample[UpscanUpload])),
                sample[UserType],
                wasUkResident = sample[Boolean],
                isFurtherReturn = false,
                None
              )

            val newDraftReturn = draftReturn.copy(
              yearToDateLiabilityAnswers = Some(allQuestionAnswered.copy(pendingUpscanUpload = None))
            )
            val newJourney     = journey.copy(draftReturn = newDraftReturn)

            val newSession = session.copy(journeyStatus = Some(newJourney))

            "there is an error updating the draft return" in {
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
                mockStoreDraftReturn(newJourney)(Left(Error("")))
              }

              checkIsTechnicalErrorPage(performAction())
            }

            "there is an error updating the session" in {
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
                mockStoreDraftReturn(newJourney)(Right(()))
                mockStoreSession(newSession)(Left(Error("")))
              }

              checkIsTechnicalErrorPage(performAction())
            }

          }

        }

        "display the page" when {

          "the user has just answered all the questions in the section and all updates are successful" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockStoreDraftReturn(updatedJourney)(Right(()))
              mockStoreSession(updatedSession)(Right(()))
            }

            checkPageIsDisplayed(
              performAction(),
              messageFromMessageKey("ytdLiability.cya.title"),
              doc =>
                validateNonCalculatedYearToDateLiabilityPage(
                  completeAnswers,
                  doc,
                  Some(UserType.Individual),
                  None,
                  isFurtherOrAmendReturn = false,
                  hideEstimatesQuestion = false
                )
            )
          }

          "the user is on an amend journey where the estimates question should be hidden" in {
            forAll { completeAnswers: CompleteNonCalculatedYTDAnswers =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithSingleDisposalState(
                    Some(completeAnswers),
                    Some(sample[DisposalDate]),
                    UserType.Individual,
                    wasUkResident = true,
                    amendReturnData = Some(
                      sample[AmendReturnData].copy(
                        originalReturn = sample[CompleteReturnWithSummary].copy(
                          completeReturn = sample[CompleteSingleDisposalReturn].copy(
                            yearToDateLiabilityAnswers = Right(
                              sample[CompleteCalculatedYTDAnswers].copy(
                                hasEstimatedDetails = false
                              )
                            )
                          )
                        )
                      )
                    )
                  )._1
                )
              }

              checkPageIsDisplayed(
                performAction(),
                messageFromMessageKey("ytdLiability.cya.title"),
                doc =>
                  validateNonCalculatedYearToDateLiabilityPage(
                    completeAnswers,
                    doc,
                    Some(UserType.Individual),
                    None,
                    isFurtherOrAmendReturn = true,
                    hideEstimatesQuestion = true
                  )
              )
            }
          }

          "the user has just answered all the questions for a further return in the section and all updates are successful" in {
            val yearToDateLiability             = sample[AmountInPence]
            val (session, journey, draftReturn) = sessionWithMultipleDisposalsState(
              allQuestionAnswered.copy(
                yearToDateLiability = Some(yearToDateLiability),
                checkForRepayment = Some(true)
              ),
              UserType.Individual,
              wasUkResident = true,
              isFurtherReturn = true,
              None
            )
            val updatedDraftReturn              =
              draftReturn.copy(yearToDateLiabilityAnswers =
                Some(
                  completeAnswers.copy(
                    yearToDateLiability = Some(yearToDateLiability),
                    checkForRepayment = Some(true)
                  )
                )
              )
            val updatedJourney                  = journey.copy(draftReturn = updatedDraftReturn)
            val updatedSession                  = session.copy(journeyStatus = Some(updatedJourney))

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockStoreDraftReturn(updatedJourney)(Right(()))
              mockStoreSession(updatedSession)(Right(()))
            }

            checkPageIsDisplayed(
              performAction(),
              messageFromMessageKey("ytdLiability.cya.title"),
              doc =>
                validateNonCalculatedYearToDateLiabilityPage(
                  completeAnswers,
                  doc,
                  Some(UserType.Individual),
                  None,
                  isFurtherOrAmendReturn = true,
                  hideEstimatesQuestion = false
                )
            )
          }

          "the user has already completed the section" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(updatedSession)
            }

            checkPageIsDisplayed(
              performAction(),
              messageFromMessageKey("ytdLiability.cya.title"),
              doc =>
                validateNonCalculatedYearToDateLiabilityPage(
                  completeAnswers,
                  doc,
                  Some(UserType.Individual),
                  None,
                  isFurtherOrAmendReturn = false,
                  hideEstimatesQuestion = false
                )
            )
          }

        }

      }

    }

    "handling requests to display the upload mandatory evidence page" must {

      def performAction(): Future[Result] =
        controller.uploadMandatoryEvidence()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      behave like markUnmetDependencyBehaviour(controller.uploadMandatoryEvidence())

      behave like commonUploadMandatoryEvidenceBehaviour(performAction)

      "show an error page" when {

        val answers = IncompleteNonCalculatedYTDAnswers(
          Some(sample[AmountInPence]),
          Some(sample[Boolean]),
          Some(sample[AmountInPence]),
          None,
          None,
          None,
          None,
          None
        )

        val (session, journey, draftReturn) =
          sessionWithMultipleDisposalsState(
            answers,
            UserType.Individual,
            wasUkResident = true,
            isFurtherReturn = false,
            None
          )

        val upscanUpload   = sample[UpscanUpload]
        val newAnswers     = answers.copy(pendingUpscanUpload = Some(upscanUpload))
        val newDraftReturn =
          draftReturn.copy(yearToDateLiabilityAnswers = Some(newAnswers))
        val newJourney     = journey.copy(draftReturn = newDraftReturn)

        "there is an error performing an upscan initiate call" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockUpscanInitiate(
              routes.YearToDateLiabilityController
                .uploadMandatoryEvidenceFailure(),
              _ => routes.YearToDateLiabilityController.scanningMandatoryEvidence()
            )(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction())
        }

        "there is an error updating the draft return" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockUpscanInitiate(
              routes.YearToDateLiabilityController
                .uploadMandatoryEvidenceFailure(),
              _ => routes.YearToDateLiabilityController.scanningMandatoryEvidence()
            )(Right(upscanUpload))
            mockStoreDraftReturn(newJourney)(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction())
        }

        "there is an error updating the session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockUpscanInitiate(
              routes.YearToDateLiabilityController
                .uploadMandatoryEvidenceFailure(),
              _ => routes.YearToDateLiabilityController.scanningMandatoryEvidence()
            )(Right(upscanUpload))
            mockStoreDraftReturn(newJourney)(Right(()))
            mockStoreSession(
              session.copy(
                journeyStatus = Some(journey.copy(draftReturn = newDraftReturn))
              )
            )(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction())
        }

      }

      "display the page" when {

        def test(
          answers: YearToDateLiabilityAnswers,
          backLink: Call,
          isFurtherReturn: Boolean
        ): Unit = {
          val draftReturn = singleDisposalDraftReturnWithCompleteJourneys(
            Some(answers),
            sample[DisposalDate],
            completeReliefDetailsAnswersWithNoOtherReliefs,
            Some(Self)
          )
          val journey     = sample[FillingOutReturn].copy(
            draftReturn = draftReturn,
            previousSentReturns =
              Some(PreviousReturnData(if (isFurtherReturn) List(sample[ReturnSummary]) else List.empty, None)),
            amendReturnData = None
          )

          val session = SessionData.empty.copy(journeyStatus = Some(journey))

          val upscanUpload = sample[UpscanUpload]
          val newAnswers   = answers match {
            case c: CalculatedYTDAnswers    =>
              c.unset(_.expiredEvidence)
                .unset(_.mandatoryEvidence)
                .copy(pendingUpscanUpload = Some(upscanUpload))
            case n: NonCalculatedYTDAnswers =>
              n.unset(_.expiredEvidence)
                .unset(_.mandatoryEvidence)
                .copy(pendingUpscanUpload = Some(upscanUpload))
          }

          val newDraftReturn =
            draftReturn.copy(yearToDateLiabilityAnswers = Some(newAnswers))
          val newJourney     = journey.copy(draftReturn = newDraftReturn)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockUpscanInitiate(
              routes.YearToDateLiabilityController
                .uploadMandatoryEvidenceFailure(),
              _ => routes.YearToDateLiabilityController.scanningMandatoryEvidence()
            )(Right(upscanUpload))
            mockStoreDraftReturn(newJourney)(Right(()))
            mockStoreSession(
              session.copy(
                journeyStatus = Some(journey.copy(draftReturn = newDraftReturn))
              )
            )(Right(()))
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("mandatoryEvidence.title"),
            { doc =>
              doc.select("#back").attr("href") shouldBe backLink.url
              doc
                .select("#content > article > form")
                .attr(
                  "action"
                )                              shouldBe upscanUpload.upscanUploadMeta.uploadRequest.href
            }
          )
        }

        "the user is on a calculated journey and" when {

          val calculatedTaxDue = sample[GainCalculatedTaxDue]
            .copy(amountOfTaxDue = AmountInPence(100L))

          "the section is incomplete" in {
            test(
              IncompleteCalculatedYTDAnswers(
                Some(AmountInPence.zero),
                None,
                Some(true),
                Some(calculatedTaxDue),
                Some(AmountInPence(200L)),
                None,
                None,
                None
              ),
              routes.YearToDateLiabilityController.taxDue(),
              isFurtherReturn = false
            )
          }

          "the section is complete" in {
            test(
              sample[CompleteCalculatedYTDAnswers].copy(
                calculatedTaxDue = calculatedTaxDue,
                taxDue = AmountInPence(200L)
              ),
              routes.YearToDateLiabilityController.checkYourAnswers(),
              isFurtherReturn = false
            )
          }
        }

        "the user is on a non-calculated journey and" when {

          "the section is incomplete" in {
            test(
              IncompleteNonCalculatedYTDAnswers(
                Some(AmountInPence.zero),
                Some(true),
                Some(AmountInPence(200L)),
                None,
                None,
                None,
                None,
                None
              ),
              routes.YearToDateLiabilityController.nonCalculatedEnterTaxDue(),
              isFurtherReturn = false
            )
          }

          "the section is incomplete and it is a further return" in {
            test(
              IncompleteNonCalculatedYTDAnswers(
                Some(sample[AmountInPence]),
                Some(true),
                Some(sample[AmountInPence]),
                None,
                None,
                None,
                Some(sample[AmountInPence]),
                Some(true)
              ),
              routes.YearToDateLiabilityController.repayment(),
              isFurtherReturn = true
            )
          }

          "the section is complete" in {
            test(
              sample[CompleteNonCalculatedYTDAnswers],
              routes.YearToDateLiabilityController.checkYourAnswers(),
              isFurtherReturn = false
            )
          }
        }

      }

    }

    "handling submits from the check you answers page" must {

      def performAction(): Future[Result] =
        controller.checkYourAnswersSubmit()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      behave like markUnmetDependencyBehaviour(controller.checkYourAnswersSubmit())

      "redirect to the task list page" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            sessionWithSingleDisposalState(
              sample[CompleteCalculatedYTDAnswers],
              sample[DisposalDate],
              UserType.Individual,
              wasUkResident = true
            )._1
          )
        }

        checkIsRedirect(
          performAction(),
          returns.routes.TaskListController.taskList()
        )
      }

    }

    "handling requests to display the taxable gain or net loss page" must {

      def performAction(): Future[Result] =
        controller.taxableGainOrLoss()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      behave like markUnmetDependencyBehaviour(controller.taxableGainOrLoss())

      behave like redirectWhenNotNonCalculatedJourneyBehaviour(performAction)

      "display the page" when {

        def test(
          session: SessionData,
          expectedBackLink: Call,
          expectedTitleKey: String,
          testPage: Document => Unit = _ => ()
        ): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(expectedTitleKey),
            { doc =>
              doc.select("#back").attr("href") shouldBe expectedBackLink.url
              doc
                .select("#content > article > form")
                .attr("action")                shouldBe routes.YearToDateLiabilityController
                .taxableGainOrLossSubmit()
                .url
              testPage(doc)
            }
          )
        }

        "the user has not started this section yet and they are on a multiple disposals journey" in {
          test(
            sessionWithMultipleDisposalsState(
              None,
              UserType.Individual,
              wasUkResident = true
            )._1,
            returns.routes.TaskListController.taskList(),
            "taxableGainOrLoss.multiple.title"
          )
        }

        "the user has not started this section yet and they are on a single disposal journey and " +
          "have selected to use other reliefs" in {
          test(
            sessionWithSingleDisposalState(
              None,
              Some(sample[DisposalDate]),
              UserType.Individual,
              wasUkResident = true,
              Some(
                sample[CompleteReliefDetailsAnswers].copy(
                  otherReliefs = Some(sample[OtherReliefsOption.OtherReliefs])
                )
              )
            )._1,
            returns.routes.TaskListController.taskList(),
            "taxableGainOrLoss.title"
          )
        }

        "the user has not started this section yet and they are on a single indirect disposal journey and " +
          "have selected to use other reliefs" in {
          test(
            sessionWithSingleIndirectDisposalState(
              None,
              Some(sample[DisposalDate]),
              UserType.Individual,
              wasUkResident = true,
              address = Some(sample[Address])
            )._1,
            returns.routes.TaskListController.taskList(),
            "taxableGainOrLoss.title"
          )
        }

        "the user has already started this uncalculated section but have not completed it yet" in {
          test(
            sessionWithMultipleDisposalsState(
              IncompleteNonCalculatedYTDAnswers.empty.copy(taxableGainOrLoss =
                Some(
                  AmountInPence(-100L)
                )
              ),
              UserType.Individual,
              wasUkResident = true,
              isFurtherReturn = false,
              None
            )._1,
            returns.routes.TaskListController.taskList(),
            "taxableGainOrLoss.multiple.title",
            { doc =>
              doc
                .select("#taxableGainOrLoss-1")
                .attr("checked")                   shouldBe "checked"
              doc.select("#netLoss").attr("value") shouldBe "1"
            }
          )
        }

        "the user has completed this uncalculated section" in {
          test(
            sessionWithSingleMixedUseDisposalState(
              sample[CompleteNonCalculatedYTDAnswers]
                .copy(taxableGainOrLoss = AmountInPence(0L)),
              sample[DisposalDate],
              UserType.Individual,
              wasUkResident = true
            )._1,
            routes.YearToDateLiabilityController.checkYourAnswers(),
            "taxableGainOrLoss.title",
            doc =>
              doc
                .select("#taxableGainOrLoss-2")
                .attr("checked") shouldBe "checked"
          )

        }

        "the user is a personal rep in period of admin for a single disposal journey" in {
          test(
            sessionWithSingleMixedUseDisposalState(
              Some(
                sample[CompleteNonCalculatedYTDAnswers]
                  .copy(taxableGainOrLoss = AmountInPence(0L))
              ),
              Some(sample[DisposalDate]),
              UserType.Individual,
              wasUkResident = true,
              individualUserType = Some(PersonalRepresentativeInPeriodOfAdmin)
            )._1,
            routes.YearToDateLiabilityController.checkYourAnswers(),
            "taxableGainOrLoss.personalRepInPeriodOfAdmin.title",
            doc =>
              doc
                .select("#taxableGainOrLoss-2")
                .attr("checked") shouldBe "checked"
          )
        }

        "the user is a personal rep in period of admin for a multiple disposals journey" in {
          test(
            sessionWithMultipleDisposalsState(
              Some(
                sample[CompleteNonCalculatedYTDAnswers]
                  .copy(taxableGainOrLoss = AmountInPence(0L))
              ),
              UserType.Individual,
              wasUkResident = true,
              individualUserType = Some(PersonalRepresentativeInPeriodOfAdmin)
            )._1,
            routes.YearToDateLiabilityController.checkYourAnswers(),
            "taxableGainOrLoss.personalRepInPeriodOfAdmin.multiple.title",
            doc =>
              doc
                .select("#taxableGainOrLoss-2")
                .attr("checked") shouldBe "checked"
          )
        }

        "the user is on a further return journey and" when {

          def testFurtherReturnPage(
            session: SessionData,
            expectedBackLink: Call,
            expectedTitleKey: String,
            expectedSubtitle: String,
            expectedNetGainLabel: String,
            expectedNetLossLabel: String,
            expectedLinkText: String
          ): Unit =
            test(
              session,
              expectedBackLink,
              expectedTitleKey,
              { doc =>
                doc.select("#taxableGainOrLoss > legend > h2").text()              shouldBe expectedSubtitle
                doc.select("#taxableGainOrLoss > div:nth-child(2) > label").text() shouldBe expectedNetGainLabel
                doc.select("#taxableGainOrLoss > div:nth-child(4) > label").text() shouldBe expectedNetLossLabel
                doc.select("#link").text()                                         shouldBe expectedLinkText
                doc
                  .select("#link")
                  .attr("href")                                                    shouldBe controllers.returns.triage.routes.FurtherReturnGuidanceController
                  .taxableGainGuidance()
                  .url
              }
            )

          val taxYear                    = sample[TaxYear]
          val (taxYearStart, taxYearEnd) =
            taxYear.startDateInclusive.getYear.toString -> taxYear.endDateExclusive.getYear.toString

          "they are completing the return for themselves" in {
            testFurtherReturnPage(
              sessionWithSingleDisposalState(
                None,
                Some(sample[DisposalDate].copy(taxYear = taxYear)),
                UserType.Individual,
                wasUkResident = true,
                Some(
                  sample[CompleteReliefDetailsAnswers].copy(
                    otherReliefs = Some(OtherReliefsOption.NoOtherReliefs)
                  )
                ),
                isFurtherReturn = true,
                individualUserType = Some(Self)
              )._1,
              returns.routes.TaskListController.taskList(),
              "taxableGainOrLoss.furtherReturn.title",
              messageFromMessageKey("taxableGainOrLoss.furtherReturn.h2", taxYearStart, taxYearEnd),
              messageFromMessageKey("taxableGainOrLoss.furtherReturn.gain.label"),
              messageFromMessageKey("taxableGainOrLoss.furtherReturn.loss.label"),
              messageFromMessageKey("taxableGainOrLoss.furtherReturn.link")
            )
          }

          "they are an agent" in {
            testFurtherReturnPage(
              sessionWithMultipleDisposalsState(
                None,
                UserType.Agent,
                wasUkResident = true,
                Some(Self),
                isFurtherReturn = true,
                taxYear
              )._1,
              returns.routes.TaskListController.taskList(),
              "taxableGainOrLoss.agent.furtherReturn.title",
              messageFromMessageKey("taxableGainOrLoss.agent.furtherReturn.h2", taxYearStart, taxYearEnd),
              messageFromMessageKey("taxableGainOrLoss.agent.furtherReturn.gain.label"),
              messageFromMessageKey("taxableGainOrLoss.agent.furtherReturn.loss.label"),
              messageFromMessageKey("taxableGainOrLoss.agent.furtherReturn.link")
            )
          }

          "they are a trust" in {
            testFurtherReturnPage(
              sessionWithSingleIndirectDisposalState(
                None,
                Some(sample[DisposalDate].copy(taxYear = taxYear)),
                UserType.Organisation,
                wasUkResident = true,
                isFurtherReturn = true,
                individualUserType = None
              )._1,
              returns.routes.TaskListController.taskList(),
              "taxableGainOrLoss.trust.furtherReturn.title",
              messageFromMessageKey("taxableGainOrLoss.trust.furtherReturn.h2", taxYearStart, taxYearEnd),
              messageFromMessageKey("taxableGainOrLoss.trust.furtherReturn.gain.label"),
              messageFromMessageKey("taxableGainOrLoss.trust.furtherReturn.loss.label"),
              messageFromMessageKey("taxableGainOrLoss.trust.furtherReturn.link")
            )
          }

          "they are a capacitor" in {
            testFurtherReturnPage(
              sessionWithSingleDisposalState(
                None,
                Some(sample[DisposalDate].copy(taxYear = taxYear)),
                UserType.Individual,
                wasUkResident = true,
                Some(
                  sample[CompleteReliefDetailsAnswers].copy(
                    otherReliefs = Some(OtherReliefsOption.NoOtherReliefs)
                  )
                ),
                isFurtherReturn = true,
                individualUserType = Some(Capacitor)
              )._1,
              returns.routes.TaskListController.taskList(),
              "taxableGainOrLoss.capacitor.furtherReturn.title",
              messageFromMessageKey("taxableGainOrLoss.capacitor.furtherReturn.h2", taxYearStart, taxYearEnd),
              messageFromMessageKey("taxableGainOrLoss.capacitor.furtherReturn.gain.label"),
              messageFromMessageKey("taxableGainOrLoss.capacitor.furtherReturn.loss.label"),
              messageFromMessageKey("taxableGainOrLoss.capacitor.furtherReturn.link")
            )
          }

          "they are a personal rep" in {
            testFurtherReturnPage(
              sessionWithSingleDisposalState(
                Some(sample[CompleteNonCalculatedYTDAnswers]),
                Some(sample[DisposalDate].copy(taxYear = taxYear)),
                UserType.Individual,
                wasUkResident = true,
                Some(
                  sample[CompleteReliefDetailsAnswers].copy(
                    otherReliefs = Some(OtherReliefsOption.NoOtherReliefs)
                  )
                ),
                isFurtherReturn = true,
                individualUserType = Some(PersonalRepresentative)
              )._1,
              routes.YearToDateLiabilityController.checkYourAnswers(),
              "taxableGainOrLoss.personalRep.furtherReturn.title",
              messageFromMessageKey("taxableGainOrLoss.personalRep.furtherReturn.h2", taxYearStart, taxYearEnd),
              messageFromMessageKey("taxableGainOrLoss.personalRep.furtherReturn.gain.label"),
              messageFromMessageKey("taxableGainOrLoss.personalRep.furtherReturn.loss.label"),
              messageFromMessageKey("taxableGainOrLoss.personalRep.furtherReturn.link")
            )
          }

          "they are a personal rep in a period of admin" in {
            testFurtherReturnPage(
              sessionWithSingleDisposalState(
                Some(sample[CompleteNonCalculatedYTDAnswers]),
                Some(sample[DisposalDate].copy(taxYear = taxYear)),
                UserType.Individual,
                wasUkResident = true,
                Some(
                  sample[CompleteReliefDetailsAnswers].copy(
                    otherReliefs = Some(OtherReliefsOption.NoOtherReliefs)
                  )
                ),
                isFurtherReturn = true,
                individualUserType = Some(PersonalRepresentativeInPeriodOfAdmin)
              )._1,
              routes.YearToDateLiabilityController.checkYourAnswers(),
              "taxableGainOrLoss.personalRepInPeriodOfAdmin.furtherReturn.title",
              messageFromMessageKey(
                "taxableGainOrLoss.personalRepInPeriodOfAdmin.furtherReturn.h2",
                taxYearStart,
                taxYearEnd
              ),
              messageFromMessageKey("taxableGainOrLoss.personalRepInPeriodOfAdmin.furtherReturn.gain.label"),
              messageFromMessageKey("taxableGainOrLoss.personalRepInPeriodOfAdmin.furtherReturn.loss.label"),
              messageFromMessageKey("taxableGainOrLoss.personalRepInPeriodOfAdmin.furtherReturn.link")
            )
          }

          "they are an agent of a personal rep in a period of admin" in {
            testFurtherReturnPage(
              sessionWithSingleDisposalState(
                Some(sample[CompleteNonCalculatedYTDAnswers]),
                Some(sample[DisposalDate].copy(taxYear = taxYear)),
                UserType.Agent,
                wasUkResident = true,
                Some(
                  sample[CompleteReliefDetailsAnswers].copy(
                    otherReliefs = Some(OtherReliefsOption.NoOtherReliefs)
                  )
                ),
                isFurtherReturn = true,
                individualUserType = Some(PersonalRepresentativeInPeriodOfAdmin)
              )._1,
              routes.YearToDateLiabilityController.checkYourAnswers(),
              "taxableGainOrLoss.personalRepInPeriodOfAdmin.agent.furtherReturn.title",
              messageFromMessageKey(
                "taxableGainOrLoss.personalRepInPeriodOfAdmin.agent.furtherReturn.h2",
                taxYearStart,
                taxYearEnd
              ),
              messageFromMessageKey("taxableGainOrLoss.personalRepInPeriodOfAdmin.agent.furtherReturn.gain.label"),
              messageFromMessageKey("taxableGainOrLoss.personalRepInPeriodOfAdmin.agent.furtherReturn.loss.label"),
              messageFromMessageKey("taxableGainOrLoss.personalRepInPeriodOfAdmin.agent.furtherReturn.link")
            )
          }

        }

      }

    }

    "handling submits on the taxable gain or net loss page" must {

      def performAction(formData: (String, String)*): Future[Result] =
        controller.taxableGainOrLossSubmit()(
          FakeRequest().withFormUrlEncodedBody(formData: _*)
        )

      behave like redirectToStartBehaviour(() => performAction())

      behave like markUnmetDependencyBehaviour(controller.taxableGainOrLossSubmit())

      behave like redirectWhenNotNonCalculatedJourneyBehaviour(() => performAction())

      {
        val (_, journey, draftReturn) = sessionWithMultipleDisposalsState(
          None,
          UserType.Individual,
          wasUkResident = true
        )

        behave like unsuccessfulUpdateBehaviour(
          journey,
          journey.copy(draftReturn =
            draftReturn.copy(yearToDateLiabilityAnswers =
              Some(
                IncompleteNonCalculatedYTDAnswers.empty.copy(
                  taxableGainOrLoss = Some(AmountInPence(101L))
                )
              )
            )
          ),
          () =>
            performAction(
              "taxableGainOrLoss" -> "0",
              "taxableGain"       -> "1.01"
            )
        )
      }

      "show a form error" when {

        "it is a first return and" when {

          val currentSession = sessionWithMultipleDisposalsState(
            None,
            UserType.Individual,
            wasUkResident = true
          )._1

          def test(data: (String, String)*)(expectedErrorKey: String): Unit =
            testFormError(data: _*)(
              expectedErrorKey
            )("taxableGainOrLoss.multiple.title")(performAction, currentSession)

          "no option is selected" in {
            test()("taxableGainOrLoss.multiple.error.required")
          }

          "the amount of gain is invalid" in {
            AmountOfMoneyErrorScenarios
              .amountOfMoneyErrorScenarios("taxableGain")
              .foreach { scenario =>
                withClue(s"For $scenario: ") {
                  val data = ("taxableGainOrLoss" -> "0") :: scenario.formData
                  test(data: _*)(scenario.expectedErrorMessageKey)
                }
              }
          }

          "the amount of gain is zero" in {
            test(
              "taxableGainOrLoss" -> "0",
              "taxableGain"       -> "0"
            )("taxableGain.multiple.error.tooSmall")
          }

          "the amount of loss is invalid" in {
            AmountOfMoneyErrorScenarios
              .amountOfMoneyErrorScenarios("netLoss")
              .foreach { scenario =>
                withClue(s"For $scenario: ") {
                  val data = ("taxableGainOrLoss" -> "1") :: scenario.formData
                  test(data: _*)(scenario.expectedErrorMessageKey)
                }
              }
          }

          "the amount of loss is zero" in {
            test(
              "taxableGainOrLoss" -> "1",
              "netLoss"           -> "0"
            )("netLoss.multiple.error.tooSmall")
          }
        }

        "it is a further return and" when {

          def test(
            sessionData: SessionData
          )(data: (String, String)*)(expectedTitleKey: String, expectedErrorKey: String): Unit =
            testFormError(data: _*)(
              expectedErrorKey
            )(expectedTitleKey)(performAction, sessionData)

          val testCases = {
            def session(userType: UserType, individualUserType: Option[IndividualUserType]) =
              sessionWithMultipleDisposalsState(
                None,
                userType,
                wasUkResident = true,
                individualUserType,
                isFurtherReturn = true
              )._1

            List(
              ""                                  -> session(UserType.Individual, Some(Self)),
              ".agent"                            -> session(UserType.Agent, Some(Self)),
              ".trust"                            -> session(UserType.Organisation, None),
              ".capacitor"                        -> session(UserType.Individual, Some(Capacitor)),
              ".personalRep"                      -> session(UserType.Individual, Some(PersonalRepresentative)),
              ".personalRepInPeriodOfAdmin"       -> session(
                UserType.Individual,
                Some(PersonalRepresentativeInPeriodOfAdmin)
              ),
              ".personalRepInPeriodOfAdmin.agent" -> session(
                UserType.Agent,
                Some(PersonalRepresentativeInPeriodOfAdmin)
              )
            )
          }

          "no option is selected" in {
            testCases.foreach {
              case (userKey, session) =>
                withClue(s"For user key '$userKey': ") {
                  test(session)()(
                    s"taxableGainOrLoss$userKey.furtherReturn.title",
                    s"taxableGainOrLoss$userKey.furtherReturn.error.required"
                  )
                }
            }
          }

          "the amount of gain is invalid" in {
            testCases.foreach {
              case (userKey, session) =>
                AmountOfMoneyErrorScenarios
                  .amountOfMoneyErrorScenarios("taxableGain", errorContext = Some(s"taxableGain.furtherReturn"))
                  .foreach { scenario =>
                    withClue(s"For user key '$userKey' and $scenario: ") {
                      val data = ("taxableGainOrLoss" -> "0") :: scenario.formData
                      test(session)(data: _*)(
                        s"taxableGainOrLoss$userKey.furtherReturn.title",
                        scenario.expectedErrorMessageKey
                      )
                    }
                  }
            }
          }

          "the amount of gain is zero" in {
            testCases.foreach {
              case (userKey, session) =>
                withClue(s"For user key '$userKey': ") {
                  test(session)(
                    "taxableGainOrLoss" -> "0",
                    "taxableGain"       -> "0"
                  )(
                    s"taxableGainOrLoss$userKey.furtherReturn.title",
                    "taxableGain.furtherReturn.error.tooSmall"
                  )
                }
            }
          }

          "the amount of loss is invalid" in {
            testCases.foreach {
              case (userKey, session) =>
                AmountOfMoneyErrorScenarios
                  .amountOfMoneyErrorScenarios("netLoss", errorContext = Some(s"netLoss.furtherReturn"))
                  .foreach { scenario =>
                    withClue(s"For user key '$userKey' and $scenario: ") {
                      val data = ("taxableGainOrLoss" -> "1") :: scenario.formData
                      test(session)(data: _*)(
                        s"taxableGainOrLoss$userKey.furtherReturn.title",
                        scenario.expectedErrorMessageKey
                      )
                    }
                  }
            }
          }

          "the amount of loss is zero" in {
            testCases.foreach {
              case (userKey, session) =>
                withClue(s"For user key '$userKey': ") {
                  test(session)(
                    "taxableGainOrLoss" -> "1",
                    "netLoss"           -> "0"
                  )(
                    s"taxableGainOrLoss$userKey.furtherReturn.title",
                    "netLoss.furtherReturn.error.tooSmall"
                  )
                }
            }
          }

        }

      }

      "redirect to the check your answers page" when {

        "all updates are successful and" when {

          "the section had not been started yet" in {
            val newAmount = AmountInPence(3000L)

            testSuccessfulUpdatesAfterSubmitWithMultipleDisposals(
              performAction(
                "taxableGainOrLoss" -> "0",
                "taxableGain"       -> "30"
              ),
              None,
              IncompleteNonCalculatedYTDAnswers.empty
                .copy(taxableGainOrLoss = Some(newAmount)),
              None
            )
          }

          "the section had been started but not completed" in {
            val newAmount = AmountInPence(-3000L)

            testSuccessfulUpdatesAfterSubmitWithSingleDisposal(
              performAction(
                "taxableGainOrLoss" -> "1",
                "netLoss"           -> "30"
              ),
              IncompleteNonCalculatedYTDAnswers.empty
                .copy(taxableGainOrLoss = Some(AmountInPence(2L))),
              IncompleteNonCalculatedYTDAnswers.empty
                .copy(taxableGainOrLoss = Some(newAmount)),
              sample[CompleteReliefDetailsAnswers].copy(otherReliefs = Some(sample[OtherReliefsOption.OtherReliefs]))
            )
          }

          "the section was complete" in {
            val newAmount  = AmountInPence(0L)
            val answers    =
              sample[CompleteNonCalculatedYTDAnswers]
                .copy(taxableGainOrLoss = AmountInPence(1L))
            val newAnswers = IncompleteNonCalculatedYTDAnswers(
              Some(newAmount),
              None,
              None,
              None,
              None,
              None,
              answers.yearToDateLiability,
              answers.checkForRepayment
            )
            testSuccessfulUpdatesAfterSubmitWithMultipleDisposals(
              performAction(
                "taxableGainOrLoss" -> "2"
              ),
              answers,
              newAnswers,
              isFurtherReturn = true
            )
          }

          "the user is on an amend journey where the estimates question should be preserved" in {
            val newAmount  = AmountInPence(0L)
            val answers    =
              sample[CompleteNonCalculatedYTDAnswers]
                .copy(taxableGainOrLoss = AmountInPence(1L))
            val newAnswers = IncompleteNonCalculatedYTDAnswers(
              Some(newAmount),
              Some(answers.hasEstimatedDetails),
              None,
              None,
              None,
              None,
              None,
              None
            )
            testSuccessfulUpdatesAfterSubmitWithMultipleDisposals(
              performAction(
                "taxableGainOrLoss" -> "2"
              ),
              answers,
              newAnswers,
              isFurtherReturn = true,
              amendReturnData = Some(
                sample[AmendReturnData].copy(
                  originalReturn = sample[CompleteReturnWithSummary].copy(
                    completeReturn = sample[CompleteSingleMixedUseDisposalReturn].copy(
                      yearToDateLiabilityAnswers = sample[CompleteNonCalculatedYTDAnswers].copy(
                        hasEstimatedDetails = false
                      )
                    )
                  )
                )
              )
            )
          }

          "the section had not been started yet and the user is on an amend journey where the estimates question should be preserved" in {
            val newAmount = AmountInPence(3000L)

            testSuccessfulUpdatesAfterSubmitWithMultipleDisposals(
              performAction(
                "taxableGainOrLoss" -> "0",
                "taxableGain"       -> "30"
              ),
              None,
              IncompleteNonCalculatedYTDAnswers.empty
                .copy(
                  taxableGainOrLoss = Some(newAmount),
                  hasEstimatedDetails = Some(false)
                ),
              amendReturnData = Some(
                sample[AmendReturnData].copy(
                  originalReturn = sample[CompleteReturnWithSummary].copy(
                    completeReturn = sample[CompleteSingleMixedUseDisposalReturn].copy(
                      yearToDateLiabilityAnswers = sample[CompleteNonCalculatedYTDAnswers].copy(
                        hasEstimatedDetails = false
                      )
                    )
                  )
                )
              )
            )
          }

        }

      }

      "not do any updates" when {

        "the answer supplied is the same as one already stored" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithMultipleDisposalsState(
                sample[CompleteNonCalculatedYTDAnswers]
                  .copy(taxableGainOrLoss = AmountInPence.zero),
                UserType.Individual,
                wasUkResident = true,
                isFurtherReturn = false,
                None
              )._1
            )
          }

          checkIsRedirect(
            performAction("taxableGainOrLoss" -> "2"),
            routes.YearToDateLiabilityController.checkYourAnswers()
          )
        }

      }

    }

    "handling requests to display the non calculated enter tax due page" must {

      def performAction(): Future[Result] =
        controller.nonCalculatedEnterTaxDue()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      behave like markUnmetDependencyBehaviour(controller.nonCalculatedEnterTaxDue())

      behave like redirectWhenNotNonCalculatedJourneyBehaviour(performAction)

      behave like noYearToDateLiabilityBehaviour(performAction)

      "redirect to the has estimated details page" when {

        "the question has not been answered yet" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithMultipleDisposalsState(
                IncompleteNonCalculatedYTDAnswers.empty.copy(yearToDateLiability = Some(sample[AmountInPence])),
                UserType.Individual,
                wasUkResident = true,
                isFurtherReturn = false,
                None
              )._1
            )
          }

          checkIsRedirect(
            performAction(),
            routes.YearToDateLiabilityController.hasEstimatedDetails()
          )
        }

      }

      "display the page" when {

        def test(
          session: SessionData,
          expectedBackLink: Call,
          testPage: Document => Unit = _ => ()
        ): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(
              "nonCalculatedTaxDue.title"
            ),
            { doc =>
              doc.select("#back").attr("href") shouldBe expectedBackLink.url
              doc
                .select("#content > article > form")
                .attr("action")                shouldBe routes.YearToDateLiabilityController
                .nonCalculatedEnterTaxDueSubmit()
                .url
              testPage(doc)
            }
          )
        }

        "the user has not answered this question before" in {
          test(
            sessionWithMultipleDisposalsState(
              IncompleteNonCalculatedYTDAnswers.empty
                .copy(
                  hasEstimatedDetails = Some(true)
                ),
              UserType.Individual,
              wasUkResident = true,
              isFurtherReturn = false,
              None
            )._1,
            routes.YearToDateLiabilityController.hasEstimatedDetails()
          )
        }

        "the user has answered this question before" in {
          test(
            sessionWithSingleDisposalState(
              Some(sample[CompleteNonCalculatedYTDAnswers]),
              Some(sample[DisposalDate]),
              UserType.Individual,
              wasUkResident = true,
              Some(
                sample[CompleteReliefDetailsAnswers].copy(otherReliefs = Some(sample[OtherReliefsOption.OtherReliefs]))
              )
            )._1,
            routes.YearToDateLiabilityController.checkYourAnswers()
          )
        }

        "the user has answered this question before for single indirect disposal" in {
          test(
            sessionWithSingleIndirectDisposalState(
              Some(sample[CompleteNonCalculatedYTDAnswers]),
              Some(sample[DisposalDate]),
              UserType.Individual,
              wasUkResident = true
            )._1,
            routes.YearToDateLiabilityController.checkYourAnswers()
          )
        }

        "the user has answered this question before for single mixed use disposal" in {
          test(
            sessionWithSingleMixedUseDisposalState(
              Some(sample[CompleteNonCalculatedYTDAnswers]),
              Some(sample[DisposalDate]),
              UserType.Individual,
              wasUkResident = true
            )._1,
            routes.YearToDateLiabilityController.checkYourAnswers()
          )
        }

        "the return is a further return where the previous year to date is present" when {

          def testCheckTaxDuePage(
            yearToDateLiability: AmountInPence,
            previousYearToDateLiability: AmountInPence,
            taxDue: AmountInPence,
            userType: UserType,
            expectedP1Key: String
          ): Unit = {
            val sessionData = SessionData.empty.copy(
              userType = Some(userType),
              journeyStatus = Some(
                sample[FillingOutReturn].copy(
                  draftReturn = sample[DraftSingleDisposalReturn].copy(
                    triageAnswers = sample[CompleteSingleDisposalTriageAnswers].copy(individualUserType = Some(Self)),
                    yearToDateLiabilityAnswers = Some(
                      sample[CompleteNonCalculatedYTDAnswers].copy(
                        yearToDateLiability = Some(yearToDateLiability)
                      )
                    )
                  ),
                  agentReferenceNumber = if (userType == UserType.Agent) Some(sample[AgentReferenceNumber]) else None,
                  previousSentReturns = Some(
                    sample[PreviousReturnData].copy(
                      summaries = List(sample[ReturnSummary]),
                      previousYearToDate = Some(previousYearToDateLiability)
                    )
                  ),
                  subscribedDetails = sample[SubscribedDetails].copy(
                    name =
                      if (userType == UserType.Organisation) Left(sample[TrustName])
                      else Right(sample[IndividualName])
                  ),
                  amendReturnData = None
                )
              )
            )

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(sessionData)
            }

            checkPageIsDisplayed(
              performAction(),
              messageFromMessageKey("nonCalculatedTaxDue.furtherReturn.checkTaxDue.title"),
              { doc =>
                val formattedTaxDue = MoneyUtils.formatAmountOfMoneyWithPoundSign(taxDue.inPounds())

                doc.select("#content > article > dl > div:nth-child(1) > dd").text() shouldBe MoneyUtils
                  .formatAmountOfMoneyWithPoundSign(yearToDateLiability.inPounds())

                doc.select("#content > article > dl > div:nth-child(2) > dd").text() shouldBe s"- ${MoneyUtils
                  .formatAmountOfMoneyWithPoundSign(previousYearToDateLiability.inPounds())}"

                doc.select("#content > article > dl > div.sum-total > dd").text() shouldBe s"= $formattedTaxDue"

                doc.select("#content > article > p:nth-child(6)").text() shouldBe messageFromMessageKey(
                  expectedP1Key,
                  formattedTaxDue
                )

                doc.select("#content > article > form").attr("action") shouldBe routes.YearToDateLiabilityController
                  .nonCalculatedEnterTaxDueSubmit()
                  .url
              }
            )
          }

          "the user is an individual" in {
            testCheckTaxDuePage(
              AmountInPence(1000L),
              AmountInPence(3000L),
              AmountInPence.zero,
              UserType.Individual,
              "nonCalculatedTaxDue.furtherReturn.checkTaxDue.p1"
            )

          }

          "the user is an agent" in {
            testCheckTaxDuePage(
              AmountInPence(3000L),
              AmountInPence(1000L),
              AmountInPence(2000L),
              UserType.Agent,
              "nonCalculatedTaxDue.furtherReturn.checkTaxDue.agent.p1"
            )
          }

          "the user is a trust" in {
            testCheckTaxDuePage(
              AmountInPence(1000L),
              AmountInPence(1000L),
              AmountInPence.zero,
              UserType.Organisation,
              "nonCalculatedTaxDue.furtherReturn.checkTaxDue.trust.p1"
            )
          }

        }

        "the return is a further return where the previous year to date is not present" when {

          def testEnterTaxDuePage(
            yearToDateLiability: AmountInPence,
            userType: UserType,
            individualUserType: IndividualUserType,
            expectedP1Key: String
          ): Unit = {
            val sessionData = SessionData.empty.copy(
              userType = Some(userType),
              journeyStatus = Some(
                sample[FillingOutReturn].copy(
                  draftReturn = sample[DraftSingleDisposalReturn].copy(
                    triageAnswers = sample[CompleteSingleDisposalTriageAnswers].copy(
                      individualUserType = Some(individualUserType)
                    ),
                    yearToDateLiabilityAnswers = Some(
                      sample[CompleteNonCalculatedYTDAnswers].copy(
                        yearToDateLiability = Some(yearToDateLiability)
                      )
                    ),
                    representeeAnswers = Some(sample[CompleteRepresenteeAnswers].copy(isFirstReturn = false))
                  ),
                  agentReferenceNumber = if (userType == UserType.Agent) Some(sample[AgentReferenceNumber]) else None,
                  previousSentReturns = Some(
                    sample[PreviousReturnData].copy(
                      summaries = List(sample[ReturnSummary]),
                      previousYearToDate = None
                    )
                  ),
                  subscribedDetails = sample[SubscribedDetails].copy(
                    name =
                      if (userType == UserType.Organisation) Left(sample[TrustName])
                      else Right(sample[IndividualName])
                  ),
                  amendReturnData = None
                )
              )
            )

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(sessionData)
            }

            checkPageIsDisplayed(
              performAction(),
              messageFromMessageKey("nonCalculatedTaxDue.furtherReturn.enterTaxDue.title"),
              { doc =>
                doc.select("#nonCalculatedTaxDue-form-hint") contains expectedP1Key
                doc.select("#content > article > form").attr("action") shouldBe routes.YearToDateLiabilityController
                  .nonCalculatedEnterTaxDueSubmit()
                  .url
              }
            )
          }

          "the user is an individual" in {
            testEnterTaxDuePage(
              AmountInPence(1000L),
              UserType.Individual,
              IndividualUserType.Self,
              "nonCalculatedTaxDue.furtherReturn.enterTaxDue.helpText.p1"
            )
          }

          "the user is an agent" in {
            testEnterTaxDuePage(
              AmountInPence(1000L),
              UserType.Agent,
              IndividualUserType.Self,
              "nonCalculatedTaxDue.furtherReturn.enterTaxDue.agent.helpText.p1"
            )
          }

          "the user is a trust" in {
            testEnterTaxDuePage(
              AmountInPence(1000L),
              UserType.Organisation,
              IndividualUserType.Self,
              "nonCalculatedTaxDue.furtherReturn.enterTaxDue.trust.helpText.p1"
            )
          }

          "the user is a capacitor" in {
            testEnterTaxDuePage(
              AmountInPence(1000L),
              UserType.Individual,
              IndividualUserType.Capacitor,
              "nonCalculatedTaxDue.furtherReturn.enterTaxDue.capacitor.helpText.p1"
            )
          }

          "the user is a personal rep" in {
            testEnterTaxDuePage(
              AmountInPence(3000L),
              UserType.Individual,
              IndividualUserType.PersonalRepresentative,
              "nonCalculatedTaxDue.furtherReturn.enterTaxDue.personalRep.helpText.p1"
            )
          }

          "the user is a personal rep in period of admin" in {
            testEnterTaxDuePage(
              AmountInPence(1000L),
              UserType.Individual,
              IndividualUserType.PersonalRepresentativeInPeriodOfAdmin,
              "nonCalculatedTaxDue.furtherReturn.enterTaxDue.personalRepInPeriodOfAdmin.helpText.p1"
            )
          }

        }

      }

    }

    "handling submits on the non calculated enter tax due page" must {

      def performAction(formData: (String, String)*): Future[Result] =
        controller.nonCalculatedEnterTaxDueSubmit()(
          FakeRequest().withFormUrlEncodedBody(formData: _*)
        )

      behave like redirectToStartBehaviour(() => performAction())

      behave like markUnmetDependencyBehaviour(controller.nonCalculatedEnterTaxDueSubmit())

      behave like redirectWhenNotNonCalculatedJourneyBehaviour(() => performAction())

      behave like noYearToDateLiabilityBehaviour(() => performAction())

      {
        val answers =
          IncompleteNonCalculatedYTDAnswers.empty.copy(
            taxableGainOrLoss = Some(AmountInPence.zero),
            hasEstimatedDetails = Some(true)
          )

        val (_, journey, draftReturn) = sessionWithMultipleDisposalsState(
          answers,
          UserType.Individual,
          wasUkResident = true,
          isFurtherReturn = false,
          None
        )

        behave like unsuccessfulUpdateBehaviour(
          journey,
          journey.copy(draftReturn =
            draftReturn.copy(yearToDateLiabilityAnswers =
              Some(
                answers.copy(taxDue = Some(AmountInPence(100L)))
              )
            )
          ),
          () =>
            performAction(
              "nonCalculatedTaxDue" -> "1"
            )
        )
      }

      "show a form error" when {

        val currentSession = sessionWithMultipleDisposalsState(
          sample[CompleteNonCalculatedYTDAnswers],
          UserType.Individual,
          wasUkResident = true,
          isFurtherReturn = false,
          None
        )._1

        def test(data: (String, String)*)(expectedErrorKey: String): Unit =
          testFormError(data: _*)(
            expectedErrorKey
          )("nonCalculatedTaxDue.title")(performAction, currentSession)

        "the value submitted is invalid" in {
          AmountOfMoneyErrorScenarios
            .amountOfMoneyErrorScenarios("nonCalculatedTaxDue")
            .foreach { scenario =>
              withClue(s"For $scenario: ") {
                test(scenario.formData: _*)(scenario.expectedErrorMessageKey)
              }
            }
        }

      }

      "redirect to the check your answers page" when {

        "the user is on a further return journey where a previous year to date value is available" in {
          val yearToDateLiability         = AmountInPence(10L)
          val previousYearToDateLiability = AmountInPence(9L)
          val taxDue                      = AmountInPence(1L)
          val answers                     = sample[CompleteNonCalculatedYTDAnswers].copy(
            yearToDateLiability = Some(yearToDateLiability)
          )
          val newAnswers                  = IncompleteNonCalculatedYTDAnswers(
            Some(answers.taxableGainOrLoss),
            Some(answers.hasEstimatedDetails),
            Some(taxDue),
            None,
            None,
            None,
            answers.yearToDateLiability,
            None
          )
          val draftReturn                 = sample[DraftSingleDisposalReturn].copy(
            triageAnswers = sample[CompleteSingleDisposalTriageAnswers].copy(individualUserType = Some(Self)),
            yearToDateLiabilityAnswers = Some(answers),
            gainOrLossAfterReliefs = None
          )
          val journey                     = sample[FillingOutReturn].copy(
            draftReturn = draftReturn,
            previousSentReturns = Some(
              sample[PreviousReturnData].copy(
                summaries = List(sample[ReturnSummary]),
                previousYearToDate = Some(previousYearToDateLiability)
              )
            ),
            subscribedDetails = sample[SubscribedDetails].copy(name = Right(sample[IndividualName])),
            amendReturnData = None
          )
          val newJourney                  = journey.copy(
            draftReturn = draftReturn.copy(
              yearToDateLiabilityAnswers = Some(newAnswers),
              gainOrLossAfterReliefs = None
            )
          )

          val sessionData = SessionData.empty.copy(journeyStatus = Some(journey))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionData)
            mockStoreDraftReturn(newJourney)(Right(()))
            mockStoreSession(
              sessionData.copy(journeyStatus = Some(newJourney))
            )(Right(()))
          }

          checkIsRedirect(
            performAction(),
            routes.YearToDateLiabilityController.checkYourAnswers()
          )
        }

        "the user is on a further return journey where a previous year to date value is not available" in {
          val yearToDateLiability = AmountInPence(10L)
          val taxDue              = AmountInPence(1050L)
          val answers             = sample[CompleteNonCalculatedYTDAnswers].copy(
            yearToDateLiability = Some(yearToDateLiability)
          )
          val newAnswers          = IncompleteNonCalculatedYTDAnswers(
            Some(answers.taxableGainOrLoss),
            Some(answers.hasEstimatedDetails),
            Some(taxDue),
            None,
            None,
            None,
            answers.yearToDateLiability,
            answers.checkForRepayment
          )
          val draftReturn         = sample[DraftSingleDisposalReturn].copy(
            triageAnswers = sample[CompleteSingleDisposalTriageAnswers].copy(
              individualUserType = Some(PersonalRepresentativeInPeriodOfAdmin)
            ),
            yearToDateLiabilityAnswers = Some(answers),
            representeeAnswers = Some(sample[CompleteRepresenteeAnswers].copy(isFirstReturn = false))
          )
          val journey             = sample[FillingOutReturn].copy(
            draftReturn = draftReturn,
            previousSentReturns = Some(
              sample[PreviousReturnData].copy(
                summaries = List(sample[ReturnSummary]),
                previousYearToDate = None
              )
            ),
            subscribedDetails = sample[SubscribedDetails].copy(name = Right(sample[IndividualName])),
            amendReturnData = None
          )
          val newJourney          = journey.copy(
            draftReturn = draftReturn.copy(
              yearToDateLiabilityAnswers = Some(newAnswers)
            )
          )

          val sessionData = SessionData.empty.copy(journeyStatus = Some(journey))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionData)
            mockStoreDraftReturn(newJourney)(Right(()))
            mockStoreSession(
              sessionData.copy(journeyStatus = Some(newJourney))
            )(Right(()))
          }

          checkIsRedirect(
            performAction(
              "nonCalculatedTaxDue" -> "10.50"
            ),
            routes.YearToDateLiabilityController.checkYourAnswers()
          )
        }

        "all updates are successful and" when {

          "the section had been started but not completed" in {
            val newAmount = AmountInPence(101L)
            val answers   = IncompleteNonCalculatedYTDAnswers.empty.copy(
              taxableGainOrLoss = Some(AmountInPence(2L)),
              yearToDateLiability = Some(AmountInPence(1L)),
              hasEstimatedDetails = Some(true)
            )
            testSuccessfulUpdatesAfterSubmitWithSingleDisposal(
              performAction(
                "nonCalculatedTaxDue" -> "1.01"
              ),
              answers,
              answers.copy(taxDue = Some(newAmount)),
              sample[CompleteReliefDetailsAnswers].copy(otherReliefs = Some(sample[OtherReliefsOption.OtherReliefs]))
            )
          }

          "the section was complete" in {
            val newAmount  = AmountInPence(0L)
            val answers    =
              sample[CompleteNonCalculatedYTDAnswers]
                .copy(
                  taxDue = AmountInPence(1L)
                )
            val newAnswers = IncompleteNonCalculatedYTDAnswers(
              Some(answers.taxableGainOrLoss),
              Some(answers.hasEstimatedDetails),
              Some(newAmount),
              None,
              None,
              None,
              answers.yearToDateLiability,
              answers.checkForRepayment
            )
            testSuccessfulUpdatesAfterSubmitWithMultipleDisposals(
              performAction(
                "nonCalculatedTaxDue" -> "0"
              ),
              answers,
              newAnswers
            )
          }

        }

      }

      "not do any updates" when {

        "the answer supplied is the same as one already stored" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithMultipleDisposalsState(
                sample[CompleteNonCalculatedYTDAnswers]
                  .copy(taxDue = AmountInPence.zero),
                UserType.Individual,
                wasUkResident = true,
                isFurtherReturn = false,
                None
              )._1
            )
          }

          checkIsRedirect(
            performAction("nonCalculatedTaxDue" -> "0"),
            routes.YearToDateLiabilityController.checkYourAnswers()
          )
        }

      }

    }

    "handling requests to display the scanning mandatory evidence page" must {

      def performAction(): Future[Result] =
        controller.scanningMandatoryEvidence()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      behave like markUnmetDependencyBehaviour(controller.scanningMandatoryEvidence())

      behave like noPendingUploadbBehaviour(performAction)

      "show an error page" when {

        val upscanUpload = sample[UpscanUpload].copy(
          upscanCallBack = None
        )

        val answers                         = IncompleteNonCalculatedYTDAnswers(
          Some(sample[AmountInPence]),
          Some(sample[Boolean]),
          Some(sample[AmountInPence]),
          None,
          None,
          Some(upscanUpload),
          None,
          None
        )
        val (session, journey, draftReturn) =
          sessionWithMultipleDisposalsState(
            answers,
            sample[UserType],
            wasUkResident = sample[Boolean],
            isFurtherReturn = false,
            None
          )

        val callback       = sample[UpscanFailure]
        val newAnswers     = answers.copy(pendingUpscanUpload = Some(upscanUpload.copy(upscanCallBack = Some(callback))))
        val newDraftReturn =
          draftReturn.copy(yearToDateLiabilityAnswers = Some(newAnswers))
        val newJourney     = journey.copy(draftReturn = newDraftReturn)

        "there is an error getting the upscan upload" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockGetUpscanUpload(upscanUpload.uploadReference)(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction())
        }

        "there is an error updating the draft return with a callback" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockGetUpscanUpload(upscanUpload.uploadReference)(
              Right(upscanUpload.copy(upscanCallBack = Some(callback)))
            )
            mockStoreDraftReturn(newJourney)(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction())
        }

        "there is an error updating the session with a callback" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockGetUpscanUpload(upscanUpload.uploadReference)(
              Right(
                upscanUpload.copy(
                  upscanCallBack = Some(callback)
                )
              )
            )
            mockStoreDraftReturn(newJourney)(Right(()))
            mockStoreSession(session.copy(journeyStatus = Some(newJourney)))(
              Left(Error(""))
            )
          }

          checkIsTechnicalErrorPage(performAction())
        }

      }

      "show that the scan is still in progress" when {

        "there is no callback yet" in {
          val upscanUpload = sample[UpscanUpload].copy(upscanCallBack = None)

          val answers = sample[IncompleteCalculatedYTDAnswers].copy(
            pendingUpscanUpload = Some(upscanUpload)
          )

          val session = sessionWithSingleDisposalState(
            answers,
            sample[DisposalDate],
            sample[UserType],
            wasUkResident = sample[Boolean]
          )._1

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockGetUpscanUpload(upscanUpload.uploadReference)(
              Right(upscanUpload)
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(
              "mandatoryEvidence.scan-progress.title"
            ),
            { doc =>
              doc
                .select("#content > article > p:nth-child(3)")
                .text() shouldBe messageFromMessageKey(
                "mandatoryEvidence.scan-progress.p1"
              )
              doc
                .select("#content > article > p:nth-child(4)")
                .text() shouldBe messageFromMessageKey(
                "mandatoryEvidence.scan-progress.p2"
              )
            }
          )
        }

      }

      "show that the scan has failed" when {

        def checkPage(result: Future[Result]): Unit =
          checkPageIsDisplayed(
            result,
            messageFromMessageKey(
              "mandatoryEvidence.scan-failed.title"
            ),
            doc =>
              doc
                .select("#content > article > form")
                .attr("action") shouldBe routes.YearToDateLiabilityController
                .uploadMandatoryEvidence()
                .url
          )

        val upscanUpload = sample[UpscanUpload].copy(
          upscanCallBack = None
        )

        val answers                         = IncompleteNonCalculatedYTDAnswers(
          Some(sample[AmountInPence]),
          Some(sample[Boolean]),
          Some(sample[AmountInPence]),
          None,
          None,
          Some(upscanUpload),
          None,
          None
        )
        val (session, journey, draftReturn) =
          sessionWithMultipleDisposalsState(
            answers,
            sample[UserType],
            wasUkResident = sample[Boolean],
            isFurtherReturn = false,
            None
          )

        val callback       = sample[UpscanFailure]
        val newAnswers     = answers.copy(pendingUpscanUpload = Some(upscanUpload.copy(upscanCallBack = Some(callback))))
        val newDraftReturn =
          draftReturn.copy(yearToDateLiabilityAnswers = Some(newAnswers))
        val newJourney     = journey.copy(draftReturn = newDraftReturn)
        val updatedSession = session.copy(journeyStatus = Some(newJourney))

        "the callback indicates that the scan has failed and all updates are successful" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockGetUpscanUpload(upscanUpload.uploadReference)(
              Right(upscanUpload.copy(upscanCallBack = Some(callback)))
            )
            mockStoreDraftReturn(newJourney)(Right(()))
            mockStoreSession(updatedSession)(Right(()))
          }

          checkPage(performAction())
        }

        "the session indicates that the scan has already failed" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(updatedSession)
          }

          checkPage(performAction())

        }

      }

      "redirect to the check your answers page" when {
        val upscanUpload = sample[UpscanUpload].copy(
          upscanCallBack = None
        )

        val answers = IncompleteNonCalculatedYTDAnswers(
          Some(sample[AmountInPence]),
          Some(sample[Boolean]),
          Some(sample[AmountInPence]),
          None,
          None,
          Some(upscanUpload),
          None,
          None
        )

        val (session, journey, draftReturn) =
          sessionWithSingleDisposalState(
            answers,
            sample[DisposalDate],
            sample[UserType],
            wasUkResident = sample[Boolean]
          )

        val fileName       = "file"
        val callback       = sample[UpscanSuccess]
          .copy(uploadDetails = Map("fileName" -> fileName))

        val newAnswers     = answers.copy(
          pendingUpscanUpload = None,
          mandatoryEvidence = Some(
            MandatoryEvidence(
              upscanUpload.uploadReference,
              upscanUpload.upscanUploadMeta,
              upscanUpload.uploadedOn,
              callback,
              fileName
            )
          )
        )
        val newDraftReturn =
          draftReturn.copy(yearToDateLiabilityAnswers = Some(newAnswers))
        val newJourney     = journey.copy(draftReturn = newDraftReturn)

        val updatedSession = session.copy(journeyStatus = Some(newJourney))

        "the upscan callback comes back as a success" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockGetUpscanUpload(upscanUpload.uploadReference)(
              Right(upscanUpload.copy(upscanCallBack = Some(callback)))
            )
            mockStoreDraftReturn(newJourney)(Right(()))
            mockStoreSession(updatedSession)(Right(()))
          }

          checkIsRedirect(
            performAction(),
            routes.YearToDateLiabilityController.checkYourAnswers()
          )
        }

        "the session indicates that the scan has already failed" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(updatedSession)
          }

          checkIsRedirect(
            performAction(),
            routes.YearToDateLiabilityController.checkYourAnswers()
          )
        }

      }

    }

    "handling submits on the scanning mandatory evidence page" must {

      "redirect to the scanning mandatory evidence page" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData.empty)
        }

        checkIsRedirect(
          controller.scanningMandatoryEvidenceSubmit()(FakeRequest()),
          routes.YearToDateLiabilityController.scanningMandatoryEvidence()
        )
      }
    }

    "handling requests to display the upload mandatory evidence failure page" must {

      def performAction(): Future[Result] =
        controller.uploadMandatoryEvidenceFailure()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      behave like markUnmetDependencyBehaviour(controller.uploadMandatoryEvidenceFailure())

      behave like noPendingUploadbBehaviour(performAction)

      "show an error page" when {
        val answers = sample[IncompleteCalculatedYTDAnswers]
          .copy(pendingUpscanUpload = Some(sample[UpscanUpload]))

        val (session, journey, draftReturn) = sessionWithSingleDisposalState(
          answers,
          sample[DisposalDate],
          sample[UserType],
          wasUkResident = sample[Boolean]
        )

        val newAnswers     = answers.copy(pendingUpscanUpload = None)
        val newDraftReturn =
          draftReturn.copy(yearToDateLiabilityAnswers = Some(newAnswers))
        val newJourney     = journey.copy(draftReturn = newDraftReturn)

        "there is an error updating the draft return" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(newJourney)(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction())

        }

        "there is an error updating the session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(newJourney)(Right(()))
            mockStoreSession(session.copy(journeyStatus = Some(newJourney)))(
              Left(Error(""))
            )
          }

          checkIsTechnicalErrorPage(performAction())
        }

      }

      "display the upscan failed page" when {

        "all update are successful and" when {

          "the user is in on a single disposal journey" in {
            val answers = sample[IncompleteCalculatedYTDAnswers]
              .copy(pendingUpscanUpload = Some(sample[UpscanUpload]))

            val (session, journey, draftReturn) =
              sessionWithSingleDisposalState(
                answers,
                sample[DisposalDate],
                sample[UserType],
                wasUkResident = sample[Boolean]
              )

            val newAnswers     = answers.copy(pendingUpscanUpload = None)
            val newDraftReturn =
              draftReturn.copy(yearToDateLiabilityAnswers = Some(newAnswers))
            val newJourney     = journey.copy(draftReturn = newDraftReturn)

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockStoreDraftReturn(newJourney)(Right(()))
              mockStoreSession(session.copy(journeyStatus = Some(newJourney)))(
                Right(())
              )
            }
            checkIsRedirect(
              performAction(),
              routes.YearToDateLiabilityController.documentDidNotUpload()
            )
          }

          "the user is in on a single indirect disposal journey" in {
            val answers =
              sample[IncompleteNonCalculatedYTDAnswers]
                .copy(pendingUpscanUpload = Some(sample[UpscanUpload]))

            val (session, journey, draftReturn) =
              sessionWithSingleIndirectDisposalState(
                answers,
                sample[DisposalDate],
                sample[UserType],
                wasUkResident = sample[Boolean]
              )

            val newAnswers     = answers.copy(pendingUpscanUpload = None)
            val newDraftReturn =
              draftReturn.copy(yearToDateLiabilityAnswers = Some(newAnswers))
            val newJourney     = journey.copy(draftReturn = newDraftReturn)

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockStoreDraftReturn(newJourney)(Right(()))
              mockStoreSession(session.copy(journeyStatus = Some(newJourney)))(
                Right(())
              )
            }

            checkIsRedirect(
              performAction(),
              routes.YearToDateLiabilityController.documentDidNotUpload()
            )
          }

          "the user is in on a single mixed use disposal journey" in {
            val answers =
              sample[IncompleteNonCalculatedYTDAnswers]
                .copy(pendingUpscanUpload = Some(sample[UpscanUpload]))

            val (session, journey, draftReturn) =
              sessionWithSingleMixedUseDisposalState(
                answers,
                sample[DisposalDate],
                sample[UserType],
                wasUkResident = sample[Boolean]
              )

            val newAnswers     = answers.copy(pendingUpscanUpload = None)
            val newDraftReturn =
              draftReturn.copy(yearToDateLiabilityAnswers = Some(newAnswers))
            val newJourney     = journey.copy(draftReturn = newDraftReturn)

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockStoreDraftReturn(newJourney)(Right(()))
              mockStoreSession(session.copy(journeyStatus = Some(newJourney)))(
                Right(())
              )
            }

            checkIsRedirect(
              performAction(),
              routes.YearToDateLiabilityController.documentDidNotUpload()
            )
          }

          "the user is on a multiple disposal journey" in {
            val answers =
              sample[IncompleteNonCalculatedYTDAnswers]
                .copy(pendingUpscanUpload = Some(sample[UpscanUpload]))

            val (session, journey, draftReturn) =
              sessionWithMultipleDisposalsState(
                answers,
                sample[UserType],
                wasUkResident = sample[Boolean],
                isFurtherReturn = false,
                None
              )

            val newAnswers     = answers.copy(pendingUpscanUpload = None)
            val newDraftReturn =
              draftReturn.copy(yearToDateLiabilityAnswers = Some(newAnswers))
            val newJourney     = journey.copy(draftReturn = newDraftReturn)

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockStoreDraftReturn(newJourney)(Right(()))
              mockStoreSession(session.copy(journeyStatus = Some(newJourney)))(
                Right(())
              )
            }

            checkIsRedirect(
              performAction(),
              routes.YearToDateLiabilityController.documentDidNotUpload()
            )

          }

        }

      }

    }

    "handling requests to display the mandatory evidence expired page" must {

      def performAction(): Future[Result] =
        controller.mandatoryEvidenceExpired()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      behave like markUnmetDependencyBehaviour(controller.mandatoryEvidenceExpired())

      "redirect to the check your answers page" when {

        def test(answers: YearToDateLiabilityAnswers): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithSingleDisposalState(
                answers,
                sample[DisposalDate],
                sample[UserType],
                wasUkResident = sample[Boolean]
              )._1
            )
          }

          checkIsRedirect(
            performAction(),
            routes.YearToDateLiabilityController.checkYourAnswers()
          )
        }

        "there is no expired evidence when the journey is incomplete and" when {

          "the user is on an calculated journey" in {
            test(
              sample[IncompleteCalculatedYTDAnswers]
                .copy(expiredEvidence = None)
            )
          }

          "the user is on a non-calulated jouney" in {
            test(
              sample[IncompleteCalculatedYTDAnswers]
                .copy(expiredEvidence = None)
            )
          }

        }

        "the user has completed the section on a calculated journey" in {
          test(sample[CompleteCalculatedYTDAnswers])
        }

        "the user has completed the section on a non-calculated journey" in {
          test(sample[CompleteNonCalculatedYTDAnswers])
        }

      }

      "display the page" when {
        val expiredEvidence = sample[MandatoryEvidence]

        def checkPage(result: Future[Result]) =
          checkPageIsDisplayed(
            result,
            messageFromMessageKey("mandatoryEvidenceExpired.title"),
            doc => {
              doc
                .select("#content > article > p")
                .get(0)
                .text()       shouldBe messageFromMessageKey("mandatoryEvidenceExpired.p1")
              doc
                .select("#content > article > p")
                .get(1)
                .text()       shouldBe messageFromMessageKey("mandatoryEvidenceExpired.p2")
              doc
                .select("#content > article > a")
                .text()       shouldBe messageFromMessageKey("mandatoryEvidenceExpired.button.text")
              doc
                .select("#content > article > a")
                .attr("href") shouldBe routes.YearToDateLiabilityController.uploadMandatoryEvidence().url
              doc
                .select("#content > article > dl > div > dt")
                .text()       shouldBe expiredEvidence.fileName
              doc
                .select("#content > article > dl > div > dd:eq(2)")
                .text()       shouldBe messageFromMessageKey("mandatoryEvidenceExpired.label")
            }
          )

        "the user is on a calculated journey" in {
          val session = sessionWithSingleDisposalState(
            sample[IncompleteCalculatedYTDAnswers]
              .copy(expiredEvidence = Some(expiredEvidence)),
            sample[DisposalDate],
            sample[UserType],
            wasUkResident = sample[Boolean]
          )._1

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkPage(performAction())
        }

        "the user is on a non-calculated journey" in {
          val session = sessionWithMultipleDisposalsState(
            sample[IncompleteNonCalculatedYTDAnswers]
              .copy(expiredEvidence = Some(expiredEvidence)),
            sample[UserType],
            wasUkResident = sample[Boolean],
            isFurtherReturn = false,
            None
          )._1

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkPage(performAction())
        }

      }

    }

    "handling requests to display the year to date liability page" must {

      def performAction(): Future[Result] = controller.yearToDateLiability()(FakeRequest())

      val requiredPreviousAnswers = IncompleteNonCalculatedYTDAnswers(
        Some(sample[AmountInPence]),
        Some(true),
        None,
        None,
        None,
        None,
        None,
        None
      )

      behave like redirectToStartBehaviour(() => performAction())

      behave like markUnmetDependencyBehaviour(controller.yearToDateLiability())

      "redirect to the check your answers page" when {

        "the return is not a first return" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithMultipleDisposalsState(
                sample[CompleteNonCalculatedYTDAnswers],
                UserType.Individual,
                wasUkResident = true,
                isFurtherReturn = false,
                None
              )._1
            )
          }

          checkIsRedirect(performAction(), routes.YearToDateLiabilityController.checkYourAnswers())
        }

      }

      "redirect to the estimates page if the user has not answered that question yet" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            sessionWithSingleDisposalState(
              Some(requiredPreviousAnswers.copy(hasEstimatedDetails = None)),
              Some(sample[DisposalDate].copy(taxYear = sample[TaxYear])),
              UserType.Individual,
              wasUkResident = true,
              Some(
                sample[CompleteReliefDetailsAnswers].copy(
                  otherReliefs = Some(OtherReliefsOption.NoOtherReliefs)
                )
              ),
              isFurtherReturn = true,
              individualUserType = Some(Self)
            )._1
          )
        }

        checkIsRedirect(performAction(), routes.YearToDateLiabilityController.hasEstimatedDetails())
      }

      "redirect to the taxable gain or loss page if the user has not answered that question yet and the estimates question should be " +
        "preserved for an amend journey" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            sessionWithSingleDisposalState(
              Some(requiredPreviousAnswers.copy(taxableGainOrLoss = None)),
              Some(sample[DisposalDate].copy(taxYear = sample[TaxYear])),
              UserType.Individual,
              wasUkResident = true,
              Some(
                sample[CompleteReliefDetailsAnswers].copy(
                  otherReliefs = Some(OtherReliefsOption.NoOtherReliefs)
                )
              ),
              isFurtherReturn = true,
              individualUserType = Some(Self),
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
            )._1
          )
        }

        checkIsRedirect(performAction(), routes.YearToDateLiabilityController.taxableGainOrLoss())
      }

      "display the page" when {

        def testFurtherReturnPage(
          session: SessionData,
          taxYear: TaxYear,
          expectedBackLink: Call,
          expectedTitleKey: String,
          expectedP1Message: String,
          expectedLi3Key: Option[String],
          expectedSubtitleKey: String,
          expectedLinkKey: String
        ): Unit = {
          val (taxYearStart, taxYearEnd) =
            taxYear.startDateInclusive.getYear.toString -> taxYear.endDateExclusive.getYear.toString

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(expectedTitleKey, taxYearStart, taxYearEnd),
            { doc =>
              doc.select("#back").attr("href") shouldBe expectedBackLink.url

              doc.select("#content > article > p:nth-child(4)").html() shouldBe expectedP1Message

              doc.select("#content > article > ol > li:nth-child(3)").text() shouldBe expectedLi3Key
                .map(messageFromMessageKey(_))
                .getOrElse("")

              doc
                .select("#subheading")
                .text()                  shouldBe messageFromMessageKey(
                expectedSubtitleKey,
                taxYearStart,
                taxYearEnd
              )
              doc.select("#link").text() shouldBe messageFromMessageKey(expectedLinkKey)
            }
          )
        }

        val taxYear = sample[TaxYear]

        "they are completing the return for themselves" in {
          testFurtherReturnPage(
            sessionWithSingleDisposalState(
              Some(requiredPreviousAnswers),
              Some(sample[DisposalDate].copy(taxYear = taxYear)),
              UserType.Individual,
              wasUkResident = true,
              Some(
                sample[CompleteReliefDetailsAnswers].copy(
                  otherReliefs = Some(OtherReliefsOption.NoOtherReliefs)
                )
              ),
              isFurtherReturn = true,
              individualUserType = Some(Self)
            )._1,
            taxYear,
            routes.YearToDateLiabilityController.hasEstimatedDetails(),
            "yearToDateLiability.title",
            messageFromMessageKey("yearToDateLiability.p1", viewConfig.cgtRatesUrl),
            Some("yearToDateLiability.li3"),
            "yearToDateLiability.h2",
            "yearToDateLiability.link"
          )
        }

        "they are an agent" in {
          testFurtherReturnPage(
            sessionWithMultipleDisposalsState(
              Some(requiredPreviousAnswers),
              UserType.Agent,
              wasUkResident = true,
              Some(Self),
              isFurtherReturn = true,
              taxYear
            )._1,
            taxYear,
            routes.YearToDateLiabilityController.hasEstimatedDetails(),
            "yearToDateLiability.agent.title",
            messageFromMessageKey("yearToDateLiability.p1", viewConfig.cgtRatesUrl),
            Some("yearToDateLiability.agent.li3"),
            "yearToDateLiability.agent.h2",
            "yearToDateLiability.agent.link"
          )
        }

        "they are a trust" in {
          testFurtherReturnPage(
            sessionWithSingleIndirectDisposalState(
              Some(requiredPreviousAnswers),
              Some(sample[DisposalDate].copy(taxYear = taxYear)),
              UserType.Organisation,
              wasUkResident = true,
              isFurtherReturn = true,
              individualUserType = None
            )._1,
            taxYear,
            routes.YearToDateLiabilityController.hasEstimatedDetails(),
            "yearToDateLiability.trust.title",
            messageFromMessageKey("yearToDateLiability.trust.p1", viewConfig.trustsAndCgtUrl),
            None,
            "yearToDateLiability.trust.h2",
            "yearToDateLiability.trust.link"
          )
        }

        "they are a capacitor" in {
          testFurtherReturnPage(
            sessionWithSingleDisposalState(
              Some(requiredPreviousAnswers),
              Some(sample[DisposalDate].copy(taxYear = taxYear)),
              UserType.Individual,
              wasUkResident = true,
              Some(
                sample[CompleteReliefDetailsAnswers].copy(
                  otherReliefs = Some(OtherReliefsOption.NoOtherReliefs)
                )
              ),
              isFurtherReturn = true,
              individualUserType = Some(Capacitor)
            )._1,
            taxYear,
            routes.YearToDateLiabilityController.hasEstimatedDetails(),
            "yearToDateLiability.capacitor.title",
            messageFromMessageKey("yearToDateLiability.p1", viewConfig.cgtRatesUrl),
            Some("yearToDateLiability.capacitor.li3"),
            "yearToDateLiability.capacitor.h2",
            "yearToDateLiability.capacitor.link"
          )
        }

        "they are a personal rep" in {
          testFurtherReturnPage(
            sessionWithSingleDisposalState(
              Some(sample[CompleteNonCalculatedYTDAnswers]),
              Some(sample[DisposalDate].copy(taxYear = taxYear)),
              UserType.Individual,
              wasUkResident = true,
              Some(
                sample[CompleteReliefDetailsAnswers].copy(
                  otherReliefs = Some(OtherReliefsOption.NoOtherReliefs)
                )
              ),
              isFurtherReturn = true,
              individualUserType = Some(PersonalRepresentative)
            )._1,
            taxYear,
            routes.YearToDateLiabilityController.checkYourAnswers(),
            "yearToDateLiability.personalRep.title",
            messageFromMessageKey("yearToDateLiability.p1", viewConfig.cgtRatesUrl),
            Some("yearToDateLiability.personalRep.li3"),
            "yearToDateLiability.personalRep.h2",
            "yearToDateLiability.personalRep.link"
          )
        }

        "they are a personal rep in a period of admin" in {
          testFurtherReturnPage(
            sessionWithSingleDisposalState(
              Some(sample[CompleteNonCalculatedYTDAnswers]),
              Some(sample[DisposalDate].copy(taxYear = taxYear)),
              UserType.Individual,
              wasUkResident = true,
              Some(
                sample[CompleteReliefDetailsAnswers].copy(
                  otherReliefs = Some(OtherReliefsOption.NoOtherReliefs)
                )
              ),
              isFurtherReturn = true,
              individualUserType = Some(PersonalRepresentativeInPeriodOfAdmin)
            )._1,
            taxYear,
            routes.YearToDateLiabilityController.checkYourAnswers(),
            "yearToDateLiability.personalRepInPeriodOfAdmin.title",
            messageFromMessageKey("yearToDateLiability.personalRepInPeriodOfAdmin.p1", viewConfig.trustsAndCgtUrl),
            None,
            "yearToDateLiability.personalRepInPeriodOfAdmin.h2",
            "yearToDateLiability.personalRepInPeriodOfAdmin.link"
          )
        }

        "they are an agent of a personal rep in a period of admin" in {
          testFurtherReturnPage(
            sessionWithSingleDisposalState(
              Some(sample[CompleteNonCalculatedYTDAnswers]),
              Some(sample[DisposalDate].copy(taxYear = taxYear)),
              UserType.Agent,
              wasUkResident = true,
              Some(
                sample[CompleteReliefDetailsAnswers].copy(
                  otherReliefs = Some(OtherReliefsOption.NoOtherReliefs)
                )
              ),
              isFurtherReturn = true,
              individualUserType = Some(PersonalRepresentativeInPeriodOfAdmin)
            )._1,
            taxYear,
            routes.YearToDateLiabilityController.checkYourAnswers(),
            "yearToDateLiability.personalRepInPeriodOfAdmin.agent.title",
            messageFromMessageKey(
              "yearToDateLiability.personalRepInPeriodOfAdmin.agent.p1",
              viewConfig.trustsAndCgtUrl
            ),
            None,
            "yearToDateLiability.personalRepInPeriodOfAdmin.agent.h2",
            "yearToDateLiability.personalRepInPeriodOfAdmin.agent.link"
          )
        }

        "the user is on an amend journey where the estimates answer should be preserved" in {
          testFurtherReturnPage(
            sessionWithSingleDisposalState(
              Some(requiredPreviousAnswers),
              Some(sample[DisposalDate].copy(taxYear = taxYear)),
              UserType.Individual,
              wasUkResident = true,
              Some(
                sample[CompleteReliefDetailsAnswers].copy(
                  otherReliefs = Some(OtherReliefsOption.NoOtherReliefs)
                )
              ),
              isFurtherReturn = true,
              individualUserType = Some(Self),
              amendReturnData = Some(
                sample[AmendReturnData].copy(
                  originalReturn = sample[CompleteReturnWithSummary].copy(
                    completeReturn = sample[CompleteSingleMixedUseDisposalReturn].copy(
                      yearToDateLiabilityAnswers = sample[CompleteNonCalculatedYTDAnswers].copy(
                        hasEstimatedDetails = false
                      )
                    )
                  )
                )
              )
            )._1,
            taxYear,
            routes.YearToDateLiabilityController.taxableGainOrLoss(),
            "yearToDateLiability.title",
            messageFromMessageKey("yearToDateLiability.p1", viewConfig.cgtRatesUrl),
            Some("yearToDateLiability.li3"),
            "yearToDateLiability.h2",
            "yearToDateLiability.link"
          )
        }

      }

    }

    "handling submits on the year to date liability page" must {

      def performAction(formData: (String, String)*): Future[Result] =
        controller.yearToDateLiabilitySubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*))

      behave like redirectToStartBehaviour(() => performAction())

      behave like markUnmetDependencyBehaviour(controller.yearToDateLiabilitySubmit())

      {
        val answers = IncompleteNonCalculatedYTDAnswers.empty.copy(
          taxableGainOrLoss = Some(AmountInPence(101L)),
          hasEstimatedDetails = Some(false)
        )

        val (_, journey, draftReturn) = sessionWithMultipleDisposalsState(
          Some(answers),
          UserType.Individual,
          wasUkResident = true,
          isFurtherReturn = true
        )

        behave like unsuccessfulUpdateBehaviour(
          journey,
          journey.copy(draftReturn =
            draftReturn.copy(yearToDateLiabilityAnswers =
              Some(
                answers.copy(
                  yearToDateLiability = Some(AmountInPence(100000L))
                )
              )
            )
          ),
          () => performAction("yearToDateLiability" -> "1000")
        )
      }
      "redirect to the check your answers page" when {

        "the return is not a first return" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithMultipleDisposalsState(
                sample[CompleteNonCalculatedYTDAnswers],
                UserType.Individual,
                wasUkResident = true,
                isFurtherReturn = false,
                None
              )._1
            )
          }

          checkIsRedirect(performAction(), routes.YearToDateLiabilityController.checkYourAnswers())
        }

      }

      "show a form error" when {

        def test(sessionData: SessionData)(
          data: (String, String)*
        )(expectedTitleKey: String, expectedErrorKey: String, expectedErrorArgs: String*): Unit =
          testFormError(data: _*)(
            expectedErrorKey,
            expectedErrorArgs
          )(expectedTitleKey)(performAction, sessionData)

        val taxYear = sample[TaxYear]

        val (taxYearStart, taxYearEnd) =
          taxYear.startDateInclusive.getYear.toString -> taxYear.endDateExclusive.getYear.toString

        val testCases = {
          def session(userType: UserType, individualUserType: Option[IndividualUserType]) =
            sessionWithMultipleDisposalsState(
              Some(sample[CompleteNonCalculatedYTDAnswers]),
              userType,
              wasUkResident = true,
              individualUserType,
              isFurtherReturn = true,
              taxYear
            )._1

          List(
            ""                                  -> session(UserType.Individual, Some(Self)),
            ".agent"                            -> session(UserType.Agent, Some(Self)),
            ".trust"                            -> session(UserType.Organisation, None),
            ".capacitor"                        -> session(UserType.Individual, Some(Capacitor)),
            ".personalRep"                      -> session(UserType.Individual, Some(PersonalRepresentative)),
            ".personalRepInPeriodOfAdmin"       -> session(
              UserType.Individual,
              Some(PersonalRepresentativeInPeriodOfAdmin)
            ),
            ".personalRepInPeriodOfAdmin.agent" -> session(
              UserType.Agent,
              Some(PersonalRepresentativeInPeriodOfAdmin)
            )
          )
        }

        "nothing is submitted" in {
          testCases.foreach {
            case (userKey, session) =>
              withClue(s"For user key '$userKey': ") {
                test(session)()(
                  s"yearToDateLiability$userKey.title",
                  s"yearToDateLiability$userKey.error.required",
                  taxYearStart,
                  taxYearEnd
                )
              }
          }
        }

        "the amount submitted is invalid" in {
          testCases.foreach {
            case (userKey, session) =>
              AmountOfMoneyErrorScenarios
                .amountOfMoneyErrorScenarios("yearToDateLiability")
                .filter(s => s.input.filter(_.nonEmpty).isDefined)
                .foreach { scenario =>
                  withClue(s"For user key '$userKey' and $scenario: ") {
                    test(session)(scenario.formData: _*)(
                      s"yearToDateLiability$userKey.title",
                      scenario.expectedErrorMessageKey,
                      taxYearStart,
                      taxYearEnd
                    )
                  }
                }
          }
        }

      }

      "redirect to the cya endpoint" when {

        "all updates are successful" in {
          val submittedAnswer = "100"
          val answers         = sample[CompleteNonCalculatedYTDAnswers].copy(
            yearToDateLiability = Some(AmountInPence(1L)),
            taxDue = AmountInPence(10L)
          )
          val newAnswers      = IncompleteNonCalculatedYTDAnswers
            .fromCompleteAnswers(answers)
            .copy(
              yearToDateLiability = Some(AmountInPence(10000L)),
              taxDue = Some(AmountInPence(10L)),
              mandatoryEvidence = None
            )

          val (session, journey, draftReturn) = sessionWithSingleDisposalState(
            Some(answers),
            userType = UserType.Individual,
            wasUkResident = true,
            isFurtherReturn = true
          )

          val newDraftReturn = draftReturn.copy(yearToDateLiabilityAnswers = Some(newAnswers))
          val newJourney     = journey.copy(draftReturn = newDraftReturn)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(newJourney)(
              Right(())
            )
            mockStoreSession(session.copy(journeyStatus = Some(journey.copy(draftReturn = newDraftReturn))))(Right(()))
          }

          checkIsRedirect(
            performAction("yearToDateLiability" -> submittedAnswer),
            routes.YearToDateLiabilityController.checkYourAnswers()
          )

        }

      }

      "not do any updates" when {

        "the answer submitted is the same as the one held in session" in {
          val submittedAnswer = "0"
          val answers         = sample[CompleteNonCalculatedYTDAnswers].copy(
            yearToDateLiability = Some(AmountInPence.zero)
          )

          val (session, _, _) = sessionWithSingleDisposalState(
            Some(answers),
            userType = UserType.Individual,
            wasUkResident = true,
            isFurtherReturn = true
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkIsRedirect(
            performAction("yearToDateLiability" -> submittedAnswer),
            routes.YearToDateLiabilityController.checkYourAnswers()
          )
        }

      }

    }

    "handling requests to display the repayment page" must {

      def performAction(): Future[Result] = controller.repayment()(FakeRequest())

      behave like redirectToStartBehaviour(() => performAction())

      behave like markUnmetDependencyBehaviour(controller.repayment())

      "redirect to the check your answers page" when {

        "the return is not a first return" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithMultipleDisposalsState(
                sample[CompleteNonCalculatedYTDAnswers],
                UserType.Individual,
                wasUkResident = true,
                isFurtherReturn = false,
                None
              )._1
            )
          }

          checkIsRedirect(performAction(), routes.YearToDateLiabilityController.checkYourAnswers())
        }

      }

      "display the page" when {

        def test(
          session: SessionData,
          expectedBackLink: Call,
          expectedTitleKey: String,
          expectedHelpTextKey: String
        ): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(expectedTitleKey),
            { doc =>
              doc.select("#back").attr("href") shouldBe expectedBackLink.url

              doc.select("#repayment-form-hint").text shouldBe messageFromMessageKey(expectedHelpTextKey)

              doc
                .select("#content > article > form")
                .attr("action") shouldBe routes.YearToDateLiabilityController
                .repaymentSubmit()
                .url
            }
          )
        }

        val requiredPreviousAnswers = IncompleteNonCalculatedYTDAnswers(
          Some(sample[AmountInPence]),
          Some(true),
          Some(sample[AmountInPence]),
          None,
          None,
          None,
          Some(sample[AmountInPence]),
          None
        )

        "they are completing the return for themselves" in {
          test(
            sessionWithSingleDisposalState(
              Some(requiredPreviousAnswers),
              Some(sample[DisposalDate]),
              UserType.Individual,
              wasUkResident = true,
              Some(
                sample[CompleteReliefDetailsAnswers].copy(
                  otherReliefs = Some(OtherReliefsOption.NoOtherReliefs)
                )
              ),
              isFurtherReturn = true,
              individualUserType = Some(Self)
            )._1,
            routes.YearToDateLiabilityController.nonCalculatedEnterTaxDue(),
            "repayment.title",
            "repayment.helpText"
          )
        }

        "they are an agent" in {
          test(
            sessionWithMultipleDisposalsState(
              Some(requiredPreviousAnswers),
              UserType.Agent,
              wasUkResident = true,
              Some(Self),
              isFurtherReturn = true
            )._1,
            routes.YearToDateLiabilityController.nonCalculatedEnterTaxDue(),
            "repayment.agent.title",
            "repayment.agent.helpText"
          )
        }

        "they are a trust" in {
          test(
            sessionWithSingleIndirectDisposalState(
              Some(requiredPreviousAnswers),
              Some(sample[DisposalDate]),
              UserType.Organisation,
              wasUkResident = true,
              isFurtherReturn = true,
              individualUserType = None
            )._1,
            routes.YearToDateLiabilityController.nonCalculatedEnterTaxDue(),
            "repayment.trust.title",
            "repayment.trust.helpText"
          )
        }

        "they are a capacitor" in {
          test(
            sessionWithSingleDisposalState(
              Some(requiredPreviousAnswers),
              Some(sample[DisposalDate]),
              UserType.Individual,
              wasUkResident = true,
              Some(
                sample[CompleteReliefDetailsAnswers].copy(
                  otherReliefs = Some(OtherReliefsOption.NoOtherReliefs)
                )
              ),
              isFurtherReturn = true,
              individualUserType = Some(Capacitor)
            )._1,
            routes.YearToDateLiabilityController.nonCalculatedEnterTaxDue(),
            "repayment.capacitor.title",
            "repayment.capacitor.helpText"
          )
        }

        "they are a personal rep" in {
          test(
            sessionWithSingleDisposalState(
              Some(sample[CompleteNonCalculatedYTDAnswers]),
              Some(sample[DisposalDate]),
              UserType.Individual,
              wasUkResident = true,
              Some(
                sample[CompleteReliefDetailsAnswers].copy(
                  otherReliefs = Some(OtherReliefsOption.NoOtherReliefs)
                )
              ),
              isFurtherReturn = true,
              individualUserType = Some(PersonalRepresentative)
            )._1,
            routes.YearToDateLiabilityController.checkYourAnswers(),
            "repayment.personalRep.title",
            "repayment.personalRep.helpText"
          )
        }

        "they are a personal rep in a period of admin" in {
          test(
            sessionWithSingleDisposalState(
              Some(sample[CompleteNonCalculatedYTDAnswers]),
              Some(sample[DisposalDate]),
              UserType.Individual,
              wasUkResident = true,
              Some(
                sample[CompleteReliefDetailsAnswers].copy(
                  otherReliefs = Some(OtherReliefsOption.NoOtherReliefs)
                )
              ),
              isFurtherReturn = true,
              individualUserType = Some(PersonalRepresentativeInPeriodOfAdmin)
            )._1,
            routes.YearToDateLiabilityController.checkYourAnswers(),
            "repayment.personalRepInPeriodOfAdmin.title",
            "repayment.personalRepInPeriodOfAdmin.helpText"
          )
        }

        "they are an agent of a personal rep in a period of admin" in {
          test(
            sessionWithSingleDisposalState(
              Some(sample[CompleteNonCalculatedYTDAnswers]),
              Some(sample[DisposalDate]),
              UserType.Agent,
              wasUkResident = true,
              Some(
                sample[CompleteReliefDetailsAnswers].copy(
                  otherReliefs = Some(OtherReliefsOption.NoOtherReliefs)
                )
              ),
              isFurtherReturn = true,
              individualUserType = Some(PersonalRepresentativeInPeriodOfAdmin)
            )._1,
            routes.YearToDateLiabilityController.checkYourAnswers(),
            "repayment.personalRepInPeriodOfAdmin.agent.title",
            "repayment.personalRepInPeriodOfAdmin.agent.helpText"
          )
        }

      }

    }

    "handling submits on the repayment page" must {

      def performAction(formData: (String, String)*): Future[Result] =
        controller.repaymentSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*))

      behave like redirectToStartBehaviour(() => performAction())

      behave like markUnmetDependencyBehaviour(controller.repaymentSubmit())

      {
        val answers = IncompleteNonCalculatedYTDAnswers.empty.copy(
          taxDue = Some(sample[AmountInPence])
        )

        val (_, journey, draftReturn) = sessionWithMultipleDisposalsState(
          Some(answers),
          UserType.Individual,
          wasUkResident = true,
          isFurtherReturn = true
        )

        behave like unsuccessfulUpdateBehaviour(
          journey,
          journey.copy(draftReturn =
            draftReturn.copy(yearToDateLiabilityAnswers =
              Some(
                answers.copy(
                  checkForRepayment = Some(true)
                )
              )
            )
          ),
          () => performAction("repayment" -> "true")
        )
      }

      "redirect to the check your answers page" must {

        "the return is not a first return" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithMultipleDisposalsState(
                sample[CompleteNonCalculatedYTDAnswers],
                UserType.Individual,
                wasUkResident = true,
                isFurtherReturn = false,
                None
              )._1
            )
          }

          checkIsRedirect(performAction(), routes.YearToDateLiabilityController.checkYourAnswers())
        }

      }

      "show a form error" when {

        def test(
          sessionData: SessionData
        )(
          data: (String, String)*
        )(expectedTitleKey: String, expectedErrorKey: String): Unit =
          testFormError(data: _*)(
            expectedErrorKey
          )(expectedTitleKey)(performAction, sessionData)

        val testCases = {
          def session(userType: UserType, individualUserType: Option[IndividualUserType]) =
            sessionWithMultipleDisposalsState(
              Some(sample[CompleteNonCalculatedYTDAnswers]),
              userType,
              wasUkResident = true,
              individualUserType,
              isFurtherReturn = true
            )._1

          List(
            ""                                  -> session(UserType.Individual, Some(Self)),
            ".agent"                            -> session(UserType.Agent, Some(Self)),
            ".trust"                            -> session(UserType.Organisation, None),
            ".capacitor"                        -> session(UserType.Individual, Some(Capacitor)),
            ".personalRep"                      -> session(UserType.Individual, Some(PersonalRepresentative)),
            ".personalRepInPeriodOfAdmin"       -> session(
              UserType.Individual,
              Some(PersonalRepresentativeInPeriodOfAdmin)
            ),
            ".personalRepInPeriodOfAdmin.agent" -> session(
              UserType.Agent,
              Some(PersonalRepresentativeInPeriodOfAdmin)
            )
          )
        }

        "nothing is submitted" in {
          testCases.foreach {
            case (userKey, session) =>
              withClue(s"For user key '$userKey': ") {
                test(session)()(
                  s"repayment$userKey.title",
                  s"repayment$userKey.error.required"
                )
              }
          }
        }

        "the value submitted is invalid" in {
          testCases.foreach {
            case (userKey, session) =>
              withClue(s"For user key '$userKey': ") {
                test(session)("repayment" -> "123")(
                  s"repayment$userKey.title",
                  s"repayment$userKey.error.boolean"
                )
              }
          }
        }

      }

      "redirect to the cya endpoint" when {

        "all updates are successful" in {
          val answers    = sample[CompleteNonCalculatedYTDAnswers].copy(
            checkForRepayment = Some(false)
          )
          val newAnswers = IncompleteNonCalculatedYTDAnswers
            .fromCompleteAnswers(answers)
            .copy(checkForRepayment = Some(true))

          val (session, journey, draftReturn) = sessionWithSingleDisposalState(
            Some(answers),
            userType = UserType.Individual,
            wasUkResident = true,
            isFurtherReturn = true
          )

          val newDraftReturn = draftReturn.copy(yearToDateLiabilityAnswers = Some(newAnswers))
          val newJourney     = journey.copy(draftReturn = newDraftReturn)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(newJourney)(Right(()))
            mockStoreSession(session.copy(journeyStatus = Some(journey.copy(draftReturn = newDraftReturn))))(Right(()))
          }

          checkIsRedirect(
            performAction("repayment" -> "true"),
            routes.YearToDateLiabilityController.checkYourAnswers()
          )

        }

      }

      "not do any updates" when {

        "the answer submitted is the same as the one held in session" in {
          val answers = sample[CompleteNonCalculatedYTDAnswers].copy(
            checkForRepayment = Some(false)
          )

          val (session, _, _) = sessionWithSingleDisposalState(
            Some(answers),
            userType = UserType.Individual,
            wasUkResident = true,
            isFurtherReturn = true
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkIsRedirect(
            performAction("repayment" -> "false"),
            routes.YearToDateLiabilityController.checkYourAnswers()
          )
        }

      }

    }
  }

  def noPendingUploadbBehaviour(performAction: () => Future[Result]): Unit =
    "redirect to the check your answers page" when {

      def test(answers: YearToDateLiabilityAnswers): Unit = {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            sessionWithSingleDisposalState(
              answers,
              sample[DisposalDate],
              sample[UserType],
              wasUkResident = sample[Boolean]
            )._1
          )
        }

        checkIsRedirect(
          performAction(),
          routes.YearToDateLiabilityController.checkYourAnswers()
        )
      }

      "there is no pending upscan upload when the journey is incomplete and" when {

        "the user is on an calculated journey" in {
          test(
            sample[IncompleteCalculatedYTDAnswers]
              .copy(pendingUpscanUpload = None)
          )
        }

        "the user is on a non-calulated jouney" in {
          test(
            sample[IncompleteCalculatedYTDAnswers]
              .copy(pendingUpscanUpload = None)
          )
        }

      }

      "the user has completed the section on a calculated journey" in {
        test(sample[CompleteCalculatedYTDAnswers])
      }

      "the user has completed the section on a non-calculated journey" in {
        test(sample[CompleteNonCalculatedYTDAnswers])

      }

    }

  def noDisposalDateBehaviour(performAction: () => Future[Result]): Unit =
    "redirect to the task list page" when {
      "no disposal date can be found" in {

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            sessionWithSingleDisposalState(
              Some(sample[CompleteCalculatedYTDAnswers]),
              None,
              UserType.Individual,
              wasUkResident = true
            )._1
          )
        }

        checkIsRedirect(
          performAction(),
          returns.routes.TaskListController.taskList()
        )
      }
    }

  def noEstimatedIncomeBehaviour(performAction: () => Future[Result]): Unit =
    "redirect to the check your answers page" when {
      "no estimated income can be found" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            sessionWithSingleDisposalState(
              sample[IncompleteCalculatedYTDAnswers].copy(
                estimatedIncome = None,
                personalAllowance = Some(sample[AmountInPence]),
                hasEstimatedDetails = Some(sample[Boolean])
              ),
              sample[DisposalDate],
              UserType.Individual,
              wasUkResident = true
            )._1
          )
        }

        checkIsRedirect(
          performAction(),
          routes.YearToDateLiabilityController.checkYourAnswers()
        )
      }

    }

  def noYearToDateLiabilityBehaviour(performAction: () => Future[Result]): Unit =
    "redirect to the check your answers page" when {
      "no year to date liability can be found for a furthe return" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            sessionWithSingleDisposalState(
              Some(
                IncompleteNonCalculatedYTDAnswers
                  .fromCompleteAnswers(sample[CompleteNonCalculatedYTDAnswers])
                  .copy(yearToDateLiability = None)
              ),
              Some(sample[DisposalDate]),
              UserType.Individual,
              wasUkResident = true,
              isFurtherReturn = true
            )._1
          )
        }

        checkIsRedirect(
          performAction(),
          routes.YearToDateLiabilityController.checkYourAnswers()
        )
      }

    }

  def incompleteOtherJourneysBehaviour(
    performAction: () => Future[Result]
  ): Unit = {
    val draftReturn = singleDisposalDraftReturnWithCompleteJourneys(
      Some(
        sample[CompleteCalculatedYTDAnswers]
          .copy(estimatedIncome = AmountInPence.zero, personalAllowance = None)
      ),
      sample[DisposalDate],
      completeReliefDetailsAnswersWithNoOtherReliefs
    )

    "redirect to the task list page" when {

      def test(draftReturn: DraftSingleDisposalReturn): Unit = {
        val session = SessionData.empty.copy(
          journeyStatus = Some(
            sample[FillingOutReturn].copy(draftReturn = draftReturn, amendReturnData = None)
          )
        )
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkIsRedirect(
          performAction(),
          returns.routes.TaskListController.taskList()
        )
      }

      "the triage section is incomplete" in {
        test(
          draftReturn
            .copy(triageAnswers = sample[IncompleteSingleDisposalTriageAnswers])
        )
      }

      "the disposal details hasn't been started yet" in {
        test(draftReturn.copy(disposalDetailsAnswers = None))
      }

      "the disposal details hasn't been completed yet" in {
        test(
          draftReturn.copy(disposalDetailsAnswers = Some(sample[IncompleteDisposalDetailsAnswers]))
        )
      }

      "the acquisition details hasn't been started yet" in {
        test(draftReturn.copy(acquisitionDetailsAnswers = None))
      }

      "the acquisition details hasn't been completed yet" in {
        test(
          draftReturn
            .copy(acquisitionDetailsAnswers = Some(sample[IncompleteAcquisitionDetailsAnswers]))
        )
      }

      "the relief details hasn't been started yet" in {
        test(draftReturn.copy(reliefDetailsAnswers = None))
      }

      "the relief details hasn't been completed yet" in {
        test(
          draftReturn.copy(reliefDetailsAnswers = Some(sample[IncompleteReliefDetailsAnswers]))
        )
      }

      "the exemptions and losses section hasn't been started yet" in {
        test(draftReturn.copy(exemptionAndLossesAnswers = None))
      }

      "the exemptions and losses section hasn't been completed yet" in {
        test(
          draftReturn
            .copy(exemptionAndLossesAnswers = Some(sample[IncompleteExemptionAndLossesAnswers]))
        )
      }

    }

  }

  def redirectWhenNotNonCalculatedJourneyBehaviour(
    performAction: () => Future[Result]
  ): Unit =
    "redirect to the check your answers endpoint" when {

      "the user has already started a calculated year to date liability journey" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            sessionWithSingleDisposalState(
              sample[CompleteCalculatedYTDAnswers],
              sample[DisposalDate],
              UserType.Individual,
              wasUkResident = true
            )._1
          )
        }

        checkIsRedirect(
          performAction(),
          routes.YearToDateLiabilityController.checkYourAnswers()
        )
      }

      "the user has not started this section yet but they are on a single disposal journey and have " +
        "not chosen other reliefs" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            sessionWithSingleDisposalState(
              None,
              Some(sample[DisposalDate]),
              UserType.Individual,
              wasUkResident = true,
              Some(
                sample[CompleteReliefDetailsAnswers]
                  .copy(otherReliefs = Some(OtherReliefsOption.NoOtherReliefs))
              )
            )._1
          )
        }

        checkIsRedirect(
          performAction(),
          routes.YearToDateLiabilityController.checkYourAnswers()
        )
      }

    }

  def redirectWhenNotSingleDisposalCalculatedJourneyBehaviour(
    performAction: () => Future[Result]
  ): Unit =
    "redirect to the check your answers endpoint" when {

      "the user has started this section and is on a single disposal non calculated journey" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            sessionWithSingleDisposalState(
              sample[CompleteNonCalculatedYTDAnswers],
              sample[DisposalDate],
              UserType.Individual,
              wasUkResident = true
            )._1
          )
        }

        checkIsRedirect(
          performAction(),
          routes.YearToDateLiabilityController.checkYourAnswers()
        )
      }

      "the user has started this section and is on a single indirect disposal non calculated journey" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            sessionWithSingleIndirectDisposalState(
              sample[CompleteNonCalculatedYTDAnswers],
              sample[DisposalDate],
              UserType.Individual,
              wasUkResident = true
            )._1
          )
        }

        checkIsRedirect(
          performAction(),
          routes.YearToDateLiabilityController.checkYourAnswers()
        )
      }

      "the user has started this section and is on a single mixed use non calculated journey" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            sessionWithSingleMixedUseDisposalState(
              sample[CompleteNonCalculatedYTDAnswers],
              sample[DisposalDate],
              UserType.Individual,
              wasUkResident = true
            )._1
          )
        }

        checkIsRedirect(
          performAction(),
          routes.YearToDateLiabilityController.checkYourAnswers()
        )
      }

      "the user has not started this section and they are on a single disposal journey and have " +
        "chosen to use other reliefs" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            sessionWithSingleDisposalState(
              Some(sample[CompleteNonCalculatedYTDAnswers]),
              Some(sample[DisposalDate]),
              UserType.Individual,
              wasUkResident = true,
              Some(
                sample[CompleteReliefDetailsAnswers].copy(otherReliefs = Some(sample[OtherReliefsOption.OtherReliefs]))
              )
            )._1
          )
        }

        checkIsRedirect(
          performAction(),
          routes.YearToDateLiabilityController.checkYourAnswers()
        )
      }

      "the user is on a multiple disposals journey" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            sessionWithMultipleDisposalsState(
              None,
              UserType.Individual,
              wasUkResident = true
            )._1
          )
        }

        checkIsRedirect(
          performAction(),
          routes.YearToDateLiabilityController.checkYourAnswers()
        )
      }

      "the user is on a single disposal journey with no other reliefs, but it is a further return" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            sessionWithSingleDisposalState(
              None,
              Some(sample[DisposalDate]),
              UserType.Individual,
              wasUkResident = true,
              Some(sample[CompleteReliefDetailsAnswers].copy(otherReliefs = Some(OtherReliefsOption.NoOtherReliefs))),
              Some(Self),
              isFurtherReturn = true
            )._1
          )
        }

        checkIsRedirect(
          performAction(),
          routes.YearToDateLiabilityController.checkYourAnswers()
        )
      }

    }

  def unsuccessfulUpdateBehaviourForSingleDisposal(
    currentAnswers: YearToDateLiabilityAnswers,
    updatedAnswers: YearToDateLiabilityAnswers,
    result: () => Future[Result]
  ): Unit = {
    val (_, journey, draftReturn) = sessionWithSingleDisposalState(
      currentAnswers,
      sample[DisposalDate].copy(taxYear =
        sample[TaxYear]
          .copy(personalAllowance = AmountInPence(Long.MaxValue), maxPersonalAllowance = AmountInPence(Long.MaxValue))
      ),
      UserType.Individual,
      wasUkResident = true
    )
    unsuccessfulUpdateBehaviour(
      journey,
      journey.copy(draftReturn = draftReturn.copy(yearToDateLiabilityAnswers = Some(updatedAnswers))),
      result
    )
  }

  def unsuccessfulUpdateBehaviour(
    currentJourney: FillingOutReturn,
    updatedJourney: FillingOutReturn,
    result: () => Future[Result]
  ): Unit = {
    val session        = SessionData.empty.copy(journeyStatus = Some(currentJourney))
    val updatedSession = SessionData.empty.copy(journeyStatus = Some(updatedJourney))

    "show an error page" when {

      "there is an error updating the draft return" in {

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockStoreDraftReturn(updatedJourney)(Left(Error("")))
        }

        checkIsTechnicalErrorPage(result())
      }

      "there is an error updating the session data" in {

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockStoreDraftReturn(updatedJourney)(Right(()))
          mockStoreSession(updatedSession)(Left(Error("")))
        }

        checkIsTechnicalErrorPage(result())
      }

    }

  }

  def testFormError(
    data: (String, String)*
  )(
    expectedErrorMessageKey: String,
    errorArgs: Seq[String] = Nil
  )(pageTitleKey: String, titleArgs: String*)(
    performAction: Seq[(String, String)] => Future[Result],
    currentSession: SessionData = sessionWithSingleDisposalState(
      sample[CompleteCalculatedYTDAnswers],
      sample[DisposalDate],
      UserType.Individual,
      wasUkResident = true
    )._1
  ): Unit = {
    inSequence {
      mockAuthWithNoRetrievals()
      mockGetSession(currentSession)
    }
    checkPageIsDisplayed(
      performAction(data),
      messageFromMessageKey(pageTitleKey, titleArgs: _*),
      { doc =>
        doc
          .select("#error-summary-display > ul > li > a")
          .text() shouldBe messageFromMessageKey(
          expectedErrorMessageKey,
          errorArgs: _*
        )
        doc.title() should startWith("Error:")
      },
      BAD_REQUEST
    )
  }

  def testSuccessfulUpdatesAfterSubmit(
    result: => Future[Result],
    oldDraftReturn: DraftReturn,
    newDraftReturn: DraftReturn,
    amendReturnData: Option[AmendReturnData] = None
  ): Unit = {
    val fillingOutReturn =
      sample[FillingOutReturn].copy(
        draftReturn = oldDraftReturn,
        subscribedDetails = sample[SubscribedDetails].copy(name = Right(sample[IndividualName])),
        previousSentReturns = None,
        amendReturnData = amendReturnData
      )

    testSuccessfulUpdatesAfterSubmit(
      result,
      fillingOutReturn,
      fillingOutReturn.copy(draftReturn = newDraftReturn)
    )
  }

  def testSuccessfulUpdatesAfterSubmit(
    result: => Future[Result],
    journey: FillingOutReturn,
    updatedJourney: FillingOutReturn
  ): Unit = {
    val session = SessionData.empty.copy(journeyStatus = Some(journey))

    inSequence {
      mockAuthWithNoRetrievals()
      mockGetSession(session)
      mockStoreDraftReturn(updatedJourney)(
        Right(())
      )
      mockStoreSession(
        session
          .copy(journeyStatus = Some(journey.copy(draftReturn = updatedJourney.draftReturn)))
      )(Right(()))
    }

    checkIsRedirect(
      result,
      routes.YearToDateLiabilityController.checkYourAnswers()
    )
  }

  def testSuccessfulUpdatesAfterSubmitWithSingleDisposal(
    result: => Future[Result],
    oldAnswers: Option[YearToDateLiabilityAnswers],
    newAnswers: YearToDateLiabilityAnswers,
    reliefDetailsAnswers: ReliefDetailsAnswers,
    disposalDate: DisposalDate,
    individualUserType: Option[IndividualUserType],
    previousSentReturnData: Option[PreviousReturnData]
  ): Unit = {
    val draftReturn      = singleDisposalDraftReturnWithCompleteJourneys(
      oldAnswers,
      disposalDate,
      reliefDetailsAnswers,
      individualUserType
    )
    val fillingOutReturn =
      sample[FillingOutReturn].copy(
        draftReturn = draftReturn,
        subscribedDetails = sample[SubscribedDetails].copy(name = Right(sample[IndividualName])),
        previousSentReturns = previousSentReturnData,
        amendReturnData = None
      )

    val newDraftReturn = draftReturn.copy(
      yearToDateLiabilityAnswers = Some(newAnswers),
      reliefDetailsAnswers = Some(reliefDetailsAnswers)
    )
    testSuccessfulUpdatesAfterSubmit(result, fillingOutReturn, fillingOutReturn.copy(draftReturn = newDraftReturn))
  }

  def testSuccessfulUpdatesAfterSubmitWithMultipleDisposals(
    result: => Future[Result],
    oldAnswers: Option[YearToDateLiabilityAnswers],
    newAnswers: YearToDateLiabilityAnswers,
    amendReturnData: Option[AmendReturnData]
  ): Unit = {
    val draftReturn      = sessionWithMultipleDisposalsState(
      oldAnswers,
      UserType.Individual,
      wasUkResident = true
    )._3
    val fillingOutReturn =
      sample[FillingOutReturn].copy(
        draftReturn = draftReturn,
        subscribedDetails = sample[SubscribedDetails].copy(name = Right(sample[IndividualName])),
        previousSentReturns = None,
        amendReturnData = amendReturnData
      )
    val newDraftReturn   =
      draftReturn.copy(yearToDateLiabilityAnswers = Some(newAnswers))
    testSuccessfulUpdatesAfterSubmit(result, fillingOutReturn, fillingOutReturn.copy(draftReturn = newDraftReturn))
  }

  def testSuccessfulUpdatesAfterSubmitWithMultipleDisposals(
    result: => Future[Result],
    oldAnswers: YearToDateLiabilityAnswers,
    newAnswers: YearToDateLiabilityAnswers,
    isFurtherReturn: Boolean = false,
    amendReturnData: Option[AmendReturnData] = None
  ): Unit = {
    val draftReturn    = sessionWithMultipleDisposalsState(
      oldAnswers,
      UserType.Individual,
      wasUkResident = true,
      isFurtherReturn = isFurtherReturn,
      amendReturnData
    )._3
    val newDraftReturn =
      draftReturn.copy(yearToDateLiabilityAnswers = Some(newAnswers))
    testSuccessfulUpdatesAfterSubmit(result, draftReturn, newDraftReturn, amendReturnData)
  }

  def testSuccessfulUpdatesAfterSubmitWithSingleDisposal(
    result: => Future[Result],
    oldAnswers: YearToDateLiabilityAnswers,
    newAnswers: YearToDateLiabilityAnswers,
    reliefDetailsAnswers: ReliefDetailsAnswers = sample[CompleteReliefDetailsAnswers],
    disposalDate: DisposalDate = sample[DisposalDate],
    individualUserType: Option[IndividualUserType] = None,
    previousReturnData: Option[PreviousReturnData] = None
  ): Unit =
    testSuccessfulUpdatesAfterSubmitWithSingleDisposal(
      result,
      Some(oldAnswers),
      newAnswers,
      reliefDetailsAnswers,
      disposalDate,
      individualUserType,
      previousReturnData
    )

  def commonUploadMandatoryEvidenceBehaviour(
    performAction: () => Future[Result]
  ): Unit =
    "redirect to the check your answers page" when {

      val calculatedTaxDue =
        sample[GainCalculatedTaxDue].copy(amountOfTaxDue = AmountInPence(100L))

      val answers = IncompleteCalculatedYTDAnswers.empty.copy(
        estimatedIncome = Some(AmountInPence(1L)),
        personalAllowance = Some(AmountInPence.zero),
        hasEstimatedDetails = Some(false)
      )

      "there is no answer to the tax due question" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            sessionWithSingleDisposalState(
              answers,
              sample[DisposalDate],
              UserType.Individual,
              wasUkResident = true
            )._1
          )
        }

        checkIsRedirect(
          performAction(),
          routes.YearToDateLiabilityController.checkYourAnswers()
        )
      }
      "there is no answer to the tax due but it is equal to the calculated tax due" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            sessionWithSingleDisposalState(
              answers.copy(taxDue = Some(calculatedTaxDue.amountOfTaxDue)),
              sample[DisposalDate],
              UserType.Individual,
              wasUkResident = true
            )._1
          )
        }

        checkIsRedirect(
          performAction(),
          routes.YearToDateLiabilityController.checkYourAnswers()
        )
      }

      "the user hasn't verified whether or not any details given in the return were estimates" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            sessionWithSingleDisposalState(
              answers.copy(hasEstimatedDetails = None),
              sample[DisposalDate],
              UserType.Individual,
              wasUkResident = true
            )._1
          )
        }

        checkIsRedirect(
          performAction(),
          routes.YearToDateLiabilityController.checkYourAnswers()
        )
      }

    }

}

object YearToDateLiabilityControllerSpec extends Matchers {

  def validateCalculatedYearToDateLiabilityPage(
    completeYearToDateLiabilityAnswers: CompleteCalculatedYTDAnswers,
    isATrust: Boolean,
    hideEstimatesQuestion: Boolean,
    doc: Document
  ): Unit = {

    doc.select("#estimatedIncome-value-answer").text() shouldBe (
      if (isATrust) ""
      else
        formatAmountOfMoneyWithPoundSign(
          completeYearToDateLiabilityAnswers.estimatedIncome.inPounds()
        )
    )

    completeYearToDateLiabilityAnswers.personalAllowance.foreach(f =>
      doc.select("#personalAllowance-value-answer").text() shouldBe (
        if (isATrust) ""
        else formatAmountOfMoneyWithPoundSign(f.inPounds())
      )
    )

    doc.select("#hasEstimatedDetails-value-answer").text() shouldBe (
      if (hideEstimatesQuestion) ""
      else if (completeYearToDateLiabilityAnswers.hasEstimatedDetails) "Yes"
      else "No"
    )

    doc
      .select("#taxDue-value-answer")
      .text() shouldBe formatAmountOfMoneyWithPoundSign(
      completeYearToDateLiabilityAnswers.taxDue.inPounds()
    )
  }

  def validateNonCalculatedYearToDateLiabilityPage(
    answers: CompleteNonCalculatedYTDAnswers,
    doc: Document,
    userType: Option[UserType],
    individualUserType: Option[IndividualUserType],
    isFurtherOrAmendReturn: Boolean,
    hideEstimatesQuestion: Boolean
  )(implicit messages: MessagesApi, lang: Lang): Unit = {
    val userKey          = individualUserType match {
      case Some(PersonalRepresentative)                => ".personalRep"
      case Some(PersonalRepresentativeInPeriodOfAdmin) => ".personalRepInPeriodOfAdmin"
      case Some(Capacitor)                             => ".capacitor"
      case _                                           =>
        userType match {
          case Some(UserType.Individual)   => ""
          case Some(UserType.Organisation) => ".trust"
          case Some(UserType.Agent)        => ".agent"
          case _                           => ""
        }
    }
    val furtherReturnKey = if (isFurtherOrAmendReturn) ".furtherReturn" else ""

    if (answers.taxableGainOrLoss < AmountInPence.zero) {
      doc.select("#taxableGainOrLossAnswer-answer").text shouldBe messages(
        s"taxableGainOrLoss$userKey$furtherReturnKey.loss.label"
      )
      doc
        .select("#taxableGainOrLossAmount-answer")
        .text                                            shouldBe formatAmountOfMoneyWithPoundSign(
        answers.taxableGainOrLoss.inPounds().abs
      )
    } else if (answers.taxableGainOrLoss > AmountInPence.zero) {
      doc.select("#taxableGainOrLossAnswer-answer").text shouldBe messages(
        s"taxableGainOrLoss$userKey$furtherReturnKey.gain.label"
      )
      doc
        .select("#taxableGainOrLossAmount-answer")
        .text                                            shouldBe formatAmountOfMoneyWithPoundSign(
        answers.taxableGainOrLoss.inPounds()
      )
    } else
      doc.select("#taxableGainOrLossAnswer-answer").text shouldBe messages(
        s"taxableGainOrLoss$furtherReturnKey.noLossOrGain.label"
      )

    doc.select("#hasEstimatedDetails-value-answer").text() shouldBe (
      if (hideEstimatesQuestion) ""
      else if (answers.hasEstimatedDetails) "Yes"
      else "No"
    )

    if (isFurtherOrAmendReturn) {
      answers.yearToDateLiability.foreach { y =>
        doc.select("#yearToDateLiability-answer").text() shouldBe formatAmountOfMoneyWithPoundSign(y.inPounds())
      }

      answers.checkForRepayment.foreach { checkForRepayment =>
        doc.select("#repayment-answer").text() shouldBe (if (checkForRepayment) "Yes" else "No")
      }

    }

    doc
      .select("#nonCalculatedTaxDue-value-answer")
      .text() shouldBe formatAmountOfMoneyWithPoundSign(
      answers.taxDue.inPounds()
    )
  }
}
