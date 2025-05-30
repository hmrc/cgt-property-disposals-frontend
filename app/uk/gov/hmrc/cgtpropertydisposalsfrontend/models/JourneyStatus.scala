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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.models

import cats.Eq
import cats.implicits.catsSyntaxPartialOrder
import play.api.libs.json.*
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.EitherUtils.eitherFormat
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.email.{Email, EmailSource}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.AmountInPence
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.{AgentReferenceNumber, CgtReference, GGCredId}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.IndividualName
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.bpr.BusinessPartnerRecord
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.{RegistrationDetails, SubscribedDetails, SubscriptionDetails}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns._

import java.time.LocalDate

sealed trait JourneyStatus extends Product with Serializable

object JourneyStatus {

  sealed trait SubscriptionStatus extends JourneyStatus with Product with Serializable

  object SubscriptionStatus {

    // user with affinity organisation is trying to subscribe without having a trust enrolment
    final case class DeterminingIfOrganisationIsTrust(
      ggCredId: GGCredId,
      ggEmail: Option[Email],
      isReportingForTrust: Option[Boolean],
      hasTrn: Option[Boolean]
    ) extends SubscriptionStatus

    // we cannot automatically find an identifier for an individual to get their business partner record
    final case class TryingToGetIndividualsFootprint(
      hasNino: Option[Boolean],
      hasSautr: Option[Boolean],
      ggEmail: Option[Email],
      ggCredId: GGCredId
    ) extends SubscriptionStatus

    // entity is missing data in order to continue on with subscription
    final case class SubscriptionMissingData(
      businessPartnerRecord: BusinessPartnerRecord,
      manuallyEnteredEmail: Option[Email],
      manuallyEnteredAddress: Option[Address],
      ggCredId: GGCredId,
      ggEmail: Option[Email]
    ) extends SubscriptionStatus

    // subscription details have been gathered and are ready to be used to subscribe
    final case class SubscriptionReady(
      subscriptionDetails: SubscriptionDetails,
      ggCredId: GGCredId
    ) extends SubscriptionStatus
  }

  // cgt account existed in ETMP but no enrolment was found in MDTP - a new enrolment
  // has just been created to make the state consistent across ETMP and MTDTP
  final case class NewEnrolmentCreatedForMissingEnrolment(
    subscribedDetails: SubscribedDetails,
    GGCredId: GGCredId
  ) extends JourneyStatus

  // subscription has been submitted to ETMP
  final case class Subscribed(
    subscribedDetails: SubscribedDetails,
    ggCredId: GGCredId,
    agentReferenceNumber: Option[AgentReferenceNumber],
    draftReturns: List[DraftReturn],
    sentReturns: List[ReturnSummary]
  ) extends JourneyStatus

  object Subscribed {
    implicit class SubscribedOps(private val s: Subscribed) extends AnyVal {
      def totalLeftToPay(): AmountInPence                                                  =
        AmountInPence(s.sentReturns.map(_.totalOutstanding().value).sum)
      private def optionalDateOrdering(date1: Option[LocalDate], date2: Option[LocalDate]) =
        (date1, date2) match {
          case (None, None)         => true
          case (Some(_), None)      => true
          case (None, Some(_))      => false
          case (Some(d1), Some(d2)) => d1.isBefore(d2)
        }

      def returnsWithDueDates(): List[(ReturnSummary, Option[LocalDate])] =
        s.sentReturns
          .map { r =>
            val dueDate = r.charges
              .filter(_.totalOutstanding() > AmountInPence.zero)
              .map(_.dueDate)
              .sortWith(TimeUtils.localDateOrder.compare(_, _) < 0)
              .headOption
            (r, dueDate)
          }
          .sortWith { case ((r1, d1), (r2, d2)) =>
            if (!d1.exists(d2.contains(_))) optionalDateOrdering(d1, d2)
            else if (!r1.lastUpdatedDate.exists(r2.lastUpdatedDate.contains(_)))
              optionalDateOrdering(r1.lastUpdatedDate, r2.lastUpdatedDate)
            else r1.submissionDate.isAfter(r2.submissionDate)
          }

      def taxDueDate(): Option[LocalDate] = returnsWithDueDates().headOption.flatMap(_._2)
    }
  }

  final case class StartingNewDraftReturn(
    subscribedDetails: SubscribedDetails,
    ggCredId: GGCredId,
    agentReferenceNumber: Option[AgentReferenceNumber],
    newReturnTriageAnswers: Either[MultipleDisposalsTriageAnswers, SingleDisposalTriageAnswers],
    representeeAnswers: Option[RepresenteeAnswers],
    previousSentReturns: Option[PreviousReturnData]
  ) extends JourneyStatus

  final case class FillingOutReturn(
    subscribedDetails: SubscribedDetails,
    ggCredId: GGCredId,
    agentReferenceNumber: Option[AgentReferenceNumber],
    draftReturn: DraftReturn,
    previousSentReturns: Option[PreviousReturnData],
    amendReturnData: Option[AmendReturnData]
  ) extends JourneyStatus

  object FillingOutReturn {
    implicit class FillingOutReturnOps(private val f: FillingOutReturn) extends AnyVal {

      def isFurtherOrAmendReturn: Option[Boolean] =
        if (isAmendReturn) Some(true) else isFurtherReturn

      def isAmendReturn: Boolean = f.amendReturnData.isDefined

      def isFurtherReturn: Option[Boolean] =
        determineIfFurtherReturn(
          f.subscribedDetails,
          f.previousSentReturns,
          f.draftReturn.triageAnswers(),
          f.draftReturn.representeeAnswers
        )

      def withForceDisplayGainOrLossAfterReliefsForAmends: FillingOutReturn =
        f.copy(
          amendReturnData = f.amendReturnData.map(_.copy(shouldDisplayGainOrLossAfterReliefs = true)),
          draftReturn = f.draftReturn match {
            case s: DraftSingleDisposalReturn if isAmendReturn => s.copy(initialGainOrLoss = None)
            case other                                         => other
          }
        )
    }
  }

  final case class JustSubmittedReturn(
    subscribedDetails: SubscribedDetails,
    ggCredId: GGCredId,
    agentReferenceNumber: Option[AgentReferenceNumber],
    completeReturn: CompleteReturn,
    submissionResponse: SubmitReturnResponse,
    amendReturnData: Option[AmendReturnData]
  ) extends JourneyStatus

  final case class SubmitReturnFailed(
    subscribedDetails: SubscribedDetails,
    ggCredId: GGCredId,
    agentReferenceNumber: Option[AgentReferenceNumber]
  ) extends JourneyStatus

  // in progress submission
  final case class SubmittingReturn(
    subscribedDetails: SubscribedDetails,
    ggCredId: GGCredId,
    agentReferenceNumber: Option[AgentReferenceNumber]
  ) extends SubscriptionStatus

  final case class ViewingReturn(
    subscribedDetails: SubscribedDetails,
    ggCredId: GGCredId,
    agentReferenceNumber: Option[AgentReferenceNumber],
    completeReturn: CompleteReturn,
    returnType: ReturnType,
    returnSummary: ReturnSummary,
    previousSentReturns: Option[PreviousReturnData]
  ) extends JourneyStatus

  final case class StartingToAmendReturn(
    subscribedDetails: SubscribedDetails,
    ggCredId: GGCredId,
    agentReferenceNumber: Option[AgentReferenceNumber],
    originalReturn: CompleteReturnWithSummary,
    isFirstReturn: Boolean,
    previousSentReturns: Option[PreviousReturnData],
    unmetDependencyFieldUrl: Option[String]
  ) extends JourneyStatus

  final case class AlreadySubscribedWithDifferentGGAccount(
    ggCredId: GGCredId,
    cgtReference: Option[CgtReference]
  ) extends JourneyStatus

  sealed trait RegistrationStatus extends JourneyStatus with Product with Serializable {
    val emailSource: Option[EmailSource]
    val ggCredId: GGCredId
  }

  object RegistrationStatus {

    // user with individual account has said they want to register a trust
    final case class IndividualWantsToRegisterTrust(ggCredId: GGCredId) extends RegistrationStatus {
      val emailSource: Option[EmailSource] = None
    }

    // user is supplying information for subscription
    final case class IndividualSupplyingInformation(
      name: Option[IndividualName],
      address: Option[Address],
      email: Option[Email],
      emailSource: Option[EmailSource],
      ggCredId: GGCredId
    ) extends RegistrationStatus

    // we are capturing an email for a user who doesn't have one we can retrieve
    final case class IndividualMissingEmail(
      name: IndividualName,
      address: Address,
      ggCredId: GGCredId
    ) extends RegistrationStatus {
      val emailSource: Option[EmailSource] = None
    }

    // we have all the details necessary for registration
    final case class RegistrationReady(
      registrationDetails: RegistrationDetails,
      ggCredId: GGCredId
    ) extends RegistrationStatus {
      val emailSource: Option[EmailSource] = Some(
        registrationDetails.emailSource
      )
    }

  }

  case object Registering extends JourneyStatus

  object AgentStatus {

    final case class AgentSupplyingClientDetails(
      agentReferenceNumber: AgentReferenceNumber,
      agentGGCredId: GGCredId,
      verifierMatchingDetails: Option[VerifierMatchingDetails]
    ) extends JourneyStatus

    final case class VerifierMatchingDetails(
      clientDetails: SubscribedDetails,
      correctVerifierSupplied: Boolean
    )

    implicit val agentSupplyingClientDetailsFormat: OFormat[AgentSupplyingClientDetails] =
      Json.format[AgentSupplyingClientDetails]
    implicit val verifierMatchingDetailsFormat: OFormat[VerifierMatchingDetails]         =
      Json.format[VerifierMatchingDetails]

  }

  final case class PreviousReturnData(
    summaries: List[ReturnSummary],
    previousYearToDate: Option[AmountInPence],
    previousReturnsImplyEligibilityForCalculation: Option[Boolean],
    calculationData: Option[List[FurtherReturnCalculationData]]
  )

  object PreviousReturnData {
    implicit val format: OFormat[PreviousReturnData] = Json.format
  }

  case object NonGovernmentGatewayJourney extends JourneyStatus

  case object AgentWithoutAgentEnrolment extends JourneyStatus

  implicit class StartingNewDraftReturnOps(private val s: StartingNewDraftReturn) extends AnyVal {

    def isFurtherReturn: Option[Boolean] =
      determineIfFurtherReturn(
        s.subscribedDetails,
        s.previousSentReturns,
        s.newReturnTriageAnswers,
        s.representeeAnswers
      )

  }

  private def determineIfFurtherReturn(
    subscribedDetails: SubscribedDetails,
    previousReturnData: Option[PreviousReturnData],
    triageAnswers: Either[MultipleDisposalsTriageAnswers, SingleDisposalTriageAnswers],
    representeeAnswers: Option[RepresenteeAnswers]
  ): Option[Boolean] = {

    val taxYear: Option[TaxYear] =
      triageAnswers.fold(
        _.fold(
          _.taxYear,
          mc => Some(mc.taxYear)
        ),
        _.fold(
          _.disposalDate.map(_.taxYear),
          sc => Some(sc.disposalDate.taxYear)
        )
      )

    val originalReturnTaxYearStartYear: Option[String] = taxYear.map(_.startDateInclusive.getYear.toString)

    if (taxYear.isEmpty) {
      None
    } else {

      val filteredSummaries: Option[List[ReturnSummary]] =
        previousReturnData.map(r => r.summaries.filter(s => originalReturnTaxYearStartYear.contains(s.taxYear)))

      lazy val hasPreviousSentReturns = filteredSummaries.exists(_.nonEmpty)

      subscribedDetails.name match {
        case Left(_) =>
          Some(hasPreviousSentReturns)

        case Right(_) =>
          val individualUserType = triageAnswers
            .fold(
              _.fold(_.individualUserType, _.individualUserType),
              _.fold(_.individualUserType, _.individualUserType)
            )

          individualUserType.flatMap {
            case _: RepresentativeType =>
              representeeAnswers
                .flatMap(
                  _.fold(_.isFirstReturn, complete => Some(complete.isFirstReturn))
                    .map(!_)
                )
            case _                     =>
              Some(hasPreviousSentReturns)
          }
      }
    }
  }

  import AgentStatus.*
  import SubscriptionStatus.*
  import RegistrationStatus.*

  implicit val trustSubscriptionFormat: OFormat[DeterminingIfOrganisationIsTrust]    =
    Json.format[DeterminingIfOrganisationIsTrust]
  implicit val footprintSubscriptionFormat: OFormat[TryingToGetIndividualsFootprint] =
    Json.format[TryingToGetIndividualsFootprint]
  implicit val missingSubscriptionFormat: OFormat[SubscriptionMissingData]           = Json.format[SubscriptionMissingData]
  implicit val readySubscriptionFormat: OFormat[SubscriptionReady]                   = Json.format[SubscriptionReady]
  implicit val submittingSubscriptionFormat: OFormat[SubmittingReturn]               = Json.format[SubmittingReturn]
  implicit val statusSubscriptionFormat: OFormat[SubscriptionStatus]                 = Json.format[SubscriptionStatus]

  implicit val newEnrolmentCreatedFormat: OFormat[NewEnrolmentCreatedForMissingEnrolment]                      =
    Json.format[NewEnrolmentCreatedForMissingEnrolment]
  implicit val subscribedFormat: OFormat[Subscribed]                                                           = Json.format[Subscribed]
  implicit val startingNewDraftReturnFormat: OFormat[StartingNewDraftReturn]                                   =
    Json.format[StartingNewDraftReturn]
  implicit val fillingOutReturnFormat: OFormat[FillingOutReturn]                                               = Json.format[FillingOutReturn]
  implicit val justSubmittedReturnFormat: OFormat[JustSubmittedReturn]                                         = Json.format[JustSubmittedReturn]
  implicit val submitReturnFailedFormat: OFormat[SubmitReturnFailed]                                           = Json.format[SubmitReturnFailed]
  implicit val viewingReturnFormat: OFormat[ViewingReturn]                                                     = Json.format[ViewingReturn]
  implicit val startingToAmendReturnFormat: OFormat[StartingToAmendReturn]                                     =
    Json.format[StartingToAmendReturn]
  implicit val alreadySubscribedWithDifferentGGAccountFormat: OFormat[AlreadySubscribedWithDifferentGGAccount] =
    Json.format[AlreadySubscribedWithDifferentGGAccount]

  implicit val registrationReadyFormat: OFormat[RegistrationStatus.RegistrationReady]                           =
    Json.format[RegistrationStatus.RegistrationReady]
  implicit val individualWantsToRegisterTrustFormat: OFormat[RegistrationStatus.IndividualWantsToRegisterTrust] =
    Json.format[RegistrationStatus.IndividualWantsToRegisterTrust]
  implicit val individualSupplyingInformationFormat: OFormat[RegistrationStatus.IndividualSupplyingInformation] =
    Json.format[RegistrationStatus.IndividualSupplyingInformation]
  implicit val individualMissingEmailFormat: OFormat[RegistrationStatus.IndividualMissingEmail]                 =
    Json.format[RegistrationStatus.IndividualMissingEmail]
  implicit val registrationStatusFormat: OFormat[RegistrationStatus]                                            =
    Json.format[RegistrationStatus]

  implicit val registeringFormat: OFormat[Registering.type] = OFormat[Registering.type](
    Reads[Registering.type] {
      case JsObject(_) => JsSuccess(Registering)
      case _           => JsError("Empty object expected")
    },
    OWrites[Registering.type](_ => Json.obj())
  )

  implicit val ggJourneyFormat: OFormat[NonGovernmentGatewayJourney.type] = OFormat[NonGovernmentGatewayJourney.type](
    Reads[NonGovernmentGatewayJourney.type] {
      case JsObject(_) => JsSuccess(NonGovernmentGatewayJourney)
      case _           => JsError("No valid GG journey")
    },
    OWrites[NonGovernmentGatewayJourney.type](_ => Json.obj())
  )

  implicit val agentEnrolmentFormat: OFormat[AgentWithoutAgentEnrolment.type] =
    OFormat[AgentWithoutAgentEnrolment.type](
      Reads[AgentWithoutAgentEnrolment.type] {
        case JsObject(_) => JsSuccess(AgentWithoutAgentEnrolment)
        case _           => JsError("No valid agent enrolment")
      },
      OWrites[AgentWithoutAgentEnrolment.type](_ => Json.obj())
    )

  implicit val format: OFormat[JourneyStatus] = new OFormat[JourneyStatus] {
    override def reads(json: JsValue): JsResult[JourneyStatus] = json match {
      case JsObject(fields) if fields.size == 1 =>
        fields.head match {
          case ("Registering", _)                             => JsSuccess(Registering)
          case ("NonGovernmentGatewayJourney", _)             => JsSuccess(NonGovernmentGatewayJourney)
          case ("AgentWithoutAgentEnrolment", _)              => JsSuccess(AgentWithoutAgentEnrolment)
          case ("DeterminingIfOrganisationIsTrust", v)        => v.validate[DeterminingIfOrganisationIsTrust]
          case ("TryingToGetIndividualsFootprint", v)         => v.validate[TryingToGetIndividualsFootprint]
          case ("SubscriptionMissingData", v)                 => v.validate[SubscriptionMissingData]
          case ("SubscriptionReady", v)                       => v.validate[SubscriptionReady]
          case ("SubmittingReturn", v)                        => v.validate[SubmittingReturn]
          case ("NewEnrolmentCreatedForMissingEnrolment", v)  => v.validate[NewEnrolmentCreatedForMissingEnrolment]
          case ("Subscribed", v)                              => v.validate[Subscribed]
          case ("StartingNewDraftReturn", v)                  => v.validate[StartingNewDraftReturn]
          case ("FillingOutReturn", v)                        => v.validate[FillingOutReturn]
          case ("JustSubmittedReturn", v)                     => v.validate[JustSubmittedReturn]
          case ("SubmitReturnFailed", v)                      => v.validate[SubmitReturnFailed]
          case ("ViewingReturn", v)                           => v.validate[ViewingReturn]
          case ("StartingToAmendReturn", v)                   => v.validate[StartingToAmendReturn]
          case ("AlreadySubscribedWithDifferentGGAccount", v) => v.validate[AlreadySubscribedWithDifferentGGAccount]
          case ("IndividualWantsToRegisterTrust", v)          => v.validate[IndividualWantsToRegisterTrust]
          case ("IndividualSupplyingInformation", v)          => v.validate[IndividualSupplyingInformation]
          case ("IndividualMissingEmail", v)                  => v.validate[IndividualMissingEmail]
          case ("RegistrationReady", v)                       => v.validate[RegistrationReady]
          case ("AgentSupplyingClientDetails", v)             => v.validate[AgentSupplyingClientDetails]
          case _                                              => JsError("Unrecognized JourneyStatus type")
        }
      case _                                    => JsError("Expected wrapper object for JourneyStatus")
    }

    override def writes(o: JourneyStatus): JsObject = o match {
      case Registering                                => Json.obj("Registering" -> Json.obj())
      case NonGovernmentGatewayJourney                => Json.obj("NonGovernmentGatewayJourney" -> Json.obj())
      case AgentWithoutAgentEnrolment                 => Json.obj("AgentWithoutAgentEnrolment" -> Json.obj())
      case v: DeterminingIfOrganisationIsTrust        => Json.obj("DeterminingIfOrganisationIsTrust" -> Json.toJson(v))
      case v: TryingToGetIndividualsFootprint         => Json.obj("TryingToGetIndividualsFootprint" -> Json.toJson(v))
      case v: SubscriptionMissingData                 => Json.obj("SubscriptionMissingData" -> Json.toJson(v))
      case v: SubscriptionReady                       => Json.obj("SubscriptionReady" -> Json.toJson(v))
      case v: SubmittingReturn                        => Json.obj("SubmittingReturn" -> Json.toJson(v))
      case v: NewEnrolmentCreatedForMissingEnrolment  =>
        Json.obj("NewEnrolmentCreatedForMissingEnrolment" -> Json.toJson(v))
      case v: Subscribed                              => Json.obj("Subscribed" -> Json.toJson(v))
      case v: StartingNewDraftReturn                  => Json.obj("StartingNewDraftReturn" -> Json.toJson(v))
      case v: FillingOutReturn                        => Json.obj("FillingOutReturn" -> Json.toJson(v))
      case v: JustSubmittedReturn                     => Json.obj("JustSubmittedReturn" -> Json.toJson(v))
      case v: SubmitReturnFailed                      => Json.obj("SubmitReturnFailed" -> Json.toJson(v))
      case v: ViewingReturn                           => Json.obj("ViewingReturn" -> Json.toJson(v))
      case v: StartingToAmendReturn                   => Json.obj("StartingToAmendReturn" -> Json.toJson(v))
      case v: AlreadySubscribedWithDifferentGGAccount =>
        Json.obj("AlreadySubscribedWithDifferentGGAccount" -> Json.toJson(v))
      case v: IndividualWantsToRegisterTrust          => Json.obj("IndividualWantsToRegisterTrust" -> Json.toJson(v))
      case v: IndividualSupplyingInformation          => Json.obj("IndividualSupplyingInformation" -> Json.toJson(v))
      case v: IndividualMissingEmail                  => Json.obj("IndividualMissingEmail" -> Json.toJson(v))
      case v: RegistrationReady                       => Json.obj("RegistrationReady" -> Json.toJson(v))
      case v: AgentSupplyingClientDetails             => Json.obj("AgentSupplyingClientDetails" -> Json.toJson(v))
    }
  }

  implicit val eq: Eq[JourneyStatus] = Eq.fromUniversalEquals

  implicit def eqJ[J <: JourneyStatus]: Eq[J] = Eq.instance(eq.eqv(_, _))

}
