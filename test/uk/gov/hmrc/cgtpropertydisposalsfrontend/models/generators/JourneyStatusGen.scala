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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators
import org.scalacheck.Gen
import org.scalacheck.ScalacheckShapeless._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.RegistrationStatus.{IndividualMissingEmail, IndividualSupplyingInformation, IndividualWantsToRegisterTrust, RegistrationReady}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.SubscriptionStatus.SubscriptionReady
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{AgentWithoutAgentEnrolment, AlreadySubscribedWithDifferentGGAccount, FillingOutReturn, JustSubmittedReturn, NewEnrolmentCreatedForMissingEnrolment, NonGovernmentGatewayJourney, PreviousReturnData, RegistrationStatus, StartingNewDraftReturn, StartingToAmendReturn, SubmitReturnFailed, SubmittingReturn, Subscribed, ViewingReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.AgentReferenceNumber
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.SubscriptionResponse.SubscriptionSuccessful
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns._

object JourneyStatusGen extends JourneyStatusLowerPriorityGen with GenUtils {
  implicit val subscriptionReadyGen: Gen[SubscriptionReady] = for {
    subscriptionDetails <- OnboardingDetailsGen.subscriptionDetailsArb
    ggCredId            <- IdGen.ggCredIdGen
  } yield SubscriptionReady(subscriptionDetails, ggCredId)

  private val registrationStatusGen = Gen.oneOf(
    gen[IndividualWantsToRegisterTrust],
    gen[IndividualSupplyingInformation],
    gen[IndividualMissingEmail],
    gen[RegistrationReady]
  )

  implicit val journeyStatusGen: Gen[JourneyStatus] = Gen.oneOf(
    subscriptionReadyGen,
    individualSupplyingInformationGen,
    subscribedGen,
    individualMissingEmailGen,
    registrationReadyGen,
    startingNewDraftReturnGen,
    fillingOutReturnGen,
    justSubmittedReturnGen,
    viewingReturnGen,
    submitReturnFailedGen,
    submittingReturn,
    startingToAmendReturnGen,
    gen[NewEnrolmentCreatedForMissingEnrolment],
    gen[AlreadySubscribedWithDifferentGGAccount],
    gen[RegistrationStatus],
    Gen.const(NonGovernmentGatewayJourney),
    Gen.const(AgentWithoutAgentEnrolment),
    registrationStatusGen
  )

}

trait JourneyStatusLowerPriorityGen extends GenUtils {

  private val agentReferenceNumberGen = gen[AgentReferenceNumber]
  private val returnSummaryGen        = gen[ReturnSummary]
  private val completeReturnGen       = gen[CompleteReturn]

  implicit val subscriptionSuccessfulGen: Gen[SubscriptionSuccessful] =
    gen[SubscriptionSuccessful]

  implicit val individualSupplyingInformationGen: Gen[IndividualSupplyingInformation] =
    for {
      name        <- Gen.option(NameGen.individualNameGen)
      address     <- Gen.option(AddressGen.addressGen)
      email       <- Gen.option(EmailGen.emailGen)
      emailSource <- Gen.option(EmailGen.emailSourceGen)
      ggCredId    <- IdGen.ggCredIdGen
    } yield IndividualSupplyingInformation(name, address, email, emailSource, ggCredId)

  implicit val subscribedGen: Gen[Subscribed] = {
    for {
      subscribedDetails    <- SubscribedDetailsGen.subscribedDetailsGen
      ggCredId             <- IdGen.ggCredIdGen
      agentReferenceNumber <- Gen.option(agentReferenceNumberGen)
      draftReturns         <- Gen.listOf(DraftReturnGen.draftReturnGen)
      sentReturns          <- Gen.listOf(returnSummaryGen)
    } yield Subscribed(subscribedDetails, ggCredId, agentReferenceNumber, draftReturns, sentReturns)
  }

  implicit val individualMissingEmailGen: Gen[IndividualMissingEmail] = for {
    name     <- NameGen.individualNameGen
    address  <- AddressGen.addressGen
    ggCredId <- IdGen.ggCredIdGen
  } yield IndividualMissingEmail(name, address, ggCredId)

  implicit val registrationReadyGen: Gen[RegistrationReady] =
    gen[RegistrationReady]

  implicit val startingNewDraftReturnGen: Gen[StartingNewDraftReturn] =
    for {
      subscribedDetails      <- SubscribedDetailsGen.subscribedDetailsGen
      ggCredId               <- IdGen.ggCredIdGen
      agentReferenceNumber   <- Gen.option(agentReferenceNumberGen)
      newReturnTriageAnswers <- Gen.either(gen[MultipleDisposalsTriageAnswers], gen[SingleDisposalTriageAnswers])
      representeeAnswers     <- Gen.option(RepresenteeAnswersGen.representeeAnswersGen)
      previousSentReturns    <- Gen.option(ReturnGen.previousReturnDataGen)
    } yield StartingNewDraftReturn(
      subscribedDetails,
      ggCredId,
      agentReferenceNumber,
      newReturnTriageAnswers,
      representeeAnswers,
      previousSentReturns
    )

  implicit val fillingOutReturnGen: Gen[FillingOutReturn] =
    for {
      subscribedDetails    <- SubscribedDetailsGen.subscribedDetailsGen
      ggCredId             <- IdGen.ggCredIdGen
      agentReferenceNumber <- Gen.option(agentReferenceNumberGen)
      draftReturn          <- DraftReturnGen.draftReturnGen
      previousSentReturns  <- Gen.option(ReturnGen.previousReturnDataGen)
      amendReturnData      <- Gen.option(ReturnGen.amendReturnDataGen)
    } yield FillingOutReturn(
      subscribedDetails,
      ggCredId,
      agentReferenceNumber,
      draftReturn,
      previousSentReturns,
      amendReturnData
    )

  implicit val justSubmittedReturnGen: Gen[JustSubmittedReturn] =
    for {
      subscribedDetails    <- SubscribedDetailsGen.subscribedDetailsGen
      ggCredId             <- IdGen.ggCredIdGen
      agentReferenceNumber <- Gen.option(agentReferenceNumberGen)
      completeReturn       <- completeReturnGen
      submissionResponse   <- gen[SubmitReturnResponse]
      amendReturnData      <- Gen.option(ReturnGen.amendReturnDataGen)
    } yield JustSubmittedReturn(
      subscribedDetails,
      ggCredId,
      agentReferenceNumber,
      completeReturn,
      submissionResponse,
      amendReturnData
    )

  private val previousReturnDataGen = gen[PreviousReturnData]

  implicit val viewingReturnGen: Gen[ViewingReturn] =
    for {
      subscribedDetails    <- SubscribedDetailsGen.subscribedDetailsGen
      ggCredId             <- IdGen.ggCredIdGen
      agentReferenceNumber <- Gen.option(agentReferenceNumberGen)
      completeReturn       <- completeReturnGen
      returnType           <- ReturnGen.returnTypeGen
      returnSummary        <- returnSummaryGen
      previousSentReturns  <- Gen.option(previousReturnDataGen)
    } yield ViewingReturn(
      subscribedDetails,
      ggCredId,
      agentReferenceNumber,
      completeReturn,
      returnType,
      returnSummary,
      previousSentReturns
    )

  implicit val submitReturnFailedGen: Gen[SubmitReturnFailed] =
    for {
      subscribedDetails    <- SubscribedDetailsGen.subscribedDetailsGen
      ggCredId             <- IdGen.ggCredIdGen
      agentReferenceNumber <- Gen.option(agentReferenceNumberGen)
    } yield SubmitReturnFailed(subscribedDetails, ggCredId, agentReferenceNumber)

  implicit val submittingReturn: Gen[SubmittingReturn] = for {
    subscribedDetails    <- SubscribedDetailsGen.subscribedDetailsGen
    ggCredId             <- IdGen.ggCredIdGen
    agentReferenceNumber <- Gen.option(agentReferenceNumberGen)
  } yield SubmittingReturn(subscribedDetails, ggCredId, agentReferenceNumber)

  implicit val startingToAmendReturnGen: Gen[StartingToAmendReturn] = {
    for {
      subscribedDetails       <- SubscribedDetailsGen.subscribedDetailsGen
      ggCredId                <- IdGen.ggCredIdGen
      agentReferenceNumber    <- Gen.option(agentReferenceNumberGen)
      originalReturn          <- ReturnGen.completeReturnWithSummaryGen
      isFirstReturn           <- Generators.booleanGen
      previousSentReturns     <- Gen.option(previousReturnDataGen)
      unmetDependencyFieldUrl <- Gen.option(Generators.stringGen)
    } yield StartingToAmendReturn(
      subscribedDetails,
      ggCredId,
      agentReferenceNumber,
      originalReturn,
      isFirstReturn,
      previousSentReturns,
      unmetDependencyFieldUrl
    )
  }

}
