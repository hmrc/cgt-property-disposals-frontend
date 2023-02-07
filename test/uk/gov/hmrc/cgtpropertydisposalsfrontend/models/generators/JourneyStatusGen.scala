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
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.RegistrationStatus.{IndividualMissingEmail, IndividualSupplyingInformation, RegistrationReady}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, JustSubmittedReturn, StartingNewDraftReturn, StartingToAmendReturn, SubmitReturnFailed, Subscribed, ViewingReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.SubscriptionStatus.SubscriptionReady
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.SubscriptionResponse.SubscriptionSuccessful

object JourneyStatusGen extends JourneyStatusLowerPriorityGen with GenUtils {
  implicit val subscriptionReadyGen: Gen[SubscriptionReady] =
    gen[SubscriptionReady]

  implicit val journeyStatusGen: Gen[JourneyStatus] = gen[JourneyStatus]

}

trait JourneyStatusHigherPriorityGen extends JourneyStatusLowerPriorityGen { this: GenUtils =>
  implicit val journeyStatusGen: Gen[JourneyStatus] = gen[JourneyStatus]

  implicit val subscriptionReadyGen: Gen[SubscriptionReady] =
    gen[SubscriptionReady]
}

trait JourneyStatusLowerPriorityGen { this: GenUtils =>

  implicit val subscriptionSuccessfulGen: Gen[SubscriptionSuccessful] =
    gen[SubscriptionSuccessful]

  implicit val individualSupplyingInformationGen: Gen[IndividualSupplyingInformation] =
    gen[IndividualSupplyingInformation]

  implicit val subscribedGen: Gen[Subscribed] = gen[Subscribed]

  implicit val individualMissingEmailGen: Gen[IndividualMissingEmail] =
    gen[IndividualMissingEmail]

  implicit val registrationReadyGen: Gen[RegistrationReady] =
    gen[RegistrationReady]

  implicit val startingNewDraftReturnGen: Gen[StartingNewDraftReturn] =
    gen[StartingNewDraftReturn]

  implicit val fillingOutReturnGen: Gen[FillingOutReturn] =
    gen[FillingOutReturn]

  implicit val justSubmittedReturnGen: Gen[JustSubmittedReturn] =
    gen[JustSubmittedReturn]

  implicit val viewingReturnGen: Gen[ViewingReturn] = gen[ViewingReturn]

  implicit val submitReturnFailedGen: Gen[SubmitReturnFailed] =
    gen[SubmitReturnFailed]

  implicit val startingToAmendReturnGen: Gen[StartingToAmendReturn] =
    gen[StartingToAmendReturn]

}
