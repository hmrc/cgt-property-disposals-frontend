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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.models.email

import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.RegistrationStatus.{IndividualMissingEmail, RegistrationReady}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.SubscriptionStatus.{SubscriptionMissingData, SubscriptionReady}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, StartingNewDraftReturn, Subscribed}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{RepresenteeAnswers, RepresenteeContactDetails}

sealed trait EmailJourneyType extends Product with Serializable

object EmailJourneyType {

  sealed trait Onboarding extends EmailJourneyType

  sealed trait ManagingSubscription extends EmailJourneyType

  sealed trait Returns extends EmailJourneyType

  object Onboarding {

    final case class EnteringRegistrationEmail(journey: Either[RegistrationReady, IndividualMissingEmail])
        extends Onboarding

    final case class ChangingRegistrationEmail(journey: RegistrationReady) extends Onboarding

    final case class EnteringSubscriptionEmail(journey: SubscriptionMissingData) extends Onboarding

    final case class ChangingSubscriptionEmail(journey: SubscriptionReady) extends Onboarding

  }

  object ManagingSubscription {

    final case class ChangingAccountEmail(journey: Subscribed) extends ManagingSubscription

  }

  object Returns {

    final case class ChangingRepresenteeEmail(
      journey: Either[StartingNewDraftReturn, FillingOutReturn],
      answers: RepresenteeAnswers,
      contactDetails: RepresenteeContactDetails
    ) extends Returns

  }

  implicit class EmailJourneyTypeOps(private val e: EmailJourneyType) extends AnyVal {

    def captionMessageKey: String =
      e match {
        case _: EmailJourneyType.Onboarding                       => "subscription.caption"
        case _: EmailJourneyType.Returns.ChangingRepresenteeEmail => "representee.caption"
        case _: EmailJourneyType.ManagingSubscription             => "account.caption"
      }

  }

}
