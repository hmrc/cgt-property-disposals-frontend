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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.views.address

import cats.Eq
import cats.syntax.eq._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.RegistrationStatus.{IndividualSupplyingInformation, RegistrationReady}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, Subscribed}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.SubscriptionStatus.SubscriptionReady
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views.address.AddressJourneyType.Returns.FillingOutReturnAddressJourney

sealed trait AddressJourneyType[J <: JourneyStatus] extends Product with Serializable {
  val journey: J
}

object AddressJourneyType {

  // user is entering/editing an address to subscribe with
  sealed trait Onboarding[J <: JourneyStatus] extends AddressJourneyType[J]

  // user has subscribed and is managing their subscription address
  sealed trait ManagingSubscription[J <: JourneyStatus] extends AddressJourneyType[J]

  // user is entering address they want to report cgt for
  sealed trait Returns[J <: JourneyStatus] extends AddressJourneyType[J]

  object Onboarding {

    final case class RegistrationReadyAddressJourney(journey: RegistrationReady) extends Onboarding[RegistrationReady]

    final case class IndividualSupplyingInformationAddressJourney(journey: IndividualSupplyingInformation)
        extends Onboarding[IndividualSupplyingInformation]

    final case class SubscriptionReadyAddressJourney(journey: SubscriptionReady) extends Onboarding[SubscriptionReady]

  }

  object ManagingSubscription {

    final case class SubscribedAddressJourney(journey: Subscribed) extends ManagingSubscription[Subscribed]

  }

  object Returns {

    final case class FillingOutReturnAddressJourney(journey: FillingOutReturn) extends Returns[FillingOutReturn]

  }

  implicit def eq[J <: JourneyStatus]: Eq[AddressJourneyType[J]] = Eq.fromUniversalEquals[AddressJourneyType[J]]

  implicit class AddressJourneyTypeOps(private val a: AddressJourneyType[_]) extends AnyVal {

    def showAccountMenu(): Boolean = a match {
      case _: ManagingSubscription[_] => true
      case _                          => false
    }
    def captionMessageKey(): String = a match {
      case _: Onboarding[_]           => "subscription.caption"
      case _: ManagingSubscription[_] => "account.caption"
      case f: FillingOutReturnAddressJourney =>
        f.journey.draftReturn.fold(
          _ => "returns.property-details.multipleDisposals.caption",
          _ => "returns.property-address.singleDisposal.caption"
        )
    }

    def showReturnToSummaryLink(): Boolean =
      a match {
        case _: Returns[_] => true
        case _             => false
      }
  }

}
