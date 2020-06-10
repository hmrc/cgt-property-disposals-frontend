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
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.RegistrationStatus.{IndividualSupplyingInformation, RegistrationReady}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.SubscriptionStatus.{SubscriptionMissingData, SubscriptionReady}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, StartingNewDraftReturn, Subscribed}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.IndividualUserType.{Capacitor, PersonalRepresentative}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views.address.AddressJourneyType.Returns.{ChangingRepresenteeContactAddressJourney, EnteringCompanyDetails, FillingOutReturnAddressJourney}

sealed trait AddressJourneyType extends Product with Serializable

object AddressJourneyType {

  // user is entering/editing an address to subscribe with
  sealed trait Onboarding extends AddressJourneyType

  // user has subscribed and is managing their subscription address
  sealed trait ManagingSubscription extends AddressJourneyType

  // user is entering address they want to report cgt for
  sealed trait Returns extends AddressJourneyType

  object Onboarding {

    final case class RegistrationReadyAddressJourney(journey: RegistrationReady) extends Onboarding

    final case class IndividualSupplyingInformationAddressJourney(
      journey: IndividualSupplyingInformation
    ) extends Onboarding

    final case class SubscriptionReadyAddressJourney(journey: SubscriptionReady) extends Onboarding

    final case class SubscriptionEnterAddressJourney(journey: SubscriptionMissingData) extends Onboarding

  }

  object ManagingSubscription {

    final case class SubscribedAddressJourney(journey: Subscribed) extends ManagingSubscription

  }

  object Returns {

    final case class FillingOutReturnAddressJourney(
      journey: FillingOutReturn,
      draftReturn: Either[
        DraftMultipleDisposalsReturn,
        DraftSingleDisposalReturn
      ],
      individualUserType: Option[IndividualUserType]
    ) extends Returns

    final case class ChangingRepresenteeContactAddressJourney(
      journey: Either[StartingNewDraftReturn, FillingOutReturn],
      answers: RepresenteeAnswers,
      contactDetails: RepresenteeContactDetails
    ) extends Returns

    final case class EnteringCompanyDetails(
      journey: FillingOutReturn,
      draftReturn: Either[DraftMultipleIndirectDisposalsReturn, DraftSingleIndirectDisposalReturn],
      representativeType: Option[
        Either[PersonalRepresentative.type, Capacitor.type]
      ],
      isATrust: Boolean
    ) extends Returns

  }

  implicit val eq: Eq[AddressJourneyType] =
    Eq.fromUniversalEquals[AddressJourneyType]

  implicit class AddressJourneyTypeOps(private val a: AddressJourneyType) extends AnyVal {

    def showAccountMenu(): Boolean  =
      a match {
        case _: ManagingSubscription => true
        case _                       => false
      }
    def captionMessageKey(): String =
      a match {
        case _: Onboarding                               => "subscription.caption"
        case _: ManagingSubscription                     => "account.caption"
        case f: FillingOutReturnAddressJourney           =>
          f.draftReturn.fold(
            _ => "returns.property-details.multipleDisposals.caption",
            _ => "returns.property-address.singleDisposal.caption"
          )
        case c: EnteringCompanyDetails                   =>
          c.draftReturn.fold(
            _ => "returns.company-details.multipleIndirectDisposals.caption",
            _ => "companyDetails.caption"
          )

        case _: ChangingRepresenteeContactAddressJourney =>
          "representee.caption"
      }

    def showReturnToSummaryLink(): Boolean =
      a match {
        case _: FillingOutReturnAddressJourney           => true
        case c: ChangingRepresenteeContactAddressJourney => c.journey.isRight
        case _: EnteringCompanyDetails                   => true
        case _                                           => false
      }
  }

}
