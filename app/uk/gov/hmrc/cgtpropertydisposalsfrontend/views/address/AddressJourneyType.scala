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

sealed trait AddressJourneyType extends Product with Serializable

object AddressJourneyType {

  // user is entering/editing an address to subscribe with
  case object Onboarding extends AddressJourneyType

  // user has subscribed and is managing their subscription address
  case object ManagingSubscription extends AddressJourneyType

  // user is entering address they want to report cgt for
  case object Returns extends AddressJourneyType

  implicit val eq: Eq[AddressJourneyType] = Eq.fromUniversalEquals[AddressJourneyType]

  implicit class AddressJourneyTypeOps(private val a: AddressJourneyType) extends AnyVal {

    def showAccountMenu(): Boolean = a === ManagingSubscription

    def captionMessageKey(): String = a match {
      case Onboarding           => "subscription.caption"
      case ManagingSubscription => "account.caption"
      case Returns              => "returns.property-address.caption"
    }

    def showReturnToSummaryLink(): Boolean = a === Returns
  }

}
