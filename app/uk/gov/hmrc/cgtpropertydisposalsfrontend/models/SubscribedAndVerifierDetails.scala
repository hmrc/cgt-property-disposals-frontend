/*
 * Copyright 2019 HM Revenue & Customs
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
import play.api.libs.json.{Format, Json}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.EitherUtils.eitherFormat
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.CgtReference
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.{ContactName, IndividualName, TrustName}

final case class SubscribedAndVerifierDetails(
  name: Either[TrustName, IndividualName],
  emailAddress: Email,
  address: Address,
  previousAddress: Option[Address],
  contactName: ContactName,
  cgtReference: CgtReference,
  telephoneNumber: Option[TelephoneNumber],
  registeredWithId: Boolean
)

object SubscribedAndVerifierDetails {
  implicit val format: Format[SubscribedAndVerifierDetails] = Json.format
  implicit val eq: Eq[SubscribedAndVerifierDetails]         = Eq.fromUniversalEquals

  def fromSubscribedDetails(
    subscribedDetails: SubscribedDetails,
    maybeAddress: Option[Address]
  ): SubscribedAndVerifierDetails =
    maybeAddress match {
      case Some(address) =>
        SubscribedAndVerifierDetails(
          subscribedDetails.name,
          subscribedDetails.emailAddress,
          address,
          Some(subscribedDetails.address),
          subscribedDetails.contactName,
          subscribedDetails.cgtReference,
          subscribedDetails.telephoneNumber,
          subscribedDetails.registeredWithId
        )
      case None =>
        SubscribedAndVerifierDetails(
          subscribedDetails.name,
          subscribedDetails.emailAddress,
          subscribedDetails.address,
          None,
          subscribedDetails.contactName,
          subscribedDetails.cgtReference,
          subscribedDetails.telephoneNumber,
          subscribedDetails.registeredWithId
        )
    }

}
