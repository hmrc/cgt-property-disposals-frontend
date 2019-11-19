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

import cats.data.NonEmptyList
import cats.syntax.either._
import play.api.libs.json.{Format, Json}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.EitherUtils.eitherFormat
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.bpr.BusinessPartnerRecord
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.{ContactName, IndividualName, TrustName}

final case class SubscriptionDetails(
  name: Either[TrustName, IndividualName],
  emailAddress: Email,
  address: Address,
  contactName: ContactName,
  sapNumber: String,
  isGGEmail: Option[Boolean] = None
)

object SubscriptionDetails {

  implicit val format: Format[SubscriptionDetails] = Json.format

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  def apply(
    bpr: BusinessPartnerRecord,
    maybeEmail: Option[Email]
  ): Either[NonEmptyList[MissingData], SubscriptionDetails] =
    Either
      .fromOption(
        bpr.emailAddress.orElse(maybeEmail),
        NonEmptyList.one(MissingData.Email)
      )
      .map(
        email => {
          SubscriptionDetails(
            bpr.name,
            email,
            bpr.address,
            ContactName(bpr.name.fold(_.value, n => n.makeSingleName())),
            bpr.sapNumber,
            (bpr.emailAddress, maybeEmail) match {
              case (Some(_), _)    => Some(false)
              case (None, Some(_)) => Some(true)
              case (None, None)    => None
            }
          )
        }
      )

  sealed trait MissingData extends Product with Serializable

  object MissingData {

    case object Email extends MissingData

  }

}
