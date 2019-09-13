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
import play.api.libs.json.{Format, Json}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.EitherFormat.eitherFormat

final case class SubscriptionDetails(
                                      contactName: Either[TrustName,Name],
                                      emailAddress: String,
                                      address: Address,
                                      sapNumber: String
)

object SubscriptionDetails {

  implicit val format: Format[SubscriptionDetails] = Json.format

  def apply(bpr: BusinessPartnerRecord, name: Name, maybeEmail: Option[Email]): Either[NonEmptyList[MissingData], SubscriptionDetails] =
    bpr.emailAddress.orElse(maybeEmail.map(_.value))
      .fold[Either[NonEmptyList[MissingData], SubscriptionDetails]](
        Left(NonEmptyList.one(MissingData.Email))
      )(email => Right(SubscriptionDetails(Right(name), email, bpr.address, bpr.sapNumber)))

  sealed trait MissingData extends Product with Serializable

  object MissingData {

    case object Email extends MissingData

  }

}
