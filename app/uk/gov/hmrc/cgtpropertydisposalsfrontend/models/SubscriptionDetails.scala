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

import cats.syntax.either._
import play.api.libs.json.{Format, Json}

final case class SubscriptionDetails(
  forename: String,
  surname: String,
  emailAddress: String,
  address: Address,
  sapNumber: String
)

object SubscriptionDetails {

  implicit val format: Format[SubscriptionDetails] = Json.format

  def fromBusinessPartnerRecord(bpr: BusinessPartnerRecord): Either[String, SubscriptionDetails] =
    Either.fromOption(
      bpr.emailAddress.map(e => SubscriptionDetails(bpr.forename, bpr.surname, e, bpr.address, bpr.sapNumber)),
      "email address missing"
    )

}
