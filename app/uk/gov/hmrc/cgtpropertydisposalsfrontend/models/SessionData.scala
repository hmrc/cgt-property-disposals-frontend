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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.models

import cats.Eq
import play.api.libs.json.{Format, Json}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.AddressLookupResult
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.email.EmailToBeVerified
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.NeedMoreDetailsDetails

final case class SessionData(
  journeyStatus: Option[JourneyStatus],
  emailToBeVerified: Option[EmailToBeVerified],
  addressLookupResult: Option[AddressLookupResult],
  needMoreDetailsDetails: Option[NeedMoreDetailsDetails],
  userType: Option[UserType],
  journeyType: Option[JourneyType]
)

object SessionData {

  implicit val format: Format[SessionData] = Json.format

  implicit val eq: Eq[SessionData] = Eq.fromUniversalEquals[SessionData]

  val empty: SessionData = SessionData(None, None, None, None, None, None)

}
