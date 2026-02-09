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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding

import cats.Eq
import cats.syntax.either._
import play.api.libs.json.{Format, Json}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.email.Email
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.CgtReference
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.{ContactName, IndividualName, TrustName}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.EitherUtils.eitherFormat
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{TelephoneNumber, UserType}

final case class SubscribedDetails(
  name: Either[TrustName, IndividualName],
  emailAddress: Option[Email],
  address: Address,
  contactName: ContactName,
  cgtReference: CgtReference,
  telephoneNumber: Option[TelephoneNumber],
  registeredWithId: Boolean
)

object SubscribedDetails {
  implicit val format: Format[SubscribedDetails] = Json.format
  implicit val eq: Eq[SubscribedDetails]         = Eq.fromUniversalEquals

  implicit class SubscribedDetailsOps(private val details: SubscribedDetails) extends AnyVal {
    def makeAccountName(): String                                                =
      details.name.fold(n => n.value, n => n.makeSingleName)
    def userType(): Either[UserType.Organisation.type, UserType.Individual.type] =
      details.name.bimap(_ => UserType.Organisation, _ => UserType.Individual)
    def isATrust: Boolean                                                        = userType().isLeft
  }
}
