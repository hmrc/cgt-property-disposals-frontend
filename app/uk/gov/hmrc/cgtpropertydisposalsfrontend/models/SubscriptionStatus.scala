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

import julienrf.json.derived
import play.api.libs.json.OFormat
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.EitherFormat.eitherFormat

sealed trait SubscriptionStatus

object SubscriptionStatus {

  final case object OrganisationUnregisteredTrust extends SubscriptionStatus

  final case class IndividualWithInsufficientConfidenceLevel(
                                                          hasNino: Option[Boolean],
                                                          hasSautr: Option[HasSAUTR],
                                                          name: Name,
                                                          email: Option[Email]) extends SubscriptionStatus

  // entity is missing data in order to continue on with subscription
  final case class SubscriptionMissingData(businessPartnerRecord: BusinessPartnerRecord,
                                           name: Either[TrustName,Name]
                                          ) extends SubscriptionStatus

  // subscription details have been gathered and are ready to be used to subscribe
  final case class SubscriptionReady(subscriptionDetails: SubscriptionDetails) extends SubscriptionStatus

  // subscription has been done successfully
  final case class SubscriptionComplete(
    subscriptionDetails: SubscriptionDetails,
    subscriptionResponse: SubscriptionResponse)
      extends SubscriptionStatus

  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  implicit val format: OFormat[SubscriptionStatus] =
    derived.oformat[SubscriptionStatus]

}
