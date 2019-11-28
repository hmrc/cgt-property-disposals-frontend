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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding

import julienrf.json.derived
import play.api.libs.json.{Json, OFormat}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.NeedMoreDetailsDetails.AffinityGroup

final case class NeedMoreDetailsDetails(continueUrl: String, affinityGroup: AffinityGroup)

object NeedMoreDetailsDetails {

  sealed trait AffinityGroup extends Product with Serializable

  object AffinityGroup {

    case object Individual extends AffinityGroup

    case object Organisation extends AffinityGroup

    @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
    implicit val format: OFormat[AffinityGroup] = derived.oformat[AffinityGroup]

  }

  implicit val format: OFormat[NeedMoreDetailsDetails] = Json.format

}
