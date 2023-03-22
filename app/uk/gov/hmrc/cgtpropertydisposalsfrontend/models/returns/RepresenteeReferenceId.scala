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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns

import julienrf.json.derived
import play.api.libs.json.OFormat
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.{CgtReference, NINO, SAUTR}

sealed trait RepresenteeReferenceId extends Product with Serializable

object RepresenteeReferenceId {

  final case class RepresenteeNino(value: NINO) extends RepresenteeReferenceId

  final case class RepresenteeSautr(value: SAUTR) extends RepresenteeReferenceId

  final case class RepresenteeCgtReference(value: CgtReference) extends RepresenteeReferenceId

  case object NoReferenceId extends RepresenteeReferenceId

  implicit val format: OFormat[RepresenteeReferenceId] = derived.oformat()

}
