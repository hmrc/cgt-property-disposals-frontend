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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns

import cats.Eq
import julienrf.json.derived
import play.api.libs.json.OFormat

sealed trait IndividualUserType extends Product with Serializable

sealed trait RepresentativeType extends IndividualUserType

object IndividualUserType {

  case object Self extends IndividualUserType

  case object Capacitor extends RepresentativeType

  case object PersonalRepresentative extends RepresentativeType

  case object PersonalRepresentativeInPeriodOfAdmin extends RepresentativeType

  implicit val individualUserTypeEq: Eq[IndividualUserType] = Eq.fromUniversalEquals

  implicit val representativeTypeEq: Eq[RepresentativeType] = Eq.fromUniversalEquals

  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  implicit val format: OFormat[IndividualUserType] = derived.oformat()

}
