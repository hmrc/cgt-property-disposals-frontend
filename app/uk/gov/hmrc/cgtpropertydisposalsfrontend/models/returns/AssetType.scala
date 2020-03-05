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
import cats.syntax.eq._
import julienrf.json.derived
import play.api.libs.json.OFormat

sealed trait AssetType extends Product with Serializable

object AssetType {

  case object Residential extends AssetType

  case object NonResidential extends AssetType

  case object IndirectDisposal extends AssetType

  case object MixedUse extends AssetType

  implicit val eq: Eq[AssetType] = Eq.fromUniversalEquals

  implicit val format: OFormat[AssetType] = derived.oformat()

  implicit class AssetTypeOps(private val a: AssetType) extends AnyVal {
    def isResidential(): Boolean = a === AssetType.Residential
  }

}
