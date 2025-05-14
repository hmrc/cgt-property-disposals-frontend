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

import cats.Eq
import play.api.libs.json.*

sealed trait AssetType extends Product with Serializable

object AssetType {

  case object Residential extends AssetType

  case object NonResidential extends AssetType

  case object IndirectDisposal extends AssetType

  case object MixedUse extends AssetType

  implicit val eq: Eq[AssetType] = Eq.fromUniversalEquals

  implicit val format: Format[AssetType] = new Format[AssetType] {
    override def reads(json: JsValue): JsResult[AssetType] = json match {
      case JsObject(fields) if fields.size == 1 =>
        fields.head._1 match {
          case "Residential"      => JsSuccess(Residential)
          case "NonResidential"   => JsSuccess(NonResidential)
          case "IndirectDisposal" => JsSuccess(IndirectDisposal)
          case "MixedUse"         => JsSuccess(MixedUse)
          case other              => JsError(s"Invalid asset type: $other")
        }
      case _                                    => JsError("Expected JSON object with one AssetType key")
    }

    override def writes(o: AssetType): JsValue = o match {
      case Residential      => Json.obj("Residential" -> Json.obj())
      case NonResidential   => Json.obj("NonResidential" -> Json.obj())
      case IndirectDisposal => Json.obj("IndirectDisposal" -> Json.obj())
      case MixedUse         => Json.obj("MixedUse" -> Json.obj())
    }
  }

}
