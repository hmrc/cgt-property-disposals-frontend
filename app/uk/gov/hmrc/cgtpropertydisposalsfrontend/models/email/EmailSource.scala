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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.models.email

import cats.Eq
import play.api.libs.json.*

sealed trait EmailSource extends Product with Serializable

object EmailSource {

  case object GovernmentGateway extends EmailSource

  case object BusinessPartnerRecord extends EmailSource

  case object ManuallyEntered extends EmailSource

  implicit val eq: Eq[EmailSource] = Eq.fromUniversalEquals[EmailSource]

  implicit val format: Format[EmailSource] = new Format[EmailSource] {
    override def reads(json: JsValue): JsResult[EmailSource] = json match {
      case JsString("GovernmentGateway")     => JsSuccess(GovernmentGateway)
      case JsString("BusinessPartnerRecord") => JsSuccess(BusinessPartnerRecord)
      case JsString("ManuallyEntered")       => JsSuccess(ManuallyEntered)
      case _                                 => JsError("Invalid email source")
    }

    override def writes(o: EmailSource): JsValue = o match {
      case GovernmentGateway     => JsString("GovernmentGateway")
      case BusinessPartnerRecord => JsString("BusinessPartnerRecord")
      case ManuallyEntered       => JsString("ManuallyEntered")
    }
  }

}
