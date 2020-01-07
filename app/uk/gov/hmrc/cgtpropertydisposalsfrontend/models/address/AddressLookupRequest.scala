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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address

import play.api.data.Form
import play.api.data.Forms.{optional, text, mapping => formMapping}

final case class AddressLookupRequest(postcode: Postcode, filter: Option[String])

object AddressLookupRequest {
  val form: Form[AddressLookupRequest] = {
    Form(
      formMapping(
        "postcode" -> Postcode.mapping,
        "filter" -> optional(text).transform[Option[String]](_.filter(_.nonEmpty), identity)
      )(AddressLookupRequest.apply)(AddressLookupRequest.unapply)
    )
  }
}
