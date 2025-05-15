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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.views.components

sealed trait AutoCompleteType {
  val value: String
}
object AutoCompleteType {
  case object On extends AutoCompleteType {
    val value: String = "on"
  }
  case object FirstName extends AutoCompleteType {
    val value: String = "given-name"
  }
  case object LastName extends AutoCompleteType {
    val value: String = "family-name"
  }
  case object ContactName extends AutoCompleteType {
    val value: String = "name"
  }
  case object Email extends AutoCompleteType {
    val value: String = "email"
  }
  case object TrustName extends AutoCompleteType {
    val value: String = "organization"
  }
  case object StreetAddress extends AutoCompleteType {
    val value: String = "street-address"
  }
  case object AddressLine1 extends AutoCompleteType {
    val value: String = "address-line1"
  }
  case object AddressLine2 extends AutoCompleteType {
    val value: String = "address-line2"
  }
  case object AddressLevel2 extends AutoCompleteType {
    val value: String = "address-level2"
  }
  case object AddressLevel1 extends AutoCompleteType {
    val value: String = "address-level1"
  }
  case object Country extends AutoCompleteType {
    val value: String = "country"
  }
  case object CountryName extends AutoCompleteType {
    val value: String = "country-name"
  }
  case object Postcode extends AutoCompleteType {
    val value: String = "postal-code"
  }
}
