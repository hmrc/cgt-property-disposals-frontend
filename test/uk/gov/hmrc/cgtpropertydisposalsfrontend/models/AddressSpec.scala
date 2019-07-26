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

import cats.data.NonEmptyList
import org.scalatest.{Matchers, WordSpec}
import play.api.libs.json.{JsArray, JsError, JsNumber, JsString, JsSuccess, Json}

class AddressSpec extends WordSpec with Matchers {

  "Address" must {

    "have a Format instance for NonEmptyList" in {
      import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Address.nelFormat

      Json.toJson(NonEmptyList.of(1, 2, 3)) shouldBe JsArray(Seq(JsNumber(1), JsNumber(2), JsNumber(3)))
      Json.fromJson[NonEmptyList[String]](JsArray(Seq(JsString("a"), JsString("b")))) shouldBe JsSuccess(NonEmptyList.of("a", "b"))
      Json.fromJson[NonEmptyList[String]](JsArray(Seq.empty[JsString])) shouldBe JsError("list was empty")
    }

  }

}
