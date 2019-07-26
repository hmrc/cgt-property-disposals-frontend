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
import play.api.libs.json.{Format, JsError, JsSuccess, Json, Reads, Writes}

final case class Address(
    lines: NonEmptyList[String],
    postcode: String,
    countryCode: String
)

object Address {

  implicit def nelFormat[A](implicit fa: Format[A]): Format[NonEmptyList[A]] = {
    val f = implicitly[Format[List[A]]]
    Format(
      Reads(j =>
        f.reads(j).flatMap {
          case Nil    => JsError("list was empty")
          case h :: t => JsSuccess(NonEmptyList.of(h, t: _*))
        }),
      Writes(l => f.writes(l.toList))
    )
  }

  implicit val format: Format[Address] = Json.format

}
