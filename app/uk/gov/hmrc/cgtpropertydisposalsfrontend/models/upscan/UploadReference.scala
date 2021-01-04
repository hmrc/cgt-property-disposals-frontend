/*
 * Copyright 2021 HM Revenue & Customs
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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.models.upscan

import cats.Eq
import play.api.libs.functional.syntax._
import play.api.libs.json.Format
import play.api.mvc.PathBindable

final case class UploadReference(value: String) extends AnyVal

object UploadReference {
  implicit val binder: PathBindable[UploadReference] =
    new PathBindable[UploadReference] {
      val stringBinder: PathBindable[String] = implicitly[PathBindable[String]]

      override def bind(
        key: String,
        value: String
      ): Either[String, UploadReference] =
        stringBinder.bind(key, value).map(UploadReference.apply)

      override def unbind(key: String, value: UploadReference): String =
        stringBinder.unbind(key, value.value)
    }
  implicit val eq: Eq[UploadReference]               = Eq.fromUniversalEquals
  implicit val format: Format[UploadReference]       =
    implicitly[Format[String]].inmap(UploadReference(_), _.value)
}
