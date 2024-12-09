/*
 * Copyright 2024 HM Revenue & Customs
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
import play.api.data.Forms.nonEmptyText
import play.api.data.Mapping
import play.api.data.validation.{Constraint, Invalid, Valid, ValidationResult}
import play.api.libs.functional.syntax._
import play.api.libs.json.Format

import java.util.Locale
import java.util.function.Predicate

final case class UploadFileType(value: String) extends AnyVal
object UploadFileType {

  val allowedExtensions: Seq[String] = Seq(".csv", ".txt")

  //if allowed ext contains value.last index of value .substring()
  //case sensitive

  implicit val format: Format[UploadFileType] =
    implicitly[Format[String]].inmap(UploadFileType(_), _.value)

  val mapping: Mapping[UploadFileType] = {

    def validateFileType(p: UploadFileType): ValidationResult =
      if (p.value.substring(p.value.lastIndexOf(".")).length <= 0) {
        Invalid("error.pattern1")
      } else if (!allowedExtensions.contains(p.value.substring(p.value.lastIndexOf(".")))) {
        Invalid("error.pattern2")
      } else {
        Valid
      }

    nonEmptyText
      .transform[UploadFileType](p => UploadFileType(p.trim), _.value)
      .verifying(Constraint[UploadFileType](validateFileType(_)))
  }

}
