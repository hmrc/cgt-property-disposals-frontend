/*
 * Copyright 2025 HM Revenue & Customs
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

import julienrf.json.derived
import play.api.data.{Form, FormError}
import play.api.data.Forms.{mapping, nonEmptyText, of, optional}
import play.api.libs.json.{Format, Json, OFormat}
import play.api.mvc.{MultipartFormData, PathBindable}
import play.api.data.format.Formats._
import play.api.data.format.Formatter

import scala.reflect.io.File

case class FileUploadData(file: Option[MultipartFormData.FilePart[File]])
case class FileUploadData1(fileName: String)

object UploadForm {

  //implicit val format: Format[FileUploadData] = Json.format[FileUploadData]
  //val mapping: Mapping[FileUploadData] = {
  private def validateFileType(file: MultipartFormData.FilePart[File]): Boolean = {
    val allowedTypes = Seq("text/plain", "text/csv")
    file.contentType.exists(allowedTypes.contains)
  }

  private def getFileExtension(file: MultipartFormData.FilePart[File]): String = {
    val fileSplit = file.filename.split('.')
    var fileExt   = ""
    if (fileSplit.length > 1)
      fileExt = fileSplit(fileSplit.length - 1)
    fileExt
  }

  private def isLowerCase(str: String): Boolean = {
    val regex = "^[a-z]+$".r.pattern.asPredicate()
    regex.test(str);
  }

  private def validateFileExtension(file: MultipartFormData.FilePart[File]): Boolean    =
    isLowerCase(getFileExtension(file))
  private def validateFileHasExtension(file: MultipartFormData.FilePart[File]): Boolean =
    file.filename.contains('.')
  private def validateFileSize(file: MultipartFormData.FilePart[File]): Boolean = {
    val maxSize = 3 * 1024 * 1024 // 3MB
    file.ref.length <= maxSize
  }
  val fileUploadForm1: Form[FileUploadData1]                                            = Form(
    mapping(
      "fileName" -> nonEmptyText
    )(FileUploadData1.apply)(FileUploadData1.unapply)
  )
  implicit val format: OFormat[MultipartFormData.FilePart[File]] = derived.oformat()
  implicit val binder: Formatter[MultipartFormData.FilePart[File]]


  = new Formatter[MultipartFormData.FilePart[File]] {
    override def bind(key: String, data: Map[String, String]): Either[Seq[FormError], MultipartFormData.FilePart[File]] = ???
//validation here
    override def unbind(key: String, value: MultipartFormData.FilePart[File]): Map[String, String] =
      Map(key->value.filename)
  }

// reference to be shared

  val fileUploadForm: Form[FileUploadData] = Form(
    mapping(
      "file" -> optional(of[MultipartFormData.FilePart[File]])
        .verifying("error.empty-file", _.nonEmpty)
        .verifying("error.no-extension", file => file.forall(validateFileHasExtension))
        .verifying("error.uppercase-extension", file => file.forall(validateFileExtension))
        .verifying("error.wrong-extension", file => file.forall(validateFileType))
        .verifying("error.large-file", file => file.forall(validateFileSize))
    )(FileUploadData.apply)(FileUploadData.unapply)
  )





}

/*
trait Mappings extends Formatters {

  protected def text(errorKey: String = "error.required"): FieldMapping[String] =
    of(stringFormatter(errorKey))
}


trait Formatters {

  private[mappings] def stringFormatter(errorKey: String): Formatter[String] = new Formatter[String] {

    override def bind(key: String, data: Map[String, String]): Either[Seq[FormError], String] =
      data.get(key) match {
        case None => Left(Seq(FormError(key, errorKey)))
        case Some(x) if x.trim.length == 0 => Left(Seq(FormError(key, errorKey)))
        case Some(s) => Right(s.trim)
      }

    override def unbind(key: String, value: String): Map[String, String] =
      Map(key -> value.trim)
  }

  private[mappings] def intFormatter(requiredKey: String, nonNumericKey: String, args: Seq[String] = Seq.empty): Formatter[Int] =
    new Formatter[Int] {

      val formattingCharacters = "[\\s,\\-\\(\\)\\/\\.\\\\]"

      private val baseFormatter = stringFormatter(requiredKey)

      override def bind(key: String, data: Map[String, String]): Either[Seq[FormError], Int] =
        baseFormatter
          .bind(key, data)
          .map(_.replaceAll(formattingCharacters, ""))
          .flatMap {
            s =>
              nonFatalCatch
                .either(s.toInt)
                .left.map(_ => Seq(FormError(key, nonNumericKey, args)))
          }

      override def unbind(key: String, value: Int): Map[String, String] =
        baseFormatter.unbind(key, value.toString)
    }
}
 */
