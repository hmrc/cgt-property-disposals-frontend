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

import julienrf.json.derived
import play.api.libs.json.{Json, OFormat}

sealed trait UploadSupportingDocumentAnswers extends Product with Serializable

object UploadSupportingDocumentAnswers {

  final case class SupportingDocuments(
    reference: String,
    fileName: String
  )

  object SupportingDocuments {
    implicit val format = Json.format[SupportingDocuments]
  }

  final case class IncompleteUploadSupportingDocumentAnswers(
    doYouWantToUploadSupportingDocuments: Option[Boolean]
  ) extends UploadSupportingDocumentAnswers

  object IncompleteUploadSupportingDocumentAnswers {
    val empty: IncompleteUploadSupportingDocumentAnswers =
      IncompleteUploadSupportingDocumentAnswers(None)
  }

  final case class CompleteUploadSupportingDocumentAnswers(
    doYouWantToUploadSupportingDocuments: Boolean,
    documents: List[SupportingDocuments]
  ) extends UploadSupportingDocumentAnswers

  implicit class UploadSupportingDocumentsOps(private val a: UploadSupportingDocumentAnswers) extends AnyVal {

    def fold[A](
      ifIncomplete: IncompleteUploadSupportingDocumentAnswers => A,
      ifComplete: CompleteUploadSupportingDocumentAnswers => A
    ): A = a match {
      case i: IncompleteUploadSupportingDocumentAnswers => ifIncomplete(i)
      case c: CompleteUploadSupportingDocumentAnswers   => ifComplete(c)
    }

  }

  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  implicit val format: OFormat[UploadSupportingDocumentAnswers] = derived.oformat()

}
