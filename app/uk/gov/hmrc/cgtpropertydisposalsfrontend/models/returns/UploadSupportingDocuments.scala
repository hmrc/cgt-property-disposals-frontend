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

sealed trait UploadSupportingDocuments extends Product with Serializable

object UploadSupportingDocuments {

  final case class SupportingDocument(upscanReference: String)

  object SupportingDocument {
    implicit val format = Json.format[SupportingDocument]
  }

  final case class IncompleteUploadSupportingDocuments(
    hasSupportingDocuments: Option[Boolean]
  ) extends UploadSupportingDocuments

  object IncompleteUploadSupportingDocuments {
    val empty: IncompleteUploadSupportingDocuments =
      IncompleteUploadSupportingDocuments(None)
  }

  final case class CompleteUploadSupportingDocuments(
    hasSupportingDocuments: Boolean
  ) extends UploadSupportingDocuments

  implicit class UploadSupportingDocumentsOps(private val a: UploadSupportingDocuments) extends AnyVal {

    def fold[A](
      ifIncomplete: IncompleteUploadSupportingDocuments => A,
      ifComplete: CompleteUploadSupportingDocuments => A
    ): A = a match {
      case i: IncompleteUploadSupportingDocuments => ifIncomplete(i)
      case c: CompleteUploadSupportingDocuments   => ifComplete(c)
    }

  }

  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  implicit val format: OFormat[UploadSupportingDocuments] = derived.oformat()

}
